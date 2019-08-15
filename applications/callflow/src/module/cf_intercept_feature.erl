%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Intercept a call in the specified device/user/extension.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`type'</dt>
%%%   <dd>"Can be one of the `group', `user', `device' or `extension' values.</dd>
%%% </dl>
%%%
%%% This feature is using Callflow `cf_capture_group' and type to build parameters
%%% to branch to {@link cf_intercept}.
%%%
%%% <strong>Actions will result in:</strong>
%%% <ul>
%%%   <li>`group': Lookup groups by number (WIP)</li>
%%%   <li>`user': Lookup groups by number (WIP)</li>
%%%   <li>`device': Lookup device by SIP user name</li>
%%%   <li>`extension': Lookup Callflows</li>
%%% </ul>
%%%
%%% <h4>Usage Example for BLF on `spa504g'</h4>
%%% The SIP user of device we want to monitor for this example is 55578547.
%%%
%%% <ol>
%%%    <li>On <i>Phone</i> Tab, go to <i>Line Key 2</i> and set:
%%%      <ul>
%%%        <li><strong>Extension: </strong><i>disabled</i></li>
%%%        <li><strong>Share Call Appearance: </strong><i>private</i></li>
%%%        <li><strong>Extended Function: </strong>`:fnc=blf+cp;sub=55578547@sip.domain.com;ext=55578547@sip.domain.com'</li>
%%%      </ul>
%%%    </li>
%%%
%%%    <li>On <i>Attendant Console</i> Tab, set <i>Attendant Console Call Pickup Code</i> to `*98#' instead of `*98'.
%%%        This way the user name part of the subscription is passed along (`*9855578547').
%%%    </li>
%%%
%%%    <li>Create a <i>pattern</i> Callflow with `patterns' key as: `["^\\*98([0-9]*)$"]'. Set the parameter `type' to `device'.</li>
%%% </ol>
%%%
%%% <h4>Usage Example for Extension Pickup</h4>
%%% <ol>
%%%   <li>Create a <i>pattern</i> Callflow with `patterns' key as: `["^\\*7([0-9]*)$"]' and set the parameter `type' to `extension'.</li>
%%%   <li>Create simple Callflow with `number' set as `401' and set the target to a <i>ring group</i> or <i>page group</i>.</li>
%%%   <li>Dial `401' to start ringing the phones in group, in another phone dial `*7401' to pickup the call</li>
%%% </ol>
%%%
%%%
%%% @author SIPLABS LLC (Mikhail Rodionov)
%%% @author SIPLABS LLC (Maksim Krzhemenevskiy)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_intercept_feature).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module, creates the parameters and branches
%% to cf_intercept.
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Number = kapps_call:kvs_fetch('cf_capture_group', Call),
    InterceptType = kz_json:get_ne_binary_value(<<"type">>, Data),
    case build_intercept_params(Number, InterceptType, Call) of
        {'ok', Params} ->
            maybe_intercept(Data, Call, Params);
        {'error', _E} ->
            lager:info("error <<~s>> processing intercept '~s' for number ~s"
                      ,[_E, InterceptType, Number]
                      ),
            _ = kapps_call_command:b_play(<<"park-no_caller">>, Call),
            cf_exe:stop(Call)
    end.

-spec maybe_intercept(kz_json:object(), kapps_call:call(), kz_term:proplist()) -> 'ok'.
maybe_intercept(Data, Call, Params) ->
    case intercept_restrictions(Data) of
        [] -> maybe_intercept(Call, Params);
        SParams ->
            cf_intercept:handle(kz_json:from_list(SParams ++ Params), Call)
    end.

-spec maybe_intercept(kapps_call:call(), kz_term:proplist()) -> 'ok'.
maybe_intercept(Call, Params) ->
    case maybe_allowed_to_intercept(Call, Params) of
        'true' ->
            cf_intercept:handle(kz_json:from_list(Params), Call);
        'false' ->
            _ = no_permission_to_intercept(Call),
            cf_exe:stop(Call)
    end.

-spec intercept_restrictions(kz_json:object()) -> kz_term:proplist().
intercept_restrictions(Data) ->
    props:filter_undefined(
      [{<<"approved_device_id">>, kz_json:get_ne_binary_value(<<"approved_device_id">>, Data)}
      ,{<<"approved_user_id">>, kz_json:get_ne_binary_value(<<"approved_user_id">>, Data)}
      ,{<<"approved_group_id">>, kz_json:get_ne_binary_value(<<"approved_group_id">>, Data)}
      ]).

-spec maybe_allowed_to_intercept(kapps_call:call(), kz_term:proplist()) -> boolean().
maybe_allowed_to_intercept(Call, Props) ->
    case kz_datamgr:open_cache_doc(kapps_call:account_db(Call), kapps_call:authorizing_id(Call)) of
        {'ok', DeviceDoc} ->
            maybe_allowed_to_intercept(Call, Props, DeviceDoc);
        {'error', _Err} ->
            lager:info("error while opening couch document: ~p", [_Err]),
            'false'
    end.

-spec maybe_allowed_to_intercept(kapps_call:call(), kz_term:proplist(), kz_json:object()) -> boolean().
maybe_allowed_to_intercept(Call, Props, DeviceDoc) ->
    case props:get_value(<<"user_id">>, Props) of
        'undefined' ->
            can_device_intercept(Call, Props, DeviceDoc);
        UserId -> UserId =:= kz_json:get_value(<<"owner_id">>, DeviceDoc)
    end.

-spec can_device_intercept(kapps_call:call(), kz_term:proplist(), kz_json:object()) -> boolean().
can_device_intercept(Call, Props, DeviceDoc) ->
    device_has_same_owner(Call, DeviceDoc, props:get_value(<<"device_id">>, Props)).

-spec device_has_same_owner(kapps_call:call(), kz_json:object(), kz_term:api_binary()) -> boolean().
device_has_same_owner(_Call, _Device, 'undefined') -> 'false';
device_has_same_owner(Call, DeviceDoc, TargetDeviceId) ->
    case kz_datamgr:open_cache_doc(kapps_call:account_db(Call), TargetDeviceId) of
        {'ok', TargetDevice} ->
            kz_json:get_value(<<"owner_id">>, DeviceDoc)
                =:= kz_json:get_value(<<"owner_id">>, TargetDevice);
        {'error', _E} ->
            lager:info("error while opening device ~s: ~p", [TargetDeviceId, _E]),
            'false'
    end.

-spec build_intercept_params(kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) ->
                                    {'ok', kz_term:proplist()} |
                                    {'error', kz_term:ne_binary()}.
build_intercept_params(Number, <<"device">>, Call) ->
    AccountDb = kapps_call:account_db(Call),
    case cf_util:endpoint_id_by_sip_username(AccountDb, Number) of
        {'ok', EndpointId} -> {'ok', [{<<"device_id">>, EndpointId}]};
        {'error', _}=E -> E
    end;
build_intercept_params(_Number, <<"user">>, _Call) ->
    {'error', <<"work in progress">>};
build_intercept_params(Number, <<"extension">>, Call) ->
    case cf_eavesdrop_feature:get_target_for_extension(Number, Call) of
        'error' -> {'error', <<"Can't find target for extension ", Number/binary>>};
        {'ok', TargetId, TargetType} ->
            Data = kz_json:from_list([{<<"id">>, TargetId}]),
            params_from_data(TargetType, Data, Call)
    end;
build_intercept_params(_ ,'undefined', _) ->
    {'error', <<"parameter 'type' not defined">>};
build_intercept_params(_, Other, _) ->
    {'error', <<Other/binary," not implemented">>}.

-spec params_from_data(kz_term:ne_binary(), kz_json:object(), kapps_call:call()) ->
                              {'ok', kz_term:proplist()} |
                              {'error', kz_term:ne_binary()}.
params_from_data(<<"user">>, Data, _Call) ->
    EndpointId = kz_doc:id(Data),
    {'ok', [{<<"user_id">>, EndpointId}]};
params_from_data(<<"device">>, Data, _Call) ->
    EndpointId = kz_doc:id(Data),
    {'ok', [{<<"device_id">>, EndpointId}]};
params_from_data(Other, _, _) ->
    {'error',<<"module ",Other/binary," not implemented">>}.

-spec no_permission_to_intercept(kapps_call:call()) -> any().
no_permission_to_intercept(Call) ->
    kapps_call_command:answer(Call),
    kapps_call_command:b_prompt(<<"intercept-no_channels">>, Call).
