%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz INC
%%% @doc
%%% Intercept a call in the specified device/user/extension
%%%
%%% data: {
%%%   "type" : "user | device | extension"
%%% }
%%%
%%% uses cf_capture_group and type to build parameters to branch to cf_intercept
%%%
%%% user -> lookup groups by number  (WIP)
%%% device -> lookup device by sip username
%%% extension -> lookup callflows
%%%
%%% usage example for BLF on spa504g
%%% the sip user of device we want to monitor for this example is 55578547
%%% on "Phone" Tab, go to "Line Key 2" and set
%%%   Extension : disabled
%%%   Share Call Appearance : private
%%%   Extended Function :fnc=blf+cp;sub=55578547@sip.domain.com;ext=55578547@sip.domain.com
%%%
%%% on "Attendant Console" Tab, set "Attendant Console Call Pickup Code:" to *98# instead of *98
%%% this way the username part of the subscription is passed along (*9855578547)
%%%
%%% create a "pattern callflow" with "patterns": ["^\\*98([0-9]*)$"]
%%% set the parameter "type" to "device"
%%%
%%%
%%% usage example for extension pickup
%%%
%%% 1) create a "pattern callflow" with "patterns": ["^\\*7([0-9]*)$"] and set the parameter "type" to "extension"
%%% 2) create simple callflow with number = 401 and set the target to a "ring group" or "page group"
%%% 3) dial 401 to start ringing the phones in group, in another phone dial *7401 to pickup the call
%%%
%%%
%%%
%%% @end
%%% @contributors
%%%   SIPLABS LLC (Mikhail Rodionov)
%%%   SIPLABS LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(cf_intercept_feature).

-include("callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, creates the parameters and branches
%% to cf_intercept.
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Number = kapps_call:kvs_fetch('cf_capture_group', Call),
    InterceptType = kz_json:get_value(<<"type">>, Data),
    case build_intercept_params(Number, InterceptType, Call) of
        {'ok', Params} ->
            maybe_intercept(Data, Call, Params);
        {'error', _E} ->
            lager:info("Error <<~s>> processing intercept '~s' for number ~s"
                       ,[_E, InterceptType, Number]
                      ),
            _ = kapps_call_command:b_play(<<"park-no_caller">>, Call),
            cf_exe:stop(Call)
    end.

-spec maybe_intercept(kz_json:object(), kapps_call:call(), kz_proplist()) -> 'ok'.
maybe_intercept(Data, Call, Params) ->
    case intercept_restrictions(Data) of
        [] -> maybe_intercept(Call, Params);
        SParams ->
            cf_intercept:handle(kz_json:from_list(SParams ++ Params), Call)
    end.

-spec maybe_intercept(kapps_call:call(), kz_proplist()) -> 'ok'.
maybe_intercept(Call, Params) ->
    case maybe_allowed_to_intercept(Call, Params) of
        'true' ->
            cf_intercept:handle(kz_json:from_list(Params), Call);
        'false' ->
            no_permission_to_intercept(Call),
            cf_exe:stop(Call)
    end.

-spec intercept_restrictions(kz_json:object()) -> kz_proplist().
intercept_restrictions(Data) ->
    props:filter_undefined(
      [{<<"approved_device_id">>, kz_json:get_value(<<"approved_device_id">>, Data)}
       ,{<<"approved_user_id">>, kz_json:get_value(<<"approved_user_id">>, Data)}
       ,{<<"approved_group_id">>, kz_json:get_value(<<"approved_group_id">>, Data)}
      ]).

-spec maybe_allowed_to_intercept(kapps_call:call(), kz_proplist()) -> boolean().
maybe_allowed_to_intercept(Call, Props) ->
    case kz_datamgr:open_cache_doc(kapps_call:account_db(Call), kapps_call:authorizing_id(Call)) of
        {'ok', DeviceDoc} ->
            maybe_allowed_to_intercept(Call, Props, DeviceDoc);
        {'error', _Err} ->
            lager:info("error while opening couch document: ~p", [_Err]),
            'false'
    end.

-spec maybe_allowed_to_intercept(kapps_call:call(), kz_proplist(), kz_json:object()) -> boolean().
maybe_allowed_to_intercept(Call, Props, DeviceDoc) ->
    case props:get_value(<<"user_id">>, Props) of
        'undefined' ->
            can_device_intercept(Call, Props, DeviceDoc);
        UserId -> UserId =:= kz_json:get_value(<<"owner_id">>, DeviceDoc)
    end.

-spec can_device_intercept(kapps_call:call(), kz_proplist(), kz_json:object()) -> boolean().
can_device_intercept(Call, Props, DeviceDoc) ->
    device_has_same_owner(Call, DeviceDoc, props:get_value(<<"device_id">>, Props)).

-spec device_has_same_owner(kapps_call:call(), kz_json:object(), maybe(binary())) -> boolean().
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

-spec build_intercept_params(ne_binary(), ne_binary(), kapps_call:call()) ->
                                    {'ok', kz_proplist()} |
                                    {'error', ne_binary()}.
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

-spec params_from_data(ne_binary(), kz_json:object(), kapps_call:call()) ->
                              {'ok', kz_proplist()} |
                              {'error', ne_binary()}.
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
