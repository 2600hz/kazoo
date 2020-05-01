%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc Pickup a call in the specified group/device/user/extension.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`type'</dt>
%%%   <dd>"Can be one of the `group', `user', `device' or `extension' values.</dd>
%%% </dl>
%%%
%%% This feature is using Callflow `cf_capture_group' and type to build parameters
%%% to branch to {@link cf_group_pickup}.
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
%%% @author Luis Azedo  <luis.azedo@factorlusitano.com>
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_group_pickup_feature).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).


%%------------------------------------------------------------------------------
%% @doc Entry point for this module, creates the parameters and branches
%% to cf_group_pickup.
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Number = kapps_call:kvs_fetch('cf_capture_group', Call),
    PickupType = kz_json:get_ne_binary_value(<<"type">>, Data, <<"extension">>),
    case build_pickup_params(Number, PickupType, Call) of
        {'ok', Params} ->
            UpdatedData = kz_json:set_values(Params, Data),
            cf_group_pickup:handle(UpdatedData, Call);
        {'error', _E} ->
            lager:info("error <<~s>> processing pickup '~s' for number ~s"
                      ,[_E, PickupType, Number]
                      ),
            _ = kapps_call_command:b_prompt(<<"park-no_caller">>, Call),
            cf_exe:stop(Call)
    end.

-spec build_pickup_params(kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) ->
          {'ok', kz_term:proplist()} |
          {'error', kz_term:ne_binary()}.
build_pickup_params(Number, <<"device">>, Call) ->
    AccountDb = kapps_call:account_db(Call),
    case cf_util:endpoint_id_by_sip_username(AccountDb, Number) of
        {'ok', EndpointId} -> {'ok', [{<<"device_id">>, EndpointId}]};
        {'error', _}=E -> E
    end;
build_pickup_params(_Number, <<"user">>, _Call) ->
    {'error', <<"work in progress">>};
build_pickup_params(_Number, <<"group">>, _Call) ->
    {'error', <<"work in progress">>};
build_pickup_params(Number, <<"extension">>, Call) ->
    AccountId = kapps_call:account_id(Call),
    case cf_flow:lookup(Number, AccountId) of
        {'ok', FlowDoc, 'false'} ->
            Data = kz_json:get_json_value([<<"flow">>, <<"data">>], FlowDoc),
            Module = kz_json:get_ne_binary_value([<<"flow">>, <<"module">>], FlowDoc),
            ChildFlow = kz_json:get_json_value([<<"flow">>, <<"children">>, <<"_">>], FlowDoc),
            params_from_data(Module, Data, ChildFlow);
        {'ok', _FlowDoc, 'true'} ->
            {'error', <<"no callflow with extension ", Number/binary>>};
        {'error', _} = E -> E
    end;
build_pickup_params(_, Other, _) ->
    {'error', <<Other/binary," not implemented">>}.

-spec params_from_data(kz_term:api_ne_binary(), kz_json:object(), kz_term:api_object()) ->
          {'ok', kz_term:proplist()} |
          {'error', kz_term:ne_binary()}.
params_from_data(_, _, 'undefined') ->
    {'error',<<"callflow not defined">>};
params_from_data(<<"user">>, Data, _Flow) ->
    EndpointId = kz_json:get_ne_binary_value(<<"id">>, Data),
    {'ok', [{<<"user_id">>, EndpointId}]};
params_from_data(<<"device">>, Data, _Flow) ->
    EndpointId = kz_json:get_ne_binary_value(<<"id">>, Data),
    {'ok', [{<<"device_id">>, EndpointId}]};
params_from_data(<<"ring_group">>, Data, _Flow) ->
    [Endpoint |_Endpoints] = kz_json:get_list_value(<<"endpoints">>, Data, []),
    EndpointType = kz_json:get_ne_binary_value(<<"endpoint_type">>, Endpoint),
    {'ok', [{<<EndpointType/binary, "_id">>, kz_doc:id(Endpoint)}]};
params_from_data(<<"page_group">>, Data, _Flow) ->
    params_from_data(<<"ring_group">>, Data, _Flow);
params_from_data('undefined', _, _) ->
    {'error',<<"module not defined in callflow">>};
params_from_data(_Other, _, Flow) ->
    lager:debug("skipping module ~p, looking at children", [_Other]),
    Child = kz_json:get_json_value([<<"children">>, <<"_">>], Flow),
    Data = kz_json:get_json_value([<<"data">>], Child),
    Module = kz_json:get_ne_binary_value([<<"module">>], Child),
    params_from_data(Module, Data, Child).
