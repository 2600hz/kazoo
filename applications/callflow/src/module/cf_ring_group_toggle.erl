%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Logs a user in or out of a ring group.
%%% @author Max Lay
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_ring_group_toggle).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

%% 0000 00:00:00
-define(ON_VAL, 0).
%% 2100 00:00:00
-define(DEFAULT_OFF_VAL, 66269664000).

-define(MAX_SAVE_RETRIES, 3).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successful.
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    _ = kapps_call_command:answer(Call),
    CallflowId = kz_json:get_ne_binary_value(<<"callflow_id">>, Data),
    DisableUntil = case kz_json:get_ne_binary_value(<<"action">>, Data) of
                       <<"login">> -> ?ON_VAL;
                       <<"logout">> -> ?DEFAULT_OFF_VAL
                   end,
    AccountDb = kapps_call:account_db(Call),
    {'ok', Callflow} = kz_datamgr:open_cache_doc(AccountDb, CallflowId),
    UpdatedFlow = find_and_change_ring_group(kzd_callflows:flow(Callflow), Call, DisableUntil),
    save_callflow(AccountDb, kzd_callflows:set_flow(Callflow, UpdatedFlow)),
    cf_exe:continue(Call).

save_callflow(AccountDb, Callflow) ->
    save_callflow(AccountDb, Callflow, ?MAX_SAVE_RETRIES).
save_callflow(_AccountDb, Callflow, -1) ->
    lager:error("failed to update callflow to ~p", [kz_json:encode(Callflow)]);
save_callflow(AccountDb, Callflow, Retries) ->
    case kz_datamgr:save_doc(AccountDb, Callflow) of
        {'error', 'conflict'} ->
            save_callflow(AccountDb, Callflow, Retries);
        {'error', _} ->
            save_callflow(AccountDb, Callflow, Retries - 1);
        {'ok', _} ->
            lager:debug("updated flow to ~p", [kz_json:encode(Callflow)])
    end.

-spec find_and_change_ring_group(kz_json:object(), kapps_call:call(), integer()) -> kz_json:object().
find_and_change_ring_group(Action, Call, DisableUntil) ->
    NewAction = case kz_json:get_binary_value(<<"module">>, Action) of
                    <<"ring_group">> -> alter_endpoints(Action, Call, DisableUntil);
                    _ -> Action
                end,
    case kz_json:get_value(<<"children">>, NewAction) of
        'undefined' -> NewAction;
        Children -> kz_json:set_value(<<"children">>, maybe_modify_children(Children, Call, DisableUntil), NewAction)
    end.

-spec maybe_modify_children(kz_json:object(), kapps_call:call(), integer()) -> kz_json:object().
maybe_modify_children(Children, Call, DisableUntil) ->
    kz_json:foldl(fun(Branch, ChildAction, JObj) ->
                          SubModule = find_and_change_ring_group(ChildAction, Call, DisableUntil),
                          kz_json:set_value(Branch, SubModule, JObj)
                  end
                 ,Children
                 ,Children
                 ).

-spec alter_endpoints(kz_json:object(), kapps_call:call(), integer()) -> kz_json:object().
alter_endpoints(Module, Call, DisableUntil) ->
    EndpointsPath = [<<"data">>, <<"endpoints">>],
    OldEndpoints = kz_json:get_value(EndpointsPath, Module),
    {Found, EndPoints} = lists:foldl(fun(Endpoint, {Found, UpdatedEndpoints}) ->
                                             {ThisFound, Res} = maybe_alter_disable_until(kz_json:get_value(<<"endpoint_type">>, Endpoint), Endpoint, Call, DisableUntil),
                                             {Found or ThisFound, [Res | UpdatedEndpoints]}
                                     end, {'false', []}, OldEndpoints),
    Media = case {Found, DisableUntil} of
                {'false', _} ->
                    %% User not found
                    kapps_call:get_prompt(Call, <<"agent-invalid_choice">>);
                {'true', ?ON_VAL} ->
                    kapps_call:get_prompt(Call, <<"agent-logged_in">>);
                {'true', _} ->
                    kapps_call:get_prompt(Call, <<"agent-logged_out">>)
            end,
    _ = kapps_call_command:play(Media, Call),
    kz_json:set_value(EndpointsPath, EndPoints, Module).

-spec maybe_alter_disable_until(kz_term:ne_binary(), kz_json:object(), kapps_call:call(), integer()) -> {boolean(), kz_json:object()}.
maybe_alter_disable_until(<<"user">>, Endpoint, Call, DisableUntil) ->
    lager:debug("comparing owner ~p, to endpoint ~p", [kapps_call:owner_id(Call), Endpoint]),
    case kapps_call:owner_id(Call)
        =:= kz_json:get_ne_binary_value(<<"id">>, Endpoint)
    of
        'true' ->
            {'true', kz_json:set_value([<<"disable_until">>], DisableUntil, Endpoint)};
        'false' ->
            {'false', Endpoint}
    end;
maybe_alter_disable_until(_EndpointType, Endpoint, _Call, _DisableUntil) ->
    {'false', Endpoint}.
