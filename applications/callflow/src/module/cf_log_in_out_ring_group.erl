%%%-------------------------------------------------------------------
%%% @doc
%%% Logs a user in or out of a ring group
%%% @end
%%% @contributors
%%%   Max Lay
%%%-------------------------------------------------------------------
-module(cf_log_in_out_ring_group).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

-define(ON_VAL, 0).
-define(DEFAULT_OFF_VAL, 66269664000).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    _ = kapps_call_command:answer(Call),
    CallflowId = kz_json:get_value(<<"callflow_id">>, Data),
    DisableUntil = case kz_json:is_true(<<"login">>, Data) of
                       'true' -> ?ON_VAL;
                       'false' -> ?DEFAULT_OFF_VAL
                   end,
    AccountDb = kapps_call:account_db(Call),
    {'ok', Callflow} = kz_datamgr:open_doc(AccountDb, CallflowId),
    UpdatedFlow = find_and_change_ring_group(kz_json:get_value(<<"flow">>, Callflow), Call, DisableUntil),
    kz_datamgr:save_doc(AccountDb, kz_json:set_value(<<"flow">>, UpdatedFlow, Callflow)),
    lager:debug("updated flow from ~p to ~p", [kz_json:encode(Callflow), kz_json:encode(UpdatedFlow)]),
    cf_exe:continue(Call).

-spec find_and_change_ring_group(kz_json:object(), kapps_call:call(), integer()) -> kz_json:object().
find_and_change_ring_group(Module, Call, DisableUntil) ->
    NewModule = case kz_json:get_value(<<"module">>, Module) of
                    <<"ring_group">> -> alter_endpoints(Module, Call, DisableUntil);
                    _ -> Module
                end,
    case kz_json:get_value(<<"children">>, NewModule) of
        'undefined' -> NewModule;
        Children -> kz_json:set_value(<<"children">>, maybe_modify_children(Children, Call, DisableUntil), NewModule)
    end.

-spec maybe_modify_children(kz_json:object(), kapps_call:call(), integer()) -> kz_json:object().
maybe_modify_children(Children, Call, DisableUntil) ->
    lists:foldl(fun(Key, JObj) ->
                        SubModule = find_and_change_ring_group(kz_json:get_value(Key, JObj), Call, DisableUntil),
                        kz_json:set_value(Key, SubModule, JObj)
                end, Children, kz_json:get_keys(Children)).

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
                    kz_media_util:get_prompt(<<"agent-invalid_choice">>, Call);
                {'true', ?ON_VAL} ->
                    kz_media_util:get_prompt(<<"agent-logged_in">>, Call);
                {'true', _} ->
                    kz_media_util:get_prompt(<<"agent-logged_out">>, Call)
            end,
    kapps_call_command:play(Media, Call),
    kz_json:set_value(EndpointsPath, EndPoints, Module).

-spec maybe_alter_disable_until(ne_binary(), kz_json:object(), kapps_call:call(), integer()) -> {boolean(), kz_json:object()}.
maybe_alter_disable_until(<<"user">>, Endpoint, Call, DisableUntil) ->
    lager:debug("comparing owner ~p, to endpoint ~p", [kapps_call:owner_id(Call), Endpoint]),
    case kapps_call:owner_id(Call) =:= kz_json:get_value(<<"id">>, Endpoint) of
        'true' ->
            {'true', kz_json:set_value([<<"disable_until">>], DisableUntil, Endpoint)};
        'false' ->
            {'false', Endpoint}
    end;

maybe_alter_disable_until(_EndpointType, Endpoint, _Call, _DisableUntil) ->
    {'false', Endpoint}.
