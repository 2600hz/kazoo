%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(cf_call_group).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    GroupId = wh_json:get_value(<<"id">>, Data),
    AllEndpointIds = get_endpoint_ids(GroupId, Call),
    Targets = lists:filter(fun (X) -> X =/= whapps_call:authorizing_id(Call) end, AllEndpointIds),
    Timeout = wh_json:get_integer_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT_S),
    Endpoints = make_endpoints(Targets, Timeout, Call),
    Strategy = wh_json:get_binary_value(<<"strategy">>, Data, <<"simultaneous">>),
    Ringback = wh_json:get_value(<<"ringback">>, Data),
    Count = length(Endpoints),
    lager:info("attempting ring group of ~b members with strategy ~s", [Count, Strategy]),
    CallTimout = case Strategy of
                     <<"single">> -> Timeout * Count;
                     _ -> Timeout
                 end,
    case whapps_call_command:b_bridge(Endpoints, CallTimout, Strategy, <<"true">>, Ringback, Call) of
        {'ok', _} ->
            lager:info("completed successful bridge to the ring group - call finished normally"),
            cf_exe:stop(Call);
        {'fail', _}=F ->
            case cf_util:handle_bridge_failure(F, Call) of
                'ok' -> lager:debug("bridge failure handled");
                'not_found' -> cf_exe:continue(Call)
            end;
        {'error', _R} ->
            lager:info("error bridging to group: ~p"
                       ,[wh_json:get_value(<<"Error-Message">>, _R)]
                      ),
            cf_exe:continue(Call)
    end.

-spec get_endpoint_ids(ne_binary() | 'undefined', whapps_call:call()) -> ne_binaries().
get_endpoint_ids('undefined', _) ->
    lager:debug("Cant discover endpoints for unknown group"),
    [];
get_endpoint_ids(GroupId, Call) ->
    AccountDb = whapps_call:account_db(Call),
    case couch_mgr:open_cache_doc(AccountDb, GroupId) of
        {'ok', JDoc} ->
            Members = wh_json:to_proplist(wh_json:get_value(<<"endpoints">>, JDoc)),
            lager:info("Members: ~p", [Members]),
            {Users, Others} = lists:partition(fun is_user/1, Members),
            {Groups, Devices} = lists:partition(fun is_group/1, Others),
            DeviceIds = get_ids(Devices),
            UserIds = get_ids(Users),
            GroupIds = get_ids(Groups),
            lager:info("Device ids: ~p", [DeviceIds]),
            lager:info("User ids: ~p", [UserIds]),
            lager:info("group ids: ~p", [GroupIds]),
            UserDeviceIds = cf_attributes:owned_by(UserIds, <<"device">>, Call),
            DeviceIds ++ UserDeviceIds ++ lists:foldl(fun (X, Acc) -> get_endpoint_ids(X, Call) ++ Acc end, [], GroupIds);
        _ -> []
    end.

-type member() :: {ne_binary(), ne_binary()}.

-spec is_user(member()) -> boolean().
is_user({_, Desc}) ->
    wh_json:get_value(<<"type">>, Desc) =:= <<"user">>.

-spec is_group(member()) -> boolean().
is_group({_, Desc}) ->
    wh_json:get_value(<<"type">>, Desc) =:= <<"group">>.

-spec get_ids(list()) -> list().
get_ids(List) ->
    lists:foldl(fun ({Id, _}, Acc) -> [Id | Acc] end, [], List).

-spec make_endpoints(ne_binaries(), integer(), whapps_call:call()) -> wh_json:objects().
make_endpoints(Targets, Timeout, Call) ->
    Props = wh_json:from_list([{<<"timeout">>, Timeout}]),
    lists:flatmap(fun (Id) -> case cf_endpoint:build(Id, Props, Call) of
                                  {'ok', EPs} -> EPs;
                                  _ ->
                                      lager:info("Dropping id ~s", [Id]),
                                      []
                              end
                  end, Targets).
