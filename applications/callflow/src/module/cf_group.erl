%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_group).

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
    case get_endpoints(wh_json:get_value(<<"endpoints">>, Data, []), Call) of
        [] ->
            lager:notice("group has no endpoints, moving to next callflow element"),
            cf_exe:continue(Call);
        Endpoints -> attempt_endpoints(Endpoints, Data, Call)
    end.

-spec attempt_endpoints(wh_json:objects(), wh_json:object(), whapps_call:call()) -> 'ok'.
attempt_endpoints(Endpoints, Data, Call) ->
    Timeout = wh_json:get_integer_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT_S),
    Strategy = wh_json:get_binary_value(<<"strategy">>, Data, <<"simultaneous">>),
    Ringback = wh_json:get_value(<<"ringback">>, Data),
    lager:info("attempting group of ~b members with strategy ~s", [length(Endpoints), Strategy]),
    case whapps_call_command:b_bridge(Endpoints, Timeout, Strategy, <<"true">>, Ringback, Call) of
        {'ok', _} ->
            lager:info("completed successful bridge to the group - call finished normally"),
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

-spec get_endpoints(wh_json:objects(), whapps_call:call()) -> wh_json:objects().
get_endpoints(Members, Call) ->
    S = self(),
    Builders = [spawn(fun() ->
                              put('callid', whapps_call:call_id(Call)),
                              S ! {self(), catch cf_endpoint:build(EndpointId, Member, Call)}
                      end)
                || {EndpointId, Member} <- resolve_endpoint_ids(Members, Call)
               ],
    lists:foldl(fun(Pid, Acc) ->
                        receive
                            {Pid, {'ok', EP}} when is_list(EP) -> EP ++ Acc;
                            {Pid, {'ok', EP}} -> [EP | Acc];
                            {Pid, _} -> Acc
                        end
                end, [], Builders).

-spec resolve_endpoint_ids(wh_json:objects(), whapps_call:call()) -> wh_proplist().
resolve_endpoint_ids(Members, Call) ->
    [{Id, wh_json:set_value(<<"source">>, ?MODULE, Member)}
     || {Type, Id, Member} <- resolve_endpoint_ids(Members, [], Call)
            ,Type =:= <<"device">>
            ,Id =/= whapps_call:authorizing_id(Call)
    ].

-type endpoint_intermediate() :: {ne_binary(), ne_binary(), api_object()}.
-type endpoint_intermediates() :: [] | [endpoint_intermediate(),...].
-spec resolve_endpoint_ids(wh_json:objects(), endpoint_intermediates(), whapps_call:call()) ->
                                  endpoint_intermediates().
resolve_endpoint_ids([], EndpointIds, _) -> EndpointIds;
resolve_endpoint_ids([Member|Members], EndpointIds, Call) ->
    Id = wh_json:get_value(<<"id">>, Member),
    Type = wh_json:get_value(<<"endpoint_type">>, Member, <<"device">>),
    case wh_util:is_empty(Id)
        orelse lists:keymember(Id, 2, EndpointIds)
        orelse Type
    of
        'true' -> resolve_endpoint_ids(Members, EndpointIds, Call);
        <<"group">> ->
            lager:info("member ~s is a group, merge the group's members", [Id]),
            GroupMembers = get_group_members(Member, Id, Call),
            Ids = resolve_endpoint_ids(GroupMembers, EndpointIds, Call),
            resolve_endpoint_ids(Members, [{Type, Id, 'undefined'}|Ids], Call);
        <<"user">> ->
            lager:info("member ~s is a user, get all the user's endpoints", [Id]),
            Ids = lists:foldr(fun(EndpointId, Acc) ->
                                      case lists:keymember(EndpointId, 2, Acc) of
                                          'true' -> Acc;
                                          'false' ->
                                              [{<<"device">>, EndpointId, Member}|Acc]
                                      end
                              end
                              ,[{Type, Id, 'undefined'} | EndpointIds]
                              ,cf_attributes:owned_by(Id, <<"device">>, Call)),
            resolve_endpoint_ids(Members, Ids, Call);
        <<"device">> ->
            resolve_endpoint_ids(Members, [{Type, Id, Member}|EndpointIds], Call)
    end.

-spec get_group_members(wh_json:object(), ne_binary(), whapps_call:call()) ->
                               wh_json:objects().
get_group_members(Member, Id, Call) ->
    AccountDb = whapps_call:account_db(Call),
    case couch_mgr:open_cache_doc(AccountDb, Id) of
        {'ok', JObj} ->
            Endpoints = wh_json:get_ne_value(<<"endpoints">>, JObj, wh_json:new()),
            DefaultDelay = wh_json:get_value(<<"delay">>, Member),
            DefaultTimeout = wh_json:get_value(<<"timeout">>, Member),
            [wh_json:set_values([{<<"endpoint_type">>, wh_json:get_value([Key, <<"type">>], Endpoints)}
                                 ,{<<"id">>, Key}
                                 ,{<<"delay">>, wh_json:get_value([Key, <<"delay">>], Endpoints, DefaultDelay)}
                                 ,{<<"timeout">>, wh_json:get_value([Key, <<"timeout">>], Endpoints, DefaultTimeout)}
                                ], Member)
             || Key <- wh_json:get_keys(Endpoints)
            ];
        {'error', _R} ->
            lager:warning("unable to lookup members of group ~s: ~p", [Id, _R]),
            []
    end.
