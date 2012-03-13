%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_util).

-export([get_endpoints/2, log_agent_activity/3
         ,get_agents/2, get_agent_status/2
         ,fetch_queue_pid/3, store_queue_pid/4, erase_queue_pid/3
        ]).

-include("acdc.hrl").

%% TODO - remove need for callflows to be present in VM
get_endpoints(Call, UserId) ->
    lists:foldr(fun(EndpointId, Acc) ->
                        case cf_endpoint:build(EndpointId, Call) of
                            {ok, Endpoint} -> Endpoint ++ Acc;
                            {error, _E} -> Acc
                        end
                end, [], cf_attributes:fetch_owned_by(UserId, device, Call)).

log_agent_activity(Call, Action, AgentId) ->
    lager:debug("setting action for agent ~s to ~s", [AgentId, Action]),
    Doc = wh_json:from_list([{<<"call_id">>, whapps_call:call_id(Call)}
                             ,{<<"agent_id">>, AgentId}
                             ,{<<"action">>, Action}
                             ,{<<"pvt_type">>, <<"agent_activity">>}
                             ,{<<"pvt_created">>, wh_util:current_tstamp()}
                            ]),
    {ok, _} = couch_mgr:save_doc(whapps_call:account_db(Call), Doc).

get_agent_status(AcctDb, AgentId) ->
    case couch_mgr:get_results(AcctDb, <<"agents/agent_status">>, [{<<"startkey">>, [AgentId, wh_json:new()]}
                                                                   ,{<<"endkey">>, [AgentId, 0]}
                                                                   ,{<<"descending">>, true}
                                                                   ,{<<"limit">>, 1}
                                                                   ,{<<"reduce">>, false}
                                                                   ,{<<"include_docs">>, true}
                                                                  ]) of
        {ok, []} ->
            false;
        {ok, [StatusJObj]} ->
            wh_json:get_value([<<"doc">>, <<"action">>], StatusJObj);
        {error, _E} ->
            false
    end.

get_agents(AcctDb, QueueId) ->
    case couch_mgr:get_results(AcctDb, <<"agents/agent_listing">>, [{<<"key">>, QueueId}]) of
        {ok, []}=OK ->
            lager:debug("no agents assigned to ~s/~s", [AcctDb, QueueId]),
            OK;
        {ok, Agents} ->
            {ok, [wh_json:get_value(<<"id">>, Agent) || Agent <- Agents]};
        {error, _E}=Err ->
            lager:debug("error getting agents for ~s/~s", [AcctDb, QueueId]),
            Err
    end.

fetch_queue_pid(Cache, AcctDb, QueueId) ->
    wh_cache:fetch_local(Cache, queue_pid_cache_key(AcctDb, QueueId)).
store_queue_pid(Cache, AcctDb, QueueId, Pid) ->
    wh_cache:store_local(Cache, queue_pid_cache_key(AcctDb, QueueId), Pid, fun(_,V,_) -> gen_listener:stop(V) end).
erase_queue_pid(Cache, AcctDb, QueueId) ->
    wh_cache:erase_local(Cache, queue_pid_cache_key(AcctDb, QueueId)).

queue_pid_cache_key(AcctDb, QueueId) ->
    {?MODULE, AcctDb, QueueId}.
