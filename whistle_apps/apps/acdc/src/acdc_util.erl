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
         ,find_queue/2
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

find_queue(AcctDb, QueueId) ->
    {ok, Cache} = acdc_sup:cache_proc(),
    find_queue(AcctDb, QueueId, Cache).
find_queue(AcctDb, QueueId, Cache) ->
    case wh_cache:fetch_local(Cache, queue_cache_key(AcctDb, QueueId)) of
        {ok, _QueueJObj}=OK -> OK;
        {error, not_found} ->
            case couch_mgr:open_doc(AcctDb, QueueId) of
                {ok, JObj}=OK ->
                    wh_cache:store_local(Cache, queue_cache_key(AcctDb, QueueId), JObj),
                    OK;
                E -> E
            end
    end.

queue_cache_key(AcctDb, QueueId) ->
    {?MODULE, AcctDb, QueueId}.
