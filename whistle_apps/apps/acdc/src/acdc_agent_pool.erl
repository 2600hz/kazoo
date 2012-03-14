%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_agent_pool).

-export([init/0, find_agent/2]).

-include("acdc.hrl").

init() ->
    lager:debug("finding all agents and starting workers"),
    [add_agents(AcctDb) || AcctDb <- whapps_util:get_all_accounts()].

-spec find_agent/2 :: (wh_json:json_object(), wh_proplist()) -> any().
find_agent(JObj, _Prop) ->
    wh_util:put_callid(JObj),

    lager:debug("new caller in a queue"),

    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, JObj)),
    QueueId = wh_json:get_value(<<"Queue-ID">>, JObj),

    lager:debug("caller in queue ~s", [QueueId]),

    find_agent(Call, whapps_call:account_db(Call), QueueId).

-spec find_agent/3 :: (whapps_call:call(), ne_binary(), ne_binary()) -> any().
find_agent(Call, AcctDb, QueueId) ->
    {ok, Queue} = acdc_util:find_queue(AcctDb, QueueId),
    find_agent(Call, AcctDb, QueueId, wh_json:get_integer_value(<<"connection_timeout">>, Queue, 300) * 1000).

-spec find_agent/4 :: (whapps_call:call(), ne_binary(), ne_binary(), pos_integer()) -> any().
find_agent(Call, AcctDb, QueueId, CallerTimeout) ->
    Start = erlang:now(),

    lager:debug("finding agent for ~s/~s in ~b timeout", [AcctDb, QueueId, CallerTimeout]),

    case poolboy:checkout(?MODULE, true, CallerTimeout) of
        Agent when is_pid(Agent) ->
            lager:debug("checking with agent ~p", [Agent]),
            case acdc_agent:maybe_handle_call(Agent, Call, AcctDb, QueueId, CallerTimeout) of
                false ->
                    lager:debug("agent isn't handling the call"),
                    poolboy:checkin(?MODULE, Agent),
                    timer:sleep(100),
                    find_agent(Call, AcctDb, QueueId, CallerTimeout - (timer:now_diff(erlang:now(), Start) div 1000));
                down ->
                    lager:debug("agent thinks the call is down, we're done"),
                    poolboy:checkin(?MODULE, Agent);
                true ->
                    lager:debug("agent handled the call"),
                    poolboy:checkin(?MODULE, Agent)
            end;
        _Other ->
            lager:debug("checked out ~p instead of agent", [_Other]),
            find_agent(Call, AcctDb, QueueId, CallerTimeout - (timer:now_diff(erlang:now(), Start) div 1000))
    end.

add_agents(AcctDb) ->
    case couch_mgr:get_results(AcctDb, <<"agents/crossbar_listing">>, []) of
        {ok, []} ->
            lager:debug("no agents in ~s", [AcctDb]);
        {ok, As} ->
            lager:debug("found agents for ~s", [AcctDb]),
            [start_worker(AcctDb, A) || A <- As];
        {error, _E} ->
            lager:debug("error finding agents in ~s", [AcctDb])
    end.

start_worker(AcctDb, Agent) ->
    AgentId = wh_json:get_value(<<"id">>, Agent),
    AgentInfo = wh_json:get_value(<<"value">>, Agent),
    lager:debug("adding agent worker ~s", [AgentId]),
    poolboy:add_worker(?MODULE, fun(Worker) ->
                                        acdc_agent:update_agent(Worker, {AcctDb, AgentId, AgentInfo}),
                                        {ok, Worker}
                                end).
