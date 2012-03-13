%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_agent_pool).

-export([find_agent/2]).

-include("acdc.hrl").

-spec find_agent/2 :: (wh_json:json_object(), wh_proplist()) -> any().
find_agent(JObj, _Prop) ->
    wh_json:put_callid(JObj),
    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, JObj)),
    QueueId = wh_json:get_value(<<"Queue-ID">>, JObj),

    find_agent(Call, whapps_call:account_db(Call), QueueId).

-spec find_agent/3 :: (whapps_call:call(), ne_binary(), ne_binary()) -> any().
find_agent(Call, AcctDb, QueueId) ->
    {ok, Queue} = acdc_util:get_queue(AcctDb, QueueId),
    find_agent(Call, AcctDb, QueueId, wh_json:get_integer_value(<<"connection_timeout">>, Queue, 300) * 1000).

-spec find_agent/4 :: (whapps_call:call(), ne_binary(), ne_binary(), pos_integer()) -> any().
find_agent(Call, AcctDb, QueueId, CallerTimeout) ->
    Start = erlang:now(),

    case poolboy:checkout(?MODULE, true, CallerTimeout) of
        Agent when is_pid(Agent) ->
            lager:debug("checking with agent ~p", [Agent]),
            case acdc_agent:maybe_handle_call(Agent, Call, AcctDb, QueueId) of
                false ->
                    poolboy:checkin(?MODULE, Agent),
                    find_agent(Call, AcctDb, QueueId);
                true ->
                    poolboy:checkin(?MODULE, Agent)
            end;
        _Other ->
            lager:debug("checked out ~p instead of agent", [_Other]),
            find_agent(Call, AcctDb, QueueId, CallerTimeout - (timer:now_diff(erlang:now(), Start) div 1000))
    end.
