%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(acdc_agent_pool).

-export([init/0]).
-export([new_member/2]).
-export([update_agent/2]).

-include("acdc.hrl").

init() ->
    lager:debug("finding all agents and starting workers"),
    _ = [add_agents(AcctDb) || AcctDb <- whapps_util:get_all_accounts()],
    acdc_agents:reload_agents().

update_agent(JObj, _Prop) ->    
    wh_util:put_callid(JObj),
    lager:debug("recv agent update for: ~p", [wh_json:get_value(<<"doc">>, JObj)]).

-spec new_member/2 :: (wh_json:json_object(), wh_proplist()) -> any().
new_member(JObj, _Prop) ->
    wh_util:put_callid(JObj),
    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, JObj)),
    QueueId = wh_json:get_value(<<"Queue-ID">>, JObj),
    lager:debug("caller in queue ~s", [QueueId]),
    find_queue(Call, QueueId, wh_json:get_value(<<"Server-ID">>, JObj)).

-spec find_queue/3 :: (whapps_call:call(), ne_binary(), ne_binary()) -> 'ok' | {'error', term()}.
find_queue(Call, QueueId, ServerId) ->
    AccountDb = whapps_call:account_db(Call),
    case acdc_util:find_queue(AccountDb, QueueId) of
        {error, _Reason} ->
            lager:debug("unable to find ACD queue ~s/~s: ~p", [AccountDb, QueueId, _Reason]),
            CallId = whapps_call:is_call(Call) andalso whapps_call:call_id(Call),
            Result = [{<<"Call-ID">>, CallId}
                      ,{<<"Result">>, <<"FAULT">>}
                      | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                     ],
            wapi_queue:publish_result(ServerId, Result);
        {ok, Queue} ->
            ConnectionTimeout = wh_json:get_integer_value(<<"connection_timeout">>, Queue, 300) * 1000,
            acdc_agent:maybe_handle_call(Call, Queue, ServerId, ConnectionTimeout)
    end.

-spec add_agents/1 :: (ne_binary()) -> 'ok'.
add_agents(AccountDb) ->
    case couch_mgr:get_results(AccountDb, <<"agents/crossbar_listing">>, []) of
        {ok, []} ->
            lager:debug("no agents in ~s", [AccountDb]);
        {ok, As} ->
            lager:debug("found agents for ~s", [AccountDb]),
            [start_worker(AccountDb, A) || A <- As];
        {error, _E} ->
            lager:debug("error finding agents in ~s", [AccountDb])
    end.

-spec start_worker/2 :: (ne_binary(), wh_json:json_object()) -> sup_startchild_ret().
start_worker(AccountDb, Agent) ->
    AgentId = wh_json:get_value(<<"id">>, Agent),
    Queues =  wh_json:get_value([<<"value">>, <<"queues">>], Agent, []),
    lager:debug("adding agent worker ~s", [AgentId]),
    acdc_agent_sup:new(AccountDb, AgentId, Queues).
