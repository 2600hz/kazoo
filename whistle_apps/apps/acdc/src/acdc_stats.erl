%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, 2600Hz
%%% @doc
%%% Collector of stats
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_stats).

-behaviour(gen_listener).

%% Stats API
-export([call_processed/5
         ,call_abandoned/4
         ,call_missed/4
         ,call_handled/4
         ,call_waiting/3

         %% Agent-specific stats
         ,agent_active/2
         ,agent_inactive/2
         ,agent_paused/3
         ,agent_resume/2
         ,agent_wrapup/3
         ,agent_ready/2
         ,agent_oncall/3

         ,init_db/1
         ,db_name/1
        ]).

%% gen_listener functions
-export([start_link/0
         ,init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("acdc.hrl").

-type call_status() :: 'waiting'    % caller is waiting for an agent
                     | 'handling'   % caller is being handled by an agent (still active)
                     | 'finished'   % caller has finished (probably by another node)
                     | 'processed'  % caller has finished, handled by us
                     | 'missed'     % agent missed the call
                     | 'abandoned'. % caller left the queue (hangup or dtmf exit)

-record(call_stat, {
          call_id :: ne_binary()
          ,acct_id :: ne_binary()
          ,queue_id :: ne_binary()
          ,agent_id :: ne_binary() % the handling agent
          ,timestamp = wh_util:current_tstamp() :: integer()
          ,wait_time :: integer() % how long was caller on hold
          ,talk_time :: integer() % how long was agent connected to caller
          ,abandon_reason :: abandon_reason()
          ,status :: call_status()
         }).
-type call_stat() :: #call_stat{}.

-record(agent_stat, {
          agent_id :: ne_binary()
          ,acct_id :: ne_binary()
          ,timestamp = wh_util:current_tstamp() :: integer()
          ,status :: 'ready' | 'logged_out' | 'paused' | 'wrapup' | 'busy'
          ,wait_time :: integer()
          ,call_id :: ne_binary()
         }).
-type agent_stat() :: #agent_stat{}.

-type stat() :: call_stat() | agent_stat().

%% An agent connected with a caller
-spec call_processed/5 :: (ne_binary(), ne_binary()
                           ,ne_binary(), ne_binary()
                           ,integer()
                           ) -> 'ok'.
call_processed(AcctId, QueueId, AgentId, CallId, Elapsed) ->
    gen_listener:cast(?MODULE, {store, #call_stat{acct_id=AcctId
                                                  ,queue_id=QueueId
                                                  ,agent_id=AgentId
                                                  ,call_id=CallId
                                                  ,status='processed'
                                                  ,talk_time=Elapsed
                                                 }
                               }).

%% Caller left the queue
-spec call_abandoned/4 :: (ne_binary(), ne_binary()
                           ,ne_binary(), abandon_reason()
                          ) -> 'ok'.
call_abandoned(AcctId, QueueId, CallId, Reason) ->
    gen_listener:cast(?MODULE, {store, #call_stat{acct_id=AcctId
                                                  ,queue_id=QueueId
                                                  ,call_id=CallId
                                                  ,abandon_reason=Reason
                                                  ,status='abandoned'
                                                 }
                               }).

%% Agent was rung for a call, and failed to pickup in time
-spec call_missed/4 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
call_missed(AcctId, QueueId, AgentId, CallId) ->
    gen_listener:cast(?MODULE, {store, #call_stat{acct_id=AcctId
                                                  ,queue_id=QueueId
                                                  ,call_id=CallId
                                                  ,agent_id=AgentId
                                                  ,status='missed'
                                                 }
                               }).

%% Call was picked up by an agent, track how long caller was in queue
-spec call_handled/4 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
call_handled(AcctId, QueueId, CallId, AgentId) ->
    gen_listener:cast(?MODULE, {store, #call_stat{acct_id=AcctId
                                                  ,queue_id=QueueId
                                                  ,agent_id=AgentId
                                                  ,call_id=CallId
                                                  ,status='handling'
                                                 }
                               }).

%% Call was placed in the AMQP Queue
-spec call_waiting/3 :: (ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
call_waiting(AcctId, QueueId, CallId) ->
    gen_listener:cast(?MODULE, {store, #call_stat{
                                  acct_id=AcctId
                                  ,queue_id=QueueId
                                  ,call_id=CallId
                                  ,status='waiting'
                                 }
                               }).

%% marks an agent as active for an account
-spec agent_active/2 :: (ne_binary(), ne_binary()) -> 'ok'.
agent_active(AcctId, AgentId) ->
    gen_listener:cast(?MODULE, {store, #agent_stat{acct_id=AcctId
                                                   ,agent_id=AgentId
                                                   ,status='ready'
                                                  }
                               }).

%% marks an agent as inactive for an account
-spec agent_inactive/2 :: (ne_binary(), ne_binary()) -> 'ok'.
agent_inactive(AcctId, AgentId) ->
    gen_listener:cast(?MODULE, {store, #agent_stat{acct_id=AcctId
                                                   ,agent_id=AgentId
                                                   ,status='logout'
                                                  }
                               }).

%% marks an agent as back from break, effectively removing the waiting stat
-spec agent_resume/2 :: (ne_binary(), ne_binary()) -> 'ok'.
agent_resume(AcctId, AgentId) ->
    gen_listener:cast(?MODULE, {store, #agent_stat{acct_id=AcctId
                                                   ,agent_id=AgentId
                                                   ,status='ready'
                                                  }
                               }).

%% marks an agent as paused for an account
-spec agent_paused/3 :: (ne_binary(), ne_binary(), integer()) -> 'ok'.
agent_paused(AcctId, AgentId, Timeout) ->
    gen_listener:cast(?MODULE, {store, #agent_stat{acct_id=AcctId
                                                   ,agent_id=AgentId
                                                   ,status='paused'
                                                   ,wait_time=Timeout
                                                  }
                               }).

-spec agent_wrapup/3 :: (ne_binary(), ne_binary(), integer()) -> 'ok'.
agent_wrapup(AcctId, AgentId, Timeout) ->
    gen_listener:cast(?MODULE, {store, #agent_stat{acct_id=AcctId
                                                   ,agent_id=AgentId
                                                   ,status='wrapup'
                                                   ,wait_time=Timeout
                                                  }
                               }).

-spec agent_oncall/3 :: (ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
agent_oncall(AcctId, AgentId, CallId) ->
    gen_listener:cast(?MODULE, {store, #agent_stat{acct_id=AcctId
                                                   ,agent_id=AgentId
                                                   ,status='busy'
                                                   ,call_id=CallId
                                                  }
                               }).

-spec agent_ready/2 :: (ne_binary(), ne_binary()) -> 'ok'.
agent_ready(AcctId, AgentId) ->
    gen_listener:cast(?MODULE, {store, #agent_stat{acct_id=AcctId
                                                   ,agent_id=AgentId
                                                   ,status='ready'
                                                  }
                               }).

-define(BINDINGS, [{self, []}]).
-define(RESPONDERS, []).
start_link() ->
    gen_listener:start_link({local, ?MODULE}
                            ,?MODULE
                            ,[{bindings, ?BINDINGS}
                              ,{responders, ?RESPONDERS}
                             ],
                            []).

init([]) ->
    put(callid, <<"acdc.stats">>),
    lager:debug("started new acdc stats collector"),
    {ok, ok}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast({store, Stat}, State) ->
    store_stat(Stat),
    {noreply, State};
handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    lager:debug("unhandling message: ~p", [_Msg]),
    {noreply, State}.

handle_event(_JObj, _State) ->
    {reply, []}.

terminate(_Reason, _) ->
    lager:debug("acdc stats terminating: ~p", [_Reason]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

store_stat(#call_stat{acct_id=AcctId}=Stat) ->
    JObj = stat_to_jobj(Stat),
    store_stat(AcctId, JObj);
store_stat(#agent_stat{acct_id=AcctId}=Stat) ->
    JObj = stat_to_jobj(Stat),
    store_stat(AcctId, JObj).

store_stat(AcctId, JObj) ->
    case couch_mgr:save_doc(db_name(AcctId), JObj) of
        {ok, _} -> ok;
        {error, _E} ->
            lager:debug("error saving: ~p", [_E]),
            timer:sleep(250),
            store_stat(AcctId, JObj)
    end.

-spec stat_to_jobj/1 :: (stat()) -> wh_json:object().
stat_to_jobj(#call_stat{
                call_id=CallId
                ,acct_id=AcctId
                ,queue_id=QueueId
                ,agent_id=AgentId
                ,timestamp=TStamp
                ,wait_time=WaitTime
                ,talk_time=TalkTime
                ,abandon_reason=AR
                ,status=Status
               }) ->
    wh_json:from_list(
      props:filter_undefined(
        [{<<"call_id">>, CallId}
         ,{<<"account_id">>, AcctId}
         ,{<<"queue_id">>, QueueId}
         ,{<<"agent_id">>, AgentId}
         ,{<<"timestamp">>, TStamp}
         ,{<<"wait_time">>, WaitTime}
         ,{<<"talk_time">>, TalkTime}
         ,{<<"abondon_reason">>, AR}
         ,{<<"status">>, Status}
         ,{<<"type">>, <<"call_partial">>}
        ]));
stat_to_jobj(#agent_stat{
                agent_id=AgentId
                ,acct_id=AcctId
                ,timestamp=TStamp
                ,status=Status
                ,wait_time=WaitTime
                ,call_id=CallId
               }) ->
    wh_json:from_list(
      props:filter_undefined(
        [{<<"account_id">>, AcctId}
         ,{<<"agent_id">>, AgentId}
         ,{<<"timestamp">>, TStamp}
         ,{<<"wait_time">>, WaitTime}
         ,{<<"status">>, Status}
         ,{<<"type">>, <<"agent_partial">>}
         ,{<<"call_id">>, CallId}
        ])).

init_db(AcctId) ->
    DbName = db_name(AcctId),
    lager:debug("created db ~s: ~s", [DbName, couch_mgr:db_create(DbName)]),
    lager:debug("revised docs: ~p", [couch_mgr:revise_views_from_folder(DbName, acdc)]),
    ok.

db_name(Acct) ->
    <<A:2/binary, B:2/binary, Rest/binary>> = wh_util:format_account_id(Acct, raw),
    <<"acdc%2F",A/binary,"%2F",B/binary,"%2F", Rest/binary>>.
