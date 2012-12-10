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

%% Query API
-export([acct_stats/1
         ,queue_stats/1, queue_stats/2
         ,agent_stats/1, agent_stats/2
         ,current_calls/1, current_calls/2
        ]).

%% Stats API
-export([call_processed/5
         ,call_abandoned/4
         ,call_missed/4
         ,call_handled/4
         ,call_waiting/3
         ,call_finished/1

         %% Agent-specific stats
         ,agent_active/2
         ,agent_inactive/2
         ,agent_paused/3
         ,agent_resume/2
         ,agent_wrapup/3
         ,agent_ready/2
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

%% Internal functions
-export([write_to_dbs/1
         ,flush_table/0
        ]).

-include("acdc.hrl").

-define(ETS_TABLE, ?MODULE).

-type call_status() :: 'waiting'    % caller is waiting for an agent
                     | 'handling'   % caller is being handled by an agent (still active)
                     | 'finished'   % caller has finished (probably by another node)
                     | 'processed'  % caller has finished, handled by us
                     | 'abandoned'. % caller left the queue (hangup or dtmf exit)

-record(call_stat, {
          call_id :: ne_binary() | '$1' | '_'
          ,acct_id :: ne_binary() | '$1' | '_'
          ,queue_id :: ne_binary() | '$2' | '_'
          ,agent_id :: ne_binary() | '$2' | '_' % the handling agent
          ,agents_rung = [] :: [ne_binary(),...] | [] | '_' % agents rung trying to handle the call
          ,started :: integer() | '_'
          ,wait_time :: integer() | '_' % how long was caller on hold
          ,talk_time :: integer() | '_' % how long was agent connected to caller
          ,abandon_reason :: abandon_reason() | '_'
          ,status :: call_status() | '_' | '$3'
         }).
-type call_stat() :: #call_stat{}.

-record(agent_stat, {
          agent_id :: ne_binary() | '_' | '$2'
          ,acct_id :: ne_binary() | '$1' | '_'
          ,timestamp :: integer() | '_'
          ,status :: 'ready' | 'logged_out' | 'paused' | 'wrapup' | '_'
          ,wait_time :: integer() | '_'
         }).
-type agent_stat() :: #agent_stat{}.

-type stat() :: call_stat() | agent_stat().
-type stats() :: [stat(),...] | [].

-spec acct_stats/1 :: (ne_binary()) -> wh_json:json_objects().
acct_stats(AcctId) ->
    MatchSpec = [{#agent_stat{acct_id='$1', _ = '_'}
                  ,[{'=:=', '$1', AcctId}]
                  ,['$_']
                 }],
    AcctDocs = lists:foldl(fun(Stat, AcctAcc) ->
                                   update_stat(AcctAcc, Stat)
                           end, dict:new(), ets:select(?ETS_TABLE, MatchSpec)
                          ),
    wh_doc:public_fields(fetch_acct_doc(AcctId, AcctDocs)).

queue_stats(AcctId) ->
    MatchSpec = [{#call_stat{acct_id='$1', queue_id='$2', _='_'}
                  ,[{'=:=', '$1', AcctId}
                    ,{'=/=', '$2', 'undefined'}
                   ]
                  ,['$_']
                 }],
    AcctDocs = lists:foldl(fun(Stat, AcctAcc) ->
                                   update_stat(AcctAcc, Stat)
                           end, dict:new(), ets:select(?ETS_TABLE, MatchSpec)
                          ),
    wh_json:get_value([<<"queues">>], wh_doc:public_fields(fetch_acct_doc(AcctId, AcctDocs))).

queue_stats(AcctId, QueueId) ->
    MatchSpec = [{#call_stat{acct_id='$1', queue_id='$2', _='_'}
                  ,[{'=:=', '$1', AcctId}
                    ,{'=:=', '$2', QueueId}
                   ]
                  ,['$_']
                 }],
    AcctDocs = lists:foldl(fun(Stat, AcctAcc) ->
                                   update_stat(AcctAcc, Stat)
                           end, dict:new(), ets:select(?ETS_TABLE, MatchSpec)
                          ),
    wh_json:get_value([<<"queues">>, QueueId], wh_doc:public_fields(fetch_acct_doc(AcctId, AcctDocs))).

agent_stats(AcctId) ->
    MatchSpec = [{#agent_stat{acct_id='$1', agent_id='$2', _='_'}
                  ,[{'=:=', '$1', AcctId}
                    ,{'=/=', '$2', 'undefined'}
                   ]
                  ,['$_']
                 }],

    AcctDocs = lists:foldl(fun(Stat, AcctAcc) ->
                                   update_stat(AcctAcc, Stat)
                           end, dict:new(), ets:select(?ETS_TABLE, MatchSpec)
                          ),
    wh_json:get_value(<<"agents">>, wh_doc:public_fields(fetch_acct_doc(AcctId, AcctDocs))).

agent_stats(AcctId, AgentId) ->
    MatchSpec = [{#agent_stat{acct_id='$1', agent_id='$2', _='_'}
                  ,[{'=:=', '$1', AcctId}
                    ,{'=:=', '$2', AgentId}
                   ]
                  ,['$_']
                 }],

    AcctDocs = lists:foldl(fun(Stat, AcctAcc) ->
                                   update_stat(AcctAcc, Stat)
                           end, dict:new(), ets:select(?ETS_TABLE, MatchSpec)
                          ),
    wh_json:get_value([<<"agents">>, AgentId], wh_doc:public_fields(fetch_acct_doc(AcctId, AcctDocs))).

-spec current_calls/1 :: (ne_binary()) -> wh_json:json_object().
-spec current_calls/2 :: (ne_binary(), ne_binary() | '_') -> wh_json:json_object().
current_calls(AcctId) ->
    current_calls(AcctId, '_').
current_calls(AcctId, QueueId) ->
    MatchSpec = [{#call_stat{acct_id='$1'
                             ,queue_id='$2'
                             ,status='$3'
                             ,_='_'
                            }
                  ,[{'=:=', '$1', AcctId}
                    ,{'=:=', '$2', QueueId}
                    ,{'=:=', '$3', 'waiting'}
                   ]
                  ,['$_']
                 }],
    AcctDocs = lists:foldl(fun(Stat, AcctAcc) ->
                                   update_stat(AcctAcc, Stat)
                           end, dict:new(), ets:select(?ETS_TABLE, MatchSpec)
                          ),
    current_calls_resp(fetch_acct_doc(AcctId, AcctDocs), QueueId).

current_calls_resp(AcctDoc, '_') ->
    wh_json:get_value(<<"queues">>, wh_doc:public_fields(AcctDoc), wh_json:new());
current_calls_resp(AcctDoc, QueueId) ->
    wh_json:get_value([<<"queues">>, QueueId, <<"calls">>], wh_doc:public_fields(AcctDoc), wh_json:new()).

-spec find_call_stat/1 :: (ne_binary()) -> call_stat().
find_call_stat(CallId) ->
    MatchSpec = [{#call_stat{call_id='$1'
                             ,_='_'
                            }
                  ,[{'=:=', '$1', CallId}
                   ]
                  ,['$_']
                 }],
    case ets:select(?ETS_TABLE, MatchSpec) of
        [] -> #call_stat{call_id=CallId};
        [#call_stat{}=Stat]-> Stat
    end.

%% An agent connected with a caller
-spec call_processed/5 :: (ne_binary(), ne_binary()
                           ,ne_binary(), ne_binary()
                           ,integer()
                           ) -> 'ok'.
call_processed(AcctId, QueueId, AgentId, CallId, Elapsed) ->
    CallStat = find_call_stat(CallId),
    gen_listener:cast(?MODULE, {store, CallStat#call_stat{acct_id=AcctId
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
    #call_stat{started=Started
              ,agents_rung=As
              }=CallStat = find_call_stat(CallId),

    gen_listener:cast(?MODULE, {store, CallStat#call_stat{acct_id=AcctId
                                                          ,queue_id=QueueId
                                                          ,call_id=CallId
                                                          ,abandon_reason=Reason
                                                          ,agents_rung=lists:reverse(As)
                                                          ,status='abandoned'
                                                          ,wait_time=wh_util:elapsed_s(Started)
                                                         }
                               }).

%% Agent was rung for a call, and failed to pickup in time
-spec call_missed/4 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
call_missed(AcctId, QueueId, AgentId, CallId) ->
    #call_stat{agents_rung=As} = CallStat = find_call_stat(CallId),

    gen_listener:cast(?MODULE, {store, CallStat#call_stat{acct_id=AcctId
                                                          ,queue_id=QueueId
                                                          ,call_id=CallId
                                                          ,agents_rung=[AgentId | As]
                                                         }
                               }).

%% Call was picked up by an agent, track how long caller was in queue
-spec call_handled/4 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
call_handled(AcctId, QueueId, CallId, AgentId) ->
    #call_stat{agents_rung=As
               ,started=Started
              }=CallStat = find_call_stat(CallId),

    gen_listener:cast(?MODULE, {store, CallStat#call_stat{acct_id=AcctId
                                                          ,queue_id=QueueId
                                                          ,agent_id=AgentId
                                                          ,call_id=CallId
                                                          ,agents_rung=lists:reverse(As)
                                                          ,wait_time=wh_util:elapsed_s(Started)
                                                          ,status='handling'
                                                         }
                               }).

-spec call_finished/1 :: (ne_binary()) -> 'ok'.
call_finished(CallId) ->
    case find_call_stat(CallId) of
        #call_stat{status=waiting}=CallStat ->
            %% call hungup, note it and move it out of waiting
            gen_listener:cast(?MODULE, {store, CallStat#call_stat{status='finished'}});
        _CallStat ->
            %% call hungup, but someone else updated it
            ok
    end.

%% Call was placed in the AMQP Queue
-spec call_waiting/3 :: (ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
call_waiting(AcctId, QueueId, CallId) ->
    CallStat = find_call_stat(CallId),
    gen_listener:cast(?MODULE, {store, CallStat#call_stat{
                                         acct_id=AcctId
                                         ,queue_id=QueueId
                                         ,call_id=CallId
                                         ,started=wh_util:current_tstamp()
                                         ,agents_rung=[]
                                         ,status='waiting'
                                        }
                               }).

%% marks an agent as active for an account
-spec agent_active/2 :: (ne_binary(), ne_binary()) -> 'ok'.
agent_active(AcctId, AgentId) ->
    gen_listener:cast(?MODULE, {store, #agent_stat{acct_id=AcctId
                                                   ,agent_id=AgentId
                                                   ,status='ready'
                                                   ,timestamp=wh_util:current_tstamp()
                                                   ,wait_time=undefined
                                                  }
                               }).

%% marks an agent as inactive for an account
-spec agent_inactive/2 :: (ne_binary(), ne_binary()) -> 'ok'.
agent_inactive(AcctId, AgentId) ->
    gen_listener:cast(?MODULE, {store, #agent_stat{acct_id=AcctId
                                                   ,agent_id=AgentId
                                                   ,status='paused'
                                                   ,timestamp=wh_util:current_tstamp()
                                                   ,wait_time=undefined
                                                  }
                               }).

%% marks an agent as back from break, effectively removing the waiting stat
-spec agent_resume/2 :: (ne_binary(), ne_binary()) -> 'ok'.
agent_resume(AcctId, AgentId) ->
    gen_listener:cast(?MODULE, {store, #agent_stat{acct_id=AcctId
                                                   ,agent_id=AgentId
                                                   ,status='ready'
                                                   ,timestamp=wh_util:current_tstamp()
                                                   ,wait_time=undefined
                                                  }
                               }).

%% marks an agent as paused for an account
-spec agent_paused/3 :: (ne_binary(), ne_binary(), integer()) -> 'ok'.
agent_paused(AcctId, AgentId, Timeout) ->
    gen_listener:cast(?MODULE, {store, #agent_stat{acct_id=AcctId
                                                   ,agent_id=AgentId
                                                   ,status='paused'
                                                   ,timestamp=wh_util:current_tstamp()
                                                   ,wait_time=Timeout
                                                  }
                               }).

-spec agent_wrapup/3 :: (ne_binary(), ne_binary(), integer()) -> 'ok'.
agent_wrapup(AcctId, AgentId, Timeout) ->
    gen_listener:cast(?MODULE, {store, #agent_stat{acct_id=AcctId
                                                   ,agent_id=AgentId
                                                   ,status='wrapup'
                                                   ,timestamp=wh_util:current_tstamp()
                                                   ,wait_time=Timeout
                                                  }
                               }).

-spec agent_ready/2 :: (ne_binary(), ne_binary()) -> 'ok'.
agent_ready(AcctId, AgentId) ->
    gen_listener:cast(?MODULE, {store, #agent_stat{acct_id=AcctId
                                                   ,agent_id=AgentId
                                                   ,status='ready'
                                                   ,timestamp=wh_util:current_tstamp()
                                                   ,wait_time=undefined
                                                  }
                               }).

-define(BINDINGS, []).
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
    LogTime = ms_to_next_hour(),
    _ = erlang:send_after(LogTime, self(), {the_hour_is_up, LogTime}),

    StoreDir = iolist_to_binary([code:priv_dir(acdc), "/calls/"]),
    init_store(StoreDir),

    lager:debug("started new acdc stats collector, storing files in ~s", [StoreDir]),
    {ok, {StoreDir, ets:new(?ETS_TABLE, [set
                                         ,protected
                                         ,named_table
                                         ,{keypos, #call_stat.call_id} % same position as agent_id
                                        ])}
    }.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(flush_table, {_, Table}=State) ->
    ets:delete_all_objects(Table),
    {noreply, State};
handle_cast({store, Stat}, {Dir, Table}=State) ->
    true = ets:insert(Table, Stat),
    write_to_file(Dir, Stat),
    {noreply, State};
handle_cast({remove, Stat}, {_, Table}=State) ->
    _Objs = ets:select_delete(Table, [{Stat, [], [true]}]),
    lager:debug("maybe deleted ~p objects", [_Objs]),
    {noreply, State};
handle_cast(_Req, State) ->
    {noreply, State}.

%% 300 seconds (5 minutes) means we just started up, or the timers were
%% off...either way, we are ignoring the log point and starting the
%% timer back up.
handle_info({the_hour_is_up, N}, State) when N < 300000 ->
    lager:debug("the next hour came quickly: ~p", [N]),
    lager:debug("ignore and start again"),
    LogTime = ms_to_next_hour(),
    _ = erlang:send_after(LogTime, self(), {the_hour_is_up, LogTime}),
    {noreply, State};
handle_info({the_hour_is_up, N}, {_, Table}=State) ->
    lager:debug("hour is up (~b ms), time to store the data", [N]),
    LogTime = ms_to_next_hour(),
    _ = erlang:send_after(LogTime, self(), {the_hour_is_up, LogTime}),

    flush_to_db(Table),

    {noreply, State};
handle_info(_Msg, State) ->
    lager:debug("unhandling message: ~p", [_Msg]),
    {noreply, State}.

handle_event(_JObj, _State) ->
    {reply, []}.

terminate(_Reason, {_, Table}) ->
    lager:debug("acdc stats terminating: ~p", [_Reason]),
    flush_to_db(Table),
    ets:delete(Table).

flush_table() ->
    gen_listener:cast(?MODULE, flush_table).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec ms_to_next_hour/0 :: () -> pos_integer().
ms_to_next_hour() ->
    {_, {_,M,S}} = calendar:universal_time(),
    (3600 - ((60 * M) + S)) * 1000.

%% take the contents of the table, aggregrate into the appropriate stats
%% and store into the accounts' database
flush_to_db(Table) ->
    Stats = ets:tab2list(Table), % dump all stats
    true = ets:delete_all_objects(Table), % delete the stats from the table
    spawn(?MODULE, write_to_dbs, [Stats]).

-spec write_to_dbs/1 :: (stats()) -> 'ok'.
-spec write_to_dbs/3 :: (stats(), integer(), dict()) -> 'ok'.
write_to_dbs(Stats) ->
    write_to_dbs(Stats, wh_util:current_tstamp(), dict:new()).
write_to_dbs([], TStamp, AcctDocs) ->
    % write to db
    _ = [write_account_doc(AcctDoc, TStamp) || AcctDoc <- dict:to_list(AcctDocs)],
    ok;
write_to_dbs([Stat|Stats], TStamp, AcctDocs) ->
    write_to_dbs(Stats, TStamp, update_stat(AcctDocs, Stat)).

-spec write_account_doc/2 :: ({ne_binary(), wh_json:json_object()}, integer()) -> 'ok'.
write_account_doc({AcctId, AcctJObj}, TStamp) ->
    _ = couch_mgr:save_doc(wh_util:format_account_id(AcctId, encoded)
                           ,wh_json:set_value(<<"recorded_at">>, TStamp, AcctJObj)
                          ),
    lager:debug("wrote stat doc for ~s", [AcctId]).

-spec update_stat/2 :: (dict(), stat()) -> dict().
update_stat(AcctDocs, #agent_stat{status='ready'
                                  ,acct_id=AcctId
                                  ,agent_id=AgentId
                                  ,timestamp=TStamp
                                 }) ->
    dict:store(AcctId
               ,update_status(fetch_acct_doc(AcctId, AcctDocs), AgentId, TStamp, <<"login">>)
               ,AcctDocs
              );

update_stat(AcctDocs, #agent_stat{status='logged_out'
                                  ,acct_id=AcctId
                                  ,agent_id=AgentId
                                  ,timestamp=TStamp
                                 }) ->
    dict:store(AcctId
               ,update_status(fetch_acct_doc(AcctId, AcctDocs), AgentId, TStamp, <<"logout">>)
               ,AcctDocs
              );


update_stat(AcctDocs, #agent_stat{status='wrapup'
                                  ,acct_id=AcctId
                                  ,agent_id=AgentId
                                  ,timestamp=TStamp
                                  ,wait_time=Timeout
                                 }) ->
    TimeoutKey = [<<"agents">>, AgentId, <<"timeout">>],
    TimeLeftKey = [<<"agents">>, AgentId, <<"time_left">>],

    dict:store(AcctId
               ,wh_json:set_values([{TimeoutKey, Timeout}
                                    ,{TimeLeftKey, wh_util:elapsed_s(TStamp)}
                                   ]
                                   ,update_status(fetch_acct_doc(AcctId, AcctDocs), AgentId, TStamp, <<"wrapup">>)
                                  )
               ,AcctDocs
              );

update_stat(AcctDocs, #agent_stat{status='paused'
                                  ,acct_id=AcctId
                                  ,agent_id=AgentId
                                  ,timestamp=TStamp
                                  ,wait_time=Timeout
                                 }) ->
    TimeoutKey = [<<"agents">>, AgentId, <<"timeout">>],
    TimeLeftKey = [<<"agents">>, AgentId, <<"time_left">>],

    dict:store(AcctId
               ,wh_json:set_values([{TimeoutKey, Timeout}
                                    ,{TimeLeftKey, wh_util:elapsed_s(TStamp)}
                                   ]
                                   ,update_status(fetch_acct_doc(AcctId, AcctDocs), AgentId, TStamp, <<"paused">>)
                                  )
               ,AcctDocs
              );

update_stat(AcctDocs, #call_stat{status='processed'
                                 ,call_id=CallId
                                 ,acct_id=AcctId
                                 ,queue_id=QueueId
                                 ,agent_id=AgentId
                                 ,agents_rung=AgentsRung
                                 ,started=Started
                                 ,wait_time=Waited
                                 ,talk_time=Talked
                                }) ->
    AcctDoc = fetch_acct_doc(AcctId, AcctDocs),

    Funs = [{fun add_call_duration/4, [QueueId, CallId, Talked]}
            ,{fun add_call_agent/4, [QueueId, CallId, AgentId]}
            ,{fun add_agent_call/4, [AgentId, CallId, Talked]}
            ,{fun add_agent_call_queue/4, [AgentId, CallId, QueueId]}
            ,{fun add_call_timestamp/4, [QueueId, CallId, Started]}
            ,{fun add_call_status/4, [QueueId, CallId, <<"processed">>]}
            ,{fun add_call_wait/4, [QueueId, CallId, Waited]}
            ,{fun add_agents_rung/4, [QueueId, CallId, AgentsRung]}
           ],
    dict:store(AcctId
               ,lists:foldl(fun({F, Args}, AcctAcc) ->
                                    apply(F, [AcctAcc | Args])
                            end, AcctDoc, Funs)
               ,AcctDocs
              );

update_stat(AcctDocs, #call_stat{status='abandoned'
                                 ,call_id=CallId
                                 ,acct_id=AcctId
                                 ,queue_id=QueueId
                                 ,agents_rung=AgentsRung
                                 ,started=Started
                                 ,wait_time=Waited
                                 ,abandon_reason=AbandonReason
                                }) ->
    AcctDoc = fetch_acct_doc(AcctId, AcctDocs),

    Funs = [{fun add_call_abandoned/4, [QueueId, CallId, AbandonReason]}
            ,{fun add_call_timestamp/4, [QueueId, CallId, Started]}
            ,{fun add_call_status/4, [QueueId, CallId, <<"abandoned">>]}
            ,{fun add_call_wait/4, [QueueId, CallId, Waited]}
            ,{fun add_agents_rung/4, [QueueId, CallId, AgentsRung]}
           ],
    dict:store(AcctId
               ,lists:foldl(fun({F, Args}, AcctAcc) ->
                                    apply(F, [AcctAcc | Args])
                            end, AcctDoc, Funs)
               ,AcctDocs
              );

update_stat(AcctDocs, #call_stat{status='finished'
                                 ,call_id=CallId
                                 ,acct_id=AcctId
                                 ,queue_id=QueueId
                                 ,started=Started
                                }) ->
    AcctDoc = fetch_acct_doc(AcctId, AcctDocs),

    Funs = [{fun add_call_timestamp/4, [QueueId, CallId, Started]}
            ,{fun add_call_status/4, [QueueId, CallId, <<"finished">>]}
           ],
    dict:store(AcctId
               ,lists:foldl(fun({F, Args}, AcctAcc) ->
                                    apply(F, [AcctAcc | Args])
                            end, AcctDoc, Funs)
               ,AcctDocs
              );

update_stat(AcctDocs, #call_stat{status='handling'
                                 ,call_id=CallId
                                 ,acct_id=AcctId
                                 ,queue_id=QueueId
                                 ,agent_id=AgentId
                                 ,agents_rung=AgentsRung
                                 ,started=Started
                                 ,wait_time=Waited
                                }) ->
    AcctDoc = fetch_acct_doc(AcctId, AcctDocs),

    Funs = [{fun add_call_waiting/4, [QueueId, CallId, Waited]}
            ,{fun add_call_timestamp/4, [QueueId, CallId, Started]}
            ,{fun add_call_status/4, [QueueId, CallId, <<"handling">>]}
            ,{fun add_call_wait/4, [QueueId, CallId, Waited]}
            ,{fun add_agents_rung/4, [QueueId, CallId, AgentsRung]}
            ,{fun add_call_handling/4, [QueueId, CallId, AgentId]}
           ],
    dict:store(AcctId
               ,lists:foldl(fun({F, Args}, AcctAcc) ->
                                    apply(F, [AcctAcc | Args])
                            end, AcctDoc, Funs)
               ,AcctDocs
              );

update_stat(AcctDocs, #call_stat{status='waiting'
                                 ,call_id=CallId
                                 ,acct_id=AcctId
                                 ,queue_id=QueueId
                                 ,agents_rung=AgentsRung
                                 ,started=Started
                                }) ->
    AcctDoc = fetch_acct_doc(AcctId, AcctDocs),

    Funs = [{fun add_call_waiting/4, [QueueId, CallId, Started]}
            ,{fun add_call_timestamp/4, [QueueId, CallId, Started]}
            ,{fun add_call_status/4, [QueueId, CallId, <<"waiting">>]}
            ,{fun add_call_wait/4, [QueueId, CallId, wh_util:elapsed_s(Started)]}
            ,{fun add_agents_rung/4, [QueueId, CallId, AgentsRung]}
        ],
    dict:store(AcctId
               ,lists:foldl(fun({F, Args}, AcctAcc) ->
                                    apply(F, [AcctAcc | Args])
                            end, AcctDoc, Funs)
               ,AcctDocs
              );

update_stat(AcctDocs, _Stat) ->
    lager:debug("unknown stat: ~p", [_Stat]),
    AcctDocs.

-spec fetch_acct_doc/2 :: (ne_binary(), dict()) -> wh_json:json_object().
fetch_acct_doc(AcctId, AcctDocs) ->
    case catch dict:fetch(AcctId, AcctDocs) of
        {'EXIT', _} -> new_account_doc(AcctId);
        AcctJObj -> AcctJObj
    end.

-spec new_account_doc/1 :: (ne_binary()) -> wh_json:json_object().
new_account_doc(AcctId) ->
    wh_doc:update_pvt_parameters(wh_json:new()
                                 ,wh_util:format_account_id(AcctId, encoded)
                                 ,[{type, <<"acdc_stat">>}]
                                ).

-spec add_call_duration/4 :: (wh_json:json_object(), ne_binary()
                              ,ne_binary(), integer()
                             ) -> wh_json:json_object().
add_call_duration(AcctDoc, QueueId, CallId, Elapsed) ->
    Key = [<<"queues">>, QueueId, <<"calls">>, CallId, <<"duration">>],
    wh_json:set_value(Key, Elapsed, AcctDoc).

-spec add_call_waiting/4 :: (wh_json:json_object(), ne_binary()
                             ,ne_binary(), integer()
                            ) -> wh_json:json_object().
add_call_waiting(AcctDoc, QueueId, CallId, WaitTime) ->
    Props = [{[<<"queues">>, QueueId, <<"calls">>, CallId, <<"entered">>], WaitTime}
             ,{[<<"queues">>, QueueId, <<"calls">>, CallId, <<"queue_id">>], QueueId}
             ,{[<<"queues">>, QueueId, <<"calls">>, CallId, <<"wait_time">>], wh_util:elapsed_s(WaitTime)}
             ,{[<<"queues">>, QueueId, <<"calls">>, CallId, <<"call_id">>], CallId}
            ],
    wh_json:set_values(Props, AcctDoc).

add_agents_rung(AcctDoc, QueueId, CallId, AgentsRung) ->
    Key = [<<"queues">>, QueueId, <<"calls">>, CallId, <<"agents_rung">>],
    wh_json:set_value(Key, AgentsRung, AcctDoc).

-spec add_call_agent/4 :: (wh_json:json_object(), ne_binary()
                               ,ne_binary(), api_binary()
                              ) -> wh_json:json_object().
add_call_agent(AcctDoc, _, _, undefined) -> AcctDoc;
add_call_agent(AcctDoc, QueueId, CallId, AgentId) ->
    Key = [<<"queues">>, QueueId, <<"calls">>, CallId, <<"agent_id">>],
    wh_json:set_value(Key, AgentId, AcctDoc).

-spec add_call_timestamp/4 :: (wh_json:json_object(), ne_binary()
                               ,ne_binary(), integer()
                              ) -> wh_json:json_object().
add_call_timestamp(AcctDoc, QueueId, CallId, Timestamp) ->
    Key = [<<"queues">>, QueueId, <<"calls">>, CallId, <<"timestamp">>],
    wh_json:set_value(Key, Timestamp, AcctDoc).

-spec add_call_abandoned/4 :: (wh_json:json_object(), ne_binary()
                               ,ne_binary(), abandon_reason()
                              ) -> wh_json:json_object().
add_call_abandoned(AcctDoc, QueueId, CallId, Reason) ->
    Key = [<<"queues">>, QueueId, <<"calls">>, CallId, <<"abandoned">>],
    wh_json:set_value(Key, Reason, AcctDoc).

add_call_handling(AcctDoc, QueueId, CallId, AgentId) ->
    Key = [<<"queues">>, QueueId, <<"calls">>, CallId, <<"agent_id">>],
    wh_json:set_value(Key, AgentId, AcctDoc).

add_call_status(AcctDoc, QueueId, CallId, Status) ->
    Key = [<<"queues">>, QueueId, <<"calls">>, CallId, <<"status">>],
    wh_json:set_value(Key, Status, AcctDoc).

add_call_wait(AcctDoc, QueueId, CallId, Waited) ->
    Key = [<<"queues">>, QueueId, <<"calls">>, CallId, <<"wait_time">>],
    wh_json:set_value(Key, Waited, AcctDoc).

-spec add_agent_call/4 :: (wh_json:json_object(), ne_binary()
                           ,ne_binary(), integer()
                          ) -> wh_json:json_object().
add_agent_call(AcctDoc, AgentId, CallId, Elapsed) ->
    Key = [<<"agents">>, AgentId, <<"calls_handled">>, CallId, <<"duration">>],
    wh_json:set_value(Key, Elapsed, AcctDoc).

-spec add_agent_call_queue/4 :: (wh_json:json_object(), ne_binary()
                                 ,ne_binary(), ne_binary()
                                ) -> wh_json:json_object().
add_agent_call_queue(AcctDoc, AgentId, CallId, QueueId) ->
    Key = [<<"agents">>, AgentId, <<"calls_handled">>, CallId, <<"queue_id">>],
    wh_json:set_value(Key, QueueId, AcctDoc).

update_status(AcctDoc, AgentId, TStamp, Status) ->
    StatusKey = [<<"agents">>, AgentId, <<"statuses">>],
    Statuses = wh_json:get_value(StatusKey, AcctDoc, wh_json:new()),
    wh_json:set_value(StatusKey, wh_json:set_value(wh_util:to_binary(TStamp), Status, Statuses), AcctDoc).

write_to_file(_, #agent_stat{}) -> ok;
write_to_file(Dir, #call_stat{call_id=CallId
                              ,acct_id=AcctId
                              ,queue_id=QueueId
                              ,agent_id=AgentId
                              ,agents_rung=AgentsRung
                              ,started=Started
                              ,wait_time=WaitTime
                              ,talk_time=TalkTime
                              ,abandon_reason=AbandonReason
                              ,status=Status
                             }) ->
    Filename = iolist_to_binary([Dir, CallId, <<".csv">>]),
    true = maybe_create_file(Filename),

    FileLine = csvline([CallId, AcctId, QueueId, AgentId
                        ,AgentsRung, Started, WaitTime, TalkTime
                        ,AbandonReason, Status
                       ]),

    case file:open(Filename, [append]) of
        {error, _R} ->
            lager:debug("failed to write to file ~s: ~p", [Filename, _R]),
            lager:debug("wanted to write ~s", [iolist_to_binary(FileLine)]);
        {ok, IoDev} ->
            lager:debug("writing ~s", [iolist_to_binary(FileLine)]),
            ok = file:write(IoDev, FileLine),
            ok = file:close(IoDev)
    end.

maybe_create_file(Filename) ->
    case filelib:is_regular(Filename) of
        true -> true;
        false ->
            lager:debug("creating stat file ~s", [Filename]),
            case file:open(Filename, [write]) of
                {error, _E} ->
                    lager:debug("failed to open ~s for initial file write: ~p", [Filename, _E]),
                    false;
                {ok, IoDevice} ->
                    lager:debug("writing header to csv"),
                    ok = file:write(IoDevice, "\"Call ID\",\"Account\",\"Queue\",\"Agent\",\"Agents Tried\",\"Started\",\"Wait Time\",\"Talk Time\",\"Abandon Reason\",\"Status\",\"Line Written\"\n"),
                    file:close(IoDevice) =:= ok
            end
    end.

csvline(Vals) ->
    csvline(Vals, []).
csvline([], Acc) ->
    lists:reverse([$\n, $\", wh_util:to_binary(wh_util:current_tstamp()), $\" | Acc]);
csvline([V|Vs], Acc) ->
    csvline(Vs, [ $,,  $\", encode(V), $\" | Acc]).

encode(Vs) when is_list(Vs) -> wh_util:join_binary(Vs, <<" || ">>);
encode(undefined) -> <<>>;
encode(V) -> wh_util:to_binary(V).

init_store(Dir) ->
    case filelib:is_dir(Dir) of
        true -> ok;
        false ->
            case file:make_dir(Dir) of
                ok -> lager:debug("created ~s", [Dir]);
                {error, _E} -> lager:info("failed to create dir ~s: ~p", [Dir, _E])
            end
    end.
