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
        ]).

%% Stats API
-export([call_processed/5
         ,call_abandoned/4
         ,call_missed/4
         ,call_handled/5
         ,call_waiting/3
         ,call_recorded/4

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

-type stat_name() :: 'call_processed' | 'call_abandoned' |
                     'call_missed' | 'call_handled' |
                     'call_waiting' | 'call_recorded' |
                     'agent_active' | 'agent_inactive' | 'agent_wrapup' |
                     'agent_paused' | 'agent_resume'.

-record(stat, {
          name            :: stat_name() | '_'
          ,acct_id        :: ne_binary() | '$1' % for the match spec
          ,queue_id       :: ne_binary() | '$2' | '_'
          ,agent_id       :: ne_binary() | '$2' | '_'
          ,call_id        :: ne_binary() | '_'
          ,call_count     :: integer() | '_'
          ,elapsed        :: integer() | '_'
          ,timestamp = wh_util:current_tstamp() :: integer() | '_' % gregorian seconds
          ,active_since   :: integer() | '_'
          ,abandon_reason :: abandon_reason() | '_'
         }).

-spec acct_stats/1 :: (ne_binary()) -> wh_json:json_objects().
acct_stats(AcctId) ->
    MatchSpec = [{#stat{acct_id='$1', _ = '_'}
                  ,[{'=:=', '$1', AcctId}]
                  ,['$_']
                 }],
    AcctDocs = lists:foldl(fun(Stat, AcctAcc) ->
                                   update_stat(AcctAcc, Stat)
                           end, dict:new(), ets:select(?ETS_TABLE, MatchSpec)
                          ),
    wh_doc:public_fields(fetch_acct_doc(AcctId, AcctDocs)).

queue_stats(AcctId) ->
    MatchSpec = [{#stat{acct_id='$1', queue_id='$2', _='_'}
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
    MatchSpec = [{#stat{acct_id='$1', queue_id='$2', _='_'}
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
    MatchSpec = [{#stat{acct_id='$1', agent_id='$2', _='_'}
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
    MatchSpec = [{#stat{acct_id='$1', agent_id='$2', _='_'}
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

%% An agent connected with a caller
-spec call_processed/5 :: (ne_binary(), ne_binary()
                           ,ne_binary(), ne_binary()
                           ,integer()
                           ) -> 'ok'.
call_processed(AcctId, QueueId, AgentId, CallId, Elapsed) ->
    gen_listener:cast(?MODULE, {store, #stat{acct_id=AcctId
                                             ,queue_id=QueueId
                                             ,agent_id=AgentId
                                             ,call_id=CallId
                                             ,elapsed=Elapsed
                                             ,name=call_processed
                                            }
                               }).

%% Caller left the queue
-spec call_abandoned/4 :: (ne_binary(), ne_binary()
                           ,ne_binary(), abandon_reason()
                          ) -> 'ok'.
call_abandoned(AcctId, QueueId, CallId, Reason) ->
    gen_listener:cast(?MODULE, {store, #stat{acct_id=AcctId
                                             ,queue_id=QueueId
                                             ,call_id=CallId
                                             ,abandon_reason=Reason
                                             ,name=call_abandoned
                                            }
                               }).

%% Agent was rung for a call, and failed to pickup in time
-spec call_missed/4 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
call_missed(AcctId, QueueId, AgentId, CallId) ->
    gen_listener:cast(?MODULE, {store, #stat{acct_id=AcctId
                                             ,queue_id=QueueId
                                             ,agent_id=AgentId
                                             ,call_id=CallId
                                             ,name=call_missed
                                            }
                               }).

%% Call was picked up by an agent, track how long caller was in queue
-spec call_handled/5 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), integer()) -> 'ok'.
call_handled(AcctId, QueueId, CallId, AgentId, Elapsed) ->
    gen_listener:cast(?MODULE, {store, #stat{acct_id=AcctId
                                             ,queue_id=QueueId
                                             ,agent_id=AgentId
                                             ,call_id=CallId
                                             ,elapsed=Elapsed
                                             ,name=call_handled
                                            }
                               }),
    gen_listener:cast(?MODULE, {remove, call_waiting_stat(AcctId, QueueId, CallId, '_')}).

%% Call was placed in the AMQP Queue
-spec call_waiting/3 :: (ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
call_waiting(AcctId, QueueId, CallId) ->
    gen_listener:cast(?MODULE, {store, call_waiting_stat(AcctId, QueueId, CallId)}).

call_waiting_stat(AcctId, QueueId, CallId) ->
    call_waiting_stat(AcctId, QueueId, CallId, wh_util:current_tstamp()).
call_waiting_stat(AcctId, QueueId, CallId, Start) ->
    #stat{acct_id=AcctId
          ,queue_id=QueueId
          ,call_id=CallId
          ,timestamp=Start
          ,name=call_waiting
         }.

call_recorded(AcctId, QueueId, AgentId, CallId) ->
    gen_listener:cast(?MODULE, {store, #stat{acct_id=AcctId
                                             ,queue_id=QueueId
                                             ,agent_id=AgentId
                                             ,call_id=CallId
                                             ,name=call_recorded
                                            }
                               }).

%% marks an agent as active for an account
-spec agent_active/2 :: (ne_binary(), ne_binary()) -> 'ok'.
agent_active(AcctId, AgentId) ->
    gen_listener:cast(?MODULE, {store, #stat{acct_id=AcctId
                                             ,agent_id=AgentId
                                             ,active_since=wh_util:current_tstamp()
                                             ,name=agent_active
                                            }
                               }).

%% marks an agent as inactive for an account
-spec agent_inactive/2 :: (ne_binary(), ne_binary()) -> 'ok'.
agent_inactive(AcctId, AgentId) ->
    gen_listener:cast(?MODULE, {store, #stat{acct_id=AcctId
                                             ,agent_id=AgentId
                                             ,active_since=wh_util:current_tstamp()
                                             ,name=agent_inactive
                                            }
                               }).

%% marks an agent as back from break, effectively removing the waiting stat
-spec agent_resume/2 :: (ne_binary(), ne_binary()) -> 'ok'.
agent_resume(AcctId, AgentId) ->
    gen_listener:cast(?MODULE, {remove, #stat{name=agent_paused
                                              ,acct_id=AcctId
                                              ,agent_id=AgentId
                                              ,active_since='_'
                                              ,elapsed='_'
                                              ,timestamp='_'
                                             }
                               }),
    agent_active(AcctId, AgentId).

%% marks an agent as paused for an account
-spec agent_paused/3 :: (ne_binary(), ne_binary(), integer()) -> 'ok'.
agent_paused(AcctId, AgentId, Timeout) ->
    gen_listener:cast(?MODULE, {store, #stat{acct_id=AcctId
                                             ,agent_id=AgentId
                                             ,active_since=wh_util:current_tstamp()
                                             ,elapsed=Timeout
                                             ,name=agent_paused
                                            }
                               }).

-spec agent_wrapup/3 :: (ne_binary(), ne_binary(), integer()) -> 'ok'.
agent_wrapup(AcctId, AgentId, Timeout) ->
    gen_listener:cast(?MODULE, {store, #stat{acct_id=AcctId
                                             ,agent_id=AgentId
                                             ,elapsed=Timeout
                                             ,name=agent_wrapup
                                            }
                               }).

-spec agent_ready/2 :: (ne_binary(), ne_binary()) -> 'ok'.
agent_ready(AcctId, AgentId) ->
    gen_listener:cast(?MODULE, {remove, #stat{name=agent_wrapup
                                              ,acct_id=AcctId
                                              ,agent_id=AgentId
                                              ,call_id='_'
                                              ,elapsed='_'
                                              ,timestamp='_'
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
    lager:debug("started new acdc stats collector"),
    {ok, ets:new(?ETS_TABLE, [ordered_set % many instances of the key
                              ,protected
                              ,named_table
                              ,{keypos, #stat.timestamp}
                             ])}.

handle_call(_Req, _From, Table) ->
    {reply, ok, Table}.

handle_cast(flush_table, Table) ->
    ets:delete_all_objects(Table),
    {noreply, Table};
handle_cast({store, Stat}, Table) ->
    case ets:insert_new(Table, Stat) of
        false -> handle_cast({store, Stat#stat{timestamp=wh_util:current_tstamp()}}, Table);
        true ->
            {noreply, Table}
    end;
handle_cast({remove, Stat}, Table) ->
    _Objs = ets:select_delete(Table, [{Stat, [], [true]}]),
    lager:debug("maybe deleted ~p objects", [_Objs]),
    {noreply, Table};
handle_cast(_Req, Table) ->
    {noreply, Table}.

%% 300 seconds (5 minutes) means we just started up, or the timers were
%% off...either way, we are ignoring the log point and starting the
%% timer back up.
handle_info({the_hour_is_up, N}, Table) when N < 300000 ->
    lager:debug("the next hour came quickly: ~p", [N]),
    lager:debug("ignore and start again"),
    LogTime = ms_to_next_hour(),
    _ = erlang:send_after(LogTime, self(), {the_hour_is_up, LogTime}),
    {noreply, Table};
handle_info({the_hour_is_up, N}, Table) ->
    lager:debug("hour is up (~b ms), time to store the data", [N]),
    LogTime = ms_to_next_hour(),
    _ = erlang:send_after(LogTime, self(), {the_hour_is_up, LogTime}),

    flush_to_db(Table),

    {noreply, Table};
handle_info(_Msg, Table) ->
    lager:debug("unhandled message: ~p", [_Msg]),
    {noreply, Table}.

handle_event(_JObj, _Table) ->
    {reply, []}.

terminate(_Reason, Table) ->
    lager:debug("acdc stats terminating: ~p", [_Reason]),
    flush_to_db(Table),
    ets:delete(Table).

flush_table() ->
    gen_listener:cast(?MODULE, flush_table).

code_change(_OldVsn, Table, _Extra) ->
    {ok, Table}.

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

-spec write_to_dbs/1 :: ([#stat{},...] | []) -> 'ok'.
-spec write_to_dbs/3 :: ([#stat{},...] | [], integer(), dict()) -> 'ok'.
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

-spec update_stat/2 :: (dict(), #stat{}) -> dict().
update_stat(AcctDocs, #stat{name=agent_active
                            ,acct_id=AcctId
                            ,agent_id=AgentId
                           }) ->
    AcctDoc = fetch_acct_doc(AcctId, AcctDocs),

    ActiveKey = [<<"agents">>, AgentId, <<"status">>],

    dict:store(AcctId
               ,wh_json:set_value(ActiveKey, <<"login">>, AcctDoc)
               ,AcctDocs
              );
update_stat(AcctDocs, #stat{name=agent_inactive
                            ,acct_id=AcctId
                            ,agent_id=AgentId
                           }) ->
    AcctDoc = fetch_acct_doc(AcctId, AcctDocs),

    InactiveKey = [<<"agents">>, AgentId, <<"status">>],

    dict:store(AcctId
               ,wh_json:set_value(InactiveKey, <<"logout">>, AcctDoc)
               ,AcctDocs
              );

update_stat(AcctDocs, #stat{name=agent_paused
                            ,acct_id=AcctId
                            ,agent_id=AgentId
                            ,active_since=ActiveSince
                            ,elapsed=Timeout
                           }) ->
    AcctDoc = fetch_acct_doc(AcctId, AcctDocs),

    PauseKey = [<<"agents">>, AgentId, <<"status">>],
    TimeoutKey = [<<"agents">>, AgentId, <<"timeout">>],
    TimeLeftKey = [<<"agents">>, AgentId, <<"time_left">>],

    dict:store(AcctId
               ,wh_json:set_values([{PauseKey, <<"paused">>}
                                    ,{TimeoutKey, Timeout}
                                    ,{TimeLeftKey, wh_util:elapsed_s(ActiveSince)}
                                   ], AcctDoc)
               ,AcctDocs
              );

update_stat(AcctDocs, #stat{name=call_processed
                            ,acct_id=AcctId
                            ,queue_id=QueueId
                            ,agent_id=AgentId
                            ,timestamp=Timestamp
                            ,call_id=CallId
                            ,elapsed=Elapsed
                           }) ->
    AcctDoc = fetch_acct_doc(AcctId, AcctDocs),

    Funs = [{fun add_call_duration/4, [QueueId, CallId, Elapsed]}
            ,{fun add_call_agent/4, [QueueId, CallId, AgentId]}
            ,{fun add_agent_call/4, [AgentId, CallId, Elapsed]}
            ,{fun add_agent_call_queue/4, [AgentId, CallId, QueueId]}
            ,{fun add_call_timestamp/4, [QueueId, CallId, Timestamp]}
           ],
    dict:store(AcctId
               ,lists:foldl(fun({F, Args}, AcctAcc) ->
                                    apply(F, [AcctAcc | Args])
                            end, AcctDoc, Funs)
               ,AcctDocs
              );
update_stat(AcctDocs, #stat{name=call_abandoned
                            ,acct_id=AcctId
                            ,queue_id=QueueId
                            ,call_id=CallId
                            ,abandon_reason=Reason
                            ,timestamp=Timestamp
                           }) ->
    AcctDoc = fetch_acct_doc(AcctId, AcctDocs),

    Funs = [{fun add_call_abandoned/4, [QueueId, CallId, Reason]}
            ,{fun add_call_timestamp/4, [QueueId, CallId, Timestamp]}
           ],
    dict:store(AcctId
               ,lists:foldl(fun({F, Args}, AcctAcc) ->
                                    apply(F, [AcctAcc | Args])
                            end, AcctDoc, Funs)
               ,AcctDocs
              );
update_stat(AcctDocs, #stat{name=call_missed
                            ,acct_id=AcctId
                            ,queue_id=QueueId
                            ,agent_id=AgentId
                            ,call_id=CallId
                            ,timestamp=Timestamp
                           }) ->
    AcctDoc = fetch_acct_doc(AcctId, AcctDocs),

    Funs = [{fun add_call_missed/4, [AgentId, QueueId, CallId]}
            ,{fun add_call_timestamp/4, [QueueId, CallId, Timestamp]}
           ],
    dict:store(AcctId
               ,lists:foldl(fun({F, Args}, AcctAcc) ->
                                    apply(F, [AcctAcc | Args])
                            end, AcctDoc, Funs)
               ,AcctDocs
              );
update_stat(AcctDocs, #stat{name=call_handled
                            ,acct_id=AcctId
                            ,queue_id=QueueId
                            ,call_id=CallId
                            ,elapsed=Elapsed
                            ,timestamp=Timestamp
                           }) ->
    AcctDoc = fetch_acct_doc(AcctId, AcctDocs),

    Funs = [{fun add_call_handled/4, [QueueId, CallId, Elapsed]}
            ,{fun add_call_timestamp/4, [QueueId, CallId, Timestamp]}
           ],
    dict:store(AcctId
               ,lists:foldl(fun({F, Args}, AcctAcc) ->
                                    apply(F, [AcctAcc | Args])
                            end, AcctDoc, Funs)
               ,AcctDocs
              );
update_stat(AcctDocs, #stat{name=call_waiting
                            ,acct_id=AcctId
                            ,queue_id=QueueId
                            ,call_id=CallId
                            ,timestamp=Start
                           }) ->
    AcctDoc = fetch_acct_doc(AcctId, AcctDocs),

    Funs = [{fun add_call_waiting/4, [QueueId, CallId, Start]}],
    dict:store(AcctId
               ,lists:foldl(fun({F, Args}, AcctAcc) ->
                                    apply(F, [AcctAcc | Args])
                            end, AcctDoc, Funs)
               ,AcctDocs
              );

update_stat(AcctDocs, #stat{name=call_recorded
                            ,acct_id=AcctId
                            ,queue_id=QueueId
                            ,agent_id=AgentId
                            ,call_id=CallId
                           }) ->
    AcctDoc = fetch_acct_doc(AcctId, AcctDocs),

    Funs = [{fun add_call_recorded/4, [QueueId, AgentId, CallId]}],
    dict:store(AcctId
               ,lists:foldl(fun({F, Args}, AcctAcc) ->
                                    apply(F, [AcctAcc | Args])
                            end, AcctDoc, Funs)
               ,AcctDocs
              );

update_stat(AcctDocs, #stat{acct_id=AcctId
                            ,agent_id=AgentId
                            ,timestamp=WrapupTimestamp
                            ,elapsed=WaitTimeout
                            ,name=agent_wrapup
                           }) ->
    AcctDoc = fetch_acct_doc(AcctId, AcctDocs),
    Elapsed = wh_util:elapsed_s(WrapupTimestamp),
    Funs = [
            {fun add_agent_wrapup_time_on_break/3, [AgentId, Elapsed]}
            ,{fun add_agent_wrapup_time_left/3, [AgentId, WaitTimeout - Elapsed]}
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
    Key = [<<"queues">>, QueueId, <<"calls">>, CallId, <<"entered">>],
    wh_json:set_value(Key, WaitTime, AcctDoc).

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

add_call_handled(AcctDoc, QueueId, CallId, Elapsed) ->
    Key = [<<"queues">>, QueueId, <<"calls">>, CallId, <<"wait_time">>],
    wh_json:set_value(Key, Elapsed, AcctDoc).

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

-spec add_call_missed/4 :: (wh_json:json_object(), ne_binary()
                            ,ne_binary(), ne_binary()
                           ) -> wh_json:json_object().
add_call_missed(AcctDoc, AgentId, QueueId, CallId) ->
    Key = [<<"agents">>, AgentId, <<"calls_missed">>, CallId, <<"queue_id">>],
    wh_json:set_value(Key, QueueId, AcctDoc).

-spec add_call_recorded/4 :: (wh_json:json_object(), ne_binary()
                              ,ne_binary(), ne_binary()
                             ) -> wh_json:json_object().
add_call_recorded(AcctDoc, QueueId, AgentId, CallId) ->
    RecordingName = acdc_agent:get_recording_doc_id(CallId),
    Key = [<<"call_recorded">>, CallId, <<"recording_id">>],
    wh_json:set_values([{[<<"agents">>, AgentId | Key], RecordingName}
                        ,{[<<"queues">>, QueueId | Key], RecordingName}
                       ], AcctDoc).

add_agent_wrapup_time_on_break(AcctDoc, AgentId, Elapsed) ->
    WaitKey = [<<"agents">>, AgentId, <<"wrapup_elapsed">>],
    wh_json:set_value(WaitKey, Elapsed, AcctDoc).

add_agent_wrapup_time_left(AcctDoc, AgentId, WaitTimeLeft) ->
    WaitKey = [<<"agents">>, AgentId, <<"wrapup_time_left">>],
    wh_json:set_value(WaitKey, WaitTimeLeft, AcctDoc).
