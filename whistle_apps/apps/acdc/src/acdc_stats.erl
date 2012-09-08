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

         %% Agent-specific stats
         ,agent_active/2
         ,agent_inactive/2
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
-export([write_to_dbs/1]).

-include("acdc.hrl").

-type stat_name() :: 'call_processed' | 'call_abandoned' |
                     'agent_active' | 'agent_inactive'.

-record(stat, {
          name            :: stat_name()
          ,acct_id        :: ne_binary()
          ,queue_id       :: ne_binary()
          ,agent_id       :: ne_binary()
          ,call_id        :: ne_binary()
          ,call_count     :: integer()
          ,elapsed        :: integer()
          ,is_active      :: boolean()
          ,active_since   :: wh_now()
          ,abandon_reason :: abandon_reason()
         }).

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

%% marks an agent as active for an account
-spec agent_active/2 :: (ne_binary(), ne_binary()) -> 'ok'.
agent_active(AcctId, AgentId) ->
    gen_listener:cast(?MODULE, {store, #stat{acct_id=AcctId
                                             ,agent_id=AgentId
                                             ,is_active=true
                                             ,active_since=erlang:now()
                                             ,name=agent_active
                                            }
                               }).

%% marks an agent as inactive for an account
-spec agent_inactive/2 :: (ne_binary(), ne_binary()) -> 'ok'.
agent_inactive(AcctId, AgentId) ->
    gen_listener:cast(?MODULE, {store, #stat{acct_id=AcctId
                                             ,agent_id=AgentId
                                             ,is_active=false
                                             ,active_since=undefined
                                             ,name=agent_inactive
                                            }
                               }).

-define(BINDINGS, []).
-define(RESPONDERS, []).
-define(QUEUE_NAME, <<"acdc.stats">>).
-define(CONSUME_OPTIONS, []).
start_link() ->
    gen_listener:start_link({local, ?MODULE}
                            ,?MODULE
                            ,[{bindings, ?BINDINGS}
                              ,{responders, ?RESPONDERS}
                              ,{queue_name, ?QUEUE_NAME}
                              ,{consume_options, ?CONSUME_OPTIONS}
                             ],
                            []).

init([]) ->
    put(callid, <<"acdc.stats">>),
    LogTime = ms_to_next_hour(),
    _ = erlang:send_after(LogTime, self(), {the_hour_is_up, LogTime}),
    lager:debug("started new acdc stats collector"),
    {ok, ets:new(acdc_stats, [duplicate_bag % many instances of the key
                              ,protected
                              ,named_table
                              ,{keypos, #stat.name}
                             ])}.

handle_call(_Req, _From, Table) ->
    {reply, ok, Table}.

handle_cast({store, Stat}, Table) ->
    ets:insert(Table, Stat),
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

write_to_dbs(Stats) ->
    write_to_dbs(Stats, wh_util:current_tstamp(), []).
write_to_dbs([], TStamp, _AcctDocs) ->
    % write to db
    lager:debug("ready to write stats for the last hour ending at ~b", [TStamp]),
    ok;
write_to_dbs([Stat|Stats], TStamp, AcctDocs) ->
    lager:debug("stat: ~p", [Stat]),
    write_to_dbs(Stats, TStamp, AcctDocs).
