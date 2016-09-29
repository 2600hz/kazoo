%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Hesaam Farhang
%%%-------------------------------------------------------------------
-module(kvm_migrate_crawler).
-behaviour(gen_server).

%%% Public API
-export([start_link/1
        ,start/1
        ]).

%%% API used by Workers
-export([account_is_done/3
        ,account_maybe_failed/4
        ,status/0, status/2
        ,update_stats/2
        ]).

%%% gen_server callbacks
-export([init/1
        ,handle_cast/2
        ,handle_call/3
        ,handle_info/2
        ,code_change/3
        ,terminate/2
        ]).

-include("kz_voicemail.hrl").

-define(SERVER, ?MODULE).

-define(MAX_PROCESS,
        kapps_config:get_integer(?CF_CONFIG_CAT, [?KEY_VOICEMAIL, <<"migrate_max_worker">>], 10)).

-define(TIME_BETWEEN_SPAWN_ON_STARTUP, 2 * ?MILLISECONDS_IN_SECOND).
-define(TIME_BETWEEN_ACCOUNT_CRAWLS,
        kapps_config:get_integer(?CF_CONFIG_CAT, [?KEY_VOICEMAIL, <<"migrate_interaccount_delay_ms">>], 10 * ?MILLISECONDS_IN_SECOND)).


-define(MIGRATE_WORKER_SUP, ?SUPER('kvm_migrate_worker_sup')).

-record(state, {max_worker = ?MAX_PROCESS :: integer()
               ,worker_refs = [] :: [reference()]
               ,account_ids = [] :: ne_binaries()
               ,retention_passed = 'false' :: boolean()
               ,total_messages = 0 :: non_neg_integer()
               ,total_processed = 0 :: non_neg_integer()
               ,total_succeeded = 0 :: non_neg_integer()
               ,total_failed = 0 :: non_neg_integer()
               ,total_account_failed = 0 :: non_neg_integer()
               ,failed_accounts = [] :: ne_binaries()
               ,timer_ref :: reference()
               ,retention_seconds :: gregorian_seconds()
               ,account_queue :: queue:queue()
               }).
-type state() :: #state{}.

-define(DEBUG(Format, Args),
        lager:debug(Format, Args),
        io:format(Format ++ "\n", Args)
       ).

-define(WARNING(Format, Args),
        lager:warning(Format, Args),
        io:format(Format ++ "\n", Args)
       ).

-define(ERROR(Format, Args),
        lager:error(Format, Args),
        io:format(Format ++ "\n", Args)
       ).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start(pid()) -> startlink_ret().
start(MigrateSuper) ->
    gen_server:start({'local', ?SERVER}, ?SERVER, MigrateSuper, []).

-spec start_link(pid()) -> startlink_ret().
start_link(MigrateSuper) ->
    gen_server:start_link({'local', ?SERVER}, ?SERVER, MigrateSuper, []).

-spec account_is_done(ne_binary(), gregorian_seconds(), gregorian_seconds()) -> 'ok'.
account_is_done(AccountId, FirstOfMonth, LastOfMonth) ->
    gen_server:call(?SERVER, {'account_is_done', {AccountId, FirstOfMonth, LastOfMonth, 'normal'}}).

-spec account_maybe_failed(ne_binary(), gregorian_seconds(), gregorian_seconds(), any()) -> 'ok'.
account_maybe_failed(_AccountId, _FirstOfMonth, _LastOfMonth, 'timeout') ->
    lager:warning("ignoring 'timeout' error for worker of account ~s", [_AccountId]);
account_maybe_failed(AccountId, FirstOfMonth, LastOfMonth, Reason) ->
    gen_server:call(?SERVER, {'account_is_done', {AccountId, FirstOfMonth, LastOfMonth, Reason}}).

-spec update_stats(ne_binary(), kz_proplist()) -> 'ok'.
update_stats(AccountId, Stats) ->
    gen_server:call(?SERVER, {'update_stats', {AccountId, Stats}}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%--------------------------------------------------------------------
-spec init(pid()) -> {'ok', state()} |
                     'ignore'.
init(MigrateSuper) ->
    kz_util:put_callid(?SERVER),
    lager:debug("started ~s", [?SERVER]),
    case kapps_util:get_all_accounts('raw') of
        [] ->
            lager:error("no account found, going down"),
            _ = kz_util:spawn(fun kvm_migrate_sup:stop/0, []),
            'ignore';
        Ids ->
            self() ! {'start_worker_sup', MigrateSuper},
            AccountIds = kz_util:shuffle_list(Ids),
            %% start migrating messages in retention durations
            RetentionSeconds = kvm_util:retention_seconds(),
            Queue = populate_queue(AccountIds, kz_util:current_tstamp(), RetentionSeconds),
            {'ok', #state{account_ids = AccountIds
                         ,account_queue = Queue
                         ,retention_seconds = RetentionSeconds
                         }}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call({'account_is_done', {AccountId, FirstOfMonth, LastOfMonth, 'normal'}}, _From, #state{account_queue = Queue
                                                                                                }=State) ->
    lager:warning("voicemail migration for account ~s is finished, removing it from queue", [AccountId]),
    NewQueue = remove_account_from_queue({AccountId, FirstOfMonth, LastOfMonth}, Queue),
    {'reply', 'ok', State#state{account_queue = NewQueue}};
handle_call({'account_is_done', {AccountId, FirstOfMonth, LastOfMonth, Reason}}, _From, #state{account_queue = Queue
                                                                                              ,total_account_failed = TotalAccFailed
                                                                                              ,failed_accounts = FailedAccounts
                                                                                              }=State) ->
    lager:warning("failed to  migrate voicemail for account ~s with error ~p, removing it from queue", [AccountId, Reason]),
    NewQueue = remove_account_from_queue({AccountId, FirstOfMonth, LastOfMonth}, Queue),
    {'reply', 'ok', State#state{account_queue = NewQueue
                               ,total_account_failed = TotalAccFailed + 1
                               ,failed_accounts = [AccountId | FailedAccounts]
                               }};
handle_call({'update_stats', {AccountId, Props}}, _From, #state{total_processed = TotalProcessed
                                                               ,total_succeeded = TotalSucceeded
                                                               ,total_failed = TotalFailed
                                                               }=State) ->
    CycleTotalProcessed = props:get_value(<<"total_processed">>, Props, 0),
    CycleTotalSucceeded = props:get_value(<<"total_succeeded">>, Props, 0),
    CycleTotalFailed = props:get_value(<<"total_failed">>, Props, 0),

    lager:warning("a migration cycle for account ~s is done, total messages processed ~n succeeded ~b failed ~b"
                 ,[AccountId, CycleTotalProcessed, CycleTotalSucceeded, CycleTotalFailed]),

    {'reply', 'ok', State#state{total_processed = TotalProcessed + CycleTotalProcessed
                               ,total_succeeded = TotalSucceeded + CycleTotalSucceeded
                               ,total_failed = TotalFailed + CycleTotalFailed
                               }};
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast('stop', State) ->
    lager:debug("~p has been stopped", [?MODULE]),
    _ = kz_util:spawn(fun kvm_migrate_sup:stop/0, []),
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info({'start_worker_sup', MigrateSuper}, State) ->
    {'ok', Pid} = supervisor:start_child(MigrateSuper, ?MIGRATE_WORKER_SUP),
    link(Pid),
    {'noreply', State#state{timer_ref = cleanup_startup_timer()
                           }};
handle_info({'timeout', TimerRef, 'startup'}, #state{max_worker = Limit
                                                    ,worker_refs = WRefs
                                                    ,timer_ref = TimerRef
                                                    ,account_queue = Queue
                                                    }=State) when Limit > 0 ->
    case get_next_account_to_process(Queue, 'false') of
        'empty' ->
            %% migration is done waiting for workers to finish their jobs
            {'noreply', State#state{account_ids = []
                                   ,retention_passed = 'true'
                                   ,timer_ref = cleanup_startup_timer()
                                   }};
        {{_AccountId, _, _} = NextAccount, NewQ} ->
            {'ok', Pid} = kvm_migrate_worker_sup:process_account(NextAccount),
            Ref = erlang:monitor('process', Pid),
            TotalWorkersCount = length(WRefs) + Limit,
            WorkersCount = length(WRefs) + 1,
            lager:warning("started a new worker ~p (~b/~b) to process account ~s"
                         ,[Pid, WorkersCount, TotalWorkersCount, _AccountId]),
            {'noreply', State#state{max_worker = Limit - 1
                                   ,worker_refs = [Ref | WRefs]
                                   ,account_queue = NewQ
                                   ,timer_ref = cleanup_startup_timer()
                                   }}
    end;
handle_info({'timeout', TimerRef, 'startup'}, #state{max_worker = Limit
                                                    ,timer_ref = TimerRef
                                                    }=State) when Limit =< 0 ->
    {'noreply', State#state{timer_ref = cleanup_account_timer()}};
handle_info({'timeout', TimerRef, _Msg}, #state{account_ids = []
                                               ,timer_ref = TimerRef
                                               ,worker_refs = []
                                               ,retention_passed = 'true'
                                               }=State) ->
    lager:warning("voicemail migration is finished, going down", []),
    _ = kz_util:spawn(fun kvm_migrate_sup:stop/0, []),
    {'noreply', State};
handle_info({'timeout', TimerRef, _Msg}, #state{account_ids = []
                                               ,timer_ref = TimerRef
                                               ,worker_refs = WRefs
                                               ,retention_passed = 'true'
                                               }=State) ->
    lager:warning("voicemail migration is finished, waiting for ~b workers to done", [length(WRefs)]),
    {'noreply', State};
handle_info({'timeout', TimerRef, _Msg}, #state{max_worker = Limit
                                               ,worker_refs = WRefs
                                               ,timer_ref = TimerRef
                                               ,account_ids = AccountIds
                                               ,account_queue = Queue
                                               ,retention_passed = IsRetPassed
                                               ,retention_seconds = RetentionSeconds
                                               }=State) when Limit > 0 ->
    case get_next_account_to_process(Queue, IsRetPassed) of
        'retention_passed' ->
            lager:warning("all voicemails in retention duration are migrated, beginning a new cycle for migrating older voicemails", []),
            {'noreply', State#state{retention_passed = 'true'
                                   ,account_queue = populate_queue(AccountIds, RetentionSeconds)
                                   ,timer_ref = cleanup_account_timer()
                                   }};
        'empty' ->
            %% migration is done waiting for workers to finish their jobs
            {'noreply', State#state{account_ids = []
                                   ,retention_passed = 'true'
                                   ,timer_ref = cleanup_account_timer()
                                   }};
        {{_AccountId, _, _} = NextAccount, NewQ} ->
            {'ok', Pid} = kvm_migrate_worker_sup:process_account(NextAccount),
            Ref = erlang:monitor('process', Pid),
            TotalWorkersCount = length(WRefs) + Limit,
            WorkersCount = length(WRefs) + 1,
            lager:warning("started a new worker ~p (~b/~b) to process account ~s"
                         ,[Pid, WorkersCount, TotalWorkersCount, _AccountId]),
            {'noreply', State#state{max_worker = Limit - 1
                                   ,worker_refs = [Ref | WRefs]
                                   ,account_queue = NewQ
                                   ,timer_ref = cleanup_account_timer()
                                   }}
    end;
handle_info({'timeout', TimerRef, _Msg}, #state{max_worker = Limit
                                               ,timer_ref = TimerRef
                                               }=State) when Limit =< 0 ->
    lager:warning("maximum number of migration process reached", []),
    {'noreply', State};
handle_info({'DOWN', Ref, 'process', _Pid, _Reason}, #state{max_worker = Limit
                                                           ,worker_refs = WRefs
                                                           }=State) ->
    lager:warning("received down msg from worker ~p with reason ~p", [_Pid, _Reason]),
    case worker_by_ref(Ref, WRefs) of
        [] ->
            %% not ours
            {'noreply', State};
        [Ref] ->
            {'noreply', State#state{worker_refs = lists:delete(Ref, WRefs)
                                   ,timer_ref = cleanup_account_timer()
                                   ,max_worker = Limit + 1
                                   }}
    end;
handle_info(_Info, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("~s terminating: ~p", [?SERVER, _Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec cleanup_account_timer() -> reference().
cleanup_account_timer() ->
    erlang:start_timer(?TIME_BETWEEN_ACCOUNT_CRAWLS, self(), 'ok').

-spec cleanup_startup_timer() -> reference().
cleanup_startup_timer() ->
    erlang:start_timer(?TIME_BETWEEN_SPAWN_ON_STARTUP, self(), 'startup').

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_next_account_to_process(queue:queue(), boolean()) ->
                                          {next_account(), queue:queue()} |
                                          'empty' |
                                          'retention_passed'.
get_next_account_to_process(Queue, IsRetPassed) ->
    case queue:out(Queue) of
        {{'value', NextAccount}, Q} when not IsRetPassed ->
            {NextAccount, queue:in(NextAccount, Q)};
        {{'value', {AccountId, FirstOfMonth, _LastOfMonth}}, Q} ->
            PrevMonth = previous_month_timestamp(FirstOfMonth),
            NextAccount = {AccountId, PrevMonth, FirstOfMonth},
            {NextAccount, queue:in(NextAccount, Q)};
        {'empty', _} when not IsRetPassed ->
            'retention_passed';
        {'empty', _} ->
            'empty'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec worker_by_ref(reference(), [reference()]) -> [reference()].
worker_by_ref(Ref, WRefs) ->
    [R || R <- WRefs,
          R =:= Ref
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec populate_queue(ne_binaries(), gregorian_seconds()) -> queue:queue().
-spec populate_queue(ne_binaries(), gregorian_seconds(), gregorian_seconds()) -> queue:queue().
populate_queue(AccountIds, RetentionSeconds) ->
    Diff = kz_util:current_tstamp() - RetentionSeconds,
    FirstOfMonth = previous_month_timestamp(Diff),
    Props = [{AccountId, FirstOfMonth, Diff}
             || AccountId <- AccountIds
            ],
    queue:from_list(Props).

populate_queue(AccountIds, LastOfMonth, _RetentionSeconds) ->
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(LastOfMonth),
    FirstOfMonth = calendar:datetime_to_gregorian_seconds({{Year, Month, 1}, {0, 0, 0}}),
    Props = [{AccountId, FirstOfMonth, LastOfMonth}
             || AccountId <- AccountIds
            ],
    queue:from_list(Props).

-spec remove_account_from_queue(next_account(), queue:queue()) -> queue:queue().
remove_account_from_queue(Key, Queue) ->
    Fun = fun(Item) when Item =:= Key -> 'false';
             (_) -> 'true'
          end,
    queue:filter(Fun, Queue).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec previous_month_timestamp(gregorian_seconds()) -> gregorian_seconds().
previous_month_timestamp(TimeStamp) ->
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(TimeStamp),
    {PrevYear, PrevMonth} = kazoo_modb_util:prev_year_month(Year, Month),
    calendar:datetime_to_gregorian_seconds({{PrevYear, PrevMonth, 1}, {0, 0, 0}}).
