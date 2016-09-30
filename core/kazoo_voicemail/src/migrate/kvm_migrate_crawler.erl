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
-export([start/0, start/1, stop/1
        ]).

%%% API used by Workers
-export([account_is_done/4
        ,account_maybe_failed/5
        ,update_stats/3
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
        kapps_config:get_integer(?CF_CONFIG_CAT, [?KEY_VOICEMAIL, <<"migrate_max_worker">>], 3)).

-define(TIME_BETWEEN_SPAWN_ON_STARTUP, 100).
-define(TIME_BETWEEN_ACCOUNT_CRAWLS,
        kapps_config:get_integer(?CF_CONFIG_CAT, [?KEY_VOICEMAIL, <<"migrate_interaccount_delay_ms">>], 5 * ?MILLISECONDS_IN_SECOND)).


-define(MIGRATE_WORKER_SUP, ?SUPER('kvm_migrate_worker_sup')).

-record(state, {max_worker = ?MAX_PROCESS :: integer()
               ,workers = [] :: kz_proplist()
               ,account_ids = [] :: ne_binaries()
               ,retention_passed = 'false' :: boolean()
               ,total_messages = 0 :: non_neg_integer()
               ,total_processed = 0 :: non_neg_integer()
               ,total_succeeded = 0 :: non_neg_integer()
               ,total_failed = 0 :: non_neg_integer()
               ,total_account_failed = 0 :: non_neg_integer()
               ,failed_accounts = [] :: ne_binaries()
               ,calling_process = 'undefined' :: api_pid()
               ,timer_ref :: reference()
               ,retention_seconds :: gregorian_seconds()
               ,account_queue :: queue:queue()
               }).
-type state() :: #state{}.

-type next_account_ret() :: {next_account(), queue:queue()} |
                                'empty' |
                                'retention_passed' |
                                'continue'.

-define(DEBUG(Format, Args),
        begin
            lager:debug(Format, Args),
            io:format("\n" ++ Format ++ "\n", Args)
        end
       ).

-define(WARNING(Format, Args),
        begin
            lager:warning(Format, Args),
            io:format("\n" ++ Format ++ "\n", Args)
        end
       ).

-define(ERROR(Format, Args),
        begin
            lager:error(Format, Args),
            io:format("\n" ++ Format ++ "\n", Args)
        end
       ).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start() -> startlink_ret().
start() ->
    gen_server:start(?SERVER, [], []).

-spec start(pid()) -> startlink_ret().
start(Pid) ->
    gen_server:start(?SERVER, [Pid], []).

-spec stop(pid()) -> 'ok'.
stop(Server) ->
    gen_server:call(Server, 'stop').

%% Workers API
-spec account_is_done(pid(), ne_binary(), gregorian_seconds(), gregorian_seconds()) -> 'ok'.
account_is_done(Server, AccountId, FirstOfMonth, LastOfMonth) ->
    gen_server:call(Server, {'account_is_done', {AccountId, FirstOfMonth, LastOfMonth, 'normal'}}).

-spec account_maybe_failed(pid(), ne_binary(), gregorian_seconds(), gregorian_seconds(), any()) -> 'ok'.
account_maybe_failed(_Server, _AccountId, _FirstOfMonth, _LastOfMonth, 'timeout') ->
    ?WARNING("ignoring 'timeout' error for worker of account ~s", [_AccountId]);
account_maybe_failed(Server, AccountId, FirstOfMonth, LastOfMonth, Reason) ->
    gen_server:call(Server, {'account_is_done', {AccountId, FirstOfMonth, LastOfMonth, Reason}}).

-spec update_stats(pid(), ne_binary(), kz_proplist()) -> 'ok'.
update_stats(Server, AccountId, Stats) ->
    gen_server:call(Server, {'update_stats', {AccountId, Stats}}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%--------------------------------------------------------------------
-spec init(any()) -> {'ok', state()} |
                  {'stop', state()}.
init([]) -> init('undefined');
init(Pid) ->
    _ = process_flag('trap_exit', 'true'),
    kz_util:put_callid(?SERVER),
    lager:debug("started ~s", [?SERVER]),
    case kapps_util:get_all_accounts('raw') of
        [] ->
            ?ERROR("********** no account found, going down **********", []),
            {'stop', #state{}};
        Ids ->
            AccountIds = kz_util:shuffle_list(Ids),
            %% first start migrating messages in retention durations
            RetentionSeconds = kvm_util:retention_seconds(),
            Queue = populate_queue(AccountIds, kz_util:current_tstamp(), RetentionSeconds),
            {'ok', #state{account_ids = AccountIds
                         ,account_queue = Queue
                         ,retention_seconds = RetentionSeconds
                         ,timer_ref = cleanup_startup_timer()
                         ,calling_process = Pid
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
    ?WARNING("########## account ~s is done ##########"
            ,[AccountId]),
    NewQueue = remove_account_from_queue({AccountId, FirstOfMonth, LastOfMonth}, Queue),
    {'reply', 'ok', State#state{account_queue = NewQueue}};
handle_call({'account_is_done', {AccountId, FirstOfMonth, LastOfMonth, Reason}}, _From, #state{account_queue = Queue
                                                                                              ,total_account_failed = TotalAccFailed
                                                                                              ,failed_accounts = FailedAccounts
                                                                                              }=State) ->
    ?ERROR("********** account ~s failed: ~p **********", [AccountId, Reason]),
    NewQueue = remove_account_from_queue({AccountId, FirstOfMonth, LastOfMonth}, Queue),
    {'reply', 'ok', State#state{account_queue = NewQueue
                               ,total_account_failed = TotalAccFailed + 1
                               ,failed_accounts = [AccountId | FailedAccounts]
                               }};
handle_call({'update_stats', {_AccountId, Props}}, _From, #state{total_processed = TotalProcessed
                                                                ,total_succeeded = TotalSucceeded
                                                                ,total_failed = TotalFailed
                                                                }=State) ->
    CycleTotalProcessed = props:get_value(<<"total_processed">>, Props, 0),
    CycleTotalSucceeded = props:get_value(<<"total_succeeded">>, Props, 0),
    CycleTotalFailed = props:get_value(<<"total_failed">>, Props, 0),

    {'reply', 'ok', State#state{total_processed = TotalProcessed + CycleTotalProcessed
                               ,total_succeeded = TotalSucceeded + CycleTotalSucceeded
                               ,total_failed = TotalFailed + CycleTotalFailed
                               }};
handle_call('stop', _From, #state{calling_process = 'undefined'}=State) ->
    {'stop', 'normal',  State};
handle_call('stop', _From, #state{calling_process = Pid}=State) ->
    lager:debug("~p has been stopped", [?MODULE]),
    Pid ! 'done',
    {'stop', 'normal',  State};
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info({'timeout', Ref, 'startup'}, #state{max_worker = Limit
                                               ,timer_ref = Ref
                                               }=State) when Limit > 0 ->
    % io:format("~n Startup Limit ~p~n~n", [Limit]),
    NewState = spawn_worker(State),
    {'noreply', NewState#state{timer_ref = cleanup_startup_timer()}};
handle_info({'timeout', Ref, 'startup'}, #state{max_worker = Limit
                                               ,timer_ref = Ref
                                               }=State) when Limit =< 0 ->
    % io:format("~n Under Startup Limit ~p~n~n", [Limit]),
    {'noreply', State#state{timer_ref = cleanup_startup_timer()}};
handle_info({'timeout', Ref, _Msg}, #state{account_ids = []
                                          ,timer_ref = Ref
                                          ,workers = []
                                          ,retention_passed = 'true'
                                          ,calling_process = Pid
                                          }=State) ->
    ?WARNING("########## voicemail migration is finished ##########", []),
    case is_pid(Pid) of
        'false' -> 'ok';
        'true' ->
            Pid ! 'done'
    end,
    {'stop', 'normal', State};
handle_info({'timeout', Ref, _Msg}, #state{account_ids = []
                                          ,timer_ref = Ref
                                          ,workers = Workers
                                          ,retention_passed = 'true'
                                          }=State) ->
    lager:warning("########## migration is almost done, waiting for ~b workers to done ##########", [length(Workers)]),
    {'noreply', State};
handle_info({'timeout', Ref, _Msg}, #state{max_worker = Limit
                                          ,timer_ref = Ref
                                          }=State) when Limit > 0 ->
    % io:format("~n Normal Limit ~p~n~n", [Limit]),
    NewState = spawn_worker(State),
    {'noreply', NewState#state{timer_ref = cleanup_account_timer()}};
handle_info({'timeout', TimerRef, _Msg}, #state{max_worker = Limit
                                               ,timer_ref = TimerRef
                                               }=State) when Limit =< 0 ->
   % io:format("maximum number of migration process reached", []),
    {'noreply', State};
handle_info({'EXIT', Pid, 'normal'}, #state{max_worker = Limit
                                           ,workers = Workers
                                           }=State) ->
    lager:debug("worker ~p terminated normally", [Pid]),
    {'noreply', State#state{workers = props:delete(Pid, Workers)
                           ,timer_ref = cleanup_account_timer()
                           ,max_worker = Limit + 1
                           }};
handle_info({'EXIT', Pid, Reason}, #state{max_worker = Limit
                                         ,workers = Workers
                                         }=State) ->
    lager:error("worker ~p crashed: ~p", [Pid, Reason]),
    {'noreply', State#state{workers = props:delete(Pid, Workers)
                           ,timer_ref = cleanup_account_timer()
                           ,max_worker = Limit + 1
                           }};
handle_info(_Info, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
    {'noreply', State}.

-spec spawn_worker(state()) -> state().
-spec maybe_spawn_worker(state(), next_account_ret()) -> state().
spawn_worker(#state{account_queue = Queue
                   ,retention_passed = IsRetPassed
                   ,workers = Workers
                   }=State) ->
    NextAccount = get_next_account(Queue, Workers, IsRetPassed),
    maybe_spawn_worker(State, NextAccount).

maybe_spawn_worker(#state{account_ids = AccountIds
                         ,retention_seconds = RetentionSeconds
                         }=State, retention_passed) ->
    ?WARNING("########## all voicemails in retention duration are migrated, beginning a new cycle for migrating older voicemails ##########", []),
    State#state{retention_passed = 'true'
                ,account_queue = populate_queue(AccountIds, RetentionSeconds)
                ,max_worker = ?MAX_PROCESS
                };
maybe_spawn_worker(State, 'empty') ->
    %% migration is done waiting for workers to finish their jobs
    State#state{account_ids = []
               ,retention_passed = 'true'
               };
maybe_spawn_worker(State, 'continue') ->
    %% next account is in front of the queue, wait for another cycle
    State;
maybe_spawn_worker(#state{workers = Workers
                         ,max_worker = Limit
                         ,timer_ref = Ref
                         }=State, {{AccountId, _, _} = NextAccount, NewQ}) ->
    CallId = make_callid(Ref, AccountId),
    Self = self(),
    Pid = erlang:spawn_link(fun () ->
                                    _ = kz_util:put_callid(CallId),
                                    kvm_migrate_account:start_worker(NextAccount, Self)
                            end),
    MaxWorker = length(Workers) + Limit,
    WorkersCount = length(Workers) + 1,
    ?WARNING("########## started ~p (~b/~b) to process account ~s ##########"
            ,[Pid, WorkersCount, MaxWorker, AccountId]),
    State#state{max_worker = Limit - 1
               ,workers = [{Pid, NextAccount} | Workers]
               ,account_queue = NewQ
               }.

-spec make_callid(reference(), ne_binary()) -> ne_binary().
make_callid(Ref, AccountId) ->
    Id = ref_to_id(Ref),
    <<"task_", AccountId/binary, "_", Id/binary>>.

-spec ref_to_id(reference()) -> ne_binary().
ref_to_id(Ref) ->
    Bin = list_to_binary(io_lib:format("~p", [Ref])),
    Start = <<"#Ref<">>,
    StartSize = byte_size(Start),
    Size = byte_size(Bin) - StartSize - 1,
    <<Start:StartSize/binary, Id:Size/binary, ">">> = Bin,
    Id.

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
-spec get_next_account(queue:queue(), kz_proplist(), boolean()) -> next_account_ret().
get_next_account(Queue, Workers, IsRetPassed) ->
    case queue:out(Queue) of
        {{'value', NextAccount}, Q} ->
            get_next(Queue, props:get_value(NextAccount, Workers), NextAccount, Q, IsRetPassed);
        {'empty', _} when not IsRetPassed ->
            'retention_passed';
        {'empty', _} ->
            'empty'
    end.

-spec get_next(queue:queue(), next_account_ret(), next_account_ret(), queue:queue(), boolean()) ->
                      next_account_ret().
get_next(_Queue, NextAccount, NextAccount, _Q, _IsRetPassed) ->
    % io:format("~n~nfront matched with workers ~p~n~n~n~n~n", [NextAccount]),
    'continue';
get_next(_, _, {AccountId, FirstOfMonth, _LastOfMonth}, Q, 'true') ->
    PrevMonth = previous_month_timestamp(FirstOfMonth),
    NextAccount = {AccountId, PrevMonth, FirstOfMonth},
    {NextAccount, queue:in(NextAccount, Q)};
get_next(_, _, NextAccount, Q, 'false') ->
    {NextAccount, queue:in(NextAccount, Q)}.


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
