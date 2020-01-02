%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author Hesaam Farhang
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
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
        kapps_config:get_integer(?VM_CONFIG_CAT, [?KEY_VOICEMAIL, <<"migrate_max_worker">>], 10)).

-type worker() :: {pid(), next_account()}.
-type workers() :: [worker()].
-record(state, {max_worker = ?MAX_PROCESS :: integer()
               ,workers = [] :: workers()
               ,account_ids = [] :: kz_term:ne_binaries()
               ,retention_passed = 'false' :: boolean()
               ,total_account = 0 :: non_neg_integer()
               ,total_processed = 0 :: non_neg_integer()
               ,total_succeeded = 0 :: non_neg_integer()
               ,total_failed = 0 :: non_neg_integer()
               ,total_account_failed = 0 :: non_neg_integer()
               ,failed_accounts = [] :: kz_term:proplist()
               ,calling_process = 'undefined' :: kz_term:api_pid()
               ,timer_ref :: kz_term:api_reference()
               ,account_queue :: queue:queue() | 'undefined'
               }).
-type state() :: #state{}.

-type next_account_ret() :: {next_account() | 'account_hit_retention', queue:queue()} |
                            'empty' |
                            'retention_passed' |
                            {'continue', queue:queue()}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%------------------------------------------------------------------------------
-spec start() -> kz_types:startlink_ret().
start() ->
    gen_server:start(?SERVER, [], []).

-spec start(pid()) -> kz_types:startlink_ret().
start(Pid) ->
    gen_server:start(?SERVER, [Pid], []).

-spec stop(pid()) -> 'ok'.
stop(Server) ->
    gen_server:call(Server, 'stop').

%% Workers API
-spec account_is_done(pid(), kz_term:ne_binary(), kz_time:gregorian_seconds(), kz_time:gregorian_seconds()) -> 'ok'.
account_is_done(Server, AccountId, FirstOfMonth, LastOfMonth) ->
    gen_server:call(Server, {'account_is_done', {AccountId, FirstOfMonth, LastOfMonth, 'normal'}}).

-spec account_maybe_failed(pid(), kz_term:ne_binary(), kz_time:gregorian_seconds(), kz_time:gregorian_seconds(), any()) -> 'ok'.
account_maybe_failed(_Server, _AccountId, _FirstOfMonth, _LastOfMonth, 'timeout') ->
    ?SUP_LOG_WARNING("ignoring 'timeout' error for worker of account ~s", [_AccountId]);
account_maybe_failed(Server, AccountId, FirstOfMonth, LastOfMonth, Reason) ->
    gen_server:call(Server, {'account_is_done', {AccountId, FirstOfMonth, LastOfMonth, Reason}}).

-spec update_stats(pid(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
update_stats(Server, AccountId, Stats) ->
    gen_server:call(Server, {'update_stats', {AccountId, Stats}}).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init(any()) -> {'ok', state()} |
          {'stop', state()}.
init([]) -> init('undefined');
init(Pid) ->
    _ = process_flag('trap_exit', 'true'),
    kz_log:put_callid(?SERVER),
    lager:debug("started ~s", [?SERVER]),
    case kapps_util:get_all_accounts('raw') of
        [] ->
            ?SUP_LOG_ERROR(":: no account found, going down", []),
            {'stop', #state{}};
        Ids ->
            ?SUP_LOG_WARNING(":: BEGINNING MIGRATING VOICEMAIL FOR ~b ACCOUNTS", [length(Ids)]),
            AccountIds = kz_term:shuffle_list(Ids),
            %% first start migrating messages in retention duration
            Queue = populate_queue(AccountIds, kz_time:now_s()),
            {'ok', #state{account_ids = AccountIds
                         ,total_account = length(AccountIds)
                         ,account_queue = Queue
                         ,timer_ref = cleanup_account_timer()
                         ,calling_process = Pid
                         }}
    end.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call({'account_is_done', {_ ,_ ,_ ,Reason}=MaybeDone}, _From, #state{account_queue = Queue
                                                                           ,total_account_failed = TotalAccFailed
                                                                           ,failed_accounts = FailedAccounts
                                                                           }=State) ->
    {IsFailed, AccountId, NewQueue} = maybe_remove_account_from_queue(MaybeDone, Queue),
    NewState = case IsFailed of
                   'true' ->
                       State#state{account_queue = NewQueue
                                  ,total_account_failed = TotalAccFailed + 1
                                  ,failed_accounts = [{AccountId, Reason} | FailedAccounts]
                                  };
                   'false' ->
                       State#state{account_queue = NewQueue}
               end,
    {'reply', 'ok', NewState};
handle_call({'update_stats', {_AccountId, Props}}, _From, #state{total_processed = TotalProcessed
                                                                ,total_succeeded = TotalSucceeded
                                                                ,total_failed = TotalFailed
                                                                }=State) ->
    CycleTotalProcessed = props:get_value(<<"total_processed">>, Props, 0),
    CycleTotalSucceeded = props:get_value(<<"total_succeeded">>, Props, 0),
    CycleTotalFailed = props:get_value(<<"total_failed">>, Props, 0),

    NewState = State#state{total_processed = TotalProcessed + CycleTotalProcessed
                          ,total_succeeded = TotalSucceeded + CycleTotalSucceeded
                          ,total_failed = TotalFailed + CycleTotalFailed
                          },
    {'reply', 'ok', NewState};
handle_call('stop', _From, #state{calling_process = 'undefined'}=State) ->
    lager:debug("~p has been stopped", [?MODULE]),
    {'stop', 'normal',  State};
handle_call('stop', _From, #state{calling_process = Pid}=State) ->
    lager:debug("~p has been stopped", [?MODULE]),
    Pid ! 'done',
    {'stop', 'normal',  State};
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'timeout', _Ref, _Msg}, #state{account_ids = []
                                           ,workers = []
                                           ,retention_passed = 'true'
                                           ,calling_process = Pid
                                           }=State) ->
    ?SUP_LOG_WARNING(":: voicemail migration is finished", []),
    print_summary(State),
    case is_pid(Pid) of
        'false' -> 'ok';
        'true' ->
            Pid ! 'done'
    end,
    {'stop', 'normal', State};
handle_info({'timeout', _Ref, _Msg}, #state{account_ids = []
                                           ,workers = Workers
                                           ,retention_passed = 'true'
                                           }=State) ->
    lager:debug(":: migration is almost done, waiting for ~b workers to done", [length(Workers)]),
    {'noreply', State};
handle_info({'timeout', _Ref, _Msg}, #state{max_worker = Limit
                                           ,workers = Workers
                                           }=State)
  when length(Workers) < Limit ->
    NewState = spawn_worker(State),
    {'noreply', NewState#state{timer_ref = cleanup_account_timer()}};
handle_info({'timeout', _Ref, _Msg}, State) ->
    {'noreply', State#state{timer_ref = cleanup_account_timer()}};
handle_info({'EXIT', Pid, 'normal'}, #state{workers = Workers}=State) ->
    lager:debug("worker ~p terminated normally", [Pid]),
    {'noreply', State#state{workers = props:delete(Pid, Workers)
                           ,timer_ref = cleanup_account_timer()
                           }};
handle_info({'EXIT', Pid, Reason}, #state{workers = Workers}=State) ->
    lager:error("worker ~p crashed: ~p", [Pid, Reason]),
    {'noreply', State#state{workers = props:delete(Pid, Workers)
                           ,timer_ref = cleanup_account_timer()
                           }};
handle_info(_Info, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("~s terminating: ~p", [?SERVER, _Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec spawn_worker(state()) -> state().
spawn_worker(#state{account_queue = Queue
                   ,retention_passed = IsRetPassed
                   ,workers = Workers
                   }=State) ->
    NextAccount = get_next_account(Queue, Workers, IsRetPassed),
    maybe_spawn_worker(State, NextAccount).

-spec maybe_spawn_worker(state(), next_account_ret()) -> state().
maybe_spawn_worker(#state{account_ids = AccountIds
                         }=State, 'retention_passed') ->
    ?SUP_LOG_WARNING(":: all voicemails in retention duration are migrated, beginning a migrating older voicemails", []),
    State#state{retention_passed = 'true'
               ,account_queue = populate_queue(AccountIds)
               };
maybe_spawn_worker(State, 'empty') ->
    %% migration is done waiting for workers to finish their jobs
    State#state{account_ids = []
               ,retention_passed = 'true'
               };
maybe_spawn_worker(State, {'continue', NewQ}) ->
    %% there is a worker for this account, adding this account to the end of the queue
    State#state{account_queue = NewQ};
maybe_spawn_worker(State, {'account_hit_retention', NewQ}) ->
    State#state{account_queue = NewQ};
maybe_spawn_worker(#state{workers = Workers
                         ,max_worker = _Limit
                         ,timer_ref = Ref
                         }=State
                  ,{{AccountId, _, _} = NextAccount, NewQ}
                  ) ->
    CallId = make_callid(Ref, AccountId),
    Self = self(),
    Pid = erlang:spawn_link(fun () ->
                                    _ = kz_log:put_callid(CallId),
                                    kvm_migrate_account:start_worker(NextAccount, Self)
                            end),
    lager:debug(":: started ~p (~b/~b) to process account ~s"
               ,[Pid, length(Workers) + 1, _Limit, AccountId]),
    State#state{workers = [{Pid, NextAccount} | Workers]
               ,account_queue = NewQ
               }.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec make_callid(reference(), kz_term:ne_binary()) -> kz_term:ne_binary().
make_callid(Ref, AccountId) ->
    Id = ref_to_id(Ref),
    <<"task_", AccountId/binary, "_", Id/binary>>.

-spec ref_to_id(reference()) -> kz_term:ne_binary().
ref_to_id(Ref) ->
    Bin = list_to_binary(io_lib:format("~p", [Ref])),
    Start = <<"#Ref<">>,
    StartSize = byte_size(Start),
    Size = byte_size(Bin) - StartSize - 1,
    <<Start:StartSize/binary, Id:Size/binary, ">">> = Bin,
    Id.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec cleanup_account_timer() -> reference().
cleanup_account_timer() ->
    erlang:start_timer(?TIME_BETWEEN_ACCOUNT_CRAWLS, self(), 'ok').

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_next_account(queue:queue(), workers(), boolean()) ->
          next_account_ret().
get_next_account(Queue, Workers, IsRetPassed) ->
    case queue:out(Queue) of
        {{'value', {AccountId, _, _} = NextAccount}, Q} ->
            WorkerNextAccount = [WNA
                                 || {_Pid, {WorkerAccountId, _, _} = WNA} <- Workers,
                                    WorkerAccountId == AccountId
                                ],
            get_next(Queue, WorkerNextAccount, NextAccount, Q, IsRetPassed);
        {'empty', _} when not IsRetPassed ->
            'retention_passed';
        {'empty', _} ->
            'empty'
    end.

-spec get_next(queue:queue(), [next_account()], next_account(), queue:queue(), boolean()) ->
          next_account_ret().
get_next(_, [], {AccountId, FirstOfMonth, _LastOfMonth}, Q, 'false') ->
    PrevMonth = previous_month_timestamp(FirstOfMonth),
    case PrevMonth < kvm_util:retention_seconds(AccountId) of
        'true' ->
            {'account_hit_retention', Q};
        'false' ->
            NextAccount = {AccountId, PrevMonth, FirstOfMonth},
            {NextAccount, queue:in(NextAccount, Q)}
    end;
get_next(_, [], NextAccount, Q, 'true') ->
    {NextAccount, queue:in(NextAccount, Q)};
get_next(_Queue, _WorkerNextAccount, NextAccount, Q, _IsRetPassed) ->
    %% there is a worker for this account, adding this account to the end of the queue
    {'continue', queue:in(NextAccount, Q)}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec populate_queue(kz_term:ne_binaries()) -> queue:queue().
populate_queue(AccountIds) ->
    Props = [{AccountId, 'undefined', 'undefined'}
             || AccountId <- AccountIds
            ],
    queue:from_list(Props).

-spec populate_queue(kz_term:ne_binaries(), kz_time:gregorian_seconds()) -> queue:queue().
populate_queue(AccountIds, LastOfMonth) ->
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(LastOfMonth),
    FirstOfMonth = calendar:datetime_to_gregorian_seconds({{Year, Month, 1}, {0, 0, 0}}),
    Props = [{AccountId, FirstOfMonth, LastOfMonth}
             || AccountId <- AccountIds
            ],
    queue:from_list(Props).

maybe_remove_account_from_queue({AccountId, FirstOfMonth, LastOfMonth, 'normal'}, Queue) ->
    ?SUP_LOG_WARNING(":: account ~s is migrated"
                    ,[AccountId]),
    {'false', AccountId, remove_account_from_queue({AccountId, FirstOfMonth, LastOfMonth}, Queue)};
maybe_remove_account_from_queue({AccountId, FirstOfMonth, LastOfMonth, _Reason}, Queue) ->
    ?SUP_LOG_ERROR(":: account ~s migration failed: ~p", [AccountId, _Reason]),
    {'true', AccountId, remove_account_from_queue({AccountId, FirstOfMonth, LastOfMonth}, Queue)}.

-spec remove_account_from_queue(next_account(), queue:queue()) -> queue:queue().
remove_account_from_queue(Key, Queue) ->
    Fun = fun(Item) when Item =:= Key -> 'false';
             (_) -> 'true'
          end,
    queue:filter(Fun, Queue).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec previous_month_timestamp(kz_time:gregorian_seconds()) -> kz_time:gregorian_seconds().
previous_month_timestamp(TimeStamp) ->
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(TimeStamp),
    {PrevYear, PrevMonth} = kazoo_modb_util:prev_year_month(Year, Month),
    calendar:datetime_to_gregorian_seconds({{PrevYear, PrevMonth, 1}, {0, 0, 0}}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec print_summary(state()) -> 'ok'.
print_summary(#state{total_account = TotalAccount
                    ,total_account_failed = TotalAccFailed
                    ,failed_accounts = FailedAccounts
                    ,total_processed = TotalMsgsProcessed
                    ,total_succeeded = TotalSucceeded
                    ,total_failed = TotalFailed
                    }) ->

    io:format("~n~n:: VOICEMAIL MIGRATION SUMMARY :: ~n~n"),
    io:format("Total Account processed: ~b~n", [TotalAccount]),
    io:format("Total voicemail processed: ~b~n", [TotalMsgsProcessed]),
    io:format("Total succeeded: ~b~n", [TotalSucceeded]),
    io:format("Total failed: ~b~n", [TotalFailed]),
    io:format("Total Account with failed migrated messages: ~b~n~n", [TotalAccFailed]),
    io:format("Accounts with failed migrated messages:~n~n"),
    _ = [io:format(" ~s: ~s~n", [AccountId, Reason]) || {AccountId, Reason} <- FailedAccounts],
    'ok'.
