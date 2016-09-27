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

-define(TIME_BETWEEN_ACCOUNT_CRAWLS,
        kapps_config:get_integer(?CF_CONFIG_CAT, [?KEY_VOICEMAIL, <<"migrate_interaccount_delay_ms">>], 10 * ?MILLISECONDS_IN_SECOND)).


-define(MIGRATE_WORKER_SUP, ?SUPER('kvm_migrate_worker_sup')).

-record(state, {max_worker = ?MAX_PROCESS :: integer()
               ,worker_refs = [] :: [reference()]
               ,timer_ref = cleanup_account_timer() :: reference()
               ,account_ids = [] :: ne_binaries()
               ,retention_passed = 'false' :: boolean()
               ,retention_startkey = retention_range_startkey() :: gregorian_seconds()
               }).
-type state() :: #state{}.

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
            lager:error("no account found, going down", []),
            _ = kz_util:spawn(fun kvm_migrate_sup:stop/0, []),
            'ignore';
        Ids ->
            self() ! {'start_worker_sup', MigrateSuper},
            AccountIds = kz_util:shuffle_list(Ids),
            {'ok', #state{account_ids = AccountIds
                         }}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
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
handle_info({'start_worker_sup', MigrateSuper}, #state{} = State) ->
    {'ok', Pid} = supervisor:start_child(MigrateSuper, ?MIGRATE_WORKER_SUP),
    link(Pid),
    {'noreply', State#state{timer_ref = cleanup_account_timer()
                           }};
handle_info({'timeout', TimerRef, _Msg}, #state{account_ids = []
                                               ,timer_ref = TimerRef
                                               ,worker_refs = []
                                               ,retention_passed = 'true'
                                               }=State) ->
    lager:warning("voicemail migration is finished, going down"),
    _ = kz_util:spawn(fun kvm_migrate_sup:stop/0, []),
    {'noreply', State};
handle_info({'timeout', TimerRef, _Msg}, #state{account_ids = []
                                               ,timer_ref = TimerRef
                                               ,worker_refs = WRefs
                                               ,retention_passed = 'true'
                                               }=State) ->
    lager:warning("voicemail migration is finished, waiting for ~b workers to done"
                 ,[length(WRefs)]),
    {'noreply', State};
handle_info({'timeout', TimerRef, _Msg}, #state{account_ids = []
                                               ,timer_ref = TimerRef
                                               ,retention_passed = 'false'
                                               }=State) ->
    lager:warning("all voicemails in retention duration are migrated, beginning a new cycle for migrating older voicemails"),
    {'noreply', State#state{account_ids = kapps_util:get_all_accounts('raw')
                           ,retention_passed = 'true'
                           ,timer_ref = cleanup_account_timer()
                           }};
handle_info({'timeout', TimerRef, _Msg}, #state{max_worker = Limit
                                               ,worker_refs = WRefs
                                               ,timer_ref = TimerRef
                                               ,account_ids = [AccountId | AccountIds]
                                               ,retention_passed = IsRetPassed
                                               ,retention_startkey = RetStartKey
                                               }=State) when Limit > 0 ->
    Args = get_next_account_to_process(AccountId, IsRetPassed, RetStartKey),
    {'ok', Pid} = kvm_migrate_worker_sup:process_account(Args),
    Ref = erlang:monitor('process', Pid),
    lager:warning("started a new worker ~p (~b/~b) to process account ~s", [Pid, Limit - 1, Limit, AccountId]),
    {'noreply', State#state{max_worker = Limit - 1
                           ,worker_refs = [Ref | WRefs]
                           ,account_ids = AccountIds
                           }};
handle_info({'timeout', TimerRef, _Msg}, #state{max_worker = Limit
                                               ,timer_ref = TimerRef
                                               }=State) when Limit =< 0 ->
    lager:warning("maximum number of migration process reached"),
    {'noreply', State};
handle_info({'DOWN', Ref, 'process', _Pid, _Info}, #state{max_worker = Limit
                                                         ,worker_refs = WRefs
                                                         }=State) ->
    lager:warning("received down msg from worker ~p with reason ~p"
                 ,[_Pid, _Info]),
    case worker_by_ref(Ref, State) of
        [] ->
            %% Not ours
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

-spec get_next_account_to_process(ne_binary(), boolean(), gregorian_seconds()) -> next_account().
get_next_account_to_process(AccountId, 'false', RetStartKey) ->
    {AccountId, RetStartKey};
get_next_account_to_process(AccountId, 'true', _RetStartKey) ->
    AccountId.

-spec worker_by_ref(reference(), state()) -> [reference()].
worker_by_ref(Ref, State) ->
    [R || R <- State#state.worker_refs,
          R =:= Ref
    ].

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

retention_range_startkey() ->
    Now = kz_util:current_tstamp(),
    Retention = Now - kvm_util:retention_seconds(),
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Retention),
    calendar:datetime_to_gregorian_seconds({{Year, Month, 1}, {0, 0, 0}}).
