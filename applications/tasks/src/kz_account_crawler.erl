%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2017, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%     Hesaam Farhang
%%%     Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_account_crawler).
-behaviour(gen_server).

-export([start_link/0]).
-export([stop/0, check/1]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("tasks.hrl").

-define(SERVER, ?MODULE).

-record(state, {cleanup_ref = cleanup_cycle_timer() :: reference()
               ,account_ids = [] :: ne_binaries()
               }).
-type state() :: #state{}.

-define(TIME_BETWEEN_CRAWLS,
        kapps_config:get_integer(?CONFIG_CAT, <<"interaccount_delay_ms">>, 10 * ?MILLISECONDS_IN_SECOND)).

-define(TIME_BETWEEN_WHOLE_CRAWLS,
        kapps_config:get_integer(?CONFIG_CAT, <<"cycle_delay_time_ms">>, 5 * ?MILLISECONDS_IN_MINUTE)).

-define(SHOULD_CRAWL_FOR_FIRST_OCCURRENCE,
        kapps_config:get_is_true(?CONFIG_CAT, <<"should_crawl_for_first_occurrence">>, 'true')).

-define(SHOULD_CRAWL_FOR_LOW_BALANCE,
        kapps_config:get_is_true(?CONFIG_CAT, <<"should_crawl_for_low_balance">>, 'true')).

-define(LOW_BALANCE_REPEAT,
        kapps_config:get_integer(?CONFIG_CAT, <<"low_balance_repeat_s">>, 1 * ?SECONDS_IN_DAY)).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_server:start_link(?SERVER, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:cast(?SERVER, stop).

-spec check(ne_binary()) -> 'ok'.
check(Account)
  when is_binary(Account) ->
    AccountId = kz_util:format_account_id(Account),
    case kz_datamgr:open_doc(?KZ_ACCOUNTS_DB, AccountId) of
        {'ok', AccountJObj} ->
            process_account(AccountId, kz_doc:account_db(AccountJObj), AccountJObj);
        {'error', _R} ->
            lager:warning("unable to open account definition for ~s: ~p", [AccountId, _R])
    end;
check(Account) ->
    check(kz_term:to_binary(Account)).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%--------------------------------------------------------------------
-spec init([]) -> {ok, state()}.
init([]) ->
    kz_util:put_callid(?SERVER),
    lager:debug("started ~s", [?SERVER]),
    {'ok', #state{}}.

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
handle_cast(stop, State) ->
    lager:debug("crawler has been stopped"),
    {stop, normal, State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info({timeout, Ref, _Msg}, #state{cleanup_ref = Ref
                                        ,account_ids = []
                                        }=State) ->
    NewState =
        case kz_datamgr:all_docs(?KZ_ACCOUNTS_DB) of
            {'ok', JObjs} ->
                IDs = [ID || JObj <- JObjs,
                             ?MATCH_ACCOUNT_RAW(ID) <- [kz_doc:id(JObj)]
                      ],
                lager:debug("beginning crawling accounts"),
                State#state{cleanup_ref = cleanup_timer()
                           ,account_ids = kz_term:shuffle_list(IDs)
                           };
            {error, _R} ->
                lager:warning("unable to list all docs in ~s: ~p", [?KZ_ACCOUNTS_DB, _R]),
                State#state{cleanup_ref = cleanup_cycle_timer()}
        end,
    {noreply, NewState};

handle_info({timeout, Ref, _Msg}, #state{cleanup_ref = Ref
                                        ,account_ids = [AccountId]
                                        }=State) ->
    _ = crawl_account(AccountId),
    lager:info("account crawler completed a full crawl"),
    {noreply, State#state{cleanup_ref = cleanup_cycle_timer()
                         ,account_ids = []
                         }};

handle_info({timeout, Ref, _Msg}, #state{cleanup_ref = Ref
                                        ,account_ids = [AccountId | AccountIds]
                                        }=State) ->
    _ = crawl_account(AccountId),
    {noreply, State#state{cleanup_ref = cleanup_timer()
                         ,account_ids = AccountIds
                         }};

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

-spec cleanup_timer() -> reference().
cleanup_timer() ->
    erlang:start_timer(?TIME_BETWEEN_CRAWLS, self(), 'ok').

-spec cleanup_cycle_timer() -> reference().
cleanup_cycle_timer() ->
    erlang:start_timer(?TIME_BETWEEN_WHOLE_CRAWLS, self(), 'ok').

-spec crawl_account(ne_binary()) -> ok.
crawl_account(AccountId) ->
    lager:debug("crawling account ~s", [AccountId]),
    %% do not open the account def in the account db or we will
    %% be wasting bigcouch's file descriptors
    OpenResult = kz_datamgr:open_doc(?KZ_ACCOUNTS_DB, AccountId),
    check_then_process_account(AccountId, OpenResult).

-spec check_then_process_account(ne_binary(), {'ok', kz_account:doc()} | {'error',any()}) -> 'ok'.
check_then_process_account(AccountId, {'ok', AccountJObj}) ->
    case kz_doc:is_soft_deleted(AccountJObj)
        orelse not kz_account:is_enabled(AccountJObj) of
        'true' ->
            lager:debug("not processing account ~p (soft-destroyed)", [AccountId]);
        'false' ->
            process_account(AccountId, kz_doc:account_db(AccountJObj), AccountJObj)
    end;
check_then_process_account(AccountId, {'error', _R}) ->
    lager:warning("unable to open account definition for ~s: ~p", [AccountId, _R]).

-spec process_account(ne_binary(), ne_binary(), kz_account:doc()) -> 'ok'.
process_account(AccountId, AccountDb, AccountJObj) ->
    lager:debug("account crawler processing account ~s", [AccountId]),
    _ = maybe_test_for_initial_occurrences(AccountId, AccountDb, AccountJObj),
    _ = maybe_test_for_low_balance(AccountId, AccountJObj),
    _ = doodle_maintenance:start_check_sms_by_account(AccountId, AccountJObj),
    'ok'.

%%% Initial Occurrence

-spec maybe_test_for_initial_occurrences(ne_binary(), ne_binary(), kz_account:doc()) -> 'ok'.
maybe_test_for_initial_occurrences(AccountId, AccountDb, AccountJObj) ->
    case ?SHOULD_CRAWL_FOR_FIRST_OCCURRENCE of
        'false' -> 'ok';
        'true' ->
            maybe_test_for_registrations(AccountId, AccountJObj),
            maybe_test_for_initial_call(AccountId, AccountDb, AccountJObj)
    end.

%% First registration
-spec maybe_test_for_registrations(ne_binary(), kz_account:doc()) -> 'ok'.
maybe_test_for_registrations(AccountId, AccountJObj) ->
    Realm = kz_account:realm(AccountJObj),
    case Realm =:= 'undefined'
        orelse kz_account:sent_initial_registration(AccountJObj)
    of
        'true' -> 'ok';
        'false' -> test_for_registrations(AccountId, Realm)
    end.

-spec test_for_registrations(ne_binary(), ne_binary()) -> 'ok'.
test_for_registrations(AccountId, Realm) ->
    lager:debug("looking for any registrations in realm ~s", [Realm]),
    Reg = [{<<"Realm">>, Realm}
          ,{<<"Fields">>, [<<"Account-ID">>]}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kapps_util:amqp_pool_collect(Reg
                                     ,fun kapi_registration:publish_query_req/1
                                     ,{'ecallmgr', fun kapi_registration:query_resp_v/1}
                                     )
    of
        {'error', _} -> 'ok';
        {_, JObjs} ->
            case lists:any(fun kapi_registration:query_resp_v/1, JObjs) of
                'false' -> 'ok';
                'true' ->
                    lager:debug("found initial registration for account ~s (~s)", [AccountId, Realm]),
                    handle_initial_registration(AccountId)
            end
    end.

-spec handle_initial_registration(ne_binary()) -> 'ok'.
handle_initial_registration(AccountId) ->
    case kz_account:fetch(AccountId) of
        {'ok', AccountJObj} -> notify_initial_registration(AccountJObj);
        _E -> 'ok'
    end.

-spec notify_initial_registration(kz_account:doc()) -> 'ok'.
notify_initial_registration(AccountJObj) ->
    UpdatedAccountJObj = kz_account:set_initial_registration_sent(AccountJObj, 'true'),
    _ = kz_util:account_update(UpdatedAccountJObj),
    kz_notify:first_registration(kz_doc:id(AccountJObj)).

%% First Call
-spec maybe_test_for_initial_call(ne_binary(), ne_binary(), kz_account:doc()) -> 'ok'.
maybe_test_for_initial_call(AccountId, AccountDb, AccountJObj) ->
    case kz_account:sent_initial_call(AccountJObj) of
        'true' -> 'ok';
        'false' ->
            lager:debug("looking for initial call in account ~s", [AccountId]),
            test_for_initial_call(AccountId, AccountDb)
    end.

-spec test_for_initial_call(ne_binary(), ne_binary()) -> 'ok'.
test_for_initial_call(AccountId, AccountDb) ->
    ViewOptions = [{'key', <<"cdr">>}
                  ,{'limit', 1}
                  ],
    case kz_datamgr:get_results(AccountDb, <<"maintenance/listing_by_type">>, ViewOptions) of
        {'ok', [_|_]} ->
            lager:debug("found initial call in account ~s", [AccountId]),
            handle_initial_call(AccountId);
        _Else -> 'ok'
    end.

-spec handle_initial_call(ne_binary()) -> 'ok'.
handle_initial_call(AccountId) ->
    case kz_account:fetch(AccountId) of
        {'ok', AccountJObj} -> notify_initial_call(AccountJObj);
        _ -> 'ok'
    end.

-spec notify_initial_call(kz_account:doc()) -> 'ok'.
notify_initial_call(AccountJObj) ->
    UpdatedAccountJObj = kz_account:set_initial_call_sent(AccountJObj, 'true'),
    _ = kz_util:account_update(UpdatedAccountJObj),
    kz_notify:first_call(kz_doc:id(AccountJObj)).

%%% Low balance check

-spec maybe_test_for_low_balance(ne_binary(), kz_account:doc()) -> 'ok'.
maybe_test_for_low_balance(AccountId, AccountJObj) ->
    case ?SHOULD_CRAWL_FOR_LOW_BALANCE of
        'false' -> 'ok';
        'true' -> test_for_low_balance(AccountId, AccountJObj, 3)
    end.

-spec test_for_low_balance(ne_binary(), kz_account:doc(), 0..3) -> 'ok'.
test_for_low_balance(_AccountId, _AccountJObj, 0) ->
    lager:debug("max try to get account ~s current balance", [_AccountId]);
test_for_low_balance(AccountId, AccountJObj, Loop) ->
    case wht_util:current_balance(AccountId) of
        {'error', 'timeout'} ->
            test_for_low_balance(AccountId, AccountJObj, Loop - 1);
        {'error', 'not_found'} ->
            {Year, Month, _} = erlang:date(),
            kazoo_modb:maybe_create(kazoo_modb:get_modb(AccountId, Year, Month)),
            test_for_low_balance(AccountId, AccountJObj, Loop - 1);
        {'error', _R} -> 'ok';
        CurrentBalance ->
            maybe_notify_for_low_balance(AccountJObj, CurrentBalance),
            maybe_topup_account(AccountJObj, CurrentBalance)
    end.

-spec maybe_notify_for_low_balance(kz_account:doc(), kz_transaction:units()) -> 'ok'.
maybe_notify_for_low_balance(AccountJObj, CurrentBalance) ->
    AccountId = kz_account:id(AccountJObj),
    Threshold = kz_account:low_balance_threshold(AccountJObj),
    lager:info("checking if account ~s balance $~w is below notification threshold $~w"
              ,[AccountId, wht_util:units_to_dollars(CurrentBalance), Threshold]),
    case is_balance_below_notify_threshold(CurrentBalance, Threshold) of
        'false' -> maybe_reset_low_balance_sent(AccountJObj);
        'true' -> maybe_low_balance_notify(AccountJObj, CurrentBalance)
    end.

-spec is_balance_below_notify_threshold(kz_transaction:units(), number()) -> boolean().
is_balance_below_notify_threshold(CurrentBalance, Threshold) ->
    CurrentBalance =< wht_util:dollars_to_units(Threshold).

-spec maybe_topup_account(kz_account:doc(), kz_transaction:units()) -> 'ok'.
maybe_topup_account(AccountJObj, CurrentBalance) ->
    AccountId = kz_account:id(AccountJObj),
    lager:info("checking topup for account ~s with balance $~w"
              ,[AccountId, wht_util:units_to_dollars(CurrentBalance)]),
    case kz_topup:init(AccountId, CurrentBalance) of
        'ok' ->
            maybe_reset_low_balance_sent(AccountJObj),
            lager:info("topup successful for ~s", [AccountId]);
        {'error', topup_disabled} -> 'ok';
        {'error', 'balance_above_threshold'} -> 'ok';
        {'error', _Error} ->
            lager:error("topup failed for ~s: ~p", [AccountId, _Error])
    end.

-spec maybe_reset_low_balance_sent(kz_account:doc()) -> 'ok'.
maybe_reset_low_balance_sent(AccountJObj) ->
    case kz_account:low_balance_sent(AccountJObj)
        orelse kz_account:low_balance_tstamp(AccountJObj) =/= 'undefined'
    of
        'true' -> reset_low_balance_sent(AccountJObj);
        'false' -> 'ok'
    end.

-spec reset_low_balance_sent(kz_account:doc()) ->  'ok'.
reset_low_balance_sent(AccountJObj0) ->
    lager:debug("resetting low balance sent"),
    AccountJObj1 = kz_account:reset_low_balance_sent(AccountJObj0),
    AccountJObj2 = kz_account:remove_low_balance_tstamp(AccountJObj1),
    _ = kz_util:account_update(AccountJObj2),
    'ok'.

-spec maybe_low_balance_notify(kz_account:doc(), kz_transaction:units()) -> 'ok'.
maybe_low_balance_notify(AccountJObj, CurrentBalance) ->
    case kz_account:low_balance_enabled_exists(AccountJObj) of
        'false' ->
            lager:debug("low balance notification enabled key not present, using deprecated check"),
            maybe_low_balance_notify_deprecated(AccountJObj, CurrentBalance);
        'true' ->
            maybe_low_balance_notify(AccountJObj, CurrentBalance, kz_account:low_balance_enabled(AccountJObj))
    end.

-spec maybe_low_balance_notify(kz_account:doc(), kz_transaction:units(), boolean()) -> 'ok'.
maybe_low_balance_notify(_AccountJObj, _CurrentBalance, 'false') ->
    lager:debug("low balance notification disabled");
maybe_low_balance_notify(AccountJObj, CurrentBalance, 'true') ->
    lager:debug("low balance notification enabled"),
    case kz_account:low_balance_tstamp(AccountJObj) of
        LowBalanceSent when is_number(LowBalanceSent) ->
            Cycle = ?LOW_BALANCE_REPEAT,
            Diff = kz_time:current_tstamp() - LowBalanceSent,
            case Diff >= Cycle of
                'true' -> notify_of_low_balance(AccountJObj, CurrentBalance);
                'false' ->
                    lager:debug("low balance alert sent ~w seconds ago, repeats every ~w", [Diff, Cycle])
            end;
        _Else -> notify_of_low_balance(AccountJObj, CurrentBalance)
    end.

-spec maybe_low_balance_notify_deprecated(kz_account:doc(), kz_transaction:units()) -> 'ok'.
maybe_low_balance_notify_deprecated(AccountJObj, CurrentBalance) ->
    case kz_account:low_balance_sent(AccountJObj) of
        'true' -> lager:debug("low balance alert already sent");
        'false' -> notify_of_low_balance(AccountJObj, CurrentBalance)
    end.

-spec notify_of_low_balance(kz_account:doc(), kz_transaction:units()) -> 'ok'.
notify_of_low_balance(AccountJObj, CurrentBalance) ->
    AccountId = kz_account:id(AccountJObj),
    lager:debug("sending low balance alert for account ~s with balance ~w"
               ,[AccountId, CurrentBalance]),
    'ok' = kz_notify:low_balance(AccountId, CurrentBalance),
    update_account_low_balance_sent(AccountJObj).

-spec update_account_low_balance_sent(kz_account:doc()) -> 'ok'.
update_account_low_balance_sent(AccountJObj0) ->
    AccountJObj1 = kz_account:set_low_balance_sent(AccountJObj0),
    AccountJObj2 = kz_account:set_low_balance_tstamp(AccountJObj1),
    _ = kz_util:account_update(AccountJObj2),
    'ok'.
