%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(notify_account_crawler).

-behaviour(gen_server).

-export([start_link/0]).
-export([check/1]).
-export([low_balance_threshold/1]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("notify.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".account_crawler">>).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

check(Account) when is_binary(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    case couch_mgr:open_doc(?WH_ACCOUNTS_DB, AccountId) of
        {'ok', JObj} ->
            AccountDb = wh_json:get_value(<<"pvt_account_db">>, JObj),
            process_account(AccountId, AccountDb, JObj);
        {'error', _R} ->
            lager:warning("unable to open account definition for ~s: ~p", [AccountId, _R])
    end;
check(Account) ->
    check(wh_util:to_binary(Account)).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    %% The other modules are init'd because they are
    %% responders for the gen_listener...
    notify_first_occurrence:init(),
    self() ! 'crawl_accounts',
    {'ok', #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info('next_account', []) ->
    Cycle = whapps_config:get_integer(?MOD_CONFIG_CAT, <<"cycle_delay_time">>, 300000),
    erlang:send_after(Cycle, self(), 'crawl_accounts'),
    {'noreply', []};
handle_info('next_account', [Account|Accounts]) ->
    _ = case wh_json:get_value(<<"id">>, Account) of
            <<"_design", _/binary>> -> 'ok';
            AccountId ->
                %% do not open the account def in the account db or we will
                %% be wasting bigcouch's file descriptors
                case couch_mgr:open_doc(?WH_ACCOUNTS_DB, AccountId) of
                    {'ok', JObj} ->
                        AccountDb = wh_json:get_value(<<"pvt_account_db">>, JObj),
                        process_account(AccountId, AccountDb, JObj);
                    {'error', _R} ->
                        lager:warning("unable to open account definition for ~s: ~p", [AccountId, _R])
                end
        end,
    Cycle = whapps_config:get_integer(?MOD_CONFIG_CAT, <<"interaccount_delay">>, 10000),
    erlang:send_after(Cycle, self(), 'next_account'),
    {'noreply', Accounts};
handle_info('crawl_accounts', _) ->
    _ = case couch_mgr:all_docs(?WH_ACCOUNTS_DB) of
            {'ok', JObjs} ->
                self() ! 'next_account',
                {'noreply', wh_util:shuffle_list(JObjs)};
            {'error', _R} ->
                lager:warning("unable to list all docs in ~s: ~p", [?WH_ACCOUNTS_DB, _R]),
                self() ! 'next_account',
                {'noreply', []}
        end;
handle_info(_Info, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
process_account(AccountId, AccountDb, JObj) ->
    lager:debug("notify crawler processing account ~s", [AccountId]),
    _ = maybe_test_for_initial_occurrences(AccountId, AccountDb, JObj),
    _ = maybe_test_for_low_balance(AccountId, AccountDb, JObj),
    'ok'.

-spec maybe_test_for_initial_occurrences(ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
maybe_test_for_initial_occurrences(AccountId, AccountDb, JObj) ->
    case whapps_config:get_is_true(?MOD_CONFIG_CAT, <<"crawl_for_first_occurrence">>, 'true') of
        'false' -> 'ok';
        'true' ->
            test_for_initial_occurrences(AccountId, AccountDb, JObj)
    end.

-spec test_for_initial_occurrences(ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
test_for_initial_occurrences(AccountId, AccountDb, JObj) ->
    _ = maybe_test_for_registrations(AccountId, AccountDb, JObj),
    maybe_test_for_initial_call(AccountId, AccountDb, JObj).

-spec maybe_test_for_registrations(ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
maybe_test_for_registrations(AccountId, AccountDb, JObj) ->
    Realm = wh_json:get_ne_value(<<"realm">>, JObj),
    case wh_json:is_true([<<"notifications">>, <<"first_occurrence">>, <<"sent_initial_registration">>], JObj)
        orelse Realm =:= 'undefined'
    of
        'true' -> 'ok';
        'false' ->
            test_for_registrations(AccountId, AccountDb, Realm)
    end.

-spec test_for_registrations(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
test_for_registrations(AccountId, AccountDb, Realm) ->
    lager:debug("looking for any registrations in realm ~s", [Realm]),
    Reg = [{<<"Realm">>, Realm}
           ,{<<"Fields">>, [<<"Account-ID">>]}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case whapps_util:amqp_pool_collect(Reg
                                       ,fun wapi_registration:publish_query_req/1
                                       ,{'ecallmgr', fun wapi_registration:query_resp_v/1})
    of
        {'error', _} -> 'ok';
        {_, JObjs} ->
            case lists:any(fun wapi_registration:query_resp_v/1, JObjs) of
                'false' -> 'ok';
                'true' ->
                    lager:debug("found initial registration for account ~s (~s)", [AccountId, Realm]),
                    handle_initial_registration(AccountId, AccountDb)
            end
    end.

-spec handle_initial_registration(ne_binary(), ne_binary()) -> 'ok'.
handle_initial_registration(AccountId, AccountDb) ->
    case couch_mgr:open_doc(AccountDb, AccountId) of
        {'ok', JObj} ->
            notify_initial_registration(AccountDb, JObj);
        _E -> 'ok'
    end.

-spec notify_initial_registration(ne_binary(), wh_json:object()) -> 'ok'.
notify_initial_registration(AccountDb, JObj) ->
    Account = wh_json:set_value([<<"notifications">>, <<"first_occurrence">>, <<"sent_initial_registration">>]
                                ,'true'
                                ,JObj),
    case couch_mgr:save_doc(AccountDb, Account) of
        {'ok', _} ->
            couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, Account),
            notify_first_occurrence:send(<<"registration">>, Account);
        _E -> 'ok'
    end.

-spec maybe_test_for_initial_call(ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
maybe_test_for_initial_call(AccountId, AccountDb, JObj) ->
    case wh_json:is_true([<<"notifications">>, <<"first_occurrence">>, <<"sent_initial_call">>], JObj) of
        'true' -> 'ok';
        'false' ->
            lager:debug("looking for initial call in account ~s", [AccountId]),
            test_for_initial_call(AccountId, AccountDb)
    end.

-spec test_for_initial_call(ne_binary(), ne_binary()) -> 'ok'.
test_for_initial_call(AccountId, AccountDb) ->
    ViewOptions = [{'key', <<"cdr">>}
                   ,{'limit', <<"1">>}
                  ],
    case couch_mgr:get_results(AccountDb, <<"maintenance/listing_by_type">>, ViewOptions) of
        {'ok', [_|_]} ->
            lager:debug("found initial call in account ~s", [AccountId]),
            handle_initial_call(AccountId, AccountDb);
        _Else -> 'ok'
    end.

-spec handle_initial_call(ne_binary(), ne_binary()) -> 'ok'.
handle_initial_call(AccountId, AccountDb) ->
    case couch_mgr:open_doc(AccountDb, AccountId) of
        {'ok', JObj} ->
            notify_initial_call(AccountDb, JObj);
        _ -> 'ok'
    end.

-spec notify_initial_call(ne_binary(), wh_json:object()) -> any().
notify_initial_call(AccountDb, JObj) ->
    Account = wh_json:set_value([<<"notifications">>, <<"first_occurrence">>, <<"sent_initial_call">>]
                                ,'true'
                                ,JObj),
    case couch_mgr:save_doc(AccountDb, Account) of
        {'ok', _} ->
            couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, Account),
            notify_first_occurrence:send(<<"call">>, Account);
        _ -> 'ok'
    end.

-spec maybe_test_for_low_balance(ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
maybe_test_for_low_balance(AccountId, AccountDb, JObj) ->
    case whapps_config:get_is_true(?MOD_CONFIG_CAT, <<"crawl_for_low_balance">>, 'true') of
        'false' -> 'ok';
        'true' ->
            test_for_low_balance(AccountId, AccountDb, JObj)
    end.

-spec test_for_low_balance(ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
test_for_low_balance(AccountId, AccountDb, JObj) ->
    Threshold = low_balance_threshold(AccountDb),
    CurrentBalance = wht_util:current_balance(AccountId),
    lager:debug("checking if account ~s balance is below $~w", [AccountId, Threshold]),
    case CurrentBalance < wht_util:dollars_to_units(Threshold) of
        'false' ->
            maybe_reset_low_balance(AccountId, AccountDb, JObj);
        'true' ->
            maybe_handle_low_balance(CurrentBalance, AccountId, AccountDb, JObj)
    end.

-spec maybe_reset_low_balance(ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
maybe_reset_low_balance(AccountId, AccountDb, JObj) ->
    case wh_json:is_true([<<"notifications">>, <<"low_balance">>, <<"sent_low_balance">>], JObj, 'true') of
        'false' -> 'ok';
        'true' ->
            reset_low_balance(AccountId, AccountDb)
    end.

-spec reset_low_balance(ne_binary(), ne_binary()) -> 'ok'.
reset_low_balance(AccountId, AccountDb) ->
    case couch_mgr:open_doc(AccountDb, AccountId) of
        {'ok', JObj} ->
            lager:debug("reseting low balance sent flag for account ~s", [AccountId]),
            Account = wh_json:set_value([<<"notifications">>, <<"low_balance">>, <<"sent_low_balance">>], 'false', JObj),
            case couch_mgr:save_doc(AccountDb, Account) of
                {'ok', _} ->
                    couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, Account),
                    'ok';
                _E ->
                    lager:info("unable to reset low balance flag for accounnt ~s: ~p", [AccountId, _E])
            end;
        {'error', _} ->
            'ok'
    end.

-spec maybe_handle_low_balance(integer(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
maybe_handle_low_balance(CurrentBalance, AccountId, AccountDb, JObj) ->
    case wh_json:is_true([<<"notifications">>, <<"low_balance">>, <<"sent_low_balance">>], JObj)
        orelse wh_json:get_value([<<"notifications">>, <<"low_balance">>, <<"sent_low_balance">>], JObj) =:= 'undefined'
    of
        'true' -> 'ok';
        'false' ->
            handle_low_balance(CurrentBalance, AccountId, AccountDb)
    end.

-spec handle_low_balance(integer(), ne_binary(), ne_binary()) -> 'ok'.
handle_low_balance(CurrentBalance, AccountId, AccountDb) ->
    case couch_mgr:open_doc(AccountDb, AccountId) of
        {'ok', JObj} ->
            notify_low_balance(CurrentBalance, AccountId, AccountDb, JObj);
        _ -> 'ok'
    end.

-spec notify_low_balance(integer(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
notify_low_balance(CurrentBalance, AccountId, AccountDb, JObj) ->
    Account = wh_json:set_value([<<"notifications">>, <<"low_balance">>, <<"sent_low_balance">>]
                                ,'true'
                                ,JObj),
    case couch_mgr:save_doc(AccountDb, Account) of
        {'ok', _} ->
            couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, Account),
            notify_low_balance:send(CurrentBalance, Account);
        _E ->
            lager:debug("unable to update low balance flag for account ~s: ~p~n", [AccountId, _E]),
            'ok'
    end.

-spec low_balance_threshold(ne_binary()) -> float().
low_balance_threshold(_AccountDb) ->
    ConfigCat = <<(?NOTIFY_CONFIG_CAT)/binary, ".low_balance">>,
    whapps_config:get_float(ConfigCat, <<"threshold">>, 5.00).
