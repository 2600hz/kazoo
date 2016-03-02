%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz INC
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

-define(SERVER, ?MODULE).

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".account_crawler">>).

-define(KEY_LOW_BALANCE_SENT, [<<"notifications">>, <<"low_balance">>, <<"sent_low_balance">>]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_server:start_link(?SERVER, [], []).

-spec check(ne_binary()) -> 'ok'.
check(Account) when is_binary(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    case kz_datamgr:open_doc(?WH_ACCOUNTS_DB, AccountId) of
        {'ok', AccountJObj} ->
            process_account(AccountId, wh_doc:account_db(AccountJObj), AccountJObj);
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
    Cycle = whapps_config:get_integer(?MOD_CONFIG_CAT, <<"cycle_delay_time">>, 5 * ?MILLISECONDS_IN_MINUTE),
    erlang:send_after(Cycle, self(), 'crawl_accounts'),
    {'noreply', [], 'hibernate'};
handle_info('next_account', [Account|Accounts]) ->
    _ = case wh_doc:id(Account) of
            <<"_design", _/binary>> -> 'ok';
            AccountId ->
                %% do not open the account def in the account db or we will
                %% be wasting bigcouch's file descriptors
                OpenResult = kz_datamgr:open_doc(?WH_ACCOUNTS_DB, AccountId),
                check_then_process_account(AccountId, OpenResult)
        end,
    Cycle = whapps_config:get_integer(?MOD_CONFIG_CAT, <<"interaccount_delay">>, 10 * ?MILLISECONDS_IN_SECOND),
    erlang:send_after(Cycle, self(), 'next_account'),
    {'noreply', Accounts, 'hibernate'};
handle_info('crawl_accounts', _) ->
    _ = case kz_datamgr:all_docs(?WH_ACCOUNTS_DB) of
            {'ok', JObjs} ->
                self() ! 'next_account',
                {'noreply', wh_util:shuffle_list(JObjs)};
            {'error', _R} ->
                lager:warning("unable to list all docs in ~s: ~p", [?WH_ACCOUNTS_DB, _R]),
                self() ! 'next_account',
                {'noreply', []}
        end;
handle_info(_Info, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
    {'noreply', State}.

-spec check_then_process_account(ne_binary(), {'ok', kz_account:doc()} | {'error',any()}) -> 'ok'.
check_then_process_account(AccountId, {'ok', AccountJObj}) ->
    case wh_doc:is_soft_deleted(AccountJObj)
            orelse not kz_account:is_enabled(AccountJObj) of
        'true' ->
            lager:debug("not processing account ~p (soft-destroyed)", [AccountId]);
        'false' ->
            process_account(AccountId, wh_doc:account_db(AccountJObj), AccountJObj)
    end;
check_then_process_account(AccountId, {'error', _R}) ->
    lager:warning("unable to open account definition for ~s: ~p", [AccountId, _R]).

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
-spec process_account (ne_binary(), ne_binary(), kz_account:doc()) -> 'ok'.
process_account(AccountId, AccountDb, AccountJObj) ->
    lager:debug("notify crawler processing account ~s", [AccountId]),
    _ = maybe_test_for_initial_occurrences(AccountId, AccountDb, AccountJObj),
    _ = maybe_test_for_low_balance(AccountId, AccountJObj),
    _ = doodle_maintenance:start_check_sms_by_account(AccountId, AccountJObj),
    'ok'.

-spec maybe_test_for_initial_occurrences(ne_binary(), ne_binary(), kz_account:doc()) -> 'ok'.
maybe_test_for_initial_occurrences(AccountId, AccountDb, AccountJObj) ->
    case whapps_config:get_is_true(?MOD_CONFIG_CAT, <<"crawl_for_first_occurrence">>, 'true') of
        'false' -> 'ok';
        'true' ->
            test_for_initial_occurrences(AccountId, AccountDb, AccountJObj)
    end.

-spec test_for_initial_occurrences(ne_binary(), ne_binary(), kz_account:doc()) -> 'ok'.
test_for_initial_occurrences(AccountId, AccountDb, AccountJObj) ->
    _ = maybe_test_for_registrations(AccountId, AccountJObj),
    maybe_test_for_initial_call(AccountId, AccountDb, AccountJObj).

-spec maybe_test_for_registrations(ne_binary(), kz_account:doc()) -> 'ok'.
maybe_test_for_registrations(AccountId, AccountJObj) ->
    Realm = kz_account:realm(AccountJObj),
    case wh_json:is_true([<<"notifications">>, <<"first_occurrence">>, <<"sent_initial_registration">>], AccountJObj)
        orelse Realm =:= 'undefined'
    of
        'true' -> 'ok';
        'false' ->
            test_for_registrations(AccountId, Realm)
    end.

-spec test_for_registrations(ne_binary(), ne_binary()) -> 'ok'.
test_for_registrations(AccountId, Realm) ->
    lager:debug("looking for any registrations in realm ~s", [Realm]),
    Reg = [{<<"Realm">>, Realm}
           ,{<<"Fields">>, [<<"Account-ID">>]}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case whapps_util:amqp_pool_collect(Reg
                                       ,fun wapi_registration:publish_query_req/1
                                       ,{'ecallmgr', fun wapi_registration:query_resp_v/1}
                                      )
    of
        {'error', _} -> 'ok';
        {_, JObjs} ->
            case lists:any(fun wapi_registration:query_resp_v/1, JObjs) of
                'false' -> 'ok';
                'true' ->
                    lager:debug("found initial registration for account ~s (~s)", [AccountId, Realm]),
                    handle_initial_registration(AccountId)
            end
    end.

-spec handle_initial_registration(ne_binary()) -> 'ok'.
handle_initial_registration(AccountId) ->
    case kz_account:fetch(AccountId) of
        {'ok', AccountJObj} ->
            notify_initial_registration(AccountJObj);
        _E -> 'ok'
    end.

-spec notify_initial_registration(kz_account:doc()) -> 'ok'.
notify_initial_registration(AccountJObj) ->
    UpdatedAccountJObj = wh_json:set_value([<<"notifications">>, <<"first_occurrence">>, <<"sent_initial_registration">>]
                                    ,'true'
                                    ,AccountJObj
                                    ),
    wh_util:account_update(UpdatedAccountJObj),
    notify_first_occurrence:send(<<"registration">>, UpdatedAccountJObj).

-spec maybe_test_for_initial_call(ne_binary(), ne_binary(), kz_account:doc()) -> 'ok'.
maybe_test_for_initial_call(AccountId, AccountDb, AccountJObj) ->
    case wh_json:is_true([<<"notifications">>, <<"first_occurrence">>, <<"sent_initial_call">>], AccountJObj) of
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
        {'ok', AccountJObj} ->
            notify_initial_call(AccountJObj);
        _ -> 'ok'
    end.

-spec notify_initial_call(kz_account:doc()) -> any().
notify_initial_call(AccountJObj) ->
    UpdatedAccountJObj = wh_json:set_value([<<"notifications">>, <<"first_occurrence">>, <<"sent_initial_call">>]
                                           ,'true'
                                           ,AccountJObj
                                          ),
    wh_util:account_update(UpdatedAccountJObj),
    notify_first_occurrence:send(<<"call">>, UpdatedAccountJObj).

-spec maybe_test_for_low_balance(ne_binary(), kz_account:doc()) -> 'ok'.
maybe_test_for_low_balance(AccountId, AccountJObj) ->
    case whapps_config:get_is_true(?MOD_CONFIG_CAT, <<"crawl_for_low_balance">>, 'true') of
        'false' -> 'ok';
        'true' ->
            test_for_low_balance(AccountId, AccountJObj)
    end.

-spec test_for_low_balance(ne_binary(), kz_account:doc()) -> 'ok'.
test_for_low_balance(AccountId, AccountJObj) ->
    Threshold = low_balance_threshold(AccountId),
    CurrentBalance = wht_util:current_balance(AccountId),
    lager:debug("checking if account ~s balance $~w is below $~w"
               ,[AccountId, wht_util:units_to_dollars(CurrentBalance), Threshold]
               ),

    case is_account_balance_too_low(CurrentBalance, Threshold) of
        'true' ->
            maybe_low_balance_notify(AccountJObj, CurrentBalance),
            maybe_reset_low_balance_sent(
              AccountJObj
              ,maybe_topup_account(AccountId, CurrentBalance)
             );
        'false' ->
            'ok'
    end.

-spec maybe_reset_low_balance_sent(kz_account:doc(), 'ok' | 'error') -> 'ok'.
maybe_reset_low_balance_sent(AccountJObj, 'ok') ->
    UpdatedAccountJObj = wh_json:set_value(?KEY_LOW_BALANCE_SENT, 'false', AccountJObj),
    wh_util:account_update(UpdatedAccountJObj),
    lager:debug("topup succeeded, resetting ~s to 'false'", [?KEY_LOW_BALANCE_SENT]);
maybe_reset_low_balance_sent(_AccountJObj, Error) ->
    lager:debug("not resetting ~s, topup error: ~p"
               ,[?KEY_LOW_BALANCE_SENT, Error]
               ).

-spec is_account_balance_too_low(wh_transaction:units(), number()) -> boolean().
is_account_balance_too_low(CurrentBalance, Threshold) ->
    CurrentBalance < wht_util:dollars_to_units(Threshold).

-spec maybe_topup_account(ne_binary(), wh_transaction:units()) ->
                                 'ok' |
                                 wh_topup:error().
maybe_topup_account(AccountId, CurrentBalance) ->
    case wh_topup:init(AccountId, CurrentBalance) of
        'ok' ->
            lager:debug("topup successful for ~s", [AccountId]);
        {'error', Error} ->
            lager:error("topup failed for ~s: ~p", [AccountId, Error]),
            Error
    end.

-spec maybe_low_balance_notify(kz_account:doc(), wh_transaction:units()) -> 'ok'.
maybe_low_balance_notify(AccountJObj, CurrentBalance) ->
    case wh_json:is_true(?KEY_LOW_BALANCE_SENT, AccountJObj, 'false') of
        'true' -> lager:debug("low balance alert already sent apparently");
        'false' ->
            _ = notify_of_low_balance(wh_doc:id(AccountJObj), CurrentBalance),
            update_account_low_balance_sent(AccountJObj)
    end.

-spec update_account_low_balance_sent(kz_account:doc()) -> 'ok' |
                                                            {'error', any()}.
update_account_low_balance_sent(AccountJObj) ->
    UpdatedAccountJObj = wh_json:set_value(?KEY_LOW_BALANCE_SENT, 'true', AccountJObj),
    wh_util:account_update(UpdatedAccountJObj).

-spec notify_of_low_balance(ne_binary(), wh_transaction:units()) -> 'ok'.
notify_of_low_balance(AccountId, CurrentBalance) ->
    wh_notify:low_balance(AccountId, CurrentBalance).

-spec low_balance_threshold(ne_binary()) -> float().
low_balance_threshold(Account) ->
    ConfigCat = <<(?NOTIFY_CONFIG_CAT)/binary, ".low_balance">>,
    Default = whapps_config:get_float(ConfigCat, <<"threshold">>, 5.00),

    case kz_account:fetch(Account) of
        {'error', _R} -> Default;
        {'ok', JObj} ->
            TopUp = wh_json:get_float_value([<<"topup">>, <<"threshold">>], JObj, Default),
            kz_account:threshold(JObj, TopUp)
    end.
