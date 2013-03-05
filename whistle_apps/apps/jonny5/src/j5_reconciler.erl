%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_reconciler).

-behaviour(gen_server).

-export([start_link/0]).
-export([process_account/1]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include_lib("jonny5.hrl").

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

-spec process_account(ne_binary()) -> 'ok' | {'error', _}.
process_account(Account) ->
    lager:debug("attempting to reconcile jonny5 credit/debit for account ~s", [Account]),
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    ViewOptions = ['reduce', 'group'],
    case couch_mgr:get_results(AccountDb, <<"transactions/reconcile_by_callid">>, ViewOptions) of
        {'error', _R}=E -> E;
        {'ok', JObjs} ->
            _ = [correct_discrepancy(AccountDb, wh_json:get_value(<<"key">>, JObj), Units)
                 || JObj <- JObjs
                        ,not wh_util:is_empty(Units = wh_json:get_value(<<"value">>, JObj, 0))
                ],
            'ok'
    end.

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
    gen_server:cast(self(), process_account),
    {'ok', []}.

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
handle_cast('process_account', []) ->
    timer:sleep(crypto:rand_uniform(10000, 30000)),
    gen_server:cast(self(), 'process_account'),
    {'noreply', wh_util:shuffle_list(whapps_util:get_all_accounts())};
handle_cast('process_account', [Account|Accounts]) ->
    put('callid', ?LOG_SYSTEM_ID),
    timer:sleep(crypto:rand_uniform(1000, 3000)),
    process_account(Account),
    gen_server:cast(self(), 'process_account'),
    {'noreply', Accounts};
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
    lager:debug("jonny5 reconciler terminating: ~p", [_Reason]).

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
-spec correct_discrepancy(ne_binary(), ne_binary(), integer()) -> 'ok' | {'error', _}.
correct_discrepancy(Ledger, CallId, Units) ->
    put('callid', CallId),
    case maybe_get_cdr(Ledger, CallId) of
        {'error', 'not_found'} ->
            lager:debug("CDR is missing, removing any existing call charges", []),
            j5_util:remove_call_charges(Ledger, CallId);
        {'error', _} -> 'ok';
        {'ok', JObj} ->
            proper_billing_type(JObj, Ledger, CallId, Units)
    end.

-spec proper_billing_type(wh_json:object(), ne_binary(), ne_binary(), integer()) -> 'ok' | {'error', _}.
proper_billing_type(JObj, Ledger, CallId, Units) ->
    AccountBilling = wh_json:get_value([<<"custom_channel_vars">>, <<"account_billing">>], JObj),
    case AccountBilling =:=  <<"per_minute">> orelse AccountBilling =:= <<"per_minute_limit">> of
        'false' ->
            lager:debug("billing type is not per_minute, removing any existing call charges", []),
            j5_util:remove_call_charges(Ledger, CallId);
        'true' ->
            maybe_attempt_reconcile(JObj, Ledger, CallId, Units)

    end.

-spec maybe_attempt_reconcile(wh_json:object(), ne_binary(), ne_binary(), integer()) -> 'ok' | {'error', _}.
maybe_attempt_reconcile(JObj, Ledger, CallId, Units) ->
    case reconcile_grace_period_exceeded(JObj)
        andalso not_already_attempted(Ledger, CallId)
    of
        'false' -> {'error', 'contention'};
        'true' ->
            attempt_reconcile(JObj, Ledger, CallId, Units)
    end.

-spec attempt_reconcile(wh_json:object(), ne_binary(), ne_binary(), integer()) -> 'ok' | {'error', _}.
attempt_reconcile(JObj, Ledger, CallId, Units) ->
    case per_minute_discrepancy(JObj, Ledger, CallId, Units) of
        {'error', 'not_required'} -> 'ok';
        {'error', _R}=E ->
            lager:warning("unable to correct discrepancy for ~s/~s: ~p", [Ledger, CallId, _R]),
            E;
        {'ok', Transaction} ->
            lager:notice("corrected $~p discrepancy for ~s/~s", [wht_util:units_to_dollars(Units)
                                                                 ,Ledger, CallId
                                                                ]),
            _ = send_system_alert(Transaction),
            'ok'
    end.

-spec maybe_get_cdr(ne_binary(), ne_binary()) -> wh_jobj_return().
maybe_get_cdr(Ledger, CallId) ->
    LedgerDb = wh_util:format_account_id(Ledger, 'encoded'),
    case couch_mgr:open_doc(LedgerDb, CallId) of
        {'ok', JObj}=Ok ->
            case wh_json:get_value(<<"call_id">>, JObj) =:= CallId of
                'true' -> Ok;
                'false' ->
                    lager:warning("CDR ~s/~s call-id disagrees with doc id", [Ledger, CallId]),
                    {'error', 'not_found'}
            end;
        {'error', 'not_found'} ->
            case call_has_ended(CallId) of
                'false' ->
                    lager:debug("ignoring discrepancy for active call", []),
                    {'error', 'call_active'};
                'true' ->
                    lager:warning("CDR ~s/~s is missing...", [Ledger, CallId]),
                    {'error', 'not_found'}
            end;
        {'error', _R}=E ->
            lager:warning("unable to fetch CDR for ~s/~s: ~p", [Ledger, CallId, _R]),
            E
    end.

-spec call_has_ended(ne_binary()) -> boolean().
call_has_ended(CallId) ->
    case whapps_call_command:b_channel_status(CallId) of
        {'ok', _} -> 'false';
        {'error', _R} -> 'true'
    end.

-spec reconcile_grace_period_exceeded(wh_json:json_object()) -> boolean().
reconcile_grace_period_exceeded(JObj) ->
    Current = wh_util:current_tstamp(),
    Modified = wh_json:get_integer_value(<<"pvt_modified">>, JObj, Current),
    case Current - Modified > 300 of
        'true' -> 'true';
        'false' ->
            lager:debug("call has not exceeded the reconcile grace period yet", []),
            'false'
    end.

-spec not_already_attempted(ne_binary(), ne_binary()) -> boolean().
not_already_attempted(Ledger, CallId) ->
    case wh_transactions:call_charges(Ledger, CallId, <<"discrepancy">>, 'false') of
        [] -> 'true';
        [Transaction|_] ->
            maybe_remove_discrepancy(Transaction)
    end.

-spec maybe_remove_discrepancy(wh_transaction:transaction()) -> boolean().
maybe_remove_discrepancy(Transaction) ->
    Current = wh_util:current_tstamp(),
    Modified = wh_transaction:modified(Transaction),
    case Current - Modified > 300 of
        'false' ->
            %% If the correction was recently written, wait till the next cycle to make sure
            %% another j5_reconciler has not just placed this here
            'false';
        'true' ->
            %% If the correction was made some time ago then remove it as it has not
            %% corrected the discrepancy, then wait for the next cycle to make sure
            %% it wasn't the sole cause of the issue.... (due to previous bug this could happen)
            lager:debug("removing erronous correction from previous reconciler version", []),
            wh_transaction:remove(Transaction),
            'false'
    end.

-spec send_system_alert(wh_transaction:transaction()) -> pid().
send_system_alert(Transaction) ->
    spawn(fun() ->
                  LedgerDb = wh_transaction:account_db(Transaction),
                  LedgerId = wh_transaction:account_id(Transaction),
                  Account = case couch_mgr:open_cache_doc(LedgerDb, LedgerId) of
                                {'ok', J} -> J;
                                {'error', _} -> wh_json:new()
                            end,
                  AccountName = wh_json:get_value(<<"name">>, Account),
                  AccountRealm = wh_json:get_value(<<"realm">>, Account),
                  Balance = wht_util:units_to_dollars(wht_util:current_balance(LedgerId)),
                  Dollars = wht_util:units_to_dollars(wh_transaction:amount(Transaction)),
                  CallId = wh_transaction:call_id(Transaction),
                  Details = [{<<"Account-ID">>, LedgerId}
                             ,{<<"Account-Name">>, AccountName}
                             ,{<<"Account-Realm">>, AccountRealm}
                             ,{<<"Amount">>, <<"$", (wh_util:to_binary(Dollars))/binary>>}
                             ,{<<"Type">>, wh_transaction:type(Transaction)}
                             ,{<<"Call-ID">>, CallId}
                             ,{<<"New-Balance">>, <<"$", (wh_util:to_binary(Balance))/binary>>}
                            ],
                  wh_notify:system_alert("account $~p discrepancy / ~s (~s) / Call ~s", [Dollars, AccountName, LedgerId, CallId], Details)
          end).

-spec per_minute_discrepancy(wh_json:object(), ne_binary(), ne_binary(), integer()) -> wh_jobj_return().
per_minute_discrepancy(JObj, Ledger, CallId, Units) when Units > 0 ->
    LedgerId = wh_util:format_account_id(Ledger, raw),
    Routines = [fun(T) ->
                        AccountId = get_authz_account_id(JObj),
                        case AccountId =:= LedgerId of
                            true ->
                               wh_transaction:set_reason(<<"per_minute_call">>, T);
                            false ->
                                T1 = wh_transaction:set_reason(<<"sub_account_per_minute_call">>, T),
                                wh_transaction:set_sub_account_id(AccountId, T1)
                        end
                end
                ,fun(T) -> wh_transaction:set_event(<<"DISCREPANCY">>, T) end
                ,fun(T) -> wh_transaction:set_call_id(CallId, T) end
                ,fun(T) ->
                         wh_transaction:set_description(<<"per minute call descrepancy">>, T)
                 end
               ],
    T = lists:foldl(fun(F, T) -> F(T) end, wh_transaction:debit(LedgerId, Units), Routines),
    wh_transaction:save(T);
per_minute_discrepancy(JObj, Ledger, CallId, Units) when Units < 0 ->
    LedgerId = wh_util:format_account_id(Ledger, raw),
    Routines = [fun(T) ->
                        AccountId = get_authz_account_id(JObj),
                        case AccountId =:= LedgerId of
                            true ->
                               wh_transaction:set_reason(<<"per_minute_call">>, T);
                            false ->
                                T1 = wh_transaction:set_reason(<<"sub_account_per_minute_call">>, T),
                                wh_transaction:set_sub_account_id(AccountId, T1)
                        end
                end
                ,fun(T) -> wh_transaction:set_event(<<"DISCREPANCY">>, T) end
                ,fun(T) -> wh_transaction:set_call_id(CallId, T) end
                ,fun(T) ->
                         wh_transaction:set_description(<<"per minute call descrepancy">>, T)
                 end
               ],
    T = lists:foldl(fun(F, T) -> F(T) end, wh_transaction:credit(LedgerId, Units), Routines),
    wh_transaction:save(T);
per_minute_discrepancy(_, _, _, _) ->
    {'error', 'not_required'}.

-spec get_authz_account_id(wh_json:object()) -> api_binary().
get_authz_account_id(JObj) ->
    wh_json:get_value([<<"custom_channel_vars">>, <<"account_id">>], JObj).
