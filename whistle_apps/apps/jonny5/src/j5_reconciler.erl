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

-include_lib("jonny5/src/jonny5.hrl").

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

-spec process_account/1 :: (ne_binary()) -> 'ok' | {'error', _}. 
process_account(Account) ->
    lager:debug("attempting to reconcile jonny5 credit/debit for account ~s", [Account]),
    AccountDb = wh_util:format_account_id(Account, encoded),
    ViewOptions = [reduce, group],
    case couch_mgr:get_results(AccountDb, <<"transactions/reconcile_by_callid">>, ViewOptions) of
        {error, _R}=E -> E;
        {ok, JObjs} ->
            _ = [correct_discrepancy(AccountDb, wh_json:get_value(<<"key">>, JObj), Amount)
                 || JObj <- JObjs
                        ,not wh_util:is_empty(Amount = wh_json:get_value(<<"value">>, JObj, 0))
                ],
            ok
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
    {ok, []}.

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
    {reply, {error, not_implemented}, State}.

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
handle_cast(process_account, []) ->
    timer:sleep(crypto:rand_uniform(10000, 30000)),
    gen_server:cast(self(), process_account),
    {noreply, wh_util:shuffle_list(whapps_util:get_all_accounts())};
handle_cast(process_account, [Account|Accounts]) ->
    put(callid, ?LOG_SYSTEM_ID),
    timer:sleep(crypto:rand_uniform(1000, 3000)),
    process_account(Account),
    gen_server:cast(self(), process_account),
    {noreply, Accounts};
handle_cast(_Msg, State) ->
    {noreply, State}.

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
    {noreply, State}.

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
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec correct_discrepancy/3 :: (ne_binary(), ne_binary(), integer()) -> {'ok', wh_json:json_object()} |
                                                                        {'error', _}.
correct_discrepancy(Ledger, CallId, Amount) ->
    put(callid, CallId),
    LedgerId = wh_util:format_account_id(Ledger, raw),
    LedgerDb = wh_util:format_account_id(Ledger, encoded),
    lager:debug("attempting to reconcile call id ~s discrepancy on ledger ~s for ~p", [LedgerId, CallId, Amount]),
    case should_correct_discrepancy(LedgerDb, CallId) of
        false -> ok;
        true ->
            Timestamp = wh_util:current_tstamp(),
            Type = case Amount > 0 of true -> <<"debit">>; false -> <<"credit">> end,
            Entry = wh_json:from_list([{<<"_id">>, correction_doc_id(CallId)}
                                       ,{<<"reason">>, <<"jonny5 discrepancy correction">>}
                                       ,{<<"account_id">>, LedgerId}
                                       ,{<<"call_id">>, CallId}
                                       ,{<<"amount">>, abs(Amount)}
                                       ,{<<"balance">>, j5_util:current_balance(Ledger)}
                                       ,{<<"pvt_account_id">>, LedgerId}
                                       ,{<<"pvt_account_db">>, LedgerDb}
                                       ,{<<"pvt_type">>, wh_util:to_binary(Type)}
                                       ,{<<"pvt_created">>, Timestamp}
                                       ,{<<"pvt_modified">>, Timestamp}
                                       ,{<<"pvt_vsn">>, 1}
                                       ,{<<"pvt_whapp">>, ?APP_NAME}
                                      ]),
            lager:debug("correcting $~p discrepancy for call ~s on ~s~n", [wapi_money:units_to_dollars(Amount), CallId, LedgerId]),
            case couch_mgr:save_doc(LedgerDb, Entry) of
                {error, _} -> ok;
                {ok, _} -> 
                    _ = send_system_alert(LedgerDb, CallId, Amount, Entry),
                    ok
            end
    end.

-spec should_correct_discrepancy/2 :: (ne_binary(), ne_binary()) -> boolean().
should_correct_discrepancy(LedgerDb, CallId) ->
    call_has_ended(CallId)
        andalso reconcile_grace_period_exceeded(LedgerDb, CallId) 
        andalso correction_not_already_attempted(LedgerDb, CallId).

-spec call_has_ended/1 :: (ne_binary()) -> boolean().
call_has_ended(CallId) ->
    case whapps_call_command:b_channel_status(CallId) of
        {ok, _} -> 
            lager:debug("ignoring discrepancy for active call", []),
            false;
        {error, _} -> true
    end.    

-spec reconcile_grace_period_exceeded/2 :: (ne_binary(), ne_binary()) -> boolean().
reconcile_grace_period_exceeded(LedgerDb, CallId) ->
    case couch_mgr:open_doc(LedgerDb, CallId) of
        {error, not_found} -> 
            lager:debug("correcting discrepancy for ended call with missing cdr", []),
            true;
        {error, _R} -> 
            lager:debug("ignoring discrepancy, unable to open cdr: ~p", [_R]),
            false;
        {ok, JObj} ->
            case wh_json:get_value([<<"custom_channel_vars">>, <<"account_billing">>], JObj) of                
                <<"per_minute">> ->
                    Current = wh_util:current_tstamp(),
                    Modified = wh_json:get_integer_value(<<"pvt_modified">>, JObj, Current),
                    Current - Modified > 300;
                _Else ->
                    lager:debug("billing type ~s does not require reconciliation, removing any existing discrepancy corrections", [_Else]),
                    DocId = correction_doc_id(CallId),
                    _ = couch_mgr:del_doc(LedgerDb, DocId),
                    false
            end            
    end.

-spec correction_not_already_attempted/2 :: (ne_binary(), ne_binary()) -> boolean().
correction_not_already_attempted(LedgerDb, CallId) ->
    DocId = correction_doc_id(CallId),
    case couch_mgr:open_doc(LedgerDb, DocId) of
        {error, not_found} -> true;
        {ok, JObj} -> maybe_remove_correction(JObj, LedgerDb, DocId);
        _Else -> false
    end.

-spec maybe_remove_correction/3 :: (wh_json:json_object(), ne_binary(), ne_binary()) -> boolean().
maybe_remove_correction(JObj, LedgerDb, DocId) ->
    Current = wh_util:current_tstamp(),
    Modified = wh_json:get_integer_value(<<"pvt_modified">>, JObj, Current), 
    case Current - Modified > 300 of
        false -> 
            %% If the correction was recently written, wait till the next cycle to make sure
            %% another j5_reconciler has not just placed this here
            false;
        true ->
            %% If the correction was made some time ago then remove it as it has not
            %% corrected the discrepancy, then wait for the next cycle to make sure
            %% it wasn't the sole cause of the issue.... (due to previous bug this could happen)
            lager:debug("removing erronous correction from previous reconciler version", []),
            _ = couch_mgr:del_doc(LedgerDb, DocId),
            false
    end.

-spec correction_doc_id/1 :: (ne_binary()) -> ne_binary().
correction_doc_id(CallId) ->
    <<CallId/binary, "-discrepancy">>.

-spec send_system_alert/4 :: (ne_binary(), ne_binary(), integer(), wh_json:json_object()) -> pid().
send_system_alert(Ledger, CallId, Amount, Entry) ->
    spawn(fun() ->
                  LedgerDb = wh_util:format_account_id(Ledger, encoded),
                  LedgerId = wh_util:format_account_id(Ledger, raw),
                  Dollars = wapi_money:units_to_dollars(abs(Amount)),
                  Account = case couch_mgr:open_cache_doc(LedgerDb, LedgerId) of
                                {ok, J} -> J;
                                {error, _} -> wh_json:new()
                            end,
                  AccountName = wh_json:get_value(<<"name">>, Account),
                  AccountRealm = wh_json:get_value(<<"realm">>, Account),
                  Balance = wapi_money:units_to_dollars(j5_util:current_balance(Ledger)),
                  Details = [{<<"Account-ID">>, LedgerId}
                             ,{<<"Account-Name">>, AccountName}
                             ,{<<"Account-Realm">>, AccountRealm}
                             ,{<<"Amount">>, <<"$", (wh_util:to_binary(Dollars))/binary>>}
                             ,{<<"Type">>, wh_json:get_value(<<"pvt_type">>, Entry)}
                             ,{<<"Call-ID">>, CallId}
                             ,{<<"New-Balance">>, <<"$", (wh_util:to_binary(Balance))/binary>>}
                            ],
                  wh_notify:system_alert("account $~p discrepancy / ~s (~s) / Call ~s", [Dollars, AccountName, LedgerId, CallId], Details)
          end).
