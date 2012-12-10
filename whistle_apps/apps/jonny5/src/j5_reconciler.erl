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
-spec correct_discrepancy/3 :: (ne_binary(), ne_binary(), integer()) -> wh_jobj_return().
correct_discrepancy(Ledger, CallId, Amount) ->
    put(callid, CallId),
    case should_correct_discrepancy(Ledger, CallId) of
        {error, _} -> ok;
        {ok, JObj} ->
            case per_minute_discrepancy(Amount, j5_util:get_limits(Ledger), JObj) of
                {error, _} -> ok;
                {ok, Entry} -> 
                    _ = send_system_alert(Ledger, CallId, Amount, Entry),
                    ok
            end
    end.

-spec should_correct_discrepancy/2 :: (ne_binary(), ne_binary()) -> wh_jobj_return().
should_correct_discrepancy(Ledger, CallId) ->
    case get_cdr(Ledger, CallId) of
        {error, _}=E -> E;
        {ok, JObj}=Ok ->
            DiscrepancyId = discrepancy_doc_id(JObj),
            case wh_json:get_value([<<"custom_channel_vars">>, <<"account_billing">>], JObj) =:=  <<"per_minute">> of
                false ->
                    lager:debug("billing type does not require reconciliation, removing any existing discrepancy corrections", []),
                    LedgerDb = wh_util:format_account_id(Ledger, encoded),                    
                    _ = couch_mgr:del_doc(LedgerDb, DiscrepancyId),
                    {error, not_per_minute};
                true ->
                    case reconcile_grace_period_exceeded(JObj) 
                        andalso not_already_attempted(Ledger, DiscrepancyId)
                    of
                        false -> {error, contention};
                        true -> Ok
                    end
            end
    end.

-spec get_cdr/2 :: (ne_binary(), ne_binary()) -> wh_jobj_return().
get_cdr(Ledger, CallId) ->
    LedgerDb = wh_util:format_account_id(Ledger, encoded),
    case couch_mgr:open_doc(LedgerDb, CallId) of
        {ok, _}=Ok -> Ok;
        {error, not_found} -> 
            maybe_create_cdr(Ledger, CallId)
    end.

-spec maybe_create_cdr/2 :: (ne_binary(), ne_binary()) -> wh_jobj_return().
maybe_create_cdr(Ledger, CallId) ->
    case call_has_ended(CallId) of
        false -> {error, call_active};
        true ->
            %% Call is down but cdr was not saved, crash?
            AccountDb = wh_util:format_account_id(Ledger, encoded),
            AccountId = wh_util:format_account_id(Ledger, raw),
            Timestamp = wh_util:to_binary(wh_util:current_tstamp() - 3600),
            CCVs = [{<<"account_id">>, AccountId}],
            Props = [{<<"_id">>, CallId}
                     ,{<<"call_id">>, CallId}
                     ,{<<"timestamp">>, Timestamp}
                     ,{<<"custom_channel_vars">>, wh_json:from_list(CCVs)}
                     ,{<<"pvt_account_id">>, AccountId}
                     ,{<<"pvt_account_db">>, AccountDb}
                     ,{<<"pvt_created">>, Timestamp}
                     ,{<<"pvt_modified">>, Timestamp}
                     ,{<<"pvt_type">>, <<"cdr">>}
                     ,{<<"pvt_vsn">>, <<"0">>}
                    ],
            {ok, wh_json:from_list(Props)}
    end.

-spec call_has_ended/1 :: (ne_binary()) -> boolean().
call_has_ended(CallId) ->
    case whapps_call_command:b_channel_status(CallId) of
        {ok, _} -> 
            lager:debug("ignoring discrepancy for active call", []),
            false;
        {error, _R} -> 
            true
    end.    

-spec reconcile_grace_period_exceeded/1 :: (wh_json:json_object()) -> boolean().
reconcile_grace_period_exceeded(JObj) ->
    Current = wh_util:current_tstamp(),
    Modified = wh_json:get_integer_value(<<"pvt_modified">>, JObj, Current), 
    Current - Modified > 300.

-spec not_already_attempted/2 :: (ne_binary(), ne_binary()) -> boolean().
not_already_attempted(LedgerDb, DocId) ->
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

-spec discrepancy_doc_id/1 :: (wh_json:json_object()) -> ne_binary().
discrepancy_doc_id(JObj) ->
    %% This needs to stay in sync with the write to ledger function in j5_util
    <<(j5_util:get_session_id(JObj))/binary, "-discrepancy">>.

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

-spec per_minute_discrepancy/3 :: (float(), #limits{}, wh_json:json_object()) -> wh_jobj_return().
per_minute_discrepancy(Units, #limits{account_id=AccountId}=Limits, JObj) when Units > 0 ->
    Props = [{<<"reason">>, <<"jonny5 discrepancy correction">>}
             ,{<<"balance">>, j5_util:current_balance(AccountId)}
             ,{<<"pvt_type">>, <<"debit">>}
            ],
    j5_util:write_to_ledger(<<"discrepancy">>, Props, Units, Limits, JObj);
per_minute_discrepancy(Units, #limits{account_id=AccountId}=Limits, JObj) when Units < 0 ->
    Props = [{<<"reason">>, <<"jonny5 discrepancy correction">>}
             ,{<<"balance">>, j5_util:current_balance(AccountId)}
             ,{<<"pvt_type">>, <<"credit">>}
            ],
    j5_util:write_to_ledger(<<"discrepancy">>, Props, Units, Limits, JObj);
per_minute_discrepancy(_, _, _) ->
    {ok, wh_json:new()}.
