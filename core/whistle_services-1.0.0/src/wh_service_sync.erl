%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_sync).

-behaviour(gen_server).

-export([start_link/0]).
-export([sync/1]).
-export([clean/1]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("whistle_services.hrl").

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
-spec start_link() -> startlink_ret().
start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec sync(ne_binary()) -> wh_std_return().
sync(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    wh_util:put_callid(<<AccountId/binary, "-sync">>),
    case wh_services:fetch_services_doc(AccountId, 'true') of
        {'error', _}=E -> E;
        {'ok', ServiceJObj} ->
            sync(AccountId, ServiceJObj)
    end.

-spec clean(ne_binary()) -> wh_std_return().
clean(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    case couch_mgr:open_doc(?WH_SERVICES_DB, AccountId) of
        {'error', _}=E -> E;
        {'ok', ServiceJObj} ->
            immediate_sync(AccountId, wh_doc:set_soft_deleted(ServiceJObj, 'true'))
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
-spec init([]) -> {'ok', #state{}}.
init([]) ->
    case whapps_config:get_is_true(?WHS_CONFIG_CAT, <<"sync_services">>, 'false') of
        'false' -> {'ok', #state{}};
        'true' ->
            _Ref = start_sync_service_timer(),
            {'ok', #state{}}
    end.

-spec start_sync_service_timer() -> reference().
start_sync_service_timer() ->
    ScanRate = whapps_config:get_integer(?WHS_CONFIG_CAT, <<"scan_rate">>, 20 * ?MILLISECONDS_IN_SECOND),
    erlang:send_after(ScanRate, self(), {'try_sync_service'}).

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
handle_info({'try_sync_service'}, State) ->
    _ = maybe_sync_service(),
    _Ref = start_sync_service_timer(),
    {'noreply', State};
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
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    lager:debug("whistle service sync terminating: ~p", [_Reason]).

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
-spec sync(ne_binary(), wh_json:object()) -> wh_std_return().
sync(AccountId, ServiceJObj) ->
    case get_billing_id(AccountId, ServiceJObj) of
        AccountId -> maybe_sync_services(AccountId, ServiceJObj);
        BillingId ->
            io:format("Account ~s is configured to use the credit card of ~s, following billing tree~n"
                ,[AccountId, BillingId]),
            sync(BillingId)
    end.

-spec maybe_sync_service() -> wh_std_return().
maybe_sync_service() ->
    SyncBufferPeriod = whapps_config:get_integer(?WHS_CONFIG_CAT, <<"sync_buffer_period">>, 600),
    ViewOptions = [{'limit', 1}
                   ,'include_docs'
                   ,{'endkey', wh_util:current_tstamp() - SyncBufferPeriod}
                  ],
    case couch_mgr:get_results(?WH_SERVICES_DB, <<"services/dirty">>, ViewOptions) of
        {'error', _}=E -> E;
        {'ok', [JObj]} -> bump_modified(wh_json:get_value(<<"doc">>, JObj));
        {'ok', _} -> {'error', 'no_dirty_services'}
    end.

-spec bump_modified(wh_json:object()) -> wh_std_return().
bump_modified(JObj) ->
    AccountId = wh_doc:account_id(JObj),
    Services = wh_services:reconcile_only(AccountId),
    'true' = (Services =/= 'false'),

    UpdatedJObj = wh_json:set_values([{<<"pvt_modified">>, wh_util:current_tstamp()}
                                      ,{<<"_rev">>, wh_doc:revision(JObj)}
                                     ]
                                     ,wh_services:to_json(Services)
                                    ),
    case couch_mgr:save_doc(?WH_SERVICES_DB, UpdatedJObj) of
        {'error', _}=E ->
            %% If we conflict or cant save the doc with a new modified timestamp
            %% then another process is probably handling it, move on
            E;
        {'ok', NewJObj} ->
            %% If we can change the timestamp then (since the view requires the
            %% modified time to be x mins in the past) we have gain exclusive
            %% control for x mins.... good luck!
            [RevNum, _] = binary:split(wh_doc:revision(NewJObj), <<"-">>),
            put('callid', <<AccountId/binary, "-", RevNum/binary>>),
            lager:debug("start synchronization of services with bookkeepers"),
            maybe_follow_billing_id(AccountId, NewJObj)
    end.

-spec maybe_follow_billing_id(ne_binary(), wh_json:object()) -> wh_std_return().
maybe_follow_billing_id(AccountId, ServiceJObj) ->
    case get_billing_id(AccountId, ServiceJObj) of
        AccountId -> maybe_sync_services(AccountId, ServiceJObj);
        BillingId -> follow_billing_id(BillingId, AccountId, ServiceJObj)
    end.

-spec follow_billing_id(ne_binary(), ne_binary(), wh_json:object()) -> wh_std_return().
follow_billing_id(BillingId, AccountId, ServiceJObj) ->
    %% NOTE: First try to make the parent (to be billed) as dirty
    %%  if that is successful then mark the current service doc cleans
    case mark_dirty(BillingId) of
        {'ok', _} ->
            lager:debug("following billing id ~s", [BillingId]),
            mark_clean(ServiceJObj);
        {'error', 'not_found'} ->
            maybe_update_billing_id(BillingId, AccountId, ServiceJObj);
        {'error', _R}=E ->
            lager:debug("unable to mark billing services ~s dirty: ~p", [BillingId, _R]),
            E
    end.

-spec maybe_sync_services(ne_binary(), wh_json:object()) -> wh_std_return().
maybe_sync_services(AccountId, ServiceJObj) ->
    case wh_service_plans:create_items(ServiceJObj) of
        {'error', 'no_plans'} ->
            lager:debug("no services plans found"),
            _ = mark_clean_and_status(kzd_services:status_good(), ServiceJObj),
            maybe_sync_reseller(AccountId, ServiceJObj);
        {'ok', ServiceItems} ->
            sync_services(AccountId, ServiceJObj, ServiceItems)
    end.

-spec sync_services(ne_binary(), wh_json:object(), wh_service_items:items()) -> wh_std_return().
sync_services(AccountId, ServiceJObj, ServiceItems) ->
    try sync_services_bookkeeper(AccountId, ServiceJObj, ServiceItems) of
        'ok' ->
            _ = mark_clean_and_status(kzd_services:status_good(), ServiceJObj),
            io:format("synchronization with bookkeeper complete~n"),
            lager:debug("synchronization with bookkeeper complete"),
            maybe_sync_reseller(AccountId, ServiceJObj)
    catch
        'throw':{Reason, _}=_R ->
            lager:info("bookkeeper error: ~p", [_R]),
            _ = mark_clean_and_status(wh_util:to_binary(Reason), ServiceJObj),
            maybe_sync_reseller(AccountId, ServiceJObj);
        _E:R ->
            %% TODO: certain errors (such as no CC or expired, ect) should
            %%    move the account of good standing...
            lager:info("unable to sync services(~p): ~p", [_E, R]),
            {'error', R}
    end.

-spec sync_services_bookkeeper(ne_binary(), wh_json:object(), wh_service_items:items()) -> 'ok'.
sync_services_bookkeeper(AccountId, ServiceJObj, ServiceItems) ->
    Bookkeeper = wh_services:select_bookkeeper(AccountId),
    Bookkeeper:sync(ServiceItems, AccountId),
    case wh_json:get_value(<<"transactions">>, ServiceJObj, []) of
        [] -> 'ok';
        Transactions ->
            BillingId = wh_json:get_value(<<"billing_id">>, ServiceJObj),
            FailedTransactions = Bookkeeper:charge_transactions(BillingId, Transactions),
            case couch_mgr:save_doc(?WH_SERVICES_DB, wh_json:set_value(<<"transactions">>, FailedTransactions, ServiceJObj)) of
                {'error', _E} -> lager:warning("failed to clean pending transactions ~p", [_E]);
                {'ok', _} -> handle_topup_transactions(AccountId, Transactions, FailedTransactions)
            end
    end.

-spec handle_topup_transactions(ne_binary(), wh_json:objects(), wh_json:objects() | integer()) -> 'ok'.
handle_topup_transactions(Account, JObjs, Failed) when is_list(Failed) ->
    case did_topup_failed(Failed) of
        'true' -> 'ok';
        'false' -> handle_topup_transactions(Account, JObjs, 3)
    end;
handle_topup_transactions(_, [], _) -> 'ok';
handle_topup_transactions(Account, [JObj|JObjs], Retry) when Retry > 0 ->
    case wh_json:get_integer_value(<<"pvt_code">>, JObj) of
        ?CODE_TOPUP ->
            Amount = wh_json:get_value(<<"pvt_amount">>, JObj),
            Transaction = wh_transaction:credit(Account, Amount),
            Transaction1 = wh_transaction:set_reason(<<"topup">>, Transaction),
            case wh_transaction:save(Transaction1) of
                {'ok', _} -> 'ok';
                {'error', 'conflict'} ->
                    lager:warning("did not write top up transaction for account ~s already exist for today", [Account]);
                {'error', _E} ->
                    lager:error("failed to write top up transaction ~p , for account ~s (amount: ~p), retrying ~p..."
                                ,[_E, Account, Amount, Retry]
                               ),
                    handle_topup_transactions(Account, [JObj|JObjs], Retry-1)
            end;
        _ -> handle_topup_transactions(Account, JObjs, 3)
    end;
handle_topup_transactions(Account, _, _) ->
    lager:error("failed to write top up transaction for account ~s too many retries", [Account]).

-spec did_topup_failed(wh_json:objects()) -> boolean().
did_topup_failed(JObjs) ->
    lists:foldl(
        fun(JObj, Acc) ->
            case wh_json:get_integer_value(<<"pvt_code">>, JObj) of
                ?CODE_TOPUP -> 'true';
                _ -> Acc
            end
        end
        ,'false'
        ,JObjs
    ).

-spec maybe_sync_reseller(ne_binary(), wh_json:object()) -> wh_std_return().
maybe_sync_reseller(AccountId, ServiceJObj) ->
    case kzd_services:reseller_id(ServiceJObj, AccountId) of
        AccountId -> {'ok', ServiceJObj};
        ResellerId ->
            lager:debug("marking reseller ~s as dirty", [ResellerId]),
            mark_dirty(ResellerId)
    end.

-spec get_billing_id(ne_binary(), wh_json:object()) -> ne_binary().
get_billing_id(AccountId, JObj) ->
    case kzd_services:is_reseller(JObj) of
        'true' -> AccountId;
        'false' -> kzd_services:billing_id(JObj, AccountId)
    end.

-spec mark_dirty(ne_binary() | wh_json:object()) -> wh_std_return().
mark_dirty(AccountId) when is_binary(AccountId) ->
    case couch_mgr:open_doc(?WH_SERVICES_DB, AccountId) of
        {'error', _}=E -> E;
        {'ok', JObj} -> mark_dirty(JObj)
    end;
mark_dirty(JObj) ->
    couch_mgr:save_doc(?WH_SERVICES_DB
                       ,wh_json:set_values([{<<"pvt_dirty">>, 'true'}
                                            ,{<<"pvt_modified">>, wh_util:current_tstamp()}
                                           ]
                                           ,JObj
                                          )
                      ).

-spec mark_clean(wh_json:object()) -> wh_std_return().
mark_clean(JObj) ->
    couch_mgr:save_doc(?WH_SERVICES_DB, kzd_services:set_is_dirty(JObj, 'false')).

-spec mark_clean_and_status(ne_binary(), wh_json:object()) -> wh_std_return().
mark_clean_and_status(Status, JObj) ->
    lager:debug("marking services clean with status ~s", [Status]),
    couch_mgr:save_doc(?WH_SERVICES_DB, wh_json:set_values([{<<"pvt_dirty">>, 'false'}
                                                            ,{<<"pvt_status">>, Status}
                                                           ], JObj)).

-spec maybe_update_billing_id(ne_binary(), ne_binary(), wh_json:object()) -> wh_std_return().
maybe_update_billing_id(BillingId, AccountId, ServiceJObj) ->
    case couch_mgr:open_doc(?WH_ACCOUNTS_DB, BillingId) of
        {'error', _} ->
            lager:debug("billing id ~s on ~s does not exist anymore, updating to bill self"
                        ,[BillingId, AccountId]),
            couch_mgr:save_doc(?WH_SERVICES_DB, kzd_services:set_billing_id(ServiceJObj, AccountId));
        {'ok', JObj} ->
            case wh_doc:is_soft_deleted(JObj) of
                'false' -> wh_services:reconcile(BillingId);
                'true' ->
                    lager:debug("billing id ~s on ~s was deleted, updating to bill self"
                                ,[BillingId, AccountId]),
                    couch_mgr:save_doc(?WH_SERVICES_DB, kzd_services:set_billing_id(ServiceJObj, AccountId))
            end
    end.

-spec immediate_sync(ne_binary(), wh_json:object()) -> wh_std_return().
immediate_sync(AccountId, ServiceJObj) ->
    case wh_service_plans:create_items(ServiceJObj) of
        {'error', 'no_plans'}=E -> E;
        {'ok', ServiceItems} ->
            %% TODO: support other bookkeepers...
            try wh_bookkeeper_braintree:sync(ServiceItems, AccountId) of
                _ -> {'ok', ServiceJObj}
            catch
                'throw':{_, R} -> {'error', R};
                _E:R -> {'error', R}
            end
    end.
