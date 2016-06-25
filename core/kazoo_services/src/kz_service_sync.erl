%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_service_sync).

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

-include_lib("kazoo_services/src/kazoo_services.hrl").

-define(SERVER, ?MODULE).

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

-spec sync(ne_binary()) -> kz_std_return().
sync(Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    kz_util:put_callid(<<AccountId/binary, "-sync">>),
    case kz_services:fetch_services_doc(AccountId, 'true') of
        {'error', _}=E -> E;
        {'ok', ServicesJObj} ->
            sync(AccountId, ServicesJObj)
    end.

-spec clean(ne_binary()) -> kz_std_return().
clean(Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    case kz_datamgr:open_doc(?KZ_SERVICES_DB, AccountId) of
        {'error', _}=E -> E;
        {'ok', ServicesJObj} ->
            immediate_sync(AccountId, kz_doc:set_soft_deleted(ServicesJObj, 'true'))
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
    io:format("getting ~s sync_services~n", [?WHS_CONFIG_CAT]),
    lager:debug("getting ~s sync_services", [?WHS_CONFIG_CAT]),
    case kapps_config:get_is_true(?WHS_CONFIG_CAT, <<"sync_services">>, 'false') of
        'false' ->
            io:format("not starting sync services~n"),
            lager:debug("not starting sync services"),
            {'ok', #state{}};
        'true' ->
            _Ref = start_sync_service_timer(),
            io:format("started sync services ~p\n", [_Ref]),
            lager:debug("started sync services ~p", [_Ref]),
            {'ok', #state{}}
    end.

-spec start_sync_service_timer() -> reference().
start_sync_service_timer() ->
    ScanRate = kapps_config:get_integer(?WHS_CONFIG_CAT, <<"scan_rate">>, 20 * ?MILLISECONDS_IN_SECOND),
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
    _ = maybe_clear_process_dictionary(),
    _Ref = start_sync_service_timer(),
    {'noreply', State, 'hibernate'};
handle_info(_Info, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
    {'noreply', State}.

-spec maybe_clear_process_dictionary() -> 'ok'.
maybe_clear_process_dictionary() ->
    lists:foreach(fun maybe_clear_dictionary_entry/1, get()).

-spec maybe_clear_dictionary_entry({any(), any()}) -> any().
maybe_clear_dictionary_entry({{'phone_number_doc', _AccountId}=Key, _Doc}) ->
    erase(Key);
maybe_clear_dictionary_entry(_) -> 'ok'.

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
    lager:debug("kazoo service sync terminating: ~p", [_Reason]).

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
-spec sync(ne_binary(), kz_json:object()) -> kz_std_return().
sync(AccountId, ServicesJObj) ->
    case get_billing_id(AccountId, ServicesJObj) of
        AccountId -> maybe_sync_services(AccountId, ServicesJObj);
        BillingId ->
            io:format("Account ~s is configured to use the credit card of ~s, following billing tree~n"
                     ,[AccountId, BillingId]),
            lager:debug("account ~s is configured to use the credit card of ~s, following billing tree"
                       ,[AccountId, BillingId]),
            sync(BillingId)
    end.

-spec maybe_sync_service() -> kz_std_return().
maybe_sync_service() ->
    SyncBufferPeriod = kapps_config:get_integer(?WHS_CONFIG_CAT, <<"sync_buffer_period">>, 600),
    ViewOptions = [{'limit', 1}
                   ,'include_docs'
                   ,{'endkey', kz_util:current_tstamp() - SyncBufferPeriod}
                  ],
    case kz_datamgr:get_results(?KZ_SERVICES_DB, <<"services/dirty">>, ViewOptions) of
        {'error', _}=E -> E;
        {'ok', [JObj]} -> bump_modified(kz_json:get_value(<<"doc">>, JObj));
        {'ok', _} -> {'error', 'no_dirty_services'}
    end.

-spec bump_modified(kz_json:object()) -> kz_std_return().
bump_modified(JObj) ->
    AccountId = kz_doc:account_id(JObj),
    Services = kz_services:reconcile_only(AccountId),
    'true' = (Services =/= 'false'),

    UpdatedServicesJObj =
        kz_json:set_values([{<<"pvt_modified">>, kz_util:current_tstamp()}
                           ,{<<"_rev">>, kz_doc:revision(JObj)}
                           ]
                          ,kz_services:to_json(Services)
                          ),
    case kz_datamgr:save_doc(?KZ_SERVICES_DB, UpdatedServicesJObj) of
        {'error', _}=E ->
            %% If we conflict or cant save the doc with a new modified timestamp
            %% then another process is probably handling it, move on
            E;
        {'ok', ServicesJObj} ->
            %% If we can change the timestamp then (since the view requires the
            %% modified time to be x mins in the past) we have gain exclusive
            %% control for x mins.... good luck!
            [RevNum, _] = binary:split(kz_doc:revision(ServicesJObj), <<"-">>),
            kz_util:put_callid(<<AccountId/binary, "-", RevNum/binary>>),
            lager:debug("start synchronization of services with bookkeepers"),
            maybe_follow_billing_id(AccountId, ServicesJObj)
    end.

-spec maybe_follow_billing_id(ne_binary(), kzd_services:doc()) -> kz_std_return().
maybe_follow_billing_id(AccountId, ServicesJObj) ->
    case get_billing_id(AccountId, ServicesJObj) of
        AccountId -> maybe_sync_services(AccountId, ServicesJObj);
        BillingId -> follow_billing_id(BillingId, AccountId, ServicesJObj)
    end.

-spec follow_billing_id(ne_binary(), ne_binary(), kz_json:object()) -> kz_std_return().
follow_billing_id(BillingId, AccountId, ServicesJObj) ->
    %% NOTE: First try to make the parent (to be billed) as dirty
    %%  if that is successful then mark the current service doc cleans
    case mark_dirty(BillingId) of
        {'ok', _} ->
            lager:debug("following billing id ~s", [BillingId]),
            mark_clean(ServicesJObj);
        {'error', 'not_found'} ->
            maybe_update_billing_id(BillingId, AccountId, ServicesJObj);
        {'error', _R}=E ->
            lager:debug("unable to mark billing services ~s dirty: ~p", [BillingId, _R]),
            E
    end.

-spec maybe_sync_services(ne_binary(), kzd_services:doc()) -> kz_std_return().
maybe_sync_services(AccountId, ServicesJObj) ->
    case kz_service_plans:create_items(ServicesJObj) of
        {'error', 'no_plans'} ->
            lager:debug("no services plans found"),
            _ = maybe_sync_transactions(AccountId, ServicesJObj),
            _ = mark_clean_and_status(kzd_services:status_good(), ServicesJObj),
            maybe_sync_reseller(AccountId, ServicesJObj);
        {'ok', ServiceItems} ->
            sync_services(AccountId, ServicesJObj, ServiceItems)
    end.

-spec sync_services(ne_binary(), kzd_services:doc(), kz_service_items:items()) -> kz_std_return().
sync_services(AccountId, ServicesJObj, ServiceItems) ->
    try sync_services_bookkeeper(AccountId, ServicesJObj, ServiceItems) of
        'ok' ->
            _ = mark_clean_and_status(kzd_services:status_good(), ServicesJObj),
            io:format("synchronization with bookkeeper complete~n"),
            lager:debug("synchronization with bookkeeper complete"),
            maybe_sync_reseller(AccountId, ServicesJObj)
    catch
        'throw':{Reason, _}=_R ->
            lager:info("bookkeeper error: ~p", [_R]),
            _ = mark_clean_and_status(kz_util:to_binary(Reason), ServicesJObj),
            maybe_sync_reseller(AccountId, ServicesJObj);
        _E:R ->
            %% TODO: certain errors (such as no CC or expired, etc) should
            %%    move the account of good standing...
            lager:info("unable to sync services(~p): ~p", [_E, R]),
            {'error', R}
    end.

-spec sync_services_bookkeeper(ne_binary(), kz_json:object(), kz_service_items:items()) -> 'ok'.
sync_services_bookkeeper(AccountId, ServicesJObj, ServiceItems) ->
    Bookkeeper = kz_services:select_bookkeeper(AccountId),
    Bookkeeper:sync(ServiceItems, AccountId),

    maybe_sync_transactions(AccountId, ServicesJObj, Bookkeeper).

-spec maybe_sync_transactions(ne_binary(), kzd_services:doc()) -> 'ok'.
-spec maybe_sync_transactions(ne_binary(), kzd_services:doc(), atom()) -> 'ok'.
maybe_sync_transactions(AccountId, ServicesJObj) ->
    Bookkeeper = kz_services:select_bookkeeper(AccountId),
    maybe_sync_transactions(AccountId, ServicesJObj, Bookkeeper).

maybe_sync_transactions(AccountId, ServicesJObj, Bookkeeper) ->
    case kzd_services:transactions(ServicesJObj) of
        [] -> 'ok';
        Transactions ->
            sync_transactions(AccountId, ServicesJObj, Bookkeeper, Transactions)
    end.

-spec sync_transactions(ne_binary(), kzd_services:doc(), atom(), kz_json:objects()) ->
                               'ok'.
sync_transactions(AccountId, ServicesJObj, Bookkeeper, Transactions) ->
    BillingId = kzd_services:billing_id(ServicesJObj),
    FailedTransactions = Bookkeeper:charge_transactions(BillingId, Transactions),
    case kz_datamgr:save_doc(?KZ_SERVICES_DB
                           ,kzd_services:set_transactions(ServicesJObj, FailedTransactions)
                           )
    of
        {'error', _E} ->
            lager:warning("failed to clean pending transactions ~p", [_E]);
        {'ok', _} ->
            handle_topup_transactions(AccountId, Transactions, FailedTransactions)
    end.

-spec handle_topup_transactions(ne_binary(), kz_json:objects(), kz_json:objects() | integer()) -> 'ok'.
handle_topup_transactions(Account, JObjs, Failed) when is_list(Failed) ->
    case did_topup_failed(Failed) of
        'true' -> 'ok';
        'false' -> handle_topup_transactions(Account, JObjs, 3)
    end;
handle_topup_transactions(_, [], _) -> 'ok';
handle_topup_transactions(Account, [JObj|JObjs]=List, Retry) when Retry > 0 ->
    case kz_json:get_integer_value(<<"pvt_code">>, JObj) of
        ?CODE_TOPUP ->
            Amount = kz_json:get_value(<<"pvt_amount">>, JObj),
            Transaction = kz_transaction:credit(Account, Amount),
            Transaction1 = kz_transaction:set_reason(<<"topup">>, Transaction),
            case kz_transaction:save(Transaction1) of
                {'ok', _} -> 'ok';
                {'error', 'conflict'} ->
                    lager:warning("did not write top up transaction for account ~s already exist for today", [Account]);
                {'error', _E} ->
                    lager:error("failed to write top up transaction ~p , for account ~s (amount: ~p), retrying ~p..."
                                ,[_E, Account, Amount, Retry]
                               ),
                    handle_topup_transactions(Account, List, Retry-1)
            end;
        _ -> handle_topup_transactions(Account, JObjs, 3)
    end;
handle_topup_transactions(Account, _, _) ->
    lager:error("failed to write top up transaction for account ~s too many retries", [Account]).

-spec did_topup_failed(kz_json:objects()) -> boolean().
did_topup_failed(JObjs) ->
    lists:foldl(
        fun(JObj, Acc) ->
            case kz_json:get_integer_value(<<"pvt_code">>, JObj) of
                ?CODE_TOPUP -> 'true';
                _ -> Acc
            end
        end
        ,'false'
        ,JObjs
    ).

-spec maybe_sync_reseller(ne_binary(), kzd_services:doc()) -> kz_std_return().
maybe_sync_reseller(AccountId, ServicesJObj) ->
    case kzd_services:reseller_id(ServicesJObj, AccountId) of
        AccountId -> {'ok', ServicesJObj};
        ResellerId ->
            lager:debug("marking reseller ~s as dirty", [ResellerId]),
            mark_dirty(ResellerId)
    end.

-spec get_billing_id(ne_binary(), kzd_services:doc()) -> ne_binary().
get_billing_id(AccountId, ServicesJObj) ->
    case kzd_services:is_reseller(ServicesJObj) of
        'true' -> AccountId;
        'false' ->
            case ?SUPPORT_BILLING_ID of
                'true' -> kzd_services:billing_id(ServicesJObj, AccountId);
                'false' -> AccountId
            end
    end.

-spec mark_dirty(ne_binary() | kzd_services:doc()) -> kz_std_return().
mark_dirty(AccountId) when is_binary(AccountId) ->
    case kz_datamgr:open_doc(?KZ_SERVICES_DB, AccountId) of
        {'error', _}=E -> E;
        {'ok', ServicesJObj} -> mark_dirty(ServicesJObj)
    end;
mark_dirty(ServicesJObj) ->
    kz_datamgr:save_doc(?KZ_SERVICES_DB
                       ,kz_json:set_values([{<<"pvt_dirty">>, 'true'}
                                            ,{<<"pvt_modified">>, kz_util:current_tstamp()}
                                           ]
                                          ,ServicesJObj
                                          )
                      ).

-spec mark_clean(kzd_services:doc()) -> kz_std_return().
mark_clean(ServicesJObj) ->
    kz_datamgr:save_doc(?KZ_SERVICES_DB
                      ,kzd_services:set_is_dirty(ServicesJObj, 'false')
                      ).

-spec mark_clean_and_status(ne_binary(), kzd_services:doc()) -> kz_std_return().
mark_clean_and_status(Status, ServicesJObj) ->
    lager:debug("marking services clean with status ~s", [Status]),
    kz_datamgr:save_doc(?KZ_SERVICES_DB
                      ,kz_json:set_values([{<<"pvt_dirty">>, 'false'}
                                          ,{<<"pvt_status">>, Status}
                                          ]
                                         ,ServicesJObj
                                         )
                      ).

-spec maybe_update_billing_id(ne_binary(), ne_binary(), kz_json:object()) -> kz_std_return().
maybe_update_billing_id(BillingId, AccountId, ServicesJObj) ->
    case kz_datamgr:open_doc(?KZ_ACCOUNTS_DB, BillingId) of
        {'error', _} ->
            lager:debug("billing id ~s on ~s does not exist anymore, updating to bill self"
                        ,[BillingId, AccountId]
                       ),
            kz_datamgr:save_doc(?KZ_SERVICES_DB, kzd_services:set_billing_id(ServicesJObj, AccountId));
        {'ok', JObj} ->
            case kz_doc:is_soft_deleted(JObj) of
                'false' -> kz_services:reconcile(BillingId);
                'true' ->
                    lager:debug("billing id ~s on ~s was deleted, updating to bill self"
                                ,[BillingId, AccountId]
                               ),
                    kz_datamgr:save_doc(?KZ_SERVICES_DB, kzd_services:set_billing_id(ServicesJObj, AccountId))
            end
    end.

-spec immediate_sync(ne_binary(), kzd_services:doc()) -> kz_std_return().
immediate_sync(AccountId, ServicesJObj) ->
    case kz_service_plans:create_items(ServicesJObj) of
        {'error', 'no_plans'}=E -> E;
        {'ok', ServiceItems} ->
            %% TODO: support other bookkeepers...
            try kz_bookkeeper_braintree:sync(ServiceItems, AccountId) of
                _ -> {'ok', ServicesJObj}
            catch
                'throw':{_, R} -> {'error', R};
                _E:R -> {'error', R}
            end
    end.
