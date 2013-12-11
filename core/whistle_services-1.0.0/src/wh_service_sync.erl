%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
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
    immediate_sync(Account).

-spec clean(ne_binary()) -> wh_std_return().
clean(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    case couch_mgr:open_doc(?WH_SERVICES_DB, AccountId) of
        {'error', _}=E -> E;
        {'ok', ServiceJObj} ->
            immediate_sync(AccountId, wh_json:set_value(<<"pvt_deleted">>, 'true', ServiceJObj))
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
            ScanRate = whapps_config:get_integer(?WHS_CONFIG_CAT, <<"scan_rate">>, 20000),
            _TRef = erlang:send_after(ScanRate, self(), {'try_sync_service'}),
            {'ok', #state{}}
    end.

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
    ScanRate = whapps_config:get_integer(?WHS_CONFIG_CAT, <<"scan_rate">>, 20000),
    _TRef = erlang:send_after(ScanRate, self(), {'try_sync_service'}),
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
    AccountId = wh_json:get_value(<<"pvt_account_id">>, JObj),
    UpdatedJObj = wh_json:set_value(<<"pvt_modified">>, wh_util:current_tstamp(), JObj),
    case couch_mgr:save_doc(?WH_SERVICES_DB, UpdatedJObj) of
        {'error', _}=E ->
            %% If we conflict or cant save the doc with a new modified timestamp
            %% then another process is probably handling it, move on
            E;
        {'ok', NewJObj} ->
            %% If we can change the timestamp then (since the view requires the
            %% modified time to be x mins in the past) we have gain exclusive
            %% control for x mins.... good luck!
            [RevNum, _] = binary:split(wh_json:get_value(<<"_rev">>, NewJObj), <<"-">>),
            put('callid', <<AccountId/binary, "-", RevNum/binary>>),
            lager:debug("start synchronization of services with bookkeepers", []),
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
            lager:debug("no services plans found", []),
            _ = mark_clean_and_status(<<"good_standing">>, ServiceJObj),
            maybe_sync_reseller(AccountId, ServiceJObj);
        {'ok', ServiceItems} ->
            sync_services(AccountId, ServiceJObj, ServiceItems)
    end.

-spec sync_services(ne_binary(), wh_json:object(), wh_service_items:items()) -> wh_std_return().
sync_services(AccountId, ServiceJObj, ServiceItems) ->
    try sync_services_bookkeeper(AccountId, ServiceJObj, ServiceItems) of
        'ok' ->
            _ = mark_clean_and_status(<<"good_standing">>, ServiceJObj),
            lager:debug("synchronization with bookkeeper complete", []),
            maybe_sync_reseller(AccountId, ServiceJObj)
    catch
        'throw':{Reason, _}=_R ->
            lager:info("bookkeeper error: ~p", [_R]),
            _ = mark_status(wh_util:to_binary(Reason), ServiceJObj),
            maybe_sync_reseller(AccountId, ServiceJObj);
        _E:R ->
            %% TODO: certain errors (such as no CC or expired, ect) should
            %% move the account of good standing...
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
            Bookkeeper:charge_transaction(BillingId, Transactions),
            case couch_mgr:save_doc(?WH_SERVICES_DB, wh_json:set_value(<<"transactions">>, [], ServiceJObj)) of
                {'error', _E} -> lager:warning("failed to clean pending transactions ~p", [_E]);
                {'ok', _} -> 'ok'
            end
    end.

-spec maybe_sync_reseller(ne_binary(), wh_json:object()) -> wh_std_return().
maybe_sync_reseller(AccountId, ServiceJObj) ->
    case wh_json:get_ne_value(<<"pvt_reseller_id">>, ServiceJObj, AccountId) of
        AccountId -> {'ok', ServiceJObj};
        ResellerId ->
            lager:debug("marking reseller ~s as dirty", [ResellerId]),
            mark_dirty(ResellerId)
    end.

-spec get_billing_id(ne_binary(), wh_json:object()) -> ne_binary().
get_billing_id(AccountId, JObj) ->
    case wh_json:is_true(<<"pvt_reseller">>, JObj) of
        'true' -> AccountId;
        'false' -> wh_json:get_ne_value(<<"billing_id">>, JObj, AccountId)
    end.

-spec mark_dirty(ne_binary() | wh_json:object()) -> wh_std_return().
mark_dirty(AccountId) when is_binary(AccountId) ->
    case couch_mgr:open_doc(?WH_SERVICES_DB, AccountId) of
        {'error', _}=E -> E;
        {'ok', JObj} -> mark_dirty(JObj)
    end;
mark_dirty(JObj) ->
    couch_mgr:save_doc(?WH_SERVICES_DB, wh_json:set_values([{<<"pvt_dirty">>, 'true'}
                                                            ,{<<"pvt_modified">>, wh_util:current_tstamp()}
                                                           ], JObj)).

-spec mark_clean(wh_json:object()) -> wh_std_return().
mark_clean(JObj) ->
    couch_mgr:save_doc(?WH_SERVICES_DB, wh_json:set_value(<<"pvt_dirty">>, 'false', JObj)).

-spec mark_clean_and_status(ne_binary(), wh_json:object()) -> wh_std_return().
mark_clean_and_status(Status, JObj) ->
    lager:debug("marking services clean with status ~s", [Status]),
    couch_mgr:save_doc(?WH_SERVICES_DB, wh_json:set_values([{<<"pvt_dirty">>, 'false'}
                                                            ,{<<"pvt_status">>, Status}
                                                           ], JObj)).

-spec mark_status(ne_binary(), wh_json:object()) -> wh_std_return().
mark_status(Status, JObj) ->
    lager:debug("marking services with status ~s", [Status]),
    couch_mgr:save_doc(?WH_SERVICES_DB, wh_json:set_values([{<<"pvt_modified">>, wh_util:current_tstamp()}
                                                            ,{<<"pvt_status">>, Status}
                                                           ], JObj)).

-spec maybe_update_billing_id(ne_binary(), ne_binary(), wh_json:object()) -> wh_std_return().
maybe_update_billing_id(BillingId, AccountId, ServiceJObj) ->
    case couch_mgr:open_doc(?WH_ACCOUNTS_DB, BillingId) of
        {'error', _} ->
            lager:debug("billing id ~s on ~s does not exist anymore, updating to bill self", [BillingId, AccountId]),
            couch_mgr:save_doc(?WH_SERVICES_DB, wh_json:set_value(<<"billing_id">>, AccountId, ServiceJObj));
        {'ok', JObj} ->
            case wh_json:is_true(<<"pvt_deleted">>, JObj) of
                'false' -> wh_services:reconcile(BillingId);
                'true' ->
                    lager:debug("billing id ~s on ~s was deleted, updating to bill self", [BillingId, AccountId]),
                    couch_mgr:save_doc(?WH_SERVICES_DB, wh_json:set_value(<<"billing_id">>, AccountId, ServiceJObj))
            end
    end.

-spec immediate_sync(ne_binary()) -> wh_std_return().
-spec immediate_sync(ne_binary(), wh_json:object()) -> wh_std_return().

immediate_sync(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    case couch_mgr:open_doc(?WH_SERVICES_DB, AccountId) of
        {'error', _}=E -> E;
        {'ok', ServiceJObj} ->
            immediate_sync(AccountId, ServiceJObj)
    end.

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
