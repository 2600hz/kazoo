%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2018, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_transactions).

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,validate/1, validate/2
        ,to_csv/1
        ,put/2
        ,delete/2
        ]).

-include("crossbar.hrl").
-include_lib("kazoo_transactions/include/kazoo_transactions.hrl").

-define(CURRENT_BALANCE, <<"current_balance">>).
-define(MONTHLY, <<"monthly_recurring">>). %% wht_util:monthly_recurring()
-define(SUBSCRIPTIONS, <<"subscriptions">>).
-define(CREDIT, <<"credit">>).
-define(DEBIT, <<"debit">>).

-type payload() :: {cowboy_req:req(), cb_context:context()}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.transactions">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.transactions">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.transactions">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.transactions">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.transactions">>, ?MODULE, 'delete'),
    _ = crossbar_bindings:bind(<<"*.to_csv.get.transactions">>, ?MODULE, 'to_csv').

-spec to_csv(payload()) -> payload().
to_csv({Req, Context}) ->
    JObjs = flatten(cb_context:resp_data(Context), []),
    {Req, cb_context:set_resp_data(Context, JObjs)}.

-spec flatten(kz_json:objects(), kz_json:objects()) -> kz_json:objects().
flatten([], Results) ->
    wht_util:collapse_call_transactions(Results);
flatten([JObj|JObjs], Results) ->
    Metadata = kz_json:get_ne_value(<<"metadata">>, JObj),
    case kz_json:is_json_object(Metadata) of
        'true' ->
            Props = kz_json:to_proplist(Metadata),
            flatten(JObjs, [kz_json:set_values(Props, JObj)|Results]);
        'false' ->
            flatten(JObjs, [JObj|Results])
    end;
flatten(Else, _) -> Else.

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?CURRENT_BALANCE) ->
    [?HTTP_GET];
allowed_methods(?CREDIT) ->
    [?HTTP_PUT];
allowed_methods(?DEBIT) ->
    [?HTTP_DELETE];
allowed_methods(?MONTHLY) ->
    [?HTTP_GET];
allowed_methods(?SUBSCRIPTIONS) ->
    [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% For example:
%%
%% ```
%%    /transactions => []
%%    /transactions/foo => [<<"foo">>]
%%    /transactions/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /transactions might load a list of transactions objects
%% /transactions/123 might load the transactions object 123
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_transactions(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, PathToken) ->
    validate_transaction(Context, PathToken, cb_context:req_verb(Context)).


%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, ?CREDIT) ->
    case cb_context:resp_status(Context) of
        'success' -> maybe_credit_billing_id(Context);
        _Error -> Context
    end.

-spec maybe_credit_billing_id(cb_context:context()) -> cb_context:context().
maybe_credit_billing_id(Context) ->
    AuthAccountId = cb_context:auth_account_id(Context),
    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
    CreditAccountId = cb_context:account_id(Context),

    case kz_services:find_reseller_id(CreditAccountId) of
        MasterAccountId when AuthAccountId =:= MasterAccountId
                             andalso CreditAccountId =:= MasterAccountId ->
            lager:debug("master account is about to credit himself - should be always free"),
            free_credit(Context);
        MasterAccountId when AuthAccountId =:= MasterAccountId ->
            lager:debug("master account is about to add credit, we need to check whether to invoke a bookkeeper or not"),
            maybe_free_credit(Context);
        MasterAccountId when AuthAccountId =:= CreditAccountId ->
            lager:debug("master's child wants to credit himself. invoking a bookkeeper to add credit"),
            normal_credit(Context);
        AuthAccountId when AuthAccountId =/= CreditAccountId ->
            lager:debug("allowing non master reseller to credit its child (but not himself) without invoking a bookkeeper"),
            free_credit(Context);
        ResellerId ->
            lager:debug("sub-resellers must contact resellers to add credit to themselves"),
            Resp = kz_json:from_list(
                     [{<<"message">>, <<"Please contact your phone provider to add credit.">>}
                     ,{<<"cause">>, ResellerId}
                     ]),
            cb_context:add_validation_error(<<"amount">>, <<"forbidden">>, Resp, Context)
    end.

-spec free_credit(cb_context:context()) -> cb_context:context().
free_credit(Context) ->
    maybe_create_credit_tansaction('save', Context).

-spec normal_credit(cb_context:context()) -> cb_context:context().
normal_credit(Context) ->
    maybe_create_credit_tansaction('service_save', Context).

-spec maybe_free_credit(cb_context:context()) -> cb_context:context().
maybe_free_credit(Context) ->
    case kz_json:get_value(<<"credit_type">>, cb_context:req_data(Context)) of
        <<"free">> -> free_credit(Context);
        _ -> normal_credit(Context)
    end.

-spec maybe_create_credit_tansaction('save'|'service_save', cb_context:context()) -> cb_context:context().
maybe_create_credit_tansaction(CreditType, Context) ->
    case create_credit_tansaction(CreditType, Context) of
        {'error', _R}=Error ->
            lager:error("failed to create credit transaction : ~p", [_R]),
            Msg = kz_json:from_list(
                    [{<<"message">>, <<"failed to create credit transaction">>}
                    ,{<<"cause">>, kz_term:error_to_binary(Error)}
                    ]),
            cb_context:add_system_error('transaction_failed', Msg, Context);
        {'ok', Transaction} ->
            cb_context:set_resp_data(Context, kz_transaction:to_public_json(Transaction))
    end.

-spec create_credit_tansaction('save'|'service_save', cb_context:context()) ->
                                      {'ok', kz_transaction:transaction()} |
                                      {'error', _}.
create_credit_tansaction(CreditType, Context) ->
    AccountId = cb_context:account_id(Context),
    JObj = cb_context:req_data(Context),
    Amount = kz_json:get_float_value(<<"amount">>, JObj),
    Units = wht_util:dollars_to_units(Amount),
    Meta = kz_json:from_list(
             [{<<"auth_account_id">>, cb_context:auth_account_id(Context)}
             ]),
    Reason = kz_json:get_value(<<"reason">>, JObj, wht_util:manual_addition()),
    Description = kz_json:get_ne_binary_value(<<"description">>, JObj, wht_util:admin_discretion()),

    Routines = [fun(Tr) -> kz_transaction:set_reason(Reason, Tr) end
               ,fun(Tr) -> kz_transaction:set_description(Description, Tr) end
               ,fun(Tr) -> kz_transaction:set_metadata(Meta, Tr) end
               ,fun kz_transaction:CreditType/1
               ],
    lists:foldl(fun(F, Tr) -> F(Tr) end
               ,kz_transaction:credit(AccountId, Units)
               ,Routines
               ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, ?DEBIT) ->
    case cb_context:resp_status(Context) of
        'success' -> maybe_debit_billing_id(Context);
        _Error -> Context
    end.

%% Note: really similar to cb_braintree:maybe_charge_billing_id/2
-spec maybe_debit_billing_id(cb_context:context()) -> cb_context:context().
maybe_debit_billing_id(Context) ->
    AuthAccountId = cb_context:auth_account_id(Context),
    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),

    case kz_services:find_reseller_id(cb_context:account_id(Context)) of
        MasterAccountId ->
            lager:debug("invoking a bookkeeper to remove requested credit"),
            maybe_create_debit_tansaction(Context);
        AuthAccountId when AuthAccountId =/= MasterAccountId ->
            lager:debug("allowing reseller to remove credit without invoking a bookkeeper"),
            maybe_create_debit_tansaction(Context);
        ResellerId ->
            lager:debug("sub-accounts of non-master resellers must contact the reseller to change their credit"),
            Resp = kz_json:from_list(
                     [{<<"message">>, <<"Please contact your phone provider to remove credit.">>}
                     ,{<<"cause">>, ResellerId}
                     ]),
            cb_context:add_validation_error(<<"amount">>, <<"forbidden">>, Resp, Context)
    end.

-spec maybe_create_debit_tansaction(cb_context:context()) -> cb_context:context().
maybe_create_debit_tansaction(Context) ->
    case create_debit_tansaction(Context) of
        {'error', _R}=Error ->
            lager:error("failed to create debit transaction : ~p", [_R]),
            cb_context:add_system_error('transaction_failed'
                                       ,kz_json:from_list([{<<"message">>, <<"failed to create debit transaction">>}
                                                          ,{<<"cause">>, kz_term:error_to_binary(Error)}
                                                          ])
                                       ,Context
                                       );
        {'ok', Transaction} ->
            cb_context:set_resp_data(Context
                                    ,kz_transaction:to_public_json(Transaction)
                                    )
    end.

-spec create_debit_tansaction(cb_context:context()) ->
                                     {'ok', kz_transaction:transaction()} |
                                     {'error', _}.
create_debit_tansaction(Context) ->
    AccountId = cb_context:account_id(Context),
    JObj = cb_context:req_data(Context),
    Amount = kz_json:get_float_value(<<"amount">>, JObj),
    Units = wht_util:dollars_to_units(Amount),
    Meta =
        kz_json:from_list([{<<"auth_account_id">>, cb_context:auth_account_id(Context)}]),
    Reason = kz_json:get_value(<<"reason">>, JObj, wht_util:admin_discretion()),
    Description = kz_json:get_ne_binary_value(<<"description">>, JObj, wht_util:admin_discretion()),

    Routines = [fun(Tr) -> kz_transaction:set_reason(Reason, Tr) end
               ,fun(Tr) -> kz_transaction:set_description(Description, Tr) end
               ,fun(Tr) -> kz_transaction:set_metadata(Meta, Tr) end
               ,fun kz_transaction:save/1
               ],
    lists:foldl(fun(F, Tr) -> F(Tr) end
               ,kz_transaction:debit(AccountId, Units)
               ,Routines
               ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec validate_transactions(cb_context:context(), http_method()) -> cb_context:context().
validate_transactions(Context, ?HTTP_GET) ->
    case crossbar_view:time_range(Context) of
        {CreatedFrom, CreatedTo} ->
            Reason = cb_context:req_value(Context, <<"reason">>),
            fetch_transactions(Context, CreatedFrom, CreatedTo, Reason);
        Context1 -> Context1
    end.

-spec validate_transaction(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_transaction(Context, ?CURRENT_BALANCE, ?HTTP_GET) ->
    CurrentBalance = case wht_util:current_balance(cb_context:account_id(Context)) of
                         {'ok', Bal} -> Bal;
                         {'error', _} -> 0 %% shouldn't we use crossbar_doc:handle_datamgr_errors/3 here?
                     end,
    Balance = wht_util:units_to_dollars(CurrentBalance),
    JObj = kz_json:from_list([{<<"balance">>, Balance}]),
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_status/2, 'success'}
                       ,{fun cb_context:set_resp_data/2, JObj}
                       ]);
validate_transaction(Context, ?MONTHLY, ?HTTP_GET) ->
    case crossbar_view:time_range(Context) of
        {CreatedFrom, CreatedTo} ->
            Reason = cb_context:req_value(Context, <<"reason">>),
            fetch_monthly_recurring(Context, CreatedFrom, CreatedTo, Reason);
        Context1 -> Context1
    end;
validate_transaction(Context, ?SUBSCRIPTIONS, ?HTTP_GET) ->
    filter_subscriptions(Context);
validate_transaction(Context, ?CREDIT, ?HTTP_PUT) ->
    validate_credit(Context);
validate_transaction(Context, ?DEBIT, ?HTTP_DELETE) ->
    validate_debit(Context);
validate_transaction(Context, _PathToken, _Verb) ->
    cb_context:add_system_error('bad_identifier',  Context).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec validate_credit(cb_context:context()) -> cb_context:context().
validate_credit(Context) ->
    Amount = kz_json:get_float_value(<<"amount">>, cb_context:req_data(Context)),
    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),

    case cb_context:is_superduper_admin(Context) of
        'true' -> validate_credit(Context, Amount);
        'false' ->
            case kz_services:is_reseller(cb_context:auth_account_id(Context))
                orelse MasterAccountId =:= kz_services:find_reseller_id(cb_context:account_id(Context))
            of
                'true' -> validate_credit(Context, Amount);
                'false' -> cb_context:add_system_error('forbidden', Context)
            end
    end.

-spec validate_credit(cb_context:context(), kz_term:api_float()) -> cb_context:context().
validate_credit(Context, 'undefined') ->
    Message = kz_json:from_list([{<<"message">>, <<"Amount is required">>}]),
    cb_context:add_validation_error(<<"amount">>, <<"required">>, Message, Context);
validate_credit(Context, Amount) when Amount =< 0 ->
    Message = kz_json:from_list([{<<"message">>, <<"Amount must be greater than 0">>}]),
    cb_context:add_validation_error(<<"amount">>, <<"minimum">>, Message, Context);
validate_credit(Context, _) ->
    cb_context:set_resp_status(Context, 'success').

-spec validate_debit(cb_context:context()) -> cb_context:context().
validate_debit(Context) ->
    Amount = kz_json:get_float_value(<<"amount">>, cb_context:req_data(Context)),

    case cb_context:is_superduper_admin(Context) of
        'true' -> validate_debit(Context, Amount);
        'false' ->
            case kz_services:is_reseller(cb_context:auth_account_id(Context)) of
                'true' -> validate_debit(Context, Amount);
                'false' -> cb_context:add_system_error('forbidden', Context)
            end
    end.

-spec validate_debit(cb_context:context(), kz_term:api_float()) -> cb_context:context().
validate_debit(Context, 'undefined') ->
    Message = kz_json:from_list([{<<"message">>, <<"Amount is required">>}]),
    cb_context:add_validation_error(<<"amount">>, <<"required">>, Message, Context);
validate_debit(Context, Amount) when Amount =< 0 ->
    Message = kz_json:from_list([{<<"message">>, <<"Amount must be more than 0">>}]),
    cb_context:add_validation_error(<<"amount">>, <<"minimum">>, Message, Context);
validate_debit(Context, Amount) ->
    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
    AuthAccountId = cb_context:auth_account_id(Context),
    AccountId = cb_context:account_id(Context),
    case AuthAccountId == MasterAccountId
        orelse AuthAccountId == kz_services:find_reseller_id(AccountId)
    of
        'true' ->
            cb_context:set_resp_status(Context, 'success');
        'false' ->
            FuturAmount = case wht_util:current_account_dollars(AccountId) of
                              {'ok', AccBal} -> AccBal - Amount;
                              {'error', _} -> 0 - Amount
                          end,
            case FuturAmount < 0 of
                'false' ->
                    cb_context:set_resp_status(Context, 'success');
                'true' ->
                    Message = <<"Available credit can not be less than 0">>,
                    JObj = kz_json:from_list([{<<"message">>, Message},{<<"cause">>, FuturAmount}]),
                    cb_context:add_validation_error(<<"amount">>, <<"minimum">>, JObj, Context)
            end
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch_transactions(cb_context:context(), kz_time:gregorian_seconds(), kz_time:gregorian_seconds(), kz_term:api_binary()) ->
                                cb_context:context().

fetch_transactions(Context, From, To, 'undefined') ->
    case kz_transactions:fetch(cb_context:account_id(Context), From, To) of
        {'error', _R}=Error -> send_resp(Error, Context);
        {'ok', Transactions} ->
            JObjs = kz_transactions:to_public_json(Transactions),
            send_resp({'ok', JObjs}, Context)
    end;
fetch_transactions(Context, From, To, <<"only_calls">>) ->
    case kz_transactions:fetch_local(cb_context:account_id(Context), From, To) of
        {'error', _R}=Error -> send_resp(Error, Context);
        {'ok', Transactions} ->
            JObjs = [kz_transaction:to_public_json(Transaction)
                     || Transaction <- kz_transactions:filter_for_per_minute(Transactions)
                    ],
            send_resp({'ok', JObjs}, Context)
    end;
fetch_transactions(Context, From, To, Reason)
  when Reason =:= <<"only_bookkeeper">>; Reason =:= <<"no_calls">> ->
    case kz_transactions:fetch_bookkeeper(cb_context:account_id(Context), From, To) of
        {'error', _R}=Error -> send_resp(Error, Context);
        {'ok', Transactions} ->
            Filtered = kz_transactions:filter_by_reason(Reason, Transactions),
            JObjs = kz_transactions:to_public_json(Filtered),
            send_resp({'ok', JObjs}, Context)
    end;
fetch_transactions(Context, From, To, Reason) ->
    case kz_transactions:fetch(cb_context:account_id(Context), From, To) of
        {'error', _R}=Error -> send_resp(Error, Context);
        {'ok', Transactions} ->
            Filtered = kz_transactions:filter_by_reason(Reason, Transactions),
            JObjs = kz_transactions:to_public_json(Filtered),
            send_resp({'ok', JObjs}, Context)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch_monthly_recurring(cb_context:context(), kz_time:gregorian_seconds(), kz_time:gregorian_seconds(), kz_term:api_binary()) ->
                                     cb_context:context().
fetch_monthly_recurring(Context, From, To, Reason) ->
    case kz_bookkeeper_braintree:transactions(cb_context:account_id(Context), From, To) of
        {'error', _}=E -> send_resp(E, Context);
        {'ok', Transactions} ->
            JObjs = [kz_transaction:to_public_json(Transaction)
                     || Transaction <- kz_transactions:filter_by_reason(Reason, Transactions),
                        kz_transaction:code(Transaction) =:= ?CODE_MONTHLY_RECURRING
                    ],
            send_resp({'ok', JObjs}, Context)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec filter_subscriptions(cb_context:context()) -> cb_context:context().
filter_subscriptions(Context) ->
    AccountId = cb_context:account_id(Context),
    case kz_service_transactions:current_billing_period(AccountId, 'subscriptions') of
        'not_found' ->
            send_resp({'error', <<"no data found in braintree">>}, Context);
        'unknown_error' ->
            send_resp({'error', <<"unknown braintree error">>}, Context);
        BSubscriptions ->
            JObjs = [filter_subscription(BSub) || BSub <- BSubscriptions],
            send_resp({'ok', JObjs}, Context)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec filter_subscription(kz_json:object()) -> kz_json:object().
filter_subscription(BSubscription) ->
    Routines = [fun clean_braintree_subscription/1
               ,fun correct_date_braintree_subscription/1
               ],
    lists:foldl(fun(F, BSub) -> F(BSub) end, BSubscription, Routines).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec clean_braintree_subscription(kz_json:object()) -> kz_json:object().
clean_braintree_subscription(BSubscription) ->
    RemoveKeys = [<<"billing_dom">>
                 ,<<"failure_count">>
                 ,<<"merchant_account_id">>
                 ,<<"never_expires">>
                 ,<<"paid_through_date">>
                 ,<<"payment_token">>
                 ,<<"trial_period">>
                 ,<<"do_not_inherit">>
                 ,<<"start_immediately">>
                 ,<<"prorate_charges">>
                 ,<<"revert_on_prorate_fail">>
                 ,<<"replace_add_ons">>
                 ,<<"create">>
                 ],
    kz_json:delete_keys(RemoveKeys, BSubscription).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec correct_date_braintree_subscription(kz_json:object()) -> kz_json:object().
correct_date_braintree_subscription(BSubscription) ->
    Keys = [<<"billing_first_date">>
           ,<<"billing_end_date">>
           ,<<"billing_start_date">>
           ,<<"next_bill_date">>
           ],
    lists:foldl(fun correct_date_braintree_subscription_fold/2, BSubscription, Keys).

-spec correct_date_braintree_subscription_fold(kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
correct_date_braintree_subscription_fold(Key, BSub) ->
    case kz_json:get_value(Key, BSub, 'null') of
        'null' -> BSub;
        Value ->
            [Y, M, D | _] = string:tokens(binary_to_list(Value), "-"),
            DateTime = {{list_to_integer(Y), list_to_integer(M), list_to_integer(D)}, {0, 0, 0}},
            Timestamp = calendar:datetime_to_gregorian_seconds(DateTime),
            kz_json:set_value(Key, Timestamp, BSub)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec send_resp({'ok', any()} | {'error', any()}, cb_context:context()) -> cb_context:context().
send_resp({'ok', JObj}, Context) ->
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_status/2, 'success'}
                       ,{fun cb_context:set_resp_data/2, JObj}
                       ]);
send_resp({'error', Details}, Context) ->
    cb_context:add_system_error('bad_identifier', kz_json:from_list([{<<"cause">>, Details}]), Context).
