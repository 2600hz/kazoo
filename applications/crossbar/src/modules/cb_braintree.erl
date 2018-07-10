%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc Handle client requests for braintree documents
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_braintree).

-export([init/0
        ,allowed_methods/1, allowed_methods/2
        ,resource_exists/1, resource_exists/2
        ,validate/2, validate/3
        ,put/2
        ,post/2, post/3
        ,delete/3
        ]).

-include("crossbar.hrl").
-include_lib("kazoo_transactions/include/kazoo_transactions.hrl").
-include_lib("braintree/include/braintree.hrl").

-define(CUSTOMER_PATH_TOKEN, <<"customer">>).
-define(CARDS_PATH_TOKEN, <<"cards">>).
-define(ADDRESSES_PATH_TOKEN, <<"addresses">>).
-define(TRANSACTIONS_PATH_TOKEN, <<"transactions">>).
-define(CREDITS_PATH_TOKEN, <<"credits">>).
-define(CLIENT_TOKEN_PATH_TOKEN, <<"client_token">>).

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".braintree">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> ok.
init() ->
    _ = ssl:start(),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.braintree">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.braintree">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.braintree">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.braintree">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.braintree">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.braintree">>, ?MODULE, 'delete'),
    ok.

%%------------------------------------------------------------------------------
%% @doc This function determines the verbs that are appropriate for the
%% given Nouns. For example `/accounts/' can only accept `GET' and `PUT'.
%%
%% Failure here returns `405 Method Not Allowed'.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?CUSTOMER_PATH_TOKEN) ->
    [?HTTP_GET, ?HTTP_POST];
allowed_methods(?CARDS_PATH_TOKEN) ->
    [?HTTP_GET, ?HTTP_PUT];
allowed_methods(?ADDRESSES_PATH_TOKEN) ->
    [?HTTP_GET, ?HTTP_PUT];
allowed_methods(?TRANSACTIONS_PATH_TOKEN) ->
    [?HTTP_GET];
allowed_methods(?CREDITS_PATH_TOKEN) ->
    [?HTTP_GET, ?HTTP_PUT];
allowed_methods(?CLIENT_TOKEN_PATH_TOKEN) ->
    [?HTTP_GET].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(?CARDS_PATH_TOKEN, _CardId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE];
allowed_methods(?ADDRESSES_PATH_TOKEN, _AddressId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE];
allowed_methods(?TRANSACTIONS_PATH_TOKEN, _TransactionId) ->
    [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns are valid.
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists(path_token()) -> 'true'.
resource_exists(?CUSTOMER_PATH_TOKEN) -> 'true';
resource_exists(?CARDS_PATH_TOKEN) -> 'true';
resource_exists(?ADDRESSES_PATH_TOKEN) -> 'true';
resource_exists(?TRANSACTIONS_PATH_TOKEN) -> 'true';
resource_exists(?CREDITS_PATH_TOKEN) -> 'true';
resource_exists(?CLIENT_TOKEN_PATH_TOKEN) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(?CARDS_PATH_TOKEN, _) -> 'true';
resource_exists(?ADDRESSES_PATH_TOKEN, _) -> 'true';
resource_exists(?TRANSACTIONS_PATH_TOKEN, _) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400.
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
%% CUSTOMER API
validate(Context, ?CUSTOMER_PATH_TOKEN) ->
    validate_customer(Context, cb_context:req_verb(Context));
validate(Context, ?CARDS_PATH_TOKEN) ->
    validate_cards(Context, cb_context:req_verb(Context));
validate(Context, ?ADDRESSES_PATH_TOKEN) ->
    validate_addresses(Context, cb_context:req_verb(Context));
validate(Context, ?TRANSACTIONS_PATH_TOKEN) ->
    validate_transactions(Context, cb_context:req_verb(Context));
validate(Context, ?CREDITS_PATH_TOKEN) ->
    validate_credits(Context, cb_context:req_verb(Context));
validate(Context, ?CLIENT_TOKEN_PATH_TOKEN) ->
    validate_token(Context, cb_context:req_verb(Context)).

-spec validate_customer(cb_context:context(), path_token()) -> cb_context:context().
validate_customer(Context, ?HTTP_GET) ->
    AccountId = cb_context:account_id(Context),
    try braintree_customer:find(AccountId) of
        #bt_customer{}=Customer ->
            Resp = braintree_customer:record_to_json(Customer),
            crossbar_util:response(Resp, Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{'not_found', _} ->
            Customer = braintree_customer:new(AccountId),
            Resp = braintree_customer:record_to_json(Customer),
            crossbar_util:response(Resp, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', kz_term:to_binary(Error), 500, Reason, Context)
    end;
validate_customer(Context, ?HTTP_POST) ->
    JObj = cb_context:req_data(Context),
    Generators = [fun(J) ->
                          case kz_json:get_value(<<"credit_card">>, J) of
                              'undefined' -> J;
                              _Else ->
                                  Id = kz_datamgr:get_uuid(),
                                  kz_json:set_value([<<"credit_card">>, <<"id">>], Id, J)
                          end
                  end
                 ,fun(J) ->
                          kz_json:set_value(<<"id">>, cb_context:account_id(Context), J)
                  end
                 ],
    Customer = braintree_customer:json_to_record(lists:foldr(fun(F, J) -> F(J) end, JObj, Generators)),
    crossbar_util:response(kz_json:new(), cb_context:store(Context, 'braintree', Customer)).

%% CARD API
-spec validate_cards(cb_context:context(), path_token()) -> cb_context:context().
validate_cards(Context, ?HTTP_GET) ->
    try braintree_customer:find(cb_context:account_id(Context)) of
        #bt_customer{credit_cards=Cards} ->
            Resp = [braintree_card:record_to_json(Card) || Card <- Cards],
            crossbar_util:response(Resp, Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', kz_term:to_binary(Error), 500, Reason, Context)
    end;
validate_cards(Context, ?HTTP_PUT) ->
    Card0 = braintree_card:json_to_record(cb_context:req_data(Context)),
    Card = Card0#bt_card{customer_id = cb_context:account_id(Context)},
    crossbar_util:response(kz_json:new(), cb_context:store(Context, 'braintree', Card)).

-spec validate_addresses(cb_context:context(), path_token()) -> cb_context:context().
validate_addresses(Context, ?HTTP_GET) ->
    try braintree_customer:find(cb_context:account_id(Context)) of
        #bt_customer{addresses=Addresses} ->
            Resp = [braintree_address:record_to_json(Address) || Address <- Addresses],
            crossbar_util:response(Resp, Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', kz_term:to_binary(Error), 500, Reason, Context)
    end;
validate_addresses(Context, ?HTTP_PUT) ->
    Address0 = braintree_address:json_to_record(cb_context:req_data(Context)),
    Address = Address0#bt_address{customer_id = cb_context:account_id(Context)},
    crossbar_util:response(kz_json:new(), cb_context:store(Context, 'braintree', Address)).

-spec validate_transactions(cb_context:context(), path_token()) -> cb_context:context().
validate_transactions(Context, ?HTTP_GET) ->
    try braintree_transaction:find_by_customer(cb_context:account_id(Context)) of
        Transactions ->
            Resp = [braintree_transaction:record_to_json(Transaction) || Transaction <- Transactions],
            crossbar_util:response(Resp, Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', kz_term:to_binary(Error), 500, Reason, Context)
    end.

-spec validate_credits(cb_context:context(), path_token()) -> cb_context:context().
validate_credits(Context, ?HTTP_GET) ->
    AccountId = cb_context:account_id(Context),
    Doc = cb_context:doc(Context),
    BillingAccountId = kz_json:get_integer_value(<<"billing_account_id">>, Doc, AccountId),
    Resp =
        kz_json:from_list([{<<"amount">>, current_account_dollars(AccountId)}
                          ,{<<"billing_account_id">>, BillingAccountId}
                          ]),
    crossbar_util:response(Resp, Context);
validate_credits(Context, ?HTTP_PUT) ->
    Amount = kz_json:get_float_value(<<"amount">>, cb_context:req_data(Context)),
    MaxCredit = kapps_config:get_float(?MOD_CONFIG_CAT, <<"max_account_credit">>, 500.00),
    FutureAmount = Amount + current_account_dollars(cb_context:account_id(Context)),
    case FutureAmount > MaxCredit of
        'true' ->
            error_max_credit(Context, MaxCredit, FutureAmount);
        'false' ->
            Context1 = cb_context:store(Context, 'bt_order_id', kz_binary:rand_hex(16)),
            maybe_charge_billing_id(Amount, Context1)
    end.

-spec error_max_credit(cb_context:context(), number(), number()) -> cb_context:context().
error_max_credit(Context, MaxCredit, FutureAmount) ->
    Message = <<"Available credit can not exceed $", (kz_term:to_binary(MaxCredit))/binary>>,
    cb_context:add_validation_error(<<"amount">>
                                   ,<<"maximum">>
                                   ,kz_json:from_list(
                                      [{<<"message">>, Message}
                                      ,{<<"cause">>, FutureAmount}
                                      ])
                                   ,Context
                                   ).

-spec current_account_dollars(kz_term:ne_binary()) -> dollars().
current_account_dollars(AccountId) ->
    case wht_util:current_account_dollars(AccountId) of
        {'ok', Dollars} -> Dollars;
        {'error', _R} ->
            lager:debug("failed to get current account ~s dollars, assuming 0: ~p", [AccountId, _R]),
            0
    end.

-spec validate_token(cb_context:context(), path_token()) -> cb_context:context().
validate_token(Context, ?HTTP_GET) ->
    try braintree_client_token:get_client_token(cb_context:account_id(Context)) of
        Token ->
            JObj = kz_json:from_list([{<<"client_token">>, Token}]),
            crossbar_util:response(JObj, Context)
    catch
        'throw':{Error, Reason} ->
            crossbar_util:response('error', kz_term:to_binary(Error), 500, Reason, Context)
    end.

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, ?CARDS_PATH_TOKEN, CardId) ->
    validate_card(Context, CardId, cb_context:req_verb(Context));
validate(Context, ?ADDRESSES_PATH_TOKEN, AddressId) ->
    validate_address(Context, AddressId, cb_context:req_verb(Context));
validate(Context, ?TRANSACTIONS_PATH_TOKEN, TransactionId) ->
    validate_transaction(Context, TransactionId, cb_context:req_verb(Context)).

-spec validate_card(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate_card(Context, CardId, ?HTTP_GET) ->
    AccountId = cb_context:account_id(Context),
    try braintree_card:find(CardId) of
        #bt_card{customer_id=AccountId}=Card ->
            Resp = braintree_card:record_to_json(Card),
            crossbar_util:response(Resp, Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', kz_term:to_binary(Error), 500, Reason, Context)
    end;
validate_card(Context, CardId, ?HTTP_POST) ->
    Card0 = braintree_card:json_to_record(cb_context:req_data(Context)),
    Card = Card0#bt_card{customer_id = cb_context:account_id(Context)
                        ,token = CardId
                        },
    crossbar_util:response(kz_json:new(), cb_context:store(Context, 'braintree', Card));
validate_card(Context, _CardId, ?HTTP_DELETE) ->
    crossbar_util:response(kz_json:new(), Context).

-spec validate_address(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate_address(Context, AddressId, ?HTTP_GET) ->
    AccountId = cb_context:account_id(Context),
    try braintree_address:find(AccountId, AddressId) of
        #bt_address{customer_id=AccountId}=Address ->
            Resp = braintree_address:record_to_json(Address),
            crossbar_util:response(Resp, Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', kz_term:to_binary(Error), 500, Reason, Context)
    end;
validate_address(Context, AddressId, ?HTTP_POST) ->
    Address0 = braintree_address:json_to_record(cb_context:req_data(Context)),
    Address = Address0#bt_address{customer_id = cb_context:account_id(Context)
                                 ,id = AddressId
                                 },
    crossbar_util:response(kz_json:new(), cb_context:store(Context, 'braintree', Address));
validate_address(Context, _AddressId, ?HTTP_DELETE) ->
    crossbar_util:response(kz_json:new(), Context).

-spec validate_transaction(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate_transaction(Context, TransactionId, ?HTTP_GET) ->
    try braintree_transaction:find(TransactionId) of
        #bt_transaction{}=Transaction ->
            Resp = braintree_transaction:record_to_json(Transaction),
            crossbar_util:response(Resp, Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', kz_term:to_binary(Error), 500, Reason, Context)
    end.

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, ?CUSTOMER_PATH_TOKEN) ->
    try braintree_customer:update(cb_context:fetch(Context, 'braintree')) of
        #bt_customer{}=Customer ->
            Resp = braintree_customer:record_to_json(Customer),
            _ = sync(Context),
            crossbar_util:response(Resp, Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{'not_found', _} ->
            create_braintree_customer(Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', kz_term:to_binary(Error), 500, Reason, Context)
    end.

-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, ?CARDS_PATH_TOKEN, CardId) ->
    try braintree_card:update(cb_context:fetch(Context, 'braintree')) of
        #bt_card{}=Card ->
            Resp = braintree_card:record_to_json(Card),
            _ = sync(Context),
            crossbar_util:response(Resp, Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{'not_found', _} ->
            cb_context:add_system_error('bad_identifier', kz_json:from_list([{<<"cause">>, CardId}]), Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', kz_term:to_binary(Error), 500, Reason, Context)
    end;
post(Context, ?ADDRESSES_PATH_TOKEN, AddressId) ->
    try braintree_address:update(cb_context:fetch(Context, 'braintree')) of
        #bt_address{}=Address ->
            Resp = braintree_address:record_to_json(Address),
            crossbar_util:response(Resp, Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{'not_found', _} ->
            crossbar_util:response_bad_identifier(AddressId, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', kz_term:to_binary(Error), 500, Reason, Context)
    end.

-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, ?CREDITS_PATH_TOKEN) ->
    create_credits(Context);
put(Context, ?ADDRESSES_PATH_TOKEN) ->
    try braintree_address:create(cb_context:fetch(Context, 'braintree')) of
        #bt_address{}=Address ->
            Resp = braintree_address:record_to_json(Address),
            crossbar_util:response(Resp, Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', kz_term:to_binary(Error), 500, Reason, Context)
    end;
put(Context, ?CARDS_PATH_TOKEN) ->
    try braintree_card:create(cb_context:fetch(Context, 'braintree')) of
        #bt_card{}=Card ->
            Resp = braintree_card:record_to_json(Card),
            _ = sync(Context),
            crossbar_util:response(Resp, Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', kz_term:to_binary(Error), 500, Reason, Context)
    end.

-spec create_credits(cb_context:context()) -> cb_context:context().
create_credits(Context) ->
    AccountId = cb_context:account_id(Context),
    AuthAccountId = cb_context:auth_account_id(Context),

    Units = wht_util:dollars_to_units(
              kz_json:get_float_value(<<"amount">>, cb_context:req_data(Context), 0.0)
             ),
    BTData = kz_json:delete_keys([<<"billing_address">>
                                 ,<<"shipping_address">>
                                 ,[<<"card">>, <<"billing_address">>]
                                 ,[?CUSTOMER_PATH_TOKEN, <<"credit_cards">>]
                                 ,[?CUSTOMER_PATH_TOKEN, ?ADDRESSES_PATH_TOKEN]
                                 ]
                                ,cb_context:resp_data(Context)
                                ),
    OrderId = cb_context:fetch(Context, 'bt_order_id'),

    case add_credit_to_account(BTData, Units, AccountId, AuthAccountId, OrderId) of
        {'ok', Transaction} ->
            kapi_money:publish_credit([{<<"Amount">>, Units}
                                      ,{<<"Account-ID">>, kz_transaction:account_id(Transaction)}
                                      ,{<<"Transaction-ID">>, kz_transaction:id(Transaction)}
                                       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                                      ]),
            JObj = kz_transaction:to_json(Transaction),
            _ = reset_low_balance_notification(AccountId),
            cb_context:setters(Context
                              ,[{fun cb_context:set_resp_status/2, 'success'}
                               ,{fun cb_context:set_doc/2, JObj}
                               ,{fun cb_context:set_resp_data/2, kz_doc:public_fields(JObj)}
                               ]);
        {'error', Reason} ->
            crossbar_util:response('error', <<"transaction error">>, 500, Reason, Context)
    end.

-spec reset_low_balance_notification(kz_term:ne_binary()) -> 'ok'.
reset_low_balance_notification(AccountId) ->
    case kzd_accounts:fetch(AccountId) of
        {'error', _} -> 'ok';
        {'ok', AccountJObj0} ->
            AccountJObj1 = kzd_accounts:reset_low_balance_sent(AccountJObj0),
            AccountJObj2 = kzd_accounts:remove_low_balance_tstamp(AccountJObj1),
            kzd_accounts:save(AccountJObj2)
    end.

-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete(Context, ?CARDS_PATH_TOKEN, CardId) ->
    try braintree_card:delete(CardId) of
        #bt_card{}=Card ->
            Resp = braintree_card:record_to_json(Card),
            crossbar_util:response(Resp, Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', kz_term:to_binary(Error), 500, Reason, Context)
    end;
delete(Context, ?ADDRESSES_PATH_TOKEN, AddressId) ->
    try braintree_address:delete(cb_context:account_id(Context), AddressId) of
        #bt_address{}=Address ->
            Resp = braintree_address:record_to_json(Address),
            crossbar_util:response(Resp, Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', kz_term:to_binary(Error), 500, Reason, Context)
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Creates an empty customer in braintree
%% @end
%%------------------------------------------------------------------------------
-spec create_braintree_customer(cb_context:context()) -> cb_context:context().
create_braintree_customer(Context) ->
    try
        C = case cb_context:fetch(Context, 'braintree') of
                #bt_customer{}=Customer -> Customer;
                _Else -> cb_context:account_id(Context)
            end,
        Resp = braintree_customer:record_to_json(braintree_customer:create(C)),
        _ = sync(Context),
        crossbar_util:response(Resp, Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', kz_term:to_binary(Error), 500, Reason, Context)
    end.

-spec maybe_charge_billing_id(float(), cb_context:context()) -> cb_context:context().
maybe_charge_billing_id(Amount, Context) ->
    AuthAccountId = cb_context:auth_account_id(Context),
    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),

    case kz_services:find_reseller_id(cb_context:account_id(Context)) of
        MasterAccountId ->
            lager:debug("invoking a bookkeeper to acquire requested credit"),
            charge_billing_id(Amount, Context);
        AuthAccountId when AuthAccountId =/= MasterAccountId ->
            lager:debug("allowing reseller to apply credit without invoking a bookkeeper"),
            Resp = kz_json:from_list([{<<"amount">>, Amount}]),
            crossbar_util:response(Resp, Context);
        ResellerId ->
            lager:debug("sub-accounts of non-master resellers must contact the reseller to change their credit"),
            Resp = kz_json:from_list(
                     [{<<"message">>, <<"Please contact your phone provider to add credit.">>}
                     ,{<<"cause">>, ResellerId}
                     ]),
            cb_context:add_system_error(<<"forbidden">>, Resp, Context)
    end.

-spec charge_billing_id(float(), cb_context:context()) -> cb_context:context().
charge_billing_id(Amount, Context) ->
    AccountId = cb_context:account_id(Context),
    BillingId = kz_json:get_value(<<"billing_account_id">>, cb_context:req_data(Context), AccountId),

    Props = case BillingId of
                AccountId ->
                    [{<<"purchase_order">>, ?CODE_MANUAL_ADDITION}
                    ,{<<"order_id">>, cb_context:fetch(Context, 'bt_order_id')}
                    ];
                _Id ->
                    [{<<"purchase_order">>, ?CODE_MANUAL_ADDITION_SUB_ACCOUNT}
                    ,{<<"order_id">>, cb_context:fetch(Context, 'bt_order_id')}
                    ]
            end,

    try braintree_transaction:quick_sale(BillingId, kz_term:to_binary(Amount), Props) of
        #bt_transaction{}=Transaction ->
            send_transaction_notify(AccountId, Transaction),
            crossbar_util:response(braintree_transaction:record_to_json(Transaction), Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', kz_term:to_binary(Error), 500, Reason, Context)
    end.

-spec send_transaction_notify(kz_term:ne_binary(), #bt_transaction{}) -> 'ok'.
send_transaction_notify(AccountId, Transaction) ->
    Props = [{<<"Account-ID">>, AccountId}
             | braintree_transaction:record_to_notification_props(Transaction)
             ++ kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    kapps_notify_publisher:cast(Props, fun kapi_notifications:publish_transaction/1).

-spec add_credit_to_account(kz_json:object(), integer(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_binary()) ->
                                   {'ok', kz_transaction:transaction()} |
                                   {'error', any()}.
add_credit_to_account(BraintreeData, Units, LedgerId, AccountId, OrderId) ->
    lager:debug("putting ~p units", [Units]),
    Routines = [fun(T) ->
                        case LedgerId =/= AccountId of
                            'false' ->
                                kz_transaction:set_reason(wht_util:manual_addition(), T);
                            'true'  ->
                                T1 = kz_transaction:set_sub_account_info(AccountId, T),
                                kz_transaction:set_reason(wht_util:sub_account_manual_addition(), T1)
                        end
                end
               ,fun(T) -> kz_transaction:set_bookkeeper_info(BraintreeData, T) end
               ,fun(T) ->
                        kz_transaction:set_description(<<"credit addition from credit card">>, T)
                end
               ,fun (T) -> kz_transaction:set_order_id(OrderId, T) end
               ],
    Transaction = lists:foldl(fun(F, T) -> F(T) end, kz_transaction:credit(LedgerId, Units), Routines),
    kz_transaction:save(Transaction).

-spec sync(cb_context:context()) -> 'ok'.
sync(Context) ->
    AccountId = cb_context:account_id(Context),
    _P = kz_util:spawn(fun kz_services:sync/1, [AccountId]),
    lager:debug("syncing ~s in ~p", [AccountId, _P]).
