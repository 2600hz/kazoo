%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% Handle client requests for braintree documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_braintree).

-export([init/0
         ,allowed_methods/1, allowed_methods/2
         ,resource_exists/1, resource_exists/2
         ,validate/2, validate/3
         ,put/2
         ,post/2, post/3
         ,delete/3
         ,billing/1
        ]).

-include_lib("crossbar/include/crossbar.hrl").
-include_lib("braintree/include/braintree.hrl").

-define(CUSTOMER_PATH_TOKEN, <<"customer">>).
-define(CARDS_PATH_TOKEN, <<"cards">>).
-define(ADDRESSES_PATH_TOKEN, <<"addresses">>).
-define(TRANSACTIONS_PATH_TOKEN, <<"transactions">>).
-define(CREDITS_PATH_TOKEN, <<"credits">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    ssl:start(),
    _ = crossbar_bindings:bind(<<"v1_resource.billing">>, ?MODULE, billing),
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.braintree">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.braintree">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.braintree">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.braintree">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.braintree">>, ?MODULE, post),
    crossbar_bindings:bind(<<"v1_resource.execute.delete.braintree">>, ?MODULE, delete).

-spec billing/1 :: (#cb_context{}) -> #cb_context{}.
billing(#cb_context{req_verb = <<"head">>}=Context) ->
    Context;
billing(#cb_context{req_nouns=Nouns}=Context) ->
    _ = crossbar_util:put_reqid(Context),

    case props:get_value(<<"connectivity">>, Nouns) of
        undefined ->
            Context;
        Params ->
            authorize_trunkstore(Params, Context)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/1 :: (path_token()) -> http_methods().
-spec allowed_methods/2 :: (path_token(), path_token()) -> http_methods().
allowed_methods(?CUSTOMER_PATH_TOKEN) ->
    ['GET', 'POST'];
allowed_methods(?CARDS_PATH_TOKEN) ->
    ['GET', 'PUT'];
allowed_methods(?ADDRESSES_PATH_TOKEN) ->
    ['GET', 'PUT'];
allowed_methods(?TRANSACTIONS_PATH_TOKEN) ->
    ['GET', 'PUT'];
allowed_methods(?CREDITS_PATH_TOKEN) ->
    ['GET', 'PUT'].

allowed_methods(?CARDS_PATH_TOKEN, _) ->
    ['GET', 'POST', 'DELETE'];
allowed_methods(?ADDRESSES_PATH_TOKEN, _) ->
    ['GET', 'POST', 'DELETE'];
allowed_methods(?TRANSACTIONS_PATH_TOKEN, _) ->
    ['GET'].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/1 :: (path_token()) -> 'true'.
-spec resource_exists/2 :: (path_token(), path_token()) -> 'true'.
resource_exists(?CUSTOMER_PATH_TOKEN) ->
    ['GET', 'POST'];
resource_exists(?CARDS_PATH_TOKEN) ->
    ['GET', 'PUT'];
resource_exists(?ADDRESSES_PATH_TOKEN) ->
    ['GET', 'PUT'];
resource_exists(?TRANSACTIONS_PATH_TOKEN) ->
    ['GET', 'PUT'];
resource_exists(?CREDITS_PATH_TOKEN) ->
    ['GET', 'PUT'].

resource_exists(?CARDS_PATH_TOKEN, _) ->
    ['GET', 'POST', 'DELETE'];
resource_exists(?ADDRESSES_PATH_TOKEN, _) ->
    ['GET', 'POST', 'DELETE'];
resource_exists(?TRANSACTIONS_PATH_TOKEN, _) ->
    ['GET'].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
%% CUSTOMER API
validate(#cb_context{req_verb = <<"get">>, account_id=AccountId}=Context, ?CUSTOMER_PATH_TOKEN) ->
    case braintree_customer:find(AccountId) of
        {ok, #bt_customer{}=C} ->
            Resp = braintree_customer:record_to_json(C),
            disable_cardless_accounts(wh_json:get_value(<<"credit_cards">>, Resp, []), Context),
            crossbar_util:response(Resp, Context);
        {error, #bt_api_error{}=ApiError} ->
            Resp = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Resp, Context);
        {error, not_found} ->
            disable_cardless_accounts([], Context),
            create_placeholder_account(Context);
        {error, _} ->
            crossbar_util:response_db_fatal(Context)
    end;
validate(#cb_context{req_verb = <<"post">>, req_data=JObj, account_id=AccountId}=Context, ?CUSTOMER_PATH_TOKEN) ->
    Generators = [fun(J) ->
                          case wh_json:get_value(<<"credit_card">>, J) of
                              undefined -> J;
                              _Else ->
                                  Id = couch_mgr:get_uuid(),
                                  wh_json:set_value([<<"credit_card">>, <<"id">>], Id, J) 
                          end
                  end
                  ,fun(J) ->
                           wh_json:set_value(<<"id">>, AccountId, J) 
                   end
                 ],
    Customer = braintree_customer:json_to_record(lists:foldr(fun(F, J) -> F(J) end, JObj, Generators)),
    crossbar_util:response([], crossbar_util:store(braintree, Customer, Context));

%% CARD API
validate(#cb_context{req_verb = <<"get">>, account_id=AccountId}=Context, ?CARDS_PATH_TOKEN) ->
    case braintree_customer:find(AccountId) of
        {ok, #bt_customer{credit_cards=Cards}} ->
            Resp = [braintree_card:record_to_json(Card) || Card <- Cards],
            disable_cardless_accounts(Resp, Context),
            crossbar_util:response(Resp, Context);
        {error, #bt_api_error{}=ApiError} ->
            Resp = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Resp, Context);
        {error, _} ->
            crossbar_util:response_db_fatal(Context)
    end;
validate(#cb_context{req_verb = <<"put">>, req_data=JObj, account_id=AccountId}=Context, ?CARDS_PATH_TOKEN) ->
    Card = (braintree_card:json_to_record(JObj))#bt_card{customer_id=wh_util:to_list(AccountId)},
    crossbar_util:response([], crossbar_util:store(braintree, Card, Context));

validate(#cb_context{req_verb = <<"get">>, account_id=AccountId}=Context, ?ADDRESSES_PATH_TOKEN) ->
    case braintree_customer:find(AccountId) of
        {ok, #bt_customer{addresses=Addresses}} ->
            Resp = [braintree_address:record_to_json(Address) || Address <- Addresses],
            crossbar_util:response(Resp, Context);
        {error, #bt_api_error{}=ApiError} ->
            Resp = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Resp, Context);
        {error, _} ->
            crossbar_util:response_db_fatal(Context)
    end;
validate(#cb_context{req_verb = <<"put">>, req_data=JObj, account_id=AccountId}=Context, ?ADDRESSES_PATH_TOKEN) ->
    Address = (braintree_address:json_to_record(JObj))#bt_address{customer_id=wh_util:to_list(AccountId)},
    crossbar_util:response([], crossbar_util:store(braintree, Address, Context));

validate(#cb_context{req_verb = <<"get">>, account_id=AccountId}=Context, ?TRANSACTIONS_PATH_TOKEN) ->
    case braintree_transaction:find_by_customer(AccountId) of
        {ok, Transactions} ->
            Resp = [braintree_transaction:record_to_json(Transaction) || Transaction <- Transactions],
            crossbar_util:response(Resp, Context);
        {error, #bt_api_error{}=ApiError} ->
            Resp = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Resp, Context);
        {error, _} ->
            crossbar_util:response_db_fatal(Context)
    end;

validate(#cb_context{req_verb = <<"get">>, account_id=AccountId, doc=JObj}=Context, ?CREDITS_PATH_TOKEN) ->
    %% TODO: request current balance from jonny5 and put it here
    DB = wh_util:format_account_id(AccountId, encoded),
    Units = case couch_mgr:get_results(DB, <<"transactions/credit_remaining">>, [{<<"reduce">>, true}]) of
                {ok, []} -> lager:debug("No results"), 0;
                {ok, [ViewRes|_]} -> lager:debug("Found obj ~p", [ViewRes]), wh_json:get_value(<<"value">>, ViewRes, 0);
                {error, _E} -> lager:debug("Error loading view: ~p", [_E]), 0
            end,
    crossbar_util:response(wh_json:from_list([{<<"amount">>, wapi_money:units_to_dollars(Units)}
                                              ,{<<"billing_account_id">>, wh_json:get_value(<<"billing_account_id">>, JObj, AccountId)}
                                             ]), Context);
validate(#cb_context{req_verb = <<"put">>, account_id=AccountId, req_data=JObj}=Context, ?CREDITS_PATH_TOKEN) ->
    BillingId = wh_json:get_value(<<"billing_account_id">>, JObj, AccountId),
    Amount = wh_json:get_value(<<"amount">>, JObj, <<"0.0">>),
    case braintree_transaction:quick_sale(BillingId, Amount) of
        {ok, #bt_transaction{}=Transaction} ->
            crossbar_util:response(braintree_transaction:record_to_json(Transaction), Context);
        {error, #bt_api_error{}=ApiError} ->
            Resp = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Resp, Context);
        {error, Error} ->
            crossbar_util:response(error, <<"braintree api error">>, 400
                                   ,wh_json:from_list([{<<"cause">>, wh_util:to_binary(Error)}])
                                   ,Context)
    end.

validate(#cb_context{req_verb = <<"get">>, account_id=Account}=Context, ?CARDS_PATH_TOKEN, CardId) ->
    AccountId = wh_util:to_list(Account),
    case braintree_card:find(CardId) of
        {ok, #bt_card{customer_id=AccountId}=C} ->
            Resp = braintree_card:record_to_json(C),
            crossbar_util:response(Resp, Context);
        {error, #bt_api_error{}=ApiError} ->
            Resp = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Resp, Context);
        {error, not_found} ->
            crossbar_util:response_bad_identifier(CardId, Context);
        {error, _} ->
            crossbar_util:response_db_fatal(Context)
    end;
validate(#cb_context{req_verb = <<"post">>, req_data=JObj, account_id=AccountId}=Context, ?CARDS_PATH_TOKEN, CardId) ->
    Card = (braintree_card:json_to_record(JObj))#bt_card{customer_id=wh_util:to_list(AccountId), token=CardId},
    crossbar_util:response([], crossbar_util:store(braintree, Card, Context));
validate(#cb_context{req_verb = <<"delete">>}=Context, ?CARDS_PATH_TOKEN, _) ->
    crossbar_util:response([], Context);
validate(#cb_context{req_verb = <<"get">>, account_id=Account}=Context, ?ADDRESSES_PATH_TOKEN, AddressId) ->
    AccountId = wh_util:to_list(Account),
    case braintree_address:find(AddressId) of
        {ok, #bt_address{customer_id=AccountId}=C} ->
            Resp = braintree_address:record_to_json(C),
            crossbar_util:response(Resp, Context);
        {error, #bt_api_error{}=ApiError} ->
            Resp = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Resp, Context);
        {error, not_found} ->
            crossbar_util:response_bad_identifier(AddressId, Context);
        {error, _} ->
            crossbar_util:response_db_fatal(Context)
    end;
validate(#cb_context{req_verb = <<"post">>, req_data=JObj, account_id=AccountId}=Context, ?ADDRESSES_PATH_TOKEN, AddressId) ->
    Address = (braintree_address:json_to_record(JObj))#bt_address{customer_id=wh_util:to_list(AccountId), id=AddressId},
    crossbar_util:response([], crossbar_util:store(braintree, Address, Context));
validate(#cb_context{req_verb = <<"delete">>}=Context, ?ADDRESSES_PATH_TOKEN, _) ->
    crossbar_util:response([], Context);

validate(#cb_context{req_verb = <<"get">>}=Context, ?TRANSACTIONS_PATH_TOKEN, TransactionId) ->
    case braintree_transaction:find(TransactionId) of
        {ok, #bt_transaction{}=T} ->
            Resp = braintree_address:record_to_json(T),
            crossbar_util:response(Resp, Context);
        {ok, Transactions} ->
            Resp = [braintree_transaction:record_to_json(Transaction) || Transaction <- Transactions],
            crossbar_util:response(Resp, Context);
        {error, #bt_api_error{}=ApiError} ->
            Resp = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Resp, Context);
        {error, _} ->
            crossbar_util:response_db_fatal(Context)
    end.

-spec post/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
-spec post/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
post(#cb_context{account_id=AccountId}=Context, ?CUSTOMER_PATH_TOKEN) ->
    _ = crossbar_util:put_reqid(Context),
    Customer = crossbar_util:fetch(braintree, Context),
    create_placeholder_account(Context),
    case braintree_customer:update(Customer) of
        {ok, #bt_customer{}=C} ->
            crossbar_util:enable_account(AccountId),
            Resp = braintree_customer:record_to_json(C),
            disable_cardless_accounts(wh_json:get_value(<<"credit_cards">>, Resp, []), Context),
            crossbar_util:response(Resp, Context);
        {error, #bt_api_error{}=ApiError} ->
            Resp = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Resp, Context);
        {error, _} ->
            crossbar_util:response_db_fatal(Context)
    end.

post(Context, ?CARDS_PATH_TOKEN, CardId) ->
    _ = crossbar_util:put_reqid(Context),

    Card = crossbar_util:fetch(braintree, Context),
    case braintree_card:update(Card) of
        {ok, #bt_card{}=C} ->
            Resp = braintree_card:record_to_json(C),
            disable_cardless_accounts(Resp, Context),
            crossbar_util:response(Resp, Context);
        {error, #bt_api_error{}=ApiError} ->
            Resp = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Resp, Context);
        {error, not_found} ->
            crossbar_util:response_bad_identifier(CardId, Context);
        {error, _} ->
            crossbar_util:response_db_fatal(Context)
    end;
post(Context, ?ADDRESSES_PATH_TOKEN, AddressId) ->
    _ = crossbar_util:put_reqid(Context),

    Address = crossbar_util:fetch(braintree, Context),
    case braintree_address:update(Address) of
        {ok, #bt_address{}=A} ->
            Resp = braintree_address:record_to_json(A),
            crossbar_util:response(Resp, Context);
        {error, #bt_api_error{}=ApiError} ->
            Resp = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Resp, Context);
        {error, not_found} ->
            crossbar_util:response_bad_identifier(AddressId, Context);
        {error, _} ->
            crossbar_util:response_db_fatal(Context)
    end.

-spec put/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
put(#cb_context{req_data=ReqData, resp_data=RespData}=Context, ?CREDITS_PATH_TOKEN) ->
    _ = crossbar_util:put_reqid(Context),

    Units = wapi_money:dollars_to_units(wh_json:get_float_value(<<"amount">>, ReqData)),
    lager:debug("putting ~p units", [Units]),

    BTCleanup = [fun(J) -> wh_json:delete_key([<<"card">>, <<"billing_address">>], J) end
                 ,fun(J) -> wh_json:delete_key(<<"billing_address">>, J) end
                 ,fun(J) -> wh_json:delete_key(<<"shipping_address">>, J) end
                 ,fun(J) -> wh_json:delete_key([?CUSTOMER_PATH_TOKEN, <<"credit_cards">>], J) end
                 ,fun(J) -> wh_json:delete_key([?CUSTOMER_PATH_TOKEN, ?ADDRESSES_PATH_TOKEN], J) end
                ],

    Updaters = [fun(J) -> wh_json:set_value(<<"amount">>, Units, J) end
                ,fun(J) -> wh_json:set_value(<<"pvt_type">>, <<"credit">>, J) end
                ,fun(J) -> wh_json:set_value(<<"braintree">>, lists:foldr(fun(F, J2) -> F(J2) end, RespData, BTCleanup), J) end
               ],

    #cb_context{resp_status=success, doc=Saved}
        = crossbar_doc:ensure_saved(Context#cb_context{doc=lists:foldr(fun(F, J) -> F(J) end, wh_json:new(), Updaters)}),

    wapi_money:publish_credit([{<<"Amount">>, Units}
                               ,{<<"Account-ID">>, wh_json:get_value(<<"pvt_account_id">>, Saved)}
                               ,{<<"Transaction-ID">>, wh_json:get_value(<<"_id">>, Saved)}
                               | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                              ]),
    Context;
put(Context, ?ADDRESSES_PATH_TOKEN) ->
    _ = crossbar_util:put_reqid(Context),

    Address = crossbar_util:fetch(braintree, Context),
    case braintree_address:create(Address) of
        {ok, #bt_address{}=A} ->
            Resp = braintree_address:record_to_json(A),
            crossbar_util:response(Resp, Context);
        {error, #bt_api_error{}=ApiError} ->
            Resp = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Resp, Context);
        {error, _} ->
            crossbar_util:response_db_fatal(Context)
    end;
put(Context, ?CARDS_PATH_TOKEN) ->
    _ = crossbar_util:put_reqid(Context),

    Card = crossbar_util:fetch(braintree, Context),
    case braintree_card:create(Card) of
        {ok, #bt_card{}=C} ->
            Resp = braintree_card:record_to_json(C),
            disable_cardless_accounts(Resp, Context),
            crossbar_util:response(Resp, Context);
        {error, #bt_api_error{}=ApiError} ->
            Resp = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Resp, Context);
        {error, _} ->
            crossbar_util:response_db_fatal(Context)
    end.

-spec delete/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
delete(Context, ?CARDS_PATH_TOKEN, CardId) ->
    _ = crossbar_util:put_reqid(Context),

    case braintree_card:delete(CardId) of
        {ok, #bt_card{}=C} ->
            Resp = braintree_card:record_to_json(C),
            disable_cardless_accounts(Resp, Context),
            crossbar_util:response(Resp, Context);
        {error, #bt_api_error{}=ApiError} ->
            Resp = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Resp, Context);
        {error, not_found} ->
            crossbar_util:response_bad_identifier(CardId, Context);
        {error, _} ->
            crossbar_util:response_db_fatal(Context)
    end;
delete(Context, ?ADDRESSES_PATH_TOKEN, AddressId) ->
    _ = crossbar_util:put_reqid(Context),

    case braintree_address:delete(Context#cb_context.account_id, AddressId) of
        {ok, #bt_address{}=A} ->
            Resp = braintree_card:record_to_json(A),
            crossbar_util:response(Resp, Context);
        {error, #bt_api_error{}=ApiError} ->
            Resp = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Resp, Context);
        {error, not_found} ->
            crossbar_util:response_bad_identifier(AddressId, Context);
        {error, _} ->
            crossbar_util:response_db_fatal(Context)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Any account that does not have a credit card is disabled, also
%% disabling all decendants... BRING THE HAMMER
%% @end
%%--------------------------------------------------------------------
-spec disable_cardless_accounts/2 :: (list(), #cb_context{}) -> #cb_context{}.
disable_cardless_accounts([], #cb_context{account_id=AccountId}) ->
    crossbar_util:disable_account(AccountId);
disable_cardless_accounts(_, #cb_context{account_id=AccountId}) ->
    crossbar_util:enable_account(AccountId).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates an empty customer in braintree
%% @end
%%--------------------------------------------------------------------
-spec create_placeholder_account/1 :: (#cb_context{}) -> #cb_context{}.
create_placeholder_account(#cb_context{account_id=AccountId}=Context) ->
    case braintree_customer:create(#bt_customer{id=wh_util:to_list(AccountId)}) of
        {ok, #bt_customer{}=C} ->
            lager:debug("created new customer ~s", [AccountId]),
            Resp = braintree_customer:record_to_json(C),
            crossbar_util:response(Resp, Context);
        {error, #bt_api_error{message=Msg}=ApiError} ->
            lager:debug("failed to created new customer ~s", [Msg]),
            Resp = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Resp, Context);
        {error, _}=E ->
            lager:debug("failed to created new customer ~p", [E]),
            crossbar_util:response_db_fatal(Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function processes any trunkstore requests for billing
%% changes, preforms the necessary braintree updates/charges and
%% terminates the request if it fails
%% @end
%%--------------------------------------------------------------------
-spec authorize_trunkstore/2 :: (wh_json:json_strings(), #cb_context{}) -> #cb_context{}.
authorize_trunkstore(_, #cb_context{req_verb = <<"get">>}=Context) ->
    Context#cb_context{resp_status=success};

authorize_trunkstore([], #cb_context{req_verb = <<"put">>, doc=JObj, account_id=AccountId}=Context) ->
    Updates = [{"outbound_us", fun() -> ts_outbound_us_quantity(JObj) end()}
               ,{"did_us", fun() -> ts_did_us_quantity(JObj) end()}
               ,{"tollfree_us", fun() -> ts_tollfree_us_quantity(JObj) end()}
               ,{"e911", fun() -> ts_e911_quantity(JObj) end()}],
    BillingAccount = wh_json:get_value(<<"billing_account_id">>, JObj, AccountId),
    case ts_get_subscription(JObj, BillingAccount) of
        {ok, Subscription} ->
            change_subscription(Updates, Subscription, Context);
        {api_error, Resp} ->    
            crossbar_util:response(error, <<"braintree api error">>, 400, Resp, Context);
        {inactive, Status} ->
            crossbar_util:response(error, <<"billing account is not active">>, 400
                                   ,wh_json:from_list([{<<"current_account_status">>, Status}])
                                   ,Context);
        {error, not_found} ->
            crossbar_util:response(error, <<"no credit card on file">>, 400, Context);        
        {error, no_card} ->
            crossbar_util:response(error, <<"no credit card on file">>, 400, Context);
        {error, no_account} ->
            crossbar_util:response(error, <<"billing account id unspecified">>, 400, Context);
        {error, _} ->
            crossbar_util:response_db_fatal(Context)
    end;
authorize_trunkstore([_], #cb_context{req_verb = <<"post">>, doc=JObj, account_id=AccountId}=Context) ->
    Updates = [{"outbound_us", fun() -> ts_outbound_us_quantity(JObj) end()}
               ,{"did_us", fun() -> ts_did_us_quantity(JObj) end()}
               ,{"tollfree_us", fun() -> ts_tollfree_us_quantity(JObj) end()}
               ,{"e911", fun() -> ts_e911_quantity(JObj) end()}],
    BillingAccount = wh_json:get_value(<<"billing_account_id">>, JObj, AccountId),
    case ts_get_subscription(JObj, BillingAccount) of
        {ok, Subscription} ->
            change_subscription(Updates, Subscription, Context);
        {api_error, Resp} ->    
            crossbar_util:response(error, <<"braintree api error">>, 400, Resp, Context);
        {inactive, Status} ->
            crossbar_util:response(error, <<"billing account is not active">>, 400
                                   ,wh_json:from_list([{<<"current_account_status">>, Status}])
                                   ,Context);
        {error, not_found} ->
            crossbar_util:response(error, <<"no credit card on file">>, 400, Context);        
        {error, no_card} ->
            crossbar_util:response(error, <<"no credit card on file">>, 400, Context);
        {error, no_account} ->
            crossbar_util:response(error, <<"billing account id unspecified">>, 400, Context);
        {error, _} ->
            crossbar_util:response_db_fatal(Context)
    end;
authorize_trunkstore([_], #cb_context{req_verb = <<"delete">>, doc=JObj, account_id=AccountId}=Context) ->
    BillingAccount = wh_json:get_value(<<"billing_account_id">>, JObj, AccountId),
    case ts_get_subscription(JObj, BillingAccount) of
        {ok, #bt_subscription{id=SubscriptionId}} ->
            case braintree_subscription:cancel(SubscriptionId) of
                {ok, #bt_subscription{}} ->
                    lager:debug("cancelled braintree subscription ~s", [SubscriptionId]),
                    Context#cb_context{resp_status=success};
                {error, not_found} ->
                    Context#cb_context{resp_status=success};
                {error, #bt_api_error{message=Msg}=ApiError} ->
                    lager:debug("failed to cancel braintree subscription: ~s", [Msg]),
                    Resp = braintree_util:bt_api_error_to_json(ApiError),
                    crossbar_util:response(error, <<"braintree api error">>, 400, Resp, Context);
                Error ->
                    lager:debug("failed to cancel braintree subscription: ~p", [Error]),
                    crossbar_util:response_db_fatal(Context)
            end;
        {api_error, Resp} ->    
            crossbar_util:response(error, <<"braintree api error">>, 400, Resp, Context);
        {inactive, Status} ->
            crossbar_util:response(error, <<"billing account is not active">>, 400
                                   ,wh_json:from_list([{<<"current_account_status">>, Status}])
                                   ,Context);
        {error, not_found} ->
            Context#cb_context{resp_status=success};
        {error, no_card} ->
            Context#cb_context{resp_status=success};
        {error, no_account} ->
            crossbar_util:response(error, <<"billing account id unspecified">>, 400, Context);
        {error, _} ->
            crossbar_util:response_db_fatal(Context)
    end.

-spec ts_outbound_us_quantity/1 :: (wh_json:json_object()) -> pos_integer().
ts_outbound_us_quantity(JObj) ->
    wh_json:get_integer_value([<<"account">>, <<"trunks">>], JObj, 0).

-spec ts_did_us_quantity/1 :: (wh_json:json_object()) -> pos_integer().
ts_did_us_quantity(JObj) ->
    InUse = [wh_json:get_keys(wh_json:get_value(<<"DIDs">>, Server, []))
             || Server <- wh_json:get_value(<<"servers">>, JObj, [])],
    Unassigned = [wh_json:get_keys(wh_json:get_value(<<"DIDs_Unassigned">>, JObj, []))],
    lists:foldr(ts_fold_did_fun(false), 0, lists:flatten([InUse|Unassigned])).

-spec ts_tollfree_us_quantity/1 :: (wh_json:json_object()) -> pos_integer().
ts_tollfree_us_quantity(JObj) ->
    InUse = [wh_json:get_keys(wh_json:get_value(<<"DIDs">>, Server, []))
             || Server <- wh_json:get_value(<<"servers">>, JObj, [])],
    Unassigned = [wh_json:get_keys(wh_json:get_value(<<"DIDs_Unassigned">>, JObj, []))],
    lists:foldr(ts_fold_did_fun(true), 0, lists:flatten([InUse|Unassigned])).

-spec ts_e911_quantity/1 :: (wh_json:json_object()) -> pos_integer().
ts_e911_quantity(JObj) ->
    E911 = [wh_json:get_value(<<"e911_info">>, Server, [])
             || Server <- wh_json:get_value(<<"servers">>, JObj, [])],
    length(E911).

-spec ts_fold_did_fun/1 :: (boolean()) -> pos_integer().
ts_fold_did_fun(true) ->
    fun(Number, Count) ->
            case wnm_util:is_tollfree(Number) of
                nomatch -> Count + 1;
                _ -> Count
            end
    end;
ts_fold_did_fun(false) ->
    fun(Number, Count) ->
            case wnm_util:is_tollfree(Number) of
                nomatch -> Count;
                _ -> Count + 1
            end
    end.

-spec ts_get_subscription/2 :: (wh_json:json_object(), 'undefined' | ne_binary()) -> {'ok', #bt_subscription{}} |
                                                                                     {'error', atom()} |
                                                                                     {'inactive', ne_binary()} |
                                                                                     {'api_error', wh_json:json_object()}.
ts_get_subscription(_, undefined) ->
    {error, no_account};
ts_get_subscription(JObj, BillingAccount) ->
    ts_get_subscription(JObj, BillingAccount, true).
ts_get_subscription(JObj, BillingAccount, Create) ->
    SubscriptionId = wh_json:get_string_value([<<"pvt_braintree">>, <<"trunkstore_subscription_id">>], JObj),
    case SubscriptionId =/= undefined andalso braintree_subscription:find(SubscriptionId) of
        false when Create ->
            lager:debug("no trunkstore subscription id found"),
            Token = get_payment_token(BillingAccount),
            create_subscription(Token, "SIP_Services");
        false -> {error, not_found};
        {ok, #bt_subscription{status=?BT_ACTIVE}}=Ok ->
            lager:debug("found active trunkstore subscription ~s for account ~s", [SubscriptionId, BillingAccount]),
            Ok;
        {error, not_found} when Create ->
            lager:debug("trunkstore subscription id is not valid"),
            Token = get_payment_token(BillingAccount),
            create_subscription(Token, "SIP_Services");
        {error, not_found} -> {error, not_found};
        {ok, #bt_subscription{status=Status}} ->
            lager:debug("found trunkstore subscription ~s for account ~s", [SubscriptionId, BillingAccount]),
            {inactive, wh_util:to_binary(Status)};
        {error, #bt_api_error{}=ApiError} ->
            Resp = braintree_util:bt_api_error_to_json(ApiError),
            lager:debug("api error getting ts subscription for account ~s: ~p", [BillingAccount, wh_json:encode(Resp)]),
            {api_error, Resp};
        {error, no_card} ->
            lager:debug("account ~s has no card on file", [BillingAccount]),
            {error, no_card};
        {error, _E} ->
            lager:debug("error getting ts subscription for account ~s: ~p", [BillingAccount, _E]),
            {error, fatal}                     
    end.

-spec change_subscription/3 :: (ne_binary(), #bt_subscription{}, #cb_context{}) -> #cb_context{}.
change_subscription(Updates, #bt_subscription{id=undefined}=Subscription, #cb_context{doc=JObj}=Context) ->
    NewSubscription =
        lists:foldr(fun({AddOn, Quantity}, Sub) ->
                            {ok, Subscription1} =
                                braintree_subscription:update_addon_quantity(Sub, AddOn, Quantity),
                            Subscription1
                    end, Subscription, Updates),
    case braintree_subscription:create(NewSubscription) of
        {ok, #bt_subscription{id=Id}} ->
            lager:debug("created braintree subscription ~s", [Id]),
            Context#cb_context{doc=wh_json:set_value([<<"pvt_braintree">>, <<"trunkstore_subscription_id">>]
                                                     ,wh_util:to_binary(Id)
                                                     ,JObj)
                               ,resp_status=success};
        {error, #bt_api_error{message=Msg}=ApiError} ->
            lager:debug("failed to create braintree subscription: ~s", [Msg]),
            Resp = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Resp, Context);
        Error ->
            lager:debug("failed to create braintree subscription: ~p", [Error]),
            crossbar_util:response_db_fatal(Context)
    end;
change_subscription(Updates, Subscription, #cb_context{doc=JObj}=Context) ->
    NewSubscription =
        lists:foldr(fun({AddOn, Quantity}, Sub) ->
                            {ok, Subscription1} =
                                braintree_subscription:update_addon_quantity(Sub, AddOn, Quantity),
                            Subscription1
                    end, Subscription, Updates),
    case braintree_subscription:update(NewSubscription) of
        {ok, #bt_subscription{id=Id}} ->
            lager:debug("updated braintree subscription ~s", [Id]),
            Context#cb_context{doc=wh_json:set_value([<<"pvt_braintree">>, <<"trunkstore_subscription_id">>]
                                                     ,wh_util:to_binary(Id)
                                                     ,JObj)
                               ,resp_status=success};
        {error, #bt_api_error{message=Msg}=ApiError} ->
            lager:debug("failed to updated braintree subscription: ~s", [Msg]),
            Resp = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Resp, Context);
        Error ->
            lager:debug("failed to updated braintree subscription: ~p", [Error]),
            crossbar_util:response_db_fatal(Context)
    end.

-spec create_subscription/2 :: ({'ok', ne_binary()} |
                                {'error', atom()} |
                                {'inactive', ne_binary()} |
                                {'api_error', wh_json:json_object()}
                                ,ne_binary()) -> {'ok', #bt_subscription{}} |
                                                 {'error', atom()} |
                                                 {'inactive', ne_binary()} |
                                                 {'api_error', wh_json:json_object()}.
create_subscription({ok, Token}, Plan) ->
    lager:debug("creating new subscription ~s with token ~s", [Plan, Token]),
    {ok, #bt_subscription{payment_token=Token, plan_id=Plan, do_not_inherit=true}};
create_subscription(Error, _) ->
    Error.

-spec get_payment_token/1 :: (ne_binary() | list()) -> {'ok', ne_binary()} |
                                                       {'error', atom()} |
                                                       {'api_error', wh_json:json_object()}.
get_payment_token(BillingAccount) when not is_list(BillingAccount) ->
    get_payment_token(wh_util:to_list(BillingAccount));
get_payment_token(BillingAccount) ->
    case braintree_customer:find(BillingAccount) of
        {ok, #bt_customer{credit_cards=Cards}} ->
            lager:debug("found braintree customer ~s", [BillingAccount]),
            case [Card || #bt_card{default=Default}=Card <- Cards, Default] of
                [#bt_card{token=Token}] ->
                    lager:debug("braintree customer ~s default credit card token ~s", [BillingAccount, Token]),
                    {ok, Token};
                _ ->
                    lager:debug("braintree customer ~s has no credit card on file", [BillingAccount]),
                    {error, no_card}
            end;
        {error, not_found} ->
            case  braintree_customer:create(#bt_customer{id=BillingAccount}) of
                {ok, #bt_customer{}} ->
                    lager:debug("braintree customer ~s has no credit card on file", [BillingAccount]),
                    {error, no_card};
                {error, #bt_api_error{}=ApiError} ->
                    Resp = braintree_util:bt_api_error_to_json(ApiError),
                    {api_error, Resp};
                _Else ->
                    {error, fatal}
            end;
        {error, #bt_api_error{message=Msg}=ApiError} ->
            lager:debug("failed to find braintree customer: ~s", [Msg]),
            Resp = braintree_util:bt_api_error_to_json(ApiError),
            {api_error, Resp};
        _Else ->
            lager:debug("failed to find braintree customer: ~p", [_Else]),
            {error, fatal}
    end.
