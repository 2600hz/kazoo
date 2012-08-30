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
        ]).

-include("include/crossbar.hrl").
-include_lib("braintree/include/braintree.hrl").

-define(CUSTOMER_PATH_TOKEN, <<"customer">>).
-define(CARDS_PATH_TOKEN, <<"cards">>).
-define(ADDRESSES_PATH_TOKEN, <<"addresses">>).
-define(TRANSACTIONS_PATH_TOKEN, <<"transactions">>).
-define(CREDITS_PATH_TOKEN, <<"credits">>).

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".braintree">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = ssl:start(),
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.braintree">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.braintree">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.braintree">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.braintree">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.braintree">>, ?MODULE, post),
    crossbar_bindings:bind(<<"v1_resource.execute.delete.braintree">>, ?MODULE, delete).

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
    true;
resource_exists(?CARDS_PATH_TOKEN) ->
    true;
resource_exists(?ADDRESSES_PATH_TOKEN) ->
    true;
resource_exists(?TRANSACTIONS_PATH_TOKEN) ->
    true;
resource_exists(?CREDITS_PATH_TOKEN) ->
    true.

resource_exists(?CARDS_PATH_TOKEN, _) ->
    true;
resource_exists(?ADDRESSES_PATH_TOKEN, _) ->
    true;
resource_exists(?TRANSACTIONS_PATH_TOKEN, _) ->
    true.

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
    try braintree_customer:find(AccountId) of
        #bt_customer{}=Customer ->
            Resp = braintree_customer:record_to_json(Customer),
            crossbar_util:response(Resp, Context)
    catch
        throw:{api_error, Reason} ->
            crossbar_util:response(error, <<"braintree api error">>, 400, Reason, Context);
        throw:{not_found, _} ->
            Customer = braintree_customer:new(AccountId),
            Resp = braintree_customer:record_to_json(Customer),
            crossbar_util:response(Resp, Context);
        throw:{Error, Reason} ->
            crossbar_util:response(error, wh_util:to_binary(Error), 500, Reason, Context)
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
    crossbar_util:response([], cb_context:store(braintree, Customer, Context));

%% CARD API
validate(#cb_context{req_verb = <<"get">>, account_id=AccountId}=Context, ?CARDS_PATH_TOKEN) ->
    try braintree_customer:find(AccountId) of
        #bt_customer{credit_cards=Cards} ->
            Resp = [braintree_card:record_to_json(Card) || Card <- Cards],
            crossbar_util:response(Resp, Context)
    catch
        throw:{api_error, Reason} ->
            crossbar_util:response(error, <<"braintree api error">>, 400, Reason, Context);
        throw:{Error, Reason} ->
            crossbar_util:response(error, wh_util:to_binary(Error), 500, Reason, Context)
    end;
validate(#cb_context{req_verb = <<"put">>, req_data=JObj, account_id=AccountId}=Context, ?CARDS_PATH_TOKEN) ->
    Card = (braintree_card:json_to_record(JObj))#bt_card{customer_id=wh_util:to_list(AccountId)},
    crossbar_util:response([], cb_context:store(braintree, Card, Context));

validate(#cb_context{req_verb = <<"get">>, account_id=AccountId}=Context, ?ADDRESSES_PATH_TOKEN) ->
    try braintree_customer:find(AccountId) of
        #bt_customer{addresses=Addresses} ->
            Resp = [braintree_address:record_to_json(Address) || Address <- Addresses],
            crossbar_util:response(Resp, Context)
    catch
        throw:{api_error, Reason} ->
            crossbar_util:response(error, <<"braintree api error">>, 400, Reason, Context);
        throw:{Error, Reason} ->
            crossbar_util:response(error, wh_util:to_binary(Error), 500, Reason, Context)
    end;
validate(#cb_context{req_verb = <<"put">>, req_data=JObj, account_id=AccountId}=Context, ?ADDRESSES_PATH_TOKEN) ->
    Address = (braintree_address:json_to_record(JObj))#bt_address{customer_id=wh_util:to_list(AccountId)},
    crossbar_util:response([], cb_context:store(braintree, Address, Context));

validate(#cb_context{req_verb = <<"get">>, account_id=AccountId}=Context, ?TRANSACTIONS_PATH_TOKEN) ->
    try braintree_transaction:find_by_customer(AccountId) of
       Transactions ->
            Resp = [braintree_transaction:record_to_json(Transaction) || Transaction <- Transactions],
            crossbar_util:response(Resp, Context)
    catch
        throw:{api_error, Reason} ->
            crossbar_util:response(error, <<"braintree api error">>, 400, Reason, Context);
        throw:{Error, Reason} ->
            crossbar_util:response(error, wh_util:to_binary(Error), 500, Reason, Context)
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
                                              ,{<<"billing_account_id">>, wh_json:get_integer_value(<<"billing_account_id">>, JObj, AccountId)}
                                             ]), Context);
validate(#cb_context{req_verb = <<"put">>, account_id=AccountId, req_data=JObj}=Context, ?CREDITS_PATH_TOKEN) ->
    DB = wh_util:format_account_id(AccountId, encoded),
    Units = case couch_mgr:get_results(DB, <<"transactions/credit_remaining">>, [{<<"reduce">>, true}]) of
                {ok, []} -> lager:debug("No results"), 0;
                {ok, [ViewRes|_]} -> lager:debug("Found obj ~p", [ViewRes]), wh_json:get_integer_value(<<"value">>, ViewRes, 0);
                {error, _E} -> lager:debug("Error loading view: ~p", [_E]), 0
            end,
    BillingId = wh_json:get_value(<<"billing_account_id">>, JObj, AccountId),
    Amount = wh_json:get_value(<<"amount">>, JObj, <<"0.0">>),
    MaxCredit = whapps_config:get_float(?MOD_CONFIG_CAT, <<"max_account_credit">>, 500.00),
    case wapi_money:units_to_dollars(Units) + wh_util:to_float(Amount) > MaxCredit of
        true -> 
            Message = <<"Available credit can not exceed $", (wh_util:to_binary(MaxCredit))/binary>>,
            Reason = wh_json:from_list([{<<"amount">>, wh_json:from_list([{<<"max_credit">>, Message}])}]),
            crossbar_util:response(error, <<"max_credit">>, 500, Reason, Context);
        false ->
            try braintree_transaction:quick_sale(BillingId, Amount) of
                #bt_transaction{}=Transaction ->
                    wh_notify:transaction(AccountId, braintree_transaction:record_to_json(Transaction)),
                    crossbar_util:response(braintree_transaction:record_to_json(Transaction), Context)
            catch
                throw:{api_error, Reason} ->
                    crossbar_util:response(error, <<"braintree api error">>, 400, Reason, Context);
                throw:{Error, Reason} ->
                    crossbar_util:response(error, wh_util:to_binary(Error), 500, Reason, Context)
            end
    end.

validate(#cb_context{req_verb = <<"get">>, account_id=AccountId}=Context, ?CARDS_PATH_TOKEN, CardId) ->
    try braintree_card:find(CardId) of
        #bt_card{customer_id=AccountId}=Card ->
            Resp = braintree_card:record_to_json(Card),
            crossbar_util:response(Resp, Context)
    catch
        throw:{api_error, Reason} ->
            crossbar_util:response(error, <<"braintree api error">>, 400, Reason, Context);
        throw:{Error, Reason} ->
            crossbar_util:response(error, wh_util:to_binary(Error), 500, Reason, Context)
    end;
validate(#cb_context{req_verb = <<"post">>, req_data=JObj, account_id=AccountId}=Context, ?CARDS_PATH_TOKEN, CardId) ->
    Card = (braintree_card:json_to_record(JObj))#bt_card{customer_id=wh_util:to_list(AccountId), token=CardId},
    crossbar_util:response([], cb_context:store(braintree, Card, Context));
validate(#cb_context{req_verb = <<"delete">>}=Context, ?CARDS_PATH_TOKEN, _) ->
    crossbar_util:response([], Context);
validate(#cb_context{req_verb = <<"get">>, account_id=AccountId}=Context, ?ADDRESSES_PATH_TOKEN, AddressId) ->
    try braintree_address:find(AddressId) of
        #bt_address{customer_id=AccountId}=Address ->
            Resp = braintree_address:record_to_json(Address),
            crossbar_util:response(Resp, Context)
    catch
        throw:{api_error, Reason} ->
            crossbar_util:response(error, <<"braintree api error">>, 400, Reason, Context);
        throw:{Error, Reason} ->
            crossbar_util:response(error, wh_util:to_binary(Error), 500, Reason, Context)
    end;
validate(#cb_context{req_verb = <<"post">>, req_data=JObj, account_id=AccountId}=Context, ?ADDRESSES_PATH_TOKEN, AddressId) ->
    Address = (braintree_address:json_to_record(JObj))#bt_address{customer_id=wh_util:to_list(AccountId), id=AddressId},
    crossbar_util:response([], cb_context:store(braintree, Address, Context));
validate(#cb_context{req_verb = <<"delete">>}=Context, ?ADDRESSES_PATH_TOKEN, _) ->
    crossbar_util:response([], Context);

validate(#cb_context{req_verb = <<"get">>}=Context, ?TRANSACTIONS_PATH_TOKEN, TransactionId) ->
    try braintree_transaction:find(TransactionId) of
        #bt_transaction{}=Transaction ->
            Resp = braintree_address:record_to_json(Transaction),
            crossbar_util:response(Resp, Context)
    catch
        throw:{api_error, Reason} ->
            crossbar_util:response(error, <<"braintree api error">>, 400, Reason, Context);
        throw:{Error, Reason} ->
            crossbar_util:response(error, wh_util:to_binary(Error), 500, Reason, Context)
    end.

-spec post/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
-spec post/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
post(Context, ?CUSTOMER_PATH_TOKEN) ->
    try braintree_customer:update(cb_context:fetch(braintree, Context)) of
        #bt_customer{}=Customer ->
            Resp = braintree_customer:record_to_json(Customer),
            crossbar_util:response(Resp, Context)
    catch
        throw:{api_error, Reason} ->
            crossbar_util:response(error, <<"braintree api error">>, 400, Reason, Context);
        throw:{not_found, _} ->
            create_braintree_customer(Context);
        throw:{Error, Reason} ->
            crossbar_util:response(error, wh_util:to_binary(Error), 500, Reason, Context)
    end.

post(Context, ?CARDS_PATH_TOKEN, CardId) ->
    try braintree_card:update(cb_context:fetch(braintree, Context)) of
        #bt_card{}=Card ->
            Resp = braintree_card:record_to_json(Card),
            crossbar_util:response(Resp, Context)
    catch
        throw:{api_error, Reason} ->
            crossbar_util:response(error, <<"braintree api error">>, 400, Reason, Context);
        throw:{not_found, _} ->
            crossbar_util:response_bad_identifier(CardId, Context);
        throw:{Error, Reason} ->
            crossbar_util:response(error, wh_util:to_binary(Error), 500, Reason, Context)
    end;
post(Context, ?ADDRESSES_PATH_TOKEN, AddressId) ->
    try braintree_address:update(cb_context:fetch(braintree, Context)) of
        #bt_address{}=Address ->
            Resp = braintree_address:record_to_json(Address),
            crossbar_util:response(Resp, Context)
    catch
        throw:{api_error, Reason} ->
            crossbar_util:response(error, <<"braintree api error">>, 400, Reason, Context);
        throw:{not_found, _} ->
            crossbar_util:response_bad_identifier(AddressId, Context);
        throw:{Error, Reason} ->
            crossbar_util:response(error, wh_util:to_binary(Error), 500, Reason, Context)
    end.

-spec put/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
put(#cb_context{req_data=ReqData, resp_data=RespData}=Context, ?CREDITS_PATH_TOKEN) ->
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
    try braintree_address:create(cb_context:fetch(braintree, Context)) of
        #bt_address{}=Address ->
            Resp = braintree_address:record_to_json(Address),
            crossbar_util:response(Resp, Context)
    catch
        throw:{api_error, Reason} ->
            crossbar_util:response(error, <<"braintree api error">>, 400, Reason, Context);
        throw:{Error, Reason} ->
            crossbar_util:response(error, wh_util:to_binary(Error), 500, Reason, Context)
    end;
put(Context, ?CARDS_PATH_TOKEN) ->
    try braintree_card:create(cb_context:fetch(braintree, Context)) of
        #bt_card{}=Card ->
            Resp = braintree_card:record_to_json(Card),
            crossbar_util:response(Resp, Context)
    catch
        throw:{api_error, Reason} ->
            crossbar_util:response(error, <<"braintree api error">>, 400, Reason, Context);
        throw:{Error, Reason} ->
            crossbar_util:response(error, wh_util:to_binary(Error), 500, Reason, Context)
    end.

-spec delete/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
delete(Context, ?CARDS_PATH_TOKEN, CardId) ->
    try braintree_card:delete(CardId) of
        #bt_card{}=Card ->
            Resp = braintree_card:record_to_json(Card),
            crossbar_util:response(Resp, Context)
    catch
        throw:{api_error, Reason} ->
            crossbar_util:response(error, <<"braintree api error">>, 400, Reason, Context);
        throw:{Error, Reason} ->
            crossbar_util:response(error, wh_util:to_binary(Error), 500, Reason, Context)
    end;
delete(Context, ?ADDRESSES_PATH_TOKEN, AddressId) ->
    try braintree_address:delete(Context#cb_context.account_id, AddressId) of
        #bt_address{}=Address ->
            Resp = braintree_card:record_to_json(Address),
            crossbar_util:response(Resp, Context)
    catch
        throw:{api_error, Reason} ->
            crossbar_util:response(error, <<"braintree api error">>, 400, Reason, Context);
        throw:{Error, Reason} ->
            crossbar_util:response(error, wh_util:to_binary(Error), 500, Reason, Context)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates an empty customer in braintree
%% @end
%%--------------------------------------------------------------------
-spec create_braintree_customer/1 :: (#cb_context{}) -> #cb_context{}.
create_braintree_customer(#cb_context{account_id=AccountId}=Context) ->
    try
        case cb_context:fetch(braintree, Context) of
            #bt_customer{}=Customer ->
                C = braintree_customer:create(Customer),
                Resp = braintree_customer:record_to_json(C),
                crossbar_util:response(Resp, Context);
            _Else ->
                C = braintree_customer:create(AccountId),
                Resp = braintree_customer:record_to_json(C),
                crossbar_util:response(Resp, Context)
        end
    catch
        throw:{api_error, Reason} ->
            crossbar_util:response(error, <<"braintree api error">>, 400, Reason, Context);
        throw:{Error, Reason} ->
            crossbar_util:response(error, wh_util:to_binary(Error), 500, Reason, Context)
    end.
