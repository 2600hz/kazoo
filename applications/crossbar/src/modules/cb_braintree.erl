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

-include("../crossbar.hrl").
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
    _ = crossbar_bindings:bind(<<"*.allowed_methods.braintree">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.braintree">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.braintree">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.braintree">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.braintree">>, ?MODULE, 'post'),
    crossbar_bindings:bind(<<"*.execute.delete.braintree">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(?CUSTOMER_PATH_TOKEN) ->
    [?HTTP_GET, ?HTTP_POST];
allowed_methods(?CARDS_PATH_TOKEN) ->
    [?HTTP_GET, ?HTTP_PUT];
allowed_methods(?ADDRESSES_PATH_TOKEN) ->
    [?HTTP_GET, ?HTTP_PUT];
allowed_methods(?TRANSACTIONS_PATH_TOKEN) ->
    [?HTTP_GET, ?HTTP_PUT];
allowed_methods(?CREDITS_PATH_TOKEN) ->
    [?HTTP_GET, ?HTTP_PUT].

allowed_methods(?CARDS_PATH_TOKEN, _) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE];
allowed_methods(?ADDRESSES_PATH_TOKEN, _) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE];
allowed_methods(?TRANSACTIONS_PATH_TOKEN, _) ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(?CUSTOMER_PATH_TOKEN) -> 'true';
resource_exists(?CARDS_PATH_TOKEN) -> 'true';
resource_exists(?ADDRESSES_PATH_TOKEN) -> 'true';
resource_exists(?TRANSACTIONS_PATH_TOKEN) -> 'true';
resource_exists(?CREDITS_PATH_TOKEN) -> 'true'.

resource_exists(?CARDS_PATH_TOKEN, _) -> 'true';
resource_exists(?ADDRESSES_PATH_TOKEN, _) -> 'true';
resource_exists(?TRANSACTIONS_PATH_TOKEN, _) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
%% CUSTOMER API
validate(#cb_context{req_verb = ?HTTP_GET
                     ,account_id=AccountId
                    }=Context, ?CUSTOMER_PATH_TOKEN) ->
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
            crossbar_util:response('error', wh_util:to_binary(Error), 500, Reason, Context)
    end;
validate(#cb_context{req_verb = ?HTTP_POST
                     ,req_data=JObj
                     ,account_id=AccountId
                    }=Context, ?CUSTOMER_PATH_TOKEN) ->
    Generators = [fun(J) ->
                          case wh_json:get_value(<<"credit_card">>, J) of
                              'undefined' -> J;
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
    crossbar_util:response(wh_json:new(), cb_context:store('braintree', Customer, Context));

%% CARD API
validate(#cb_context{req_verb = ?HTTP_GET
                     ,account_id=AccountId
                    }=Context, ?CARDS_PATH_TOKEN) ->
    try braintree_customer:find(AccountId) of
        #bt_customer{credit_cards=Cards} ->
            Resp = [braintree_card:record_to_json(Card) || Card <- Cards],
            crossbar_util:response(Resp, Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', wh_util:to_binary(Error), 500, Reason, Context)
    end;
validate(#cb_context{req_verb = ?HTTP_PUT
                     ,req_data=JObj
                     ,account_id=AccountId
                    }=Context, ?CARDS_PATH_TOKEN) ->
    Card = (braintree_card:json_to_record(JObj))#bt_card{customer_id=wh_util:to_binary(AccountId)},
    crossbar_util:response(wh_json:new(), cb_context:store('braintree', Card, Context));

validate(#cb_context{req_verb = ?HTTP_GET
                     ,account_id=AccountId
                    }=Context, ?ADDRESSES_PATH_TOKEN) ->
    try braintree_customer:find(AccountId) of
        #bt_customer{addresses=Addresses} ->
            Resp = [braintree_address:record_to_json(Address) || Address <- Addresses],
            crossbar_util:response(Resp, Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', wh_util:to_binary(Error), 500, Reason, Context)
    end;
validate(#cb_context{req_verb = ?HTTP_PUT
                     ,req_data=JObj
                     ,account_id=AccountId
                    }=Context, ?ADDRESSES_PATH_TOKEN) ->
    Address = (braintree_address:json_to_record(JObj))#bt_address{customer_id=AccountId},
    crossbar_util:response(wh_json:new(), cb_context:store('braintree', Address, Context));

validate(#cb_context{req_verb = ?HTTP_GET
                     ,account_id=AccountId
                    }=Context, ?TRANSACTIONS_PATH_TOKEN) ->
    try braintree_transaction:find_by_customer(AccountId) of
       Transactions ->
            Resp = [braintree_transaction:record_to_json(Transaction) || Transaction <- Transactions],
            crossbar_util:response(Resp, Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', wh_util:to_binary(Error), 500, Reason, Context)
    end;
validate(#cb_context{req_verb = ?HTTP_GET
                     ,account_id=AccountId
                     ,doc=JObj
                    }=Context, ?CREDITS_PATH_TOKEN) ->
    crossbar_util:response(wh_json:from_list([{<<"amount">>, current_account_dollars(AccountId)}
                                              ,{<<"billing_account_id">>, wh_json:get_integer_value(<<"billing_account_id">>, JObj, AccountId)}
                                             ]), Context);
validate(#cb_context{req_verb = ?HTTP_PUT
                     ,account_id=AccountId
                     ,req_data=JObj
                    }=Context, ?CREDITS_PATH_TOKEN) ->
    Amount = wh_json:get_float_value(<<"amount">>, JObj),
    MaxCredit = whapps_config:get_float(?MOD_CONFIG_CAT, <<"max_account_credit">>, 500.00),
    case current_account_dollars(AccountId) + Amount > MaxCredit of
        'true' -> 
            Message = <<"Available credit can not exceed $", (wh_util:to_binary(MaxCredit))/binary>>,
            cb_context:add_validation_error(<<"amount">>, <<"maximum">>, Message, Context);
        'false' ->
            maybe_charge_billing_id(Amount, Context)
    end.

validate(#cb_context{req_verb = ?HTTP_GET
                     ,account_id=AccountId
                    }=Context, ?CARDS_PATH_TOKEN, CardId) ->
    try braintree_card:find(CardId) of
        #bt_card{customer_id=AccountId}=Card ->
            Resp = braintree_card:record_to_json(Card),
            crossbar_util:response(Resp, Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', wh_util:to_binary(Error), 500, Reason, Context)
    end;
validate(#cb_context{req_verb = ?HTTP_POST
                     ,req_data=JObj
                     ,account_id=AccountId
                    }=Context, ?CARDS_PATH_TOKEN, CardId) ->
    Card0 = braintree_card:json_to_record(JObj),
    Card = Card0#bt_card{customer_id=AccountId
                         ,token=CardId
                        },
    crossbar_util:response(wh_json:new(), cb_context:store('braintree', Card, Context));
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, ?CARDS_PATH_TOKEN, _) ->
    crossbar_util:response(wh_json:new(), Context);
validate(#cb_context{req_verb = ?HTTP_GET
                     ,account_id=AccountId
                    }=Context, ?ADDRESSES_PATH_TOKEN, AddressId) ->
    try braintree_address:find(AccountId, AddressId) of
        #bt_address{customer_id=AccountId}=Address ->
            Resp = braintree_address:record_to_json(Address),
            crossbar_util:response(Resp, Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', wh_util:to_binary(Error), 500, Reason, Context)
    end;
validate(#cb_context{req_verb = ?HTTP_POST
                     ,req_data=JObj
                     ,account_id=AccountId
                    }=Context, ?ADDRESSES_PATH_TOKEN, AddressId) ->
    Address = (braintree_address:json_to_record(JObj))#bt_address{customer_id=AccountId, id=AddressId},
    crossbar_util:response(wh_json:new(), cb_context:store('braintree', Address, Context));
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, ?ADDRESSES_PATH_TOKEN, _) ->
    crossbar_util:response(wh_json:new(), Context);

validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?TRANSACTIONS_PATH_TOKEN, TransactionId) ->
    try braintree_transaction:find(TransactionId) of
        #bt_transaction{}=Transaction ->
            Resp = braintree_transaction:record_to_json(Transaction),
            crossbar_util:response(Resp, Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', wh_util:to_binary(Error), 500, Reason, Context)
    end.

-spec post(cb_context:context(), path_token()) -> cb_context:context().
-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, ?CUSTOMER_PATH_TOKEN) ->
    try braintree_customer:update(cb_context:fetch('braintree', Context)) of
        #bt_customer{}=Customer ->
            Resp = braintree_customer:record_to_json(Customer),
            crossbar_util:response(Resp, Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{'not_found', _} ->
            create_braintree_customer(Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', wh_util:to_binary(Error), 500, Reason, Context)
    end.

post(Context, ?CARDS_PATH_TOKEN, CardId) ->
    try braintree_card:update(cb_context:fetch('braintree', Context)) of
        #bt_card{}=Card ->
            Resp = braintree_card:record_to_json(Card),
            crossbar_util:response(Resp, Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{'not_found', _} ->
            cb_context:add_system_error('bad_identifier', [{'details', CardId}], Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', wh_util:to_binary(Error), 500, Reason, Context)
    end;
post(Context, ?ADDRESSES_PATH_TOKEN, AddressId) ->
    try braintree_address:update(cb_context:fetch('braintree', Context)) of
        #bt_address{}=Address ->
            Resp = braintree_address:record_to_json(Address),
            crossbar_util:response(Resp, Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{'not_found', _} ->
            crossbar_util:response_bad_identifier(AddressId, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', wh_util:to_binary(Error), 500, Reason, Context)
    end.

-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(#cb_context{req_data=ReqData
                ,resp_data=RespData
                ,account_id=AccountId
                ,auth_account_id=AuthAccountId
               }=Context, ?CREDITS_PATH_TOKEN) ->
    Amount = wh_json:get_float_value(<<"amount">>, ReqData, 0.0),
    Units = wht_util:dollars_to_units(Amount),
    BTData = wh_json:delete_keys([<<"billing_address">>
                                  ,<<"shipping_address">>
                                  ,[<<"card">>, <<"billing_address">>]
                                  ,[?CUSTOMER_PATH_TOKEN, <<"credit_cards">>]
                                  ,[?CUSTOMER_PATH_TOKEN, ?ADDRESSES_PATH_TOKEN]
                                 ], RespData),
    case add_credit_to_account(BTData, Units, AccountId, AuthAccountId) of
        {'ok', Transaction} ->
            wapi_money:publish_credit([{<<"Amount">>, Units}
                                       ,{<<"Account-ID">>, wh_transaction:account_id(Transaction)}
                                       ,{<<"Transaction-ID">>, wh_transaction:id(Transaction)}
                                       | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                                      ]),
            JObj = wh_transaction:to_json(Transaction),
            Context#cb_context{resp_status='success'
                               ,doc=JObj
                               ,resp_data=wh_json:public_fields(JObj)
                              };
        {'error', Reason} ->
            crossbar_util:response('error', <<"transaction error">>, 500, Reason, Context)
    end;
put(Context, ?ADDRESSES_PATH_TOKEN) ->
    try braintree_address:create(cb_context:fetch('braintree', Context)) of
        #bt_address{}=Address ->
            Resp = braintree_address:record_to_json(Address),
            crossbar_util:response(Resp, Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', wh_util:to_binary(Error), 500, Reason, Context)
    end;
put(Context, ?CARDS_PATH_TOKEN) ->
    try braintree_card:create(cb_context:fetch('braintree', Context)) of
        #bt_card{}=Card ->
            Resp = braintree_card:record_to_json(Card),
            crossbar_util:response(Resp, Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', wh_util:to_binary(Error), 500, Reason, Context)
    end.

-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete(Context, ?CARDS_PATH_TOKEN, CardId) ->
    try braintree_card:delete(CardId) of
        #bt_card{}=Card ->
            crossbar_util:response(braintree_card:record_to_json(Card), Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', wh_util:to_binary(Error), 500, Reason, Context)
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
            crossbar_util:response('error', wh_util:to_binary(Error), 500, Reason, Context)
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
-spec create_braintree_customer(cb_context:context()) -> cb_context:context().
create_braintree_customer(#cb_context{account_id=AccountId}=Context) ->
    try
        case cb_context:fetch('braintree', Context) of
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
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', wh_util:to_binary(Error), 500, Reason, Context)
    end.

-spec current_account_dollars(ne_binary()) -> float().
current_account_dollars(Account) ->
    Units = wht_util:current_balance(Account),
    wht_util:units_to_dollars(Units).

-spec maybe_charge_billing_id(float(), cb_context:context()) -> cb_context:context().
maybe_charge_billing_id(Amount, #cb_context{auth_account_id=AuthAccountId, account_id=AccountId}=Context) ->
    {'ok', MasterAccount} = whapps_util:get_master_account_id(),
    case wh_services:find_reseller_id(AccountId) of
        AuthAccountId -> 
            lager:debug("allowing reseller to apply credit without invoking a bookkeeper", []),
            Resp = wh_json:from_list([{<<"amount">>, Amount}]),
            crossbar_util:response(Resp, Context);
        MasterAccount -> 
            lager:debug("invoking a bookkeeper to acquire requested credit", []),
            charge_billing_id(Amount, Context);
        _Else -> 
            lager:debug("sub-accounts of non-master resellers must contact the reseller to change their credit", []),
            Message = <<"Please contact your phone provider to add credit.">>,
            cb_context:add_validation_error(<<"amount">>, <<"forbidden">>, Message, Context)
    end.

-spec charge_billing_id(float(), cb_context:context()) -> cb_context:context().
charge_billing_id(Amount, #cb_context{account_id=AccountId, req_data=JObj}=Context) ->
    BillingId = wh_json:get_value(<<"billing_account_id">>, JObj, AccountId),
    try braintree_transaction:quick_sale(BillingId, wh_util:to_binary(Amount)) of
        #bt_transaction{}=Transaction ->
            wh_notify:transaction(AccountId, braintree_transaction:record_to_json(Transaction)),
            crossbar_util:response(braintree_transaction:record_to_json(Transaction), Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', wh_util:to_binary(Error), 500, Reason, Context)
    end.

-spec add_credit_to_account(wh_json:object(), integer(), ne_binary(), ne_binary()) ->
                                   {'ok', wh_transaction:transaction()} |
                                   {'error', _}.
add_credit_to_account(BraintreeData, Units, LedgerId, AccountId) ->
    lager:debug("putting ~p units", [Units]),
    Routines = [fun(T) ->
                        case LedgerId =/= AccountId of
                            'false' ->
                                wh_transaction:set_reason(<<"manual_addition">>, T);
                            'true' ->
                                T1 = wh_transaction:set_sub_account_id(AccountId, T),
                                wh_transaction:set_reason(<<"sub_account_manual_addition">>, T1)
                        end
                end
                ,fun(T) -> wh_transaction:set_bookkeeper_info(BraintreeData, T) end
                ,fun(T) ->
                         wh_transaction:set_description(<<"credit addition from credit card">>, T)
                 end
               ],
    Transaction = lists:foldl(fun(F, T) -> F(T) end, wh_transaction:credit(LedgerId, Units), Routines),
    wh_transaction:save(Transaction).
