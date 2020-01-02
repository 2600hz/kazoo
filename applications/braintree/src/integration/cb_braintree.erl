%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Handle client requests for braintree documents
%%% @author Karl Anderson
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
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

-include("braintree_sdk.hrl").
-include_lib("crossbar/src/crossbar.hrl").

-define(CUSTOMER_PATH_TOKEN, <<"customer">>).
-define(CARDS_PATH_TOKEN, <<"cards">>).
-define(ADDRESSES_PATH_TOKEN, <<"addresses">>).
-define(TRANSACTIONS_PATH_TOKEN, <<"transactions">>).
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
                                  Props = [{[<<"credit_card">>, <<"id">>], Id}
                                          ,{[<<"credit_card">>, <<"verify">>], 'true'}
                                          ],
                                  kz_json:set_values(Props, J)
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
    Card = Card0#bt_card{customer_id = cb_context:account_id(Context)
                        ,verify = 'true'
                        },
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
            crossbar_util:response(Resp, Context);
        #bt_card{} ->
            crossbar_util:response_bad_identifier(CardId, Context)
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
validate_card(Context, CardId, ?HTTP_DELETE) ->
    AccountId = cb_context:account_id(Context),
    try braintree_card:find(CardId) of
        #bt_card{customer_id=AccountId}=Card ->
            crossbar_util:response(kz_json:new(), cb_context:store(Context, 'braintree', Card));
        #bt_card{} ->
            crossbar_util:response_bad_identifier(CardId, Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', kz_term:to_binary(Error), 500, Reason, Context)
    end.

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
        'throw':{'not_found', _} ->
            crossbar_util:response_bad_identifier(AddressId, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', kz_term:to_binary(Error), 500, Reason, Context)
    end;
validate_address(Context, AddressId, ?HTTP_POST) ->
    Address0 = braintree_address:json_to_record(cb_context:req_data(Context)),
    Address = Address0#bt_address{customer_id = cb_context:account_id(Context)
                                 ,id = AddressId
                                 },
    crossbar_util:response(kz_json:new(), cb_context:store(Context, 'braintree', Address));
validate_address(Context, AddressId, ?HTTP_DELETE) ->
    AccountId = cb_context:account_id(Context),
    try braintree_address:find(AccountId, AddressId) of
        #bt_address{customer_id=AccountId}=Address ->
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
        #bt_customer{credit_cards=Cards}=Customer ->
            Resp = braintree_customer:record_to_json(Customer),
            _ = braintree_util:update_services_cards(cb_context:account_id(Context), Cards),
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
            _ = braintree_util:update_services_card(cb_context:account_id(Context), Card),
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
            _ = braintree_util:update_services_card(cb_context:account_id(Context), Card),
            _ = sync(Context),
            crossbar_util:response(Resp, Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', kz_term:to_binary(Error), 500, Reason, Context)
    end.

-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete(Context, ?CARDS_PATH_TOKEN, _CardId) ->
    try braintree_card:delete(cb_context:fetch(Context, 'braintree')) of
        #bt_card{}=Card ->
            Resp = braintree_card:record_to_json(Card),
            _ = braintree_util:delete_services_card(cb_context:account_id(Context), Card),
            crossbar_util:response(Resp, Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', kz_term:to_binary(Error), 500, Reason, Context)
    end;
delete(Context, ?ADDRESSES_PATH_TOKEN, _AddressId) ->
    try braintree_address:delete(cb_context:fetch(Context, 'braintree')) of
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
        #bt_customer{credit_cards=Cards} = Created = braintree_customer:create(C),
        Resp = braintree_customer:record_to_json(Created),
        _ = braintree_util:update_services_cards(cb_context:account_id(Context), Cards),
        _ = sync(Context),
        crossbar_util:response(Resp, Context)
    catch
        'throw':{'api_error', Reason} ->
            crossbar_util:response('error', <<"braintree api error">>, 400, Reason, Context);
        'throw':{Error, Reason} ->
            crossbar_util:response('error', kz_term:to_binary(Error), 500, Reason, Context)
    end.

-spec sync(cb_context:context()) -> 'ok'.
sync(Context) ->
    AccountId = cb_context:account_id(Context),
    _P = kz_process:spawn(fun kz_services_bookkeeper:sync/1, [AccountId]),
    lager:debug("syncing ~s in ~p", [AccountId, _P]).
