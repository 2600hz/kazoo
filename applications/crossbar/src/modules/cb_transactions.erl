%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_transactions).

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,authorize/2
        ,validate/1, validate/2
        ,to_csv/1
        ,put/2
        ]).

-include("crossbar.hrl").

-define(REFUND, <<"refund">>).
-define(SALE, <<"sale">>).
-define(VIEW_BY_TIMESTAMP, <<"transactions/list_by_timestamp">>).

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
    _ = crossbar_bindings:bind(<<"*.authorize.transactions">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.validate.transactions">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.transactions">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.transactions">>, ?MODULE, 'delete'),
    _ = crossbar_bindings:bind(<<"*.to_csv.get.transactions">>, ?MODULE, 'to_csv').

-spec to_csv(payload()) -> payload().
to_csv({Req, Context}) ->
    JObjs = flatten(cb_context:resp_data(Context), []),
    {Req, cb_context:set_resp_data(Context, JObjs)}.

-spec flatten(kz_json:objects(), kz_json:objects()) -> kz_json:objects().
flatten([], Results) -> Results;
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
allowed_methods(?REFUND) ->
    [?HTTP_PUT];
allowed_methods(?SALE) ->
    [?HTTP_PUT];
allowed_methods(_TransactionId) ->
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
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authorize(cb_context:context(), path_token()) -> boolean() | {'stop', cb_context:context()}.
authorize(Context, Path) ->
    try authorize_request(Context, Path, cb_context:req_verb(Context))
    catch
        _E:_R ->
            {'stop', cb_context:add_system_error('forbidden', Context)}
    end.

-spec authorize_request(cb_context:context(), path_token(), http_method()) ->
                               boolean() |
                               {'stop', cb_context:context()}.
authorize_request(Context, ?REFUND, ?HTTP_PUT) ->
    authorize_create(Context);
authorize_request(Context, ?SALE, ?HTTP_PUT) ->
    authorize_create(Context);
authorize_request(Context, _, ?HTTP_PUT) ->
    {'stop', cb_context:add_system_error('forbidden', Context)};
authorize_request(_Context, _Path, _Verb) ->
    'false'.

-spec authorize_create(cb_context:context()) -> boolean() |
                                                {'stop', cb_context:context()}.
authorize_create(Context) ->
    IsAuthenticated = cb_context:is_authenticated(Context),
    IsSuperDuperAdmin = cb_context:is_superduper_admin(Context),
    AccountId = cb_context:account_id(Context),
    BookkeeperVendor = kz_services:bookkeeper_vendor_id(
                         kz_services:fetch(AccountId)
                        ),
    AuthAccountId = cb_context:auth_account_id(Context),
    IsReseller = kz_term:is_not_empty(BookkeeperVendor)
        andalso BookkeeperVendor =:= AuthAccountId,
    case IsAuthenticated
        andalso (IsSuperDuperAdmin
                 orelse IsReseller
                )
    of
        'true' -> 'true';
        'false' -> {'stop', cb_context:add_system_error('forbidden', Context)}
    end.

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

-spec validate_transactions(cb_context:context(), http_method()) -> cb_context:context().
validate_transactions(Context, ?HTTP_GET) ->
    Options = [{'mapper', fun normalize_view_results/3}],
    crossbar_view:load_modb(Context, ?VIEW_BY_TIMESTAMP, Options).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, PathToken) ->
    validate_transaction(Context, PathToken, cb_context:req_verb(Context)).

-spec validate_transaction(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_transaction(Context, ?REFUND, ?HTTP_PUT) ->
    Amount = kz_json:get_float_value(<<"amount">>, cb_context:req_data(Context)),
    validate_refund(Context, Amount);
validate_transaction(Context, ?SALE, ?HTTP_PUT) ->
    Amount = kz_json:get_float_value(<<"amount">>, cb_context:req_data(Context)),
    validate_sale(Context, Amount);
validate_transaction(Context, Id, ?HTTP_GET) ->
    AccountId = cb_context:account_id(Context),
    case kz_transaction:fetch(AccountId, Id) of
        {'ok', Transaction} ->
            crossbar_doc:handle_json_success(kz_transaction:public_json(Transaction), Context);
        {'error', Reason} ->
            crossbar_doc:handle_datamgr_errors(Reason, Id, Context)
    end.

-spec validate_refund(cb_context:context(), kz_term:api_float()) -> cb_context:context().
validate_refund(Context, 'undefined') ->
    Message = kz_json:from_list([{<<"message">>, <<"Amount is required">>}]),
    cb_context:add_validation_error(<<"amount">>, <<"required">>, Message, Context);
validate_refund(Context, Amount) when Amount =< 0 ->
    Message = kz_json:from_list([{<<"message">>, <<"Amount must be greater than 0">>}]),
    cb_context:add_validation_error(<<"amount">>, <<"minimum">>, Message, Context);
validate_refund(Context, _) ->
    cb_context:set_resp_status(Context, 'success').

-spec validate_sale(cb_context:context(), kz_term:api_float()) -> cb_context:context().
validate_sale(Context, 'undefined') ->
    Message = kz_json:from_list([{<<"message">>, <<"Amount is required">>}]),
    cb_context:add_validation_error(<<"amount">>, <<"required">>, Message, Context);
validate_sale(Context, Amount) when Amount =< 0 ->
    Message = kz_json:from_list([{<<"message">>, <<"Amount must be more than 0">>}]),
    cb_context:add_validation_error(<<"amount">>, <<"minimum">>, Message, Context);
validate_sale(Context, _) ->
    cb_context:set_resp_status(Context, 'success').

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, Action) ->
    ReqData = cb_context:req_data(Context),
    AccountId = cb_context:account_id(Context),
    Setters =
        props:filter_empty(
          [{fun kz_transaction:set_account/2, AccountId}
          ,{fun kz_transaction:set_description/2
           ,kz_json:get_ne_binary_value(<<"description">>, ReqData)
           }
          ,{fun kz_transaction:set_metadata/2
           ,kz_json:get_ne_json_value(<<"metadata">>, ReqData, kz_json:new())
           }
          ,{fun kz_transaction:set_order_id/2
           ,kz_json:get_ne_binary_value(<<"order_id">>, ReqData)
           }
          ,{fun kz_transaction:set_dollar_amount/2
           ,abs(kz_json:get_integer_value(<<"amount">>, ReqData, 0))
           }
          ,{fun kz_transaction:set_audit/2
           ,crossbar_services:audit_log(Context)
           }
          ,{fun kz_transaction:set_executor_trigger/2
           ,<<"crossbar">>
           }
          ,{fun kz_transaction:set_executor_module/2
           ,kz_term:to_binary(?MODULE)
           }
          ]
         ),
    process_action(Context, Action, kz_transaction:setters(Setters)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec process_action(cb_context:context(), kz_term:ne_binary(), kz_transaction:transaction()) ->
                            cb_context:context().
process_action(Context, ?REFUND, Transaction) ->
    case kz_transaction:refund(Transaction) of
        {'error', Reason} ->
            crossbar_util:response('error', Reason, Context);
        {'ok', SavedTransaction} ->
            handle_bookkeeper_result(Context, SavedTransaction)
    end;
process_action(Context, ?SALE, Transaction) ->
    case kz_transaction:sale(Transaction) of
        {'error', Reason} ->
            crossbar_util:response('error', Reason, Context);
        {'ok', SavedTransaction} ->
            handle_bookkeeper_result(Context, SavedTransaction)
    end.

-spec handle_bookkeeper_result(cb_context:context(), kz_transaction:transaction()) -> cb_context:context().
handle_bookkeeper_result(Context, Transaction) ->
    crossbar_services:transaction_to_error(Context, Transaction).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec normalize_view_results(cb_context:context(), kzd_transactions:doc(), kz_json:objects()) ->
                                    kz_json:objects().
normalize_view_results(_Context, JObj, Acc) ->
    [normalize_view_result(JObj) | Acc].

-spec normalize_view_result(kz_json:object()) -> kz_json:object().
normalize_view_result(JObj) ->
    Value = kz_json:get_ne_json_value(<<"value">>, JObj),
    Amount = kz_currency:units_to_dollars(
               kz_json:get_integer_value(<<"amount">>, Value, 0)
              ),
    kz_json:set_value(<<"amount">>, Amount, Value).
