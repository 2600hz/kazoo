%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors:
%%%     Peter Defebvre
%%%-------------------------------------------------------------------
-module(cb_transactions).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
        ]).

-include("../crossbar.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.transactions">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.transactions">>, ?MODULE, 'resource_exists'),
    crossbar_bindings:bind(<<"*.validate.transactions">>, ?MODULE, 'validate').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET].
allowed_methods(_) ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /transactions => []
%%    /transactions/foo => [<<"foo">>]
%%    /transactions/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /transactions mights load a list of transactions objects
%% /transactions/123 might load the transactions object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().

validate(Context) ->
    validate_transactions(Context, cb_context:req_verb(Context)).

validate(Context, PathToken) ->
    validate_transaction(Context, PathToken, cb_context:req_verb(Context)).

-spec validate_transactions(cb_context:context(), http_method()) ->
                                   cb_context:context().
validate_transactions(Context, ?HTTP_GET) ->
    case cb_modules_util:range_view_options(Context) of
        {CreatedFrom, CreatedTo} ->
            Options = [{'from', CreatedFrom}
                       ,{'to', CreatedTo}
                       ,{'prorated', 'true'}
                       ,{'reason', cb_context:req_value(Context, <<"reason">>)}
                      ],
            fetch(Context, Options);
        Context1 -> Context1
    end.

-spec validate_transaction(cb_context:context(), path_token(), http_method()) ->
                                  cb_context:context().
validate_transaction(Context, <<"current_balance">>, ?HTTP_GET) ->
    Balance = wht_util:units_to_dollars(wht_util:current_balance(cb_context:account_id(Context))),
    JObj = wh_json:from_list([{<<"balance">>, Balance}]),
    cb_context:setters(Context
                       ,[{fun cb_context:set_resp_status/2, 'success'}
                         ,{fun cb_context:set_resp_data/2, JObj}
                        ]);
validate_transaction(Context, <<"monthly_recurring">>, ?HTTP_GET) ->
    case cb_modules_util:range_view_options(Context) of
        {CreatedFrom, CreatedTo} ->
            Options = [{'from', CreatedFrom}
                       ,{'to', CreatedTo}
                       ,{'prorated', 'false'}
                       ,{'reason', cb_context:req_value(Context, <<"reason">>)}
                      ],
            fetch_monthly_recurring(Context, Options);
        Context1 -> Context1
    end;
validate_transaction(Context, <<"subscriptions">>, ?HTTP_GET) ->
    fetch_braintree_subscriptions(Context);
validate_transaction(Context, _PathToken, _Verb) ->
    cb_context:add_system_error('bad_identifier',  Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch(cb_context:context(), wh_proplist()) -> cb_context:context().
fetch(Context, Options) ->
    case fetch_transactions(Context, Options) of
        {'error', _R}=Error -> send_resp(Error, Context);
        {'ok', Transactions} ->
            JObjs = maybe_filter_by_reason(Transactions, Options),
            send_resp({'ok', wht_util:collapse_call_transactions(JObjs)}, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch_monthly_recurring(cb_context:context(), wh_proplist()) ->
                                     cb_context:context().
fetch_monthly_recurring(Context, Options) ->
    Transactions = fetch_braintree_transactions(Context, Options),
    case Transactions of
        {'ok', _}=Resp -> send_resp(Resp, Context);
        {'error', _}=E -> send_resp(E, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch_transactions(cb_context:context(), wh_proplist()) ->
                                {'ok', wh_json:objects()} |
                                {'error', _}.
fetch_transactions(Context, Options) ->
    From = props:get_value('from', Options),
    To = props:get_value('to', Options),
    wh_transactions:fetch_since(cb_context:account_id(Context), From, To).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch_braintree_transactions(cb_context:context(), wh_proplist()) ->
                                          {'ok', wh_json:objects()} |
                                          {'error', ne_binary()}.
fetch_braintree_transactions(Context, Options) ->
    case wh_service_transactions:current_billing_period(
           cb_context:account_id(Context)
           ,'transactions'
           ,{props:get_value('from', Options)
             ,props:get_value('to', Options)
            }
          )
    of
        {'error', _Reason}=Error -> Error;
        {'ok', Transactions} ->
            filter_prorated_transactions(Transactions
                                         ,props:get_value('prorated', Options, 'false')
                                        )
    end.

-spec filter_prorated_transactions(wh_transaction:transactions(), boolean()) ->
                                          {'ok', wh_json:objects()}.
filter_prorated_transactions(Transactions, Prorated) ->
    {'ok'
     ,lists:foldl(fun(Transaction, Acc) ->
                          filter_prorated_transaction_fold(Transaction, Acc, Prorated)
                  end, [], Transactions
                 )
    }.

-spec filter_prorated_transaction_fold(wh_transaction:transaction(), wh_json:objects(), boolean()) ->
                                              wh_json:objects().
filter_prorated_transaction_fold(Transaction, Acc, Prorated) ->
    JObj = wh_transaction:to_public_json(Transaction),
    case braintree_transaction_is_prorated(JObj) of
        Prorated -> [JObj|Acc];
        _ -> Acc
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_filter_by_reason(any(), wh_proplist()) -> wh_json:objects().
maybe_filter_by_reason(Transactions, Options) ->
    case props:get_value('reason', Options) of
        'undefined' ->
            wh_transactions:to_public_json(Transactions);
        Reason ->
            Filtered = wh_transactions:filter_by_reason(Reason, Transactions),
            wh_transactions:to_public_json(Filtered)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec braintree_transaction_is_prorated(wh_json:object()) -> boolean().
braintree_transaction_is_prorated(Transaction) ->
    BTransaction = wh_json:get_value(<<"metadata">>, Transaction, wh_json:new()),
    case wh_json:get_value(<<"subscription_id">>, BTransaction) of
        'undefined' -> 'true';
        _Id ->
            Addon = calculate_addon(BTransaction),
            Discount = calculate_discount(BTransaction),
            Amount = wh_json:get_number_value(<<"amount">>, BTransaction, 0),
            (Addon - Discount) =/= Amount
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate_addon(wh_json:object()) -> number().
calculate_addon(BTransaction) ->
    Addons = wh_json:get_value(<<"add_ons">>, BTransaction, []),
    calculate(Addons, 0).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate_discount(wh_json:object()) -> number().
calculate_discount(BTransaction) ->
    Addons = wh_json:get_value(<<"discounts">>, BTransaction, []),
    calculate(Addons, 0).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate(wh_json:objects(), number()) -> number().
calculate([], Acc) -> Acc/100;
calculate([Addon|Addons], Acc) ->
    Amount = wh_json:get_number_value(<<"amount">>, Addon, 0)*100,
    Quantity = wh_json:get_number_value(<<"quantity">>, Addon, 0),
    calculate(Addons, (Amount*Quantity+Acc)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch_braintree_subscriptions(cb_context:context()) -> cb_context:context().
fetch_braintree_subscriptions(Context) ->
    filter_braintree_subscriptions(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec filter_braintree_subscriptions(cb_context:context()) -> cb_context:context().
filter_braintree_subscriptions(Context) ->
    case wh_service_transactions:current_billing_period(cb_context:account_id(Context), 'subscriptions') of
        'not_found' ->
            send_resp({'error', <<"no data found in braintree">>}, Context);
        'unknow_error' ->
            send_resp({'error', <<"unknown braintree error">>}, Context);
        BSubscriptions ->
            JObjs = [filter_braintree_subscription(BSub) || BSub <- BSubscriptions],
            send_resp({'ok', JObjs}, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec filter_braintree_subscription(wh_json:object()) -> wh_json:object().
filter_braintree_subscription(BSubscription) ->
    Routines = [fun(BSub) -> clean_braintree_subscription(BSub) end
                ,fun(BSub) -> correct_date_braintree_subscription(BSub) end
               ],
    lists:foldl(fun(F, BSub) -> F(BSub) end, BSubscription, Routines).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec clean_braintree_subscription(wh_json:object()) -> wh_json:object().
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
    wh_json:delete_keys(RemoveKeys, BSubscription).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec correct_date_braintree_subscription(wh_json:object()) -> wh_json:object().
correct_date_braintree_subscription(BSubscription) ->
    Keys = [<<"billing_first_date">>
            ,<<"billing_end_date">>
            ,<<"billing_start_date">>
            ,<<"next_bill_date">>
           ],
    lists:foldl(fun correct_date_braintree_subscription_fold/2, BSubscription, Keys).

-spec correct_date_braintree_subscription_fold(ne_binary(), wh_json:object()) -> wh_json:object().
correct_date_braintree_subscription_fold(Key, BSub) ->
    case wh_json:get_value(Key, BSub, 'null') of
        'null' -> BSub;
        V1 ->
            V2 = binary:bin_to_list(V1),
            [Y, M, D|_] = string:tokens(V2, "-"),
            {{Y1, _}, {M1, _}, {D1, _}} = {string:to_integer(Y), string:to_integer(M), string:to_integer(D)},
            DateTime = {{Y1, M1, D1}, {0, 0, 0}},
            Timestamp = calendar:datetime_to_gregorian_seconds(DateTime),
            wh_json:set_value(Key, Timestamp, BSub)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec send_resp({'ok', _} | {'error', _}, cb_context:context()) -> cb_context:context().
send_resp({'ok', JObj}, Context) ->
    cb_context:setters(Context
                       ,[{fun cb_context:set_resp_status/2, 'success'}
                         ,{fun cb_context:set_resp_data/2, JObj}
                        ]);
send_resp({'error', Details}, Context) ->
    cb_context:add_system_error('bad_identifier', [{'details', Details}], Context).
