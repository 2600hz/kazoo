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

%% 1 month
-define(FETCH_DEFAULT, 60*60*24*30).
%% 1 month
-define(FETCH_MAX, 60*60*24*31).

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

validate_transactions(Context, ?HTTP_GET) ->
    From = wh_util:to_integer(cb_context:req_value(Context, <<"created_from">>, 0)),
    To = wh_util:to_integer(cb_context:req_value(Context, <<"created_to">>, 0)),
    case validate_date(From, To) of
        {'true', VFrom, VTo} ->
            Options = [{'from', VFrom}
                       ,{'to', VTo}
                       ,{'prorated', 'true'}
                       ,{'reason', cb_context:req_value(Context, <<"reason">>)}
                      ],
            fetch(Context, Options);
        {'false', R} ->
            cb_context:add_validation_error(<<"created_from/created_to">>
                                            ,<<"date_range">>
                                            ,R
                                            ,Context
                                           )
    end.

validate(Context, PathToken) ->
    validate_transaction(Context, PathToken, cb_context:req_verb(Context)).

validate_transaction(Context, <<"current_balance">>, ?HTTP_GET) ->
    Balance = wht_util:units_to_dollars(wht_util:current_balance(cb_context:account_id(Context))),
    JObj = wh_json:from_list([{<<"balance">>, Balance}]),
    cb_context:setters(Context
                       ,[{fun cb_context:set_resp_status/2, 'success'}
                         ,{fun cb_context:set_resp_data/2, JObj}
                        ]);
validate_transaction(Context, <<"monthly_recurring">>, ?HTTP_GET) ->
    From = wh_util:to_integer(cb_context:req_value(Context, <<"created_from">>, 0)),
    To = wh_util:to_integer(cb_context:req_value(Context, <<"created_to">>, 0)),
    case validate_date(From, To) of
        {'true', VFrom, VTo} ->
            Options = [{'from', VFrom}
                       ,{'to', VTo}
                       ,{'prorated', 'false'}
                      ],
            fetch_monthly_recurring(Context, Options);
        {'false', R} ->
            cb_context:add_validation_error(<<"created_from/created_to">>
                                            ,<<"date_range">>
                                            ,R
                                            ,Context
                                           )
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
    Transactions = fetch_transactions(Context, Options),
    ProratedTransactions = fetch_braintree_transactions(Context, Options),
    case {Transactions, ProratedTransactions} of
        {{'ok', Trs}, {'ok', PTrs}} ->
            send_resp({'ok', Trs ++ PTrs}, Context);
        {_, {'error', _}=E} -> send_resp(E, Context);
        {{'error', _}=E, _} -> send_resp(E, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch_monthly_recurring(cb_context:context(), wh_proplist()) -> cb_context:context().
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
                                {'error', ne_binary()}.
fetch_transactions(Context, Options) ->
    From = props:get_value('from', Options),
    To = props:get_value('to', Options),
    try wh_transactions:fetch_since(cb_context:account_id(Context), From, To) of
        {'ok', Transactions} ->
            JObjs = maybe_filter_by_reason(Transactions, Options),
            {'ok', wht_util:collapse_call_transactions(JObjs)};
        {'error', _}=Error -> Error
    catch
        _:_ ->
            {'error', <<"error while fetching transactions">>}
    end.

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
    From = props:get_value('from', Options),
    To = props:get_value('to', Options),
    Prorated = props:get_value('prorated', Options, 'false'),
    case wh_service_transactions:current_billing_period(
           cb_context:account_id(Context)
           ,'transactions'
           ,{timestamp_to_braintree(From), timestamp_to_braintree(To)}
          )
    of
        'not_found' ->
            {'error', <<"no data found in braintree">>};
        'unknown_error' ->
            {'error', <<"unknown braintree error">>};
        BTransactions ->
            JObjs = lists:foldl(fun(BTr, Acc) ->
                                        IsProrated = braintree_transaction_is_prorated(BTr),
                                        case IsProrated =:= Prorated of
                                            'true' -> [filter_braintree_transaction(BTr)|Acc];
                                            'false' -> Acc
                                        end
                                end, [], BTransactions),
            {'ok', JObjs}
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
-spec braintree_transaction_is_prorated(wh_json:objects()) -> boolean().
braintree_transaction_is_prorated(BTransaction) ->
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
-spec filter_braintree_transaction(wh_json:object()) ->
                                          wh_json:object().
filter_braintree_transaction(BTransaction) ->
    Routines = [fun(BTr) -> clean_braintree_transaction(BTr) end
                ,fun(BTr) -> correct_date_braintree_transaction(BTr) end
                ,fun(BTr) -> prorated_braintree_transaction(BTr) end
               ],
    lists:foldl(fun(F, BTr) -> F(BTr) end, BTransaction, Routines).

-spec clean_braintree_transaction(wh_json:object()) -> wh_json:object().
clean_braintree_transaction(BTransaction) ->
    RemoveKeys = [<<"status">>
                  ,<<"type">>
                  ,<<"currency_code">>
                  ,<<"merchant_account_id">>
                  ,<<"settlement_batch">>
                  ,<<"avs_postal_response">>
                  ,<<"avs_street_response">>
                  ,<<"ccv_response_code">>
                  ,<<"processor_authorization_code">>
                  ,<<"processor_response_code">>
                  ,<<"tax_exempt">>
                  ,<<"billing_address">>
                  ,<<"shipping_address">>
                  ,<<"customer">>
                  ,<<"card">>
                 ],
    wh_json:delete_keys(RemoveKeys, BTransaction).

-spec correct_date_braintree_transaction(wh_json:object()) -> wh_json:object().
correct_date_braintree_transaction(BTransaction) ->
    Keys = [<<"created_at">>, <<"update_at">>],
    lists:foldl(fun correct_date_braintree_transaction_fold/2, BTransaction, Keys).

correct_date_braintree_transaction_fold(Key, BTr) ->
    case wh_json:get_value(Key, BTr, 'null') of
        'null' -> BTr;
        V1 ->
            V2 = string:substr(binary:bin_to_list(V1), 1, 10),
            [Y, M, D|_] = string:tokens(V2, "-"),
            {{Y1, _}, {M1, _}, {D1, _}} = {string:to_integer(Y), string:to_integer(M), string:to_integer(D)},
            DateTime = {{Y1, M1, D1}, {0, 0, 0}},
            Timestamp = calendar:datetime_to_gregorian_seconds(DateTime),
            wh_json:set_value(Key, Timestamp, BTr)
    end.

-spec prorated_braintree_transaction(wh_json:object()) -> wh_json:object().
prorated_braintree_transaction(BTransaction) ->
    wh_json:set_value(<<"prorated">>
                      ,braintree_transaction_is_prorated(BTransaction)
                      ,BTransaction).

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

-spec correct_date_braintree_subscription_fold(ne_binary(), wh_json:object()) -> wh_json:objects().
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec timestamp_to_braintree(pos_integer()) -> ne_binary().
timestamp_to_braintree(Timestamp) ->
    {{Y, M, D}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    <<(wh_util:to_binary(M))/binary, "/"
      ,(wh_util:to_binary(D))/binary, "/"
      ,(wh_util:to_binary(Y))/binary
    >>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_date(any(), any()) -> {'true', pos_integer(), pos_integer()} |
                                     {'false', ne_binary()}.
validate_date(0, 0) ->
    validate_date(wh_util:current_tstamp() - ?FETCH_DEFAULT, wh_util:current_tstamp());
validate_date(0, To) ->
    validate_date(To - ?FETCH_DEFAULT, To);
validate_date(From, 0) ->
    validate_date(From, From + ?FETCH_DEFAULT);
validate_date(From, To) when is_integer(From) andalso is_integer(To) ->
    Diff = To - From,
    case {Diff < 0, Diff > ?FETCH_MAX} of
        {'true', _} ->
            {'false', <<"created_from is gretter than created_to">>};
        {_, 'true'} ->
            {'false', <<"Max range is 1 month">>};
        {'false', 'false'} ->
            {'true', From, To}
    end;
validate_date(From, To) ->
    try {wh_util:to_integer(From), wh_util:to_integer(To)} of
        {From1, To1} ->
            validate_date(From1, To1)
    catch
        _:_ ->
            {'false', <<"created_from or created_to filter is not a timestamp">>}
    end.

-ifdef(TEST).

validate_date_test() ->
    Tstamp = wh_util:current_tstamp(),
    MaxFrom = Tstamp - ?FETCH_DEFAULT,
    MaxTo = Tstamp + ?FETCH_DEFAULT,
    ?assertMatch({'true', _, _}, validate_date(0, 0)),
    ?assertMatch({'true', MaxFrom, Tstamp}, validate_date(0, Tstamp)),
    ?assertMatch({'true', Tstamp, MaxTo}, validate_date(Tstamp, 0)),
    T1 = Tstamp + 10,
    ?assertMatch({'true', Tstamp, T1}, validate_date(Tstamp, T1)),
    ?assertMatch({'false', _}, validate_date(T1, Tstamp)),
    T2 = Tstamp + ?FETCH_MAX + 1,
    ?assertMatch({'false', _}, validate_date(Tstamp, T2)).

-endif.
