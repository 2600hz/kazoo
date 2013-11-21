%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, VoIP INC
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

%%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
%%-endif.

%% 1 month
-define(FETCH_DEFAULT, 60*60*24*30).
%% 1 year
-define(FETCH_MAX, 60*60*24*30*12).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init/0 :: () -> 'ok'.
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
-spec allowed_methods/0 :: () -> http_methods() | [].
-spec allowed_methods/1 :: (path_token()) -> http_methods() | [].
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
-spec resource_exists/0 :: () -> 'true'.
-spec resource_exists/1 :: (path_token()) -> 'true'.
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
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
-spec validate/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = ?HTTP_GET,  query_json=Query}=Context) ->
    From = wh_json:get_integer_value(<<"created_from">>, Query, 0),
    To = wh_json:get_integer_value(<<"created_to">>, Query, 0),
    Reason = wh_json:get_value(<<"reason">>, Query, 'undefined'),
    case Reason of
        <<"no_call">> ->
            Reasons = wht_util:reasons(2000),
            fetch(From, To, Context, Reasons);
        _ ->
            fetch(From, To, Context)
    end.

validate(#cb_context{req_verb = ?HTTP_GET, account_id=AccountId}=Context, <<"current_balance">>) ->
    Balance = wht_util:units_to_dollars(wht_util:current_balance(AccountId)),
    JObj = wh_json:from_list([{<<"balance">>, Balance}]),
    Context#cb_context{resp_status=success, resp_data=JObj};
validate(#cb_context{req_verb = ?HTTP_GET, query_json=Query}=Context, <<"monthly_recurring">>) ->
    From = wh_json:get_integer_value(<<"created_from">>, Query, 0),
    To = wh_json:get_integer_value(<<"created_to">>, Query, 0),
    fetch_braintree_transactions(From, To, Context);
validate(#cb_context{req_verb = ?HTTP_GET}=Context, <<"subscriptions">>) ->
    fetch_braintree_subscriptions(Context);
validate(Context, _) ->
    cb_context:add_system_error('bad_identifier',  Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch/3 :: (integer(), integer(), #cb_context{}) -> #cb_context{}.
-spec fetch/4 :: (integer(), integer(), #cb_context{}, ne_binary()) -> #cb_context{}.
fetch(From, To, Context) ->
    case validate_date(From, To) of
        {'true', VFrom, VTo} ->
            filter(VFrom, VTo, Context);
        {'false', R} ->
            cb_context:add_validation_error(<<"created_from/created_to">>
                                                ,<<"date_range">>
                                                ,R
                                            ,Context
                                           )
    end.
fetch(From, To, Context, Reason) ->
    case validate_date(From, To) of
        {'true', VFrom, VTo} ->
            filter(VFrom, VTo, Context, Reason);
        {'false', R} ->
            cb_context:add_validation_error(<<"created_from/created_to">>
                                                ,<<"date_range">>
                                                ,R
                                            ,Context
                                           )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch_braintree_transactions(ne_binary(), ne_binary(), cb_context:context()) -> cb_context:context().
fetch_braintree_transactions(Min, Max, Context) ->
    case validate_date(Min, Max) of
        {'true', From, To} ->
            filter_braintree_transactions(
              timestamp_to_braintree(From)
              ,timestamp_to_braintree(To)
              ,Context
             );
        {'false', R} ->
            cb_context:add_validation_error(<<"created_from/created_to">>
                                                ,<<"date_range">>
                                                ,R
                                            ,Context
                                           )
    end.

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
-spec filter/3 :: (integer(), integer(), cb_context:context()) -> cb_context:context().
-spec filter/4 :: (integer(), integer(), #cb_context{}, ne_binary())  -> #cb_context{}.
filter(From, To, #cb_context{account_id=AccountId}=Context) ->
    try wh_transactions:fetch_since(AccountId, From, To) of
        Transactions ->
            JObj = wh_transactions:to_public_json(Transactions),
            JObj1 = wht_util:collapse_call_transactions(JObj),
            send_resp({'ok', JObj1}, Context)
    catch
        _:_ ->
            send_resp({'error', <<"error while fetching transactions">>}, Context)
    end.
filter(From, To, #cb_context{account_id=AccountId}=Context, Reason) ->
    try wh_transactions:fetch_since(AccountId, From, To) of
        Transactions ->
            Filtered = wh_transactions:filter_by_reason(Reason, Transactions),
            JObj = wh_transactions:to_public_json(Filtered),
            JObj1 = wht_util:collapse_call_transactions(JObj),
            send_resp({'ok', JObj1}, Context)
    catch
        _:_ ->
            send_resp({'error', <<"error while fetching transactions">>}, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec filter_braintree_transactions(ne_binary(), ne_binary(), cb_context:context()) -> cb_context:context().
filter_braintree_transactions(From, To, #cb_context{account_id=AccountId}=Context) ->
    case  wh_service_transactions:current_billing_period(AccountId, 'transactions', {From, To}) of
        'not_found' ->
            send_resp({'error', <<"no data found in braintree">>}, Context);
        'unknow_error' ->
            send_resp({'error', <<"unknow  braintree error">>}, Context);
        BTransactions ->
            JObjs = lists:foldl(fun(BTr, Acc) ->
                        case is_prorated_braintree_transaction(BTr) of
                            'false' -> Acc;
                            _Id -> [filter_braintree_transaction(BTr)|Acc]
                        end
                    end, [], BTransactions),
            send_resp({'ok', JObjs}, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec filter_braintree_subscriptions(cb_context:context()) -> cb_context:context().
filter_braintree_subscriptions(#cb_context{account_id=AccountId}=Context) ->
    case wh_service_transactions:current_billing_period(AccountId, 'subscriptions') of
        'not_found' ->
            send_resp({'error', <<"no data found in braintree">>}, Context);
        'unknow_error' ->
            send_resp({'error', <<"unknow braintree error">>}, Context);
        BSubscriptions ->
            JObjs = [filter_braintree_subscirption(BSub) || BSub <- BSubscriptions],
            send_resp({'ok', JObjs}, Context)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec filter_braintree_transaction(wh_json:object()) -> wh_json:object().
filter_braintree_transaction(BTransaction) ->
    Routines = [fun(BTr) -> clean_braintree_transaction(BTr) end
                ,fun(BTr) -> correct_date_braintree_transaction(BTr) end
                ,fun(BTr) -> prorated_braintree_transaction(BTr) end
               ],
    lists:foldl(fun(F, BTr) -> F(BTr) end, BTransaction, Routines).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec filter_braintree_subscirption(wh_json:object()) -> wh_json:object().
filter_braintree_subscirption(BSubscription) ->
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
-spec is_prorated_braintree_transaction(wh_json:object()) -> wh_json:object().
is_prorated_braintree_transaction(BTransaction) ->
    wh_json:get_value(<<"subscription_id">>, BTransaction, 'false').

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
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
-spec prorated_braintree_transaction(wh_json:object()) -> wh_json:object().
prorated_braintree_transaction(BTransaction) ->
    case wh_json:get_value(<<"subscription_id">>, BTransaction, 'false') of
        'false' ->
            wh_json:new();
        _Id ->
            calculate_prorated(BTransaction)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec correct_date_braintree_transaction(wh_json:object()) -> wh_json:object().
correct_date_braintree_transaction(BTransaction) ->
    Keys = [<<"created_at">>, <<"update_at">>],
    lists:foldl(
      fun(Key, BTr) ->
              case wh_json:get_value(Key, BTr, 'null') of
                  'null' -> BTr;
                  V1 ->
                      V2 = string:substr(binary:bin_to_list(V1), 1, 10),
                      [Y, M, D|_] = string:tokens(V2, "-"),
                      {{Y1, _}, {M1, _}, {D1, _}} = {string:to_integer(Y), string:to_integer(M), string:to_integer(D)},
                      DateTime = {{Y1, M1, D1}, {0, 0, 0}},
                      Timestamp = calendar:datetime_to_gregorian_seconds(DateTime),
                      wh_json:set_value(Key, Timestamp, BTr)
                  end
      end, BTransaction, Keys).


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
    lists:foldl(
      fun(Key, BSub) ->
              case wh_json:get_value(Key, BSub, 'null') of
                  'null' -> BSub;
                  V1 ->
                      V2 = binary:bin_to_list(V1),
                      [Y, M, D|_] = string:tokens(V2, "-"),
                      {{Y1, _}, {M1, _}, {D1, _}} = {string:to_integer(Y), string:to_integer(M), string:to_integer(D)},
                      DateTime = {{Y1, M1, D1}, {0, 0, 0}},
                      Timestamp = calendar:datetime_to_gregorian_seconds(DateTime),
                      wh_json:set_value(Key, Timestamp, BSub)
                  end
      end, BSubscription, Keys).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate_prorated(wh_json:object()) -> wh_json:object().
calculate_prorated(BTransaction) ->
    Addon = calculate_addon(BTransaction),
    Discount = calculate_discount(BTransaction),
    Amount = wh_json:get_number_value(<<"amount">>, BTransaction, 0),
    case (Addon - Discount) =:= Amount of
        'true' ->
            wh_json:set_value(<<"prorated">>, 'false', BTransaction);
        'false' ->
            wh_json:set_value(<<"prorated">>, 'true', BTransaction)
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
calculate([], Acc) ->
    Acc;
calculate([Addon|Addons], Acc) ->
    Amount = wh_json:get_number_value(<<"amount">>, Addon, 0),
    Quantity = wh_json:get_number_value(<<"quantity">>, Addon, 0),
    calculate(Addons, (Amount*Quantity+Acc)).


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
send_resp(Resp, Context) ->
    case Resp of
        {'ok', JObj} ->
            Context#cb_context{resp_status=success, resp_data=JObj};
        {'error', Details} ->
            cb_context:add_system_error('bad_identifier', [{'details', Details}], Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
timestamp_to_braintree(Timestamp) ->
    {{Y, M, D}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    <<(wh_util:to_binary(M))/binary, "/", (wh_util:to_binary(D))/binary, "/", (wh_util:to_binary(Y))/binary>>.


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_date/2 :: (any(), any()) -> {true, integer(), integer()} | {false, ne_binary()}.
validate_date(0, 0) ->
    validate_date(wh_util:current_tstamp()-?FETCH_DEFAULT, wh_util:current_tstamp());
validate_date(0, To) ->
    validate_date(To-?FETCH_DEFAULT, To);
validate_date(From, 0) ->
    validate_date(From, From+?FETCH_DEFAULT);
validate_date(From, To) when is_integer(From) andalso is_integer(To) ->
    Max = ?FETCH_MAX,
    Diff = To - From,
    case {Diff < 0, Diff > Max} of
        {'true', _} ->
            {'false', <<"created_from is gretter than created_to">>};
        {_, 'true'} ->
            {'false', <<"Max range is a year">>};
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
