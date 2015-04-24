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
         ,to_csv/1
        ]).

-include("../crossbar.hrl").
-include_lib("whistle_transactions/include/whistle_transactions.hrl").

-type payload() :: {cowboy_req:req(), cb_context:context()}.

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
    _ = crossbar_bindings:bind(<<"*.validate.transactions">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.to_csv.get.transactions">>, ?MODULE, 'to_csv').

-spec to_csv(payload()) -> payload().
to_csv({Req, Context}) ->
    JObjs = flatten(cb_context:resp_data(Context), []),
    {Req, cb_context:set_resp_data(Context, JObjs)}.

-spec flatten(wh_json:objects(), wh_json:objects()) -> wh_json:objects().
flatten([], Results) ->
    wht_util:collapse_call_transactions(Results);
flatten([JObj|JObjs], Results) ->
    Metadata = wh_json:get_ne_value(<<"metadata">>, JObj),
    case wh_json:is_json_object(Metadata) of
        'true' ->
            Props = wh_json:to_proplist(Metadata),
            flatten(JObjs, [wh_json:set_values(Props, JObj)|Results]);
        'false' ->
            flatten(JObjs, [JObj|Results])
    end;
flatten(Else, _) -> Else.

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
%% /transactions might load a list of transactions objects
%% /transactions/123 might load the transactions object 123
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_transactions(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, PathToken) ->
    validate_transaction(Context, PathToken, cb_context:req_verb(Context)).

-spec validate_transactions(cb_context:context(), http_method()) ->
                                   cb_context:context().
validate_transactions(Context, ?HTTP_GET) ->
    case cb_modules_util:range_view_options(Context) of
        {CreatedFrom, CreatedTo} ->
            Reason = cb_context:req_value(Context, <<"reason">>),
            fetch(Context, CreatedFrom, CreatedTo, Reason);
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
            Reason = cb_context:req_value(Context, <<"reason">>),
            fetch_monthly_recurring(Context, CreatedFrom, CreatedTo, Reason);
        Context1 -> Context1
    end;
validate_transaction(Context, <<"subscriptions">>, ?HTTP_GET) ->
    filter_braintree_subscriptions(Context);
validate_transaction(Context, _PathToken, _Verb) ->
    cb_context:add_system_error('bad_identifier',  Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch(cb_context:context(), ne_binary(), ne_binary(), api_binary()) ->
                   cb_context:context().
fetch(Context, From, To, Reason) ->
    case wh_transactions:fetch_since(cb_context:account_id(Context), From, To) of
        {'error', _R}=Error -> send_resp(Error, Context);
        {'ok', Transactions} ->
            JObjs = maybe_filter_by_reason(Reason, Transactions),
            send_resp({'ok', JObjs}, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch_monthly_recurring(cb_context:context(), ne_binary(), ne_binary(), api_binary()) ->
                                     cb_context:context().
fetch_monthly_recurring(Context, From, To, Reason) ->
    case wh_bookkeeper_braintree:transactions(cb_context:account_id(Context), From, To) of
        {'error', _}=E -> send_resp(E, Context);
        {'ok', Transactions} ->
            JObjs = [JObj
                     || JObj <- maybe_filter_by_reason(Reason, Transactions),
                        wh_json:get_integer_value(<<"code">>, JObj) =:= ?CODE_MONTHLY_RECURRING
                    ],
            send_resp({'ok', JObjs}, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_filter_by_reason(api_binary(), wh_transaction:transactions()) -> wh_json:objects().
maybe_filter_by_reason('undefined', Transactions) ->
    wh_transactions:to_public_json(Transactions);
maybe_filter_by_reason(Reason, Transactions) ->
    Filtered = wh_transactions:filter_by_reason(Reason, Transactions),
    wh_transactions:to_public_json(Filtered).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec filter_braintree_subscriptions(cb_context:context()) -> cb_context:context().
filter_braintree_subscriptions(Context) ->
    AccountId = cb_context:account_id(Context),
    case wh_service_transactions:current_billing_period(AccountId, 'subscriptions') of
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
    Routines = [fun clean_braintree_subscription/1
                ,fun correct_date_braintree_subscription/1
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
        Value ->
            [Y, M, D | _] = string:tokens(binary_to_list(Value), "-"),
            DateTime = {{list_to_integer(Y), list_to_integer(M), list_to_integer(D)}, {0, 0, 0}},
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
    cb_context:add_system_error(
      'bad_identifier'
      ,wh_json:from_list([{<<"cause">>, Details}])
      ,Context
     ).
