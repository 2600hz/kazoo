%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-module(wht_util).

-export([reasons/0
         ,reasons/1
         ,reasons/2]).
-export([dollars_to_units/1]).
-export([units_to_dollars/1]).
-export([base_call_cost/3]).
-export([current_balance/1]).
-export([call_cost/1]).
-export([per_minute_cost/1]).
-export([calculate_cost/5]).
-export([default_reason/0]).
-export([is_valid_reason/1]).
-export([reason_code/1]).
-export([collapse_call_transactions/1]).

-include("whistle_transactions.hrl").

%% tracked in hundred-ths of a cent
-define(DOLLAR_TO_UNIT, 10000).

-define(REASONS, [{<<"per_minute_call">>, 1001}
                  ,{<<"sub_account_per_minute_call">>, 1002}
                  ,{<<"feature_activation">>, 2001}
                  ,{<<"sub_account_feature_activation">>, 2002}
                  ,{<<"number_activation">>, 2003}
                  ,{<<"sub_account_number_activation">>, 2004}
                  ,{<<"manual_addition">>, 3001}
                  ,{<<"sub_account_manual_addition">>, 3002}
                  ,{<<"auto_addition">>, 3003}
                  ,{<<"sub_account_auto_addition">>, 3004}
                  ,{<<"admin_discretion">>, 3005}
                  ,{<<"unknown">>, 9999}
                 ]).

reasons() ->
    ?REASONS.
reasons(Min) ->
    reasons(Min, 10000).
reasons(Min, Max) ->
    reasons(Min, Max, ?REASONS, []).
reasons(_, _, [], Acc) ->
    Acc;
reasons(Min, Max, [{R, C} | T], Acc) when C > Min andalso C < Max ->
    reasons(Min, Max, T, [R | Acc]);
reasons(Min, Max, [_ | T], Acc) ->
    reasons(Min, Max, T, Acc).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec dollars_to_units(float() | integer()) -> integer().
dollars_to_units(Dollars) when is_number(Dollars) ->
    round(Dollars * ?DOLLAR_TO_UNIT);
dollars_to_units(Dollars) ->
    dollars_to_units(wh_util:to_float(Dollars)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec units_to_dollars(float() | number()) -> float().
units_to_dollars(Units) when is_number(Units) ->
    trunc(Units) / ?DOLLAR_TO_UNIT;
units_to_dollars(Units) ->
    units_to_dollars(wh_util:to_integer(Units)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec base_call_cost(integer(), integer(), integer()) -> integer().
base_call_cost(RateCost, RateMin, RateSurcharge) when is_integer(RateCost),
                                                      is_integer(RateMin),
                                                      is_integer(RateSurcharge) ->
    RateCost * ( RateMin div 60 ) + RateSurcharge.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec current_balance/1 :: (ne_binary()) -> integer().
current_balance(AccountId) ->
    AccountDB = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:get_results(AccountDB, <<"transactions/credit_remaining">>, []) of
        {'ok', []} ->
            lager:debug("no current balance for ~s", [AccountId]),
            0;
        {'ok', [ViewRes|_]} ->
            wh_json:get_integer_value(<<"value">>, ViewRes, 0);
        {'error', _R} ->
            lager:debug("unable to get current balance for ~s: ~p", [AccountId, _R]),
            0
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec call_cost(wh_json:object()) -> integer().
call_cost(JObj) ->
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    BillingSecs = wh_json:get_integer_value(<<"Billing-Seconds">>, JObj)
        - wh_json:get_integer_value(<<"Billing-Seconds-Offset">>, CCVs, 0),
    %% if we transition from allotment to per_minute the offset has a slight
    %% fudge factor to allow accounts with no credit to terminate the call
    %% on the next re-authorization cycle (to allow for the in-flight time)
    case BillingSecs =< 0 of
        'true' -> 0;
        'false' ->
            Rate = wh_json:get_integer_value(<<"Rate">>, CCVs, 0),
            RateIncr = wh_json:get_integer_value(<<"Rate-Increment">>, CCVs, 60),
            RateMin = wh_json:get_integer_value(<<"Rate-Minimum">>, CCVs, 0),
            Surcharge = wh_json:get_integer_value(<<"Surcharge">>, CCVs, 0),
            Cost = calculate_cost(Rate, RateIncr, RateMin, Surcharge, BillingSecs),
            Discount = (wh_json:get_integer_value(<<"Discount-Percentage">>, CCVs, 0) * 0.01) * Cost,
            lager:warning("rate $~p/~ps, minimum ~ps, surcharge $~p, for ~ps, sub total $~p, discount $~p, total $~p"
                        ,[units_to_dollars(Rate)
                          ,RateIncr, RateMin
                          ,units_to_dollars(Surcharge)
                          ,BillingSecs
                          ,units_to_dollars(Cost)
                          ,units_to_dollars(Discount)
                          ,units_to_dollars(Cost - Discount)
                         ]),
            trunc(Cost - Discount)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec per_minute_cost(wh_json:object()) -> integer().
per_minute_cost(JObj) ->
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    BillingSecs = wh_json:get_integer_value(<<"Billing-Seconds">>, JObj)
        - wh_json:get_integer_value(<<"Billing-Seconds-Offset">>, CCVs, 0),
    case BillingSecs =< 0 of
        'true' -> 0;
        'false' ->
            RateIncr = wh_json:get_integer_value(<<"Rate-Increment">>, CCVs, 60),
            case wh_json:get_integer_value(<<"Rate">>, CCVs, 0) of
                0 -> 0;
                Rate ->
                    trunc((RateIncr / 60) * Rate)
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% R :: rate, per minute, in units (0.01, 1 cent per minute)
%% RI :: rate increment, in seconds, bill in this increment AFTER rate minimum is taken from Secs
%% RM :: rate minimum, in seconds, minimum number of seconds to bill for
%% Sur :: surcharge, in units, (0.05, 5 cents to connect the call)
%% Secs :: billable seconds
%% @end
%%--------------------------------------------------------------------
-spec calculate_cost(integer(), integer(), integer(), integer(), integer()) -> integer().
calculate_cost(_, _, _, _, 0) -> 0;
calculate_cost(R, 0, RM, Sur, Secs) ->
    calculate_cost(R, 60, RM, Sur, Secs);
calculate_cost(R, RI, RM, Sur, Secs) ->
    case Secs =< RM of
        'true' ->
            trunc(Sur + ((RM / 60) * R));
        'false' ->
            trunc(Sur + ((RM / 60) * R) + (wh_util:ceiling((Secs - RM) / RI) * ((RI / 60) * R)))
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec default_reason() -> ne_binary().
default_reason() ->
    <<"unknown">>.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_valid_reason(ne_binary()) -> boolean().
is_valid_reason(Reason) ->
    lists:keyfind(Reason, 1, ?REASONS) =/= 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reason_code(ne_binary()) -> integer().
reason_code(Reason) ->
    {_, Code} = lists:keyfind(Reason, 1, ?REASONS),
    Code.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec collapse_call_transactions(wh_json:objects()) -> wh_json:objects().
collapse_call_transactions(Transactions) ->
    collapse_call_transactions(Transactions
                               ,dict:new()
                               ,[]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec collapse_call_transactions(wh_json:objects(), dict(), wh_json:objects()) ->
                                        wh_json:objects().
collapse_call_transactions([], Calls, Transactions) ->
    clean_transactions(Transactions ++ dict:to_list(Calls));
collapse_call_transactions([JObj|JObjs], Calls, Transactions) ->
    case wh_json:get_integer_value(<<"code">>, JObj) of
        Code when Code >= 1000 andalso Code < 2000 ->
            C = collapse_call_transaction(JObj, Calls),
            collapse_call_transactions(JObjs, C, Transactions);
        _Else ->
            Amount = get_amount(JObj),
            NJObj = wh_json:set_value(<<"amount">>, Amount, JObj),
            collapse_call_transactions(JObjs, Calls, [NJObj|Transactions])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec collapse_call_transaction(wh_json:object(), dict()) -> dict().
-spec collapse_call_transaction(binary(), wh_json:object(), dict()) -> dict().
collapse_call_transaction(JObj, Calls) ->
    case wh_json:get_value(<<"call_id">>, JObj) of
        'undefined' ->
            Calls;
        CallId ->
            collapse_call_transaction(CallId, JObj, Calls)
    end.

collapse_call_transaction(CallId, JObj, Calls) ->
    case dict:find(CallId, Calls) of
        'error' ->
            Amount = get_amount(JObj),
            NJObj = wh_json:set_value(<<"amount">>, Amount, JObj),
            dict:store(CallId, NJObj, Calls);
        {'ok', Call} ->
            Routines = [fun(C) -> collapse_created_time(C, JObj) end
                        ,fun(C) -> collapse_ended_time(C, JObj) end
                        ,fun(C) -> collapse_amount(C, JObj) end
                        ,fun(C) -> collapse_metadata(C, JObj) end
                       ],
            C = lists:foldl(fun(F, C) -> F(C) end, Call, Routines),
            dict:store(CallId, C, Calls)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec collapse_created_time(wh_json:object(), wh_json:object()) -> wh_json:object().
collapse_created_time(Call, JObj) ->
    CurrentCreated = wh_json:get_integer_value(<<"created">>, Call, 0),
    MaybeCreated = wh_json:get_integer_value(<<"created">>, JObj, 0),
    case CurrentCreated < MaybeCreated of
        'true' -> Call;
        'false' -> wh_json:set_value(<<"created">>, MaybeCreated, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec collapse_ended_time(wh_json:object(), wh_json:object()) -> wh_json:object().
collapse_ended_time(Call, JObj) ->
    CurrentEnd = wh_json:get_integer_value(<<"ended">>, Call, 0),
    MaybeEnd = wh_json:get_integer_value(<<"created">>, JObj, 0),
    case CurrentEnd > MaybeEnd of
        'true' -> Call;
        'false' -> wh_json:set_value(<<"ended">>, MaybeEnd, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec collapse_amount(wh_json:object(), wh_json:object()) -> wh_json:object().
collapse_amount(Call, JObj) ->
    CurrentAmount = wh_json:get_value(<<"amount">>, Call, 0),
    MaybeAmount = get_amount(JObj),
    wh_json:set_value(<<"amount">>, MaybeAmount+CurrentAmount, Call).


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec collapse_metadata(wh_json:object(), wh_json:object()) -> wh_json:object().
collapse_metadata(Call, JObj) ->
    case wh_json:get_value(<<"metadata">>, Call) of
        'undefined' ->
            case wh_json:get_value(<<"metadata">>, JObj) of
                'undefined' -> Call;
                MetaData -> wh_json:set_value(<<"metadata">>, MetaData, Call)
            end;
        _ -> Call
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_amount(wh_json:object()) -> float().
get_amount(Call) ->
    Amount = wh_json:get_value(<<"amount">>, Call, 0),
    Type = wh_json:get_value(<<"type">>, Call),
    case Type of
        <<"debit">> -> Amount*-1;
        _ -> Amount
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec clean_transactions(wh_json:objects()) -> wh_json:objects().
-spec clean_transactions(wh_json:objects(), wh_json:objects()) -> wh_json:objects().
clean_transactions(Transactions) ->
    clean_transactions(Transactions, []).

clean_transactions([], Acc) -> Acc;
clean_transactions([{_, Transaction}|Transactions], Acc) ->
    Amount = wh_json:get_value(<<"amount">>, Transaction),
    case Amount == 0 of
        'false' ->
            clean_transactions(Transactions, [clean_transaction(Transaction)|Acc]);
        'true' ->
            clean_transactions(Transactions, Acc)
    end;
clean_transactions([Transaction|Transactions], Acc) ->
    clean_transactions(Transactions, [clean_transaction(Transaction)|Acc]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec clean_transaction(wh_json:object()) -> wh_json:object().
clean_transaction(Transaction) ->
    Routines = [fun(T) -> clean_amount(T) end
                ,fun(T) -> clean_version(T) end
                ,fun(T) -> clean_event(T) end
                ,fun(T) -> clean_id(T) end
               ],
    lists:foldl(fun(F, T) -> F(T) end, Transaction, Routines).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec clean_amount(wh_json:object()) -> wh_json:object().
clean_amount(Transaction) ->
    Amount = wh_json:get_value(<<"amount">>, Transaction),
    case Amount < 0 of
        'true' ->
            wh_json:set_values([{<<"type">>, <<"debit">>}
                                ,{<<"amount">>, Amount*-1}
                               ]
                               ,Transaction);
        'false' ->
            wh_json:set_value(<<"type">>, <<"credit">>, Transaction)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec clean_version(wh_json:object()) -> wh_json:object().
clean_version(Transaction) ->
    wh_json:delete_key(<<"version">>, Transaction).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec clean_event(wh_json:object()) -> wh_json:object().
clean_event(Transaction) ->
    wh_json:delete_key(<<"event">>, Transaction).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec clean_id(wh_json:object()) -> wh_json:object().
clean_id(Transaction) ->
    wh_json:delete_key(<<"id">>, Transaction).
