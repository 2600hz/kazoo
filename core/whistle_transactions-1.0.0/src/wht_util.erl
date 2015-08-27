%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-module(wht_util).

-export([reasons/0
         ,reasons/1
         ,reasons/2
        ]).
-export([dollars_to_units/1]).
-export([units_to_dollars/1]).
-export([pretty_print_dollars/1]).
-export([base_call_cost/3]).
-export([current_balance/1
         ,previous_balance/3
         ,current_account_dollars/1
        ]).
-export([get_balance_from_account/2]).
-export([call_cost/1]).
-export([per_minute_cost/1]).
-export([calculate_cost/5]).
-export([default_reason/0]).
-export([is_valid_reason/1]).
-export([reason_code/1, code_reason/1]).
-export([collapse_call_transactions/1]).
-export([modb/1]).
-export([rollup/1
         ,rollup/2
        ]).

-include("../include/whistle_transactions.hrl").

%% tracked in hundred-ths of a cent
-define(DOLLAR_TO_UNIT, 10000).

-define(REASONS, [{<<"per_minute_call">>, ?CODE_PER_MINUTE_CALL}
                  ,{<<"sub_account_per_minute_call">>, ?CODE_SUB_ACCOUNT_PER_MINUTE_CALL}
                  ,{<<"feature_activation">>, ?CODE_FEATURE_ACTIVATION}
                  ,{<<"sub_account_feature_activation">>, ?CODE_SUB_ACCOUNT_FEATURE_ACTIVATION}
                  ,{<<"number_activation">>, ?CODE_NUMBER_ACTIVATION}
                  ,{<<"sub_account_number_activation">>, ?CODE_SUB_ACCOUNT_NUMBER_ACTIVATION}
                  ,{<<"manual_addition">>, ?CODE_MANUAL_ADDITION}
                  ,{<<"sub_account_manual_addition">>, ?CODE_SUB_ACCOUNT_MANUAL_ADDITION}
                  ,{<<"auto_addition">>, ?CODE_AUTO_ADDITION}
                  ,{<<"sub_account_auto_addition">>, ?CODE_SUB_ACCOUNT_AUTO_ADDITION}
                  ,{<<"admin_discretion">>, ?CODE_ADMIN_DISCRETION}
                  ,{<<"topup">>, ?CODE_TOPUP}
                  ,{<<"database_rollup">>, ?CODE_DATABASE_ROLLUP}
                  ,{<<"recurring">>, ?CODE_RECURRING}
                  ,{<<"monthly_recurring">>, ?CODE_MONTHLY_RECURRING}
                  ,{<<"recurring_prorate">>, ?CODE_RECURRING_PRORATE}
                  ,{<<"unknown">>, ?CODE_UNKNOWN}
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
-spec dollars_to_units(text() | number()) -> integer().
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
-spec units_to_dollars(number()) -> float().
units_to_dollars(Units) when is_number(Units) ->
    trunc(Units) / ?DOLLAR_TO_UNIT;
units_to_dollars(Units) ->
    units_to_dollars(wh_util:to_integer(Units)).

%% @public
-spec pretty_print_dollars(float()) -> ne_binary().
pretty_print_dollars(Amount) ->
    wh_util:to_binary(io_lib:format("$~.2f", [Amount])).


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
-spec current_balance(ne_binary()) -> integer().
current_balance(Account) -> get_balance(Account, []).

-spec previous_balance(ne_binary(), ne_binary(), ne_binary()) -> integer().
previous_balance(Account, Year, Month) ->
    get_balance(Account, [{'year', Year}, {'month', Month}]).

-spec get_balance(ne_binary(), wh_proplist()) -> integer().
get_balance(Account, ViewOptions) ->
    View = <<"transactions/credit_remaining">>,
    case kazoo_modb:get_results(Account, View, ViewOptions) of
        {'ok', []} -> get_balance_from_previous(Account, ViewOptions);
        {'ok', [ViewRes|_]} ->
            Balance = wh_json:get_integer_value(<<"value">>, ViewRes, 0),
            maybe_rollup(Account, ViewOptions, Balance);
        {'error', _E} ->
            lager:warning("unable to get current balance for ~s: ~p", [Account, _E]),
            0
    end.

-spec get_balance_from_previous(ne_binary(), wh_proplist()) -> integer().
-spec get_balance_from_previous(ne_binary(), wh_proplist(), integer()) -> integer().
get_balance_from_previous(Account, ViewOptions) ->
    Retry = props:get_value('retry', ViewOptions, 3),
    get_balance_from_previous(Account, ViewOptions, Retry).

get_balance_from_previous(Account, ViewOptions, Retry) when Retry >= 0 ->
    {DefaultYear, DefaultMonth, _} = erlang:date(),
    Y = props:get_integer_value('year', ViewOptions, DefaultYear),
    M = props:get_integer_value('month', ViewOptions, DefaultMonth),
    {Year, Month} = kazoo_modb_util:prev_year_month(Y, M),

    VOptions = [{'year', wh_util:to_binary(Year)}
                ,{'month', wh_util:pad_month(Month)}
                ,{'retry', Retry-1}
               ],
    lager:warning("could not find current balance trying previous month: ~p", [VOptions]),
    get_balance(Account, VOptions);
get_balance_from_previous(Account, ViewOptions, _) ->
    lager:warning("3 attempt to find balance in previous modb getting from account", []),
    get_balance_from_account(Account, ViewOptions).

-spec maybe_rollup(ne_binary(), wh_proplist(), integer()) -> integer().
maybe_rollup(Account, ViewOptions, Balance) ->
    case props:get_integer_value('retry', ViewOptions) of
        'undefined' when Balance =< 0 ->
            %% NOTE: If the balance was found in the current modb
            %%   but is negative make sure the rollup exists
            verify_monthly_rollup_exists(Account, Balance);
        'undefined' ->
            %% NOTE: if the balance is positive the rollup likey
            %%   occured without issue
            Balance;
        _Else ->
            %% NOTE: if the balance required retries try to
            %%   create the rollup for this month
            maybe_rollup_previous_month(Account, Balance)
    end.

-spec verify_monthly_rollup_exists(ne_binary(), integer()) -> integer().
verify_monthly_rollup_exists(Account, Balance) ->
    case kazoo_modb:open_doc(Account, <<"monthly_rollup">>) of
        {'ok', _} -> Balance;
        {'error', 'not_found'} ->
            %% NOTE: if the monthly_rollup is not in the current modb
            %%   then try to rollup the value
            maybe_rollup_previous_month(Account, Balance);
        {'error', _R} -> Balance
    end.

-spec maybe_rollup_previous_month(ne_binary(), integer()) -> integer().
maybe_rollup_previous_month(Account, Balance) ->
    case get_rollup_from_previous(Account) of
        {'error', _} -> Balance;
        {'ok', PrevBalance} ->
            _ = rollup(Account, PrevBalance),
            PrevBalance
    end.

-spec get_rollup_from_previous(ne_binary()) ->
                                      {'ok', integer()} |
                                      {'error', _}.
get_rollup_from_previous(Account) ->
    {Y, M, _} = erlang:date(),
    {Year, Month} = kazoo_modb_util:prev_year_month(Y, M),
    ModbOptions = [{'year', wh_util:to_binary(Year)}
                   ,{'month', wh_util:pad_month(Month)}
                  ],
    case kazoo_modb:open_doc(Account, <<"monthly_rollup">>, ModbOptions) of
        {'ok', _} ->
            %% NOTE: the previous modb had a rollup, use its balance as
            %%  the value for the current rollup.
            get_rollup_balance(Account, ModbOptions);
        {'error', 'not_found'} ->
            %% NOTE: the previous modb did not have a rollup, this must be
            %%   the first rollup of this series, move the balance from the
            %%   account
            {'ok', get_balance_from_account(Account, ModbOptions)};
        {'error', _R}=E ->
            lager:warning("unable to get previous monthly_rollup: ~p", [_R]),
            E
    end.

-spec get_rollup_balance(ne_binary(), wh_proplist()) ->
                                {'ok', integer()} |
                                {'error', _}.
get_rollup_balance(Account, ViewOptions) ->
    View = <<"transactions/credit_remaining">>,
    case kazoo_modb:get_results(Account, View, ViewOptions) of
        {'ok', []} -> {'error', 'not_found'};
        {'ok', [ViewRes|_]} ->
            {'ok', wh_json:get_integer_value(<<"value">>, ViewRes, 0)};
        {'error', _R}=E ->
            lager:warning("unable to get rollup balance for ~s: ~p"
                          ,[Account, _R]
                         ),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec current_account_dollars(ne_binary()) -> float().
current_account_dollars(Account) ->
    Units = wht_util:current_balance(Account),
    wht_util:units_to_dollars(Units).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_balance_from_account(ne_binary(), wh_proplist()) -> integer().
get_balance_from_account(Account, ViewOptions) ->
    View = <<"transactions/credit_remaining">>,
    AccountId = wh_util:format_account_id(Account, 'raw'),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:get_results(AccountDb, View, ViewOptions) of
        {'ok', []} ->
            lager:debug("no current balance for ~s", [AccountId]),
            0;
        {'ok', [ViewRes|_]} ->
            wh_json:get_integer_value(<<"value">>, ViewRes, 0);
        {'error', _R} ->
            lager:warning("unable to get current balance for ~s: ~p", [AccountId, _R]),
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
    CCVs = wh_json:get_first_defined([<<"Custom-Channel-Vars">>
                                      ,<<"custom_channel_vars">>
                                     ], JObj, JObj),
    RateNoChargeTime = get_integer_value(<<"Rate-NoCharge-Time">>, CCVs),
    BillingSecs = get_integer_value(<<"Billing-Seconds">>, JObj)
        - get_integer_value(<<"Billing-Seconds-Offset">>, CCVs),
    %% if we transition from allotment to per_minute the offset has a slight
    %% fudge factor to allow accounts with no credit to terminate the call
    %% on the next re-authorization cycle (to allow for the in-flight time)
    case BillingSecs =< 0 of
        'true' -> 0;
        'false' when BillingSecs =< RateNoChargeTime ->
            lager:info("billing seconds less then ~ps, no charge",[RateNoChargeTime]),
            0;
        'false' ->
            Rate = get_integer_value(<<"Rate">>, CCVs),
            RateIncr = get_integer_value(<<"Rate-Increment">>, CCVs, 60),
            RateMin = get_integer_value(<<"Rate-Minimum">>, CCVs),
            Surcharge = get_integer_value(<<"Surcharge">>, CCVs),
            Cost = calculate_cost(Rate, RateIncr, RateMin, Surcharge, BillingSecs),
            Discount = (get_integer_value(<<"Discount-Percentage">>, CCVs) * 0.01) * Cost,
            lager:info("rate $~p/~ps, minimum ~ps, surcharge $~p, for ~ps, no charge time ~ps, sub total $~p, discount $~p, total $~p"
                       ,[units_to_dollars(Rate)
                         ,RateIncr, RateMin
                         ,units_to_dollars(Surcharge)
                         ,BillingSecs
                         ,RateNoChargeTime
                         ,units_to_dollars(Cost)
                         ,units_to_dollars(Discount)
                         ,units_to_dollars(Cost - Discount)
                        ]),
            trunc(Cost - Discount)
    end.

-spec get_integer_value(ne_binary(), wh_json:object()) -> integer().
get_integer_value(Key, JObj) ->
    get_integer_value(Key, JObj, 0).

-spec get_integer_value(ne_binary(), wh_json:object(), any()) -> integer().
get_integer_value(Key, JObj, Default) ->
    Keys = [Key, wh_json:normalize_key(Key)],
    wh_util:to_integer(wh_json:get_first_defined(Keys, JObj, Default)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec per_minute_cost(wh_json:object()) -> integer().
per_minute_cost(JObj) ->
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    RateNoChargeTime = get_integer_value(<<"Rate-NoCharge-Time">>, CCVs),
    BillingSecs = wh_json:get_integer_value(<<"Billing-Seconds">>, JObj)
        - wh_json:get_integer_value(<<"Billing-Seconds-Offset">>, CCVs, 0),
    case BillingSecs =< 0 of
        'true' -> 0;
        'false' when BillingSecs =< RateNoChargeTime -> 0;
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
default_reason() -> <<"unknown">>.

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
-spec reason_code(ne_binary()) -> pos_integer().
reason_code(Reason) ->
    case lists:keyfind(Reason, 1, ?REASONS) of
        'false' -> ?CODE_UNKNOWN;
        {_, Code} -> Code
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec code_reason(pos_integer()) -> api_binary().
code_reason(Code) ->
    case lists:keyfind(Code, 2, ?REASONS) of
        'false' -> <<"unknown">>;
        {Reason, _} -> Reason
    end.

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
                               ,[]
                              ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec modb(ne_binary()) -> 'ok'.
modb(AccountMODb) ->
    Routines = [fun kazoo_modb_util:prev_year_month/1
                ,fun({Year, Month}) -> previous_balance(AccountMODb, Year, Month) end
                ,fun(Balance) -> rollup(AccountMODb, Balance) end
               ],
    lists:foldl(fun(F, A) -> F(A) end, AccountMODb, Routines).

-spec rollup(wh_transaction:transaction()) -> 'ok'.
-spec rollup(ne_binary(), integer()) -> 'ok'.
rollup(Transaction) ->
    Transaction1 = wh_transaction:set_reason(<<"database_rollup">>, Transaction),
    Transaction2 = wh_transaction:set_description(<<"monthly rollup">>, Transaction1),
    case wh_transaction:save(Transaction2) of
        {'error', 'conflict'} ->
            lager:warning("monthly rollup transaction failed: document already exist", []);
        {'error', _E} ->
            lager:error("monthly rollup transaction failed: ~p", [_E]);
        {'ok', _} ->
            lager:debug("monthly rollup transaction success", [])
    end.

rollup(<<_/binary>> = AccountMODb, Balance) when Balance >= 0 ->
    AccountId = wh_util:format_account_id(AccountMODb, 'raw'),
    rollup(wh_transaction:credit(AccountId, Balance));
rollup(<<_/binary>> = AccountMODb, Balance) ->
    AccountId = wh_util:format_account_id(AccountMODb, 'raw'),
    rollup(wh_transaction:debit(AccountId, -1*Balance)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec collapse_call_transactions(wh_json:objects(), dict:dict(), wh_json:objects()) ->
                                        wh_json:objects().
collapse_call_transactions([], Calls, Transactions) ->
    clean_transactions(Transactions ++ dict:to_list(Calls));
collapse_call_transactions([JObj|JObjs], Calls, Transactions) ->
    case wh_json:get_integer_value(<<"pvt_code">>, JObj) of
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
-spec collapse_call_transaction(wh_json:object(), dict:dict()) -> dict:dict().
-spec collapse_call_transaction(binary(), wh_json:object(), dict:dict()) -> dict:dict().
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
