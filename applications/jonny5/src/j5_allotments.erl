%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_allotments).

-export([is_available/2]).
-export([reauthorize/2]).
-export([reconcile_cdr/2]).

-include("jonny5.hrl").

-spec is_available(#limits{}, wh_json:json_object()) -> boolean().
is_available(Limits, JObj) ->
    case try_find_allotment(Limits, JObj) of
        'undefined' -> 'false';
        Allotment ->
            maybe_consume_allotment(Allotment, Limits, JObj)
    end.

-spec reauthorize(#limits{}, wh_json:json_object()) -> 'ok'.
reauthorize(Limits, JObj) ->
    Timestamp = wh_json:get_integer_value(<<"Timestamp">>, JObj),
    Answered = wh_json:get_integer_value(<<"Answered-Time">>, JObj),
    case (Timestamp - Answered) > 55 of
        'false' -> j5_reauthz_req:send_allow_resp(JObj);
        'true' ->
            Allotment = try_find_allotment(Limits, JObj),
            maybe_transition_to_per_minute(Allotment, Limits, JObj)
    end.

-spec reconcile_cdr(ne_binary(), wh_json:json_object()) -> 'ok'.
reconcile_cdr(Account, JObj) ->
    Limits = j5_util:get_limits(Account),
    case wh_json:get_ne_value(<<"Billing-Seconds">>, JObj) =:= 'undefined'
        andalso try_find_allotment(Limits, JObj)
    of
        'undefined' -> 'ok';
        'false' -> 'ok';
        Allotment ->
            Name =  wh_json:get_value(<<"name">>, Allotment),
            Cycle = wh_json:get_ne_value(<<"cycle">>, Allotment, <<"monthly">>),
            return_allotment_consumption([{<<"name">>, Name}], 60, Limits, JObj),
            allotment_consumed_sofar(Name, Cycle, Limits)
    end,
    'ok'.

-spec maybe_transition_to_per_minute('undefined' | wh_json:json_object(), #limits{}, wh_json:json_object()) -> 'ok'.
maybe_transition_to_per_minute('undefined', _, _) -> 'ok';
maybe_transition_to_per_minute(Allotment, Limits, JObj) ->
    Amount = wh_json:get_integer_value(<<"amount">>, Allotment, 0),
    Name = wh_json:get_value(<<"name">>, Allotment),
    Cycle = wh_json:get_ne_value(<<"cycle">>, Allotment, <<"monthly">>),
    case allotment_consumed_sofar(Name, Cycle, Limits) of
        Consumed when (Amount - Consumed) =< 60 ->
            tick_allotment_consumption([{<<"name">>, Name}], 60, Limits, JObj),
            transition_to_per_minute(JObj);
        _Else ->
            tick_allotment_consumption([{<<"name">>, Name}], 60, Limits, JObj),
            j5_reauthz_req:send_allow_resp(JObj)
    end.

-spec transition_to_per_minute(wh_json:json_object()) -> 'ok'.
transition_to_per_minute(JObj) ->
    lager:debug("allotment consumed, transitioning to per_minute", []),
    Timestamp = wh_json:get_integer_value(<<"Timestamp">>, JObj),
    Answered = wh_json:get_integer_value(<<"Answered-Time">>, JObj),
    CCVs = wh_json:from_list([{<<"Billing-Seconds-Offset">>, (Timestamp - Answered) + 62}]),
    j5_reauthz_req:send_allow_resp(wh_json:set_value(<<"Type">>, <<"per_minute">>, JObj), CCVs).

-spec try_find_allotment(#limits{}, wh_json:json_object()) -> wh_json:json_object() | 'undefined'.
try_find_allotment(#limits{allotments=Allotments}, JObj) ->
    [Number, _] = binary:split(wh_json:get_value(<<"Request">>, JObj), <<"@">>),
    case wnm_util:classify_number(Number) of
        'undefined' -> 'undefined';
        Classification ->
            ensure_name_present(wh_json:get_value(Classification, Allotments), Classification)
    end.

-spec ensure_name_present('undefined' | wh_json:json_object(), ne_binary()) -> 'undefined' | wh_json:json_object().
ensure_name_present('undefined', _) -> 'undefined';
ensure_name_present(Allotment, Classification) ->
    Name = wh_json:get_ne_value(<<"name">>, Allotment, Classification),
    wh_json:set_value(<<"name">>, Name, Allotment).

-spec maybe_consume_allotment(wh_json:json_object(), #limits{}, wh_json:json_object()) -> boolean().
maybe_consume_allotment(Allotment, #limits{account_id=AccountId}=Limits, JObj) ->
    Amount = wh_json:get_integer_value(<<"amount">>, Allotment, 0),
    Name = wh_json:get_value(<<"name">>, Allotment),
    Cycle = wh_json:get_ne_value(<<"cycle">>, Allotment, <<"monthly">>),
    case allotment_consumed_sofar(Name, Cycle, Limits) of
        Consumed when Consumed > (Amount - 60) ->
            lager:debug("~s allotment ~s for ~s at ~w/~w", [Cycle, Name, AccountId, Consumed, Amount]),
            'false';
        Consumed ->
            lager:debug("~s allotment ~s for ~s at ~w/~w", [Cycle, Name, AccountId, Consumed, Amount]),
            start_allotment_consumption([{<<"name">>, Name}], 60, Limits, JObj),
            'true'
    end.

-spec allotment_consumed_sofar(ne_binary(), ne_binary(), #limits{}) -> integer().
allotment_consumed_sofar(Name, Cycle, #limits{account_db=AccountDb}) ->
    ViewOptions = [{'startkey', [Name, cycle_start(Cycle)]}
                   ,'group'
                   ,{'group_level', 1}
                   ,'reduce'
                  ],
    case couch_mgr:get_results(AccountDb, <<"transactions/allotment_consumed">>, ViewOptions) of
        {'error', _R} ->
            lager:warning("unable to get consumed allotment for ~s in ~s: ~p", [Name, AccountDb, _R]),
            0;
        {'ok', []} -> 0;
        {'ok', [JObj|_]} ->
            abs(wh_json:get_integer_value(<<"value">>, JObj, 0))
    end.

-spec cycle_start(ne_binary()) -> integer().
cycle_start(<<"yearly">>) ->
    {{Year, _, _}, _} = calendar:universal_time(),
    calendar:datetime_to_gregorian_seconds({{Year, 1, 1}, {0, 0, 0}});
cycle_start(<<"monthly">>) ->
    {{Year, Month, _}, _} = calendar:universal_time(),
    calendar:datetime_to_gregorian_seconds({{Year, Month, 1}, {0, 0, 0}});
cycle_start(<<"weekly">>) ->
    {Date, _} = calendar:universal_time(),
    calendar:datetime_to_gregorian_seconds({Date, {0, 0, 0}}) - (calendar:day_of_the_week(Date) - 1) * 86400;
cycle_start(<<"daily">>) ->
    {{Year, Month, Day}, _} = calendar:universal_time(),
    calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {0, 0, 0}});
cycle_start(<<"hourly">>) ->
    {{Year, Month, Day}, {Hour, _, _}} = calendar:universal_time(),
    calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, 0, 0}});
cycle_start(<<"minutely">>) ->
    {{Year, Month, Day}, {Hour, Min, _}} = calendar:universal_time(),
    calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Min, 0}}).

-spec start_allotment_consumption(proplist(), integer(), #limits{}, wh_json:json_object()) -> wh_jobj_return().
start_allotment_consumption(Props, Units, Limits, JObj) ->
    CompleteProps = [{<<"reason">>, <<"allotment_channel">>}
                     ,{<<"pvt_type">>, <<"debit_allotment">>}
                     | Props
                    ],
    write_to_ledger(<<"start">>, CompleteProps, Units, Limits, JObj).

-spec tick_allotment_consumption(proplist(), integer(), #limits{}, wh_json:json_object()) -> wh_jobj_return().
tick_allotment_consumption(Props, Units, Limits, JObj) ->
    Timestamp = wh_json:get_integer_value(<<"Timestamp">>, JObj, wh_util:current_tstamp()),
    CompleteProps = [{<<"reason">>, <<"allotment_channel">>}
                     ,{<<"pvt_type">>, <<"debit_allotment">>}
                     | Props
                    ],
    write_to_ledger(wh_util:to_binary(Timestamp), CompleteProps, Units, Limits, JObj).

-spec return_allotment_consumption(proplist(), integer(), #limits{}, wh_json:json_object()) -> wh_jobj_return().
return_allotment_consumption(Props, Units, Limits, JObj) ->
    CompleteProps = [{<<"reason">>, <<"allotment_channel">>}
                     ,{<<"pvt_type">>, <<"credit_allotment">>}
                     | Props
                    ],
    write_to_ledger(<<"return">>, CompleteProps, Units, Limits, JObj).

-spec write_to_ledger(ne_binary(), proplist(), integer(), #limits{},  wh_json:object()) -> wh_jobj_return().
-ifdef(TEST).
write_to_ledger(_Suffix, _Props, _Units, _Limits, _JObj) -> {'ok', wh_json:new()}.
-else.
write_to_ledger(Suffix, Props, Units, #limits{account_id=LedgerId, account_db=LedgerDb}, JObj) ->
    Timestamp = get_timestamp(JObj),
    CallId = get_call_id(JObj),
    Id = <<CallId/binary, "-", (wh_util:to_binary(Suffix))/binary>>,
    case props:get_value(<<"pvt_type">>, Props) of
        <<"credit_allotment">> ->
            lager:debug("credit allotment ~s ~wsec for session ~s: ~s"
                        ,[LedgerId, Units, CallId, props:get_value(<<"reason">>, Props, <<"no_reason">>)]);
        <<"debit_allotment">> ->
            lager:debug("debit allotment ~s ~wsec for session ~s: ~s"
                        ,[LedgerId, Units, CallId, props:get_value(<<"reason">>, Props, <<"no_reason">>)])
    end,
    Entry = wh_json:from_list([{<<"_id">>, Id}
                               ,{<<"account_id">>, get_account_id(JObj)}
                               ,{<<"call_id">>, get_call_id(JObj)}
                               ,{<<"amount">>, abs(Units)}
                               ,{<<"pvt_account_id">>, LedgerId}
                               ,{<<"pvt_account_db">>, LedgerDb}
                               ,{<<"pvt_created">>, Timestamp}
                               ,{<<"pvt_modified">>, Timestamp}
                               ,{<<"pvt_vsn">>, 1}
                               ,{<<"pvt_whapp">>, ?APP_NAME}
                               | Props
                              ]),
    couch_mgr:save_doc(LedgerDb, Entry).

-spec get_timestamp(wh_json:object()) -> integer().
get_timestamp(JObj) ->
    case wh_json:get_integer_value(<<"Timestamp">>, JObj) of
        'undefined' -> wh_json:get_integer_value(<<"timestamp">>, JObj, wh_util:current_tstamp());
        Timestamp -> Timestamp
    end.

-spec get_account_id(wh_json:object()) -> integer().
get_account_id(JObj) ->
    case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj) of
        'undefined' ->
            wh_json:get_value([<<"custom_channel_vars">>, <<"account_id">>], JObj);
        AccountId -> AccountId
    end.

-spec get_call_id(wh_json:object()) -> ne_binary().
get_call_id(JObj) ->
    case wh_json:get_value(<<"Call-ID">>, JObj) of
        'undefined' -> wh_json:get_value(<<"call_id">>, JObj);
        CallId-> CallId
    end.
-endif.

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

%% Before we used the wnm_util:classify I had tests here, but now they can't be run in a test env
-endif.
