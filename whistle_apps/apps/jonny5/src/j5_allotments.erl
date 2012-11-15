%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_allotments).

-export([is_available/2]).
-export([session_heartbeat/2]).
-export([reconcile_cdr/2]).

-include("jonny5.hrl").

-spec is_available/2 :: (#limits{}, wh_json:json_object()) -> boolean().
is_available(Limits, JObj) ->
    case try_find_allotment(Limits, JObj) of
        undefined -> false;
        Allotment ->
            maybe_consume_allotment(Allotment, Limits, JObj)
    end.

-spec session_heartbeat/2 :: (ne_binary(), wh_json:json_object()) -> 'ok'.
session_heartbeat(Account, JObj) ->
    Limits = j5_util:get_limits(Account),
    Timestamp = wh_json:get_integer_value(<<"Timestamp">>, JObj),
    Answered = wh_json:get_integer_value(<<"Answered-Time">>, JObj),
    case (Timestamp - Answered) > 55 of
        false -> false;
        true ->
            Allotment = try_find_allotment(Limits, JObj),
            Props = [{<<"name">>, wh_json:get_value(<<"name">>, Allotment)}],
            tick_allotment_consumption(Props, 60, Limits, JObj)
    end.

-spec reconcile_cdr/2 :: (ne_binary(), wh_json:json_object()) -> 'ok'.
reconcile_cdr(Account, JObj) ->
    Limits = j5_util:get_limits(Account),
    case wh_json:get_ne_value(<<"Billing-Seconds">>, JObj) =:= undefined
        andalso try_find_allotment(Limits, JObj)
    of
        undefined -> ok;
        false -> ok;
        Allotment ->
            Name =  wh_json:get_value(<<"name">>, Allotment),
            Cycle = wh_json:get_ne_value(<<"cycle">>, Allotment, <<"monthly">>),
            return_allotment_consumption([{<<"name">>, Name}], 60, Limits, JObj),
            allotment_consumed_sofar(Name, Cycle, Limits)
    end,
    ok.

-spec try_find_allotment/2 :: (#limits{}, wh_json:json_object()) -> wh_json:json_object() | 'undefined'.
-spec try_find_allotment/3 :: ([] | [ne_binary(),...], wh_json:json_object(), ne_binary()) -> wh_json:json_object() | 'undefined'.

try_find_allotment(#limits{allotments=Allotments}, JObj) ->
    [Num, _] = binary:split(wh_json:get_value(<<"Request">>, JObj), <<"@">>),
    Number = wnm_util:to_e164(Num),
    try_find_allotment(wh_json:get_keys(Allotments), Allotments, Number).

try_find_allotment([], _, _) -> undefined;
try_find_allotment([Key|Keys], Allotments, Number) ->
    case re:run(Number, Key) of
        nomatch -> try_find_allotment(Keys, Allotments, Number);
        _Match -> 
            DefaultName = wh_util:to_hex_binary(crypto:md5(Key)),
            Name = wh_json:get_ne_value([Key, <<"name">>], Allotments, DefaultName),
            wh_json:set_value(<<"name">>, Name, wh_json:get_value(Key, Allotments))
    end.

maybe_consume_allotment(Allotment, #limits{account_id=AccountId}=Limits, JObj) ->
    Amount = wh_json:get_integer_value(<<"amount">>, Allotment, 0),
    Name = wh_json:get_value(<<"name">>, Allotment),
    Cycle = wh_json:get_ne_value(<<"cycle">>, Allotment, <<"monthly">>),
    case allotment_consumed_sofar(Name, Cycle, Limits) of
        Consumed when Consumed > (Amount - 60) -> 
            lager:debug("~s allotment ~s for ~s at ~w/~w", [Cycle, Name, AccountId, Consumed, Amount]),
            false;
        Consumed ->
            lager:debug("~s allotment ~s for ~s at ~w/~w", [Cycle, Name, AccountId, Consumed, Amount]),
            start_allotment_consumption([{<<"name">>, Name}], 60, Limits, JObj),
            true
    end.

allotment_consumed_sofar(Name, Cycle, #limits{account_db=AccountDb}) ->
    ViewOptions = [{startkey, [Name, cycle_start(Cycle)]}
                   ,group
                   ,{group_level, 1}
                   ,reduce
                  ],
    case couch_mgr:get_results(AccountDb, <<"transactions/allotment_consumed">>, ViewOptions) of
        {error, _R} ->
            lager:warning("unable to get consumed allotment for ~s in ~s: ~p", [Name, AccountDb, _R]),
            0;
        {ok, []} -> 0;
        {ok, [JObj|_]} ->
            abs(wh_json:get_integer_value(<<"value">>, JObj, 0))
    end.

-spec cycle_start/1 :: (ne_binary()) -> integer().
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

-spec start_allotment_consumption/4 :: (proplist(), integer(), #limits{}, wh_json:json_object()) -> wh_couch_return().
start_allotment_consumption(Props, Units, Limits, JObj) ->
    CompleteProps = [{<<"reason">>, <<"allotment_channel">>}
                     ,{<<"pvt_type">>, <<"debit_allotment">>}
                     | Props
                    ],
    j5_util:write_to_ledger(<<"start">>, CompleteProps, Units, Limits, JObj).

-spec tick_allotment_consumption/4 :: (proplist(), integer(), #limits{}, wh_json:json_object()) -> wh_couch_return().
tick_allotment_consumption(Props, Units, Limits, JObj) ->
    Timestamp = wh_json:get_integer_value(<<"Timestamp">>, JObj, wh_util:current_tstamp()),
    CompleteProps = [{<<"reason">>, <<"allotment_channel">>}
                     ,{<<"pvt_type">>, <<"debit_allotment">>}
                     | Props
                    ], 
    j5_util:write_to_ledger(wh_util:to_binary(Timestamp), CompleteProps, Units, Limits, JObj).

-spec return_allotment_consumption/4 :: (proplist(), integer(), #limits{}, wh_json:json_object()) -> wh_couch_return().
return_allotment_consumption(Props, Units, Limits, JObj) ->
    CompleteProps = [{<<"reason">>, <<"allotment_channel">>}
                     ,{<<"pvt_type">>, <<"credit_allotment">>}
                     | Props
                    ],
    j5_util:write_to_ledger(<<"return">>, CompleteProps, Units, Limits, JObj).

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

limits(415) ->
    #limits{allotments=wh_json:decode(<<"{\"^\\\\+?1415\\\\d{7}$\":{\"allotment\": 100}}">>)};
limits(named) ->
    #limits{allotments=wh_json:decode(<<"{\"^\\\\+?1415\\\\d{7}$\":{\"allotment\": 100, \"name\":\"san_francisco\"}}">>)}.

request_jobj(R) ->
    wh_json:from_list([{<<"Request">>, <<R/binary, "@2600hz.com">>}]).

find_allotments_test() ->
    ?assertEqual(wh_json:from_list([{<<"allotment">>, 100}
                                    ,{<<"name">>, <<"1595b518c6e1b8103fb0843288e5d8e7">>}
                                   ])
                 ,try_find_allotment(limits(415), request_jobj(<<"+14158867900">>))),
    ?assertEqual(wh_json:from_list([{<<"allotment">>, 100}
                                    ,{<<"name">>, <<"san_francisco">>}
                                   ])
                 ,try_find_allotment(limits(named), request_jobj(<<"+14158867900">>))),
    ?assertEqual(undefined
                 ,try_find_allotment(limits(415), request_jobj(<<"+18008867900">>))),
    ok.

-endif.
