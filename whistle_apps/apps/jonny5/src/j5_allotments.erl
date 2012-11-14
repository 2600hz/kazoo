%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_allotments).

-export([is_available/2]).

-include("jonny5.hrl").

-spec is_available/2 :: (#limits{}, wh_json:json_object()) -> boolean().
is_available(Limits, JObj) ->
    case try_find_allotment(Limits, JObj) of
        undefined -> false;
        Allotment -> 
            maybe_consume_allotment(Allotment, Limits, JObj)
    end.

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

maybe_consume_allotment(Allotment, Limits, JObj) ->
    Amount = wh_json:get_integer_value(<<"amount">>, Allotment, 0),
    case (allotment_consumed_sofar(Allotment, Limits) + 60) < Amount of
        false -> false;
        true -> consume_allotment(Allotment, Limits, JObj)
    end.

allotment_consumed_sofar(Allotment, #limits{account_db=AccountDb}) ->
    Cycle = wh_json:get_ne_value(<<"cycle">>, Allotment, <<"monthly">>),
    Name = wh_json:get_value(<<"name">>, Allotment),
    ViewOptions = [{startkey, [Name, cycle_start(Cycle)]}
                   ,{endkey, [Name, wh_json:new()]}
                   ,group
                   ,reduce
                  ],
    case couch_mgr:get_results(AccountDb, <<"transactions/allotment_by_timestamp">>, ViewOptions) of
        {error, _R} ->
            lager:warning("unable to get consumed allotment for ~s in ~s: ~p", [Name, AccountDb, _R]),
            0;
        {ok, JObj} ->
            wh_json:get_integer_value(<<"value">>, JObj, 0)
    end.

consume_allotment(Allotment, #limits{account_db=AccountDb}, JObj) ->
    j5_util:write_allotment_to_ledger(<<"start">>, 60, JObj, AccountDb), 
    true.

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
    calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, 0, 0}}).

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
