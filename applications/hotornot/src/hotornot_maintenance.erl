%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz
%%% @doc
%%% Helper functions for users to inspect how HotOrNot is running
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(hotornot_maintenance).

-export([local_summary/0
         ,rates_for_did/1, rates_for_did/3
         ,rates_between/2
        ]).

-include("hotornot.hrl").

-define(LOCAL_SUMMARY_ROW_FORMAT, " ~45.s | ~9.s | ~9.s | ~9.s | ~9.s | ~9.s | ~15.s |~n").
-define(LOCAL_SUMMARY_HEADER, io:format(?LOCAL_SUMMARY_ROW_FORMAT, [<<"RATE NAME">>, <<"COST">>, <<"INCREMENT">>, <<"MINIMUM">>
                                                                    ,<<"SURCHARGE">>, <<"WEIGHT">>, <<"PREFIX">>
                                                                   ])).

-spec local_summary() -> 'ok'.
local_summary() ->
    io:format("use rates_for_did/1 to see what rates would be used for a DID").

-spec rates_for_did(ne_binary()) -> 'ok'.
-spec rates_for_did(ne_binary(), api_binary(), trunking_options()) -> 'ok'.
rates_for_did(DID) ->
    rates_for_did(DID, 'undefined', []).
rates_for_did(DID, Direction, RouteOptions) when is_list(RouteOptions) ->
    case hon_util:candidate_rates(DID) of
        {'ok', []} -> io:format("rate lookup had no results~n");
        {'error', _E} -> io:format("rate lookup error: ~p~n", [_E]);
        {'ok', Rates} ->
            io:format("Candidates:~n", []),
            ?LOCAL_SUMMARY_HEADER,
            _ = [print_rate(R) || R <- Rates],

            print_matching(hon_util:matching_rates(Rates, DID, Direction, RouteOptions))
    end;
rates_for_did(DID, Direction, Opt) ->
    rates_for_did(DID, Direction, [Opt]).

-spec print_matching(wh_json:objects()) -> 'ok'.
print_matching([]) ->
    io:format("no rates matched~n", []);
print_matching(Matching) ->
    io:format("Matching:~n", []),
    ?LOCAL_SUMMARY_HEADER,

    [Winning|Sorted] = hon_util:sort_rates(Matching),
    Name = wh_json:get_value(<<"rate_name">>, Winning),

    [print_rate(R) || R <- [wh_json:set_value(<<"rate_name">>, <<"* ", Name/binary>>, Winning) | Sorted]],
    'ok'.

-spec rates_between(ne_binary(), ne_binary()) -> 'ok'.
rates_between(Pre, Post) ->
    ViewOpts = [{'startkey', wh_util:to_binary(Pre)}
                ,{'endkey', wh_util:to_binary(Post)}
               ],
    case couch_mgr:get_results(?WH_RATES_DB, <<"rates/lookup">>, ViewOpts) of
        {'ok', []} -> io:format("rate lookup had no results~n");
        {'error', _E} -> io:format("rate lookup error: ~p~n", [_E]);
        {'ok', Rates} ->
            io:format("Rates between:~n", []),
            ?LOCAL_SUMMARY_HEADER,
            [print_rate(wh_json:get_value(<<"value">>, R)) || R <- Rates],
            'ok'
    end.

-spec print_rate(wh_json:object()) -> 'ok'.
print_rate(JObj) ->
    io:format(?LOCAL_SUMMARY_ROW_FORMAT, [wh_json:get_binary_value(<<"rate_name">>, JObj)
                                          ,wh_json:get_binary_value(<<"rate_cost">>, JObj)
                                          ,wh_json:get_binary_value(<<"rate_increment">>, JObj)
                                          ,wh_json:get_binary_value(<<"rate_minimum">>, JObj)
                                          ,wh_json:get_binary_value(<<"rate_surcharge">>, JObj, <<"0.0">>)
                                          ,wh_json:get_binary_value(<<"weight">>, JObj)
                                          ,wh_json:get_binary_value(<<"prefix">>, JObj)
                                         ]).
