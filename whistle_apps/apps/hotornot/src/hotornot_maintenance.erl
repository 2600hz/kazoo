%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Helper functions for users to inspect how HotOrNot is running
%%% @end
%%% Created :  6 Dec 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(hotornot_maintenance).

-export([local_summary/0, rates_for_did/1, rates_between/2]).

-include("hotornot.hrl").

-define(LOCAL_SUMMARY_ROW_FORMAT, " ~10.s | ~9.s | ~9.s | ~9.s | ~9.s | ~9.s |~n").
-define(LOCAL_SUMMARY_HEADER, io:format(?LOCAL_SUMMARY_ROW_FORMAT, [<<"RATE NAME">>, <<"COST">>, <<"INCREMENT">>, <<"MINIMUM">>
                                                                        ,<<"SURCHARGE">>, <<"WEIGHT">>
                                                                   ])).

local_summary() ->
    ok.

rates_for_did(DID) ->
    E164 = wh_util:to_e164(DID),
    <<"+", Start:1/binary, Rest/binary>> = E164,
    End = <<Start/binary, Rest/binary>>,
    case couch_mgr:get_results(?WH_RATES_DB, <<"rating/lookup">>, [{<<"startkey">>, wh_util:to_integer(Start)}
                                                                ,{<<"endkey">>, wh_util:to_integer(End)}]) of
        {ok, []} -> io:format("rate lookup had no results~n");
        {error, _E} -> io:format("rate lookup error: ~p~n", [_E]);
        {ok, Rates} ->
            io:format("Candidates:~n", []),
            ?LOCAL_SUMMARY_HEADER,
            _ = [ print_rate(wh_json:get_value(<<"value">>, R)) || R <- Rates],

            Matching = [Rate
                        || Rate <- Rates,
                           lists:any(fun(Regex) -> re:run(E164, Regex) =/= nomatch end, wh_json:get_value([<<"value">>, <<"routes">>], Rate))
                       ],

            io:format("Matching:~n", []),
            [ print_rate(wh_json:get_value(<<"value">>, R)) || R <- Matching]
    end.

rates_between(Pre, Post) ->
    case couch_mgr:get_results(?WH_RATES_DB, <<"rates/lookup">>, [{<<"startkey">>, wh_util:to_integer(Pre)}
                                                                ,{<<"endkey">>, wh_util:to_integer(Post)}]) of
        {ok, []} -> io:format("rate lookup had no results~n");
        {error, _E} -> io:format("rate lookup error: ~p~n", [_E]);
        {ok, Rates} ->
            io:format("Rates between:~n", []),
            ?LOCAL_SUMMARY_HEADER,
            [ print_rate(wh_json:get_value(<<"value">>, R)) || R <- Rates]
    end.

print_rate(JObj) ->
    io:format(?LOCAL_SUMMARY_ROW_FORMAT, [
                                          wh_json:get_binary_value(<<"rate_name">>, JObj)
                                          ,wh_json:get_binary_value(<<"rate_cost">>, JObj)
                                          ,wh_json:get_binary_value(<<"rate_increment">>, JObj)
                                          ,wh_json:get_binary_value(<<"rate_minimum">>, JObj)
                                          ,wh_json:get_binary_value(<<"rate_surcharge">>, JObj)
                                          ,wh_json:get_binary_value(<<"weight">>, JObj)
                                          ]).
