%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%% Helper functions for users to inspect how HotOrNot is running
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(hotornot_maintenance).

-export([local_summary/0
        ,rates_for_did/1, rates_for_did/3, rates_for_did/4
        ,rates_between/2
        ,trie_rebuild/0
        ,get_rate_version/0, set_rate_version/1
        ]).

-include("hotornot.hrl").

-define(LOCAL_SUMMARY_ROW_FORMAT,
        " ~45.s | ~9.s | ~9.s | ~9.s | ~9.s | ~9.s | ~15.s | ~15.s | ~15.s |~n").
-define(LOCAL_SUMMARY_HEADER,
        io:format(?LOCAL_SUMMARY_ROW_FORMAT, [<<"RATE NAME">>, <<"COST">>, <<"INCREMENT">>, <<"MINIMUM">>
                                             ,<<"SURCHARGE">>, <<"WEIGHT">>, <<"PREFIX">>, <<"RATEDECK NAME">>
                                             ,<<"VERSION">>
                                             ])).

-spec local_summary() -> 'ok'.
local_summary() ->
    io:format("use rates_for_did/1 to see what rates would be used for a DID").

-spec trie_rebuild() -> {'ok', pid()} | {'error', any()}.
trie_rebuild() ->
    case hon_util:use_trie() of
        'true' -> hon_trie:rebuild();
        'false' -> {'error', 'trie_not_enabled'}
    end.

-spec rates_for_did(ne_binary()) -> 'ok'.
-spec rates_for_did(ne_binary(), api_binary(), trunking_options()) -> 'ok'.
-spec rates_for_did(ne_binary(), api_binary(), api_binary(), trunking_options()) -> 'ok'.
rates_for_did(DID) ->
    rates_for_did(DID, 'undefined', []).
rates_for_did(DID, Direction, RouteOptions) ->
    rates_for_did(DID, Direction, 'undefined', RouteOptions).
rates_for_did(DID, Direction, AccountId, RouteOptions) when is_list(RouteOptions) ->
    case hon_util:candidate_rates(DID) of
        {'ok', []} -> io:format("rate lookup had no results~n");
        {'error', _E} -> io:format("rate lookup error: ~p~n", [_E]);
        {'ok', Rates} ->
            ReqJObj = kz_json:from_list(
                        props:filter_undefined(
                          [{<<"Account-ID">>, AccountId}
                          ,{<<"Direction">>, Direction}
                          ,{<<"Options">>, RouteOptions}
                          ,{<<"To-DID">>, DID}
                          ])),
            io:format("Candidates:~n", []),
            ?LOCAL_SUMMARY_HEADER,
            lists:foreach(fun print_rate/1, Rates),
            print_matching(hon_util:matching_rates(Rates, ReqJObj))
    end;
rates_for_did(DID, Direction, AccountId, Opt) ->
    rates_for_did(DID, Direction, AccountId, [Opt]).

-spec print_matching(kz_json:objects()) -> 'ok'.
print_matching([]) ->
    io:format("no rates matched~n", []);
print_matching(Matching) ->
    io:format("Matching:~n", []),
    ?LOCAL_SUMMARY_HEADER,

    [Winning|Sorted] = hon_util:sort_rates(Matching),
    Name = kz_json:get_value(<<"rate_name">>, Winning),

    lists:foreach(fun print_rate/1
                 ,[kz_json:set_value(<<"rate_name">>, <<"* ", Name/binary>>, Winning)
                   | Sorted
                  ]).

-spec rates_between(ne_binary(), ne_binary()) -> 'ok'.
rates_between(Pre, Post) ->
    ViewOpts = [{'startkey', kz_util:to_binary(Pre)}
               ,{'endkey', kz_util:to_binary(Post)}
               ],
    case kz_datamgr:get_results(?KZ_RATES_DB, <<"rates/lookup">>, ViewOpts) of
        {'ok', []} -> io:format("rate lookup had no results~n");
        {'error', _E} -> io:format("rate lookup error: ~p~n", [_E]);
        {'ok', Rates} ->
            io:format("Rates between:~n", []),
            ?LOCAL_SUMMARY_HEADER,
            _ = [print_rate(kz_json:get_value(<<"value">>, R)) || R <- Rates],
            'ok'
    end.

-spec print_rate(kz_json:object()) -> 'ok'.
print_rate(JObj) ->
    io:format(?LOCAL_SUMMARY_ROW_FORMAT, [kz_json:get_binary_value(<<"rate_name">>, JObj)
                                         ,kz_json:get_binary_value(<<"rate_cost">>, JObj)
                                         ,kz_json:get_binary_value(<<"rate_increment">>, JObj)
                                         ,kz_json:get_binary_value(<<"rate_minimum">>, JObj)
                                         ,kz_json:get_binary_value(<<"rate_surcharge">>, JObj, <<"0.0">>)
                                         ,kz_json:get_binary_value(<<"weight">>, JObj)
                                         ,kz_json:get_binary_value(<<"prefix">>, JObj)
                                         ,kz_json:get_binary_value(<<"ratedeck_name">>, JObj)
                                         ,kz_json:get_binary_value(<<"rate_version">>, JObj)
                                         ]).

-spec get_rate_version() -> api_binary().
get_rate_version() ->
    kapps_config:get_binary(?APP_NAME, <<"rate_version">>).

-spec set_rate_version(ne_binary()) -> 'ok'.
set_rate_version(Version) ->
    kapps_config:set(?APP_NAME, <<"rate_version">>, Version),
    'ok'.
