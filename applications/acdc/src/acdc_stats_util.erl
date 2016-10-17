%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% Stat util functions
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_stats_util).

-export([wait_time/2
        ,pause_time/2
        ,caller_id_name/2
        ,caller_id_number/2

        ,get_query_limit/1
        ,db_name/1
        ]).

-include("acdc.hrl").
-include("acdc_stats.hrl").

-spec wait_time(ne_binary(), kz_json:object()) -> api_integer().
wait_time(<<"paused">>, _) -> 'undefined';
wait_time(_, JObj) -> kz_json:get_integer_value(<<"Wait-Time">>, JObj).

-spec pause_time(ne_binary(), kz_json:object()) -> api_integer().
pause_time(<<"paused">>, JObj) ->
    case kz_json:get_integer_value(<<"Pause-Time">>, JObj) of
        'undefined' -> kz_json:get_integer_value(<<"Wait-Time">>, JObj);
        PT -> PT
    end;
pause_time(_, _JObj) -> 'undefined'.

-spec caller_id_name(any(), kz_json:object()) -> api_ne_binary().
-spec caller_id_number(any(), kz_json:object()) -> api_integer().
caller_id_name(_, JObj) ->
    kz_json:get_value(<<"Caller-ID-Name">>, JObj).
caller_id_number(_, JObj) ->
    kz_json:get_value(<<"Caller-ID-Number">>, JObj).

-spec get_query_limit(kz_json:object()) -> pos_integer().
get_query_limit(JObj) ->
    Max = ?MAX_RESULT_SET,
    case kz_json:get_integer_value(<<"Limit">>, JObj) of
        'undefined' -> Max;
        N when N > Max -> Max;
        N when N < 1 -> 1;
        N -> N
    end.

-spec db_name(ne_binary()) -> ne_binary().
db_name(Account) ->
    kz_util:format_account_mod_id(Account).
