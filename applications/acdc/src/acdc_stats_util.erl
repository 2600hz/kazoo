%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Stat util functions
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_stats_util).

-export([wait_time/2
        ,pause_time/2
        ,caller_id_name/2
        ,caller_id_number/2
        ,queue_id/2

        ,get_query_limit/1
        ,db_name/1
        ]).

-include("acdc.hrl").
-include("acdc_stats.hrl").

-spec wait_time(kz_term:ne_binary(), kz_json:object()) -> kz_term:api_integer().
wait_time(<<"paused">>, _) -> 'undefined';
wait_time(_, JObj) -> kz_json:get_integer_value(<<"Wait-Time">>, JObj).

-spec pause_time(kz_term:ne_binary(), kz_json:object()) -> kz_term:api_integer().
pause_time(<<"paused">>, JObj) ->
    case kz_json:get_integer_value(<<"Pause-Time">>, JObj) of
        'undefined' -> kz_json:get_integer_value(<<"Wait-Time">>, JObj);
        PT -> PT
    end;
pause_time(_, _JObj) -> 'undefined'.

-spec caller_id_name(any(), kz_json:object()) -> kz_term:api_ne_binary().
caller_id_name(_, JObj) ->
    kz_json:get_value(<<"Caller-ID-Name">>, JObj).

-spec caller_id_number(any(), kz_json:object()) -> kz_term:api_integer().
caller_id_number(_, JObj) ->
    kz_json:get_value(<<"Caller-ID-Number">>, JObj).

-spec queue_id(any(), kz_json:object()) -> kz_term:ne_binary().
queue_id(_, JObj) ->
    kz_json:get_value(<<"Queue-ID">>, JObj).

-spec get_query_limit(kz_json:object()) -> pos_integer() | 'no_limit'.
get_query_limit(JObj) ->
    get_query_limit(JObj, ?STATS_QUERY_LIMITS_ENABLED).

-spec get_query_limit(kz_json:object(), boolean()) -> pos_integer() | 'no_limit'.
get_query_limit(JObj, 'true') ->
    Max = ?MAX_RESULT_SET,
    case kz_json:get_integer_value(<<"Limit">>, JObj) of
        'undefined' -> Max;
        N when N > Max -> Max;
        N when N < 1 -> 1;
        N -> N
    end;
get_query_limit(JObj, 'false') ->
    case kz_json:get_integer_value(<<"Limit">>, JObj) of
        'undefined' -> 'no_limit';
        N when N < 1 -> 1;
        N -> N
    end.

-spec db_name(kz_term:ne_binary()) -> kz_term:ne_binary().
db_name(Account) ->
    kz_util:format_account_mod_id(Account).
