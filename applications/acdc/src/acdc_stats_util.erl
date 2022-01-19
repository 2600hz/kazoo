%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc Stat util functions
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_stats_util).

-export([wait_time/2
        ,pause_time/2
        ,caller_id_name/2
        ,caller_id_number/2
        ,queue_id/2

        ,get_query_limit/1
        ,apply_query_window_wiggle_room/2
        ,db_name/1
        ]).

-include("acdc.hrl").
-include("acdc_stats.hrl").

-spec wait_time(kz_term:ne_binary(), kz_json:object()) -> kz_term:api_integer().
wait_time(<<"paused">>, _) -> 'undefined';
wait_time(_, JObj) -> kz_json:get_integer_value(<<"Wait-Time">>, JObj).

-spec pause_time(kz_term:ne_binary(), kz_json:object()) -> timeout() | 'undefined'.
pause_time(<<"paused">>, JObj) ->
    case kz_json:get_value(<<"Timeout">>, JObj) of
        'undefined' -> 'undefined';
        <<"infinity">> -> 'infinity';
        Timeout -> kz_term:to_integer(Timeout)
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

%%------------------------------------------------------------------------------
%% @doc If a query timestamp value is less than the minimum permitted by
%% validation, allow a little wiggle room in case the request just took a little
%% while to be processed.
%% @end
%%------------------------------------------------------------------------------
-spec apply_query_window_wiggle_room(pos_integer(), pos_integer()) -> pos_integer().
apply_query_window_wiggle_room(Timestamp, Minimum) ->
    Offset = Minimum - Timestamp,
    WithinWiggleRoom = Offset < ?QUERY_WINDOW_WIGGLE_ROOM_S,
    case Offset =< 0 of
        'true' -> Timestamp;
        'false' when WithinWiggleRoom -> Minimum;
        'false' -> Timestamp
    end.

-spec db_name(kz_term:ne_binary()) -> kz_term:ne_binary().
db_name(Account) ->
    kz_util:format_account_mod_id(Account).
