-ifndef(WHISTLE_TYPES_INCLUDED).

-type proplist() :: list( tuple(binary() | atom(), term()) ) | [].

%% for setting types on dicts
-type dict(K,V) :: [{K, V}].

%% -type iolist() :: [char() | binary() | iolist()].
%% -type iodata() :: iolist() | binary().

-define(EMPTY_JSON_OBJECT, {struct, []}).

-type json_string() :: atom() | binary().
-type json_number() :: integer() | float().
-type json_array() :: [json_term()].
-type json_object() :: {struct, [{json_string(), json_term()}]}.
-type json_iolist() :: {json, iolist()}.
-type json_term() :: json_string() | json_number() | json_array() | json_object() | json_iolist().
-type json_objects() :: [json_object()] | [].
-type mochijson() :: json_object() | json_objects() | json_term() | [].

-type wh_year() :: non_neg_integer().
-type wh_month() :: 1..12.
-type wh_day() :: 1..31.
-type wh_hour() :: 0..23.
-type wh_minute() :: 0..59.
-type wh_second() :: 0..59.
-type wh_daynum() :: 1..7.
-type wh_weeknum() :: 1..53.
-type wh_date() :: tuple(wh_year(), wh_month(), wh_day()).
-type wh_time() :: tuple(wh_hour(), wh_minute(), wh_second()).
-type wh_datetime() :: tuple(wh_date(), wh_time()).
-type wh_iso_week() :: tuple(wh_year(), wh_weeknum()).

-define(WHISTLE_TYPES_INCLUDED, true).
-endif.
