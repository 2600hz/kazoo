%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_temporal_rules).

-export([new/0]).
-export([cycle/1, cycle/2, set_cycle/2]).
-export([days/1, days/2, set_days/2]).
-export([enabled/1, enabled/2, set_enabled/2]).
-export([interval/1, interval/2, set_interval/2]).
-export([month/1, month/2, set_month/2]).
-export([name/1, name/2, set_name/2]).
-export([ordinal/1, ordinal/2, set_ordinal/2]).
-export([start_date/1, start_date/2, set_start_date/2]).
-export([time_window_start/1, time_window_start/2, set_time_window_start/2]).
-export([time_window_stop/1, time_window_stop/2, set_time_window_stop/2]).
-export([wdays/1, wdays/2, set_wdays/2]).


-export([delete_enabled/1]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"temporal_rules">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec cycle(doc()) -> binary().
cycle(Doc) ->
    cycle(Doc, 'undefined').

-spec cycle(doc(), Default) -> binary() | Default.
cycle(Doc, Default) ->
    kz_json:get_binary_value([<<"cycle">>], Doc, Default).

-spec set_cycle(doc(), binary()) -> doc().
set_cycle(Doc, Cycle) ->
    kz_json:set_value([<<"cycle">>], Cycle, Doc).

-spec days(doc()) -> kz_term:api_integers().
days(Doc) ->
    days(Doc, 'undefined').

-spec days(doc(), Default) -> kz_term:integers() | Default.
days(Doc, Default) ->
    kz_json:get_list_value([<<"days">>], Doc, Default).

-spec set_days(doc(), kz_term:integers()) -> doc().
set_days(Doc, Days) ->
    kz_json:set_value([<<"days">>], Days, Doc).

-spec enabled(doc()) -> kz_term:api_boolean().
enabled(Doc) ->
    enabled(Doc, 'undefined').

-spec enabled(doc(), Default) -> boolean() | Default.
enabled(Doc, Default) ->
    kz_json:get_boolean_value([<<"enabled">>], Doc, Default).

-spec set_enabled(doc(), boolean()) -> doc().
set_enabled(Doc, Enabled) ->
    kz_json:set_value([<<"enabled">>], Enabled, Doc).

-spec delete_enabled(doc()) -> doc().
delete_enabled(Doc) ->
    kz_json:delete_key([<<"enabled">>], Doc).

-spec interval(doc()) -> integer().
interval(Doc) ->
    interval(Doc, 1).

-spec interval(doc(), Default) -> integer() | Default.
interval(Doc, Default) ->
    kz_json:get_integer_value([<<"interval">>], Doc, Default).

-spec set_interval(doc(), integer()) -> doc().
set_interval(Doc, Interval) ->
    kz_json:set_value([<<"interval">>], Interval, Doc).

-spec month(doc()) -> integer().
month(Doc) ->
    month(Doc, 'undefined').

-spec month(doc(), Default) -> integer() | Default.
month(Doc, Default) ->
    kz_json:get_integer_value([<<"month">>], Doc, Default).

-spec set_month(doc(), integer()) -> doc().
set_month(Doc, Month) ->
    kz_json:set_value([<<"month">>], Month, Doc).

-spec name(doc()) -> kz_term:api_ne_binary().
name(Doc) ->
    name(Doc, 'undefined').

-spec name(doc(), Default) -> kz_term:ne_binary() | Default.
name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), kz_term:ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

-spec ordinal(doc()) -> binary().
ordinal(Doc) ->
    ordinal(Doc, 'undefined').

-spec ordinal(doc(), Default) -> binary() | Default.
ordinal(Doc, Default) ->
    kz_json:get_binary_value([<<"ordinal">>], Doc, Default).

-spec set_ordinal(doc(), binary()) -> doc().
set_ordinal(Doc, Ordinal) ->
    kz_json:set_value([<<"ordinal">>], Ordinal, Doc).

-spec start_date(doc()) -> integer().
start_date(Doc) ->
    start_date(Doc, 62586115200).

-spec start_date(doc(), Default) -> integer() | Default.
start_date(Doc, Default) ->
    kz_json:get_integer_value([<<"start_date">>], Doc, Default).

-spec set_start_date(doc(), integer()) -> doc().
set_start_date(Doc, StartDate) ->
    kz_json:set_value([<<"start_date">>], StartDate, Doc).

-spec time_window_start(doc()) -> integer().
time_window_start(Doc) ->
    time_window_start(Doc, 'undefined').

-spec time_window_start(doc(), Default) -> integer() | Default.
time_window_start(Doc, Default) ->
    kz_json:get_integer_value([<<"time_window_start">>], Doc, Default).

-spec set_time_window_start(doc(), integer()) -> doc().
set_time_window_start(Doc, TimeWindowStart) ->
    kz_json:set_value([<<"time_window_start">>], TimeWindowStart, Doc).

-spec time_window_stop(doc()) -> kz_term:api_integer().
time_window_stop(Doc) ->
    time_window_stop(Doc, 'undefined').

-spec time_window_stop(doc(), Default) -> integer() | Default.
time_window_stop(Doc, Default) ->
    kz_json:get_integer_value([<<"time_window_stop">>], Doc, Default).

-spec set_time_window_stop(doc(), integer()) -> doc().
set_time_window_stop(Doc, TimeWindowStop) ->
    kz_json:set_value([<<"time_window_stop">>], TimeWindowStop, Doc).

-spec wdays(doc()) -> kz_term:api_ne_binaries().
wdays(Doc) ->
    wdays(Doc, 'undefined').

-spec wdays(doc(), Default) -> kz_term:ne_binaries() | Default.
wdays(Doc, Default) ->
    kz_json:get_list_value([<<"wdays">>], Doc, Default).

-spec set_wdays(doc(), kz_term:ne_binaries()) -> doc().
set_wdays(Doc, Wdays) ->
    kz_json:set_value([<<"wdays">>], Wdays, Doc).
