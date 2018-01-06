-module(kzd_device_rate_limits).

-export([new/0]).
-export([per_minute/1, per_minute/2, set_per_minute/2]).
-export([per_second/1, per_second/2, set_per_second/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec per_minute(doc()) -> api_object().
-spec per_minute(doc(), Default) -> kz_json:object() | Default.
per_minute(Doc) ->
    per_minute(Doc, 'undefined').
per_minute(Doc, Default) ->
    kz_json:get_json_value(<<"per_minute">>, Doc, Default).

-spec set_per_minute(doc(), kz_json:object()) -> doc().
set_per_minute(Doc, PerMinute) ->
    kz_json:set_value(<<"per_minute">>, PerMinute, Doc).

-spec per_second(doc()) -> api_object().
-spec per_second(doc(), Default) -> kz_json:object() | Default.
per_second(Doc) ->
    per_second(Doc, 'undefined').
per_second(Doc, Default) ->
    kz_json:get_json_value(<<"per_second">>, Doc, Default).

-spec set_per_second(doc(), kz_json:object()) -> doc().
set_per_second(Doc, PerSecond) ->
    kz_json:set_value(<<"per_second">>, PerSecond, Doc).
