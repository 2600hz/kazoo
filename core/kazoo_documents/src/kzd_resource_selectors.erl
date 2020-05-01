%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_resource_selectors).

-export([new/0]).
-export([name/1, name/2, set_name/2]).
-export([resource/1, resource/2, set_resource/2]).
-export([selector/1, selector/2, set_selector/2]).
-export([start_time/1, start_time/2, set_start_time/2]).
-export([stop_time/1, stop_time/2, set_stop_time/2]).
-export([value/1, value/2, set_value/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"resource_selectors">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec name(doc()) -> kz_term:api_binary().
name(Doc) ->
    name(Doc, 'undefined').

-spec name(doc(), Default) -> binary() | Default.
name(Doc, Default) ->
    kz_json:get_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

-spec resource(doc()) -> kz_term:api_binary().
resource(Doc) ->
    resource(Doc, 'undefined').

-spec resource(doc(), Default) -> binary() | Default.
resource(Doc, Default) ->
    kz_json:get_binary_value([<<"resource">>], Doc, Default).

-spec set_resource(doc(), binary()) -> doc().
set_resource(Doc, Resource) ->
    kz_json:set_value([<<"resource">>], Resource, Doc).

-spec selector(doc()) -> kz_term:api_binary().
selector(Doc) ->
    selector(Doc, 'undefined').

-spec selector(doc(), Default) -> binary() | Default.
selector(Doc, Default) ->
    kz_json:get_binary_value([<<"selector">>], Doc, Default).

-spec set_selector(doc(), binary()) -> doc().
set_selector(Doc, Selector) ->
    kz_json:set_value([<<"selector">>], Selector, Doc).

-spec start_time(doc()) -> kz_term:api_integer().
start_time(Doc) ->
    start_time(Doc, 'undefined').

-spec start_time(doc(), Default) -> integer() | Default.
start_time(Doc, Default) ->
    kz_json:get_integer_value([<<"start_time">>], Doc, Default).

-spec set_start_time(doc(), integer()) -> doc().
set_start_time(Doc, StartTime) ->
    kz_json:set_value([<<"start_time">>], StartTime, Doc).

-spec stop_time(doc()) -> kz_term:api_integer().
stop_time(Doc) ->
    stop_time(Doc, 'undefined').

-spec stop_time(doc(), Default) -> integer() | Default.
stop_time(Doc, Default) ->
    kz_json:get_integer_value([<<"stop_time">>], Doc, Default).

-spec set_stop_time(doc(), integer()) -> doc().
set_stop_time(Doc, StopTime) ->
    kz_json:set_value([<<"stop_time">>], StopTime, Doc).

-spec value(doc()) -> kz_term:api_binary().
value(Doc) ->
    value(Doc, 'undefined').

-spec value(doc(), Default) -> binary() | Default.
value(Doc, Default) ->
    kz_json:get_binary_value([<<"value">>], Doc, Default).

-spec set_value(doc(), binary()) -> doc().
set_value(Doc, Value) ->
    kz_json:set_value([<<"value">>], Value, Doc).
