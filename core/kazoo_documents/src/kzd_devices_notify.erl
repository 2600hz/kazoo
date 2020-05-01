%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_devices_notify).

-export([new/0]).
-export([data/1, data/2, set_data/2]).
-export([data_body/1, data_body/2, set_data_body/2]).
-export([data_body_content_type/1, data_body_content_type/2, set_data_body_content_type/2]).
-export([data_body_data/1, data_body_data/2, set_data_body_data/2]).
-export([data_event/1, data_event/2, set_data_event/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"devices_notify">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec data(doc()) -> kz_term:api_object().
data(Doc) ->
    data(Doc, 'undefined').

-spec data(doc(), Default) -> kz_json:object() | Default.
data(Doc, Default) ->
    kz_json:get_json_value([<<"data">>], Doc, Default).

-spec set_data(doc(), kz_json:object()) -> doc().
set_data(Doc, Data) ->
    kz_json:set_value([<<"data">>], Data, Doc).

-spec data_body(doc()) -> kz_term:api_object().
data_body(Doc) ->
    data_body(Doc, 'undefined').

-spec data_body(doc(), Default) -> kz_json:object() | Default.
data_body(Doc, Default) ->
    kz_json:get_json_value([<<"data">>, <<"body">>], Doc, Default).

-spec set_data_body(doc(), kz_json:object()) -> doc().
set_data_body(Doc, DataBody) ->
    kz_json:set_value([<<"data">>, <<"body">>], DataBody, Doc).

-spec data_body_content_type(doc()) -> kz_term:api_binary().
data_body_content_type(Doc) ->
    data_body_content_type(Doc, 'undefined').

-spec data_body_content_type(doc(), Default) -> binary() | Default.
data_body_content_type(Doc, Default) ->
    kz_json:get_binary_value([<<"data">>, <<"body">>, <<"content_type">>], Doc, Default).

-spec set_data_body_content_type(doc(), binary()) -> doc().
set_data_body_content_type(Doc, DataBodyContentType) ->
    kz_json:set_value([<<"data">>, <<"body">>, <<"content_type">>], DataBodyContentType, Doc).

-spec data_body_data(doc()) -> kz_term:api_binary().
data_body_data(Doc) ->
    data_body_data(Doc, 'undefined').

-spec data_body_data(doc(), Default) -> binary() | Default.
data_body_data(Doc, Default) ->
    kz_json:get_binary_value([<<"data">>, <<"body">>, <<"data">>], Doc, Default).

-spec set_data_body_data(doc(), binary()) -> doc().
set_data_body_data(Doc, DataBodyData) ->
    kz_json:set_value([<<"data">>, <<"body">>, <<"data">>], DataBodyData, Doc).

-spec data_event(doc()) -> kz_term:api_binary().
data_event(Doc) ->
    data_event(Doc, 'undefined').

-spec data_event(doc(), Default) -> binary() | Default.
data_event(Doc, Default) ->
    kz_json:get_binary_value([<<"data">>, <<"event">>], Doc, Default).

-spec set_data_event(doc(), binary()) -> doc().
set_data_event(Doc, DataEvent) ->
    kz_json:set_value([<<"data">>, <<"event">>], DataEvent, Doc).
