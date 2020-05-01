%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_tasks).

-export([new/0]).
-export([file_name/1, file_name/2, set_file_name/2]).
-export([records/1, records/2, set_records/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"tasks">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec file_name(doc()) -> kz_term:api_binary().
file_name(Doc) ->
    file_name(Doc, 'undefined').

-spec file_name(doc(), Default) -> binary() | Default.
file_name(Doc, Default) ->
    kz_json:get_binary_value([<<"file_name">>], Doc, Default).

-spec set_file_name(doc(), binary()) -> doc().
set_file_name(Doc, FileName) ->
    kz_json:set_value([<<"file_name">>], FileName, Doc).

-spec records(doc()) -> kz_term:api_objects().
records(Doc) ->
    records(Doc, 'undefined').

-spec records(doc(), Default) -> kz_json:objects() | Default.
records(Doc, Default) ->
    kz_json:get_list_value([<<"records">>], Doc, Default).

-spec set_records(doc(), kz_json:objects()) -> doc().
set_records(Doc, Records) ->
    kz_json:set_value([<<"records">>], Records, Doc).
