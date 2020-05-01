%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_resource_jobs).

-export([new/0]).
-export([name/1, name/2, set_name/2]).
-export([numbers/1, numbers/2, set_numbers/2]).
-export([resource_id/1, resource_id/2, set_resource_id/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"resource_jobs">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec name(doc()) -> kz_term:api_ne_binary().
name(Doc) ->
    name(Doc, 'undefined').

-spec name(doc(), Default) -> kz_term:ne_binary() | Default.
name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), kz_term:ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

-spec numbers(doc()) -> kz_term:api_ne_binaries().
numbers(Doc) ->
    numbers(Doc, 'undefined').

-spec numbers(doc(), Default) -> kz_term:ne_binaries() | Default.
numbers(Doc, Default) ->
    kz_json:get_list_value([<<"numbers">>], Doc, Default).

-spec set_numbers(doc(), kz_term:ne_binaries()) -> doc().
set_numbers(Doc, Numbers) ->
    kz_json:set_value([<<"numbers">>], Numbers, Doc).

-spec resource_id(doc()) -> kz_term:api_binary().
resource_id(Doc) ->
    resource_id(Doc, 'undefined').

-spec resource_id(doc(), Default) -> binary() | Default.
resource_id(Doc, Default) ->
    kz_json:get_binary_value([<<"resource_id">>], Doc, Default).

-spec set_resource_id(doc(), binary()) -> doc().
set_resource_id(Doc, ResourceId) ->
    kz_json:set_value([<<"resource_id">>], ResourceId, Doc).
