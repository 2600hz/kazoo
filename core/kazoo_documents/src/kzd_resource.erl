%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_resource).

-export([new/0
        ,type/0, type/1
        ]).

-export([flat_rate_whitelist/1, flat_rate_whitelist/2
        ,flat_rate_blacklist/1, flat_rate_blacklist/2
        ]).

-include("kz_documents.hrl").

-define(PVT_TYPE, <<"resource">>).
-define(SCHEMA, <<"resources">>).

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_doc:set_type(kz_json_schema:default_object(?SCHEMA), type()).

-spec type() -> kz_term:ne_binary().
type() -> ?PVT_TYPE.

-spec type(doc()) -> kz_term:ne_binary().
type(Doc) ->
    kz_doc:type(Doc, ?PVT_TYPE).

-spec flat_rate_whitelist(doc()) -> kz_term:api_ne_binary().
flat_rate_whitelist(Doc) ->
    flat_rate_whitelist(Doc, 'undefined').

-spec flat_rate_whitelist(doc(), Default) -> kz_term:ne_binary() | Default.
flat_rate_whitelist(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"flat_rate_whitelist">>, Doc, Default).

-spec flat_rate_blacklist(doc()) -> kz_term:api_ne_binary().
flat_rate_blacklist(Doc) ->
    flat_rate_blacklist(Doc, 'undefined').

-spec flat_rate_blacklist(doc(), Default) -> kz_term:ne_binary() | Default.
flat_rate_blacklist(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"flat_rate_blacklist">>, Doc, Default).
