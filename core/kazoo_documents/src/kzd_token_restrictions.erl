%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_token_restrictions).

-export([new/0]).
-export([restrictions/1, restrictions/2, set_restrictions/2]).
-export([restriction/2, restriction/3, set_restriction/3]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"token_restrictions">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec restrictions(doc()) -> kz_term:api_object().
restrictions(Doc) ->
    restrictions(Doc, 'undefined').

-spec restrictions(doc(), Default) -> kz_json:object() | Default.
restrictions(Doc, Default) ->
    kz_json:get_json_value([<<"restrictions">>], Doc, Default).

-spec set_restrictions(doc(), kz_json:object()) -> doc().
set_restrictions(Doc, Restrictions) ->
    kz_json:set_value([<<"restrictions">>], Restrictions, Doc).

-spec restriction(doc(), kz_json:key()) -> kz_term:api_object().
restriction(Doc, Restriction) ->
    restriction(Doc, Restriction, 'undefined').

-spec restriction(doc(), kz_json:key(), Default) -> kz_json:object() | Default.
restriction(Doc, Restriction, Default) ->
    kz_json:get_json_value([<<"restrictions">>, Restriction], Doc, Default).

-spec set_restriction(doc(), kz_json:key(), kz_json:object()) -> doc().
set_restriction(Doc, Restriction, Value) ->
    kz_json:set_value([<<"restrictions">>, Restriction], Value, Doc).
