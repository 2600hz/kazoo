%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_lists).

-export([new/0]).
-export([description/1, description/2, set_description/2]).
-export([name/1, name/2, set_name/2]).
-export([org/1, org/2, set_org/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"lists">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec description(doc()) -> kz_term:api_ne_binary().
description(Doc) ->
    description(Doc, 'undefined').

-spec description(doc(), Default) -> kz_term:ne_binary() | Default.
description(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"description">>], Doc, Default).

-spec set_description(doc(), kz_term:ne_binary()) -> doc().
set_description(Doc, Description) ->
    kz_json:set_value([<<"description">>], Description, Doc).

-spec name(doc()) -> kz_term:api_ne_binary().
name(Doc) ->
    name(Doc, 'undefined').

-spec name(doc(), Default) -> kz_term:ne_binary() | Default.
name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), kz_term:ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

-spec org(doc()) -> kz_term:api_binary().
org(Doc) ->
    org(Doc, 'undefined').

-spec org(doc(), Default) -> binary() | Default.
org(Doc, Default) ->
    kz_json:get_binary_value([<<"org">>], Doc, Default).

-spec set_org(doc(), binary()) -> doc().
set_org(Doc, Org) ->
    kz_json:set_value([<<"org">>], Org, Doc).
