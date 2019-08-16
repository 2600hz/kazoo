%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_dialplans).

-export([new/0]).
-export([system/1, system/2, set_system/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"dialplans">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec system(doc()) -> kz_term:api_ne_binaries().
system(Doc) ->
    system(Doc, 'undefined').

-spec system(doc(), Default) -> kz_term:ne_binaries() | Default.
system(Doc, Default) ->
    kz_json:get_list_value([<<"system">>], Doc, Default).

-spec set_system(doc(), kz_term:ne_binaries()) -> doc().
set_system(Doc, System) ->
    kz_json:set_value([<<"system">>], System, Doc).
