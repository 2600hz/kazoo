%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_ips).

-export([new/0]).
-export([ips/1, ips/2, set_ips/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"ips">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec ips(doc()) -> kz_term:api_ne_binaries().
ips(Doc) ->
    ips(Doc, 'undefined').

-spec ips(doc(), Default) -> kz_term:ne_binaries() | Default.
ips(Doc, Default) ->
    kz_json:get_list_value([<<"ips">>], Doc, Default).

-spec set_ips(doc(), kz_term:ne_binaries()) -> doc().
set_ips(Doc, Ips) ->
    kz_json:set_value([<<"ips">>], Ips, Doc).
