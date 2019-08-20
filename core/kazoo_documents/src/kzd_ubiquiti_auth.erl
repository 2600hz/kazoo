%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_ubiquiti_auth).

-export([new/0]).
-export([password/1, password/2, set_password/2]).
-export([username/1, username/2, set_username/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"ubiquiti_auth">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec password(doc()) -> kz_term:api_ne_binary().
password(Doc) ->
    password(Doc, 'undefined').

-spec password(doc(), Default) -> kz_term:ne_binary() | Default.
password(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"password">>], Doc, Default).

-spec set_password(doc(), kz_term:ne_binary()) -> doc().
set_password(Doc, Password) ->
    kz_json:set_value([<<"password">>], Password, Doc).

-spec username(doc()) -> kz_term:api_ne_binary().
username(Doc) ->
    username(Doc, 'undefined').

-spec username(doc(), Default) -> kz_term:ne_binary() | Default.
username(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"username">>], Doc, Default).

-spec set_username(doc(), kz_term:ne_binary()) -> doc().
set_username(Doc, Username) ->
    kz_json:set_value([<<"username">>], Username, Doc).
