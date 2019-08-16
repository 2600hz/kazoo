%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_user_auth_recovery_reset).

-export([new/0]).
-export([reset_id/1, reset_id/2, set_reset_id/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"user_auth_recovery_reset">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec reset_id(doc()) -> kz_term:api_binary().
reset_id(Doc) ->
    reset_id(Doc, 'undefined').

-spec reset_id(doc(), Default) -> binary() | Default.
reset_id(Doc, Default) ->
    kz_json:get_binary_value([<<"reset_id">>], Doc, Default).

-spec set_reset_id(doc(), binary()) -> doc().
set_reset_id(Doc, ResetId) ->
    kz_json:set_value([<<"reset_id">>], ResetId, Doc).
