%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_bookkeepers).

-export([new/0]).
-export([braintree/1, braintree/2, set_braintree/2]).
-export([local/1, local/2, set_local/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"bookkeepers">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec braintree(doc()) -> kz_term:api_object().
braintree(Doc) ->
    braintree(Doc, 'undefined').

-spec braintree(doc(), Default) -> kz_json:object() | Default.
braintree(Doc, Default) ->
    kz_json:get_json_value([<<"braintree">>], Doc, Default).

-spec set_braintree(doc(), kz_json:object()) -> doc().
set_braintree(Doc, Braintree) ->
    kz_json:set_value([<<"braintree">>], Braintree, Doc).

-spec local(doc()) -> kz_term:api_object().
local(Doc) ->
    local(Doc, 'undefined').

-spec local(doc(), Default) -> kz_json:object() | Default.
local(Doc, Default) ->
    kz_json:get_json_value([<<"local">>], Doc, Default).

-spec set_local(doc(), kz_json:object()) -> doc().
set_local(Doc, Local) ->
    kz_json:set_value([<<"local">>], Local, Doc).
