%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_call_recording).

-export([new/0]).
-export([any/1, any/2, set_any/2]).
-export([inbound/1, inbound/2, set_inbound/2]).
-export([outbound/1, outbound/2, set_outbound/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"call_recording">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec any(doc()) -> kz_term:api_object().
any(Doc) ->
    any(Doc, 'undefined').

-spec any(doc(), Default) -> kz_json:object() | Default.
any(Doc, Default) ->
    kz_json:get_json_value([<<"any">>], Doc, Default).

-spec set_any(doc(), kz_json:object()) -> doc().
set_any(Doc, Any) ->
    kz_json:set_value([<<"any">>], Any, Doc).

-spec inbound(doc()) -> kz_term:api_object().
inbound(Doc) ->
    inbound(Doc, 'undefined').

-spec inbound(doc(), Default) -> kz_json:object() | Default.
inbound(Doc, Default) ->
    kz_json:get_json_value([<<"inbound">>], Doc, Default).

-spec set_inbound(doc(), kz_json:object()) -> doc().
set_inbound(Doc, Inbound) ->
    kz_json:set_value([<<"inbound">>], Inbound, Doc).

-spec outbound(doc()) -> kz_term:api_object().
outbound(Doc) ->
    outbound(Doc, 'undefined').

-spec outbound(doc(), Default) -> kz_json:object() | Default.
outbound(Doc, Default) ->
    kz_json:get_json_value([<<"outbound">>], Doc, Default).

-spec set_outbound(doc(), kz_json:object()) -> doc().
set_outbound(Doc, Outbound) ->
    kz_json:set_value([<<"outbound">>], Outbound, Doc).
