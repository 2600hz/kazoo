%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_acls).

-export([new/0]).
-export([cidr/1, cidr/2, set_cidr/2]).
-export([description/1, description/2, set_description/2]).
-export([network_list_name/1, network_list_name/2, set_network_list_name/2]).
-export([type/1, type/2, set_type/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"acls">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec cidr(doc()) -> kz_term:api_binary().
cidr(Doc) ->
    cidr(Doc, 'undefined').

-spec cidr(doc(), Default) -> binary() | Default.
cidr(Doc, Default) ->
    kz_json:get_binary_value([<<"cidr">>], Doc, Default).

-spec set_cidr(doc(), binary()) -> doc().
set_cidr(Doc, Cidr) ->
    kz_json:set_value([<<"cidr">>], Cidr, Doc).

-spec description(doc()) -> kz_term:api_binary().
description(Doc) ->
    description(Doc, 'undefined').

-spec description(doc(), Default) -> binary() | Default.
description(Doc, Default) ->
    kz_json:get_binary_value([<<"description">>], Doc, Default).

-spec set_description(doc(), binary()) -> doc().
set_description(Doc, Description) ->
    kz_json:set_value([<<"description">>], Description, Doc).

-spec network_list_name(doc()) -> kz_term:api_binary().
network_list_name(Doc) ->
    network_list_name(Doc, 'undefined').

-spec network_list_name(doc(), Default) -> binary() | Default.
network_list_name(Doc, Default) ->
    kz_json:get_binary_value([<<"network_list_name">>], Doc, Default).

-spec set_network_list_name(doc(), binary()) -> doc().
set_network_list_name(Doc, NetworkListName) ->
    kz_json:set_value([<<"network_list_name">>], NetworkListName, Doc).

-spec type(doc()) -> binary().
type(Doc) ->
    type(Doc, <<"allow">>).

-spec type(doc(), Default) -> binary() | Default.
type(Doc, Default) ->
    kz_json:get_binary_value([<<"type">>], Doc, Default).

-spec set_type(doc(), binary()) -> doc().
set_type(Doc, Type) ->
    kz_json:set_value([<<"type">>], Type, Doc).
