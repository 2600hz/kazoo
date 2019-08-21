%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_account_rate_limits).

-export([new/0]).
-export([account/1, account/2, set_account/2]).
-export([device/1, device/2, set_device/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"account_rate_limits">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec account(doc()) -> kz_term:api_object().
account(Doc) ->
    account(Doc, 'undefined').

-spec account(doc(), Default) -> kz_json:object() | Default.
account(Doc, Default) ->
    kz_json:get_json_value([<<"account">>], Doc, Default).

-spec set_account(doc(), kz_json:object()) -> doc().
set_account(Doc, Account) ->
    kz_json:set_value([<<"account">>], Account, Doc).

-spec device(doc()) -> kz_term:api_object().
device(Doc) ->
    device(Doc, 'undefined').

-spec device(doc(), Default) -> kz_json:object() | Default.
device(Doc, Default) ->
    kz_json:get_json_value([<<"device">>], Doc, Default).

-spec set_device(doc(), kz_json:object()) -> doc().
set_device(Doc, Device) ->
    kz_json:set_value([<<"device">>], Device, Doc).
