%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_user_auth_recovery).

-export([new/0]).
-export([account_name/1, account_name/2, set_account_name/2]).
-export([account_realm/1, account_realm/2, set_account_realm/2]).
-export([phone_number/1, phone_number/2, set_phone_number/2]).
-export([ui_url/1, ui_url/2, set_ui_url/2]).
-export([username/1, username/2, set_username/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"user_auth_recovery">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec account_name(doc()) -> kz_term:api_ne_binary().
account_name(Doc) ->
    account_name(Doc, 'undefined').

-spec account_name(doc(), Default) -> kz_term:ne_binary() | Default.
account_name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"account_name">>], Doc, Default).

-spec set_account_name(doc(), kz_term:ne_binary()) -> doc().
set_account_name(Doc, AccountName) ->
    kz_json:set_value([<<"account_name">>], AccountName, Doc).

-spec account_realm(doc()) -> kz_term:api_ne_binary().
account_realm(Doc) ->
    account_realm(Doc, 'undefined').

-spec account_realm(doc(), Default) -> kz_term:ne_binary() | Default.
account_realm(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"account_realm">>], Doc, Default).

-spec set_account_realm(doc(), kz_term:ne_binary()) -> doc().
set_account_realm(Doc, AccountRealm) ->
    kz_json:set_value([<<"account_realm">>], AccountRealm, Doc).

-spec phone_number(doc()) -> kz_term:api_ne_binary().
phone_number(Doc) ->
    phone_number(Doc, 'undefined').

-spec phone_number(doc(), Default) -> kz_term:ne_binary() | Default.
phone_number(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"phone_number">>], Doc, Default).

-spec set_phone_number(doc(), kz_term:ne_binary()) -> doc().
set_phone_number(Doc, PhoneNumber) ->
    kz_json:set_value([<<"phone_number">>], PhoneNumber, Doc).

-spec ui_url(doc()) -> kz_term:api_ne_binary().
ui_url(Doc) ->
    ui_url(Doc, 'undefined').

-spec ui_url(doc(), Default) -> kz_term:ne_binary() | Default.
ui_url(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"ui_url">>], Doc, Default).

-spec set_ui_url(doc(), kz_term:ne_binary()) -> doc().
set_ui_url(Doc, UiUrl) ->
    kz_json:set_value([<<"ui_url">>], UiUrl, Doc).

-spec username(doc()) -> kz_term:api_ne_binary().
username(Doc) ->
    username(Doc, 'undefined').

-spec username(doc(), Default) -> kz_term:ne_binary() | Default.
username(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"username">>], Doc, Default).

-spec set_username(doc(), kz_term:ne_binary()) -> doc().
set_username(Doc, Username) ->
    kz_json:set_value([<<"username">>], Username, Doc).
