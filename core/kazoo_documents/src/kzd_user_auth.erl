%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_user_auth).

-export([new/0]).
-export([account_name/1, account_name/2, set_account_name/2]).
-export([account_realm/1, account_realm/2, set_account_realm/2]).
-export([credentials/1, credentials/2, set_credentials/2]).
-export([method/1, method/2, set_method/2]).
-export([phone_number/1, phone_number/2, set_phone_number/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"user_auth">>).

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

-spec credentials(doc()) -> kz_term:api_ne_binary().
credentials(Doc) ->
    credentials(Doc, 'undefined').

-spec credentials(doc(), Default) -> kz_term:ne_binary() | Default.
credentials(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"credentials">>], Doc, Default).

-spec set_credentials(doc(), kz_term:ne_binary()) -> doc().
set_credentials(Doc, Credentials) ->
    kz_json:set_value([<<"credentials">>], Credentials, Doc).

-spec method(doc()) -> binary().
method(Doc) ->
    method(Doc, <<"md5">>).

-spec method(doc(), Default) -> binary() | Default.
method(Doc, Default) ->
    kz_json:get_binary_value([<<"method">>], Doc, Default).

-spec set_method(doc(), binary()) -> doc().
set_method(Doc, Method) ->
    kz_json:set_value([<<"method">>], Method, Doc).

-spec phone_number(doc()) -> kz_term:api_ne_binary().
phone_number(Doc) ->
    phone_number(Doc, 'undefined').

-spec phone_number(doc(), Default) -> kz_term:ne_binary() | Default.
phone_number(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"phone_number">>], Doc, Default).

-spec set_phone_number(doc(), kz_term:ne_binary()) -> doc().
set_phone_number(Doc, PhoneNumber) ->
    kz_json:set_value([<<"phone_number">>], PhoneNumber, Doc).
