%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_audit_logs).

-export([new/0]).
-export([audit/1, audit/2, set_audit/2]).
-export([account_id/2, account_id/3, set_account_id/3]).
-export([authenticating_user/1, authenticating_user/2, set_authenticating_user/2]).
-export([authenticating_user_account_id/1, authenticating_user_account_id/2, set_authenticating_user_account_id/2]).
-export([authenticating_user_account_name/1, authenticating_user_account_name/2, set_authenticating_user_account_name/2]).
-export([authenticating_user_first_name/1, authenticating_user_first_name/2, set_authenticating_user_first_name/2]).
-export([authenticating_user_last_name/1, authenticating_user_last_name/2, set_authenticating_user_last_name/2]).
-export([authenticating_user_user_id/1, authenticating_user_user_id/2, set_authenticating_user_user_id/2]).
-export([authenticating_user_id/1, authenticating_user_id/2, set_authenticating_user_id/2]).
-export([tree/1, tree/2, set_tree/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"audit_logs">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec audit(doc()) -> kz_term:api_object().
audit(Doc) ->
    audit(Doc, 'undefined').

-spec audit(doc(), Default) -> kz_json:object() | Default.
audit(Doc, Default) ->
    kz_json:get_json_value([<<"audit">>], Doc, Default).

-spec set_audit(doc(), kz_json:object()) -> doc().
set_audit(Doc, Audit) ->
    kz_json:set_value([<<"audit">>], Audit, Doc).

-spec account_id(doc(), kz_json:key()) -> kz_term:api_object().
account_id(Doc, AccountId) ->
    account_id(Doc, AccountId, 'undefined').

-spec account_id(doc(), kz_json:key(), Default) -> kz_json:object() | Default.
account_id(Doc, AccountId, Default) ->
    kz_json:get_json_value([<<"audit">>, AccountId], Doc, Default).

-spec set_account_id(doc(), kz_json:key(), kz_json:object()) -> doc().
set_account_id(Doc, AccountId, Value) ->
    kz_json:set_value([<<"audit">>, AccountId], Value, Doc).

-spec authenticating_user(doc()) -> kz_term:api_object().
authenticating_user(Doc) ->
    authenticating_user(Doc, 'undefined').

-spec authenticating_user(doc(), Default) -> kz_json:object() | Default.
authenticating_user(Doc, Default) ->
    kz_json:get_json_value([<<"authenticating_user">>], Doc, Default).

-spec set_authenticating_user(doc(), kz_json:object()) -> doc().
set_authenticating_user(Doc, AuthenticatingUser) ->
    kz_json:set_value([<<"authenticating_user">>], AuthenticatingUser, Doc).

-spec authenticating_user_account_id(doc()) -> kz_term:api_binary().
authenticating_user_account_id(Doc) ->
    authenticating_user_account_id(Doc, 'undefined').

-spec authenticating_user_account_id(doc(), Default) -> binary() | Default.
authenticating_user_account_id(Doc, Default) ->
    kz_json:get_binary_value([<<"authenticating_user">>, <<"account_id">>], Doc, Default).

-spec set_authenticating_user_account_id(doc(), binary()) -> doc().
set_authenticating_user_account_id(Doc, AuthenticatingUserAccountId) ->
    kz_json:set_value([<<"authenticating_user">>, <<"account_id">>], AuthenticatingUserAccountId, Doc).

-spec authenticating_user_account_name(doc()) -> kz_term:api_binary().
authenticating_user_account_name(Doc) ->
    authenticating_user_account_name(Doc, 'undefined').

-spec authenticating_user_account_name(doc(), Default) -> binary() | Default.
authenticating_user_account_name(Doc, Default) ->
    kz_json:get_binary_value([<<"authenticating_user">>, <<"account_name">>], Doc, Default).

-spec set_authenticating_user_account_name(doc(), binary()) -> doc().
set_authenticating_user_account_name(Doc, AuthenticatingUserAccountName) ->
    kz_json:set_value([<<"authenticating_user">>, <<"account_name">>], AuthenticatingUserAccountName, Doc).

-spec authenticating_user_first_name(doc()) -> kz_term:api_binary().
authenticating_user_first_name(Doc) ->
    authenticating_user_first_name(Doc, 'undefined').

-spec authenticating_user_first_name(doc(), Default) -> binary() | Default.
authenticating_user_first_name(Doc, Default) ->
    kz_json:get_binary_value([<<"authenticating_user">>, <<"first_name">>], Doc, Default).

-spec set_authenticating_user_first_name(doc(), binary()) -> doc().
set_authenticating_user_first_name(Doc, AuthenticatingUserFirstName) ->
    kz_json:set_value([<<"authenticating_user">>, <<"first_name">>], AuthenticatingUserFirstName, Doc).

-spec authenticating_user_last_name(doc()) -> kz_term:api_binary().
authenticating_user_last_name(Doc) ->
    authenticating_user_last_name(Doc, 'undefined').

-spec authenticating_user_last_name(doc(), Default) -> binary() | Default.
authenticating_user_last_name(Doc, Default) ->
    kz_json:get_binary_value([<<"authenticating_user">>, <<"last_name">>], Doc, Default).

-spec set_authenticating_user_last_name(doc(), binary()) -> doc().
set_authenticating_user_last_name(Doc, AuthenticatingUserLastName) ->
    kz_json:set_value([<<"authenticating_user">>, <<"last_name">>], AuthenticatingUserLastName, Doc).

-spec authenticating_user_user_id(doc()) -> kz_term:api_binary().
authenticating_user_user_id(Doc) ->
    authenticating_user_user_id(Doc, 'undefined').

-spec authenticating_user_user_id(doc(), Default) -> binary() | Default.
authenticating_user_user_id(Doc, Default) ->
    kz_json:get_binary_value([<<"authenticating_user">>, <<"user_id">>], Doc, Default).

-spec set_authenticating_user_user_id(doc(), binary()) -> doc().
set_authenticating_user_user_id(Doc, AuthenticatingUserUserId) ->
    kz_json:set_value([<<"authenticating_user">>, <<"user_id">>], AuthenticatingUserUserId, Doc).

-spec tree(doc()) -> kz_term:api_ne_binaries().
tree(Doc) ->
    tree(Doc, 'undefined').

-spec tree(doc(), Default) -> kz_term:ne_binaries() | Default.
tree(Doc, Default) ->
    kz_json:get_list_value([<<"tree">>], Doc, Default).

-spec set_tree(doc(), kz_term:ne_binaries()) -> doc().
set_tree(Doc, Tree) ->
    kz_json:set_value([<<"tree">>], Tree, Doc).

-spec authenticating_user_id(doc()) -> kz_term:api_binary().
authenticating_user_id(Doc) ->
    authenticating_user_id(Doc, 'undefined').

-spec authenticating_user_id(doc(), Default) -> binary() | Default.
authenticating_user_id(Doc, Default) ->
    kz_json:get_binary_value([<<"authenticating_user">>, <<"auth_user_id">>], Doc, Default).

-spec set_authenticating_user_id(doc(), binary()) -> doc().
set_authenticating_user_id(Doc, AuthenticatingUserUserId) ->
    kz_json:set_value([<<"authenticating_user">>, <<"auth_user_id">>], AuthenticatingUserUserId, Doc).
