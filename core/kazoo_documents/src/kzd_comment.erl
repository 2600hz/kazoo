%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Accessors for `comment' document.
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_comment).

-export([new/0]).
-export([account_id/1, account_id/2, set_account_id/2]).
-export([action_required/1, action_required/2, set_action_required/2]).
-export([author/1, author/2, set_author/2]).
-export([content/1, content/2, set_content/2]).
-export([is_private/1, is_private/2, set_is_private/2]).
-export([timestamp/1, timestamp/2, set_timestamp/2]).
-export([user_id/1, user_id/2, set_user_id/2]).

-export([account_id_path/0]).
-export([action_required_path/0]).
-export([author_path/0]).
-export([content_path/0]).
-export([is_private_comment_paths/0]).
-export([is_private_path/0]).
-export([superduper_comment_path/0]).
-export([timestamp_path/0]).
-export([user_id_path/0]).

-export([is_private_legacy/1]).
-export([is_private_legacy/2]).
-export([migrate_to_is_private/1]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"comment">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec account_id(doc()) -> kz_term:api_binary().
account_id(Doc) ->
    account_id(Doc, 'undefined').

-spec account_id(doc(), Default) -> binary() | Default.
account_id(Doc, Default) ->
    kz_json:get_binary_value([<<"account_id">>], Doc, Default).

-spec set_account_id(doc(), binary()) -> doc().
set_account_id(Doc, AccountId) ->
    kz_json:set_value([<<"account_id">>], AccountId, Doc).

-spec action_required(doc()) -> boolean().
action_required(Doc) ->
    action_required(Doc, 'false').

-spec action_required(doc(), Default) -> boolean() | Default.
action_required(Doc, Default) ->
    kz_json:get_boolean_value([<<"action_required">>], Doc, Default).

-spec set_action_required(doc(), boolean()) -> doc().
set_action_required(Doc, ActionRequired) ->
    kz_json:set_value([<<"action_required">>], ActionRequired, Doc).

-spec author(doc()) -> kz_term:api_binary().
author(Doc) ->
    author(Doc, 'undefined').

-spec author(doc(), Default) -> binary() | Default.
author(Doc, Default) ->
    kz_json:get_binary_value([<<"author">>], Doc, Default).

-spec set_author(doc(), binary()) -> doc().
set_author(Doc, Author) ->
    kz_json:set_value([<<"author">>], Author, Doc).

-spec content(doc()) -> kz_term:api_binary().
content(Doc) ->
    content(Doc, 'undefined').

-spec content(doc(), Default) -> binary() | Default.
content(Doc, Default) ->
    kz_json:get_binary_value([<<"content">>], Doc, Default).

-spec set_content(doc(), binary()) -> doc().
set_content(Doc, Content) ->
    kz_json:set_value([<<"content">>], Content, Doc).

-spec is_private(doc()) -> boolean().
is_private(Doc) ->
    is_private(Doc, 'false').

-spec is_private(doc(), Default) -> boolean() | Default.
is_private(Doc, Default) ->
    is_private_legacy(Doc, Default).

-spec set_is_private(doc(), boolean()) -> doc().
set_is_private(Doc, IsPrivate) ->
    kz_json:set_value(is_private_path(), IsPrivate, Doc).

-spec timestamp(doc()) -> kz_term:api_integer().
timestamp(Doc) ->
    timestamp(Doc, 'undefined').

-spec timestamp(doc(), Default) -> integer() | Default.
timestamp(Doc, Default) ->
    kz_json:get_integer_value([<<"timestamp">>], Doc, Default).

-spec set_timestamp(doc(), integer()) -> doc().
set_timestamp(Doc, Timestamp) ->
    kz_json:set_value([<<"timestamp">>], Timestamp, Doc).

-spec user_id(doc()) -> kz_term:api_binary().
user_id(Doc) ->
    user_id(Doc, 'undefined').

-spec user_id(doc(), Default) -> binary() | Default.
user_id(Doc, Default) ->
    kz_json:get_binary_value([<<"user_id">>], Doc, Default).

-spec set_user_id(doc(), binary()) -> doc().
set_user_id(Doc, UserId) ->
    kz_json:set_value([<<"user_id">>], UserId, Doc).

-spec account_id_path() -> kz_json:path().
account_id_path() ->
    [<<"account_id">>].

-spec action_required_path() -> kz_json:path().
action_required_path() ->
    [<<"action_required">>].

-spec author_path() -> kz_json:path().
author_path() ->
    [<<"author">>].

-spec content_path() -> kz_json:path().
content_path() ->
    [<<"content">>].

-spec is_private_comment_paths() -> kz_json:paths().
is_private_comment_paths() ->
    [superduper_comment_path(), is_private_path()].

-spec is_private_path() -> kz_json:path().
is_private_path() ->
    [<<"is_private">>].

-spec superduper_comment_path() -> kz_json:path().
superduper_comment_path() ->
    [<<"superduper_comment">>].

-spec timestamp_path() -> kz_json:path().
timestamp_path() ->
    [<<"timestamp">>].

-spec user_id_path() -> kz_json:path().
user_id_path() ->
    [<<"user_id">>].

-spec is_private_legacy(doc()) -> boolean().
is_private_legacy(Doc) ->
    is_private_legacy(Doc, 'false').

-spec is_private_legacy(doc(), Default) -> boolean() | Default.
is_private_legacy(Doc, Default) ->
    kz_term:is_true(
      kz_json:get_first_defined(is_private_comment_paths(), Doc, Default)
     ).

-spec migrate_to_is_private(doc()) -> doc().
migrate_to_is_private(Doc) ->
    set_is_private(kz_json:delete_key(superduper_comment_path(), Doc)
                  ,is_private_legacy(Doc)
                  ).
