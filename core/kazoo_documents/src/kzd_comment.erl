%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Accessors for `comment' document.
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
    action_required(Doc, false).

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
    is_private(Doc, false).

-spec is_private(doc(), Default) -> boolean() | Default.
is_private(Doc, Default) ->
    kz_json:get_boolean_value([<<"is_private">>], Doc, Default).

-spec set_is_private(doc(), boolean()) -> doc().
set_is_private(Doc, IsPrivate) ->
    kz_json:set_value([<<"is_private">>], IsPrivate, Doc).

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
