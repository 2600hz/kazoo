-module(kzd_cccps).

-export([new/0]).
-export([active/1, active/2, set_active/2]).
-export([cid/1, cid/2, set_cid/2]).
-export([comment/1, comment/2, set_comment/2]).
-export([max_concurent_calls_per_user/1, max_concurent_calls_per_user/2, set_max_concurent_calls_per_user/2]).
-export([pin/1, pin/2, set_pin/2]).
-export([retain_cid/1, retain_cid/2, set_retain_cid/2]).
-export([user_id/1, user_id/2, set_user_id/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec active(doc()) -> boolean().
-spec active(doc(), Default) -> boolean() | Default.
active(Doc) ->
    active(Doc, false).
active(Doc, Default) ->
    kz_json:get_boolean_value(<<"active">>, Doc, Default).

-spec set_active(doc(), boolean()) -> doc().
set_active(Doc, Active) ->
    kz_json:set_value(<<"active">>, Active, Doc).

-spec cid(doc()) -> api_binary().
-spec cid(doc(), Default) -> binary() | Default.
cid(Doc) ->
    cid(Doc, 'undefined').
cid(Doc, Default) ->
    kz_json:get_binary_value(<<"cid">>, Doc, Default).

-spec set_cid(doc(), binary()) -> doc().
set_cid(Doc, Cid) ->
    kz_json:set_value(<<"cid">>, Cid, Doc).

-spec comment(doc()) -> api_binary().
-spec comment(doc(), Default) -> binary() | Default.
comment(Doc) ->
    comment(Doc, 'undefined').
comment(Doc, Default) ->
    kz_json:get_binary_value(<<"comment">>, Doc, Default).

-spec set_comment(doc(), binary()) -> doc().
set_comment(Doc, Comment) ->
    kz_json:set_value(<<"comment">>, Comment, Doc).

-spec max_concurent_calls_per_user(doc()) -> api_integer().
-spec max_concurent_calls_per_user(doc(), Default) -> integer() | Default.
max_concurent_calls_per_user(Doc) ->
    max_concurent_calls_per_user(Doc, 'undefined').
max_concurent_calls_per_user(Doc, Default) ->
    kz_json:get_integer_value(<<"max_concurent_calls_per_user">>, Doc, Default).

-spec set_max_concurent_calls_per_user(doc(), integer()) -> doc().
set_max_concurent_calls_per_user(Doc, MaxConcurentCallsPerUser) ->
    kz_json:set_value(<<"max_concurent_calls_per_user">>, MaxConcurentCallsPerUser, Doc).

-spec pin(doc()) -> api_binary().
-spec pin(doc(), Default) -> binary() | Default.
pin(Doc) ->
    pin(Doc, 'undefined').
pin(Doc, Default) ->
    kz_json:get_binary_value(<<"pin">>, Doc, Default).

-spec set_pin(doc(), binary()) -> doc().
set_pin(Doc, Pin) ->
    kz_json:set_value(<<"pin">>, Pin, Doc).

-spec retain_cid(doc()) -> api_boolean().
-spec retain_cid(doc(), Default) -> boolean() | Default.
retain_cid(Doc) ->
    retain_cid(Doc, 'undefined').
retain_cid(Doc, Default) ->
    kz_json:get_boolean_value(<<"retain_cid">>, Doc, Default).

-spec set_retain_cid(doc(), boolean()) -> doc().
set_retain_cid(Doc, RetainCid) ->
    kz_json:set_value(<<"retain_cid">>, RetainCid, Doc).

-spec user_id(doc()) -> api_ne_binary().
-spec user_id(doc(), Default) -> ne_binary() | Default.
user_id(Doc) ->
    user_id(Doc, 'undefined').
user_id(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"user_id">>, Doc, Default).

-spec set_user_id(doc(), ne_binary()) -> doc().
set_user_id(Doc, UserId) ->
    kz_json:set_value(<<"user_id">>, UserId, Doc).