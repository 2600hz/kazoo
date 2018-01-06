-module(kzd_directories).

-export([new/0]).
-export([confirm_match/1, confirm_match/2, set_confirm_match/2]).
-export([max_dtmf/1, max_dtmf/2, set_max_dtmf/2]).
-export([min_dtmf/1, min_dtmf/2, set_min_dtmf/2]).
-export([name/1, name/2, set_name/2]).
-export([sort_by/1, sort_by/2, set_sort_by/2]).
-export([users/1, users/2, set_users/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec confirm_match(doc()) -> boolean().
-spec confirm_match(doc(), Default) -> boolean() | Default.
confirm_match(Doc) ->
    confirm_match(Doc, true).
confirm_match(Doc, Default) ->
    kz_json:get_boolean_value(<<"confirm_match">>, Doc, Default).

-spec set_confirm_match(doc(), boolean()) -> doc().
set_confirm_match(Doc, ConfirmMatch) ->
    kz_json:set_value(<<"confirm_match">>, ConfirmMatch, Doc).

-spec max_dtmf(doc()) -> integer().
-spec max_dtmf(doc(), Default) -> integer() | Default.
max_dtmf(Doc) ->
    max_dtmf(Doc, 0).
max_dtmf(Doc, Default) ->
    kz_json:get_integer_value(<<"max_dtmf">>, Doc, Default).

-spec set_max_dtmf(doc(), integer()) -> doc().
set_max_dtmf(Doc, MaxDtmf) ->
    kz_json:set_value(<<"max_dtmf">>, MaxDtmf, Doc).

-spec min_dtmf(doc()) -> integer().
-spec min_dtmf(doc(), Default) -> integer() | Default.
min_dtmf(Doc) ->
    min_dtmf(Doc, 3).
min_dtmf(Doc, Default) ->
    kz_json:get_integer_value(<<"min_dtmf">>, Doc, Default).

-spec set_min_dtmf(doc(), integer()) -> doc().
set_min_dtmf(Doc, MinDtmf) ->
    kz_json:set_value(<<"min_dtmf">>, MinDtmf, Doc).

-spec name(doc()) -> api_ne_binary().
-spec name(doc(), Default) -> ne_binary() | Default.
name(Doc) ->
    name(Doc, 'undefined').
name(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"name">>, Doc, Default).

-spec set_name(doc(), ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value(<<"name">>, Name, Doc).

-spec sort_by(doc()) -> binary().
-spec sort_by(doc(), Default) -> binary() | Default.
sort_by(Doc) ->
    sort_by(Doc, <<"last_name">>).
sort_by(Doc, Default) ->
    kz_json:get_binary_value(<<"sort_by">>, Doc, Default).

-spec set_sort_by(doc(), binary()) -> doc().
set_sort_by(Doc, SortBy) ->
    kz_json:set_value(<<"sort_by">>, SortBy, Doc).

-spec users(doc()) -> ne_binaries().
-spec users(doc(), Default) -> ne_binaries() | Default.
users(Doc) ->
    users(Doc, []).
users(Doc, Default) ->
    kz_json:get_list_value(<<"users">>, Doc, Default).

-spec set_users(doc(), ne_binaries()) -> doc().
set_users(Doc, Users) ->
    kz_json:set_value(<<"users">>, Users, Doc).
