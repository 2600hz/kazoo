%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_directories).

-export([new/0]).
-export([confirm_match/1, confirm_match/2, set_confirm_match/2]).
-export([flags/1, flags/2, set_flags/2]).
-export([max_dtmf/1, max_dtmf/2, set_max_dtmf/2]).
-export([min_dtmf/1, min_dtmf/2, set_min_dtmf/2]).
-export([name/1, name/2, set_name/2]).
-export([search_fields/1, search_fields/2, set_search_fields/2]).
-export([sort_by/1, sort_by/2, set_sort_by/2]).
-export([users/1, users/2, set_users/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"directories">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec confirm_match(doc()) -> boolean().
confirm_match(Doc) ->
    confirm_match(Doc, true).

-spec confirm_match(doc(), Default) -> boolean() | Default.
confirm_match(Doc, Default) ->
    kz_json:get_boolean_value([<<"confirm_match">>], Doc, Default).

-spec set_confirm_match(doc(), boolean()) -> doc().
set_confirm_match(Doc, ConfirmMatch) ->
    kz_json:set_value([<<"confirm_match">>], ConfirmMatch, Doc).

-spec flags(doc()) -> kz_term:api_ne_binaries().
flags(Doc) ->
    flags(Doc, 'undefined').

-spec flags(doc(), Default) -> kz_term:ne_binaries() | Default.
flags(Doc, Default) ->
    kz_json:get_list_value([<<"flags">>], Doc, Default).

-spec set_flags(doc(), kz_term:ne_binaries()) -> doc().
set_flags(Doc, Flags) ->
    kz_json:set_value([<<"flags">>], Flags, Doc).

-spec max_dtmf(doc()) -> integer().
max_dtmf(Doc) ->
    max_dtmf(Doc, 0).

-spec max_dtmf(doc(), Default) -> integer() | Default.
max_dtmf(Doc, Default) ->
    kz_json:get_integer_value([<<"max_dtmf">>], Doc, Default).

-spec set_max_dtmf(doc(), integer()) -> doc().
set_max_dtmf(Doc, MaxDtmf) ->
    kz_json:set_value([<<"max_dtmf">>], MaxDtmf, Doc).

-spec min_dtmf(doc()) -> integer().
min_dtmf(Doc) ->
    min_dtmf(Doc, 3).

-spec min_dtmf(doc(), Default) -> integer() | Default.
min_dtmf(Doc, Default) ->
    kz_json:get_integer_value([<<"min_dtmf">>], Doc, Default).

-spec set_min_dtmf(doc(), integer()) -> doc().
set_min_dtmf(Doc, MinDtmf) ->
    kz_json:set_value([<<"min_dtmf">>], MinDtmf, Doc).

-spec name(doc()) -> kz_term:api_ne_binary().
name(Doc) ->
    name(Doc, 'undefined').

-spec name(doc(), Default) -> kz_term:ne_binary() | Default.
name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), kz_term:ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

-spec search_fields(doc()) -> kz_term:api_binary().
search_fields(Doc) ->
    search_fields(Doc, 'undefined').

-spec search_fields(doc(), Default) -> binary() | Default.
search_fields(Doc, Default) ->
    kz_json:get_binary_value([<<"search_fields">>], Doc, Default).

-spec set_search_fields(doc(), binary()) -> doc().
set_search_fields(Doc, SearchFields) ->
    kz_json:set_value([<<"search_fields">>], SearchFields, Doc).

-spec sort_by(doc()) -> binary().
sort_by(Doc) ->
    sort_by(Doc, <<"last_name">>).

-spec sort_by(doc(), Default) -> binary() | Default.
sort_by(Doc, Default) ->
    kz_json:get_binary_value([<<"sort_by">>], Doc, Default).

-spec set_sort_by(doc(), binary()) -> doc().
set_sort_by(Doc, SortBy) ->
    kz_json:set_value([<<"sort_by">>], SortBy, Doc).

-spec users(doc()) -> kz_term:ne_binaries().
users(Doc) ->
    users(Doc, []).

-spec users(doc(), Default) -> kz_term:ne_binaries() | Default.
users(Doc, Default) ->
    kz_json:get_list_value([<<"users">>], Doc, Default).

-spec set_users(doc(), kz_term:ne_binaries()) -> doc().
set_users(Doc, Users) ->
    kz_json:set_value([<<"users">>], Users, Doc).
