%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_groups).

-export([new/0]).
-export([endpoints/1, endpoints/2, set_endpoints/2]).
-export([flags/1, flags/2, set_flags/2]).
-export([music_on_hold/1, music_on_hold/2, set_music_on_hold/2]).
-export([music_on_hold_media_id/1, music_on_hold_media_id/2, set_music_on_hold_media_id/2]).
-export([name/1, name/2, set_name/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"groups">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec endpoints(doc()) -> kz_json:object().
endpoints(Doc) ->
    endpoints(Doc, kz_json:new()).

-spec endpoints(doc(), Default) -> kz_json:object() | Default.
endpoints(Doc, Default) ->
    kz_json:get_json_value([<<"endpoints">>], Doc, Default).

-spec set_endpoints(doc(), kz_json:object()) -> doc().
set_endpoints(Doc, Endpoints) ->
    kz_json:set_value([<<"endpoints">>], Endpoints, Doc).

-spec flags(doc()) -> kz_term:api_ne_binaries().
flags(Doc) ->
    flags(Doc, 'undefined').

-spec flags(doc(), Default) -> kz_term:ne_binaries() | Default.
flags(Doc, Default) ->
    kz_json:get_list_value([<<"flags">>], Doc, Default).

-spec set_flags(doc(), kz_term:ne_binaries()) -> doc().
set_flags(Doc, Flags) ->
    kz_json:set_value([<<"flags">>], Flags, Doc).

-spec music_on_hold(doc()) -> kz_json:object().
music_on_hold(Doc) ->
    music_on_hold(Doc, kz_json:new()).

-spec music_on_hold(doc(), Default) -> kz_json:object() | Default.
music_on_hold(Doc, Default) ->
    kz_json:get_json_value([<<"music_on_hold">>], Doc, Default).

-spec set_music_on_hold(doc(), kz_json:object()) -> doc().
set_music_on_hold(Doc, MusicOnHold) ->
    kz_json:set_value([<<"music_on_hold">>], MusicOnHold, Doc).

-spec music_on_hold_media_id(doc()) -> kz_term:api_binary().
music_on_hold_media_id(Doc) ->
    music_on_hold_media_id(Doc, 'undefined').

-spec music_on_hold_media_id(doc(), Default) -> binary() | Default.
music_on_hold_media_id(Doc, Default) ->
    kz_json:get_binary_value([<<"music_on_hold">>, <<"media_id">>], Doc, Default).

-spec set_music_on_hold_media_id(doc(), binary()) -> doc().
set_music_on_hold_media_id(Doc, MusicOnHoldMediaId) ->
    kz_json:set_value([<<"music_on_hold">>, <<"media_id">>], MusicOnHoldMediaId, Doc).

-spec name(doc()) -> kz_term:api_ne_binary().
name(Doc) ->
    name(Doc, 'undefined').

-spec name(doc(), Default) -> kz_term:ne_binary() | Default.
name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), kz_term:ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).
