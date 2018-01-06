-module(kzd_groups).

-export([new/0]).
-export([endpoints/1, endpoints/2, set_endpoints/2]).
-export([music_on_hold/1, music_on_hold/2, set_music_on_hold/2]).
-export([name/1, name/2, set_name/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec endpoints(doc()) -> kz_json:object().
-spec endpoints(doc(), Default) -> kz_json:object() | Default.
endpoints(Doc) ->
    endpoints(Doc, kz_json:new()).
endpoints(Doc, Default) ->
    kz_json:get_json_value(<<"endpoints">>, Doc, Default).

-spec set_endpoints(doc(), kz_json:object()) -> doc().
set_endpoints(Doc, Endpoints) ->
    kz_json:set_value(<<"endpoints">>, Endpoints, Doc).

-spec music_on_hold(doc()) -> kz_json:object().
-spec music_on_hold(doc(), Default) -> kz_json:object() | Default.
music_on_hold(Doc) ->
    music_on_hold(Doc, kz_json:new()).
music_on_hold(Doc, Default) ->
    kz_json:get_json_value(<<"music_on_hold">>, Doc, Default).

-spec set_music_on_hold(doc(), kz_json:object()) -> doc().
set_music_on_hold(Doc, MusicOnHold) ->
    kz_json:set_value(<<"music_on_hold">>, MusicOnHold, Doc).

-spec name(doc()) -> api_ne_binary().
-spec name(doc(), Default) -> ne_binary() | Default.
name(Doc) ->
    name(Doc, 'undefined').
name(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"name">>, Doc, Default).

-spec set_name(doc(), ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value(<<"name">>, Name, Doc).
