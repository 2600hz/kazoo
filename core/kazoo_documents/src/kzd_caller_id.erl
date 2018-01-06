-module(kzd_caller_id).

-export([new/0]).
-export([emergency/1, emergency/2, set_emergency/2]).
-export([external/1, external/2, set_external/2]).
-export([internal/1, internal/2, set_internal/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec emergency(doc()) -> api_object().
-spec emergency(doc(), Default) -> kz_json:object() | Default.
emergency(Doc) ->
    emergency(Doc, 'undefined').
emergency(Doc, Default) ->
    kz_json:get_json_value(<<"emergency">>, Doc, Default).

-spec set_emergency(doc(), kz_json:object()) -> doc().
set_emergency(Doc, Emergency) ->
    kz_json:set_value(<<"emergency">>, Emergency, Doc).

-spec external(doc()) -> api_object().
-spec external(doc(), Default) -> kz_json:object() | Default.
external(Doc) ->
    external(Doc, 'undefined').
external(Doc, Default) ->
    kz_json:get_json_value(<<"external">>, Doc, Default).

-spec set_external(doc(), kz_json:object()) -> doc().
set_external(Doc, External) ->
    kz_json:set_value(<<"external">>, External, Doc).

-spec internal(doc()) -> api_object().
-spec internal(doc(), Default) -> kz_json:object() | Default.
internal(Doc) ->
    internal(Doc, 'undefined').
internal(Doc, Default) ->
    kz_json:get_json_value(<<"internal">>, Doc, Default).

-spec set_internal(doc(), kz_json:object()) -> doc().
set_internal(Doc, Internal) ->
    kz_json:set_value(<<"internal">>, Internal, Doc).
