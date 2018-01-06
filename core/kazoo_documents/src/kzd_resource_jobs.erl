-module(kzd_resource_jobs).

-export([new/0]).
-export([name/1, name/2, set_name/2]).
-export([numbers/1, numbers/2, set_numbers/2]).
-export([resource_id/1, resource_id/2, set_resource_id/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec name(doc()) -> api_ne_binary().
-spec name(doc(), Default) -> ne_binary() | Default.
name(Doc) ->
    name(Doc, 'undefined').
name(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"name">>, Doc, Default).

-spec set_name(doc(), ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value(<<"name">>, Name, Doc).

-spec numbers(doc()) -> api_ne_binaries().
-spec numbers(doc(), Default) -> ne_binaries() | Default.
numbers(Doc) ->
    numbers(Doc, 'undefined').
numbers(Doc, Default) ->
    kz_json:get_list_value(<<"numbers">>, Doc, Default).

-spec set_numbers(doc(), ne_binaries()) -> doc().
set_numbers(Doc, Numbers) ->
    kz_json:set_value(<<"numbers">>, Numbers, Doc).

-spec resource_id(doc()) -> api_binary().
-spec resource_id(doc(), Default) -> binary() | Default.
resource_id(Doc) ->
    resource_id(Doc, 'undefined').
resource_id(Doc, Default) ->
    kz_json:get_binary_value(<<"resource_id">>, Doc, Default).

-spec set_resource_id(doc(), binary()) -> doc().
set_resource_id(Doc, ResourceId) ->
    kz_json:set_value(<<"resource_id">>, ResourceId, Doc).
