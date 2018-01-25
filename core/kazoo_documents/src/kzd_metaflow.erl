-module(kzd_metaflow).

-export([new/0]).
-export([children/1, children/2, set_children/2]).
-export([child/2, child/3, set_child/3]).
-export([data/1, data/2, set_data/2]).
-export([module/1, module/2, set_module/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec children(doc()) -> kz_term:api_object().
children(Doc) ->
    children(Doc, 'undefined').

-spec children(doc(), Default) -> kz_json:object() | Default.
children(Doc, Default) ->
    kz_json:get_json_value([<<"children">>], Doc, Default).

-spec set_children(doc(), kz_json:object()) -> doc().
set_children(Doc, Children) ->
    kz_json:set_value([<<"children">>], Children, Doc).

-spec child(doc(), kz_json:key()) -> kz_term:api_object().
child(Doc, Child) ->
    child(Doc, Child, 'undefined').

-spec child(doc(), kz_json:key(), Default) -> kz_json:object() | Default.
child(Doc, Child, Default) ->
    kz_json:get_json_value([<<"children">>, Child], Doc, Default).

-spec set_child(doc(), kz_json:key(), kz_json:object()) -> doc().
set_child(Doc, Child, Value) ->
    kz_json:set_value([<<"children">>, Child], Value, Doc).

-spec data(doc()) -> kz_json:object().
data(Doc) ->
    data(Doc, kz_json:new()).

-spec data(doc(), Default) -> kz_json:object() | Default.
data(Doc, Default) ->
    kz_json:get_json_value([<<"data">>], Doc, Default).

-spec set_data(doc(), kz_json:object()) -> doc().
set_data(Doc, Data) ->
    kz_json:set_value([<<"data">>], Data, Doc).

-spec module(doc()) -> kz_term:api_ne_binary().
module(Doc) ->
    module(Doc, 'undefined').

-spec module(doc(), Default) -> kz_term:ne_binary() | Default.
module(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"module">>], Doc, Default).

-spec set_module(doc(), kz_term:ne_binary()) -> doc().
set_module(Doc, Module) ->
    kz_json:set_value([<<"module">>], Module, Doc).
