-module(kzd_metaflow).

-export([new/0]).
-export([children/1, children/2, set_children/2]).
-export([data/1, data/2, set_data/2]).
-export([module/1, module/2, set_module/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec children(doc()) -> api_object().
-spec children(doc(), Default) -> kz_json:object() | Default.
children(Doc) ->
    children(Doc, 'undefined').
children(Doc, Default) ->
    kz_json:get_json_value([<<"children">>], Doc, Default).

-spec set_children(doc(), kz_json:object()) -> doc().
set_children(Doc, Children) ->
    kz_json:set_value([<<"children">>], Children, Doc).

-spec data(doc()) -> kz_json:object().
-spec data(doc(), Default) -> kz_json:object() | Default.
data(Doc) ->
    data(Doc, kz_json:new()).
data(Doc, Default) ->
    kz_json:get_json_value([<<"data">>], Doc, Default).

-spec set_data(doc(), kz_json:object()) -> doc().
set_data(Doc, Data) ->
    kz_json:set_value([<<"data">>], Data, Doc).

-spec module(doc()) -> api_ne_binary().
-spec module(doc(), Default) -> ne_binary() | Default.
module(Doc) ->
    module(Doc, 'undefined').
module(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"module">>], Doc, Default).

-spec set_module(doc(), ne_binary()) -> doc().
set_module(Doc, Module) ->
    kz_json:set_value([<<"module">>], Module, Doc).
