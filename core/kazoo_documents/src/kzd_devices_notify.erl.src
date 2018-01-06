-module(kzd_devices_notify).

-export([new/0]).
-export([data/1, data/2, set_data/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec data(doc()) -> api_object().
-spec data(doc(), Default) -> kz_json:object() | Default.
data(Doc) ->
    data(Doc, 'undefined').
data(Doc, Default) ->
    kz_json:get_json_value(<<"data">>, Doc, Default).

-spec set_data(doc(), kz_json:object()) -> doc().
set_data(Doc, Data) ->
    kz_json:set_value(<<"data">>, Data, Doc).
