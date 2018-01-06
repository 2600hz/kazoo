-module(kzd_devices_notify).

-export([new/0]).
-export([data/1, data/2, set_data/2]).
-export([data_body/1, data_body/2, set_data_body/2]).
-export([databody_content_type/1, databody_content_type/2, set_databody_content_type/2]).
-export([databody_data/1, databody_data/2, set_databody_data/2]).
-export([data_event/1, data_event/2, set_data_event/2]).


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

-spec data_body(doc()) -> api_object().
-spec data_body(doc(), Default) -> kz_json:object() | Default.
data_body(Doc) ->
    data_body(Doc, 'undefined').
data_body(Doc, Default) ->
    kz_json:get_json_value([<<"data">>, <<"body">>], Doc, Default).

-spec set_data_body(doc(), kz_json:object()) -> doc().
set_data_body(Doc, DataBody) ->
    kz_json:set_value([<<"data">>, <<"body">>], DataBody, Doc).

-spec databody_content_type(doc()) -> api_binary().
-spec databody_content_type(doc(), Default) -> binary() | Default.
databody_content_type(Doc) ->
    databody_content_type(Doc, 'undefined').
databody_content_type(Doc, Default) ->
    kz_json:get_binary_value([[<<"data">>, <<"body">>], <<"content_type">>], Doc, Default).

-spec set_databody_content_type(doc(), binary()) -> doc().
set_databody_content_type(Doc, DatabodyContentType) ->
    kz_json:set_value([[<<"data">>, <<"body">>], <<"content_type">>], DatabodyContentType, Doc).

-spec databody_data(doc()) -> api_binary().
-spec databody_data(doc(), Default) -> binary() | Default.
databody_data(Doc) ->
    databody_data(Doc, 'undefined').
databody_data(Doc, Default) ->
    kz_json:get_binary_value([[<<"data">>, <<"body">>], <<"data">>], Doc, Default).

-spec set_databody_data(doc(), binary()) -> doc().
set_databody_data(Doc, DatabodyData) ->
    kz_json:set_value([[<<"data">>, <<"body">>], <<"data">>], DatabodyData, Doc).

-spec data_event(doc()) -> api_binary().
-spec data_event(doc(), Default) -> binary() | Default.
data_event(Doc) ->
    data_event(Doc, 'undefined').
data_event(Doc, Default) ->
    kz_json:get_binary_value([<<"data">>, <<"event">>], Doc, Default).

-spec set_data_event(doc(), binary()) -> doc().
set_data_event(Doc, DataEvent) ->
    kz_json:set_value([<<"data">>, <<"event">>], DataEvent, Doc).
