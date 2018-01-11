-module(kzd_storage).

-export([new/0]).
-export([attachments/1, attachments/2, set_attachments/2]).
-export([connections/1, connections/2, set_connections/2]).
-export([id/1, id/2, set_id/2]).
-export([plan/1, plan/2, set_plan/2]).
-export([ui_metadata/1, ui_metadata/2, set_ui_metadata/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec attachments(doc()) -> kz_term:api_object().
-spec attachments(doc(), Default) -> kz_json:object() | Default.
attachments(Doc) ->
    attachments(Doc, 'undefined').
attachments(Doc, Default) ->
    kz_json:get_json_value([<<"attachments">>], Doc, Default).

-spec set_attachments(doc(), kz_json:object()) -> doc().
set_attachments(Doc, Attachments) ->
    kz_json:set_value([<<"attachments">>], Attachments, Doc).

-spec connections(doc()) -> kz_term:api_object().
-spec connections(doc(), Default) -> kz_json:object() | Default.
connections(Doc) ->
    connections(Doc, 'undefined').
connections(Doc, Default) ->
    kz_json:get_json_value([<<"connections">>], Doc, Default).

-spec set_connections(doc(), kz_json:object()) -> doc().
set_connections(Doc, Connections) ->
    kz_json:set_value([<<"connections">>], Connections, Doc).

-spec id(doc()) -> kz_term:api_binary().
-spec id(doc(), Default) -> binary() | Default.
id(Doc) ->
    id(Doc, 'undefined').
id(Doc, Default) ->
    kz_json:get_binary_value([<<"id">>], Doc, Default).

-spec set_id(doc(), binary()) -> doc().
set_id(Doc, Id) ->
    kz_json:set_value([<<"id">>], Id, Doc).

-spec plan(doc()) -> kz_term:api_object().
-spec plan(doc(), Default) -> kz_json:object() | Default.
plan(Doc) ->
    plan(Doc, 'undefined').
plan(Doc, Default) ->
    kz_json:get_json_value([<<"plan">>], Doc, Default).

-spec set_plan(doc(), kz_json:object()) -> doc().
set_plan(Doc, Plan) ->
    kz_json:set_value([<<"plan">>], Plan, Doc).

-spec ui_metadata(doc()) -> kz_term:api_object().
-spec ui_metadata(doc(), Default) -> kz_json:object() | Default.
ui_metadata(Doc) ->
    ui_metadata(Doc, 'undefined').
ui_metadata(Doc, Default) ->
    kz_json:get_json_value([<<"ui_metadata">>], Doc, Default).

-spec set_ui_metadata(doc(), kz_json:object()) -> doc().
set_ui_metadata(Doc, UiMetadata) ->
    kz_json:set_value([<<"ui_metadata">>], UiMetadata, Doc).
