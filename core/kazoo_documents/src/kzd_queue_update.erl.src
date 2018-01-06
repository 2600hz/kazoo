-module(kzd_queue_update).

-export([new/0]).
-export([action/1, action/2, set_action/2]).
-export([queue_id/1, queue_id/2, set_queue_id/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec action(doc()) -> api_binary().
-spec action(doc(), Default) -> binary() | Default.
action(Doc) ->
    action(Doc, 'undefined').
action(Doc, Default) ->
    kz_json:get_binary_value(<<"action">>, Doc, Default).

-spec set_action(doc(), binary()) -> doc().
set_action(Doc, Action) ->
    kz_json:set_value(<<"action">>, Action, Doc).

-spec queue_id(doc()) -> api_ne_binary().
-spec queue_id(doc(), Default) -> ne_binary() | Default.
queue_id(Doc) ->
    queue_id(Doc, 'undefined').
queue_id(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"queue_id">>, Doc, Default).

-spec set_queue_id(doc(), ne_binary()) -> doc().
set_queue_id(Doc, QueueId) ->
    kz_json:set_value(<<"queue_id">>, QueueId, Doc).
