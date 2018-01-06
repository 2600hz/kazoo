-module(kzd_sms).

-export([new/0]).
-export([body/1, body/2, set_body/2]).
-export([from/1, from/2, set_from/2]).
-export([scheduled/1, scheduled/2, set_scheduled/2]).
-export([to/1, to/2, set_to/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec body(doc()) -> api_ne_binary().
-spec body(doc(), Default) -> ne_binary() | Default.
body(Doc) ->
    body(Doc, 'undefined').
body(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"body">>], Doc, Default).

-spec set_body(doc(), ne_binary()) -> doc().
set_body(Doc, Body) ->
    kz_json:set_value([<<"body">>], Body, Doc).

-spec from(doc()) -> api_binary().
-spec from(doc(), Default) -> binary() | Default.
from(Doc) ->
    from(Doc, 'undefined').
from(Doc, Default) ->
    kz_json:get_binary_value([<<"from">>], Doc, Default).

-spec set_from(doc(), binary()) -> doc().
set_from(Doc, From) ->
    kz_json:set_value([<<"from">>], From, Doc).

-spec scheduled(doc()) -> api_integer().
-spec scheduled(doc(), Default) -> integer() | Default.
scheduled(Doc) ->
    scheduled(Doc, 'undefined').
scheduled(Doc, Default) ->
    kz_json:get_integer_value([<<"scheduled">>], Doc, Default).

-spec set_scheduled(doc(), integer()) -> doc().
set_scheduled(Doc, Scheduled) ->
    kz_json:set_value([<<"scheduled">>], Scheduled, Doc).

-spec to(doc()) -> api_binary().
-spec to(doc(), Default) -> binary() | Default.
to(Doc) ->
    to(Doc, 'undefined').
to(Doc, Default) ->
    kz_json:get_binary_value([<<"to">>], Doc, Default).

-spec set_to(doc(), binary()) -> doc().
set_to(Doc, To) ->
    kz_json:set_value([<<"to">>], To, Doc).
