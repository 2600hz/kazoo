-module(kzd_bookkeepers).

-export([new/0]).
-export([braintree/1, braintree/2, set_braintree/2]).
-export([local/1, local/2, set_local/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec braintree(doc()) -> api_object().
-spec braintree(doc(), Default) -> kz_json:object() | Default.
braintree(Doc) ->
    braintree(Doc, 'undefined').
braintree(Doc, Default) ->
    kz_json:get_json_value(<<"braintree">>, Doc, Default).

-spec set_braintree(doc(), kz_json:object()) -> doc().
set_braintree(Doc, Braintree) ->
    kz_json:set_value(<<"braintree">>, Braintree, Doc).

-spec local(doc()) -> api_object().
-spec local(doc(), Default) -> kz_json:object() | Default.
local(Doc) ->
    local(Doc, 'undefined').
local(Doc, Default) ->
    kz_json:get_json_value(<<"local">>, Doc, Default).

-spec set_local(doc(), kz_json:object()) -> doc().
set_local(Doc, Local) ->
    kz_json:set_value(<<"local">>, Local, Doc).
