-module(kzd_api_auth).

-export([new/0]).
-export([api_key/1, api_key/2, set_api_key/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec api_key(doc()) -> api_ne_binary().
-spec api_key(doc(), Default) -> ne_binary() | Default.
api_key(Doc) ->
    api_key(Doc, 'undefined').
api_key(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"api_key">>, Doc, Default).

-spec set_api_key(doc(), ne_binary()) -> doc().
set_api_key(Doc, ApiKey) ->
    kz_json:set_value(<<"api_key">>, ApiKey, Doc).
