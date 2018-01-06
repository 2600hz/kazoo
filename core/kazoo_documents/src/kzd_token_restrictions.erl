-module(kzd_token_restrictions).

-export([new/0]).
-export([restrictions/1, restrictions/2, set_restrictions/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec restrictions(doc()) -> api_object().
-spec restrictions(doc(), Default) -> kz_json:object() | Default.
restrictions(Doc) ->
    restrictions(Doc, 'undefined').
restrictions(Doc, Default) ->
    kz_json:get_json_value([<<"restrictions">>], Doc, Default).

-spec set_restrictions(doc(), kz_json:object()) -> doc().
set_restrictions(Doc, Restrictions) ->
    kz_json:set_value([<<"restrictions">>], Restrictions, Doc).
