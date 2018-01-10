-module(kzd_ubiquiti_auth).

-export([new/0]).
-export([password/1, password/2, set_password/2]).
-export([username/1, username/2, set_username/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec password(doc()) -> kz_term:api_ne_binary().
-spec password(doc(), Default) -> kz_term:ne_binary() | Default.
password(Doc) ->
    password(Doc, 'undefined').
password(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"password">>], Doc, Default).

-spec set_password(doc(), kz_term:ne_binary()) -> doc().
set_password(Doc, Password) ->
    kz_json:set_value([<<"password">>], Password, Doc).

-spec username(doc()) -> kz_term:api_ne_binary().
-spec username(doc(), Default) -> kz_term:ne_binary() | Default.
username(Doc) ->
    username(Doc, 'undefined').
username(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"username">>], Doc, Default).

-spec set_username(doc(), kz_term:ne_binary()) -> doc().
set_username(Doc, Username) ->
    kz_json:set_value([<<"username">>], Username, Doc).
