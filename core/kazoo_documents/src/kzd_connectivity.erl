-module(kzd_connectivity).

-export([new/0]).
-export([account/1, account/2, set_account/2]).
-export([name/1, name/2, set_name/2]).
-export([servers/1, servers/2, set_servers/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec account(doc()) -> api_object().
-spec account(doc(), Default) -> kz_json:object() | Default.
account(Doc) ->
    account(Doc, 'undefined').
account(Doc, Default) ->
    kz_json:get_json_value(<<"account">>, Doc, Default).

-spec set_account(doc(), kz_json:object()) -> doc().
set_account(Doc, Account) ->
    kz_json:set_value(<<"account">>, Account, Doc).

-spec name(doc()) -> api_binary().
-spec name(doc(), Default) -> binary() | Default.
name(Doc) ->
    name(Doc, 'undefined').
name(Doc, Default) ->
    kz_json:get_binary_value(<<"name">>, Doc, Default).

-spec set_name(doc(), binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value(<<"name">>, Name, Doc).

-spec servers(doc()) -> list().
-spec servers(doc(), Default) -> list() | Default.
servers(Doc) ->
    servers(Doc, []).
servers(Doc, Default) ->
    kz_json:get_list_value(<<"servers">>, Doc, Default).

-spec set_servers(doc(), list()) -> doc().
set_servers(Doc, Servers) ->
    kz_json:set_value(<<"servers">>, Servers, Doc).
