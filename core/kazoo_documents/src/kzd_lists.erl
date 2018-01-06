-module(kzd_lists).

-export([new/0]).
-export([description/1, description/2, set_description/2]).
-export([name/1, name/2, set_name/2]).
-export([org/1, org/2, set_org/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec description(doc()) -> api_ne_binary().
-spec description(doc(), Default) -> ne_binary() | Default.
description(Doc) ->
    description(Doc, 'undefined').
description(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"description">>, Doc, Default).

-spec set_description(doc(), ne_binary()) -> doc().
set_description(Doc, Description) ->
    kz_json:set_value(<<"description">>, Description, Doc).

-spec name(doc()) -> api_ne_binary().
-spec name(doc(), Default) -> ne_binary() | Default.
name(Doc) ->
    name(Doc, 'undefined').
name(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"name">>, Doc, Default).

-spec set_name(doc(), ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value(<<"name">>, Name, Doc).

-spec org(doc()) -> api_binary().
-spec org(doc(), Default) -> binary() | Default.
org(Doc) ->
    org(Doc, 'undefined').
org(Doc, Default) ->
    kz_json:get_binary_value(<<"org">>, Doc, Default).

-spec set_org(doc(), binary()) -> doc().
set_org(Doc, Org) ->
    kz_json:set_value(<<"org">>, Org, Doc).
