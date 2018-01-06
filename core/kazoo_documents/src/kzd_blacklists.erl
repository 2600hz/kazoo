-module(kzd_blacklists).

-export([new/0]).
-export([name/1, name/2, set_name/2]).
-export([numbers/1, numbers/2, set_numbers/2]).
-export([should_block_anonymous/1, should_block_anonymous/2, set_should_block_anonymous/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec name(doc()) -> api_ne_binary().
-spec name(doc(), Default) -> ne_binary() | Default.
name(Doc) ->
    name(Doc, 'undefined').
name(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"name">>, Doc, Default).

-spec set_name(doc(), ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value(<<"name">>, Name, Doc).

-spec numbers(doc()) -> kz_json:object().
-spec numbers(doc(), Default) -> kz_json:object() | Default.
numbers(Doc) ->
    numbers(Doc, kz_json:new()).
numbers(Doc, Default) ->
    kz_json:get_json_value(<<"numbers">>, Doc, Default).

-spec set_numbers(doc(), kz_json:object()) -> doc().
set_numbers(Doc, Numbers) ->
    kz_json:set_value(<<"numbers">>, Numbers, Doc).

-spec should_block_anonymous(doc()) -> api_boolean().
-spec should_block_anonymous(doc(), Default) -> boolean() | Default.
should_block_anonymous(Doc) ->
    should_block_anonymous(Doc, 'undefined').
should_block_anonymous(Doc, Default) ->
    kz_json:get_boolean_value(<<"should_block_anonymous">>, Doc, Default).

-spec set_should_block_anonymous(doc(), boolean()) -> doc().
set_should_block_anonymous(Doc, ShouldBlockAnonymous) ->
    kz_json:set_value(<<"should_block_anonymous">>, ShouldBlockAnonymous, Doc).