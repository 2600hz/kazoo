%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_blacklists).

-export([new/0]).
-export([name/1, name/2, set_name/2]).
-export([numbers/1, numbers/2, set_numbers/2]).
-export([should_block_anonymous/1, should_block_anonymous/2, set_should_block_anonymous/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"blacklists">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec name(doc()) -> kz_term:api_ne_binary().
name(Doc) ->
    name(Doc, 'undefined').

-spec name(doc(), Default) -> kz_term:ne_binary() | Default.
name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), kz_term:ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

-spec numbers(doc()) -> kz_json:object().
numbers(Doc) ->
    numbers(Doc, kz_json:new()).

-spec numbers(doc(), Default) -> kz_json:object() | Default.
numbers(Doc, Default) ->
    kz_json:get_json_value([<<"numbers">>], Doc, Default).

-spec set_numbers(doc(), kz_json:object()) -> doc().
set_numbers(Doc, Numbers) ->
    kz_json:set_value([<<"numbers">>], Numbers, Doc).

-spec should_block_anonymous(doc()) -> kz_term:api_boolean().
should_block_anonymous(Doc) ->
    should_block_anonymous(Doc, 'undefined').

-spec should_block_anonymous(doc(), Default) -> boolean() | Default.
should_block_anonymous(Doc, Default) ->
    kz_json:get_boolean_value([<<"should_block_anonymous">>], Doc, Default).

-spec set_should_block_anonymous(doc(), boolean()) -> doc().
set_should_block_anonymous(Doc, ShouldBlockAnonymous) ->
    kz_json:set_value([<<"should_block_anonymous">>], ShouldBlockAnonymous, Doc).
