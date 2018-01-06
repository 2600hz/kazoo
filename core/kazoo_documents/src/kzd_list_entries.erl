-module(kzd_list_entries).

-export([new/0]).
-export([displayname/1, displayname/2, set_displayname/2]).
-export([firstname/1, firstname/2, set_firstname/2]).
-export([lastname/1, lastname/2, set_lastname/2]).
-export([list_id/1, list_id/2, set_list_id/2]).
-export([number/1, number/2, set_number/2]).
-export([pattern/1, pattern/2, set_pattern/2]).
-export([profile/1, profile/2, set_profile/2]).
-export([type/1, type/2, set_type/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec displayname(doc()) -> api_ne_binary().
-spec displayname(doc(), Default) -> ne_binary() | Default.
displayname(Doc) ->
    displayname(Doc, 'undefined').
displayname(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"displayname">>, Doc, Default).

-spec set_displayname(doc(), ne_binary()) -> doc().
set_displayname(Doc, Displayname) ->
    kz_json:set_value(<<"displayname">>, Displayname, Doc).

-spec firstname(doc()) -> api_ne_binary().
-spec firstname(doc(), Default) -> ne_binary() | Default.
firstname(Doc) ->
    firstname(Doc, 'undefined').
firstname(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"firstname">>, Doc, Default).

-spec set_firstname(doc(), ne_binary()) -> doc().
set_firstname(Doc, Firstname) ->
    kz_json:set_value(<<"firstname">>, Firstname, Doc).

-spec lastname(doc()) -> api_ne_binary().
-spec lastname(doc(), Default) -> ne_binary() | Default.
lastname(Doc) ->
    lastname(Doc, 'undefined').
lastname(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"lastname">>, Doc, Default).

-spec set_lastname(doc(), ne_binary()) -> doc().
set_lastname(Doc, Lastname) ->
    kz_json:set_value(<<"lastname">>, Lastname, Doc).

-spec list_id(doc()) -> api_binary().
-spec list_id(doc(), Default) -> binary() | Default.
list_id(Doc) ->
    list_id(Doc, 'undefined').
list_id(Doc, Default) ->
    kz_json:get_binary_value(<<"list_id">>, Doc, Default).

-spec set_list_id(doc(), binary()) -> doc().
set_list_id(Doc, ListId) ->
    kz_json:set_value(<<"list_id">>, ListId, Doc).

-spec number(doc()) -> api_binary().
-spec number(doc(), Default) -> binary() | Default.
number(Doc) ->
    number(Doc, 'undefined').
number(Doc, Default) ->
    kz_json:get_binary_value(<<"number">>, Doc, Default).

-spec set_number(doc(), binary()) -> doc().
set_number(Doc, Number) ->
    kz_json:set_value(<<"number">>, Number, Doc).

-spec pattern(doc()) -> api_binary().
-spec pattern(doc(), Default) -> binary() | Default.
pattern(Doc) ->
    pattern(Doc, 'undefined').
pattern(Doc, Default) ->
    kz_json:get_binary_value(<<"pattern">>, Doc, Default).

-spec set_pattern(doc(), binary()) -> doc().
set_pattern(Doc, Pattern) ->
    kz_json:set_value(<<"pattern">>, Pattern, Doc).

-spec profile(doc()) -> kz_json:object().
-spec profile(doc(), Default) -> kz_json:object() | Default.
profile(Doc) ->
    profile(Doc, {}).
profile(Doc, Default) ->
    kz_json:get_json_value(<<"profile">>, Doc, Default).

-spec set_profile(doc(), kz_json:object()) -> doc().
set_profile(Doc, Profile) ->
    kz_json:set_value(<<"profile">>, Profile, Doc).

-spec type(doc()) -> api_ne_binary().
-spec type(doc(), Default) -> ne_binary() | Default.
type(Doc) ->
    type(Doc, 'undefined').
type(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"type">>, Doc, Default).

-spec set_type(doc(), ne_binary()) -> doc().
set_type(Doc, Type) ->
    kz_json:set_value(<<"type">>, Type, Doc).
