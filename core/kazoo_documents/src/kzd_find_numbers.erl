%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_find_numbers).

-export([new/0]).
-export([country/1, country/2, set_country/2]).
-export([prefix/1, prefix/2, set_prefix/2]).
-export([quantity/1, quantity/2, set_quantity/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"find_numbers">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec country(doc()) -> kz_term:ne_binary().
country(Doc) ->
    country(Doc, <<"US">>).

-spec country(doc(), Default) -> kz_term:ne_binary() | Default.
country(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"country">>], Doc, Default).

-spec set_country(doc(), kz_term:ne_binary()) -> doc().
set_country(Doc, Country) ->
    kz_json:set_value([<<"country">>], Country, Doc).

-spec prefix(doc()) -> kz_term:api_ne_binary().
prefix(Doc) ->
    prefix(Doc, 'undefined').

-spec prefix(doc(), Default) -> kz_term:ne_binary() | Default.
prefix(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"prefix">>], Doc, Default).

-spec set_prefix(doc(), kz_term:ne_binary()) -> doc().
set_prefix(Doc, Prefix) ->
    kz_json:set_value([<<"prefix">>], Prefix, Doc).

-spec quantity(doc()) -> integer().
quantity(Doc) ->
    quantity(Doc, 1).

-spec quantity(doc(), Default) -> integer() | Default.
quantity(Doc, Default) ->
    kz_json:get_integer_value([<<"quantity">>], Doc, Default).

-spec set_quantity(doc(), integer()) -> doc().
set_quantity(Doc, Quantity) ->
    kz_json:set_value([<<"quantity">>], Quantity, Doc).
