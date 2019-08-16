%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_metaflows).

-export([new/0]).
-export([binding_digit/1, binding_digit/2, set_binding_digit/2]).
-export([digit_timeout/1, digit_timeout/2, set_digit_timeout/2]).
-export([listen_on/1, listen_on/2, set_listen_on/2]).
-export([numbers/1, numbers/2, set_numbers/2]).
-export([number/2, number/3, set_number/3]).
-export([patterns/1, patterns/2, set_patterns/2]).
-export([pattern/2, pattern/3, set_pattern/3]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"metaflows">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec binding_digit(doc()) -> kz_term:ne_binary().
binding_digit(Doc) ->
    binding_digit(Doc, <<"*">>).

-spec binding_digit(doc(), Default) -> kz_term:ne_binary() | Default.
binding_digit(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"binding_digit">>], Doc, Default).

-spec set_binding_digit(doc(), kz_term:ne_binary()) -> doc().
set_binding_digit(Doc, BindingDigit) ->
    kz_json:set_value([<<"binding_digit">>], BindingDigit, Doc).

-spec digit_timeout(doc()) -> kz_term:api_integer().
digit_timeout(Doc) ->
    digit_timeout(Doc, 'undefined').

-spec digit_timeout(doc(), Default) -> integer() | Default.
digit_timeout(Doc, Default) ->
    kz_json:get_integer_value([<<"digit_timeout">>], Doc, Default).

-spec set_digit_timeout(doc(), integer()) -> doc().
set_digit_timeout(Doc, DigitTimeout) ->
    kz_json:set_value([<<"digit_timeout">>], DigitTimeout, Doc).

-spec listen_on(doc()) -> kz_term:api_binary().
listen_on(Doc) ->
    listen_on(Doc, 'undefined').

-spec listen_on(doc(), Default) -> binary() | Default.
listen_on(Doc, Default) ->
    kz_json:get_binary_value([<<"listen_on">>], Doc, Default).

-spec set_listen_on(doc(), binary()) -> doc().
set_listen_on(Doc, ListenOn) ->
    kz_json:set_value([<<"listen_on">>], ListenOn, Doc).

-spec numbers(doc()) -> kz_term:api_object().
numbers(Doc) ->
    numbers(Doc, 'undefined').

-spec numbers(doc(), Default) -> kz_json:object() | Default.
numbers(Doc, Default) ->
    kz_json:get_json_value([<<"numbers">>], Doc, Default).

-spec set_numbers(doc(), kz_json:object()) -> doc().
set_numbers(Doc, Numbers) ->
    kz_json:set_value([<<"numbers">>], Numbers, Doc).

-spec number(doc(), kz_json:key()) -> kz_term:api_object().
number(Doc, Number) ->
    number(Doc, Number, 'undefined').

-spec number(doc(), kz_json:key(), Default) -> kz_json:object() | Default.
number(Doc, Number, Default) ->
    kz_json:get_json_value([<<"numbers">>, Number], Doc, Default).

-spec set_number(doc(), kz_json:key(), kz_json:object()) -> doc().
set_number(Doc, Number, Value) ->
    kz_json:set_value([<<"numbers">>, Number], Value, Doc).

-spec patterns(doc()) -> kz_term:api_object().
patterns(Doc) ->
    patterns(Doc, 'undefined').

-spec patterns(doc(), Default) -> kz_json:object() | Default.
patterns(Doc, Default) ->
    kz_json:get_json_value([<<"patterns">>], Doc, Default).

-spec set_patterns(doc(), kz_json:object()) -> doc().
set_patterns(Doc, Patterns) ->
    kz_json:set_value([<<"patterns">>], Patterns, Doc).

-spec pattern(doc(), kz_json:key()) -> kz_term:api_object().
pattern(Doc, Pattern) ->
    pattern(Doc, Pattern, 'undefined').

-spec pattern(doc(), kz_json:key(), Default) -> kz_json:object() | Default.
pattern(Doc, Pattern, Default) ->
    kz_json:get_json_value([<<"patterns">>, Pattern], Doc, Default).

-spec set_pattern(doc(), kz_json:key(), kz_json:object()) -> doc().
set_pattern(Doc, Pattern, Value) ->
    kz_json:set_value([<<"patterns">>, Pattern], Value, Doc).
