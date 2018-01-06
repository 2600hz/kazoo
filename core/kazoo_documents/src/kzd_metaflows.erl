-module(kzd_metaflows).

-export([new/0]).
-export([binding_digit/1, binding_digit/2, set_binding_digit/2]).
-export([digit_timeout/1, digit_timeout/2, set_digit_timeout/2]).
-export([listen_on/1, listen_on/2, set_listen_on/2]).
-export([numbers/1, numbers/2, set_numbers/2]).
-export([patterns/1, patterns/2, set_patterns/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec binding_digit(doc()) -> ne_binary().
-spec binding_digit(doc(), Default) -> ne_binary() | Default.
binding_digit(Doc) ->
    binding_digit(Doc, <<"*">>).
binding_digit(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"binding_digit">>, Doc, Default).

-spec set_binding_digit(doc(), ne_binary()) -> doc().
set_binding_digit(Doc, BindingDigit) ->
    kz_json:set_value(<<"binding_digit">>, BindingDigit, Doc).

-spec digit_timeout(doc()) -> api_integer().
-spec digit_timeout(doc(), Default) -> integer() | Default.
digit_timeout(Doc) ->
    digit_timeout(Doc, 'undefined').
digit_timeout(Doc, Default) ->
    kz_json:get_integer_value(<<"digit_timeout">>, Doc, Default).

-spec set_digit_timeout(doc(), integer()) -> doc().
set_digit_timeout(Doc, DigitTimeout) ->
    kz_json:set_value(<<"digit_timeout">>, DigitTimeout, Doc).

-spec listen_on(doc()) -> api_binary().
-spec listen_on(doc(), Default) -> binary() | Default.
listen_on(Doc) ->
    listen_on(Doc, 'undefined').
listen_on(Doc, Default) ->
    kz_json:get_binary_value(<<"listen_on">>, Doc, Default).

-spec set_listen_on(doc(), binary()) -> doc().
set_listen_on(Doc, ListenOn) ->
    kz_json:set_value(<<"listen_on">>, ListenOn, Doc).

-spec numbers(doc()) -> api_object().
-spec numbers(doc(), Default) -> kz_json:object() | Default.
numbers(Doc) ->
    numbers(Doc, 'undefined').
numbers(Doc, Default) ->
    kz_json:get_json_value(<<"numbers">>, Doc, Default).

-spec set_numbers(doc(), kz_json:object()) -> doc().
set_numbers(Doc, Numbers) ->
    kz_json:set_value(<<"numbers">>, Numbers, Doc).

-spec patterns(doc()) -> api_object().
-spec patterns(doc(), Default) -> kz_json:object() | Default.
patterns(Doc) ->
    patterns(Doc, 'undefined').
patterns(Doc, Default) ->
    kz_json:get_json_value(<<"patterns">>, Doc, Default).

-spec set_patterns(doc(), kz_json:object()) -> doc().
set_patterns(Doc, Patterns) ->
    kz_json:set_value(<<"patterns">>, Patterns, Doc).
