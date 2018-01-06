%% Base document module to use when creating new document modules
-module(kzd_metaflow).

-export([new/0
        ,binding_digit/1, binding_digit/2
        ,digit_timeout/1, digit_timeout/2
        ,listen_on/1, listen_on/2
        ,numbers/1
        ,patterns/1
        ]).

-include("kz_documents.hrl").

-define(SCHEMA, <<"metaflows">>).

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec binding_digit(doc()) -> ne_binary().
-spec binding_digit(doc(), Default) -> ne_binary() | Default.
binding_digit(Doc) ->
    binding_digit(Doc, <<"*">>).
binding_digit(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"binding_digit">>, Doc, Default).

-spec digit_timeout(doc()) -> api_integer().
-spec digit_timeout(doc(), Default) -> non_neg_integer() | Default.
digit_timeout(Doc) ->
    digit_timeout(Doc, 0).
digit_timeout(Doc, Default) ->
    kz_json:get_integer_value(<<"digit_timeout">>, Doc, Default).

-spec listen_on(doc()) -> api_ne_binary().
-spec listen_on(doc(), Default) -> ne_binary() | Default.
listen_on(Doc) ->
    listen_on(Doc, 'undefined').
listen_on(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"listen_on">>, Doc, Default).

-spec numbers(doc()) -> api_object().
numbers(Doc) ->
    kz_json:get_json_value(<<"numbers">>, Doc).

-spec patterns(doc()) -> api_object().
patterns(Doc) ->
    kz_json:get_json_value(<<"patterns">>, Doc).
