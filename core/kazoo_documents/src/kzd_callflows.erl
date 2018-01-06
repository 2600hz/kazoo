-module(kzd_callflows).

-export([new/0]).
-export([featurecode/1, featurecode/2, set_featurecode/2]).
-export([flow/1, flow/2, set_flow/2]).
-export([metaflow/1, metaflow/2, set_metaflow/2]).
-export([numbers/1, numbers/2, set_numbers/2]).
-export([patterns/1, patterns/2, set_patterns/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec featurecode(doc()) -> api_object().
-spec featurecode(doc(), Default) -> kz_json:object() | Default.
featurecode(Doc) ->
    featurecode(Doc, 'undefined').
featurecode(Doc, Default) ->
    kz_json:get_json_value(<<"featurecode">>, Doc, Default).

-spec set_featurecode(doc(), kz_json:object()) -> doc().
set_featurecode(Doc, Featurecode) ->
    kz_json:set_value(<<"featurecode">>, Featurecode, Doc).

-spec flow(doc()) -> api_object().
-spec flow(doc(), Default) -> kz_json:object() | Default.
flow(Doc) ->
    flow(Doc, 'undefined').
flow(Doc, Default) ->
    kz_json:get_json_value(<<"flow">>, Doc, Default).

-spec set_flow(doc(), kz_json:object()) -> doc().
set_flow(Doc, Flow) ->
    kz_json:set_value(<<"flow">>, Flow, Doc).

-spec metaflow(doc()) -> api_object().
-spec metaflow(doc(), Default) -> kz_json:object() | Default.
metaflow(Doc) ->
    metaflow(Doc, 'undefined').
metaflow(Doc, Default) ->
    kz_json:get_json_value(<<"metaflow">>, Doc, Default).

-spec set_metaflow(doc(), kz_json:object()) -> doc().
set_metaflow(Doc, Metaflow) ->
    kz_json:set_value(<<"metaflow">>, Metaflow, Doc).

-spec numbers(doc()) -> ne_binaries().
-spec numbers(doc(), Default) -> ne_binaries() | Default.
numbers(Doc) ->
    numbers(Doc, []).
numbers(Doc, Default) ->
    kz_json:get_list_value(<<"numbers">>, Doc, Default).

-spec set_numbers(doc(), ne_binaries()) -> doc().
set_numbers(Doc, Numbers) ->
    kz_json:set_value(<<"numbers">>, Numbers, Doc).

-spec patterns(doc()) -> ne_binaries().
-spec patterns(doc(), Default) -> ne_binaries() | Default.
patterns(Doc) ->
    patterns(Doc, []).
patterns(Doc, Default) ->
    kz_json:get_list_value(<<"patterns">>, Doc, Default).

-spec set_patterns(doc(), ne_binaries()) -> doc().
set_patterns(Doc, Patterns) ->
    kz_json:set_value(<<"patterns">>, Patterns, Doc).