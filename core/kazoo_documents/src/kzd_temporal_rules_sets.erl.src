-module(kzd_temporal_rules_sets).

-export([new/0]).
-export([name/1, name/2, set_name/2]).
-export([temporal_rules/1, temporal_rules/2, set_temporal_rules/2]).


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
    kz_json:get_ne_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

-spec temporal_rules(doc()) -> api_ne_binaries().
-spec temporal_rules(doc(), Default) -> ne_binaries() | Default.
temporal_rules(Doc) ->
    temporal_rules(Doc, 'undefined').
temporal_rules(Doc, Default) ->
    kz_json:get_list_value([<<"temporal_rules">>], Doc, Default).

-spec set_temporal_rules(doc(), ne_binaries()) -> doc().
set_temporal_rules(Doc, TemporalRules) ->
    kz_json:set_value([<<"temporal_rules">>], TemporalRules, Doc).
