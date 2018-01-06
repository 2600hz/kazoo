-module(kzd_multi_factor_provider).

-export([new/0]).
-export([enabled/1, enabled/2, set_enabled/2]).
-export([name/1, name/2, set_name/2]).
-export([provider_name/1, provider_name/2, set_provider_name/2]).
-export([settings/1, settings/2, set_settings/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec enabled(doc()) -> api_boolean().
-spec enabled(doc(), Default) -> boolean() | Default.
enabled(Doc) ->
    enabled(Doc, 'undefined').
enabled(Doc, Default) ->
    kz_json:get_boolean_value(<<"enabled">>, Doc, Default).

-spec set_enabled(doc(), boolean()) -> doc().
set_enabled(Doc, Enabled) ->
    kz_json:set_value(<<"enabled">>, Enabled, Doc).

-spec name(doc()) -> api_binary().
-spec name(doc(), Default) -> binary() | Default.
name(Doc) ->
    name(Doc, 'undefined').
name(Doc, Default) ->
    kz_json:get_binary_value(<<"name">>, Doc, Default).

-spec set_name(doc(), binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value(<<"name">>, Name, Doc).

-spec provider_name(doc()) -> api_binary().
-spec provider_name(doc(), Default) -> binary() | Default.
provider_name(Doc) ->
    provider_name(Doc, 'undefined').
provider_name(Doc, Default) ->
    kz_json:get_binary_value(<<"provider_name">>, Doc, Default).

-spec set_provider_name(doc(), binary()) -> doc().
set_provider_name(Doc, ProviderName) ->
    kz_json:set_value(<<"provider_name">>, ProviderName, Doc).

-spec settings(doc()) -> api_object().
-spec settings(doc(), Default) -> kz_json:object() | Default.
settings(Doc) ->
    settings(Doc, 'undefined').
settings(Doc, Default) ->
    kz_json:get_json_value(<<"settings">>, Doc, Default).

-spec set_settings(doc(), kz_json:object()) -> doc().
set_settings(Doc, Settings) ->
    kz_json:set_value(<<"settings">>, Settings, Doc).
