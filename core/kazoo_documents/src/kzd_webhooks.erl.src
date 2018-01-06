-module(kzd_webhooks).

-export([new/0]).
-export([custom_data/1, custom_data/2, set_custom_data/2]).
-export([enabled/1, enabled/2, set_enabled/2]).
-export([hook/1, hook/2, set_hook/2]).
-export([http_verb/1, http_verb/2, set_http_verb/2]).
-export([include_internal_legs/1, include_internal_legs/2, set_include_internal_legs/2]).
-export([include_subaccounts/1, include_subaccounts/2, set_include_subaccounts/2]).
-export([name/1, name/2, set_name/2]).
-export([retries/1, retries/2, set_retries/2]).
-export([uri/1, uri/2, set_uri/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec custom_data(doc()) -> api_object().
-spec custom_data(doc(), Default) -> kz_json:object() | Default.
custom_data(Doc) ->
    custom_data(Doc, 'undefined').
custom_data(Doc, Default) ->
    kz_json:get_json_value([<<"custom_data">>], Doc, Default).

-spec set_custom_data(doc(), kz_json:object()) -> doc().
set_custom_data(Doc, CustomData) ->
    kz_json:set_value([<<"custom_data">>], CustomData, Doc).

-spec enabled(doc()) -> boolean().
-spec enabled(doc(), Default) -> boolean() | Default.
enabled(Doc) ->
    enabled(Doc, true).
enabled(Doc, Default) ->
    kz_json:get_boolean_value([<<"enabled">>], Doc, Default).

-spec set_enabled(doc(), boolean()) -> doc().
set_enabled(Doc, Enabled) ->
    kz_json:set_value([<<"enabled">>], Enabled, Doc).

-spec hook(doc()) -> api_binary().
-spec hook(doc(), Default) -> binary() | Default.
hook(Doc) ->
    hook(Doc, 'undefined').
hook(Doc, Default) ->
    kz_json:get_binary_value([<<"hook">>], Doc, Default).

-spec set_hook(doc(), binary()) -> doc().
set_hook(Doc, Hook) ->
    kz_json:set_value([<<"hook">>], Hook, Doc).

-spec http_verb(doc()) -> binary().
-spec http_verb(doc(), Default) -> binary() | Default.
http_verb(Doc) ->
    http_verb(Doc, <<"post">>).
http_verb(Doc, Default) ->
    kz_json:get_binary_value([<<"http_verb">>], Doc, Default).

-spec set_http_verb(doc(), binary()) -> doc().
set_http_verb(Doc, HttpVerb) ->
    kz_json:set_value([<<"http_verb">>], HttpVerb, Doc).

-spec include_internal_legs(doc()) -> boolean().
-spec include_internal_legs(doc(), Default) -> boolean() | Default.
include_internal_legs(Doc) ->
    include_internal_legs(Doc, true).
include_internal_legs(Doc, Default) ->
    kz_json:get_boolean_value([<<"include_internal_legs">>], Doc, Default).

-spec set_include_internal_legs(doc(), boolean()) -> doc().
set_include_internal_legs(Doc, IncludeInternalLegs) ->
    kz_json:set_value([<<"include_internal_legs">>], IncludeInternalLegs, Doc).

-spec include_subaccounts(doc()) -> api_boolean().
-spec include_subaccounts(doc(), Default) -> boolean() | Default.
include_subaccounts(Doc) ->
    include_subaccounts(Doc, 'undefined').
include_subaccounts(Doc, Default) ->
    kz_json:get_boolean_value([<<"include_subaccounts">>], Doc, Default).

-spec set_include_subaccounts(doc(), boolean()) -> doc().
set_include_subaccounts(Doc, IncludeSubaccounts) ->
    kz_json:set_value([<<"include_subaccounts">>], IncludeSubaccounts, Doc).

-spec name(doc()) -> api_binary().
-spec name(doc(), Default) -> binary() | Default.
name(Doc) ->
    name(Doc, 'undefined').
name(Doc, Default) ->
    kz_json:get_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

-spec retries(doc()) -> integer().
-spec retries(doc(), Default) -> integer() | Default.
retries(Doc) ->
    retries(Doc, 2).
retries(Doc, Default) ->
    kz_json:get_integer_value([<<"retries">>], Doc, Default).

-spec set_retries(doc(), integer()) -> doc().
set_retries(Doc, Retries) ->
    kz_json:set_value([<<"retries">>], Retries, Doc).

-spec uri(doc()) -> api_binary().
-spec uri(doc(), Default) -> binary() | Default.
uri(Doc) ->
    uri(Doc, 'undefined').
uri(Doc, Default) ->
    kz_json:get_binary_value([<<"uri">>], Doc, Default).

-spec set_uri(doc(), binary()) -> doc().
set_uri(Doc, Uri) ->
    kz_json:set_value([<<"uri">>], Uri, Doc).
