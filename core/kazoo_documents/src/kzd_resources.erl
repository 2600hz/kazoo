-module(kzd_resources).

-export([new/0]).
-export([emergency/1, emergency/2, set_emergency/2]).
-export([enabled/1, enabled/2, set_enabled/2]).
-export([flags/1, flags/2, set_flags/2]).
-export([flat_rate_blacklist/1, flat_rate_blacklist/2, set_flat_rate_blacklist/2]).
-export([flat_rate_whitelist/1, flat_rate_whitelist/2, set_flat_rate_whitelist/2]).
-export([format_from_uri/1, format_from_uri/2, set_format_from_uri/2]).
-export([formatters/1, formatters/2, set_formatters/2]).
-export([from_uri_realm/1, from_uri_realm/2, set_from_uri_realm/2]).
-export([gateways/1, gateways/2, set_gateways/2]).
-export([grace_period/1, grace_period/2, set_grace_period/2]).
-export([media/1, media/2, set_media/2]).
-export([name/1, name/2, set_name/2]).
-export([require_flags/1, require_flags/2, set_require_flags/2]).
-export([rules/1, rules/2, set_rules/2]).
-export([weight_cost/1, weight_cost/2, set_weight_cost/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec emergency(doc()) -> boolean().
-spec emergency(doc(), Default) -> boolean() | Default.
emergency(Doc) ->
    emergency(Doc, false).
emergency(Doc, Default) ->
    kz_json:get_boolean_value(<<"emergency">>, Doc, Default).

-spec set_emergency(doc(), boolean()) -> doc().
set_emergency(Doc, Emergency) ->
    kz_json:set_value(<<"emergency">>, Emergency, Doc).

-spec enabled(doc()) -> boolean().
-spec enabled(doc(), Default) -> boolean() | Default.
enabled(Doc) ->
    enabled(Doc, true).
enabled(Doc, Default) ->
    kz_json:get_boolean_value(<<"enabled">>, Doc, Default).

-spec set_enabled(doc(), boolean()) -> doc().
set_enabled(Doc, Enabled) ->
    kz_json:set_value(<<"enabled">>, Enabled, Doc).

-spec flags(doc()) -> ne_binaries().
-spec flags(doc(), Default) -> ne_binaries() | Default.
flags(Doc) ->
    flags(Doc, []).
flags(Doc, Default) ->
    kz_json:get_list_value(<<"flags">>, Doc, Default).

-spec set_flags(doc(), ne_binaries()) -> doc().
set_flags(Doc, Flags) ->
    kz_json:set_value(<<"flags">>, Flags, Doc).

-spec flat_rate_blacklist(doc()) -> api_binary().
-spec flat_rate_blacklist(doc(), Default) -> binary() | Default.
flat_rate_blacklist(Doc) ->
    flat_rate_blacklist(Doc, 'undefined').
flat_rate_blacklist(Doc, Default) ->
    kz_json:get_binary_value(<<"flat_rate_blacklist">>, Doc, Default).

-spec set_flat_rate_blacklist(doc(), binary()) -> doc().
set_flat_rate_blacklist(Doc, FlatRateBlacklist) ->
    kz_json:set_value(<<"flat_rate_blacklist">>, FlatRateBlacklist, Doc).

-spec flat_rate_whitelist(doc()) -> api_binary().
-spec flat_rate_whitelist(doc(), Default) -> binary() | Default.
flat_rate_whitelist(Doc) ->
    flat_rate_whitelist(Doc, 'undefined').
flat_rate_whitelist(Doc, Default) ->
    kz_json:get_binary_value(<<"flat_rate_whitelist">>, Doc, Default).

-spec set_flat_rate_whitelist(doc(), binary()) -> doc().
set_flat_rate_whitelist(Doc, FlatRateWhitelist) ->
    kz_json:set_value(<<"flat_rate_whitelist">>, FlatRateWhitelist, Doc).

-spec format_from_uri(doc()) -> api_boolean().
-spec format_from_uri(doc(), Default) -> boolean() | Default.
format_from_uri(Doc) ->
    format_from_uri(Doc, 'undefined').
format_from_uri(Doc, Default) ->
    kz_json:get_boolean_value(<<"format_from_uri">>, Doc, Default).

-spec set_format_from_uri(doc(), boolean()) -> doc().
set_format_from_uri(Doc, FormatFromUri) ->
    kz_json:set_value(<<"format_from_uri">>, FormatFromUri, Doc).

-spec formatters(doc()) -> api_object().
-spec formatters(doc(), Default) -> kz_json:object() | Default.
formatters(Doc) ->
    formatters(Doc, 'undefined').
formatters(Doc, Default) ->
    kz_json:get_json_value(<<"formatters">>, Doc, Default).

-spec set_formatters(doc(), kz_json:object()) -> doc().
set_formatters(Doc, Formatters) ->
    kz_json:set_value(<<"formatters">>, Formatters, Doc).

-spec from_uri_realm(doc()) -> api_binary().
-spec from_uri_realm(doc(), Default) -> binary() | Default.
from_uri_realm(Doc) ->
    from_uri_realm(Doc, 'undefined').
from_uri_realm(Doc, Default) ->
    kz_json:get_binary_value(<<"from_uri_realm">>, Doc, Default).

-spec set_from_uri_realm(doc(), binary()) -> doc().
set_from_uri_realm(Doc, FromUriRealm) ->
    kz_json:set_value(<<"from_uri_realm">>, FromUriRealm, Doc).

-spec gateways(doc()) -> api_list().
-spec gateways(doc(), Default) -> list() | Default.
gateways(Doc) ->
    gateways(Doc, 'undefined').
gateways(Doc, Default) ->
    kz_json:get_list_value(<<"gateways">>, Doc, Default).

-spec set_gateways(doc(), list()) -> doc().
set_gateways(Doc, Gateways) ->
    kz_json:set_value(<<"gateways">>, Gateways, Doc).

-spec grace_period(doc()) -> integer().
-spec grace_period(doc(), Default) -> integer() | Default.
grace_period(Doc) ->
    grace_period(Doc, 5).
grace_period(Doc, Default) ->
    kz_json:get_integer_value(<<"grace_period">>, Doc, Default).

-spec set_grace_period(doc(), integer()) -> doc().
set_grace_period(Doc, GracePeriod) ->
    kz_json:set_value(<<"grace_period">>, GracePeriod, Doc).

-spec media(doc()) -> api_object().
-spec media(doc(), Default) -> kz_json:object() | Default.
media(Doc) ->
    media(Doc, 'undefined').
media(Doc, Default) ->
    kz_json:get_json_value(<<"media">>, Doc, Default).

-spec set_media(doc(), kz_json:object()) -> doc().
set_media(Doc, Media) ->
    kz_json:set_value(<<"media">>, Media, Doc).

-spec name(doc()) -> api_ne_binary().
-spec name(doc(), Default) -> ne_binary() | Default.
name(Doc) ->
    name(Doc, 'undefined').
name(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"name">>, Doc, Default).

-spec set_name(doc(), ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value(<<"name">>, Name, Doc).

-spec require_flags(doc()) -> api_boolean().
-spec require_flags(doc(), Default) -> boolean() | Default.
require_flags(Doc) ->
    require_flags(Doc, 'undefined').
require_flags(Doc, Default) ->
    kz_json:get_boolean_value(<<"require_flags">>, Doc, Default).

-spec set_require_flags(doc(), boolean()) -> doc().
set_require_flags(Doc, RequireFlags) ->
    kz_json:set_value(<<"require_flags">>, RequireFlags, Doc).

-spec rules(doc()) -> ne_binaries().
-spec rules(doc(), Default) -> ne_binaries() | Default.
rules(Doc) ->
    rules(Doc, []).
rules(Doc, Default) ->
    kz_json:get_list_value(<<"rules">>, Doc, Default).

-spec set_rules(doc(), ne_binaries()) -> doc().
set_rules(Doc, Rules) ->
    kz_json:set_value(<<"rules">>, Rules, Doc).

-spec weight_cost(doc()) -> integer().
-spec weight_cost(doc(), Default) -> integer() | Default.
weight_cost(Doc) ->
    weight_cost(Doc, 50).
weight_cost(Doc, Default) ->
    kz_json:get_integer_value(<<"weight_cost">>, Doc, Default).

-spec set_weight_cost(doc(), integer()) -> doc().
set_weight_cost(Doc, WeightCost) ->
    kz_json:set_value(<<"weight_cost">>, WeightCost, Doc).
