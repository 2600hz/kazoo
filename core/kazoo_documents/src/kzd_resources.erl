%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_resources).

-export([new/0
        ,type/0, type/1
        ]).
-export([caller_id_options/1, caller_id_options/2, set_caller_id_options/2]).
-export([caller_id_options_type/1, caller_id_options_type/2, set_caller_id_options_type/2]).
-export([cid_rules/1, cid_rules/2, set_cid_rules/2]).
-export([classifiers/1, classifiers/2, set_classifiers/2]).
-export([classifier_emergency/2,   classifier_emergency/3,   set_classifier_emergency/3]).
-export([classifier_enabled/2,     classifier_enabled/3,     set_classifier_enabled/3]).
-export([classifier_prefix/2,      classifier_prefix/3,      set_classifier_prefix/3]).
-export([classifier_regex/2,       classifier_regex/3,       set_classifier_regex/3]).
-export([classifier_suffix/2,      classifier_suffix/3,      set_classifier_suffix/3]).
-export([classifier_weight_cost/2, classifier_weight_cost/3, set_classifier_weight_cost/3]).
-export([emergency/1, emergency/2, set_emergency/2]).
-export([enabled/1, enabled/2, set_enabled/2]).
-export([flags/1, flags/2, set_flags/2]).
-export([flat_rate_blacklist/1, flat_rate_blacklist/2, set_flat_rate_blacklist/2]).
-export([flat_rate_whitelist/1, flat_rate_whitelist/2, set_flat_rate_whitelist/2]).
-export([format_from_uri/1, format_from_uri/2, set_format_from_uri/2]).
-export([formatters/1, formatters/2, set_formatters/2]).
-export([from_account_realm/1, from_account_realm/2, set_from_account_realm/2]).
-export([from_uri_realm/1, from_uri_realm/2, set_from_uri_realm/2]).
-export([gateway_strategy/1, gateway_strategy/2, set_gateway_strategy/2]).
-export([gateways/1, gateways/2, set_gateways/2]).
-export([grace_period/1, grace_period/2, set_grace_period/2]).
-export([ignore_flags/1, ignore_flags/2, set_ignore_flags/2]).
-export([media/1, media/2, set_media/2]).
-export([name/1, name/2, set_name/2]).
-export([require_flags/1, require_flags/2, set_require_flags/2]).
-export([rules/1, rules/2, set_rules/2]).
-export([rules_test/1, rules_test/2, set_rules_test/2]).
-export([weight_cost/1, weight_cost/2, set_weight_cost/2]).

-export([media_fax_option/1, media_fax_option/2
        ,media_bypass_media/1, media_bypass_media/2
        ,media_audio_codecs/1, media_audio_codecs/2
        ,media_video_codecs/1, media_video_codecs/2
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-type docs() :: [doc()].
-export_type([doc/0, docs/0]).

-define(PVT_TYPE, <<"resource">>).
-define(SCHEMA, <<"resources">>).

-spec new() -> doc().
new() ->
    kz_doc:set_type(kz_json_schema:default_object(?SCHEMA), type()).

-spec type() -> kz_term:ne_binary().
type() -> ?PVT_TYPE.

-spec type(doc()) -> kz_term:ne_binary().
type(Doc) ->
    kz_doc:type(Doc, ?PVT_TYPE).

-spec caller_id_options(doc()) -> kz_term:api_object().
caller_id_options(Doc) ->
    caller_id_options(Doc, 'undefined').

-spec caller_id_options(doc(), Default) -> kz_json:object() | Default.
caller_id_options(Doc, Default) ->
    kz_json:get_json_value([<<"caller_id_options">>], Doc, Default).

-spec set_caller_id_options(doc(), kz_json:object()) -> doc().
set_caller_id_options(Doc, CallerIdOptions) ->
    kz_json:set_value([<<"caller_id_options">>], CallerIdOptions, Doc).

-spec caller_id_options_type(doc()) -> kz_term:api_binary().
caller_id_options_type(Doc) ->
    caller_id_options_type(Doc, 'undefined').

-spec caller_id_options_type(doc(), Default) -> binary() | Default.
caller_id_options_type(Doc, Default) ->
    kz_json:get_binary_value([<<"caller_id_options">>, <<"type">>], Doc, Default).

-spec set_caller_id_options_type(doc(), binary()) -> doc().
set_caller_id_options_type(Doc, CallerIdOptionsType) ->
    kz_json:set_value([<<"caller_id_options">>, <<"type">>], CallerIdOptionsType, Doc).

-spec cid_rules(doc()) -> kz_term:api_ne_binaries().
cid_rules(Doc) ->
    cid_rules(Doc, 'undefined').

-spec cid_rules(doc(), Default) -> kz_term:ne_binaries() | Default.
cid_rules(Doc, Default) ->
    kz_json:get_list_value([<<"cid_rules">>], Doc, Default).

-spec set_cid_rules(doc(), kz_term:ne_binaries()) -> doc().
set_cid_rules(Doc, CidRules) ->
    kz_json:set_value([<<"cid_rules">>], CidRules, Doc).

-spec classifiers(doc()) -> kz_term:api_object().
classifiers(Doc) ->
    classifiers(Doc, 'undefined').

-spec classifiers(doc(), Default) -> kz_json:object() | Default.
classifiers(Doc, Default) ->
    kz_json:get_json_value([<<"classifiers">>], Doc, Default).

-spec set_classifiers(doc(), kz_json:object()) -> doc().
set_classifiers(Doc, Classifiers) ->
    kz_json:set_value([<<"classifiers">>], Classifiers, Doc).

-spec classifier_emergency(doc(), kz_json:key()) -> boolean().
classifier_emergency(Doc, ClassifierName) ->
    classifier_emergency(Doc, ClassifierName, false).

-spec classifier_emergency(doc(), kz_json:key(), Default) -> boolean() | Default.
classifier_emergency(Doc, ClassifierName, Default) ->
    kz_json:get_boolean_value([<<"classifiers">>, ClassifierName, <<"emergency">>], Doc, Default).

-spec set_classifier_emergency(doc(), kz_json:key(), boolean()) -> doc().
set_classifier_emergency(Doc, ClassifierName, ClassifiersEmergency) ->
    kz_json:set_value([<<"classifiers">>, ClassifierName, <<"emergency">>], ClassifiersEmergency, Doc).

-spec classifier_enabled(doc(), kz_json:key()) -> boolean().
classifier_enabled(Doc, ClassifierName) ->
    classifier_enabled(Doc, ClassifierName, true).

-spec classifier_enabled(doc(), kz_json:key(), Default) -> boolean() | Default.
classifier_enabled(Doc, ClassifierName, Default) ->
    kz_json:get_boolean_value([<<"classifiers">>, ClassifierName, <<"enabled">>], Doc, Default).

-spec set_classifier_enabled(doc(), kz_json:key(), boolean()) -> doc().
set_classifier_enabled(Doc, ClassifierName, ClassifiersEnabled) ->
    kz_json:set_value([<<"classifiers">>, ClassifierName, <<"enabled">>], ClassifiersEnabled, Doc).

-spec classifier_prefix(doc(), kz_json:key()) -> kz_term:api_binary().
classifier_prefix(Doc, ClassifierName) ->
    classifier_prefix(Doc, ClassifierName, 'undefined').

-spec classifier_prefix(doc(), kz_json:key(), Default) -> binary() | Default.
classifier_prefix(Doc, ClassifierName, Default) ->
    kz_json:get_binary_value([<<"classifiers">>, ClassifierName, <<"prefix">>], Doc, Default).

-spec set_classifier_prefix(doc(), kz_json:key(), binary()) -> doc().
set_classifier_prefix(Doc, ClassifierName, ClassifiersPrefix) ->
    kz_json:set_value([<<"classifiers">>, ClassifierName, <<"prefix">>], ClassifiersPrefix, Doc).

-spec classifier_regex(doc(), kz_json:key()) -> kz_term:api_binary().
classifier_regex(Doc, ClassifierName) ->
    classifier_regex(Doc, ClassifierName, 'undefined').

-spec classifier_regex(doc(), kz_json:key(), Default) -> binary() | Default.
classifier_regex(Doc, ClassifierName, Default) ->
    kz_json:get_binary_value([<<"classifiers">>, ClassifierName, <<"regex">>], Doc, Default).

-spec set_classifier_regex(doc(), kz_json:key(), binary()) -> doc().
set_classifier_regex(Doc, ClassifierName, ClassifiersRegex) ->
    kz_json:set_value([<<"classifiers">>, ClassifierName, <<"regex">>], ClassifiersRegex, Doc).

-spec classifier_suffix(doc(), kz_json:key()) -> kz_term:api_binary().
classifier_suffix(Doc, ClassifierName) ->
    classifier_suffix(Doc, ClassifierName, 'undefined').

-spec classifier_suffix(doc(), kz_json:key(), Default) -> binary() | Default.
classifier_suffix(Doc, ClassifierName, Default) ->
    kz_json:get_binary_value([<<"classifiers">>, ClassifierName, <<"suffix">>], Doc, Default).

-spec set_classifier_suffix(doc(), kz_json:key(), binary()) -> doc().
set_classifier_suffix(Doc, ClassifierName, ClassifiersSuffix) ->
    kz_json:set_value([<<"classifiers">>, ClassifierName, <<"suffix">>], ClassifiersSuffix, Doc).

-spec classifier_weight_cost(doc(), kz_json:key()) -> integer().
classifier_weight_cost(Doc, ClassifierName) ->
    classifier_weight_cost(Doc, ClassifierName, 50).

-spec classifier_weight_cost(doc(), kz_json:key(), Default) -> integer() | Default.
classifier_weight_cost(Doc, ClassifierName, Default) ->
    kz_json:get_integer_value([<<"classifiers">>, ClassifierName, <<"weight_cost">>], Doc, Default).

-spec set_classifier_weight_cost(doc(), kz_json:key(), integer()) -> doc().
set_classifier_weight_cost(Doc, ClassifierName, ClassifiersWeightCost) ->
    kz_json:set_value([<<"classifiers">>, ClassifierName, <<"weight_cost">>], ClassifiersWeightCost, Doc).

-spec emergency(doc()) -> boolean().
emergency(Doc) ->
    emergency(Doc, false).

-spec emergency(doc(), Default) -> boolean() | Default.
emergency(Doc, Default) ->
    kz_json:get_boolean_value([<<"emergency">>], Doc, Default).

-spec set_emergency(doc(), boolean()) -> doc().
set_emergency(Doc, Emergency) ->
    kz_json:set_value([<<"emergency">>], Emergency, Doc).

-spec enabled(doc()) -> boolean().
enabled(Doc) ->
    enabled(Doc, true).

-spec enabled(doc(), Default) -> boolean() | Default.
enabled(Doc, Default) ->
    kz_json:get_boolean_value([<<"enabled">>], Doc, Default).

-spec set_enabled(doc(), boolean()) -> doc().
set_enabled(Doc, Enabled) ->
    kz_json:set_value([<<"enabled">>], Enabled, Doc).

-spec flags(doc()) -> kz_term:ne_binaries().
flags(Doc) ->
    flags(Doc, []).

-spec flags(doc(), Default) -> kz_term:ne_binaries() | Default.
flags(Doc, Default) ->
    kz_json:get_list_value([<<"flags">>], Doc, Default).

-spec set_flags(doc(), kz_term:ne_binaries()) -> doc().
set_flags(Doc, Flags) ->
    kz_json:set_value([<<"flags">>], Flags, Doc).

-spec flat_rate_blacklist(doc()) -> kz_term:api_binary().
flat_rate_blacklist(Doc) ->
    flat_rate_blacklist(Doc, 'undefined').

-spec flat_rate_blacklist(doc(), Default) -> binary() | Default.
flat_rate_blacklist(Doc, Default) ->
    kz_json:get_binary_value([<<"flat_rate_blacklist">>], Doc, Default).

-spec set_flat_rate_blacklist(doc(), binary()) -> doc().
set_flat_rate_blacklist(Doc, FlatRateBlacklist) ->
    kz_json:set_value([<<"flat_rate_blacklist">>], FlatRateBlacklist, Doc).

-spec flat_rate_whitelist(doc()) -> kz_term:api_binary().
flat_rate_whitelist(Doc) ->
    flat_rate_whitelist(Doc, 'undefined').

-spec flat_rate_whitelist(doc(), Default) -> binary() | Default.
flat_rate_whitelist(Doc, Default) ->
    kz_json:get_binary_value([<<"flat_rate_whitelist">>], Doc, Default).

-spec set_flat_rate_whitelist(doc(), binary()) -> doc().
set_flat_rate_whitelist(Doc, FlatRateWhitelist) ->
    kz_json:set_value([<<"flat_rate_whitelist">>], FlatRateWhitelist, Doc).

-spec format_from_uri(doc()) -> kz_term:api_boolean().
format_from_uri(Doc) ->
    format_from_uri(Doc, 'undefined').

-spec format_from_uri(doc(), Default) -> boolean() | Default.
format_from_uri(Doc, Default) ->
    kz_json:get_boolean_value([<<"format_from_uri">>], Doc, Default).

-spec set_format_from_uri(doc(), boolean()) -> doc().
set_format_from_uri(Doc, FormatFromUri) ->
    kz_json:set_value([<<"format_from_uri">>], FormatFromUri, Doc).

-spec formatters(doc()) -> kz_term:api_object().
formatters(Doc) ->
    formatters(Doc, 'undefined').

-spec formatters(doc(), Default) -> kz_json:object() | Default.
formatters(Doc, Default) ->
    kz_json:get_json_value([<<"formatters">>], Doc, Default).

-spec set_formatters(doc(), kz_json:object()) -> doc().
set_formatters(Doc, Formatters) ->
    kz_json:set_value([<<"formatters">>], Formatters, Doc).

-spec from_account_realm(doc()) -> boolean().
from_account_realm(Doc) ->
    from_account_realm(Doc, 'false').

-spec from_account_realm(doc(), Default) -> boolean() | Default.
from_account_realm(Doc, Default) ->
    kz_json:get_boolean_value([<<"from_account_realm">>], Doc, Default).

-spec set_from_account_realm(doc(), boolean()) -> doc().
set_from_account_realm(Doc, FromAccountRealm) ->
    kz_json:set_value([<<"from_account_realm">>], FromAccountRealm, Doc).

-spec from_uri_realm(doc()) -> kz_term:api_binary().
from_uri_realm(Doc) ->
    from_uri_realm(Doc, 'undefined').

-spec from_uri_realm(doc(), Default) -> binary() | Default.
from_uri_realm(Doc, Default) ->
    kz_json:get_binary_value([<<"from_uri_realm">>], Doc, Default).

-spec set_from_uri_realm(doc(), binary()) -> doc().
set_from_uri_realm(Doc, FromUriRealm) ->
    kz_json:set_value([<<"from_uri_realm">>], FromUriRealm, Doc).

-spec gateway_strategy(doc()) -> kz_term:api_binary().
gateway_strategy(Doc) ->
    gateway_strategy(Doc, 'undefined').

-spec gateway_strategy(doc(), Default) -> binary() | Default.
gateway_strategy(Doc, Default) ->
    kz_json:get_binary_value([<<"gateway_strategy">>], Doc, Default).

-spec set_gateway_strategy(doc(), binary()) -> doc().
set_gateway_strategy(Doc, GatewayStrategy) ->
    kz_json:set_value([<<"gateway_strategy">>], GatewayStrategy, Doc).

-spec gateways(doc()) -> kz_term:api_objects().
gateways(Doc) ->
    gateways(Doc, 'undefined').

-spec gateways(doc(), Default) -> kz_json:objects() | Default.
gateways(Doc, Default) ->
    kz_json:get_list_value([<<"gateways">>], Doc, Default).

-spec set_gateways(doc(), kz_json:objects()) -> doc().
set_gateways(Doc, Gateways) ->
    kz_json:set_value([<<"gateways">>], Gateways, Doc).

-spec grace_period(doc()) -> integer().
grace_period(Doc) ->
    grace_period(Doc, 5).

-spec grace_period(doc(), Default) -> integer() | Default.
grace_period(Doc, Default) ->
    kz_json:get_integer_value([<<"grace_period">>], Doc, Default).

-spec set_grace_period(doc(), integer()) -> doc().
set_grace_period(Doc, GracePeriod) ->
    kz_json:set_value([<<"grace_period">>], GracePeriod, Doc).

-spec ignore_flags(doc()) -> kz_term:api_boolean().
ignore_flags(Doc) ->
    ignore_flags(Doc, 'undefined').

-spec ignore_flags(doc(), Default) -> boolean() | Default.
ignore_flags(Doc, Default) ->
    kz_json:get_boolean_value([<<"ignore_flags">>], Doc, Default).

-spec set_ignore_flags(doc(), boolean()) -> doc().
set_ignore_flags(Doc, IgnoreFlags) ->
    kz_json:set_value([<<"ignore_flags">>], IgnoreFlags, Doc).

-spec media(doc()) -> kz_term:api_object().
media(Doc) ->
    media(Doc, 'undefined').

-spec media(doc(), Default) -> kz_json:object() | Default.
media(Doc, Default) ->
    kz_json:get_json_value([<<"media">>], Doc, Default).

-spec set_media(doc(), kz_json:object()) -> doc().
set_media(Doc, Media) ->
    kz_json:set_value([<<"media">>], Media, Doc).

-spec name(doc()) -> kz_term:api_ne_binary().
name(Doc) ->
    name(Doc, 'undefined').

-spec name(doc(), Default) -> kz_term:ne_binary() | Default.
name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), kz_term:ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

-spec require_flags(doc()) -> kz_term:api_boolean().
require_flags(Doc) ->
    require_flags(Doc, 'undefined').

-spec require_flags(doc(), Default) -> boolean() | Default.
require_flags(Doc, Default) ->
    kz_json:get_boolean_value([<<"require_flags">>], Doc, Default).

-spec set_require_flags(doc(), boolean()) -> doc().
set_require_flags(Doc, RequireFlags) ->
    kz_json:set_value([<<"require_flags">>], RequireFlags, Doc).

-spec rules(doc()) -> kz_term:ne_binaries().
rules(Doc) ->
    rules(Doc, []).

-spec rules(doc(), Default) -> kz_term:ne_binaries() | Default.
rules(Doc, Default) ->
    kz_json:get_list_value([<<"rules">>], Doc, Default).

-spec set_rules(doc(), kz_term:ne_binaries()) -> doc().
set_rules(Doc, Rules) ->
    kz_json:set_value([<<"rules">>], Rules, Doc).

-spec rules_test(doc()) -> kz_term:ne_binaries().
rules_test(Doc) ->
    rules_test(Doc, []).

-spec rules_test(doc(), Default) -> kz_term:ne_binaries() | Default.
rules_test(Doc, Default) ->
    kz_json:get_list_value([<<"rules_test">>], Doc, Default).

-spec set_rules_test(doc(), kz_term:ne_binaries()) -> doc().
set_rules_test(Doc, RulesTest) ->
    kz_json:set_value([<<"rules_test">>], RulesTest, Doc).

-spec weight_cost(doc()) -> integer().
weight_cost(Doc) ->
    weight_cost(Doc, 50).

-spec weight_cost(doc(), Default) -> integer() | Default.
weight_cost(Doc, Default) ->
    kz_json:get_integer_value([<<"weight_cost">>], Doc, Default).

-spec set_weight_cost(doc(), integer()) -> doc().
set_weight_cost(Doc, WeightCost) ->
    kz_json:set_value([<<"weight_cost">>], WeightCost, Doc).

-spec media_fax_option(doc()) -> boolean().
media_fax_option(Doc) ->
    media_fax_option(Doc, 'false').

-spec media_fax_option(doc(), Default) -> boolean() | Default.
media_fax_option(Doc, Default) ->
    kz_json:is_true([<<"media">>, <<"fax_option">>], Doc, Default).

-spec media_bypass_media(doc()) -> boolean().
media_bypass_media(Doc) ->
    media_bypass_media(Doc, 'false').

-spec media_bypass_media(doc(), Default) -> boolean() | Default.
media_bypass_media(Doc, Default) ->
    kz_json:is_true([<<"media">>, <<"bypass_media">>], Doc, Default).

-spec media_audio_codecs(doc()) -> kz_term:api_ne_binaries().
media_audio_codecs(Doc) ->
    media_audio_codecs(Doc, 'undefined').

-spec media_audio_codecs(doc(), Default) -> kz_term:ne_binaries() | Default.
media_audio_codecs(Doc, Default) ->
    kz_json:get_list_value([<<"media">>, <<"audio">>, <<"codecs">>], Doc, Default).

-spec media_video_codecs(doc()) -> kz_term:api_ne_binaries().
media_video_codecs(Doc) ->
    media_video_codecs(Doc, 'undefined').

-spec media_video_codecs(doc(), Default) -> kz_term:ne_binaries() | Default.
media_video_codecs(Doc, Default) ->
    kz_json:get_list_value([<<"media">>, <<"video">>, <<"codecs">>], Doc, Default).
