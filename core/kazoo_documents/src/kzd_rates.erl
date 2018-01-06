-module(kzd_rates).

-export([new/0]).
-export([account_id/1, account_id/2, set_account_id/2]).
-export([carrier/1, carrier/2, set_carrier/2]).
-export([description/1, description/2, set_description/2]).
-export([direction/1, direction/2, set_direction/2]).
-export([internal_rate_cost/1, internal_rate_cost/2, set_internal_rate_cost/2]).
-export([iso_country_code/1, iso_country_code/2, set_iso_country_code/2]).
-export([options/1, options/2, set_options/2]).
-export([prefix/1, prefix/2, set_prefix/2]).
-export([rate_cost/1, rate_cost/2, set_rate_cost/2]).
-export([rate_increment/1, rate_increment/2, set_rate_increment/2]).
-export([rate_minimum/1, rate_minimum/2, set_rate_minimum/2]).
-export([rate_name/1, rate_name/2, set_rate_name/2]).
-export([rate_nocharge_time/1, rate_nocharge_time/2, set_rate_nocharge_time/2]).
-export([rate_surcharge/1, rate_surcharge/2, set_rate_surcharge/2]).
-export([rate_version/1, rate_version/2, set_rate_version/2]).
-export([ratedeck_id/1, ratedeck_id/2, set_ratedeck_id/2]).
-export([routes/1, routes/2, set_routes/2]).
-export([weight/1, weight/2, set_weight/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec account_id(doc()) -> api_binary().
-spec account_id(doc(), Default) -> binary() | Default.
account_id(Doc) ->
    account_id(Doc, 'undefined').
account_id(Doc, Default) ->
    kz_json:get_binary_value(<<"account_id">>, Doc, Default).

-spec set_account_id(doc(), binary()) -> doc().
set_account_id(Doc, AccountId) ->
    kz_json:set_value(<<"account_id">>, AccountId, Doc).

-spec carrier(doc()) -> api_binary().
-spec carrier(doc(), Default) -> binary() | Default.
carrier(Doc) ->
    carrier(Doc, 'undefined').
carrier(Doc, Default) ->
    kz_json:get_binary_value(<<"carrier">>, Doc, Default).

-spec set_carrier(doc(), binary()) -> doc().
set_carrier(Doc, Carrier) ->
    kz_json:set_value(<<"carrier">>, Carrier, Doc).

-spec description(doc()) -> api_binary().
-spec description(doc(), Default) -> binary() | Default.
description(Doc) ->
    description(Doc, 'undefined').
description(Doc, Default) ->
    kz_json:get_binary_value(<<"description">>, Doc, Default).

-spec set_description(doc(), binary()) -> doc().
set_description(Doc, Description) ->
    kz_json:set_value(<<"description">>, Description, Doc).

-spec direction(doc()) -> api_ne_binaries().
-spec direction(doc(), Default) -> ne_binaries() | Default.
direction(Doc) ->
    direction(Doc, 'undefined').
direction(Doc, Default) ->
    kz_json:get_list_value(<<"direction">>, Doc, Default).

-spec set_direction(doc(), ne_binaries()) -> doc().
set_direction(Doc, Direction) ->
    kz_json:set_value(<<"direction">>, Direction, Doc).

-spec internal_rate_cost(doc()) -> any().
-spec internal_rate_cost(doc(), Default) -> any() | Default.
internal_rate_cost(Doc) ->
    internal_rate_cost(Doc, 'undefined').
internal_rate_cost(Doc, Default) ->
    kz_json:get_value(<<"internal_rate_cost">>, Doc, Default).

-spec set_internal_rate_cost(doc(), any()) -> doc().
set_internal_rate_cost(Doc, InternalRateCost) ->
    kz_json:set_value(<<"internal_rate_cost">>, InternalRateCost, Doc).

-spec iso_country_code(doc()) -> api_binary().
-spec iso_country_code(doc(), Default) -> binary() | Default.
iso_country_code(Doc) ->
    iso_country_code(Doc, 'undefined').
iso_country_code(Doc, Default) ->
    kz_json:get_binary_value(<<"iso_country_code">>, Doc, Default).

-spec set_iso_country_code(doc(), binary()) -> doc().
set_iso_country_code(Doc, IsoCountryCode) ->
    kz_json:set_value(<<"iso_country_code">>, IsoCountryCode, Doc).

-spec options(doc()) -> api_ne_binaries().
-spec options(doc(), Default) -> ne_binaries() | Default.
options(Doc) ->
    options(Doc, 'undefined').
options(Doc, Default) ->
    kz_json:get_list_value(<<"options">>, Doc, Default).

-spec set_options(doc(), ne_binaries()) -> doc().
set_options(Doc, Options) ->
    kz_json:set_value(<<"options">>, Options, Doc).

-spec prefix(doc()) -> api_integer().
-spec prefix(doc(), Default) -> integer() | Default.
prefix(Doc) ->
    prefix(Doc, 'undefined').
prefix(Doc, Default) ->
    kz_json:get_integer_value(<<"prefix">>, Doc, Default).

-spec set_prefix(doc(), integer()) -> doc().
set_prefix(Doc, Prefix) ->
    kz_json:set_value(<<"prefix">>, Prefix, Doc).

-spec rate_cost(doc()) -> any().
-spec rate_cost(doc(), Default) -> any() | Default.
rate_cost(Doc) ->
    rate_cost(Doc, 'undefined').
rate_cost(Doc, Default) ->
    kz_json:get_value(<<"rate_cost">>, Doc, Default).

-spec set_rate_cost(doc(), any()) -> doc().
set_rate_cost(Doc, RateCost) ->
    kz_json:set_value(<<"rate_cost">>, RateCost, Doc).

-spec rate_increment(doc()) -> api_integer().
-spec rate_increment(doc(), Default) -> integer() | Default.
rate_increment(Doc) ->
    rate_increment(Doc, 'undefined').
rate_increment(Doc, Default) ->
    kz_json:get_integer_value(<<"rate_increment">>, Doc, Default).

-spec set_rate_increment(doc(), integer()) -> doc().
set_rate_increment(Doc, RateIncrement) ->
    kz_json:set_value(<<"rate_increment">>, RateIncrement, Doc).

-spec rate_minimum(doc()) -> api_integer().
-spec rate_minimum(doc(), Default) -> integer() | Default.
rate_minimum(Doc) ->
    rate_minimum(Doc, 'undefined').
rate_minimum(Doc, Default) ->
    kz_json:get_integer_value(<<"rate_minimum">>, Doc, Default).

-spec set_rate_minimum(doc(), integer()) -> doc().
set_rate_minimum(Doc, RateMinimum) ->
    kz_json:set_value(<<"rate_minimum">>, RateMinimum, Doc).

-spec rate_name(doc()) -> api_binary().
-spec rate_name(doc(), Default) -> binary() | Default.
rate_name(Doc) ->
    rate_name(Doc, 'undefined').
rate_name(Doc, Default) ->
    kz_json:get_binary_value(<<"rate_name">>, Doc, Default).

-spec set_rate_name(doc(), binary()) -> doc().
set_rate_name(Doc, RateName) ->
    kz_json:set_value(<<"rate_name">>, RateName, Doc).

-spec rate_nocharge_time(doc()) -> api_integer().
-spec rate_nocharge_time(doc(), Default) -> integer() | Default.
rate_nocharge_time(Doc) ->
    rate_nocharge_time(Doc, 'undefined').
rate_nocharge_time(Doc, Default) ->
    kz_json:get_integer_value(<<"rate_nocharge_time">>, Doc, Default).

-spec set_rate_nocharge_time(doc(), integer()) -> doc().
set_rate_nocharge_time(Doc, RateNochargeTime) ->
    kz_json:set_value(<<"rate_nocharge_time">>, RateNochargeTime, Doc).

-spec rate_surcharge(doc()) -> any().
-spec rate_surcharge(doc(), Default) -> any() | Default.
rate_surcharge(Doc) ->
    rate_surcharge(Doc, 'undefined').
rate_surcharge(Doc, Default) ->
    kz_json:get_value(<<"rate_surcharge">>, Doc, Default).

-spec set_rate_surcharge(doc(), any()) -> doc().
set_rate_surcharge(Doc, RateSurcharge) ->
    kz_json:set_value(<<"rate_surcharge">>, RateSurcharge, Doc).

-spec rate_version(doc()) -> api_binary().
-spec rate_version(doc(), Default) -> binary() | Default.
rate_version(Doc) ->
    rate_version(Doc, 'undefined').
rate_version(Doc, Default) ->
    kz_json:get_binary_value(<<"rate_version">>, Doc, Default).

-spec set_rate_version(doc(), binary()) -> doc().
set_rate_version(Doc, RateVersion) ->
    kz_json:set_value(<<"rate_version">>, RateVersion, Doc).

-spec ratedeck_id(doc()) -> api_binary().
-spec ratedeck_id(doc(), Default) -> binary() | Default.
ratedeck_id(Doc) ->
    ratedeck_id(Doc, 'undefined').
ratedeck_id(Doc, Default) ->
    kz_json:get_binary_value(<<"ratedeck_id">>, Doc, Default).

-spec set_ratedeck_id(doc(), binary()) -> doc().
set_ratedeck_id(Doc, RatedeckId) ->
    kz_json:set_value(<<"ratedeck_id">>, RatedeckId, Doc).

-spec routes(doc()) -> api_ne_binaries().
-spec routes(doc(), Default) -> ne_binaries() | Default.
routes(Doc) ->
    routes(Doc, 'undefined').
routes(Doc, Default) ->
    kz_json:get_list_value(<<"routes">>, Doc, Default).

-spec set_routes(doc(), ne_binaries()) -> doc().
set_routes(Doc, Routes) ->
    kz_json:set_value(<<"routes">>, Routes, Doc).

-spec weight(doc()) -> api_integer().
-spec weight(doc(), Default) -> integer() | Default.
weight(Doc) ->
    weight(Doc, 'undefined').
weight(Doc, Default) ->
    kz_json:get_integer_value(<<"weight">>, Doc, Default).

-spec set_weight(doc(), integer()) -> doc().
set_weight(Doc, Weight) ->
    kz_json:set_value(<<"weight">>, Weight, Doc).
