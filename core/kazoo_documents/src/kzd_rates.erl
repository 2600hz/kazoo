%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_rates).

-export([new/0]).
-export([account_id/1, account_id/2, set_account_id/2]).
-export([caller_id_numbers/1, caller_id_numbers/2, set_caller_id_numbers/2]).
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
-export([rate_suffix/1, rate_suffix/2, set_rate_suffix/2]).
-export([rate_surcharge/1, rate_surcharge/2, set_rate_surcharge/2]).
-export([rate_version/1, rate_version/2, set_rate_version/2]).
-export([ratedeck_id/1, ratedeck_id/2, set_ratedeck_id/2]).
-export([routes/1, routes/2, set_routes/2]).
-export([weight/1, weight/2, set_weight/2]).

-export([from_map/1, from_json/1]).
-export([type/0, type/1, set_type/1]).
-export([constrain_weight/1]).
-export([private_cost/1, private_cost/2, set_private_cost/2]).
-export([private_surcharge/1, private_surcharge/2, set_private_surcharge/2]).
-export([default_routes/1, set_default_route/1]).

-include("kz_documents.hrl").
-define(KEY_DIRECTION, <<"direction">>).

-type doc() :: kz_json:object().
-type docs() :: [doc()].
-type weight_range() :: 1..100.
-export_type([doc/0, docs/0
             ,weight_range/0
             ]).

-define(SCHEMA, <<"rates">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec account_id(doc()) -> kz_term:api_binary().
account_id(Doc) ->
    account_id(Doc, 'undefined').

-spec account_id(doc(), Default) -> binary() | Default.
account_id(Doc, Default) ->
    kz_json:get_binary_value([<<"account_id">>], Doc, Default).

-spec set_account_id(doc(), binary()) -> doc().
set_account_id(Doc, AccountId) ->
    kz_json:set_value([<<"account_id">>], AccountId, Doc).

-spec caller_id_numbers(doc()) -> kz_term:api_ne_binary().
caller_id_numbers(Doc) ->
    caller_id_numbers(Doc, 'undefined').

-spec caller_id_numbers(doc(), Default) -> kz_term:ne_binary() | Default.
caller_id_numbers(Doc, Default) ->
    kz_json:get_binary_value([<<"caller_id_numbers">>], Doc, Default).

-spec set_caller_id_numbers(doc(), kz_term:ne_binary()) -> doc().
set_caller_id_numbers(Doc, <<CallerIdNumbers/binary>>) ->
    kz_json:set_value([<<"caller_id_numbers">>], CallerIdNumbers, Doc).

-spec carrier(doc()) -> kz_term:api_binary().
carrier(Doc) ->
    carrier(Doc, 'undefined').

-spec carrier(doc(), Default) -> binary() | Default.
carrier(Doc, Default) ->
    kz_json:get_binary_value([<<"carrier">>], Doc, Default).

-spec set_carrier(doc(), binary()) -> doc().
set_carrier(Doc, Carrier) ->
    kz_json:set_value([<<"carrier">>], Carrier, Doc).

-spec description(doc()) -> kz_term:api_binary().
description(Doc) ->
    description(Doc, 'undefined').

-spec description(doc(), Default) -> binary() | Default.
description(Doc, Default) ->
    kz_json:get_binary_value([<<"description">>], Doc, Default).

-spec set_description(doc(), binary()) -> doc().
set_description(Doc, Description) ->
    kz_json:set_value([<<"description">>], Description, Doc).

-define(BOTH_DIRECTIONS, [<<"inbound">>, <<"outbound">>]).

-spec direction(doc()) -> kz_term:ne_binaries().
direction(Doc) ->
    direction(Doc, ?BOTH_DIRECTIONS).

-spec direction(doc(), Default) -> kz_term:ne_binaries() | Default.
direction(Doc, Default) ->
    case kz_json:get_value([<<"direction">>], Doc) of
        'undefined' -> Default;
        <<"inbound">>=V -> [V];
        <<"outbound">>=V -> [V];
        Directions when is_list(Directions) -> Directions
    end.

-spec set_direction(doc(), kz_term:ne_binaries() | kz_term:ne_binary()) -> doc().
set_direction(Doc, Directions) when is_list(Directions) ->
    kz_json:set_value([<<"direction">>], Directions, Doc);
set_direction(Doc, Direction) when is_binary(Direction) ->
    set_direction(Doc, [Direction]).

-spec internal_rate_cost(doc()) -> kz_term:api_number().
internal_rate_cost(Doc) ->
    internal_rate_cost(Doc, 'undefined').

-spec internal_rate_cost(doc(), Default) -> number() | Default.
internal_rate_cost(Doc, Default) ->
    kz_json:get_float_value([<<"internal_rate_cost">>], Doc, Default).

-spec set_internal_rate_cost(doc(), number()) -> doc().
set_internal_rate_cost(Doc, InternalRateCost) ->
    kz_json:set_value([<<"internal_rate_cost">>], InternalRateCost, Doc).

-spec iso_country_code(doc()) -> kz_term:api_binary().
iso_country_code(Doc) ->
    iso_country_code(Doc, 'undefined').

-spec iso_country_code(doc(), Default) -> binary() | Default.
iso_country_code(Doc, Default) ->
    kz_json:get_binary_value([<<"iso_country_code">>], Doc, Default).

-spec set_iso_country_code(doc(), binary()) -> doc().
set_iso_country_code(Doc, IsoCountryCode) ->
    kz_json:set_value([<<"iso_country_code">>], IsoCountryCode, Doc).

-spec options(doc()) -> kz_term:api_ne_binaries().
options(Doc) ->
    options(Doc, 'undefined').

-spec options(doc(), Default) -> kz_term:ne_binaries() | Default.
options(Doc, Default) ->
    kz_json:get_list_value([<<"options">>], Doc, Default).

-spec set_options(doc(), kz_term:ne_binaries()) -> doc().
set_options(Doc, Options) ->
    kz_json:set_value([<<"options">>], Options, Doc).

-spec prefix(doc()) -> kz_term:api_integer().
prefix(Doc) ->
    prefix(Doc, 'undefined').

-spec prefix(doc(), Default) -> integer() | Default.
prefix(Doc, Default) ->
    kz_json:get_integer_value([<<"prefix">>], Doc, Default).

-spec set_prefix(doc(), integer()) -> doc().
set_prefix(Doc, Prefix) ->
    kz_json:set_value([<<"prefix">>], Prefix, Doc).

-spec rate_cost(doc()) -> kz_term:api_number().
rate_cost(Doc) ->
    rate_cost(Doc, 'undefined').

-spec rate_cost(doc(), Default) -> number() | Default.
rate_cost(Doc, Default) ->
    kz_json:get_float_value([<<"rate_cost">>], Doc, Default).

-spec set_rate_cost(doc(), number()) -> doc().
set_rate_cost(Doc, RateCost) ->
    kz_json:set_value([<<"rate_cost">>], RateCost, Doc).

-spec rate_increment(doc()) -> kz_term:api_integer().
rate_increment(Doc) ->
    rate_increment(Doc, 'undefined').

-spec rate_increment(doc(), Default) -> integer() | Default.
rate_increment(Doc, Default) ->
    kz_json:get_integer_value([<<"rate_increment">>], Doc, Default).

-spec set_rate_increment(doc(), integer()) -> doc().
set_rate_increment(Doc, RateIncrement) ->
    kz_json:set_value([<<"rate_increment">>], RateIncrement, Doc).

-spec rate_minimum(doc()) -> kz_term:api_integer().
rate_minimum(Doc) ->
    rate_minimum(Doc, 'undefined').

-spec rate_minimum(doc(), Default) -> integer() | Default.
rate_minimum(Doc, Default) ->
    kz_json:get_integer_value([<<"rate_minimum">>], Doc, Default).

-spec set_rate_minimum(doc(), integer()) -> doc().
set_rate_minimum(Doc, RateMinimum) ->
    kz_json:set_value([<<"rate_minimum">>], RateMinimum, Doc).

-spec rate_name(doc()) -> kz_term:api_binary().
rate_name(Doc) ->
    rate_name(Doc, 'undefined').

-spec rate_name(doc(), Default) -> binary() | Default.
rate_name(Doc, Default) ->
    kz_json:get_binary_value([<<"rate_name">>], Doc, Default).

-spec set_rate_name(doc(), binary()) -> doc().
set_rate_name(Doc, RateName) ->
    kz_json:set_value([<<"rate_name">>], RateName, Doc).

-spec rate_nocharge_time(doc()) -> kz_term:api_integer().
rate_nocharge_time(Doc) ->
    rate_nocharge_time(Doc, 'undefined').

-spec rate_nocharge_time(doc(), Default) -> integer() | Default.
rate_nocharge_time(Doc, Default) ->
    kz_json:get_integer_value([<<"rate_nocharge_time">>], Doc, Default).

-spec set_rate_nocharge_time(doc(), integer()) -> doc().
set_rate_nocharge_time(Doc, RateNochargeTime) ->
    kz_json:set_value([<<"rate_nocharge_time">>], RateNochargeTime, Doc).

-spec rate_surcharge(doc()) -> number().
rate_surcharge(Doc) ->
    rate_surcharge(Doc, 0.0).

-spec rate_surcharge(doc(), Default) -> number() | Default.
rate_surcharge(Doc, Default) ->
    kz_json:get_float_value([<<"rate_surcharge">>], Doc, Default).

-spec set_rate_surcharge(doc(), number()) -> doc().
set_rate_surcharge(Doc, RateSurcharge) ->
    kz_json:set_value([<<"rate_surcharge">>], RateSurcharge, Doc).

-spec rate_version(doc()) -> kz_term:api_binary().
rate_version(Doc) ->
    rate_version(Doc, 'undefined').

-spec rate_version(doc(), Default) -> binary() | Default.
rate_version(Doc, Default) ->
    kz_json:get_binary_value([<<"rate_version">>], Doc, Default).

-spec set_rate_version(doc(), binary()) -> doc().
set_rate_version(Doc, RateVersion) ->
    kz_json:set_value([<<"rate_version">>], RateVersion, Doc).

-spec ratedeck_id(doc()) -> kz_term:api_binary().
ratedeck_id(Doc) ->
    ratedeck_id(Doc, 'undefined').

-spec ratedeck_id(doc(), Default) -> binary() | Default.
ratedeck_id(Doc, Default) ->
    kz_json:get_binary_value([<<"ratedeck_id">>], Doc, Default).

-spec set_ratedeck_id(doc(), binary()) -> doc().
set_ratedeck_id(Doc, RatedeckId) ->
    kz_json:set_value([<<"ratedeck_id">>], RatedeckId, Doc).

-spec routes(doc()) -> kz_term:api_ne_binaries().
routes(Doc) ->
    routes(Doc, 'undefined').

-spec routes(doc(), Default) -> kz_term:ne_binaries() | Default.
routes(Doc, Default) ->
    kz_json:get_list_value([<<"routes">>], Doc, Default).

-spec set_routes(doc(), kz_term:ne_binaries()) -> doc().
set_routes(Doc, Routes) ->
    kz_json:set_value([<<"routes">>], Routes, Doc).

-spec weight(doc()) -> kz_term:api_integer().
weight(Doc) ->
    weight(Doc, 'undefined').

-spec weight(doc(), Default) -> integer() | Default.
weight(Doc, Default) ->
    kz_json:get_integer_value([<<"weight">>], Doc, Default).

-spec set_weight(doc(), integer()) -> doc().
set_weight(Rate, Weight) when is_integer(Weight) ->
    kz_json:set_value(<<"weight">>, constrain_weight(Weight), Rate).

-spec constrain_weight(kz_term:text() | integer()) -> weight_range().
constrain_weight(X) when not is_integer(X) -> constrain_weight(kz_term:to_integer(X));
constrain_weight(X) when X =< 0 -> 1;
constrain_weight(X) when X >= 100 -> 100;
constrain_weight(X) -> X.

-spec from_json(kz_json:object()) -> doc().
from_json(JObj) ->
    Rate = kz_doc:public_fields(JObj),
    Fs = [fun set_type/1
         ,fun ensure_id/1
         ,fun maybe_fix_direction/1
         ],
    lists:foldl(fun(F, R) -> F(R) end, Rate, Fs).

-spec from_map(map()) -> doc().
from_map(Map) ->
    from_json(kz_json:from_map(Map)).

-spec maybe_fix_direction(doc()) -> doc().
maybe_fix_direction(Rate) ->
    case kz_json:get_value(?KEY_DIRECTION, Rate) of
        'undefined' -> Rate;
        L when is_list(L) -> Rate;
        <<"inbound">> -> set_direction(Rate, [<<"inbound">>]);
        <<"outbound">> -> set_direction(Rate, [<<"outbound">>])
    end.

-spec ensure_id(doc()) -> doc().
ensure_id(Rate) ->
    ensure_id(Rate, kz_doc:id(Rate)).

-spec ensure_id(doc(), kz_term:api_ne_binary()) -> doc().
ensure_id(Rate, 'undefined') ->
    ID = list_to_binary([iso_country_code(Rate, <<"XX">>)
                        ,<<"-">>
                        ,kz_term:to_binary(prefix(Rate))
                        ,rate_suffix(Rate)
                        ]),
    kz_doc:set_id(Rate, ID);
ensure_id(Rate, ID) ->
    kz_doc:set_id(Rate, ID).

-spec type() -> kz_term:ne_binary().
type() -> <<"rate">>.

-spec type(doc()) -> kz_term:ne_binary().
type(Doc) -> kz_doc:type(Doc, type()).

-spec set_type(doc()) -> doc().
set_type(Doc) -> kz_doc:set_type(Doc, type()).

-spec rate_suffix(doc()) -> kz_term:ne_binary().
rate_suffix(Rate) ->
    rate_suffix(Rate, 'undefined').

-spec rate_suffix(doc(), Default) -> kz_term:ne_binary() | Default.
rate_suffix(Rate, Default) ->
    case kz_json:get_ne_binary_value(<<"rate_suffix">>, Rate, Default) of
        'undefined' -> <<>>;
        RateSuffix -> <<"-", RateSuffix/binary>>
    end.

-spec set_rate_suffix(doc(), kz_term:ne_binary()) -> doc().
set_rate_suffix(Rate, Suffix) ->
    kz_json:set_value(<<"rate_suffix">>, Suffix, Rate).

-spec private_cost(doc()) -> kz_currency:units().
private_cost(Rate) ->
    private_cost(Rate, 0.0).

-spec private_cost(doc(), float()) -> kz_currency:units().
private_cost(Rate, Default) ->
    Cost = kz_json:get_float_value(<<"pvt_internal_rate_cost">>, Rate, Default),
    kz_currency:dollars_to_units(Cost).

-spec set_private_cost(doc(), float()) -> doc().
set_private_cost(Rate, Cost) when is_float(Cost) ->
    kz_json:set_value(<<"pvt_internal_rate_cost">>, Cost, Rate).

-spec private_surcharge(doc()) -> kz_currency:units().
private_surcharge(Rate) ->
    private_surcharge(Rate, 0.0).

-spec private_surcharge(doc(), float()) -> kz_currency:units().
private_surcharge(Rate, Default) ->
    Surcharge = kz_json:get_float_value(<<"pvt_rate_surcharge">>, Rate, Default),
    kz_currency:dollars_to_units(Surcharge).

-spec set_private_surcharge(doc(), float()) -> doc().
set_private_surcharge(Rate, Surcharge) when is_float(Surcharge) ->
    kz_json:set_value(<<"pvt_rate_surcharge">>, Surcharge, Rate).

-spec set_default_route(doc()) -> doc().
set_default_route(Rate) ->
    set_default_route(Rate, prefix(Rate)).

-spec set_default_route(doc(), integer()) -> doc().
set_default_route(Rate, Prefix) ->
    set_routes(Rate, default_routes(Prefix)).

-spec default_routes(doc() | integer() | kz_term:ne_binary()) -> kz_term:ne_binaries().
default_routes(<<Prefix/binary>>) ->
    [<<"^\\+?", Prefix/binary, ".+\$">>];
default_routes(Prefix) when is_integer(Prefix) ->
    default_routes(kz_term:to_binary(Prefix));
default_routes(Rate) ->
    default_routes(prefix(Rate)).
