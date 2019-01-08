%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Rate document accessors
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_rate).

-export([account_id/1, account_id/2
        ,carrier/1, carrier/2, set_carrier/2
        ,description/1, description/2, set_description/2
        ,direction/1, direction/2, set_direction/2
        ,increment/1, increment/2, set_increment/2
        ,iso_country_code/1, iso_country_code/2, set_iso_country_code/2
        ,minimum/1, minimum/2, set_minimum/2
        ,name/1, name/2, set_name/2
        ,no_charge/1, no_charge/2
        ,options/1, options/2, set_options/2
        ,prefix/1, prefix/2, set_prefix/2
        ,private_cost/1, private_cost/2, set_private_cost/2
        ,private_surcharge/1, private_surcharge/2, set_private_surcharge/2
        ,rate_cost/1, rate_cost/2, set_rate_cost/2
        ,ratedeck/1, ratedeck/2, set_ratedeck/2
        ,routes/1, routes/2, set_routes/2
        ,caller_id_numbers/1, caller_id_numbers/2, set_caller_id_numbers/2
        ,surcharge/1, surcharge/2, set_surcharge/2
        ,type/0, type/1, set_type/1
        ,version/1, version/2
        ,weight/1, weight/2, set_weight/2, constrain_weight/1

        ,from_map/1
        ]).

-include("kz_documents.hrl").

-define(KEY_DIRECTION, <<"direction">>).

-type doc() :: kz_json:object().
-type docs() :: [doc()].
-type weight_range() :: 1..100.
-export_type([doc/0, docs/0
             ,weight_range/0
             ]).

-spec from_map(map()) -> doc().
from_map(Map) ->
    Rate = kz_doc:public_fields(kz_json:from_map(Map)),
    Fs = [fun set_type/1
         ,fun ensure_id/1
         ,fun maybe_fix_direction/1
         ],
    lists:foldl(fun(F, R) -> F(R) end, Rate, Fs).

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
    ensure_id(Rate, kz_json:get_ne_binary_value(<<"_id">>, Rate)).

-spec ensure_id(doc(), kz_term:api_ne_binary()) -> doc().
ensure_id(Rate, 'undefined') ->
    ID = list_to_binary([iso_country_code(Rate, <<"XX">>)
                        ,<<"-">>
                        ,prefix(Rate)
                        ,rate_suffix(Rate)
                        ]),
    kz_doc:set_id(Rate, ID);
ensure_id(Rate, ID) ->
    kz_doc:set_id(Rate, ID).

-spec minimum(doc()) -> integer().
minimum(Rate) ->
    minimum(Rate, 0).

-spec minimum(doc(), integer()) -> integer().
minimum(Rate, Default) ->
    kz_json:get_integer_value(<<"minimum">>, Rate, Default).

-spec set_minimum(doc(), integer()) -> doc().
set_minimum(Rate, Minimum) when is_integer(Minimum) ->
    kz_json:set_value(<<"minimum">>, Minimum, Rate).

-spec increment(doc()) -> integer().
increment(Rate) ->
    increment(Rate, 0).

-spec increment(doc(), integer()) -> integer().
increment(Rate, Default) ->
    kz_json:get_integer_value(<<"rate_increment">>, Rate, Default).

-spec set_increment(doc(), integer()) -> doc().
set_increment(Rate, Increment) when is_integer(Increment) ->
    kz_json:set_value(<<"rate_increment">>, Increment, Rate).

-spec no_charge(doc()) -> integer().
no_charge(Rate) ->
    no_charge(Rate, 0).

-spec no_charge(doc(), integer()) -> integer().
no_charge(Rate, Default) ->
    kz_json:get_integer_value(<<"rate_nocharge_time">>, Rate, Default).

-spec surcharge(doc()) -> kz_transaction:units().
surcharge(Rate) ->
    surcharge(Rate, 0.0).

-spec surcharge(doc(), float()) -> kz_transaction:units().
surcharge(Rate, Default) ->
    Surcharge = kz_json:get_float_value(<<"rate_surcharge">>, Rate, Default),
    wht_util:dollars_to_units(Surcharge).

-spec set_surcharge(doc(), float()) -> doc().
set_surcharge(Rate, Surcharge) when is_float(Surcharge) ->
    kz_json:set_value(<<"rate_surcharge">>, Surcharge, Rate).

-spec private_surcharge(doc()) -> kz_transaction:units().
private_surcharge(Rate) ->
    private_surcharge(Rate, 0.0).

-spec private_surcharge(doc(), float()) -> kz_transaction:units().
private_surcharge(Rate, Default) ->
    Surcharge = kz_json:get_float_value(<<"pvt_rate_surcharge">>, Rate, Default),
    wht_util:dollars_to_units(Surcharge).

-spec set_private_surcharge(doc(), float()) -> doc().
set_private_surcharge(Rate, Surcharge) when is_float(Surcharge) ->
    kz_json:set_value(<<"pvt_rate_surcharge">>, Surcharge, Rate).

-spec rate_cost(doc()) -> kz_transaction:units().
rate_cost(Rate) ->
    rate_cost(Rate, 0.0).

-spec rate_cost(doc(), float()) -> kz_transaction:units().
rate_cost(Rate, Default) ->
    Cost = kz_json:get_float_value(<<"rate_cost">>, Rate, Default),
    wht_util:dollars_to_units(Cost).

-spec set_rate_cost(doc(), float()) -> doc().
set_rate_cost(Rate, Cost) when is_float(Cost) ->
    kz_json:set_value(<<"rate_cost">>, Cost, Rate).

-spec private_cost(doc()) -> kz_transaction:units().
private_cost(Rate) ->
    private_cost(Rate, 0.0).

-spec private_cost(doc(), float()) -> kz_transaction:units().
private_cost(Rate, Default) ->
    Cost = kz_json:get_float_value(<<"pvt_internal_rate_cost">>, Rate, Default),
    wht_util:dollars_to_units(Cost).

-spec set_private_cost(doc(), float()) -> doc().
set_private_cost(Rate, Cost) when is_float(Cost) ->
    kz_json:set_value(<<"pvt_internal_rate_cost">>, Cost, Rate).

-spec version(doc()) -> kz_term:api_ne_binary().
version(Rate) ->
    version(Rate, 'undefined').

-spec version(doc(), Default) -> kz_term:ne_binary() | Default.
version(Rate, Default) ->
    kz_json:get_ne_binary_value(<<"rate_version">>, Rate, Default).

-spec prefix(doc()) -> kz_term:api_ne_binary().
prefix(Rate) ->
    prefix(Rate, 'undefined').

-spec prefix(doc(), Default) -> kz_term:ne_binary() | Default.
prefix(Rate, Default) ->
    kz_json:get_ne_binary_value(<<"prefix">>, Rate, Default).

-spec set_prefix(doc(), kz_term:ne_binary()) -> doc().
set_prefix(Rate, Prefix) when is_binary(Prefix) ->
    kz_json:set_value(<<"prefix">>, Prefix, Rate).

-spec name(doc()) -> kz_term:api_ne_binary().
name(Rate) ->
    name(Rate, 'undefined').

-spec name(doc(), Default) -> kz_term:ne_binary() | Default.
name(Rate, Default) ->
    kz_json:get_ne_binary_value(<<"rate_name">>, Rate, Default).

-spec set_name(doc(), kz_term:ne_binary()) -> doc().
set_name(Rate, ?NE_BINARY = Name) ->
    kz_json:set_value(<<"rate_name">>, Name, Rate).

-spec ratedeck(doc()) -> kz_term:api_ne_binary().
ratedeck(Rate) ->
    ratedeck(Rate, 'undefined').

-spec ratedeck(doc(), Default) -> kz_term:ne_binary() | Default.
ratedeck(Rate, Default) ->
    kz_json:get_ne_binary_value(<<"ratedeck_id">>, Rate, Default).

-spec set_ratedeck(doc(), kz_term:ne_binary()) -> doc().
set_ratedeck(Rate, Deck) ->
    kz_json:set_value(<<"ratedeck_id">>, Deck, Rate).

-spec description(doc()) -> kz_term:api_ne_binary().
description(Rate) ->
    description(Rate, 'undefined').

-spec description(doc(), Default) -> kz_term:ne_binary() | Default.
description(Rate, Default) ->
    kz_json:get_ne_binary_value(<<"description">>, Rate, Default).

-spec set_description(doc(), kz_term:ne_binary()) -> doc().
set_description(Rate, Description) when is_binary(Description) ->
    kz_json:set_value(<<"description">>, Description, Rate).

-spec weight(doc()) -> pos_integer().
weight(Rate) ->
    weight(Rate, 1).

-spec weight(doc(), Default) -> pos_integer() | Default.
weight(Rate, Default) ->
    case kz_json:get_integer_value(<<"weight">>, Rate, Default) of
        Default -> Default;
        Weight -> constrain_weight(Weight)
    end.

-spec set_weight(doc(), integer()) -> doc().
set_weight(Rate, Weight) when is_integer(Weight) ->
    kz_json:set_value(<<"weight">>, constrain_weight(Weight), Rate).

-spec constrain_weight(kz_term:text() | integer()) -> weight_range().
constrain_weight(X) when not is_integer(X) -> constrain_weight(kz_term:to_integer(X));
constrain_weight(X) when X =< 0 -> 1;
constrain_weight(X) when X >= 100 -> 100;
constrain_weight(X) -> X.

-define(BOTH_DIRECTIONS, [<<"inbound">>, <<"outbound">>]).

-spec direction(doc()) -> kz_term:ne_binaries().
direction(Rate) ->
    direction(Rate, ?BOTH_DIRECTIONS).

-spec direction(doc(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
direction(Rate, Default) ->
    case kz_json:get_value(<<"direction">>, Rate) of
        <<"inbound">>=V -> [V];
        <<"outbound">>=V -> [V];
        'undefined' -> Default;
        Directions when is_list(Directions) -> Directions
    end.

-spec set_direction(doc(), kz_term:ne_binary() | kz_term:ne_binaries()) -> doc().
set_direction(Rate, Directions) when is_list(Directions) ->
    kz_json:set_value(<<"direction">>, Directions, Rate);
set_direction(Rate, Direction) when is_binary(Direction) ->
    set_direction(Rate, [Direction]).

-spec options(doc()) -> kz_term:ne_binaries().
options(Rate) ->
    options(Rate, []).

-spec options(doc(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
options(Rate, Default) ->
    kz_json:get_list_value(<<"options">>, Rate, Default).

-spec set_options(doc(), kz_term:ne_binaries()) -> doc().
set_options(Rate, Options) when is_list(Options) ->
    kz_json:set_value(<<"options">>, Options, Rate).

-spec caller_id_numbers(doc()) -> kz_term:ne_binaries().
caller_id_numbers(Rate) ->
    caller_id_numbers(Rate, []).

-spec caller_id_numbers(doc(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
caller_id_numbers(Rate, Default) ->
    kz_json:get_list_value(<<"caller_id_numbers">>, Rate, Default).

-spec set_caller_id_numbers(doc(), kz_term:ne_binaries()) -> doc().
set_caller_id_numbers(Rate, CID_Numbers) when is_list(CID_Numbers) ->
    kz_json:set_value(<<"caller_id_numbers">>, CID_Numbers, Rate).

-spec routes(doc()) -> kz_term:ne_binaries().
routes(Rate) ->
    routes(Rate, []).

-spec routes(doc(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
routes(Rate, Default) ->
    kz_json:get_list_value(<<"routes">>, Rate, Default).

-spec set_routes(doc(), kz_term:ne_binaries()) -> doc().
set_routes(Rate, Routes) when is_list(Routes) ->
    kz_json:set_value(<<"routes">>, Routes, Rate).

-spec account_id(doc()) -> kz_term:api_ne_binary().
account_id(Rate) ->
    account_id(Rate, 'undefined').

-spec account_id(doc(), Default) -> kz_term:ne_binary() | Default.
account_id(Rate, Default) ->
    kz_json:get_ne_binary_value(<<"account_id">>, Rate, Default).

-spec iso_country_code(doc()) -> kz_term:api_ne_binary().
iso_country_code(Rate) ->
    iso_country_code(Rate, 'undefined').

-spec iso_country_code(doc(), Default) -> kz_term:ne_binary() | Default.
iso_country_code(Rate, Default) ->
    kz_json:get_ne_binary_value(<<"iso_country_code">>, Rate, Default).

-spec set_iso_country_code(doc(), kz_term:ne_binary()) -> doc().
set_iso_country_code(Rate, Code) when is_binary(Code) ->
    kz_json:set_value(<<"iso_country_code">>, Code, Rate).

-spec type() -> kz_term:ne_binary().
type() -> <<"rate">>.

-spec type(doc()) -> kz_term:ne_binary().
type(Doc) -> kz_doc:type(Doc, type()).

-spec set_type(doc()) -> doc().
set_type(Doc) -> kz_doc:set_type(Doc, type()).

-spec carrier(doc()) -> kz_term:ne_binary().
carrier(Rate) ->
    carrier(Rate, <<"default">>).

-spec carrier(doc(), Default) -> kz_term:ne_binary() | Default.
carrier(Rate, Default) ->
    kz_json:get_ne_binary_value(<<"pvt_carrier">>, Rate, Default).

-spec set_carrier(doc(), kz_term:ne_binary()) -> doc().
set_carrier(Rate, Carrier) when is_binary(Carrier) ->
    kz_json:set_value(<<"pvt_carrier">>, Carrier, Rate).

-spec rate_suffix(doc()) -> kz_term:ne_binary().
rate_suffix(Rate) ->
    rate_suffix(Rate, 'undefined').

-spec rate_suffix(doc(), Default) -> kz_term:ne_binary() | Default.
rate_suffix(Rate, Default) ->
    case kz_json:get_ne_binary_value(<<"rate_suffix">>, Rate, Default) of
        'undefined' -> <<>>;
        RateSuffix -> <<"-", RateSuffix/binary>>
    end.
