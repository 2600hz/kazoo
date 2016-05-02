%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kzd_item_plan).

-export([minimum/1, minimum/2
         ,flat_rates/1, flat_rates/2
         ,rates/1, rates/2
         ,rate/1, rate/2
         ,exceptions/1, exceptions/2
         ,should_cascade/1, should_cascade/2
         ,masquerade_as/1, masquerade_as/2
         ,name/1
         ,discounts/1, discounts/2
         ,single_discount/1, single_discount/2
         ,cumulative_discount/1, cumulative_discount/2
         ,activation_charge/1, activation_charge/2
         ,is_enabled/1
         ,keys/1
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-type api_doc() :: api_object().
-export_type([doc/0
              ,api_doc/0
             ]).

-define(ACTIVATION_CHARGE, <<"activation_charge">>).
-define(MINIMUM, <<"minimum">>).
-define(FLAT_RATES, <<"flat_rates">>).
-define(RATES, <<"rates">>).
-define(RATE, <<"rate">>).
-define(EXCEPTIONS, <<"exceptions">>).
-define(CASCADE, <<"cascade">>).
-define(MASQUERADE, <<"as">>).
-define(NAME, <<"name">>).
-define(DISCOUNTS, <<"discounts">>).
-define(SINGLE, <<"single">>).
-define(CUMULATIVE, <<"cumulative">>).
-define(ENABLED, <<"enabled">>).

-spec keys(doc()) -> kz_json:keys().
keys(ItemPlan) ->
    kz_json:get_keys(ItemPlan).

-spec minimum(doc()) -> integer().
-spec minimum(doc(), Default) -> integer() | Default.
minimum(ItemPlan) ->
    minimum(ItemPlan, 0).
minimum(ItemPlan, Default) ->
    kz_json:get_integer_value(?MINIMUM, ItemPlan, Default).

-spec flat_rates(doc()) -> kz_json:object().
-spec flat_rates(doc(), Default) -> kz_json:object() | Default.
flat_rates(ItemPlan) ->
    flat_rates(ItemPlan, kz_json:new()).
flat_rates(ItemPlan, Default) ->
    kz_json:get_json_value(?FLAT_RATES, ItemPlan, Default).

-spec rates(doc()) -> kz_json:object().
-spec rates(doc(), Default) -> kz_json:object() | Default.
rates(ItemPlan) ->
    rates(ItemPlan, kz_json:new()).
rates(ItemPlan, Default) ->
    kz_json:get_json_value(?RATES, ItemPlan, Default).

-spec rate(doc()) -> api_float().
-spec rate(doc(), Default) -> float() | Default.
rate(ItemPlan) ->
    rate(ItemPlan, 'undefined').
rate(ItemPlan, Default) ->
    kz_json:get_float_value(?RATE, ItemPlan, Default).

-spec exceptions(doc()) -> ne_binaries().
-spec exceptions(doc(), Default) -> float() | Default.
exceptions(ItemPlan) ->
    exceptions(ItemPlan, []).
exceptions(ItemPlan, Default) ->
    kz_json:get_value(?EXCEPTIONS, ItemPlan, Default).

-spec should_cascade(doc()) -> boolean().
-spec should_cascade(doc(), Default) -> boolean() | Default.
should_cascade(ItemPlan) ->
    should_cascade(ItemPlan, 'false').
should_cascade(ItemPlan, Default) ->
    kz_json:is_true(?CASCADE, ItemPlan, Default).

-spec masquerade_as(doc()) -> api_binary().
-spec masquerade_as(doc(), Default) -> ne_binary() | Default.
masquerade_as(ItemPlan) ->
    masquerade_as(ItemPlan, 'undefined').
masquerade_as(ItemPlan, Default) ->
    kz_json:get_value(?MASQUERADE, ItemPlan, Default).

-spec name(doc()) -> api_binary().
name(ItemPlan) ->
    case kz_json:get_value(?NAME, ItemPlan) of
        'undefined' -> masquerade_as(ItemPlan);
        Name -> Name
    end.

-spec discounts(doc()) -> kz_json:object().
-spec discounts(doc(), Default) -> kz_json:object() | Default.
discounts(ItemPlan) ->
    discounts(ItemPlan, kz_json:new()).
discounts(ItemPlan, Default) ->
    kz_json:get_json_value(?DISCOUNTS, ItemPlan, Default).

-spec single_discount(doc()) -> api_object().
-spec single_discount(doc(), Default) -> kz_json:object() | Default.
single_discount(ItemPlan) ->
    single_discount(ItemPlan, 'undefined').
single_discount(ItemPlan, Default) ->
    kz_json:get_json_value([?DISCOUNTS, ?SINGLE], ItemPlan, Default).

-spec cumulative_discount(doc()) -> api_object().
-spec cumulative_discount(doc(), Default) -> kz_json:object() | Default.
cumulative_discount(ItemPlan) ->
    cumulative_discount(ItemPlan, 'undefined').
cumulative_discount(ItemPlan, Default) ->
    kz_json:get_json_value([?DISCOUNTS, ?CUMULATIVE], ItemPlan, Default).

-spec activation_charge(doc()) -> api_object().
-spec activation_charge(doc(), Default) -> kz_json:object() | Default.
activation_charge(ItemPlan) ->
    activation_charge(ItemPlan, 0).
activation_charge(ItemPlan, Default) ->
    kz_json:get_float_value(?ACTIVATION_CHARGE, ItemPlan, Default).

-spec is_enabled(doc()) -> boolean().
is_enabled(ItemPlan) ->
    kz_json:is_true(?ENABLED, ItemPlan).
