%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kzd_service_plan).

-export([new/0
        ,type/0
        ,account_id/1, account_id/2
        ,overrides/1, overrides/2
        ,merge_overrides/2

        ,plan/1, plan/2
        ,set_plan/2

        ,item_plan/3, item_plan/4
        ,category_plan/2, category_plan/3

        ,item_activation_charge/3, item_activation_charge/4
        ,category_activation_charge/2, category_activation_charge/3

        ,item_minimum/3, item_minimum/4
        ,item_name/3
        ,item_exceptions/3

        ,categories/1, category/2, category/3
        ,items/2, item/3

        ,bookkeepers/1, bookkeeper/2, bookkeeper_ids/1
        ,grouping_category/1, grouping_category/2

        ,all_items_key/0
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-type docs() :: [doc()].
-type api_doc() :: api_object().
-export_type([doc/0
             ,api_doc/0
             ,docs/0
             ]).

-define(PLAN, <<"plan">>).
-define(ACTIVATION_CHARGE, <<"activation_charge">>).
-define(ALL, <<"_all">>).
-define(BOOKKEEPERS, <<"bookkeepers">>).

-spec new() -> doc().
new() ->
    kz_doc:set_type(kz_json:new(), type()).

-spec type() -> ne_binary().
type() -> <<"service_plan">>.

-spec all_items_key() -> ne_binary().
all_items_key() -> ?ALL.

-spec account_id(doc()) -> api_binary().
-spec account_id(doc(), Default) -> ne_binary() | Default.
account_id(Plan) ->
    account_id(Plan, 'undefined').
account_id(Plan, Default) ->
    kz_json:get_value(<<"account_id">>, Plan, Default).

-spec overrides(doc()) -> kz_json:object().
-spec overrides(doc(), Default) -> kz_json:object() | Default.
overrides(Plan) ->
    overrides(Plan, kz_json:new()).
overrides(Plan, Default) ->
    kz_json:get_json_value(<<"overrides">>, Plan, Default).

-spec merge_overrides(doc(), kz_json:object()) -> doc().
merge_overrides(Plan, Overrides) ->
    kz_json:merge(Plan, kz_json:from_list([{?PLAN, Overrides}])).

-spec item_activation_charge(doc(), ne_binary(), ne_binary()) -> api_float().
-spec item_activation_charge(doc(), ne_binary(), ne_binary(), Default) -> float() | Default.
item_activation_charge(Plan, Category, Item) ->
    item_activation_charge(Plan, Category, Item, 0).
item_activation_charge(Plan, Category, Item, Default) ->
    ItemConfig = kz_json:get_json_value([?PLAN, Category, Item]
                                       ,Plan
                                       ,kz_json:new()
                                       ),
    kzd_item_plan:activation_charge(ItemConfig, Default).

-spec category_activation_charge(doc(), ne_binary()) -> float().
-spec category_activation_charge(doc(), ne_binary(), Default) -> float() | Default.
category_activation_charge(Plan, Category) ->
    category_activation_charge(Plan, Category, 0.0).
category_activation_charge(Plan, Category, Default) ->
    item_activation_charge(Plan, Category, ?ALL, Default).

-spec categories(doc()) -> ne_binaries().
categories(Plan) ->
    kz_json:get_keys(?PLAN, Plan).

-spec category(doc(), ne_binary()) -> api_object().
-spec category(doc(), ne_binary(), Default) -> api_object() | Default.
category(Plan, CategoryId) ->
    category(Plan, CategoryId, 'undefined').
category(Plan, CategoryId, Default) ->
    kz_json:get_json_value([?PLAN, CategoryId], Plan, Default).

-spec items(doc(), ne_binary()) -> ne_binaries().
items(Plan, Category) ->
    kz_json:get_keys([?PLAN, Category], Plan).

-spec item(doc(), ne_binary(), ne_binary()) -> api_object().
item(Plan, CategoryId, ItemId) ->
    kz_json:get_json_value([?PLAN, CategoryId, ItemId], Plan).

-spec bookkeepers(doc()) -> kz_json:object().
bookkeepers(Plan) ->
    kz_json:get_json_value(?BOOKKEEPERS, Plan, kz_json:new()).

-spec bookkeeper_ids(doc()) -> ne_binaries().
bookkeeper_ids(Plan) ->
    kz_json:get_keys(?BOOKKEEPERS, Plan).

-spec bookkeeper(doc(), ne_binary()) -> kz_json:object().
bookkeeper(Plan, BookkeeperId) ->
    kz_json:get_json_value(BookkeeperId, bookkeepers(Plan), kz_json:new()).

-spec item_minimum(doc(), ne_binary(), ne_binary()) -> integer().
-spec item_minimum(doc(), ne_binary(), ne_binary(), Default) -> integer() | Default.
item_minimum(Plan, CategoryId, ItemId) ->
    item_minimum(Plan, CategoryId, ItemId, 0).
item_minimum(Plan, CategoryId, ItemId, Default) ->
    kzd_item_plan:minimum(
      kz_json:get_json_value([?PLAN, CategoryId, ItemId]
                            ,Plan
                            ,kz_json:new()
                            )
                         ,Default
     ).

-spec item_name(doc(), ne_binary(), ne_binary()) -> ne_binary().
item_name(Plan, CategoryId, ItemId) ->
    kzd_item_plan:name(
      kz_json:get_json_value(
        [?PLAN, CategoryId, ItemId]
                            ,Plan
                            ,kz_json:new()
       )
     ).

-spec item_exceptions(doc(), ne_binary(), ne_binary()) ->
                             ne_binaries().
-spec item_exceptions(doc(), ne_binary(), ne_binary(), ne_binaries()) ->
                             ne_binaries().
item_exceptions(Plan, CategoryId, ItemId) ->
    item_exceptions(Plan, CategoryId, ItemId, []).
item_exceptions(Plan, CategoryId, ItemId, Default) ->
    Item = kz_json:get_json_value([?PLAN, CategoryId, ItemId]
                                 ,Plan
                                 ,kz_json:new()
                                 ),
    kzd_item_plan:exceptions(Item, Default).

-spec item_plan(doc(), ne_binary(), ne_binary()) -> kz_json:object().
-spec item_plan(doc(), ne_binary(), ne_binary(), Default) -> kz_json:object() | Default.
item_plan(Plan, CategoryId, ItemId) ->
    item_plan(Plan, CategoryId, ItemId, kz_json:new()).
item_plan(Plan, CategoryId, ItemId, Default) ->
    kz_json:get_json_value([?PLAN, CategoryId, ItemId], Plan, Default).

-spec category_plan(doc(), ne_binary()) -> kz_json:object().
-spec category_plan(doc(), ne_binary(), Default) -> kz_json:object() | Default.
category_plan(Plan, CategoryId) ->
    category_plan(Plan, CategoryId, kz_json:new()).
category_plan(Plan, CategoryId, Default) ->
    item_plan(Plan, CategoryId, ?ALL, Default).

-spec plan(doc()) -> kz_json:object().
-spec plan(doc(), Default) -> kz_json:object() | Default.
plan(Plan) ->
    plan(Plan, kz_json:new()).
plan(Plan, Default) ->
    kz_json:get_json_value(?PLAN, Plan, Default).

-spec set_plan(doc(), kz_json:object()) -> doc().
set_plan(Plan, P) ->
    kz_json:set_value(?PLAN, P, Plan).

-spec grouping_category(doc()) -> api_ne_binary().
-spec grouping_category(doc(), Default) -> ne_binary() | Default.
grouping_category(ServicePlan) ->
    grouping_category(ServicePlan, 'undefined').
grouping_category(ServicePlan, Default) ->
    kz_json:get_ne_binary_value(<<"category">>, ServicePlan, Default).
