%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
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

        ,merge_strategy/1
        ,merge_priority/1
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-type docs() :: [doc()].
-type api_doc() :: kz_term:api_object().
-export_type([doc/0
             ,api_doc/0
             ,docs/0
             ]).

-define(PLAN, <<"plan">>).
-define(ACTIVATION_CHARGE, <<"activation_charge">>).
-define(ALL, <<"_all">>).
-define(BOOKKEEPERS, <<"bookkeepers">>).
-define(MERGE, <<"merge">>).
-define(MERGE_STRATEGY, [?MERGE, <<"strategy">>]).
-define(MERGE_PRIORITY, [?MERGE, <<"priority">>]).

-define(DEFAULT_MERGE_PRIORITY, 0).

-spec new() -> doc().
new() ->
    kz_doc:set_type(kz_json:new(), type()).

-spec type() -> kz_term:ne_binary().
type() -> <<"service_plan">>.

-spec all_items_key() -> kz_term:ne_binary().
all_items_key() -> ?ALL.

-spec account_id(doc()) -> kz_term:api_binary().
account_id(Plan) ->
    account_id(Plan, 'undefined').

-spec account_id(doc(), Default) -> kz_term:ne_binary() | Default.
account_id(Plan, Default) ->
    kz_json:get_value(<<"account_id">>, Plan, Default).

-spec overrides(doc()) -> kz_json:object().
overrides(Plan) ->
    overrides(Plan, kz_json:new()).

-spec overrides(doc(), Default) -> kz_json:object() | Default.
overrides(Plan, Default) ->
    kz_json:get_json_value(<<"overrides">>, Plan, Default).

-spec merge_overrides(doc(), kz_json:object()) -> doc().
merge_overrides(Plan, Overrides) ->
    kz_json:merge(Plan, kz_json:from_list([{?PLAN, Overrides}])).

-spec item_activation_charge(doc(), kz_term:ne_binary(), kz_term:ne_binary()) -> float().
item_activation_charge(Plan, Category, Item) ->
    item_activation_charge(Plan, Category, Item, 0.0).

-spec item_activation_charge(doc(), kz_term:ne_binary(), kz_term:ne_binary(), Default) -> float() | Default.
item_activation_charge(Plan, Category, Item, Default) ->
    Path = [?PLAN, Category, Item],
    ItemConfig = kz_json:get_json_value(Path, Plan, kz_json:new()),
    kzd_item_plan:activation_charge(ItemConfig, Default).

-spec category_activation_charge(doc(), kz_term:ne_binary()) -> float().
category_activation_charge(Plan, Category) ->
    category_activation_charge(Plan, Category, 0.0).

-spec category_activation_charge(doc(), kz_term:ne_binary(), Default) -> float() | Default.
category_activation_charge(Plan, Category, Default) ->
    item_activation_charge(Plan, Category, ?ALL, Default).

-spec categories(doc()) -> kz_term:ne_binaries().
categories(Plan) ->
    kz_json:get_keys(?PLAN, Plan).

-spec category(doc(), kz_term:ne_binary()) -> kz_term:api_object().
category(Plan, CategoryId) ->
    category(Plan, CategoryId, 'undefined').

-spec category(doc(), kz_term:ne_binary(), Default) -> kz_term:api_object() | Default.
category(Plan, CategoryId, Default) ->
    kz_json:get_json_value([?PLAN, CategoryId], Plan, Default).

-spec items(doc(), kz_term:ne_binary()) -> kz_term:ne_binaries().
items(Plan, Category) ->
    kz_json:get_keys([?PLAN, Category], Plan).

-spec item(doc(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_object().
item(Plan, CategoryId, ItemId) ->
    item(Plan, CategoryId, ItemId, 'undefined').

-spec item(doc(), kz_term:ne_binary(), kz_term:ne_binary(), Default) -> kz_json:object() | Default.
item(Plan, CategoryId, ItemId, Default) ->
    kz_json:get_json_value([?PLAN, CategoryId, ItemId], Plan, Default).

-spec bookkeepers(doc()) -> kz_json:object().
bookkeepers(Plan) ->
    kz_json:get_json_value(?BOOKKEEPERS, Plan, kz_json:new()).

-spec bookkeeper_ids(doc()) -> kz_term:ne_binaries().
bookkeeper_ids(Plan) ->
    kz_json:get_keys(?BOOKKEEPERS, Plan).

-spec bookkeeper(doc(), kz_term:ne_binary()) -> kz_json:object().
bookkeeper(Plan, BookkeeperId) ->
    kz_json:get_json_value(BookkeeperId, bookkeepers(Plan), kz_json:new()).

-spec item_minimum(doc(), kz_term:ne_binary(), kz_term:ne_binary()) -> integer().
item_minimum(Plan, CategoryId, ItemId) ->
    item_minimum(Plan, CategoryId, ItemId, 0).

-spec item_minimum(doc(), kz_term:ne_binary(), kz_term:ne_binary(), Default) -> integer() | Default.
item_minimum(Plan, CategoryId, ItemId, Default) ->
    kzd_item_plan:minimum(item(Plan, CategoryId, ItemId, kz_json:new())
                         ,Default
                         ).

-spec item_name(doc(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
item_name(Plan, CategoryId, ItemId) ->
    kzd_item_plan:name(item(Plan, CategoryId, ItemId, kz_json:new())).

-spec item_exceptions(doc(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                             kz_term:ne_binaries().
item_exceptions(Plan, CategoryId, ItemId) ->
    item_exceptions(Plan, CategoryId, ItemId, []).

-spec item_exceptions(doc(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binaries()) ->
                             kz_term:ne_binaries().
item_exceptions(Plan, CategoryId, ItemId, Default) ->
    Item = item(Plan, CategoryId, ItemId, kz_json:new()),
    kzd_item_plan:exceptions(Item, Default).

-spec item_plan(doc(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
item_plan(Plan, CategoryId, ItemId) ->
    item_plan(Plan, CategoryId, ItemId, kz_json:new()).

-spec item_plan(doc(), kz_term:ne_binary(), kz_term:ne_binary(), Default) -> kz_json:object() | Default.
item_plan(Plan, CategoryId, ItemId, Default) ->
    kz_json:get_json_value([?PLAN, CategoryId, ItemId], Plan, Default).

-spec category_plan(doc(), kz_term:ne_binary()) -> kz_json:object().
category_plan(Plan, CategoryId) ->
    category_plan(Plan, CategoryId, kz_json:new()).

-spec category_plan(doc(), kz_term:ne_binary(), Default) -> kz_json:object() | Default.
category_plan(Plan, CategoryId, Default) ->
    item_plan(Plan, CategoryId, ?ALL, Default).

-spec plan(doc()) -> kz_json:object().
plan(Plan) ->
    plan(Plan, kz_json:new()).

-spec plan(doc(), Default) -> kz_json:object() | Default.
plan(Plan, Default) ->
    kz_json:get_json_value(?PLAN, Plan, Default).

-spec set_plan(doc(), kz_json:object()) -> doc().
set_plan(Plan, P) ->
    kz_json:set_value(?PLAN, P, Plan).

-spec grouping_category(doc()) -> kz_term:api_ne_binary().
grouping_category(ServicePlan) ->
    grouping_category(ServicePlan, 'undefined').

-spec grouping_category(doc(), Default) -> kz_term:ne_binary() | Default.
grouping_category(ServicePlan, Default) ->
    kz_json:get_ne_binary_value(<<"category">>, ServicePlan, Default).

-spec merge_strategy(doc()) -> kz_term:ne_binary().
merge_strategy(ServicePlan) ->
    kz_json:get_ne_binary_value(?MERGE_STRATEGY, ServicePlan).

-spec merge_priority(doc()) -> kz_term:ne_binary().
merge_priority(ServicePlan) ->
    kz_json:get_integer_value(?MERGE_PRIORITY, ServicePlan, ?DEFAULT_MERGE_PRIORITY).
