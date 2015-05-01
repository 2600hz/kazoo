%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kzd_service_plan).

-export([new/0
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

         ,categories/1, category/2
         ,items/2, item/3

         ,bookkeepers/1, bookkeeper/2, bookkeeper_ids/1
        ]).

-include("kz_documents.hrl").

-type doc() :: wh_json:object().
-type api_doc() :: api_object().
-export_type([doc/0
              ,api_doc/0
             ]).

-define(PLAN, <<"plan">>).
-define(ACTIVATION_CHARGE, <<"activation_charge">>).
-define(ALL, <<"_all">>).
-define(BOOKKEEPERS, <<"bookkeepers">>).

-spec new() -> doc().
new() ->
    wh_json:new().

-spec account_id(doc()) -> api_binary().
-spec account_id(doc(), Default) -> ne_binary() | Default.
account_id(Plan) ->
    account_id(Plan, 'undefined').
account_id(Plan, Default) ->
    wh_json:get_value(<<"account_id">>, Plan, Default).

-spec overrides(doc()) -> wh_json:object().
-spec overrides(doc(), Default) -> wh_json:object() | Default.
overrides(Plan) ->
    overrides(Plan, wh_json:new()).
overrides(Plan, Default) ->
    wh_json:get_json_value(<<"overrides">>, Plan, Default).

-spec merge_overrides(doc(), wh_json:object()) -> doc().
merge_overrides(Plan, Overrides) ->
    wh_json:merge_recursive(Plan, wh_json:from_list([{?PLAN, Overrides}])).

-spec item_activation_charge(doc(), ne_binary(), ne_binary()) -> api_float().
-spec item_activation_charge(doc(), ne_binary(), ne_binary(), Default) -> float() | Default.
item_activation_charge(Plan, Category, Item) ->
    item_activation_charge(Plan, Category, Item, 'undefined').
item_activation_charge(Plan, Category, Item, Default) ->
    wh_json:get_float_value([?PLAN, Category, Item, ?ACTIVATION_CHARGE]
                            ,Plan
                            ,Default
                           ).

-spec category_activation_charge(doc(), ne_binary()) -> float().
-spec category_activation_charge(doc(), ne_binary(), Default) -> float() | Default.
category_activation_charge(Plan, Category) ->
    category_activation_charge(Plan, Category, 0.0).
category_activation_charge(Plan, Category, Default) ->
    item_activation_charge(Plan, Category, ?ALL, Default).

-spec categories(doc()) -> ne_binaries().
categories(Plan) ->
    wh_json:get_keys(?PLAN, Plan).

-spec category(doc(), ne_binary()) -> api_object().
category(Plan, CategoryId) ->
    wh_json:get_json_value([?PLAN, CategoryId], Plan).

-spec items(doc(), ne_binary()) -> ne_binaries().
items(Plan, Category) ->
    wh_json:get_keys([?PLAN, Category], Plan).

-spec item(doc(), ne_binary(), ne_binary()) -> api_object().
item(Plan, CategoryId, ItemId) ->
    wh_json:get_json_value([?PLAN, CategoryId, ItemId], Plan).

-spec bookkeepers(doc()) -> wh_json:object().
bookkeepers(Plan) ->
    wh_json:get_json_value(?BOOKKEEPERS, Plan, wh_json:new()).

-spec bookkeeper_ids(doc()) -> ne_binaries().
bookkeeper_ids(Plan) ->
    wh_json:get_keys(?BOOKKEEPERS, Plan).

-spec bookkeeper(doc(), ne_binary()) -> wh_json:object().
bookkeeper(Plan, BookkeeperId) ->
    wh_json:get_json_value(BookkeeperId, bookkeepers(Plan), wh_json:new()).

-spec item_minimum(doc(), ne_binary(), ne_binary()) -> integer().
-spec item_minimum(doc(), ne_binary(), ne_binary(), Default) -> integer() | Default.
item_minimum(Plan, CategoryId, ItemId) ->
    item_minimum(Plan, CategoryId, ItemId, 0).
item_minimum(Plan, CategoryId, ItemId, Default) ->
    kzd_item_plan:minimum(
      wh_json:get_json_value([?PLAN, CategoryId, ItemId]
                             ,Plan
                             ,wh_json:new()
                            )
      ,Default
     ).

-spec item_plan(doc(), ne_binary(), ne_binary()) -> wh_json:object().
-spec item_plan(doc(), ne_binary(), ne_binary(), Default) -> wh_json:object() | Default.
item_plan(Plan, CategoryId, ItemId) ->
    item_plan(Plan, CategoryId, ItemId, wh_json:new()).
item_plan(Plan, CategoryId, ItemId, Default) ->
    wh_json:get_json_value([?PLAN, CategoryId, ItemId], Plan, Default).

-spec category_plan(doc(), ne_binary()) -> wh_json:object().
-spec category_plan(doc(), ne_binary(), Default) -> wh_json:object() | Default.
category_plan(Plan, CategoryId) ->
    category_plan(Plan, CategoryId, wh_json:new()).
category_plan(Plan, CategoryId, Default) ->
    item_plan(Plan, CategoryId, ?ALL, Default).

-spec plan(doc()) -> wh_json:object().
-spec plan(doc(), Default) -> wh_json:object() | Default.
plan(Plan) ->
    plan(Plan, wh_json:new()).
plan(Plan, Default) ->
    wh_json:get_json_value(?PLAN, Plan, Default).

-spec set_plan(doc(), wh_json:object()) -> doc().
set_plan(Plan, P) ->
    wh_json:set_value(?PLAN, Plan, P).
