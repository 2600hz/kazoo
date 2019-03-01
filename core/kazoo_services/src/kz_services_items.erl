%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_items).

-export([empty/0]).
-export([create/2]).
-export([foldl/3]).

-export([public_json/1]).

-export([changes/1]).
-export([has_changes/1]).
-export([has_additions/1]).
-export([additions/1]).
-export([has_billable_additions/1]).
-export([billable_additions/1]).

-export([reset/1]).

-export([annotate/2
        ,annotate/3
        ]).
-export([difference/2
        ,difference/3
        ,difference_simple/2
        ,difference_number/2
        ,difference_list/2
        ]).

-type items() :: [kz_services_item:item()].
-type fold_fun() :: fun((kz_services_item:item(), Acc) -> Acc).
-type difference_routine() :: {kz_json:key_path(), fun((any(), any()) -> any())}.
-type difference_routines() :: [difference_routine()].
-export_type([items/0
             ,fold_fun/0
             ]).

-include("services.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec empty() -> items().
empty() ->
    [].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create(kz_services:services(), kz_services_plan:plan()) -> items().
create(Services, Plan) ->
    lists:foldl(fun({CategoryName, ItemName}, Items) ->
                        ItemPlan = get_item_plan(Plan, CategoryName, ItemName),
                        [kz_services_item:create(Services, ItemPlan, CategoryName, ItemName)|Items]
                end
               ,[]
               ,get_plan_items(Services, Plan)
               ).

-spec get_item_plan(kz_services_plan:plan(), kz_term:ne_binary(), kz_term:ne_binary()) -> kzd_item_plan:doc().
get_item_plan(Plan, CategoryName, ItemName) ->
    PlanJObj = kz_services_plan:jobj(Plan),
    case kzd_service_plan:item(PlanJObj, CategoryName, ItemName) of
        'undefined' ->
            lager:info("unable to find item plan ~s/~s, using generic"
                      ,[CategoryName, ItemName]
                      ),
            get_generic_item_plan(PlanJObj, CategoryName);
        ItemPlan -> ItemPlan
    end.

-spec get_generic_item_plan(kzd_service_plan:doc(), kz_term:ne_binary()) -> kzd_item_plan:doc().
get_generic_item_plan(PlanJObj, CategoryName) ->
    case kzd_service_plan:item(PlanJObj, CategoryName, <<"_all">>) of
        'undefined' -> kz_json:new();
        ItemPlan -> ItemPlan
    end.

-spec get_plan_items(kz_services:services(), kz_services_plan:plan()) -> kz_term:proplist().
get_plan_items(_Services, Plan) ->
    PlanJObj = kz_services_plan:jobj(Plan),
    [{CategoryName, ItemName}
     || CategoryName <- kzd_service_plan:categories(PlanJObj),
        ItemName <- kzd_service_plan:items(PlanJObj, CategoryName)
    ].
%% TODO: figure out how to handle the billing object, used to be that it would
%%   result in items regardless of what was on the service plan...
%%    [{CategoryName, ItemName}
%%     || CategoryName <- kz_services:list_categories(Services),
%%        ItemName <- kz_services:list_items(Services, CategoryName)
%%    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec foldl(fold_fun(), Acc, items()) -> Acc.
foldl(FoldFun, Acc, Items) ->
    lists:foldl(FoldFun, Acc, Items).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec public_json(items()) -> kz_json:objects().
public_json(Items) ->
    [kz_services_item:public_json(Item) || Item <- Items].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec has_changes(items()) -> boolean().
has_changes(Items) ->
    changes(Items) =/= [].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec changes(items()) -> items().
changes(Items) ->
    [Item
     || Item <- Items,
        kz_services_item:has_changes(Item)
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec has_additions(items()) -> boolean().
has_additions(Items) ->
    additions(Items) =/= [].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec additions(items()) -> items().
additions(Items) ->
    [Item
     || Item <- Items,
        kz_services_item:has_additions(Item)
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec has_billable_additions(items()) -> boolean().
has_billable_additions(Items) ->
    billable_additions(Items) =/= [].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec billable_additions(items()) -> items().
billable_additions(Items) ->
    [Item
     || Item <- Items,
        kz_services_item:has_billable_additions(Item)
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reset(items()) -> items().
reset(Items) ->
    [kz_services_item:reset(Item)
     || Item <- Items
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec annotate(items(), items()) -> items().
annotate(CurrentItems, ProposedItems) ->
    annotate(CurrentItems, ProposedItems, 'undefined').

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec annotate(items(), items(), kz_term:api_binary()) -> items().
annotate(CurrentItems, ProposedItems, Reason) ->
    annotate(CurrentItems, ProposedItems, Reason, []).

-spec annotate(items(), items(), kz_term:api_binary(), items()) -> items().
annotate([], [], _Reason, Items) -> Items;
annotate([CurrentItem|CurrentItems], [], Reason, Items) ->
    ProposedItem = kz_services_item:empty(),
    Difference = difference(CurrentItem, ProposedItem),
    Item = maybe_annotate(<<"removed">>, Difference, Reason, CurrentItem),
    annotate(CurrentItems, [], Reason, [Item|Items]);
annotate(CurrentItems, [ProposedItem|ProposedItems], Reason, Items) ->
    case split_items(CurrentItems, ProposedItem) of
        {'undefined', RemainingCurrentItems} ->
            Item = maybe_annotate(<<"created">>, 'undefined', Reason, ProposedItem),
            annotate(RemainingCurrentItems, ProposedItems, Reason, [Item|Items]);
        {CurrentItem, RemainingCurrentItems} ->
            Difference = difference(CurrentItem, ProposedItem),
            Item = maybe_annotate(<<"modified">>, Difference, Reason, ProposedItem),
            annotate(RemainingCurrentItems, ProposedItems, Reason, [Item|Items])
    end.

-spec maybe_annotate(kz_term:ne_binary(), kz_term:api_object(), kz_term:api_binary(), kz_services_item:item()) ->
                            kz_services_item:item().
maybe_annotate(Type, Difference, Reason, Item) ->
    case kz_json:is_empty(Difference) of
        'true' -> Item;
        'false' ->
            lager:debug("update ~s the item ~s/~s"
                       ,[Type
                        ,kz_services_item:category_name(Item)
                        ,kz_services_item:item_name(Item)
                        ]
                       ),
            Changes = props:filter_empty(
                        [{<<"type">>, Type}
                        ,{<<"reason">>, Reason}
                        ,{<<"difference">>, Difference}
                        ]
                       ),
            kz_services_item:set_changes(Item, kz_json:from_list(Changes))
    end.

-type api_item() :: kz_serivces_item:item() | 'undefined'.
-spec split_items(items(), kz_services_item:item()) -> {api_item(), items()}.
split_items(Items, ProposedItem) ->
    CategoryName = kz_services_item:category_name(ProposedItem),
    ItemName = kz_services_item:item_name(ProposedItem),
    Masqueraded = kz_services_item:is_masquerading(ProposedItem),
    case lists:splitwith(fun(Item) ->
                                 kz_services_item:category_name(Item) =:= CategoryName
                                     andalso
                                     kz_services_item:item_name(Item) =:= ItemName
                                     andalso
                                     kz_services_item:is_masquerading(Item) =:= Masqueraded
                         end, Items)
    of
        {[], RemainingItems} ->
            {'undefined', RemainingItems};
        {[Item], RemainingItems} ->
            {Item, RemainingItems}
    end.

%%------------------------------------------------------------------------------
%% @doc Compares two service items, returning the difference
%% @end
%%------------------------------------------------------------------------------
-spec difference(kz_services_item:item(), kz_services_item:item()) -> kz_json:object().
difference(Item1, Item2) ->
    Routines = [{<<"activation_charge">>, fun difference_number/2}
               ,{<<"billable">>, fun difference_number/2}
               ,{<<"category">>, fun difference_simple/2}
               ,{[<<"discounts">>, <<"cumulative">>, <<"quantity">>], fun difference_number/2}
               ,{[<<"discounts">>, <<"cumulative">>, <<"rate">>], fun difference_number/2}
               ,{[<<"discounts">>, <<"single">>, <<"rate">>], fun difference_number/2}
               ,{<<"exceptions">>, fun difference_list/2}
               ,{<<"item">>, fun difference_simple/2}
               ,{<<"minimum">>, fun difference_number/2}
               ,{<<"name">>, fun difference_simple/2}
               ,{<<"quantity">>, fun difference_number/2}
               ,{<<"rate">>, fun difference_number/2}
               ],
    difference(Item1, Item2, Routines).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec difference(kz_services_item:item(), kz_services_item:item(), difference_routines()) -> kz_json:object().
difference(Item1, Item2, Routines) ->
    JObj1 = kz_services_item:public_json(Item1),
    JObj2 = kz_services_item:public_json(Item2),
    lists:foldl(fun({Key, Fun}, JObj) ->
                        Value1 = kz_json:get_value(Key, JObj1),
                        Value2 = kz_json:get_value(Key, JObj2),
                        case Fun(Value1, Value2) of
                            'undefined' -> JObj;
                            Difference -> kz_json:set_value(Key, Difference, JObj)
                        end
                end
               ,kz_json:new()
               ,Routines
               ).

-spec difference_simple(any(), Value) -> 'undefined' | Value.
difference_simple(Value, Value) -> 'undefined';
difference_simple(_, Value) -> Value.

-spec difference_number(kz_term:api_float() | integer(), kz_term:api_float() | integer()) -> kz_term:api_float() | integer().
difference_number(Value, Value) -> 'undefined';
difference_number('undefined', Value) ->
    difference_number(0, Value);
difference_number(Value, 'undefined') ->
    difference_number(Value, 0);
difference_number(Value1, Value2) -> Value2 - Value1.

-spec difference_list(kz_term:api_ne_binaries(), kz_term:api_ne_binaries()) -> kz_term:proplist() | 'undefined'.
difference_list(List, List) -> 'undefined';
difference_list('undefined', List) ->
    difference_list([], List);
difference_list(List, 'undefined') ->
    difference_list(List, []);
difference_list(List1, List2) ->
    Set1 = sets:from_list(List1),
    Set2 = sets:from_list(List2),
    props:filter_empty(
      [{<<"removals">>, sets:to_list(sets:subtract(Set1, Set2))}
      ,{<<"additions">>, sets:to_list(sets:subtract(Set2, Set1))}
      ]
     ).
