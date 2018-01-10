%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_service_plans).

-export([empty/0]).
-export([public_json/1]).
-export([add_service_plan/3]).
-export([delete_service_plan/2]).
-export([from_service_json/1]).
-export([plan_summary/1]).
-export([activation_charges/3]).
-export([create_items/1
        ,create_items/2
        ]).
-export([public_json_items/1]).

-ifdef(TEST).
-export([append_vendor_plan/3]).
-endif.

-include("kazoo_services.hrl").

-record(kz_service_plans, {vendor_id :: api_binary()
                          ,plans = [] :: kzd_service_plan:docs()
                          }).

-type plan() :: #kz_service_plans{}.
-type plans() :: [plan()].

-export_type([plan/0, plans/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create an empty service plans data structure.
%% @end
%%--------------------------------------------------------------------
-spec empty() -> plans().
empty() -> [].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec from_service_json(kzd_services:doc()) -> plans().
from_service_json(ServicesJObj) ->
    Routines = [fun get_services_plan/2
               ,fun get_object_plans/2
               ],
    Plans = lists:foldl(fun(F, ServicePlans) ->
                                F(ServicesJObj, ServicePlans)
                        end
                       ,empty()
                       ,Routines
                       ),
    merge_vendors(Plans).

-spec get_services_plan(kz_json:object(), plans()) -> plans().
get_services_plan(ServicesJObj, ServicePlans) ->
    ResellerId = find_reseller_id(ServicesJObj),
    lists:foldl(get_service_plans_fold(ResellerId, ServicesJObj)
               ,ServicePlans
               ,kzd_services:plan_ids(ServicesJObj)
               ).

-spec get_service_plans_fold(ne_binary(), kz_json:object()) ->
                                    fun((ne_binary(), ne_binary(), kz_json:object(), plans()) ->
                                               plans()).
get_service_plans_fold(ResellerId, ServicesJObj) ->
    fun(PlanId, ServicePlans) ->
            get_services_plan(PlanId, ResellerId, ServicesJObj, ServicePlans)
    end.

-spec get_services_plan(ne_binary(), ne_binary(), kz_json:object(), plans()) -> plans().
get_services_plan(PlanId, ResellerId, ServicesJObj, ServicePlans) ->
    AccountId = kzd_services:plan_account_id(ServicesJObj, PlanId, ResellerId),
    Overrides = kzd_services:plan_overrides(ServicesJObj, PlanId),
    case fetch_plan(PlanId, AccountId, Overrides) of
        'undefined' -> ServicePlans;
        ServicePlan ->
            append_vendor_plan(ServicePlan
                              ,AccountId
                              ,ServicePlans
                              )
    end.

-ifdef(TEST).
-spec get_object_plans(kz_json:object(), plans()) -> plans().
get_object_plans(_, ServicePlans) -> ServicePlans.
-else.
-spec get_object_plans(kz_json:object(), plans()) -> plans().
get_object_plans(ServicesJObj, ServicePlans) ->
    ResellerId = find_reseller_id(ServicesJObj),
    Account = kz_doc:id(ServicesJObj),
    AccountDb = kz_util:format_account_db(Account),
    {'ok', JObjs} = kz_datamgr:get_results(AccountDb, <<"services/object_plans">>),
    Props = [{PlanId, kz_json:get_value([<<"value">>, PlanId], JObj)}
             || JObj <- JObjs
                    ,PlanId <- kz_json:get_keys(<<"value">>, JObj)
            ],
    lists:foldl(get_object_plans_fold(ResellerId)
               ,ServicePlans
               ,Props
               ).

-spec get_object_plans_fold(ne_binary()) ->
                                   fun(({ne_binary(), kz_json:object()}, plans()) ->
                                              plans()).
get_object_plans_fold(ResellerId) ->
    fun({PlanId, JObj}, ServicePlans) ->
            get_object_plan(PlanId, ResellerId, JObj, ServicePlans)
    end.

get_object_plan(PlanId, ResellerId, JObj, ServicePlans) ->
    AccountId = kz_json:get_ne_value(<<"account_id">>, JObj, ResellerId),
    Overrides = kz_json:get_ne_value(<<"overrides">>, JObj, kz_json:new()),
    case fetch_plan(PlanId, AccountId, Overrides) of
        'undefined' -> ServicePlans;
        ServicePlan ->
            append_vendor_plan(ServicePlan
                              ,AccountId
                              ,ServicePlans
                              )
    end.
-endif.

-spec fetch_plan(ne_binary(), ne_binary(), kz_json:object()) -> api_object().
fetch_plan(PlanId, AccountId, Overrides) ->
    AreOverridesEmpty = kz_json:is_empty(Overrides),
    case kz_service_plan:fetch(PlanId, AccountId) of
        'undefined' -> 'undefined';
        ServicePlan when not AreOverridesEmpty ->
            kzd_service_plan:merge_overrides(ServicePlan, Overrides);
        ServicePlan -> ServicePlan
    end.

-spec append_vendor_plan(kz_json:object(), ne_binary(), plans()) -> plans().
append_vendor_plan(Plan, VendorId, ServicePlans) ->
    case lists:keyfind(VendorId, #kz_service_plans.vendor_id, ServicePlans) of
        'false' ->
            ServicePlan = #kz_service_plans{vendor_id=VendorId
                                           ,plans=[Plan]
                                           },
            [ServicePlan|ServicePlans];
        #kz_service_plans{plans=Plans}=ServicePlan ->
            lists:keyreplace(VendorId
                            ,#kz_service_plans.vendor_id
                            ,ServicePlans
                            ,ServicePlan#kz_service_plans{plans=[Plan|Plans]}
                            )
    end.

-spec find_reseller_id(kzd_services:doc()) -> api_binary().
find_reseller_id(ServicesJObj) ->
    case kzd_services:reseller_id(ServicesJObj) of
        'undefined' -> kz_json:get_value(<<"reseller_id">>, ServicesJObj);
        ResellerId -> ResellerId
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec public_json(plans()) -> kzd_service_plan:doc().
public_json(ServicePlans) ->
    public_json(ServicePlans, kz_json:new()).

-spec public_json(plans(), kz_json:object()) -> kzd_service_plan:doc().
public_json([], JObj) ->
    kzd_service_plan:set_plan(kzd_service_plan:new(), JObj);
public_json([ServicesPlan|ServicesPlans], JObj) ->
    #kz_service_plans{plans=NewJObj}=merge_plan(ServicesPlan, JObj),
    public_json(ServicesPlans, NewJObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec add_service_plan(ne_binary(), ne_binary(), kzd_services:doc()) -> kzd_services:doc().
add_service_plan(PlanId, ResellerId, ServicesJObj) ->
    ResellerDb = kz_util:format_account_id(ResellerId, 'encoded'),
    case kz_datamgr:open_cache_doc(ResellerDb, PlanId) of
        {'error', _R} ->
            lager:info("failed to load service plan ~s from ~s: ~p", [PlanId, ResellerDb, _R]),
            Plan = kz_json:from_list([{<<"account_id">>, ResellerId}]),
            kzd_services:set_plan(ServicesJObj, PlanId, Plan);
        {'ok', ServicePlan} ->
            Plan =
                kz_json:from_list(
                  [{<<"account_id">>, ResellerId}
                  ,{<<"category">>, kzd_service_plan:grouping_category(ServicePlan)}
                  ]),
            kzd_services:set_plan(ServicesJObj, PlanId, Plan)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec delete_service_plan(ne_binary(), kzd_services:doc()) -> kzd_services:doc().
delete_service_plan(PlanId, ServicesJObj) ->
    kzd_services:set_plan(ServicesJObj, PlanId, 'undefined').

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec plan_summary(kzd_services:doc()) -> kz_json:object().
plan_summary(ServicesJObj) ->
    ResellerId = kzd_services:reseller_id(ServicesJObj),
    lists:foldl(fun(PlanId, J) ->
                        Plan = kzd_services:plan(ServicesJObj, PlanId),
                        case kzd_service_plan:account_id(Plan) of
                            ResellerId -> kz_json:set_value(PlanId, Plan, J);
                            _Else -> J
                        end
                end
               ,kz_json:new()
               ,kz_json:get_keys(kzd_services:plans(ServicesJObj))
               ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec activation_charges(ne_binary(), ne_binary(), plans()) -> float().
activation_charges(Category, Item, ServicePlans) ->
    lists:sum(
      [kz_service_plan:activation_charges(Category, Item, Plan)
       || ServicePlan <- ServicePlans,
          Plan <- ServicePlan#kz_service_plans.plans
      ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a the services on an account (and descedants) as well as the
%% service plans the account is subscribed to create a list of items
%% suitable for use with the bookkeepers.
%% @end
%%--------------------------------------------------------------------
-spec create_items(kzd_services:doc()) ->
                          {'ok', kz_service_items:items()} |
                          {'error', 'no_plans'}.
create_items(ServiceJObj) ->
    case from_service_json(ServiceJObj) of
        [] -> {'error', 'no_plans'};
        ServicePlans ->
            {'ok', create_items(ServiceJObj, ServicePlans)}
    end.

-spec create_items(kzd_services:doc(), plans()) -> kz_service_items:items().
create_items(ServiceJObj, ServicePlans) ->
    Services = kz_services:from_service_json(ServiceJObj),
    Plans = [Plan
             || #kz_service_plans{plans=Plans} <- ServicePlans,
                Plan <- Plans
            ],
    lists:foldl(fun(Plan, Items) ->
                        kz_service_plan:create_items(Plan, Items, Services)
                end
               ,kz_service_items:empty()
               ,Plans
               ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Return a json object with all the items for an account
%% @end
%%--------------------------------------------------------------------
-spec public_json_items(kzd_services:doc()) -> kz_json:object().
public_json_items(ServiceJObj) ->
    case create_items(ServiceJObj) of
        {'ok', Items} ->
            kz_service_items:public_json(Items);
        {'error', _} ->
            kz_json:new()
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a plan and a vendor id append it to the list of service plans
%% for that vendor, creating a new list (record) if not present.
%% @end
%%--------------------------------------------------------------------
-spec merge_vendors(plans()) -> plans().
merge_vendors(ServicesPlans) ->
    lists:map(fun merge_plan/1, ServicesPlans).

-spec merge_plan(plan()) -> plan().
merge_plan(ServicesPlan) ->
    merge_plan(ServicesPlan, 'false').

-spec merge_plan(plan(), boolean() | kz_json:object()) -> plan().
merge_plan(#kz_service_plans{plans=PlanJObjs}=ServicesPlan, MergeToSingle) ->
    Dict = lists:foldl(fun(PlanJObj, D) ->
                               Strategy = kzd_service_plan:merge_strategy(PlanJObj),
                               Priority = kzd_service_plan:merge_priority(PlanJObj),
                               dict:append(Strategy, {Priority, PlanJObj}, D)
                       end, dict:new(), PlanJObjs),
    Sorted = lists:sort(fun merge_plan_strategy_sort/2
                       ,dict:to_list(Dict)
                       ),
    Merged = merge_plan_plans(Sorted, []),
    case MergeToSingle of
        'false' ->
            ServicesPlan#kz_service_plans{plans=Merged};
        'true' ->
            ServicesPlan#kz_service_plans{plans=lists:foldl(fun merge_to_single/2, kz_json:new(), Merged)};
        Else ->
            'true' = kz_json:is_json_object(Else),
            ServicesPlan#kz_service_plans{plans=lists:foldl(fun merge_to_single/2, Else, Merged)}
    end.

-spec merge_to_single(kz_json:object(), kz_json:object()) -> kz_json:object().
merge_to_single(JObj, Merged) ->
    kz_json:merge(Merged, JObj).

-spec merge_plan_strategy_sort(merge_strategy_group(), merge_strategy_group()) -> boolean().
merge_plan_strategy_sort({A, _}, {B, _}) ->
    merge_strategy_priority(A) =< merge_strategy_priority(B).

-spec merge_plan_plans(merge_strategy_groups(), kz_json:objects()) -> kz_json:objects().
merge_plan_plans([], Merged) -> Merged;
merge_plan_plans([{<<"simple">>, Group}|Tail], Merged) ->
    Sorted = lists:sort(fun merge_plan_plans_sort/2, Group),
    MergedGroup = lists:foldl(fun simple_merge_plans/2, kz_json:new(), Sorted),
    merge_plan_plans(Tail, [MergedGroup|Merged]);
merge_plan_plans([{<<"cumulative">>, Group}|Tail], Merged) ->
    Props = [{[CategoryId, ItemId], kzd_service_plan:item(PlanJObj, CategoryId, ItemId)}
             || {_, PlanJObj} <- lists:sort(fun merge_plan_plans_sort/2, Group)
                    ,CategoryId <- kzd_service_plan:categories(PlanJObj)
                    ,ItemId <- kzd_service_plan:items(PlanJObj, CategoryId)
            ],
    Dict = lists:foldl(fun({Key, Value}, D) ->
                               dict:append(Key, Value, D)
                       end, dict:new(), Props),
    Values = [{cumulative_merge_keys(Root, Key), Fun(Key, JObjs)}
              || {Root, JObjs} <- dict:to_list(Dict)
                     ,{Key, Fun} <- kzd_item_plan:cumulative_merge_scheme()
             ],
    MergedGroup = kz_json:from_list([{<<"_id">>, <<"cumulative">>}
                                    ,{<<"plans">>, [PlanJObj || {_, PlanJObj} <- Group]}
                                    ,{<<"plan">>, kz_json:set_values(Values, kz_json:new())}
                                    ]),
    merge_plan_plans(Tail, [MergedGroup|Merged]);
merge_plan_plans([{_S, Group}|Tail], Merged) ->
    MergedGroup = [PlanJObj || {_, PlanJObj} <- Group],
    merge_plan_plans(Tail, MergedGroup ++ Merged).

-spec cumulative_merge_keys(ne_binary() | ne_binaries(), ne_binary() | ne_binaries()) -> ne_binaries().
cumulative_merge_keys(Root, Key) ->
    lists:flatten([Root, Key]).

-spec merge_plan_plans_sort(merge_strategy_plan(), merge_strategy_plan()) -> boolean().
merge_plan_plans_sort({A, _}, {B, _}) ->
    A =< B.

-spec simple_merge_plans(merge_strategy_plan(), kz_json:object()) -> kz_json:object().
simple_merge_plans({_, PlanJObj}, Merged) ->
    kz_json:merge(Merged, PlanJObj).

-type merge_strategy_plan() :: {non_neg_integer(), kz_json:object()}.
-type merge_strategy_plans() :: [merge_strategy_plan()].
-type merge_strategy_group() :: {ne_binary(), merge_strategy_plans()}.
-type merge_strategy_groups() :: [merge_strategy_groups()].

-spec merge_strategy_priority_map() -> kz_proplist().
merge_strategy_priority_map() ->
    [{<<"simple">>, 10}
    ,{<<"cumulative">>, 20}
    ].

-spec merge_strategy_priority(ne_binary()) -> non_neg_integer().
merge_strategy_priority(Strategy) ->
    props:get_value(Strategy, merge_strategy_priority_map()).
