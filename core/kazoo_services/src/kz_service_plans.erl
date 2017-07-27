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

-include("services.hrl").

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
    PlanIds = kzd_services:plan_ids(ServicesJObj),
    ?LOG_DEBUG("found plans: ~s", [kz_util:iolist_join($,, PlanIds)]),
    ResellerId = find_reseller_id(ServicesJObj),

    get_plans(PlanIds, ResellerId, ServicesJObj).

-spec find_reseller_id(kzd_services:doc()) -> api_ne_binary().
find_reseller_id(ServicesJObj) ->
    case kzd_services:reseller_id(ServicesJObj) of
        'undefined' -> kz_json:get_ne_binary_value(<<"reseller_id">>, ServicesJObj);
        ResellerId -> ResellerId
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec public_json(plans()) -> kz_json:object().
public_json(ServicePlans) ->
    PlansJObj = lists:foldl(fun merge_service_plans/2, kz_json:new(), ServicePlans),
    kz_doc:public_fields(kzd_service_plan:set_plan(kzd_service_plan:new(), PlansJObj)).

-spec merge_service_plans(plan(), kz_json:object()) -> kz_json:object().
merge_service_plans(#kz_service_plans{plans = Plans}, PlansJObj) ->
    lists:foldl(fun merge_plans/2, PlansJObj, Plans).

-spec merge_plans(kzd_service_plan:doc(), kz_json:object()) -> kz_json:object().
merge_plans(SerivcePlan, PlansJObj) ->
    case kzd_service_plan:plan(SerivcePlan, 'undefined') of
        'undefined' -> PlansJObj;
        Plan -> kz_json:merge(PlansJObj, Plan)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec add_service_plan(ne_binary(), ne_binary(), kzd_services:doc()) -> kzd_services:doc().
add_service_plan(PlanId, ResellerId, ServicesJObj) ->
    ResellerDb = kz_util:format_account_db(ResellerId),
    case open_cache_doc(ResellerDb, PlanId) of
        {'error', _R} ->
            lager:info("failed to load service plan ~s from ~s: ~p", [PlanId, ResellerDb, _R]),
            Plan = kz_json:from_list(
                     [{<<"account_id">>, ResellerId}
                     ]),
            kzd_services:set_plan(ServicesJObj, PlanId, Plan);
        {'ok', ServicePlan} ->
            Plan = kz_json:from_list(
                     [{<<"account_id">>, ResellerId}
                     ,{<<"category">>, kzd_service_plan:grouping_category(ServicePlan)}
                     ]),
            kzd_services:set_plan(ServicesJObj, PlanId, Plan)
    end.

-ifdef(TEST).
open_cache_doc(?A_MASTER_ACCOUNT_DB, ?A_MASTER_PLAN_ID) ->
    {ok, kz_services_test:fixture("a_master_plans.json")}.
-else.
open_cache_doc(Db, Id) ->
    kz_datamgr:open_cache_doc(Db, Id).
-endif.

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
-spec create_items(kzd_services:doc(), plans()) -> kz_service_items:items().

create_items(ServiceJObj) ->
    case from_service_json(ServiceJObj) of
        [] -> {'error', 'no_plans'};
        ServicePlans ->
            {'ok', create_items(ServiceJObj, ServicePlans)}
    end.

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
        {'ok', Items} -> kz_service_items:public_json(Items);
        {'error', _} -> kz_json:new()
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% For each plans object fetch the service plan and store it
%% in the vendors #kz_service_plans data structure.
%% @end
%%--------------------------------------------------------------------
-spec get_plans(ne_binaries(), ne_binary(), kzd_services:doc()) -> plans().
-spec get_plan(ne_binary(), ne_binary(), kzd_services:doc(), plans()) -> plans().

get_plans(PlanIds, ResellerId, Services) ->
    lists:foldl(fun(PlanId, ServicePlans) ->
                        get_plan(PlanId, ResellerId, Services, ServicePlans)
                end
               ,empty()
               ,PlanIds
               ).

get_plan(PlanId, ResellerId, Services, ServicePlans) ->
    VendorId = kzd_services:plan_account_id(Services, PlanId, ResellerId),
    Overrides = kzd_services:plan_overrides(Services, PlanId),
    case maybe_fetch_vendor_plan(PlanId, VendorId, ResellerId, Overrides) of
        'undefined' -> ServicePlans;
        ServicePlan -> append_vendor_plan(ServicePlan, VendorId, ServicePlans)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_fetch_vendor_plan(ne_binary(), ne_binary(), ne_binary(), kz_json:object()) ->
                                     kzd_service_plan:api_doc().
maybe_fetch_vendor_plan(PlanId, VendorId, VendorId, Overrides) ->
    AreOverridesEmpty = kz_json:is_empty(Overrides),

    case kz_service_plan:fetch(PlanId, VendorId) of
        'undefined' -> 'undefined';
        ServicePlan when not AreOverridesEmpty ->
            kzd_service_plan:merge_overrides(ServicePlan, Overrides);
        ServicePlan -> ServicePlan
    end;
maybe_fetch_vendor_plan(PlanId, _, ResellerId, _) ->
    lager:debug("service plan ~s doesnt belong to reseller ~s", [PlanId, ResellerId]),
    'undefined'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a plan and a vendor id append it to the list of service plans
%% for that vendor, creating a new list (record) if not present.
%% @end
%%--------------------------------------------------------------------
-spec append_vendor_plan(kzd_service_plan:doc(), ne_binary(), plans()) -> plans().
append_vendor_plan(Plan, VendorId, ServicePlans) ->
    case lists:keyfind(VendorId, #kz_service_plans.vendor_id, ServicePlans) of
        'false' ->
            ServicePlan = #kz_service_plans{vendor_id = VendorId
                                           ,plans = [Plan]
                                           },
            [ServicePlan|ServicePlans];
        #kz_service_plans{plans = Plans}=ServicePlan ->
            lists:keyreplace(VendorId
                            ,#kz_service_plans.vendor_id
                            ,ServicePlans
                            ,ServicePlan#kz_service_plans{plans = [Plan|Plans]}
                            )
    end.
