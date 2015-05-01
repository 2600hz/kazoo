%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_plans).

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

-include("whistle_services.hrl").

-record(wh_service_plans, {vendor_id :: api_binary()
                           ,plans = [] :: wh_json:objects()
                          }).

-type plans() :: [#wh_service_plans{},...] | [].
-export_type([plans/0]).

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
    PlanIds = wh_json:get_keys(kzd_services:plans(ServicesJObj)),
    ResellerId = find_reseller_id(ServicesJObj),
    get_plans(PlanIds, ResellerId, ServicesJObj).

-spec find_reseller_id(kzd_services:doc()) -> api_binary().
find_reseller_id(ServicesJObj) ->
    case kzd_services:reseller_id(ServicesJObj) of
        'undefined' -> wh_json:get_value(<<"reseller_id">>, ServicesJObj);
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
    public_json(ServicePlans, wh_json:new()).

-spec public_json(plans(), wh_json:object()) -> kzd_service_plan:doc().
public_json([], JObj) -> kzd_service_plan:set_plan(kzd_service_plan:new(), JObj);
public_json([#wh_service_plans{plans=Plans}|ServicePlans], JObj) ->
    NewJObj = lists:foldl(fun merge_plans/2, JObj, Plans),
    public_json(ServicePlans, NewJObj).

-spec merge_plans(kzd_service_plan:doc(), wh_json:object()) -> wh_json:object().
merge_plans(SerivcePlan, JObj) ->
    wh_json:merge_recursive(JObj, kzd_service_plan:plan(SerivcePlan)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec add_service_plan(ne_binary(), ne_binary(), kzd_services:doc()) -> kzd_services:doc().
add_service_plan(PlanId, ResellerId, ServicesJObj) ->
    Plan = wh_json:from_list([{<<"account_id">>, ResellerId}]),
    kzd_services:set_plan(ServicesJObj, PlanId, Plan).

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
-spec plan_summary(kzd_services:doc()) -> wh_json:object().
plan_summary(ServicesJObj) ->
    ResellerId = kzd_services:reseller_id(ServicesJObj),
    lists:foldl(fun(PlanId, J) ->
                        Plan = kzd_services:plan(ServicesJObj, PlanId),
                        case kzd_service_plan:account_id(Plan) of
                            ResellerId -> wh_json:set_value(PlanId, Plan, J);
                            _Else -> J
                        end
                end
                ,wh_json:new()
                ,wh_json:get_keys(kzd_services:plans(ServicesJObj))
               ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec activation_charges(ne_binary(), ne_binary(), plans()) -> float().
activation_charges(Category, Item, ServicePlans) ->
    Plans = [Plan
             || ServicePlan <- ServicePlans,
                Plan <- ServicePlan#wh_service_plans.plans
            ],
    lists:foldl(fun(Plan, Charges) ->
                        wh_service_plan:activation_charges(Category, Item, Plan)
                            + Charges
                end, 0.0, Plans).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a the services on an account (and descedants) as well as the
%% service plans the account is subscribed to create a list of items
%% suitable for use with the bookkeepers.
%% @end
%%--------------------------------------------------------------------
-spec create_items(kzd_services:doc()) ->
                          {'ok', wh_service_items:items()} |
                          {'error', 'no_plans'}.
-spec create_items(wh_services:services(), plans()) -> wh_service_items:items().

create_items(ServiceJObj) ->
    Services = wh_services:from_service_json(ServiceJObj),
    case from_service_json(ServiceJObj) of
        [] -> {'error', 'no_plans'};
        ServicePlans ->
            {'ok', create_items(Services, ServicePlans)}
    end.

create_items(Services, ServicePlans) ->
    Plans = [Plan
             || #wh_service_plans{plans=Plans} <- ServicePlans,
                Plan <- Plans
            ],
    lists:foldl(fun(Plan, Items) ->
                        wh_service_plan:create_items(Plan, Items, Services)
                end
                ,wh_service_items:empty()
                ,Plans
               ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Return a json object with all the items for an account
%% @end
%%--------------------------------------------------------------------
-spec public_json_items(kzd_services:doc()) -> wh_json:object().
public_json_items(ServiceJObj) ->
    case create_items(ServiceJObj) of
        {'ok', Items} ->
            wh_service_items:public_json(Items);
        {'error', _} ->
            wh_json:new()
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% For each plans object fetch the service plan and store it
%% in the vendors #wh_service_plans data structure.
%% @end
%%--------------------------------------------------------------------
-spec get_plans(ne_binaries(), ne_binary(), kzd_services:doc()) -> plans().
-spec get_plans(ne_binaries(), ne_binary(), kzd_services:doc(), plans()) -> plans().

get_plans(PlanIds, ResellerId, Services) ->
    get_plans(PlanIds, ResellerId, Services, empty()).

get_plans([], _, _, ServicePlans) ->
    ServicePlans;
get_plans([PlanId|PlanIds], ResellerId, Services, ServicePlans) ->
    VendorId = kzd_services:plan_account_id(Services, PlanId, ResellerId),
    Overrides = kzd_services:plan_overrides(Services, PlanId),

    case maybe_fetch_vendor_plan(PlanId, VendorId, ResellerId, Overrides) of
        'undefined' -> get_plans(PlanIds, ResellerId, Services, ServicePlans);
        Plan -> get_plans(PlanIds, ResellerId, Services, append_vendor_plan(Plan, VendorId, ServicePlans))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_fetch_vendor_plan(ne_binary(), ne_binary(), ne_binary(), wh_json:object()) ->
                                     api_object().
maybe_fetch_vendor_plan(PlanId, VendorId, VendorId, Overrides) ->
    wh_service_plan:fetch(PlanId, VendorId, Overrides);
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
-spec append_vendor_plan(wh_json:object(), ne_binary(), plans()) -> plans().
append_vendor_plan(Plan, VendorId, ServicePlans) ->
    case lists:keyfind(VendorId, #wh_service_plans.vendor_id, ServicePlans) of
        'false' ->
            ServicePlan = #wh_service_plans{vendor_id=VendorId
                                            ,plans=[Plan]
                                           },
            [ServicePlan|ServicePlans];
        #wh_service_plans{plans=Plans}=ServicePlan ->
            lists:keyreplace(VendorId
                             ,#wh_service_plans.vendor_id
                             ,ServicePlans
                             ,ServicePlan#wh_service_plans{plans=[Plan|Plans]}
                            )
    end.
