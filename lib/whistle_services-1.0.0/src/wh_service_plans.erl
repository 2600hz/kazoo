%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_plans).

-include_lib("whistle_services/src/whistle_services.hrl").

-export([empty/0]).
-export([from_service_json/1]).
-export([create_items/1
         ,create_items/2
        ]).

-record(wh_service_plans, {vendor_id = undefined
                           ,plans = []
                          }).

-type(plans() :: [#wh_service_plans{},...] | []).
-export_type([plans/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create an empty service plans data structure.
%% @end
%%--------------------------------------------------------------------
-spec empty/0 :: () -> plans().
empty() -> [].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec from_service_json/1 :: (wh_json:json_object()) -> plans().
from_service_json(ServicesJObj) ->
    PlanIds = wh_json:get_keys(<<"plans">>, ServicesJObj),
    get_plans(PlanIds, ServicesJObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a the services on an account (and descedants) as well as the
%% service plans the account is subscribed to create a list of items
%% suitable for use with the bookkeepers.
%% @end
%%--------------------------------------------------------------------
-spec create_items/1 :: (wh_json:json_object()) -> {'ok', wh_service_items:items()} | {'error', 'no_plans'}.
-spec create_items/2 :: (wh_services:services(), plans()) -> wh_service_items:items().

create_items(ServiceJObj) ->
    Services = wh_services:from_service_json(ServiceJObj),
    case from_service_json(ServiceJObj) of
        [] -> {error, no_plans};
        ServicePlans ->
            {ok, create_items(Services, ServicePlans)}
    end.

create_items(Services, ServicePlans) ->
    Plans = [Plan
             || ServicePlan <- ServicePlans
                    ,Plan <- ServicePlan#wh_service_plans.plans
            ],
    lists:foldl(fun(Plan, Items) ->
                        wh_service_plan:create_items(Plan, Items, Services)
                end, wh_service_items:empty(), Plans).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% For each plans object fetch the service plan and store it
%% in the vendors #wh_service_plans data structure.
%% @end
%%--------------------------------------------------------------------
-spec get_plans/2 :: ([ne_binary(),...] | [], wh_json:json_object()) -> plans().
-spec get_plans/3 :: ([ne_binary(),...] | [], wh_json:json_object(), plans()) -> plans().

get_plans(PlanIds, Sevices) ->
    get_plans(PlanIds, Sevices, empty()).

get_plans([], _, ServicePlans) ->
    ServicePlans;
get_plans([PlanId|PlanIds], Services, ServicePlans) ->
    VendorId = wh_json:get_value([<<"plans">>, PlanId, <<"vendor_id">>], Services),
    Overrides = wh_json:get_value([<<"plans">>, PlanId, <<"overrides">>], Services, wh_json:new()),
    case wh_service_plan:fetch(PlanId, VendorId, Overrides) of
        undefined -> get_plans(PlanIds, Services, ServicePlans);
        Plan -> get_plans(PlanIds, Services, append_vendor_plan(Plan, VendorId, ServicePlans))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a plan and a vendor id append it to the list of service plans
%% for that vendor, creating a new list (record) if not present.
%% @end
%%--------------------------------------------------------------------
-spec append_vendor_plan/3 :: (wh_service_plan:plan(), ne_binary(), plans()) -> plans().
append_vendor_plan(Plan, VendorId, ServicePlans) ->
    case lists:keyfind(VendorId, #wh_service_plans.vendor_id, ServicePlans) of
        false -> 
            ServicePlan = #wh_service_plans{vendor_id=VendorId
                                            ,plans=[Plan]},
            [ServicePlan|ServicePlans];
        #wh_service_plans{plans=Plans}=ServicePlan ->
            lists:keyreplace(VendorId, #wh_service_plans.vendor_id, ServicePlans
                             ,ServicePlan#wh_service_plans{plans=[Plan|Plans]})
    end.
