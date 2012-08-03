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
-export([fetch/1]).
-export([create_items/2]).

-record(wh_service_plans, {vendor_id = undefined
                           ,vendor_db = undefined
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
%% Given an account id fetch object defining the current service
%% plans that should be applied to the account
%% @end
%%--------------------------------------------------------------------
-spec fetch/1 :: (ne_binary()) -> plans().
fetch(AccountId) ->
    Services = case couch_mgr:open_doc(?WH_SERVICES_DB, AccountId) of
                       {ok, JObj} -> JObj;
                       {error, _R} ->
                           lager:debug("unable to open services doc for ~s: ~p", [AccountId, _R]),
                           wh_json:new()
                   end,
    PlanIds = wh_json:get_keys(<<"plans">>, Services),
    get_plans(PlanIds, Services).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a the services on an account (and descedants) as well as the
%% service plans the account is subscribed to create a list of items
%% suitable for use with the bookkeepers.
%% @end
%%--------------------------------------------------------------------
-spec create_items/2 :: (plans(), wh_services:services()) -> wh_service_items:items().
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
get_plans(PlanIds, Sevices) ->
    get_plans(PlanIds, Sevices, empty()).

get_plans([], _, ServicePlans) ->
    ServicePlans;
get_plans([PlanId|PlanIds], Services, ServicePlans) ->
    VendorId = wh_json:get_value([<<"plans">>, PlanId, <<"vendor_id">>], Services),
    VendorDb = wh_util:format_account_id(VendorId, encoded),
    Overrides = wh_json:get_value([<<"plans">>, PlanId, <<"overrides">>], Services, wh_json:new()),
    case wh_service_plan:fetch(PlanId, VendorDb, Overrides) of
        undefined -> get_plans(PlanIds, Services, ServicePlans);
        Plan ->
            case lists:keyfind(VendorId, #wh_service_plans.vendor_id, ServicePlans) of
                false ->
                    ServicePlan = #wh_service_plans{vendor_id = VendorId
                                                    ,vendor_db = VendorDb
                                                    ,plans = [Plan]},
                    get_plans(PlanIds, Services, [ServicePlan|ServicePlans]);
                #wh_service_plans{plans=Plans}=ServicePlan ->
                    lists:keyreplace(VendorId, #wh_service_plans.vendor_id, ServicePlans
                                     ,ServicePlan#wh_service_plans{plans = [Plan|Plans]})
            end
    end.
