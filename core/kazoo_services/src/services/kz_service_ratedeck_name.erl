%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%     Sergey Korobkov
%%%-------------------------------------------------------------------
-module(kz_service_ratedeck_name).
-behaviour(kz_gen_service).

-export([reconcile/1, reconcile/2]).
-export([get_ratedeck_name/1]).

-include("services.hrl").

-define(SERVICE_CATEGORY, <<"ratedeck_name">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile(kz_services:services()) -> kz_services:services().
-spec reconcile(kz_services:services(), api_binary()) -> kz_services:services().
reconcile(Services) ->
    ServicePlanJObj = kz_services:service_plan_json(Services),
    %% TODO: resolve conflict when there is more than one ratedeck

    case kzd_service_plan:items(ServicePlanJObj, ?SERVICE_CATEGORY) of
        [] ->
            kz_services:reset_category(?SERVICE_CATEGORY, Services);
        [RatedeckId] ->
            kz_services:update(?SERVICE_CATEGORY, RatedeckId, 1, Services);
        [_|_] = R ->
            AccountId = kz_services:account_id(Services),
            kz_notify:system_alert("several ratedeck names ~p for account ~s", [R, AccountId]),
            lager:error("several ratedeck names ~p for account ~s", [R, AccountId]),
            Services
    end.

reconcile(Services, 'undefined') ->
    kz_services:reset_category(?SERVICE_CATEGORY, Services);
reconcile(Services, RatedeckName) ->
    kz_services:update(?SERVICE_CATEGORY, RatedeckName, 1, Services).

-spec get_ratedeck_name(api_binary() | kzd_service_plan:doc()) -> api_binary().
get_ratedeck_name('undefined') -> 'undefined';
get_ratedeck_name(<<_/binary>> = AccountId) ->
    get_ratedeck_name(kz_services:service_plan_json(AccountId));
get_ratedeck_name(ServicePlan) ->
    case kzd_service_plan:items(ServicePlan, ?SERVICE_CATEGORY) of
        [] -> 'undefined';
        [RatedeckId] -> RatedeckId
    end.
