%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%     Sergey Korobkov
%%%-------------------------------------------------------------------
-module(kz_service_ratedeck).

-export([reconcile/1, reconcile/2]).
-export([get_ratedeck/1]).

-include("kazoo_services.hrl").

-define(SERVICE_CATEGORY, <<"ratedeck">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile(kz_services:services()) -> kz_services:services().
-spec reconcile(kz_services:services(), api_binary()) -> kz_services:services().
reconcile(Services) ->
    PlanJObj = kz_services:service_plan_json(Services),
    %% TODO: resolve conflict when there is more than one ratedeck
    case kz_json:get_keys(kz_json:get_json_value([<<"plan">>, ?SERVICE_CATEGORY], PlanJObj, kz_json:new())) of
        [] -> kz_services:reset_category(?SERVICE_CATEGORY, Services);
        [RatedeckId] -> kz_services:update(?SERVICE_CATEGORY, RatedeckId, 1, Services);
        [_|_] = R ->
            AccountId = kz_services:account_id(Services),
            kz_notify:system_alert("several ratedecks ~p for account ~s", [R, AccountId]),
            lager:error("several ratedecks ~p for account ~s", [R, AccountId]),
            Services
    end.

reconcile(Services, 'undefined') -> kz_services:reset_category(?SERVICE_CATEGORY, Services);
reconcile(Services, RatedeckId) -> kz_services:update(?SERVICE_CATEGORY, RatedeckId, 1, Services).

-spec get_ratedeck(api_binary() | kz_services:services()) -> api_binary().
get_ratedeck('undefined') -> 'undefined';
get_ratedeck(<<_/binary>> = AccountId) ->
    get_ratedeck(kz_services:service_plan_json(AccountId));
get_ratedeck(ServicePlan) ->
    case kzd_service_plan:items(ServicePlan, ?SERVICE_CATEGORY) of
        [] -> 'undefined';
        [RatedeckId] -> RatedeckId
    end.
