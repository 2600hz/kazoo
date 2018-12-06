%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_ratedecks).

-export([id/1]).
-export([name/1]).

-include("services.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec id(kz_services:services() | kz_term:ne_binary()) -> kz_term:api_ne_binary().
id(?NE_BINARY = AccountId) ->
    FetchOptions = ['hydrate_plans'],
    id(kz_services:fetch(AccountId, FetchOptions));
id(Services) ->
    %% TODO: these are here for backward compatibility
    %% but the ratedeck integration on services and
    %% service_plan documents could use a revisit...
    ServicesJObj = kz_services:services_jobj(Services),
    case kzd_services:ratedeck_id(ServicesJObj) of
        'undefined' -> plan_ratedeck_id(Services);
        RatedeckId -> RatedeckId
    end.

-spec plan_ratedeck_id(kz_services:services()) -> kz_term:api_ne_binary().
plan_ratedeck_id(Services) ->
    kz_services_plan:ratedeck_id(
      merge_all_plans(Services)
     ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec name(kz_services:services() | kz_term:ne_binary()) -> kz_term:api_ne_binary().
name(?NE_BINARY = AccountId) ->
    FetchOptions = ['hydrate_plans'],
    name(kz_services:fetch(AccountId, FetchOptions));
name(Services) ->
    %% TODO: these are here for backward compatibility
    %% but the ratedeck integration on services and
    %% service_plan documents could use a revisit...
    ServicesJObj = kz_services:services_jobj(Services),
    case kzd_services:ratedeck_name(ServicesJObj) of
        'undefined' -> plan_ratedeck_name(Services);
        RatedeckName -> RatedeckName
    end.

-spec plan_ratedeck_name(kz_services:services()) -> kz_term:api_ne_binary().
plan_ratedeck_name(Services) ->
    kz_services_plan:ratedeck_name(
      merge_all_plans(Services)
     ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec merge_all_plans(kz_services:services()) -> kz_services_plan:plan().
merge_all_plans(Services) ->
    case kz_services_plans:foldl(fun collect_plans_foldl/3, [], kz_services:plans(Services)) of
        [] -> kz_services_plan:empty();
        Plans -> kz_services_plans:merge(Plans)
    end.

-spec collect_plans_foldl(kz_term:ne_binary()
                         ,kz_services_plans:plans_list()
                         ,kz_services_plans:plans_list()
                         ) -> kz_services_plans:plans_list().
collect_plans_foldl(_BookkeeperHash, PlansList, Plans) ->
    PlansList ++ Plans.
