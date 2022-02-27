%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2022, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_ratedecks).

-export([fetch/1]).
-export([id/1]).
-export([name/1]).

-include("services.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch(kz_services:services() | kz_term:ne_binary()) -> kz_json:object().
fetch(?NE_BINARY = AccountId) ->
    FetchOptions = ['hydrate_plans'],
    fetch(kz_services:fetch(AccountId, FetchOptions));
fetch(Services) ->
    %% TODO: these are here for backward compatibility
    %% but the ratedeck integration on services and
    %% service_plan documents could use a revisit...
    ServicesJObj = kz_services:services_jobj(Services),
    Plan = merge_all_plans(Services),
    kz_json:from_list(
      [{<<"id">>, get_plan_ratedeck_id(ServicesJObj, Plan)}
      ,{<<"name">>, get_plan_ratedeck_name(ServicesJObj, Plan)}
      ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec id(kz_services:services() | kz_term:ne_binary()) -> kz_term:api_ne_binary().
id(Thing) ->
    kz_doc:id(fetch(Thing)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec name(kz_services:services() | kz_term:ne_binary()) -> kz_term:api_ne_binary().
name(Thing) ->
    kzd_ratedeck:name(fetch(Thing)).

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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_plan_ratedeck_id(kz_json:object(), kz_services_plan:plan()) -> kz_term:api_ne_binary().
get_plan_ratedeck_id(ServicesJObj, Plan) ->
    case kzd_services:ratedeck_id(ServicesJObj) of
        'undefined' -> kz_services_plan:ratedeck_id(Plan);
        RatedeckId -> RatedeckId
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_plan_ratedeck_name(kz_json:object(), kz_services_plan:plan()) -> kz_term:api_ne_binary().
get_plan_ratedeck_name(ServicesJObj, Plan) ->
    case kzd_services:ratedeck_name(ServicesJObj) of
        'undefined' -> kz_services_plan:ratedeck_name(Plan);
        RatedeckName -> RatedeckName
    end.
