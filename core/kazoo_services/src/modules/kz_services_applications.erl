%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_applications).

-export([fetch/1]).

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
    AppsDict = kz_services_plans:foldl(fun fetch_foldl/3
                                      ,dict:new()
                                      ,kz_services:plans(Services)
                                      ),
    kz_json:from_list(dict:to_list(AppsDict)).

-spec fetch_foldl(kz_term:ne_binary(), kz_services_plans:plans_list(), dict:dict()) -> dict:dict().
fetch_foldl(_BookkeeperHash, [], Apps) ->
    Apps;
fetch_foldl(_BookkeeperHash, PlansList, Apps) ->
    Plan = kz_services_plans:merge(PlansList),
    kz_json:foldl(fun(K, V, A) ->
                          dict:store(K, V, A)
                  end
                 ,Apps
                 ,kz_services_plan:applications(Plan)
                 ).
