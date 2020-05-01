%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_asr).

-export([fetch/1
        ,flat_rate/1, flat_rate/2
        ]).

-include("services.hrl").

-define(DEFAULT_FLAT_RATE, 0).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch(kz_services:services() | kz_term:ne_binary()) -> kz_json:object().
fetch(?NE_BINARY=AccountId) ->
    FetchOptions = ['hydrate_plans'],
    fetch(kz_services:fetch(AccountId, FetchOptions));
fetch(Services) ->
    ASRDict = kz_services_plans:foldl(fun fetch_foldl/3
                                     ,dict:new()
                                     ,kz_services:plans(Services)
                                     ),
    kz_json:from_list(dict:to_list(ASRDict)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch_foldl(kz_term:ne_binary(), kz_services_plans:plans_list(), dict:dict()) -> dict:dict().
fetch_foldl(_BookkeeperHash, [], Providers) ->
    Providers;
fetch_foldl(_BookkeeperHash, PlansList, Providers) ->
    Plan = kz_services_plans:merge(PlansList),
    kz_json:foldl(fun(K, V, A) ->
                          dict:store(K, V, A)
                  end
                 ,Providers
                 ,kz_services_plan:asr(Plan)
                 ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec flat_rate(kz_term:ne_binary()) -> kz_currency:dollars().
flat_rate(AccountId) ->
    flat_rate(AccountId, kazoo_asr:default_provider()).

-spec flat_rate(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_currency:dollars().
flat_rate(AccountId, Provider) ->
    Items = fetch(AccountId),
    kz_json:get_number_value([Provider, <<"rate">>], Items, ?DEFAULT_FLAT_RATE).
