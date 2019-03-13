%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_limits).

-export([fetch/1]).

-include("services.hrl").
-include_lib("kazoo_caches/include/kazoo_caches.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch(kz_services:services() | kz_term:ne_binary()) -> kzd_limits:doc().
fetch(?NE_BINARY = AccountId) ->
    fetch(kz_services:fetch(AccountId));
fetch(Services) ->
    #{cache_origins := CacheOrigins
     ,limits := LimitsMap
     } = kz_services_plans:foldl(fun limits_foldl/3
                                ,#{cache_origins => []
                                  ,limits => #{}
                                  }
                                ,kz_services:plans(Services)
                                ),
    PlansLimits = kz_json:from_map(LimitsMap),
    {AccountOrigin, AccountLimits} = get_account_limits(Services),
    Limits = kz_json:merge(PlansLimits, AccountLimits),

    Origins = [{'db', ?KZ_SERVICES_DB, kz_services:account_id(Services)}
               | CacheOrigins ++ AccountOrigin
              ],
    kz_json:set_value(<<"pvt_cache_origins">>
                     ,lists:usort(Origins)
                     ,Limits
                     ).

-spec limits_foldl(kz_term:ne_binary(), kz_services_plans:plans_list(), map()) -> map().
limits_foldl(_BookkeeperHash, [], Acc) ->
    Acc;
limits_foldl(_BookkeeperHash, PlansList, #{cache_origins := CacheOrigins}=Acc) ->
    Origins = [{'db'
               ,kz_util:format_account_db(kz_services_plan:vendor_id(Plan))
               ,kz_services_plan:id(Plan)
               }
               || Plan <- PlansList
              ],
    Plan = kz_services_plans:merge(PlansList),

    kz_json:foldl(fun(K, V, #{limits := Limits}=Acc1) ->
                          Acc1#{limits => Limits#{<<"pvt_", K/binary>> => V}}
                  end
                 ,Acc#{cache_origins => Origins ++ CacheOrigins}
                 ,kz_services_plan:limits(Plan)
                 ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_account_limits(kz_services:services()) -> {origin_tuples(), kz_json:object()}.
get_account_limits(Services) ->
    AccountDb = kz_util:format_account_db(kz_services:account_id(Services)),
    case kz_datamgr:open_doc(AccountDb, <<"limits">>) of
        {'ok', JObj} ->
            {[{'db', AccountDb, <<"limits">>}], JObj};
        {'error', 'not_found'} ->
            lager:debug("limits doc in account db ~s not found, creating it...", [AccountDb]),
            create_account_limits_jobj(AccountDb);
        {'error', _R} ->
            lager:debug("failed to open limits doc in account db '~s': ~p"
                       ,[AccountDb, _R]),
            {[], kz_json:new()}
    end.

-spec create_account_limits_jobj(kz_term:ne_binary()) -> {origin_tuples(), kz_json:object()}.
create_account_limits_jobj(AccountDb) ->
    case kz_datamgr:save_doc(AccountDb, kzd_limits:new(AccountDb)) of
        {'ok', SavedJObj} ->
            lager:debug("created initial limits document in db ~s", [AccountDb]),
            {[{'db', AccountDb, <<"limits">>}], SavedJObj};
        {'error', _R} ->
            lager:debug("failed to create initial limits document in db ~s: ~p", [AccountDb, _R]),
            {[], kz_json:new()}
    end.
