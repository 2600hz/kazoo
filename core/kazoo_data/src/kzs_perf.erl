%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Profiling
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzs_perf).

-include("kz_data.hrl").

-define(CACHE_PROFILE_FROM_FILE, kz_json:load_fixture_from_file(?APP, "defaults", "perf.json")).
-define(CACHE_PROFILE_OPTS, [{'origin', [{'db', ?KZ_CONFIG_DB, ?CONFIG_CAT}]}
                            ,{'expires', 'infinity'}
                            ]).

%%==============================================================================
%% API functions
%%==============================================================================

-export([profile/2]).

-spec profile(mfa(), list()) -> any().
profile({Mod, Fun, Arity}=MFA, Args) ->
    case profile_match(Mod, Fun, Arity) of
        #{} -> do_profile(MFA, Args, #{});
        _ -> erlang:apply(Mod, Fun, Args)
    end.

%%==============================================================================
%% Internal functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec load_profile_config_from_disk() -> kz_json:object().
load_profile_config_from_disk() ->
    Doc = ?CACHE_PROFILE_FROM_FILE,
    kz_json:get_value(<<"performance">>, Doc, kz_json:new()).

-spec load_profile_config() -> kz_json:object().
load_profile_config() ->
    case kazoo_data_config:get_json(<<"performance">>) of
        'undefined' -> load_profile_config_from_disk();
        JObj -> JObj
    end.

-spec profile_config() -> map().
profile_config() ->
    case kz_cache:fetch_local(?KAZOO_DATA_PLAN_CACHE, {?MODULE, 'config'}) of
        {'error', 'not_found'} ->
            kz_process:spawn(fun update_profile_config/0),
            Config = load_profile_config_from_disk(),
            update_profile_config(Config);
        {'ok', Map} -> Map
    end.

-spec update_profile_config() -> map().
update_profile_config() ->
    lager:info("deferring update profile config 1 minute"),
    timer:sleep(?SECONDS_IN_MINUTE),
    update_profile_config(load_profile_config()).

-spec update_profile_config(kz_json:object()) -> map().
update_profile_config(JObj) ->
    Map = kz_maps:keys_to_atoms(kz_json:to_map(JObj)),
    kz_cache:store_local(?KAZOO_DATA_PLAN_CACHE, {?MODULE, 'config'}, Map, ?CACHE_PROFILE_OPTS),
    Map.

-spec profile_match(atom(), atom(), arity()) -> map() | 'undefined'.
profile_match(Mod, Fun, Arity) ->
    try
        #{ Mod := #{ Fun := #{'arity' := Arity, 'enabled' := 'true'}=M}} = profile_config(),
        M
    catch
        _E:_R -> 'undefined'
    end.

-spec do_profile({atom(), atom(), arity()}, list(), map()) -> any().
do_profile({Mod, Fun, _Arity}, Args, PD) ->
    [Plan, DbName | Others] = Args,
    {Time, Result} = timer:tc(Mod, Fun, Args),
    From = kz_util:calling_process(),
    FromList = [{kz_term:to_atom(<<"from_", (kz_term:to_binary(K))/binary>>, true), V} || {K,V} <- maps:to_list(From)],
    MD = FromList ++ maps:to_list(maps:merge(Plan, PD)),
    _ = data:debug([{'mod', Mod}
                   ,{'func', Fun}
                   ,{'plan', Plan}
                   ,{'duration', Time}
                   ,{'database', DbName}
                   ,{'from', From}
                    | MD
                   ]
                  ,"execution of {~s:~s} in database ~s with args ~p took ~b μs"
                  ,[Mod, Fun, DbName, Others, Time]
                  ),
    Result.
