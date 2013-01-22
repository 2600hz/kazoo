%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Shared functions
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(reg_util).

-export([lookup_auth_user/2
         ,cache_reg_key/1, cache_user_to_reg_key/2, cache_user_key/2
         ,hash_contact/1
         ,get_expires/1
         ,lookup_registrations/1, lookup_registration/2
         ,fetch_all_registrations/0
         ,reg_removed_from_cache/3
         ,search_for_registration/2
         ,remove_registration/2
        ]).

-include_lib("registrar/src/reg.hrl").

cache_reg_key(Id) -> {?MODULE, registration, Id}.
cache_user_to_reg_key(Realm, User) -> {?MODULE, registration, Realm, User}.
cache_user_key(Realm, User) -> {?MODULE, sip_credentials, Realm, User}.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% look up a cached registration by realm and optionally username
%% @end
%%-----------------------------------------------------------------------------
-spec lookup_registrations/1 :: (ne_binary()) -> {'ok', wh_json:objects()} |
                                                 {'error', 'not_found'}.
lookup_registrations(Realm) when not is_binary(Realm) ->
    lookup_registrations(wh_util:to_binary(Realm));
lookup_registrations(Realm) ->
    Registrations = wh_cache:filter_local(?REGISTRAR_CACHE, fun({?MODULE, registration, Realm1, _}, _) when Realm =:= Realm1 ->
                                                                    true;
                                                               (_K, _V) ->
                                                                    false
                                                            end),
    case [V || {_, V} <- Registrations] of
        [] -> {error, not_found};
        Else -> {'ok', Else}
    end.

-spec lookup_registration/2 :: (ne_binary(), api_binary()) ->
                                       {'ok', wh_json:object()} |
                                       {'ok', wh_json:objects()} | % if no username, find all for realm
                                       {'error', 'not_found'}.
lookup_registration(Realm, undefined) ->
    lookup_registrations(Realm);
lookup_registration(Realm, Username) when not is_binary(Realm) ->
    lookup_registration(wh_util:to_binary(Realm), Username);
lookup_registration(Realm, Username) when not is_binary(Username) ->
    lookup_registration(Realm, wh_util:to_binary(Username));
lookup_registration(Realm, Username) ->
    wh_cache:peek_local(?REGISTRAR_CACHE, cache_user_to_reg_key(Realm, Username)).

-spec remove_registration/2 :: (ne_binary(), ne_binary()) -> 'ok'.
remove_registration(Realm, Username) ->
    wh_cache:erase_local(?REGISTRAR_CACHE, cache_user_to_reg_key(Realm, Username)).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% get a complete list of registrations in the cache
%% @end
%%-----------------------------------------------------------------------------
-spec fetch_all_registrations/0 :: () -> {'ok', wh_json:objects()}.
fetch_all_registrations() ->
    Registrations = wh_cache:filter_local(?REGISTRAR_CACHE
                                          ,fun({?MODULE, registration, _, _}, _) -> true;
                                              (_K, _V) -> false
                                           end),
    {'ok', [Registration || {_, Registration} <- Registrations]}.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% calculate expiration time
%% @end
%%-----------------------------------------------------------------------------
-spec get_expires/1 :: (wh_json:object()) -> integer().
get_expires(JObj) ->
    Multiplier = whapps_config:get_float(?CONFIG_CAT, <<"expires_multiplier">>, 1.25),
    Fudge = whapps_config:get_integer(?CONFIG_CAT, <<"expires_fudge_factor">>, 120),
    Expiry = wh_json:get_integer_value(<<"Expires">>, JObj, 3600),
    erlang:trunc(Expiry * Multiplier) + Fudge.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% hash a registration contact string
%% @end
%%-----------------------------------------------------------------------------
-spec hash_contact/1 :: (ne_binary()) -> ne_binary().
hash_contact(Contact) ->
    wh_util:to_hex_binary(erlang:md5(Contact)).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% look up the user and realm in the database and return the result
%% @end
%%-----------------------------------------------------------------------------
-spec lookup_auth_user/2 :: (ne_binary(), ne_binary()) ->
                                    {'ok', wh_json:object()} |
                                    {'error', 'not_found'}.
lookup_auth_user(Name, Realm) ->
    CacheKey = cache_user_key(Realm, Name),
    case wh_cache:peek_local(?REGISTRAR_CACHE, CacheKey) of
        {'error', not_found} ->
            case get_auth_user(Name, Realm) of
                {'ok', UserJObj} -> check_user_doc(UserJObj, CacheKey);
                {error, _}=E -> E
            end;
        {'ok', UserJObj}=OK ->
            case wh_util:is_account_enabled(wh_json:get_value([<<"doc">>, <<"pvt_account_id">>], UserJObj)) of
                true -> OK;
                false -> {error, not_found}
            end
    end.

check_user_doc(UserJObj, CacheKey) ->
    case wh_util:is_account_enabled(wh_json:get_value([<<"doc">>, <<"pvt_account_id">>], UserJObj)) of
        true ->
            CacheTTL = whapps_config:get_integer(?CONFIG_CAT, <<"credentials_cache_ttl">>, 300),
            wh_cache:store_local(?REGISTRAR_CACHE, CacheKey, UserJObj, CacheTTL
                                 ,fun reg_util:reg_removed_from_cache/3
                                ),
            {ok, UserJObj};
        false -> {error, not_found}
    end.

-spec get_auth_user/2 :: (ne_binary(), ne_binary()) ->
                                 {'ok', wh_json:object()} |
                                 {'error', 'not_found'}.
get_auth_user(Name, Realm) ->
    case whapps_util:get_account_by_realm(Realm) of
        {'error', E} ->
            lager:debug("failed to lookup realm ~s in accounts: ~p", [Realm, E]),
            get_auth_user_in_agg(Name, Realm);
        {'multiples', []} ->
            lager:debug("failed to find realm ~s in accounts", [Realm]),
            get_auth_user_in_agg(Name, Realm);
        {'multiples', [AccountDB|_]} ->
            lager:debug("found multiple accounts by realm ~s, using first: ~s", [Realm, AccountDB]),
            get_auth_user_in_account(Name, Realm, AccountDB);
        {'ok', AccountDB} ->
            get_auth_user_in_account(Name, Realm, AccountDB)
    end.

-spec get_auth_user_in_agg/2 :: (ne_binary(), ne_binary()) ->
                                        {'ok', wh_json:object()} |
                                        {'error', 'not_found'}.
get_auth_user_in_agg(Name, Realm) ->
    UseAggregate = whapps_config:get_is_true(?CONFIG_CAT, <<"use_aggregate">>, false),
    ViewOptions = [{key, [Realm, Name]}, include_docs],
    case UseAggregate andalso couch_mgr:get_results(?WH_SIP_DB, <<"credentials/lookup">>, ViewOptions) of
        false ->
            lager:debug("SIP credential aggregate db is disabled"),
            {'error', 'not_found'};
        {'error', R} ->
            lager:debug("failed to look up SIP credentials ~p in aggregate", [R]),
            {'error', 'not_found'};
        {'ok', []} ->
            lager:debug("~s@~s not found in aggregate", [Name, Realm]),
            {'error', 'not_found'};
        {'ok', [User|_]} ->
            lager:debug("~s@~s found in aggregate", [Name, Realm]),
            {'ok', User}
    end.

-spec get_auth_user_in_account/3 :: (ne_binary(), ne_binary(), ne_binary()) ->
                                            {'ok', wh_json:object()} |
                                            {'error', 'not_found'}.
get_auth_user_in_account(Name, Realm, AccountDB) ->
    case couch_mgr:get_results(AccountDB, <<"devices/sip_credentials">>, [{key, Name}, include_docs]) of
        {'error', R} ->
            lager:debug("failed to look up SIP credentials in ~s: ~p", [AccountDB, R]),
            get_auth_user_in_agg(Name, Realm);
        {'ok', []} ->
            lager:debug("~s@~s not found in ~s", [Name, Realm, AccountDB]),
            get_auth_user_in_agg(Name, Realm);
        {'ok', [User|_]} ->
            lager:debug("~s@~s found in account db: ~s", [Name, Realm, AccountDB]),
            {'ok', User}
    end.

-spec reg_removed_from_cache/3 :: (term(), term(), 'expire' | 'flush' | 'erase') -> 'ok'.
reg_removed_from_cache({?MODULE, registration, Realm, User}, Reg, expire) ->
    lager:debug("received notice that user ~s@~s registration has expired", [User, Realm]),
    SuppressUnregister = wh_json:is_true(<<"Suppress-Unregister-Notify">>, Reg),
    case search_for_registration(User, Realm) of
        {ok, _} ->
            lager:debug("registration still exists in another segment, defering to their expiration");
        {error, timeout} when SuppressUnregister ->
            lager:debug("registration for ~s@~s has expired in this segment, but notifications are suppressed", [Realm, User]);
        {error, timeout} ->
            lager:debug("registration for ~s@~s has expired in this segment, sending notification", [Realm, User]),
            Updaters = [fun(J) -> wh_json:set_value(<<"Event-Name">>,  <<"deregister">>, J) end
                        ,fun(J) -> wh_json:set_value(<<"Event-Category">>, <<"notification">>, J) end
                        ,fun(J) -> wh_json:delete_key(<<"App-Version">>, J) end
                        ,fun(J) -> wh_json:delete_key(<<"App-Name">>, J) end
                        ,fun(J) -> wh_json:delete_key(<<"Server-ID">>, J) end
                       ],
            Event = wh_json:to_proplist(lists:foldr(fun(F, J) -> F(J) end, Reg, Updaters))
                ++ wh_api:default_headers(?APP_NAME, ?APP_VERSION),
            wapi_notifications:publish_deregister(Event)
    end;
reg_removed_from_cache(_, _, _) -> ok.

-spec search_for_registration/2 :: (ne_binary(), ne_binary()) ->
                                           {'ok', wh_json:object()} |
                                           {'error', 'timeout'}.
search_for_registration(User, Realm) ->
    wh_amqp_worker:call(whapps_amqp_pool
                        ,[{<<"Username">>, User}
                          ,{<<"Realm">>, Realm}
                          ,{<<"Fields">>, [<<"Username">>, <<"Realm">>]}
                          | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                         ]
                        ,fun wapi_registration:publish_query_req/1
                        ,fun wapi_registration:query_resp_v/1
                       ).
