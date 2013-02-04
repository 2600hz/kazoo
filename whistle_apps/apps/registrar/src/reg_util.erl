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
         ,cache_user_to_reg_key/2
         ,hash_contact/1
         ,get_expires/1
         ,lookup_registrations/1, lookup_registration/2
         ,fetch_all_registrations/0
         ,reg_removed_from_cache/3
         ,search_for_registration/2
         ,remove_registration/2
        ]).

-include("reg.hrl").

cache_user_to_reg_key(Realm, Username) -> {?MODULE, 'registration', Realm, Username}.

cache_auth_user_key(Realm, Username) -> {?MODULE, 'auth_user', Realm, Username}.
    
%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% look up a cached registration by realm and optionally username
%% @end
%%-----------------------------------------------------------------------------
-spec lookup_registrations(ne_binary()) -> {'ok', wh_json:objects()} | {'error', 'not_found'}.
lookup_registrations(Realm) when not is_binary(Realm) ->
    lookup_registrations(wh_util:to_binary(Realm));
lookup_registrations(Realm) ->
    Filter = fun({?MODULE, 'registration', R, _}, _) ->
                     R =:= Realm;
                (_K, _V) -> 'false'
             end,
    case [V || {_, V} <- wh_cache:filter_local(?REGISTRAR_CACHE, Filter)] of
        [] -> {'error', 'not_found'};
        Else -> {'ok', Else}
    end.

-spec lookup_registration(ne_binary(), api_binary()) ->
                                 {'ok', wh_json:object()} |
                                 {'ok', wh_json:objects()} |
                                 {'error', 'not_found'}.
lookup_registration(Realm, 'undefined') ->
    lookup_registrations(Realm);
lookup_registration(Realm, Username) when not is_binary(Realm) ->
    lookup_registration(wh_util:to_binary(Realm), Username);
lookup_registration(Realm, Username) when not is_binary(Username) ->
    lookup_registration(Realm, wh_util:to_binary(Username));
lookup_registration(Realm, Username) ->
    wh_cache:peek_local(?REGISTRAR_CACHE, cache_user_to_reg_key(Realm, Username)).

-spec remove_registration(ne_binary(), ne_binary()) -> 'ok'.
remove_registration(Realm, Username) ->
    wh_cache:erase_local(?REGISTRAR_CACHE, cache_user_to_reg_key(Realm, Username)).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% get a complete list of registrations in the cache
%% @end
%%-----------------------------------------------------------------------------
-spec fetch_all_registrations() -> {'ok', wh_json:objects()}.
fetch_all_registrations() ->
    Registrations = wh_cache:filter_local(?REGISTRAR_CACHE
                                          ,fun({?MODULE, 'registration', _, _}, _) -> 'true';
                                              (_K, _V) -> 'false'
                                           end),
    {'ok', [Registration || {_, Registration} <- Registrations]}.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% calculate expiration time
%% @end
%%-----------------------------------------------------------------------------
-spec get_expires(wh_json:object()) -> integer().
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
-spec hash_contact(ne_binary()) -> ne_binary().
hash_contact(Contact) ->
    wh_util:to_hex_binary(erlang:md5(Contact)).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% look up the user and realm in the database and return the result
%% @end
%%-----------------------------------------------------------------------------
-spec lookup_auth_user(ne_binary(), ne_binary()) ->
                              {'ok', #auth_user{}} |
                              {'error', 'not_found'}.
lookup_auth_user(Username, Realm) ->
    case wh_cache:peek_local(?REGISTRAR_CACHE, cache_auth_user_key(Realm, Username)) of
        {'ok', _}=Ok -> Ok; 
        {'error', 'not_found'} ->
            maybe_fetch_auth_user(Username, Realm)
    end.

-spec maybe_fetch_auth_user(ne_binary(), ne_binary()) ->
                                   {'ok', #auth_user{}} |
                                   {'error', _}.            
maybe_fetch_auth_user(Username, Realm) ->
    case get_auth_user(Username, Realm) of
        {'error', _}=E -> E;
        {'ok', JObj} ->
            check_auth_user(JObj, Username, Realm)
    end.

-spec check_auth_user(wh_json:object(), ne_binary(), ne_binary()) ->
                             {'ok', #auth_user{}} |
                             {'error', _}.
check_auth_user(JObj, Username, Realm) ->
    case wh_util:is_account_enabled(wh_json:get_value([<<"doc">>, <<"pvt_account_id">>], JObj)) of
        'false' -> {'error', 'not_found'};
        'true' -> prepare_response(JObj, Username, Realm)
    end.

-spec prepare_response(wh_json:object(), ne_binary(), ne_binary()) -> {'ok', #auth_user{}}.
prepare_response(JObj, Username, Realm) ->    
    AuthUser = jobj_to_auth_user(JObj, Username, Realm),
    CacheProps = [{origin, [{db, AuthUser#auth_user.account_db, AuthUser#auth_user.authorizing_id}
                            ,{db, AuthUser#auth_user.account_db, AuthUser#auth_user.account_id}
                           ]}
                 ],
    wh_cache:store_local(?REGISTRAR_CACHE, cache_auth_user_key(Realm, Username), AuthUser, CacheProps),
    {ok, AuthUser}.

-spec get_auth_user(ne_binary(), ne_binary()) ->
                           {'ok', wh_json:object()} |
                           {'error', 'not_found'}.
get_auth_user(Username, Realm) ->
    case whapps_util:get_account_by_realm(Realm) of
        {'error', E} ->
            lager:debug("failed to lookup realm ~s in accounts: ~p", [Realm, E]),
            get_auth_user_in_agg(Username, Realm);
        {'multiples', []} ->
            lager:debug("failed to find realm ~s in accounts", [Realm]),
            get_auth_user_in_agg(Username, Realm);
        {'multiples', [AccountDB|_]} ->
            lager:debug("found multiple accounts by realm ~s, using first: ~s", [Realm, AccountDB]),
            get_auth_user_in_account(Username, Realm, AccountDB);
        {'ok', AccountDB} ->
            get_auth_user_in_account(Username, Realm, AccountDB)
    end.

-spec get_auth_user_in_agg(ne_binary(), ne_binary()) ->
                                  {'ok', wh_json:object()} |
                                  {'error', 'not_found'}.
get_auth_user_in_agg(Username, Realm) ->
    ViewOptions = [{'key', [Realm, Username]}
                   ,'include_docs'
                  ],
    case whapps_config:get_is_true(?CONFIG_CAT, <<"use_aggregate">>, 'false')
        andalso couch_mgr:get_results(?WH_SIP_DB, <<"credentials/lookup">>, ViewOptions) 
    of
        'false' ->
            lager:debug("SIP credential aggregate db is disabled"),
            {'error', 'not_found'};
        {'error', _R} ->
            lager:warning("failed to look up SIP credentials ~p in aggregate", [_R]),
            {'error', 'not_found'};
        {'ok', []} ->
            lager:debug("~s@~s not found in aggregate", [Username, Realm]),
            {'error', 'not_found'};
        {'ok', [User|_]} ->
            lager:debug("~s@~s found in aggregate", [Username, Realm]),
            {'ok', User}
    end.

-spec get_auth_user_in_account(ne_binary(), ne_binary(), ne_binary()) ->
                                      {'ok', wh_json:object()} |
                                      {'error', 'not_found'}.
get_auth_user_in_account(Username, Realm, AccountDB) ->
    ViewOptions = [{'key', Username}
                   ,'include_docs'
                  ],
    case couch_mgr:get_results(AccountDB, <<"devices/sip_credentials">>, ViewOptions) of
        {'error', _R} ->
            lager:warning("failed to look up SIP credentials in ~s: ~p", [AccountDB, _R]),
            get_auth_user_in_agg(Username, Realm);
        {'ok', []} ->
            lager:debug("~s@~s not found in ~s", [Username, Realm, AccountDB]),
            get_auth_user_in_agg(Username, Realm);
        {'ok', [User|_]} ->
            lager:debug("~s@~s found in account db: ~s", [Username, Realm, AccountDB]),
            {'ok', User}
    end.

-spec reg_removed_from_cache(term(), term(), 'expire' | 'flush' | 'erase') -> 'ok'.
reg_removed_from_cache({?MODULE, 'registration', Realm, User}, Reg, 'expire') ->
    lager:debug("received notice that user ~s@~s registration has expired", [User, Realm]),
    SuppressUnregister = wh_json:is_true(<<"Suppress-Unregister-Notify">>, Reg),
    case search_for_registration(User, Realm) of
        {ok, _} ->
            lager:debug("registration still exists in another segment, defering to their expiration");
        {error, timeout} when SuppressUnregister ->
            lager:info("registration for ~s@~s has expired in this segment, but notifications are suppressed", [Realm, User]);
        {error, timeout} ->
            lager:info("registration for ~s@~s has expired in this segment, sending notification", [Realm, User]),
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
reg_removed_from_cache(_, _, _) -> 'ok'.

-spec search_for_registration(ne_binary(), ne_binary()) ->
                                     {'ok', wh_json:object()} |
                                     {'error', 'timeout'}.
search_for_registration(User, Realm) ->
    wh_amqp_worker:call('whapps_amqp_pool'
                        ,[{<<"Username">>, User}
                          ,{<<"Realm">>, Realm}
                          ,{<<"Fields">>, [<<"Username">>, <<"Realm">>]}
                          | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                         ]
                        ,fun wapi_registration:publish_query_req/1
                        ,fun wapi_registration:query_resp_v/1
                       ).

-spec jobj_to_auth_user(wh_json:object(), ne_binary(), ne_binary()) -> #auth_user{}.
jobj_to_auth_user(JObj, Username, Realm) ->
    AuthValue = wh_json:get_value(<<"value">>, JObj),
    AuthDoc = wh_json:get_value(<<"doc">>, JObj),
    #auth_user{realm = Realm
               ,username = Username
               ,password = wh_json:get_value(<<"password">>, AuthValue)
               ,account_id = get_account_id(AuthDoc)
               ,account_db = get_account_db(AuthDoc)
               ,authorizing_type = wh_json:get_value(<<"authorizing_type">>, AuthValue, <<"anonymous">>)
               ,authorizing_id = wh_json:get_value(<<"id">>, JObj)
               ,method = get_auth_method(AuthValue)
               ,owner_id = wh_json:get_value(<<"owner_id">>, AuthDoc)
               ,suppress_unregister_notifications = wh_json:is_true(<<"suppress_unregister_notifications">>, AuthDoc)
              }.

-spec get_account_id/1  :: (wh_json:object()) -> api_binary().
get_account_id(JObj) ->
    case wh_json:get_value(<<"pvt_account_id">>, JObj) of
        'undefined' ->
            case wh_json:get_value(<<"pvt_account_db">>, JObj) of
                'undefined' -> 'undefined';
                AccountDb -> wh_util:format_account_id(AccountDb, raw)
            end;
        AccountId -> AccountId
    end.

-spec get_account_db/1  :: (wh_json:object()) -> api_binary().
get_account_db(JObj) ->
    case wh_json:get_value(<<"pvt_account_db">>, JObj) of
        'undefined' ->
            case wh_json:get_value(<<"pvt_account_id">>, JObj) of
                'undefined' -> 'undefined';
                AccountId -> wh_util:format_account_id(AccountId, 'encoded')
            end;
        AccountDb -> AccountDb
    end.

-spec get_auth_method/1  :: (wh_json:object()) -> ne_binary().
get_auth_method(JObj) ->
    Method = wh_json:get_binary_value(<<"method">>, JObj, <<"password">>),
    wh_util:to_lower_binary(Method).
