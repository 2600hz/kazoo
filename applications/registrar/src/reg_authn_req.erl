%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%% Handle authn_req messages
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(reg_authn_req).

-export([init/0, handle_req/2]).

-include("reg.hrl").

init() -> 'ok'.

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_authn:req_v(JObj),
    _ = wh_util:put_callid(JObj),
    Username = get_auth_user(JObj),
    Realm = wapi_authn:get_auth_realm(JObj),
    lager:debug("trying to authenticate ~s@~s", [Username, Realm]),
    case lookup_auth_user(Username, Realm, JObj) of
        {'ok', #auth_user{}=AuthUser} ->
            send_auth_resp(AuthUser, JObj);
        {'error', _R} ->
            lager:notice("auth failure for ~s@~s: ~p"
                         ,[Username, Realm, _R]),
            send_auth_error(JObj)
    end.

-spec get_auth_user(wh_json:object()) -> ne_binary().
get_auth_user(JObj) ->
    case wapi_authn:get_auth_user(JObj) of
        <<"unknown">> ->
            To = wh_json:get_value(<<"To">>, JObj,<<"nouser@nodomain">>),
            [ToUser, _ToDomain] = binary:split(To, <<"@">>),
            wh_util:to_lower_binary(ToUser);
        Username -> Username    
    end.

-spec send_auth_resp(auth_user(), wh_json:object()) -> 'ok'.
send_auth_resp(#auth_user{password=Password
                          ,username=Username
                          ,method=Method
                          ,realm=Realm
                          ,suppress_unregister_notifications=SupressUnregister
                          ,register_overwrite_notify=RegisterOverwrite
                          ,nonce=Nonce
                         }=AuthUser, JObj) ->
    Category = wh_json:get_value(<<"Event-Category">>, JObj),
    Resp = props:filter_undefined(
             [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
              ,{<<"Auth-Password">>, Password}
              ,{<<"Auth-Method">>, Method}
              ,{<<"Auth-Nonce">>, Nonce}
              ,{<<"Suppress-Unregister-Notifications">>, SupressUnregister}
              ,{<<"Register-Overwrite-Notify">>, RegisterOverwrite}
              ,{<<"Custom-Channel-Vars">>, create_ccvs(AuthUser)}
              ,{<<"Custom-Headers">>, create_headers(Method, AuthUser)}
              | wh_api:default_headers(Category, <<"authn_resp">>, ?APP_NAME, ?APP_VERSION)
             ]),
    lager:info("sending SIP authentication reply, with credentials for user ~s@~s",[Username,Realm]),
    wapi_authn:publish_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp).

-spec send_auth_error(wh_json:object()) -> 'ok'.
send_auth_error(JObj) ->
%% NOTE: Kamailio needs registrar errors since it is blocking with no
%%   timeout (at the moment) but when we seek auth for INVITEs we need
%%   to wait for conferences, ect.  Since Kamailio does not honor
%%   Defer-Response we can use that flag on registrar errors
%%   to queue in Kazoo but still advance Kamailio.
    Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            ,{<<"Defer-Response">>, <<"true">>}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    lager:debug("sending SIP authentication error"),
    wapi_authn:publish_error(wh_json:get_value(<<"Server-ID">>, JObj), Resp).

-spec create_ccvs(auth_user()) -> wh_json:object().
create_ccvs(#auth_user{}=AuthUser) ->
    Props = [{<<"Username">>, AuthUser#auth_user.username}
             ,{<<"Realm">>, AuthUser#auth_user.realm}
             ,{<<"Account-ID">>, AuthUser#auth_user.account_id}
             ,{<<"Authorizing-ID">>, AuthUser#auth_user.authorizing_id}
             ,{<<"Authorizing-Type">>, AuthUser#auth_user.authorizing_type}
             ,{<<"Owner-ID">>, AuthUser#auth_user.owner_id}
             ,{<<"Account-Realm">>, AuthUser#auth_user.account_realm}
             ,{<<"Account-Name">>, AuthUser#auth_user.account_name}
            ],
    wh_json:from_list(props:filter_undefined(Props)).

-spec create_headers(api_binary(), auth_user()) -> api_object().
create_headers(?GSM_AUTH_METHOD, #auth_user{a3a8_kc=KC
                                           ,a3a8_sres=SRES
                                           ,primary_number=Number
                                           ,account_realm=Realm
                                           ,username=Username
                                           }=AuthUser) ->
    Props = props:filter_undefined(
              [{<<"P-GSM-Kc">>, KC}
               ,{<<"P-GSM-SRes">>, SRES}
               ,{<<"P-Asserted-Identity">>, <<"<sip:", Username/binary, "@", Realm/binary, ">">>}
               ,{<<"P-Associated-URI">>, get_tel_uri(Number)}
               ,{<<"P-Associated-URI">>, <<"<sip:", Username/binary, "@", Realm/binary, ">">>}
              ]),
    case length(Props) of
        0 -> 'undefined';
        _ -> wh_json:from_list(props:filter_undefined(Props))
    end;
create_headers(?ANY_AUTH_METHOD, _) -> 'undefined'.


-spec get_asserted_identity(api_binary(), api_binary()) -> api_binary().
get_asserted_identity('undefined', _) -> 'undefined';
get_asserted_identity(Number, Realm) ->
    <<"<sip:", Number/binary, "@", Realm/binary, ">">>.

-spec get_tel_uri(api_binary()) -> api_binary().
get_tel_uri('undefined') -> 'undefined';
get_tel_uri(Number) ->
    <<"<tel:", Number/binary,">">>.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% look up the user and realm in the database and return the result
%% @end
%%-----------------------------------------------------------------------------
-spec lookup_auth_user(ne_binary(), ne_binary(), wh_json:object()) ->
                              {'ok', auth_user()} |
                              {'error', _}.
lookup_auth_user(Username, Realm, Req) ->
    case get_auth_user(Username, Realm) of
        {'error', _}=E -> E;
        {'ok', JObj} -> check_auth_user(JObj, Username, Realm, Req)
    end.

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

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec check_auth_user(wh_json:object(), ne_binary(), ne_binary(), wh_json:object()) -> 
                             {'ok', auth_user()} |
                             {'error', _}.                             
check_auth_user(JObj, Username, Realm, Req) ->
    case is_account_enabled(JObj)
        andalso maybe_auth_type_enabled(JObj)
        andalso maybe_owner_enabled(JObj)
    of
        'true' -> jobj_to_auth_user(JObj, Username, Realm, Req);
        'false' -> {'error', 'disabled'}
    end.

-spec is_account_enabled(wh_json:object()) -> boolean().
is_account_enabled(JObj) ->
    AccountId = get_account_id(JObj),
    case wh_util:is_account_enabled(AccountId) of
        'true' -> 'true';
        'false' ->
            lager:notice("rejecting authn for disabled account ~s", [AccountId]),
            'false'
    end.

-spec maybe_auth_type_enabled(wh_json:object()) -> boolean().
maybe_auth_type_enabled(JObj) ->
    case wh_json:get_value([<<"doc">>, <<"pvt_type">>], JObj) of
        <<"device">> -> is_device_enabled(JObj);
        _Else -> 'true'
    end.

-spec is_device_enabled(wh_json:object()) -> boolean().
is_device_enabled(JObj) ->
    Default = whapps_config:get_is_true(?CONFIG_CAT, <<"device_enabled_default">>, 'true'),
    case wh_json:is_true([<<"doc">>, <<"enabled">>], JObj, Default) of
        'true' -> 'true';
        'false' ->
            lager:notice("rejecting authn for disabled device ~s"
                         ,[wh_json:get_value(<<"id">>, JObj)]),
            'false'
    end.

-spec maybe_owner_enabled(wh_json:object()) -> boolean().
maybe_owner_enabled(JObj) ->
    case wh_json:get_value([<<"doc">>, <<"owner_id">>], JObj) of
        'undefined' -> 'true';
        OwnerId -> is_owner_enabled(get_account_db(JObj), OwnerId)
    end.

-spec is_owner_enabled(ne_binary(), ne_binary()) -> boolean().
is_owner_enabled(AccountDb, OwnerId) ->
    Default = whapps_config:get_is_true(?CONFIG_CAT, <<"owner_enabled_default">>, 'true'),
    case couch_mgr:open_cache_doc(AccountDb, OwnerId) of
        {'ok', JObj} ->
            case wh_json:is_true(<<"enabled">>, JObj, Default) of
                'true' -> 'true';
                'false' ->
                    lager:notice("rejecting authn for disabled owner ~s"
                                 ,[OwnerId]),
                    'false'
            end;
        {'error', _R} ->
            lager:debug("unable to fetch owner doc ~s: ~p", [OwnerId, _R]),
            'true'
    end.

-spec jobj_to_auth_user(wh_json:object(), ne_binary(), ne_binary(), wh_json:object()) -> 
          {'ok', auth_user()} | {'error', any()}.
jobj_to_auth_user(JObj, Username, Realm, Req) ->
    AuthValue = wh_json:get_value(<<"value">>, JObj),
    AuthDoc = wh_json:get_value(<<"doc">>, JObj),
    Method = wh_json:get_lower_binary(<<"method">>, AuthValue, <<"password">>),
    AuthUser = #auth_user{realm = Realm
               ,username = Username
               ,account_id = get_account_id(AuthDoc)
               ,account_db = get_account_db(AuthDoc)
               ,password = wh_json:get_value(<<"password">>, AuthValue)
               ,authorizing_type = wh_json:get_value(<<"authorizing_type">>, AuthValue, <<"anonymous">>)
               ,authorizing_id = wh_json:get_value(<<"id">>, JObj)
               ,method = Method
               ,owner_id = wh_json:get_value(<<"owner_id">>, AuthDoc)
               ,suppress_unregister_notifications = wh_json:is_true(<<"suppress_unregister_notifications">>, AuthDoc)
               ,register_overwrite_notify = wh_json:is_true(<<"register_overwrite_notify">>, AuthDoc)
              },
    maybe_auth_method(add_account_name(AuthUser), JObj, Req, Method).

-spec add_account_name(auth_user()) -> 'ok'.
add_account_name(#auth_user{account_id=AccountId}=AuthUser) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {'error', _} -> AuthUser;
        {'ok', Account} ->
              AuthUser#auth_user{account_name=wh_json:get_value(<<"name">>, Account)
                                 ,account_realm=wh_json:get_lower_binary(<<"realm">>, Account)}
    end.


-spec maybe_auth_method(auth_user(), wh_json:object(), wh_json:object(), ne_binary()) ->
          {'ok', auth_user()} | {'error', any()}.
maybe_auth_method(AuthUser, JObj, Req, ?GSM_AUTH_METHOD)->
    SipDoc = wh_json:get_value([<<"doc">>,<<"sip">>], JObj),
    Nonce = registrar_util:remove_dashes(
              wh_json:get_value(<<"nonce">>, SipDoc, 
                                wh_json:get_value(<<"Auth-Nonce">>, Req,
                                                  wh_util:rand_hex_binary(16)))), 
    GsmKey = wh_json:get_value(<<"gsm_key">>, SipDoc),
    GsmNumber = wh_json:get_first_defined([[<<"sip">>,<<"gsm_primary_number">>]
                                           ,[<<"mobile">>,<<"mdn">>]
                                          ], JObj),
    gsm_auth(AuthUser#auth_user{primary_number=GsmNumber}, Nonce, GsmKey);
maybe_auth_method(AuthUser, _JObj, _Req, ?ANY_AUTH_METHOD)-> {'ok', AuthUser}.

-spec gsm_auth(auth_user(), ne_binary(), api_binary()) -> {'ok', auth_user()}.
gsm_auth(#auth_user{password=Password}=AuthUser, Nonce, 'undefined') ->
    {'ok', AuthUser#auth_user{a3a8_sres=Password
                             ,nonce=Nonce}};
gsm_auth(AuthUser, NonceHex, GsmKey) ->
    Key = registrar_util:hexstr_to_bin(GsmKey),
    Nonce = registrar_util:hexstr_to_bin(NonceHex),
    SRes = registrar_crypto:a3a8(Nonce, Key),
    SResHex = wh_util:to_lower_binary(registrar_util:bin_to_hexstr(SRes)),
    <<SRES:8/binary, KC/binary>> = SResHex,
    {'ok', AuthUser#auth_user{a3a8_sres=SRES
                             ,a3a8_kc=KC
                             ,password=SRES
                             ,nonce=NonceHex}}.


%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec get_account_id(wh_json:object()) -> api_binary().
get_account_id(JObj) ->
    case wh_json:get_first_defined([[<<"doc">>, <<"pvt_account_id">>]
                                    ,<<"pvt_account_id">>
                                    ,[<<"doc">>, <<"pvt_account_db">>]
                                    ,<<"pvt_account_db">>
                                   ], JObj)
    of
        'undefined' -> 'undefined';
        AccountId -> wh_util:format_account_id(AccountId, 'raw')
    end.


%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec get_account_db(wh_json:object()) -> api_binary().
get_account_db(JObj) ->
    case wh_json:get_first_defined([[<<"doc">>, <<"pvt_account_db">>]
                                    ,<<"pvt_account_db">>
                                    ,[<<"doc">>, <<"pvt_account_id">>]
                                    ,<<"pvt_account_id">>
                                   ], JObj)
    of
        'undefined' -> 'undefined';
        AccountDb -> wh_util:format_account_id(AccountDb, 'encoded')
    end.
