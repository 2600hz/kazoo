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
              ,{<<"Auth-Method">>, get_auth_method(Method)}
              ,{<<"Auth-Nonce">>, Nonce}
              ,{<<"Expires">>, wh_json:get_value(<<"Expires">>,JObj)}
              ,{<<"Suppress-Unregister-Notifications">>, SupressUnregister}
              ,{<<"Register-Overwrite-Notify">>, RegisterOverwrite}
              ,{<<"Custom-Channel-Vars">>, create_ccvs(AuthUser)}
              ,{<<"Custom-SIP-Headers">>, create_custom_sip_headers(Method, AuthUser)}
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
             ,{<<"Presence-ID">>, maybe_get_presence_id(AuthUser)}
            | create_specific_ccvs(AuthUser, AuthUser#auth_user.method)
            ],
    wh_json:from_list(props:filter_undefined(Props)).

-spec maybe_get_presence_id(auth_user()) -> api_binary().
maybe_get_presence_id(#auth_user{account_db=AccountDb
                                 ,authorizing_id=DeviceId
                                 ,owner_id=OwnerId
                                 ,username=Username
                                 ,account_realm=Realm}) ->
    case get_presence_id(AccountDb, DeviceId, OwnerId) of
        'undefined' -> <<Username/binary, "@", Realm/binary>>;
        PresenceId ->
            case binary:match(PresenceId, <<"@">>) of
                'nomatch' -> <<PresenceId/binary, "@", Realm/binary>>;
                _ -> PresenceId
        end
    end.

-spec get_presence_id(api_binary(), api_binary(), api_binary()) -> api_binary().
get_presence_id('undefined', _, _) -> 'undefined';
get_presence_id(_, 'undefined', 'undefined') -> 'undefined';
get_presence_id(AccountDb, DeviceId, 'undefined') ->
    get_device_presence_id(AccountDb, DeviceId);
get_presence_id(AccountDb, DeviceId, OwnerId) ->
    maybe_get_owner_presence_id(AccountDb, DeviceId, OwnerId).

-spec maybe_get_owner_presence_id(ne_binary(), ne_binary(), ne_binary()) -> api_binary().
maybe_get_owner_presence_id(AccountDb, DeviceId, OwnerId) ->
    case couch_mgr:open_cache_doc(AccountDb, OwnerId) of
        {'error', _} -> 'undefined';
        {'ok', JObj} ->
            case wh_json:get_ne_value(<<"presence_id">>, JObj) of
                'undefined' -> get_device_presence_id(AccountDb, DeviceId);
                PresenceId -> PresenceId
            end
    end.

-spec get_device_presence_id(ne_binary(), ne_binary()) -> api_binary().
get_device_presence_id(AccountDb, DeviceId) ->
    case couch_mgr:open_cache_doc(AccountDb, DeviceId) of
        {'error', _} -> 'undefined';
        {'ok', JObj} -> wh_json:get_ne_value(<<"presence_id">>, JObj)
    end.

-spec create_specific_ccvs(auth_user(), ne_binary()) -> wh_proplist().
create_specific_ccvs(#auth_user{}=AuthUser, ?GSM_ANY_METHOD) ->
    [{<<"Caller-ID">>, AuthUser#auth_user.msisdn}
     ,{<<"variable_effective_caller_id_number">>, AuthUser#auth_user.msisdn}
    ];
create_specific_ccvs(_, _) -> [].

-spec create_custom_sip_headers(api_binary(), auth_user()) -> api_object().
create_custom_sip_headers(?GSM_ANY_METHOD, #auth_user{a3a8_kc=KC
                                           ,a3a8_sres=SRES
                                           ,msisdn=Number
                                           ,account_realm=AccountRealm
                                           ,realm=Realm
                                           ,username=Username
                                           }) ->
    Props = props:filter_undefined(
              [{<<"P-GSM-Kc">>, KC}
               ,{<<"P-GSM-SRes">>, SRES}
               ,{<<"P-Asserted-Identity">>, <<"<sip:", Username/binary, "@", Realm/binary, ">">>}
               ,{<<"P-Associated-URI">>, get_tel_uri(Number)}
               ,{<<"P-Associated-URI">>, <<"<sip:", Username/binary, "@", AccountRealm/binary, ">">>}
               ,{<<"P-Kazoo-Primary-Number">>, Number}
              ]),
    case Props of
        [] -> 'undefined';
        _ -> wh_json:from_list(Props)
    end;
create_custom_sip_headers(?ANY_AUTH_METHOD, _) -> 'undefined'.


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
    case whapps_config:get_is_true(?CONFIG_CAT, <<"use_aggregate">>, 'true')
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
    AuthValue = get_auth_value(JObj),
    AuthDoc = wh_json:get_value(<<"doc">>, JObj),
    Method = get_auth_method(AuthDoc),
    AuthUser = #auth_user{realm = Realm
               ,username = Username
               ,account_id = get_account_id(AuthDoc)
               ,account_db = get_account_db(AuthDoc)
               ,password = wh_json:get_value(<<"password">>, AuthValue, wh_util:rand_hex_binary(6))
               ,authorizing_type = wh_json:get_value(<<"pvt_type">>, AuthDoc, <<"anonymous">>)
               ,authorizing_id = wh_json:get_value(<<"id">>, JObj)
               ,method = wh_util:to_lower_binary(Method)
               ,owner_id = wh_json:get_value(<<"owner_id">>, AuthDoc)
               ,suppress_unregister_notifications = wh_json:is_true(<<"suppress_unregister_notifications">>, AuthDoc)
               ,register_overwrite_notify = wh_json:is_true(<<"register_overwrite_notify">>, AuthDoc)
               ,doc=AuthDoc
               ,request=Req
              },
    maybe_auth_method(add_account_name(AuthUser), AuthDoc, Req, Method).

-spec get_auth_value(wh_json:object()) -> 'undefined' | wh_json:object().
get_auth_value(JObj) ->
    wh_json:get_first_defined([[<<"doc">>,<<"sip">>]
                              ,<<"value">>
                              ], JObj).

-spec add_account_name(auth_user()) -> 'ok'.
add_account_name(#auth_user{account_id=AccountId}=AuthUser) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {'error', _} -> AuthUser;
        {'ok', Account} ->
              AuthUser#auth_user{account_name=wh_json:get_value(<<"name">>, Account)
                                 ,account_realm=wh_json:get_lower_binary(<<"realm">>, Account)}
    end.

-spec get_auth_method(wh_json:object() | ne_binary()) -> ne_binary().
get_auth_method(?GSM_ANY_METHOD=M) when is_binary(M)-> <<"gsm">>;
get_auth_method(M) when is_binary(M) -> M;
get_auth_method(JObj) ->
    wh_json:get_first_defined([ [<<"gsm">>,<<"method">>]
                                ,[<<"sip">>,<<"method">>]
                              ], JObj, <<"password">>).


-spec maybe_auth_method(auth_user(), wh_json:object(), wh_json:object(), ne_binary()) ->
                               {'ok', auth_user()} | {'error', any()}.
maybe_auth_method(AuthUser, JObj, Req, ?GSM_ANY_METHOD)->
    GsmDoc = wh_json:get_value(<<"gsm">>, JObj),
    Nonce = remove_dashes(
              wh_json:get_value(<<"nonce">>, GsmDoc,
                                wh_json:get_value(<<"Auth-Nonce">>, Req,
                                                  wh_util:rand_hex_binary(16)))),
    GsmKey = wh_json:get_value(<<"key">>, GsmDoc),
    GsmSRes = wh_json:get_value(<<"sres">>, GsmDoc, wh_util:rand_hex_binary(6)),
    GsmNumber = wh_json:get_value(<<"msisdn">>, GsmDoc),
    ReqMethod = wh_json:get_value(<<"Method">>, Req),
    gsm_auth(
      maybe_update_gsm(
        ReqMethod,
        AuthUser#auth_user{msisdn=GsmNumber
                           ,a3a8_key=GsmKey
                           ,a3a8_sres=GsmSRes
                           ,nonce=Nonce}
       )
     );
maybe_auth_method(AuthUser, _JObj, _Req, ?ANY_AUTH_METHOD)-> {'ok', AuthUser}.

-define(GSM_PRE_REGISTER_ROUTINES, [fun maybe_msisdn/1]).
-define(GSM_REGISTER_ROUTINES, [fun maybe_msisdn/1]).

-spec maybe_update_gsm(api_binary(), auth_user()) -> auth_user().
maybe_update_gsm(<<"PRE-REGISTER">>, AuthUser) ->
    lists:foldl(fun(F,A) -> F(A) end, AuthUser, ?GSM_PRE_REGISTER_ROUTINES);
maybe_update_gsm(<<"REGISTER">>, AuthUser) ->
    lists:foldl(fun(F,A) -> F(A) end, AuthUser, ?GSM_REGISTER_ROUTINES);
maybe_update_gsm(_, AuthUser) -> AuthUser.

-spec maybe_msisdn(auth_user()) -> auth_user().
maybe_msisdn(#auth_user{msisdn='undefined'
                        ,owner_id='undefined'
                        ,authorizing_id=Id
                       }=AuthUser) ->
    maybe_msisdn_from_callflows(AuthUser, <<"device">>, Id);
maybe_msisdn(#auth_user{msisdn='undefined'
                        ,owner_id=OwnerId
                       }=AuthUser) ->
    maybe_msisdn_from_callflows(AuthUser, <<"user">>, OwnerId);
maybe_msisdn(AuthUser) -> AuthUser.

-spec maybe_msisdn_from_callflows(auth_user(), ne_binary(), ne_binary()) -> auth_user().
maybe_msisdn_from_callflows(#auth_user{account_db=AccountDB}=AuthUser
                            ,Type, Id) ->
    ViewOptions = [{'startkey', [Type, Id]}
                   ,{'endkey', [Type, Id, <<"9999999">>]}
                  ],
    case couch_mgr:get_results(AccountDB, <<"callflow/msisdn">>, ViewOptions) of
        {'error', _R} ->
            lager:warning("failed to look up msisdn  in ~s: ~p", [AccountDB, _R]),
            AuthUser;
        {'ok', []} ->
            lager:debug("msisdn not found for ~s@~s in ~s", [Type, Id, AccountDB]),
            AuthUser;
        {'ok', [User|_]} ->
            MSISDN = wh_json:get_value([<<"value">>,<<"msisdn">>], User),
            lager:debug("found msisdn ~s for ~s@~s in account db: ~s"
                        ,[MSISDN, Type, Id, AccountDB]),
            AuthUser#auth_user{msisdn=MSISDN}
    end.

-spec gsm_auth(auth_user()) -> {'ok', auth_user()}.
gsm_auth(#auth_user{method=?GSM_CACHED_METHOD
                    ,a3a8_sres=SRES
                   }=AuthUser) ->
    {'ok', AuthUser#auth_user{password=SRES}};
gsm_auth(#auth_user{method=?GSM_A3A8_METHOD
                    ,a3a8_key=GsmKey
                    ,nonce=NonceHex
                   }=AuthUser) ->
    Key = wh_util:from_hex_binary(GsmKey),
    Nonce = wh_util:from_hex_binary(NonceHex),
    SRes = registrar_crypto:a3a8(Nonce, Key),
    SResHex = wh_util:to_hex_binary(SRes),
    <<SRES:8/binary, KC/binary>> = SResHex,
    {'ok', AuthUser#auth_user{a3a8_sres=SRES
                             ,a3a8_kc=KC
                             ,password=SRES
                             }};
gsm_auth(AuthUser) -> {'ok', AuthUser}.

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

-spec remove_dashes(ne_binary()) -> ne_binary().
remove_dashes(Bin) ->
  << <<B>> || <<B>> <= Bin, B =/= $->>.
