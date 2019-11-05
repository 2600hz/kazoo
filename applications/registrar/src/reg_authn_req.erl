%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Handle authn_req messages
%%% @author James Aimonetti
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(reg_authn_req).

-export([init/0
        ,handle_req/2
        ]).

-include("reg.hrl").

-define(ENCRYPTION_MAP, [{<<"srtp">>, [{<<"RTP-Secure-Media">>, 'true'}]}
                        ,{<<"zrtp">>, [{<<"ZRTP-Secure-Media">>, 'true'}
                                      ,{<<"ZRTP-Enrollment">>, 'true'}
                                      ]}
                        ]).

-spec init() -> 'ok'.
init() -> 'ok'.

-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_authn:req_v(JObj),
    _ = kz_log:put_callid(JObj),
    Realm = kz_json:get_value(<<"Auth-Realm">>, JObj, <<"missing.realm">>),
    case kz_network_utils:is_ipv4(Realm)
        orelse kz_network_utils:is_ipv6(Realm)
    of
        'true' ->
            lager:debug("realm is an IP address (~s) : skipping", [Realm]);
        'false' ->
            Username = kapi_authn:get_auth_user(JObj),
            lager:debug("trying to authenticate ~s@~s", [Username, Realm]),
            case lookup_auth_user(Username, Realm, JObj) of
                {'ok', #auth_user{}=AuthUser} ->
                    send_auth_resp(AuthUser, JObj);
                {'error', _R} ->
                    lager:notice("auth failure for ~s@~s: ~p"
                                ,[Username, Realm, _R]
                                ),
                    send_auth_error(JObj)
            end
    end.

-spec send_auth_resp(auth_user(), kz_json:object()) -> 'ok'.
send_auth_resp(#auth_user{password=Password
                         ,username=Username
                         ,method=Method
                         ,realm=Realm
                         ,suppress_unregister_notifications=SupressUnregister
                         ,register_overwrite_notify=RegisterOverwrite
                         ,nonce=Nonce
                         }=AuthUser
              ,JObj
              ) ->
    Category = kz_json:get_value(<<"Event-Category">>, JObj),
    Resp = props:filter_undefined(
             [{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
             ,{<<"Auth-Password">>, Password}
             ,{<<"Auth-Method">>, get_auth_method(Method)}
             ,{<<"Auth-Nonce">>, Nonce}
             ,{<<"Expires">>, kz_json:get_value(<<"Expires">>,JObj)}
             ,{<<"Suppress-Unregister-Notifications">>, SupressUnregister}
             ,{<<"Register-Overwrite-Notify">>, RegisterOverwrite}
             ,{<<"Custom-Channel-Vars">>, create_ccvs(AuthUser)}
             ,{<<"Custom-SIP-Headers">>, create_custom_sip_headers(Method, AuthUser)}
              | kz_api:default_headers(Category, <<"authn_resp">>, ?APP_NAME, ?APP_VERSION)
             ]),
    lager:info("sending SIP authentication reply, with credentials for user ~s@~s",[Username,Realm]),
    kapi_authn:publish_resp(kz_json:get_value(<<"Server-ID">>, JObj), Resp).

-spec send_auth_error(kz_json:object()) -> 'ok'.
send_auth_error(JObj) ->
    %% NOTE: Kamailio needs registrar errors since it is blocking with no
    %%   timeout (at the moment) but when we seek auth for INVITEs we need
    %%   to wait for conferences, etc.  Since Kamailio does not honor
    %%   Defer-Response we can use that flag on registrar errors
    %%   to queue in Kazoo but still advance Kamailio.
    Resp = [{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
           ,{<<"Defer-Response">>, <<"true">>}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    lager:debug("sending SIP authentication error"),
    kapi_authn:publish_error(kz_json:get_value(<<"Server-ID">>, JObj), Resp).

-spec create_ccvs(auth_user()) -> kz_json:object().
create_ccvs(#auth_user{doc=JObj}=AuthUser) ->
    kz_json:from_list(
      [{<<"Username">>, AuthUser#auth_user.username}
      ,{<<"Realm">>, AuthUser#auth_user.realm}
      ,{<<"Account-ID">>, AuthUser#auth_user.account_id}
      ,{<<"Authorizing-ID">>, AuthUser#auth_user.authorizing_id}
      ,{<<"Authorizing-Type">>, AuthUser#auth_user.authorizing_type}
      ,{<<"Owner-ID">>, AuthUser#auth_user.owner_id}
      ,{<<"Account-Realm">>, AuthUser#auth_user.account_normalized_realm}
      ,{<<"Account-Name">>, AuthUser#auth_user.account_name}
      ,{<<"Presence-ID">>, maybe_get_presence_id(AuthUser)}
      ,{<<"Suppress-Unregister-Notifications">>, AuthUser#auth_user.suppress_unregister_notifications}
      ,{<<"Register-Overwrite-Notify">>, AuthUser#auth_user.register_overwrite_notify}
      ,{<<"Pusher-Application">>, kz_json:get_value([<<"push">>, <<"Token-App">>], JObj)}
       | (create_specific_ccvs(AuthUser, AuthUser#auth_user.method)
          ++ generate_security_ccvs(AuthUser)
          ++ maybe_add_hotdesk_current_id(AuthUser))
      ]).

-spec maybe_get_presence_id(auth_user()) -> kz_term:api_binary().
maybe_get_presence_id(#auth_user{account_db=AccountDb
                                ,authorizing_id=DeviceId
                                ,owner_id=OwnerId
                                ,account_realm=AccountRealm
                                }
                     ) ->
    case get_presence_id(AccountDb, DeviceId, OwnerId) of
        'undefined' -> 'undefined';
        PresenceId ->
            case binary:match(PresenceId, <<"@">>) of
                'nomatch' -> <<PresenceId/binary, "@", AccountRealm/binary>>;
                _ -> PresenceId
            end
    end.

-spec get_presence_id(kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary()) -> kz_term:api_binary().
get_presence_id('undefined', _, _) -> 'undefined';
get_presence_id(_, 'undefined', 'undefined') -> 'undefined';
get_presence_id(AccountDb, DeviceId, 'undefined') ->
    get_device_presence_id(AccountDb, DeviceId);
get_presence_id(AccountDb, DeviceId, OwnerId) ->
    maybe_get_owner_presence_id(AccountDb, DeviceId, OwnerId).

-spec maybe_get_owner_presence_id(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_binary().
maybe_get_owner_presence_id(AccountDb, DeviceId, OwnerId) ->
    case kz_datamgr:open_cache_doc(AccountDb, OwnerId) of
        {'error', _} -> 'undefined';
        {'ok', UserJObj} ->
            case kzd_users:presence_id(UserJObj) of
                'undefined' -> get_device_presence_id(AccountDb, DeviceId);
                PresenceId -> PresenceId
            end
    end.

-spec get_device_presence_id(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_binary().
get_device_presence_id(AccountDb, DeviceId) ->
    case kz_datamgr:open_cache_doc(AccountDb, DeviceId) of
        {'error', _} -> 'undefined';
        {'ok', JObj} ->
            case kzd_devices:presence_id(JObj) of
                'undefined' -> 'undefined';
                PresenceId -> PresenceId
            end
    end.

-spec create_specific_ccvs(auth_user(), kz_term:ne_binary()) -> kz_term:proplist().
create_specific_ccvs(#auth_user{msisdn=MSISDN}, ?GSM_ANY_METHOD) ->
    [{<<"Caller-ID">>, MSISDN}
    ,{<<"Caller-ID-Number">>, MSISDN}
    ];
create_specific_ccvs(_, _) -> [].

-spec create_custom_sip_headers(kz_term:api_binary(), auth_user()) -> kz_term:api_object().
create_custom_sip_headers(?GSM_ANY_METHOD
                         ,#auth_user{a3a8_kc=KC
                                    ,a3a8_sres=SRES
                                    ,msisdn=Number
                                    ,account_realm=AccountRealm
                                    ,realm=Realm
                                    ,username=Username
                                    }) ->
    create_custom_sip_headers(
      props:filter_undefined(
        [{<<"P-GSM-Kc">>, KC}
        ,{<<"P-GSM-SRes">>, SRES}
        ,{<<"P-Asserted-Identity">>, <<"<sip:", Username/binary, "@", Realm/binary, ">">>}
        ,{<<"P-Associated-URI">>, get_tel_uri(Number)}
        ,{<<"P-Associated-URI">>, <<"<sip:", Username/binary, "@", AccountRealm/binary, ">">>}
        ,{<<"P-Kazoo-Primary-Number">>, Number}
        ])
     );
create_custom_sip_headers(?ANY_AUTH_METHOD, _) -> 'undefined'.

-spec create_custom_sip_headers(kz_term:proplist()) -> kz_term:api_object().
create_custom_sip_headers([]) -> 'undefined';
create_custom_sip_headers(Props) -> kz_json:from_list(Props).

-spec get_tel_uri(kz_term:api_binary()) -> kz_term:api_binary().
get_tel_uri('undefined') -> 'undefined';
get_tel_uri(Number) -> <<"<tel:", Number/binary,">">>.

%%------------------------------------------------------------------------------
%% @doc look up the user and realm in the database and return the result
%% @end
%%------------------------------------------------------------------------------
-spec lookup_auth_user(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
                              {'ok', auth_user()} |
                              {'error', any()}.
lookup_auth_user(Username, Realm, Req) ->
    case get_auth_user(Username, Realm) of
        {'error', _}=E -> E;
        {'ok', JObj} -> check_auth_user(JObj, Username, Realm, Req)
    end.

-spec get_auth_user(kz_term:ne_binary(), kz_term:ne_binary()) ->
                           {'ok', kz_json:object()} |
                           {'error', 'not_found'}.
get_auth_user(Username, Realm) ->
    case kapps_util:get_account_by_realm(Realm) of
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

-spec get_auth_user_in_agg(kz_term:ne_binary(), kz_term:ne_binary()) ->
                                  {'ok', kz_json:object()} |
                                  {'error', 'not_found'}.
get_auth_user_in_agg(Username, Realm) ->
    ViewOptions = [{'key', [Realm, Username]}
                  ,'include_docs'
                  ],
    case kapps_config:get_is_true(?CONFIG_CAT, <<"use_aggregate">>, 'true')
        andalso kz_datamgr:get_results(?KZ_SIP_DB, <<"credentials/lookup">>, ViewOptions)
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

-spec get_auth_user_in_account(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                                      {'ok', kz_json:object()} |
                                      {'error', 'not_found'}.
get_auth_user_in_account(Username, Realm, AccountDB) ->
    ViewOptions = [{'key', Username}
                  ,'include_docs'
                  ],
    case kz_datamgr:get_results(AccountDB, <<"devices/sip_credentials">>, ViewOptions) of
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec check_auth_user(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
                             {'ok', auth_user()} |
                             {'error', 'disabled'}.
check_auth_user(JObj, Username, Realm, Req) ->
    Things = [{<<"account">>, get_account_id(JObj)}
             ,{kz_json:get_value([<<"doc">>, <<"pvt_type">>], JObj), kz_doc:id(JObj)}
             ,{<<"owner">>, kz_json:get_value([<<"doc">>, <<"owner_id">>], JObj)}
             ],
    case kapps_util:are_all_enabled(Things) of
        'true' -> jobj_to_auth_user(JObj, Username, Realm, Req);
        {'false', Reason} ->
            lager:notice("rejecting authn for ~p", [Reason]),
            {'error', 'disabled'}
    end.

-spec jobj_to_auth_user(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
                               {'ok', auth_user()} |
                               {'error', any()}.
jobj_to_auth_user(JObj, Username, Realm, Req) ->
    AuthValue = get_auth_value(JObj),
    AuthDoc = kz_json:get_value(<<"doc">>, JObj),
    Method = get_auth_method(AuthDoc),
    AuthUser = #auth_user{realm = Realm
                         ,username = Username
                         ,account_id = get_account_id(AuthDoc)
                         ,account_db = get_account_db(AuthDoc)
                         ,password = kz_json:get_value(<<"password">>, AuthValue, kz_binary:rand_hex(6))
                         ,authorizing_type = get_auth_type(AuthDoc)
                         ,authorizing_id = kz_doc:id(JObj)
                         ,method = kz_term:to_lower_binary(Method)
                         ,owner_id = kz_json:get_value(<<"owner_id">>, AuthDoc)
                         ,suppress_unregister_notifications = kz_json:is_true(<<"suppress_unregister_notifications">>, AuthDoc)
                         ,register_overwrite_notify = kz_json:is_true(<<"register_overwrite_notify">>, AuthDoc)
                         ,doc=AuthDoc
                         ,request=Req
                         },
    maybe_auth_method(add_account_name(AuthUser), AuthDoc, Req, Method).

-spec get_auth_type(kz_json:object()) -> kz_term:ne_binary().
get_auth_type(AuthDoc) ->
    case kz_json:get_first_defined([<<"endpoint_type">>, <<"device_type">>], AuthDoc) of
        <<"mobile">> -> <<"mobile">>;
        _ -> kz_doc:type(AuthDoc, <<"anonymous">>)
    end.

-spec get_auth_value(kz_json:object()) -> kz_term:api_object().
get_auth_value(JObj) ->
    kz_json:get_first_defined([[<<"doc">>,<<"sip">>]
                              ,<<"value">>
                              ]
                             ,JObj
                             ).

-spec add_account_name(auth_user()) -> auth_user().
add_account_name(#auth_user{account_id=AccountId}=AuthUser) ->
    case kzd_accounts:fetch(AccountId) of
        {'error', _} -> AuthUser;
        {'ok', Account} ->
            Realm = kzd_accounts:realm(Account),
            AuthUser#auth_user{account_name = kzd_accounts:name(Account)
                              ,account_realm = Realm
                              ,account_normalized_realm = kz_term:to_lower_binary(Realm)
                              }
    end.

-spec get_auth_method(kz_json:object() | kz_term:ne_binary()) -> kz_term:ne_binary().
get_auth_method(?GSM_ANY_METHOD=M) when is_binary(M)-> <<"gsm">>;
get_auth_method(M) when is_binary(M) -> M;
get_auth_method(JObj) ->
    kz_json:get_first_defined([[<<"gsm">>,<<"method">>]
                              ,[<<"sip">>,<<"method">>]
                              ]
                             ,JObj
                             ,<<"password">>
                             ).

-spec maybe_auth_method(auth_user(), kz_json:object(), kz_json:object(), kz_term:ne_binary()) ->
                               {'ok', auth_user()} |
                               {'error', any()}.
maybe_auth_method(AuthUser, JObj, Req, ?GSM_ANY_METHOD)->
    GsmDoc = kz_json:get_value(<<"gsm">>, JObj),
    CachedNonce = kz_json:get_value(<<"nonce">>, GsmDoc, kz_binary:rand_hex(16)),
    Nonce = remove_dashes(
              kz_json:get_first_defined([<<"nonce">>, <<"Auth-Nonce">>], Req, CachedNonce)
             ),
    GsmKey = kz_json:get_value(<<"key">>, GsmDoc),
    GsmSRes = kz_json:get_value(<<"sres">>, GsmDoc, kz_binary:rand_hex(6)),
    GsmNumber = kz_json:get_value(<<"msisdn">>, GsmDoc),
    ReqMethod = kz_json:get_value(<<"Method">>, Req),
    gsm_auth(
      maybe_update_gsm(ReqMethod
                      ,AuthUser#auth_user{msisdn=GsmNumber
                                         ,a3a8_key=GsmKey
                                         ,a3a8_sres=GsmSRes
                                         ,nonce=Nonce
                                         }
                      )
     );
maybe_auth_method(AuthUser, _JObj, _Req, ?ANY_AUTH_METHOD)->
    {'ok', AuthUser}.

-define(GSM_PRE_REGISTER_ROUTINES, [fun maybe_msisdn/1]).
-define(GSM_REGISTER_ROUTINES, [fun maybe_msisdn/1]).

-spec maybe_update_gsm(kz_term:api_binary(), auth_user()) -> auth_user().
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

-spec maybe_msisdn_from_callflows(auth_user(), kz_term:ne_binary(), kz_term:ne_binary()) -> auth_user().
maybe_msisdn_from_callflows(#auth_user{account_db=AccountDB}=AuthUser
                           ,Type
                           ,Id
                           ) ->
    ViewOptions = [{'startkey', [Type, Id]}
                  ,{'endkey', [Type, Id, <<"9999999">>]}
                  ],
    case kz_datamgr:get_results(AccountDB, <<"callflows/msisdn">>, ViewOptions) of
        {'error', _R} ->
            lager:warning("failed to look up msisdn  in ~s: ~p", [AccountDB, _R]),
            AuthUser;
        {'ok', []} ->
            lager:debug("msisdn not found for ~s@~s in ~s", [Type, Id, AccountDB]),
            AuthUser;
        {'ok', [User|_]} ->
            MSISDN = kz_json:get_value([<<"value">>,<<"msisdn">>], User),
            lager:debug("found msisdn ~s for ~s@~s in account db: ~s"
                       ,[MSISDN, Type, Id, AccountDB]
                       ),
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
    Key = kz_binary:from_hex(GsmKey),
    Nonce = kz_binary:from_hex(NonceHex),
    SRes = registrar_crypto:a3a8(Nonce, Key),
    SResHex = kz_term:to_hex_binary(SRes),
    <<SRES:8/binary, KC/binary>> = SResHex,
    {'ok', AuthUser#auth_user{a3a8_sres=SRES
                             ,a3a8_kc=KC
                             ,password=SRES
                             }};
gsm_auth(AuthUser) -> {'ok', AuthUser}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_account_id(kz_json:object()) -> kz_term:api_binary().
get_account_id(JObj) ->
    case get_account_db(JObj) of
        'undefined' -> 'undefined';
        AccountDb -> kz_util:format_account_id(AccountDb, 'raw')
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_account_db(kz_json:object()) -> kz_term:api_binary().
get_account_db(JObj) ->
    case kz_json:get_first_defined([[<<"doc">>, <<"pvt_account_db">>]
                                   ,<<"pvt_account_db">>
                                   ,[<<"doc">>, <<"pvt_account_id">>]
                                   ,<<"pvt_account_id">>
                                   ], JObj)
    of
        'undefined' -> 'undefined';
        AccountDb -> kz_util:format_account_id(AccountDb, 'encoded')
    end.

-spec remove_dashes(kz_term:ne_binary()) -> kz_term:ne_binary().
remove_dashes(Bin) ->
    << <<B>> || <<B>> <= Bin, B =/= $->>.

-spec encryption_method_map(kz_term:proplist(), kz_term:api_binaries() | kz_json:object()) -> kz_term:proplist().
encryption_method_map(Props, []) -> Props;
encryption_method_map(Props, [Method|Methods]) ->
    case props:get_value(Method, ?ENCRYPTION_MAP, []) of
        [] -> encryption_method_map(Props, Methods);
        Values -> encryption_method_map(props:set_values(Values, Props), Methods)
    end;
encryption_method_map(Props, JObj) ->
    Key = [<<"media">>, <<"encryption">>, <<"methods">>],
    Methods = kz_json:get_value(Key, JObj, []),
    encryption_method_map(Props, Methods).


-spec generate_security_ccvs(auth_user()) -> kz_term:proplist().
generate_security_ccvs(#auth_user{}=User) ->
    generate_security_ccvs(User, []).

-spec generate_security_ccvs(auth_user(), kz_term:proplist()) -> kz_term:proplist().
generate_security_ccvs(#auth_user{}=User, Acc0) ->
    CCVFuns = [fun maybe_enforce_security/1
              ,fun maybe_set_encryption_flags/1
              ],
    {_, Acc} = lists:foldl(fun(F, Acc) -> F(Acc) end, {User, Acc0}, CCVFuns),
    Acc.

-spec maybe_enforce_security({auth_user(), kz_term:proplist()}) -> {auth_user(), kz_term:proplist()}.
maybe_enforce_security({#auth_user{doc=JObj}=User, Acc}) ->
    case kz_json:is_true([<<"media">>
                         ,<<"encryption">>
                         ,<<"enforce_security">>
                         ], JObj, 'false')
    of
        'true' -> {User, [{<<"Media-Encryption-Enforce-Security">>, 'true'} | Acc]};
        'false' -> {User, Acc}
    end.

-spec maybe_set_encryption_flags({auth_user(), kz_term:proplist()}) -> {auth_user(), kz_term:proplist()}.
maybe_set_encryption_flags({#auth_user{doc=JObj}=User, Acc}) ->
    {User, encryption_method_map(Acc, JObj)}.

-spec maybe_add_hotdesk_current_id(auth_user()) -> kz_term:proplist().
maybe_add_hotdesk_current_id(#auth_user{doc=JObj}) ->
    case kzd_devices:hotdesk_ids(JObj, []) of
        [] -> [];
        [Id | _] -> [{<<"Hotdesk-Current-ID">>, Id}]
    end.
