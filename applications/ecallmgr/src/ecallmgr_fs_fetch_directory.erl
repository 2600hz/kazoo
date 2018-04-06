%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc Directory lookups from FS
%%%
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_fetch_directory).

-export([directory_lookup/1]).
-export([lookup_user/4]).
-export([init/0]).

-include("ecallmgr.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    kazoo_bindings:bind(<<"fetch.directory.*.*.#">>, ?MODULE, 'directory_lookup'),
    'ok'.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec directory_lookup(map()) -> fs_handlecall_ret().
directory_lookup(#{node := Node, fetch_id := FetchId, payload := JObj}) ->
    kz_util:put_callid(FetchId),
    lager:debug_unsafe("DIRECTORY FETCH ~s", [kz_json:encode(JObj, ['pretty'])]),
    lager:debug("received fetch request (~s) user directory from ~s", [FetchId, Node]),
    case kzd_fetch:fetch_action(JObj, <<"sip_auth">>) of
        <<"reverse-auth-lookup">> -> lookup_user(Node, FetchId, <<"reverse-lookup">>, JObj);
        <<"sip_auth">> -> maybe_sip_auth_response(Node, FetchId, JObj);
        <<"group_call">> -> maybe_group_response(Node, FetchId, JObj);
        <<"user_call">> -> maybe_kamailio_association(Node, FetchId, JObj);
        _Other -> directory_not_found(Node, FetchId)
    end.

maybe_group_response(Node, FetchId, JObj) ->
    case  kzd_fetch:fetch_user(JObj) of
        'undefined' -> group_response(Node, FetchId, JObj);
        _ -> maybe_kamailio_association(Node, FetchId, JObj)
    end.

group_response(Node, Id, JObj) ->
    GroupID = kz_json:get_ne_binary_value(<<"group">>, JObj),
    AccountId = kz_json:get_ne_binary_value(<<"domain">>, JObj),
    Members = [<<"d6c63df15ddbec1b7bc6828570983215">>
              ,<<"6eab0d57342549f9cd8207d49bc6b758">>
              ,<<"b560cfa5724bb5de7c99349809bfbd83">>
              ],
%%    {"data":{"name":"fetch","endpoints":{"d6c63df15ddbec1b7bc6828570983215":{"type":"user"},"6eab0d57342549f9cd8207d49bc6b758":{"type":"user"},"b560cfa5724bb5de7c99349809bfbd83":{"type":"user"}},"ui_metadata":{"version":"4.3.0","ui":"monster-ui","origin":"voip"},"music_on_hold":{},"smartpbx":{"ringback":{"enabled":true}},"id":"a0bdb8b86c593aa86c2e0c96625edb97"},"revision":"2-405c2caff956f66665036eb69e3f1930","timestamp":"2018-03-14T13:49:23","version":"4.2.0","node":"_OcjWEXxdDJYOgtvt5eYKA","request_id":"e80450a9f96746d8874497ef13c0e978","status":"success","auth_token":"eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6IjI3Zjk3YjI3NzhkMWM5YjgxZGRiODFhMTBmMGE0OTUwIn0.eyJpc3MiOiJrYXpvbyIsImlkZW50aXR5X3NpZyI6IldvMFIzRjhnTk1ySXExbURIdG93UnczdmR3cU00LUJzZHJhVmxneXJfYXMiLCJhY2NvdW50X2lkIjoiNWJhMDFhZDdhZDE2MTFkNDM2YjE4NjBkOGM1NTI4OTciLCJvd25lcl9pZCI6IjU2Y2I5ODFhZjc1YmIzZjIyYjc3YjY3MmQ2NzRiNWM5IiwibWV0aG9kIjoiY2JfYXV0aCIsInBob3RvVXJsIjoiaHR0cHM6Ly9saDUuZ29vZ2xldXNlcmNvbnRlbnQuY29tLy15U1hVbUpzTUlvcy9BQUFBQUFBQUFBSS9BQUFBQUFBQUFzYy9waFhuVzV3Wi0tYy9waG90by5qcGc_c3o9NTAiLCJkaXNwbGF5TmFtZSI6Ikx1aXMgQXplZG8iLCJzY29wZSI6Imh0dHBzOi8vd3d3Lmdvb2dsZWFwaXMuY29tL2F1dGgvcGx1cy5tZSBodHRwczovL3d3dy5nb29nbGVhcGlzLmNvbS9hdXRoL3VzZXJpbmZvLnByb2ZpbGUgaHR0cHM6Ly93d3cuZ29vZ2xlYXBpcy5jb20vYXV0aC91c2VyaW5mby5lbWFpbCIsImVtYWlsIjoibHVpcy5hemVkb0BmYWN0b3JsdXNpdGFuby5jb20iLCJhdXRoX3Byb3ZpZGVyIjoiZ29vZ2xlIiwiYXV0aF9hcHBfaWQiOiI5OTg2NTI0MzUwMjktMWNrZ29jbHBwamd2bmhlMWtkczU0b2JkYjk5NThxbW0uYXBwcy5nb29nbGV1c2VyY29udGVudC5jb20iLCJhdXRoX2lkIjoiOGJjMWZmMmQyNjQ3MmI1OTA4MDI1YzE5NjVkODNmOWQtMTA3NTI5MTI1ODQ3NTg0NDI2NzgyIiwib3duZXJfaWQiOiI1NmNiOTgxYWY3NWJiM2YyMmI3N2I2NzJkNjc0YjVjOSIsImFjY291bnRfaWQiOiI1YmEwMWFkN2FkMTYxMWQ0MzZiMTg2MGQ4YzU1Mjg5NyIsImF1dGhfYXBwX3Rva2VuX3R5cGUiOiJCZWFyZXIiLCJhdXRoX2FwcF90b2tlbiI6InlhMjkuR21KLUJSVWNpeEZ3VnlPMGxYNy1vdWthaVVwc0QtVGl1RGJUUjU0a3luNUxheGpNTVZXSkVxelRGSjZEWC1FMTh3aVlJdldackNSWEJEWmlzejExdE5DS2czMlZxZkdIRmZfc29qaUxDc0tILXZxclI5SWJVdXZEU1RfdnFodVQtR3p4ZWcifQ.OULuDiQ5gSUJPLOG523zk8nj-pVurdHRx-fn2fSgyJpH1YIeUB-fkQlq7aSLm4VuckclqhPHAIILXmJhN345bj-SgoeRVBZVbKmG3cvlCTvB2SX13rPXnrpPltUb930bnftsvv21ccQzRy8F5C0sAi2-_EokQQxGXV0zI3XGxrhs92n0vs6_yKLmDngAze3u2WpoxUZY575p4e82umyXebF-CTC1ZM6CSQOweADrYXPH-DFBvcPSDwmg1uC8cACJzMBVRinbSLOKM2nM5IWs7Co2_vCHjlDLxwO335JPQ482cPhHC_QLggYqUmQs7ErZwmzvhITnfETjbJtqVaAyEg"}
    Props1 = [{<<"Requested-Domain-Name">>, AccountId}
             ,{<<"Requested-Group-ID">>, GroupID}
             ],
    J1 = kz_json:set_values(Props1, JObj),
    Props = [{<<"Domain-Name">>, AccountId}
            ,{<<"Group-ID">>, GroupID}
            ,{<<"Members">>, Members}
            ],
    Group = kz_json:from_list(Props),
    {'ok', Xml} = ecallmgr_fs_xml:directory_resp_group_xml(Group, J1),
    lager:debug_unsafe("sending directory XML to ~w: ~s", [Node, iolist_to_binary(Xml)]),
    freeswitch:fetch_reply(Node, Id, 'directory', iolist_to_binary(Xml)).

-spec maybe_sip_auth_response(atom(), kz_term:ne_binary(), kz_json:object()) -> fs_handlecall_ret().
maybe_sip_auth_response(Node, Id, JObj) ->
    case kzd_fetch:auth_response(JObj) of
        'undefined' -> maybe_kamailio_association(Node, Id, JObj);
        _ ->
            lager:debug("attempting to get device password to verify SIP auth response"),
            lookup_user(Node, Id, <<"password">>, JObj)
    end.

-spec maybe_kamailio_association(atom(), kz_term:ne_binary(), kz_json:object()) -> fs_handlecall_ret().
maybe_kamailio_association(Node, Id, JObj) ->
    kamailio_association(Node, Id, kzd_fetch:fetch_user(JObj), kzd_fetch:fetch_key_value(JObj), JObj).

-spec kamailio_association(atom(), kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:api_ne_binary(), kz_json:object()) -> fs_handlecall_ret().
kamailio_association(Node, Id, 'undefined', _AccountId, _) -> directory_not_found(Node, Id);
kamailio_association(Node, Id, _EndpointId, 'undefined', _) -> directory_not_found(Node, Id);
kamailio_association(Node, Id, <<(EndpointId):32/binary>>, ?MATCH_ACCOUNT_RAW(AccountId), JObj) ->
    case kz_endpoint:profile(EndpointId, AccountId) of
        {ok, Endpoint} ->
            lager:debug("building directory resp for ~s@~s from endpoint", [EndpointId, AccountId]),
            {'ok', Xml} = ecallmgr_fs_xml:directory_resp_endpoint_xml(Endpoint, JObj),
            lager:debug_unsafe("SENDING ENDPOINT ~s", [kz_json:encode(Endpoint, ['pretty'])]),
            lager:debug_unsafe("SENDING JOBJ ~s", [kz_json:encode(JObj, ['pretty'])]),
            lager:debug_unsafe("sending directory XML to ~w: ~s", [Node, iolist_to_binary(Xml)]),
            freeswitch:fetch_reply(Node, Id, 'directory', iolist_to_binary(Xml));
        {error, _Err} ->
            lager:debug("error getting profile for for ~s@~s from endpoint : ~p", [EndpointId, AccountId, _Err]),
            directory_not_found(Node, Id)
    end;
kamailio_association(Node, Id, UserId, ?MATCH_ACCOUNT_RAW(AccountId), JObj) ->
    case kz_json:get_ne_binary_value(<<"X-ecallmgr_Authorizing-ID">>, JObj) of
        'undefined' -> directory_not_found(Node, Id);
        EndpointId ->
            lager:debug("got the endpoint_id ~s for user ~s", [EndpointId, UserId]),
            JObjRetry = kz_json:set_value(<<"Requested-User-ID">>, UserId, JObj),
            kamailio_association(Node, Id, EndpointId, AccountId, JObjRetry)
    end;
kamailio_association(Node, Id, EndpointId, Realm, JObj) ->
    maybe_x_auth_token(Node, Id, EndpointId, Realm, JObj).
%%     case kz_json:get_ne_binary_value(<<"X-ecallmgr_Account-ID">>, JObj) of
%%         'undefined' ->
%%             case kapps_util:get_account_by_realm(Realm) of
%%                 {'ok', Account} ->
%%                     lager:debug("got the account_id ~s from realm ~s", [kz_util:format_account_id(Account), Realm]),
%%                     JObjRetry = kz_json:set_value(<<"Requested-Domain-Name">>, Realm, JObj),
%%                     AccountId = kz_util:format_account_id(Account),
%%                     kamailio_association(Node, Id, EndpointId, AccountId, JObjRetry);
%%                 _ -> directory_not_found(Node, Id)
%%             end;
%%         AccountId ->
%%             lager:debug("got the account_id ~s from x-header", [AccountId]),
%%             JObjRetry = kz_json:set_value(<<"Requested-Domain-Name">>, Realm, JObj),
%%             kamailio_association(Node, Id, EndpointId, AccountId, JObjRetry)
%%     end.

maybe_x_auth_token(Node, Id, UserId, Realm, JObj) ->
    case kz_json:get_ne_binary_value(<<"X-AUTH-Token">>, JObj) of
        'undefined' -> maybe_x_account_id(Node, Id, UserId, Realm, JObj);
        AuthToken ->
            JObjRetry = kz_json:set_values([{<<"Requested-Domain-Name">>, Realm}
                                           ,{<<"Requested-User-ID">>, UserId}
                                           ], JObj),
            [EndpointId, AccountId] = binary:split(AuthToken, <<"@">>),
            lager:debug("got the endpoint_id/account_id ~s/~s from x-auth-token", [EndpointId, AccountId]),
            kamailio_association(Node, Id, EndpointId, AccountId, JObjRetry)
    end.

maybe_x_account_id(Node, Id, EndpointId, Realm, JObj) ->
    case kz_json:get_ne_binary_value(<<"X-ecallmgr_Account-ID">>, JObj) of
        'undefined' ->
            case kapps_util:get_account_by_realm(Realm) of
                {'ok', Account} ->
                    lager:debug("got the account_id ~s from realm ~s", [kz_util:format_account_id(Account), Realm]),
                    JObjRetry = kz_json:set_value(<<"Requested-Domain-Name">>, Realm, JObj),
                    AccountId = kz_util:format_account_id(Account),
                    kamailio_association(Node, Id, EndpointId, AccountId, JObjRetry);
                _ -> directory_not_found(Node, Id)
            end;
        AccountId ->
            lager:debug("got the account_id ~s from x-header", [AccountId]),
            JObjRetry = kz_json:set_value(<<"Requested-Domain-Name">>, Realm, JObj),
            kamailio_association(Node, Id, EndpointId, AccountId, JObjRetry)
    end.

-spec directory_not_found(atom(), kz_term:ne_binary()) -> fs_handlecall_ret().
directory_not_found(Node, FetchId) ->
    {'ok', Xml} = ecallmgr_fs_xml:not_found(),
    lager:debug("sending directory not found XML to ~w", [Node]),
    freeswitch:fetch_reply(Node, FetchId, 'directory', iolist_to_binary(Xml)).

-spec lookup_user(atom(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> fs_handlecall_ret().
lookup_user(Node, Id, Method,  JObj) ->
    Domain = kz_json:get_value(<<"domain">>, JObj),
    {'ok', Xml} =
        case get_auth_realm(JObj) of
            AuthRealm=?NE_BINARY ->
                [Realm|_] = binary:split(AuthRealm, [<<":">>, <<";">>]),
                Username = kz_json:get_first_defined([<<"user">>, <<"Auth-User">>], JObj),
                ReqResp = maybe_query_registrar(Realm, Username, Node, Id, Method, JObj),
                handle_lookup_resp(Method, Domain, Username, ReqResp);
            _R ->
                lager:error_unsafe("bad auth realm: ~p : ~s", [_R, kz_json:encode(JObj, ['pretty'])]),
                ecallmgr_fs_xml:not_found()
        end,
    lager:debug("sending authn XML to ~w: ~s", [Node, Xml]),
    freeswitch:fetch_reply(Node, Id, 'directory', iolist_to_binary(Xml)).

-spec get_auth_realm(kz_json:object()) -> kz_term:ne_binary().
get_auth_realm(JObj) ->
    case kz_json:get_first_defined([<<"sip_auth_realm">>
                                   ,<<"domain">>
                                   ], JObj)
    of
        'undefined' -> get_auth_uri_realm(JObj);
        Realm ->
            case kz_network_utils:is_ipv4(Realm)
                orelse kz_network_utils:is_ipv6(Realm)
            of
                'true' -> get_auth_uri_realm(JObj);
                'false' -> kz_term:to_lower_binary(Realm)
            end
    end.

-spec get_auth_uri_realm(kz_json:object()) -> kz_term:ne_binary().
get_auth_uri_realm(JObj) ->
    AuthURI = kz_json:get_value(<<"sip_auth_uri">>, JObj, <<>>),
    case binary:split(AuthURI, <<"@">>) of
        [_, Realm] -> kz_term:to_lower_binary(Realm);
        _Else ->
            kz_json:get_first_defined([<<"Auth-Realm">>
                                      ,<<"sip_request_host">>
                                      ,<<"sip_to_host">>
                                      ], JObj)
    end.

-spec handle_lookup_resp(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()
                        ,{'ok', kz_json:object()} | {'error', _}) ->
                                {'ok', _}.
handle_lookup_resp(<<"reverse-lookup">>, Realm, Username, {'ok', JObj}) ->
    Props = [{<<"Domain-Name">>, Realm}
            ,{<<"User-ID">>, Username}
            ],
    lager:debug("building reverse authn resp for ~s@~s", [Username, Realm]),
    ecallmgr_fs_xml:reverse_authn_resp_xml(kz_json:set_values(Props, JObj));
handle_lookup_resp(_, Realm, Username, {'ok', JObj}) ->
    Props = [{<<"Domain-Name">>, Realm}
            ,{<<"User-ID">>, Username}
            ,{<<"Expires">>, 0}
            ],
    lager:debug("building authn resp for ~s@~s", [Username, Realm]),
    ecallmgr_fs_xml:authn_resp_xml(kz_json:set_values(Props, JObj));
handle_lookup_resp(_, _, _, {'error', _R}) ->
    lager:debug("authn request lookup failed: ~p", [_R]),
    ecallmgr_fs_xml:not_found().

-spec maybe_query_registrar(kz_term:ne_binary(), kz_term:ne_binary(), atom(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
                                   {'ok', kz_json:object()} |
                                   {'error', any()}.
maybe_query_registrar(Realm, Username, Node, Id, Method, JObj) ->
    case kz_cache:peek_local(?ECALLMGR_AUTH_CACHE, ?CREDS_KEY(Realm, Username)) of
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} -> query_registrar(Realm, Username, Node, Id, Method, JObj)
    end.

-spec query_registrar(kz_term:ne_binary(), kz_term:ne_binary(), atom(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
                             {'ok', kz_json:object()} |
                             {'error', any()}.
query_registrar(Realm, Username, Node, Id, Method, JObj) ->
    lager:debug("looking up credentials of ~s@~s for a ~s", [Username, Realm, Method]),
    Req = [{<<"Msg-ID">>, Id}
          ,{<<"To">>, kzd_fetch:auth_to(JObj)}
          ,{<<"From">>, kzd_fetch:auth_from(JObj)}
          ,{<<"Orig-IP">>, kzd_fetch:from_network_ip(JObj)}
          ,{<<"Orig-Port">>, kzd_fetch:from_network_port(JObj)}
          ,{<<"Method">>, Method}
          ,{<<"Auth-User">>, Username}
          ,{<<"Auth-Realm">>, Realm}
          ,{<<"Expires">>, kzd_fetch:auth_expires(JObj)}
          ,{<<"Auth-Nonce">>, kzd_fetch:auth_nonce(JObj)}
          ,{<<"Auth-Response">>, kzd_fetch:auth_response(JObj)}
          ,{<<"Custom-SIP-Headers">>, kzd_fetch:ccvs(JObj)}
          ,{<<"User-Agent">>, kzd_fetch:user_agent(JObj)}
          ,{<<"Media-Server">>, kz_term:to_binary(Node)}
          ,{<<"Call-ID">>, kzd_fetch:call_id(JObj)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    ReqResp = kz_amqp_worker:call(props:filter_undefined(Req)
                                 ,fun kapi_authn:publish_req/1
                                 ,fun kapi_authn:resp_v/1
                                 ),
    case ReqResp of
        {'error', _}=E -> E;
        {'ok', JObj} -> maybe_defered_error(Realm, Username, JObj)
    end.

%% NOTE: Kamailio needs registrar errors since it is blocking with no
%%   timeout (at the moment) but when we seek auth for INVITEs we need
%%   to wait for conferences, etc.  Since Kamailio does not honor
%%   Defer-Response we can use that flag on registrar errors
%%   to queue in Kazoo but still advance Kamailio, just need to check here.
-spec maybe_defered_error(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> {'ok', kz_json:object()} | {'error', 'timeout'}.
maybe_defered_error(Realm, Username, JObj) ->
    case kapi_authn:resp_v(JObj) of
        'false' -> {'error', 'timeout'};
        'true' ->
            AccountId = kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj),
            AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
            AuthorizingId = kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Authorizing-ID">>], JObj),
            OwnerIdProp = case kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Owner-ID">>], JObj) of
                              'undefined' -> [];
                              OwnerId -> [{'db', AccountDb, OwnerId}]
                          end,
            CacheProps = [{'origin', [{'db', AccountDb, AuthorizingId}
                                     ,{'db', AccountDb, AccountId}
                                      | OwnerIdProp
                                     ]}
                         ],
            kz_cache:store_local(?ECALLMGR_AUTH_CACHE, ?CREDS_KEY(Realm, Username), JObj, CacheProps),
            {'ok', JObj}
    end.
