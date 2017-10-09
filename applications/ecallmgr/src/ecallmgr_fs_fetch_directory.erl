%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
%%% @doc
%%% Directory lookups from FS
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_fetch_directory).

-export([directory_lookup/1
        ,directory_not_found/1
        ]).
-export([lookup_user/4]).
-export([init/0]).

-include("ecallmgr.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> 'ok'.
init() ->
    kazoo_bindings:bind(<<"fetch.directory.domain.name.*">>, ?MODULE, 'directory_lookup'),
    'ok'.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec directory_lookup(map()) -> fs_handlecall_ret().
directory_lookup(#{node := Node, fetch_id := FetchId, jobj := JObj}) ->
    kz_util:put_callid(FetchId),
    lager:debug("received fetch request (~s) user directory from ~s", [FetchId, Node]),
    case kzd_fetch:fetch_action(JObj, <<"sip_auth">>) of
        <<"reverse-auth-lookup">> -> lookup_user(Node, FetchId, <<"reverse-lookup">>, JObj);
        <<"sip_auth">> -> maybe_sip_auth_response(Node, FetchId, JObj);
        _Other -> directory_not_found({Node, FetchId, JObj})
    end.

-spec maybe_sip_auth_response(atom(), ne_binary(), kz_json:object()) -> fs_handlecall_ret().
maybe_sip_auth_response(Node, Id, JObj) ->
    case kz_json:get_value(<<"sip_auth_response">>, JObj) of
        'undefined' -> maybe_kamailio_association(Node, Id, JObj);
        _ ->
            lager:debug("attempting to get device password to verify SIP auth response"),
            lookup_user(Node, Id, <<"password">>, JObj)
    end.

-spec maybe_kamailio_association(atom(), ne_binary(), kz_json:object()) -> fs_handlecall_ret().
maybe_kamailio_association(Node, Id, JObj) ->
    kamailio_association(Node, Id, JObj, kzd_fetch:fetch_user(JObj), kzd_fetch:fetch_key_value(JObj)).

-spec kamailio_association(atom(), ne_binary(), kz_json:object(), ne_binary(), ne_binary()) -> fs_handlecall_ret().
kamailio_association(Node, Id, JObj, EndpointId, AccountId) ->
    case kz_endpoint:profile(EndpointId, AccountId) of
        {ok, Endpoint} ->
            lager:debug("building authn resp for ~s@~s from endpoint", [EndpointId, AccountId]),
            {'ok', Xml} = ecallmgr_fs_xml:directory_resp_endpoint_xml(Endpoint),
            lager:debug("sending authn XML to ~w: ~s", [Node, Xml]),
            freeswitch:fetch_reply(Node, Id, 'directory', iolist_to_binary(Xml));
        {error, _Err} ->
            lager:debug("error getting profile for for ~s@~s from endpoint : ~p", [EndpointId, AccountId, _Err]),
            directory_not_found({Node, Id, JObj})
    end.

-spec directory_not_found({atom(), ne_binary(), kz_json:object()}) -> fs_handlecall_ret().
directory_not_found(#{node := Node, fetch_id := FetchId, jobj := _JObj}) ->
    {'ok', Xml} = ecallmgr_fs_xml:not_found(),
    lager:debug("sending authn not found XML to ~w", [Node]),
    freeswitch:fetch_reply(Node, FetchId, 'directory', iolist_to_binary(Xml)).

-spec lookup_user(atom(), ne_binary(), ne_binary(), kz_proplist()) -> fs_handlecall_ret().
lookup_user(Node, Id, Method,  Props) ->
    Domain = props:get_value(<<"domain">>, Props),
    {'ok', Xml} =
        case get_auth_realm(Props) of
            AuthRealm=?NE_BINARY ->
                [Realm|_] = binary:split(AuthRealm, [<<":">>, <<";">>]),
                Username = props:get_value(<<"user">>, Props, props:get_value(<<"Auth-User">>, Props)),
                ReqResp = maybe_query_registrar(Realm, Username, Node, Id, Method, Props),
                handle_lookup_resp(Method, Domain, Username, ReqResp);
            _R ->
                lager:error("bad auth realm: ~p", [_R]),
                props:to_log(Props, <<"lookup_user">>),
                ecallmgr_fs_xml:not_found()
        end,
    lager:debug("sending authn XML to ~w: ~s", [Node, Xml]),
    freeswitch:fetch_reply(Node, Id, 'directory', iolist_to_binary(Xml)).

-spec get_auth_realm(kz_proplist()) -> ne_binary().
get_auth_realm(Props) ->
    case props:get_first_defined([<<"sip_auth_realm">>
                                 ,<<"domain">>
                                 ], Props)
    of
        'undefined' -> get_auth_uri_realm(Props);
        Realm ->
            case kz_network_utils:is_ipv4(Realm)
                orelse kz_network_utils:is_ipv6(Realm)
            of
                'true' -> get_auth_uri_realm(Props);
                'false' -> kz_term:to_lower_binary(Realm)
            end
    end.

-spec get_auth_uri_realm(kz_proplist()) -> ne_binary().
get_auth_uri_realm(Props) ->
    AuthURI = props:get_value(<<"sip_auth_uri">>, Props, <<>>),
    case binary:split(AuthURI, <<"@">>) of
        [_, Realm] -> kz_term:to_lower_binary(Realm);
        _Else ->
            props:get_first_defined([<<"Auth-Realm">>
                                    ,<<"sip_request_host">>
                                    ,<<"sip_to_host">>
                                    ], Props)
    end.

-spec handle_lookup_resp(ne_binary(), ne_binary(), ne_binary()
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

-spec maybe_query_registrar(ne_binary(), ne_binary(), atom(), ne_binary(), ne_binary(), kz_proplist()) ->
                                   {'ok', kz_json:object()} |
                                   {'error', any()}.
maybe_query_registrar(Realm, Username, Node, Id, Method, Props) ->
    case kz_cache:peek_local(?ECALLMGR_AUTH_CACHE, ?CREDS_KEY(Realm, Username)) of
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} -> query_registrar(Realm, Username, Node, Id, Method, Props)
    end.

-spec query_registrar(ne_binary(), ne_binary(), atom(), ne_binary(), ne_binary(), kz_proplist()) ->
                             {'ok', kz_json:object()} |
                             {'error', any()}.
query_registrar(Realm, Username, Node, Id, Method, Props) ->
    lager:debug("looking up credentials of ~s@~s for a ~s", [Username, Realm, Method]),
    Req = [{<<"Msg-ID">>, Id}
          ,{<<"To">>, ecallmgr_util:get_sip_to(Props)}
          ,{<<"From">>, ecallmgr_util:get_sip_from(Props)}
          ,{<<"Orig-IP">>, ecallmgr_util:get_orig_ip(Props)}
          ,{<<"Orig-Port">>, ecallmgr_util:get_orig_port(Props)}
          ,{<<"Method">>, Method}
          ,{<<"Auth-User">>, Username}
          ,{<<"Auth-Realm">>, Realm}
          ,{<<"Expires">>, props:get_value(<<"expires">>, Props)}
          ,{<<"Auth-Nonce">>, props:get_value(<<"sip_auth_nonce">>, Props)}
          ,{<<"Auth-Response">>, props:get_value(<<"sip_auth_response">>, Props)}
          ,{<<"Custom-SIP-Headers">>, kz_json:from_list(ecallmgr_util:custom_sip_headers(Props))}
          ,{<<"User-Agent">>, props:get_value(<<"sip_user_agent">>, Props)}
          ,{<<"Media-Server">>, kz_term:to_binary(Node)}
          ,{<<"Call-ID">>, props:get_value(<<"sip_call_id">>, Props, Id)}
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
-spec maybe_defered_error(ne_binary(), ne_binary(), kz_json:object()) -> {'ok', kz_json:object()} | {'error', 'timeout'}.
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
