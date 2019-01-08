%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Directory lookups from FS
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_authn).
-behaviour(gen_server).

%% API
-export([start_link/1
        ,start_link/2
        ]).
-export([handle_directory_lookup/3]).
-export([lookup_user/4]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-define(SERVER, ?MODULE).

-include("ecallmgr.hrl").

-record(state, {node :: atom()
               ,options = [] :: kz_term:proplist()
               }).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------

-spec start_link(atom()) -> kz_types:startlink_ret().
start_link(Node) -> start_link(Node, []).

-spec start_link(atom(), kz_term:proplist()) -> kz_types:startlink_ret().
start_link(Node, Options) ->
    gen_server:start_link(?SERVER, [Node, Options], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([atom() | kz_term:proplist()]) -> {'ok', state()}.
init([Node, Options]) ->
    process_flag('trap_exit', 'true'),
    kz_util:put_callid(Node),
    lager:info("starting new fs authn listener for ~s", [Node]),
    gen_server:cast(self(), 'bind_to_directory'),
    {'ok', #state{node=Node
                 ,options=Options
                 }}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast('bind_to_directory', #state{node=Node}=State) ->
    case freeswitch:bind(Node, 'directory') of
        'ok' -> {'noreply', State};
        {'error', Reason} ->
            lager:critical("unable to establish directory bindings: ~p", [Reason]),
            {'stop', Reason, State}
    end;
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'fetch', 'directory', <<"domain">>, <<"name">>, _Value, Id, ['undefined' | Props]}
           ,#state{node=Node}=State) ->
    _ = kz_util:spawn(fun handle_directory_lookup/3, [Id, Props, Node]),
    {'noreply', State};
handle_info({'fetch', _Section, _Something, _Key, _Value, Id, ['undefined' | _Props]}, #state{node=Node}=State) ->
    kz_util:spawn(
      fun() ->
              lager:debug("sending empyt reply for request (~s) for ~s ~s from ~s"
                         ,[Id, _Section, _Something, Node]
                         ),
              {'ok', Resp} = ecallmgr_fs_xml:empty_response(),
              freeswitch:fetch_reply(Node, Id, 'directory', Resp)
      end),
    {'noreply', State};
handle_info({'EXIT', _, 'noconnection'}, State) ->
    {stop, {'shutdown', 'noconnection'}, State};
handle_info({'EXIT', _, Reason}, State) ->
    {stop, Reason, State};
handle_info(_Info, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{node=Node}) ->
    lager:info("authn listener for ~s terminating: ~p", [Node, _Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_directory_lookup(kz_term:ne_binary(), kz_term:proplist(), atom()) -> fs_handlecall_ret().
handle_directory_lookup(Id, Props, Node) ->
    kz_util:put_callid(Id),
    lager:debug("received fetch request (~s) user directory from ~s", [Id, Node]),
    case props:get_value(<<"action">>, Props, <<"sip_auth">>) of
        <<"reverse-auth-lookup">> -> lookup_user(Node, Id, <<"reverse-lookup">>, Props);
        <<"sip_auth">> -> maybe_sip_auth_response(Id, Props, Node);
        _Other -> directory_not_found(Node, Id)
    end.

-spec maybe_sip_auth_response(kz_term:ne_binary(), kz_term:proplist(), atom()) -> fs_handlecall_ret().
maybe_sip_auth_response(Id, Props, Node) ->
    case kz_term:is_not_empty(props:get_value(<<"sip_auth_response">>, Props)) of
        'false' -> maybe_kamailio_association(Id, Props, Node);
        'true' ->
            lager:debug("attempting to get device password to verify SIP auth response"),
            lookup_user(Node, Id, <<"password">>, Props)
    end.

-spec maybe_kamailio_association(kz_term:ne_binary(), kz_term:proplist(), atom()) -> fs_handlecall_ret().
maybe_kamailio_association(Id, Props, Node) ->
    case kz_term:is_not_empty(kzd_freeswitch:authorizing_id(Props))
        andalso kz_term:is_not_empty(kzd_freeswitch:authorizing_type(Props))
    of
        'true' -> kamailio_association(Id, Props, Node);
        'false' -> directory_not_found(Node, Id)
    end.

-spec kamailio_association(kz_term:ne_binary(), kz_term:proplist(), atom()) -> fs_handlecall_ret().
kamailio_association(Id, Props, Node) ->
    Password = kz_binary:rand_hex(12),
    Realm = props:get_value(<<"domain">>, Props),
    Username = props:get_value(<<"user">>, Props, props:get_value(<<"Auth-User">>, Props)),
    CCVs = [{Key, Value} || {<<"X-ecallmgr_", Key/binary>>, Value} <- Props],
    JObj = kz_json:from_list_recursive([{<<"Auth-Method">>, <<"password">>}
                                       ,{<<"Auth-Password">>, Password}
                                       ,{<<"Domain-Name">>, Realm}
                                       ,{<<"User-ID">>, Username}
                                       ,{<<"Custom-Channel-Vars">>, CCVs}
                                       ,{<<"Expires">>, 0}
                                       ]),
    lager:debug("building authn resp for ~s@~s from kamailio headers", [Username, Realm]),
    {'ok', Xml} = ecallmgr_fs_xml:authn_resp_xml(JObj),
    lager:debug("sending authn XML to ~w: ~s", [Node, Xml]),
    freeswitch:fetch_reply(Node, Id, 'directory', iolist_to_binary(Xml)).

-spec directory_not_found(atom(), kz_term:ne_binary()) -> fs_handlecall_ret().
directory_not_found(Node, Id) ->
    {'ok', Xml} = ecallmgr_fs_xml:not_found(),
    lager:debug("sending authn not found XML to ~w", [Node]),
    freeswitch:fetch_reply(Node, Id, 'directory', iolist_to_binary(Xml)).

-spec lookup_user(atom(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> fs_handlecall_ret().
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

-spec get_auth_realm(kz_term:proplist()) -> kz_term:ne_binary().
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

-spec get_auth_uri_realm(kz_term:proplist()) -> kz_term:ne_binary().
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

-spec maybe_query_registrar(kz_term:ne_binary(), kz_term:ne_binary(), atom(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) ->
                                   {'ok', kz_json:object()} |
                                   {'error', any()}.
maybe_query_registrar(Realm, Username, Node, Id, Method, Props) ->
    case kz_cache:peek_local(?ECALLMGR_AUTH_CACHE, ?CREDS_KEY(Realm, Username)) of
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} -> query_registrar(Realm, Username, Node, Id, Method, Props)
    end.

-spec query_registrar(kz_term:ne_binary(), kz_term:ne_binary(), atom(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) ->
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

%% Note: Kamailio needs registrar errors since it is blocking with no
%% timeout (at the moment) but when we seek auth for INVITEs we need
%% to wait for conferences, etc.  Since Kamailio does not honor
%% Defer-Response we can use that flag on registrar errors
%% to queue in Kazoo but still advance Kamailio, just need to check here.
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
