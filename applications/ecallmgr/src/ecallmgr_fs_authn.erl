%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%% Directory lookups from FS
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
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
                ,options = [] :: wh_proplist()
               }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Node) -> start_link(Node, []).
start_link(Node, Options) -> gen_server:start_link(?MODULE, [Node, Options], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Node, Options]) ->
    wh_util:put_callid(Node),
    lager:info("starting new fs authn listener for ~s", [Node]),
    gen_server:cast(self(), 'bind_to_directory'),
    {'ok', #state{node=Node
                  ,options=Options
                 }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'fetch', 'directory', <<"domain">>, <<"name">>, _Value, Id, ['undefined' | Props]}
            ,#state{node=Node}=State) ->
    _ = wh_util:spawn(?MODULE, 'handle_directory_lookup', [Id, Props, Node]),
    {'noreply', State};
handle_info({'fetch', _Section, _Something, _Key, _Value, Id, ['undefined' | _Props]}, #state{node=Node}=State) ->
    wh_util:spawn(
      fun() ->
              lager:debug("sending empyt reply for request (~s) for ~s ~s from ~s",
                          [Id, _Section, _Something, Node]),
              {'ok', Resp} = ecallmgr_fs_xml:empty_response(),
              freeswitch:fetch_reply(Node, Id, 'directory', Resp)
      end),
    {'noreply', State};
handle_info(_Info, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{node=Node}) ->
    lager:info("authn listener for ~s terminating: ~p", [Node, _Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_directory_lookup(Id, Props, Node) ->
    wh_util:put_callid(Id),
    case props:get_value(<<"sip_auth_method">>, Props) of
        <<"REGISTER">> ->
            lager:debug("received fetch request (~s) for sip registration creds from ~s", [Id, Node]);
        Else ->
            lager:debug("received fetch request for ~s (~s) user creds from ~s", [Else, Id, Node])
    end,
    whistle_stats:increment_counter("register-attempt"),
    case {props:get_value(<<"Event-Name">>, Props), props:get_value(<<"action">>, Props, <<"sip_auth">>)} of
        {<<"REQUEST_PARAMS">>, <<"sip_auth">>} ->
            Method = props:get_value(<<"sip_auth_method">>, Props, <<"password">>),
            lookup_user(Node, Id, Method, Props);
        {<<"REQUEST_PARAMS">>, <<"reverse-auth-lookup">>} ->
            lookup_user(Node, Id, <<"reverse-lookup">>, Props);
        _Other ->
            {'ok', Resp} = ecallmgr_fs_xml:empty_response(),
            _ = freeswitch:fetch_reply(Node, Id, 'directory', Resp),
            lager:debug("ignoring authn request from ~s for ~p", [Node, _Other])
    end.

-spec lookup_user(atom(), ne_binary(), ne_binary(), wh_proplist()) -> fs_handlecall_ret().
lookup_user(Node, Id, Method,  Props) ->
    Domain = props:get_value(<<"domain">>, Props),
    Realm = get_auth_realm(Props),
    Username = props:get_value(<<"user">>, Props, props:get_value(<<"Auth-User">>, Props)),
    ReqResp = maybe_query_registrar(Realm, Username, Node, Id, Method, Props),
    {'ok', Xml} = handle_lookup_resp(Method, Domain, Username, ReqResp),
    lager:debug("sending authn XML to ~w: ~s", [Node, Xml]),
    freeswitch:fetch_reply(Node, Id, 'directory', iolist_to_binary(Xml)).

-spec get_auth_realm(wh_proplist()) -> ne_binary().
get_auth_realm(Props) ->
    case props:get_first_defined([<<"sip_auth_realm">>
                                  ,<<"domain">>
                                 ], Props)
    of
        'undefined' -> get_auth_uri_realm(Props);
        Realm ->
            case wh_network_utils:is_ipv4(Realm)
                orelse wh_network_utils:is_ipv6(Realm)
            of
                'true' -> get_auth_uri_realm(Props);
                'false' -> wh_util:to_lower_binary(Realm)
            end
    end.

-spec get_auth_uri_realm(wh_proplist()) -> ne_binary().
get_auth_uri_realm(Props) ->
    AuthURI = props:get_value(<<"sip_auth_uri">>, Props, <<>>),
    case binary:split(AuthURI, <<"@">>) of
        [_, Realm] -> wh_util:to_lower_binary(Realm);
        _Else ->
            props:get_first_defined([<<"Auth-Realm">>
                                    ,<<"sip_request_host">>
                                    ,<<"sip_to_host">>
                                    ], Props)
    end.

-spec handle_lookup_resp(ne_binary(), ne_binary(), ne_binary()
                         ,{'ok', wh_json:object()} | {'error', _}) ->
                                {'ok', _}.
handle_lookup_resp(<<"reverse-lookup">>, Realm, Username, {'ok', JObj}) ->
    Props = [{<<"Domain-Name">>, Realm}
             ,{<<"User-ID">>, Username}
            ],
    lager:debug("building reverse authn resp for ~s@~s", [Username, Realm]),
    ecallmgr_fs_xml:reverse_authn_resp_xml(wh_json:set_values(Props, JObj));
handle_lookup_resp(_, Realm, Username, {'ok', JObj}) ->
    Props = [{<<"Domain-Name">>, Realm}
             ,{<<"User-ID">>, Username}
            ],
    lager:debug("building authn resp for ~s@~s", [Username, Realm]),
    ecallmgr_fs_xml:authn_resp_xml(wh_json:set_values(Props, JObj));
handle_lookup_resp(_, _, _, {'error', _R}) ->
    lager:debug("authn request lookup failed: ~p", [_R]),
    ecallmgr_fs_xml:not_found().

-spec maybe_query_registrar(ne_binary(), ne_binary(), atom(), ne_binary(), ne_binary(), wh_proplist()) ->
                                   {'ok', wh_json:object()} |
                                   {'error', _}.
maybe_query_registrar(Realm, Username, Node, Id, Method, Props) ->
    case wh_cache:peek_local(?ECALLMGR_AUTH_CACHE, ?CREDS_KEY(Realm, Username)) of
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} -> query_registrar(Realm, Username, Node, Id, Method, Props)
    end.

-spec query_registrar(ne_binary(), ne_binary(), atom(), ne_binary(), ne_binary(), wh_proplist()) ->
                             {'ok', wh_json:object()} |
                             {'error', _}.
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
           ,{<<"Custom-SIP-Headers">>, wh_json:from_list(ecallmgr_util:custom_sip_headers(Props))}
           ,{<<"User-Agent">>, props:get_value(<<"sip_user_agent">>, Props)}
           ,{<<"Media-Server">>, wh_util:to_binary(Node)}
           ,{<<"Call-ID">>, props:get_value(<<"sip_call_id">>, Props, Id)}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    ReqResp = wh_amqp_worker:call(props:filter_undefined(Req)
                                  ,fun wapi_authn:publish_req/1
                                  ,fun wapi_authn:resp_v/1
                                 ),
    case ReqResp of
        {'error', _}=E -> E;
        {'ok', JObj} -> maybe_defered_error(Realm, Username, JObj)
    end.

%% NOTE: Kamailio needs registrar errors since it is blocking with no
%%   timeout (at the moment) but when we seek auth for INVITEs we need
%%   to wait for conferences, ect.  Since Kamailio does not honor
%%   Defer-Response we can use that flag on registrar errors
%%   to queue in Kazoo but still advance Kamailio, just need to check here.
-spec maybe_defered_error(ne_binary(), ne_binary(), wh_json:object()) -> {'ok', wh_json:object()} | {'error', 'timeout'}.
maybe_defered_error(Realm, Username, JObj) ->
    case wapi_authn:resp_v(JObj) of
        'false' -> {'error', 'timeout'};
        'true' ->
            AccountId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj),
            AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
            AuthorizingId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Authorizing-ID">>], JObj),
            OwnerIdProp = case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Owner-ID">>], JObj) of
                              'undefined' -> [];
                              OwnerId -> [{'db', AccountDb, OwnerId}]
                          end,
            CacheProps = [{'origin', [{'db', AccountDb, AuthorizingId}
                                     ,{'db', AccountDb, AccountId}
                                      | OwnerIdProp
                                     ]}
                         ],
            wh_cache:store_local(?ECALLMGR_AUTH_CACHE, ?CREDS_KEY(Realm, Username), JObj, CacheProps),
            {'ok', JObj}
    end.
