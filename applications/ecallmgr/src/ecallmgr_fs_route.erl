%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%% Receive route(dialplan) requests from FS, request routes and respond
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_route).

-behaviour(gen_server).

-export([start_link/1, start_link/2]).
-export([process_route_req/5]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-define(SERVER, ?MODULE).

-include_lib("nksip/include/nksip.hrl").
-include("ecallmgr.hrl").

-record(state, {node = 'undefined' :: atom()
                ,options = [] :: wh_proplist()
               }).

-define(CALLER_PRIVACY(Props)
        ,props:is_true(<<"Caller-Screen-Bit">>, Props, 'false')
       ).

-define(CALLER_PRIVACY_NUMBER(Props)
        ,?CALLER_PRIVACY(Props)
        andalso props:is_true(<<"Caller-Privacy-Hide-Number">>, Props, 'false')
       ).

-define(CALLER_PRIVACY_NAME(Props)
        ,?CALLER_PRIVACY(Props)
        andalso props:is_true(<<"Caller-Privacy-Hide-Name">>, Props, 'false')
       ).

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
-spec start_link(atom()) -> startlink_ret().
-spec start_link(atom(), list()) -> startlink_ret().
start_link(Node) ->
    start_link(Node, []).
start_link(Node, Options) ->
    gen_server:start_link(?MODULE, [Node, Options], []).

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
    lager:info("starting new fs route listener for ~s", [Node]),
    gen_server:cast(self(), 'bind_to_dialplan'),
    gen_server:cast(self(), 'bind_to_chatplan'),
    {'ok', #state{node=Node, options=Options}}.

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
handle_cast('bind_to_dialplan', #state{node=Node}=State) ->
    case freeswitch:bind(Node, 'dialplan') of
        'ok' -> {'noreply', State};
        {'error', Reason} ->
            lager:critical("unable to establish dialplan route bindings: ~p", [Reason]),
            {'stop', Reason, State}
    end;
handle_cast('bind_to_chatplan', #state{node=Node}=State) ->
    case freeswitch:bind(Node, 'chatplan') of
        'ok' -> {'noreply', State};
        {'error', Reason} ->
            lager:critical("unable to establish chatplan route bindings: ~p", [Reason]),
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
handle_info({'fetch', 'chatplan', Something, Key, Value, Id, ['undefined' | Data]}, State) ->
    MsgId = wh_util:rand_hex_binary(16),
    handle_info({'fetch', 'chatplan', Something, Key, Value, Id, [MsgId, {<<"Unique-ID">>, MsgId} | Data]}, State);
handle_info({'fetch', _Section, _Something, _Key, _Value, Id, ['undefined' | _Data]}, #state{node=Node}=State) ->
    lager:warning("fetch unknown section from ~s: ~p So: ~p, K: ~p V: ~p Id: ~s"
                  ,[Node, _Section, _Something, _Key, _Value, Id]),
    {'ok', Resp} = ecallmgr_fs_xml:empty_response(),
    _ = freeswitch:fetch_reply(Node, Id, _Section, Resp),
    {'noreply', State};
handle_info({'fetch', Section, _Tag, _Key, _Value, FSId, [CallId | FSData]}, #state{node=Node}=State) ->
    case {Section
          ,props:get_value(<<"Event-Name">>, FSData)
          ,props:get_value(<<"Event-Subclass">>, FSData)
          ,props:get_value(<<"Caller-Context">>, FSData)
         }
    of
        {'dialplan', <<"REQUEST_PARAMS">>, _SubClass, _Context} ->
            %% TODO: move this to a supervisor somewhere
            lager:info("processing dialplan fetch request ~s (call ~s) from ~s", [FSId, CallId, Node]),
            _ = wh_util:spawn(fun process_route_req/5, [Section, Node, FSId, CallId, FSData]),
            {'noreply', State, 'hibernate'};
        {'chatplan', <<"CUSTOM">>, <<"KZ::", _/binary>>, _Context} ->
            %% TODO: move this to a supervisor somewhere
            lager:info("processing chatplan fetch request ~s (call ~s) from ~s", [FSId, CallId, Node]),
            _ = wh_util:spawn(fun process_route_req/5, [Section, Node, FSId, CallId, init_message_props(FSData)]),
            {'noreply', State, 'hibernate'};
        {'chatplan', <<"REQUEST_PARAMS">>, _SubClass, _Context} ->
            %% TODO: move this to a supervisor somewhere
            lager:info("processing chatplan fetch request ~s (call ~s) from ~s", [FSId, CallId, Node]),
            _ = wh_util:spawn(fun process_route_req/5, [Section, Node, FSId, CallId, init_message_props(FSData)]),
            {'noreply', State, 'hibernate'};
        {'chatplan', <<"MESSAGE">>, _SubClass, _Context} ->
            %% TODO: move this to a supervisor somewhere
            lager:info("processing chatplan fetch request ~s (call ~s) from ~s", [FSId, CallId, Node]),
            _ = wh_util:spawn(fun process_route_req/5, [Section, Node, FSId, CallId, init_message_props(FSData)]),
            {'noreply', State, 'hibernate'};
        {_, _Other, _, _Context} ->
            lager:debug("ignoring ~s event ~s in context ~s from ~s", [Section, _Other, _Context, Node]),
            {'ok', Resp} = ecallmgr_fs_xml:empty_response(),
            _ = freeswitch:fetch_reply(Node, FSId, Section, Resp),
            {'noreply', State, 'hibernate'}
    end;
handle_info(_Other, State) ->
    lager:debug("unhandled msg: ~p", [_Other]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{node=Node}) ->
    lager:info("route listener for ~s terminating: ~p", [Node, _Reason]).

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
-spec should_expand_var(any()) -> boolean().
should_expand_var({<<?CHANNEL_VAR_PREFIX, _/binary>>, _}) -> 'true';
should_expand_var({<<"sip_", _/binary>>, _}) -> 'true';
should_expand_var(_) -> 'false'.

-spec init_message_props(wh_proplist()) -> wh_proplist().
init_message_props(Props) ->
    Routines = [fun add_message_missing_props/1
                ,fun expand_message_vars/1
               ],
    lists:foldl(fun(F,P) -> F(P) end, Props, Routines).

-spec add_message_missing_props(wh_proplist()) -> wh_proplist().
add_message_missing_props(Props) ->
    lists:foldl(fun({K, _V}= A,B) ->
                        case props:get_value(K, Props) of
                            'undefined' -> [A | B];
                            _Else -> B
                        end
                end, Props,
                [{<<"Call-Direction">>, <<"outbound">>}
                 ,{<<"Resource-Type">>,<<"sms">>}
                 ,{<<"Message-ID">>, wh_util:rand_hex_binary(16)}
                 ,{<<"Caller-Caller-ID-Number">>, props:get_value(<<"from_user">>, Props)}
                 ,{<<"Caller-Destination-Number">>, props:get_value(<<"to_user">>, Props)}
                ]).

-spec expand_message_vars(wh_proplist()) -> wh_proplist().
expand_message_vars(Props) ->
    lists:foldl(fun({K,V}, Ac) ->
                        case props:get_value(<<"variable_", K/binary>>, Ac) of
                            'undefined' -> props:set_value(<<"variable_", K/binary>>, V, Ac);
                            _ -> Ac
                        end
                end, Props,
                props:filter(fun should_expand_var/1, Props)
                 ).

-spec process_route_req(atom(), atom(), ne_binary(), ne_binary(), wh_proplist()) -> 'ok'.
process_route_req(Section, Node, FetchId, CallId, Props) ->
    wh_util:put_callid(CallId),
    case wh_util:is_true(props:get_value(<<"variable_recovered">>, Props)) of
        'false' -> search_for_route(Section, Node, FetchId, CallId, ecallmgr_fs_loopback:filter(Node, CallId, Props));
        'true' ->
            lager:debug("recovered channel already exists on ~s, park it", [Node]),
            JObj = wh_json:from_list([{<<"Routes">>, []}
                                      ,{<<"Method">>, <<"park">>}
                                      ,{<<"Context">>, hunt_context(Props)}
                                     ]),
            reply_affirmative(Section, Node, FetchId, CallId, JObj)
    end.

-spec search_for_route(atom(), atom(), ne_binary(), ne_binary(), wh_proplist()) -> 'ok'.
search_for_route(Section, Node, FetchId, CallId, Props) ->
    SetupCall = props:set_value(<<"Call-Setup">>, <<"true">>, Props),
    _ = wh_util:spawn(fun ecallmgr_fs_authz:authorize/3, [SetupCall, CallId, Node]),
    ReqResp = wh_amqp_worker:call(route_req(CallId, FetchId, Props, Node)
                                  ,fun wapi_route:publish_req/1
                                  ,fun wapi_route:is_actionable_resp/1
                                  ,ecallmgr_fs_node:fetch_timeout(Node)
                                 ),
    case ReqResp of
        {'error', _R} ->
            lager:info("did not receive route response for request ~s: ~p", [FetchId, _R]);
        {'ok', JObj} ->
            'true' = wapi_route:resp_v(JObj),
            J = wh_json:set_value(<<"Context">>, hunt_context(Props), JObj),
            maybe_wait_for_authz(Section, Node, FetchId, CallId, J)
    end.

-spec hunt_context(wh_proplist()) -> api_binary().
hunt_context(Props) ->
    props:get_value(<<"Hunt-Context">>, Props, ?DEFAULT_FREESWITCH_CONTEXT).

-spec maybe_wait_for_authz(atom(), atom(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
maybe_wait_for_authz(Section, Node, FetchId, CallId, JObj) ->
    case wh_util:is_true(ecallmgr_config:get(<<"authz_enabled">>, 'false'))
        andalso wh_json:get_value(<<"Method">>, JObj) =/= <<"error">>
    of
        'true' -> wait_for_authz(Section, Node, FetchId, CallId, JObj);
        'false' -> reply_affirmative(Section, Node, FetchId, CallId, JObj)
    end.

-spec wait_for_authz(atom(), atom(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
wait_for_authz(Section, Node, FetchId, CallId, JObj) ->
    case wh_cache:wait_for_key_local(?ECALLMGR_UTIL_CACHE, ?AUTHZ_RESPONSE_KEY(CallId)) of
        {'ok', {'true', AuthzCCVs}} ->
            _ = wh_cache:erase_local(?ECALLMGR_UTIL_CACHE, ?AUTHZ_RESPONSE_KEY(CallId)),
            CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
            J = wh_json:set_value(<<"Custom-Channel-Vars">>
                                  ,wh_json:merge_jobjs(CCVs, AuthzCCVs)
                                  ,JObj
                                 ),
            reply_affirmative(Section, Node, FetchId, CallId, J);
        _Else -> reply_forbidden(Section, Node, FetchId)
    end.

%% Reply with a 402 for unauthzed calls
-spec reply_forbidden(atom(), atom(), ne_binary()) -> 'ok'.
reply_forbidden(Section, Node, FetchId) ->
    lager:info("received forbidden route response for ~s, sending 403 Incoming call barred", [FetchId]),
    {'ok', XML} = ecallmgr_fs_xml:route_resp_xml([{<<"Method">>, <<"error">>}
                                                  ,{<<"Route-Error-Code">>, <<"403">>}
                                                  ,{<<"Route-Error-Message">>, <<"Incoming call barred">>}
                                                  ,{<<"Fetch-Section">>, wh_util:to_binary(Section)}
                                                 ]),
    lager:debug("sending XML to ~s: ~s", [Node, XML]),
    case freeswitch:fetch_reply(Node, FetchId, Section, iolist_to_binary(XML), 3 * ?MILLISECONDS_IN_SECOND) of
        'ok' -> lager:info("node ~s accepted ~s route response for request ~s", [Node, Section, FetchId]);
        {'error', Reason} -> lager:debug("node ~s rejected our ~s route unauthz: ~p", [Node, Section, Reason])
    end.

-spec reply_affirmative(atom(), atom(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
reply_affirmative(Section, Node, FetchId, CallId, JObj) ->
    lager:info("received affirmative route response for request ~s", [FetchId]),
    {'ok', XML} = ecallmgr_fs_xml:route_resp_xml(JObj),
    lager:debug("sending XML to ~s: ~s", [Node, XML]),
    case freeswitch:fetch_reply(Node, FetchId, Section, iolist_to_binary(XML), 3 * ?MILLISECONDS_IN_SECOND) of
        {'error', _Reason} -> lager:debug("node ~s rejected our ~s route response: ~p", [Node, Section, _Reason]);
        'ok' ->
            lager:info("node ~s accepted ~s route response for request ~s", [Node, Section, FetchId]),
            ecallmgr_fs_channels:update(CallId, #channel.handling_locally, 'true'),
            maybe_start_call_handling(Node, FetchId, CallId, JObj)
    end.

-spec maybe_start_call_handling(atom(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
maybe_start_call_handling(Node, FetchId, CallId, JObj) ->
    case wh_json:get_value(<<"Method">>, JObj) of
        <<"error">> -> lager:debug("sent error response to ~s, not starting call handling", [Node]);
        <<"sms">> -> start_message_handling(Node, FetchId, CallId, JObj);
        _Else -> start_call_handling(Node, FetchId, CallId, JObj)
    end.

-spec start_call_handling(atom(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
start_call_handling(Node, FetchId, CallId, JObj) ->
    ServerQ = wh_json:get_value(<<"Server-ID">>, JObj),
    CCVs =
        wh_json:set_values(
          [{<<"Application-Name">>, wh_json:get_value(<<"App-Name">>, JObj)}
           ,{<<"Application-Node">>, wh_json:get_value(<<"Node">>, JObj)}
          ]
          ,wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new())
         ),
    _Evt = ecallmgr_call_sup:start_event_process(Node, CallId),
    _Ctl = ecallmgr_call_sup:start_control_process(Node, CallId, FetchId, ServerQ, CCVs),

    lager:debug("started event ~p and control ~p processes", [_Evt, _Ctl]),

    ecallmgr_util:set(Node, CallId, wh_json:to_proplist(CCVs)).

-spec start_message_handling(atom(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
start_message_handling(_Node, _FetchId, CallId, JObj) ->
    ServerQ = wh_json:get_value(<<"Server-ID">>, JObj),
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
    Win = [{<<"Msg-ID">>, CallId}
           ,{<<"Call-ID">>, CallId}
           ,{<<"Control-Queue">>, <<"chatplan_ignored">>}
           ,{<<"Custom-Channel-Vars">>, CCVs}
           | wh_api:default_headers(<<"dialplan">>, <<"route_win">>, ?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("sending route_win to ~s", [ServerQ]),
    wh_amqp_worker:cast(Win, fun(Payload)-> wapi_route:publish_win(ServerQ, Payload) end).

-spec route_req(ne_binary(), ne_binary(), wh_proplist(), atom()) -> wh_proplist().
route_req(CallId, FetchId, Props, Node) ->
    SwitchURL = ecallmgr_fs_node:sip_url(Node),
    [_, SwitchURIHost] = binary:split(SwitchURL, <<"@">>),
    SwitchURI = <<"sip:", SwitchURIHost/binary>>,
    [{<<"Msg-ID">>, FetchId}
     ,{<<"Call-ID">>, CallId}
     ,{<<"Message-ID">>, props:get_value(<<"Message-ID">>, Props)}
     ,{<<"Caller-ID-Name">>, caller_id_name(Props)}
     ,{<<"Caller-ID-Number">>, caller_id_number(Props)}
     ,{<<"From-Network-Addr">>, kzd_freeswitch:from_network_ip(Props)}
     ,{<<"From-Network-Port">>, kzd_freeswitch:from_network_port(Props)}
     ,{<<"User-Agent">>, kzd_freeswitch:user_agent(Props)}
     ,{<<"To">>, ecallmgr_util:get_sip_to(Props)}
     ,{<<"From">>, ecallmgr_util:get_sip_from(Props)}
     ,{<<"Request">>, ecallmgr_util:get_sip_request(Props)}
     ,{<<"Body">>, get_body(Props) }
     ,{<<"SIP-Request-Host">>, props:get_value(<<"variable_sip_req_host">>, Props)}
     ,{<<"Switch-Nodename">>, wh_util:to_binary(Node)}
     ,{<<"Switch-Hostname">>, props:get_value(<<"FreeSWITCH-Hostname">>, Props)}
     ,{<<"Switch-URL">>, SwitchURL}
     ,{<<"Switch-URI">>, SwitchURI}
     ,{<<"Custom-Channel-Vars">>, wh_json:from_list(route_req_ccvs(FetchId, Props))}
     ,{<<"Custom-SIP-Headers">>, wh_json:from_list(ecallmgr_util:custom_sip_headers(Props))}
     ,{<<"Resource-Type">>, kzd_freeswitch:resource_type(Props, <<"audio">>)}
     ,{<<"To-Tag">>, props:get_value(<<"variable_sip_to_tag">>, Props)}
     ,{<<"From-Tag">>, props:get_value(<<"variable_sip_from_tag">>, Props)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec route_req_ccvs(ne_binary(), wh_proplist()) -> wh_proplist().
route_req_ccvs(FetchId, Props) ->
    {RedirectedBy, RedirectedReason} = get_redirected(Props),

    props:filter_undefined(
      [{<<"Fetch-ID">>, FetchId}
       ,{<<"Redirected-By">>, RedirectedBy}
       ,{<<"Redirected-Reason">>, RedirectedReason}
       ,{<<"Caller-Privacy-Number">>, ?CALLER_PRIVACY_NUMBER(Props)}
       ,{<<"Caller-Privacy-Name">>, ?CALLER_PRIVACY_NAME(Props)}
       | ecallmgr_util:custom_channel_vars(Props)
      ]
     ).

%% TODO
%% check content-type and decode properly
%% some sip clients send text/html with entities encoded
%% some other use application/vnd.3gpp.sms
-spec get_body(wh_proplist()) -> api_binary().
get_body(Props) ->
    props:get_value(<<"body">>, Props).

-spec get_redirected(wh_proplist()) ->
                            {api_binary(), api_binary()}.
get_redirected(Props) ->
    case props:get_value(<<"variable_last_bridge_hangup_cause">>, Props) of
        <<"REDIRECTION_TO_NEW_DESTINATION">> ->
            case props:get_value(<<"variable_sip_redirected_by">>, Props) of
                'undefined' -> {'undefined' , 'undefined'};
                Contact ->
                    [#uri{user=User,ext_opts=Opts}] = nksip_parse:uris(Contact),
                    {User , props:get_value(<<"reason">>,Opts)}
            end;
        _ -> {'undefined' , 'undefined'}
    end.

-spec caller_id_name(wh_proplist()) -> ne_binary().
caller_id_name(Props) ->
    caller_id_name(?CALLER_PRIVACY_NAME(Props), Props).

-spec caller_id_name(boolean(), wh_proplist()) -> ne_binary().
caller_id_name('true', _Props) ->
    wh_util:anonymous_caller_id_name();
caller_id_name('false', Props) ->
    kzd_freeswitch:caller_id_name(Props, wh_util:anonymous_caller_id_name()).

-spec caller_id_number(wh_proplist()) -> ne_binary().
caller_id_number(Props) ->
    caller_id_number(?CALLER_PRIVACY_NUMBER(Props), Props).

-spec caller_id_number(boolean(), wh_proplist()) -> ne_binary().
caller_id_number('true', _Props) ->
    wh_util:anonymous_caller_id_number();
caller_id_number('false', Props) ->
    kzd_freeswitch:caller_id_number(Props, wh_util:anonymous_caller_id_number()).
