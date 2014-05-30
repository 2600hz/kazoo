%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%% Notify-type requests, like MWI updates, received and processed here
%%% @end
%%% @contributors
%%%    Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_msg).

-behaviour(gen_listener).

-export([start_link/1, start_link/2]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-export([process_route_req/4]).
-export([handle_message_route/2]).

-record(state, {
          node :: atom()
          ,options :: wh_proplist()
         }).

-define(SERVER, ?MODULE).

-define(BINDINGS, [{'sms', []}
                   ,{'self', []}
                  ]).
-define(RESPONDERS, [
                     {{?MODULE, 'handle_message_route'}, [{<<"message">>, <<"route">>}]}
                    ]).

-define(QUEUE_NAME, <<"ecallmgr_fs_msg">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

-include_lib("ecallmgr.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom()) -> startlink_ret().
-spec start_link(atom(), wh_proplist()) -> startlink_ret().

start_link(Node) ->
    start_link(Node, []).

start_link(Node, Options) ->
    gen_listener:start_link(?MODULE
                            ,[{'responders', ?RESPONDERS}
                              ,{'bindings', ?BINDINGS}
                              ,{'queue_name', ?QUEUE_NAME}
                              ,{'queue_options', ?QUEUE_OPTIONS}
                              ,{'consume_options', ?CONSUME_OPTIONS}
                             ]
                            ,[Node, Options]).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
init([Node, Options]) ->
    put(callid, Node),
    lager:debug("starting new ecallmgr msg process"),
    gproc:reg({'p', 'l', 'fs_msg'}),
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
handle_cast('bind_to_msg_events', #state{node=Node}=State) ->
    gproc:reg({'p', 'l', ?FS_EVENT_REG_MSG(Node, <<"KZ::DELIVERY_REPORT">>)}),
    gproc:reg({'p', 'l', ?FS_EVENT_REG_MSG(Node, <<"KZ::MESSAGE">>)}),
    lager:debug("bound to recv_message events on node ~s", [Node]),
    {'noreply', State};
handle_cast('bind_to_chatplan', #state{node=Node}=State) ->
    gen_listener:cast(self(), 'bind_to_msg_events'),
    case freeswitch:bind(Node, 'chatplan') of
        'ok' -> {'noreply', State};
        {'error', Reason} ->
            lager:critical("unable to establish route bindings: ~p", [Reason]),
            {'stop', Reason, State}
    end;
handle_cast(_Msg, State) ->
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
handle_info({'fetch', _Section, _Something, _Key, _Value, Id, ['undefined' | _Data]}, #state{node=Node}=State) ->
    lager:warning("fetch unknown section from ~s: ~p So: ~p, K: ~p V: ~p Id: ~s"
                  ,[Node, _Section, _Something, _Key, _Value, Id]),
    {'ok', Resp} = ecallmgr_fs_xml:empty_response(),
    _ = freeswitch:fetch_reply(Node, Id, _Section, Resp),
    {'noreply', State};
handle_info({'fetch', 'chatplan', _Tag, _Key, _Value, FSId, [CallId | FSData]}, #state{node=Node}=State) ->
    case {props:get_value(<<"Event-Name">>, FSData)
          ,props:get_value(<<"Event-Subclass">>, FSData)
          ,props:get_value(<<"Caller-Context">>, FSData)
         }
    of
        {<<"REQUEST_PARAMS">>, _, _Context} ->
            %% TODO: move this to a supervisor somewhere
            lager:info("processing chatplan fetch request ~s (call ~s) from ~s", [FSId, CallId, Node]),
            spawn(?MODULE, 'process_route_req', [Node, FSId, CallId, init_props(FSData)]),
            {'noreply', State, 'hibernate'};
        {<<"CUSTOM">>, <<"KZ::", _/binary>>, _Context} ->
            %% TODO: move this to a supervisor somewhere
            lager:info("processing chatplan fetch request ~s (call ~s) from ~s", [FSId, CallId, Node]),
            spawn(?MODULE, 'process_route_req', [Node, FSId, CallId, init_props(FSData)]),
            {'noreply', State, 'hibernate'};
        {_Other, _OtherSub, _Context} ->
            lager:debug("ignoring event ~s/~s in context ~s from ~s", [_Other, _OtherSub, _Context, Node]),
            {'ok', Resp} = ecallmgr_fs_xml:empty_response(),
            _ = freeswitch:fetch_reply(Node, FSId, 'chatplan', Resp),
            props:to_log(FSData, <<"IGNORED">>),
            {'noreply', State, 'hibernate'}
    end;
handle_info({'event', [_X | Props]}, #state{node=Node}=State) ->
    lager:info("fs event ~p",[_X]),
    props:to_log(Props, <<"FS_MSG">>),
    {'noreply', State, 'hibernate'};
handle_info({'EXIT', _, _}, State) ->
    {'noreply', State};
handle_info(_Info, State) ->
    lager:debug("MSG UN", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, #state{node=Node}) ->
    {'reply', [{'node', Node}]}.

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
    lager:info("notify listener for ~s terminating: ~p", [Node, _Reason]).

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

-spec is_channel_var(term()) -> boolean().
is_channel_var({<<?CHANNEL_VAR_PREFIX, _/binary>>, _}) -> 'true';
is_channel_var(_) -> 'false'.

-spec init_props(wh_proplist()) -> wh_proplist().
init_props(Props) ->
    Filtered = props:filter(fun is_channel_var/1, Props),
    lists:foldl(fun({K,V}, Ac) ->
                        case props:get_value(<<"variable_", K/binary>>, Ac) of
                            'undefined' -> props:set_value(<<"variable_", K/binary>>, V, Ac);
                            _ -> Ac
                        end
                end, Props, Filtered).
    

get_target(To, Username) ->
    [ToUser, ToRealm] = binary:split(To , <<"@">>),
    lager:info("To Parts ~p / ~p",[ToUser, ToRealm]),
    Target = <<Username/binary, "@", ToRealm/binary>>.

get_caller_id(CID, From) ->
    [FromUser, FromRealm] = binary:split(From , <<"@">>),
    lager:info("From Parts ~p / ~p",[FromUser, FromRealm]),
    CallerID = <<CID/binary, "@", FromRealm/binary>>.
    

-spec handle_message_route(wh_json:object(), wh_proplist()) -> no_return().
handle_message_route(JObj, Props) ->
    _ = wh_util:put_callid(JObj),
    lager:info("process_message received SIP/SIMPLE Msg : ~p", [JObj]),
    Node = props:get_value('node', Props),
    From = wh_json:get_value(<<"From">>, JObj),
    CIDNumber = wh_json:get_value(<<"Caller-ID-Number">>, JObj),
    To = wh_json:get_value(<<"To">>, JObj),
    Body = wh_json:get_value(<<"Body">>, JObj),
    MessageId = wh_json:get_value(<<"Message-ID">>, JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    
    Contact = wh_json:get_value(<<"Contact">>, JObj),
    IP = wh_json:get_value(<<"Contact-IP">>, JObj),
    Port = wh_json:get_value(<<"Contact-Port">>, JObj),
    
%    case ecallmgr_registrar:lookup_original_contact(Realm, Username) of
%        {'error', 'not_found'} ->
%            lager:warning("failed to find contact for ~s@~s, dropping MWI update", [Username, Realm]);
%        {'ok', Contact} ->
%            Node = props:get_value('node', Props),
%            send_mwi_update(JObj, Node, Username, Realm, Contact)
%    end.

    
    Username = wh_json:get_value(<<"Contact-Username">>, JObj),
    [ContactUser, ContactRealm] = binary:split(Contact , <<"@">>),
 %   lager:info("Contact Parts ~p / ~p",[ContactUser, ContactRealm]),
    [FromUser, FromRealm] = binary:split(From , <<"@">>),
 %   lager:info("From Parts ~p / ~p",[FromUser, FromRealm]),
    [ToUser, ToRealm] = binary:split(To , <<"@">>),
 %   lager:info("To Parts ~p / ~p",[ToUser, ToRealm]),
    Target = get_target(To, Username),
    CallerID = get_caller_id(CIDNumber, From),
    CallerIDFull = <<"<sip:", CallerID/binary, ">">>,
    
    [User, Realm] = binary:split(To, <<"@">>),
            Header2 = [
               {"sip_profile", ?DEFAULT_FS_PROFILE}
              ,{"proto", "sip"}
              ,{"blocking", "true"}
              ,{"dest_proto", "sip"}
              ,{"to", wh_util:to_list(Target)}
              ,{"to_sip_ip", wh_util:to_list(IP)}
              ,{"to_sip_port", wh_util:to_list(Port)}
              ,{"from", wh_util:to_list(CallerID)}
              ,{"from_full", wh_util:to_list(CallerIDFull)}
              ,{"content-length", wh_util:to_list(size(Body))}
              ,{"type", "text/plain"}
              ,{"body", Body}
              ,{"Call-ID", wh_util:to_list(CallId)}
              ,{"Unique-ID", "qwertyuiop+sdfghjkl"}
              ,{"TTY-67", wh_util:to_list(CallId)}
              ,{"Message-ID", wh_util:to_list(MessageId)}
            ],
%    lager:info("SENDING ~p",[Header2]),
%            Resp = freeswitch:sendevent(Node, 'SEND_MESSAGE', Header),
            Resp = freeswitch:sendevent_custom(Node, 'SMS::SEND_MESSAGE', Header2),    
            lager:info("sent SIP/SIMPLE Msg to '~s' via ~s: ~p", [To, Node, Resp]).


   

-spec reply_affirmative2(atom(), ne_binary(), wh_proplist()) -> 'ok'.
reply_affirmative2(Node, FetchId, Props) ->
    XML1 = ["<document type='freeswitch/xml'><section name='chatplan' description='Chat Response'><context name='context_2'>
       <extension name='demo'>
         <condition field='to' expression='^321(.*)$'>
         <action application='set' data='to_sip_ip=192.168.0.21' />
         <action application='set' data='to_sip_port=5062' />
         <action application='set' data='to=IMSI001010000000001@mobile.rangenetworks.com' />
         <action application='set' data='from_full=sip:2002@mobile.rangenetworks.com' />
         <action application='send' />
         </condition>
       </extension>
       <extension name='demo2'>
         <condition field='to' expression='^(.*)$'>
         <action application='reply' data='Your Message to ${to_user} was not delivered: ${_body}'/>
         </condition>
       </extension>
     </context></section></document>"],
    XML2 = ["<document type='freeswitch/xml'><section name='chatplan' description='Chat Response'><context name='context_2'>
       <extension name='stop'>
         <condition field='to' expression='^(.*)$'>
           <action application='stop' data='stored'/>
         </condition>
       </extension>
     </context></section></document>"],
    
    % {'ok', XML3} = ecallmgr_fs_xml:route_resp_xml(JObj),
    {'ok', XML} = ecallmgr_fs_xml:route_resp_xml([{<<"Method">>, <<"chat">>}
                                                  ,{<<"Route-Error-Code">>, <<"403">>}
                                                  ,{<<"Route-Error-Message">>, <<"Incoming call barred">>}
                                                 ]),
    
    
    case freeswitch:fetch_reply(Node, FetchId, 'chatplan', iolist_to_binary(XML), 3000) of
        {'error', _Reason} -> lager:debug("node ~s rejected our route response: ~p", [Node, _Reason]);
        'ok' ->
            lager:info("node ~s accepted route response for request ~s", [Node, FetchId])
    end.

-spec process_route_req(atom(), ne_binary(), ne_binary(), wh_proplist()) -> 'ok'.
process_route_req(Node, FetchId, CallId, Props) ->
    put('callid', FetchId),
    props:to_log(Props,<<"CHAT PROPS">>),


    _ = spawn('ecallmgr_fs_authz', 'authorize', [props:set_values([{<<"Message-Setup">>, <<"true">>}
                                                                   ,{<<"Call-Setup">>, <<"true">>}
                                                                   ,{<<"Call-Direction">>, <<"inbound">>}
                                                                  ], Props)
                                                 ,CallId
                                                 ,Node
                                                ]),
    
    ReqResp = wh_amqp_worker:call(?ECALLMGR_AMQP_POOL
                                  ,route_req(CallId, FetchId, Props, Node)
                                  ,fun wapi_route:publish_req/1
                                  ,fun wapi_route:is_actionable_resp/1
                                  ,2500
                                 ),
    case ReqResp of
        {'error', _R} ->
            lager:info("did not receive route response for request ~s: ~p", [FetchId, _R]);
        {'ok', JObj} ->
            lager:debug("ROUTE RESPONSE ~p", [JObj]),
            'true' = wapi_route:resp_v(JObj),
            J = wh_json:set_value(<<"Context">>, hunt_context(Props), JObj),
            maybe_wait_for_authz(J, Node, FetchId, CallId)   
    end.
    
hunt_context(Props) ->
    props:get_value(<<"Hunt-Context">>, Props, ?DEFAULT_FREESWITCH_CONTEXT).

maybe_wait_for_authz(JObj, Node, FetchId, CallId) ->
    case wh_util:is_true(ecallmgr_config:get(<<"authz_enabled">>, 'false'))
        andalso wh_json:get_value(<<"Method">>, JObj) =/= <<"error">>
    of
        'true' -> wait_for_authz(JObj, Node, FetchId, CallId);
        'false' -> reply_affirmative(Node, FetchId, CallId, JObj)
    end.

wait_for_authz(JObj, Node, FetchId, CallId) ->
    case wh_cache:wait_for_key_local(?ECALLMGR_UTIL_CACHE, ?AUTHZ_RESPONSE_KEY(CallId)) of
        {'ok', {'true', AuthzCCVs}} ->
            _ = wh_cache:erase_local(?ECALLMGR_UTIL_CACHE, ?AUTHZ_RESPONSE_KEY(CallId)),
            CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
            J = wh_json:set_value(<<"Custom-Channel-Vars">>
                                  ,wh_json:merge_jobjs(CCVs, AuthzCCVs)
                                  ,JObj),
            reply_affirmative(Node, FetchId, CallId, J);
        _Else -> reply_forbidden(Node, FetchId)
    end.

%% Reply with a 402 for unauthzed calls
-spec reply_forbidden(atom(), ne_binary()) -> 'ok'.
reply_forbidden(Node, FetchId) ->
    lager:info("received forbidden route response for ~s, sending 403 Incoming call barred", [FetchId]),
    {'ok', XML} = ecallmgr_fs_xml:route_resp_xml([{<<"Method">>, <<"sms_error">>}
                                                  ,{<<"Route-Error-Code">>, <<"403">>}
                                                  ,{<<"Route-Error-Message">>, <<"Incoming call barred">>}
                                                 ]),
    lager:debug("sending XML to ~s: ~s", [Node, XML]),
    case freeswitch:fetch_reply(Node, FetchId, 'chatplan', iolist_to_binary(XML), 3000) of
        'ok' -> lager:info("node ~s accepted route response for request ~s", [Node, FetchId]);
        {'error', Reason} -> lager:debug("node ~s rejected our route unauthz: ~p", [Node, Reason])
    end.

-spec reply_affirmative(atom(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
reply_affirmative(Node, FetchId, CallId, JObj) ->
    lager:info("received affirmative route response for request ~s", [FetchId]),
    {'ok', XML} = ecallmgr_fs_xml:route_resp_xml(JObj),
    lager:debug("sending XML to ~s: ~s", [Node, XML]),
    case freeswitch:fetch_reply(Node, FetchId, 'chatplan', iolist_to_binary(XML), 3000) of
        {'error', _Reason} -> lager:debug("node ~s rejected our route response: ~p", [Node, _Reason]);
        'ok' ->
            lager:info("node ~s accepted route response for request ~s", [Node, FetchId])
%            maybe_start_call_handling(Node, FetchId, CallId, JObj)
    end.

    

-spec route_req(ne_binary(), ne_binary(), wh_proplist(), atom()) -> wh_proplist().
route_req(CallId, FetchId, Props, Node) ->
    props:filter_undefined([{<<"Msg-ID">>, FetchId}
     ,{<<"Caller-ID-Name">>, props:get_first_defined([<<"variable_effective_caller_id_name">>
                                                      ,<<"Caller-Caller-ID-Name">>
                                                      ,<<"from_user">>
                                                     ], Props, <<"Unknown">>)}
     ,{<<"Caller-ID-Number">>, props:get_first_defined([<<"variable_effective_caller_id_number">>
                                                        ,<<"Caller-Caller-ID-Number">>
                                                       ,<<"from_user">>
                                                       ], Props, <<"0000000000">>)}
     ,{<<"From-Network-Addr">>, props:get_first_defined([<<"variable_sip_h_X-AUTH-IP">>
                                                         ,<<"variable_sip_received_ip">>
                                                        ], Props)}
     ,{<<"User-Agent">>, props:get_first_defined([<<"variable_sip_user_agent">>
                                                  ,<<"sip_user_agent">>
                                                 ], Props)}
     ,{<<"Message-ID">>, props:get_value(<<"Message-ID">>, Props), CallId}
     ,{<<"Call-ID">>, CallId}
     ,{<<"To">>,  props:get_value(<<"to">>, Props)}
     ,{<<"From">>,  props:get_value(<<"from">>, Props)}
     ,{<<"Request">>, props:get_value(<<"to">>, Props)}
     ,{<<"Body">>, props:get_value(<<"body">>, Props)}
     ,{<<"SIP-Request-Host">>, props:get_value(<<"variable_sip_req_host">>, Props)}
     ,{<<"Switch-Nodename">>, wh_util:to_binary(Node)}
     ,{<<"Switch-Hostname">>, props:get_value(<<"FreeSWITCH-Hostname">>, Props)}
     ,{<<"Custom-Channel-Vars">>, wh_json:from_list(message_ccvs(FetchId, Props))}
     ,{<<"Custom-SIP-Headers">>, wh_json:from_list(ecallmgr_util:custom_sip_headers(Props))}
     ,{<<"Resource-Type">>, <<"SMS">>}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ]).

-spec message_ccvs(ne_binary(), wh_proplist()) -> wh_proplist().
message_ccvs(FetchId, Props) ->
    props:filter_undefined(
      [{<<"Fetch-ID">>, FetchId}
       | ecallmgr_util:custom_channel_vars(Props)
      ]
     ).
