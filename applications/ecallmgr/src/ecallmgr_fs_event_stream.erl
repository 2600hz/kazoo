%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_event_stream).
-behaviour(gen_server).

-export([start_link/3]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-type bindings() :: atom() | [atom(),...] | ne_binary() | ne_binaries().

-record(state, {node :: atom()
               ,bindings :: bindings()
               ,subclasses :: bindings()
               ,ip :: inet:ip_address() | 'undefined'
               ,port :: inet:port_number() | 'undefined'
               ,socket :: inet:socket() | 'undefined'
               ,idle_alert = 'infinity' :: kz_timeout()
               ,switch_url :: api_ne_binary()
               ,switch_uri :: api_ne_binary()
               ,switch_info = 'false' :: boolean()
               }).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link(atom(), bindings(), bindings()) -> startlink_ret().
start_link(Node, Bindings, Subclasses) ->
    gen_server:start_link(?SERVER, [Node, Bindings, Subclasses], []).

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
-spec init([atom() | bindings()]) -> {'ok', state()} | {'stop', any()}.
init([Node, Bindings, Subclasses]) ->
    process_flag('trap_exit', 'true'),
    kz_util:put_callid(list_to_binary([kz_term:to_binary(Node), <<"-eventstream">>])),
    request_event_stream(#state{node=Node
                               ,bindings=Bindings
                               ,subclasses=Subclasses
                               ,idle_alert=idle_alert_timeout()
                               }).

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
-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
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
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast('connect', #state{ip=IP, port=Port, idle_alert=Timeout}=State) ->
    PacketType = ecallmgr_config:get_integer(<<"tcp_packet_type">>, 2),
    case gen_tcp:connect(IP, Port, [{'mode', 'binary'}
                                   ,{'packet', PacketType}
                                   ])
    of
        {'ok', Socket} ->
            lager:debug("opened event stream socket to ~p:~p for ~p"
                       ,[IP, Port, get_event_bindings(State)]),
            {'noreply', State#state{socket=Socket}, Timeout};
        {'error', Reason} ->
            {'stop', Reason, State}
    end;
handle_cast(_Msg, #state{socket='undefined'}=State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State};
handle_cast(_Msg, #state{idle_alert=Timeout}=State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State, Timeout}.

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
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info({'tcp', Socket, Data}, #state{socket=Socket
                                         ,node=Node
                                         ,switch_info='false'
                                         }=State) ->
    try ecallmgr_fs_node:sip_url(Node) of
        'undefined' ->
            lager:debug("no sip url available yet for ~s", [Node]),
            {'noreply', State, 'hibernate'};
        SwitchURL ->
            [_, SwitchURIHost] = binary:split(SwitchURL, <<"@">>),
            SwitchURI = <<"sip:", SwitchURIHost/binary>>,
            handle_info({'tcp', Socket, Data}, State#state{switch_uri=SwitchURI
                                                          ,switch_url=SwitchURL
                                                          ,switch_info='true'
                                                          })
    catch
        _E:_R ->
            lager:warning("failed to include switch_url/uri for node ~s : ~p : ~p", [Node, _E, _R]),
            {'noreply', State, 'hibernate'}
    end;
handle_info({'tcp', Socket, Data}, #state{socket=Socket
                                         ,node=Node
                                         ,idle_alert=Timeout
                                         ,switch_uri=SwitchURI
                                         ,switch_url=SwitchURL
                                         }=State) ->
    try binary_to_term(Data) of
        {'event', [UUID | Props]} when is_binary(UUID)
                                       orelse UUID =:= 'undefined' ->
            _ = kz_util:spawn(fun handle_fs_props/5, [UUID, Props, Node, SwitchURI, SwitchURL]),
            {'noreply', State, Timeout};
        _Else ->
            io:format("~p~n", [_Else]),
            {'noreply', State, Timeout}
    catch
        'error':'badarg' ->
            lager:warning("failed to decode packet from ~s (~p b) for ~p: ~p"
                         ,[Node, byte_size(Data), get_event_bindings(State), Data]),
            {'stop', 'decode_error', State}
    end;
handle_info({'tcp_closed', Socket}, #state{socket=Socket, node=Node}=State) ->
    lager:info("event stream for ~p on node ~p closed"
              ,[get_event_bindings(State), Node]
              ),
    timer:sleep(3 * ?MILLISECONDS_IN_SECOND),
    {'stop', 'tcp_close', State#state{socket='undefined'}};
handle_info({'tcp_error', Socket, _Reason}, #state{socket=Socket}=State) ->
    lager:warning("event stream tcp error: ~p", [_Reason]),
    gen_tcp:close(Socket),
    timer:sleep(3 * ?MILLISECONDS_IN_SECOND),
    {'stop', 'tcp_error', State#state{socket='undefined'}};
handle_info('timeout', #state{node=Node, idle_alert=Timeout}=State) ->
    lager:warning("event stream for ~p on node ~p is unexpectedly idle",
                  [get_event_bindings(State), Node]
                 ),
    {'noreply', State, Timeout};
handle_info({'EXIT', _, 'noconnection'}, State) ->
    {stop, {'shutdown', 'noconnection'}, State};
handle_info({'EXIT', _, Reason}, State) ->
    {stop, Reason, State};
handle_info(_Msg, #state{socket='undefined'}=State) ->
    lager:debug("unhandled message: ~p", [_Msg]),
    {'noreply', State};
handle_info(_Msg, #state{idle_alert=Timeout}=State) ->
    lager:debug("unhandled message: ~p", [_Msg]),
    {'noreply', State, Timeout}.

-spec handle_fs_props(api_binary(), kzd_freeswitch:data(), atom(), ne_binary(), ne_binary()) -> pid().
handle_fs_props(UUID, Props, Node, SwitchURI, SwitchURL) ->
    kz_util:put_callid(UUID),
    EventName = props:get_value(<<"Event-Subclass">>, Props, props:get_value(<<"Event-Name">>, Props)),
    EventProps = props:filter_undefined([{<<"Switch-URL">>, SwitchURL}
                                        ,{<<"Switch-URI">>, SwitchURI}
                                        ,{<<"Switch-Nodename">>, kz_term:to_binary(Node)}
                                        ]
                                       )
        ++ Props ,
    process_stream(EventName, UUID, EventProps, Node).

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
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{socket='undefined', node=Node}=State) ->
    lager:debug("event stream for ~p on node ~p terminating: ~p"
               ,[get_event_bindings(State), Node, _Reason]
               );
terminate(_Reason, #state{socket=Socket, node=Node}=State) ->
    gen_tcp:close(Socket),
    lager:debug("event stream for ~p on node ~p terminating: ~p"
               ,[get_event_bindings(State), Node, _Reason]
               ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec request_event_stream(state()) -> {'ok', state()} | {'stop', any()}.
request_event_stream(#state{node=Node}=State) ->
    Bindings = get_event_bindings(State),
    case maybe_bind(Node, Bindings) of
        {'ok', {IP, Port}} ->
            {'ok', IPAddress} = inet_parse:address(IP),
            gen_server:cast(self(), 'connect'),
            kz_util:put_callid(list_to_binary([kz_term:to_binary(Node)
                                              ,$-, kz_term:to_binary(IP)
                                              ,$:, kz_term:to_binary(Port)
                                              ])),
            {'ok', State#state{ip=IPAddress, port=kz_term:to_integer(Port)}};
        {'EXIT', ExitReason} ->
            {'stop', {'shutdown', ExitReason}};
        {'error', ErrorReason} ->
            lager:warning("unable to establish event stream to ~p for ~p: ~p", [Node, Bindings, ErrorReason]),
            {'stop', ErrorReason}
    end.

-spec get_event_bindings(state()) -> atoms().
get_event_bindings(State) ->
    get_event_bindings(State, []).

-spec get_event_bindings(state(), atoms()) -> atoms().
get_event_bindings(#state{bindings='undefined'
                         ,subclasses='undefined'
                         ,idle_alert='infinity'
                         }, Acc) ->
    Acc;
get_event_bindings(#state{bindings='undefined'
                         ,subclasses='undefined'
                         }, Acc) ->
    ['HEARTBEAT' | Acc];
get_event_bindings(#state{subclasses=Subclasses}=State, Acc) when is_list(Subclasses) ->
    get_event_bindings(State#state{subclasses='undefined'}
                      ,[kz_term:to_atom(Subclass, 'true') || Subclass <- Subclasses] ++ Acc
                      );
get_event_bindings(#state{subclasses=Subclass}=State, Acc)
  when is_atom(Subclass),
       Subclass =/= 'undefined' ->
    get_event_bindings(State#state{subclasses='undefined'}, [Subclass | Acc]);
get_event_bindings(#state{subclasses=Subclass}=State, Acc) when is_binary(Subclass) ->
    get_event_bindings(State#state{subclasses='undefined'}
                      ,[kz_term:to_atom(Subclass, 'true') | Acc]
                      );
get_event_bindings(#state{bindings=Bindings}=State, Acc) when is_list(Bindings) ->
    get_event_bindings(State#state{bindings='undefined'}
                      ,[kz_term:to_atom(Binding, 'true') || Binding <- Bindings] ++ Acc
                      );
get_event_bindings(#state{bindings=Binding}=State, Acc)
  when is_atom(Binding),
       Binding =/= 'undefined' ->
    get_event_bindings(State#state{bindings='undefined'}, [Binding | Acc]);
get_event_bindings(#state{bindings=Binding}=State, Acc) when is_binary(Binding) ->
    get_event_bindings(State#state{bindings='undefined'}
                      ,[kz_term:to_atom(Binding, 'true') | Acc]
                      ).

-spec maybe_bind(atom(), atoms()) ->
                        {'ok', {text(), inet:port_number()}} |
                        {'error', any()} |
                        {'EXIT', any()}.
-spec maybe_bind(atom(), atoms(), non_neg_integer()) ->
                        {'ok', {text(), inet:port_number()}} |
                        {'error', any()} |
                        {'EXIT', any()}.
maybe_bind(Node, Bindings) ->
    maybe_bind(Node, Bindings, 0).

maybe_bind(Node, Bindings, 2) ->
    case catch gen_server:call({'mod_kazoo', Node}, {'event', Bindings}, 2 * ?MILLISECONDS_IN_SECOND) of
        {'ok', {_IP, _Port}}=OK -> OK;
        {'EXIT', {'timeout',_}} -> {'error', 'timeout'};
        {'EXIT', _} = Exit -> Exit;
        {'error', _Reason}=E -> E
    end;
maybe_bind(Node, Bindings, Attempts) ->
    case catch gen_server:call({'mod_kazoo', Node}, {'event', Bindings}, 2 * ?MILLISECONDS_IN_SECOND) of
        {'ok', {_IP, _Port}}=OK -> OK;
        {'EXIT', {'timeout',_}} ->
            lager:debug("timeout on attempt ~b to bind: ~p", [Attempts, Bindings]),
            maybe_bind(Node, Bindings, Attempts+1);
        {'error', _Reason} ->
            lager:debug("failed on attempt ~b to bind: ~p", [Attempts, _Reason]),
            maybe_bind(Node, Bindings, Attempts+1)
    end.

-spec process_stream(ne_binary(), api_binary(), kzd_freeswitch:data(), atom()) -> any().
process_stream(<<"CHANNEL_CREATE">> = EventName, UUID, EventProps, Node) ->
    Props = ecallmgr_fs_loopback:filter(Node, UUID, EventProps, 'true'),
    process_event(EventName, UUID, Props, Node),
    maybe_send_event(EventName, UUID, Props, Node);
process_stream(<<"sofia::transferor">> = EventName, UUID, Props, Node) ->
    case props:get_value(<<"variable_refer_uuid">>, Props) of
        'undefined' -> 'ok';
        ReferUUID ->
            lager:debug("found refer uuid ~s for interaction caching", [ReferUUID]),
            {'ok', Channel} = ecallmgr_fs_channel:fetch(UUID),
            CDR = kz_json:get_value(<<"interaction_id">>, Channel),
            kz_cache:store_local(?ECALLMGR_INTERACTION_CACHE, ReferUUID, CDR),
            lager:debug("caching interaction id ~s for callid ~s", [CDR, ReferUUID]),
            ecallmgr_fs_command:set(Node, ReferUUID, [{<<?CALL_INTERACTION_ID>>, CDR}])
    end,
    maybe_send_event(EventName, UUID, Props, Node),
    process_event(EventName, UUID, Props, Node);
process_stream(<<"sofia::intercepted">> = EventName, UUID, Props, Node) ->
    InterceptedBy = props:get_value(<<"intercepted_by">>, Props),
    case ecallmgr_fs_channel:fetch(UUID, 'record') of
        {'ok', #channel{interaction_id=InterAction
                       ,direction=Direction
                       }
        } ->
            lager:debug("sofia::intercepted: channel ~s Intercepted by ~s", [UUID, InterceptedBy]),
            Vars = [{<<"Application-Logical-Direction">>, Direction}
                   ,{<<?CALL_INTERACTION_ID>>, InterAction}
                   ],
            ecallmgr_fs_command:set(Node, InterceptedBy, Vars);
        _ -> 'ok'
    end,
    ChannelUUID = props:get_value(<<"Channel-Call-UUID">>, Props),
    Updates = props:filter_undefined(
                [{<<"Caller-Callee-ID-Name">>, props:get_value(<<"Caller-Callee-ID-Name">>, Props)}
                ,{<<"Caller-Callee-ID-Number">>, props:get_value(<<"Caller-Callee-ID-Number">>, Props)}
                ]),
    ecallmgr_fs_command:set(Node, ChannelUUID, Updates),
    maybe_send_event(EventName, UUID, Props, Node),
    process_event(EventName, UUID, Props, Node);
process_stream(<<"CHANNEL_HOLD">> = EventName, UUID, Props, Node) ->
    gproc:send({'p', 'l', ?FS_EVENT_REG_MSG(Node, EventName)}, {'event', [UUID | Props]});
process_stream(<<"CHANNEL_UNHOLD">> = EventName, UUID, Props, Node) ->
    gproc:send({'p', 'l', ?FS_EVENT_REG_MSG(Node, EventName)}, {'event', [UUID | Props]});
process_stream(EventName, UUID, EventProps, Node) ->
    maybe_send_event(EventName, UUID, EventProps, Node),
    process_event(EventName, UUID, EventProps, Node).

-spec process_event(ne_binary(), api_binary(), kzd_freeswitch:data(), atom()) -> any().
process_event(<<"CHANNEL_CREATE">>, UUID, Props, Node) ->
    _ = ecallmgr_fs_channel:new(Props, Node),
    lager:debug("channel added to cache"),
    _ = ecallmgr_fs_channel:maybe_update_interaction_id(Props, Node),
    maybe_start_event_listener(Node, UUID);
process_event(?CHANNEL_MOVE_RELEASED_EVENT_BIN, _, Props, Node) ->
    UUID = props:get_value(<<"old_node_channel_uuid">>, Props),
    gproc:send({'p', 'l', ?CHANNEL_MOVE_REG(Node, UUID)}
              ,?CHANNEL_MOVE_RELEASED_MSG(Node, UUID, Props)
              );
process_event(?CHANNEL_MOVE_COMPLETE_EVENT_BIN, _, Props, Node) ->
    UUID = props:get_value(<<"old_node_channel_uuid">>, Props),
    gproc:send({'p', 'l', ?CHANNEL_MOVE_REG(Node, UUID)}
              ,?CHANNEL_MOVE_COMPLETE_MSG(Node, UUID, Props)
              );
process_event(<<"sofia::register">>, _UUID, Props, Node) ->
    gproc:send({'p', 'l', ?REGISTER_SUCCESS_REG}, ?REGISTER_SUCCESS_MSG(Node, Props));
process_event(<<"loopback::bowout">>, _UUID, Props, Node) ->
    ResigningUUID = props:get_value(?RESIGNING_UUID, Props),
    kz_util:put_callid(ResigningUUID),
    lager:debug("bowout detected on ~s, transferring to ~s"
               ,[ResigningUUID, props:get_value(?ACQUIRED_UUID, Props)]
               ),
    gproc:send({'p', 'l', ?LOOPBACK_BOWOUT_REG(ResigningUUID)}, ?LOOPBACK_BOWOUT_MSG(Node, Props));
process_event(_, _, _, _) -> 'ok'.

-spec maybe_send_event(ne_binary(), api_binary(), kzd_freeswitch:data(), atom()) -> any().
maybe_send_event(<<"HEARTBEAT">>, _UUID, _Props, _Node) -> 'ok';
maybe_send_event(<<"CHANNEL_BRIDGE">>=EventName, UUID, Props, Node) ->
    BridgeID = props:get_value(<<"variable_bridge_uuid">>, Props),
    DialPlan = props:get_value(<<"Caller-Dialplan">>, Props),
    Direction = props:get_value(?GET_CCV(<<"Application-Logical-Direction">>), Props),
    App = props:get_value(<<"variable_current_application">>, Props),
    Destination = props:get_value(<<"Caller-Destination-Number">>, Props),

    _ = case {BridgeID, Direction, DialPlan, App, Destination} of
            {'undefined', _, _, _, _} -> 'ok';
            {BridgeID, <<"outbound">>, <<"inline">>, <<"intercept">>, 'undefined'} ->
                ALeg = props:get_value(<<"Bridge-A-Unique-ID">>, Props),
                BLeg = props:get_value(<<"Bridge-B-Unique-ID">>, Props),
                lager:debug("channel bridge intercept: UUID: ~s, A : ~s, B : ~s", [UUID, ALeg, BLeg]),
                case ecallmgr_fs_channel:channel_data(Node, BLeg) of
                    {'ok', CData} ->
                        Data = props:filter_undefined(
                                 [{<<"Event-Subclass">>, props:get_value(<<"Event-Subclass">>, Props)}
                                 ,{<<"Event-Name">>, props:get_value(<<"Event-Name">>, Props)}
                                 ]) ++ CData,
                        gproc:send({'p', 'l', ?FS_EVENT_REG_MSG(Node, EventName)}, {'event', [BLeg | Data]}),
                        gproc:send({'p', 'l', ?FS_CALL_EVENT_REG_MSG(Node, BLeg)}, {'event', [BLeg | Data]});
                    _ ->
                        lager:debug("channel bridge intercept: failed to get channel data for ~s", [BLeg])
                end;
            _Else -> 'ok'
        end,
    gproc:send({'p', 'l', ?FS_EVENT_REG_MSG(Node, EventName)}, {'event', [UUID | Props]}),
    maybe_send_call_event(UUID, EventName, Props, Node);
maybe_send_event(<<"loopback::bowout">> = EventName, _UUID, Props, Node) ->
    ResigningUUID = props:get_value(?RESIGNING_UUID, Props),
    _AcquiringUUID = props:get_value(?ACQUIRED_UUID, Props),
    kz_util:put_callid(ResigningUUID),

    lager:debug("bowout for '~s', resigning ~s acquiring ~s", [_UUID, ResigningUUID, _AcquiringUUID]),

    send_event(EventName, ResigningUUID, Props, Node);
maybe_send_event(<<"CHANNEL_DESTROY">> = EventName, UUID, Props, Node) ->
    case ecallmgr_fs_channel:node(UUID) of
        {'ok', OtherNode} when Node =/= OtherNode  ->
            lager:info("dropping channel destroy from ~s (expected node: ~s)", [Node, OtherNode]);
        _Else ->
            gproc:send({'p', 'l', ?FS_EVENT_REG_MSG(Node, EventName)}, {'event', [UUID | Props]}),
            maybe_send_call_event(UUID, EventName, Props, Node)
    end;
maybe_send_event(EventName, UUID, Props, Node) ->
    case kz_term:is_true(props:get_value(<<"variable_channel_is_moving">>, Props)) of
        'true' -> 'ok';
        'false' ->
            send_event(EventName, UUID, Props, Node)
    end.

send_event(EventName, UUID, Props, Node) ->
    gproc:send({'p', 'l', ?FS_EVENT_REG_MSG(Node, EventName)}, {'event', [UUID | Props]}),
    maybe_send_call_event(UUID, EventName, Props, Node).

-spec maybe_send_call_event(api_binary(), ne_binary(), kz_proplist(), atom()) -> any().
maybe_send_call_event('undefined', _, _, _) -> 'ok';
maybe_send_call_event(CallId, EventName, Props, Node) ->
    gproc:send({'p', 'l', ?FS_CALL_EVENT_MSG(Node, EventName, CallId)}, {'event', EventName, [CallId | Props]}),
    gproc:send({'p', 'l', ?FS_CALL_EVENT_REG_MSG(Node, CallId)}, {'event', [CallId | Props]}).

-spec maybe_start_event_listener(atom(), ne_binary()) -> 'ok' | sup_startchild_ret().
maybe_start_event_listener(Node, UUID) ->
    case kz_cache:fetch_local(?ECALLMGR_UTIL_CACHE, {UUID, 'start_listener'}) of
        {'ok', 'true'} -> ecallmgr_call_sup:start_event_process(Node, UUID);
        _E -> 'ok'
    end.

-spec idle_alert_timeout() -> kz_timeout().
idle_alert_timeout() ->
    case ecallmgr_config:get_integer(<<"event_stream_idle_alert">>, 0) of
        Timeout when Timeout =< 30 -> 'infinity';
        Else -> Else * ?MILLISECONDS_IN_SECOND
    end.
