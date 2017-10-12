%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_event_stream).
-behaviour(gen_server).

-export([start_link/2]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-type bindings() :: atom() | [atom(),...] | kz_term:ne_binary() | kz_term:ne_binaries().

-record(state, {node :: atom()
               ,bindings :: bindings()
               ,ip :: inet:ip_address() | 'undefined'
               ,port :: inet:port_number() | 'undefined'
               ,socket :: inet:socket() | 'undefined'
               ,idle_alert = 'infinity' :: timeout()
               ,channel_mon :: api_reference()
               }).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server
%%------------------------------------------------------------------------------
-spec start_link(atom(), bindings()) -> kz_types:startlink_ret().
start_link(Node, Bindings) ->
    gen_server:start_link(?SERVER, [Node, Bindings], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([atom() | bindings()]) -> {'ok', state()} | {'stop', any()}.
init([Node, Bindings]) ->
    process_flag('trap_exit', 'true'),
    kz_util:put_callid(list_to_binary([kz_term:to_binary(Node), <<"-eventstream">>])),
    request_event_stream(#state{node=Node
                               ,bindings=Bindings
                               ,idle_alert=idle_alert_timeout()
                               }).

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
handle_cast('connect', #state{ip=IP, port=Port, idle_alert=Timeout}=State) ->
    PacketType = kapps_config:get_integer(?APP_NAME, <<"tcp_packet_type">>, 2),
    case gen_tcp:connect(IP, Port, [{'mode', 'binary'}
                                   ,{'packet', PacketType}
                                   ])
    of
        {'ok', Socket} ->
            _ = kz_amqp_channel:requisition(),
            lager:debug("opened event stream socket to ~p:~p for ~p"
                       ,[IP, Port, get_event_bindings(State)]),
            {'noreply', State#state{socket=Socket}, Timeout};
        {'error', Reason} ->
            {'stop', Reason, State}
    end;
handle_cast({'kz_amqp_assignment', {'new_channel', _, Channel}}, State) ->
    lager:debug("channel acquired ~p", [Channel]),
    _ = kz_amqp_channel:consumer_channel(Channel),
    {'noreply', State#state{channel_mon = erlang:monitor(process, Channel)}};
handle_cast(_Msg, #state{socket='undefined'}=State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State};
handle_cast(_Msg, #state{idle_alert=Timeout}=State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State, Timeout}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'DOWN', _MonitorRef, _Type, _Object, _Info}, State)->
    kz_amqp_channel:requisition(),
    {'noreply', State};    
handle_info({'tcp', Socket, Data}, #state{socket=Socket
                                         ,node=Node
                                         ,idle_alert=Timeout
                                         }=State) ->
    try binary_to_term(Data) of
        {'event', JObj} ->
            Channel = kz_amqp_channel:consumer_channel(),
            _ = kz_util:spawn(fun handle_fs_event/3, [Node, JObj, Channel]),
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

<<<<<<< Upstream, based on 2600hz/master
-spec handle_fs_props(kz_term:api_binary(), kzd_freeswitch:data(), atom(), kz_term:ne_binary(), kz_term:ne_binary()) -> pid().
handle_fs_props(UUID, Props, Node, SwitchURI, SwitchURL) ->
    kz_util:put_callid(UUID),
    EventName = props:get_value(<<"Event-Subclass">>, Props, props:get_value(<<"Event-Name">>, Props)),
    EventProps = props:filter_undefined([{<<"Switch-URL">>, SwitchURL}
                                        ,{<<"Switch-URI">>, SwitchURI}
                                        ,{<<"Switch-Nodename">>, kz_term:to_binary(Node)}
                                        ]
                                       )
        ++ Props ,
    ecallmgr_events:event(EventName, UUID, EventProps, Node).

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
=======
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
>>>>>>> 2ce04e3 plugable event stream processing
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{socket='undefined', node=Node}=State) ->
    catch(kz_amqp_channel:release()),
    lager:debug("event stream for ~p on node ~p terminating: ~p"
               ,[get_event_bindings(State), Node, _Reason]
               );
terminate(_Reason, #state{socket=Socket, node=Node}=State) ->
    gen_tcp:close(Socket),
    catch(kz_amqp_channel:release()),
    lager:debug("event stream for ~p on node ~p terminating: ~p"
               ,[get_event_bindings(State), Node, _Reason]
               ).

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

-spec get_event_bindings(state()) -> kz_term:atoms().
get_event_bindings(State) ->
    get_event_bindings(State, []).

-spec get_event_bindings(state(), kz_term:atoms()) -> kz_term:atoms().
get_event_bindings(#state{bindings='undefined'
                         ,idle_alert='infinity'
                         }, Acc) ->
    Acc;
get_event_bindings(#state{bindings='undefined'
                         }, Acc) ->
    ['HEARTBEAT' | Acc];
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

-spec maybe_bind(atom(), kz_term:atoms()) ->
                        {'ok', {kz_term:text(), inet:port_number()}} |
                        {'error', any()} |
                        {'EXIT', any()}.
maybe_bind(Node, Bindings) ->
    maybe_bind(Node, Bindings, 0).

-spec maybe_bind(atom(), kz_term:atoms(), non_neg_integer()) ->
                        {'ok', {kz_term:text(), inet:port_number()}} |
                        {'error', any()} |
                        {'EXIT', any()}.
maybe_bind(Node, Bindings, 2) ->
    case catch gen_server:call({'mod_kazoo', Node}, {'event', Bindings}, 2 * ?MILLISECONDS_IN_SECOND) of
        {'ok', {_IP, _Port}}=OK -> OK;
        {'EXIT', {'timeout',_}} -> {'error', 'timeout'};
        {'EXIT', {{'nodedown',_}, _}} -> {'error', 'nodedown'};
        {'EXIT', _} = Exit -> Exit;
        {'error', _Reason}=E -> E
    end;
maybe_bind(Node, Bindings, Attempts) ->
    case catch gen_server:call({'mod_kazoo', Node}, {'event', Bindings}, 2 * ?MILLISECONDS_IN_SECOND) of
        {'ok', {_IP, _Port}}=OK -> OK;
        {'EXIT', {'timeout',_}} ->
            lager:debug("timeout on attempt ~b to bind: ~p", [Attempts, Bindings]),
            maybe_bind(Node, Bindings, Attempts+1);
        {'EXIT', {{'nodedown',_}, _}} -> {'error', 'nodedown'};
        {'error', _Reason} ->
            lager:debug("failed on attempt ~b to bind: ~p", [Attempts, _Reason]),
            maybe_bind(Node, Bindings, Attempts+1)
    end.

-spec idle_alert_timeout() -> timeout().
idle_alert_timeout() ->
    case kapps_config:get_integer(?APP_NAME, <<"event_stream_idle_alert">>, 0) of
        Timeout when Timeout =< 30 -> 'infinity';
        Else -> Else * ?MILLISECONDS_IN_SECOND
    end.

-spec handle_fs_event(atom(), kz_json:object(), pid()) -> any().
handle_fs_event(Node, JObj, Channel) ->
    kz_amqp_channel:consumer_channel(Channel),
    kz_util:put_callid(JObj),
    case kz_api:event_name(JObj) of
        'undefined' -> lager:debug_unsafe("received unknown fs event : ~s", [kz_json:encode(JObj, ['pretty'])]);
        _ -> lager:debug_unsafe("received fs ~s : ~s", [kz_api:event_category(JObj), kz_api:event_name(JObj)])
    end,
    UUID = kz_api:call_id(JObj),
    Category = kz_api:event_category(JObj),
    Event = kz_api:event_name(JObj),
    run_bindings(Node, UUID, Category, Event, JObj).

run_bindings(Node, UUID, Category, Event, JObj) ->
    Stages = [fun run_event/5
             ,fun run_process/5
             ,fun run_notify/5
             ,fun run_publish/5
             ],
    Fun = fun(StageFun) -> StageFun(Node, UUID, Category, Event, JObj) end,
    lists:foreach(Fun, Stages).

run_event(Node, UUID, Category, Event, JObj) ->
    Routing = create_routing(<<"event">>, Category, Event),
    kazoo_bindings:map(Routing, {Node, UUID, Category, Event, JObj}).

run_process(Node, UUID, Category, Event, JObj) ->
    Routing = create_routing(<<"process">>, Category, Event),
    kazoo_bindings:map(Routing, {Node, UUID, Category, Event, JObj}).

run_notify(Node, UUID, Category, Event, JObj) ->
    Routing = create_routing(<<"notify">>, Category, Event),
    kazoo_bindings:map(Routing, {Node, UUID, Category, Event, JObj}).

run_publish(Node, UUID, Category, Event, JObj) ->
    Routing = create_routing(<<"publish">>, Category, Event),
    kazoo_bindings:map(Routing, {Node, UUID, Category, Event, JObj}).

create_routing(Name, undefined, Event) ->
    create_routing(Name, <<"invalid">>, Event);
create_routing(Name, Category, Event) ->
    <<"event_stream.", Name/binary, ".", Category/binary, ".", Event/binary>>.
