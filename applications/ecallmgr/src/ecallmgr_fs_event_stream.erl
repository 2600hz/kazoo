%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_event_stream).

-behaviour(gen_server).

-export([start_link/3]).
-export([process_event/2]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("ecallmgr.hrl").

-type bindings() :: atom() | [atom(),...] | ne_binary() | ne_binaries().

-record(state, {node :: atom()
                ,bindings :: bindings()
                ,subclasses :: bindings()
                ,ip :: inet:ip_address()
                ,port :: inet:port_number()
                ,socket :: inet:socket()
               }).
-type state() :: #state{}.

-define(PUBLISH_CHANNEL_STATE, <<"publish_channel_state">>).

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
-spec start_link(atom(), bindings(), bindings()) -> startlink_ret().
start_link(Node, Bindings, Subclasses) ->
    gen_server:start_link(?MODULE, [Node, Bindings, Subclasses], []).

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
init([Node, Bindings, Subclasses]) ->
    put('callid', list_to_binary([wh_util:to_binary(Node)
                                  ,<<"-eventstream">>
                                 ])),
    gen_server:cast(self(), 'request_event_stream'),
    {'ok', #state{node=Node
                  ,bindings=Bindings
                  ,subclasses=Subclasses
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
handle_cast('request_event_stream', #state{node=Node}=State) ->
    Bindings = get_event_bindings(State),
    case maybe_bind(Node, Bindings) of
        {'ok', {IP, Port}} ->
            {'ok', IPAddress} = inet_parse:address(IP),
            gen_server:cast(self(), 'connect'),
            put('callid', list_to_binary([wh_util:to_binary(Node)
                                         ,$-, wh_util:to_binary(IP)
                                         ,$:, wh_util:to_binary(Port)
                                         ])),
            {'noreply', State#state{ip=IPAddress, port=wh_util:to_integer(Port)}};
        {'error', Reason} ->
            lager:warning("unable to establish event stream to ~p for ~p: ~p", [Node, Bindings, Reason]),
            {'stop', Reason, State}
    end;
handle_cast('connect', #state{ip=IP, port=Port}=State) ->
    case gen_tcp:connect(IP, Port, [{'mode', 'binary'}, {'packet', 2}]) of
        {'ok', Socket} ->
            lager:debug("opened event stream socket to ~p:~p for ~p"
                        ,[IP, Port, get_event_bindings(State)]
                       ),
            {'noreply', State#state{socket=Socket}};
        {'error', Reason} ->
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
handle_info({'tcp', Socket, Data}, #state{socket=Socket, node=Node}=State) ->
    _ = spawn(?MODULE, 'process_event', [Data, Node]),
    {'noreply', State};
handle_info({'tcp_closed', Socket}, #state{socket=Socket, node=Node}=State) ->
    lager:info("event stream for ~p on node ~p closed"
                , [get_event_bindings(State), Node]),
    {'stop', 'normal', State#state{socket='undefined'}};
handle_info({'tcp_error', Socket, _Reason}, #state{socket=Socket}=State) ->
    lager:warning("event stream tcp error: ~p", [_Reason]),
    gen_tcp:close(Socket),
    gen_server:cast(self(), 'request_event_stream'),
    {'noreply', State#state{socket='undefined'}};
handle_info(_Msg, State) ->
    lager:debug("unhandled message: ~p", [_Msg]),
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
terminate(_Reason, #state{socket='undefined', node=Node}=State) ->
    lager:debug("event stream for ~p on node ~p terminating: ~p"
                ,[get_event_bindings(State), Node, _Reason]);
terminate(_Reason, #state{socket=Socket, node=Node}=State) ->
    gen_tcp:close(Socket),
    lager:debug("event stream for ~p on node ~p terminating: ~p"
                ,[get_event_bindings(State), Node, _Reason]).

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
-spec get_event_bindings(state()) -> atoms().
get_event_bindings(State) ->
    get_event_bindings(State, []).

-spec get_event_bindings(state(), atoms()) -> atoms().
get_event_bindings(#state{bindings='undefined', subclasses='undefined'}, Acc) -> Acc;
get_event_bindings(#state{subclasses=Subclasses}=State, Acc) when is_list(Subclasses) ->
    get_event_bindings(State#state{subclasses='undefined'},
                       [wh_util:to_atom(Subclass, 'true') || Subclass <- Subclasses] ++ Acc);
get_event_bindings(#state{subclasses=Subclass}=State, Acc)
  when is_atom(Subclass), Subclass =/= 'undefined' ->
    get_event_bindings(State#state{subclasses='undefined'}, [Subclass | Acc]);
get_event_bindings(#state{subclasses=Subclass}=State, Acc) when is_binary(Subclass) ->
    get_event_bindings(State#state{subclasses='undefined'}
                      ,[wh_util:to_atom(Subclass, 'true') | Acc]);
get_event_bindings(#state{bindings=Bindings}=State, Acc) when is_list(Bindings) ->
    get_event_bindings(State#state{bindings='undefined'},
                       [wh_util:to_atom(Binding, 'true') || Binding <- Bindings] ++ Acc);
get_event_bindings(#state{bindings=Binding}=State, Acc)
  when is_atom(Binding), Binding =/= 'undefined' ->
    get_event_bindings(State#state{bindings='undefined'}, [Binding | Acc]);
get_event_bindings(#state{bindings=Binding}=State, Acc) when is_binary(Binding) ->
    get_event_bindings(State#state{bindings='undefined'}
                      ,[wh_util:to_atom(Binding, 'true') | Acc]).

-spec maybe_bind(atom(), atoms()) -> {'ok', {ne_binary(), ne_binary()}} |
                                     {'error', _}.
maybe_bind(Node, Bindings) ->
    maybe_bind(Node, Bindings, 0).
maybe_bind(Node, Bindings, 2) ->
    case gen_server:call({'mod_kazoo', Node}, {'event', Bindings}, 2000) of
        {'ok', {_IP, _Port}}=OK -> OK;
        {'error', _Reason}=E -> E
    end;
maybe_bind(Node, Bindings, Attempts) ->
    case gen_server:call({'mod_kazoo', Node}, {'event', Bindings}, 2000) of
        {'ok', {_IP, _Port}}=OK -> OK;
        {'error', _Reason} ->
            lager:debug("failed on attempt ~b to bind: ~p", [Attempts, _Reason]),
            maybe_bind(Node, Bindings, Attempts+1)
    end.

-spec process_event(ne_binary(), atom()) -> any().
process_event(Data, Node) ->
    case binary_to_term(Data) of
        {'event', [UUID | Props]} when is_binary(UUID) orelse UUID =:= 'undefined' ->
            wh_util:put_callid(UUID),

            EventName = props:get_value(<<"Event-Subclass">>, Props, props:get_value(<<"Event-Name">>, Props)),
            maybe_send_event(EventName, UUID, Props, Node),
            process_event(EventName, UUID, Props, Node);
         _Else ->
            'ok'
    end.

-spec process_event(ne_binary(), api_binary(), wh_proplist(), atom()) -> any().
process_event(<<"CHANNEL_CREATE">>, UUID, _Props, Node) ->
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
process_event(_, _, _, _) -> 'ok'.

-spec maybe_send_event(ne_binary(), api_binary(), wh_proplist(), atom()) -> any().
maybe_send_event(<<"CHANNEL_BRIDGE">>=EventName, UUID, Props, Node) ->
    BridgeID = props:get_value(<<"variable_bridge_uuid">>, Props),
    DialPlan = props:get_value(<<"Caller-Dialplan">>, Props),
    Direction = props:get_value(<<"Call-Direction">>, Props),
    App = props:get_value(<<"variable_current_application">>, Props),
    Destination = props:get_value(<<"Caller-Destination-Number">>, Props),

    case {BridgeID, Direction, DialPlan, App, Destination} of
        {'undefined', _, _, _, _} ->
            gproc:send({'p', 'l', ?FS_EVENT_REG_MSG(Node, EventName)}, {'event', [UUID | Props]}),
            maybe_send_call_event(UUID, Props, Node);
        {BridgeID, <<"inbound">>, <<"inline">>, <<"intercept">>, 'undefined'} ->
            SwappedProps = ecallmgr_call_events:swap_call_legs(Props),
            gproc:send({'p', 'l', ?FS_EVENT_REG_MSG(Node, EventName)}, {'event', [BridgeID | SwappedProps]}),
            maybe_send_call_event(BridgeID, SwappedProps, Node);
        _Else ->
            gproc:send({'p', 'l', ?FS_EVENT_REG_MSG(Node, EventName)}, {'event', [UUID | Props]}),
            maybe_send_call_event(UUID, Props, Node)
    end;
maybe_send_event(EventName, UUID, Props, Node) ->
    case wh_util:is_true(props:get_value(<<"variable_channel_is_moving">>, Props)) of
        'true' -> 'ok';
        'false' ->
            gproc:send({'p', 'l', ?FS_EVENT_REG_MSG(Node, EventName)}, {'event', [UUID | Props]}),
            maybe_send_call_event(UUID, Props, Node)
    end.

-spec maybe_send_call_event(api_binary(), wh_proplist(), atom()) -> any().
maybe_send_call_event('undefined', _, _) -> 'ok';
maybe_send_call_event(CallId, Props, Node) ->
    gproc:send({'p', 'l', ?FS_CALL_EVENT_REG_MSG(Node, CallId)}, {'event', [CallId | Props]}).

-spec maybe_start_event_listener(atom(), ne_binary()) -> 'ok' | sup_startchild_ret().
maybe_start_event_listener(Node, UUID) ->
    case wh_cache:fetch_local(?ECALLMGR_UTIL_CACHE, {UUID, 'start_listener'}) of
        {'ok', 'true'} -> ecallmgr_call_sup:start_event_process(Node, UUID);
        _E -> 'ok'
    end.
