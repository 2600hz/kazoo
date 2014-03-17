%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%% Blackhole resource
%%% @end
%%% @contributors
%%%   Ben Wann
%%%-------------------------------------------------------------------
-module(blackhole_resource).

-behavior(gen_server).

-include("blackhole.hrl").

-define(MOD_CONFIG_CAT, <<(?BLACKHOLE_CONFIG_CAT)/binary>>).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
         ,status/1
        ]).

-export([connect_socket/2, disconnect_socket/2]).
-export([add_listener_to_context/4, remove_listener_from_context/4]).
-export([handle_message/3, handle_event/4]).

-record(state, {}).

-type state() :: #state{}.

-define(SERVER, ?MODULE).

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
start_link() ->    
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).

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
init([]) ->
    lager:debug("blackhole_dispatcher init"),
    gen_server:cast(self(), 'init_dispatcher'),
        {'ok', #state{}}.


-spec handle_message(wh_json:object(), ne_binary(), pid()) -> 'ok'.
handle_message(Message, SessionId, SessionPid) ->
    gen_server:cast({'handle_message', {Message, SessionId, SessionPid}}).

-spec handle_event(api_binary(), wh_json:object(), ne_binary(), pid()) -> 'ok'.
handle_event(Event, Data, SessionId, SessionPid) ->
    gen_server:cast({'handle_event', {Event, Data, SessionId, SessionPid}}).
    
-spec connect_socket(api_binary(), pid()) -> 'ok'.
connect_socket(SessionId, SessionPid) ->
    gen_server:cast(self(), {'connect_socket', SessionId, SessionPid}).

-spec disconnect_socket(api_binary(), pid()) -> 'ok'.
disconnect_socket(SessionId, SessionPid) ->
    gen_server:cast(self(), {'disconnect_socket', SessionId, SessionPid}).

-spec add_listener_to_context(pid(), ne_binary(), ne_binary(), pid()) -> 'ok'.
add_listener_to_context(ListenerPid, ListenerModule, SessionId, SessionPid)->
    gen_server:cast(self(), {'add_listener_to_session'
                            ,{ListenerPid, ListenerModule}
                            ,SessionId
                            ,SessionPid}).

-spec remove_listener_from_context(pid(), ne_binary(), ne_binary(), pid()) -> 'ok'.
remove_listener_from_context(ListenerPid, ListenerModule, SessionId, SessionPid)->
    gen_server:cast(self(), {'remove_listener_from_session'
                            ,{ListenerPid, ListenerModule}
                            ,SessionId
                            ,SessionPid}).

-spec status(pid()) -> handle_call_ret() | wh_std_return().
status(ServerPid) ->
    case is_pid(ServerPid) of
        'true' ->    
            gen_server:call(ServerPid, 'status');
                'false' -> {'error', 'process_not_found'}
    end.

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
-spec handle_call(atom(), any(), state()) -> handle_call_ret().
handle_call('status', _, State) -> 
    Status = blackhole_bindings:modules_loaded(),
    {'reply', Status, State};
handle_call(_Request, _From, State) ->
    lager:debug("unhandled handle_call executed ~p~p", [_Request, _From]),
    Reply = 'ok',
    {'reply', Reply, State}.

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
handle_cast('init_dispatcher', State) ->
    _ = ets:new('socket_connections', ['set', 'protected', 'named_table']),
    {'noreply', State};
handle_cast({'connect_socket', SessionId, SessionPid}, State) ->
    ets:insert('socket_connections', {SessionId, {SessionPid, []}}),
    {'noreply', State};
handle_cast({'handle_message', Message, SessionId, _SessionPid}, State) ->
    lager:debug("message received: ~p for socket ~s", [Message, SessionId]),
    {'noreply', State};
handle_cast({'handle_event', EventName, Data, SessionId, SessionPid}, State) ->
    Version = find_version(EventName),
    %%%ClientIP = get_client_ip(),
    ClientIP = <<"127.0.0.1">>,
    Context0 = #bh_context{api_version=Version
                          ,client_ip=ClientIP
                          ,event_name=EventName
                          ,event_data=Data
                          ,websocket_session_id=SessionId
                          ,websocket_pid=SessionPid
                          },
    _ = blackhole_bindings:map(EventName, Context0),
    {'noreply', State};
handle_cast({'disconnect_socket', SessionId, _SessionPid}, State) ->
    case ets:lookup('socket_connections', SessionId) of
        [] -> 'ok';
        [{_, Listeners}] ->
            [(ListenerModule):unsubscribe(ListenerPid, SessionId)
             || {ListenerPid, ListenerModule} <- Listeners
            ]
    end,           
    ets:delete('socket_connections', SessionId),
    {'noreply', State};
handle_cast({'add_listener_to_context', ListenerCallback, SessionId, _SessionPid}, State) ->
    case ets:lookup('socket_connections', SessionId) of
        [] -> lager:debug('socket session ~s not connected to context', [SessionId]);
        [{_, Listeners}] ->
            ets:insert('socket_connections', {SessionId, [ListenerCallback|Listeners]})
    end,            
    {'noreply', State};
handle_cast({'remove_listener_from_context', {ListenerPid, ListenerModule}, SessionId, _SessionPid}, State) ->
    case ets:lookup('socket_connections', SessionId) of
        [] -> lager:debug('socket session ~s not connected to context', [SessionId]);
        [{_, Listeners}] ->
            (ListenerModule):unsubscribe(ListenerPid, SessionId),
            RemListeners = [{RemListenerPid, RemListenerModule}
                            || 
                               {RemListenerPid, RemListenerModule} <- Listeners, RemListenerPid /= ListenerPid
                           ],
            ets:insert('socket_connections', {SessionId, RemListeners})
    end,            
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled handle_cast ~p", [_Msg]),
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
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
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
terminate(_Reason, _State) ->
    ets:delete('socket_connections'),
    lager:debug("blackhole_dispatcher terminated: ~p", [_Reason]),
    'ok'.

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
get_client_ip(Request) ->
    Peer = <<"foo">>,
    ClientIP = case cowboy_req:header(<<"x-forwarded-for">>, Request) of
                   {'undefined', _} -> wh_network_utils:iptuple_to_binary(Peer);
                   {ForwardIP, _} -> wh_util:to_binary(ForwardIP)
               end,
    ClientIP.
    
find_version(EventName) ->                              
    lager:debug("find version in ~s", [EventName]),
    case binary:split(EventName, <<".">>, ['global']) of
        [EventName] ->
            <<"v1">>;
        [<<>>, Ver | _] -> to_version(Ver);
        [Ver | _] -> to_version(Ver)
    end.

to_version(<<"v", Int/binary>>=Version) ->
    try wh_util:to_integer(Int) of
        _ ->
            Version
    catch
        _:_ ->
            <<"v1">>
    end;
to_version(_) -> <<"v1">>.
