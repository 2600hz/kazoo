%%%-----------------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%%
%%% Prior to any handlers being created, ecallmgr_fs_handler must be given an
%%% AMQP host string to use for sending/receiving API calls.
%%%
%%% When a device tries to register with a known FS node, FS sends a
%%% fetch request to the corresponding fetch_handler (a process spawned
%%% during the call to add_fs_node/1). The fetch_handler checks to see
%%% that the fetch request is one the handler can, you know, handle
%%% and if so, spawns a call to lookup_(user|route)/4.
%%%
%%% CALL AUTHENTICATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% For a user lookup, the newly spawned process creates an Authentication
%%% Request API message, as well as a queue on the Targeted Exchange, and
%%% places the AuthReq onto the Broadcast Exchange. Lookup_user then waits,
%%% up to a threshold, for an AuthResp message. If a timeout occurs, an
%%% empty response is returned to the fetch_handler; otherwise, lookup_user
%%% tries to create the appropriate XML response to pass back to the
%%% fetch_handler. Upon receiving the XML, fetch_handler sends the XML to
%%% the FS node, and goes back to waiting for another fetch request. The
%%% lookup_user process ends after sending the XML and cleaning up.
%%%
%%%                 ---------------
%%%                 |ecallmgr_auth|
%%%                 ---------------
%%%                        |
%%%             -------------------------
%%%             |                       |
%%%     ---------------           ---------------
%%%     |fetch_handler|           |fetch_handler|
%%%     |(per FS Node)|           |(per FS Node)|
%%%     ---------------           ---------------
%%%            |                        |
%%%     ---------------          ---------------
%%%     |      |      |          |      |      |
%%%   -----  -----  -----      -----  -----  -----
%%%   |L/U|  |L/U|  |L/U|      |L/U|  |L/U|  |L/U|
%%%   -----  -----  -----      -----  -----  -----
%%%
%%% L/U = lookup_user per auth request
%%%
%%% CALL ROUTING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% For a route lookup, the newly spawned process creates a Route Request
%%% API message, three queues (one for the lookup_route process on the
%%% Targeted Exchange, one for the call's event stream on the CallEvent
%%% Exchange, and one to receive call control API messages from an
%%% application on the CallControl Exchange), and send the Req message to
%%% the Broadcast Exchange. lookup_route/4 then waits, up to a threshold,
%%% for a returning Route Response API message. If a timeout occurs, an
%%% empty XML snippet is returned; otherwise lookup_route tries to create
%%% appropriate routing XML to pass back to the handler. After sending the
%%% XML to the fetch handler, lookup_route creates two processes, one to
%%% listen for events related to the call-id and publish them to the 
%%% CallEvent queue, and one to listen for control messages on the CallControl
%%% queue. Further call processing is handled via the CallControl process,
%%% and call events are made known to interested applications via the 
%%% CallEvents process.
%%%
%%%                 ----------------
%%%                 |ecallmgr_route|
%%%                 ----------------
%%%                        |
%%%             -------------------------
%%%             |                       |
%%%     ---------------           ---------------
%%%     |fetch_handler|           |fetch_handler|
%%%     |(per FS Node)|           |(per FS Node)|
%%%     ---------------           ---------------
%%%            |                        |
%%%     ---------------          ---------------
%%%     |      |      |          |      |      |
%%%   -----  -----  -----      -----  -----  -----
%%%   |L/U|  |L/U|  |L/U|      |L/U|  |L/U|  |L/U|
%%%   -----  -----  -----      -----  -----  -----
%%%     |                        |
%%%   -----                    -----
%%%   |   |                    |   |
%%% ---- ----                ---- ----
%%% |CC| |CE|                |CC| |CE|
%%% ---- ----                ---- ----
%%%
%%% L/U = lookup_route per dialplan request
%%% CC = CallControl process
%%% CE = CallEvent process
%%%
%%% @end
%%% Created : 08 Oct 2010 by James Aimonetti <james@2600hz.com>
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_handler).

-behaviour(gen_server).

%% API
-export([start_link/0, add_fs_node/1, add_fs_node/2, rm_fs_node/1, diagnostics/0, set_amqp_host/1]).

%% Resource allotment
-export([request_resource/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(props, [get_value/2, get_value/3]).
-import(logger, [log/2, format_log/3]).

-include("freeswitch_xml.hrl").
-include("ecallmgr.hrl").
-include("whistle_api.hrl").

-define(SERVER, ?MODULE).
-define(AUTH_MOD, ecallmgr_fs_auth).
-define(ROUTE_MOD, ecallmgr_fs_route).
-define(NODE_MOD, ecallmgr_fs_node).

-record(node_handler, {node = 'undefined' :: atom()
		       ,options = [] :: proplist()
		       ,auth_handler_pid = undefined :: pid() | undefined
                       ,route_handler_pid = undefined :: pid() | undefined
                       ,node_handler_pid = undefined :: pid() | undefined
                       ,node_watch_pid = undefined :: pid() | undefined % If a node goes down, this pid is responsible for watching for the node to restart
		      }).

-record(state, {fs_nodes = [] :: list(#node_handler{})
		,amqp_host = "" :: string()
	       }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% returns ok or {error, some_error_atom_explaining_more}
add_fs_node(Node) -> add_fs_node(Node, []).
add_fs_node(Node, Opts) ->
    gen_server:call(?MODULE, {add_fs_node, Node, Opts}, infinity).

%% returns ok or {error, some_error_atom_explaining_more}
rm_fs_node(Node) ->
    gen_server:call(?MODULE, {rm_fs_node, Node}, infinity).

%% calls all handlers and gets diagnostic info from them
diagnostics() ->
    gen_server:call(?MODULE, {diagnostics}, infinity).

set_amqp_host(Host) ->
    gen_server:call(?MODULE, {set_amqp_host, Host}, infinity).

%% Type - audio | video
%% Options - Proplist
%%   {min_channels_requested, 1}
%%   {max_channels_requested, 1}
%% Returns - Proplist
%%   {max_channels_available, 4}
%%   {bias, 1}
-spec(request_resource/2 :: (Type :: binary(), Options :: proplist()) -> list(proplist())).
request_resource(Type, Options) ->
    gen_server:call(?MODULE, {request_resource, Type, Options}).

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
    process_flag(trap_exit, true),
    try
	H = net_adm:localhost(),
	start_amqp(H),
	{ok, #state{amqp_host=H}}
    catch
	_:_ -> {ok, #state{}}
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
%% #state{fs_nodes=[{FSNode, HandlerPid}]}
%%--------------------------------------------------------------------
handle_call({set_amqp_host, Host}, _From, #state{amqp_host=H}=State) ->
    format_log(info, "FS_HANDLER(~w): Switching AMQP hosts from ~w to ~w~n", [self(), H, Host]),
    start_amqp(Host),
    {reply, ok, State#state{amqp_host=Host}};
handle_call({diagnostics}, _From, #state{fs_nodes=Nodes, amqp_host=Host}=State) ->
    {ok, Vsn} = application:get_key(ecallmgr, vsn),
    {KnownNodes, HandlerD} = lists:foldl(fun(#node_handler{node=FSNode, auth_handler_pid=AHP, route_handler_pid=RHP, node_handler_pid=NHP}
					     ,{KN, HD}) ->
						 AuthHandlerD = diagnostics_query(AHP),
						 RteHandlerD = diagnostics_query(RHP),
						 NodeHandlerD = diagnostics_query(NHP),
						 {[FSNode | KN], [{FSNode
								   ,{auth_handler, AuthHandlerD}
								   ,{route_handler, RteHandlerD}
								   ,{node_handler, NodeHandlerD}
								  }
								  | HD]}
					 end, {[], []}, Nodes),
    Resp = [{gen_server, ?MODULE}
	    ,{host, net_adm:localhost()}
	    ,{version, Vsn}
	    ,{known_fs_nodes, KnownNodes}
	    ,{handler_diagnostics, HandlerD}
	    ,{recorded, erlang:now()}
	    ,{amqp_host, Host}
	   ],
    {reply, Resp, State};
handle_call({add_fs_node, _Node, _Options}, _From, #state{amqp_host=""}=State) ->
    {reply, {error, no_amqp_host_defined}, State};
handle_call({add_fs_node, Node, Options}, _From, State) ->
    {Resp, State1} = add_fs_node(Node, check_options(Options), State),
    {reply, Resp, State1};
handle_call({rm_fs_node, Node}, _From, State) ->
    {Resp, State1} = rm_fs_node(Node, State),
    {reply, Resp, State1};
handle_call({request_resource, Type, Options}, _From, #state{fs_nodes=Nodes}=State) ->
    Resp = process_resource_request(Type, Nodes, Options),
    format_log(info, "FS_HANDLER(~w): Resource Resp: ~w~n", [self(), Resp]),
    {reply, Resp, State};
handle_call(_Request, _From, State) ->
    format_log(error, "FS_HANDLER(~w): Unhandled call ~w~n", [self(), _Request]),
    {reply, {error, unhandled_request}, State}.

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
handle_cast(_Msg, State) ->
    {noreply, State}.

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
handle_info({'EXIT', HPid, Reason}, #state{fs_nodes=Nodes, amqp_host=Host}=State) ->
    format_log(error, "FS_HANDLER(~w): Handler(~w) EXITed: ~w~n~w~n", [self(), HPid, Reason, Nodes]),
    NewNodes = check_down_pid(Nodes, HPid, Host),
    {noreply, State#state{fs_nodes=NewNodes}};
handle_info({nodedown, Node}, #state{fs_nodes=Nodes}=State) ->
    case lists:filter(fun(#node_handler{node=Node1}) when Node =:= Node1 -> true; (_) -> false end, Nodes) of
	[] ->
	    format_log(info, "FS_HANDLER(~w): Received nodedown for ~w but node is not known~n", [self(), Node]),
	    {noreply, State};
	[#node_handler{node=Node, options=Opts}=N|_] ->
	    erlang:monitor_node(Node, false),
	    WatchPid = spawn_link(fun() -> watch_node_for_restart(Node, Opts) end),
	    format_log(info, "FS_HANDLER(~w): Node ~w has gone down~n", [self(), Node]),
	    {noreply, State#state{fs_nodes=[N#node_handler{node_watch_pid=WatchPid} | lists:keydelete(Node, 2, Nodes)]}}
    end;
handle_info(_Info, State) ->
    format_log(info, "FS_HANDLER(~w): Unhandled Info: ~w~n", [self(), _Info]),
    {noreply, State}.

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
terminate(_Reason, #state{fs_nodes=Nodes, amqp_host=H}) ->
    Self = self(),
    format_log(error, "FS_HANDLER(~w): terminating: ~w~n", [Self, _Reason]),
    lists:foreach(fun close_node/1, Nodes),
    amqp_util:channel_close(H),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec(watch_node_for_restart/2 :: (Node :: atom(), Opts :: proplist()) -> no_return()).
watch_node_for_restart(Node, Opts) ->
    watch_node_for_restart(Node, Opts, 1000).

-spec(watch_node_for_restart/3 :: (Node :: atom(), Opts :: proplist(), Timeout :: integer()) -> no_return()).
watch_node_for_restart(Node, Opts, Timeout) when Timeout > ?MAX_TIMEOUT_FOR_NODE_RESTART ->
    is_node_up(Node, Opts, ?MAX_TIMEOUT_FOR_NODE_RESTART);
watch_node_for_restart(Node, Opts, ?MAX_TIMEOUT_FOR_NODE_RESTART) ->
    is_node_up(Node, Opts, ?MAX_TIMEOUT_FOR_NODE_RESTART);
watch_node_for_restart(Node, Opts, Timeout) ->
    is_node_up(Node, Opts, Timeout * 2).

is_node_up(Node, Opts, Timeout) ->
    case net_adm:ping(Node) of
	pong ->
	    format_log(info, "FS_HANDLER.is_node_up(~w): ~w has risen~n", [self(), Node]),
	    ?MODULE:add_fs_node(Node, Opts);
	pang ->
	    receive
		shutdown -> format_log(info, "FS_HANDLER(~w): watcher going down for ~w~n", [self(), Node])
	    after
		Timeout ->
		    format_log(info, "FS_HANDLER.is_node_up(~w): trying ~w again, then waiting for ~w secs~n", [self(), Node, Timeout div 1000]),
		    watch_node_for_restart(Node, Opts, Timeout)
	    end
    end.

-spec(check_options/1 :: (Opts :: proplist()) -> proplist()).
check_options([]) ->
    [{bias, 1}, {max_channels, 100}];
check_options(Opts) ->
    Opts0 = case get_value(bias, Opts) of
		undefined -> [{bias, 1} | Opts];
		_ -> Opts
	    end,
    case get_value(max_channels, Opts0) of
	undefined -> [{max_channels, 100} | Opts0];
	_ -> Opts0
    end.

-spec(check_down_pid/3 :: (Nodes :: list(#node_handler{}), HPid :: pid(), Host :: string()) -> list(#node_handler{})).
check_down_pid(Nodes, HPid, Host) ->
    lists:foldl(fun(#node_handler{auth_handler_pid=AHP
				  ,route_handler_pid=RHP
				  ,node_handler_pid=NHP
				  ,node_watch_pid=undefined
				  ,options=Options
				  ,node=Node
				 }=N, Nodes0) ->
			Restart = fun(CurrHPid, StartFun) -> restart_handler(Node, Options, Host, HPid, CurrHPid, StartFun) end,
			NewAHP = Restart(AHP, fun start_auth_handler/3),
			NewRHP = Restart(RHP, fun start_route_handler/3),
			NewNHP = Restart(NHP, fun start_node_handler/3),
			[N#node_handler{auth_handler_pid=NewAHP
					,route_handler_pid=NewRHP
					,node_handler_pid=NewNHP}
			 | Nodes0];
		   (#node_handler{node_watch_pid=NWP}=N, Nodes0) when NWP =:= HPid ->
			[N#node_handler{node_watch_pid=undefined} | Nodes0];
		   (#node_handler{node_watch_pid=NWP}, Nodes0) when is_pid(NWP) ->
			Nodes0
		end, [], Nodes).

-spec(restart_handler/6 :: (Node :: atom(), Options :: proplist(), Host :: string(), DownPid :: pid() | undefined, CurrPid :: pid() | undefined, StartFun :: fun()) ->
				pid() | undefined).
restart_handler(Node, Options, Host, _, undefined, StartFun) ->
    case StartFun(Node, Options, Host) of
	{error, Err} ->
	    format_log(error, "FS_HANDLER.rstrt_h(~w): Error ~w starting handler~n", [self(), Err]),
	    undefined;
	Pid when is_pid(Pid) -> Pid
    end;
restart_handler(Node, Options, Host, HPid, HPid, StartFun) ->
    case StartFun(Node, Options, Host) of
	{error, Err} ->
	    format_log(error, "FS_HANDLER.rstrt_h(~w): Error ~w restarting handler ~w~n", [self(), Err, HPid]),
	    undefined;
	Pid when is_pid(Pid) ->
	    format_log(info, "FS_HANDLER.rstrt_h(~w): Restarting ~w as ~w~n", [self(), HPid, Pid]),
	    Pid
    end;
restart_handler(_, _, _, _, Pid, _) -> Pid.

%% query a pid for its diagnostics info
-spec(diagnostics_query/1 :: (Pid :: pid()) -> tuple(ok, proplist()) | tuple(error, atom(), term())).
diagnostics_query(Pid) when is_pid(Pid) ->
    case erlang:is_process_alive(Pid) of
	true ->
	    Pid ! {diagnostics, self()},
	    receive
		X -> {ok, X}
	    after
		500 -> {error, timed_out, handler_busy}
	    end;
	false ->
	    {error, not_responding, handler_down}
    end;
diagnostics_query(X) ->
    {error, handler_down, X}.

-spec(add_fs_node/3 :: (Node :: atom(), Options :: proplist(), State :: tuple()) -> tuple(ok, tuple()) | tuple(tuple(error, atom()), tuple())).
add_fs_node(Node, Options, #state{fs_nodes=Nodes, amqp_host=Host}=State) ->
    case lists:filter(fun(#node_handler{node=Node1}) when Node =:= Node1 -> true; (_) -> false end, Nodes) of
	[] ->
	    case net_adm:ping(Node) of
		pong ->
		    erlang:monitor_node(Node, true),
		    Restart = fun(StartFun) -> restart_handler(Node, Options, Host, undefined, undefined, StartFun) end,

		    AHP = Restart(fun start_auth_handler/3),
		    RHP = Restart(fun start_route_handler/3),
		    NHP = Restart(fun start_node_handler/3),

		    {ok, State#state{fs_nodes=[#node_handler{node=Node
							     ,options=Options
							     ,auth_handler_pid=AHP
							     ,route_handler_pid=RHP
							     ,node_handler_pid=NHP
							    }
					       | Nodes]}};
		pang ->
		    format_log(error, "FS_HANDLER(~w): ~w not responding~n", [self(), Node]),
		    {{error, no_connection}, State}
	    end;
	[#node_handler{node=Node, auth_handler_pid=AHP, route_handler_pid=RHP, node_handler_pid=NHP}=N] ->
	    format_log(info, "FS_HANDLER(~w): handlers known ~w~n", [self(), N]),
	    Restart = fun(Pid, _StartFun) when is_pid(Pid) ->
			      Pid ! {update_options, Options},
			      Pid;
			 (_, StartFun) ->
			      case StartFun(Node, Options, Host) of
				  {error, E} ->
				      format_log(error, "FS_HANDLER.add(~w): Handler start on ~w failed because ~w~n", [self(), Node, E]),
				      undefined;
				  Pid -> Pid
			      end
		      end,
	    {ok, State#state{fs_nodes=[N#node_handler{auth_handler_pid=Restart(AHP, fun start_auth_handler/3)
						      ,route_handler_pid=Restart(RHP, fun start_route_handler/3)
						      ,node_handler_pid=Restart(NHP, fun start_route_handler/3)
						      ,options=Options}
				       | lists:keydelete(Node, 2, Nodes)]}}
    end.

-spec(rm_fs_node/2 :: (Node :: atom(), State :: tuple()) -> {ok | tuple(error, no_node, atom()), tuple()}).
rm_fs_node(Node, #state{fs_nodes=Nodes}=State) ->
    case lists:keyfind(Node, 2, Nodes) of
	false ->
	    format_log(error, "FS_HANDLER(~w): No handlers found for ~w~n", [self(), Node]),
	    {{error, no_node, Node}, State};
	N ->
	    Res = close_node(N),
	    {{ok, Res}, State#state{fs_nodes=lists:keydelete(Node, 2, Nodes)}}
    end.

-spec(close_node/1 :: (N :: #node_handler{}) -> tuple(atom(), ok | tuple(), ok | tuple(), ok | tuple())).
close_node(#node_handler{node=Node
			 ,auth_handler_pid=AHP
			 ,route_handler_pid=RHP
			 ,node_handler_pid=NHP}) ->
    erlang:monitor_node(Node, false),
    {Node, close_handler(AHP), close_handler(RHP), close_handler(NHP)}.

-spec(close_handler/1 :: (Pid :: pid()) -> ok | tuple(error, handler_down)).
close_handler(Pid) ->
    case is_pid(Pid) andalso erlang:is_process_alive(Pid) of
	true -> Pid ! shutdown, ok;
	false -> {error, handler_down}
    end.

-spec(start_auth_handler/3 :: (Node :: atom(), Options :: proplist(), Host :: string()) -> pid() | tuple(error, term())).
start_auth_handler(Node, Options, Host) ->
    start_handler(Node, Options, Host, ?AUTH_MOD).

-spec(start_route_handler/3 :: (Node :: atom(), Options :: proplist(), Host :: string()) -> pid() | tuple(error, term())).
start_route_handler(Node, Options, Host) ->
    start_handler(Node, Options, Host, ?ROUTE_MOD).

-spec(start_node_handler/3 :: (Node :: atom(), Options :: proplist(), Host :: string()) -> pid() | tuple(error, term())).
start_node_handler(Node, Options, Host) ->
    start_handler(Node, Options, Host, ?NODE_MOD).

-spec(start_handler/4 :: (Node :: atom(), Options :: proplist(), Host :: string(), Mod :: atom()) -> pid() | tuple(error, term())).
start_handler(Node, Options, Host, Mod) ->
    try
	case Mod:start_handler(Node, Options, Host) of
	    Pid when is_pid(Pid) ->
		link(Pid),
		Pid;
	    {error, _Other}=E ->
		format_log(error, "FS_HANDLER(~w): Failed to start ~w for ~w on ~w: got ~w~n", [self(), Mod, Node, Host, _Other]),
		E
	end
    catch
	What:Why ->
	    format_log(error, "FS_HANDLER(~w): Failed to start ~w for ~w on ~w: got ~w:~w~n", [self(), Mod, Node, Host, What, Why]),
	    {error, Why}
    end.

-spec(process_resource_request/3 :: (Type :: binary(), Nodes :: list(#node_handler{}), Options :: proplist()) -> list(proplist()) | []).
process_resource_request(<<"audio">>=Type, Nodes, Options) ->
    NodesResp = lists:map(fun(#node_handler{node_handler_pid=NodePid}=N) ->
				  format_log(info, "FS_HANDLER.prr: NodePid ~w N ~w~n", [NodePid, N]),
				  NodePid ! {resource_request, self(), Type, Options},
				  receive
				      {resource_response, NodePid, Resp} ->
					  Resp
				  after 500 ->
					  []
				  end
			  end, Nodes),
    [ X || X <- NodesResp, X =/= []];
process_resource_request(Type, _Nodes, _Options) ->
    format_log(info, "FS_HANDLER(~w): Unhandled resource request type ~w~n", [self(), Type]),
    [].

-spec(start_amqp/1 :: (Host :: string()) -> no_return()).
start_amqp(Host) ->
    amqp_util:targeted_exchange(Host),
    amqp_util:callmgr_exchange(Host),
    amqp_util:callevt_exchange(Host),
    amqp_util:callctl_exchange(Host).
