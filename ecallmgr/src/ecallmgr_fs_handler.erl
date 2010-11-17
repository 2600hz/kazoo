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
-include("whistle_api.hrl").

-define(SERVER, ?MODULE).
-define(OPTIONS_POS, 2).
-define(AUTH_MOD, ecallmgr_fs_auth).
-define(AUTH_MOD_POS, 3).
-define(ROUTE_MOD, ecallmgr_fs_route).
-define(ROUTE_MOD_POS, 4).
-define(NODE_MOD, ecallmgr_fs_node).
-define(NODE_MOD_POS, 5).

-define(MAX_TIMEOUT_FOR_NODE_RESTART, 300000). % 5 minutes

-define(START_HANDLERS, [fun start_auth_handler/3, fun start_route_handler/3, fun start_node_handler/3]).
-define(HANDLER_POSS, [{?AUTH_MOD_POS, fun start_auth_handler/3}
		       ,{?ROUTE_MOD_POS, fun start_route_handler/3}
		       ,{?NODE_MOD_POS, fun start_node_handler/3}
		      ]).

%% fs_nodes = [{FSNode, Options, AuthHandlerPid, RouteHandlerPid, FSNodeHandlerPid}]
-type node_handlers() :: tuple(atom(), pid(), pid(), pid(), proplist()).
-record(state, {fs_nodes = [] :: list(node_handlers())
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
-spec(request_resource/2 :: (Type :: binary(), Options :: proplist()) -> proplist()).
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
    {ok, #state{}}.

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
    format_log(info, "FS_HANDLER(~p): Setting AMQP Host to ~p from ~p~n", [self(), Host, H]),
    amqp_util:targeted_exchange(Host),
    amqp_util:callmgr_exchange(Host),
    amqp_util:callevt_exchange(Host),
    amqp_util:callctl_exchange(Host),
    {reply, ok, State#state{amqp_host=Host}};
handle_call({diagnostics}, _From, #state{fs_nodes=Nodes, amqp_host=Host}=State) ->
    {ok, Vsn} = application:get_key(ecallmgr, vsn),
    {KnownNodes, HandlerD} = lists:foldl(fun(Tup, {KN, HD}) ->
						 FSNode = element(1, Tup),
						 AuthHandlerD = diagnostics_query(element(?AUTH_MOD_POS, Tup)),
						 RteHandlerD = diagnostics_query(element(?ROUTE_MOD_POS, Tup)),
						 NodeHandlerD = diagnostics_query(element(?NODE_MOD_POS, Tup)),
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
    format_log(info, "FS_HANDLER(~p): Resource Resp: ~p~n", [self(), Resp]),
    {reply, Resp, State};
handle_call(_Request, _From, State) ->
    format_log(error, "FS_HANDLER(~p): Unhandled call ~p~n", [self(), _Request]),
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
    format_log(error, "FS_HANDLER(~p): Handler(~p) EXITed: ~p~n", [self(), HPid, Reason]),
    Funs = ?START_HANDLERS,
    NewNodes = check_down_pid(Nodes, HPid, Host, Funs),
    {noreply, State#state{fs_nodes=NewNodes}};
handle_info({nodedown, Node}, #state{fs_nodes=Nodes}=State) ->
    case lists:keyfind(Node, 1, Nodes) of
	false ->
	    format_log(info, "FS_HANDLER(~p): Received nodedown for ~p but node is not known~n", [self(), Node]),
	    {noreply, State};
	N ->
	    Node = element(1, N),
	    Opts = element(?OPTIONS_POS, N),
	    erlang:monitor_node(Node, false),
	    spawn(fun() -> watch_node_for_restart(Node, Opts) end),
	    format_log(info, "FS_HANDLER(~p): Node ~p has gone down~n", [self(), Node]),
	    {noreply, State}
    end;
handle_info(_Info, State) ->
    format_log(info, "FS_HANDLER(~p): Unhandled Info: ~p~n", [self(), _Info]),
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
terminate(_Reason, #state{fs_nodes=Nodes}) ->
    Self = self(),
    format_log(error, "FS_HANDLER(~p): terminating: ~p~n", [Self, _Reason]),
    lists:foreach(fun close_node/1, Nodes),
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
    lib:flush_receive(),
    case net_adm:ping(Node) of
	pong -> ?MODULE:add_fs_node(Node, Opts);
	pang ->
	    receive
	    after
		Timeout ->
		    format_log(info, "FS_HANDLER.is_node_up(~p): ~p still down, trying again in ~p secs~n", [self(), Node, Timeout div 1000]),
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

-spec(check_down_pid/4 :: (Nodes :: list(node_handlers()), HPid :: pid(), Host :: string(), Fs :: list(fun())) -> list(node_handlers())).
check_down_pid(Nodes, HPid, Host, Fs) ->
    Ps = [?AUTH_MOD_POS, ?ROUTE_MOD_POS, ?NODE_MOD_POS],
    check_down_pid(Nodes, HPid, Host, Fs, Ps).

-spec(check_down_pid/5 :: (Nodes :: list(node_handlers()), HPid :: pid(), Host :: string(), Fs :: list(fun()), Ps :: list(integer())) ->
 list(node_handlers())).
check_down_pid(Nodes, HPid, Host, [F | Fs], [Pos | Ps]) ->
    case restart_handler(Nodes, HPid, Pos, Host, F) of
	Tuple when is_tuple(Tuple) ->
	    Node = element(1, Tuple),
	    erlang:monitor_node(Node, true),
	    [Tuple | lists:keydelete(Node, 1, Nodes)];
	false ->
	    check_down_pid(Nodes, HPid, Host, Fs, Ps)
    end;
check_down_pid(Nodes, _, _, [], []) -> % not a handler pid
    Nodes.

-spec(restart_handler/5 :: (Nodes :: list(node_handlers()), HPid :: pid(), Pos :: integer(), Host :: string(), StartFun :: fun()) -> node_handlers() | false).
restart_handler(Nodes, HPid, Pos, Host, StartFun) ->
    case lists:keyfind(HPid, Pos, Nodes) of
	Tuple when is_tuple(Tuple) ->
	    Node = element(1, Tuple),
	    case element(Pos, Tuple) of
		HPid ->
		    Opts = element(?OPTIONS_POS, Tuple),
		    case StartFun(Node, Opts, Host) of
			{error, Err} ->
			    format_log(error, "FS_HANDLER(~p): Handler(pos ~p) failed to restart for ~p: ~p~n", [self(), Pos, Node, Err]),
			    setelement(Pos, Tuple, undefined);
			NewHPid when is_pid(NewHPid) ->
			    format_log(info, "FS_HANDLER(~p): Handler down(~p @ ~p), restarting handler(~p).~n", [self(), HPid, Pos, NewHPid]),
			    setelement(Pos, Tuple, NewHPid)
		    end;
		_ ->
		    Tuple
	    end;
	false ->
	    false
    end.

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
    case lists:keyfind(Node, 1, Nodes) of
	T when is_tuple(T) ->
	    format_log(info, "FS_HANDLER(~p): handlers known ~p~n", [self(), T]),
	    NewL = lists:foldl(fun({Pos, StartFun}, Acc) ->
				       case element(Pos, T) of
					   Pid when is_pid(Pid) ->
					       Pid ! {update_options, Options},
					       [Pid | Acc];
					   _Other ->
					       [StartFun(Node, Options, Host) | Acc]
				       end
			       end, [Options, Node], ?HANDLER_POSS),
	    NewT = list_to_tuple(lists:reverse(NewL)),
	    format_log(info, "FS_HANDLER(~p): Restarted handlers. Old: ~p New ~p~n", [self(), T, NewT]),
	    {ok, State#state{fs_nodes=[NewT | lists:keydelete(Node, 1, Nodes)]}};
	false ->
	    case net_adm:ping(Node) of
		pong ->
		    erlang:monitor_node(Node, true),
		    HPids = lists:map(fun(StartFun) -> StartFun(Node, Options, Host) end, ?START_HANDLERS),
		    Tuple = erlang:list_to_tuple([Node, Options | HPids]),
		    {ok, State#state{fs_nodes=[Tuple | Nodes]}};
		pang ->
		    format_log(error, "FS_HANDLER(~p): ~p not responding~n", [self(), Node]),
		    {{error, no_connection}, State}
	    end
    end.

-spec(rm_fs_node/2 :: (Node :: atom(), State :: tuple()) -> {ok | tuple(error, no_node, atom()), tuple()}).
rm_fs_node(Node, #state{fs_nodes=Nodes}=State) ->
    case lists:keyfind(Node, 1, Nodes) of
	N when is_tuple(N) ->
	    Res = close_node(N),
	    {{ok, Res}, State#state{fs_nodes=lists:keydelete(Node, 1, Nodes)}};
	false ->
	    format_log(error, "FS_HANDLER(~p): No handlers found for ~p~n", [self(), Node]),
	    {{error, no_node, Node}, State}
    end.

-spec(close_node/1 :: (N :: node_handlers()) -> tuple(atom(), ok | tuple(), ok | tuple(), ok | tuple())).
close_node(N) ->
    Node = element(1, N),
    Options = element(?OPTIONS_POS, N),
    erlang:monitor_node(Node, false),
    Ps = [?AUTH_MOD_POS, ?ROUTE_MOD_POS, ?NODE_MOD_POS],
    close_node(N, Ps, [Node, Options]).

close_node(_N, [], L) ->
    list_to_tuple(lists:reverse(L));
close_node(N, [P | Ps], L) ->
    Pid = element(P, N),
    Res = case is_pid(Pid) andalso erlang:is_process_alive(Pid) of
	      true -> Pid ! shutdown, ok;
	      false -> {error, handler_down}
	  end,
    close_node(N, Ps, [Res | L]).

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
		format_log(error, "FS_HANDLER(~p): Failed to start ~p for ~p on ~p: got ~p~n", [self(), Mod, Node, Host, _Other]),
		E
	end
    catch
	What:Why ->
	    format_log(error, "FS_HANDLER(~p): Failed to start ~p for ~p on ~p: got ~p:~p~n", [self(), Mod, Node, Host, What, Why]),
	    {error, Why}
    end.

-spec(process_resource_request/3 :: (Type :: binary(), Nodes :: list(node_handlers()), Options :: proplist()) -> list(proplist()) | list()).
process_resource_request(<<"audio">>=Type, Nodes, Options) ->
    NodesResp = lists:map(fun(N) ->
				  NodePid = element(?NODE_MOD_POS, N),
				  format_log(info, "FS_HANDLER.prr: NodePid ~p N ~p~n", [NodePid, N]),
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
    format_log(info, "FS_HANDLER(~p): Unhandled resource request type ~p~n", [self(), Type]),
    [].
