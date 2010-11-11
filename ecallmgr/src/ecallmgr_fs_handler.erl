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
-export([start_link/0, add_fs_node/1, rm_fs_node/1, diagnostics/0, set_amqp_host/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(props, [get_value/2, get_value/3]).
-import(logger, [log/2, format_log/3]).

-include("freeswitch_xml.hrl").
-include("whistle_api.hrl").

-define(SERVER, ?MODULE). 
-define(AUTH_MOD, ecallmgr_fs_auth).
-define(ROUTE_MOD, ecallmgr_fs_route).
-define(NODE_MOD, ecallmgr_fs_node).

-define(START_HANDLERS, [fun start_auth_handler/2, fun start_route_handler/2, fun start_node_handler/2]).

%% fs_nodes = [{FSNode, AuthHandlerPid, RouteHandlerPid, FSNodeHandlerPid}]
-type node_handlers() :: tuple(atom(), pid(), pid(), pid()).
-record(state, {fs_nodes = [] :: list(node_handlers())
		,amqp_host = "" :: string()
	       }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% returns ok or {error, some_error_atom_explaining_more}
add_fs_node(Node) ->
    gen_server:call(?MODULE, {add_fs_node, Node}, infinity).

%% returns ok or {error, some_error_atom_explaining_more}
rm_fs_node(Node) ->
    gen_server:call(?MODULE, {rm_fs_node, Node}, infinity).

%% calls all handlers and gets diagnostic info from them
diagnostics() ->
    gen_server:call(?MODULE, {diagnostics}, infinity).

set_amqp_host(Host) ->
    gen_server:call(?MODULE, {set_amqp_host, Host}, infinity).

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
    {KnownNodes, HandlerD} = lists:foldl(fun({FSNode, AuthHPid, RouteHPid, NodeHPid}, {KN, HD}) ->
						 AuthHandlerD = diagnostics_query(AuthHPid),
						 RteHandlerD = diagnostics_query(RouteHPid),
						 NodeHandlerD = diagnostics_query(NodeHPid),
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
handle_call({add_fs_node, _Node}, _From, #state{amqp_host=""}=State) ->
    {reply, {error, no_amqp_host_defined}, State};
handle_call({add_fs_node, Node}, _From, State) ->
    {Resp, State1} = add_fs_node(Node, State),
    {reply, Resp, State1};
handle_call({rm_fs_node, Node}, _From, State) ->
    {Resp, State1} = rm_fs_node(Node, State),
    {reply, Resp, State1};
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
	{Node, _, _, _}=N ->
	    close_node(N),
	    {noreply, State#state{fs_nodes=lists:keydelete(Node, 1, Nodes)}}
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
-spec(check_down_pid/4 :: (Nodes :: list(node_handlers()), HPid :: pid(), Host :: string(), Fs :: list(fun())) -> list(node_handlers())).
check_down_pid(Nodes, HPid, Host, Fs) ->
    check_down_pid(Nodes, HPid, Host, Fs, 2).

-spec(check_down_pid/5 :: (Nodes :: list(node_handlers()), HPid :: pid(), Host :: string(), Fs :: list(fun()), Pos :: integer()) -> list(node_handlers())).
check_down_pid(Nodes, HPid, Host, [F | Fs], Pos) ->
    case restart_handler(Nodes, HPid, Pos, Host, F) of
	{Node, _, _, _}=Tuple when is_tuple(Tuple) ->
	    [Tuple | lists:keydelete(Node, 1, Nodes)];
	false ->
	    check_down_pid(Nodes, HPid, Host, Fs, Pos+1)
    end;
check_down_pid(Nodes, _, _, [], _) -> % not a handler pid
    Nodes.

-spec(restart_handler/5 :: (Nodes :: list(node_handlers()), HPid :: pid(), Pos :: integer(), Host :: string(), StartFun :: fun()) -> node_handlers() | false).
restart_handler(Nodes, HPid, Pos, Host, StartFun) ->
    case lists:keyfind(HPid, Pos, Nodes) of
	{Node, _, _, _}=Tuple ->
	    case element(Pos, Tuple) of
		HPid ->
		    case StartFun(Node, Host) of
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
diagnostics_query(Pid) ->
    case erlang:is_process_alive(Pid) of
	true ->
	    Pid ! {diagnostics, self()},
	    receive
		X -> X
	    after
		500 -> {error, timed_out, handler_busy}
	    end;
	false ->
	    {error, not_responding, handler_down}
    end.

-spec(add_fs_node/2 :: (Node :: atom(), State :: tuple()) -> {ok, tuple()} | {{error, atom()}, tuple()}).
add_fs_node(Node, #state{fs_nodes=Nodes, amqp_host=Host}=State) ->
    case lists:keyfind(Node, 1, Nodes) of
	T when is_tuple(T) ->
	    format_log(info, "FS_HANDLER(~p): handlers known ~p~n", [self(), T]),
	    {{error, node_is_known}, State};
	false ->
	    case net_adm:ping(Node) of
		pong ->
		    erlang:monitor_node(Node, true),
		    HPids = lists:map(fun(StartFun) -> StartFun(Node, Host) end, ?START_HANDLERS),
		    Tuple = erlang:list_to_tuple([Node | HPids]),
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
    erlang:monitor_node(Node, false),
    close_node(N, tuple_size(N), 2, [Node]).

close_node(_N, S, P, L) when P > S ->
    list_to_tuple(lists:reverse(L));
close_node(N, S, P, L) ->
    Pid = element(P, N),
    Res = case is_pid(Pid) andalso erlang:is_process_alive(Pid) of
	      true -> Pid ! shutdown, ok;
	      false -> {error, handler_down}
	  end,
    close_node(N, S, P+1, [Res | L]).

-spec(start_auth_handler/2 :: (Node :: atom(), Host :: string()) -> pid() | tuple(error, term())).
start_auth_handler(Node, Host) ->
    start_handler(Node, Host, ?AUTH_MOD).

-spec(start_route_handler/2 :: (Node :: atom(), Host :: string()) -> pid() | tuple(error, term())).
start_route_handler(Node, Host) ->
    start_handler(Node, Host, ?ROUTE_MOD).

-spec(start_node_handler/2 :: (Node :: atom(), Host :: string()) -> pid() | tuple(error, term())).
start_node_handler(Node, Host) ->
    start_handler(Node, Host, ?NODE_MOD).

-spec(start_handler/3 :: (Node :: atom(), Host :: string(), Mod :: atom()) -> pid() | tuple(error, term())).
start_handler(Node, Host, Mod) ->
    case Mod:start_handler(Node, Host) of
	Pid when is_pid(Pid) ->
	    link(Pid),
	    Pid;
	{error, _Other}=E ->
	    format_log(error, "FS_HANDLER(~p): Failed to start ~p for ~p on ~p: got ~p~n", [self(), Mod, Node, Host, _Other]),
	    E
    end.
