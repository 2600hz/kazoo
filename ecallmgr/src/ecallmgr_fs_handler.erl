%%%-----------------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
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
-export([start_link/0, add_fs_node/1, rm_fs_node/1, diagnostics/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(props, [get_value/2, get_value/3]).
-import(logger, [log/2, format_log/3]).

-include("../include/amqp_client/include/amqp_client.hrl").
-include("freeswitch_xml.hrl").
-include("whistle_api.hrl").

-define(SERVER, ?MODULE). 
-define(AUTH_MOD, ecallmgr_fs_auth).
-define(ROUTE_MOD, ecallmgr_fs_route).

%% fs_nodes = [{FSNode, AuthHandlerPid, RouteHandlerPid}]
-record(state, {fs_nodes=[]}).

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
handle_call({diagnostics}, _From, #state{fs_nodes=Nodes}=State) ->
    {ok, Vsn} = application:get_key(ecallmgr, vsn),
    {KnownNodes, HandlerD} = lists:foldl(fun({FSNode, AuthHPid, RouteHPid}, {KN, HD}) ->
						 AuthHandlerD = case erlang:is_process_alive(AuthHPid) of
								    true ->
									AuthHPid ! {diagnostics, self()},
									receive
									    X -> X
									after
									    500 -> {error, timed_out, handler_busy}
									end;
								    false ->
									{error, not_responding, handler_down}
								end,
						 RteHandlerD = case erlang:is_process_alive(RouteHPid) of
								   true ->
								       RouteHPid ! {diagnostics, self()},
								       receive
									   Y -> Y
								       after
									   500 -> {error, timed_out, handler_busy}
								       end;
								   false ->
								       {error, not_responding, handler_down}
							       end,
						 {[FSNode | KN], [{FSNode, {auth_handler, AuthHandlerD}, {route_handler, RteHandlerD}} | HD]}
					 end, {[], []}, Nodes),
    Resp = [{gen_server, ?MODULE}
	    ,{host, net_adm:localhost()}
	    ,{version, Vsn}
	    ,{known_fs_nodes, KnownNodes}
	    ,{handler_diagnostics, HandlerD}
	    ,{recorded, erlang:now()}
	   ],
    {reply, Resp, State};
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
handle_info({'EXIT', HPid, Reason}, #state{fs_nodes=Nodes}=State) ->
    case lists:keyfind(HPid, 2, Nodes) of
	{Node, HPid, RouteHPid} ->
	    APid = start_auth_handler(Node),
	    format_log(error, "FS_HANDLER(~p): Auth Handler EXITed(~p) for ~p, restarting handler as ~p...~n", [self(), Reason, HPid, APid]),
	    {noreply, State#state{fs_nodes=[{Node, APid, RouteHPid} | lists:keydelete(Node, 1, Nodes)]}};
	false ->
	    case lists:keyfind(HPid, 3, Nodes) of
		{Node, AuthHPid, HPid} ->
		    RPid = start_route_handler(Node),
		    format_log(error, "FS_HANDLER(~p): Route Handler EXITed(~p) for ~p, restarting handler as ~p...~n", [self(), Reason, HPid, RPid]),
		    {noreply, State#state{fs_nodes=[{Node, AuthHPid, RPid} | lists:keydelete(Node, 1, Nodes)]}};
		false ->
		    format_log(error, "FS_HANDLER(~p): Received EXIT(~p) for unknown (or already ended) pid ~p~n", [self(), Reason, HPid]),
		    {noreply, State}
	    end
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
    lists:foreach(fun({_FSNode, AuthHPid, RouteHPid}) ->
			  format_log(error, "FS_HANDLER(~p): terminate handlers: ~p and ~p for node ~p~n", [Self, AuthHPid, RouteHPid, _FSNode]),
			  AuthHPid ! shutdown,
			  RouteHPid ! shutdown
		  end, Nodes),
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
-spec(add_fs_node/2 :: (Node :: atom(), State :: tuple()) -> {ok, tuple()} | {{error, atom()}, tuple()}).
add_fs_node(Node, #state{fs_nodes=Nodes}=State) ->
    case lists:keyfind(Node, 1, Nodes) of
	{Node, _AuthHPid, _RteHPid} ->
	    format_log(info, "FS_HANDLER(~p): handlers(~p and ~p) known for ~p~n", [self(), _AuthHPid, _RteHPid, Node]),
	    {{error, node_is_known}, State};
	false ->
	    case net_adm:ping(Node) of
		pong ->
		    {AuthHPid, RouteHPid} = setup_fs_conn(Node),
		    format_log(info, "FS_HANDLER(~p): Starting handlers(~p and ~p) for ~p~n", [self(), AuthHPid, RouteHPid, Node]),
		    {ok, State#state{fs_nodes=[{Node, AuthHPid, RouteHPid} | Nodes]}};
		pang ->
		    format_log(error, "FS_HANDLER(~p): ~p not responding~n", [self(), Node]),
		    {{error, no_connection}, State}
	    end
    end.

rm_fs_node(Node, #state{fs_nodes=Nodes}=State) ->
    case lists:keyfind(Node, 1, Nodes) of
	{Node, AuthHPid, RteHPid} ->
	    AuthRes = case erlang:is_process_alive(AuthHPid) of
			  true ->
			      AuthHPid ! shutdown,
			      format_log(info, "FS_HANDLER(~p): Shutting down auth handler ~p for ~p~n", [self(), AuthHPid, Node]),
			      ok;
			  false ->
			      format_log(error, "FS_HANDLER(~p): Auth Handler ~p already down for ~p~n", [self(), AuthHPid, Node]),
			      {error, auth_handler_down, Node}
		      end,
	    RouteRes = case erlang:is_process_alive(RteHPid) of
			   true ->
			       RteHPid ! shutdown,
			       format_log(info, "FS_HANDLER(~p): Shutting down route handler ~p for ~p~n", [self(), AuthHPid, Node]),
			       ok;
			   false ->
			       format_log(error, "FS_HANDLER(~p): Route Handler ~p already down for ~p~n", [self(), AuthHPid, Node]),
			       {error, route_handler_down, Node}
		       end,
	    {{ok, {Node, AuthRes, RouteRes}}, State#state{fs_nodes=lists:keydelete(Node, 1, Nodes)}};
	false ->
	    format_log(error, "FS_HANDLER(~p): No handlers found for ~p~n", [self(), Node]),
	    {{error, no_node, Node}, State}
    end.

start_auth_handler(Node) ->
    start_handler(Node, ?AUTH_MOD).

start_route_handler(Node) ->
    start_handler(Node, ?ROUTE_MOD).

start_handler(Node, Mod) ->
    Pid = Mod:start_handler(Node),
    link(Pid),
    Pid.

%% setup a connection to mod_erlang_event for directory and dialplan events
-spec(setup_fs_conn(Node :: atom()) -> {pid(), pid()}).
setup_fs_conn(Node) ->
    {start_auth_handler(Node), start_route_handler(Node)}.
