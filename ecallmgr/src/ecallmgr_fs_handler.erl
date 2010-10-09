%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Centralize the fetch_handler methods, leaving the lookup pieces in 
%%% the auth and route modules
%%% @end
%%% Created : 08 Oct 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_handler).

-behaviour(gen_server).

%% API
-export([start_link/0, add_fs_node/1, rm_fs_node/1, diagnostics/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(proplists, [get_value/2, get_value/3]).
-import(logger, [log/2, format_log/3]).

-include("../include/amqp_client/include/amqp_client.hrl").
-include("freeswitch_xml.hrl").
-include("whistle_api.hrl").

-define(SERVER, ?MODULE). 
-define(AUTH_MOD, ecallmgr_auth).
-define(ROUTE_MOD, ecallmgr_route).

%% fs_nodes = [{FSNode, AuthHandlerPid, RouteHandlerPid}]
-record(state, {fs_nodes=[]}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% returns ok or {error, some_error_atom_explaining_more}
add_fs_node(Node) ->
    gen_server:call(?MODULE, {add_fs_node, Node}).

%% returns ok or {error, some_error_atom_explaining_more}
rm_fs_node(Node) ->
    gen_server:call(?MODULE, {rm_fs_node, Node}).

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
						 AuthHPid ! {diagnostics, self()},
						 AuthHandlerD = receive
								X -> X
							    after
								500 -> {error, timed_out, handler_busy}
							    end,
						 RouteHPid ! {diagnostics, self()},
						 RteHandlerD = receive
								Y -> Y
							    after
								500 -> {error, timed_out, handler_busy}
							    end,
						 {[FSNode | KN], [{FSNode, AuthHandlerD, RteHandlerD} | HD]}
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
	    format_log(error, "FS_HANDLER(~p): Auth Handler EXITed(~p) for ~p, restarting handler...~n", [self(), Reason, HPid]),
	    APid = start_auth_handler(Node),
	    {noreply, State#state{fs_nodes=[{Node, APid, RouteHPid} | lists:keydelete(Node, 1, Nodes)]}};
	false ->
	    case lists:keyfind(HPid, 3, Nodes) of
		{Node, AuthHPid, HPid} ->
		    format_log(error, "FS_HANDLER(~p): Route Handler EXITed(~p) for ~p, restarting handler...~n", [self(), Reason, HPid]),
		    RPid = start_route_handler(Node),
		    {noreply, State#state{fs_nodes=[{Node, AuthHPid, RPid} | lists:keydelete(Node, 1, Nodes)]}};
		false ->
		    format_log(error, "FS_HANDLER(~p): Received EXIT(~p) for unknown ~p~n", [self(), Reason, HPid]),
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
