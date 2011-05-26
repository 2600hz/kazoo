
%%%-----------------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, VoIP INC
%%% @doc
%%%
%%%
%%%
%%% @end
%%% Created : 08 Oct 2010 by James Aimonetti <james@2600hz.com>
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_handler).

-behaviour(gen_server).

%% API
-export([start_link/0, add_fs_node/1, add_fs_node/2, rm_fs_node/1, diagnostics/0]).
-export([is_node_up/1]).

%% Resource allotment
-export([request_resource/2, request_node/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).
-define(AUTH_MOD, ecallmgr_fs_auth).
-define(ROUTE_MOD, ecallmgr_fs_route).
-define(NODE_MOD, ecallmgr_fs_node).

-record(node_handler, {node = 'undefined' :: atom()
		       ,options = [] :: proplist()
		      }).

-record(state, {fs_nodes = [] :: list(#node_handler{}) }).

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

is_node_up(Node) ->
    gen_server:call(?MODULE, {is_node_up, Node}, infinity).

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

-spec(request_node/1 :: (Type :: binary()) -> tuple(ok, atom()) | tuple(error, binary())).
request_node(Type) ->
    gen_server:call(?MODULE, {request_node, Type}).

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
    {ok, #state{}, 0}.

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
handle_call({request_node, <<"audio">>}, From, #state{fs_nodes=Nodes}=State) ->
    spawn(fun() ->
		  try
		      {_, Node} = hd(lists:keysort(1, [ {random:uniform(), Node} || #node_handler{node=Node} <- Nodes ])),
		      gen_server:reply(From, {ok, Node})
		  catch
		      _:E -> gen_server:reply(From, {error, E})
		  end
	  end),
    {noreply, State};
handle_call({is_node_up, Node}, _From, #state{fs_nodes=Nodes}=State) ->
    {reply, [ Node1 || #node_handler{node=Node1} <- Nodes, Node1 =:= Node ] =/= [], State};

handle_call({diagnostics}, From, #state{fs_nodes=Nodes}=State) ->
    spawn(fun() ->
		  {KnownNodes, HandlerD} = lists:foldl(fun(#node_handler{node=FSNode}, {KN, HD}) ->
							       {AHP, RHP, NHP} = ecallmgr_fs_sup:get_handler_pids(FSNode),
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
			  ,{version, ?APP_VERSION}
			  ,{known_fs_nodes, KnownNodes}
			  ,{handler_diagnostics, HandlerD}
			  ,{recorded, erlang:now()}
			  ,{amqp_host, amqp_manager:get_host()}
			 ],
		  gen_server:reply(From, Resp)
	  end),
    {noreply, State};
handle_call({add_fs_node, Node, Options}, _From, State) ->
	{Resp, State1} = add_fs_node(Node, check_options(Options), State),
	{reply, Resp, State1};
handle_call({rm_fs_node, Node}, _From, State) ->
    {Resp, State1} = rm_fs_node(Node, State),
    {reply, Resp, State1};
handle_call({request_resource, Type, Options}, From, #state{fs_nodes=Nodes}=State) ->
    spawn(fun() ->
		  Resp = process_resource_request(Type, Nodes, Options),
		  logger:format_log(info, "FS_HANDLER(~p): Resource Resp: ~p~n", [self(), Resp]),
		  gen_server:reply(From, Resp)
	  end),
    {noreply, State};

handle_call(_Request, _From, State) ->
    logger:format_log(error, "FS_HANDLER(~p): Unhandled call ~p~n", [self(), _Request]),
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
handle_info({nodedown, Node}, #state{fs_nodes=Nodes}=State) ->
    case [N || #node_handler{node=Node1}=N <- Nodes, Node =:= Node1] of
	[] ->
	    logger:format_log(info, "FS_HANDLER(~p): Received nodedown for ~p but node is not known~n", [self(), Node]),
	    {noreply, State};
	[#node_handler{node=Node, options=Opts}|_] ->
	    erlang:monitor_node(Node, false),
	    ecallmgr_fs_sup:stop_handlers(Node),

	    spawn_link(fun() ->
			       logger:format_log(info, "FS_HANDLER.node_watch(~p) starting~n", [self()]),
			       watch_node_for_restart(Node, Opts)
		       end),

	    logger:format_log(info, "FS_HANDLER(~p): Node ~p has gone down~n", [self(), Node]),
	    {noreply, State#state{fs_nodes=lists:keydelete(Node, 2, Nodes)}}
    end;
handle_info(timeout, State) ->
    spawn(fun() ->
                  {ok, Startup} = file:consult(?STARTUP_FILE),
                  Nodes = props:get_value(fs_nodes, Startup, []),
                  lists:foreach(fun(Node) -> 
                                        add_fs_node(whistle_util:to_atom(Node, true)) 
                                end, Nodes)
          end),    
    {noreply, State};
handle_info({'EXIT', Pid, normal}, State) ->
    logger:format_log(info, "FS_HANDLER(~p): ~p exited normally.~n", [self(), Pid]),
    {noreply, State};
handle_info({'EXIT', Pid, _Reason}, State) ->
    logger:format_log(info, "FS_HANDLER(~p): ~p exited abnormally: ~p.~n", [self(), Pid, _Reason]),
    {noreply, State};
handle_info(_Info, State) ->
    logger:format_log(info, "FS_HANDLER(~p): Unhandled Info: ~p~n", [self(), _Info]),
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
    logger:format_log(error, "FS_HANDLER(~p): terminating: ~p~n", [Self, _Reason]),
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
    case net_adm:ping(Node) of
	pong ->
	    logger:format_log(info, "FS_HANDLER.is_node_up(~p): ~p has risen~n", [self(), Node]),
	    ?MODULE:add_fs_node(Node, Opts);
	pang ->
	    receive
		shutdown -> logger:format_log(info, "FS_HANDLER.is_node_up(~p): watcher going down for ~p~n", [self(), Node])
	    after
		Timeout ->
		    logger:format_log(info, "FS_HANDLER.is_node_up(~p): trying ~p again, then waiting for ~p secs~n", [self(), Node, Timeout div 1000]),
		    watch_node_for_restart(Node, Opts, Timeout)
	    end
    end.

-spec(check_options/1 :: (Opts :: proplist()) -> proplist()).
check_options([]) ->
    [{bias, 1}, {max_channels, 100}];
check_options(Opts) ->
    Opts0 = case props:get_value(bias, Opts) of
		undefined -> [{bias, 1} | Opts];
		_ -> Opts
	    end,
    case props:get_value(max_channels, Opts0) of
	undefined -> [{max_channels, 100} | Opts0];
	_ -> Opts0
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

-spec(add_fs_node/3 :: (Node :: atom(), Options :: proplist(), State :: #state{}) -> tuple(ok, #state{}) | tuple(tuple(error, no_connection), #state{})).
add_fs_node(Node, Options, #state{fs_nodes=Nodes}=State) ->
    case [N || #node_handler{node=Node1}=N <- Nodes, Node =:= Node1] of
	[] ->
	    case net_adm:ping(Node) of
		pong ->
		    erlang:monitor_node(Node, true),
		    logger:format_log(info, "FS_HANDLER(~p): No node matching ~p found, adding~n", [self(), Node]),

		    case lists:all(fun({ok, Pid}) when is_pid(Pid) -> true;
				      ({error, {already_started, Pid}}) when is_pid(Pid) -> true;
				      (_) -> false
				   end, ecallmgr_fs_sup:start_handlers(Node, Options)) of
			true ->
			    {ok, State#state{fs_nodes=[#node_handler{node=Node, options=Options} | Nodes]}};
			false ->
			    {{error, failed_starting_handlers}, State}
		    end;
		pang ->
		    logger:format_log(error, "FS_HANDLER(~p): ~p not responding~n", [self(), Node]),
		    {{error, no_connection}, State}
	    end;
	[#node_handler{node=Node}=N] ->
	    logger:format_log(info, "FS_HANDLER(~p): handlers known for node ~p~n", [self(), Node]),

	    {_, _, NHP} = Handlers = ecallmgr_fs_sup:get_handler_pids(Node),
	    is_pid(NHP) andalso NHP ! {update_options, Options},

	    case lists:any(fun(error) -> true; (undefined) -> true; (_) -> false end, tuple_to_list(Handlers)) of
		true ->
		    _ = ecallmgr_fs_sup:stop_handlers(Node),
		    logger:format_log(info, "removed handlers cause something is wonky: handlers: ~p", [Handlers]),
		    add_fs_node(Node, Options, State#state{fs_nodes=lists:keydelete(Node, 2, Nodes)});
		false ->
		    {ok, State#state{fs_nodes=[N#node_handler{options=Options} | lists:keydelete(Node, 2, Nodes)]}}
	    end
    end.

-spec(rm_fs_node/2 :: (Node :: atom(), State :: tuple()) -> {ok | tuple(error, no_node, atom()), tuple()}).
rm_fs_node(Node, #state{fs_nodes=Nodes}=State) ->
    case lists:keyfind(Node, 2, Nodes) of
	false ->
	    logger:format_log(error, "FS_HANDLER(~p): No handlers found for ~p~n", [self(), Node]),
	    {{error, no_node, Node}, State};
	N ->
	    logger:format_log(info, "FS_HANDLER(~p): Found node handler: ~p~n", [self(), N]),
	    Res = close_node(N),
	    {{ok, Res}, State#state{fs_nodes=lists:keydelete(Node, 2, Nodes)}}
    end.

-spec(close_node/1 :: (N :: #node_handler{}) -> list(ok | tuple(error, term()))).
close_node(#node_handler{node=Node}) ->
    erlang:monitor_node(Node, false),
    ecallmgr_fs_sup:stop_handlers(Node).

-spec(process_resource_request/3 :: (Type :: binary(), Nodes :: list(#node_handler{}), Options :: proplist()) -> list(proplist()) | []).
process_resource_request(<<"audio">>=Type, Nodes, Options) ->
    NodesResp = [begin
		     {_,_,NHP} = ecallmgr_fs_sup:get_handler_pids(Node),
		     NHP ! {resource_request, self(), Type, Options},
		     receive {resource_response, NHP, Resp} -> Resp
		     after 500 -> []
		     end
		 end || #node_handler{node=Node} <- Nodes],
    [ X || X <- NodesResp, X =/= []];
process_resource_request(Type, _Nodes, _Options) ->
    logger:format_log(info, "FS_HANDLER(~p): Unhandled resource request type ~p~n", [self(), Type]),
    [].
