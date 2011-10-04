%%%-----------------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%%
%%% When connecting to a FreeSWITCH node, we create three processes: one to
%%% handle authentication (directory) requests; one to handle route (dialplan)
%%% requests, and one to monitor the node and various stats about the node.
%%%
%%% @end
%%% Created : 08 Oct 2010 by James Aimonetti <james@2600hz.org>
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

-record(state, {
          fs_nodes = [] :: [#node_handler{},...] | []
         ,node_reconnect_pids = [] :: [{atom(), pid()},...] | [] % kill watchers if rm_fs_node is called for Node
         }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% returns ok or {error, some_error_atom_explaining_more}
-spec add_fs_node/1 :: (Node) -> 'ok' | {'error', 'no_connection'} when
      Node :: atom().
-spec add_fs_node/2 :: (Node, Opts) -> 'ok' | {'error', 'no_connection'} when
      Node :: atom(),
      Opts :: proplist().
add_fs_node(Node) -> add_fs_node(Node, []).
add_fs_node(Node, Opts) ->
    gen_server:call(?MODULE, {add_fs_node, Node, Opts}, infinity).

%% returns ok or {error, some_error_atom_explaining_more}
-spec rm_fs_node/1 :: (Node) -> ok | tuple(error, no_node, Node) when
      Node :: atom().
rm_fs_node(Node) ->
    gen_server:call(?MODULE, {rm_fs_node, Node}, infinity).

%% calls all handlers and gets diagnostic info from them
-spec diagnostics/0 :: () -> proplist().
diagnostics() ->
    gen_server:call(?MODULE, {diagnostics}, infinity).

-spec is_node_up/1 :: (Node) -> boolean() when
      Node :: atom().
is_node_up(Node) ->
    gen_server:call(?MODULE, {is_node_up, Node}, infinity).

%% Type - audio | video
%% Options - Proplist
%%   {min_channels_requested, 1}
%%   {max_channels_requested, 1}
%% Returns - Proplist
%%   {max_channels_available, 4}
%%   {bias, 1}
-spec request_resource/2 :: (Type, Options) -> list(proplist()) when
      Type :: binary(),
      Options :: proplist().
request_resource(Type, Options) ->
    gen_server:call(?MODULE, {request_resource, Type, Options}).

-spec request_node/1 :: (Type) -> tuple(ok, atom()) | tuple(error, binary()) when
      Type :: binary().
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
    ?LOG_SYS("starting new fs handler"),
    process_flag(trap_exit, true),

    spawn(fun() ->
		  'ok' = case ecallmgr_config:load_config(?STARTUP_FILE) of
			     ok -> ?LOG("Configs loaded from ~s", [?STARTUP_FILE]);
			     {error, enoent} ->
				 'ok' = ecallmgr_config:write_config(?STARTUP_FILE, ?STARTUP_FILE_CONTENTS),
				 ?LOG("Wrote config file at ~s", [?STARTUP_FILE]),
				 ecallmgr_config:load_config(?STARTUP_FILE)
			 end,
		  [?MODULE:add_fs_node(wh_util:to_atom(N, true)) || N <- ecallmgr_config:fetch(fs_nodes, [])]
	  end),

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
			  ,{amqp_host, amqp_mgr:get_host()}
			 ],
		  gen_server:reply(From, Resp)
	  end),
    {noreply, State};
handle_call({add_fs_node, Node, Options}, _From, State) ->
    ?LOG("trying to add ~s", [Node]),
    {Resp, State1} = add_fs_node(Node, check_options(Options), State),
    {reply, Resp, State1, hibernate};
handle_call({rm_fs_node, Node}, _From, State) ->
    {Resp, State1} = rm_fs_node(Node, State),
    {reply, Resp, State1, hibernate};
handle_call({request_resource, Type, Options}, From, #state{fs_nodes=Nodes}=State) ->
    spawn(fun() ->
		  Resp = process_resource_request(Type, Nodes, Options),
		  gen_server:reply(From, Resp)
	  end),
    {noreply, State};

handle_call(_Request, _From, State) ->
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
handle_info({nodedown, Node}, #state{fs_nodes=Nodes, node_reconnect_pids=ReconPids}=State) ->
    ?LOG_SYS("node ~p has gone down", [Node]),
    WatcherPid = spawn_link(fun() ->
                                    case [N || #node_handler{node=Node1}=N <- Nodes, Node =:= Node1] of
                                        [#node_handler{node=Node, options=Opts}] ->
                                            erlang:monitor_node(Node, false),
                                            _ = ecallmgr_fs_sup:stop_handlers(Node),
                                            ?LOG_SYS("node watch starting for ~p", [Node]),
                                            watch_node_for_restart(Node, Opts);
                                        [] ->
                                            ?LOG_SYS("node watch starting for ~p", [Node]),
                                            watch_node_for_restart(Node, [])
                                    end
                            end),
    ?LOG("Started ~p to watch ~s", [WatcherPid, Node]),
    {noreply, State#state{fs_nodes=lists:keydelete(Node, 2, Nodes), node_reconnect_pids = [{Node, WatcherPid} | ReconPids]}, hibernate};
handle_info({'EXIT', Pid, _Reason}, #state{node_reconnect_pids=ReconPids}=State) ->
    ?LOG_SYS("Pid ~p exited: ~p", [Pid, _Reason]),
    {noreply, State#state{node_reconnect_pids=lists:keydelete(Pid, 2, ReconPids)}};
handle_info(_Info, State) ->
    ?LOG_SYS("Unhandled message: ~p", [_Info]),
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
    ?LOG_SYS("fs handler ~p termination", [_Reason]),
    lists:foreach(fun close_node/1, Nodes).

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
-spec watch_node_for_restart/2 :: (Node, Opts) -> 'ok' | {'error', 'no_connection'} when
      Node :: atom(),
      Opts :: proplist().
watch_node_for_restart(Node, Opts) ->
    watch_node_for_restart(Node, Opts, 250).

-spec watch_node_for_restart/3 :: (Node, Opts, Timeout) -> 'ok' | {'error', 'no_connection'} when
      Node :: atom(),
      Opts :: proplist(),
      Timeout :: pos_integer().
watch_node_for_restart(Node, Opts, Timeout) when Timeout > ?MAX_TIMEOUT_FOR_NODE_RESTART ->
    is_node_up(Node, Opts, ?MAX_TIMEOUT_FOR_NODE_RESTART);
watch_node_for_restart(Node, Opts, ?MAX_TIMEOUT_FOR_NODE_RESTART) ->
    is_node_up(Node, Opts, ?MAX_TIMEOUT_FOR_NODE_RESTART);
watch_node_for_restart(Node, Opts, Timeout) ->
    is_node_up(Node, Opts, Timeout * 2).

-spec is_node_up/3 :: (Node, Opts, Timeout) -> 'ok' | {'error', 'no_connection'} when
      Node :: atom(),
      Opts :: proplist(),
      Timeout :: pos_integer().
is_node_up(Node, Opts, Timeout) ->
    case net_adm:ping(Node) of
	pong ->
	    ?LOG_SYS("node ~s has risen", [Node]),
	    ?MODULE:add_fs_node(Node, Opts);
	pang ->
	    ?LOG_SYS("waiting ~b seconds to ping again", [Timeout div 1000]),
	    receive
		shutdown ->
		    ?LOG_SYS("watcher for ~s asked to go down", [Node])
	    after
		Timeout ->
		    ?LOG_SYS("Pinging ~s again", [Node]),
		    watch_node_for_restart(Node, Opts, Timeout)
	    end
    end.

-spec check_options/1 :: (Opts) -> proplist() when
      Opts :: proplist().
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
-spec diagnostics_query/1 :: (Pid) -> tuple(ok, proplist()) | tuple(error, atom(), term()) when
      Pid :: pid().
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

-spec add_fs_node/3 :: (Node, Options, State) -> {'ok', #state{}} | {{'error', 'no_connection'}, #state{}} when
      Node :: atom(),
      Options :: proplist(),
      State :: #state{}.
add_fs_node(Node, Options, #state{fs_nodes=Nodes}=State) ->
    case [N || #node_handler{node=Node1}=N <- Nodes, Node =:= Node1] of
	[] ->
	    case net_adm:ping(Node) of
		pong ->
		    erlang:monitor_node(Node, true),
		    ?LOG_SYS("no node matching ~p found, adding", [Node]),

		    case lists:all(fun({ok, Pid}) when is_pid(Pid) -> true;
				      ({error, {already_started, Pid}}) when is_pid(Pid) -> true;
				      (_) -> false
				   end, ecallmgr_fs_sup:start_handlers(Node, Options)) of
			true ->
			    {ok, State#state{fs_nodes=[#node_handler{node=Node, options=Options} | Nodes]}};
			false ->
                            self() ! {nodedown, Node},
			    {{error, failed_starting_handlers}, State}
		    end;
		pang ->
		    ?LOG_SYS("node ~p not responding, can't connect", [Node]),
                    self() ! {nodedown, Node},
		    {{error, no_connection}, State}
	    end;
	[#node_handler{node=Node}=N] ->
	    ?LOG_SYS("handlers known for node ~p", [Node]),

	    {_, _, NHP} = Handlers = ecallmgr_fs_sup:get_handler_pids(Node),
	    is_pid(NHP) andalso NHP ! {update_options, Options},

	    case lists:any(fun(error) -> true; (undefined) -> true; (_) -> false end, tuple_to_list(Handlers)) of
		true ->
		    _ = ecallmgr_fs_sup:stop_handlers(Node),
		    ?LOG_SYS("removed handlers for node ~p because something is wonky: handlers: ~p", [Node, Handlers]),
		    add_fs_node(Node, Options, State#state{fs_nodes=lists:keydelete(Node, 2, Nodes)});
		false ->
		    {ok, State#state{fs_nodes=[N#node_handler{options=Options} | lists:keydelete(Node, 2, Nodes)]}}
	    end
    end.

-spec rm_fs_node/2 :: (Node, State) -> {'ok' | {'error', 'no_node', atom()}, #state{}} when
      Node :: atom(),
      State :: #state{}.
rm_fs_node(Node, #state{fs_nodes=Nodes, node_reconnect_pids=ReconPids}=State) ->
    kill_watchers(Node, ReconPids),
    case lists:keyfind(Node, 2, Nodes) of
	false ->
	    ?LOG_SYS("no handlers found for ~s", [Node]),
	    {{error, no_node, Node}, State};
	N ->
	    ?LOG_SYS("closing node handler for ~s", [Node]),
	    {{ok, close_node(N)}, State#state{fs_nodes=lists:keydelete(Node, 2, Nodes)}}
    end.

-spec kill_watchers/2 :: (Node, Watchers) -> 'ok' when
      Node :: atom(),
      Watchers :: [{atom(), pid()},...] | [].
kill_watchers(Node, [{Node, Pid}|Rest]) ->
    Pid ! shutdown,
    kill_watchers(Node, Rest);
kill_watchers(Node, [_|Rest]) ->
    kill_watchers(Node, Rest);
kill_watchers(_, []) ->
    ok.

-spec close_node/1 :: (N) -> ['ok' | {'error', 'not_found' | 'running' | 'simple_one_for_one'},...] when
      N :: #node_handler{}.
close_node(#node_handler{node=Node}) ->
    erlang:monitor_node(Node, false),
    ecallmgr_fs_sup:stop_handlers(Node).

-spec process_resource_request/3 :: (Type, Nodes, Options) -> [proplist(),...] | [] when
      Type :: binary(),
      Nodes :: [#node_handler{},...],
      Options :: proplist().
process_resource_request(<<"audio">> = Type, Nodes, Options) ->
    NodesResp = [begin
		     {_,_,NHP} = ecallmgr_fs_sup:get_handler_pids(Node),
		     NHP ! {resource_request, self(), Type, Options},
		     receive {resource_response, NHP, Resp} -> Resp
		     after 500 -> []
		     end
		 end || #node_handler{node=Node} <- Nodes],
    [ X || X <- NodesResp, X =/= []];
process_resource_request(Type, _Nodes, _Options) ->
    ?LOG_SYS("unhandled resource request type ~p", [Type]),
    [].
