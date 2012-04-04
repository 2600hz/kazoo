%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2012, VoIP INC
%%% @doc
%%%
%%% When connecting to a FreeSWITCH node, we create three processes: one to
%%% handle authentication (directory) requests; one to handle route (dialplan)
%%% requests, and one to monitor the node and various stats about the node.
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
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
-define(CONFIG_MOD, ecallmgr_fs_config).

-record(node_handler, {node = 'undefined' :: atom()
                       ,options = [] :: proplist()
                      }).

-record(state, {
          fs_nodes = [] :: [#node_handler{},...] | []
         ,node_reconnect_pids = [] :: [{atom(), pid()},...] | [] % kill watchers if rm_fs_node is called for Node
         ,preconfigured_lookup :: pid()
         }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% returns ok or {error, some_error_atom_explaining_more}
-spec add_fs_node/1 :: (atom()) -> 'ok' | {'error', 'no_connection'}.
-spec add_fs_node/2 :: (atom(), proplist()) -> 'ok' | {'error', 'no_connection'}.
add_fs_node(Node) -> add_fs_node(Node, []).
add_fs_node(Node, Opts) ->
    gen_server:call(?MODULE, {add_fs_node, Node, Opts}).

%% returns ok or {error, some_error_atom_explaining_more}
-spec rm_fs_node/1 :: (atom()) -> 'ok'.
rm_fs_node(Node) ->
    gen_server:cast(?MODULE, {rm_fs_node, Node}).

%% calls all handlers and gets diagnostic info from them
-spec diagnostics/0 :: () -> proplist().
diagnostics() ->
    gen_server:call(?MODULE, {diagnostics}).

-spec is_node_up/1 :: (atom()) -> boolean().
is_node_up(Node) ->
    gen_server:call(?MODULE, {is_node_up, Node}).

%% Type - audio | video
%% Options - Proplist
%%   {min_channels_requested, 1}
%%   {max_channels_requested, 1}
%% Returns - Proplist
%%   {max_channels_available, 4}
%%   {bias, 1}
-spec request_resource/2 :: (ne_binary(), proplist()) -> [proplist(),...].
request_resource(Type, Options) ->
    gen_server:call(?MODULE, {request_resource, Type, Options}).

-spec request_node/1 :: (ne_binary()) -> {'ok', atom()} | {'error', ne_binary()}.
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
    put(callid, ?LOG_SYSTEM_ID),
    lager:debug("starting new fs handler"),
    process_flag(trap_exit, true),

    Pid = spawn(fun() -> start_preconfigured_servers() end),

    {ok, #state{preconfigured_lookup=Pid}}.

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
                                                               {AHP, RHP, NHP, _} = ecallmgr_fs_sup:get_handler_pids(FSNode),
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
handle_call({add_fs_node, Node, Options}, {Pid, _}, #state{preconfigured_lookup=Pid}=State) ->
    lager:debug("trying to add ~s", [Node]),
    {Resp, State1} = add_fs_node(Node, check_options(Options), State),
    {reply, Resp, State1, hibernate};
handle_call({add_fs_node, Node, Options}, _From, #state{preconfigured_lookup=Pid}=State) ->
    lager:debug("trying to add ~s", [Node]),
    {Resp, State1} = add_fs_node(Node, check_options(Options), State),
    Pid1 = maybe_stop_preconfigured_lookup(Resp, Pid),
    {reply, Resp, State1#state{preconfigured_lookup=Pid1}, hibernate};
handle_call({request_resource, Type, Options}, From, #state{fs_nodes=Nodes}=State) ->
    spawn(fun() ->
                  Resp = process_resource_request(Type, Nodes, Options),
                  gen_server:reply(From, Resp)
          end),
    {noreply, State};

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

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
handle_cast({rm_fs_node, Node}, State) ->
    {noreply, rm_fs_node(Node, State), hibernate};
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
    lager:warning("node ~p has gone down", [Node]),
    WatcherPid = spawn_link(fun() ->
                                    case [N || #node_handler{node=Node1}=N <- Nodes, Node =:= Node1] of
                                        [#node_handler{node=Node, options=Opts}] ->
                                            erlang:monitor_node(Node, false),
                                            _ = ecallmgr_fs_sup:stop_handlers(Node),
                                            lager:debug("node watch starting for ~p", [Node]),
                                            watch_node_for_restart(Node, Opts);
                                        [] ->
                                            lager:debug("node watch starting for ~p", [Node]),
                                            watch_node_for_restart(Node, [])
                                    end
                            end),
    lager:debug("started ~p to watch ~s", [WatcherPid, Node]),
    {noreply, State#state{fs_nodes=lists:keydelete(Node, 2, Nodes), node_reconnect_pids = [{Node, WatcherPid} | ReconPids]}, hibernate};
handle_info({'EXIT', Pid, _Reason}, #state{node_reconnect_pids=ReconPids}=State) ->
    lager:debug("pid ~p exited: ~p", [Pid, _Reason]),
    {noreply, State#state{node_reconnect_pids=lists:keydelete(Pid, 2, ReconPids)}};
handle_info(_Info, State) ->
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
    lager:debug("fs handler ~p termination", [_Reason]),
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
-spec watch_node_for_restart/2 :: (atom(), proplist()) -> 'ok' | {'error', 'no_connection'}.
-spec watch_node_for_restart/3 :: (atom(), proplist(), pos_integer()) -> 'ok' | {'error', 'no_connection'}.
watch_node_for_restart(Node, Opts) ->
    watch_node_for_restart(Node, Opts, 250).

watch_node_for_restart(Node, Opts, Timeout) when Timeout > ?MAX_TIMEOUT_FOR_NODE_RESTART ->
    is_node_up(Node, Opts, ?MAX_TIMEOUT_FOR_NODE_RESTART);
watch_node_for_restart(Node, Opts, ?MAX_TIMEOUT_FOR_NODE_RESTART) ->
    is_node_up(Node, Opts, ?MAX_TIMEOUT_FOR_NODE_RESTART);
watch_node_for_restart(Node, Opts, Timeout) ->
    is_node_up(Node, Opts, Timeout * 2).

-spec is_node_up/3 :: (atom(), proplist(), pos_integer()) -> 'ok' | {'error', 'no_connection'}.
is_node_up(Node, Opts, Timeout) ->
    case net_adm:ping(Node) of
        pong ->
            lager:info("node ~s has risen", [Node]),
            ?MODULE:add_fs_node(Node, Opts);
        pang ->
            lager:debug("waiting ~b seconds to ping again", [Timeout div 1000]),
            receive
                shutdown ->
                    lager:debug("watcher for ~s asked to go down", [Node])
            after
                Timeout ->
                    lager:debug("Pinging ~s again", [Node]),
                    watch_node_for_restart(Node, Opts, Timeout)
            end
    end.

-spec check_options/1 :: (proplist()) -> proplist().
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
-spec diagnostics_query/1 :: (pid()) -> {'ok', proplist()} | {'error', 'timed_out' | 'not_responding', 'handler_busy' | 'handler_down'}.
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

-spec add_fs_node/3 :: (atom(), proplist(), #state{}) -> {'ok', #state{}} | {{'error', 'no_connection'}, #state{}}.
add_fs_node(Node, Options, #state{fs_nodes=Nodes}=State) ->
    case [N || #node_handler{node=Node1}=N <- Nodes, Node =:= Node1] of
        [] ->
            case net_adm:ping(Node) of
                pong ->
                    erlang:monitor_node(Node, true),
                    lager:debug("no node matching ~p found, adding", [Node]),

                    case lists:all(fun({ok, Pid}) when is_pid(Pid) -> true;
                                      ({error, {already_started, Pid}}) when is_pid(Pid) -> true;
                                      (_) -> false
                                   end, ecallmgr_fs_sup:start_handlers(Node, Options)) of
                        true ->
                            lager:info("successfully connected to node '~s'", [Node]),
                            {ok, State#state{fs_nodes=[#node_handler{node=Node, options=Options} | Nodes]}};
                        false ->
                            lager:warning("failed to start all handlers for node '~s'", [Node]),
                            self() ! {nodedown, Node},
                            {{error, failed_starting_handlers}, State}
                    end;
                pang ->
                    lager:info("unable to connect to node '~s'; ensure it is reachable from this server and using cookie '~s'", [Node, erlang:get_cookie()]),
                    self() ! {nodedown, Node},
                    {{error, no_connection}, State}
            end;
        [#node_handler{node=Node}=N] ->
            lager:debug("handlers known for node ~p", [Node]),

            {_, _, NHP, _} = Handlers = ecallmgr_fs_sup:get_handler_pids(Node),
            is_pid(NHP) andalso NHP ! {update_options, Options},

            case lists:any(fun(error) -> true; (undefined) -> true; (_) -> false end, tuple_to_list(Handlers)) of
                true ->
                    _ = ecallmgr_fs_sup:stop_handlers(Node),
                    lager:debug("removed handlers for node ~p because something is wonky: handlers: ~p", [Node, Handlers]),
                    add_fs_node(Node, Options, State#state{fs_nodes=lists:keydelete(Node, 2, Nodes)});
                false ->
                    {ok, State#state{fs_nodes=[N#node_handler{options=Options} | lists:keydelete(Node, 2, Nodes)]}}
            end
    end.

-spec rm_fs_node/2 :: (atom(), #state{}) -> #state{}.
rm_fs_node(Node, #state{fs_nodes=Nodes, node_reconnect_pids=ReconPids}=State) ->
    kill_watchers(Node, ReconPids),
    case lists:keyfind(Node, 2, Nodes) of
        false ->
            lager:debug("no handlers found for ~s", [Node]),
            State;
        N ->
            lager:debug("closing node handler for ~s", [Node]),
            _ = close_node(N),
            State#state{fs_nodes=lists:keydelete(Node, 2, Nodes)}
    end.

-spec kill_watchers/2 :: (atom(), [{atom(), pid()},...] | []) -> 'ok'.
kill_watchers(Node, [{Node, Pid}|Rest]) ->
    Pid ! shutdown,
    kill_watchers(Node, Rest);
kill_watchers(Node, [_|Rest]) ->
    kill_watchers(Node, Rest);
kill_watchers(_, []) ->
    ok.

-spec close_node/1 :: (#node_handler{}) -> ['ok' | {'error', 'not_found' | 'running' | 'simple_one_for_one'},...].
close_node(#node_handler{node=Node}) ->
    erlang:monitor_node(Node, false),
    ecallmgr_fs_sup:stop_handlers(Node).

-spec process_resource_request/3 :: (ne_binary(), [#node_handler{},...], wh_proplist()) -> [wh_proplist(),...] | [].
process_resource_request(<<"audio">> = Type, Nodes, Options) ->
    NodesResp = [begin
                     {_,_,NHP,_} = ecallmgr_fs_sup:get_handler_pids(Node),
                     NHP ! {resource_request, self(), Type, Options},
                     receive {resource_response, NHP, Resp} -> Resp
                     after 500 -> []
                     end
                 end || #node_handler{node=Node} <- Nodes],
    [ X || X <- NodesResp, X =/= []];
process_resource_request(Type, _Nodes, _Options) ->
    lager:debug("unhandled resource request type ~p", [Type]),
    [].

start_preconfigured_servers() ->
    put(callid, ?LOG_SYSTEM_ID),

    case ecallmgr_config:get(<<"fs_nodes">>, []) of
        [] ->
            lager:debug("no preconfigured servers, waiting then trying again"),
            lager:info("no preconfigured servers available. Is the sysconf whapp running?"),

            timer:sleep(5000),
            start_preconfigured_servers();
        Nodes when is_list(Nodes) ->
            lager:debug("nodes retrieved, adding..."),
            lager:info("successfully retrieved FreeSWITCH nodes to connect with, doing so..."),

            [?MODULE:add_fs_node(wh_util:to_atom(N, true)) || N <- Nodes];
        _E ->
            lager:debug("recieved a non-list for fs_nodes: ~p", [_E]),
            timer:sleep(5000),
            start_preconfigured_servers()
    end.

-spec maybe_stop_preconfigured_lookup/2 :: ('ok' | {'error', _}, pid() | 'undefined') -> pid() | 'undefined'.
maybe_stop_preconfigured_lookup(_, undefined) -> undefined;
maybe_stop_preconfigured_lookup(ok, Pid) ->
    case is_pid(Pid) andalso is_process_alive(Pid) of
        true ->
            exit(Pid, kill),
            undefined;
        false ->
            undefined
    end;
maybe_stop_preconfigured_lookup(_, Pid) ->
    Pid.
