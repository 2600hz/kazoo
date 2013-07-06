%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz INC
%%% @doc
%%%
%%% When connecting to a FreeSWITCH node, we create three processes: one to
%%% handle authentication (directory) requests; one to handle route (dialplan)
%%% requests, and one to monitor the node and various stats about the node.
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_nodes).

-behaviour(gen_server).

-export([start_link/0]).
-export([connected/0]).
-export([all_nodes_connected/0]).
-export([add/1, add/2, add/3]).
-export([remove/1]).
-export([nodeup/1]).
-export([is_node_up/1]).
-export([status/0]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).
-define(EXPIRE_CHECK, 60000).

-record(node, {node :: atom()
               ,cookie :: atom()
               ,connected = 'false' :: boolean()
               ,client_version :: api_binary()
               ,options = [] :: wh_proplist()
              }).
-type fs_node() :: #node{}.

-record(state, {nodes = dict:new() :: dict() %fs_nodes()
                ,self = self() :: pid()
                ,preconfigured_lookup :: pid()
               }).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> startlink_ret().
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% returns 'ok' or {'error', some_error_atom_explaining_more}
-spec add(atom()) -> 'ok' | {'error', 'no_connection'}.
-spec add(atom(), wh_proplist() | atom()) -> 'ok' | {'error', 'no_connection'}.
-spec add(atom(), atom(), wh_proplist() | atom()) -> 'ok' | {'error', 'no_connection'}.

add(Node) -> add(Node, []).

add(Node, Opts) when is_list(Opts) -> add(Node, erlang:get_cookie(), Opts);
add(Node, Cookie) when is_atom(Cookie) -> add(Node, Cookie, [{'cookie', Cookie}]).

add(Node, Cookie, Opts) ->
    gen_server:call(?MODULE, {'add_fs_node', Node, Cookie, [{'cookie', Cookie}
                                                            | props:delete('cookie', Opts)
                                                           ]}, 60000).

-spec nodeup(atom()) -> 'ok'.
nodeup(Node) -> gen_server:cast(?MODULE, {'fs_nodeup', Node}).

%% returns 'ok' or {'error', some_error_atom_explaining_more}
-spec remove(atom()) -> 'ok'.
remove(Node) -> gen_server:cast(?MODULE, {'rm_fs_node', Node}).

-spec connected() -> [atom(),...] | [].
connected() -> gen_server:call(?MODULE, 'connected_nodes').

-spec is_node_up(atom()) -> boolean().
is_node_up(Node) -> gen_server:call(?MODULE, {'is_node_up', Node}).

-spec status() -> 'ok'.
status() ->
    _ = [begin
             NodeName = Node#node.node,
             io:format("Node: ~s~n", [NodeName]),
             io:format("Cookie: ~s~n", [Node#node.cookie]),
             io:format("Client: ~s~n", [Node#node.client_version]),
             io:format("Connected: ~p~n", [Node#node.connected]),
             io:format("Options: ~p~n", [Node#node.options]),
             _ = case ecallmgr_fs_pinger_sup:find_pinger(NodeName) of
                     'undefined' -> 'ok';
                     PingerPid ->
                         io:format("Pinger: ~p~n", [PingerPid])
                 end,
             _ = case ecallmgr_fs_sup:find_node(NodeName) of
                   'undefined' -> 'ok';
                   NodeSupPid ->
                         io:format("Supervisor: ~p~n", [NodeSupPid]),
                         io:format("Workers:~n", []),
                         [begin
                              io:format("    ~p (~s)~n", [Pid, Name])
                          end
                          || {Name, Pid, _, _} <- supervisor:which_children(NodeSupPid)
                         ]
                 end,
             io:format("~n", [])
         end
         || {_, Node} <- gen_server:call(?MODULE, 'nodes')
        ],
    'ok'.

-spec all_nodes_connected() -> boolean().
all_nodes_connected() ->
    length(ecallmgr_config:get(<<"fs_nodes">>, [])) =:= length(connected()).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {'ok', State} |
%%                     {'ok', State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    put('callid', ?LOG_SYSTEM_ID),
    process_flag('trap_exit', 'true'),
    lager:debug("starting new fs handler"),
    _ = spawn_link(fun() -> start_preconfigured_servers() end),
    _ = ets:new('sip_subscriptions', ['set', 'public', 'named_table', {'keypos', #sip_subscription.key}]),
    _ = erlang:send_after(?EXPIRE_CHECK, self(), 'expire_sip_subscriptions'),
    {'ok', #state{}}.

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
%% #state{nodes=[{FSNode, HandlerPid}]}
%%--------------------------------------------------------------------
handle_call({'is_node_up', Node}, _From, #state{nodes=Nodes}=State) ->
    Resp = case dict:find(Node, Nodes) of
               'error' -> 'false';
               {'ok', #node{connected=Connected}} ->
                   Connected
           end,
    {'reply', Resp, State};
handle_call('connected_nodes', _From, #state{nodes=Nodes}=State) ->
    Resp = [Node
             || {_, #node{node=Node, connected=Connected}} <- dict:to_list(Nodes)
                ,Connected
           ],
    {'reply', Resp, State};
handle_call({'add_fs_node', NodeName, Cookie, Options}, From, State) ->
    spawn(fun() ->
                  Reply = maybe_add_node(NodeName, Cookie, Options, State),
                  gen_server:reply(From, Reply)
          end),
    {'noreply', State};
handle_call('nodes', _From, #state{nodes=Nodes}=State) ->
    {'reply', dict:to_list(Nodes), State};
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
handle_cast({'fs_nodeup', NodeName}, State) ->
    spawn(fun() -> maybe_handle_nodeup(NodeName, State) end),
    {'noreply', State};
handle_cast({'update_node', #node{node=NodeName, connected=Connected}=Node}
            ,#state{nodes=Nodes}=State) ->
    erlang:monitor_node(NodeName, Connected),
    {'noreply', State#state{nodes=dict:store(NodeName, Node, Nodes)}};
handle_cast({'remove_node', #node{node=NodeName}}, #state{nodes=Nodes}=State) ->
    erlang:monitor_node(NodeName, 'false'),
    {'noreply', State#state{nodes=dict:erase(NodeName, Nodes)}};
handle_cast({'rm_fs_node', NodeName}, State) ->
    spawn(fun() -> maybe_rm_fs_node(NodeName, State) end),
    {'noreply', State};
handle_cast(_Cast, State) ->
    lager:debug("unhandled cast: ~p", [_Cast]),
    {'noreply', State, 'hibernate'}.

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
handle_info('expire_sip_subscriptions', Cache) ->
    Now = wh_util:current_tstamp(),
    DeleteSpec = [{#sip_subscription{expires = '$1', timestamp = '$2', _ = '_'},
                   [{'>', {'const', Now}, {'+', '$2', '$1'}}],
                   ['true']}
                 ],
    ets:select_delete('sip_subscriptions', DeleteSpec),
    _ = erlang:send_after(?EXPIRE_CHECK, self(), 'expire_sip_subscriptions'),
    {'noreply', Cache};
handle_info({'nodedown', NodeName}, State) ->
    spawn(fun() -> maybe_handle_nodedown(NodeName, State) end),
    {'noreply', State};
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
    ets:delete('sip_subscriptions'),
    ets:delete(?CHANNELS_TBL),
    lager:debug("fs nodes termination: ~p", [ _Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {'ok', NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec maybe_handle_nodeup(fs_node(), state()) -> 'ok'.
maybe_handle_nodeup(NodeName, #state{nodes=Nodes}=State) ->
    case dict:find(NodeName, Nodes) of
        {'ok', #node{connected='false'}=Node} ->
            handle_nodeup(Node, State);
        _Else -> 'ok'
    end.

-spec maybe_handle_nodedown(fs_node(), state()) -> 'ok'.
maybe_handle_nodedown(NodeName, #state{nodes=Nodes}=State) ->
    case dict:find(NodeName, Nodes) of
        {'ok', #node{connected='true'}=Node} ->
            handle_nodedown(Node, State);
        _Else -> 'ok'
    end.

-spec maybe_add_node(text(), text(), wh_proplist(), state()) -> 'ok' | {'error', _}.
maybe_add_node(NodeName, Cookie, Options, #state{self=Srv, nodes=Nodes}) ->
    case dict:find(NodeName, Nodes) of
        {'ok', #node{}} -> {'error', 'node_exists'};
        'error' ->
            Node = create_node(NodeName, Cookie, Options),
            case maybe_connect_to_node(Node) of
                {'error', _}=E ->
                    _ = gen_listener:cast(Srv, {'update_node', Node#node{connected='false'}}),
                    _ = maybe_start_node_pinger(Node),
                    E;
                'ok' ->
                    gen_listener:cast(Srv, {'update_node', Node#node{connected='true'}}),
                    'ok'
            end
    end.

-spec maybe_rm_fs_node(atom(), state()) -> 'ok'.
maybe_rm_fs_node(NodeName, #state{nodes=Nodes}=State) ->
    case dict:find(NodeName, Nodes) of
        'error' -> 'ok';
        {'ok', #node{}=Node} ->
            rm_fs_node(Node, State)
    end.

-spec rm_fs_node(fs_node(), state()) -> 'ok'.
rm_fs_node(#node{}=Node, #state{self=Srv}) ->
    _ = maybe_disconnect_from_node(Node),
    gen_listener:cast(Srv, {'remove_node', Node}).

-spec handle_nodeup(fs_node(), state()) -> 'ok'.
handle_nodeup(#node{}=Node, #state{self=Srv}) ->
    NewNode = get_fs_client_version(Node),
    case maybe_connect_to_node(NewNode) of
        {'error', _} ->
            _ = gen_listener:cast(Srv, {'update_node', Node#node{connected='false'}}),
            _ = maybe_start_node_pinger(Node),
            'ok';
        'ok' ->
            gen_listener:cast(Srv, {'update_node', NewNode#node{connected='true'}})
    end.

-spec handle_nodedown(fs_node(), state()) -> 'ok'.
handle_nodedown(#node{node=NodeName}=Node, #state{self=Srv}) ->
    lager:critical("recieved node down notice for ~s", [NodeName]),
    _ = maybe_disconnect_from_node(Node),
    case maybe_connect_to_node(Node) of
        {'error', _} ->
            _ = gen_listener:cast(Srv, {'update_node', Node#node{connected='false'}}),
            _ = maybe_start_node_pinger(Node),
            'ok';
        'ok' ->
            gen_listener:cast(Srv, {'update_node', Node#node{connected='true'}})
    end.

-spec maybe_connect_to_node(fs_node()) -> 'ok' | {'error', _}.
maybe_connect_to_node(#node{node=NodeName}=Node) ->
    lager:debug("attempting to connect to freeswitch node ~s", [NodeName]),
    case maybe_ping_node(Node) of
        {'error', _R}=E -> E;
        'ok' ->
            lager:notice("successfully connected to freeswitch node ~s", [NodeName]),
            'ok'
    end.

-spec maybe_ping_node(fs_node()) -> 'ok' | {'error', _}.
maybe_ping_node(#node{node=NodeName, cookie=Cookie}=Node) ->
    erlang:set_cookie(NodeName, Cookie),
    case net_adm:ping(NodeName) of
        'pong' ->
            _ = ecallmgr_fs_pinger_sup:remove_node(NodeName),
            maybe_start_node_handlers(Node);
        _Else ->
            lager:warning("unable to connect to node '~s'; ensure it is reachable from this server and using cookie '~s'", [NodeName, Cookie]),
            {'error', 'no_connection'}
    end.

-spec maybe_start_node_handlers(fs_node()) -> 'ok' | {'error', _}.
maybe_start_node_handlers(#node{node=NodeName
                                ,client_version=Version
                                ,cookie=Cookie
                                ,options=Props
                               }=Node) ->
    try ecallmgr_fs_sup:add_node(NodeName, [{'cookie', Cookie}
                                            ,{'client_version', Version}
                                            | props:delete('cookie', Props)
                                           ])
    of
        {'ok', _} -> initialize_node_connection(Node);
        {'error', {'already_started', _}} -> 'ok';
        {'error', _R}=E ->
            lager:warning("unable to start node ~s handlers: ~-255p", [NodeName, _R]),
            E;
        'timeout' ->
            lager:warning("connection timeout while starting node ~s handlers", [NodeName]),
            {'error', 'timeout'};
        _Else ->
            lager:warning("unexpected result trying to start ~s node handlers: ~-255p", [NodeName, _Else]),
            {'error', 'failed_starting_handlers'}
    catch
        _:Reason ->
            ST = erlang:get_stacktrace(),
            lager:warning("exception starting node ~s handlers: ~p", [NodeName, Reason]),
            wh_util:log_stacktrace(ST),
            {'error', Reason}
    end.

-spec initialize_node_connection(fs_node()) -> 'ok'.
initialize_node_connection(#node{}=Node) ->
    start_node_stats(Node).

maybe_disconnect_from_node(#node{node=NodeName, connected='true'}=Node) ->
    lager:warning("disconnected from node ~s", [NodeName]),
    _ = close_node(Node),
    reset_node_stats(Node);
maybe_disconnect_from_node(#node{connected='false'}) ->
    'ok'.

-spec maybe_start_node_pinger(fs_node()) -> 'ok'.
maybe_start_node_pinger(#node{node=NodeName, options=Props}=Node) ->
    case ecallmgr_fs_pinger_sup:add_node(NodeName, Props) of
        {'ok', _} -> 'ok';
        {'error', {'already_started', _}} -> 'ok';
        {'error', 'already_present'} ->
            _ = ecallmgr_fs_pinger_sup:remove_node(NodeName),
            maybe_start_node_pinger(Node);
        _Else ->
            lager:critical("failed to start fs pinger for node '~s': ~p", [NodeName, _Else])
    end.

-spec close_node(fs_node()) -> 'ok' | {'error','not_found' | 'running' | 'simple_one_for_one'}.
close_node(#node{node=NodeName}) ->
    _ = ecallmgr_fs_sup:remove_node(NodeName),
    ecallmgr_fs_pinger_sup:remove_node(NodeName).

-spec create_node(text(), text(), wh_proplist()) -> fs_node().
create_node(NodeName, Cookie, Options) when not is_atom(NodeName) ->
    create_node(wh_util:to_atom(NodeName, 'true'), Cookie, Options);
create_node(NodeName, Cookie, Options) when not is_atom(Cookie) ->
    create_node(NodeName, wh_util:to_atom(Cookie, 'true'), Options);
create_node(NodeName, Cookie, Options) ->
    #node{node=NodeName
          ,cookie=get_fs_cookie(Cookie, Options)
          ,client_version=get_fs_client_version(NodeName)
          ,options=Options
         }.

-spec get_fs_cookie(atom(), wh_proplist()) -> atom().
get_fs_cookie('undefined', Props) ->
    wh_util:to_atom(props:get_value('cookie', Props, erlang:get_cookie()));
get_fs_cookie(Cookie, _) when is_atom(Cookie) ->
    Cookie.

-spec get_fs_client_version(fs_node()) -> fs_node();
                           (atom()) -> api_binary().
get_fs_client_version(#node{node=NodeName}=Node) ->
    Node#node{client_version=get_fs_client_version(NodeName)};
get_fs_client_version(NodeName) ->
    case freeswitch:version(NodeName) of
        {'ok', Version} ->
            lager:debug("got freeswitch erlang client version: ~s", [Version]),
            Version;
        _Else ->
            lager:debug("unable to get freeswitch client version: ~p", [_Else]),
            'undefined'
    end.

-spec reset_node_stats(fs_node()) -> 'ok'.
reset_node_stats(#node{node=NodeName}) ->
    NodeBin = amqp_util:encode(wh_util:to_binary(NodeName)),
    wh_gauge:set(<<"freeswitch.nodes.", NodeBin/binary, ".up">>, 0),
    wh_timer:delete(<<"freeswitch.nodes.", NodeBin/binary, ".uptime">>),
    'ok'.

-spec start_node_stats(fs_node()) -> 'ok'.
start_node_stats(#node{node=NodeName}) ->
    NodeBin = amqp_util:encode(wh_util:to_binary(NodeName)),
    wh_gauge:set(<<"freeswitch.nodes.", NodeBin/binary, ".up">>, 1),
    wh_timer:update(<<"freeswitch.nodes.", NodeBin/binary, ".uptime">>),
    wh_timer:update(<<"freeswitch.nodes.", NodeBin/binary, ".last_connected">>),
    'ok'.

start_preconfigured_servers() ->
    put('callid', ?LOG_SYSTEM_ID),
    case ecallmgr_config:get(<<"fs_nodes">>) of
        [] ->
            lager:info("no preconfigured servers available. Is the sysconf whapp running?"),
            timer:sleep(5000),
            _ = ecallmgr_config:flush(<<"fs_nodes">>),
            start_preconfigured_servers();
        Nodes when is_list(Nodes) ->
            lager:info("successfully retrieved FreeSWITCH nodes to connect with, doing so..."),
            [spawn(fun() -> start_node_from_config(N) end) || N <- Nodes];
        _E ->
            lager:debug("recieved a non-list for fs_nodes: ~p", [_E]),
            timer:sleep(5000),
            _ = ecallmgr_config:flush(<<"fs_nodes">>),
            start_preconfigured_servers()
    end.

start_node_from_config(MaybeJObj) ->
    case wh_json:is_json_object(MaybeJObj) of
        'false' -> ?MODULE:add(wh_util:to_atom(MaybeJObj, 'true'));
        'true' ->
            {[Cookie], [Node]} = wh_json:get_values(MaybeJObj),
            ?MODULE:add(wh_util:to_atom(Node, 'true'), wh_util:to_atom(Cookie, 'true'))
    end.
