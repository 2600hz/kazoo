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
-export([sip_url/1]).
-export([sip_external_ip/1]).
-export([summary/0]).
-export([details/0
         ,details/1
        ]).

-export([has_capability/2
         ,set_capability/3
         ,add_capability/2
         ,get_capability/2, get_capabilities/1, get_capabilities/2
        ]).

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

-record(capability, {node :: atom()
                     ,name :: ne_binary()
                     ,module :: ne_binary()
                     ,is_loaded = 'false' :: boolean()
                    }).
-type capability() :: #capability{}.
-type capabilities() :: [capability(),...] | [].

-define(CAPABILITY_TBL, 'ecallmgr_fs_node_capabilities').

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

-spec sip_url(text()) -> api_binary().
sip_url(Node) when not is_atom(Node) ->
    sip_url(wh_util:to_atom(Node, 'true'));
sip_url(Node) ->
    case [ecallmgr_fs_node:sip_url(Srv)
          || Srv <- gproc:lookup_pids({'p', 'l', 'fs_node'})
                 ,ecallmgr_fs_node:fs_node(Srv) =:= Node
         ]
    of
        [URL|_] -> URL;
        _Else -> 'undefined'
    end.

-spec sip_external_ip(text()) -> api_binary().
sip_external_ip(Node) when not is_atom(Node) ->
    sip_external_ip(wh_util:to_atom(Node, 'true'));
sip_external_ip(Node) ->
    case [ecallmgr_fs_node:sip_external_ip(Srv)
          || Srv <- gproc:lookup_pids({'p', 'l', 'fs_node'})
                 ,ecallmgr_fs_node:fs_node(Srv) =:= Node
         ]
    of
        [IP|_] -> IP;
        _Else -> 'undefined'
    end.

-spec all_nodes_connected() -> boolean().
all_nodes_connected() ->
    length(ecallmgr_config:get(<<"fs_nodes">>, [])) =:= length(connected()).

-spec summary() -> 'ok'.
summary() ->
    print_summary(gen_server:call(?MODULE, 'nodes')).

-spec details() -> 'ok'.
-spec details(text()) -> 'ok'.

details() ->
    print_details(gen_server:call(?MODULE, 'nodes')).

details(NodeName) when not is_atom(NodeName) ->
    details(wh_util:to_atom(NodeName, 'true'));
details(NodeName) ->
    case gen_server:call(?MODULE, {'node', NodeName}) of
        {'error', 'not_found'} ->
            io:format("Node ~s not found!~n", [NodeName]);
        {'ok', Node} ->
            details([{'undefined', Node}])
    end.

-spec has_capability(atom(), ne_binary()) -> boolean().
has_capability(Node, Capability) ->
    MatchSpec = [{#capability{node='$1'
                              ,name='$2'
                              ,is_loaded='$3'
                              ,_='_'
                             }
                  ,[{'=:=', '$1', Node}
                    ,{'=:=', '$2', Capability}
                   ]
                  ,['$3']
                 }],
    case ets:select(?CAPABILITY_TBL, MatchSpec) of
        [] -> 'false';
        [Loaded] -> Loaded
    end.

-spec remove_capabilities(atom()) -> non_neg_integer().
remove_capabilities(Node) ->
    MatchSpec = [{#capability{node='$1'
                              ,_='_'
                             }
                  ,[{'=:=', '$1', Node}]
                  ,['true']
                 }],
    ets:select_delete(?CAPABILITY_TBL, MatchSpec).

-spec remove_capability(atom(), ne_binary()) -> non_neg_integer().
remove_capability(Node, Name) ->
    MatchSpec = [{#capability{node='$1'
                              ,name='$2'
                              ,_='_'
                             }
                  ,[{'=:=', '$1', Node}
                    ,{'=:=', '$2', Name}
                   ]
                  ,['true']
                 }],
    ets:select_delete(?CAPABILITY_TBL, MatchSpec).


-spec get_capability(atom(), ne_binary()) ->
                            capability() | api_object().
-spec get_capability(atom(), ne_binary(), 'json' | 'record') ->
                            capability() | api_object().
get_capability(Node, Capability) ->
    get_capability(Node, Capability, 'json').
get_capability(Node, Capability, Format) ->
    MatchSpec = [{#capability{node='$1'
                              ,name='$2'
                              ,_='_'
                             }
                  ,[{'=:=', '$1', Node}
                    ,{'=:=', '$2', Capability}
                   ]
                  ,['$_']
                 }],
    format_capability(Format, ets:select(?CAPABILITY_TBL, MatchSpec)).

-spec get_capabilities(atom()) ->
                              wh_json:objects() | capabilities().
-spec get_capabilities(atom(), 'json' | 'record') ->
                              wh_json:objects() | capabilities().
get_capabilities(Node) ->
    get_capabilities(Node, 'json').

get_capabilities(Node, Format) ->
    MatchSpec = [{#capability{node='$1'
                              ,_='_'
                             }
                  ,[{'=:=', '$1', Node}]
                  ,['$_']
                 }],
    format_capabilities(Format, ets:select(?CAPABILITY_TBL, MatchSpec)).

-spec format_capabilities('json' | 'record', capabilities()) -> capabilities() | wh_json:objects().
format_capabilities('record', Results) -> Results;
format_capabilities('json', Results) ->
    [capability_to_json(Result) || Result <- Results].

-spec format_capability('json' | 'record', [capability()]) -> api_object() | capability().
format_capability('record', [Capability]) -> Capability;
format_capability('json', [Capability]) -> capability_to_json(Capability);
format_capability(_, []) -> 'undefined'.

-spec set_capability(atom(), ne_binary(), boolean()) -> 'ok'.
set_capability(Node, Capability, Toggle) when is_boolean(Toggle) ->
    gen_listener:call(?MODULE, {'set_capability', Node, Capability, Toggle}).

-spec add_capability(atom(), wh_json:object()) -> 'ok'.
add_capability(Node, Capability) ->
    gen_listener:call(?MODULE, {'add_capability', Node, Capability}).

capability_to_json(#capability{node=Node
                               ,name=Capability
                               ,module=Module
                               ,is_loaded=IsLoaded
                              }) ->
    wh_json:from_list([{<<"node">>, wh_util:to_binary(Node)}
                       ,{<<"capability">>, Capability}
                       ,{<<"module">>, Module}
                       ,{<<"is_loaded">>, IsLoaded}
                      ]).

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
    _ = ets:new(?CAPABILITY_TBL, ['bag', 'protected', 'named_table', {'keypos', #capability.node}]),
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
                  try maybe_add_node(NodeName, Cookie, Options, State) of
                      Reply ->
                          gen_server:reply(From, Reply)
                  catch
                      _E:R ->
                          lager:debug("failed to add fs node ~s(~s): ~s: ~p", [NodeName, Cookie, _E, R]),
                          gen_server:reply(From, R)
                  end
          end),
    {'noreply', State};
handle_call('nodes', _From, #state{nodes=Nodes}=State) ->
    {'reply', dict:to_list(Nodes), State};
handle_call({'node', Node}, _From, #state{nodes=Nodes}=State) ->
    case dict:find(Node, Nodes) of
        'error' -> {'reply', {'error', 'not_found'}, State};
        {'ok', #node{}=N} -> {'reply', {'ok', N}, State}
    end;
handle_call({'add_capability', Node, Capability}, _, State) ->
    ets:insert(?CAPABILITY_TBL, #capability{node=Node
                                            ,name=wh_json:get_value(<<"capability">>, Capability)
                                            ,module=wh_json:get_value(<<"module">>, Capability)
                                            ,is_loaded=wh_json:is_true(<<"is_loaded">>, Capability)
                                           }),
    {'reply', 'ok', State};
handle_call({'set_capability', Node, Name, Toggle}, _, State) ->
    case get_capability(Node, Name, 'record') of
        'undefined' -> {'reply', {'error', 'no_capability'}, State};
        #capability{}=Capability ->
            ets:delete_object(?CAPABILITY_TBL, Capability),
            ets:insert(?CAPABILITY_TBL, Capability#capability{is_loaded=Toggle}),
            {'reply', 'ok', State}
    end;

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
    remove_capabilities(NodeName),
    {'noreply', State#state{nodes=dict:erase(NodeName, Nodes)}};
handle_cast({'remove_capabilities', NodeName}, State) ->
    _Rm = remove_capabilities(NodeName),
    lager:debug("removed ~p capabilities from ~s", [_Rm, NodeName]),
    {'noreply', State};
handle_cast({'rm_fs_node', NodeName}, State) ->
    LogId = get('callid'),
    spawn(fun() ->
                  put('callid', LogId),
                  maybe_rm_fs_node(NodeName, State)
          end),
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

-spec maybe_add_node(text(), text(), wh_proplist(), state()) ->
                            'ok' | {'error', _}.
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
        'error' -> close_node(#node{node=NodeName});
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
    gen_server:cast(Srv, {'remove_capabilities', NodeName}),
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
maybe_ping_node(#node{node=NodeName
                      ,cookie=Cookie
                     }=Node) ->
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
    try freeswitch:version(NodeName) of
        {'ok', Version} ->
            lager:debug("got freeswitch erlang client version: ~s", [Version]),
            Version;
        _Else ->
            lager:debug("unable to get freeswitch client version: ~p", [_Else]),
            'undefined'
    catch
        _E:_R ->
            lager:debug("unable to contact freeswitch ~s: ~p", [NodeName, _R]),
            'undefined'
    end.

-spec reset_node_stats(fs_node()) -> 'ok'.
reset_node_stats(#node{}) ->
    'ok'.

-spec start_node_stats(fs_node()) -> 'ok'.
start_node_stats(#node{}) ->
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
        'undefined' ->
            lager:debug("failed to receive a response for node configs"),
            timer:sleep(5000),
            _ = ecallmgr_config:flush(<<"fs_nodes">>),
            start_preconfigured_servers();
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
            try ?MODULE:add(wh_util:to_atom(Node, 'true'), wh_util:to_atom(Cookie, 'true')) of
                _OK -> lager:debug("added ~s(~s) successfully: ~p", [Node, Cookie, _OK])
            catch
                _E:_R -> lager:debug("failed to add ~s(~s): ~s: ~p", [Node, Cookie, _E, _R])
            end
    end.

print_details([]) ->
    io:format("No nodes found!~n", []);
print_details(Nodes) ->
    print_details(Nodes, 0).

print_details([], Count) ->
    io:format("~n"),
    io:format("Found ~p nodes~n", [Count]);
print_details([{NodeName, Node}|Nodes],Count) ->
    io:format("~n"),
    io:format("~-12s: ~s~n", [<<"Node">>, NodeName]),
    io:format("~-12s: ~s~n", [<<"Cookie">>, Node#node.cookie]),
    io:format("~-12s: ~s~n", [<<"Client">>, Node#node.client_version]),
    io:format("~-12s: ~p~n", [<<"Connected">>, Node#node.connected]),
    io:format("~-12s: ~p~n", [<<"Options">>, Node#node.options]),
    _ = case ecallmgr_fs_pinger_sup:find_pinger(NodeName) of
            'undefined' -> 'ok';
            PingerPid ->
                io:format("~-12s: ~p~n", [<<"Pinger">>, PingerPid])
        end,
    _ = case ecallmgr_fs_sup:find_node(NodeName) of
            'undefined' -> 'ok';
            NodeSupPid ->
                io:format("~-12s: ~p~n", [<<"Supervisor">>, NodeSupPid]),
                io:format("Workers~n"),
                [begin
                     io:format("    ~-15w ~s~n", [Pid, Name])
                 end
                 || {Name, Pid, _, _} <- supervisor:which_children(NodeSupPid)
                ],
                print_node_details(NodeName)
        end,
    print_details(Nodes, Count + 1).

print_node_details(Node) ->
    Capabilities = get_capabilities(Node),
    io:format("Capabilities:~n", []),
    lists:foreach(fun print_capability/1, Capabilities).

print_capability(Capability) ->
    IsLoaded = case wh_json:is_true(<<"is_loaded">>, Capability) of
                   'true' -> <<>>;
                   'false' -> <<" not">>
               end,
    io:format("  ~-12s provided by ~s, is~s available~n", [wh_json:get_value(<<"capability">>, Capability)
                                                           ,wh_json:get_value(<<"module">>, Capability)
                                                           ,IsLoaded
                                                          ]).

print_summary([]) ->
    io:format("No nodes found!~n", []);
print_summary(Nodes) ->
    io:format("+----------------------------------------------------+-----------+----------------------------------+----------------------+--------------------~n"),
    io:format("| Node Name                                          | Connected | Cookie                           | Version              | Capabilities~n"),
    io:format("+====================================================+===========+==================================+======================+====================~n"),
    print_summary(Nodes, 0).

print_summary([], Count) ->
    io:format("+----------------------------------------------------+-----------+----------------------------------+----------------------+--------------------~n"),
    io:format("Found ~p nodes~n", [Count]);
print_summary([{_, Node}|Nodes], Count) ->
    Capabilities = ecallmgr_fs_nodes:get_capabilities(Node#node.node),
    Loaded = [wh_json:get_value(<<"capability">>, Capability)
              || Capability <- Capabilities,
                 wh_json:is_true(<<"is_loaded">>, Capability)
             ],
    io:format("| ~-50s | ~-9s | ~-32s | ~-20s | ~s~n"
              ,[Node#node.node
                ,Node#node.connected
                ,Node#node.cookie
                ,Node#node.client_version
                ,wh_util:join_binary(Loaded, <<", ">>)
               ]),
    print_summary(Nodes, Count + 1).
