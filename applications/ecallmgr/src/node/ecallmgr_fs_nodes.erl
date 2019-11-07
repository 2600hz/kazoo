%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc When connecting to a FreeSWITCH node, we create three processes: one to
%%% handle authentication (directory) requests; one to handle route (dialplan)
%%% requests, and one to monitor the node and various stats about the node.
%%%
%%%
%%% @author James Aimonetti
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_nodes).
-behaviour(gen_listener).

-export([start_link/0]).
-export([connected/0, connected/1]).
-export([all_nodes_connected/0]).
-export([add/1, add/2, add/3]).
-export([remove/1]).
-export([nodeup/1, nodeup/2, nodedown/1]).
-export([is_node/1, is_node_up/1, is_node_down/1]).
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
        ,remove_capabilities/1, remove_capability/2
        ,flush/0, flush/2
        ]).

-export([handle_fs_xml_flush/2]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-define(RESPONDERS, [{{?MODULE, 'handle_fs_xml_flush'}
                     ,[{<<"switch_event">>, <<"fs_xml_flush">>}]
                     }
                    ]).
-define(BINDINGS, [{'switch', [{'restrict_to', ['fs_xml_flush']}
                              ,'federate'
                              ]}
                  ]).
-define(QUEUE_NAME, <<"fs_nodes_shared_listener">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

-define(EXPIRE_CHECK, 60 * ?MILLISECONDS_IN_SECOND).

-type connect_strategy() :: 'ping' | 'heartbeat'.

-record(node, {node :: atom()
              ,cookie :: atom()
              ,connected = 'false' :: boolean()
              ,started = kz_time:now_s() :: kz_time:gregorian_seconds()
              ,client_version :: kz_term:api_binary()
              ,options = [] :: kz_term:proplist()
              ,connect_strategy = 'ping' :: connect_strategy()
              }).
-type fs_node() :: #node{}.

-record(capability, {node :: atom() | '$1' | '_'
                    ,name :: kz_term:ne_binary() | '$1' | '$2' | '_'
                    ,module :: kz_term:ne_binary() | '_'
                    ,is_loaded = 'false' :: boolean() | '$3' | '_'
                    }).
-type capability() :: #capability{}.
-type capabilities() :: [capability()].

-define(CAPABILITY_TBL, 'ecallmgr_fs_node_capabilities').

-record(state, {nodes = dict:new() :: dict:dict() %fs_nodes()
               ,self = self() :: pid()
               ,init_pidref :: kz_term:pid_ref() | 'undefined'
               }).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_listener:start_link({'local', ?SERVER}, ?MODULE
                           , [{'responders', ?RESPONDERS}
                             ,{'bindings', ?BINDINGS}
                             ,{'queue_name', ?QUEUE_NAME}
                             ,{'queue_options', ?QUEUE_OPTIONS}
                             ,{'consume_options', ?CONSUME_OPTIONS}
                             ], []).

%% returns 'ok' or {'error', some_error_atom_explaining_more}

-spec add(atom()) -> 'ok' | {'error', 'no_connection'}.
add(Node) -> add(Node, []).

-spec add(atom(), kz_term:proplist() | atom()) -> 'ok' | {'error', 'no_connection'}.
add(Node, Opts) when is_list(Opts) ->
    add(Node, erlang:get_cookie(), Opts);
add(Node, Cookie) when is_atom(Cookie) ->
    add(Node, Cookie, [{'cookie', Cookie}]).

-spec add(atom(), atom(), kz_term:proplist() | atom()) -> 'ok' | {'error', 'no_connection'}.
add(Node, Cookie, Opts) when is_atom(Node) ->
    gen_server:call(?SERVER
                   ,{'add_fs_node'
                    ,Node
                    ,Cookie
                    ,[{'cookie', Cookie}
                      | props:delete('cookie', Opts)
                     ]
                    }
                   ,60 * ?MILLISECONDS_IN_SECOND
                   ).

-spec nodeup(atom()) -> 'ok'.
nodeup(Node) ->
    nodeup(Node, 'ping').

-spec nodeup(atom(), atom()) -> 'ok'.
nodeup(Node, Strategy) when is_atom(Node) ->
    gen_server:cast(?SERVER, {'fs_nodeup', Node, Strategy}).

%% returns 'ok' or {'error', some_error_atom_explaining_more}
-spec remove(atom()) -> 'ok'.
remove(Node) when is_atom(Node) ->
    gen_server:cast(?SERVER, {'rm_fs_node', Node}).

-spec connected() -> kz_term:atoms() | kz_term:proplist_kv(atom(), kz_time:gregorian_seconds()).
connected() ->
    connected('false').

-spec connected('false') -> [atom()];
               ('true') -> [{atom(), kz_time:gregorian_seconds()}].
connected(Verbose) ->
    gen_server:call(?SERVER, {'connected_nodes', Verbose}).

-spec flush() -> 'ok'.
flush() -> do_flush(<<>>).

-spec flush(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
flush(User, Realm) ->
    Args = list_to_binary(["id ", User, " ", Realm]),
    do_flush(Args).

-spec do_flush(binary()) -> 'ok'.
do_flush(Args) ->
    lager:debug("flushing xml cache ~s from all FreeSWITCH servers", [Args]),
    _ = [freeswitch:api(Node, 'xml_flush_cache', Args)
         || Node <- connected()
        ],
    'ok'.

-spec is_node_up(atom()) -> boolean().
is_node_up(Node) when is_atom(Node) ->
    gen_server:call(?SERVER, {'is_node_up', Node}).

-spec is_node_down(atom()) -> boolean().
is_node_down(Node) when is_atom(Node) ->
    not gen_server:call(?SERVER, {'is_node_up', Node}).

-spec is_node(atom()) -> boolean().
is_node(Node) when is_atom(Node) ->
    gen_server:call(?SERVER, {'is_node', Node}).

-spec sip_url(atom() | kz_term:text()) -> kz_term:api_binary().
sip_url(Node) when not is_atom(Node) ->
    sip_url(kz_term:to_atom(Node, 'true'));
sip_url(Node) when is_atom(Node) ->
    case [ecallmgr_fs_node:sip_url(Srv)
          || Srv <- gproc:lookup_pids({'p', 'l', 'fs_node'}),
             ecallmgr_fs_node:fs_node(Srv) =:= Node
         ]
    of
        [URL|_] -> URL;
        _Else -> 'undefined'
    end.

-spec sip_external_ip(atom() | kz_term:text()) -> kz_term:api_binary().
sip_external_ip(Node) when not is_atom(Node) ->
    sip_external_ip(kz_term:to_atom(Node, 'true'));
sip_external_ip(Node) when is_atom(Node) ->
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
    length(?FS_NODES) =:= length(connected()).

-spec summary() -> 'ok'.
summary() ->
    print_summary(gen_server:call(?SERVER, 'nodes')).


-spec details() -> 'ok'.
details() ->
    print_details(gen_server:call(?SERVER, 'nodes')).

-spec details(kz_term:text() | atom()) -> 'ok'.
details(NodeName) when not is_atom(NodeName) ->
    details(kz_term:to_atom(NodeName, 'true'));
details(NodeName) when is_atom(NodeName) ->
    case gen_server:call(?SERVER, {'node', NodeName}) of
        {'error', 'not_found'} ->
            io:format("Node ~s not found!~n", [NodeName]);
        {'ok', Node} ->
            print_details([{'undefined', Node}])
    end.

-spec has_capability(atom(), kz_term:ne_binary() | kz_json:object()) -> boolean().
has_capability(Node, Capability) when is_binary(Capability) ->
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
    end;
has_capability(Node, Capability) ->
    has_capability(Node, kz_json:get_ne_binary_value(<<"capability">>, Capability)).

-spec remove_capabilities(atom()) -> non_neg_integer().
remove_capabilities(Node) ->
    MatchSpec = [{#capability{node='$1'
                             ,_='_'
                             }
                 ,[{'=:=', '$1', Node}]
                 ,['true']
                 }],
    ets:select_delete(?CAPABILITY_TBL, MatchSpec).

-spec remove_capability(atom(), kz_term:ne_binary()) -> non_neg_integer().
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

-spec get_capability(atom(), kz_term:ne_binary()) ->
                            capability() | kz_term:api_object().
get_capability(Node, Capability) ->
    get_capability(Node, Capability, 'json').

-spec get_capability(atom(), kz_term:ne_binary(), 'json' | 'record') ->
                            capability() | kz_term:api_object().
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
                              kz_json:objects() | capabilities().
get_capabilities(Node) ->
    get_capabilities(Node, 'json').

-spec get_capabilities(atom(), 'json' | 'record') ->
                              kz_json:objects() | capabilities().
get_capabilities(Node, Format) ->
    MatchSpec = [{#capability{node='$1'
                             ,_='_'
                             }
                 ,[{'=:=', '$1', Node}]
                 ,['$_']
                 }],
    format_capabilities(Format, ets:select(?CAPABILITY_TBL, MatchSpec)).

-spec format_capabilities('json' | 'record', capabilities()) -> capabilities() | kz_json:objects().
format_capabilities('record', Results) -> Results;
format_capabilities('json', Results) ->
    [capability_to_json(Result) || Result <- Results].

-spec format_capability('json' | 'record', [capability()]) -> kz_term:api_object() | capability().
format_capability('record', [Capability]) -> Capability;
format_capability('json', [Capability]) -> capability_to_json(Capability);
format_capability(_, []) -> 'undefined'.

-spec set_capability(atom(), kz_term:ne_binary(), boolean()) -> 'ok'.
set_capability(Node, Capability, Toggle) when is_boolean(Toggle) ->
    gen_server:call(?SERVER, {'set_capability', Node, Capability, Toggle}).

-spec add_capability(atom(), kz_json:object()) -> 'ok'.
add_capability(Node, Capability) ->
    case has_capability(Node, Capability) of
        'true' -> 'ok';
        'false' -> gen_server:call(?SERVER, {'add_capability', Node, Capability})
    end.

capability_to_json(#capability{node=Node
                              ,name=Capability
                              ,module=Module
                              ,is_loaded=IsLoaded
                              }) ->
    kz_json:from_list([{<<"node">>, kz_term:to_binary(Node)}
                      ,{<<"capability">>, Capability}
                      ,{<<"module">>, Module}
                      ,{<<"is_loaded">>, IsLoaded}
                      ]).

-spec handle_fs_xml_flush(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_fs_xml_flush(JObj, _Props) ->
    'true' = kapi_switch:fs_xml_flush_v(JObj),
    Username = kz_json:get_value(<<"Username">>, JObj),
    Realm = kz_json:get_value(<<"Realm">>, JObj, <<>>),
    flush(Username, Realm).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_log:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    process_flag('trap_exit', 'true'),
    lager:debug("starting new fs handler"),
    _ = ets:new(?CAPABILITY_TBL, ['bag', 'protected', 'named_table', {'keypos', #capability.node}]),
    InitPidRef = kz_process:spawn_monitor(fun start_preconfigured_servers/0, []),
    {'ok', #state{init_pidref=InitPidRef}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%% #state{nodes=[{FSNode, HandlerPid}]}
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call({'is_node_up', Node}, _From, #state{nodes=Nodes}=State) ->
    Resp = case dict:find(Node, Nodes) of
               'error' -> 'false';
               {'ok', #node{connected=Connected}} -> Connected
           end,
    {'reply', Resp, State};
handle_call({'is_node', Node}, _From, #state{nodes=Nodes}=State) ->
    {'reply', dict:find(Node, Nodes) =/= 'error', State};
handle_call({'connected_nodes', 'false'}, _From, #state{nodes=Nodes}=State) ->
    Resp = [Node
            || {_, #node{node=Node
                        ,connected=IsConnected
                        }
               } <- dict:to_list(Nodes),
               IsConnected
           ],
    {'reply', Resp, State};
handle_call({'connected_nodes', 'true'}, _From, #state{nodes=Nodes}=State) ->
    Resp = [{Node, Started}
            || {_, #node{node=Node
                        ,connected=Connected
                        ,started=Started
                        }
               } <- dict:to_list(Nodes),
               Connected
           ],
    {'reply', Resp, State};
handle_call({'add_fs_node', NodeName, Cookie, Options}, From, State) ->
    _ = kz_process:spawn(
          fun() ->
                  try maybe_add_node(NodeName, Cookie, Options, State) of
                      Reply ->
                          gen_server:reply(From, Reply)
                  catch
                      _E:R:_ ->
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
                                           ,name=kz_json:get_value(<<"capability">>, Capability)
                                           ,module=kz_json:get_value(<<"module">>, Capability)
                                           ,is_loaded=kz_json:is_true(<<"is_loaded">>, Capability)
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

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'fs_nodeup', NodeName, Strategy}, State) ->
    _ = kz_process:spawn(fun maybe_handle_nodeup/3, [NodeName, Strategy, State]),
    {'noreply', State};
handle_cast({'update_node', #node{node=NodeName}=Node}
           ,#state{nodes=Nodes}=State) ->
    {'noreply', State#state{nodes=dict:store(NodeName, Node, Nodes)}};
handle_cast({'remove_node', #node{node=NodeName}}, #state{nodes=Nodes}=State) ->
    remove_capabilities(NodeName),
    {'noreply', State#state{nodes=dict:erase(NodeName, Nodes)}};
handle_cast({'remove_capabilities', NodeName}, State) ->
    _Rm = remove_capabilities(NodeName),
    lager:debug("removed ~p capabilities from ~s", [_Rm, NodeName]),
    {'noreply', State};
handle_cast({'rm_fs_node', NodeName}, State) ->
    _ = kz_process:spawn(fun maybe_rm_fs_node/2, [NodeName, State]),
    {'noreply', State};
handle_cast(_Cast, State) ->
    lager:debug("unhandled cast: ~p", [_Cast]),
    {'noreply', State, 'hibernate'}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'nodedown', NodeName}, State) ->
    _ = kz_process:spawn(fun maybe_handle_nodedown/2, [NodeName, State]),
    call_control_fs_nodedown(NodeName),
    {'noreply', State};
handle_info({'DOWN', Ref, 'process', Pid, _Reason}, #state{init_pidref={Pid, Ref}}=State) ->
    lager:debug("initialization complete: ~p", [_Reason]),
    {'noreply', State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_term:proplist()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
    {'reply', []}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    ets:delete('sip_subscriptions'),
    lager:debug("fs nodes termination: ~p", [ _Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec call_control_fs_nodeup(atom()) -> 'ok'.
call_control_fs_nodeup(NodeName) ->
    Pids = gproc:lookup_pids({'p', 'l', 'call_control'}),
    call_control_fs_nodeup(Pids, NodeName).

-spec call_control_fs_nodeup(kz_term:pids(), atom()) -> 'ok'.
call_control_fs_nodeup([], _) -> 'ok';
call_control_fs_nodeup([Pid|Pids], NodeName) ->
    _ = ecallmgr_call_control:fs_nodeup(Pid, NodeName),
    call_control_fs_nodeup(Pids, NodeName).

-spec call_control_fs_nodedown(atom()) -> 'ok'.
call_control_fs_nodedown(NodeName) ->
    Pids = gproc:lookup_pids({'p', 'l', 'call_control'}),
    call_control_fs_nodedown(Pids, NodeName).

-spec call_control_fs_nodedown(kz_term:pids(), atom()) -> 'ok'.
call_control_fs_nodedown([], _) -> 'ok';
call_control_fs_nodedown([Pid|Pids], NodeName) ->
    _ = ecallmgr_call_control:fs_nodedown(Pid, NodeName),
    call_control_fs_nodedown(Pids, NodeName).

-spec maybe_handle_nodeup(fs_node(), atom(), state()) -> 'ok'.
maybe_handle_nodeup(NodeName, Strategy, #state{nodes=Nodes}=State) ->
    case dict:find(NodeName, Nodes) of
        {'ok', #node{connected='false'}=Node} ->
            handle_nodeup(Node#node{connect_strategy=Strategy}, State);
        _Else -> 'ok'
    end.

-spec maybe_handle_nodedown(fs_node(), state()) -> 'ok'.
maybe_handle_nodedown(NodeName, #state{nodes=Nodes}=State) ->
    case dict:find(NodeName, Nodes) of
        {'ok', #node{connected='true'}=Node} ->
            handle_nodedown(Node, State);
        _Else -> 'ok'
    end.

-spec maybe_add_node(kz_term:text(), kz_term:text(), kz_term:proplist(), state()) ->
                            'ok' | {'error', any()}.
maybe_add_node(NodeName, Cookie, Options, #state{self=Srv, nodes=Nodes}) ->
    case dict:find(NodeName, Nodes) of
        {'ok', #node{}} -> {'error', 'node_exists'};
        'error' ->
            Node = create_node(NodeName, Cookie, Options),
            case maybe_connect_to_node(Node) of
                {'error', _}=E ->
                    _ = gen_server:cast(Srv, {'update_node', Node#node{connected='false'}}),
                    _ = maybe_start_node_pinger(Node),
                    E;
                'ok' ->
                    gen_server:cast(Srv, {'update_node', Node#node{started=kz_time:now_s()
                                                                  ,connected='true'
                                                                  }})
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
rm_fs_node(#node{node=NodeName}=Node, #state{self=Srv}) ->
    _ = maybe_disconnect_from_node(Node),
    _ = ecallmgr_fs_pinger_sup:remove_node(NodeName),
    gen_server:cast(Srv, {'remove_node', Node}).

-spec handle_nodeup(fs_node(), state()) -> 'ok'.
handle_nodeup(#node{}=Node, #state{self=Srv}) ->
    NewNode = get_fs_client_version(Node),
    case min_version(NewNode)
        andalso maybe_connect_to_node(NewNode)
    of
        'false' ->
            _ = gen_server:cast(Srv, {'update_node', Node#node{connected='false'}}),
            _ = kz_process:spawn(fun() ->
                                         timer:sleep(?MILLISECONDS_IN_HOUR),
                                         _ = maybe_start_node_pinger(Node)
                                 end),
            'ok';
        {'error', _} ->
            _ = gen_server:cast(Srv, {'update_node', Node#node{connected='false'}}),
            _ = maybe_start_node_pinger(Node),
            'ok';
        'ok' ->
            gen_server:cast(Srv, {'update_node', NewNode#node{started=kz_time:now_s()
                                                             ,connected='true'
                                                             }})
    end.

-spec handle_nodedown(fs_node(), state()) -> 'ok'.
handle_nodedown(#node{connect_strategy='heartbeat', node=NodeName}=Node, #state{self=Srv}) ->
    lager:critical("received node down notice for ~s", [NodeName]),
    _ = maybe_disconnect_from_node(Node),
    gen_server:cast(Srv, {'remove_capabilities', NodeName}),
    gen_server:cast(Srv, {'update_node', Node#node{connected='false'}}),
    _ = maybe_start_node_pinger(Node),
    'ok';
handle_nodedown(#node{node=NodeName}=Node, #state{self=Srv}) ->
    lager:critical("received node down notice for ~s", [NodeName]),
    _ = maybe_disconnect_from_node(Node),
    gen_server:cast(Srv, {'remove_capabilities', NodeName}),
    case maybe_connect_to_node(Node) of
        {'error', _} ->
            _ = gen_server:cast(Srv, {'update_node', Node#node{connected='false'}}),
            _ = maybe_start_node_pinger(Node),
            'ok';
        'ok' ->
            gen_server:cast(Srv, {'update_node', Node#node{started=kz_time:now_s()
                                                          ,connected='true'
                                                          }})
    end.

-spec maybe_connect_to_node(fs_node()) -> 'ok' | {'error', any()}.
maybe_connect_to_node(#node{node=NodeName}=Node) ->
    timer:sleep(3 * ?MILLISECONDS_IN_SECOND),
    lager:debug("attempting to connect to freeswitch node ~s", [NodeName]),
    case maybe_ping_node(Node) of
        {'error', _R}=E -> E;
        'ok' ->
            lager:notice("successfully connected to freeswitch node ~s", [NodeName]),
            _ = freeswitch:no_legacy(NodeName),
            call_control_fs_nodeup(NodeName),
            'ok'
    end.

-spec maybe_ping_node(fs_node()) -> 'ok' | {'error', any()}.
maybe_ping_node(#node{node=NodeName
                     ,connect_strategy='heartbeat'
                     }=Node) ->
    _ = ecallmgr_fs_pinger_sup:remove_node(NodeName),
    maybe_start_node_handlers(Node);
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

-spec maybe_start_node_handlers(fs_node()) -> 'ok' | {'error', any()}.
maybe_start_node_handlers(#node{node=NodeName
                               ,client_version=Version
                               ,connect_strategy=Strategy
                               ,cookie=Cookie
                               ,options=Props
                               }=Node) ->
    _ = freeswitch:no_legacy(NodeName),
    try ecallmgr_fs_sup:add_node(NodeName, lists:usort([{'cookie', Cookie}
                                                       ,{'client_version', Version}
                                                       ,{'connect_strategy', Strategy}
                                                        | Props
                                                       ]))
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
        _:Reason:ST ->
            lager:warning("exception starting node ~s handlers: ~p", [NodeName, Reason]),
            kz_log:log_stacktrace(ST),
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

-spec close_node(fs_node()) -> 'ok' | {'error', any()}.
close_node(#node{node=NodeName}) ->
    _ = ecallmgr_fs_sup:remove_node(NodeName),
    ecallmgr_fs_pinger_sup:remove_node(NodeName).

-spec create_node(kz_term:text(), kz_term:text(), kz_term:proplist()) -> fs_node().
create_node(NodeName, Cookie, Options) when not is_atom(NodeName) ->
    create_node(kz_term:to_atom(NodeName, 'true'), Cookie, Options);
create_node(NodeName, Cookie, Options) when not is_atom(Cookie) ->
    create_node(NodeName, kz_term:to_atom(Cookie, 'true'), Options);
create_node(NodeName, Cookie, Options) ->
    #node{node=NodeName
         ,cookie=get_fs_cookie(Cookie, Options)
         ,client_version=get_fs_client_version(NodeName)
         ,options=Options
         ,connect_strategy=props:get_value('connect_strategy', Options, 'ping')
         }.

-spec get_fs_cookie(atom(), kz_term:proplist()) -> atom().
get_fs_cookie('undefined', Props) ->
    kz_term:to_atom(props:get_value('cookie', Props, erlang:get_cookie()));
get_fs_cookie(Cookie, _) when is_atom(Cookie) ->
    Cookie.

-spec get_fs_client_version(fs_node()) -> fs_node();
                           (atom()) -> kz_term:api_binary().
get_fs_client_version(#node{node=NodeName}=Node) ->
    case get_fs_client_version(NodeName) of
        'undefined' -> Node;
        Version -> Node#node{client_version=Version}
    end;
get_fs_client_version(NodeName) ->
    try freeswitch:version(NodeName) of
        {'ok', Version} ->
            lager:debug("got freeswitch erlang client version: ~s", [Version]),
            Version;
        _Else ->
            lager:debug("unable to get freeswitch client version: ~p", [_Else]),
            'undefined'
    catch
        _E:_R:_ ->
            lager:debug("unable to contact freeswitch ~s: ~p", [NodeName, _R]),
            'undefined'
    end.

-spec reset_node_stats(fs_node()) -> 'ok'.
reset_node_stats(#node{}) ->
    'ok'.

-spec start_node_stats(fs_node()) -> 'ok'.
start_node_stats(#node{}) ->
    'ok'.

-spec start_preconfigured_servers() -> 'ok'.
start_preconfigured_servers() ->
    start_preconfigured_servers(0).

-spec start_preconfigured_servers(integer()) -> 'ok'.
start_preconfigured_servers(5) ->
    lager:info("no preconfigured servers available and default not available.");
start_preconfigured_servers(Try) ->
    kz_log:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    case get_configured_nodes() of
        Nodes when is_list(Nodes) ->
            _ = [kz_process:spawn(fun start_node_from_config/1, [N]) || N <- Nodes];
        _ ->
            case try_connect_to_default_fs() of
                'ok' -> 'ok';
                _ ->
                    timer:sleep(5 * ?MILLISECONDS_IN_SECOND),
                    start_preconfigured_servers(Try + 1)
            end
    end.

-spec get_configured_nodes() -> 'ok' | [node()].
get_configured_nodes() ->
    case kapps_config:get(?APP_NAME, <<"fs_nodes">>) of
        [] ->
            lager:info("no preconfigured servers available. Is the sysconf whapp running?");
        Nodes when is_list(Nodes) ->
            lager:info("successfully retrieved FreeSWITCH nodes to connect with, doing so..."),
            Nodes;
        'undefined' ->
            lager:debug("failed to receive a response for node configs");
        _E ->
            lager:debug("received a non-list for fs_nodes: ~p", [_E])
    end.

-spec default_fs_host() -> list().
default_fs_host() ->
    Node = erlang:atom_to_list(node()),
    case string:tokens(Node, "@") of
        [_Name] -> "";
        [_Name, Host] -> "@" ++ Host
    end.

-spec default_fs_node() -> atom().
default_fs_node() ->
    erlang:list_to_atom("freeswitch" ++ default_fs_host()).

-spec try_connect_to_default_fs() -> 'skip' | 'ok' | {'error', 'no_connection'}.
try_connect_to_default_fs() ->
    Node = default_fs_node(),
    lager:info("attempting to connect default freeswitch node ~p", [Node]),
    case net_adm:ping(Node) of
        'pong' -> add(Node);
        _ -> 'skip'
    end.

-spec start_node_from_config(kz_json:object()|atom()) -> 'ok' | 'error' | {'error', 'no_connection'}.
start_node_from_config(MaybeJObj) ->
    case kz_json:is_json_object(MaybeJObj) of
        'false' -> add(kz_term:to_atom(MaybeJObj, 'true'));
        'true' ->
            {[Cookie], [Node]} = kz_json:get_values(MaybeJObj),
            try add(kz_term:to_atom(Node, 'true'), kz_term:to_atom(Cookie, 'true')) of
                _OK -> lager:debug("added ~s(~s) successfully: ~p", [Node, Cookie, _OK]), 'ok'
            catch
                _E:_R:_ -> lager:debug("failed to add ~s(~s): ~s: ~p", [Node, Cookie, _E, _R]), 'error'
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
    io:format("~-12s: ~s~n", [<<"SIP URL">>, sip_url(Node#node.node)]),
    io:format("~-12s: ~s~n", [<<"SIP External IP">>, sip_external_ip(Node#node.node)]),
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
                _ = [io:format("    ~-20w ~s~n", [Pid, Name])
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
    IsLoaded = case kz_json:is_true(<<"is_loaded">>, Capability) of
                   'true' -> <<>>;
                   'false' -> <<" not">>
               end,
    io:format("  ~-12s provided by ~s, is~s available~n", [kz_json:get_value(<<"capability">>, Capability)
                                                          ,kz_json:get_value(<<"module">>, Capability)
                                                          ,IsLoaded
                                                          ]).

print_summary([]) ->
    io:format("No nodes found!~n", []);
print_summary(Nodes) ->
    io:format("+----------------------------------------------------+-----------+----------------------------------+----------------------+~n"),
    io:format("| Node Name                                          | Connected | Cookie                           | Version              |~n"),
    io:format("+====================================================+===========+==================================+======================+~n"),
    print_summary(Nodes, 0).

print_summary([], Count) ->
    io:format("+----------------------------------------------------+-----------+----------------------------------+----------------------+~n"),
    io:format("Found ~p nodes~n", [Count]);
print_summary([{_, Node}|Nodes], Count) ->
    io:format("| ~-50s | ~-9s | ~-32s | ~-20s |~n"
             ,[Node#node.node
              ,Node#node.connected
              ,Node#node.cookie
              ,print_version(Node#node.client_version)
              ]),
    print_summary(Nodes, Count + 1).

-spec print_version(kz_term:ne_binary()) -> kz_term:ne_binary().
print_version(<<"mod_kazoo ", Version/binary>>) -> Version;
print_version(Version) -> Version.

-spec nodedown(atom()) -> 'ok'.
nodedown(Node) ->
    ?SERVER ! {'nodedown', Node},
    'ok'.

-spec min_version(fs_node()) -> boolean().
min_version(#node{client_version=ClientVersion,node=NodeName}) ->
    case freeswitch:release(ClientVersion) of
        {_, Release, _}
          when Release > ?MIN_FS_VERSION ->
            lager:debug("node ~s has minimum release ~s (~s) supported", [NodeName, Release, ?MIN_FS_VERSION]),
            'true';
        _ ->
            lager:warning("node ~s does not have minimum version ~s required to use this ecallmgr version.", [NodeName, ?MIN_FS_VERSION]),
            lager:warning("please upgrade your media node (~s) to freeswitch 1.10 or later", [NodeName]),
            'false'
    end.
