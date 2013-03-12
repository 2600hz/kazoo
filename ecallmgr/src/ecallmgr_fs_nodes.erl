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
-export([account_summary/1]).

-export([get_call_precedence/1]).
-export([channels_by_auth_id/1]).

-export([sync_channels/0, sync_channels/1
         ,sync_conferences/0, sync_conferences/1
         ,flush_node_channels/1
         ,flush_node_conferences/1
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
-record(astats, {billing_ids =          sets:new() :: set()
                 ,outbound_flat_rate =  sets:new() :: set()
                 ,inbound_flat_rate =   sets:new() :: set()
                 ,outbound_per_minute = sets:new() :: set()
                 ,inbound_per_minute =  sets:new() :: set()
                 ,resource_consumers =  sets:new() :: set()
                }).
-type astats() :: #astats{}.

-record(state, {nodes = dict:new() :: dict() %fs_nodes()
                ,self = self() :: pid()
                ,preconfigured_lookup :: pid()
               }).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

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

-spec account_summary(ne_binary()) -> wh_json:object().
account_summary(AccountId) ->
    summarize_account_usage(ecallmgr_fs_channel:account_summary(AccountId)).

-spec get_call_precedence(ne_binary()) -> integer().
get_call_precedence(UUID) ->
    MatchSpec = [{#channel{uuid = '$1', precedence = '$2', _ = '_'}
                  ,[{'=:=', '$1', {const, UUID}}]
                  ,['$2']}
                ],
    case ets:select(?CHANNELS_TBL, MatchSpec) of
        [Presedence] -> Presedence;
        _ -> 5
    end.

-spec channels_by_auth_id(ne_binary()) ->
                                 {'ok', wh_json:objects()} |
                                 {'error', 'not_found'}.
channels_by_auth_id(AuthorizingId) ->
    MatchSpec = [{#channel{authorizing_id = '$1', _ = '_'}
                  ,[{'=:=', '$1', {const, AuthorizingId}}]
                  ,['$_']}
                ],
    case ets:select(?CHANNELS_TBL, MatchSpec) of
        [] -> {'error', 'not_found'};
        Channels -> {'ok', [ecallmgr_fs_channel:record_to_json(Channel)
                            || Channel <- Channels
                           ]}
    end.

-spec sync_channels() -> 'ok'.
-spec sync_channels(string() | binary() | atom()) -> 'ok'.
sync_channels() ->
    _ = [ecallmgr_fs_node:sync_channels(Srv)
         || Srv <- gproc:lookup_pids({p, l, fs_node})
        ],
    'ok'.

sync_channels(Node) ->
    N = wh_util:to_atom(Node, 'true'),
    _ = [ecallmgr_fs_node:sync_channels(Srv)
         || Srv <- gproc:lookup_pids({p, l, fs_node})
                ,ecallmgr_fs_node:fs_node(Srv) =:= N
        ],
    'ok'.

-spec sync_conferences() -> 'ok'.
-spec sync_conferences(string() | binary() | atom()) -> 'ok'.
sync_conferences() ->
    _ = [ecallmgr_fs_node:sync_conferences(Srv)
         || Srv <- gproc:lookup_pids({p, l, fs_node})
        ],
    'ok'.

sync_conferences(Node) ->
    N = wh_util:to_atom(Node, 'true'),
    _ = [ecallmgr_fs_node:sync_conferences(Srv)
         || Srv <- gproc:lookup_pids({p, l, fs_node})
                ,ecallmgr_fs_node:fs_node(Srv) =:= N
        ],
    'ok'.

-spec flush_node_channels(string() | binary() | atom()) -> 'ok'.
flush_node_channels(Node) ->
    gen_server:cast(?MODULE, {'flush_node_channels', wh_util:to_atom(Node, 'true')}).

-spec flush_node_conferences(string() | binary() | atom()) -> 'ok'.
flush_node_conferences(Node) ->
    gen_server:cast(?MODULE, {'flush_node_conferences', wh_util:to_atom(Node, 'true')}).

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
    process_flag(trap_exit, 'true'),
    lager:debug("starting new fs handler"),
    _ = spawn_link(fun() -> start_preconfigured_servers() end),
    _ = ets:new('sip_subscriptions', [set, public, named_table, {keypos, #sip_subscription.key}]),
    _ = ets:new(?CHANNELS_TBL, [set, protected, named_table, {keypos, #channel.uuid}]),
    _ = ets:new(?CONFERENCES_TBL, [set, protected, named_table, {keypos, #conference.name}]),
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
handle_call({'add_fs_node', NodeName, Cookie, Options}, From, State) ->    spawn(fun() ->
                  Reply = maybe_add_node(NodeName, Cookie, Options, State),
                  gen_server:reply(From, Reply)
          end),
    {'noreply', State};
handle_call('nodes', _From, #state{nodes=Nodes}=State) ->
    {'reply', dict:to_list(Nodes), State};
handle_call({'new_conference', #conference{node=Node, name=Name}=C}, _, State) ->
    case ets:lookup(?CONFERENCES_TBL, Name) of
        [] ->
            lager:debug("creating new conference ~s on node ~s", [Name, Node]),
            ets:insert(?CONFERENCES_TBL, C);
        [#conference{node=Node}] -> lager:debug("conference ~s already on node ~s", [Name, Node]);
        [#conference{node=_Other}] -> lager:debug("conference ~s already on ~s, not ~s", [Name, _Other, Node])
    end,
    {'reply', 'ok', State, 'hibernate'};
handle_call({'conference_update', Node, Name, Update}, _, State) ->
    case ets:lookup(?CONFERENCES_TBL, Name) of
        [#conference{node=Node}] ->
            ets:update_element(?CONFERENCES_TBL, Name, Update),
            lager:debug("conference ~s already on node ~s", [Name, Node]);
        [#conference{node=_Other}] -> lager:debug("conference ~s already on ~s, not ~s, ignoring update", [Name, _Other, Node]);
        [] -> lager:debug("no conference ~s on ~s, ignoring update", [Name, Node])
    end,
    {'reply', 'ok', State, 'hibernate'};
handle_call({'conference_destroy', Node, Name}, _, State) ->
    MatchSpecC = [{#conference{name='$1', node='$2', _ = '_'}
                   ,[{'andalso'
                      ,{'=:=', '$2', {const, Node}}
                      ,{'=:=', '$1', Name}}
                    ],
                   [true]
                  }],
    N = ets:select_delete(?CONFERENCES_TBL, MatchSpecC),
    lager:debug("removed ~p conference(s) with id ~s on ~s", [N, Name, Node]),
    MatchSpecP = [{#participant{conference_name='$1', node='$2', _ = '_'}
                   ,[{'andalso', {'=:=', '$2', {const, Node}}
                      ,{'=:=', '$1', Name}}
                    ],
                   [true]
                  }],
    N1 = ets:select_delete(?CONFERENCES_TBL, MatchSpecP),
    lager:debug("removed ~p participant(s) in conference ~s on ~s", [N1, Name, Node]),
    {'reply', 'ok', State, 'hibernate'};
handle_call({'participant_update', Node, UUID, Update}, _, State) ->
    case ets:lookup(?CONFERENCES_TBL, UUID) of
        [] ->
            lager:debug("no participant ~s, creating", [Node]),
            'true' = ets:insert_new(?CONFERENCES_TBL, #participant{uuid=UUID}),
            'true' = ets:update_element(?CONFERENCES_TBL, UUID, Update);
        [#participant{node=Node}] ->
            lager:debug("participant ~s on ~s, applying update", [UUID, Node]),
            ets:update_element(?CONFERENCES_TBL, UUID, Update);
        [#participant{node=_OtherNode}] ->
            lager:debug("participant ~s is on ~s, not ~s, ignoring update", [UUID, _OtherNode, Node])
    end,
    {'reply', 'ok', State, 'hibernate'};
handle_call({'participant_destroy', Node, UUID}, _, State) ->
    MatchSpec = [{#participant{uuid='$1', node='$2', _ = '_'}
                  ,[{'andalso'
                     ,{'=:=', '$2', {const, Node}}
                     ,{'=:=', '$1', UUID}}
                   ],
                  [true]
                 }],
    N = ets:select_delete(?CONFERENCES_TBL, MatchSpec),
    lager:debug("removed ~p participants(s) with id ~s on ~s", [N, UUID, Node]),
    {'reply', 'ok', State, 'hibernate'};
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
handle_cast({'new_channel', Channel}, State) ->
    ets:insert(?CHANNELS_TBL, Channel),
    {'noreply', State, 'hibernate'};
handle_cast({'channel_update', UUID, Update}, State) ->
    ets:update_element(?CHANNELS_TBL, UUID, Update),
    {'noreply', State, 'hibernate'};
handle_cast({'destroy_channel', UUID, Node}, State) ->
    MatchSpec = [{#channel{uuid='$1', node='$2', _ = '_'}
                  ,[{'andalso', {'=:=', '$2', {const, Node}}
                     ,{'=:=', '$1', UUID}}
                   ],
                  [true]
                 }],
    N = ets:select_delete(?CHANNELS_TBL, MatchSpec),
    lager:debug("removed ~p channel(s) with id ~s on ~s", [N, UUID, Node]),
    {'noreply', State, 'hibernate'};
handle_cast({'sync_channels', Node, Channels}, State) ->
    lager:debug("ensuring channel cache is in sync with ~s", [Node]),
    MatchSpec = [{#channel{uuid = '$1', node = '$2', _ = '_'}
                  ,[{'=:=', '$2', {const, Node}}]
                  ,['$1']}
                ],
    CachedChannels = sets:from_list(ets:select(?CHANNELS_TBL, MatchSpec)),
    SyncChannels = sets:from_list(Channels),
    Remove = sets:subtract(CachedChannels, SyncChannels),
    Add = sets:subtract(SyncChannels, CachedChannels),
    _ = [begin
             lager:debug("removed channel ~s from cache during sync with ~s", [UUID, Node]),
             ets:delete(?CHANNELS_TBL, UUID)
         end
         || UUID <- sets:to_list(Remove)
        ],
    _ = [begin
             lager:debug("added channel ~s to cache during sync with ~s", [UUID, Node]),
             case build_channel_record(Node, UUID) of
                 {'ok', C} -> ets:insert(?CHANNELS_TBL, C);
                 {'error', _R} -> lager:warning("failed to sync channel ~s: ~p", [UUID, _R])
             end
         end
         || UUID <- sets:to_list(Add)
        ],
    {'noreply', State, 'hibernate'};
handle_cast({'sync_conferences', Node, Conferences}, State) ->
    lager:debug("ensuring conferences cache is in sync with ~s", [Node]),
    CachedConferences = ets:match_object(?CONFERENCES_TBL, #conference{node = Node, _ = '_'}) ++
        ets:match_object(?CONFERENCES_TBL, #participant{node = Node, _ = '_'}),
    Remove = subtract_from(CachedConferences, Conferences),
    Add = subtract_from(Conferences, CachedConferences),
    _ = [ets:delete_object(?CONFERENCES_TBL, R) || R <- Remove],
    _ = [ets:insert(?CONFERENCES_TBL, C) || C <- Add],
    {'noreply', State, 'hibernate'};
handle_cast({'flush_node_channels', Node}, State) ->
    lager:debug("flushing all channels in cache associated to node ~s", [Node]),
    MatchSpec = [{#channel{node = '$1', _ = '_'}
                  ,[{'=:=', '$1', {const, Node}}]
                  ,['true']}
                ],
    ets:select_delete(?CHANNELS_TBL, MatchSpec),
    {'noreply', State};
handle_cast({'flush_node_conferences', Node}, State) ->
    lager:debug("flushing all conferences in cache associated to node ~s", [Node]),
    MatchSpecC = [{#conference{node = '$1', _ = '_'}
                   ,[{'=:=', '$1', {const, Node}}]
                   ,['true']}
                 ],
    _ = ets:select_delete(?CONFERENCES_TBL, MatchSpecC),
    MatchSpecP = [{#participant{node = '$1', _ = '_'}
                   ,[{'=:=', '$1', {const, Node}}]
                   ,['true']}
                 ],
    _ = ets:select_delete(?CONFERENCES_TBL, MatchSpecP),
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
                   [{'>', {const, Now}, {'+', '$2', '$1'}}],
                   [true]}
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
    ets:delete(?CONFERENCES_TBL),
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
-spec maybe_handle_nodeup(#node{}, state()) -> 'ok'.
maybe_handle_nodeup(NodeName, #state{nodes=Nodes}=State) ->
    case dict:find(NodeName, Nodes) of
        {'ok', #node{connected='false'}=Node} ->
            handle_nodeup(Node, State);
        _Else -> 'ok'
    end.

-spec maybe_handle_nodedown(#node{}, state()) -> 'ok'.
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

-spec rm_fs_node(#node{}, state()) -> 'ok'.
rm_fs_node(#node{}=Node, #state{self=Srv}) ->
    _ = maybe_disconnect_from_node(Node),
    gen_listener:cast(Srv, {'remove_node', Node}).

-spec handle_nodeup(#node{}, state()) -> 'ok'.
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

-spec handle_nodedown(#node{}, state()) -> 'ok'.
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

-spec maybe_connect_to_node(#node{}) -> 'ok' | {'error', _}.
maybe_connect_to_node(#node{node=NodeName}=Node) ->
    lager:debug("attempting to connect to freeswitch node ~s", [NodeName]),
    case maybe_ping_node(Node) of
        {'error', _R}=E -> E;
        'ok' ->
            lager:notice("succesfully connected to freeswitch node ~s", [NodeName]),
            'ok'
    end.

-spec maybe_ping_node(#node{}) -> 'ok' | {'error', _}.
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

-spec maybe_start_node_handlers(#node{}) -> 'ok' | {'error', _}.
maybe_start_node_handlers(#node{node=NodeName, client_version=Version
                                ,cookie=Cookie, options=Props}=Node) ->
    try ecallmgr_fs_sup:add_node(NodeName, [{cookie, Cookie}
                                            ,{client_version, Version}
                                            | props:delete(cookie, Props)
                                           ])
    of
        {'ok', _} -> initialize_node_connection(Node);
        {'error', {'already_started', _}} -> ok;
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
            _ = [lager:debug("st: ~p", [S]) || S <- ST],
            {'error', Reason}
    end.

-spec initialize_node_connection(#node{}) -> 'ok'.
initialize_node_connection(#node{}=Node) ->
    start_node_stats(Node),
    'ok'.

maybe_disconnect_from_node(#node{node=NodeName, connected='true'}=Node) ->
    lager:warning("disconnected from node ~s", [NodeName]),
    _ = close_node(Node),
    reset_node_stats(Node);
maybe_disconnect_from_node(#node{connected='false'}) ->
    'ok'.

-spec maybe_start_node_pinger(#node{}) -> 'ok'.
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

-spec close_node(#node{}) -> 'ok' | {'error','not_found' | 'running' | 'simple_one_for_one'}.
close_node(#node{node=NodeName}) ->
    _ = ecallmgr_fs_sup:remove_node(NodeName),
    ecallmgr_fs_pinger_sup:remove_node(NodeName).

-spec create_node(text(), text(), wh_proplist()) -> #node{}.
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

-spec get_fs_client_version(#node{}) -> #node{};
                              (atom()) -> 'undefined' | ne_binary().
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

-spec reset_node_stats(#node{}) -> 'ok'.
reset_node_stats(#node{node=NodeName}) ->
    NodeBin = amqp_util:encode(wh_util:to_binary(NodeName)),
    wh_gauge:set(<<"freeswitch.nodes.", NodeBin/binary, ".up">>, 0),
    wh_timer:delete(<<"freeswitch.nodes.", NodeBin/binary, ".uptime">>),
    'ok'.

-spec start_node_stats(#node{}) -> 'ok'.
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

-spec build_channel_record(atom(), ne_binary()) ->
                                  {'ok', channel()} |
                                  {'error', 'timeout' | 'badarg'}.
build_channel_record(Node, UUID) ->
    case freeswitch:api(Node, 'uuid_dump', wh_util:to_list(UUID)) of
        {'ok', Dump} ->
            Props = ecallmgr_util:eventstr_to_proplist(Dump),
            {'ok', ecallmgr_fs_channel:props_to_record(Props, Node)};
        {'error', _}=E -> E;
        'timeout' -> {'error', 'timeout'}
    end.

-spec summarize_account_usage(channels()) -> wh_json:object().
summarize_account_usage(Channels) ->
    AStats = lists:foldr(fun classify_channel/2, #astats{}, Channels),
    wh_json:from_list(
      [{<<"Calls">>, sets:size(AStats#astats.billing_ids)}
       ,{<<"Channels">>,  length(Channels)}
       ,{<<"Outbound-Flat-Rate">>, sets:size(AStats#astats.outbound_flat_rate)}
       ,{<<"Inbound-Flat-Rate">>, sets:size(AStats#astats.inbound_flat_rate)}
       ,{<<"Outbound-Per-Minute">>, sets:size(AStats#astats.outbound_per_minute)}
       ,{<<"Inbound-Per-Minute">>, sets:size(AStats#astats.inbound_per_minute)}
       ,{<<"Resource-Consuming-Calls">>, sets:size(AStats#astats.resource_consumers)}
      ]).

-spec classify_channel(channel(), astats()) -> astats().
classify_channel(#channel{billing_id='undefined', uuid=UUID}=Channel, AStats) ->
    classify_channel(Channel#channel{billing_id=wh_util:to_hex_binary(crypto:md5(UUID))}
                     ,AStats
                    );
classify_channel(#channel{bridge_id='undefined', billing_id=BillingId}=Channel, AStats) ->
    classify_channel(Channel#channel{bridge_id=BillingId}, AStats);
classify_channel(#channel{direction = <<"outbound">>
                          ,account_billing = <<"flat_rate">>
                          ,bridge_id=BridgeId
                          ,billing_id=BillingId
                         }
                 ,#astats{outbound_flat_rate=OutboundFlatRates
                          ,resource_consumers=ResourceConsumers
                          ,billing_ids=BillingIds
                         }=AStats) ->
    AStats#astats{outbound_flat_rate=sets:add_element(BridgeId, OutboundFlatRates)
                  ,resource_consumers=sets:add_element(BillingId, ResourceConsumers)
                  ,billing_ids=sets:add_element(BillingId, BillingIds)
                 };
classify_channel(#channel{direction = <<"inbound">>
                          ,account_billing = <<"flat_rate">>
                          ,bridge_id=BridgeId
                          ,billing_id=BillingId
                         }
                 ,#astats{inbound_flat_rate=InboundFlatRates
                          ,resource_consumers=ResourceConsumers
                          ,billing_ids=BillingIds
                         }=AStats) ->
    AStats#astats{inbound_flat_rate=sets:add_element(BridgeId, InboundFlatRates)
                  ,resource_consumers=sets:add_element(BillingId, ResourceConsumers)
                  ,billing_ids=sets:add_element(BillingId, BillingIds)
                 };
classify_channel(#channel{direction = <<"outbound">>
                          ,account_billing = <<"per_minute">>
                          ,bridge_id=BridgeId
                          ,billing_id=BillingId
                         }
                 ,#astats{outbound_per_minute=OutboundPerMinute
                          ,resource_consumers=ResourceConsumers
                          ,billing_ids=BillingIds
                         }=AStats) ->
    AStats#astats{outbound_per_minute=sets:add_element(BridgeId, OutboundPerMinute)
                  ,resource_consumers=sets:add_element(BillingId, ResourceConsumers)
                  ,billing_ids=sets:add_element(BillingId, BillingIds)
                 };
classify_channel(#channel{direction = <<"inbound">>
                          ,account_billing = <<"per_minute">>
                          ,bridge_id=BridgeId
                          ,billing_id=BillingId
                         }
                 ,#astats{inbound_per_minute=InboundPerMinute
                          ,resource_consumers=ResourceConsumers
                          ,billing_ids=BillingIds
                         }=AStats) ->
    AStats#astats{inbound_per_minute=sets:add_element(BridgeId, InboundPerMinute)
                  ,resource_consumers=sets:add_element(BillingId, ResourceConsumers)
                  ,billing_ids=sets:add_element(BillingId, BillingIds)
                 };
classify_channel(#channel{direction = <<"inbound">>
                          ,authorizing_id='undefined'
                          ,billing_id=BillingId
                         }
                 ,#astats{resource_consumers=ResourceConsumers
                          ,billing_ids=BillingIds
                         }=AStats) ->
    AStats#astats{resource_consumers=sets:add_element(BillingId, ResourceConsumers)
                  ,billing_ids=sets:add_element(BillingId, BillingIds)
                 };
classify_channel(#channel{direction = <<"inbound">>
                          ,billing_id=BillingId
                         }
                 ,#astats{billing_ids=BillingIds}=AStats) ->
    AStats#astats{billing_ids=sets:add_element(BillingId, BillingIds)};
classify_channel(#channel{direction = <<"outbound">>
                          ,resource_id='undefined'
                          ,billing_id=BillingId
                         }
                 ,#astats{billing_ids=BillingIds}=AStats) ->
    AStats#astats{billing_ids=sets:add_element(BillingId, BillingIds)};
classify_channel(#channel{direction = <<"outbound">>
                          ,billing_id=BillingId
                         }
                 ,#astats{resource_consumers=ResourceConsumers
                          ,billing_ids=BillingIds
                         }=AStats) ->
    AStats#astats{resource_consumers=sets:add_element(BillingId, ResourceConsumers)
                  ,billing_ids=sets:add_element(BillingId, BillingIds)
                 }.

subtract_from([], _) -> [];
subtract_from(Set1, []) -> Set1;
subtract_from(Set1, [S2|Set2]) ->
    subtract_from([S1 || S1 <- Set1, should_remove(S1, S2)], Set2).

should_remove(#participant{uuid=UUID1}, #participant{uuid=UUID2}) -> UUID1 =/= UUID2;
should_remove(#conference{name=N1}, #conference{name=N2}) -> N1 =/= N2;
should_remove(_, _) -> 'true'.
