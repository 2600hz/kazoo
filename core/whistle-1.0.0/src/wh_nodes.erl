%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wh_nodes).

-behaviour(gen_listener).

-export([start_link/0]).
-export([whapp_count/1
         ,whapp_count/2
        ]).
-export([status/0]).
-export([flush/0]).
-export([handle_advertise/2]).
-export([notify_new/0
         ,notify_new/1
        ]).
-export([notify_expire/0
         ,notify_expire/1
        ]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("../include/wh_types.hrl").
-include("../include/wh_log.hrl").

-define(BINDINGS, [{'nodes', ['federate']}
                   ,{'self', []}
                  ]).
-define(RESPONDERS, [{{?MODULE, 'handle_advertise'}
                      ,[{<<"nodes">>, <<"advertise">>}]
                     }]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, [{'no_local', 'true'}]).

-define(EXPIRE_PERIOD, 1000).
-define(FUDGE_FACTOR, 1.25).
-define(APP_NAME, <<"wh_nodes">>).
-define(APP_VERSION, <<"0.1.0">>).

-record(state, {heartbeat_ref :: 'undefined' | reference()
                ,tab :: ets:tid()
                ,notify_new = sets:new() :: set()
                ,notify_expire = sets:new() :: set()
                ,node :: ne_binary()
                ,zone = 'local' :: atom()
               }).
-type nodes_state() :: #state{}.

-record(node, {node = node() :: atom() | '$1' | '_'
               ,expires = 0 :: non_neg_integer() | 'undefined' | '$2' | '_'
               ,whapps = [] :: ne_binaries() | '$1' | '_'
               ,media_servers = [] :: ne_binaries() | '_'
               ,last_heartbeat = wh_util:now_ms(now()) :: pos_integer() | 'undefined' | '$3' | '_'
               ,zone :: ne_binary() | '_'
               ,broker :: api_binary() | '_'
               ,used_memory = 0 :: non_neg_integer() | '_'
               ,processes = 0 :: non_neg_integer() | '_'
               ,ports = 0 :: non_neg_integer() | '_'
               ,version :: api_binary() | '_'
               ,channels = 0 :: non_neg_integer() | '_'
               ,registrations = 0 :: non_neg_integer() | '_'
              }).

-type wh_node() :: #node{}.
-type wh_nodes() :: [wh_node(),...] | [].

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_listener:start_link({'local', ?MODULE}, ?MODULE
                            ,[{'bindings', ?BINDINGS}
                              ,{'responders', ?RESPONDERS}
                              ,{'queue_name', ?QUEUE_NAME}
                              ,{'queue_options', ?QUEUE_OPTIONS}
                              ,{'consume_options', ?CONSUME_OPTIONS}
                             ], []).

-spec whapp_count(text()) -> integer().
whapp_count(Whapp) ->
    whapp_count(Whapp, 'false').

-spec whapp_count(text(), text() | boolean()) -> integer().
whapp_count(Whapp, 'false') ->
    MatchSpec = [{#node{whapps='$1'
                        ,zone = <<"local">>
                        ,_ = '_'
                       }
                  ,[{'=/=', '$1', []}]
                  ,['$1']
                 }],
    determine_whapp_count(wh_util:to_binary(Whapp), MatchSpec);
whapp_count(Whapp, 'true') ->
    MatchSpec = [{#node{whapps='$1'
                        ,_ = '_'
                       }
                  ,[{'=/=', '$1', []}]
                  ,['$1']
                 }],
    determine_whapp_count(wh_util:to_binary(Whapp), MatchSpec).

-spec determine_whapp_count(ne_binary(), ets:match_spec()) -> non_neg_integer().
determine_whapp_count(Whapp, MatchSpec) ->
    lists:foldl(fun(Whapps, Acc) when is_list(Whapps) ->
                        determine_whapp_count_fold(Whapps, Acc, Whapp)
                end, 0, ets:select(?MODULE, MatchSpec)).

-spec determine_whapp_count_fold(ne_binaries(), non_neg_integer(), ne_binary()) -> non_neg_integer().
determine_whapp_count_fold(Whapps, Acc, Whapp) ->
    case lists:member(Whapp, Whapps) of
        'true' -> Acc + 1;
        'false' -> Acc
    end.

-spec status() -> 'no_return'.
status() ->
    try
        Nodes = lists:sort(fun(N1, N2) ->
                                   N1#node.node > N2#node.node
                           end, ets:tab2list(?MODULE)),
        print_status(Nodes)
    catch
        {'EXIT', {'badarg', _}} ->
            io:format("status unknown until node is fully initialized, try again in a moment~n", []),
            'no_return'                
    end.

-spec print_status(wh_nodes()) -> 'no_return'.
print_status(Nodes) -> 
    _ = [begin
             MemoryUsage = wh_network_utils:pretty_print_bytes(Node#node.used_memory),
             io:format("Node          : ~s~n", [Node#node.node]),
             io:format("Version       : ~s~n", [Node#node.version]),
             io:format("Memory Usage  : ~s~n", [MemoryUsage]),
             io:format("Processes     : ~B~n", [Node#node.processes]),
             io:format("Ports         : ~B~n", [Node#node.ports]),
             io:format("Zone          : ~s~n", [Node#node.zone]),
             io:format("Broker        : ~s~n", [Node#node.broker]),
             _ = case lists:sort(Node#node.whapps) of
                     []-> 'ok';
                     Whapps ->
                         io:format("WhApps        : ", []),
                         status_list(Whapps, 0)
                 end,
             _ = case lists:sort(Node#node.media_servers) of
                     []-> 'ok';
                     [Server|Servers] ->
                         io:format("Channels      : ~B~n", [Node#node.channels]),
                         io:format("Registrations : ~B~n", [Node#node.registrations]),
                         io:format("Media Servers : ~s~n", [Server]),
                         [begin
                              io:format("                ~s~n", [S])
                          end
                          || S <- Servers
                         ]
                 end,
             io:format("~n", [])
         end
         || Node <- Nodes
        ],
    'no_return'.

-spec status_list(ne_binaries(), 0..4) -> 'ok'.
status_list([], _) -> io:format("~n", []);
status_list(Whapps, Column) when Column > 3 ->
    io:format("~n                ", []),
    status_list(Whapps, 0);
status_list([Whapp|Whapps], Column) ->
    io:format("~-20s", [Whapp]),
    status_list(Whapps, Column + 1).

-spec flush() -> 'ok'.
flush() ->
    gen_server:cast(?MODULE, 'flush').

-spec notify_new() -> 'ok'.
notify_new() ->
    notify_new(self()).

-spec notify_new(pid()) -> 'ok'.
notify_new(Pid) ->
    gen_server:cast(?MODULE, {'notify_new', Pid}).

-spec notify_expire() -> 'ok'.
notify_expire() ->
    notify_expire(self()).

-spec notify_expire(pid()) -> 'ok'.
notify_expire(Pid) ->
    gen_server:cast(?MODULE, {'notify_expire', Pid}).

-spec handle_advertise(wh_json:object(), wh_proplist()) -> 'ok'.
handle_advertise(JObj, Props) ->
    'true' = wapi_nodes:advertise_v(JObj),
    Srv = props:get_value('server', Props),
    Node = props:get_value('node', Props),
    case wh_json:get_value(<<"Node">>, JObj, Node) =:= Node of
        'false' -> gen_server:cast(Srv, {'advertise', JObj});
        'true' -> 'ok'
    end.

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
    wapi_nodes:declare_exchanges(),
    wapi_self:declare_exchanges(),
    Tab = ets:new(?MODULE, ['set'
                            ,'protected'
                            ,'named_table'
                            ,{'keypos', #node.node}
                           ]),
    _ = erlang:send_after(?EXPIRE_PERIOD, self(), 'expire_nodes'),
    net_kernel:monitor_nodes('true', ['nodedown_reason'
                                      ,{'node_type', 'all'}
                                     ]),
    State = #state{tab=Tab},
    Node = create_node('undefined', State),
    ets:insert(Tab, Node),
    {'ok', State#state{node=wh_util:to_binary(Node#node.node)}}.

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
%%--------------------------------------------------------------------
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
handle_cast({'notify_new', Pid}, #state{notify_new=Set}=State) ->
    _ = erlang:monitor('process', Pid),
    {'noreply', State#state{notify_new=sets:add_element(Pid, Set)}};
handle_cast({'notify_expire', Pid}, #state{notify_expire=Set}=State) ->
    _ = erlang:monitor('process', Pid),
    {'noreply', State#state{notify_expire=sets:add_element(Pid, Set)}};
handle_cast({'advertise', JObj}, #state{tab=Tab}=State) ->
    Node = from_json(JObj, State),
    _ = case ets:insert_new(Tab, Node) of
            'true' -> spawn(fun() -> notify_new(Node#node.node, State) end);
            'false' -> ets:insert(Tab, Node)
        end,
    {'noreply', State};
handle_cast({'gen_listener', {'created_queue', _}}
            ,#state{heartbeat_ref='undefined'}=State) ->
    Reference = erlang:make_ref(),
    self() ! {'heartbeat', Reference},
    {'noreply', State#state{heartbeat_ref=Reference}};
handle_cast('flush', State) ->
    ets:delete_all_objects(?MODULE),
    {'noreply', State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {'noreply', State} |
%%                                   {'noreply', State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info('expire_nodes', #state{tab=Tab}=State) ->
    Now = wh_util:now_ms(now()),
    FindSpec = [{#node{node='$1', expires='$2', last_heartbeat='$3'
                       ,_ = '_'}
                 ,[{'andalso'
                    ,{'=/=', '$2', 'undefined'}
                    ,{'>', {const, Now}, {'+', '$2', '$3'}}
                   }]
                 ,['$1']}
               ],
    Nodes = ets:select(Tab, FindSpec),
    _ = [ets:delete(Tab, Node) || Node <- Nodes],
    _ = spawn(fun() -> notify_expire(Nodes, State) end),
    _ = erlang:send_after(?EXPIRE_PERIOD, self(), 'expire_nodes'),
    {'noreply', State};
handle_info({'heartbeat', Ref}, #state{heartbeat_ref=Ref
                                       ,tab=Tab}=State) ->
    _ = ets:insert(Tab, create_node('undefined', State)),
    Heartbeat = crypto:rand_uniform(5000, 15000),
    try create_node(Heartbeat, State) of
        Node ->
            wapi_nodes:publish_advertise(advertise_payload(Node))
    catch
        _:_ -> 'ok'
    end,
    Reference = erlang:make_ref(),
    _ = erlang:send_after(Heartbeat, self(), {'heartbeat', Reference}),
    {'noreply', State#state{heartbeat_ref=Reference}};
handle_info({'DOWN', Ref, 'process', Pid, _}, #state{notify_new=NewSet
                                                     ,notify_expire=ExpireSet}=State) ->
    erlang:demonitor(Ref, ['flush']),
    {'noreply', State#state{notify_new=sets:del_element(Pid, NewSet)
                            ,notify_expire=sets:del_element(Pid, ExpireSet)
                           }};

handle_info({'nodedown', Node, InfoList}, State) ->
    lager:info("VM ~s is no longer connected:", [Node]),
    [lager:info(" ~p: ~p", [K, V]) || {K, V} <- InfoList],
    {'noreply', State};
handle_info({'nodedown', Node}, State) ->
    lager:info("VM ~s is no longer connected", [Node]),
    {'noreply', State};
handle_info({'nodeup', Node, InfoList}, State) ->
    lager:info("VM ~s is now connected:", [Node]),
    [lager:info(" ~p: ~p", [K, V]) || {K, V} <- InfoList],
    {'noreply', State};
handle_info({'nodeup', Node}, State) ->
    lager:info("VM ~s is now connected", [Node]),
    {'noreply', State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, #state{node=Node}) ->
    {'reply', [{'node', Node}]}.

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
    lager:debug("listener terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec create_node('undefined' | 5000..15000, nodes_state()) -> wh_node().
create_node(Heartbeat, #state{zone=Zone}) ->
    maybe_add_whapps_data(#node{expires=Heartbeat
                                ,broker=wh_amqp_connections:primary_broker()
                                ,used_memory=erlang:memory('total')
                                ,processes=erlang:system_info('process_count')
                                ,ports=length(erlang:ports())
                                ,version=wh_util:to_binary(erlang:system_info('otp_release'))
                                ,zone=wh_util:to_binary(Zone)
                               }).

-spec maybe_add_whapps_data(wh_node()) -> wh_node().
maybe_add_whapps_data(Node) ->
    case is_whapps_present() of
        'false' ->
            maybe_add_ecallmgr_data(Node);
        'true' ->
            add_whapps_data(Node)
    end.

-spec add_whapps_data(wh_node()) -> wh_node().
add_whapps_data(Node) ->
    Whapps = [wh_util:to_binary(Whapp)
              || Whapp <- whapps_controller:list_apps()
             ],
    maybe_add_ecallmgr_data(Node#node{whapps=Whapps}).

-spec maybe_add_ecallmgr_data(wh_node()) -> wh_node().
maybe_add_ecallmgr_data(Node) ->
    case is_ecallmgr_present() of
        'false' -> Node;
        'true' -> add_ecallmgr_data(Node)
    end.

-spec add_ecallmgr_data(wh_node()) -> wh_node().
add_ecallmgr_data(#node{whapps=Whapps}=Node) ->
    Servers = [wh_util:to_binary(Server)
               || Server <- ecallmgr_fs_nodes:connected()
              ],
    Node#node{media_servers=Servers
              ,whapps=[<<"ecallmgr">>|Whapps]
              ,channels=ecallmgr_fs_channels:count()
              ,registrations=ecallmgr_registrar:count()
             }.

-spec is_whapps_present() -> boolean().
is_whapps_present() ->
    lists:any(fun({'whistle_apps', _, _}) -> 'true'; (_) -> 'false' end
              ,application:loaded_applications()).

-spec is_ecallmgr_present() -> boolean().
is_ecallmgr_present() ->
    lists:any(fun({'ecallmgr', _, _}) -> 'true'; (_) -> 'false' end
              ,application:loaded_applications()).

-spec advertise_payload(wh_node()) -> wh_proplist().
advertise_payload(Node) ->
    [{<<"Expires">>, wh_util:to_binary(Node#node.expires)}
     ,{<<"WhApps">>, Node#node.whapps}
     ,{<<"Media-Servers">>, Node#node.media_servers}
     ,{<<"Used-Memory">>, Node#node.used_memory}
     ,{<<"Processes">>, Node#node.processes}
     ,{<<"Ports">>, Node#node.ports}
     ,{<<"Version">>, Node#node.version}
     ,{<<"Channels">>, Node#node.channels}
     ,{<<"Registrations">>, Node#node.registrations}
     ,{<<"Zone">>, Node#node.zone}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec from_json(wh_json:object(), nodes_state()) -> wh_node().
from_json(JObj, State) ->
    Node = wh_json:get_value(<<"Node">>, JObj),
    #node{node=wh_util:to_atom(Node, 'true')
          ,expires=wh_util:to_integer(wh_json:get_integer_value(<<"Expires">>, JObj, 0) * ?FUDGE_FACTOR)
          ,whapps=wh_json:get_value(<<"WhApps">>, JObj, [])
          ,media_servers=wh_json:get_value(<<"Media-Servers">>, JObj, [])
          ,used_memory=wh_json:get_integer_value(<<"Used-Memory">>, JObj, 0)
          ,processes=wh_json:get_integer_value(<<"Processes">>, JObj, 0)
          ,ports=wh_json:get_integer_value(<<"Ports">>, JObj, 0)
          ,version=wh_json:get_value(<<"Version">>, JObj, <<"unknown">>)
          ,channels=wh_json:get_integer_value(<<"Channels">>, JObj, 0)
          ,registrations=wh_json:get_integer_value(<<"Registrations">>, JObj, 0)
          ,zone=get_zone(JObj, State)
          ,broker=get_amqp_broker(JObj)
         }.

-spec get_zone(wh_json:object(), nodes_state()) -> ne_binary().
get_zone(JObj, #state{zone=Zone}) ->
    case wh_json:get_value(<<"Zone">>, JObj, Zone) of
        Zone -> <<"local">>;
        RemoteZone -> RemoteZone
    end.

-spec get_amqp_broker(api_binary() | wh_json:object()) -> api_binary().
get_amqp_broker('undefined') ->
    wh_amqp_connections:primary_broker();
get_amqp_broker(Broker) when is_binary(Broker) -> Broker;
get_amqp_broker(JObj) ->
    get_amqp_broker(wh_json:get_ne_value(<<"AMQP-Broker">>, JObj)).

-spec notify_expire(atoms(), nodes_state() | pids()) -> 'ok'.
notify_expire([], _) -> 'ok';
notify_expire(_, []) -> 'ok';
notify_expire(Nodes, #state{notify_expire=Set}) ->
    notify_expire(Nodes, sets:to_list(Set));
notify_expire([Node|Nodes], Pids) ->
    lager:warning("node ~s heartbeat has expired", [Node]),
    _ = [gen_server:cast(Pid, {'wh_nodes', {'expire', Node}})
         || Pid <- Pids
        ],
    notify_expire(Nodes, Pids).

-spec notify_new(atom(), nodes_state() | pids()) -> 'ok'.
notify_new(Node, #state{notify_new=Set}) ->
    notify_new(Node, sets:to_list(Set));
notify_new(Node, Pids) ->
    lager:info("recieved heartbeat from new node ~s", [Node]),
    _ = [gen_server:cast(Pid, {'wh_nodes', {'new', Node}})
         || Pid <- Pids
        ],
    'ok'.

-spec get_zone_name() -> atom().
get_zone_name() ->
    case wh_config:get(wh_config:get_node_section_name(), 'zone') of
        [Zone] -> Zone;
        _Else -> 'local'
    end.
