%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
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
         ,whapp_oldest_node/1
         ,whapp_oldest_node/2
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
-export([local_zone/0]).
-export([whapp_zones/1, whapp_zone_count/1]).

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
                     }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, [{'no_local', 'true'}]).

-define(HEARTBEAT, crypto:rand_uniform(5 * ?MILLISECONDS_IN_SECOND, 15 * ?MILLISECONDS_IN_SECOND)).
-define(EXPIRE_PERIOD, 1 * ?MILLISECONDS_IN_SECOND).
-define(FUDGE_FACTOR, 1.25).
-define(APP_NAME, <<"wh_nodes">>).
-define(APP_VERSION, <<"0.1.0">>).

-define(MEDIA_SERVERS_HEADER, "Media Servers : ").
-define(MEDIA_SERVERS_LINE, "                ").
-define(MEDIA_SERVERS_DETAIL, "~s (~s)").

-record(state, {heartbeat_ref :: reference()
                ,tab :: ets:tid()
                ,notify_new = sets:new() :: set()
                ,notify_expire = sets:new() :: set()
                ,node = node() :: atom()
                ,zone = 'local' :: atom()
                ,version :: ne_binary()
                ,zones = [] :: wh_proplist()
               }).
-type nodes_state() :: #state{}.

-record(whapp_info, {startup :: gregorian_seconds()}).

-type whapp_info() :: #whapp_info{}.
-type whapps_info() :: [{binary(), whapp_info()}].

-type media_server() :: {ne_binary(), wh_json:object()}.
-type media_servers() :: [media_server()].

-record(node, {node = node() :: atom() | '$1' | '$2' | '_'
               ,expires = 0 :: non_neg_integer() | 'undefined' | '$2' | '_'
               ,whapps = [] :: whapps_info() | '$1' | '_'
               ,media_servers = [] :: media_servers() | '_'
               ,last_heartbeat = wh_util:now_ms(wh_util:now()) :: pos_integer() | 'undefined' | '$3' | '_'
               ,zone :: atom() | 'undefined' | '$2' | '_'
               ,broker :: api_binary() | '_'
               ,used_memory = 0 :: non_neg_integer() | '_'
               ,processes = 0 :: non_neg_integer() | '_'
               ,ports = 0 :: non_neg_integer() | '_'
               ,version :: api_binary() | '_'
               ,channels = 0 :: non_neg_integer() | '_'
               ,registrations = 0 :: non_neg_integer() | '_'
              }).

-type wh_node() :: #node{}.
-type wh_nodes() :: [wh_node()].

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
    gen_listener:start_link({'local', ?MODULE}
                            ,?MODULE
                            ,[{'bindings', ?BINDINGS}
                              ,{'responders', ?RESPONDERS}
                              ,{'queue_name', ?QUEUE_NAME}
                              ,{'queue_options', ?QUEUE_OPTIONS}
                              ,{'consume_options', ?CONSUME_OPTIONS}
                             ]
                            ,[]
                           ).

-spec whapp_count(text()) -> integer().
whapp_count(Whapp) ->
    whapp_count(Whapp, 'false').

-spec whapp_count(text(), text() | boolean() | 'remote') -> integer().
whapp_count(Whapp, Arg) when not is_atom(Arg) ->
    whapp_count(Whapp, wh_util:to_atom(Arg, 'true'));
whapp_count(Whapp, 'false') ->
    MatchSpec = [{#node{whapps='$1'
                        ,zone = local_zone()
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
    determine_whapp_count(wh_util:to_binary(Whapp), MatchSpec);
whapp_count(Whapp, 'remote') ->
    Zone = local_zone(),
    MatchSpec = [{#node{whapps='$1'
                        ,zone='$2'
                        ,_ = '_'
                       }
                  ,[{'andalso', {'=/=', '$1', []}
                              , {'=/=', '$2', {'const', Zone}}
                    }]
                  ,['$1']
                 }],
    determine_whapp_count(wh_util:to_binary(Whapp), MatchSpec);
whapp_count(Whapp, Unhandled) ->
    lager:debug("invalid parameters", [Whapp, Unhandled]),
    0.

-spec determine_whapp_count(ne_binary(), ets:match_spec()) -> non_neg_integer().
determine_whapp_count(Whapp, MatchSpec) ->
    lists:foldl(fun(Whapps, Acc) when is_list(Whapps) ->
                        determine_whapp_count_fold(Whapps, Acc, Whapp)
                end, 0, ets:select(?MODULE, MatchSpec)).

-spec determine_whapp_count_fold(whapps_info(), non_neg_integer(), ne_binary()) -> non_neg_integer().
determine_whapp_count_fold(Whapps, Acc, Whapp) ->
    case props:is_defined(Whapp, Whapps) of
        'true' -> Acc + 1;
        'false' -> Acc
    end.

-spec whapp_zones(text()) -> list().
whapp_zones(Whapp) ->
    MatchSpec = [{#node{whapps='$1'
                        ,zone='$2'
                        ,_ = '_'
                       }
                  ,[{'=/=', '$1', []}]
                  ,[{{'$2', '$1'}}]
                 }],
    determine_whapp_zones(wh_util:to_binary(Whapp), MatchSpec).

-spec determine_whapp_zones(ne_binary(), ets:match_spec()) -> list().
determine_whapp_zones(Whapp, MatchSpec) ->
    {Whapp, Zones, _} = lists:foldl(fun determine_whapp_zones_fold/2, {Whapp, [],0}, ets:select(?MODULE, MatchSpec)),
    Zones.

-spec whapp_zone_count(text()) -> integer().
whapp_zone_count(Whapp) ->
    length(whapp_zones(Whapp)).

-type fold_zones_acc() :: {atom(), list(), non_neg_integer()}.

-spec determine_whapp_zones_fold({atom(), whapps_info()}, fold_zones_acc()) -> fold_zones_acc().
determine_whapp_zones_fold({Zone, Whapps}, {Whapp, Zones, C}=Acc) ->
    case props:is_defined(Whapp, Whapps) andalso
             not lists:member(Zone, Zones) of
        'true' -> {Whapp, [Zone | Zones], C+ 1};
        'false' -> Acc
    end.

-spec status() -> 'no_return'.
status() ->
    try
        Nodes = lists:sort(fun(N1, N2) ->
                                   N1#node.node > N2#node.node
                           end, ets:tab2list(?MODULE)),
        print_status(Nodes, gen_listener:call(?MODULE, 'zone'))
    catch
        {'EXIT', {'badarg', _}} ->
            io:format("status unknown until node is fully initialized, try again in a moment~n", []),
            'no_return'
    end.

-spec print_status(wh_nodes(), atom()) -> 'no_return'.
print_status(Nodes, Zone) ->
    _ = [print_node_status(Node, Zone) || Node <- Nodes],
    'no_return'.

-spec print_node_status(wh_node(), atom()) -> 'ok'.
print_node_status(#node{zone=NodeZone
                        ,node=N
                        ,version=Version
                        ,processes=Processes
                        ,ports=Ports
                        ,used_memory=UsedMemory
                        ,broker=Broker
                        ,whapps=Whapps
                       }=Node
                  ,Zone
                 ) ->
    MemoryUsage = wh_network_utils:pretty_print_bytes(UsedMemory),
    io:format("Node          : ~s~n", [N]),
    io:format("Version       : ~s~n", [Version]),
    io:format("Memory Usage  : ~s~n", [MemoryUsage]),
    io:format("Processes     : ~B~n", [Processes]),
    io:format("Ports         : ~B~n", [Ports]),

    _ = maybe_print_zone(wh_util:to_binary(NodeZone)
                         ,wh_util:to_binary(Zone)
                        ),

    io:format("Broker        : ~s~n", [Broker]),

    _ = maybe_print_whapps(Whapps),
    _ = maybe_print_media_servers(Node),

    io:format("~n").

-spec maybe_print_zone(ne_binary(), ne_binary()) -> 'ok'.
maybe_print_zone(Zone, Zone) when Zone =/= <<"local">> ->
    io:format("Zone          : ~s (local)~n", [Zone]);
maybe_print_zone(NodeZone, _Zone) ->
    io:format("Zone          : ~s~n", [NodeZone]).

-spec maybe_print_whapps(wh_proplist()) -> 'ok'.
maybe_print_whapps(Whapps) ->
    case lists:sort(fun({K1,_}, {K2,_}) -> K1 < K2 end
                    ,Whapps
                   )
    of
        []-> 'ok';
        SortedWhapps ->
            io:format("WhApps        : ", []),
            status_list(SortedWhapps, 0)
    end.

-spec maybe_print_media_servers(wh_node()) -> 'ok'.
maybe_print_media_servers(#node{media_servers=MediaServers
                                ,registrations=Registrations
                                ,channels=Channels
                               }) ->
    case lists:sort(MediaServers) of
        [] when Registrations =:= 0 -> 'ok';
        [] when Registrations > 0 ->
            io:format("Registrations : ~B~n", [Registrations]);
        [Server|Servers] ->
            io:format("Channels      : ~B~n", [Channels]),
            io:format("Registrations : ~B~n", [Registrations]),
            print_media_server(Server, ?MEDIA_SERVERS_HEADER),
            _ =[print_media_server(OtherServer) || OtherServer <- Servers],
            'ok'
    end.

-spec print_media_server(media_server()) -> 'ok'.
print_media_server(Server) ->
    print_media_server(Server, ?MEDIA_SERVERS_LINE).

-spec print_media_server(media_server(), string()) -> 'ok'.
print_media_server({Name, JObj}, Format) ->
    io:format(lists:flatten([Format, ?MEDIA_SERVERS_DETAIL, "~n"]),
              [Name
              ,wh_util:pretty_print_elapsed_s(wh_util:elapsed_s(wh_json:get_integer_value(<<"Startup">>, JObj)))
              ]).

-spec status_list(whapps_info(), 0..4) -> 'ok'.
status_list([], _) -> io:format("~n", []);
status_list(Whapps, Column) when Column > 3 ->
    io:format("~n~-16s", [""]),
    status_list(Whapps, 0);
status_list([{Whapp, #whapp_info{startup='undefined'}}|Whapps], Column) ->
    io:format("~-25s", [Whapp]),
    status_list(Whapps, Column + 1);
status_list([{Whapp, #whapp_info{startup=Started}}|Whapps], Column) ->
    Elapsed = wh_util:elapsed_s(Started),
    Print = <<(wh_util:to_binary(Whapp))/binary, "(", (wh_util:pretty_print_elapsed_s(Elapsed))/binary, ")">>,
    io:format("~-25s", [Print]),
    status_list(Whapps, Column + 1).

-spec flush() -> 'ok'.
flush() ->
    gen_listener:cast(?MODULE, 'flush').

-spec notify_new() -> 'ok'.
notify_new() ->
    notify_new(self()).

-spec notify_new(pid()) -> 'ok'.
notify_new(Pid) ->
    gen_listener:cast(?MODULE, {'notify_new', Pid}).

-spec notify_expire() -> 'ok'.
notify_expire() ->
    notify_expire(self()).

-spec notify_expire(pid()) -> 'ok'.
notify_expire(Pid) ->
    gen_listener:cast(?MODULE, {'notify_expire', Pid}).

-spec handle_advertise(wh_json:object(), wh_proplist()) -> 'ok'.
handle_advertise(JObj, Props) ->
    'true' = wapi_nodes:advertise_v(JObj),
    Srv = props:get_value('server', Props),
    Node = props:get_value('node', Props),
    case wh_json:get_value(<<"Node">>, JObj, Node) =:= Node of
        'false' -> gen_listener:cast(Srv, {'advertise', JObj});
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
    lager:debug("starting nodes watcher"),
    wapi_nodes:declare_exchanges(),
    wapi_self:declare_exchanges(),
    Tab = ets:new(?MODULE, ['set'
                            ,'protected'
                            ,'named_table'
                            ,{'keypos', #node.node}
                           ]),
    lager:debug("started ETS ~p", [Tab]),
    _ = erlang:send_after(?EXPIRE_PERIOD, self(), 'expire_nodes'),
    _ = net_kernel:monitor_nodes('true', ['nodedown_reason'
                                          ,{'node_type', 'all'}
                                         ]),
    lager:debug("monitoring nodes"),
    State = #state{tab=Tab, zone=get_zone()},
    Version = <<(wh_util:whistle_version())/binary
                ," - "
                ,(wh_util:to_binary(erlang:system_info('otp_release')))/binary
              >>,
    {'ok', State#state{version=Version}}.

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
handle_call({'print_status', Nodes}, _From, State) ->
    print_status(Nodes, State),
    {'reply', 'ok', State};
handle_call('zone', _From, #state{zone=Zone}=State) ->
    {'reply', Zone, State};
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
    #node{node=N}=Node = from_json(JObj, State),
    _ = case ets:insert_new(Tab, Node) of
            'true' -> wh_util:spawn(fun() -> notify_new(N, State) end);
            'false' -> ets:insert(Tab, Node)
        end,
    {'noreply', maybe_add_zone(Node, State)};
handle_cast({'gen_listener', {'created_queue', _}}
            ,#state{heartbeat_ref='undefined'}=State
           ) ->
    Reference = erlang:make_ref(),
    erlang:send_after(?HEARTBEAT, self(), {'heartbeat', Reference}),
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
    Now = wh_util:now_ms(wh_util:now()),
    FindSpec = [{#node{node='$1'
                       ,expires='$2'
                       ,last_heartbeat='$3'
                       ,_ = '_'
                      }
                 ,[{'andalso'
                    ,{'=/=', '$2', 'undefined'}
                    ,{'>', {'const', Now}, {'+', '$2', '$3'}}
                   }]
                 ,['$1']}
               ],
    Nodes = ets:select(Tab, FindSpec),
    _ = [ets:delete(Tab, Node) || Node <- Nodes],
    _ = wh_util:spawn(fun() -> notify_expire(Nodes, State) end),
    _ = erlang:send_after(?EXPIRE_PERIOD, self(), 'expire_nodes'),
    {'noreply', State};
handle_info({'heartbeat', Ref}, #state{heartbeat_ref=Ref
                                       ,tab=Tab
                                      }=State) ->
    Heartbeat = ?HEARTBEAT,
    Reference = erlang:make_ref(),
    try create_node(Heartbeat, State) of
        Node ->
            _ = ets:insert(Tab, Node),
            wapi_nodes:publish_advertise(advertise_payload(Node))
    catch
        _:_ -> 'ok'
    end,
    _ = erlang:send_after(Heartbeat, self(), {'heartbeat', Reference}),
    {'noreply', State#state{heartbeat_ref=Reference}};
handle_info({'DOWN', Ref, 'process', Pid, _}, #state{notify_new=NewSet
                                                     ,notify_expire=ExpireSet
                                                    }=State) ->
    erlang:demonitor(Ref, ['flush']),
    {'noreply', State#state{notify_new=sets:del_element(Pid, NewSet)
                            ,notify_expire=sets:del_element(Pid, ExpireSet)
                           }};

handle_info({'nodedown', Node, InfoList}, State) ->
    lager:info("VM ~s is no longer connected:", [Node]),
    _ = [lager:info(" ~p: ~p", [K, V]) || {K, V} <- InfoList],
    {'noreply', State};
handle_info({'nodedown', Node}, State) ->
    lager:info("VM ~s is no longer connected", [Node]),
    {'noreply', State};
handle_info({'nodeup', Node, InfoList}, State) ->
    lager:info("VM ~s is now connected:", [Node]),
    _ = [lager:info(" ~p: ~p", [K, V]) || {K, V} <- InfoList],
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
    {'reply', [{'node', wh_util:to_binary(Node)}]}.

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
create_node(Heartbeat, #state{zone=Zone
                              ,version=Version
                             }) ->
    maybe_add_whapps_data(#node{expires=Heartbeat
                                ,broker=wh_util:normalize_amqp_uri(wh_amqp_connections:primary_broker())
                                ,used_memory=erlang:memory('total')
                                ,processes=erlang:system_info('process_count')
                                ,ports=length(erlang:ports())
                                ,version=Version
                                ,zone=Zone
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
    Whapps = [{wh_util:to_binary(Whapp), get_whapp_info(Whapp)}
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
    Servers = [{wh_util:to_binary(Server)
                ,wh_json:set_values(
                   [{<<"Startup">>, Started}
                    ,{<<"Interface">>, wh_json:from_list(ecallmgr_fs_node:interface(Server))}
                   ]
                   ,wh_json:new()
                  )
               }
               || {Server, Started} <- ecallmgr_fs_nodes:connected('true')
              ],
    Node#node{media_servers=Servers
              ,whapps=[{<<"ecallmgr">>, get_whapp_info('ecallmgr')} | Whapps]
              ,channels=ecallmgr_fs_channels:count()
              ,registrations=ecallmgr_registrar:count()
             }.

-spec get_whapp_info(atom() | pid() | wh_proplist() | 'undefined') -> whapp_info().
get_whapp_info('undefined') -> #whapp_info{};
get_whapp_info(Whapp) when is_atom(Whapp) ->
    get_whapp_info(application_controller:get_master(Whapp));
get_whapp_info(Master) when is_pid(Master) ->
    get_whapp_info(application_master:get_child(Master));
get_whapp_info({Pid, _Module}) when is_pid(Pid) ->
    get_whapp_process_info(erlang:process_info(Pid));
get_whapp_info(_Arg) ->
    lager:debug("can't get info for ~p", [_Arg]),
    #whapp_info{}.

-spec get_whapp_process_info(wh_proplist() | 'undefined') -> whapp_info().
get_whapp_process_info('undefined') -> #whapp_info{};
get_whapp_process_info([]) -> #whapp_info{};
get_whapp_process_info(PInfo) ->
    Startup = props:get_value('$startup', props:get_value('dictionary', PInfo, [])),
    #whapp_info{startup=Startup}.

-spec is_whapps_present() -> boolean().
is_whapps_present() ->
    lists:any(fun({'whistle_apps', _, _}) -> 'true';
                 (_) -> 'false'
              end
              ,application:loaded_applications()
             ).

-spec is_ecallmgr_present() -> boolean().
is_ecallmgr_present() ->
    lists:any(fun({'ecallmgr', _, _}) -> 'true';
                 (_) -> 'false'
              end
              ,application:loaded_applications()
             )
        andalso whereis('ecallmgr_fs_nodes') =/= 'undefined'
        andalso whereis('ecallmgr_fs_channels') =/= 'undefined'.

-spec advertise_payload(wh_node()) -> wh_proplist().
advertise_payload(#node{expires=Expires
                        ,whapps=Whapps
                        ,media_servers=MediaServers
                        ,used_memory=UsedMemory
                        ,processes=Processes
                        ,ports=Ports
                        ,version=Version
                        ,channels=Channels
                        ,registrations=Registrations
                        ,zone=Zone
                       }) ->
    props:filter_undefined(
      [{<<"Expires">>, wh_util:to_binary(Expires)}
       ,{<<"WhApps">>, whapps_to_json(Whapps) }
       ,{<<"Media-Servers">>, media_servers_to_json(MediaServers)}
       ,{<<"Used-Memory">>, UsedMemory}
       ,{<<"Processes">>, Processes}
       ,{<<"Ports">>, Ports}
       ,{<<"Version">>, Version}
       ,{<<"Channels">>, Channels}
       ,{<<"Registrations">>, Registrations}
       ,{<<"Zone">>, wh_util:to_binary(Zone)}
       | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).

-spec media_servers_to_json(media_servers()) -> wh_json:object().
media_servers_to_json([]) -> 'undefined';
media_servers_to_json(Servers) ->
    wh_json:from_list(Servers).

-spec media_servers_from_json(wh_json:object()) -> media_servers().
media_servers_from_json(Servers) ->
    [{Key, wh_json:get_value(Key, Servers)}
     || Key <- wh_json:get_keys(Servers)
    ].

-spec from_json(wh_json:object(), nodes_state()) -> wh_node().
from_json(JObj, State) ->
    Node = wh_json:get_value(<<"Node">>, JObj),
    #node{node=wh_util:to_atom(Node, 'true')
          ,expires=wh_util:to_integer(wh_json:get_integer_value(<<"Expires">>, JObj, 0) * ?FUDGE_FACTOR)
          ,whapps=whapps_from_json(wh_json:get_value(<<"WhApps">>, JObj, []))
          ,media_servers=media_servers_from_json(wh_json:get_value(<<"Media-Servers">>, JObj, wh_json:new()))
          ,used_memory=wh_json:get_integer_value(<<"Used-Memory">>, JObj, 0)
          ,processes=wh_json:get_integer_value(<<"Processes">>, JObj, 0)
          ,ports=wh_json:get_integer_value(<<"Ports">>, JObj, 0)
          ,version=wh_json:get_first_defined([<<"Version">>, <<"App-Version">>], JObj, <<"unknown">>)
          ,channels=wh_json:get_integer_value(<<"Channels">>, JObj, 0)
          ,registrations=wh_json:get_integer_value(<<"Registrations">>, JObj, 0)
          ,broker=get_amqp_broker(JObj)
          ,zone=get_zone(JObj, State)
         }.

-spec whapps_from_json(api_terms()) -> whapps_info().
-spec whapp_from_json(binary(), wh_json:object()) -> {binary(), whapp_info()}.
-spec whapp_info_from_json(wh_json:object()) -> whapp_info().

whapps_from_json(Whapps) when is_list(Whapps) ->
    [{Whapp, #whapp_info{}} || Whapp <- Whapps];
whapps_from_json(JObj) ->
    Keys = wh_json:get_keys(JObj),
    [whapp_from_json(Key, JObj) || Key <- Keys].

whapp_from_json(Key, JObj) ->
    {Key, whapp_info_from_json(wh_json:get_value(Key, JObj))}.

whapp_info_from_json(JObj) ->
    case wh_json:get_value(<<"Startup">>, JObj) of
        'undefined' ->
            #whapp_info{};
        V when V < ?UNIX_EPOCH_IN_GREGORIAN ->
            #whapp_info{startup=wh_util:unix_seconds_to_gregorian_seconds(V)};
        V ->
            #whapp_info{startup=V}
    end.

-spec whapps_to_json(whapps_info()) -> wh_json:object().
-spec whapp_to_json({ne_binary(), whapp_info()}) -> {ne_binary(), wh_json:object()}.
-spec whapp_info_to_json(whapp_info()) -> wh_json:object().

whapps_to_json(Whapps) ->
    List = [whapp_to_json(Whapp) || Whapp <- Whapps],
    wh_json:from_list(List).

whapp_to_json({K, Info}) ->
    {K, whapp_info_to_json(Info)}.

whapp_info_to_json(#whapp_info{startup=Start}) ->
    wh_json:from_list(
      props:filter_undefined(
        [{<<"Startup">>, Start}]
       )).

-spec get_zone() -> atom().
get_zone() ->
    case wh_config:get(wh_config:get_node_section_name(), 'zone') of
        [Zone] -> Zone;
        _Else -> 'local'
    end.

-spec get_zone(wh_json:object(), nodes_state()) -> atom().
get_zone(JObj, #state{zones=Zones, zone=LocalZone}) ->
    case wh_json:get_first_defined([<<"Zone">>, <<"AMQP-Broker-Zone">>], JObj) of
        'undefined' ->
            case wh_json:get_value(<<"AMQP-Broker">>, JObj) of
                'undefined' -> LocalZone;
                Broker ->
                    case props:get_value(Broker, Zones) of
                        'undefined' -> LocalZone;
                        Zone -> wh_util:to_atom(Zone, 'true')
                    end
            end;
        Zone -> wh_util:to_atom(Zone, 'true')
    end.


-spec local_zone() -> atom().
local_zone() ->
    case get('amqp_zone') of
        'undefined' ->
            Zone = gen_listener:call(?MODULE, 'zone'),
            put('amqp_zone', Zone),
            Zone;
        Zone -> Zone
    end.

-spec get_amqp_broker(api_binary() | wh_json:object()) -> api_binary().
get_amqp_broker('undefined') ->
    wh_util:normalize_amqp_uri(wh_amqp_connections:primary_broker());
get_amqp_broker(Broker) when is_binary(Broker) -> wh_util:normalize_amqp_uri(Broker);
get_amqp_broker(JObj) ->
    get_amqp_broker(wh_json:get_ne_value(<<"AMQP-Broker">>, JObj)).

-spec notify_expire(atoms(), nodes_state() | pids()) -> 'ok'.
notify_expire([], _) -> 'ok';
notify_expire(_, []) -> 'ok';
notify_expire(Nodes, #state{notify_expire=Set}) ->
    notify_expire(Nodes, sets:to_list(Set));
notify_expire([Node|Nodes], Pids) ->
    lager:warning("node ~s heartbeat has expired", [Node]),
    _ = [gen_listener:cast(Pid, {'wh_nodes', {'expire', Node}})
         || Pid <- Pids
        ],
    notify_expire(Nodes, Pids).

-spec notify_new(atom(), nodes_state() | pids()) -> 'ok'.
notify_new(Node, #state{notify_new=Set}) ->
    notify_new(Node, sets:to_list(Set));
notify_new(Node, Pids) ->
    lager:info("recieved heartbeat from new node ~s", [Node]),
    _ = [gen_listener:cast(Pid, {'wh_nodes', {'new', Node}})
         || Pid <- Pids
        ],
    'ok'.

-spec whapp_oldest_node(text()) -> api_integer().
whapp_oldest_node(Whapp) ->
    whapp_oldest_node(Whapp, 'false').

-spec whapp_oldest_node(text(), text() | boolean() | atom()) -> api_integer().
whapp_oldest_node(Whapp, 'false') ->
    Zone = gen_listener:call(?MODULE, 'zone'),
    MatchSpec = [{#node{whapps='$1'
                        ,node='$2'
                        ,zone = Zone
                        ,_ = '_'
                       }
                  ,[{'=/=', '$1', []}]
                  ,[{{'$1', '$2'}}]
                 }],
    determine_whapp_oldest_node(wh_util:to_binary(Whapp), MatchSpec);
whapp_oldest_node(Whapp, 'true') ->
    MatchSpec = [{#node{whapps='$1'
                        ,node='$2'
                        ,_ = '_'
                       }
                  ,[{'=/=', '$1', []}]
                  ,[{{'$1','$2'}}]
                 }],
    determine_whapp_oldest_node(wh_util:to_binary(Whapp), MatchSpec);
whapp_oldest_node(Whapp, Federated)
  when is_binary(Federated) ->
    whapp_oldest_node(Whapp, wh_util:is_true(Federated));
whapp_oldest_node(Whapp, Zone)
  when is_atom(Zone) ->
    MatchSpec = [{#node{whapps='$1'
                        ,node='$2'
                        ,zone = Zone
                        ,_ = '_'
                       }
                  ,[{'=/=', '$1', []}]
                  ,[{{'$1', '$2'}}]
                 }],
    determine_whapp_oldest_node(wh_util:to_binary(Whapp), MatchSpec).

-spec determine_whapp_oldest_node(ne_binary(), ets:match_spec()) -> api_integer().
determine_whapp_oldest_node(Whapp, MatchSpec) ->
    case lists:foldl(fun({Whapps, _Node}=Info, Acc) when is_list(Whapps) ->
                             determine_whapp_oldest_node_fold(Info, Acc, Whapp)
                     end, 'undefined', ets:select(?MODULE, MatchSpec)) of
        {Node, _Start} -> Node;
        'undefined' -> 'undefined'
    end.

-spec determine_whapp_oldest_node_fold({whapps_info(), node()}
                                       ,'undefined' | {node(), gregorian_seconds()}
                                       ,ne_binary()
                                       ) -> 'undefined' | {node(), gregorian_seconds()}.
determine_whapp_oldest_node_fold({Whapps, Node}, 'undefined', Whapp) ->
    case props:get_value(Whapp, Whapps) of
        'undefined' -> 'undefined';
        #whapp_info{startup=Start} -> {Node, Start}
    end;
determine_whapp_oldest_node_fold({Whapps, Node}, {_, Startup}=Acc, Whapp) ->
    case props:get_value(Whapp, Whapps) of
        'undefined' -> Acc;
        #whapp_info{startup='undefined'} -> Acc;
        #whapp_info{startup=Start}
          when Start =< Startup -> {Node, Start};
        _ -> Acc
    end.

maybe_add_zone(#node{zone='undefined'}, #state{}=State) -> State;
maybe_add_zone(#node{zone=Zone, broker=B}, #state{zones=Zones}=State) ->
    Broker = wh_util:normalize_amqp_uri(B),
    case props:get_value(Broker, Zones) of
        'undefined' -> State#state{zones=[{Broker, Zone} | Zones]};
        _ -> State
    end.
