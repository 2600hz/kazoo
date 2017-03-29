%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(kz_nodes).
-behaviour(gen_listener).

-export([start_link/0]).
-export([is_up/1]).
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
        ,request/1
        ]).
-export([local_zone/0]).
-export([whapp_zones/1, whapp_zone_count/1]).
-export([globals_scope/0]).
-export([node_encoded/0]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-type request_info() :: {'app', atom()} |
                        {'media_servers', [{ne_binary(), kz_json:object()}]} |
                        {'channels', non_neg_integer()} |
                        {'registrations', non_neg_integer()} |
                        {'info', whapp_info()}.
-type request_acc() :: [request_info()].

-export_type([request_acc/0]).

-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").

-define(SERVER, ?MODULE).

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
-define(APP_NAME, <<"kz_nodes">>).
-define(APP_VERSION, <<"4.0.0">>).

%% kz_nodes lives in this app
-define(APP_NAME_ATOM, 'kazoo_globals').

-define(MEDIA_SERVERS_HEADER, "Media Servers : ").
-define(MEDIA_SERVERS_LINE, "                ").
-define(MEDIA_SERVERS_DETAIL, "~s (~s)").

-define(HEADER_COL, "~-14s").
-define(SIMPLE_ROW_STR, ?HEADER_COL ": ~s~n").
-define(SIMPLE_ROW_NUM, ?HEADER_COL ": ~B~n").

-record(state, {heartbeat_ref = erlang:make_ref() :: reference()
               ,tab :: ets:tid()
               ,notify_new = sets:new() :: sets:set()
               ,notify_expire = sets:new() :: sets:set()
               ,node = node() :: atom()
               ,md5 :: ne_binary()
               ,zone = 'local' :: atom()
               ,version :: ne_binary()
               ,zones = [] :: kz_proplist()
               }).
-type nodes_state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link({'local', ?SERVER}
                           ,?MODULE
                           ,[{'bindings', ?BINDINGS}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}
                            ,{'queue_options', ?QUEUE_OPTIONS}
                            ,{'consume_options', ?CONSUME_OPTIONS}
                            ]
                           ,[]
                           ).

-spec is_up(node()) -> boolean().
is_up(Node) ->
    case ets:match(?MODULE, #kz_node{node = Node
                                    ,expires = '$2'
                                    ,_ = '_'
                                    })
    of
        [] -> 'false';
        [_] -> 'true'
    end.

-spec globals_scope() -> integer().
globals_scope() ->
    MatchSpec = [{#kz_node{globals='$1'
                          ,node='$2'
                          ,_ = '_'
                          }
                 ,[{'andalso'
                   ,{'=/=', '$1', []}
                   ,{'=/=', '$2', {'const', node()}}
                   }]
                 ,['$1']
                 }],
    length(ets:select(?MODULE, MatchSpec)).

-spec whapp_count(text()) -> integer().
whapp_count(Whapp) ->
    whapp_count(Whapp, 'false').

-spec whapp_count(text(), text() | boolean() | 'remote') -> integer().
whapp_count(Whapp, Arg) when not is_atom(Arg) ->
    whapp_count(Whapp, kz_term:to_atom(Arg, 'true'));
whapp_count(Whapp, 'false') ->
    MatchSpec = [{#kz_node{kapps='$1'
                          ,zone = local_zone()
                          ,_ = '_'
                          }
                 ,[{'=/=', '$1', []}]
                 ,['$1']
                 }],
    determine_whapp_count(kz_term:to_binary(Whapp), MatchSpec);
whapp_count(Whapp, 'true') ->
    MatchSpec = [{#kz_node{kapps='$1'
                          ,_ = '_'
                          }
                 ,[{'=/=', '$1', []}]
                 ,['$1']
                 }],
    determine_whapp_count(kz_term:to_binary(Whapp), MatchSpec);
whapp_count(Whapp, 'remote') ->
    Zone = local_zone(),
    MatchSpec = [{#kz_node{kapps='$1'
                          ,zone='$2'
                          ,_ = '_'
                          }
                 ,[{'andalso'
                   ,{'=/=', '$1', []}
                   ,{'=/=', '$2', {'const', Zone}}
                   }]
                 ,['$1']
                 }],
    determine_whapp_count(kz_term:to_binary(Whapp), MatchSpec);
whapp_count(Whapp, Unhandled) ->
    lager:debug("invalid parameters", [Whapp, Unhandled]),
    0.

-spec determine_whapp_count(ne_binary(), ets:match_spec()) -> non_neg_integer().
determine_whapp_count(Whapp, MatchSpec) ->
    lists:foldl(fun(Whapps, Acc) when is_list(Whapps) ->
                        determine_whapp_count_fold(Whapps, Acc, Whapp)
                end
               ,0
               ,ets:select(?MODULE, MatchSpec)
               ).

-spec determine_whapp_count_fold(kapps_info(), non_neg_integer(), ne_binary()) -> non_neg_integer().
determine_whapp_count_fold(Whapps, Acc, Whapp) ->
    case props:is_defined(Whapp, Whapps) of
        'true' -> Acc + 1;
        'false' -> Acc
    end.

-spec whapp_zones(text()) -> list().
whapp_zones(Whapp) ->
    MatchSpec = [{#kz_node{kapps='$1'
                          ,zone='$2'
                          ,_ = '_'
                          }
                 ,[{'=/=', '$1', []}]
                 ,[{{'$2', '$1'}}]
                 }],
    determine_whapp_zones(kz_term:to_binary(Whapp), MatchSpec).

-spec determine_whapp_zones(ne_binary(), ets:match_spec()) -> list().
determine_whapp_zones(Whapp, MatchSpec) ->
    {Whapp, Zones, _} =
        lists:foldl(fun determine_whapp_zones_fold/2
                   ,{Whapp, [], 0}
                   ,ets:select(?MODULE, MatchSpec)
                   ),
    Zones.

-spec whapp_zone_count(text()) -> integer().
whapp_zone_count(Whapp) ->
    length(whapp_zones(Whapp)).

-type fold_zones_acc() :: {atom(), list(), non_neg_integer()}.

-spec determine_whapp_zones_fold({atom(), kapps_info()}, fold_zones_acc()) -> fold_zones_acc().
determine_whapp_zones_fold({Zone, Whapps}, {Whapp, Zones, C}=Acc) ->
    case props:is_defined(Whapp, Whapps)
        andalso not lists:member(Zone, Zones)
    of
        'true' -> {Whapp, [Zone | Zones], C+ 1};
        'false' -> Acc
    end.

-spec status() -> 'no_return'.
status() ->
    try
        Nodes = lists:sort(fun compare_nodes/2, ets:tab2list(?MODULE)),
        print_status(Nodes, gen_listener:call(?SERVER, 'zone'))
    catch
        error:badarg ->
            io:format("status unknown until node is fully initialized, try again in a moment\n"),
            'no_return'
    end.

-spec compare_nodes(kz_node(), kz_node()) -> boolean().
compare_nodes(#kz_node{node = N1}, #kz_node{node = N2}) -> N1 > N2.

-spec print_status(kz_nodes(), atom()) -> 'no_return'.
print_status(Nodes, Zone) ->
    F = fun (Node) -> print_node_status(Node, Zone) end,
    lists:foreach(F, Nodes),
    'no_return'.

-spec print_node_status(kz_node(), atom()) -> 'ok'.
print_node_status(#kz_node{zone=NodeZone
                          ,node=N
                          ,md5=MD5
                          ,version=Version
                          ,processes=Processes
                          ,ports=Ports
                          ,used_memory=UsedMemory
                          ,broker=Broker
                          ,kapps=Whapps
                          ,globals=Globals
                          ,node_info=NodeInfo
                          }=Node
                 ,Zone
                 ) ->
    MemoryUsage = kz_network_utils:pretty_print_bytes(UsedMemory),
    io:format(?SIMPLE_ROW_STR, [<<"Node">>, N]),
    _ = maybe_print_md5(MD5),
    io:format(?SIMPLE_ROW_STR, [<<"Version">>, Version]),
    io:format(?SIMPLE_ROW_STR, [<<"Memory Usage">>, MemoryUsage]),
    io:format(?SIMPLE_ROW_NUM, [<<"Processes">>, Processes]),
    io:format(?SIMPLE_ROW_NUM, [<<"Ports">>, Ports]),

    _ = maybe_print_zone(kz_term:to_binary(NodeZone)
                        ,kz_term:to_binary(Zone)
                        ),

    io:format(?SIMPLE_ROW_STR, [<<"Broker">>, Broker]),

    _ = maybe_print_globals(Globals),
    _ = maybe_print_node_info(NodeInfo),

    _ = maybe_print_kapps(Whapps),
    _ = maybe_print_media_servers(Node),

    io:format("~n").

-spec maybe_print_md5(api_binary()) -> 'ok'.
maybe_print_md5('undefined') -> 'ok';
maybe_print_md5(MD5) ->
    io:format(?SIMPLE_ROW_STR, [<<"md5">>, MD5]).

-spec maybe_print_zone(ne_binary(), ne_binary()) -> 'ok'.
maybe_print_zone(Zone, Zone) when Zone =/= <<"local">> ->
    io:format(?SIMPLE_ROW_STR, [<<"Zone">>, <<Zone/binary, " (local)">>]);
maybe_print_zone(NodeZone, _Zone) ->
    io:format(?SIMPLE_ROW_STR, [<<"Zone">>, NodeZone]).

-spec maybe_print_globals(kz_proplist()) -> 'ok'.
maybe_print_globals([]) -> 'ok';
maybe_print_globals(Globals) ->
    io:format(?HEADER_COL ++ ":", [<<"Globals">>]),
    lists:foreach(fun({K,V}) -> io:format(" ~s (~B)",[K, V]) end, Globals),
    io:format("~n").

-spec maybe_print_kapps(kz_proplist()) -> 'ok'.
maybe_print_kapps(Whapps) ->
    case lists:sort(fun compare_apps/2, Whapps) of
        []-> 'ok';
        SortedWhapps ->
            io:format(?HEADER_COL ": ", [<<"WhApps">>]),
            status_list(SortedWhapps, 0)
    end.

-spec compare_apps({binary(), any()}, {binary(), any()}) -> boolean().
compare_apps({K1,_}, {K2,_}) -> K1 < K2.

-spec maybe_print_media_servers(kz_node()) -> 'ok'.
maybe_print_media_servers(#kz_node{media_servers=MediaServers
                                  ,registrations=Registrations
                                  ,channels=Channels
                                  }) ->
    case lists:sort(MediaServers) of
        [] when Registrations =:= 0 -> 'ok';
        [] when Registrations > 0 ->
            io:format(?SIMPLE_ROW_NUM, [<<"Registrations">>, Registrations]);
        [Server|Servers] ->
            io:format(?SIMPLE_ROW_NUM, [<<"Channels">>, Channels]),
            io:format(?SIMPLE_ROW_NUM, [<<"Registrations">>, Registrations]),
            print_media_server(Server, ?MEDIA_SERVERS_HEADER),
            lists:foreach(fun print_media_server/1, Servers)
    end.

-spec print_media_server(media_server()) -> 'ok'.
print_media_server(Server) ->
    print_media_server(Server, ?MEDIA_SERVERS_LINE).

-spec print_media_server(media_server(), string()) -> 'ok'.
print_media_server({Name, JObj}, Format) ->
    io:format(lists:flatten([Format, ?MEDIA_SERVERS_DETAIL, "~n"])
             ,[Name
              ,kz_time:pretty_print_elapsed_s(
                 kz_time:elapsed_s(kz_json:get_integer_value(<<"Startup">>, JObj))
                )
              ]).

-spec maybe_print_node_info(api_object()) -> 'ok'.
maybe_print_node_info('undefined') -> 'ok';
maybe_print_node_info(NodeInfo) ->
    io:format(?HEADER_COL ++ ": ", [<<"Node Info">>]),
    [First | Rest] = kz_json:to_proplist(NodeInfo),
    print_node_info(First),
    lists:foreach(fun print_each_node_info/1, Rest).

-spec print_each_node_info({ne_binary(), ne_binary() | integer()}) -> 'ok'.
print_each_node_info(KV) ->
    io:format(?HEADER_COL "  ", [<<>>]),
    print_node_info(KV).

-spec print_node_info({ne_binary(), ne_binary() | integer()}) -> 'ok'.
print_node_info({K, ?NE_BINARY = V}) ->
    io:format("~s: ~s~n", [K, V]);
print_node_info({K, V}) when is_integer(V) ->
    io:format("~s: ~B~n", [K, V]).

-spec status_list(kapps_info(), 0..4) -> 'ok'.
status_list([], _) -> io:format("~n", []);
status_list(Whapps, Column) when Column > 3 ->
    io:format("~n" ++ ?HEADER_COL ++ "  ", [""]),
    status_list(Whapps, 0);
status_list([{Whapp, #whapp_info{startup='undefined'}}|Whapps], Column) ->
    io:format("~-25s", [Whapp]),
    status_list(Whapps, Column + 1);
status_list([{Whapp, #whapp_info{startup=Started,roles=[]}}|Whapps], Column) ->
    Elapsed = kz_time:elapsed_s(Started),
    Print = <<(kz_term:to_binary(Whapp))/binary, "(", (kz_time:pretty_print_elapsed_s(Elapsed))/binary, ")">>,
    io:format("~-25s", [Print]),
    status_list(Whapps, Column + 1);
status_list([{Whapp, #whapp_info{startup=Started,roles=Roles}}|Whapps], _Column) ->
    Elapsed = kz_time:elapsed_s(Started),
    Print = <<(kz_term:to_binary(Whapp))/binary, "(", (kz_time:pretty_print_elapsed_s(Elapsed))/binary, ")">>,
    io:format("~-25s", [Print]),
    io:format("~s", [kz_binary:join(Roles, <<" , ">>)]),
    status_list(Whapps, 4).

-spec flush() -> 'ok'.
flush() ->
    gen_listener:cast(?SERVER, 'flush').

-spec notify_new() -> 'ok'.
notify_new() ->
    notify_new(self()).

-spec notify_new(pid()) -> 'ok'.
notify_new(Pid) ->
    gen_listener:cast(?SERVER, {'notify_new', Pid}).

-spec notify_expire() -> 'ok'.
notify_expire() ->
    notify_expire(self()).

-spec notify_expire(pid()) -> 'ok'.
notify_expire(Pid) ->
    gen_listener:cast(?SERVER, {'notify_expire', Pid}).

-spec handle_advertise(kz_json:object(), kz_proplist()) -> 'ok'.
handle_advertise(JObj, Props) ->
    'true' = kapi_nodes:advertise_v(JObj),

    Srv = props:get_value('server', Props),
    Node = props:get_value('node', Props),
    case kz_json:get_value(<<"Node">>, JObj, Node) =:= Node of
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
-spec init([]) -> {'ok', nodes_state()}.
init([]) ->
    lager:debug("starting nodes watcher"),
    kapi_nodes:declare_exchanges(),
    kapi_self:declare_exchanges(),
    Tab = ets:new(?MODULE, ['set'
                           ,'protected'
                           ,'named_table'
                           ,{'keypos', #kz_node.node}
                           ]),
    lager:debug("started ETS ~p", [Tab]),
    _ = erlang:send_after(?EXPIRE_PERIOD, self(), 'expire_nodes'),
    _ = net_kernel:monitor_nodes('true', ['nodedown_reason'
                                         ,{'node_type', 'all'}
                                         ]),
    lager:debug("monitoring nodes"),
    State = #state{tab=Tab, zone=get_zone()},
    Version = <<(kz_util:kazoo_version())/binary
                ," - "
                ,(kz_term:to_binary(erlang:system_info('otp_release')))/binary
              >>,

    self() ! {'heartbeat', State#state.heartbeat_ref},
    {'ok', State#state{version=Version, md5=node_encoded()}}.

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
-spec handle_call(any(), pid_ref(), nodes_state()) -> handle_call_ret_state(nodes_state()).
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
-spec handle_cast(any(), nodes_state()) -> handle_cast_ret_state(nodes_state()).
handle_cast({'notify_new', Pid}, #state{notify_new=Set}=State) ->
    _ = erlang:monitor('process', Pid),
    {'noreply', State#state{notify_new=sets:add_element(Pid, Set)}};
handle_cast({'notify_expire', Pid}, #state{notify_expire=Set}=State) ->
    _ = erlang:monitor('process', Pid),
    {'noreply', State#state{notify_expire=sets:add_element(Pid, Set)}};
handle_cast({'advertise', JObj}, #state{tab=Tab}=State) ->
    #kz_node{}=Node = from_json(JObj, State),
    _ = case ets:insert_new(Tab, Node) of
            'true' -> kz_util:spawn(fun notify_new/2, [Node, State]);
            'false' -> ets:insert(Tab, Node)
        end,
    {'noreply', maybe_add_zone(Node, State)};
handle_cast({'gen_listener', {'created_queue', _Q}}, State) ->
    lager:info("nodes acquired queue name ~s, starting remote heartbeats", [_Q]),
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', IsConsuming}}, State) ->
    lager:debug("heartbeat from remotes is ~p", [IsConsuming]),
    {'noreply', State};
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
-spec handle_info(any(), nodes_state()) -> handle_info_ret_state(nodes_state()).
handle_info('expire_nodes', #state{node=ThisNode, tab=Tab}=State) ->
    Now = kz_time:now_ms(),
    FindSpec = [{#kz_node{node='$1'
                         ,expires='$2'
                         ,last_heartbeat='$3'
                         ,_ = '_'
                         }
                ,[{'andalso'
                  ,{'=/=','$2','undefined'}
                  ,{'andalso'
                   ,{'>',{'const',Now},{'+','$2','$3'}}
                   ,{'=/=','$1',{'const',ThisNode}}
                   }
                  }
                 ]
                ,['$_']
                }
               ],
    Nodes = ets:select(Tab, FindSpec),
    _ = [ets:delete(Tab, Node) || #kz_node{node=Node} <- Nodes],
    _ = kz_util:spawn(fun notify_expire/2, [Nodes, State]),
    _ = erlang:send_after(?EXPIRE_PERIOD, self(), 'expire_nodes'),
    {'noreply', State};
handle_info({'heartbeat', Ref}
           ,#state{heartbeat_ref=Ref
                  ,tab=Tab
                  }=State
           ) ->
    Heartbeat = ?HEARTBEAT,
    Reference = erlang:make_ref(),
    try create_node(Heartbeat, State) of
        Node ->
            _ = ets:insert(Tab, Node),
            kz_amqp_worker:cast(advertise_payload(Node), fun kapi_nodes:publish_advertise/1)
    catch
        _E:_N ->
            lager:error("error creating node ~p : ~p", [_E, _N]),
            kz_util:log_stacktrace()
    end,
    _ = erlang:send_after(Heartbeat, self(), {'heartbeat', Reference}),
    {'noreply', State#state{heartbeat_ref=Reference}};
handle_info({'DOWN', Ref, 'process', Pid, _}
           ,#state{notify_new=NewSet
                  ,notify_expire=ExpireSet
                  }=State
           ) ->
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
-spec handle_event(kz_json:object(), nodes_state()) -> gen_listener:handle_event_return().
handle_event(_JObj, #state{node=Node}) ->
    {'reply', [{'node', kz_term:to_binary(Node)}]}.

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
-spec terminate(any(), nodes_state()) -> 'ok'.
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
-spec code_change(any(), nodes_state(), any()) -> {'ok', nodes_state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec create_node('undefined' | 5000..15000, nodes_state()) -> kz_node().
create_node(Heartbeat, #state{zone=Zone
                             ,version=Version
                             ,md5=MD5
                             }) ->
    add_kapps_data(#kz_node{expires=Heartbeat
                           ,broker=normalize_amqp_uri(kz_amqp_connections:primary_broker())
                           ,used_memory=erlang:memory('total')
                           ,processes=erlang:system_info('process_count')
                           ,ports=length(erlang:ports())
                           ,version=Version
                           ,zone=Zone
                           ,md5=MD5
                           ,globals=kz_globals:stats()
                           ,node_info=node_info()
                           }).

-spec normalize_amqp_uri(ne_binary()) -> ne_binary().
normalize_amqp_uri(URI) ->
    kz_term:to_binary(amqp_uri:remove_credentials(kz_term:to_list(URI))).

-spec add_kapps_data(kz_node()) -> kz_node().
add_kapps_data(Node) ->
    lists:foldl(fun kapp_data/2, Node, kapps_controller:list_apps()).

-spec request(request_acc()) -> request_acc().
request(Acc) ->
    App = props:get_value('app', Acc),
    [{'info', get_whapp_info(App)} | Acc].

-spec kapp_data(atom(), kz_node()) -> kz_node().
kapp_data(App, Node) ->
    kapp_data(App, Node, kz_nodes_bindings:request(App)).
kapp_data(App
         ,#kz_node{kapps=Kapps
                  ,media_servers=Servers
                  ,channels=Channels
                  ,registrations=Registrations
                  }=Node
         ,RequestAcc
         ) ->
    Node#kz_node{kapps=maybe_add_info(App, props:get_value('info', RequestAcc), Kapps)
                ,media_servers=props:get_value('media_servers', RequestAcc, []) ++ Servers
                ,channels=props:get_integer_value('channels', RequestAcc, 0) + Channels
                ,registrations=props:get_integer_value('registrations', RequestAcc, 0) + Registrations
                }.

-spec maybe_add_info(atom(), 'undefined' | whapp_info(), kapps_info()) -> kapps_info().
maybe_add_info(_App, 'undefined', Kapps) -> Kapps;
maybe_add_info(App, AppInfo, Kapps) ->
    [{kz_term:to_binary(App), AppInfo} | Kapps].

-spec get_whapp_info(atom() | pid() | kz_proplist() | 'undefined') -> whapp_info().
get_whapp_info('undefined') -> #whapp_info{};
get_whapp_info(Whapp) when is_atom(Whapp) ->
    try
        get_whapp_info(application_controller:get_master(Whapp))
    catch
        _E:_R -> #whapp_info{}
    end;
get_whapp_info(Master) when is_pid(Master) ->
    try
        get_whapp_info(application_master:get_child(Master))
    catch
        _E:_R -> #whapp_info{}
    end;
get_whapp_info({Pid, _Module}) when is_pid(Pid) ->
    try
        get_whapp_process_info(erlang:process_info(Pid))
    catch
        _E:_R -> #whapp_info{}
    end;
get_whapp_info(_Arg) ->
    lager:debug("can't get info for ~p", [_Arg]),
    #whapp_info{}.

-spec get_whapp_process_info(kz_proplist() | 'undefined') -> whapp_info().
get_whapp_process_info('undefined') -> #whapp_info{};
get_whapp_process_info([]) -> #whapp_info{};
get_whapp_process_info(PInfo) ->
    Startup = props:get_value('$startup', props:get_value('dictionary', PInfo, [])),
    #whapp_info{startup=Startup}.

-spec advertise_payload(kz_node()) -> kz_proplist().
advertise_payload(#kz_node{expires=Expires
                          ,kapps=Whapps
                          ,media_servers=MediaServers
                          ,used_memory=UsedMemory
                          ,processes=Processes
                          ,ports=Ports
                          ,version=Version
                          ,channels=Channels
                          ,registrations=Registrations
                          ,zone=Zone
                          ,globals=Globals
                          ,md5=MD5
                          }) ->
    props:filter_undefined(
      [{<<"md5">>, MD5}
      ,{<<"Expires">>, Expires}
      ,{<<"WhApps">>, kapps_to_json(Whapps) }
      ,{<<"Media-Servers">>, media_servers_to_json(MediaServers)}
      ,{<<"Used-Memory">>, UsedMemory}
      ,{<<"Processes">>, Processes}
      ,{<<"Ports">>, Ports}
      ,{<<"Version">>, Version}
      ,{<<"Channels">>, Channels}
      ,{<<"Registrations">>, Registrations}
      ,{<<"Zone">>, kz_term:to_binary(Zone)}
      ,{<<"Globals">>, kz_json:from_list(Globals)}
      ,{<<"Node-Info">>, node_info()}
       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).

-spec media_servers_to_json(media_servers()) -> kz_json:object().
media_servers_to_json([]) -> 'undefined';
media_servers_to_json(Servers) ->
    kz_json:from_list(Servers).

-spec media_servers_from_json(kz_json:object()) -> media_servers().
media_servers_from_json(Servers) ->
    [{Key, kz_json:get_value(Key, Servers)}
     || Key <- kz_json:get_keys(Servers)
    ].

-spec from_json(kz_json:object(), nodes_state()) -> kz_node().
from_json(JObj, State) ->
    Node = kz_json:get_value(<<"Node">>, JObj),
    #kz_node{node=kz_term:to_atom(Node, 'true')
            ,md5=kz_json:get_value(<<"md5">>, JObj)
            ,expires=kz_term:to_integer(kz_json:get_integer_value(<<"Expires">>, JObj, 0) * ?FUDGE_FACTOR)
            ,kapps=kapps_from_json(kz_json:get_value(<<"WhApps">>, JObj, []))
            ,media_servers=media_servers_from_json(kz_json:get_value(<<"Media-Servers">>, JObj, kz_json:new()))
            ,used_memory=kz_json:get_integer_value(<<"Used-Memory">>, JObj, 0)
            ,processes=kz_json:get_integer_value(<<"Processes">>, JObj, 0)
            ,ports=kz_json:get_integer_value(<<"Ports">>, JObj, 0)
            ,version=kz_json:get_first_defined([<<"Version">>, <<"App-Version">>], JObj, <<"unknown">>)
            ,channels=kz_json:get_integer_value(<<"Channels">>, JObj, 0)
            ,registrations=kz_json:get_integer_value(<<"Registrations">>, JObj, 0)
            ,broker=get_amqp_broker(JObj)
            ,zone=get_zone(JObj, State)
            ,globals=kz_json:to_proplist(kz_json:get_value(<<"Globals">>, JObj, kz_json:new()))
            ,node_info=kz_json:get_json_value(<<"Node-Info">>, JObj)
            }.

-spec kapps_from_json(api_terms()) -> kapps_info().
-spec whapp_from_json(binary(), kz_json:object()) -> {binary(), whapp_info()}.
-spec whapp_info_from_json(kz_json:object()) -> whapp_info().
-spec whapp_info_from_json(kz_json:object(), {kz_json:json_terms(), kz_json:keys()}) -> whapp_info().

kapps_from_json(Whapps) when is_list(Whapps) ->
    [{Whapp, #whapp_info{}} || Whapp <- Whapps];
kapps_from_json(JObj) ->
    Keys = kz_json:get_keys(JObj),
    [whapp_from_json(Key, JObj) || Key <- Keys].

whapp_from_json(Key, JObj) ->
    {Key, whapp_info_from_json(kz_json:get_value(Key, JObj))}.

whapp_info_from_json(JObj) ->
    whapp_info_from_json(#whapp_info{}, kz_json:get_values(JObj)).

whapp_info_from_json(Info, {[], []}) -> Info;
whapp_info_from_json(Info, {[V | V1], [<<"Roles">> | K1]}) ->
    whapp_info_from_json(Info#whapp_info{roles=V}, {V1, K1});
whapp_info_from_json(Info, {[V | V1], [<<"Startup">> | K1]}) ->
    whapp_info_from_json(Info#whapp_info{startup=V}, {V1, K1}).

-spec kapps_to_json(kapps_info()) -> kz_json:object().
-spec whapp_to_json({ne_binary(), whapp_info()}) -> {ne_binary(), kz_json:object()}.
-spec whapp_info_to_json(whapp_info()) -> kz_json:object().

kapps_to_json(Whapps) ->
    List = [whapp_to_json(Whapp) || Whapp <- Whapps],
    kz_json:from_list(List).

whapp_to_json({K, Info}) ->
    {K, whapp_info_to_json(Info)}.

whapp_info_to_json(#whapp_info{startup=Start, roles=Roles}) ->
    kz_json:from_list(
      props:filter_undefined(
        [{<<"Startup">>, Start}
        ,{<<"Roles">>, Roles}
        ])).

-spec get_zone() -> atom().
get_zone() ->
    case kz_config:get(kz_config:get_node_section_name(), 'zone') of
        [Zone] -> kz_term:to_atom(Zone, 'true');
        _Else -> 'local'
    end.

-spec get_zone(kz_json:object(), nodes_state()) -> atom().
get_zone(JObj, #state{zones=Zones, zone=LocalZone}) ->
    case kz_json:get_first_defined([<<"Zone">>, <<"AMQP-Broker-Zone">>], JObj) of
        'undefined' ->
            case kz_json:get_value(<<"AMQP-Broker">>, JObj) of
                'undefined' -> LocalZone;
                Broker ->
                    case props:get_value(Broker, Zones) of
                        'undefined' -> LocalZone;
                        Zone -> kz_term:to_atom(Zone, 'true')
                    end
            end;
        Zone -> kz_term:to_atom(Zone, 'true')
    end.

-spec local_zone() -> atom().
local_zone() -> kz_config:zone().

-spec get_amqp_broker(api_binary() | kz_json:object()) -> api_binary().
get_amqp_broker('undefined') ->
    normalize_amqp_uri(kz_amqp_connections:primary_broker());
get_amqp_broker(Broker) when is_binary(Broker) -> normalize_amqp_uri(Broker);
get_amqp_broker(JObj) ->
    get_amqp_broker(kz_json:get_ne_value(<<"AMQP-Broker">>, JObj)).

-spec notify_expire(kz_nodes(), nodes_state() | pids()) -> 'ok'.
notify_expire([], _) -> 'ok';
notify_expire(_, []) -> 'ok';
notify_expire(Nodes, #state{notify_expire=Set}) ->
    notify_expire(Nodes, sets:to_list(Set));
notify_expire([#kz_node{node=NodeName}=Node|Nodes], Pids) ->
    lager:warning("node ~s heartbeat has expired", [NodeName]),
    _ = [gen_listener:cast(Pid, {?MODULE, {'expire', Node}})
         || Pid <- Pids
        ],
    notify_expire(Nodes, Pids).

-spec notify_new(kz_node(), nodes_state() | pids()) -> 'ok'.
notify_new(Node, #state{notify_new=Set}) ->
    notify_new(Node, sets:to_list(Set));
notify_new(#kz_node{node=NodeName}=Node, Pids) ->
    lager:info("received heartbeat from new node ~s", [NodeName]),
    _ = [gen_listener:cast(Pid, {?MODULE, {'new', Node}})
         || Pid <- Pids
        ],
    'ok'.

-spec whapp_oldest_node(text()) -> api_integer().
whapp_oldest_node(Whapp) ->
    whapp_oldest_node(Whapp, 'false').

-spec whapp_oldest_node(text(), text() | boolean() | atom()) -> api_integer().
whapp_oldest_node(Whapp, Federated)
  when is_binary(Federated) ->
    whapp_oldest_node(Whapp, kz_term:is_true(Federated));
whapp_oldest_node(Whapp, 'true') ->
    MatchSpec = [{#kz_node{kapps='$1'
                          ,node='$2'
                          ,_ = '_'
                          }
                 ,[{'=/=', '$1', []}]
                 ,[{{'$1','$2'}}]
                 }],
    determine_whapp_oldest_node(kz_term:to_binary(Whapp), MatchSpec);
whapp_oldest_node(Whapp, 'false') ->
    Zone = gen_listener:call(?SERVER, 'zone'),
    whapp_oldest_node(Whapp, Zone);
whapp_oldest_node(Whapp, Zone)
  when is_atom(Zone) ->
    MatchSpec = [{#kz_node{kapps='$1'
                          ,node='$2'
                          ,zone = Zone
                          ,_ = '_'
                          }
                 ,[{'=/=', '$1', []}]
                 ,[{{'$1', '$2'}}]
                 }],
    determine_whapp_oldest_node(kz_term:to_binary(Whapp), MatchSpec).

-spec determine_whapp_oldest_node(ne_binary(), ets:match_spec()) ->
                                         'undefined' | node().
determine_whapp_oldest_node(Whapp, MatchSpec) ->
    case oldest_whapp_node(Whapp, MatchSpec) of
        {Node, _Start} -> Node;
        'undefined' -> 'undefined'
    end.

-type oldest_whapp_node() :: 'undefined' |
                             {node(), gregorian_seconds()}.

-spec oldest_whapp_node(ne_binary(), ets:match_spec()) ->
                               oldest_whapp_node().
oldest_whapp_node(Whapp, MatchSpec) ->
    lists:foldl(fun({Whapps, _Node}=Info, Acc) when is_list(Whapps) ->
                        determine_whapp_oldest_node_fold(Info, Acc, Whapp)
                end
               ,'undefined'
               ,ets:select(?MODULE, MatchSpec)
               ).

-spec determine_whapp_oldest_node_fold({kapps_info(), node()}, oldest_whapp_node(), ne_binary()) ->
                                              oldest_whapp_node().
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

-spec maybe_add_zone(kz_node(), nodes_state()) -> nodes_state().
maybe_add_zone(#kz_node{zone='undefined'}, #state{}=State) -> State;
maybe_add_zone(#kz_node{zone=Zone, broker=B}, #state{zones=Zones}=State) ->
    Broker = normalize_amqp_uri(B),
    case props:get_value(Broker, Zones) of
        'undefined' -> State#state{zones=[{Broker, Zone} | Zones]};
        _ -> State
    end.

-spec node_info() -> kz_json:object().
node_info() ->
    kz_json:from_list(pool_states()).

-spec pool_states() -> kz_proplist().
pool_states() ->
    lists:keysort(1, [pool_state(Pool) || {Pool, _Pid} <- kz_amqp_sup:pools()]).

-spec pool_state(atom()) -> {ne_binary(), ne_binary()}.
-spec pool_state(atom(), atom(), integer(), integer(), integer()) -> {ne_binary(), ne_binary()}.
pool_state(Name) ->
    {PoolState, Workers, OverFlow, Monitors} = poolboy:status(Name),
    pool_state(Name, PoolState, Workers, OverFlow, Monitors).

pool_state(Name, State, Workers, Overflow, Monitors) ->
    {kz_term:to_binary(Name)
    ,iolist_to_binary(
       io_lib:format("~p/~p/~p (~p)", [Workers, Monitors, Overflow, State])
      )
    }.

-spec node_encoded() -> ne_binary().
node_encoded() ->
    case application:get_env(?APP_NAME_ATOM, 'node_encoded') of
        'undefined' ->
            Encoded = kz_base64url:encode(crypto:hash(md5, kz_term:to_binary(node()))),
            application:set_env(?APP_NAME_ATOM, 'node_encoded', Encoded),
            Encoded;
        {'ok', Encoded} -> Encoded
    end.
