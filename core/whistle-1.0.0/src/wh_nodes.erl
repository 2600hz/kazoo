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

-define(EXPIRE_PERIOD, 1000).
-define(FUDGE_FACTOR, 1.25).
-define(APP_NAME, <<"wh_nodes">>).
-define(APP_VERSION, <<"0.1.0">>).

-record(state, {heartbeat_ref :: reference()
                ,tab :: ets:tid()
                ,notify_new = sets:new() :: set()
                ,notify_expire = sets:new() :: set()
                ,node :: ne_binary()
                ,zone = 'local' :: atom()
                ,version :: ne_binary()
               }).
-type nodes_state() :: #state{}.

-record(whapp_info, {startup :: gregorian_seconds()}).

-type whapp_info() :: #whapp_info{}.
-type whapps_info() :: [{binary(), whapp_info()},...] | [].

-record(node, {node = node() :: atom() | '$1' | '$2' | '_'
               ,expires = 0 :: non_neg_integer() | 'undefined' | '$2' | '_'
               ,whapps = [] :: whapps_info() | '$1' | '_'
               ,media_servers = [] :: wh_proplist() | atoms() | '_'
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

-spec determine_whapp_count_fold(whapps_info(), non_neg_integer(), ne_binary()) -> non_neg_integer().
determine_whapp_count_fold(Whapps, Acc, Whapp) ->
    case props:is_defined(Whapp, Whapps) of
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
             _ = case lists:sort(fun({K1,_}, {K2,_}) -> K1 < K2 end
                                 ,Node#node.whapps
                                )
                 of
                     []-> 'ok';
                     Whapps ->
                         io:format("WhApps        : ", []),
                         status_list(Whapps, 0)
                 end,
             _ = case lists:sort(Node#node.media_servers) of
                     [] when Node#node.registrations =:= 0 -> 'ok';
                     [] when Node#node.registrations > 0 ->
                         io:format("Registrations : ~B~n", [Node#node.registrations]);
                     [{Server, Started}|Servers] ->
                         io:format("Channels      : ~B~n", [Node#node.channels]),
                         io:format("Registrations : ~B~n", [Node#node.registrations]),
                         io:format("Media Servers : ~s(~s)~n", [Server, Started]),
                         [begin
                              io:format("                ~s~n", [S])
                          end
                          || S <- Servers
                         ];
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
    net_kernel:monitor_nodes('true', ['nodedown_reason'
                                      ,{'node_type', 'all'}
                                     ]),
    lager:debug("monitoring nodes"),
    State = #state{tab=Tab},
    Node = create_node('undefined', State),
    lager:debug("created node: ~p", [Node]),
    ets:insert(Tab, Node),
    lager:debug("inserted node"),
    Version = <<(wh_util:whistle_version())/binary
                ," - "
                ,(wh_util:to_binary(erlang:system_info('otp_release')))/binary
              >>,
    {'ok', State#state{node=wh_util:to_binary(Node#node.node)
                       ,version=Version
                      }}.

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
    _ = spawn(fun() -> notify_expire(Nodes, State) end),
    _ = erlang:send_after(?EXPIRE_PERIOD, self(), 'expire_nodes'),
    {'noreply', State};
handle_info({'heartbeat', Ref}, #state{heartbeat_ref=Ref
                                       ,tab=Tab
                                      }=State) ->
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
                                                     ,notify_expire=ExpireSet
                                                    }=State) ->
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
create_node(Heartbeat, #state{zone=Zone
                              ,version=Version
                             }) ->
    maybe_add_whapps_data(#node{expires=Heartbeat
                                ,broker=wh_amqp_connections:primary_broker()
                                ,used_memory=erlang:memory('total')
                                ,processes=erlang:system_info('process_count')
                                ,ports=length(erlang:ports())
                                ,version=Version
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
    Servers = [{wh_util:to_binary(Server), wh_util:pretty_print_elapsed_s(wh_util:elapsed_s(Started))}
               || {Server, Started} <- ecallmgr_fs_nodes:connected('true')
              ],
    Node#node{media_servers=Servers
              ,whapps=[{<<"ecallmgr">>, get_whapp_info('ecallmgr')} |Whapps]
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
get_whapp_info(_) ->
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
advertise_payload(Node) ->
    [{<<"Expires">>, wh_util:to_binary(Node#node.expires)}
     ,{<<"WhApps">>, whapps_to_json(Node#node.whapps) }
     ,{<<"Media-Servers">>, media_servers_to_json(Node#node.media_servers)}
     ,{<<"Used-Memory">>, Node#node.used_memory}
     ,{<<"Processes">>, Node#node.processes}
     ,{<<"Ports">>, Node#node.ports}
     ,{<<"Version">>, Node#node.version}
     ,{<<"Channels">>, Node#node.channels}
     ,{<<"Registrations">>, Node#node.registrations}
     ,{<<"Zone">>, Node#node.zone}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec media_servers_to_json(atoms() | wh_proplist()) -> ne_binaries().
media_servers_to_json(Servers) ->
    [media_server_to_json(Server) || Server <- Servers].

-spec media_server_to_json(atom() | {atom(), ne_binary()}) -> ne_binary().
media_server_to_json({Server, Started}) ->
    list_to_binary([wh_util:to_binary(Server), "(", Started, ")"]);
media_server_to_json(Server) -> wh_util:to_binary(Server).

-spec from_json(wh_json:object(), nodes_state()) -> wh_node().
from_json(JObj, State) ->
    Node = wh_json:get_value(<<"Node">>, JObj),
    #node{node=wh_util:to_atom(Node, 'true')
          ,expires=wh_util:to_integer(wh_json:get_integer_value(<<"Expires">>, JObj, 0) * ?FUDGE_FACTOR)
          ,whapps=whapps_from_json(wh_json:get_value(<<"WhApps">>, JObj, []))
          ,media_servers=wh_json:get_value(<<"Media-Servers">>, JObj, [])
          ,used_memory=wh_json:get_integer_value(<<"Used-Memory">>, JObj, 0)
          ,processes=wh_json:get_integer_value(<<"Processes">>, JObj, 0)
          ,ports=wh_json:get_integer_value(<<"Ports">>, JObj, 0)
          ,version=wh_json:get_first_defined([<<"Version">>, <<"App-Version">>], JObj, <<"unknown">>)
          ,channels=wh_json:get_integer_value(<<"Channels">>, JObj, 0)
          ,registrations=wh_json:get_integer_value(<<"Registrations">>, JObj, 0)
          ,zone=get_zone(JObj, State)
          ,broker=get_amqp_broker(JObj)
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

-spec whapp_oldest_node(text()) -> api_integer().
whapp_oldest_node(Whapp) ->
    whapp_oldest_node(Whapp, 'false').

-spec whapp_oldest_node(text(), text() | boolean()) -> api_integer().
whapp_oldest_node(Whapp, 'false') ->
    MatchSpec = [{#node{whapps='$1'
                        ,node='$2'
                        ,zone = <<"local">>
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
