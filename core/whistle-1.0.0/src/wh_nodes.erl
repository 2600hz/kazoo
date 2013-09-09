%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_nodes).

-behaviour(gen_listener).

-export([start_link/0]).
-export([whapp_count/1]).
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

-define(BINDINGS, [{'nodes', []}
                   ,{'self', []}
                  ]).
-define(RESPONDERS, [{{?MODULE, 'handle_advertise'}, [{<<"nodes">>, <<"advertise">>}]}]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-define(EXPIRE_PERIOD, 1000).
-define(FUDGE_FACTOR, 1.25).
-define(APP_NAME, <<"wh_nodes">>).
-define(APP_VERSION, <<"0.1.0">>).

-record(state, {heartbeat_ref :: 'undefined' | reference()
                ,tab
                ,notify_new = sets:new() :: set()
                ,notify_expire = sets:new() :: set()
               }).

-record(node, {node = node()
               ,expires = 0
               ,whapps = []
               ,media_servers = []
               ,last_heartbeat = wh_util:now_ms(now())
              }).

-type wh_node() :: #node{}.
-type nodes_state() :: #state{}.

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
whapp_count(Whapp) when is_binary(Whapp) ->
    FindSpec = [{#node{whapps='$1', _ = '_'}
                 ,[{'=/=', '$1', []}]
                 ,['$1']}
               ],
    lists:foldl(fun(Whapps, Acc) ->
                        case lists:member(Whapp, Whapps) of
                            'true' -> Acc + 1;
                            'false' -> Acc
                        end
                end, 0, ets:select(?MODULE, FindSpec));
whapp_count(Whapp) ->
    whapp_count(wh_util:to_binary(Whapp)).

-spec status() -> 'no_return'.
status() ->
    _ = [begin
             io:format("Node: ~s~n", [Node#node.node]),
             _ = case Node#node.whapps of
                     []-> 'ok';
                     Whapps ->
                         io:format("WhApps:~n", []),
                         [begin
                              io:format("    ~s~n", [Whapp])
                          end
                          || Whapp <- Whapps
                         ]
                 end,
             _ = case Node#node.media_servers of
                     []-> 'ok';
                     Servers ->
                         io:format("Media Servers:~n", []),
                         [begin
                              io:format("    ~s~n", [Server])
                          end
                          || Server <- Servers
                         ]
                 end,
             io:format("~n", [])
         end
         || Node <- ets:tab2list(?MODULE)
        ],
    'no_return'.

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
    gen_server:cast(Srv, {'advertise', JObj}).

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
    Tab = ets:new(?MODULE, ['set', 'protected', 'named_table', {'keypos', #node.node}]),
    _ = erlang:send_after(?EXPIRE_PERIOD, self(), 'expire_nodes'),
    {'ok', #state{tab=Tab}}.

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
    Node = from_json(JObj),
    _ = case ets:insert_new(Tab, Node) of
            'true' -> spawn(fun() -> notify_new(Node#node.node, State) end);
            'false' -> ets:insert(Tab, Node)
        end,
    {'noreply', State};
handle_cast({'wh_amqp_channel', {'new_channel', _}}, State) ->
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
    FindSpec = [{#node{node='$1', expires = '$2', last_heartbeat = '$3'
                       ,_ = '_'}
                 ,[{'>', {const, Now}, {'+', '$2', '$3'}}]
                 ,['$1']}
               ],
    Nodes = ets:select(Tab, FindSpec),
    _ = [ets:delete(Tab, Node) || Node <- Nodes],
    _ = spawn(fun() -> notify_expire(Nodes, State) end),
    _ = erlang:send_after(?EXPIRE_PERIOD, self(), 'expire_nodes'),
    {'noreply', State};
handle_info({'heartbeat', Ref}, #state{heartbeat_ref=Ref}=State) ->
    Heartbeat = crypto:rand_uniform(5000, 15000),
    try create_node(Heartbeat) of
        Node -> wapi_nodes:publish_advertise(advertise_payload(Node))
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
                          ,notify_expire=sets:del_element(Pid, ExpireSet)}};
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
handle_event(_JObj, _State) ->
    {'reply', []}.

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
-spec create_node(5000..15000) -> wh_node().
create_node(Heartbeat) ->
    maybe_add_whapps_data(#node{expires=Heartbeat}).

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
              || Whapp <- whapps_controller:running_apps()
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
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec from_json(wh_json:object()) -> wh_node().
from_json(JObj) ->
    Node = wh_json:get_value(<<"Node">>, JObj),
    #node{node=wh_util:to_atom(Node, 'true')
          ,expires=wh_json:get_integer_value(<<"Expires">>, JObj, 0) * ?FUDGE_FACTOR
          ,whapps=wh_json:get_value(<<"WhApps">>, JObj, [])
          ,media_servers=wh_json:get_value(<<"Media-Servers">>, JObj, [])
         }.

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
