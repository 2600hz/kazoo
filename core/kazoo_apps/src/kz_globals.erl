%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(kz_globals).

-behaviour(gen_listener).

-export([start_link/0]).
-export([delete_by_pid/1]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-export([whereis_name/1
        ,register_name/2
        ,unregister_name/1
        ,send/2
        ]).

-export([handle_amqp_register/2
        ,handle_amqp_unregister/2
        ,handle_amqp_send/2
        ,handle_amqp_call/2
        ,handle_amqp_query/2
        ]).

-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_api.hrl").

-define(SERVER, ?MODULE).

-define(BINDINGS, [{'globals', ['federate']}
                   ,{'self', []}
                  ]).
-define(RESPONDERS, [{{?MODULE, 'handle_amqp_call'}
                      ,[{<<"globals">>, <<"call">>}]
                     }
                    ,{{?MODULE, 'handle_amqp_send'}
                      ,[{<<"globals">>, <<"send">>}]
                     }
                    ,{{?MODULE, 'handle_amqp_register'}
                      ,[{<<"globals">>, <<"register">>}]
                     }
                    ,{{?MODULE, 'handle_amqp_unregister'}
                      ,[{<<"globals">>, <<"unregister">>}]
                     }
                    ,{{?MODULE, 'handle_amqp_query'}
                      ,[{<<"globals">>, <<"query">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, [{'no_local', 'true'}]).

-define(HEARTBEAT, crypto:rand_uniform(5 * ?MILLISECONDS_IN_SECOND, 15 * ?MILLISECONDS_IN_SECOND)).
-define(EXPIRE_PERIOD, 1 * ?MILLISECONDS_IN_SECOND).
-define(FUDGE_FACTOR, 1.25).
-define(APP_NAME, <<"kz_globals">>).
-define(APP_VERSION, <<"4.0.0">>).

-define(AMQP_REGISTER_FUN, fun kapi_globals:publish_register/1).
-define(AMQP_UNREGISTER_FUN, fun kapi_globals:publish_unregister/1).
-define(AMQP_QUERY_FUN, fun kapi_globals:publish_query/1).
-define(AMQP_CALL_SCOPE, {'kz_globals', 'true', 'true'}).

-define(TAB_NAME, kazoo_global_names).

-record(state, {zone = 'local' :: atom()
                ,zones = [] :: kz_proplist()
                ,queue :: api_binary()
                ,node = node() :: node()
               }).
-type globals_state() :: #state{}.

-record(kz_global, {node = node() :: atom() | '_'
                  ,zone :: atom() | '_'
                  ,pid :: api_pid() | '_'
                  ,server :: any() | '_'
                  ,name :: term() | '_'
                  ,monitor :: api_reference() | '_'
                  ,state = 'none' :: kapi_globals:state() | '_'
                 }).

-type kz_global() :: #kz_global{}.
-type kz_globals() :: [kz_global()].

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
    lager:debug("starting globals registry"),
    kapi_globals:declare_exchanges(),
    kapi_self:declare_exchanges(),
    _ = ets:new(?TAB_NAME, ['set'
                            ,'protected'
                            ,'named_table'
                            ,{'keypos', #kz_global.name}
                           , {read_concurrency, true}
                           ]),
    Zone = kz_config:zone(),
    {'ok', #state{zone=Zone}}.

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
handle_call({'delete_remote', Pid}, _From, State) ->
    _ = delete_by_pid(Pid),
    {'reply', 'ok', State};
handle_call({'insert_remote', JObj}, _From, State) ->
    Global = from_json(JObj, State),
    {'reply', register_remote(Global), State};
handle_call({'register', Name, Pid}, _From, State) ->
    {'reply', amqp_register(Name, Pid, State), State};
handle_call({'unregister', Name}, _From, State) ->
    {'reply', amqp_unregister(Name), State};
handle_call({'insert', Global}, _From, State) ->
    ets:insert(?TAB_NAME, Global),
    {'reply', State};
handle_call({'delete', Global}, _From, State) ->
    ets:delete(?TAB_NAME, Global),
    {'reply', State};
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
handle_cast({'amqp_delete', #kz_global{pid=Pid}, 'undefined'}, State) ->
    Pid ! '$proxy_stop',
    {'noreply', State};
handle_cast({'amqp_delete', #kz_global{pid=Pid}, Reason}, State) ->
    Pid ! {'$proxy_stop', Reason},
    {'noreply', State};
handle_cast({'insert_remote', Global}, State) ->
    _ = register_remote(Global),
    {'noreply', State};
handle_cast({'add_zone', Zone}, #state{zones=Zones}=State) ->
    case lists:member(Zone, Zones) of
        'false' -> {'noreply', State#state{zones=[Zone | Zones]}};
        'true' -> {'noreply', State}
    end;
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'created_queue', Q}}, State) ->
    lager:info("globals acquired queue name ~s", [Q]),
    {'noreply', State#state{queue=Q}};
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
handle_info({'DOWN', Ref, 'process', Pid, Reason}, State) ->
    erlang:demonitor(Ref, ['flush']),
    delete_by_pid(Pid, Reason),
    {'noreply', State};
handle_info({'EXIT', Pid, Reason}, State) ->
    lager:debug("proxy process ~p exited with reason ~p", [Pid, Reason]),
    {'noreply', State};
handle_info({'nodedown', Node, InfoList}, State) ->
    lager:info("VM ~s is no longer connected:", [Node]),
    _ = [lager:info(" ~p: ~p", [K, V]) || {K, V} <- InfoList],
    {'noreply', State};
handle_info({'nodedown', Node}, State) ->
    lager:info("VM ~s is no longer connected", [Node]),
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
handle_event(JObj, State) ->
    case kz_api:node(JObj) =:= kz_util:to_binary(node()) of
        'true' -> 'ignore';
        'false' ->
            {'reply', [{'node', node()}
                       ,{'state', State}
                      ]}
    end.

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

-spec get_zone(kz_json:object(), globals_state()) -> atom().
get_zone(JObj, #state{zones=Zones, zone=LocalZone}) ->
    case kz_json:get_first_defined([<<"Zone">>, <<"AMQP-Broker-Zone">>], JObj) of
        'undefined' ->
            case kz_json:get_value(<<"AMQP-Broker">>, JObj) of
                'undefined' -> LocalZone;
                Broker ->
                    case props:get_value(Broker, Zones) of
                        'undefined' -> LocalZone;
                        Zone -> kz_util:to_atom(Zone, 'true')
                    end
            end;
        Zone -> kz_util:to_atom(Zone, 'true')
    end.

-spec maybe_add_zone(kz_global(), globals_state()) -> 'ok'.
maybe_add_zone(#kz_global{zone='undefined'}, _State) -> 'ok';
maybe_add_zone(#kz_global{zone=Zone}, #state{zones=Zones}) ->
    case lists:member(Zone, Zones) of
        'false' -> gen_server:cast(?MODULE, {'add_zone', Zone});
        'true' -> 'ok'
    end.

-spec amqp_register(term(), pid(), globals_state()) -> 'yes' | 'no'.
amqp_register(Name, Pid, State) ->
    case where(Name) of
        undefined -> do_amqp_register(Name, Pid, State);
        _ -> no
    end.

-spec do_amqp_register(term(), pid(), globals_state()) -> 'yes' | 'no'.
do_amqp_register(Name, Pid, #state{}=State) ->
    Payload = [{<<"Name">>, Name}
               ,{<<"State">>, 'pending'}
                | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    case kz_amqp_worker:call_collect(Payload, ?AMQP_REGISTER_FUN) of
        {'ok', JObjs} ->
            amqp_register_check(JObjs, Name, Pid, State);
        {'timeout', JObjs} ->
            amqp_register_check(JObjs, Name, Pid, State);
        {'error', Error} ->
            lager:error("error '~p' calling register ~p", [Error, Name]),
            'no'
    end.

-spec amqp_register_check(kz_json:objects(), term(), pid(), globals_state()) -> 'yes' | 'no'.
amqp_register_check([], Name, Pid, State) ->
    register_local(Name, Pid, State);
amqp_register_check([JObj | JObjs], Name, Pid, State) ->
    case kapi_globals:is_pending(JObj) of
        'true' -> amqp_register_check(JObjs, Name, Pid, State);
        'false' -> 'no'
    end.

-spec register_local(term(), pid(), globals_state()) -> 'yes'.
register_local(Name, Pid, #state{zone=Zone, queue=Q}) ->
    Global = #kz_global{
                        node = node()
                       ,zone = Zone
                       ,server = Q
                       ,pid = Pid
                       ,monitor = erlang:monitor(process, Pid)
                       ,name = kz_util:to_binary(Name)
                       ,state='local'
               },
    ets:insert(?TAB_NAME, Global),
    advertise_register(Global),
    'yes'.

-spec advertise_register(kz_global()) -> 'ok'.
advertise_register(#kz_global{name=Name, server=Q}) ->
    Payload = [{<<"Name">>, Name}
               ,{<<"State">>, 'local'}
                | kz_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
              ],
    kz_amqp_worker:cast(Payload, ?AMQP_REGISTER_FUN).

-spec register_remote(kz_global()) -> pid().
register_remote(Global) ->
    {Pid, Ref} = start_proxy(Global),
    ets:insert(?TAB_NAME, Global#kz_global{monitor=Ref
                                           ,pid=Pid
                                          }),
    Pid.


-spec amqp_unregister(term()) -> 'ok'.
amqp_unregister(Name) ->
    case where(Name) of
        #kz_global{state='local'
                  } = Global ->
            do_amqp_unregister(Global, 'normal');
        _ -> ok
    end.

-spec do_amqp_unregister(kz_global(), term()) -> 'ok'.
do_amqp_unregister(#kz_global{name=Name}, Reason) ->
    Payload = [{<<"Name">>, Name}
               ,{<<"Reason">>, kapi_globals:encode(Reason)}
                | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    ets:delete(?TAB_NAME, Name),
    kz_amqp_worker:cast(Payload, ?AMQP_UNREGISTER_FUN).

-spec amqp_send(kz_global(), term()) -> 'ok'.
amqp_send(#kz_global{server=_ServerId, name=Name}, Message) ->
    Payload = [{<<"Name">>, Name}
               ,{<<"Message">>, kapi_globals:encode(Message)}
                | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    Publisher = fun kapi_globals:publish_send/1,
    kz_amqp_worker:cast(Payload, Publisher).

-spec amqp_call(kz_global(), term()) -> term().
amqp_call(#kz_global{server=_ServerId, name=Name}, Msg) ->
    Payload = [{<<"Name">>, Name}
               ,{<<"Message">>, kapi_globals:encode(Msg)}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    Publisher = fun kapi_globals:publish_call/1,
    case kz_amqp_worker:call(Payload, Publisher, fun kapi_globals:reply_msg_v/1) of
        {'ok', JObj} -> kapi_globals:reply(JObj);
        Error -> Error
    end.

-spec amqp_query(term()) -> 'undefined' | pid().
amqp_query(Name) ->
    Payload = [{<<"Name">>, Name}
                | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    case kz_amqp_worker:call_collect(Payload, ?AMQP_QUERY_FUN) of
        {'ok', [JObj]} ->
            gen_server:call(?SERVER, {'insert_remote', JObj});
        {'timeout', [JObj]} ->
            gen_server:call(?SERVER, {'insert_remote', JObj});
        {'timeout', []} ->
            'undefined';
        {'error', Error} ->
            lager:error("error '~p' calling register ~p", [Error, Name]),
            'undefined'
    end.

-spec handle_amqp_call(kz_json:object(), kz_proplist()) -> 'ok'.
handle_amqp_call(JObj, Props) ->
    Node = props:get_value('node', Props),
    case where(kapi_globals:name(JObj)) of
        #kz_global{state='local'
                   ,node=Node
                   ,pid=Pid
                  } ->
            case catch gen_server:call(Pid, kapi_globals:message(JObj)) of
                {'EXIT',Reason} -> amqp_reply(JObj, {'error', Reason});
                Res -> amqp_reply(JObj, Res)
            end;
        undefined -> 'ok'
    end.

-spec amqp_reply(kz_json:object(), term()) -> 'ok'.
amqp_reply(JObj, Result) ->
    Payload = [{<<"Name">>, kapi_globals:name(JObj)}
               ,{<<"Reply">>, kapi_globals:encode(Result)}
               ,{?KEY_MSG_ID, kz_api:msg_id(JObj)}
                | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    ServerId = kz_api:server_id(JObj),
    Publisher = fun(P) -> kapi_globals:publish_reply(ServerId, P) end,
    kz_amqp_worker:cast(Payload, Publisher).

-spec handle_amqp_send(kz_json:object(), kz_proplist()) -> 'ok'.
handle_amqp_send(JObj, Props) ->
    Node = props:get_value('node', Props),
    case where(kapi_globals:name(JObj)) of
        #kz_global{state='local'
                   ,node=Node
                   ,pid=Pid
                  } ->
            Pid ! kapi_globals:message(JObj);
        'undefined' -> 'ok'
    end.

-spec handle_amqp_query(kz_json:object(), kz_proplist()) -> 'ok'.
handle_amqp_query(JObj, Props) ->
    Node = props:get_value('node', Props),
    case where(kapi_globals:name(JObj)) of
        #kz_global{state='local'
                   ,node=Node
                  }=Global ->
            amqp_query_reply(JObj, Global);
        'undefined' -> 'ok'
    end.

-spec amqp_query_reply(kz_json:object(), kz_global()) -> 'ok'.
amqp_query_reply(JObj, Global) ->
    Payload = [{<<"Name">>, kapi_globals:name(JObj)}
               ,{<<"State">>, Global#kz_global.state}
               ,{?KEY_MSG_ID, kz_api:msg_id(JObj)}
                | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    ServerId = kz_api:server_id(JObj),
    Publisher = fun(P) -> kapi_globals:publish_query_resp(ServerId, P) end,
    kz_amqp_worker:cast(Payload, Publisher).

-spec handle_amqp_register(kz_json:object(), kz_proplist()) -> 'ok'.
handle_amqp_register(JObj, Props) ->
    handle_amqp_register(kapi_globals:state(JObj), JObj, Props).

-spec handle_amqp_register(kapi_globals:state(), kz_json:object(), kz_proplist()) -> 'ok'.
handle_amqp_register('pending', JObj, _Props) ->
    _Node = kz_api:node(JObj),
    case where(kapi_globals:name(JObj)) of
        #kz_global{}=Global ->
            advertise_register(Global),
            amqp_register_reply(JObj, Global);
        'undefined' -> amqp_register_reply(JObj)
    end;
handle_amqp_register('local', JObj, Props) ->
    State = props:get_value('state', Props),
    Global = from_json(JObj,  State),
    gen_server:cast(?SERVER, {'insert_remote', Global}).

-spec amqp_register_reply(kz_json:object()) -> 'ok'.
amqp_register_reply(JObj) ->
    Payload = [{<<"Name">>, kapi_globals:name(JObj)}
               ,{<<"State">>, 'pending'}
               ,{?KEY_MSG_ID, kz_api:msg_id(JObj)}
                | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    ServerId = kz_api:server_id(JObj),
    Publisher = fun(P) -> kapi_globals:publish_register_resp(ServerId, P) end,
    kz_amqp_worker:cast(Payload, Publisher).

-spec amqp_register_reply(kz_json:object(), kz_global()) -> 'ok'.
amqp_register_reply(JObj, Global) ->
    Payload = [{<<"Name">>, kapi_globals:name(JObj)}
               ,{<<"State">>, Global#kz_global.state}
               ,{?KEY_MSG_ID, kz_api:msg_id(JObj)}
                | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    ServerId = kz_api:server_id(JObj),
    Publisher = fun(P) -> kapi_globals:publish_register_resp(ServerId, P) end,
    kz_amqp_worker:cast(Payload, Publisher).


-spec handle_amqp_unregister(kz_json:object(), kz_proplist()) -> 'ok'.
handle_amqp_unregister(JObj, _Props) ->
    case where(kapi_globals:name(JObj)) of
        #kz_global{}=Global ->
            gen_server:cast(?MODULE, {'amqp_delete', Global, kapi_globals:reason(JObj)});
        _ -> 'ok'
    end.

-spec send(Name, Msg) -> Pid when
          Name :: term(),
          Msg :: term(),
          Pid :: pid().
send(Name, Msg) ->
    case where(Name) of
        #kz_global{pid=Pid} ->
            Pid ! Msg,
            Pid;
        undefined ->
            exit({'badarg', {Name, Msg}})
    end.

-spec whereis_name(Name) -> pid() | 'undefined' when
      Name :: term().
whereis_name(Name) ->
    case where(Name) of
        #kz_global{pid=Pid} -> Pid;
        'undefined' -> amqp_query(Name)
    end.

-spec where(Name) -> kz_global() | 'undefined' when
          Name :: term().
where(Name) ->
    case ets:lookup(?TAB_NAME, kz_util:to_binary(Name)) of
        [#kz_global{}=Global] -> Global;
        [] -> 'undefined'
    end.

-spec register_name(Name, Pid) -> 'yes' | 'no' when
          Name :: term(),
          Pid :: pid().
register_name(Name, Pid) when is_pid(Pid) ->
    case where(Name) of
        undefined ->
            gen_server:call(?SERVER, {'register', Name, Pid}, 'infinity');
        _ ->
            'no'
    end.

-spec unregister_name(Name) -> _ when
          Name :: term().
unregister_name(Name) ->
    case where(Name) of
        'undefined' ->
            'ok';
        _ ->
            gen_server:call(?MODULE, {'unregister', Name}, 'infinity')
    end.

-spec start_proxy(kz_global()) -> {pid(), reference()}.
start_proxy(Global) ->
    spawn_opt(fun() -> proxy_loop(Global) end, [link, monitor]).

proxy_loop(#kz_global{}=Global) ->
    receive
        {'$gen_call', {_Pid, _Mref}=From, Request} ->
            gen_server:reply(From, amqp_call(Global, Request)),
            proxy_loop(Global);
        {'$proxy_stop', Reason} ->
            gen_server:call(?MODULE, {'delete_remote', self()}),
            exit(Reason);
        '$proxy_stop' ->
            gen_server:call(?MODULE, {'delete_remote', self()}),
            exit('normal');
        Request ->
            amqp_send(Global, Request),
            proxy_loop(Global)
    end.

-spec from_json(kz_json:object(), globals_state()) -> kz_global().
from_json(JObj, State) ->
    Node = kz_api:node(JObj),
    Zone = get_zone(JObj, State),
    Global = #kz_global{node=kz_util:to_atom(Node, 'true')
                        ,zone=Zone
                        ,server = kz_api:server_id(JObj)
                        ,name = kapi_globals:name(JObj)
                        ,state = kapi_globals:state(JObj)
                       },
    maybe_add_zone(Global, State),
    Global.

-spec delete_by_pid(pid()) -> 'ok'.
delete_by_pid(Pid) ->
    delete_by_pid(Pid, 'normal').

-spec delete_by_pid(pid(), term()) -> 'ok'.
delete_by_pid(Pid, Reason) ->
    MatchSpec = [{#kz_global{pid = Pid, _ = '_'} ,[],['$_']}],
    Globals = ets:select(?TAB_NAME, MatchSpec),
    delete_globals(Globals, Reason),
    lager:info("deleted ~p proxies", [length(Globals)]).

-spec delete_globals(kz_globals(), term()) -> 'ok'.
delete_globals([], _) -> 'ok';
delete_globals([#kz_global{node=Node} = Global | Globals], Reason)
  when Node =:= node() ->
    do_amqp_unregister(Global, Reason),
    delete_globals(Globals, Reason);
delete_globals([#kz_global{name=Name} | Globals], Reason) ->
    ets:delete(?TAB_NAME, Name),
    delete_globals(Globals, Reason).
