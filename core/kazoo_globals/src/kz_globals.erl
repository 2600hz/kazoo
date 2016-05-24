%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(kz_globals).

-behaviour(gen_listener).

%% Public API
-export([whereis_name/1
        ,register_name/2
        ,unregister_name/1
        ,send/2
        ,registered/0
        ,reconcile/0
        ]).

-export([start_link/0
        ,stop/0, flush/0
        ,delete_by_pid/1
        ]).

%% gen listener callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

%% ETS Management
-export([table_id/0
         ,table_options/0
         ,gift_data/0
         ,find_me/0
        ]).

%% AMQP handlers
-export([handle_amqp_register/2
        ,handle_amqp_unregister/2
        ,handle_amqp_send/2
        ,handle_amqp_call/2
        ,handle_amqp_query/2
        ]).

-include("kazoo_globals.hrl").

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

-define(AMQP_REGISTER_FUN, fun kapi_globals:publish_register/1).
-define(AMQP_UNREGISTER_FUN, fun kapi_globals:publish_unregister/1).
-define(AMQP_QUERY_FUN, fun kapi_globals:publish_query/1).
-define(AMQP_CALL_SCOPE, {'kazoo_globals', 'true', 'true'}).

-define(TAB_NAME, 'kazoo_global_names').

-record(state, {zone = 'local' :: atom()
               ,zones = [] :: kz_proplist()
               ,queue :: api_binary()
               ,node = node() :: atom()
               ,ready = 'false' :: boolean()
               }).
-type globals_state() :: #state{}.

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

-spec stop() -> 'ok'.
stop() ->
    gen_listener:cast(?SERVER, 'stop').

-spec flush() -> 'ok'.
flush() ->
    gen_listener:call(?SERVER, 'flush').

-spec send(kz_global:name(), term()) -> pid().
send(Name, Msg) ->
    case where(Name) of
        'undefined' -> exit({'badarg', {Name, Msg}});
        Global ->
            kz_global_proxy:send(kz_global:pid(Global), Msg),
            kz_global:pid(Global)
    end.

-spec whereis_name(kz_global:name()) -> api_pid().
whereis_name(Name) ->
    gen_listener:call(?SERVER, {'where_is', Name}, 'infinity').

-spec where_is(kz_global:name(), globals_state()) -> api_pid().
where_is(Name, State) ->
    case where(Name) of
        'undefined' -> amqp_query(Name, State);
        Global -> kz_global:pid(Global)
    end.

-spec where(kz_global:name()) -> kz_global:global() | 'undefined'.
where(Name) ->
    case ets:lookup(?TAB_NAME, Name) of
        [Global] -> Global;
        [] -> 'undefined'
    end.

-spec register_name(kz_global:name(), pid()) -> 'yes' | 'no'.
register_name(Name, Pid) ->
    case where(Name) of
        'undefined' ->
            gen_listener:call(?SERVER, {'register', Name, Pid}, 'infinity');
        _Pid -> 'no'
    end.

registered() ->
    kz_global:all_names(?TAB_NAME).

-spec unregister_name(kz_global:name()) -> 'ok'.
unregister_name(Name) ->
    case where(Name) of
        'undefined' -> 'ok';
        _Global ->
            gen_listener:call(?SERVER, {'unregister', Name}, 'infinity')
    end.

-spec reconcile() -> [{pid(), 'ok'}].
reconcile() ->
    [{Pid, unregister_name(Name)} ||
        {Name, Pid} <- kz_global:all_dead_pids(?TAB_NAME)
    ].

-spec table_id() -> ?TAB_NAME.
table_id() -> ?TAB_NAME.

-spec table_options() -> list().
table_options() -> ['set'
                   ,'protected'
                   ,'named_table'
                   ,{'keypos', kz_global:name_pos()}
                   ,{'read_concurrency', 'true'}
                   ].

-spec gift_data() -> 'ok'.
gift_data() -> 'ok'.

-spec find_me() -> api_pid().
find_me() ->
    whereis(?SERVER).

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
    process_flag('trap_exit', 'true'),
    lager:debug("starting globals registry"),
    kapi_globals:declare_exchanges(),
    kapi_self:declare_exchanges(),

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
handle_call('flush', _From, State) ->
    ets:delete_all_objects(?TAB_NAME),
    lager:debug("flushed table"),
    {'reply', 'ok', State};
handle_call({'where_is', Name}, _From, State) ->
    {'reply', where_is(Name, State), State};
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
    lager:debug("inserting ~p", [Global]),
    ets:insert(?TAB_NAME, Global),
    {'reply', State};
handle_call({'delete', Global}, _From, State) ->
    lager:debug("deleting ~p", [Global]),
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
handle_cast({'amqp_delete', Global, 'undefined'}, State) ->
    kz_global_proxy:stop(kz_global:pid(Global)),
    {'noreply', State};
handle_cast({'amqp_delete', Global, Reason}, State) ->
    kz_global_proxy:stop(kz_global:pid(Global), Reason),
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
handle_cast('stop', #state{ready='true'}=State) ->
    lager:debug("instructed to stop"),
    ets:delete_all_objects(?TAB_NAME),
    {'stop', 'normal', State};
handle_cast('stop', #state{ready='false'}=State) ->
    lager:debug("want to go down, but not in control of ETS"),
    timer:sleep(50),
    ?MODULE:stop(),
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
handle_info({'DOWN', Ref, 'process', Pid, Reason}, State) ->
    lager:debug("monitor ~p detected process ~p exited with reason ~p", [Ref, Pid, Reason]),
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
handle_info({'ETS-TRANSFER', _TblId, _From, _Data}, State) ->
    remonitor_globals(),
    lager:info("ready to register names"),
    {'noreply', State#state{ready='true'}};
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
get_zone(JObj, State) ->
    case kz_json:get_first_defined([<<"Zone">>, <<"AMQP-Broker-Zone">>], JObj) of
        'undefined' ->
            get_zone_from_broker(JObj, State);
        Zone -> kz_util:to_atom(Zone, 'true')
    end.

-spec get_zone_from_broker(kz_json:object(), globals_state()) -> atom().
get_zone_from_broker(JObj, #state{zones=Zones
                                 ,zone=LocalZone
                                 }) ->
    case kz_json:get_value(<<"AMQP-Broker">>, JObj) of
        'undefined' -> LocalZone;
        Broker ->
            case props:get_value(Broker, Zones) of
                'undefined' -> LocalZone;
                Zone -> kz_util:to_atom(Zone, 'true')
            end
    end.

-spec maybe_add_zone(atom(), globals_state()) -> 'ok'.
maybe_add_zone('undefined', _State) -> 'ok';
maybe_add_zone(Zone, #state{zones=Zones}) ->
    case lists:member(Zone, Zones) of
        'false' -> gen_listener:cast(?SERVER, {'add_zone', Zone});
        'true' -> 'ok'
    end.

-spec amqp_register(kz_global:name(), pid(), globals_state()) -> 'yes' | 'no'.
amqp_register(Name, Pid, State) ->
    case where(Name) of
        'undefined' -> do_amqp_register(Name, Pid, State);
        _Pid -> 'no'
    end.

-spec do_amqp_register(kz_global:name(), pid(), globals_state()) -> 'yes' | 'no'.
do_amqp_register(Name, Pid, #state{}=State) ->
    Payload = [{<<"Name">>, Name}
              ,{<<"State">>, 'pending'}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    case kz_amqp_worker:call_collect(Payload, ?AMQP_REGISTER_FUN, ?AMQP_CALL_SCOPE) of
        {'ok', JObjs} ->
            amqp_register_check(JObjs, Name, Pid, State);
        {'timeout', JObjs} ->
            amqp_register_check(JObjs, Name, Pid, State);
        {'error', Error} ->
            lager:error("error '~p' calling register ~p", [Error, Name]),
            'no'
    end.

-spec amqp_register_check(kz_json:objects(), kz_global:name(), pid(), globals_state()) ->
                                 'yes' | 'no'.
amqp_register_check([], Name, Pid, State) ->
    lager:debug("no responses, registering locally"),
    register_local(Name, Pid, State);
amqp_register_check(Responses, Name, Pid, State) ->
    case lists:all(fun kapi_globals:is_pending/1, Responses) of
        'true' ->
            lager:debug("registering ~p locally", [Name]),
            register_local(Name, Pid, State);
        'false' ->
            'no'
    end.

-spec register_local(kz_global:name(), pid(), globals_state()) -> 'yes'.
register_local(Name, Pid, #state{zone=Zone, queue=Q}) ->
    Global = kz_global:new_local(Name, Pid, Zone, Q),
    lager:debug("inserting local ~p", [Global]),
    ets:insert(?TAB_NAME, Global),
    advertise_register(Global),
    'yes'.

-spec advertise_register(kz_global:global()) -> 'ok'.
-spec advertise_register(kz_global:name(), ne_binary()) -> 'ok'.
advertise_register(Global) ->
    advertise_register(kz_global:name(Global)
                       ,kz_global:server(Global)
                      ).
advertise_register(Name, Queue) ->
    Payload = [{<<"Name">>, Name}
              ,{<<"State">>, 'local'}
               | kz_api:default_headers(Queue, ?APP_NAME, ?APP_VERSION)
              ],
    kz_amqp_worker:cast(Payload, ?AMQP_REGISTER_FUN).

-spec register_remote(kz_global:global()) -> pid().
register_remote(Global) ->
    {Pid, Ref} = start_proxy(Global),
    ProxyGlobal = kz_global:update_with_pid_ref(Global, Pid, Ref),
    lager:debug("inserting proxy ~p", [ProxyGlobal]),
    ets:insert(?TAB_NAME, ProxyGlobal),
    Pid.

-spec amqp_unregister(kz_global:name()) -> 'ok'.
amqp_unregister(Name) ->
    case where(Name) of
        'undefined' -> 'ok';
        Global ->
            (kz_global:is_local(Global)
                andalso do_amqp_unregister(Global, 'normal') =:= 'ok'
            ) orelse lager:debug("can't unregister ~p", [Global]),
            'ok'
    end.

-spec do_amqp_unregister(kz_global:global(), term()) -> 'ok'.
do_amqp_unregister(Global, Reason) ->
    Name = kz_global:name(Global),
    Payload = [{<<"Name">>, Name}
              ,{<<"Reason">>, kapi_globals:encode(Reason)}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    lager:debug("deleting ~p", [Name]),
    ets:delete(?TAB_NAME, Name),
    kz_amqp_worker:cast(Payload, ?AMQP_UNREGISTER_FUN).

-spec amqp_query(kz_global:name(), globals_state()) -> api_pid().
amqp_query(Name, State) ->
    Payload = [{<<"Name">>, Name}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],

    case kz_amqp_worker:call_collect(Payload, ?AMQP_QUERY_FUN, ?AMQP_CALL_SCOPE) of
        {'error', Error} ->
            lager:error("error '~p' calling register ~p", [Error, Name]),
            'undefined';
        {_, []} ->
            lager:debug("cluster didn't know ~p", [Name]),
            'undefined';
        {_, [JObj]} ->
            lager:debug("cluster knew ~p: ~p", [Name, JObj]),
            Global = from_json(JObj, State),
            register_remote(Global)
    end.

-spec handle_amqp_call(kz_json:object(), kz_proplist()) -> 'ok'.
handle_amqp_call(JObj, Props) ->
    case where(kapi_globals:name(JObj)) of
        'undefined' -> 'ok';
        Global ->
            maybe_handle_local_call(JObj
                                   ,Global
                                   ,kz_global:is_local(Global)
                                   ,kz_global:node(Global) =:= props:get_value('node', Props)
                                   )
    end.

-spec maybe_handle_local_call(kz_json:object(), kz_global:global(), boolean(), boolean()) -> 'ok'.
maybe_handle_local_call(_JObj, _Global, 'false', _SameNode) -> 'ok';
maybe_handle_local_call(_JObj, _Global, 'true', 'false') -> 'ok';
maybe_handle_local_call(JObj, Global, 'true', 'true') ->
    attempt_call(JObj, kz_global:pid(Global)).

-spec attempt_call(kz_json:object(), pid()) -> 'ok'.
attempt_call(JObj, Pid) ->
    try gen_server:call(Pid, kapi_globals:message(JObj)) of
        Res -> amqp_reply(JObj, Res)
    catch
        Class:Reason ->
            amqp_reply(JObj, {'error', {Class, Reason}})
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
    case where(kapi_globals:name(JObj)) of
        'undefined' -> 'ok';
        Global ->
            maybe_handle_local_send(JObj
                                    ,Global
                                    ,kz_global:is_local(Global)
                                    ,kz_global:node(Global) =:= props:get_value('node', Props)
                                   )
    end.

-spec maybe_handle_local_send(kz_json:object(), kz_global:global(), boolean(), boolean()) -> 'ok'.
maybe_handle_local_send(_JObj, _Global, 'false', _SameNode) -> 'ok';
maybe_handle_local_send(_JObj, _Global, 'true', 'false') -> 'ok';
maybe_handle_local_send(JObj, Global, 'true', 'true') ->
    kz_global_proxy:send(kz_global:pid(Global), kapi_globals:message(JObj)).

-spec handle_amqp_query(kz_json:object(), kz_proplist()) -> 'ok'.
handle_amqp_query(JObj, Props) ->
    case where(kapi_globals:name(JObj)) of
        'undefined' -> 'ok';
        Global ->
            maybe_handle_local_query(JObj
                                     ,Global
                                     ,kz_global:is_local(Global)
                                     ,kz_global:node(Global) =:= props:get_value('node', Props)
                                    )
    end.

maybe_handle_local_query(_JObj, _Global, 'false', _SameNode) -> 'ok';
maybe_handle_local_query(_JObj, _Global, 'true', 'false') -> 'ok';
maybe_handle_local_query(JObj, Global, 'true', 'true') ->
    amqp_query_reply(JObj, Global).

-spec amqp_query_reply(kz_json:object(), kz_global:global()) -> 'ok'.
amqp_query_reply(JObj, Global) ->
    Payload = [{<<"Name">>, kapi_globals:name(JObj)}
              ,{<<"State">>, kz_global:state(Global)}
              ,{?KEY_MSG_ID, kz_api:msg_id(JObj)}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    ServerId = kz_api:server_id(JObj),
    Publisher = fun(P) -> kapi_globals:publish_query_resp(ServerId, P) end,
    kz_amqp_worker:cast(Payload, Publisher).

-spec handle_amqp_register(kz_json:object(), kz_proplist()) -> 'ok'.
handle_amqp_register(JObj, Props) ->
    handle_amqp_register(JObj, Props, kapi_globals:state(JObj)).

-spec handle_amqp_register(kz_json:object(), kz_proplist(), kapi_globals:state()) -> 'ok'.
handle_amqp_register(JObj, _Props, 'pending') ->
    case where(kapi_globals:name(JObj)) of
        'undefined' -> amqp_register_reply(JObj);
        Global ->
            advertise_register(Global),
            amqp_register_reply(JObj, Global)
    end;
handle_amqp_register(JObj, Props, 'local') ->
    State = props:get_value('state', Props),
    Global = from_json(JObj, State),
    gen_listener:cast(?SERVER, {'insert_remote', Global}).

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

-spec amqp_register_reply(kz_json:object(), kz_global:global()) -> 'ok'.
amqp_register_reply(JObj, Global) ->
    Payload = [{<<"Name">>, kapi_globals:name(JObj)}
              ,{<<"State">>, kz_global:state(Global)}
              ,{?KEY_MSG_ID, kz_api:msg_id(JObj)}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    ServerId = kz_api:server_id(JObj),
    Publisher = fun(P) -> kapi_globals:publish_register_resp(ServerId, P) end,
    kz_amqp_worker:cast(Payload, Publisher).

-spec handle_amqp_unregister(kz_json:object(), kz_proplist()) -> 'ok'.
handle_amqp_unregister(JObj, _Props) ->
    case where(kapi_globals:name(JObj)) of
        'undefined' -> 'ok';
        Global ->
            gen_listener:cast(?SERVER
                             ,{'amqp_delete', Global, kapi_globals:reason(JObj)}
                             )
    end.

-spec start_proxy(kz_global:global()) -> pid_ref().
start_proxy(Global) ->
    {'ok', Pid} = kz_global_proxies_sup:new(Global),
    link(Pid),
    {Pid, monitor('process', Pid)}.

-spec from_json(kz_json:object(), globals_state()) -> kz_global:global().
from_json(JObj, State) ->
    Zone = get_zone(JObj, State),
    Global = kz_global:from_jobj(JObj, Zone),
    maybe_add_zone(kz_global:zone(Global), State),
    Global.

-spec delete_by_pid(pid()) -> 'ok'.
delete_by_pid(Pid) ->
    delete_by_pid(Pid, 'normal').

-spec delete_by_pid(pid(), term()) -> 'ok'.
delete_by_pid(Pid, Reason) ->
    _Res = [delete_global(Global, Reason)
         || Global <- kz_global:all_globals_by_pid(?TAB_NAME, Pid)
        ],
    lager:info("deleted ~p proxies", [length(_Res)]).

-spec delete_global(kz_global:global(), term()) -> 'ok' | 'true'.
-spec delete_global(kz_global:global(), term(), atom()) -> 'ok' | 'true'.
delete_global(Global, Reason) ->
    delete_global(Global, Reason, kz_global:node(Global)).

delete_global(Global, Reason, Node) when Node =:= node() ->
    do_amqp_unregister(Global, Reason);
delete_global(Global, _Reason, _Node) ->
    lager:debug("deleting ~p", [Global]),
    ets:delete(?TAB_NAME, kz_global:name(Global)).

-spec remonitor_globals() -> 'ok'.
remonitor_globals() ->
    remonitor_globals(
      ets:select(kz_globals:table_id(), [{'_', [], ['$_']}], 1)
     ).

remonitor_globals('$end_of_table') -> 'ok';
remonitor_globals({[Global], Continuation}) ->
    remonitor_global(Global),
    remonitor_globals(ets:select(Continuation)).

remonitor_global(Global) ->
    remonitor_global(Global
                    ,erlang:is_process_alive(kz_global:pid(Global))
                    ,kz_global:is_local(Global)
                    ).

remonitor_global(Global, 'false', _IsLocal) ->
    lager:info("global ~p(~p) down, cleaning up"
              ,[kz_global:pid(Global), kz_global:name(Global)]
              ),
    unregister_name(kz_global:name(Global));
remonitor_global(Global, 'true', 'true') ->
    Pid = kz_global:pid(Global),
    Ref = erlang:monitor('process', Pid),
    lager:debug("remonitoring local ~p", [Pid]),
    ets:insert(?TAB_NAME, kz_global:update_with_pid_ref(Global, Pid, Ref));
remonitor_global(Global, 'true', 'false') ->
    Pid = kz_global:pid(Global),
    link(Pid),
    Ref = erlang:monitor('process', Pid),
    lager:debug("remonitoring proxy ~p", [Pid]),
    ets:insert(?TAB_NAME, kz_global:update_with_pid_ref(Global, Pid, Ref)).
