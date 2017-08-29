%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz INC
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
        ,is_ready/0
        ,stats/0
        ]).

-export([start_link/0
        ,stop/0, flush/0
        ,delete_by_pid/1
        ,where_is/1
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

-define(HEARTBEAT, rand:uniform(5 * ?MILLISECONDS_IN_SECOND, 15 * ?MILLISECONDS_IN_SECOND)).
-define(EXPIRE_PERIOD, 1 * ?MILLISECONDS_IN_SECOND).
-define(FUDGE_FACTOR, 1.25).

-define(AMQP_REGISTER_FUN, fun kapi_globals:publish_register/1).
-define(AMQP_UNREGISTER_FUN, fun kapi_globals:publish_unregister/1).
-define(AMQP_QUERY_FUN, fun kapi_globals:publish_query/1).
-define(AMQP_CALL_SCOPE, amqp_call_scope()).
-define(AMQP_CALL_TIMEOUT, amqp_call_timeout()).
-define(AMQP_CALL_COLLECT(P,F), kz_amqp_worker:call_collect(P, F, ?AMQP_CALL_SCOPE, ?AMQP_CALL_TIMEOUT)).

-define(TAB_NAME, 'kazoo_global_names').

-record(state, {zone = 'local' :: atom()
               ,zones = [] :: kz_proplist()
               ,queue :: api_binary()
               ,node = node() :: atom()
               ,is_consuming = 'false' :: boolean()
               ,has_ets = 'false' :: boolean()
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
    gen_listener:call(?SERVER, {'whereis_name', Name}, ?MILLISECONDS_IN_DAY).

-spec where_is(kz_global:name()) -> pid() | 'undefined'.
where_is(Name) ->
    case where(Name) of
        'undefined' -> 'undefined';
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
            gen_listener:call(?SERVER, {'register', Name, Pid}, ?MILLISECONDS_IN_DAY);
        _Pid -> 'no'
    end.

-spec registered() -> kz_global:names().
registered() ->
    kz_global:all_names(?TAB_NAME).

-spec stats() -> kz_proplist().
stats() ->
    case kz_global:stats(?TAB_NAME) of
        [] -> [{'total', 0}];
        Stats -> Stats
    end.

-spec unregister_name(kz_global:name()) -> 'ok'.
unregister_name(Name) ->
    case where(Name) of
        'undefined' -> 'ok';
        _Global ->
            gen_listener:call(?SERVER, {'unregister', Name}, ?MILLISECONDS_IN_DAY)
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
-spec init([]) -> {'ok', globals_state()}.
init([]) ->
    process_flag('trap_exit', 'true'),
    lager:debug("starting globals registry"),
    kapi_globals:declare_exchanges(),
    kapi_self:declare_exchanges(),

    Zone = kz_config:zone(),
    kz_nodes:notify_expire(),
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
-spec handle_call(any(), pid_ref(), globals_state()) -> handle_call_ret_state(globals_state()).
handle_call('flush', _From, State) ->
    ets:delete_all_objects(?TAB_NAME),
    lager:debug("flushed table"),
    {'reply', 'ok', State};
handle_call({'whereis_name', Name}, From, State) ->
    kz_util:spawn(fun maybe_amqp_query/2 , [Name, From]),
    {'noreply', State};
handle_call({'delete_remote', Pid}, _From, State) ->
    _ = delete_by_pid(Pid),
    {'reply', 'ok', State};
handle_call({'amqp_delete', Global, 'undefined'}, _From, State) ->
    kz_global_proxy:stop(kz_global:pid(Global)),
    {'reply', 'ok', State};
handle_call({'amqp_delete', Global, Reason}, _From, State) ->
    kz_global_proxy:stop(kz_global:pid(Global), Reason),
    {'reply', 'ok', State};
handle_call({'register_remote', JObj}, From, State) ->
    _ = register_remote(from_json(JObj, State), From),
    {'noreply', State};
handle_call({'register', Name, Pid}, From
           ,#state{zone=Zone, queue=Q}=State
           ) ->
    Global = kz_global:new_global(Name, Pid, Zone, Q, 'pending'),
    lager:debug("inserting ~p for ~p", [Name, Pid]),
    ets:insert(?TAB_NAME, Global),
    kz_util:spawn(fun amqp_register/2 , [Global, From]),
    {'noreply', State};
handle_call({'unregister', Name}, _From, State) ->
    {'reply', amqp_unregister(Name), State};
handle_call({'insert', Global}, _From, State) ->
    lager:debug("inserting global ~p", [Global]),
    ets:insert(?TAB_NAME, Global),
    {'reply', State};
handle_call({'delete', Global}, _From, State) ->
    lager:debug("deleting ~p", [Global]),
    ets:delete(?TAB_NAME, Global),
    {'reply', State};
handle_call('is_ready', _From, #state{is_consuming='true', has_ets='true'}=State) ->
    {'reply', 'true', State};
handle_call('is_ready', _From, State) ->
    {'reply', 'false', State};
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
-spec handle_cast(any(), globals_state()) -> handle_cast_ret_state(globals_state()).
handle_cast({'amqp_delete', Global, 'undefined'}, State) ->
    kz_global_proxy:stop(kz_global:pid(Global)),
    {'noreply', State};
handle_cast({'amqp_delete', Global, Reason}, State) ->
    kz_global_proxy:stop(kz_global:pid(Global), Reason),
    {'noreply', State};
handle_cast({'register_remote', JObj}, State) ->
    _ = register_remote(from_json(JObj, State), 'undefined'),
    {'noreply', State};
handle_cast({'register_remote', JObj, From}, State) ->
    _ = register_remote(from_json(JObj, State), From),
    {'noreply', State};
handle_cast({'register_local', Global, From}, State) ->
    gen_listener:reply(From, register_local(Global)),
    {'noreply', State};
handle_cast({'add_zone', Zone}, #state{zones=Zones}=State) ->
    case lists:member(Zone, Zones) of
        'false' -> {'noreply', State#state{zones=[Zone | Zones]}};
        'true' -> {'noreply', State}
    end;
handle_cast({'gen_listener', {'is_consuming', IsConsuming}}, State) ->
    {'noreply', State#state{is_consuming=IsConsuming}};
handle_cast({'gen_listener', {'created_queue', Q}}, State) ->
    lager:info("globals acquired queue name ~s", [Q]),
    {'noreply', State#state{queue=Q}};
handle_cast('stop', #state{has_ets='true'}=State) ->
    lager:debug("instructed to stop"),
    ets:delete_all_objects(?TAB_NAME),
    {'stop', 'normal', State};
handle_cast('stop', #state{has_ets='false'}=State) ->
    lager:debug("want to go down, but not in control of ETS"),
    timer:sleep(50),
    stop(),
    {'noreply', State};
handle_cast({'kz_nodes', {'expire', #kz_node{node=Node}}}, State) ->
    delete_by_node(Node),
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
-spec handle_info(any(), globals_state()) -> handle_info_ret_state(globals_state()).
handle_info({'DOWN', Ref, 'process', Pid, Reason}, State) ->
    lager:debug("monitor ~p detected process ~p exited with reason ~p", [Ref, Pid, Reason]),
    erlang:demonitor(Ref, ['flush']),
    delete_by_pid(Pid, Reason),
    {'noreply', State};
handle_info({'EXIT', Pid, Reason}, State) ->
    lager:debug("proxy process ~p exited with reason ~p", [Pid, Reason]),
    {'noreply', State};
handle_info({'ETS-TRANSFER', _TblId, _From, _Data}, State) ->
    remonitor_globals(),
    lager:info("ready to register names"),
    {'noreply', State#state{has_ets='true'}};
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
-spec handle_event(kz_json:object(), kz_proplist()) -> gen_listener:handle_event_return().
handle_event(JObj, State) ->
    case kz_api:node(JObj) =:= kz_term:to_binary(node()) of
        'true' -> 'ignore';
        'false' ->
            {'reply', [{'state', State}
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
-spec terminate(any(), globals_state()) -> 'ok'.
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
-spec code_change(any(), globals_state(), any()) -> {'ok', globals_state()}.
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
        Zone -> kz_term:to_atom(Zone, 'true')
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
                Zone -> kz_term:to_atom(Zone, 'true')
            end
    end.

-spec maybe_add_zone(atom(), globals_state()) -> 'ok'.
maybe_add_zone('undefined', _State) -> 'ok';
maybe_add_zone(Zone, #state{zones=Zones}) ->
    case lists:member(Zone, Zones) of
        'false' -> gen_listener:cast(?SERVER, {'add_zone', Zone});
        'true' -> 'ok'
    end.

-spec amqp_register(kz_global:global(), term()) -> 'ok'.
amqp_register(Global, From) ->
    Name = kz_global:name(Global),
    case where(Name) of
        Global ->
            case do_amqp_register(Global) of
                'yes' ->
                    lager:debug("amqp register ok, registering ~p locally", [Name]),
                    gen_listener:cast(?SERVER, {'register_local', Global, From});
                'no' -> gen_listener:reply(From, 'no')
            end;
        _Pid -> gen_listener:reply(From, 'no')
    end.

-spec do_amqp_register(kz_global:global()) -> 'yes' | 'no'.
do_amqp_register(Global) ->
    Payload = [{<<"Name">>, kz_global:name(Global)}
              ,{<<"State">>, kz_global:state(Global)}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    case ?AMQP_CALL_COLLECT(Payload, ?AMQP_REGISTER_FUN) of
        {'error', Error} ->
            lager:error("error '~p' calling register ~p", [Error, kz_global:name(Global)]),
            'no';
        {_, JObjs} ->
            amqp_register_check_responses(JObjs, Global)
    end.

-spec amqp_register_check_responses(kz_json:objects(), kz_global:global()) -> 'yes' | 'no'.
amqp_register_check_responses([], _) ->
    lager:debug("no responses, registering locally"),
    'yes';
amqp_register_check_responses(Responses, Global) ->
    case lists:all(fun(JObj) -> amqp_register_check_response(JObj, Global) end, Responses) of
        'true' ->
            lager:debug("all amqp responses cleared"),
            'yes';
        'false' ->
            lager:debug("one or more amqp register responses denied"),
            'no'
    end.

-spec amqp_register_check_response(kz_json:object(), kz_global:global()) -> boolean().
amqp_register_check_response(JObj, Global) ->
    Routines = [fun amqp_register_check_valid_state/2
               ,fun amqp_register_check_pending/2
               ],
    amqp_register_check_response_fold(Routines, JObj, Global) =:= 'yes'.

-type check_response_fun() :: fun((kz_json:object(), kz_global:global()) -> 'yes' | 'no' | 'maybe').
-type check_response_funs() :: [check_response_fun()].

-spec amqp_register_check_response_fold(check_response_funs(), kz_json:object(), kz_global:global()) -> 'yes' | 'no'.
amqp_register_check_response_fold([Fun | Funs], JObj, Global) ->
    case Fun(JObj, Global) of
        'maybe' -> amqp_register_check_response_fold(Funs, JObj, Global);
        Other -> Other
    end.


-spec amqp_register_check_valid_state(kz_json:object(), kz_global:global()) -> 'yes' | 'no' | 'maybe'.
amqp_register_check_valid_state(JObj, _Global) ->
    amqp_register_check_response_state(kapi_globals:state(JObj)).

-spec amqp_register_check_response_state(kapi_globals:state()) -> 'yes' | 'no' | 'maybe'.
amqp_register_check_response_state('none') -> 'yes';
amqp_register_check_response_state('pending') -> 'maybe';
amqp_register_check_response_state('local') -> 'no';
amqp_register_check_response_state('remote') -> 'yes'.

-spec amqp_register_check_pending(kz_json:object(), kz_global:global()) -> 'yes' | 'no' | 'maybe'.
amqp_register_check_pending(JObj, Global) ->
    T1 = kapi_globals:timestamp(JObj),
    T2 = kz_global:timestamp(Global),
    N1 = kapi_globals:node(JObj),
    N2 = kz_global:node(Global),
    case T1 of
        T1 when N1 =:= N2 -> 'no';
        T1 when T1 < T2 -> 'no';
        T1 when T1 > T2 -> 'yes';
        T1 when T1 =:= T2
                andalso N1 < N2 -> 'no';
        T1 when T1 =:= T2
                andalso N1 > N2 -> 'yes'
    end.

-spec register_local(kz_global:global()) -> 'yes' | 'no'.
register_local(Global) ->
    case where(kz_global:name(Global)) of
        Global ->
            Updated = kz_global:register_local(?TAB_NAME, Global),
            advertise_register(Updated),
            'yes';
        _ ->
            lager:debug("cannot register ~p locally", [kz_global:name(Global)]),
            'no'
    end.

-spec advertise_register(kz_global:global()) -> 'ok'.
advertise_register(Global) ->
    Payload = [{<<"Name">>, kz_global:name(Global)}
              ,{<<"State">>, 'registered'}
              ,{<<"Timestamp">>, kz_global:timestamp(Global)}
               | kz_api:default_headers(kz_global:server(Global), ?APP_NAME, ?APP_VERSION)
              ],
    kz_amqp_worker:cast(Payload, ?AMQP_REGISTER_FUN).

-spec register_remote(kz_global:global(), term()) -> 'ok'.
register_remote(Global, From) ->
    register_remote(where(kz_global:name(Global)), Global, From).

-spec register_remote(kz_global:global() | 'undefined', kz_global:global(), term()) -> 'ok'.
register_remote('undefined', Global, From) ->
    ProxyGlobal = kz_global:register_remote(?TAB_NAME, Global),
    maybe_register_remote_reply(From, kz_global:pid(ProxyGlobal));
register_remote(Local, Remote, From) ->
    case register_remote_resolve(Local, Remote) of
        'no' ->  maybe_register_remote_reply(From, kz_global:pid(Local));
        'yes' ->
            ProxyGlobal = kz_global:register_remote(?TAB_NAME, Remote),
            maybe_register_remote_reply(From, kz_global:pid(ProxyGlobal))
    end.

-spec register_remote_resolve(kz_global:global(), kz_global:global()) -> 'yes' | 'no'.
register_remote_resolve(Local, Remote) ->
    T1 = kz_global:timestamp(Local),
    T2 = kz_global:timestamp(Remote),
    N1 = kz_global:node(Local),
    N2 = kz_global:node(Remote),
    case T1 of
        T1 when N1 =:= N2 -> 'no';
        T1 when T1 < T2 -> 'no';
        T1 when T1 > T2 -> 'yes';
        T1 when T1 =:= T2
                andalso N1 < N2 -> 'no';
        T1 when T1 =:= T2
                andalso N1 > N2 -> 'yes'
    end.

-spec maybe_register_remote_reply(term(), pid()) -> 'ok'.
maybe_register_remote_reply('undefined', _Pid) -> 'ok';
maybe_register_remote_reply(From, Pid) ->
    gen_listener:reply(From, Pid).

-spec amqp_unregister(kz_global:name()) -> 'ok'.
amqp_unregister(Name) ->
    case where(Name) of
        'undefined' -> 'ok';
        Global ->
            (kz_global:is_local(Global)
             andalso do_amqp_unregister(Global, 'normal') =:= 'ok'
            )
                orelse lager:debug("can't unregister ~p", [Global]),
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

-spec maybe_amqp_query(kz_global:name(), term()) -> 'ok'.
maybe_amqp_query(Name, From) ->
    case where(Name) of
        'undefined' -> amqp_query(Name, From);
        Global -> gen_listener:reply(From, kz_global:pid(Global))
    end.

-spec amqp_query(kz_global:name(), term()) -> 'ok'.
amqp_query(Name, From) ->
    Payload = [{<<"Name">>, Name}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],

    case ?AMQP_CALL_COLLECT(Payload, ?AMQP_QUERY_FUN) of
        {'error', Error} ->
            lager:error("error '~p' calling register ~p", [Error, Name]),
            gen_listener:reply(From, 'undefined');
        {_, []} ->
            lager:debug("cluster didn't know ~p", [Name]),
            gen_listener:reply(From, 'undefined');
        {_, [_ | _] = JObjs} ->
            case lists:filter(fun(J)-> kapi_globals:state(J) /= 'none' end, JObjs) of
                [] ->
                    lager:debug("cluster didn't know ~p", [Name]),
                    gen_listener:reply(From, 'undefined');
                [JObj | _] ->
                    lager:debug("cluster knew ~p: ~p", [Name, JObj]),
                    Pid = gen_listener:call(?SERVER, {'register_remote', JObj}),
                    gen_listener:reply(From, Pid)
            end
    end.

-spec handle_amqp_call(kz_json:object(), kz_proplist()) -> 'ok'.
handle_amqp_call(JObj, _Props) ->
    case where(kapi_globals:name(JObj)) of
        'undefined' -> 'ok';
        Global ->
            maybe_handle_local_call(JObj
                                   ,Global
                                   ,kz_global:is_local(Global)
                                   ,kz_global:is_local_node(Global)
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
handle_amqp_send(JObj, _Props) ->
    case where(kapi_globals:name(JObj)) of
        'undefined' -> 'ok';
        Global ->
            maybe_handle_local_send(JObj
                                   ,Global
                                   ,kz_global:is_local(Global)
                                   ,kz_global:is_local_node(Global)
                                   )
    end.

-spec maybe_handle_local_send(kz_json:object(), kz_global:global(), boolean(), boolean()) -> 'ok'.
maybe_handle_local_send(_JObj, _Global, 'false', _SameNode) -> 'ok';
maybe_handle_local_send(_JObj, _Global, 'true', 'false') -> 'ok';
maybe_handle_local_send(JObj, Global, 'true', 'true') ->
    kz_global_proxy:send(kz_global:pid(Global), kapi_globals:message(JObj)).

-spec handle_amqp_query(kz_json:object(), kz_proplist()) -> 'ok'.
handle_amqp_query(JObj, _Props) ->
    case where(kapi_globals:name(JObj)) of
        'undefined' -> amqp_query_empty_reply(JObj);
        Global ->
            maybe_handle_local_query(JObj
                                    ,Global
                                    ,kz_global:is_local(Global)
                                    ,kz_global:is_local_node(Global)
                                    )
    end.

maybe_handle_local_query(JObj, _Global, 'false', _SameNode) ->
    amqp_query_empty_reply(JObj);
maybe_handle_local_query(JObj, _Global, 'true', 'false') ->
    amqp_query_empty_reply(JObj);
maybe_handle_local_query(JObj, Global, 'true', 'true') ->
    advertise_register(Global),
    amqp_query_reply(JObj, Global).

-spec amqp_query_empty_reply(kz_json:object()) -> 'ok'.
amqp_query_empty_reply(JObj) ->
    Payload = [{<<"Name">>, kapi_globals:name(JObj)}
              ,{<<"State">>, 'none'}
              ,{?KEY_MSG_ID, kz_api:msg_id(JObj)}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    ServerId = kz_api:server_id(JObj),
    Publisher = fun(P) -> kapi_globals:publish_query_resp(ServerId, P) end,
    kz_amqp_worker:cast(Payload, Publisher).

-spec amqp_query_reply(kz_json:object(), kz_global:global()) -> 'ok'.
amqp_query_reply(JObj, Global) ->
    Payload = [{<<"Name">>, kapi_globals:name(JObj)}
              ,{<<"State">>, kz_global:state(Global)}
              ,{<<"Timestamp">>, kz_global:timestamp(Global)}
              ,{?KEY_MSG_ID, kz_api:msg_id(JObj)}
               | kz_api:default_headers(kz_global:server(Global), ?APP_NAME, ?APP_VERSION)
              ],
    ServerId = kz_api:server_id(JObj),
    Publisher = fun(P) -> kapi_globals:publish_query_resp(ServerId, P) end,
    kz_amqp_worker:cast(Payload, Publisher).

-spec handle_amqp_register(kz_json:object(), kz_proplist()) -> 'ok'.
handle_amqp_register(JObj, _Props) ->
    lager:debug("checking amqp register ~p: ~p", [kapi_globals:state(JObj), JObj]),
    handle_amqp_register_state(JObj, kapi_globals:state(JObj)).

-spec handle_amqp_register_state(kz_json:object(), kapi_globals:state()) -> 'ok'.
handle_amqp_register_state(JObj, 'pending') ->
    case where(kapi_globals:name(JObj)) of
        'undefined' -> amqp_register_reply(JObj);
        Global ->
            maybe_handle_local_register(JObj, Global
                                       ,kz_global:is_local(Global)
                                       ,kz_global:is_local_node(Global)
                                       )
    end;
handle_amqp_register_state(JObj, 'registered') ->
    gen_listener:call(?SERVER, {'register_remote', JObj}).

-spec amqp_register_reply(kz_json:object()) -> 'ok'.
amqp_register_reply(JObj) ->
    Payload = [{<<"Name">>, kapi_globals:name(JObj)}
              ,{<<"State">>, 'none'}
              ,{?KEY_MSG_ID, kz_api:msg_id(JObj)}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    ServerId = kz_api:server_id(JObj),
    Publisher = fun(P) -> kapi_globals:publish_register_resp(ServerId, P) end,
    lager:debug("replying to ~p that ~p isn't found locally"
               ,[ServerId, kapi_globals:name(JObj)]
               ),
    kz_amqp_worker:cast(Payload, Publisher).

maybe_handle_local_register(JObj, Global, 'true', 'true') ->
    advertise_register(Global),
    amqp_register_reply(JObj, Global);
maybe_handle_local_register(JObj, Global, _, _) ->
    amqp_register_reply(JObj, Global).

-spec amqp_register_reply(kz_json:object(), kz_global:global()) -> 'ok'.
amqp_register_reply(JObj, Global) ->
    Payload = [{<<"Name">>, kapi_globals:name(JObj)}
              ,{<<"State">>, kz_global:state(Global)}
              ,{<<"Timestamp">>, kz_global:timestamp(Global)}
              ,{?KEY_MSG_ID, kz_api:msg_id(JObj)}
               | kz_api:default_headers(kz_global:server(Global), ?APP_NAME, ?APP_VERSION)
              ],
    ServerId = kz_api:server_id(JObj),
    Publisher = fun(P) -> kapi_globals:publish_register_resp(ServerId, P) end,
    lager:debug("replying to ~p that ~p is found locally in state ~p"
               ,[ServerId, kapi_globals:name(JObj), kz_global:state(Global)]
               ),
    kz_amqp_worker:cast(Payload, Publisher).

-spec handle_amqp_unregister(kz_json:object(), kz_proplist()) -> 'ok'.
handle_amqp_unregister(JObj, _Props) ->
    case where(kapi_globals:name(JObj)) of
        'undefined' -> 'ok';
        Global -> maybe_unregister_remote(Global
                                         ,kapi_globals:reason(JObj)
                                         ,kz_global:is_local_node(Global)
                                         )
    end.

-spec maybe_unregister_remote(kz_global:global(), term(), boolean()) -> 'ok'.
maybe_unregister_remote(_Global, _Reason, 'true') -> 'ok';
maybe_unregister_remote(Global, Reason, 'false') ->
    gen_listener:call(?SERVER
                     ,{'amqp_delete', Global, Reason}
                     ).

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

-spec delete_by_node(atom()) -> 'ok'.
delete_by_node(Node)
  when Node =:= node() ->
    'ok';
delete_by_node(Node) ->
    _Res = [begin
                kz_global_proxy:stop(kz_global:pid(Global)),
                delete_global(Global, 'expired')
            end
            || Global <- kz_global:all_globals_by_node(?TAB_NAME, Node)
           ],
    lager:info("deleted ~p proxies for expired node ~p", [length(_Res), Node]).

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
-spec remonitor_globals('$end_of_table' | {[kz_global:global()], ets:continuation()}) ->
                               'ok'.
remonitor_globals() ->
    remonitor_globals(
      ets:select(table_id(), [{'_', [], ['$_']}], 1)
     ).

remonitor_globals('$end_of_table') -> 'ok';
remonitor_globals({[Global], Continuation}) ->
    remonitor_global(Global),
    remonitor_globals(ets:select(Continuation)).

-spec remonitor_global(kz_global:global()) -> 'true'.
-spec remonitor_global(kz_global:global(), boolean(), boolean()) -> 'true'.
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

-spec amqp_call_timeout() -> integer().
amqp_call_timeout() ->
    {StartTime, _} = statistics('wall_clock'),
    amqp_call_timeout(StartTime).

-spec amqp_call_timeout(integer()) -> integer().
amqp_call_timeout(Seconds)
  when Seconds < ?MILLISECONDS_IN_MINUTE * 2 ->
    10 * ?MILLISECONDS_IN_SECOND;
amqp_call_timeout(_Seconds) ->
    2 * ?MILLISECONDS_IN_SECOND.

-spec amqp_call_scope() -> fun() | 'undefined'.
amqp_call_scope() ->
    Count = kz_nodes:globals_scope(),
    {StartTime, _} = statistics('wall_clock'),
    amqp_call_scope(Count, StartTime).

-spec amqp_call_scope(integer(), integer()) -> fun() | 'undefined'.
amqp_call_scope(_N, Seconds)
  when Seconds < ?MILLISECONDS_IN_MINUTE * 2 ->
    lager:debug("system running for less than 2 minutes, attempting to collect 10 responses from kazoo_globals"),
    amqp_call_scope_fun(10);
amqp_call_scope(N, _Seconds) ->
    amqp_call_scope_fun(N).

-spec amqp_call_scope_fun(integer()) -> fun() | 'undefined'.
amqp_call_scope_fun(0) -> 'undefined';
amqp_call_scope_fun(Count) ->
    lager:debug("attempting to collect ~p responses from kazoo_globals", [Count]),
    fun([_|_]=Responses) ->
            length(Responses) >= Count
    end.

-spec is_ready() -> boolean().
is_ready() ->
    try
        gen_listener:call(?MODULE, 'is_ready')
    catch
        _T:_E -> lager:info("globals is_ready returned ~p : ~p", [_T,_E]),
                 false
    end.

