%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%    Karl Anderson
%%%    James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_originate).

-behaviour(gen_listener).

-export([start_link/2]).
-export([handle_originate_execute/2]).
-export([handle_call_events/2]).
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

-type created_uuid() :: {'fs' | 'api', ne_binary()}.
-record(state, {node :: atom()
                ,server_id :: api_binary()
                ,originate_req = wh_json:new() :: wh_json:object()
                ,uuid :: created_uuid()
                ,action :: api_binary()
                ,app :: api_binary()
                ,dialstrings :: api_binary()
                ,queue :: api_binary()
                ,control_pid :: api_pid()
                ,tref :: api_reference()
                ,fetch_id = wh_util:rand_hex_binary(16)
               }).
-type state() :: #state{}.

-define(BINDINGS, [{'self', []}]).
-define(RESPONDERS, [{{?MODULE, 'handle_originate_execute'}
                      ,[{<<"dialplan">>, <<"originate_execute">>}]
                     }
                     ,{{?MODULE, 'handle_call_events'}
                       ,[{<<"call_event">>, <<"*">>}]
                      }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-define(ORIGINATE_PARK, <<"&park()">>).
-define(ORIGINATE_EAVESDROP, <<"eavesdrop">>).
-define(REPLY_TIMEOUT, 5 * ?MILLISECONDS_IN_SECOND).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link(atom(), wh_json:object()) -> startlink_ret().
start_link(Node, JObj) ->
    gen_listener:start_link(?SERVER
                            ,[{'bindings', ?BINDINGS}
                              ,{'responders', ?RESPONDERS}
                              ,{'queue_name', ?QUEUE_NAME}
                              ,{'queue_options', ?QUEUE_OPTIONS}
                              ,{'consume_options', ?CONSUME_OPTIONS}
                             ]
                            ,[Node, JObj]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call_events(wh_json:object(), wh_proplist()) -> 'ok'.
handle_call_events(JObj, Props) ->
    Srv = props:get_value('server', Props),
    case props:get_value('uuid', Props) =:=  wh_json:get_binary_value(<<"Call-ID">>, JObj)
        andalso wh_json:get_value(<<"Event-Name">>, JObj)
    of
        <<"CHANNEL_EXECUTE_COMPLETE">> ->
            case wh_json:get_value(<<"Application-Name">>, JObj) of
                <<"bridge">> ->
                    gen_listener:cast(Srv, {'bridge_execute_complete', JObj});
                _Else -> 'ok'
            end;
        <<"CHANNEL_DESTROY">> ->
            gen_listener:cast(Srv, {'channel_destroy', JObj});
        _Else -> 'ok'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_originate_execute(wh_json:object(), wh_proplist()) -> 'ok'.
handle_originate_execute(JObj, Props) ->
    'true' = wapi_dialplan:originate_execute_v(JObj),
    Srv = props:get_value('server', Props),
    UUID = props:get_value('uuid', Props),
    lager:debug("recv originate_execute for ~s", [UUID]),
    _ = case wh_json:get_ne_binary_value(<<"Server-ID">>, JObj) of
            'undefined' -> 'ok';
            ServerId ->
                gen_listener:cast(Srv, {'update_server_id', ServerId})
        end,
    kz_cache:store_local(?ECALLMGR_UTIL_CACHE, {UUID, 'start_listener'}, 'true'),
    gen_listener:cast(Srv, {'originate_execute'}).

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
init([Node, JObj]) ->
    _ = wh_util:put_callid(JObj),
    ServerId = wh_json:get_ne_binary_value(<<"Server-ID">>, JObj),
    _ = bind_to_events(freeswitch:version(Node), Node),
    case wapi_resource:originate_req_v(JObj) of
        'false' ->
            Error = <<"originate failed to execute as JObj did not validate">>,
            publish_error(Error, 'undefined', JObj, ServerId),
            {'stop', 'normal'};
        'true' ->
            {'ok', #state{node=Node
                          ,originate_req=JObj
                          ,server_id=ServerId
                         }}
    end.

-spec bind_to_events({'ok', ne_binary()}, atom()) -> 'ok'.
bind_to_events({'ok', <<"mod_kazoo", _/binary>>}, Node) ->
    'ok' = freeswitch:event(Node, ['CUSTOM', 'loopback::bowout']);
bind_to_events(_, Node) ->
    gproc:reg({'p', 'l', {'event', Node, <<"loopback::bowout">>}}).

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
handle_cast({'gen_listener', {'created_queue', Q}}, State) ->
    lager:debug("starting originate request"),
    Routines = [fun create_uuid/1
                ,fun set_app/1
                ,fun maybe_update_endpoints/1
                ,fun get_originate_action/1
                ,fun build_originate_args/1
                ,fun update_dialstrings/1
               ],
    Return = lists:foldl(fun(F, #state{} = S) -> F(S);
                            (_, {'break', #state{} = NState}) -> NState;
                            (_, Ret) -> Ret
                         end
                         ,State#state{queue=Q}
                         ,Routines),
    case Return of
        #state{} = NewState ->
            {'noreply', NewState};
        {'stop', #state{} = NewState} ->
            {'stop', 'normal', NewState};
        Return ->
            Return
    end;
handle_cast({'update_server_id', ServerId}, State) ->
    {'noreply', State#state{server_id=ServerId}, 'hibernate'};
handle_cast({'maybe_update_node', Node}, #state{node=Node}=State) ->
    {'noreply', State};
handle_cast({'maybe_update_node', Node}, #state{node=_OldNode}=State) ->
    lager:debug("updating node from ~s to ~s", [_OldNode, Node]),
    {'noreply', State#state{node=Node}, 'hibernate'};
handle_cast({'originate_ready'}, State) ->
    originate_ready(State);
handle_cast({'originate_execute'}, #state{tref=TRef}=State) when is_reference(TRef) ->
    _ = erlang:cancel_timer(TRef),
    handle_cast({'originate_execute'}, State#state{tref='undefined'});
handle_cast({'originate_execute'}, #state{dialstrings=Dialstrings
                                          ,node=Node
                                          ,originate_req=JObj
                                          ,uuid={_, UUID}
                                          ,server_id=ServerId
                                          ,control_pid=CtrlPid
                                         }=State) ->
    ControlDisabled = wh_json:is_false(<<"Start-Control-Process">>, JObj, 'false'),
    case originate_execute(Node, Dialstrings, find_originate_timeout(JObj)) of
        {'ok', UUID} when is_pid(CtrlPid) ->
            lager:debug("originate completed for: ~s with ctrl ~p", [UUID, CtrlPid]),
            _ = publish_originate_resp(ServerId, JObj, UUID),
            {'stop', 'normal', State#state{control_pid='undefined'}};
        {'ok', WinningUUID} when is_pid(CtrlPid) ->
            lager:debug("originate completed for other UUID: ~s (not ~s)", [WinningUUID, UUID]),
            _ = publish_originate_resp(ServerId, JObj, WinningUUID),
            CtrlPids = ecallmgr_call_control:control_procs(WinningUUID),
            _ = case lists:member(CtrlPid, CtrlPids) of
                'true' -> 'ok';
                'false' -> ecallmgr_call_control:stop(CtrlPid)
            end,
            {'stop', 'normal', State#state{control_pid='undefined'}};
        {'ok', CallId} when ControlDisabled ->
            lager:debug("originate completed for: ~s with no control pid", [CallId]),
            _ = publish_originate_resp(ServerId, JObj, CallId),
            {'stop', 'normal', State#state{control_pid='undefined'}};
        {'ok', CallId} ->
            wh_util:put_callid(CallId),
            lager:debug("originate is executing, waiting for completion"),
            erlang:monitor_node(Node, 'true'),
            bind_to_call_events(CallId),
            CtrlQ = ecallmgr_call_control:queue_name(CtrlPid),
            _ = publish_originate_started(ServerId, CallId, JObj, CtrlQ),
            {'noreply', State#state{uuid={'api', CallId}}};
        {'error', Error} ->
            lager:debug("failed to originate: ~p", [Error]),
            _ = publish_error(Error, UUID, JObj, ServerId),
            {'stop', 'normal', State}
    end;
handle_cast({'bridge_execute_complete', JObj}, #state{server_id=ServerId}=State) ->
    lager:debug("received bridge complete event, sending originate response"),
    _ = publish_originate_resp(ServerId, JObj),
    {'stop', 'normal', State};
handle_cast({'channel_destroy', JObj}, #state{server_id=ServerId}=State) ->
    lager:debug("received channel destroy event, sending originate response"),
    _ = publish_originate_resp(ServerId, JObj),
    {'stop', 'normal', State};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
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
handle_info({'event', [_|Props]}, #state{uuid='undefined'}=State) ->
    case should_update_uuid('undefined', Props) of
        'true' ->
            NewUUID = props:get_value(<<"Acquired-UUID">>, Props),
            _ = update_uuid('undefined', NewUUID),
            {'noreply', State#state{uuid={'api', NewUUID}}};
        'false' ->
            {'noreply', State}
    end;
handle_info({'event', [_ | Props]}, #state{uuid={_, OldUUID}}=State) ->
    case should_update_uuid(OldUUID, Props) of
        'true' ->
            NewUUID = props:get_value(<<"Acquired-UUID">>, Props),
            _ = update_uuid(OldUUID, NewUUID),
            {'noreply', State#state{uuid={'api', NewUUID}}};
        'false' ->
            {'noreply', State}
    end;
handle_info({'tcp', _, Data}, State) ->
    Event = binary_to_term(Data),
   handle_info(Event, State);
handle_info({'abandon_originate'}, #state{tref='undefined'}=State) ->
    %% Cancelling a timer does not guarantee that the message has not
    %% already been delivered to the message queue.
    {'noreply', State};
handle_info({'abandon_originate'}, #state{originate_req=JObj
                                          ,uuid=UUID
                                          ,server_id=ServerId
                                         }=State) ->
    Error = <<"Failed to receive valid originate_execute in time">>,
    _ = publish_error(Error, UUID, JObj, ServerId),
    {'stop', 'normal', State};
handle_info({'nodedown', _}, #state{originate_req=JObj
                                    ,uuid=UUID
                                    ,server_id=ServerId
                                    ,node=Node
                                   }=State) ->
    erlang:monitor_node(Node, 'false'),
    Error = <<"lost connection to freeswitch node">>,
    _ = publish_error(Error, UUID, JObj, ServerId),
    {'stop', 'normal', State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State, 'hibernate'}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, #state{uuid={_, UUID}}) ->
    {'reply', [{'uuid', UUID}]};
handle_event(_JObj, #state{uuid=UUID}) ->
    {'reply', [{'uuid', UUID}]}.

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
terminate(_Reason, #state{control_pid=CtrlPid}) when is_pid(CtrlPid) ->
    lager:debug("stop abandoned call controll process ~p", [CtrlPid]),
    ecallmgr_call_control:stop(CtrlPid),
    lager:debug("originate termination: ~p", [_Reason]);
terminate(_Reason, _State) ->
    lager:debug("originate termination: ~p", [_Reason]).

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
-spec set_app(state()) -> state().
set_app(#state{originate_req=JObj} = State) ->
    ApplicationName = wh_json:get_value(<<"Application-Name">>, JObj),
    State#state{app = ApplicationName}.

-spec maybe_update_endpoints(state()) -> state().
maybe_update_endpoints(#state{originate_req=JObj} = State) ->
    case wh_json:get_value(<<"Loopback-Handler">>, JObj) of
        <<"ecallmgr">> ->
            lager:debug("trying to resolve loopback endpoints"),
            update_endpoints(State);
        _ ->
            lager:debug("loopback endpoints will be handled by freeswitch"),
            State
    end.

-spec update_endpoints(state()) -> state().
update_endpoints(#state{originate_req=JObj} = State) ->
    Endpoints = wh_json:get_value(<<"Endpoints">>, JObj, []),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    ResourceType = wh_json:get_value(<<"Resource-Type">>, JObj, <<"audio">>),
    DialMethod = wh_json:get_value(<<"Dial-Endpoint-Method">>, JObj),
    {LoopbackEndpoints, NormalEndpoints} = lists:splitwith(fun is_loopback/1, Endpoints),
    {NewDialMethod, NormalizedEndpoints} = lists:foldl(normalized_endpoints_acc(CallId, ResourceType)
                                                       ,{DialMethod, NormalEndpoints}
                                                       ,LoopbackEndpoints
                                                      ),
    NewJObj = wh_json:set_values([{<<"Endpoints">>, NormalizedEndpoints}
                                  ,{<<"Dial-Endpoint-Method">>, NewDialMethod}
                                 ]
                                 ,JObj),
    State#state{originate_req = NewJObj}.

-spec is_loopback(wh_json:object()) -> boolean().
is_loopback(JObj) ->
    wh_json:get_value(<<"Invite-Format">>, JObj) =:= <<"loopback">>.

-spec normalized_endpoints_acc(api_binary(), ne_binary()) ->
    fun((wh_json:object(), wh_json:objects()) -> wh_json:objects()).
normalized_endpoints_acc(CallId, ResourceType) ->
    fun (Endpoint, {DialMethod, Acc}) ->
            LoopbackId = wh_util:rand_hex_binary(8),
            {'ok', CtrlPid} = ecallmgr_loopback_control:start_link(CallId, ResourceType, LoopbackId, Endpoint),
            {'ok', SuggestedDialMethod, NormalizedEndpoints} =
                try
                    ecallmgr_loopback_control:get_endpoints(CtrlPid)
                catch
                    _:_ ->
                        {'ok', DialMethod, []}
                end,
            {choose_dial_method(DialMethod, SuggestedDialMethod)
             ,Acc ++ [set_ccv(<<"Loopback-ID">>, LoopbackId, E) || E <- NormalizedEndpoints]
            }
    end.

-spec choose_dial_method(api_binary(), api_binary()) -> api_binary().
choose_dial_method('undefined' = _OldMethod, NewMethod) ->
    NewMethod;
choose_dial_method(OldMethod, _NewMethod) ->
    OldMethod.

-spec set_ccv(ne_binary(), ne_binary(), wh_json:object()) -> wh_json:object().
set_ccv(Var, Value, Endpoint) ->
    wh_json:set_value([<<"Custom-Channel-Vars">>, Var], Value, Endpoint).

-spec get_originate_action(state()) -> state().
get_originate_action(#state{originate_req=JObj
                            ,node=Node
                            ,app = ApplicationName
                           }=State) ->
    Action = get_originate_action(ApplicationName, JObj),
    UseNode = maybe_update_node(JObj, Node),
    lager:debug("maybe updating node from ~s to ~s", [Node, UseNode]),
    lager:debug("originate action: ~s", [Action]),
    State#state{action=Action
                ,node=UseNode
               }.

-spec build_originate_args(state()) -> state().
build_originate_args(#state{originate_req=JObj
                            ,action = ?ORIGINATE_PARK
                            ,fetch_id=FetchId
                            ,dialstrings='undefined'
                           }=State) ->
    Endpoints = [update_endpoint(Endpoint, State)
                 || Endpoint <- wh_json:get_ne_value(<<"Endpoints">>, JObj, [])
                ],
    NewState = State#state{dialstrings=build_originate_args_from_endpoints(Endpoints, JObj, FetchId)},
    case wh_json:is_true(<<"Originate-Immediate">>, JObj) of
        'true' ->
            gen_listener:cast(self(), {'originate_execute'}),
            NewState;
        'false' ->
            originate_ready(NewState)
    end;
build_originate_args(#state{originate_req=JObj
                            ,app = ?ORIGINATE_EAVESDROP
                            ,fetch_id=FetchId
                            ,dialstrings='undefined'
                           }=State) ->
    originate_ready(State#state{dialstrings=build_originate_args(State, JObj, FetchId)});
build_originate_args(#state{originate_req=JObj
                            ,fetch_id=FetchId
                            ,dialstrings='undefined'
                           }=State) ->
    gen_listener:cast(self(), {'originate_execute'}),
    State#state{dialstrings=build_originate_args(State, JObj, FetchId)}.

-spec update_dialstrings(state()) -> state().
update_dialstrings(#state{dialstrings=DialString, action = Action} = State) ->
    State#state{dialstrings = iolist_to_binary([DialString, " ", Action])}.

-spec originate_ready(state()) -> {'noreply', state()} | {'stop', 'normal', state()}.
originate_ready(#state{node = _Node} = State) ->
    case start_control_process(State) of
        {'ok', #state{control_pid=Pid
                      ,uuid=UUID
                      ,originate_req=JObj
                      ,server_id=ServerId
                      ,queue=Q
                     }=State1} ->
            CtrlQ = gen_listener:queue_name(Pid),
            _ = publish_originate_ready(CtrlQ, UUID, JObj, Q, ServerId),
            {'noreply', update_dialstrings(State1#state{tref=start_abandon_timer()})};
        {'error', _E} ->
            lager:debug("failed to start control process: ~p", [_E]),
            {'stop', 'normal', State}
    end.

-spec get_originate_action(ne_binary(), wh_json:object()) -> ne_binary().
get_originate_action(<<"fax">>, JObj) ->
    lager:debug("got originate with action fax"),
    Data = wh_json:get_value(<<"Application-Data">>, JObj),
    <<"&txfax(${http_get(", Data/binary, ")})">>;
get_originate_action(<<"transfer">>, JObj) ->
    get_transfer_action(JObj, wh_json:get_value([<<"Application-Data">>, <<"Route">>], JObj));
get_originate_action(<<"bridge">>, JObj) ->
    lager:debug("got originate with action bridge"),
    CallId = wh_json:get_binary_value(<<"Existing-Call-ID">>, JObj),
    intercept_unbridged_only(CallId, JObj);
get_originate_action(<<"eavesdrop">>, JObj) ->
    lager:debug("got originate with action eavesdrop"),
    EavesdropCallId = wh_json:get_binary_value(<<"Eavesdrop-Call-ID">>, JObj),
    case ecallmgr_fs_channel:node(EavesdropCallId) of
        {'error', _} ->
            lager:debug("failed to find channel ~p in node list", [wh_json:get_value(<<"Eavesdrop-Call-ID">>, JObj)]),
            <<"error">>;
        {'ok', N} ->
            gen_listener:cast(self(), {'maybe_update_node', N}),
            get_eavesdrop_action(JObj)
    end;
get_originate_action(_, _) ->
    lager:debug("got originate with action park"),
    ?ORIGINATE_PARK.

-spec get_transfer_action(wh_json:object(), api_binary()) -> ne_binary().
get_transfer_action(_JObj, 'undefined') -> <<"error">>;
get_transfer_action(JObj, Route) ->
    Context = ?DEFAULT_FREESWITCH_CONTEXT,
    list_to_binary(["'m:^:", get_unset_vars(JObj)
                    ,"transfer:", Route
                    ," XML ", Context, "' inline"
                   ]).

-spec intercept_unbridged_only(ne_binary() | 'undefined', wh_json:object()) -> ne_binary().
intercept_unbridged_only('undefined', JObj) ->
    get_bridge_action(JObj);
intercept_unbridged_only(ExistingCallId, JObj) ->
    case wh_json:is_true(<<"Intercept-Unbridged-Only">>, JObj, 'true') of
        'true' ->
            <<" 'set:intercept_unbridged_only=true,intercept:", ExistingCallId/binary, "' inline ">>;
        'false' ->
            <<" 'set:intercept_unbridged_only=false,intercept:", ExistingCallId/binary, "' inline ">>
    end.

-spec get_bridge_action(wh_json:object()) -> ne_binary().
get_bridge_action(JObj) ->
    Data = wh_json:get_value(<<"Application-Data">>, JObj),
    case ecallmgr_util:build_channel(Data) of
        {'error', _} -> <<"error">>;
        {'ok', Channel} ->
            list_to_binary(["'m:^:", get_unset_vars(JObj), "bridge:", Channel, "' inline"])
    end.

-spec maybe_update_node(wh_json:object(), atom()) -> atom().
maybe_update_node(JObj, Node) ->
    case wh_json:get_binary_value(<<"Existing-Call-ID">>, JObj) of
        'undefined' -> Node;
        CallId ->
            case ecallmgr_fs_channel:node(CallId) of
                {'error', _} -> Node;
                {'ok', N} -> N
            end
    end.

-spec get_eavesdrop_action(wh_json:object()) -> ne_binary().
get_eavesdrop_action(JObj) ->
    {CallId, Group} = case wh_json:get_value(<<"Eavesdrop-Group-ID">>, JObj) of
                          'undefined' -> {wh_json:get_binary_value(<<"Eavesdrop-Call-ID">>, JObj), <<>>};
                          ID -> {<<"all">>, <<"eavesdrop_require_group=", ID/binary, ",">>}
                      end,
    case wh_json:get_value(<<"Eavesdrop-Mode">>, JObj) of
        <<"whisper">> -> <<Group/binary, "queue_dtmf:w2@500,eavesdrop:", CallId/binary, " inline">>;
        <<"full">> -> <<Group/binary, "queue_dtmf:w3@500,eavesdrop:", CallId/binary, " inline">>;
        <<"listen">> -> <<Group/binary, "eavesdrop:", CallId/binary, " inline">>;
        'undefined' -> <<Group/binary, "eavesdrop:", CallId/binary, " inline">>
    end.

-spec build_originate_args(state(), wh_json:object(), ne_binary()) -> api_binary().
build_originate_args(State, JObj, FetchId) ->
    case wh_json:get_value(<<"Endpoints">>, JObj, []) of
        [] ->
            lager:warning("no endpoints defined in originate request"),
            'undefined';
        [Endpoint] ->
            lager:debug("only one endpoint, don't create per-endpoint UUIDs"),
            build_originate_args_from_endpoints([update_endpoint(Endpoint, State)], JObj, FetchId);
        Endpoints ->
            lager:debug("multiple endpoints defined, assigning uuids to each"),
            UpdatedEndpoints = [update_endpoint(Endpoint, State) || Endpoint <- Endpoints],
            build_originate_args_from_endpoints(UpdatedEndpoints, JObj, FetchId)
    end.

-spec build_originate_args_from_endpoints(wh_json:objects(), wh_json:object(), ne_binary()) -> ne_binary().
build_originate_args_from_endpoints(Endpoints, JObj, FetchId) ->
    lager:debug("building originate command arguments"),
    DialSeparator = ecallmgr_util:get_dial_separator(JObj, Endpoints),
    DialStrings = ecallmgr_util:build_bridge_string(Endpoints, DialSeparator),
    ChannelVars = get_channel_vars(JObj, FetchId),
    list_to_binary([ChannelVars, DialStrings]).

-spec get_channel_vars(wh_json:object(), ne_binary()) -> iolist().
get_channel_vars(JObj, FetchId) ->
    CCVs = [{[<<"Custom-Channel-Vars">>, <<"Fetch-ID">>], FetchId}
            ,{[<<"Custom-Channel-Vars">>, <<"Ecallmgr-Node">>], wh_util:to_binary(node())}
            ,{[<<"Custom-Channel-Vars">>, <<?CALL_INTERACTION_ID>>], ?CALL_INTERACTION_DEFAULT}
           ],
    Vars = maybe_add_loopback(JObj, CCVs),
    J = wh_json:set_values(Vars, JObj),
    ecallmgr_fs_xml:get_channel_vars(J).

-spec maybe_add_loopback(wh_json:object(), wh_proplist()) -> wh_proplist().
maybe_add_loopback(JObj, Props) ->
    case wh_json:get_binary_boolean(<<"Simplify-Loopback">>, JObj) of
        'undefined' -> Props;
        SimpliFly -> add_loopback(wh_util:is_true(SimpliFly)) ++ Props
    end.

-spec add_loopback(boolean()) -> wh_proplist().
add_loopback('true') ->
    [{<<"Simplify-Loopback">>, 'true'}
     ,{<<"Loopback-Bowout">>, 'true'}
    ];
add_loopback('false') ->
    [{<<"Simplify-Loopback">>, 'false'}
     ,{<<"Loopback-Bowout">>, 'false'}
    ].

-spec originate_execute(atom(), ne_binary(), pos_integer()) ->
                               {'ok', ne_binary()} |
                               {'error', ne_binary() | 'timeout' | 'crash'}.
originate_execute(Node, Dialstrings, Timeout) ->
    lager:debug("executing originate on ~s: ~s", [Node, Dialstrings]),
    case freeswitch:api(Node
                        ,'originate'
                        ,wh_util:to_list(Dialstrings)
                        ,Timeout*?MILLISECONDS_IN_SECOND
                       )
    of
        {'ok', <<"+OK ", ID/binary>>} ->
            UUID = wh_util:strip_binary(binary:replace(ID, <<"\n">>, <<>>)),
            Media = get('hold_media'),
            _Pid = wh_util:spawn(fun set_music_on_hold/3, [Node, UUID, Media]),
            {'ok', UUID};
        {'ok', Other} ->
            lager:debug("recv other 'ok': ~s", [Other]),
            {'error', wh_util:strip_binary(binary:replace(Other, <<"\n">>, <<>>))};
        {'error', Error} when is_binary(Error) ->
            lager:debug("error originating: ~s", [Error]),
            {'error', wh_util:strip_binary(binary:replace(Error, <<"\n">>, <<>>))};
        {'error', _Reason} ->
            lager:debug("error originating: ~p", [_Reason]),
            {'error', <<"unspecified">>}
    end.

-spec set_music_on_hold(atom(), ne_binary(), api_binary()) -> 'ok'.
set_music_on_hold(_, _, 'undefined') -> 'ok';
set_music_on_hold(Node, UUID, Media) ->
    Resp = ecallmgr_fs_command:set(Node, UUID, [{<<"Hold-Media">>, Media}]),
    lager:debug("setting Hold-Media resp: ~p", [Resp]).

-spec bind_to_call_events(ne_binary()) -> 'ok'.
bind_to_call_events(CallId) ->
    lager:debug("binding to call events for ~s", [CallId]),
    Options = [{'callid', CallId}
               ,{'restrict_to', ['events']}
              ],
    gen_listener:add_binding(self(), 'call', Options).

-spec unbind_from_call_events() -> 'ok'.
unbind_from_call_events() ->
    lager:debug("unbind from call events"),
    gen_listener:rm_binding(self(), 'call', []).

-spec update_uuid(api_binary(), ne_binary()) -> 'ok'.
update_uuid(OldUUID, NewUUID) ->
    wh_util:put_callid(NewUUID),
    lager:debug("updating call id from ~s to ~s", [OldUUID, NewUUID]),
    unbind_from_call_events(),
    bind_to_call_events(NewUUID),
    'ok'.

-spec create_uuid(atom()) -> created_uuid().
-spec create_uuid(wh_json:object(), atom()) -> created_uuid().
-spec create_uuid(wh_json:object(), wh_json:object(), atom()) -> created_uuid().
create_uuid(#state{node=Node
                   ,originate_req=JObj
                   ,uuid='undefined'
                  }=State) ->
    UUID = {_, Id} = create_uuid(JObj, Node),
    wh_util:put_callid(Id),
    lager:debug("created uuid ~p", [UUID]),
    case wh_json:is_true(<<"Start-Control-Process">>, JObj, 'true')
        andalso start_control_process(State#state{uuid=UUID}) of
        'false' ->
            State#state{uuid=UUID};
        {'ok', #state{control_pid=Pid}=State1} ->
            lager:debug("started control proc ~p uuid ~p", [Pid, UUID]),
            maybe_send_originate_uuid(UUID, Pid, State),
            State1;
        {'error', _E} ->
            lager:debug("failed to start control proc for ~p: ~p", [UUID, _E]),
            {'stop', 'normal', State}
    end;

create_uuid(#state{} = State) ->
    State;

create_uuid(Node) when is_atom(Node) ->
    case freeswitch:api(Node, 'create_uuid', " ") of
        {'ok', UUID} ->
            wh_util:put_callid(UUID),
            lager:debug("FS generated our uuid: ~s", [UUID]),
            {'fs', UUID};
        {'error', _E} ->
            lager:debug("unable to get a uuid from ~s: ~p", [Node, _E]),
            {'fs', wh_util:rand_hex_binary(18)}
    end.

create_uuid(JObj, Node) ->
    case wh_json:get_binary_value(<<"Outbound-Call-ID">>, JObj) of
        'undefined' -> create_uuid(Node);
        CallId -> {'api', CallId}
    end.

create_uuid(Endpoint, JObj, Node) ->
    case wh_json:get_binary_value(<<"Outbound-Call-ID">>, Endpoint) of
        'undefined' -> create_uuid(JObj, Node);
        CallId -> {'api', CallId}
    end.

-spec get_unset_vars(wh_json:object()) -> iolist().
get_unset_vars(JObj) ->
    %% Refactor (Karl wishes he had unit tests here for you to use)
    ExportProps = [{K, <<>>} || K <- wh_json:get_value(<<"Export-Custom-Channel-Vars">>, JObj, [])],
    Export = [K || KV <- lists:foldr(fun ecallmgr_fs_xml:get_channel_vars/2
                                     ,[]
                                     ,[{<<"Custom-Channel-Vars">>, wh_json:from_list(ExportProps)}]
                                    ),
                   ([K, _] = string:tokens(binary_to_list(KV), "=")) =/= 'undefined'
             ],
    case ["unset:" ++ K
          || KV <- lists:foldr(fun ecallmgr_fs_xml:get_channel_vars/2, [], wh_json:to_proplist(JObj))
                 ,not lists:member(begin [K, _] = string:tokens(binary_to_list(KV), "="), K end, Export)]
    of
        [] -> "";
        Unset ->
            [string:join(Unset, "^")
            ,maybe_fix_ignore_early_media(Export)
            ,maybe_fix_group_confirm(Export)
            ,maybe_fix_fs_auto_answer_bug(Export)
            ,"^"
            ]
    end.

-spec maybe_fix_ignore_early_media(strings()) -> string().
maybe_fix_ignore_early_media(Export) ->
    case lists:member("ignore_early_media", Export) of
        'true' -> "";
        'false' -> "^unset:ignore_early_media"
    end.

-spec maybe_fix_group_confirm(strings()) -> string().
maybe_fix_group_confirm(Export) ->
    case lists:member("group_confirm_key", Export) of
        'true' -> "";
        'false' -> "^unset:group_confirm_key^unset:group_confirm_cancel_timeout^unset:group_confirm_file"
    end.

-spec maybe_fix_fs_auto_answer_bug(strings()) -> string().
maybe_fix_fs_auto_answer_bug(Export) ->
    case lists:member("sip_auto_answer", Export) of
        'true' -> "";
        'false' ->
            "^unset:sip_h_Call-Info^unset:sip_h_Alert-Info^unset:alert_info^unset:sip_invite_params^set:sip_auto_answer=false"
    end.

-spec publish_error(ne_binary(), created_uuid() | api_binary(), wh_json:object(), api_binary()) -> 'ok'.
publish_error(_, _, _, 'undefined') -> 'ok';
publish_error(Error, {_, UUID}, Request, ServerId) ->
    publish_error(Error, UUID, Request, ServerId);
publish_error(Error, UUID, Request, ServerId) ->
    lager:debug("originate error: ~s", [Error]),
    E = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Request)}
         ,{<<"Call-ID">>, UUID}
         ,{<<"Request">>, Request}
         ,{<<"Error-Message">>, cleanup_error(Error)}
         | wh_api:default_headers(<<"error">>, <<"originate_resp">>, ?APP_NAME, ?APP_VERSION)
        ],
    wh_api:publish_error(ServerId, props:filter_undefined(E)).

-spec cleanup_error(ne_binary()) -> ne_binary().
cleanup_error(<<"-ERR ", E/binary>>) -> E;
cleanup_error(E) -> E.

-spec publish_originate_ready(ne_binary(), created_uuid() | ne_binary(), wh_json:object(), api_binary(), api_binary()) -> 'ok'.
publish_originate_ready(CtrlQ, {_, UUID}, Request, Q, ServerId) ->
    publish_originate_ready(CtrlQ, UUID, Request, Q, ServerId);
publish_originate_ready(CtrlQ, UUID, Request, Q, ServerId) ->
    lager:debug("originate command is ready, waiting for originate_execute"),
    Props = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Request, UUID)}
             ,{<<"Call-ID">>, UUID}
             ,{<<"Control-Queue">>, CtrlQ}
             | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
            ],
    wapi_dialplan:publish_originate_ready(ServerId, Props).

-spec publish_originate_resp(api_binary(), wh_json:object()) -> 'ok'.
publish_originate_resp('undefined', _) -> 'ok';
publish_originate_resp(ServerId, JObj) ->
    Resp = wh_json:set_values([{<<"Event-Category">>, <<"resource">>}
                               ,{<<"Event-Name">>, <<"originate_resp">>}
                              ], JObj),
    wapi_resource:publish_originate_resp(ServerId, Resp).

-spec publish_originate_resp(api_binary(), wh_json:object(), ne_binary()) -> 'ok'.
publish_originate_resp('undefined', _JObj, _UUID) -> 'ok';
publish_originate_resp(ServerId, JObj, UUID) ->
    Resp = wh_json:set_values([{<<"Event-Category">>, <<"resource">>}
                              ,{<<"Application-Response">>, <<"SUCCESS">>}
                              ,{<<"Event-Name">>, <<"originate_resp">>}
                              ,{<<"Call-ID">>, UUID}
                              ], JObj),
    wapi_resource:publish_originate_resp(ServerId, Resp).

-spec publish_originate_started(api_binary(), ne_binary(), wh_json:object(), ne_binary()) -> 'ok'.
publish_originate_started('undefined', _, _, _) -> 'ok';
publish_originate_started(ServerId, CallId, JObj, CtrlQ) ->
    Resp = wh_json:from_list(
             props:filter_undefined(
               [{<<"Call-ID">>, CallId}
                ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                ,{<<"Control-Queue">>, CtrlQ}
                | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
               ])),
    wapi_resource:publish_originate_started(ServerId, Resp).

-spec publish_originate_uuid(api_binary(), created_uuid() | ne_binary(), wh_json:object(), ne_binary()) -> 'ok'.
publish_originate_uuid('undefined', _, _, _) -> 'ok';
publish_originate_uuid(ServerId, UUID, JObj, CtrlQueue) ->
    Resp = props:filter_undefined(
             [{<<"Outbound-Call-ID">>, UUID}
              ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
              ,{<<"Outbound-Call-Control-Queue">>, CtrlQueue}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    lager:debug("sent originate_uuid to ~s", [ServerId]),
    wapi_resource:publish_originate_uuid(ServerId, Resp).

-spec maybe_send_originate_uuid(created_uuid(), pid(), state()) -> 'ok'.
maybe_send_originate_uuid({'fs', UUID}, Pid, #state{server_id=ServerId
                                                    ,originate_req=JObj
                                                   }) ->
    CtlQ = gen_listener:queue_name(Pid),
    publish_originate_uuid(ServerId, UUID, JObj, CtlQ);
maybe_send_originate_uuid(_, _, _) -> 'ok'.

-spec find_originate_timeout(wh_json:object()) -> pos_integer().
find_originate_timeout(JObj) ->
    OTimeout = case wh_json:get_integer_value(<<"Timeout">>, JObj) of
                   'undefined' -> 10;
                   LT when LT > 0 -> LT;
                   _ -> 10
               end,
    find_max_endpoint_timeout(
      wh_json:get_value(<<"Endpoints">>, JObj, [])
      ,OTimeout
     ).

-spec find_max_endpoint_timeout(wh_json:objects(), pos_integer()) -> pos_integer().
find_max_endpoint_timeout([], T) -> T;
find_max_endpoint_timeout([EP|EPs], T) ->
    case wh_json:get_integer_value(<<"Endpoint-Timeout">>, EP) of
        'undefined' -> find_max_endpoint_timeout(EPs, T);
        Timeout when Timeout > T -> find_max_endpoint_timeout(EPs, Timeout);
        _ -> find_max_endpoint_timeout(EPs, T)
    end.

-spec start_control_process(state()) ->
                                   {'ok', state()} |
                                   {'error', any()}.
start_control_process(#state{originate_req=JObj
                             ,node=Node
                             ,uuid={_, Id}=UUID
                             ,server_id=ServerId
                             ,fetch_id=FetchId
                             ,control_pid='undefined'
                            }=State) ->
    case ecallmgr_call_sup:start_control_process(Node, Id, FetchId, ServerId, wh_json:new()) of
        {'ok', CtrlPid} when is_pid(CtrlPid) ->
            _ = maybe_send_originate_uuid(UUID, CtrlPid, State),
            kz_cache:store_local(?ECALLMGR_UTIL_CACHE, {Id, 'start_listener'}, 'true'),
            lager:debug("started control pid ~p for uuid ~s", [CtrlPid, Id]),
            {'ok', State#state{control_pid=CtrlPid}};
        {'error', _E}=E ->
            Error = <<"failed to preemptively start a call control process">>,
            _ = publish_error(Error, UUID, JObj, ServerId),
            E
    end;
start_control_process(#state{control_pid=_Pid
                             ,uuid=_UUID
                            }=State) ->
    lager:debug("control process ~p exists for uuid ~p", [_Pid, _UUID]),
    {'ok', State}.

-spec maybe_start_call_handlers(created_uuid(), state()) -> 'ok'.
maybe_start_call_handlers(UUID, #state{originate_req=JObj}=State) ->
    case wh_json:is_true(<<"Start-Control-Process">>, JObj, 'true')
             andalso start_control_process(State#state{uuid=UUID}) of
        'false' -> 'ok';
        {'ok', #state{control_pid=_Pid}} ->
            lager:debug("started control process for ~p: ~p", [UUID, _Pid]);
        {'error', _E} ->
            lager:debug("failed to start control process for ~p: ~p", [UUID, _E])
    end.

-spec start_abandon_timer() -> reference().
start_abandon_timer() ->
    erlang:send_after(?REPLY_TIMEOUT, self(), {'abandon_originate'}).

-spec update_endpoint(wh_json:object(), state()) -> wh_json:object().
update_endpoint(Endpoint, #state{node=Node
                                 ,originate_req=JObj
                                 ,uuid=GlobalUUID
                                }=State) ->
    {_, Id} = UUID =
        case wh_json:get_value(<<"Outbound-Call-ID">>, Endpoint) of
            'undefined' -> create_uuid(Endpoint, JObj, Node);
            OutboundCallId -> {'api', OutboundCallId}
        end,

    case uuid_matches(UUID, GlobalUUID) of
        'true' -> 'ok';
        'false' ->
            maybe_start_call_handlers(UUID, State#state{uuid=UUID
                                                        ,control_pid='undefined'
                                                       })
    end,

    fix_hold_media(wh_json:set_value(<<"origination_uuid">>, Id, Endpoint)).

-spec uuid_matches(created_uuid(), created_uuid()) -> boolean().
uuid_matches({_, UUID}, {_, UUID}) -> 'true';
uuid_matches(_, _) -> 'false'.

-spec fix_hold_media(wh_json:object()) -> wh_json:object().
fix_hold_media(Endpoint) ->
    put('hold_media', wh_json:get_value(<<"Hold-Media">>, Endpoint)),
    wh_json:delete_key(<<"Hold-Media">>, Endpoint).

-spec should_update_uuid(api_binary(), wh_proplist()) -> boolean().
should_update_uuid(OldUUID, Props) ->
    case props:get_value(<<"Event-Subclass">>, Props, props:get_value(<<"Event-Name">>, Props)) of
        <<"loopback::bowout">> ->
            lager:debug("bowout detected with ~s, old uuid is ~s"
                        ,[props:get_value(?RESIGNING_UUID, Props), OldUUID]
                       ),
            props:get_value(?RESIGNING_UUID, Props) =:= OldUUID;
        _ -> 'false'
    end.
