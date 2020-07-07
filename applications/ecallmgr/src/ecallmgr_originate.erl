%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
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

-type created_uuid() :: {'fs' | 'api', kz_term:ne_binary()}.
-record(state, {node :: atom()
               ,server_id :: kz_term:api_binary()
               ,controller_q :: kz_term:api_binary()
               ,originate_req = kz_json:new() :: kz_json:object()
               ,uuid :: created_uuid() | 'undefined'
               ,action :: kz_term:api_binary()
               ,app :: kz_term:api_binary()
               ,dialstrings :: kz_term:api_binary()
               ,queue :: kz_term:api_binary()
               ,control_pid :: kz_term:api_pid()
               ,tref :: kz_term:api_reference()
               ,fetch_id = kz_binary:rand_hex(16)
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

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(atom(), kz_json:object()) -> kz_types:startlink_ret().
start_link(Node, JObj) ->
    gen_listener:start_link(?SERVER
                           ,[{'bindings', ?BINDINGS}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}
                            ,{'queue_options', ?QUEUE_OPTIONS}
                            ,{'consume_options', ?CONSUME_OPTIONS}
                            ]
                           ,[Node, JObj]
                           ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_call_events(kz_call_event:doc(), kz_term:proplist()) -> 'ok'.
handle_call_events(JObj, Props) ->
    Srv = props:get_value('server', Props),
    case props:get_value('uuid', Props) =:= kz_api:call_id(JObj)
        andalso kz_api:event_name(JObj)
    of
        <<"CHANNEL_EXECUTE_COMPLETE">> ->
            case kz_call_event:application_name(JObj) of
                <<"bridge">> ->
                    gen_listener:cast(Srv, {'bridge_execute_complete', JObj});
                _Else -> 'ok'
            end;
        <<"CHANNEL_DESTROY">> ->
            gen_listener:cast(Srv, {'channel_destroy', JObj});
        _Else -> 'ok'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_originate_execute(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_originate_execute(JObj, Props) ->
    'true' = kapi_dialplan:originate_execute_v(JObj),
    Srv = props:get_value('server', Props),
    UUID = props:get_value('uuid', Props),
    lager:debug("recv originate_execute for ~s", [UUID]),
    _ = case kz_api:queue_id(JObj) of
            'undefined' -> 'ok';
            ServerId -> gen_listener:cast(Srv, {'update_server_id', ServerId})
        end,
    kz_cache:store_local(?ECALLMGR_UTIL_CACHE, {UUID, 'start_listener'}, 'true'),
    gen_listener:cast(Srv, {'originate_execute'}).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([node() | kz_json:object()]) -> {'stop', 'normal'} | {'ok', state()}.
init([Node, JObj]) ->
    _ = kz_log:put_callid(JObj),
    ServerId = kz_api:server_id(JObj),
    ControllerQ = kz_api:queue_id(JObj),
    _ = bind_to_events(Node),
    case kapi_resource:originate_req_v(JObj) of
        'false' ->
            Error = <<"originate failed to execute as JObj did not validate">>,
            publish_error(Error, 'undefined', JObj, ServerId),
            {'stop', 'normal'};
        'true' ->
            {'ok', #state{node=Node
                         ,originate_req=JObj
                         ,server_id=ServerId
                         ,controller_q = ControllerQ
                         }}
    end.

-spec bind_to_events(atom()) -> 'ok'.
bind_to_events(Node) ->
    gproc:reg({'p', 'l', ?FS_EVENT_REG_MSG(Node, <<"loopback::bowout">>)}).

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'gen_listener', {'created_queue', Q}}, State) ->
    lager:debug("starting originate request"),
    gen_listener:cast(self(), {'get_originate_action'}),
    {'noreply', State#state{queue=Q}};
handle_cast({'update_server_id', ServerId}, State) ->
    {'noreply', State#state{server_id=ServerId}, 'hibernate'};
handle_cast({'maybe_update_node', Node}, #state{node=Node}=State) ->
    {'noreply', State};
handle_cast({'maybe_update_node', Node}, #state{node=_OldNode}=State) ->
    lager:debug("updating node from ~s to ~s", [_OldNode, Node]),
    {'noreply', State#state{node=Node}, 'hibernate'};
handle_cast({'create_uuid'}, #state{node=Node
                                   ,originate_req=JObj
                                   ,uuid='undefined'
                                   }=State) ->
    UUID = {_, Id} = create_uuid(JObj, Node),
    kz_log:put_callid(Id),
    lager:debug("created uuid ~p", [UUID]),
    case kz_json:is_true(<<"Start-Control-Process">>, JObj, 'true')
        andalso start_control_process(State#state{uuid=UUID}) of
        'false' ->
            gen_listener:cast(self(), {'build_originate_args'}),
            {'noreply', State#state{uuid=UUID}, 'hibernate'};
        {'ok', #state{control_pid=Pid}=State1} ->
            lager:debug("started control proc ~p uuid ~p", [Pid, UUID]),
            maybe_send_originate_uuid(UUID, Pid, State),
            gen_listener:cast(self(), {'build_originate_args'}),
            {'noreply', State1, 'hibernate'};
        {'error', _E} ->
            lager:debug("failed to start control proc for ~p: ~p", [UUID, _E]),
            {'stop', 'normal', State}
    end;
handle_cast({'get_originate_action'}, #state{originate_req=JObj
                                            ,node=Node
                                            }=State) ->
    gen_listener:cast(self(), {'build_originate_args'}),
    UseNode = maybe_update_node(JObj, Node),
    ApplicationName = kz_json:get_value(<<"Application-Name">>, JObj),
    Action = get_originate_action(ApplicationName, JObj, UseNode),
    lager:debug("originate action: ~s", [Action]),
    {'noreply', State#state{action=Action
                           ,app=ApplicationName
                           ,node=UseNode
                           }
    ,'hibernate'
    };
handle_cast({'build_originate_args'}, #state{uuid='undefined'}=State) ->
    gen_listener:cast(self(), {'create_uuid'}),
    {'noreply', State};

handle_cast({'build_originate_args'}, #state{originate_req=JObj
                                            ,action = ?ORIGINATE_PARK
                                            ,fetch_id=FetchId
                                            ,dialstrings='undefined'
                                            ,control_pid = CtrlPid
                                            }=State) ->
    case kz_json:is_true(<<"Originate-Immediate">>, JObj) of
        'true' -> gen_listener:cast(self(), {'originate_execute'});
        'false' -> gen_listener:cast(self(), {'originate_ready'})
    end,
    Endpoints = [update_endpoint(Endpoint, State)
                 || Endpoint <- kz_json:get_ne_value(<<"Endpoints">>, JObj, [])
                ],
    {'noreply', State#state{dialstrings=build_originate_args_from_endpoints(?ORIGINATE_PARK, Endpoints, JObj, FetchId, CtrlPid)}};
handle_cast({'build_originate_args'}, #state{originate_req=JObj
                                            ,action = Action
                                            ,app = ?ORIGINATE_EAVESDROP
                                            ,fetch_id=FetchId
                                            ,dialstrings='undefined'
                                            }=State) ->
    gen_listener:cast(self(), {'originate_ready'}),
    {'noreply', State#state{dialstrings=build_originate_args(Action, State, JObj, FetchId)}};
handle_cast({'build_originate_args'}, #state{originate_req=JObj
                                            ,action=Action
                                            ,fetch_id=FetchId
                                            ,dialstrings='undefined'
                                            }=State) ->
    case kz_json:is_true(<<"Originate-Immediate">>, JObj, 'true') of
        'true'  -> gen_listener:cast(self(), {'originate_execute'});
        'false' -> gen_listener:cast(self(), {'originate_ready'})
    end,
    {'noreply', State#state{dialstrings=build_originate_args(Action, State, JObj, FetchId)}};

handle_cast({'originate_ready'}, #state{node=_Node}=State) ->
    case start_control_process(State) of
        {'ok', #state{control_pid=Pid
                     ,uuid=UUID
                     ,originate_req=JObj
                     ,server_id=ServerId
                     ,queue=Q
                     }=State1} ->
            CtrlQ = gen_listener:queue_name(Pid),
            _ = publish_originate_ready(CtrlQ, UUID, JObj, Q, ServerId),
            {'noreply', State1#state{tref=start_abandon_timer()}};
        {'error', _E} ->
            lager:debug("failed to start control process: ~p", [_E]),
            {'stop', 'normal', State}
    end;
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
    ControlDisabled = kz_json:is_false(<<"Start-Control-Process">>, JObj, 'false'),
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
            kz_log:put_callid(CallId),
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

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'event', _UUID, FSJObj}, #state{uuid=CreatedUUID}=State) ->
    {'noreply', State#state{uuid=handle_fs_event(FSJObj, CreatedUUID)}};
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

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(_JObj, #state{uuid={_, UUID}}) ->
    {'reply', [{'uuid', UUID}]};
handle_event(_JObj, #state{uuid=UUID}) ->
    {'reply', [{'uuid', UUID}]}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{control_pid=CtrlPid}) when is_pid(CtrlPid) ->
    lager:debug("stop abandoned call control process ~p", [CtrlPid]),
    ecallmgr_call_control:stop(CtrlPid),
    lager:debug("originate termination: ~p", [_Reason]);
terminate(_Reason, _State) ->
    lager:debug("originate termination: ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec cache_fax_file(kz_term:ne_binary(), node()) -> {'ok' | 'error', kz_term:ne_binary()}.
cache_fax_file(File, Node) ->
    Self = self(),
    Fun = fun(Res, Reply) ->
                  lager:debug("cache fax file result : ~p", [{Res, Reply}]),
                  Self ! {cache_fax_file, {Res, Reply}}
          end,
    {ok, JobId} = freeswitch:bgapi(Node, 'http_get', <<"{prefetch=true}", File/binary>>, Fun),
    lager:debug("waiting for cache fax file result ~s", [JobId]),
    receive
        {cache_fax_file, Reply} -> Reply
    end.

-spec get_originate_action(kz_term:ne_binary(), kz_json:object(), node()) -> kz_term:ne_binary().
get_originate_action(<<"fax">>, JObj, Node) ->
    lager:debug("got originate with action fax"),
    Data = kz_json:get_value(<<"Application-Data">>, JObj),
    {'ok', File} = cache_fax_file(Data, Node),
    <<"&txfax(", File/binary, ")">>;
get_originate_action(<<"transfer">>, JObj, _Node) ->
    get_transfer_action(JObj, kz_json:get_value([<<"Application-Data">>, <<"Route">>], JObj));
get_originate_action(<<"bridge">>, JObj, _Node) ->
    lager:debug("got originate with action bridge"),
    CallId = kz_json:get_binary_value(<<"Existing-Call-ID">>, JObj),
    intercept_unbridged_only(CallId, JObj);
get_originate_action(<<"eavesdrop">>, JObj, _Node) ->
    lager:debug("got originate with action eavesdrop"),
    EavesdropCallId = kz_json:get_binary_value(<<"Eavesdrop-Call-ID">>, JObj),
    case ecallmgr_fs_channel:node(EavesdropCallId) of
        {'error', _} ->
            lager:debug("failed to find channel ~p in node list", [kz_json:get_value(<<"Eavesdrop-Call-ID">>, JObj)]),
            <<"error">>;
        {'ok', N} ->
            gen_listener:cast(self(), {'maybe_update_node', N}),
            get_eavesdrop_action(JObj)
    end;
get_originate_action(_, _, _) ->
    lager:debug("got originate with action park"),
    ?ORIGINATE_PARK.

-spec get_transfer_action(kz_json:object(), kz_term:api_binary()) -> kz_term:ne_binary().
get_transfer_action(_JObj, 'undefined') -> <<"error">>;
get_transfer_action(JObj, Route) ->
    Context = ?DEFAULT_FREESWITCH_CONTEXT,
    UnsetVars = get_unset_vars(JObj),
    list_to_binary(
      ["'m:^:", UnsetVars
      ,"transfer:", Route
      ," XML ", Context, "' inline"
      ]
     ).

-spec intercept_unbridged_only(kz_term:ne_binary() | 'undefined', kz_json:object()) -> kz_term:ne_binary().
intercept_unbridged_only('undefined', JObj) ->
    get_bridge_action(JObj);
intercept_unbridged_only(ExistingCallId, JObj) ->
    case kz_json:is_true(<<"Intercept-Unbridged-Only">>, JObj, 'true') of
        'true' ->
            <<" 'set:intercept_unbridged_only=true,intercept:", ExistingCallId/binary, "' inline ">>;
        'false' ->
            <<" 'set:intercept_unbridged_only=false,intercept:", ExistingCallId/binary, "' inline ">>
    end.

-spec get_bridge_action(kz_json:object()) -> kz_term:ne_binary().
get_bridge_action(JObj) ->
    Data = kz_json:get_value(<<"Application-Data">>, JObj),
    case ecallmgr_util:build_channel(Data) of
        {'error', _} -> <<"error">>;
        {'ok', Channel} ->
            UnsetVars = get_unset_vars(JObj),
            list_to_binary(
              ["'m:^:", UnsetVars
              ,"bridge:", Channel, "' inline"
              ]
             )
    end.

-spec maybe_update_node(kz_json:object(), atom()) -> atom().
maybe_update_node(JObj, Node) ->
    case kz_json:get_binary_value(<<"Existing-Call-ID">>, JObj) of
        'undefined' -> Node;
        CallId ->
            case ecallmgr_fs_channel:node(CallId) of
                {'error', _} -> Node;
                {'ok', Node} -> Node;
                {'ok', N} -> lager:debug("updating node from ~s to ~s", [Node, N]),
                             N
            end
    end.

-spec get_eavesdrop_action(kz_json:object()) -> kz_term:ne_binary().
get_eavesdrop_action(JObj) ->
    {CallId, Group} = case kz_json:get_value(<<"Eavesdrop-Group-ID">>, JObj) of
                          'undefined' -> {kz_json:get_binary_value(<<"Eavesdrop-Call-ID">>, JObj), <<>>};
                          ID -> {<<"all">>, <<"eavesdrop_require_group=", ID/binary, ",">>}
                      end,
    case kz_json:get_value(<<"Eavesdrop-Mode">>, JObj) of
        <<"whisper">> -> <<Group/binary, "queue_dtmf:w2@500,eavesdrop:", CallId/binary, " inline">>;
        <<"full">> -> <<Group/binary, "queue_dtmf:w3@500,eavesdrop:", CallId/binary, " inline">>;
        <<"listen">> -> <<Group/binary, "eavesdrop:", CallId/binary, " inline">>;
        'undefined' -> <<Group/binary, "eavesdrop:", CallId/binary, " inline">>
    end.

-spec build_originate_args(kz_term:ne_binary(), state(), kz_json:object(), kz_term:ne_binary()) -> kz_term:api_binary().
build_originate_args(Action, #state{control_pid=CtrlPid} = State, JObj, FetchId) ->
    case kz_json:get_value(<<"Endpoints">>, JObj, []) of
        [] ->
            lager:warning("no endpoints defined in originate request"),
            'undefined';
        [Endpoint] ->
            lager:debug("only one endpoint, don't create per-endpoint UUIDs"),
            build_originate_args_from_endpoints(Action, [update_endpoint(Endpoint, State)], JObj, FetchId, CtrlPid);
        Endpoints ->
            lager:debug("multiple endpoints defined, assigning uuids to each"),
            UpdatedEndpoints = [update_endpoint(Endpoint, State) || Endpoint <- Endpoints],
            build_originate_args_from_endpoints(Action, UpdatedEndpoints, JObj, FetchId, CtrlPid)
    end.

-spec build_originate_args_from_endpoints(kz_term:ne_binary(), kz_json:objects(), kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          kz_term:ne_binary().
build_originate_args_from_endpoints(Action, Endpoints, JObj, FetchId, CtrlPid) ->
    lager:debug("building originate command arguments"),
    DialSeparator = ecallmgr_util:get_dial_separator(JObj, Endpoints),

    DialStrings = ecallmgr_util:build_bridge_string(Endpoints, DialSeparator),

    ChannelVars = get_channel_vars(JObj, FetchId),
    CtrlQ = ecallmgr_call_control:queue_name(CtrlPid),

    list_to_binary([ChannelVars, "[^^!Call-Control-Queue='", CtrlQ, "']", DialStrings, " ", Action]).

-spec get_channel_vars(kz_json:object(), kz_term:ne_binary()) -> iolist().
get_channel_vars(JObj, FetchId) ->
    InteractionId = kz_json:get_value([<<"Custom-Channel-Vars">>, <<?CALL_INTERACTION_ID>>], JObj, ?CALL_INTERACTION_DEFAULT),

    CCVs = [{<<"Fetch-ID">>, FetchId}
           ,{<<"Ecallmgr-Node">>, kz_term:to_binary(node())}
           ,{<<?CALL_INTERACTION_ID>>, InteractionId}
           ],
    J = kz_json:from_list_recursive([{<<"Custom-Channel-Vars">>, add_ccvs(JObj, CCVs)}]),
    ecallmgr_fs_xml:get_channel_vars(kz_json:merge(JObj, J)).

-spec add_ccvs(kz_json:object(), kz_term:proplist()) -> kz_term:proplist().
add_ccvs(JObj, Props) ->
    Routines = [fun maybe_add_loopback/2
               ,fun maybe_add_origination_uuid/2
               ],
    lists:foldl(fun(Fun, Acc) -> Fun(JObj, Acc) end, Props, Routines).

-spec maybe_add_origination_uuid(kz_json:object(), kz_term:proplist()) -> kz_term:proplist().
maybe_add_origination_uuid(JObj, Props) ->
    case kz_json:get_ne_binary_value(<<"Outbound-Call-ID">>, JObj) of
        'undefined' -> Props;
        CallId -> [{<<"Origination-Call-ID">>, CallId} | Props]
    end.

-spec maybe_add_loopback(kz_json:object(), kz_term:proplist()) -> kz_term:proplist().
maybe_add_loopback(JObj, Props) ->
    case kz_json:get_binary_boolean(<<"Simplify-Loopback">>, JObj) of
        'undefined' -> Props;
        SimpliFly -> add_loopback(kz_term:is_true(SimpliFly)) ++ Props
    end.

-spec add_loopback(boolean()) -> kz_term:proplist().
add_loopback('true') ->
    [{<<"Simplify-Loopback">>, 'true'}
    ,{<<"Loopback-Bowout">>, 'true'}
    ];
add_loopback('false') ->
    [{<<"Simplify-Loopback">>, 'false'}
    ,{<<"Loopback-Bowout">>, 'false'}
    ].

-spec originate_execute(atom(), kz_term:ne_binary(), pos_integer()) ->
          {'ok', kz_term:ne_binary()} |
          {'error', kz_term:ne_binary() | 'timeout' | 'crash'}.
originate_execute(Node, Dialstrings, _Timeout) ->
    lager:debug("executing originate on ~s: ~s", [Node, Dialstrings]),
    freeswitch:async_api(Node, 'originate', Dialstrings).

-spec bind_to_call_events(kz_term:ne_binary()) -> 'ok'.
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

-spec update_uuid(kz_term:api_binary(), kz_term:ne_binary()) -> 'ok'.
update_uuid(OldUUID, NewUUID) ->
    kz_log:put_callid(NewUUID),
    lager:debug("updating call id from ~s to ~s", [OldUUID, NewUUID]),
    unbind_from_call_events(),
    bind_to_call_events(NewUUID),
    'ok'.


-spec create_uuid(atom()) -> created_uuid().
create_uuid(_Node) -> {'fs', kz_binary:rand_hex(18)}.

-spec create_uuid(kz_json:object(), atom()) -> created_uuid().
create_uuid(JObj, Node) ->
    case kz_json:get_binary_value(<<"Outbound-Call-ID">>, JObj) of
        'undefined' -> create_uuid(Node);
        CallId -> {'api', CallId}
    end.

-spec create_uuid(kz_json:object(), kz_json:object(), atom()) -> created_uuid().
create_uuid(Endpoint, _JObj, Node) ->
    case kz_json:get_binary_value(<<"Outbound-Call-ID">>, Endpoint) of
        'undefined' -> create_uuid(Node);
        CallId -> {'api', CallId}
    end.

-spec get_unset_vars(kz_json:object()) -> iolist().
get_unset_vars(JObj) ->
    %% Refactor (Karl wishes he had unit tests here for you to use)
    ExportProps = [{K, <<>>} || K <- kz_json:get_value(<<"Export-Custom-Channel-Vars">>, JObj, [])],
    Export = [K || KV <- lists:foldr(fun ecallmgr_fs_xml:kazoo_var_to_fs_var/2
                                    ,[]
                                    ,[{<<"Custom-Channel-Vars">>, kz_json:from_list(ExportProps)}]
                                    ),
                   ([K, _] = string:tokens(binary_to_list(KV), "=")) =/= 'undefined'
             ],
    case ["unset:" ++ K
          || KV <- lists:foldr(fun ecallmgr_fs_xml:kazoo_var_to_fs_var/2, [], kz_json:to_proplist(JObj))
                 ,not lists:member(begin [K, _] = string:tokens(binary_to_list(KV), "="), K end, Export)]
    of
        [] -> "";
        Unset ->
            [string:join(Unset, "^")
            ,maybe_fix_ignore_early_media(Export)
            ,maybe_fix_group_confirm(Export)
            ,maybe_fix_fs_auto_answer_bug(Export)
            ,maybe_fix_caller_id(Export, JObj)
            ,"^"
            ]
    end.

-spec maybe_fix_ignore_early_media(kz_term:strings()) -> string().
maybe_fix_ignore_early_media(Export) ->
    case lists:member("ignore_early_media", Export) of
        'true' -> "";
        'false' -> "^unset:ignore_early_media"
    end.

-spec maybe_fix_group_confirm(kz_term:strings()) -> string().
maybe_fix_group_confirm(Export) ->
    case lists:member("group_confirm_key", Export) of
        'true' -> "";
        'false' -> "^unset:group_confirm_key^unset:group_confirm_cancel_timeout^unset:group_confirm_file"
    end.

-spec maybe_fix_fs_auto_answer_bug(kz_term:strings()) -> string().
maybe_fix_fs_auto_answer_bug(Export) ->
    case lists:member("sip_auto_answer", Export) of
        'true' -> "";
        'false' ->
            "^unset:sip_h_Call-Info^unset:sip_h_Alert-Info^unset:alert_info^unset:sip_invite_params^set:sip_auto_answer=false"
    end.

-spec maybe_fix_caller_id(kz_term:strings(), kz_json:object()) -> string().
maybe_fix_caller_id(Export, JObj) ->
    Fix = [
           {lists:member("origination_callee_id_name", Export), kz_json:get_value(<<"Outbound-Callee-ID-Name">>, JObj), "origination_caller_id_name"}
          ,{lists:member("origination_callee_id_number", Export), kz_json:get_value(<<"Outbound-Callee-ID-Number">>, JObj), "origination_caller_id_number"}
          ],
    string:join([ "^set:" ++ Key ++ "=" ++ erlang:binary_to_list(Value) || {IsTrue, Value, Key} <- Fix, IsTrue ], ":").

-spec publish_error(kz_term:ne_binary(), created_uuid() | kz_term:api_binary(), kz_json:object(), kz_term:api_binary()) -> 'ok'.
publish_error(_, _, _, 'undefined') -> 'ok';
publish_error(Error, {_, UUID}, Request, ServerId) ->
    publish_error(Error, UUID, Request, ServerId);
publish_error(Error, UUID, Request, ServerId) ->
    lager:debug("originate error: ~s", [Error]),
    E = [{<<"Msg-ID">>, kz_api:msg_id(Request)}
        ,{<<"Call-ID">>, UUID}
        ,{<<"Request">>, Request}
        ,{<<"Error-Message">>, cleanup_error(Error)}
         | kz_api:default_headers(<<"error">>, <<"originate_resp">>, ?APP_NAME, ?APP_VERSION)
        ],
    kz_api:publish_error(ServerId, props:filter_undefined(E)).

-spec cleanup_error(kz_term:ne_binary()) -> kz_term:ne_binary().
cleanup_error(<<"-ERR ", E/binary>>) -> E;
cleanup_error(E) -> E.

-spec publish_originate_ready(kz_term:ne_binary(), created_uuid() | kz_term:ne_binary(), kz_json:object(), kz_term:api_binary(), kz_term:api_binary()) -> 'ok'.
publish_originate_ready(CtrlQ, {_, UUID}, Request, Q, ServerId) ->
    publish_originate_ready(CtrlQ, UUID, Request, Q, ServerId);
publish_originate_ready(CtrlQ, UUID, Request, Q, ServerId) ->
    lager:debug("originate command is ready, waiting for originate_execute"),
    Props = [{<<"Msg-ID">>, kz_api:msg_id(Request, UUID)}
            ,{<<"Call-ID">>, UUID}
            ,{<<"Control-Queue">>, CtrlQ}
             | kz_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
            ],
    kapi_dialplan:publish_originate_ready(ServerId, Props).

-spec publish_originate_resp(kz_term:api_binary(), kz_json:object()) -> 'ok'.
publish_originate_resp('undefined', _) -> 'ok';
publish_originate_resp(ServerId, JObj) ->
    Resp = kz_json:set_values([{<<"Event-Category">>, <<"resource">>}
                              ,{<<"Event-Name">>, <<"originate_resp">>}
                              ]
                             ,JObj
                             ),
    kapi_resource:publish_originate_resp(ServerId, Resp).

-spec publish_originate_resp(kz_term:api_binary(), kz_json:object(), kz_term:ne_binary()) -> 'ok'.
publish_originate_resp('undefined', _JObj, _UUID) -> 'ok';
publish_originate_resp(ServerId, JObj, UUID) ->
    Resp = kz_json:set_values([{<<"Event-Category">>, <<"resource">>}
                              ,{<<"Application-Response">>, <<"SUCCESS">>}
                              ,{<<"Event-Name">>, <<"originate_resp">>}
                              ,{<<"Call-ID">>, UUID}
                              ]
                             ,JObj
                             ),
    kapi_resource:publish_originate_resp(ServerId, Resp).

-spec publish_originate_started(kz_term:api_binary(), kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary()) -> 'ok'.
publish_originate_started('undefined', _, _, _) -> 'ok';
publish_originate_started(ServerId, CallId, JObj, CtrlQ) ->
    Resp = kz_json:from_list(
             [{<<"Call-ID">>, CallId}
             ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
             ,{<<"Control-Queue">>, CtrlQ}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    kapi_resource:publish_originate_started(ServerId, Resp).

-spec publish_originate_uuid(kz_term:api_binary(), created_uuid() | kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary()) -> 'ok'.
publish_originate_uuid('undefined', _, _, _) -> 'ok';
publish_originate_uuid(ServerId, UUID, JObj, CtrlQueue) ->
    Resp = props:filter_undefined(
             [{<<"Outbound-Call-ID">>, UUID}
             ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
             ,{<<"Outbound-Call-Control-Queue">>, CtrlQueue}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    lager:debug("sent originate_uuid to ~s", [ServerId]),
    kapi_resource:publish_originate_uuid(ServerId, Resp).

-spec maybe_send_originate_uuid(created_uuid(), pid(), state()) -> 'ok'.
maybe_send_originate_uuid({_, UUID}, Pid, #state{server_id=ServerId
                                                ,originate_req=JObj
                                                }) ->
    CtlQ = gen_listener:queue_name(Pid),
    publish_originate_uuid(ServerId, UUID, JObj, CtlQ).

-spec find_originate_timeout(kz_json:object()) -> pos_integer().
find_originate_timeout(JObj) ->
    OTimeout = case kz_json:get_integer_value(<<"Timeout">>, JObj) of
                   'undefined' -> 10;
                   LT when LT > 0 -> LT;
                   _ -> 10
               end,
    find_max_endpoint_timeout(kz_json:get_list_value(<<"Endpoints">>, JObj, [])
                             ,OTimeout
                             ).

-spec find_max_endpoint_timeout(kz_json:objects(), pos_integer()) -> pos_integer().
find_max_endpoint_timeout([], T) -> T;
find_max_endpoint_timeout([EP|EPs], T) ->
    case kz_json:get_integer_value(<<"Endpoint-Timeout">>, EP) of
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
                            ,controller_q=ControllerQ
                            ,server_id=ServerId
                            ,fetch_id=FetchId
                            ,control_pid='undefined'
                            }=State) ->
    Ctx = #{node => Node
           ,call_id => Id
           ,fetch_id => FetchId
           ,controller_q => ControllerQ
           ,initial_ccvs => kz_json:new()
           },
    case ecallmgr_call_sup:start_control_process(Ctx) of
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
    case kz_json:is_true(<<"Start-Control-Process">>, JObj, 'true')
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

-spec update_endpoint(kz_json:object(), state()) -> kz_json:object().
update_endpoint(Endpoint, #state{node=Node
                                ,originate_req=JObj
                                ,uuid=GlobalUUID
                                }=State) ->
    {_, Id} = UUID =
        case kz_json:get_value(<<"Outbound-Call-ID">>, Endpoint) of
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

    EP = kz_json:set_values([{<<"origination_uuid">>, Id}
                            ], Endpoint),
    fix_hold_media(EP).

-spec uuid_matches(created_uuid(), created_uuid()) -> boolean().
uuid_matches({_, UUID}, {_, UUID}) -> 'true';
uuid_matches(_, _) -> 'false'.

-spec fix_hold_media(kz_json:object()) -> kz_json:object().
fix_hold_media(Endpoint) ->
    put('hold_media', kz_json:get_value(<<"Hold-Media">>, Endpoint)),
    kz_json:delete_key(<<"Hold-Media">>, Endpoint).

-spec should_update_uuid(kz_term:api_binary(), kzd_freeswitch:data()) -> boolean().
should_update_uuid(OldUUID, FSJObj) ->
    case kzd_freeswitch:event_subclass(FSJObj, kzd_freeswitch:event_name(FSJObj)) of
        <<"loopback::bowout">> ->
            lager:debug("bowout detected with ~s, old uuid is ~s"
                       ,[kzd_freeswitch:resigning_id(FSJObj), OldUUID]
                       ),
            kzd_freeswitch:resigning_id(FSJObj) =:= OldUUID;
        _ -> 'false'
    end.

-spec handle_fs_event(kzd_freeswitch:data(), 'undefined' | created_uuid()) -> 'undefined' | created_uuid().
handle_fs_event(FSJObj, 'undefined') ->
    case should_update_uuid('undefined', FSJObj) of
        'false' -> 'undefined';
        'true' ->
            NewUUID = kzd_freeswitch:acquired_id(FSJObj),
            _ = update_uuid('undefined', NewUUID),
            {'api', NewUUID}
    end;
handle_fs_event(FSJObj, {_, OldUUID}=UUID) ->
    case should_update_uuid(OldUUID, FSJObj) of
        'false' -> UUID;
        'true' ->
            NewUUID = kzd_freeswitch:acquired_id(FSJObj),
            _ = update_uuid(OldUUID, NewUUID),
            {'api', NewUUID}
    end.
