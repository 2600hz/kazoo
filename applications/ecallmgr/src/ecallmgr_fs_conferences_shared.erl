%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_conferences_shared).

-behaviour(gen_listener).

-export([start_link/0
        ,handle_dial_req/2
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("ecallmgr.hrl").

-define(RESPONDERS, [{{?MODULE, 'handle_dial_req'}
                     ,[{<<"conference">>, <<"command">>}]
                     }
                    ]).
-define(BINDINGS, [{'conference', [{'restrict_to', [{'command', kz_config:zone('binary')}]}
                                  ,'federate'
                                  ]}
                  ]).
-define(QUEUE_NAME, <<?MODULE_STRING>>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

-define(LB_ALEG_PREFIX, "lb-aleg-").


-type state() :: 'ok'.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_listener:start_link({'local', ?MODULE}, ?MODULE,
                            [{'responders', ?RESPONDERS}
                            ,{'bindings', ?BINDINGS}
                            ,{'queue_name', ?QUEUE_NAME}
                            ,{'queue_options', ?QUEUE_OPTIONS}
                            ,{'consume_options', ?CONSUME_OPTIONS}
                            ], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_util:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    {'ok', 'ok'}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Req, _From, State) ->
    lager:debug("unhandled call from ~p: ~p", [_From, _Req]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast(_Req, State) ->
    lager:debug("unhandled cast: ~p", [_Req]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Msg, State) ->
    lager:debug("unhandled msg: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
    {'reply', []}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("conferences listener going down: ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) -> {'ok', State}.

-spec handle_dial_req(kapi_conference:doc(), kz_term:proplist()) -> 'ok'.
handle_dial_req(JObj, _Props) ->
    'true' = kapi_conference:dial_v(JObj),
    ConferenceId = kz_json:get_ne_binary_value(<<"Conference-ID">>, JObj),
    case ecallmgr_fs_conferences:node(ConferenceId) of
        {'error', 'not_found'} ->
            maybe_start_conference(JObj, ConferenceId);
        {'ok', ConferenceNode} ->
            maybe_exec_dial(ConferenceNode, ConferenceId, JObj)
    end,
    lager:debug("finished dialing").

-spec maybe_exec_dial(atom(), kz_term:ne_binary(), kapi_conference:doc()) -> 'ok'.
maybe_exec_dial(ConferenceNode, ConferenceId, JObj) ->
    {Loopbacks, Endpoints} = lists:splitwith(fun is_loopback/1, kz_json:get_list_value(<<"Endpoints">>, JObj, [])),

    maybe_exec_dial(ConferenceNode, ConferenceId, JObj, Endpoints, Loopbacks).

-spec is_loopback(kz_json:object()) -> boolean().
is_loopback(Endpoint) ->
    <<"loopback">> =:= kz_json:get_ne_binary_value(<<"Invite-Format">>, Endpoint).

-spec maybe_exec_dial(atom(), kz_term:ne_binary(), kapi_conference:doc(), kz_json:objects(), kz_json:objects()) -> 'ok'.
maybe_exec_dial(ConferenceNode, ConferenceId, JObj, Endpoints, Loopbacks) ->
    lager:info("conference ~s is running on ~s, dialing out", [ConferenceId, ConferenceNode]),
    _ = (catch gproc:reg({'p', 'l', ?FS_EVENT_REG_MSG(ConferenceNode, <<"conference::maintenance">>)})),

    EPResps = exec_endpoints(ConferenceNode, ConferenceId, JObj, Endpoints),
    LBResps = exec_loopbacks(ConferenceNode, ConferenceId, JObj, Loopbacks),

    handle_responses(ConferenceNode, JObj, EPResps ++ LBResps).

-type exec_response() :: {kz_term:ne_binary(), {'ok' | 'error', kz_term:proplist()}}.
-type exec_responses() :: [exec_response()].
-spec exec_endpoints(atom(), kz_term:ne_binary(), kapi_conference:doc(), kz_json:objects()) ->
                            exec_responses().
exec_endpoints(_ConferenceNode, _ConferenceId, _JObj, []) ->
    lager:debug("no endpoints to dial out to"),
    [];
exec_endpoints(ConferenceNode, ConferenceId, JObj, Endpoints) ->
    {_, _, _, Resps} =
        lists:foldl(fun exec_endpoint/2
                   ,{ConferenceNode, ConferenceId, JObj, []}
                   ,Endpoints
                   ),
    Resps.

-type exec_acc() :: {atom(), kz_term:ne_binary(), kapi_conference:doc(), exec_responses()}.

update_endpoint(Endpoint, EndpointCallId) ->
    Updates = [{fun kz_json:insert_value/3, <<"Outbound-Call-ID">>, EndpointCallId}
              ,{fun kz_json:set_value/3, [<<"Custom-Channel-Vars">>, <<"Ecallmgr-Node">>], node()}
              ],
    lists:foldl(fun({F, K, V}, JObj) -> F(K, V, JObj) end, Endpoint, Updates).

-spec exec_endpoint(kz_json:object(), exec_acc()) -> exec_acc().
exec_endpoint(Endpoint, {ConferenceNode, ConferenceId, JObj, Resps}) ->
    EndpointCallId = kz_json:find(<<"Outbound-Call-ID">>, [Endpoint, JObj], kz_binary:rand_hex(16)),
    EndpointId = kz_json:get_first_defined([<<"Route">>
                                           ,<<"Endpoint-ID">>
                                           ,[<<"Custom-Channel-Vars">>, <<"Authorizing-ID">>]
                                           ]
                                          ,Endpoint
                                          ),
    lager:debug("endpoint ~s(~s)", [EndpointId, EndpointCallId]),
    register_for_events(ConferenceNode, EndpointCallId),

    try ecallmgr_conference_command:dial(ConferenceNode
                                        ,ConferenceId
                                        ,JObj
                                        ,update_endpoint(Endpoint, EndpointCallId)
                                        )
    of
        {'ok', _Resp} ->
            lager:info("starting dial resulted in ~s", [_Resp]),
            {ConferenceNode, ConferenceId, JObj, [{EndpointCallId, success_resp(EndpointId)} | Resps]};
        _E ->
            lager:info("failed to exec: ~p", [_E]),
            {ConferenceNode, ConferenceId, JObj, [{EndpointCallId, error_resp(EndpointId, <<"unknown failure">>)} | Resps]}
    catch
        'throw':{'msg', E} ->
            lager:info("failed to exec: ~p", [E]),
            {ConferenceNode, ConferenceId, JObj, [{'undefined', error_resp(EndpointId, E)} | Resps]};
        'throw':Msg when is_binary(Msg) ->
            lager:info("failed to exec: ~s", [Msg]),
            {ConferenceNode, ConferenceId, JObj, [{'undefined', error_resp(EndpointId, Msg)} | Resps]}
    end.

-spec exec_loopbacks(atom(), kz_term:ne_binary(), kapi_conference:doc(), kz_json:objects()) ->
                            exec_responses().
exec_loopbacks(_ConferenceNode, _ConferenceId, _JObj, []) ->
    lager:debug("no loopbacks to dial out to"),
    [];
exec_loopbacks(ConferenceNode, ConferenceId, JObj, Loopbacks) ->
    {_, _, _, Resps} =
        lists:foldl(fun exec_loopback/2
                   ,{ConferenceNode, ConferenceId, JObj, []}
                   ,Loopbacks
                   ),
    Resps.

-spec exec_loopback(kz_json:object(), exec_acc()) -> exec_acc().
exec_loopback(Loopback, {ConferenceNode, ConferenceId, JObj, Resps}) ->
    {OutboundId, LoopbackId} =
        case kz_json:find(<<"Outbound-Call-ID">>, [Loopback, JObj]) of
            'undefined' ->
                {'undefined', <<?LB_ALEG_PREFIX, (kz_binary:rand_hex(12))/binary>>};
            OutboundCallId ->
                {OutboundCallId, <<?LB_ALEG_PREFIX, OutboundCallId/binary>>}
        end,
    Endpoint = kz_json:set_values([{<<"Outbound-Call-ID">>, LoopbackId}
                                  ,{[<<"Custom-Channel-Vars">>, <<"Outbound-Call-ID">>], OutboundId}
                                  ]
                                 ,Loopback
                                 ),
    exec_endpoint(Endpoint, {ConferenceNode, ConferenceId, kz_json:delete_key(<<"Outbound-Call-ID">>, JObj), Resps}).

-spec success_resp(kz_term:ne_binary()) -> {'ok', kz_term:proplist()}.
success_resp(EndpointId) ->
    {'ok'
    ,[{<<"Message">>, <<"dialing endpoints">>}
     ,{<<"Status">>, <<"success">>}
     ,{<<"Endpoint-ID">>, EndpointId}
     ]
    }.

-spec error_resp(kz_term:ne_binary(), kz_term:ne_binary()) -> {'error', kz_term:proplist()}.
error_resp(EndpointId, Error) ->
    {'error'
    ,[{<<"Status">>, <<"error">>}
     ,{<<"Message">>, Error}
     ,{<<"Endpoint-ID">>, EndpointId}
     ]
    }.

-spec handle_responses(atom(), kapi_conference:doc(), exec_responses()) -> 'ok'.
handle_responses(ConferenceNode, JObj, Responses) ->
    BaseResponses = [kz_json:from_list(handle_response(ConferenceNode, JObj, Response))
                     || Response <- Responses
                    ],

    publish_resp(JObj, BaseResponses).

-spec handle_response(atom(), kapi_conference:doc(), exec_response()) -> kz_term:proplist().
handle_response(_ConferenceNode, _JObj, {_CallId, {'error', Resp}}) ->
    Resp;
handle_response(ConferenceNode, JObj, {LoopbackCallId, {'ok', Resp}}) ->
    BuiltResp = handle_call_startup(ConferenceNode, JObj, LoopbackCallId, Resp),
    props:insert_value(<<"Call-ID">>, LoopbackCallId, BuiltResp).

-record(outbound_dial, {loopback_a :: kz_term:ne_binary()
                       ,loopback_b :: kz_term:api_ne_binary()
                       ,b_leg :: kz_term:api_ne_binary()
                       ,dial_resp :: kz_term:api_ne_binary()
                       ,channel_props = [] :: kz_term:proplist()
                       }
       ).
-type outbound_dial() :: #outbound_dial{}.

-spec handle_call_startup(atom(), kapi_conference:doc(), kz_term:ne_binary(), kz_term:proplist()) ->
                                 kz_term:proplist().
handle_call_startup(ConferenceNode, JObj, LoopbackCallId, Resp) ->
    case wait_for_bowout(#outbound_dial{loopback_a=LoopbackCallId}
                        ,kz_json:get_integer_value(<<"Timeout">>, JObj) * ?MILLISECONDS_IN_SECOND
                        )
    of
        {'ok', #outbound_dial{b_leg=CallId
                             ,dial_resp=DialResp
                             ,channel_props=ChannelProps
                             }=OutboundDial
        }->
            lager:debug("finished waiting for ~s, now ~s", [LoopbackCallId, CallId]),
            add_participant(JObj
                           ,CallId
                           ,start_call_handlers(ConferenceNode, JObj, OutboundDial)
                           ,ChannelProps
                           ),
            props:set_values([{<<"Message">>, DialResp}
                             ,{<<"Call-ID">>, CallId}
                             ]
                            ,Resp
                            );
        {'error', 'timeout'} ->
            props:insert_value(<<"Message">>, <<"dialing timed out before a call could be established">>, Resp);
        {'error', HangupCause, E} ->
            props:set_values([{<<"Status">>, <<"error">>}
                             ,{<<"Hangup-Cause">>, HangupCause}
                             ,{<<"Message">>, E}
                             ,{<<"Call-ID">>, 'null'}
                             ]
                            ,Resp
                            )
    end.

-type bowout_return() :: {'ok', outbound_dial()} |
                         {'error', 'timeout'} |
                         {'error', kz_term:ne_binary(), kz_term:ne_binary()}.

-spec wait_for_bowout(outbound_dial(), pos_integer()) -> bowout_return().
wait_for_bowout(#outbound_dial{loopback_a=LoopbackALeg
                              ,loopback_b=LoopbackBLeg
                              }=OutboundDial
               ,Timeout) ->
    Start = kz_time:now_s(),
    receive
        {'event', [LoopbackALeg | Props]} ->
            handle_call_event(OutboundDial, Timeout, Start, Props);
        {'event', [LoopbackBLeg | Props]} ->
            handle_call_event(OutboundDial, Timeout, Start, Props);
        ?LOOPBACK_BOWOUT_MSG(_Node, Props) when is_list(Props) ->
            handle_bowout(OutboundDial, Timeout, Start, Props)
    after Timeout ->
            lager:info("timed out waiting for ~s", [LoopbackALeg]),
            {'error', 'timeout'}
    end.

-spec handle_call_event(outbound_dial(), pos_integer(), pos_integer(), kz_term:proplist()) ->
                               bowout_return().
handle_call_event(#outbound_dial{loopback_a=LoopbackALeg
                                ,loopback_b=LoopbackBLeg
                                }=OutboundDial
                 ,Timeout, Start, Props
                 ) ->
    case {kzd_freeswitch:call_id(Props)
         ,kzd_freeswitch:event_name(Props)
         }
    of
        {LoopbackALeg, <<"CHANNEL_DESTROY">>} ->
            handle_loopback_destroy(OutboundDial, kzd_freeswitch:hangup_cause(Props));
        {LoopbackBLeg, <<"CHANNEL_DESTROY">>} ->
            handle_loopback_destroy(OutboundDial, kzd_freeswitch:hangup_cause(Props));
        {LoopbackALeg, <<"CHANNEL_CREATE">>} ->
            handle_create(OutboundDial, Timeout, Start, Props);
        {_CallId, _Evt} ->
            wait_for_bowout(OutboundDial, kz_time:decr_timeout(Timeout, Start))
    end.

-spec handle_create(outbound_dial(), pos_integer(), pos_integer(), kz_term:proplist()) ->
                           bowout_return().
handle_create(#outbound_dial{loopback_a = <<?LB_ALEG_PREFIX, _/binary>>
                            ,channel_props=ChannelProps
                            }=OutboundDial
             ,Timeout, Start, Props
             ) ->
    case {kzd_freeswitch:other_leg_call_id(Props)
         ,kzd_freeswitch:loopback_other_leg(Props)
         }
    of
        {'undefined', 'undefined'} ->
            wait_for_bowout(OutboundDial#outbound_dial{channel_props=props:insert_values(Props, ChannelProps)}
                           ,kz_time:decr_timeout(Timeout, Start)
                           );
        {'undefined', LoopbackBLeg} ->
            lager:debug("loopback bleg ~s started", [LoopbackBLeg]),
            register_for_events(kzd_freeswitch:switch_nodename(Props), LoopbackBLeg),
            maybe_update_ecallmgr_node(LoopbackBLeg),
            wait_for_bowout(OutboundDial#outbound_dial{loopback_b=LoopbackBLeg
                                                      ,channel_props=props:insert_values(Props, ChannelProps)
                                                      }
                           ,kz_time:decr_timeout(Timeout, Start)
                           );
        {LoopbackBLeg, _} ->
            lager:debug("loopback bleg ~s started", [LoopbackBLeg]),
            register_for_events(kzd_freeswitch:switch_nodename(Props), LoopbackBLeg),
            wait_for_bowout(OutboundDial#outbound_dial{channel_props=props:insert_values(Props, ChannelProps)}
                           ,kz_time:decr_timeout(Timeout, Start)
                           )
    end;
handle_create(#outbound_dial{loopback_a=ALeg
                            ,channel_props=ChannelProps
                            }=OutboundDial, _Timeout, _Start, Props) ->
    lager:debug("dial started to ~s", [ALeg]),
    {'ok'
    ,OutboundDial#outbound_dial{b_leg=ALeg
                               ,channel_props=props:insert_values(Props, ChannelProps)
                               ,dial_resp = <<"dial resulted in call id ", ALeg/binary>>
                               }
    }.

handle_loopback_destroy(#outbound_dial{loopback_a=_LoopbackALeg}, <<"NORMAL_UNSPECIFIED">>) ->
    lager:debug("loopback a-leg ~s went down", [_LoopbackALeg]);
handle_loopback_destroy(#outbound_dial{loopback_a=_LoopbackALeg}, HangupCause) ->
    lager:info("~s went down with ~s", [_LoopbackALeg, HangupCause]),
    {'error', HangupCause, <<"failed to start call: ", HangupCause/binary>>}.

handle_bowout(#outbound_dial{loopback_a=LoopbackALeg
                            ,channel_props=ChannelProps
                            }=OutboundDial
             ,Timeout, Start, Props
             ) ->
    case {props:get_value(?RESIGNING_UUID, Props)
         ,props:get_value(?ACQUIRED_UUID, Props)
         }
    of
        {LoopbackALeg, LoopbackALeg} ->
            lager:debug("call id after bowout remains the same"),
            {'ok'
            ,OutboundDial#outbound_dial{b_leg=LoopbackALeg
                                       ,channel_props=props:insert_values(Props, ChannelProps)
                                       ,dial_resp = <<"dial resulted in call id ", LoopbackALeg/binary>>
                                       }
            };
        {LoopbackALeg, AcquiringUUID} when AcquiringUUID =/= 'undefined' ->
            lager:debug("~s acquired as ~s", [LoopbackALeg, AcquiringUUID]),
            {'ok'
            ,OutboundDial#outbound_dial{b_leg=AcquiringUUID
                                       ,channel_props=props:insert_values(Props, ChannelProps)
                                       ,dial_resp = <<"dial resulted in call id ", LoopbackALeg/binary>>
                                       }
            };
        {_UUID, _AcquiringUUID} ->
            lager:debug("failed to update after bowout, r: ~s a: ~s", [_UUID, _AcquiringUUID]),
            wait_for_bowout(OutboundDial, kz_time:decr_timeout(Timeout, Start))
    end.

-spec register_for_events(atom(), kz_term:ne_binary()) -> 'ok'.
register_for_events(ConferenceNode, EndpointCallId) ->
    _ = [(catch gproc:reg({'p', 'l', ?FS_CALL_EVENT_REG_MSG(ConferenceNode, EndpointCallId)}))
        ,(catch gproc:reg({'p', 'l', ?FS_CALL_EVENTS_PROCESS_REG(ConferenceNode, EndpointCallId)}))
        ,(catch gproc:reg({'p', 'l', ?LOOPBACK_BOWOUT_REG(EndpointCallId)}))
        ],
    'ok'.

-spec start_call_handlers(atom(), kapi_conference:doc(), outbound_dial()) -> kz_term:api_ne_binary().
start_call_handlers(Node, JObj, #outbound_dial{loopback_b=LoopbackB, b_leg=CallId}) ->
    CCVs = kz_json:new(),
    FetchId = kz_api:msg_id(JObj),

    FirstValid = case LoopbackB of
                     'undefined' -> CallId;
                     _ -> LoopbackB
                 end,
    ecallmgr_call_control:publish_usurp(FirstValid, FetchId, node()),
    maybe_update_ecallmgr_node(FirstValid),

    _ = kz_util:spawn(fun ecallmgr_call_sup:start_event_process/2, [Node, CallId]),
    {'ok', CtlPid} = ecallmgr_call_sup:start_control_process(Node, CallId, FetchId, 'undefined', CCVs),

    get_control_queue(CtlPid).

maybe_update_ecallmgr_node('undefined') -> 'ok';
maybe_update_ecallmgr_node(Leg) ->
    case ecallmgr_fs_channel:fetch(Leg, 'record') of
        {'ok', #channel{node=Node}} ->
            ecallmgr_fs_command:export(Node, Leg, [{<<"Ecallmgr-Node">>, node()}]),
            lager:debug("exported ecallmgr node to ~s (~s)", [Node, Leg]);
        {'error', 'not_found'} ->
            lager:debug("leg ~s not found, skipping", [Leg])
    end.

-spec get_control_queue(pid()) -> kz_term:api_ne_binary().
get_control_queue(CtlPid) ->
    try erlang:is_process_alive(CtlPid)
             andalso ecallmgr_call_control:queue_name(CtlPid)
    of
        'false' ->
            lager:debug("control proc has disappeared"),
            'undefined';
        'undefined' ->
            lager:debug("no control queue yet for ~p", [CtlPid]),
            timer:sleep(?MILLISECONDS_IN_SECOND),
            get_control_queue(CtlPid);
        CtlQueue ->
            lager:debug("got control queue ~s", [CtlQueue]),
            CtlQueue
    catch
        'exit':{'timeout', {_M, _F, _A}} ->
            lager:info("control proc ~p timed out getting control queue"),
            'undefined';
        _E:_R ->
            ST = erlang:get_stacktrace(),
            lager:debug("failed to get queue ~s: ~p", [_E, _R]),
            kz_util:log_stacktrace(ST),
            timer:sleep(?MILLISECONDS_IN_SECOND),
            get_control_queue(CtlPid)
    end.

-spec add_participant(kapi_conference:doc(), kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:proplist()) -> 'ok'.
add_participant(_JObj, _CallId, 'undefined', _ChannelProps) ->
    lager:info("not adding participant, no control queue");
add_participant(JObj, CallId, ControlQueue, ChannelProps) ->
    ReqJObj = kz_json:set_values([{<<"Conference-ID">>, kz_json:get_ne_binary_value(<<"Conference-ID">>, JObj)}
                                 ,{<<"Call-ID">>, CallId}
                                 ,{<<"Control-Queue">>, ControlQueue}
                                 ,{<<"Account-ID">>, kz_json:get_ne_binary_value(<<"Account-ID">>, JObj)}
                                 ,{<<"Participant-Flags">>, kz_json:get_list_value(<<"Participant-Flags">>, JObj)}
                                 ,{<<"Profile-Name">>, kz_json:get_ne_binary_value(<<"Profile-Name">>, JObj)}
                                 ,{<<"Caller-ID-Name">>, kz_json:get_ne_binary_value(<<"Caller-ID-Name">>, JObj)}
                                 ,{<<"Caller-ID-Number">>, kz_json:get_ne_binary_value(<<"Caller-ID-Number">>, JObj)}
                                  | kz_api:default_headers(<<"conference">>, <<"add_participant">>, ?APP_NAME, ?APP_VERSION)
                                 ]
                                ,ecallmgr_call_events:to_json(ChannelProps)
                                ),
    Req = kz_json:merge(ReqJObj, ecallmgr_fs_channel:to_api_json(CallId)),

    lager:debug("adding participant for ~s: ~p", [CallId, Req]),
    kz_amqp_worker:cast(Req
                       ,fun(P) ->
                                kapi_conference:publish_add_participant(kz_config:zone('binary'), P)
                        end
                       ).

-spec publish_resp(kapi_conference:doc(), kz_json:objects()) -> 'ok'.
publish_resp(JObj, BaseResps) ->
    Resp = [{<<"Msg-ID">>, kz_api:msg_id(JObj)}
           ,{<<"Endpoint-Responses">>, BaseResps}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kz_amqp_worker:cast(Resp
                       ,fun(P) -> kapi_conference:publish_dial_resp(kz_api:server_id(JObj), P) end
                       ).

-spec maybe_start_conference(kapi_conference:doc(), kz_term:ne_binary()) -> 'ok'.
maybe_start_conference(JObj, ConferenceId) ->
    lager:info("conference ~s is not running yet", [ConferenceId]),
    case find_media_server(kz_json:get_ne_binary_value(<<"Target-Call-ID">>, JObj), kz_api:node(JObj)) of
        'undefined' -> lager:info("no node found for the dial command, ignoring");
        MediaServer ->
            lager:info("starting conference ~s on ~s and dialing out", [ConferenceId, MediaServer]),
            maybe_exec_dial(MediaServer, ConferenceId, JObj)
    end.

-spec find_media_server(kz_term:api_ne_binary(), kz_term:ne_binary()) -> atom().
find_media_server('undefined', IssuerNode) ->
    IssuerNodeInfo = kz_nodes:node_to_json(IssuerNode),
    MyZone = kz_config:zone('binary'),

    case kz_json:get_ne_binary_value(<<"zone">>, IssuerNodeInfo) of
        MyZone -> choose_random_media_server();
        _IssuerZone ->
            lager:info("issuer ~s is in zone ~s, ignoring request", [IssuerNode, _IssuerZone]),
            'undefined'
    end;
find_media_server(TargetCallId, IssuerNode) ->
    case ecallmgr_fs_channel:node(TargetCallId) of
        {'ok', Node} -> Node;
        {'error', 'not_found'} ->
            lager:info("failed to find node of target call-id ~s, querying cluster", [TargetCallId]),
            case query_cluster_for_call(TargetCallId) of
                {'ok', StatusJObjs} ->
                    find_media_server_from_statuses(TargetCallId, IssuerNode, StatusJObjs);
                _E ->
                    lager:info("failed to query for ~s: ~p", [TargetCallId, _E]),
                    find_media_server('undefined', IssuerNode)
            end
    end.

-spec find_media_server_from_statuses(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:objects()) -> atom().
find_media_server_from_statuses(TargetCallId, IssuerNode, []) ->
    lager:info("no one has record of ~s", [TargetCallId]),
    find_media_server('undefined', IssuerNode);
find_media_server_from_statuses(TargetCallId, IssuerNode, [Status|Statuses]) ->
    case kz_json:get_ne_binary_value([<<"Channels">>, TargetCallId, <<"Media-Node">>], Status) of
        'undefined' -> find_media_server_from_statuses(TargetCallId, IssuerNode, Statuses);
        MediaServer ->
            lager:info("found ~s on ~s", [TargetCallId, MediaServer]),
            case lists:filter(fun(MS) -> kz_term:to_binary(MS) =:= MediaServer end
                             ,ecallmgr_fs_nodes:connected()
                             )
            of
                [] ->
                    lager:info("media server ~s is not managed by us, not starting conference"
                              ,[MediaServer]
                              ),
                    'undefined';
                [MS] ->
                    lager:info("media server ~s is managed by us!", [MediaServer]),
                    MS
            end
    end.

-spec query_cluster_for_call(kz_term:ne_binary()) -> {'ok', kz_json:objects()} |
                                                     {'error', any()}.
query_cluster_for_call(CallId) ->
    Req = [{<<"Call-ID">>, CallId}
          ,{<<"Fields">>, <<"all">>}
          ,{<<"Active-Only">>, 'true'}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],

    kz_amqp_worker:call_collect(Req
                               ,fun kapi_call:publish_query_channels_req/1
                               ,{'ecallmgr', fun kapi_call:query_channels_resp_v/1}
                               ).

-spec choose_random_media_server() -> atom().
choose_random_media_server() ->
    [Server|_] = kz_term:shuffle_list(ecallmgr_fs_nodes:connected()),
    Server.
