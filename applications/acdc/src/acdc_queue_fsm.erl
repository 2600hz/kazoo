%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz INC
%%% @doc
%%% Controls how a queue process progresses a member_call
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_queue_fsm).
-behaviour(gen_fsm).

%% API
-export([start_link/3]).

%% Event injectors
-export([member_call/3
        ,member_connect_resp/2
        ,member_accepted/2
        ,member_connect_retry/2
        ,call_event/4
        ,refresh/2
        ,current_call/1
        ,status/1
        ,finish_member_call/1

         %% Accessors
        ,cdr_url/1
        ]).

%% State handlers
-export([ready/2, ready/3
        ,connect_req/2, connect_req/3
        ,connecting/2, connecting/3
        ]).

%% gen_fsm callbacks
-export([init/1
        ,handle_event/3
        ,handle_sync_event/4
        ,handle_info/3
        ,terminate/3
        ,code_change/4
        ]).

-include("acdc.hrl").

-define(SERVER, ?MODULE).

%% How long should we wait for a response to our member_connect_req
-define(COLLECT_RESP_TIMEOUT, kapps_config:get_integer(?CONFIG_CAT, <<"queue_collect_resp_timeout">>, 2000)).
-define(COLLECT_RESP_MESSAGE, 'collect_timer_expired').

%% How long will the caller wait in the call queue before being bounced out
-define(CONNECTION_TIMEOUT, 3600000).
-define(CONNECTION_TIMEOUT_MESSAGE, 'connection_timer_expired').

%% How long to ring the agent before trying the next agent
-define(AGENT_RING_TIMEOUT, 5).
-define(AGENT_RING_TIMEOUT_MESSAGE, 'agent_timer_expired').

-record(state, {queue_proc :: pid()
               ,manager_proc :: pid()
               ,connect_resps = [] :: kz_json:objects()
               ,collect_ref :: reference()
               ,account_id :: ne_binary()
               ,account_db :: ne_binary()
               ,queue_id :: ne_binary()

               ,timer_ref :: reference() % for tracking timers
               ,connection_timer_ref :: reference() % how long can a caller wait in the queue
               ,agent_ring_timer_ref :: reference() % how long to ring an agent before moving to the next

               ,member_call :: kapps_call:call()
               ,member_call_start :: non_neg_integer()
               ,member_call_winner :: api_object() %% who won the call

                                      %% Config options
               ,name :: ne_binary()
               ,connection_timeout :: pos_integer()
               ,agent_ring_timeout = 10 :: pos_integer() % how long to ring an agent before giving up
               ,max_queue_size = 0 :: integer() % restrict the number of the queued callers
               ,ring_simultaneously = 1 :: integer() % how many agents to try ringing at a time (first one wins)
               ,enter_when_empty = true :: boolean() % if a queue is agent-less, can the caller enter?
               ,agent_wrapup_time = 0 :: integer() % forced wrapup time for an agent after a call

               ,announce :: ne_binary() % media to play to customer when about to be connected to agent

               ,caller_exit_key :: ne_binary() % DTMF a caller can press to leave the queue
               ,record_caller = 'false' :: boolean() % record the caller
               ,recording_url :: api_binary() %% URL of where to POST recordings
               ,cdr_url :: api_binary() % optional URL to request for extra CDR data

               ,notifications :: api_object()
               }).
-type queue_fsm_state() :: #state{}.

-define(WSD_ID, {'file', <<(get('callid'))/binary, "_queue_fsm">>}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%% @end
%%--------------------------------------------------------------------
-spec start_link(pid(), pid(), kz_json:object()) -> startlink_ret().
start_link(MgrPid, ListenerPid, QueueJObj) ->
    gen_fsm:start_link(?SERVER, [MgrPid, ListenerPid, QueueJObj], []).

-spec refresh(pid(), kz_json:object()) -> 'ok'.
refresh(FSM, QueueJObj) ->
    gen_fsm:send_all_state_event(FSM, {'refresh', QueueJObj}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec member_call(pid(), kz_json:object(), gen_listener:basic_deliver()) -> 'ok'.
member_call(FSM, CallJObj, Delivery) ->
    gen_fsm:send_event(FSM, {'member_call', CallJObj, Delivery}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec member_connect_resp(pid(), kz_json:object()) -> 'ok'.
member_connect_resp(FSM, Resp) ->
    gen_fsm:send_event(FSM, {'agent_resp', Resp}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec member_accepted(pid(), kz_json:object()) -> 'ok'.
member_accepted(FSM, AcceptJObj) ->
    gen_fsm:send_event(FSM, {'accepted', AcceptJObj}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec member_connect_retry(pid(), kz_json:object()) -> 'ok'.
member_connect_retry(FSM, RetryJObj) ->
    gen_fsm:send_event(FSM, {'retry', RetryJObj}).

%%--------------------------------------------------------------------
%% @doc
%%   When a queue is processing a call, it will receive call events.
%%   Pass the call event to the FSM to see if action is needed (usually
%%   for hangup events).
%% @end
%%--------------------------------------------------------------------
-spec call_event(pid(), ne_binary(), ne_binary(), kz_json:object()) -> 'ok'.
call_event(FSM, <<"call_event">>, <<"CHANNEL_DESTROY">>, EvtJObj) ->
    gen_fsm:send_event(FSM, {'member_hungup', EvtJObj});
call_event(FSM, <<"call_event">>, <<"DTMF">>, EvtJObj) ->
    gen_fsm:send_event(FSM, {'dtmf_pressed', kz_json:get_value(<<"DTMF-Digit">>, EvtJObj)});
call_event(FSM, <<"call_event">>, <<"CHANNEL_BRIDGE">>, EvtJObj) ->
    gen_fsm:send_event(FSM, {'channel_bridged', EvtJObj});
call_event(_, _E, _N, _J) -> 'ok'.
%% lager:debug("unhandled event: ~s: ~s (~s)"
%%             ,[_E, _N, kz_json:get_value(<<"Application-Name">>, _J)]
%%            ).

-spec finish_member_call(pid()) -> 'ok'.
finish_member_call(FSM) ->
    gen_fsm:send_event(FSM, {'member_finished'}).

-spec current_call(pid()) -> api_object().
current_call(FSM) ->
    gen_fsm:sync_send_event(FSM, 'current_call').

-spec status(pid()) -> kz_proplist().
status(FSM) ->
    gen_fsm:sync_send_event(FSM, 'status').

-spec cdr_url(pid()) -> api_binary().
cdr_url(FSM) ->
    gen_fsm:sync_send_all_state_event(FSM, 'cdr_url').

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
-spec init(list()) -> {'ok', atom(), queue_fsm_state()}.
init([MgrPid, ListenerPid, QueueJObj]) ->
    QueueId = kz_doc:id(QueueJObj),
    kz_util:put_callid(<<"fsm_", QueueId/binary, "_", (kz_util:to_binary(self()))/binary>>),

    webseq:start(?WSD_ID),
    webseq:reg_who(?WSD_ID, self(), iolist_to_binary([<<"qFSM">>, pid_to_list(self())])),

    {'ok'
    ,'ready'
    ,#state{queue_proc = ListenerPid
           ,manager_proc = MgrPid
           ,account_id = kz_doc:account_id(QueueJObj)
           ,account_db = kz_doc:account_db(QueueJObj)
           ,queue_id = QueueId

           ,name = kz_json:get_value(<<"name">>, QueueJObj)
           ,connection_timeout = connection_timeout(kz_json:get_integer_value(<<"connection_timeout">>, QueueJObj))
           ,agent_ring_timeout = agent_ring_timeout(kz_json:get_integer_value(<<"agent_ring_timeout">>, QueueJObj))
           ,max_queue_size = kz_json:get_integer_value(<<"max_queue_size">>, QueueJObj)
           ,ring_simultaneously = kz_json:get_value(<<"ring_simultaneously">>, QueueJObj)
           ,enter_when_empty = kz_json:is_true(<<"enter_when_empty">>, QueueJObj, 'true')
           ,agent_wrapup_time = kz_json:get_integer_value(<<"agent_wrapup_time">>, QueueJObj)
           ,announce = kz_json:get_value(<<"announce">>, QueueJObj)
           ,caller_exit_key = kz_json:get_value(<<"caller_exit_key">>, QueueJObj, <<"#">>)
           ,record_caller = kz_json:is_true(<<"record_caller">>, QueueJObj, 'false')
           ,recording_url = kz_json:get_ne_value(<<"call_recording_url">>, QueueJObj)
           ,cdr_url = kz_json:get_ne_value(<<"cdr_url">>, QueueJObj)
           ,member_call = 'undefined'

           ,notifications = kz_json:get_value(<<"notifications">>, QueueJObj)
           }
    }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec ready(any(), queue_fsm_state()) -> handle_fsm_ret(queue_fsm_state()).
-spec ready(any(), any(), queue_fsm_state()) -> handle_fsm_ret(queue_fsm_state()).
ready({'member_call', CallJObj, Delivery}, #state{queue_proc=QueueSrv
                                                 ,manager_proc=MgrSrv
                                                 ,connection_timeout=ConnTimeout
                                                 ,connection_timer_ref=ConnRef
                                                 ,cdr_url=Url
                                                 }=State) ->
    Call = kapps_call:from_json(kz_json:get_value(<<"Call">>, CallJObj)),
    CallId = kapps_call:call_id(Call),
    kz_util:put_callid(CallId),

    case acdc_queue_manager:should_ignore_member_call(MgrSrv, Call, CallJObj) of
        'false' ->
            lager:debug("member call received: ~s", [CallId]),

            webseq:note(?WSD_ID, self(), 'right', [CallId, <<": member call">>]),
            webseq:evt(?WSD_ID, CallId, self(), <<"member call received">>),

            acdc_queue_listener:member_connect_req(QueueSrv, CallJObj, Delivery, Url),

            maybe_stop_timer(ConnRef), % stop the old one, maybe

            {'next_state', 'connect_req', State#state{collect_ref=start_collect_timer()
                                                     ,member_call=Call
                                                     ,member_call_start=kz_util:current_tstamp()
                                                     ,connection_timer_ref=start_connection_timer(ConnTimeout)
                                                     }};
        'true' ->
            lager:debug("queue mgr said to ignore this call: ~s", [CallId]),
            acdc_queue_listener:ignore_member_call(QueueSrv, Call, Delivery),
            {'next_state', 'ready', State}
    end;
ready({'agent_resp', _Resp}, State) ->
    lager:debug("someone jumped the gun, or was slow on the draw"),
    {'next_state', 'ready', State};
ready({'accepted', _AcceptJObj}, State) ->
    lager:debug("weird to receive an acceptance"),
    {'next_state', 'ready', State};
ready({'retry', _RetryJObj}, State) ->
    lager:debug("weird to receive a retry when we're just hanging here"),
    {'next_state', 'ready', State};
ready({'member_hungup', _CallEvt}, State) ->
    lager:debug("member hungup from previous call: ~p", [_CallEvt]),
    {'next_state', 'ready', State};
ready({'member_finished'}, State) ->
    lager:debug("member finished while in 'ready', ignore"),
    {'next_state', 'ready', State};
ready({'dtmf_pressed', _DTMF}, State) ->
    lager:debug("DTMF(~s) for old call", [_DTMF]),
    {'next_state', 'ready', State};

ready(_Event, State) ->
    lager:debug("unhandled event in ready: ~p", [_Event]),
    {'next_state', 'ready', State}.

ready('status', _, #state{cdr_url=Url
                         ,recording_url=RecordingUrl
                         }=State) ->
    {'reply', [{'state', <<"ready">>}
              ,{<<"cdr_url">>, Url}
              ,{<<"recording_url">>, RecordingUrl}
              ], 'ready', State};
ready('current_call', _, State) ->
    {'reply', 'undefined', 'ready', State}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec connect_req(any(), queue_fsm_state()) -> handle_fsm_ret(queue_fsm_state()).
-spec connect_req(any(), any(), queue_fsm_state()) -> handle_fsm_ret(queue_fsm_state()).
connect_req({'member_call', CallJObj, Delivery}, #state{queue_proc=Srv}=State) ->
    lager:debug("recv a member_call while processing a different member"),
    CallId = kz_json:get_value(<<"Call-ID">>, CallJObj),
    webseq:evt(?WSD_ID, CallId, self(), <<"member call recv while busy">>),
    acdc_queue_listener:cancel_member_call(Srv, CallJObj, Delivery),
    {'next_state', 'connect_req', State};

connect_req({'agent_resp', Resp}, #state{connect_resps=CRs
                                        ,manager_proc=MgrSrv
                                        }=State) ->
    Agents = acdc_queue_manager:current_agents(MgrSrv),
    Resps = [Resp | CRs],
    {NextState, State1} =
        case have_agents_responded(Resps, Agents) of
            'true' -> handle_agent_responses(State#state{connect_resps=Resps});
            'false' -> {'connect_req', State#state{connect_resps=Resps}}
        end,
    {'next_state', NextState, State1};

connect_req({'timeout', Ref, ?COLLECT_RESP_MESSAGE}, #state{collect_ref=Ref
                                                           ,connect_resps=[]
                                                           ,manager_proc=MgrSrv
                                                           ,member_call=Call
                                                           ,queue_proc=Srv
                                                           ,account_id=AccountId
                                                           ,queue_id=QueueId
                                                           }=State) ->
    maybe_stop_timer(Ref),
    case acdc_queue_manager:should_ignore_member_call(MgrSrv, Call, AccountId, QueueId) of
        'true' ->
            lager:debug("queue mgr said to ignore this call: ~s, not retrying agents", [kapps_call:call_id(Call)]),
            acdc_queue_listener:finish_member_call(Srv),
            {'next_state', 'ready', State};
        'false' ->
            maybe_connect_re_req(MgrSrv, Srv, State)
    end;

connect_req({'timeout', Ref, ?COLLECT_RESP_MESSAGE}, #state{collect_ref=Ref}=State) ->
    {NextState, State1} = handle_agent_responses(State),
    {'next_state', NextState, State1};

connect_req({'accepted', AcceptJObj}=Accept, #state{member_call=Call}=State) ->
    case accept_is_for_call(AcceptJObj, Call) of
        'true' ->
            lager:debug("received acceptance for call ~s: yet to send connect_req though", [kapps_call:call_id(Call)]),
            connecting(Accept, State);
        'false' ->
            lager:debug("received (and ignoring) acceptance payload"),
            {'next_state', 'connect_req', State}
    end;
connect_req({'retry', _RetryJObj}, State) ->
    lager:debug("recv retry response before win sent"),
    {'next_state', 'connect_req', State};

connect_req({'member_hungup', JObj}, #state{queue_proc=Srv
                                           ,member_call=Call
                                           ,account_id=AccountId
                                           ,queue_id=QueueId
                                           }=State) ->
    CallId = kapps_call:call_id(Call),
    case kz_json:get_value(<<"Call-ID">>, JObj) =:= CallId of
        'true' ->
            lager:debug("member hungup before we could assign an agent"),

            webseq:evt(?WSD_ID, self(), CallId, <<"member call finish - abandon">>),

            acdc_queue_listener:cancel_member_call(Srv, JObj),
            acdc_stats:call_abandoned(AccountId, QueueId, CallId, ?ABANDON_HANGUP),
            {'next_state', 'ready', clear_member_call(State), 'hibernate'};
        'false' ->
            lager:debug("hangup recv for ~s while processing ~s, ignoring", [kz_json:get_value(<<"Call-ID">>, JObj)
                                                                            ,CallId
                                                                            ]),
            {'next_state', 'connect_req', State}
    end;

connect_req({'member_finished'}, #state{member_call=Call}=State) ->
    case catch kapps_call:call_id(Call) of
        CallId when is_binary(CallId) ->
            lager:debug("member finished while in connect_req: ~s", [CallId]),
            webseq:evt(?WSD_ID, self(), CallId, <<"member call finished - forced">>);
        _E->
            lager:debug("member finished, but callid became ~p", [_E])
    end,
    {'next_state', 'ready', clear_member_call(State), 'hibernate'};

connect_req({'dtmf_pressed', DTMF}, #state{caller_exit_key=DTMF
                                          ,queue_proc=Srv
                                          ,account_id=AccountId
                                          ,queue_id=QueueId
                                          ,member_call=Call
                                          }=State) when is_binary(DTMF) ->
    lager:debug("member pressed the exit key (~s)", [DTMF]),
    CallId = kapps_call:call_id(Call),
    webseq:evt(?WSD_ID, self(), CallId, <<"member call finish - DTMF">>),

    acdc_queue_listener:exit_member_call(Srv),
    acdc_stats:call_abandoned(AccountId, QueueId, CallId, ?ABANDON_EXIT),
    {'next_state', 'ready', clear_member_call(State), 'hibernate'};

connect_req({'timeout', ConnRef, ?CONNECTION_TIMEOUT_MESSAGE}, #state{queue_proc=Srv
                                                                     ,connection_timer_ref=ConnRef
                                                                     ,account_id=AccountId
                                                                     ,queue_id=QueueId
                                                                     ,member_call=Call
                                                                     }=State) ->
    lager:debug("connection timeout occurred, bounce the caller out of the queue"),
    CallId = kapps_call:call_id(Call),
    webseq:evt(?WSD_ID, self(), CallId, <<"member call finish - timeout">>),

    acdc_queue_listener:timeout_member_call(Srv),
    acdc_stats:call_abandoned(AccountId, QueueId, CallId, ?ABANDON_TIMEOUT),
    {'next_state', 'ready', clear_member_call(State), 'hibernate'};

connect_req(_Event, State) ->
    lager:debug("unhandled event in connect_req: ~p", [_Event]),
    {'next_state', 'connect_req', State}.

connect_req('status', _, #state{member_call=Call
                               ,member_call_start=Start
                               ,connection_timer_ref=ConnRef
                               ,cdr_url=Url
                               ,recording_url=RecordingUrl
                               }=State) ->
    {'reply', [{<<"state">>, <<"connect_req">>}
              ,{<<"call_id">>, kapps_call:call_id(Call)}
              ,{<<"caller_id_name">>, kapps_call:caller_id_name(Call)}
              ,{<<"caller_id_number">>, kapps_call:caller_id_name(Call)}
              ,{<<"to">>, kapps_call:to_user(Call)}
              ,{<<"from">>, kapps_call:from_user(Call)}
              ,{<<"wait_left">>, elapsed(ConnRef)}
              ,{<<"wait_time">>, elapsed(Start)}
              ,{<<"cdr_url">>, Url}
              ,{<<"recording_url">>, RecordingUrl}
              ], 'connect_req', State};
connect_req('current_call', _, #state{member_call=Call
                                     ,member_call_start=Start
                                     ,connection_timer_ref=ConnRef
                                     }=State) ->
    {'reply', current_call(Call, ConnRef, Start), 'connect_req', State}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec connecting(any(), queue_fsm_state()) -> handle_fsm_ret(queue_fsm_state()).
-spec connecting(any(), any(), queue_fsm_state()) -> handle_fsm_ret(queue_fsm_state()).
connecting({'member_call', CallJObj, Delivery}, #state{queue_proc=Srv}=State) ->
    lager:debug("recv a member_call while connecting"),
    acdc_queue_listener:cancel_member_call(Srv, CallJObj, Delivery),
    {'next_state', 'connecting', State};

connecting({'agent_resp', _Resp}, State) ->
    lager:debug("agent resp must have just missed cutoff"),
    {'next_state', 'connecting', State};

connecting({'accepted', AcceptJObj}, #state{queue_proc=Srv
                                           ,member_call=Call
                                           ,account_id=AccountId
                                           ,queue_id=QueueId
                                           }=State) ->
    case accept_is_for_call(AcceptJObj, Call) of
        'true' ->
            lager:debug("recv acceptance from agent"),
            CallId = kapps_call:call_id(Call),
            webseq:evt(?WSD_ID, self(), CallId, <<"member call - agent acceptance">>),

            acdc_queue_listener:finish_member_call(Srv, AcceptJObj),
            acdc_stats:call_handled(AccountId, QueueId, CallId
                                   ,kz_json:get_value(<<"Agent-ID">>, AcceptJObj)
                                   ),
            {'next_state', 'ready', clear_member_call(State), 'hibernate'};
        'false' ->
            lager:debug("ignoring accepted message"),
            {'next_state', 'connecting', State}
    end;

connecting({'retry', RetryJObj}, #state{agent_ring_timer_ref=AgentRef
                                       ,collect_ref=CollectRef
                                       ,member_call_winner=Winner
                                       }=State) ->
    RetryProcId = kz_json:get_value(<<"Process-ID">>, RetryJObj),
    RetryAgentId = kz_json:get_value(<<"Agent-ID">>, RetryJObj),

    case {kz_json:get_value(<<"Agent-ID">>, Winner), kz_json:get_value(<<"Process-ID">>, Winner)} of
        {RetryAgentId, RetryProcId} ->
            lager:debug("recv retry from our winning agent ~s(~s)", [RetryAgentId, RetryProcId]),

            lager:debug("but wait, we have others who wanted to try"),
            gen_fsm:send_event(self(), {'timeout', 'undefined', ?COLLECT_RESP_MESSAGE}),

            maybe_stop_timer(CollectRef),
            maybe_stop_timer(AgentRef),

            webseq:evt(?WSD_ID, webseq:process_pid(RetryJObj), self(), <<"member call - retry">>),

            {'next_state', 'connect_req', State#state{agent_ring_timer_ref='undefined'
                                                     ,member_call_winner='undefined'
                                                     ,collect_ref='undefined'
                                                     }};
        {RetryAgentId, _OtherProcId} ->
            lager:debug("recv retry from monitoring proc ~s(~s)", [RetryAgentId, RetryProcId]),
            {'next_state', 'connecting', State};
        {_OtherAgentId, _OtherProcId} ->
            lager:debug("recv retry from unknown agent ~s(~s)", [RetryAgentId, RetryProcId]),
            {'next_state', 'connecting', State}
    end;
connecting({'timeout', AgentRef, ?AGENT_RING_TIMEOUT_MESSAGE}, #state{agent_ring_timer_ref=AgentRef
                                                                     ,member_call_winner=Winner
                                                                     ,queue_proc=Srv
                                                                     }=State) ->
    lager:debug("timed out waiting for agent to pick up"),
    lager:debug("let's try another agent"),
    gen_fsm:send_event(self(), {'timeout', 'undefined', ?COLLECT_RESP_MESSAGE}),

    acdc_queue_listener:timeout_agent(Srv, Winner),

    {'next_state', 'connect_req', State#state{agent_ring_timer_ref='undefined'
                                             ,member_call_winner='undefined'
                                             }};
connecting({'timeout', _OtherAgentRef, ?AGENT_RING_TIMEOUT_MESSAGE}, #state{agent_ring_timer_ref=_AgentRef}=State) ->
    lager:debug("unknown agent ref: ~p known: ~p", [_OtherAgentRef, _AgentRef]),
    {'next_state', 'connect_req', State};

connecting({'member_hungup', CallEvt}, #state{queue_proc=Srv
                                             ,account_id=AccountId
                                             ,queue_id=QueueId
                                             ,member_call=Call
                                             }=State) ->
    lager:debug("caller hungup while we waited for the agent to connect"),
    acdc_queue_listener:cancel_member_call(Srv, CallEvt),
    CallId = kapps_call:call_id(Call),
    acdc_stats:call_abandoned(AccountId, QueueId, CallId, ?ABANDON_HANGUP),

    webseq:evt(?WSD_ID, self(), CallId, <<"member call - hungup">>),

    {'next_state', 'ready', clear_member_call(State), 'hibernate'};

connecting({'member_finished'}, #state{member_call=Call}=State) ->
    case catch kapps_call:call_id(Call) of
        CallId when is_binary(CallId) ->
            lager:debug("member finished while in connecting: ~s", [CallId]),
            webseq:evt(?WSD_ID, self(), CallId, <<"member call finished - forced">>);
        _E->
            lager:debug("member finished, but callid became ~p", [_E])
    end,
    {'next_state', 'ready', clear_member_call(State), 'hibernate'};
connecting({'dtmf_pressed', DTMF}, #state{caller_exit_key=DTMF
                                         ,queue_proc=Srv
                                         ,account_id=AccountId
                                         ,queue_id=QueueId
                                         ,member_call=Call
                                         }=State) when is_binary(DTMF) ->
    lager:debug("member pressed the exit key (~s)", [DTMF]),
    acdc_queue_listener:exit_member_call(Srv),
    CallId = kapps_call:call_id(Call),
    webseq:evt(?WSD_ID, self(), CallId, <<"member call finish - DTMF">>),
    acdc_stats:call_abandoned(AccountId, QueueId, CallId, ?ABANDON_EXIT),
    {'next_state', 'ready', clear_member_call(State), 'hibernate'};

connecting({'dtmf_pressed', _DTMF}, State) ->
    lager:debug("caller pressed ~s, ignoring", [_DTMF]),
    {'next_state', 'connecting', State};

connecting({'timeout', ConnRef, ?CONNECTION_TIMEOUT_MESSAGE}, #state{queue_proc=Srv
                                                                    ,connection_timer_ref=ConnRef
                                                                    ,account_id=AccountId
                                                                    ,queue_id=QueueId
                                                                    ,member_call=Call
                                                                    ,member_call_winner=Winner
                                                                    }=State) ->
    lager:debug("connection timeout occurred, bounce the caller out of the queue"),

    maybe_timeout_winner(Srv, Winner),
    CallId = kapps_call:call_id(Call),
    acdc_stats:call_abandoned(AccountId, QueueId, CallId, ?ABANDON_TIMEOUT),

    webseq:evt(?WSD_ID, self(), CallId, <<"member call finish - timeout">>),

    {'next_state', 'ready', clear_member_call(State), 'hibernate'};

connecting(_Event, State) ->
    lager:debug("unhandled event in connecting: ~p", [_Event]),
    {'next_state', 'connecting', State}.

connecting('status', _, #state{member_call=Call
                              ,member_call_start=Start
                              ,connection_timer_ref=ConnRef
                              ,agent_ring_timer_ref=AgentRef
                              ,cdr_url=Url
                              ,recording_url=RecordingUrl
                              }=State) ->
    {'reply', [{<<"state">>, <<"connecting">>}
              ,{<<"call_id">>, kapps_call:call_id(Call)}
              ,{<<"caller_id_name">>, kapps_call:caller_id_name(Call)}
              ,{<<"caller_id_number">>, kapps_call:caller_id_name(Call)}
              ,{<<"to">>, kapps_call:to_user(Call)}
              ,{<<"from">>, kapps_call:from_user(Call)}
              ,{<<"wait_left">>, elapsed(ConnRef)}
              ,{<<"wait_time">>, elapsed(Start)}
              ,{<<"agent_wait_left">>, elapsed(AgentRef)}
              ,{<<"cdr_url">>, Url}
              ,{<<"recording_url">>, RecordingUrl}
              ], 'connecting', State};
connecting('current_call', _, #state{member_call=Call
                                    ,member_call_start=Start
                                    ,connection_timer_ref=ConnRef
                                    }=State) ->
    {'reply', current_call(Call, ConnRef, Start), 'connecting', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {'next_state', NextStateName, NextState} |
%%                   {'next_state', NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
-spec handle_event(any(), atom(), queue_fsm_state()) -> handle_fsm_ret(queue_fsm_state()).
handle_event({'refresh', QueueJObj}, StateName, State) ->
    lager:debug("refreshing queue configs"),
    {'next_state', StateName, update_properties(QueueJObj, State), 'hibernate'};
handle_event(_Event, StateName, State) ->
    lager:debug("unhandled event in state ~s: ~p", [StateName, _Event]),
    {'next_state', StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {'next_state', NextStateName, NextState} |
%%                   {'next_state', NextStateName, NextState, Timeout} |
%%                   {'reply', Reply, NextStateName, NextState} |
%%                   {'reply', Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
-spec handle_sync_event(any(), {pid(),any()}, atom(), queue_fsm_state()) ->
                               handle_sync_event_ret(queue_fsm_state()).
handle_sync_event('cdr_url', _, StateName, #state{cdr_url=Url}=State) ->
    {'reply', Url, StateName, State};
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = 'ok',
    lager:debug("unhandled sync event in ~s: ~p", [StateName, _Event]),
    {'reply', Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {'next_state', NextStateName, NextState} |
%%                   {'next_state', NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
-spec handle_info(any(), atom(), queue_fsm_state()) -> handle_fsm_ret(queue_fsm_state()).
handle_info(_Info, StateName, State) ->
    lager:debug("unhandled message in state ~s: ~p", [StateName, _Info]),
    {'next_state', StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(any(), atom(), queue_fsm_state()) -> 'ok'.
terminate(_Reason, _StateName, _State) ->
    lager:debug("acdc queue fsm terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), atom(), queue_fsm_state(), any()) ->
                         {'ok', atom(), queue_fsm_state()}.
code_change(_OldVsn, StateName, State, _Extra) ->
    {'ok', StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_collect_timer() ->
    gen_fsm:start_timer(?COLLECT_RESP_TIMEOUT, ?COLLECT_RESP_MESSAGE).

-spec connection_timeout(integer() | 'undefined') -> pos_integer().
connection_timeout(N) when is_integer(N), N > 0 -> N * 1000;
connection_timeout(_) -> ?CONNECTION_TIMEOUT.

-spec start_connection_timer(pos_integer()) -> reference().
start_connection_timer(ConnTimeout) ->
    gen_fsm:start_timer(ConnTimeout, ?CONNECTION_TIMEOUT_MESSAGE).

-spec agent_ring_timeout(integer() | 'undefined') -> pos_integer().
agent_ring_timeout(N) when is_integer(N), N > 0 -> N;
agent_ring_timeout(_) -> ?AGENT_RING_TIMEOUT.

-spec start_agent_ring_timer(pos_integer()) -> reference().
start_agent_ring_timer(AgentTimeout) ->
    gen_fsm:start_timer(AgentTimeout * 1600, ?AGENT_RING_TIMEOUT_MESSAGE).

-spec maybe_stop_timer(reference() | 'undefined') -> 'ok'.
maybe_stop_timer('undefined') -> 'ok';
maybe_stop_timer(ConnRef) ->
    _ = gen_fsm:cancel_timer(ConnRef),
    'ok'.

-spec maybe_timeout_winner(pid(), api_object()) -> 'ok'.
maybe_timeout_winner(Srv, 'undefined') ->
    acdc_queue_listener:timeout_member_call(Srv);
maybe_timeout_winner(Srv, Winner) ->
    acdc_queue_listener:timeout_member_call(Srv, Winner).

-spec clear_member_call(queue_fsm_state()) -> queue_fsm_state().
clear_member_call(#state{connection_timer_ref=ConnRef
                        ,agent_ring_timer_ref=AgentRef
                        ,collect_ref=CollectRef
                        ,queue_id=QueueId
                        }=State) ->
    kz_util:put_callid(QueueId),
    maybe_stop_timer(ConnRef),
    maybe_stop_timer(AgentRef),
    maybe_stop_timer(CollectRef),
    State#state{connect_resps=[]
               ,collect_ref='undefined'
               ,member_call='undefined'
               ,connection_timer_ref='undefined'
               ,agent_ring_timer_ref='undefined'
               ,member_call_start='undefined'
               ,member_call_winner='undefined'
               }.

update_properties(QueueJObj, State) ->
    State#state{
      name = kz_json:get_value(<<"name">>, QueueJObj)
               ,connection_timeout = connection_timeout(kz_json:get_integer_value(<<"connection_timeout">>, QueueJObj))
               ,agent_ring_timeout = agent_ring_timeout(kz_json:get_integer_value(<<"agent_ring_timeout">>, QueueJObj))
               ,max_queue_size = kz_json:get_integer_value(<<"max_queue_size">>, QueueJObj)
               ,ring_simultaneously = kz_json:get_value(<<"ring_simultaneously">>, QueueJObj)
               ,enter_when_empty = kz_json:is_true(<<"enter_when_empty">>, QueueJObj, 'true')
               ,agent_wrapup_time = kz_json:get_integer_value(<<"agent_wrapup_time">>, QueueJObj)
               ,announce = kz_json:get_value(<<"announce">>, QueueJObj)
               ,caller_exit_key = kz_json:get_value(<<"caller_exit_key">>, QueueJObj, <<"#">>)
               ,record_caller = kz_json:is_true(<<"record_caller">>, QueueJObj, 'false')
               ,recording_url = kz_json:get_ne_value(<<"call_recording_url">>, QueueJObj)
               ,cdr_url = kz_json:get_ne_value(<<"cdr_url">>, QueueJObj)
               ,notifications = kz_json:get_value(<<"notifications">>, QueueJObj)

      %% Changing queue strategy currently isn't feasible; definitely a TODO
      %%,strategy = get_strategy(kz_json:get_value(<<"strategy">>, QueueJObj))
     }.

-spec current_call('undefined' | kapps_call:call(), reference() | kz_timeout() | 'undefined', kz_timeout()) ->
                          kz_json:object().
current_call('undefined', _, _) -> 'undefined';
current_call(Call, QueueTimeLeft, Start) ->
    kz_json:from_list([{<<"call_id">>, kapps_call:call_id(Call)}
                      ,{<<"caller_id_name">>, kapps_call:caller_id_name(Call)}
                      ,{<<"caller_id_number">>, kapps_call:caller_id_name(Call)}
                      ,{<<"to">>, kapps_call:to_user(Call)}
                      ,{<<"from">>, kapps_call:from_user(Call)}
                      ,{<<"wait_left">>, elapsed(QueueTimeLeft)}
                      ,{<<"wait_time">>, elapsed(Start)}
                      ]).

-spec elapsed(api_reference() | kz_timeout() | integer()) -> api_integer().
elapsed('undefined') -> 'undefined';
elapsed(Ref) when is_reference(Ref) ->
    case erlang:read_timer(Ref) of
        'false' -> 'undefined';
        Ms -> Ms div 1000
    end;
elapsed(Time) -> kz_util:elapsed_s(Time).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Abort a queue call between connect_reqs if agents have left the
%% building
%%
%% @spec maybe_connect_re_req(pid(), pid(), queue_fsm_state()) ->
%%                   queue_fsm_state()
%% @end
%%--------------------------------------------------------------------
-spec maybe_connect_re_req(pid(), pid(), queue_fsm_state()) ->
                                  {'next_state', atom(), queue_fsm_state()}
                                      | {'next_state', atom(), queue_fsm_state(), 'hibernate'}.
maybe_connect_re_req(MgrSrv, ListenerSrv, #state{account_id=AccountId
                                                ,queue_id=QueueId
                                                ,member_call=Call
                                                }=State) ->
    case acdc_queue_manager:are_agents_available(MgrSrv) of
        'true' ->
            lager:debug("done waiting, no agents responded, let's ask again"),
            webseq:note(?WSD_ID, self(), 'right', <<"no agents responded, trying again">>),
            acdc_queue_listener:member_connect_re_req(ListenerSrv),
            {'next_state', 'connect_req', State#state{collect_ref=start_collect_timer()}};
        'false' ->
            lager:debug("all agents have left the queue, failing call"),
            webseq:note(?WSD_ID, self(), 'right', <<"all agents have left the queue, failing call">>),
            acdc_queue_listener:exit_member_call_empty(ListenerSrv),
            acdc_stats:call_abandoned(AccountId, QueueId, kapps_call:call_id(Call), ?ABANDON_EMPTY),
            {'next_state', 'ready', clear_member_call(State), 'hibernate'}
    end.

-spec accept_is_for_call(kz_json:object(), kapps_call:call()) -> boolean().
accept_is_for_call(AcceptJObj, Call) ->
    kz_json:get_value(<<"Call-ID">>, AcceptJObj) =:= kapps_call:call_id(Call).

-spec update_agent(kz_json:object(), kz_json:object()) -> kz_json:object().
update_agent(Agent, Winner) ->
    kz_json:set_value(<<"Agent-Process-ID">>, kz_json:get_value(<<"Process-ID">>, Winner), Agent).

-spec handle_agent_responses(queue_fsm_state()) -> {atom(), queue_fsm_state()}.
handle_agent_responses(#state{collect_ref=Ref
                             ,manager_proc=MgrSrv
                             ,queue_proc=Srv
                             ,member_call=Call
                             ,account_id=AccountId
                             ,queue_id=QueueId
                             }=State) ->
    maybe_stop_timer(Ref),
    case acdc_queue_manager:should_ignore_member_call(MgrSrv, Call, AccountId, QueueId) of
        'true' ->
            lager:debug("queue mgr said to ignore this call: ~s, not connecting to agents", [kapps_call:call_id(Call)]),
            acdc_queue_listener:finish_member_call(Srv),
            {'ready', State};
        'false' ->
            lager:debug("done waiting for agents to respond, picking a winner"),
            maybe_pick_winner(State)
    end.

-spec maybe_pick_winner(queue_fsm_state()) -> {atom(), queue_fsm_state()}.
maybe_pick_winner(#state{connect_resps=CRs
                        ,queue_proc=Srv
                        ,manager_proc=Mgr
                        ,agent_ring_timeout=RingTimeout
                        ,agent_wrapup_time=AgentWrapup
                        ,caller_exit_key=CallerExitKey
                        ,cdr_url=CDRUrl
                        ,record_caller=ShouldRecord
                        ,recording_url=RecordUrl
                        ,notifications=Notifications
                        }=State) ->
    case acdc_queue_manager:pick_winner(Mgr, CRs) of
        {[Winner|_]=Agents, Rest} ->
            QueueOpts = [{<<"Ring-Timeout">>, RingTimeout}
                        ,{<<"Wrapup-Timeout">>, AgentWrapup}
                        ,{<<"Caller-Exit-Key">>, CallerExitKey}
                        ,{<<"CDR-Url">>, CDRUrl}
                        ,{<<"Record-Caller">>, ShouldRecord}
                        ,{<<"Recording-URL">>, RecordUrl}
                        ,{<<"Notifications">>, Notifications}
                        ],

            _ = [acdc_queue_listener:member_connect_win(Srv, update_agent(Agent, Winner), QueueOpts)
                 || Agent <- Agents
                ],

            lager:debug("sending win to ~s(~s)", [kz_json:get_value(<<"Agent-ID">>, Winner)
                                                 ,kz_json:get_value(<<"Process-ID">>, Winner)
                                                 ]),
            {'connecting', State#state{connect_resps=Rest
                                      ,collect_ref='undefined'
                                      ,agent_ring_timer_ref=start_agent_ring_timer(RingTimeout)
                                      ,member_call_winner=Winner
                                      }};
        'undefined' ->
            lager:debug("no more responses to choose from"),

            acdc_queue_listener:cancel_member_call(Srv),
            {'ready', clear_member_call(State)}
    end.

-spec have_agents_responded(kz_json:objects(), ne_binaries()) -> boolean().
have_agents_responded(Resps, Agents) ->
    lists:foldl(fun filter_agents/2, Agents, Resps) =:= [].

-spec filter_agents(kz_json:object(), ne_binaries()) -> ne_binaries().
filter_agents(Resp, AgentsAcc) ->
    lists:delete(kz_json:get_value(<<"Agent-ID">>, Resp), AgentsAcc).
