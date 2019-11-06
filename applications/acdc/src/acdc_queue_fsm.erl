%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Controls how a queue process progresses a member_call
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_queue_fsm).

-behaviour(gen_statem).

%% API
-export([start_link/4]).

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
-export([ready/3
        ,connect_req/3
        ,connecting/3
        ]).

%% gen_statem callbacks
-export([init/1
        ,callback_mode/0
        ,terminate/3
        ,code_change/4
        ]).

-include("acdc.hrl").

-define(SERVER, ?MODULE).

%% How long should we wait for a response to our member_connect_req
-define(COLLECT_RESP_TIMEOUT, kapps_config:get_integer(?CONFIG_CAT, <<"queue_collect_resp_timeout">>, 2000)).
-define(COLLECT_RESP_MESSAGE, 'collect_timer_expired').

%% How long will the caller wait in the call queue before being bounced out
-define(CONNECTION_TIMEOUT, 1000 * ?SECONDS_IN_HOUR).
-define(CONNECTION_TIMEOUT_MESSAGE, 'connection_timer_expired').

%% How long to ring the agent before trying the next agent
-define(AGENT_RING_TIMEOUT, 5).
-define(AGENT_RING_TIMEOUT_MESSAGE, 'agent_timer_expired').

-record(state, {listener_proc :: kz_term:api_pid()
               ,manager_proc :: pid()
               ,connect_resps = [] :: kz_json:objects()
               ,connect_wins = [] :: kz_json:objects()
               ,collect_ref :: kz_term:api_reference()
               ,account_id :: kz_term:ne_binary()
               ,account_db :: kz_term:ne_binary()
               ,queue_id :: kz_term:ne_binary()

               ,timer_ref :: kz_term:api_reference() % for tracking timers
               ,connection_timer_ref :: kz_term:api_reference() % how long can a caller wait in the queue
               ,agent_ring_timer_ref :: kz_term:api_reference() % how long to ring an agent before moving to the next

               ,member_call :: kapps_call:call() | 'undefined'
               ,member_call_start :: kz_time:start_time() | 'undefined'
               ,member_call_winners :: [kz_term:api_object()] %% who won the call

                                       %% Config options
               ,name :: kz_term:ne_binary()
               ,connection_timeout :: pos_integer()
               ,agent_ring_timeout = 10 :: pos_integer() % how long to ring an agent before giving up
               ,max_queue_size = 0 :: integer() % restrict the number of the queued callers
               ,ring_simultaneously = 1 :: integer() % how many agents to try ringing at a time (first one wins)
               ,enter_when_empty = true :: boolean() % if a queue is agent-less, can the caller enter?
               ,agent_wrapup_time = 0 :: integer() % forced wrapup time for an agent after a call

               ,announce :: kz_term:ne_binary() % media to play to customer when about to be connected to agent

               ,caller_exit_key :: kz_term:ne_binary() % DTMF a caller can press to leave the queue
               ,record_caller = 'false' :: boolean() % record the caller
               ,recording_url :: kz_term:api_binary() %% URL of where to POST recordings
               ,cdr_url :: kz_term:api_binary() % optional URL to request for extra CDR data

               ,notifications :: kz_term:api_object()
               }).
-type state() :: #state{}.

-define(WSD_ID, {'file', <<(get('callid'))/binary, "_queue_statem">>}).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(pid(), pid(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_types:startlink_ret().
start_link(WorkerSup, MgrPid, AccountId, QueueId) ->
    gen_statem:start_link(?SERVER, [WorkerSup, MgrPid, AccountId, QueueId], []).

-spec refresh(pid(), kz_json:object()) -> 'ok'.
refresh(ServerRef, QueueJObj) ->
    gen_statem:cast(ServerRef, {'refresh', QueueJObj}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec member_call(pid(), kz_json:object(), gen_listener:basic_deliver()) -> 'ok'.
member_call(ServerRef, CallJObj, Delivery) ->
    gen_statem:cast(ServerRef, {'member_call', CallJObj, Delivery}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec member_connect_resp(pid(), kz_json:object()) -> 'ok'.
member_connect_resp(ServerRef, Resp) ->
    gen_statem:cast(ServerRef, {'agent_resp', Resp}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec member_accepted(pid(), kz_json:object()) -> 'ok'.
member_accepted(ServerRef, AcceptJObj) ->
    gen_statem:cast(ServerRef, {'accepted', AcceptJObj}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec member_connect_retry(pid(), kz_json:object()) -> 'ok'.
member_connect_retry(ServerRef, RetryJObj) ->
    gen_statem:cast(ServerRef, {'retry', RetryJObj}).

%%------------------------------------------------------------------------------
%% @doc When a queue is processing a call, it will receive call events.
%%   Pass the call event to the statem to see if action is needed (usually
%%   for hangup events).
%% @end
%%------------------------------------------------------------------------------
-spec call_event(pid(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
call_event(ServerRef, <<"call_event">>, <<"CHANNEL_DESTROY">>, EvtJObj) ->
    gen_statem:cast(ServerRef, {'member_hungup', EvtJObj});
call_event(ServerRef, <<"call_event">>, <<"DTMF">>, EvtJObj) ->
    gen_statem:cast(ServerRef, {'dtmf_pressed', kz_json:get_value(<<"DTMF-Digit">>, EvtJObj)});
call_event(ServerRef, <<"call_event">>, <<"CHANNEL_BRIDGE">>, EvtJObj) ->
    gen_statem:cast(ServerRef, {'channel_bridged', EvtJObj});
call_event(_, _E, _N, _J) -> 'ok'.
%% lager:debug("unhandled event: ~s: ~s (~s)"
%%             ,[_E, _N, kz_json:get_value(<<"Application-Name">>, _J)]
%%            ).

-spec finish_member_call(pid()) -> 'ok'.
finish_member_call(ServerRef) ->
    gen_statem:cast(ServerRef, {'member_finished'}).

-spec current_call(pid()) -> kz_term:api_object().
current_call(ServerRef) ->
    gen_statem:call(ServerRef, 'current_call').

-spec status(pid()) -> kz_term:proplist().
status(ServerRef) ->
    gen_statem:call(ServerRef, 'status').

-spec cdr_url(pid()) -> kz_term:api_binary().
cdr_url(ServerRef) ->
    gen_statem:call(ServerRef, 'cdr_url').

%%%=============================================================================
%%% gen_statem callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Whenever a gen_statem is started using
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @end
%%------------------------------------------------------------------------------
-spec init(list()) -> {'ok', atom(), state()}.
init([WorkerSup, MgrPid, AccountId, QueueId]) ->
    kz_log:put_callid(<<"statem_", QueueId/binary, "_", (kz_term:to_binary(self()))/binary>>),

    _ = webseq:start(?WSD_ID),
    webseq:reg_who(?WSD_ID, self(), iolist_to_binary([<<"qFSM">>, pid_to_list(self())])),

    AccountDb = kz_util:format_account_db(AccountId),
    {'ok', QueueJObj} = kz_datamgr:open_cache_doc(AccountDb, QueueId),

    gen_statem:cast(self(), {'get_listener_proc', WorkerSup}),
    {'ok'
    ,'ready'
    ,#state{manager_proc = MgrPid
           ,account_id = AccountId
           ,account_db = AccountDb
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
           ,member_call_winners = []

           ,notifications = kz_json:get_value(<<"notifications">>, QueueJObj)
           }
    }.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec callback_mode() -> 'state_functions'.
callback_mode() ->
    'state_functions'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec ready(gen_statem:event_type(), any(), state()) -> kz_types:handle_fsm_ret(state()).
ready('cast', {'get_listener_proc', WorkerSup}, State) ->
    ListenerSrv = acdc_queue_worker_sup:listener(WorkerSup),
    lager:debug("got listener proc: ~p", [ListenerSrv]),
    {'next_state', 'ready', State#state{listener_proc=ListenerSrv}};
ready('cast', {'member_call', CallJObj, Delivery}, #state{listener_proc=ListenerSrv
                                                         ,manager_proc=MgrSrv
                                                         }=State) ->
    Call = kapps_call:from_json(kz_json:get_value(<<"Call">>, CallJObj)),
    CallId = kapps_call:call_id(Call),
    kz_log:put_callid(CallId),

    case acdc_queue_manager:should_ignore_member_call(MgrSrv, Call, CallJObj) of
        'false' ->
            maybe_delay_connect_req(Call, CallJObj, Delivery, State);
        'true' ->
            lager:debug("queue mgr said to ignore this call: ~s", [CallId]),
            acdc_queue_listener:ignore_member_call(ListenerSrv, Call, Delivery),
            {'next_state', 'ready', State}
    end;
ready('cast', {'agent_resp', _Resp}, State) ->
    lager:debug("someone jumped the gun, or was slow on the draw"),
    {'next_state', 'ready', State};
ready('cast', {'accepted', _AcceptJObj}, State) ->
    lager:debug("weird to receive an acceptance"),
    {'next_state', 'ready', State};
ready('cast', {'retry', _RetryJObj}, State) ->
    lager:debug("weird to receive a retry when we're just hanging here"),
    {'next_state', 'ready', State};
ready('cast', {'member_hungup', _CallEvt}, State) ->
    lager:debug("member hungup from previous call: ~p", [_CallEvt]),
    {'next_state', 'ready', State};
ready('cast', {'member_finished'}, State) ->
    lager:debug("member finished while in 'ready', ignore"),
    {'next_state', 'ready', State};
ready('cast', {'dtmf_pressed', _DTMF}, State) ->
    lager:debug("DTMF(~s) for old call", [_DTMF]),
    {'next_state', 'ready', State};
ready('cast', Event, State) ->
    handle_event(Event, ready, State);
ready({'call', From}, 'status', #state{cdr_url=Url
                                      ,recording_url=RecordingUrl
                                      }=State) ->
    {'next_state', 'ready', State
    ,{'reply', From, [{'state', <<"ready">>}
                     ,{<<"cdr_url">>, Url}
                     ,{<<"recording_url">>, RecordingUrl}
                     ]}};
ready({'call', From}, 'current_call', State) ->
    {'next_state', 'ready', State, {'reply', From, 'undefined'}};
ready({'call', From}, Event, State) ->
    handle_sync_event(Event, From, ready, State).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec connect_req(gen_statem:event_type(), any(), state()) -> kz_types:handle_fsm_ret(state()).
connect_req('cast', {'member_call', CallJObj, Delivery}, #state{listener_proc=ListenerSrv}=State) ->
    lager:debug("recv a member_call while processing a different member"),
    CallId = kz_json:get_value(<<"Call-ID">>, CallJObj),
    webseq:evt(?WSD_ID, CallId, self(), <<"member call recv while busy">>),
    acdc_queue_listener:cancel_member_call(ListenerSrv, CallJObj, Delivery),
    {'next_state', 'connect_req', State};

connect_req('cast', {'agent_resp', Resp}, #state{connect_resps=CRs
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

connect_req('cast', {'accepted', AcceptJObj}=Accept, #state{member_call=Call}=State) ->
    case accept_is_for_call(AcceptJObj, Call) of
        'true' ->
            lager:debug("received acceptance for call ~s: yet to send connect_req though", [kapps_call:call_id(Call)]),
            connecting('cast', Accept, State);
        'false' ->
            lager:debug("received (and ignoring) acceptance payload"),
            {'next_state', 'connect_req', State}
    end;
connect_req('cast', {'retry', _RetryJObj}, State) ->
    lager:debug("recv retry response before win sent"),
    {'next_state', 'connect_req', State};

connect_req('cast', {'member_hungup', JObj}, #state{listener_proc=ListenerSrv
                                                   ,member_call=Call
                                                   ,account_id=AccountId
                                                   ,queue_id=QueueId
                                                   }=State) ->
    CallId = kapps_call:call_id(Call),
    case kz_json:get_value(<<"Call-ID">>, JObj) =:= CallId of
        'true' ->
            lager:debug("member hungup before we could assign an agent"),

            webseq:evt(?WSD_ID, self(), CallId, <<"member call finish - abandon">>),

            acdc_queue_listener:cancel_member_call(ListenerSrv, JObj),
            acdc_stats:call_abandoned(AccountId, QueueId, CallId, ?ABANDON_HANGUP),
            {'next_state', 'ready', clear_member_call(State), 'hibernate'};
        'false' ->
            lager:debug("hangup recv for ~s while processing ~s, ignoring", [kz_json:get_value(<<"Call-ID">>, JObj)
                                                                            ,CallId
                                                                            ]),
            {'next_state', 'connect_req', State}
    end;

connect_req('cast', {'member_finished'}, #state{member_call=Call}=State) ->
    case catch kapps_call:call_id(Call) of
        CallId when is_binary(CallId) ->
            lager:debug("member finished while in connect_req: ~s", [CallId]),
            webseq:evt(?WSD_ID, self(), CallId, <<"member call finished - forced">>);
        _E->
            lager:debug("member finished, but callid became ~p", [_E])
    end,
    {'next_state', 'ready', clear_member_call(State), 'hibernate'};

connect_req('cast', {'dtmf_pressed', DTMF}, #state{caller_exit_key=DTMF
                                                  ,listener_proc=ListenerSrv
                                                  ,account_id=AccountId
                                                  ,queue_id=QueueId
                                                  ,member_call=Call
                                                  }=State) when is_binary(DTMF) ->
    lager:debug("member pressed the exit key (~s)", [DTMF]),
    CallId = kapps_call:call_id(Call),
    webseq:evt(?WSD_ID, self(), CallId, <<"member call finish - DTMF">>),

    acdc_queue_listener:exit_member_call(ListenerSrv),
    acdc_stats:call_abandoned(AccountId, QueueId, CallId, ?ABANDON_EXIT),
    {'next_state', 'ready', clear_member_call(State), 'hibernate'};

connect_req('cast', Event, State) ->
    handle_event(Event, connect_req, State);

connect_req({'call', From}, 'status', #state{member_call=Call
                                            ,member_call_start=Start
                                            ,connection_timer_ref=ConnRef
                                            ,cdr_url=Url
                                            ,recording_url=RecordingUrl
                                            }=State) ->
    {'next_state', 'connect_req', State
    ,{'reply', From, [{<<"state">>, <<"connect_req">>}
                     ,{<<"call_id">>, kapps_call:call_id(Call)}
                     ,{<<"caller_id_name">>, kapps_call:caller_id_name(Call)}
                     ,{<<"caller_id_number">>, kapps_call:caller_id_name(Call)}
                     ,{<<"to">>, kapps_call:to_user(Call)}
                     ,{<<"from">>, kapps_call:from_user(Call)}
                     ,{<<"wait_left">>, elapsed(ConnRef)}
                     ,{<<"wait_time">>, elapsed(Start)}
                     ,{<<"cdr_url">>, Url}
                     ,{<<"recording_url">>, RecordingUrl}
                     ]}};
connect_req({'call', From}, 'current_call', #state{member_call=Call
                                                  ,member_call_start=Start
                                                  ,connection_timer_ref=ConnRef
                                                  }=State) ->
    {'next_state', 'connect_req', State
    ,{'reply', From, current_call(Call, ConnRef, Start)}
    };
connect_req({'call', From}, Event, State) ->
    handle_sync_event(Event, From, connect_req, State);

connect_req('info', {'timeout', Ref, ?COLLECT_RESP_MESSAGE}, #state{collect_ref=Ref
                                                                   ,connect_resps=[]
                                                                   ,manager_proc=MgrSrv
                                                                   ,member_call=Call
                                                                   ,listener_proc=ListenerSrv
                                                                   ,account_id=AccountId
                                                                   ,queue_id=QueueId
                                                                   }=State) ->
    maybe_stop_timer(Ref),
    case acdc_queue_manager:should_ignore_member_call(MgrSrv, Call, AccountId, QueueId) of
        'true' ->
            lager:debug("queue mgr said to ignore this call: ~s, not retrying agents", [kapps_call:call_id(Call)]),
            acdc_queue_listener:finish_member_call(ListenerSrv),
            {'next_state', 'ready', State};
        'false' ->
            maybe_connect_re_req(MgrSrv, ListenerSrv, State)
    end;
connect_req('info', {'timeout', Ref, ?COLLECT_RESP_MESSAGE}, #state{collect_ref=Ref}=State) ->
    {NextState, State1} = handle_agent_responses(State),
    {'next_state', NextState, State1};
connect_req('info', {'timeout', ConnRef, ?CONNECTION_TIMEOUT_MESSAGE}, #state{listener_proc=ListenerSrv
                                                                             ,connection_timer_ref=ConnRef
                                                                             ,account_id=AccountId
                                                                             ,queue_id=QueueId
                                                                             ,member_call=Call
                                                                             }=State) ->
    lager:debug("connection timeout occurred, bounce the caller out of the queue"),
    CallId = kapps_call:call_id(Call),
    webseq:evt(?WSD_ID, self(), CallId, <<"member call finish - timeout">>),

    acdc_queue_listener:timeout_member_call(ListenerSrv),
    acdc_stats:call_abandoned(AccountId, QueueId, CallId, ?ABANDON_TIMEOUT),
    {'next_state', 'ready', clear_member_call(State), 'hibernate'}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec connecting(gen_statem:event_type(), any(), state()) -> kz_types:handle_fsm_ret(state()).
connecting('cast', {'member_call', CallJObj, Delivery}, #state{listener_proc=ListenerSrv}=State) ->
    lager:debug("recv a member_call while connecting"),
    acdc_queue_listener:cancel_member_call(ListenerSrv, CallJObj, Delivery),
    {'next_state', 'connecting', State};

connecting('cast', {'agent_resp', _Resp}, State) ->
    lager:debug("agent resp must have just missed cutoff"),
    {'next_state', 'connecting', State};

connecting('cast', {'accepted', AcceptJObj}, #state{listener_proc=ListenerSrv
                                                   ,member_call=Call
                                                   ,account_id=AccountId
                                                   ,queue_id=QueueId
                                                   ,connect_wins=Wins
                                                   }=State) ->
    case accept_is_for_call(AcceptJObj, Call) of
        'true' ->
            lager:debug("recv acceptance from agent"),
            CallId = kapps_call:call_id(Call),
            webseq:evt(?WSD_ID, self(), CallId, <<"member call - agent acceptance">>),

            lists:foreach(fun(Win) -> acdc_queue_listener:member_connect_satisfied(ListenerSrv, Win, []) end, Wins),
            acdc_queue_listener:finish_member_call(ListenerSrv, AcceptJObj),
            acdc_stats:call_handled(AccountId, QueueId, CallId
                                   ,kz_json:get_value(<<"Agent-ID">>, AcceptJObj)
                                   ),
            {'next_state', 'ready', clear_member_call(State), 'hibernate'};
        'false' ->
            lager:debug("ignoring accepted message"),
            {'next_state', 'connecting', State}
    end;

connecting('cast', {'retry', RetryJObj}, #state{agent_ring_timer_ref=AgentRef
                                               ,collect_ref=CollectRef
                                               ,member_call_winners=[Winner|[]]
                                               }=State) ->
    RetryProcId = kz_json:get_value(<<"Process-ID">>, RetryJObj),
    RetryAgentId = kz_json:get_value(<<"Agent-ID">>, RetryJObj),

    case {kz_json:get_value(<<"Agent-ID">>, Winner), kz_json:get_value(<<"Process-ID">>, Winner)} of
        {RetryAgentId, RetryProcId} ->
            lager:debug("recv retry from our winning agent ~s(~s)", [RetryAgentId, RetryProcId]),

            lager:debug("but wait, we have others who wanted to try"),
            erlang:send(self(), {'timeout', 'undefined', ?COLLECT_RESP_MESSAGE}),

            maybe_stop_timer(CollectRef),
            maybe_stop_timer(AgentRef),

            webseq:evt(?WSD_ID, webseq:process_pid(RetryJObj), self(), <<"member call - retry">>),

            {'next_state', 'connect_req', State#state{agent_ring_timer_ref='undefined'
                                                     ,member_call_winners=[]
                                                     ,collect_ref='undefined'
                                                     }};
        {RetryAgentId, _OtherProcId} ->
            lager:debug("recv retry from monitoring proc ~s(~s)", [RetryAgentId, RetryProcId]),
            {'next_state', 'connecting', State};
        {_OtherAgentId, _OtherProcId} ->
            lager:debug("recv retry from unknown agent ~s(~s)", [RetryAgentId, RetryProcId]),
            {'next_state', 'connecting', State}
    end;
connecting('cast', {'retry', RetryJObj}, #state{member_call_winners=Winners}=State) ->
    RetryAgentId = kz_json:get_value(<<"Agent-ID">>, RetryJObj),
    RetryProcId = kz_json:get_value(<<"Process-ID">>, RetryJObj),
    lager:info("~p told to retry but we have other winners", [RetryAgentId]),
    {_Loser, Rest} = lists:partition(fun(Winner) -> {kz_json:get_value(<<"Agent-ID">>, Winner), kz_json:get_value(<<"Process-ID">>, Winner)} == {RetryAgentId, RetryProcId} end, Winners),
    {'next_state', 'connecting', State#state{member_call_winners=Rest}};

connecting('cast', {'member_hungup', CallEvt}, #state{listener_proc=ListenerSrv
                                                     ,account_id=AccountId
                                                     ,queue_id=QueueId
                                                     ,member_call=Call
                                                     }=State) ->
    lager:debug("caller hungup while we waited for the agent to connect"),
    acdc_queue_listener:cancel_member_call(ListenerSrv, CallEvt),
    CallId = kapps_call:call_id(Call),
    acdc_stats:call_abandoned(AccountId, QueueId, CallId, ?ABANDON_HANGUP),

    webseq:evt(?WSD_ID, self(), CallId, <<"member call - hungup">>),

    {'next_state', 'ready', clear_member_call(State), 'hibernate'};

connecting('cast', {'member_finished'}, #state{member_call=Call}=State) ->
    case catch kapps_call:call_id(Call) of
        CallId when is_binary(CallId) ->
            lager:debug("member finished while in connecting: ~s", [CallId]),
            webseq:evt(?WSD_ID, self(), CallId, <<"member call finished - forced">>);
        _E->
            lager:debug("member finished, but callid became ~p", [_E])
    end,
    {'next_state', 'ready', clear_member_call(State), 'hibernate'};
connecting('cast', {'dtmf_pressed', DTMF}, #state{caller_exit_key=DTMF
                                                 ,listener_proc=ListenerSrv
                                                 ,account_id=AccountId
                                                 ,queue_id=QueueId
                                                 ,member_call=Call
                                                 }=State) when is_binary(DTMF) ->
    lager:debug("member pressed the exit key (~s)", [DTMF]),
    acdc_queue_listener:exit_member_call(ListenerSrv),
    CallId = kapps_call:call_id(Call),
    webseq:evt(?WSD_ID, self(), CallId, <<"member call finish - DTMF">>),
    acdc_stats:call_abandoned(AccountId, QueueId, CallId, ?ABANDON_EXIT),
    {'next_state', 'ready', clear_member_call(State), 'hibernate'};

connecting('cast', {'dtmf_pressed', _DTMF}, State) ->
    lager:debug("caller pressed ~s, ignoring", [_DTMF]),
    {'next_state', 'connecting', State};

connecting('cast', Event, State) ->
    handle_event(Event, connecting, State);

connecting({'call', From}, 'status', #state{member_call=Call
                                           ,member_call_start=Start
                                           ,connection_timer_ref=ConnRef
                                           ,agent_ring_timer_ref=AgentRef
                                           ,cdr_url=Url
                                           ,recording_url=RecordingUrl
                                           }=State) ->
    {'next_state', 'connecting', State
    ,{'reply', From, [{<<"state">>, <<"connecting">>}
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
                     ]}};
connecting({'call', From}, 'current_call', #state{member_call=Call
                                                 ,member_call_start=Start
                                                 ,connection_timer_ref=ConnRef
                                                 }=State) ->
    {'next_state', 'connecting', State
    ,{'reply', From, current_call(Call, ConnRef, Start)}
    };
connecting({'call', From}, Event, State) ->
    handle_sync_event(Event, From, 'connecting', State);

connecting('info', {'timeout', AgentRef, ?AGENT_RING_TIMEOUT_MESSAGE}, #state{agent_ring_timer_ref=AgentRef
                                                                             ,member_call_winners=[Winner|[]]
                                                                             ,listener_proc=ListenerSrv
                                                                             }=State) ->
    lager:debug("timed out waiting for agent to pick up"),
    lager:debug("let's try another agent"),
    erlang:send(self(), {'timeout', 'undefined', ?COLLECT_RESP_MESSAGE}),

    acdc_queue_listener:timeout_agent(ListenerSrv, Winner),

    {'next_state', 'connect_req', State#state{agent_ring_timer_ref='undefined'
                                             ,member_call_winners=[]
                                             }};
connecting('info', {'timeout', _AgentRef, ?AGENT_RING_TIMEOUT_MESSAGE}, #state{member_call_winners=[_Winner|Winners]}=State)->
                                                % TODO: we should detect who told timeout and remove him
                                                % I remove the first (but this is not the right way)
    {'next_state', 'connect_req', State#state{member_call_winners=Winners}};
connecting('info', {'timeout', _OtherAgentRef, ?AGENT_RING_TIMEOUT_MESSAGE}, #state{agent_ring_timer_ref=_AgentRef}=State) ->
    lager:debug("unknown agent ref: ~p known: ~p", [_OtherAgentRef, _AgentRef]),
    {'next_state', 'connect_req', State};
connecting('info', {'timeout', ConnRef, ?CONNECTION_TIMEOUT_MESSAGE}, #state{listener_proc=ListenerSrv
                                                                            ,connection_timer_ref=ConnRef
                                                                            ,account_id=AccountId
                                                                            ,queue_id=QueueId
                                                                            ,member_call=Call
                                                                            ,member_call_winners=Winners
                                                                            }=State) ->
    lager:debug("connection timeout occurred, bounce the caller out of the queue"),

    lists:foreach(fun(Winner) -> maybe_timeout_winner(ListenerSrv, Winner) end, Winners),
    CallId = kapps_call:call_id(Call),
    acdc_stats:call_abandoned(AccountId, QueueId, CallId, ?ABANDON_TIMEOUT),

    webseq:evt(?WSD_ID, self(), CallId, <<"member call finish - timeout">>),

    {'next_state', 'ready', clear_member_call(State), 'hibernate'}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(any(), atom(), state()) -> kz_types:handle_fsm_ret(state()).
handle_event({'refresh', QueueJObj}, StateName, State) ->
    lager:debug("refreshing queue configs"),
    {'next_state', StateName, update_properties(QueueJObj, State), 'hibernate'};
handle_event(_Event, StateName, State) ->
    lager:debug("unhandled event in state ~s: ~p", [StateName, _Event]),
    {'next_state', StateName, State}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_sync_event(any(), From :: pid(), StateName :: atom(), state()) ->
                               {'next_state', StateName :: atom(), state()
                               ,{'reply', From :: pid(), any()}}.
handle_sync_event('cdr_url', From, StateName, #state{cdr_url=Url}=State) ->
    {'next_state', StateName, State
    ,{'reply', From, Url}
    };
handle_sync_event(_Event, From, StateName, State) ->
    Reply = 'ok',
    lager:debug("unhandled sync event in ~s: ~p", [StateName, _Event]),
    {'next_state', StateName, State
    ,{'reply', From, Reply}
    }.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_statem' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_statem' terminates with
%% Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), atom(), state()) -> 'ok'.
terminate(_Reason, _StateName, _State) ->
    lager:debug("acdc queue statem terminating: ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), atom(), state(), any()) -> {'ok', atom(), state()}.
code_change(_OldVsn, StateName, State, _Extra) ->
    {'ok', StateName, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
start_collect_timer() ->
    erlang:start_timer(?COLLECT_RESP_TIMEOUT, self(), ?COLLECT_RESP_MESSAGE).

-spec connection_timeout(kz_term:api_integer()) -> pos_integer().
connection_timeout(N) when is_integer(N), N > 0 -> N * 1000;
connection_timeout(_) -> ?CONNECTION_TIMEOUT.

-spec start_connection_timer(pos_integer()) -> reference().
start_connection_timer(ConnTimeout) ->
    erlang:start_timer(ConnTimeout, self(), ?CONNECTION_TIMEOUT_MESSAGE).

-spec agent_ring_timeout(kz_term:api_integer()) -> pos_integer().
agent_ring_timeout(N) when is_integer(N), N > 0 -> N;
agent_ring_timeout(_) -> ?AGENT_RING_TIMEOUT.

-spec start_agent_ring_timer(pos_integer()) -> reference().
start_agent_ring_timer(AgentTimeout) ->
    erlang:start_timer(AgentTimeout * 1600, self(), ?AGENT_RING_TIMEOUT_MESSAGE).

-spec maybe_stop_timer(kz_term:api_reference()) -> 'ok'.
maybe_stop_timer('undefined') -> 'ok';
maybe_stop_timer(ConnRef) ->
    _ = erlang:cancel_timer(ConnRef),
    'ok'.

-spec maybe_timeout_winner(pid(), kz_term:api_object()) -> 'ok'.
maybe_timeout_winner(Srv, 'undefined') ->
    acdc_queue_listener:timeout_member_call(Srv);
maybe_timeout_winner(Srv, Winner) ->
    acdc_queue_listener:timeout_member_call(Srv, Winner).

-spec clear_member_call(state()) -> state().
clear_member_call(#state{connection_timer_ref=ConnRef
                        ,agent_ring_timer_ref=AgentRef
                        ,collect_ref=CollectRef
                        ,queue_id=QueueId
                        }=State) ->
    kz_log:put_callid(QueueId),
    maybe_stop_timer(ConnRef),
    maybe_stop_timer(AgentRef),
    maybe_stop_timer(CollectRef),
    State#state{connect_resps=[]
               ,collect_ref='undefined'
               ,member_call='undefined'
               ,connection_timer_ref='undefined'
               ,agent_ring_timer_ref='undefined'
               ,member_call_start='undefined'
               ,member_call_winners=[]
               }.

update_properties(QueueJObj, State) ->
    State#state{name = kz_json:get_value(<<"name">>, QueueJObj)
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

-spec current_call('undefined' | kapps_call:call(), kz_term:api_reference() | timeout(), kz_time:start_time()) ->
                          kz_term:api_object().
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

-spec elapsed(kz_term:api_reference() | kz_time:start_time()) -> kz_term:api_integer().
elapsed('undefined') -> 'undefined';
elapsed(Ref) when is_reference(Ref) ->
    case erlang:read_timer(Ref) of
        'false' -> 'undefined';
        Ms -> Ms div 1000
    end;
elapsed(Time) -> kz_time:elapsed_s(Time).

%%------------------------------------------------------------------------------
%% @doc If some agents are busy, the manager will tell us to delay our
%% connect reqs
%%
%% @end
%%------------------------------------------------------------------------------
-spec maybe_delay_connect_req(kapps_call:call(), kz_json:object(), gen_listener:basic_deliver(), state()) ->
                                     {'next_state', 'ready' | 'connect_req', state()}.
maybe_delay_connect_req(Call, CallJObj, Delivery, #state{listener_proc=ListenerSrv
                                                        ,manager_proc=MgrSrv
                                                        ,connection_timeout=ConnTimeout
                                                        ,connection_timer_ref=ConnRef
                                                        ,cdr_url=Url
                                                        }=State) ->
    CallId = kapps_call:call_id(Call),
    case acdc_queue_manager:up_next(MgrSrv, CallId) of
        'true' ->
            lager:debug("member call received: ~s", [CallId]),

            webseq:note(?WSD_ID, self(), 'right', [CallId, <<": member call">>]),
            webseq:evt(?WSD_ID, CallId, self(), <<"member call received">>),

            acdc_queue_listener:member_connect_req(ListenerSrv, CallJObj, Delivery, Url),

            maybe_stop_timer(ConnRef), % stop the old one, maybe

            {'next_state', 'connect_req', State#state{collect_ref=start_collect_timer()
                                                     ,member_call=Call
                                                     ,member_call_start=kz_time:start_time()
                                                     ,connection_timer_ref=start_connection_timer(ConnTimeout)
                                                     }};
        'false' ->
            lager:debug("connect_req delayed (not up next)"),
            _ = timer:apply_after(1000, 'gen_statem', 'cast', [self(), {'member_call', CallJObj, Delivery}]),
            {'next_state', 'ready', State}
    end.

%%------------------------------------------------------------------------------
%% @doc Abort a queue call between connect_reqs if agents have left the
%% building
%%
%% @end
%%------------------------------------------------------------------------------
-spec maybe_connect_re_req(pid(), pid(), state()) -> kz_types:handle_fsm_ret(state()).
maybe_connect_re_req(MgrSrv, ListenerSrv, #state{account_id=AccountId
                                                ,queue_id=QueueId
                                                ,member_call=Call
                                                }=State) ->
    case acdc_queue_manager:are_agents_available(MgrSrv) of
        'true' ->
            maybe_delay_connect_re_req(MgrSrv, ListenerSrv, State);
        'false' ->
            lager:debug("all agents have left the queue, failing call"),
            webseq:note(?WSD_ID, self(), 'right', <<"all agents have left the queue, failing call">>),
            acdc_queue_listener:exit_member_call_empty(ListenerSrv),
            acdc_stats:call_abandoned(AccountId, QueueId, kapps_call:call_id(Call), ?ABANDON_EMPTY),
            {'next_state', 'ready', clear_member_call(State), 'hibernate'}
    end.

-spec maybe_delay_connect_re_req(pid(), pid(), state()) ->
                                        {'next_state', 'connect_req', state()}.
maybe_delay_connect_re_req(MgrSrv, ListenerSrv, #state{member_call=Call}=State) ->
    CallId = kapps_call:call_id(Call),
    case acdc_queue_manager:up_next(MgrSrv, CallId) of
        'true' ->
            lager:debug("done waiting, no agents responded, let's ask again"),
            webseq:note(?WSD_ID, self(), 'right', <<"no agents responded, trying again">>),
            acdc_queue_listener:member_connect_re_req(ListenerSrv),
            {'next_state', 'connect_req', State#state{collect_ref=start_collect_timer()}};
        'false' ->
            lager:debug("connect_re_req delayed (not up next)"),
            erlang:send_after(1000, self(), {'timeout', 'undefined', ?COLLECT_RESP_MESSAGE}),
            {'next_state', 'connect_req', State#state{collect_ref='undefined'}}
    end.

-spec accept_is_for_call(kz_json:object(), kapps_call:call()) -> boolean().
accept_is_for_call(AcceptJObj, Call) ->
    kz_json:get_value(<<"Call-ID">>, AcceptJObj) =:= kapps_call:call_id(Call).

-spec update_agent(kz_json:object(), kz_json:objects()) -> kz_json:object().
update_agent(Agent, Winners) ->
    AgentProcessIDs = [ProcessId || Winner <- Winners, ProcessId <- [kz_json:get_ne_binary_value(<<"Process-ID">>, Winner)], ProcessId =/= 'undefined'],
    kz_json:set_value(<<"Agent-Process-IDs">>, AgentProcessIDs, Agent).

-spec handle_agent_responses(state()) -> {atom(), state()}.
handle_agent_responses(#state{collect_ref=Ref
                             ,manager_proc=MgrSrv
                             ,listener_proc=ListenerSrv
                             ,member_call=Call
                             ,account_id=AccountId
                             ,queue_id=QueueId
                             }=State) ->
    maybe_stop_timer(Ref),
    case acdc_queue_manager:should_ignore_member_call(MgrSrv, Call, AccountId, QueueId) of
        'true' ->
            lager:debug("queue mgr said to ignore this call: ~s, not connecting to agents", [kapps_call:call_id(Call)]),
            acdc_queue_listener:finish_member_call(ListenerSrv),
            {'ready', State};
        'false' ->
            lager:debug("done waiting for agents to respond, picking a winner"),
            maybe_pick_winner(State)
    end.

-spec maybe_pick_winner(state()) -> {atom(), state()}.
maybe_pick_winner(#state{connect_resps=CRs
                        ,listener_proc=ListenerSrv
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
        {Winners, Rest} ->
            QueueOpts = [{<<"Ring-Timeout">>, RingTimeout}
                        ,{<<"Wrapup-Timeout">>, AgentWrapup}
                        ,{<<"Caller-Exit-Key">>, CallerExitKey}
                        ,{<<"CDR-Url">>, CDRUrl}
                        ,{<<"Record-Caller">>, ShouldRecord}
                        ,{<<"Recording-URL">>, RecordUrl}
                        ,{<<"Notifications">>, Notifications}
                        ],

            ConnectWins = lists:foldl(fun(Winner, Wins) ->
                                              NewAgent = update_agent(Winner, Winners),
                                              lager:debug("sending win to ~s(~s)", [kz_json:get_value(<<"Agent-ID">>, Winner)
                                                                                   ,kz_json:get_value(<<"Process-ID">>, Winner)
                                                                                   ]),
                                              acdc_queue_listener:member_connect_win(ListenerSrv, NewAgent, QueueOpts),
                                              [NewAgent|Wins] end,
                                      [], Winners),

            {'connecting', State#state{connect_resps=Rest
                                      ,connect_wins=ConnectWins
                                      ,collect_ref='undefined'
                                      ,agent_ring_timer_ref=start_agent_ring_timer(RingTimeout)
                                      ,member_call_winners=Winners
                                      }};
        'undefined' ->
            lager:debug("no more responses to choose from"),

            acdc_queue_listener:cancel_member_call(ListenerSrv),
            {'ready', clear_member_call(State)}
    end.

-spec have_agents_responded(kz_json:objects(), kz_term:ne_binaries()) -> boolean().
have_agents_responded(Resps, Agents) ->
    lists:foldl(fun filter_agents/2, Agents, Resps) =:= [].

-spec filter_agents(kz_json:object(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
filter_agents(Resp, AgentsAcc) ->
    lists:delete(kz_json:get_value(<<"Agent-ID">>, Resp), AgentsAcc).
