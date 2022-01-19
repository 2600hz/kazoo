%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2022, 2600Hz
%%% @doc The queue process manages two queues
%%%   1. a private one that Agents will send member_connect_* messages
%%%      and such
%%%   2. a shared queue that member_call messages will be published to,
%%%      each consumer will be round-robined. The consumers aren't going
%%%      to auto-ack the payloads, deferring that until the connection is
%%%      accepted by the agent.
%%%
%%%
%%% @author James Aimonetti
%%% @author Sponsored by GTNetwork LLC, Implemented by SIPLABS LLC
%%% @author Daniel Finke
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_queue_listener).
-behaviour(gen_listener).

%% API
-export([start_link/4
        ,member_call/3
        ,member_connect_req/1
        ,member_connect_win/3
        ,timeout_member_call/2
        ,timeout_agent/2
        ,exit_member_call/2
        ,exit_member_call_empty/1
        ,finish_member_call/1
        ,ignore_member_call/3
        ,cancel_member_call/2, cancel_member_call/3
        ,config/1

        ,delivery/1
        ]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("acdc.hrl").

-define(SERVER, ?MODULE).

-record(state, {queue_id :: kz_term:ne_binary()
               ,account_id :: kz_term:ne_binary()

               ,mgr_pid :: pid()
               ,fsm_pid :: kz_term:api_pid()
               ,shared_pid :: kz_term:api_pid()

                              %% AMQP-related
               ,my_id :: kz_term:ne_binary()
               ,my_q :: kz_term:api_ne_binary()
               ,member_call_queue :: kz_term:api_ne_binary()

                                     %% While processing a call
               ,call :: kapps_call:call() | 'undefined'
               ,agent_id :: kz_term:api_ne_binary()
               ,delivery :: gen_listener:basic_deliver() | 'undefined'
               }).
-type state() :: #state{}.

-define(BINDINGS, [{'self', []}]).
-define(RESPONDERS, [{{'acdc_queue_handler', 'handle_call_event'}
                     ,[{<<"call_event">>, <<"*">>}]
                     }
                    ,{{'acdc_queue_handler', 'handle_call_event'}
                     ,[{<<"error">>, <<"*">>}]
                     }
                    ,{{'acdc_queue_handler', 'handle_member_call_cancel'}
                     ,[{<<"member">>, <<"call_cancel">>}]
                     }
                    ,{{'acdc_queue_handler', 'handle_member_resp'}
                     ,[{<<"member">>, <<"connect_resp">>}]
                     }
                    ,{{'acdc_queue_handler', 'handle_member_accepted'}
                     ,[{<<"member">>, <<"connect_accepted">>}]
                     }
                    ,{{'acdc_queue_handler', 'handle_member_retry'}
                     ,[{<<"member">>, <<"connect_retry">>}]
                     }
                    ]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(pid(), pid(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_types:startlink_ret().
start_link(WorkerSup, MgrPid, AccountId, QueueId) ->
    gen_listener:start_link(?SERVER
                           ,[{'bindings', ?BINDINGS}
                            ,{'responders', ?RESPONDERS}
                            ]
                           ,[WorkerSup, MgrPid, AccountId, QueueId]
                           ).

-spec member_call(pid(), kz_json:object(), any()) -> 'ok'.
member_call(Srv, MemberCallJObj, Delivery) ->
    gen_listener:cast(Srv, {'member_call', MemberCallJObj, Delivery}).

-spec member_connect_req(pid()) -> 'ok'.
member_connect_req(Srv) ->
    gen_listener:cast(Srv, {'member_connect_req'}).

-spec member_connect_win(pid(), kz_json:object(), kz_term:proplist()) -> 'ok'.
member_connect_win(Srv, RespJObj, QueueOpts) ->
    gen_listener:cast(Srv, {'member_connect_win', RespJObj, QueueOpts}).

-spec timeout_agent(pid(), kz_json:object()) -> 'ok'.
timeout_agent(Srv, RespJObj) ->
    gen_listener:cast(Srv, {'timeout_agent', RespJObj}).

-spec timeout_member_call(pid(), kz_term:api_object()) -> 'ok'.
timeout_member_call(Srv, WinnerJObj) ->
    gen_listener:cast(Srv, {'timeout_member_call', WinnerJObj}).

-spec exit_member_call(pid(), kz_term:api_object()) -> 'ok'.
exit_member_call(Srv, WinnerJObj) ->
    gen_listener:cast(Srv, {'exit_member_call', WinnerJObj}).

-spec exit_member_call_empty(pid()) -> 'ok'.
exit_member_call_empty(Srv) ->
    gen_listener:cast(Srv, {'exit_member_call_empty'}).

-spec finish_member_call(pid()) -> 'ok'.
finish_member_call(Srv) ->
    gen_listener:cast(Srv, {'finish_member_call'}).

-spec cancel_member_call(pid(), kz_json:object()) -> 'ok'.
cancel_member_call(Srv, RejectJObj) ->
    gen_listener:cast(Srv, {'cancel_member_call', RejectJObj}).

-spec cancel_member_call(pid(), kz_json:object(), gen_listener:basic_deliver()) -> 'ok'.
cancel_member_call(Srv, MemberCallJObj, Delivery) ->
    gen_listener:cast(Srv, {'cancel_member_call', MemberCallJObj, Delivery}).

-spec ignore_member_call(pid(), kapps_call:call(), gen_listener:basic_deliver()) -> 'ok'.
ignore_member_call(Srv, Call, Delivery) ->
    gen_listener:cast(Srv, {'ignore_member_call', Call, Delivery}).

-spec config(pid()) ->
          {kz_term:ne_binary(), kz_term:ne_binary()}.
config(Srv) ->
    gen_listener:call(Srv, 'config').

-spec delivery(pid()) -> gen_listener:basic_deliver().
delivery(Srv) ->
    gen_listener:call(Srv, 'delivery').

%%%=============================================================================
%%% gen_listener callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the listener.
%% @end
%%------------------------------------------------------------------------------
-spec init(list()) -> {'ok', state()}.
init([WorkerSup, MgrPid, AccountId, QueueId]) ->
    kz_util:put_callid(QueueId),
    lager:debug("starting queue ~s", [QueueId]),
    gen_listener:cast(self(), {'get_friends', WorkerSup}),
    {'ok', #state{queue_id = QueueId
                 ,account_id = AccountId
                 ,my_id = acdc_util:proc_id()
                 ,mgr_pid = MgrPid
                 }}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call('delivery', _From, #state{delivery=D}=State) ->
    {'reply', D, State};
handle_call('config', _From, #state{account_id=AccountId
                                   ,queue_id=QueueId
                                   }=State) ->
    {'reply', {AccountId, QueueId}, State};
handle_call(_Request, _From, State) ->
    lager:debug("unhandled call from ~p: ~p", [_From, _Request]),
    {'reply', {'error', 'unhandled_call'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'get_friends', WorkerSup}, State) ->
    FSMPid = acdc_queue_worker_sup:fsm(WorkerSup),
    lager:debug("got queue FSM: ~p", [FSMPid]),
    SharedPid = acdc_queue_worker_sup:shared_queue(WorkerSup),
    lager:debug("got shared queue listener: ~p", [SharedPid]),
    {'noreply', State#state{fsm_pid=FSMPid
                           ,shared_pid=SharedPid
                           }};
handle_cast({'gen_listener', {'created_queue', Q}}, #state{my_q='undefined'}=State) ->
    {'noreply', State#state{my_q=Q}, 'hibernate'};

handle_cast({'member_call', MemberCallJObj, Delivery}, #state{queue_id=QueueId
                                                             ,account_id=AccountId
                                                             }=State) ->
    Call = kapps_call:from_json(kz_json:get_value(<<"Call">>, MemberCallJObj)),
    CallId = kapps_call:call_id(Call),

    kz_util:put_callid(CallId),

    acdc_util:bind_to_call_events(Call),
    lager:debug("bound to call events for ~s", [CallId]),

    %% Be ready in case a cancel comes in while queue_listener is handling call
    gen_listener:add_binding(self(), 'acdc_queue', [{'restrict_to', ['member_call_result']}
                                                   ,{'account_id', AccountId}
                                                   ,{'queue_id', QueueId}
                                                   ,{'callid', CallId}
                                                   ]),

    {'noreply', State#state{call=Call
                           ,delivery=Delivery
                           ,member_call_queue=kz_json:get_value(<<"Server-ID">>, MemberCallJObj)
                           }};

handle_cast({'member_connect_req'}, #state{queue_id=QueueId
                                          ,account_id=AccountId
                                          ,my_id=MyId
                                          ,my_q=MyQ
                                          ,call=Call
                                          }=State) ->
    send_member_connect_req(kapps_call:call_id(Call), AccountId, QueueId, MyQ, MyId),
    {'noreply', State};

handle_cast({'member_connect_win', RespJObj, QueueOpts}, #state{my_q=MyQ
                                                               ,my_id=MyId
                                                               ,call=Call
                                                               ,queue_id=QueueId
                                                               }=State) ->
    lager:debug("agent process won the call, sending the win"),

    send_member_connect_win(RespJObj, Call, QueueId, MyQ, MyId, QueueOpts),
    {'noreply', State#state{agent_id=kz_json:get_value(<<"Agent-ID">>, RespJObj)}, 'hibernate'};
handle_cast({'timeout_agent', RespJObj}, #state{queue_id=QueueId
                                               ,call=Call
                                               }=State) ->
    lager:debug("timing out winning agent"),
    send_agent_timeout(RespJObj, Call, QueueId),
    {'noreply', State#state{agent_id='undefined'}, 'hibernate'};
handle_cast({'timeout_member_call', WinnerJObj}, #state{call=Call
                                                       ,queue_id=QueueId
                                                       ,agent_id=AgentId
                                                       }=State) ->
    lager:debug("member call has timed out, we're done"),

    maybe_timeout_agent(AgentId, QueueId, Call, WinnerJObj),
    handle_call_failure(State),

    {'noreply', clear_call_state(State), 'hibernate'};
handle_cast({'ignore_member_call', Call, Delivery}, #state{shared_pid=SharedPid}=State) ->
    lager:debug("ignoring member call ~s, moving on", [kapps_call:call_id(Call)]),
    ack_and_unbind(Call, SharedPid, Delivery),
    {'noreply', clear_call_state(State), 'hibernate'};
handle_cast({'exit_member_call', WinnerJObj}, #state{call=Call
                                                    ,queue_id=QueueId
                                                    ,agent_id=AgentId
                                                    }=State) ->
    lager:debug("member call has exited the queue, we're done"),

    maybe_timeout_agent(AgentId, QueueId, Call, WinnerJObj),
    handle_call_failure(State, <<"Caller exited the queue via DTMF">>),

    {'noreply', clear_call_state(State), 'hibernate'};
handle_cast({'exit_member_call_empty'}, State) ->
    lager:debug("no agents left in queue to handle callers, kick everyone out"),

    handle_call_failure(State, <<"No agents left in queue">>),

    {'noreply', clear_call_state(State), 'hibernate'};
handle_cast({'finish_member_call'}, #state{call='undefined'}=State) ->
    {'noreply', State};
handle_cast({'finish_member_call'}, State) ->
    lager:debug("agent has taken care of member, we're done"),

    handle_call_success(State),

    {'noreply', clear_call_state(State), 'hibernate'};
handle_cast({'cancel_member_call', _RejectJObj}, #state{delivery='undefined'}=State) ->
    lager:debug("cancel a member_call that I don't have delivery info for"),
    {'noreply', State};
handle_cast({'cancel_member_call', _RejectJObj}, #state{queue_id=QueueId
                                                       ,account_id=AccountId
                                                       ,delivery=Delivery
                                                       ,call=Call
                                                       ,shared_pid=Pid
                                                       }=State) ->
    lager:debug("agent failed to handle the call, nack"),

    publish_queue_member_remove(AccountId, QueueId, kapps_call:call_id(Call)),
    _ = maybe_nack(Call, Delivery, Pid),
    {'noreply', clear_call_state(State), 'hibernate'};
handle_cast({'cancel_member_call', _MemberCallJObj, Delivery}, #state{shared_pid=Pid}=State) ->
    lager:debug("can't handle the member_call, sending it back up"),
    acdc_queue_shared:nack(Pid, Delivery),
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all messages from the message bus
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(_JObj, #state{fsm_pid=FSM}) ->
    {'reply', [{'fsm_pid', FSM}]}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_listener' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_listener' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("ACDc queue terminating: ~p", [_Reason]).

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
%% @doc Notify various listeners about success in handling a call and stop
%% tracking events for the call.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call_success(state()) -> 'ok'.
handle_call_success(#state{queue_id=QueueId
                          ,account_id=AccountId
                          ,shared_pid=SharedPid
                          ,my_id=MyId
                          ,member_call_queue=Q
                          ,call=Call
                          ,agent_id=AgentId
                          ,delivery=Delivery
                          }) ->
    ack_and_unbind(Call, SharedPid, Delivery),
    send_member_call_success(Q, AccountId, QueueId, MyId, AgentId, kapps_call:call_id(Call)).

%%------------------------------------------------------------------------------
%% @doc Notify various listeners about a failure to handle a call and stop
%% tracking events for the call.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call_failure(state()) -> 'ok'.
handle_call_failure(State) ->
    handle_call_failure(State, 'undefined').

-spec handle_call_failure(state(), kz_term:api_ne_binary()) -> 'ok'.
handle_call_failure(#state{queue_id=QueueId
                          ,account_id=AccountId
                          ,shared_pid=SharedPid
                          ,my_id=MyId
                          ,member_call_queue=Q
                          ,call=Call
                          ,agent_id=AgentId
                          ,delivery=Delivery
                          }, Reason) ->
    CallId = kapps_call:call_id(Call),
    publish_queue_member_remove(AccountId, QueueId, CallId),
    ack_and_unbind(Call, SharedPid, Delivery),
    send_member_call_failure(Q, AccountId, QueueId, CallId, MyId, AgentId, Reason).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_timeout_agent(kz_term:api_object(), kz_term:ne_binary(), kapps_call:call(), kz_term:api_object()) -> 'ok'.
maybe_timeout_agent('undefined', _QueueId, _Call, _JObj) -> 'ok';
maybe_timeout_agent(_AgentId, _QueueId, _Call, 'undefined') -> 'ok';
maybe_timeout_agent(_AgentId, QueueId, Call, JObj) ->
    send_agent_timeout(JObj, Call, QueueId).

-spec send_member_connect_req(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
send_member_connect_req(CallId, AccountId, QueueId, MyQ, MyId) ->
    Req = props:filter_undefined(
            [{<<"Account-ID">>, AccountId}
            ,{<<"Queue-ID">>, QueueId}
            ,{<<"Process-ID">>, MyId}
            ,{<<"Server-ID">>, MyQ}
            ,{<<"Call-ID">>, CallId}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    publish(Req, fun kapi_acdc_queue:publish_member_connect_req/1).

-spec send_member_connect_win(kz_json:object(), kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
send_member_connect_win(RespJObj, Call, QueueId, MyQ, MyId, QueueOpts) ->
    CallJSON = kapps_call:to_json(Call),
    Win = props:filter_undefined(
            [{<<"Call">>, CallJSON}
            ,{<<"Process-ID">>, MyId}
            ,{<<"Agent-Process-ID">>, kz_json:get_value(<<"Agent-Process-ID">>, RespJObj)}
            ,{<<"Queue-ID">>, QueueId}
            ,{<<"Agent-ID">>, kz_json:get_value(<<"Agent-ID">>, RespJObj)}
             | QueueOpts ++ kz_api:default_headers(MyQ, ?APP_NAME, ?APP_VERSION)
            ]),
    publish(Win, fun kapi_acdc_agent:publish_member_connect_win/1).

-spec send_agent_timeout(kz_json:object(), kapps_call:call(), kz_term:ne_binary()) -> 'ok'.
send_agent_timeout(RespJObj, Call, QueueId) ->
    Prop = [{<<"Queue-ID">>, QueueId}
           ,{<<"Call-ID">>, kapps_call:call_id(Call)}
           ,{<<"Agent-Process-ID">>, kz_json:get_value(<<"Agent-Process-ID">>, RespJObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    publish(kz_json:get_value(<<"Server-ID">>, RespJObj), Prop
           ,fun kapi_acdc_queue:publish_agent_timeout/2
           ).

send_member_call_success(Q, AccountId, QueueId, MyId, AgentId, CallId) ->
    Resp = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
             ,{<<"Queue-ID">>, QueueId}
             ,{<<"Process-ID">>, MyId}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Call-ID">>, CallId}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    publish(Q, Resp, fun kapi_acdc_queue:publish_member_call_success/2).

send_member_call_failure(Q, AccountId, QueueId, CallId, MyId, AgentId, Reason) ->
    Resp = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
             ,{<<"Queue-ID">>, QueueId}
             ,{<<"Process-ID">>, MyId}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Failure-Reason">>, Reason}
             ,{<<"Call-ID">>, CallId}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    publish(Q, Resp, fun kapi_acdc_queue:publish_member_call_failure/2).

-spec publish_queue_member_remove(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
publish_queue_member_remove(AccountId, QueueId, CallId) ->
    Prop = [{<<"Account-ID">>, AccountId}
           ,{<<"Queue-ID">>, QueueId}
           ,{<<"Call-ID">>, CallId}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_acdc_queue:publish_queue_member_remove(Prop).

-spec maybe_nack(kapps_call:call(), gen_listener:basic_deliver(), pid()) -> boolean().
maybe_nack(Call, Delivery, SharedPid) ->
    case is_call_alive(Call) of
        'true' ->
            lager:debug("call is still active, nack and replay"),
            acdc_util:unbind_from_call_events(Call),
            lager:debug("unbound from call events for ~s", [kapps_call:call_id(Call)]),
            acdc_queue_shared:nack(SharedPid, Delivery),
            'true';
        'false' ->
            lager:debug("call is probably not active, ack it (so its gone)"),
            ack_and_unbind(Call, SharedPid, Delivery),
            'false'
    end.

%%------------------------------------------------------------------------------
%% @doc Ack the AMQP msg delivery for a queue call and unbind from call events
%% for the call.
%% @end
%%------------------------------------------------------------------------------
-spec ack_and_unbind(kapps_call:call(), pid(), gen_listener:basic_deliver()) -> 'ok'.
ack_and_unbind(Call, SharedPid, Delivery) ->
    acdc_util:unbind_from_call_events(Call),
    lager:debug("unbound from call events for ~s", [kapps_call:call_id(Call)]),
    acdc_queue_shared:ack(SharedPid, Delivery).

-spec is_call_alive(kapps_call:call() | kz_term:ne_binary()) -> boolean().
is_call_alive(Call) ->
    case kapps_call_command:b_channel_status(Call) of
        {'ok', StatusJObj} ->
            lager:debug("channel is ~s", [kz_json:get_value(<<"Status">>, StatusJObj)]),
            'true';
        {'error', _E} ->
            lager:debug("failed to get status: ~p", [_E]),
            'false'
    end.

-spec clear_call_state(state()) -> state().
clear_call_state(#state{call=Call
                       ,account_id=AccountId
                       ,queue_id=QueueId
                       }=State) ->
    _ = acdc_util:queue_presence_update(AccountId, QueueId),

    case Call of
        'undefined' -> 'ok';
        _ ->
            CallId = kapps_call:call_id(Call),
            gen_listener:rm_binding(self(), 'acdc_queue', [{'restrict_to', ['member_call_result']}
                                                          ,{'account_id', AccountId}
                                                          ,{'queue_id', QueueId}
                                                          ,{'callid', CallId}
                                                          ])
    end,

    kz_util:put_callid(QueueId),
    State#state{call='undefined'
               ,member_call_queue='undefined'
               ,agent_id='undefined'
               ,delivery='undefined'
               }.

-spec publish(kz_term:api_terms(), kz_amqp_worker:publish_fun()) -> 'ok'.
publish(Req, F) ->
    try F(Req)
    catch _E:_R ->
            ST = erlang:get_stacktrace(),
            lager:debug("failed to publish message: ~p:~p", [_E, _R]),
            kz_util:log_stacktrace(ST),
            'ok'
    end.

-spec publish(kz_term:ne_binary(), kz_term:api_terms(), fun((kz_term:ne_binary(), kz_term:api_terms()) -> 'ok')) -> 'ok'.
publish(Q, Req, F) ->
    try F(Q, Req)
    catch _E:_R ->
            ST = erlang:get_stacktrace(),
            lager:debug("failed to publish message to ~s: ~p:~p", [Q, _E, _R]),
            kz_util:log_stacktrace(ST),
            'ok'
    end.
