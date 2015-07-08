%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @doc
%%%
%%% The queue process manages two queues
%%%   1. a private one that Agents will send member_connect_* messages
%%%      and such
%%%   2. a shared queue that member_call messages will be published to,
%%%      each consumer will be round-robined. The consumers aren't going
%%%      to auto-ack the payloads, defering that until the connection is
%%%      accepted by the agent.
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   KAZOO-3596: Sponsored by GTNetwork LLC, implemented by SIPLABS LLC
%%%-------------------------------------------------------------------
-module(acdc_queue_listener).

-behaviour(gen_listener).

%% API
-export([start_link/4
         ,accept_member_calls/1
         ,member_connect_req/4
         ,member_connect_re_req/1
         ,member_connect_win/3
         ,timeout_member_call/1, timeout_member_call/2
         ,timeout_agent/2
         ,exit_member_call/1
         ,finish_member_call/1, finish_member_call/2
         ,ignore_member_call/3
         ,cancel_member_call/1, cancel_member_call/2 ,cancel_member_call/3
         ,send_sync_req/2
         ,config/1
         ,send_sync_resp/4

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

-record(state, {
          queue_id :: ne_binary()
          ,account_id :: ne_binary()

           %% PIDs of the gang
          ,worker_sup :: pid()
          ,mgr_pid :: pid()
          ,fsm_pid :: pid()
          ,shared_pid :: pid()

           %% AMQP-related
          ,my_id :: ne_binary()
          ,my_q :: ne_binary()
          ,member_call_queue :: ne_binary()

           %% While processing a call
          ,call :: whapps_call:call()
          ,agent_id :: ne_binary()
          ,delivery :: gen_listener:basic_deliver()
         }).
-type state() :: #state{}.

-define(BINDINGS, [{'self', []}]).
-define(RESPONDERS, [{{'acdc_queue_handler', 'handle_call_event'}
                      ,[{<<"call_event">>, <<"*">>}]
                     }
                     ,{{'acdc_queue_handler', 'handle_call_event'}
                       ,[{<<"error">>, <<"*">>}]
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
                     ,{{'acdc_queue_handler', 'handle_sync_req'}
                       ,[{<<"queue">>, <<"sync_req">>}]
                      }
                    ]).

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
-spec start_link(pid(), pid(), ne_binary(), ne_binary()) -> startlink_ret().
start_link(WorkerSup, MgrPid, AccountId, QueueId) ->
    gen_listener:start_link(?MODULE
                            ,[{'bindings', [{'acdc_queue', [{'restrict_to', ['sync_req']}
                                                            ,{'account_id', AccountId}
                                                            ,{'queue_id', QueueId}
                                                           ]}
                                            | ?BINDINGS
                                           ]}
                              ,{'responders', ?RESPONDERS}
                             ]
                            ,[WorkerSup, MgrPid, AccountId, QueueId]
                           ).

-spec accept_member_calls(pid()) -> 'ok'.
accept_member_calls(Srv) ->
    gen_listener:cast(Srv, {'accept_member_calls'}).

-spec member_connect_req(pid(), wh_json:object(), _, api_binary()) -> 'ok'.
member_connect_req(Srv, MemberCallJObj, Delivery, Url) ->
    gen_listener:cast(Srv, {'member_connect_req', MemberCallJObj, Delivery, Url}).

-spec member_connect_re_req(pid()) -> 'ok'.
member_connect_re_req(Srv) ->
    gen_listener:cast(Srv, {'member_connect_re_req'}).

-spec member_connect_win(pid(), wh_json:object(), wh_proplist()) -> 'ok'.
member_connect_win(Srv, RespJObj, QueueOpts) ->
    gen_listener:cast(Srv, {'member_connect_win', RespJObj, QueueOpts}).

-spec timeout_agent(pid(), wh_json:object()) -> 'ok'.
timeout_agent(Srv, RespJObj) ->
    gen_listener:cast(Srv, {'timeout_agent', RespJObj}).

-spec timeout_member_call(pid()) -> 'ok'.
timeout_member_call(Srv) ->
    timeout_member_call(Srv, 'undefined').

-spec timeout_member_call(pid(), api_object()) -> 'ok'.
timeout_member_call(Srv, JObj) ->
    gen_listener:cast(Srv, {'timeout_member_call', JObj}).

-spec exit_member_call(pid()) -> 'ok'.
exit_member_call(Srv) ->
    gen_listener:cast(Srv, {'exit_member_call'}).

-spec finish_member_call(pid()) -> 'ok'.
-spec finish_member_call(pid(), wh_json:object()) -> 'ok'.
finish_member_call(Srv) ->
    gen_listener:cast(Srv, {'finish_member_call'}).
finish_member_call(Srv, AcceptJObj) ->
    gen_listener:cast(Srv, {'finish_member_call', AcceptJObj}).

-spec cancel_member_call(pid()) -> 'ok'.
-spec cancel_member_call(pid(), wh_json:object()) -> 'ok'.
-spec cancel_member_call(pid(), wh_json:object(), gen_listener:basic_deliver()) -> 'ok'.
cancel_member_call(Srv) ->
    gen_listener:cast(Srv, {'cancel_member_call'}).
cancel_member_call(Srv, RejectJObj) ->
    gen_listener:cast(Srv, {'cancel_member_call', RejectJObj}).
cancel_member_call(Srv, MemberCallJObj, Delivery) ->
    gen_listener:cast(Srv, {'cancel_member_call', MemberCallJObj, Delivery}).

-spec ignore_member_call(pid(), whapps_call:call(), gen_listener:basic_deliver()) -> 'ok'.
ignore_member_call(Srv, Call, Delivery) ->
    gen_listener:cast(Srv, {'ignore_member_call', Call, Delivery}).

-spec send_sync_req(pid(), ne_binary()) -> 'ok'.
send_sync_req(Srv, Type) ->
    gen_listener:cast(Srv, {'send_sync_req', Type}).

-spec config(pid()) ->
                    {ne_binary(), ne_binary()}.
config(Srv) ->
    gen_listener:call(Srv, 'config').

-spec send_sync_resp(pid(), atom(), term(), wh_json:object()) -> 'ok'.
send_sync_resp(Srv, Strategy, StrategyState, ReqJObj) ->
    gen_listener:cast(Srv, {'send_sync_resp', Strategy, StrategyState, ReqJObj}).

delivery(Srv) ->
    gen_listener:call(Srv, 'delivery').

%%%===================================================================
%%% gen_listener callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the listener
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([WorkerSup, MgrPid, AccountId, QueueId]) ->
    wh_util:put_callid(QueueId),

    lager:debug("starting queue ~s", [QueueId]),

    {'ok', QueueJObj} = couch_mgr:open_cache_doc(wh_util:format_account_id(AccountId, 'encoded')
                                                 ,QueueId
                                                ),

    gen_listener:cast(self(), {'start_friends', QueueJObj}),

    {'ok', #state{
       queue_id = QueueId
       ,account_id = AccountId
       ,my_id = acdc_util:proc_id()

       ,worker_sup = WorkerSup
       ,mgr_pid = MgrPid
      }}.

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
handle_call('delivery', _From, #state{delivery=D}=State) ->
    {'reply', D, State};
handle_call('config', _From, #state{account_id=AccountId
                                    ,queue_id=QueueId
                                   }=State) ->
    {'reply', {AccountId, QueueId}, State};
handle_call(_Request, _From, State) ->
    lager:debug("unhandled call from ~p: ~p", [_From, _Request]),
    {'reply', {'error', 'unhandled_call'}, State}.

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
find_pid_from_supervisor({'ok', P}) when is_pid(P) ->
    {'ok', P};
find_pid_from_supervisor({'error', {'already_started', P}}) when is_pid(P) ->
    {'ok', P};
find_pid_from_supervisor(E) -> E.

-spec start_shared_queue(state(), pid(), api_integer()) -> {'noreply', state()}.
start_shared_queue(#state{account_id=AccountId
                          ,queue_id=QueueId
                          ,worker_sup=WorkerSup
                         }=State, FSMPid, Priority) ->
    {'ok', SharedPid} =
        find_pid_from_supervisor(
          acdc_queue_worker_sup:start_shared_queue(WorkerSup, FSMPid, AccountId, QueueId, Priority)
         ),
    lager:debug("started shared queue listener: ~p", [SharedPid]),

    {'noreply', State#state{
                  fsm_pid = FSMPid
                  ,shared_pid = SharedPid
                  ,my_id = acdc_util:proc_id(FSMPid)
                 }}.

handle_cast({'start_friends', QueueJObj}, #state{worker_sup=WorkerSup
                                                 ,mgr_pid=MgrPid
                                                }=State) ->
    Priority = wh_json:get_integer_value(<<"max_priority">>, QueueJObj),
    case find_pid_from_supervisor(acdc_queue_worker_sup:start_fsm(WorkerSup, MgrPid, QueueJObj)) of
        {'ok', FSMPid} ->
            lager:debug("started queue FSM: ~p", [FSMPid]),
            start_shared_queue(State, FSMPid, Priority);
        {'error', 'already_present'} ->
            lager:debug("queue FSM is already present"),
            case acdc_queue_worker_sup:fsm(WorkerSup) of
                FSMPid when is_pid(FSMPid) ->
                    lager:debug("found queue FSM pid: ~p", [FSMPid]),
                    start_shared_queue(State, FSMPid, Priority);
                'undefined' ->
                    lager:debug("no queue FSM pid found"),
                    {'stop', 'failed_fsm', State}
            end
    end;
handle_cast({'gen_listener', {'created_queue', Q}}, #state{my_q='undefined'}=State) ->
    {'noreply', State#state{my_q=Q}, 'hibernate'};

handle_cast({'member_connect_req', MemberCallJObj, Delivery, _Url}
            ,#state{my_q=MyQ
                    ,my_id=MyId
                    ,account_id=AccountId
                    ,queue_id=QueueId
                   }=State) ->
    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, MemberCallJObj)),

    wh_util:put_callid(whapps_call:call_id(Call)),

    acdc_util:bind_to_call_events(Call),
    lager:debug("bound to call events for ~s", [whapps_call:call_id(Call)]),
    send_member_connect_req(whapps_call:call_id(Call), AccountId, QueueId, MyQ, MyId),

    {'noreply', State#state{call=Call
                            ,delivery=Delivery
                            ,member_call_queue=wh_json:get_value(<<"Server-ID">>, MemberCallJObj)
                           }
     ,'hibernate'};
handle_cast({'member_connect_re_req'}, #state{my_q=MyQ
                                              ,my_id=MyId
                                              ,account_id=AccountId
                                              ,queue_id=QueueId
                                              ,call=Call
                                             }=State) ->
    send_member_connect_req(whapps_call:call_id(Call), AccountId, QueueId, MyQ, MyId),
    {'noreply', State};
handle_cast({'member_connect_win', RespJObj, QueueOpts}, #state{my_q=MyQ
                                                                ,my_id=MyId
                                                                ,call=Call
                                                                ,queue_id=QueueId
                                                               }=State) ->
    lager:debug("agent process won the call, sending the win"),

    send_member_connect_win(RespJObj, Call, QueueId, MyQ, MyId, QueueOpts),
    {'noreply', State#state{agent_id=wh_json:get_value(<<"Agent-ID">>, RespJObj)}, 'hibernate'};
handle_cast({'timeout_agent', RespJObj}, #state{queue_id=QueueId
                                                ,call=Call
                                               }=State) ->
    lager:debug("timing out winning agent"),
    send_agent_timeout(RespJObj, Call, QueueId),
    {'noreply', State#state{agent_id='undefined'}, 'hibernate'};
handle_cast({'timeout_member_call', JObj}, #state{delivery=Delivery
                                                  ,call=Call
                                                  ,shared_pid=Pid
                                                  ,member_call_queue=Q
                                                  ,account_id=AccountId
                                                  ,queue_id=QueueId
                                                  ,my_id=MyId
                                                  ,agent_id=AgentId
                                                 }=State) ->
    lager:debug("member call has timed out, we're done"),

    acdc_util:unbind_from_call_events(Call),
    lager:debug("unbound from call events for ~s", [whapps_call:call_id(Call)]),

    maybe_timeout_agent(AgentId, QueueId, Call, JObj),

    acdc_queue_shared:ack(Pid, Delivery),
    send_member_call_failure(Q, AccountId, QueueId, whapps_call:call_id(Call), MyId, AgentId),

    {'noreply', clear_call_state(State), 'hibernate'};
handle_cast({'ignore_member_call', Call, Delivery}, #state{shared_pid=Pid}=State) ->
    lager:debug("ignoring member call ~s, moving on", [whapps_call:call_id(Call)]),
    acdc_util:unbind_from_call_events(Call),
    lager:debug("unbound from call events for ~s", [whapps_call:call_id(Call)]),
    acdc_queue_shared:ack(Pid, Delivery),
    {'noreply', clear_call_state(State), 'hibernate'};
handle_cast({'exit_member_call'}, #state{delivery=Delivery
                                         ,call=Call
                                         ,shared_pid=Pid
                                         ,member_call_queue=Q
                                         ,account_id=AccountId
                                         ,queue_id=QueueId
                                         ,my_id=MyId
                                         ,agent_id=AgentId
                                        }=State) ->
    lager:debug("member call has exited the queue, we're done"),

    acdc_util:unbind_from_call_events(Call),
    lager:debug("unbound from call events for ~s", [whapps_call:call_id(Call)]),
    acdc_queue_shared:ack(Pid, Delivery),
    send_member_call_failure(Q, AccountId, QueueId, whapps_call:call_id(Call), MyId, AgentId, <<"Caller exited the queue via DTMF">>),

    {'noreply', clear_call_state(State), 'hibernate'};
handle_cast({'finish_member_call'}, #state{call='undefined'}=State) ->
    {'noreply', State};
handle_cast({'finish_member_call'}, #state{delivery=Delivery
                                           ,call=Call
                                           ,shared_pid=Pid
                                           ,member_call_queue=Q
                                           ,account_id=AccountId
                                           ,queue_id=QueueId
                                           ,my_id=MyId
                                           ,agent_id=AgentId
                                          }=State) ->
    lager:debug("agent has taken care of member, we're done"),

    acdc_util:unbind_from_call_events(Call),
    lager:debug("unbound from call events for ~s", [whapps_call:call_id(Call)]),
    acdc_queue_shared:ack(Pid, Delivery),
    send_member_call_success(Q, AccountId, QueueId, MyId, AgentId, whapps_call:call_id(Call)),

    {'noreply', clear_call_state(State), 'hibernate'};
handle_cast({'finish_member_call', _AcceptJObj}, #state{delivery=Delivery
                                                        ,call=Call
                                                        ,shared_pid=Pid
                                                        ,member_call_queue=Q
                                                        ,account_id=AccountId
                                                        ,queue_id=QueueId
                                                        ,my_id=MyId
                                                        ,agent_id=AgentId
                                                       }=State) ->
    lager:debug("agent has taken care of member, we're done"),

    acdc_util:unbind_from_call_events(Call),
    lager:debug("unbound from call events for ~s", [whapps_call:call_id(Call)]),
    acdc_queue_shared:ack(Pid, Delivery),
    send_member_call_success(Q, AccountId, QueueId, MyId, AgentId, whapps_call:call_id(Call)),

    {'noreply', clear_call_state(State), 'hibernate'};
handle_cast({'cancel_member_call'}, #state{delivery='undefined'}=State) ->
    lager:debug("empty cancel member, no delivery info"),
    {'noreply', State};
handle_cast({'cancel_member_call'}, #state{delivery=Delivery
                                           ,call=Call
                                           ,shared_pid=Pid
                                          }=State) ->
    lager:debug("cancel member_call"),

    _ = maybe_nack(Call, Delivery, Pid),
    {'noreply', clear_call_state(State), 'hibernate'};
handle_cast({'cancel_member_call', _RejectJObj}, #state{delivery='undefined'}=State) ->
    lager:debug("cancel a member_call that I don't have delivery info for"),
    {'noreply', State};
handle_cast({'cancel_member_call', _RejectJObj}, #state{delivery=Delivery
                                                        ,call=Call
                                                        ,shared_pid=Pid
                                                       }=State) ->
    lager:debug("agent failed to handle the call, nack"),

    _ = maybe_nack(Call, Delivery, Pid),
    {'noreply', clear_call_state(State), 'hibernate'};
handle_cast({'cancel_member_call', _MemberCallJObj, Delivery}, #state{shared_pid=Pid}=State) ->
    lager:debug("can't handle the member_call, sending it back up"),
    acdc_queue_shared:nack(Pid, Delivery),
    {'noreply', State};
handle_cast({'send_sync_req', Type}, #state{my_q=MyQ
                                            ,my_id=MyId
                                            ,account_id=AccountId
                                            ,queue_id=QueueId
                                           }=State) ->
    send_sync_req(MyQ, MyId, AccountId, QueueId, Type),
    {'noreply', State};
handle_cast({'send_sync_resp', Strategy, StrategyState, ReqJObj}, #state{my_id=Id}=State) ->
    publish_sync_resp(Strategy, StrategyState, ReqJObj, Id),
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
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
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all messages from the message bus
%%
%% @spec handle_event(JObj, State) -> {reply, Proplist} |
%%                                   ignore
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, #state{fsm_pid=FSM}) ->
    {'reply', [{'fsm_pid', FSM}]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_listener when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_listener terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    lager:debug("ACDc queue terminating: ~p", [_Reason]).

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
-spec maybe_timeout_agent(api_object(), ne_binary(), whapps_call:call(), wh_json:object()) -> 'ok'.
maybe_timeout_agent('undefined', _QueueId, _Call, _JObj) -> 'ok';
maybe_timeout_agent(_AgentId, QueueId, Call, JObj) ->
    lager:debug("timing out winning agent because they should not be able to pick up after the queue timeout"),
    send_agent_timeout(JObj, Call, QueueId).

-spec send_member_connect_req(ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
send_member_connect_req(CallId, AccountId, QueueId, MyQ, MyId) ->
    Req = props:filter_undefined(
            [{<<"Account-ID">>, AccountId}
             ,{<<"Queue-ID">>, QueueId}
             ,{<<"Process-ID">>, MyId}
             ,{<<"Server-ID">>, MyQ}
             ,{<<"Call-ID">>, CallId}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    publish(Req, fun wapi_acdc_queue:publish_member_connect_req/1).

-spec send_member_connect_win(wh_json:object(), whapps_call:call(), ne_binary(), ne_binary(), ne_binary(), wh_proplist()) -> 'ok'.
send_member_connect_win(RespJObj, Call, QueueId, MyQ, MyId, QueueOpts) ->
    CallJSON = whapps_call:to_json(Call),
    Q = wh_json:get_value(<<"Server-ID">>, RespJObj),
    Win = props:filter_undefined(
            [{<<"Call">>, CallJSON}
             ,{<<"Process-ID">>, MyId}
             ,{<<"Agent-Process-ID">>, wh_json:get_value(<<"Agent-Process-ID">>, RespJObj)}
             ,{<<"Queue-ID">>, QueueId}
             | QueueOpts ++ wh_api:default_headers(MyQ, ?APP_NAME, ?APP_VERSION)
            ]),
    publish(Q, Win, fun wapi_acdc_queue:publish_member_connect_win/2).

-spec send_agent_timeout(wh_json:object(), whapps_call:call(), ne_binary()) -> 'ok'.
send_agent_timeout(RespJObj, Call, QueueId) ->
    Prop = [{<<"Queue-ID">>, QueueId}
            ,{<<"Call-ID">>, whapps_call:call_id(Call)}
            ,{<<"Agent-Process-ID">>, wh_json:get_value(<<"Agent-Process-ID">>, RespJObj)}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    publish(wh_json:get_value(<<"Server-ID">>, RespJObj), Prop
            ,fun wapi_acdc_queue:publish_agent_timeout/2
           ).

send_member_call_success(Q, AccountId, QueueId, MyId, AgentId, CallId) ->
    Resp = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
              ,{<<"Queue-ID">>, QueueId}
              ,{<<"Process-ID">>, MyId}
              ,{<<"Agent-ID">>, AgentId}
              ,{<<"Call-ID">>, CallId}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    publish(Q, Resp, fun wapi_acdc_queue:publish_member_call_success/2).

send_member_call_failure(Q, AccountId, QueueId, CallId, MyId, AgentId) ->
    send_member_call_failure(Q, AccountId, QueueId, CallId, MyId, AgentId, 'undefined').
send_member_call_failure(Q, AccountId, QueueId, CallId, MyId, AgentId, Reason) ->
    Resp = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
              ,{<<"Queue-ID">>, QueueId}
              ,{<<"Process-ID">>, MyId}
              ,{<<"Agent-ID">>, AgentId}
              ,{<<"Failure-Reason">>, Reason}
              ,{<<"Call-ID">>, CallId}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    publish(Q, Resp, fun wapi_acdc_queue:publish_member_call_failure/2).

send_sync_req(MyQ, MyId, AccountId, QueueId, Type) ->
    Resp = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
              ,{<<"Queue-ID">>, QueueId}
              ,{<<"Process-ID">>, MyId}
              ,{<<"Current-Strategy">>, Type}
              ,{<<"Server-ID">>, MyQ}
              | wh_api:default_headers(MyQ, ?APP_NAME, ?APP_VERSION)
             ]),
    publish(Resp, fun wapi_acdc_queue:publish_sync_req/1).

publish_sync_resp(Strategy, StrategyState, ReqJObj, Id) ->
    Resp = props:filter_undefined(
             [{<<"Account-ID">>, wh_json:get_value(<<"Account-ID">>, ReqJObj)}
              ,{<<"Queue-ID">>, wh_json:get_value(<<"Queue-ID">>, ReqJObj)}
              ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, ReqJObj)}
              ,{<<"Current-Strategy">>, wh_util:to_binary(Strategy)}
              ,{<<"Strategy-State">>, StrategyState}
              ,{<<"Process-ID">>, Id}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    publish(wh_json:get_value(<<"Server-ID">>, ReqJObj), Resp, fun wapi_acdc_queue:publish_sync_resp/2).

-spec maybe_nack(whapps_call:call(), gen_listener:basic_deliver(), pid()) -> boolean().
maybe_nack(Call, Delivery, SharedPid) ->
    case is_call_alive(Call) of
        'true' ->
            lager:debug("call is still active, nack and replay"),
            acdc_util:unbind_from_call_events(Call),
            lager:debug("unbound from call events for ~s", [whapps_call:call_id(Call)]),
            acdc_queue_shared:nack(SharedPid, Delivery),
            'true';
        'false' ->
            lager:debug("call is probably not active, ack it (so its gone)"),
            acdc_util:unbind_from_call_events(Call),
            lager:debug("unbound from call events for ~s", [whapps_call:call_id(Call)]),
            acdc_queue_shared:ack(SharedPid, Delivery),
            'false'
    end.

-spec is_call_alive(whapps_call:call() | ne_binary()) -> boolean().
is_call_alive(Call) ->
    case whapps_call_command:b_channel_status(Call) of
        {'ok', StatusJObj} ->
            lager:debug("channel is ~s", [wh_json:get_value(<<"Status">>, StatusJObj)]),
            'true';
        {'error', _E} ->
            lager:debug("failed to get status: ~p", [_E]),
            'false'
    end.

-spec clear_call_state(state()) -> state().
clear_call_state(#state{account_id=AccountId
                        ,queue_id=QueueId
                       }=State) ->
    _ = acdc_util:queue_presence_update(AccountId, QueueId),

    wh_util:put_callid(QueueId),
    State#state{call='undefined'
                ,member_call_queue='undefined'
                ,agent_id='undefined'
                ,delivery='undefined'
               }.

-spec publish(api_terms(), wh_amqp_worker:publish_fun()) -> 'ok'.
-spec publish(ne_binary(), api_terms(), fun((ne_binary(), api_terms()) -> 'ok')) -> 'ok'.
publish(Req, F) ->
    case catch F(Req) of
        'ok' -> 'ok';
        {'EXIT', _R} ->
            ST = erlang:get_stacktrace(),
            lager:debug("failed to publish message: ~p", [_R]),
            wh_util:log_stacktrace(ST),
            'ok'
    end.
publish(Q, Req, F) ->
    case catch F(Q, Req) of
        'ok' -> 'ok';
        {'EXIT', _R} ->
            ST = erlang:get_stacktrace(),
            lager:debug("failed to publish message to ~s: ~p", [Q, _R]),
            wh_util:log_stacktrace(ST),
            'ok'
    end.
