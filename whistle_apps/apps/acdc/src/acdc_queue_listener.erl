%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
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
%%%-------------------------------------------------------------------
-module(acdc_queue_listener).

-behaviour(gen_listener).

%% API
-export([start_link/4
         ,accept_member_calls/1
         ,member_connect_req/3
         ,member_connect_re_req/1
         ,member_connect_win/4, member_connect_win/5
         ,timeout_member_call/1
         ,exit_member_call/1
         ,finish_member_call/1, finish_member_call/2
         ,ignore_member_call/3
         ,cancel_member_call/1, cancel_member_call/2 ,cancel_member_call/3
         ,send_sync_req/2
         ,config/1
         ,send_sync_resp/4
        ]).

%% Call Manipulation
-export([put_member_on_hold/2, put_member_on_hold/3]).

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
         ,acct_id :: ne_binary()
         ,moh :: ne_binary()

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
         ,agent_process :: ne_binary()
         ,agent_id :: ne_binary()
         ,delivery :: #'basic.deliver'{}
         }).

-define(BINDINGS, [{self, []}]).
-define(RESPONDERS, [{{acdc_queue_handler, handle_call_event}
                      ,[{<<"call_event">>, <<"*">>}]
                     }
                     ,{{acdc_queue_handler, handle_call_event}
                       ,[{<<"error">>, <<"*">>}]
                      }
                     ,{{acdc_queue_handler, handle_member_resp}
                       ,[{<<"member">>, <<"connect_resp">>}]
                      }
                     ,{{acdc_queue_handler, handle_member_accepted}
                       ,[{<<"member">>, <<"connect_accepted">>}]
                      }
                     ,{{acdc_queue_handler, handle_member_retry}
                       ,[{<<"member">>, <<"connect_retry">>}]
                      }
                     ,{{acdc_queue_handler, handle_sync_req}
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
-spec start_link/4 :: (pid(), pid(), ne_binary(), ne_binary()) -> startlink_ret().
start_link(WorkerSup, MgrPid, AcctId, QueueId) ->
    gen_listener:start_link(?MODULE
                            ,[{bindings, [{acdc_queue, [{restrict_to, [sync_req]}
                                                        ,{account_id, AcctId}
                                                        ,{queue_id, QueueId}
                                                       ]}
                                          | ?BINDINGS
                                         ]}
                              ,{responders, ?RESPONDERS}
                             ]
                            ,[WorkerSup, MgrPid, AcctId, QueueId]
                           ).

accept_member_calls(Srv) ->
    gen_listener:cast(Srv, {accept_member_calls}).

member_connect_req(Srv, MemberCallJObj, Delivery) ->
    gen_listener:cast(Srv, {member_connect_req, MemberCallJObj, Delivery}).
member_connect_re_req(Srv) ->
    gen_listener:cast(Srv, {member_connect_re_req}).

member_connect_win(Srv, RespJObj, RingTimeout, AgentWrapup) ->
    member_connect_win(Srv, RespJObj, RingTimeout, AgentWrapup, <<"#">>).
member_connect_win(Srv, RespJObj, RingTimeout, AgentWrapup, CallerExitKey) ->
    gen_listener:cast(Srv, {member_connect_win, RespJObj, RingTimeout, AgentWrapup, CallerExitKey}).

timeout_member_call(Srv) ->
    gen_listener:cast(Srv, {timeout_member_call}).
exit_member_call(Srv) ->
    gen_listener:cast(Srv, {exit_member_call}).

finish_member_call(Srv) ->
    gen_listener:cast(Srv, {finish_member_call}).
finish_member_call(Srv, AcceptJObj) ->
    gen_listener:cast(Srv, {finish_member_call, AcceptJObj}).

cancel_member_call(Srv) ->
    gen_listener:cast(Srv, {cancel_member_call}).
cancel_member_call(Srv, RejectJObj) ->
    gen_listener:cast(Srv, {cancel_member_call, RejectJObj}).
cancel_member_call(Srv, MemberCallJObj, Delivery) ->
    gen_listener:cast(Srv, {cancel_member_call, MemberCallJObj, Delivery}).

ignore_member_call(Srv, Call, Delivery) ->
    gen_listener:cast(Srv, {ignore_member_call, Call, Delivery}).

-spec send_sync_req/2 :: (pid(), queue_strategy()) -> 'ok'.
send_sync_req(Srv, Type) ->
    gen_listener:cast(Srv, {send_sync_req, Type}).

config(Srv) ->
    gen_listener:call(Srv, config).

put_member_on_hold(Srv, Call) ->
    gen_listener:cast(Srv, {put_member_on_hold, Call}).
put_member_on_hold(Srv, Call, MOH) ->
    gen_listener:cast(Srv, {put_member_on_hold, Call, MOH}).

send_sync_resp(Srv, Strategy, StrategyState, ReqJObj) ->
    gen_listener:cast(Srv, {send_sync_resp, Strategy, StrategyState, ReqJObj}).

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
init([WorkerSup, MgrPid, AcctId, QueueId]) ->
    put(callid, QueueId),

    lager:debug("starting queue ~s", [QueueId]),

    {ok, QueueJObj} = couch_mgr:open_cache_doc(wh_util:format_account_id(AcctId, encoded)
                                               ,QueueId
                                              ),

    fetch_my_queue(),

    gen_listener:cast(self(), {start_friends, QueueJObj}),

    {ok, #state{
       queue_id = QueueId
       ,acct_id = AcctId
       ,my_id = acdc_util:proc_id()
       ,moh = wh_json:get_value(<<"moh">>, QueueJObj)

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
handle_call(config, _From, #state{acct_id=AcctId
                                  ,queue_id=QueueId
                                  }=State) ->
    {reply, {AcctId, QueueId}, State};
handle_call(_Request, _From, State) ->
    lager:debug("unhandled call from ~p: ~p", [_From, _Request]),
    {reply, {error, unhandled_call}, State}.

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
find_pid_from_supervisor({ok, P}) when is_pid(P) -> {ok, P};
find_pid_from_supervisor({error, {already_started, P}}) when is_pid(P) -> {ok, P};
find_pid_from_supervisor(E) -> E.

handle_cast({start_friends, QueueJObj}, #state{worker_sup=WorkerSup
                                               ,mgr_pid=MgrPid
                                               ,acct_id=AcctId
                                               ,queue_id=QueueId
                                              }=State) ->
    case find_pid_from_supervisor(acdc_queue_worker_sup:start_fsm(WorkerSup, MgrPid, QueueJObj)) of
        {ok, FSMPid} ->
            lager:debug("started queue FSM: ~p", [FSMPid]),
            {ok, SharedPid} = find_pid_from_supervisor(
                                acdc_queue_worker_sup:start_shared_queue(WorkerSup, FSMPid, AcctId, QueueId)
                               ),
            lager:debug("started shared queue listener: ~p", [SharedPid]),

            {noreply, State#state{
                        fsm_pid = FSMPid
                        ,shared_pid = SharedPid
                       }};
        {error, already_present} ->
            lager:debug("queue FSM is already present"),
            case acdc_queue_worker_sup:fsm(WorkerSup) of
                FSMPid when is_pid(FSMPid) ->
                    lager:debug("found queue FSM pid: ~p", [FSMPid]),
                    {ok, SharedPid} = find_pid_from_supervisor(
                                        acdc_queue_worker_sup:start_shared_queue(WorkerSup, FSMPid, AcctId, QueueId)
                                       ),

                    lager:debug("started shared queue listener: ~p", [SharedPid]),

                    {noreply, State#state{
                                fsm_pid = FSMPid
                                ,shared_pid = SharedPid
                       }};
                undefined ->
                    lager:debug("no queue FSM pid found"),
                    {stop, failed_fsm, State}
            end
    end;
handle_cast({queue_name, undefined}, State) ->
    fetch_my_queue(),
    {noreply, State};
handle_cast({queue_name, Q}, #state{my_q=undefined}=State) ->
    lager:debug("my queue: ~s", [Q]),
    {noreply, State#state{my_q=Q}, hibernate};

handle_cast({member_connect_req, MemberCallJObj, Delivery}, #state{my_q=MyQ
                                                                   ,my_id=MyId
                                                                   ,acct_id=AcctId
                                                                   ,queue_id=QueueId
                                                                  }=State
           ) ->
    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, MemberCallJObj)),

    put(callid, whapps_call:call_id(Call)),

    acdc_util:bind_to_call_events(Call),
    send_member_connect_req(whapps_call:call_id(Call), AcctId, QueueId, MyQ, MyId),

    {noreply, State#state{call=Call
                          ,delivery=Delivery
                          ,member_call_queue=wh_json:get_value(<<"Server-ID">>, MemberCallJObj)
                         }
    ,hibernate};

handle_cast({member_connect_re_req}, #state{my_q=MyQ
                                            ,my_id=MyId
                                            ,acct_id=AcctId
                                            ,queue_id=QueueId
                                            ,call=Call
                                           }=State
           ) ->
    case is_call_alive(Call) of
        true ->
            send_member_connect_req(whapps_call:call_id(Call), AcctId, QueueId, MyQ, MyId);
        false ->
            acdc_queue_listener:finish_member_call(self())
    end,
    {noreply, State};

handle_cast({member_connect_win, RespJObj, RingTimeout, AgentWrapup, CallerExitKey}, #state{my_q=MyQ
                                                                                            ,my_id=MyId
                                                                                            ,call=Call
                                                                                            ,queue_id=QueueId
                                                                                           }=State) ->
    lager:debug("agent process won the call, sending the win"),

    send_member_connect_win(RespJObj, RingTimeout, AgentWrapup, Call, QueueId, MyQ, MyId, CallerExitKey),
    {noreply, State#state{agent_process=wh_json:get_value(<<"Agent-Process-ID">>, RespJObj)
                          ,agent_id=wh_json:get_value(<<"Agent-ID">>, RespJObj)
                         }
    ,hibernate};

handle_cast({timeout_member_call}, #state{delivery=Delivery
                                          ,call=Call
                                          ,shared_pid=Pid
                                          ,member_call_queue=Q
                                          ,acct_id=AcctId
                                          ,queue_id=QueueId
                                          ,my_id=MyId
                                          ,agent_id=AgentId
                                         }=State) ->
    lager:debug("member call has timed out, we're done"),

    acdc_util:unbind_from_call_events(Call),
    acdc_queue_shared:ack(Pid, Delivery),
    send_member_call_failure(Q, AcctId, QueueId, MyId, AgentId),

    {noreply, clear_call_state(State), hibernate};

handle_cast({ignore_member_call, Call, Delivery}, #state{shared_pid=Pid}=State) ->
    lager:debug("ignoring member call ~s, moving on", [whapps_call:call_id(Call)]),
    acdc_util:unbind_from_call_events(Call),
    acdc_queue_shared:ack(Pid, Delivery),
    {noreply, clear_call_state(State), hibernate};

handle_cast({exit_member_call}, #state{delivery=Delivery
                                       ,call=Call
                                       ,shared_pid=Pid
                                       ,member_call_queue=Q
                                       ,acct_id=AcctId
                                       ,queue_id=QueueId
                                       ,my_id=MyId
                                       ,agent_id=AgentId
                                      }=State) ->
    lager:debug("member call has exited the queue, we're done"),

    acdc_util:unbind_from_call_events(Call),
    acdc_queue_shared:ack(Pid, Delivery),
    send_member_call_failure(Q, AcctId, QueueId, MyId, AgentId, <<"Caller exited the queue via DTMF">>),

    {noreply, clear_call_state(State), hibernate};

handle_cast({finish_member_call}, #state{delivery=Delivery
                                         ,call=Call
                                         ,shared_pid=Pid
                                         ,member_call_queue=Q
                                         ,acct_id=AcctId
                                         ,queue_id=QueueId
                                         ,my_id=MyId
                                         ,agent_id=AgentId
                                        }=State) ->
    lager:debug("agent has taken care of member, we're done"),

    acdc_util:unbind_from_call_events(Call),
    acdc_queue_shared:ack(Pid, Delivery),
    send_member_call_success(Q, AcctId, QueueId, MyId, AgentId),

    {noreply, clear_call_state(State), hibernate};
handle_cast({finish_member_call, _AcceptJObj}, #state{delivery=Delivery
                                                      ,call=Call
                                                      ,shared_pid=Pid
                                                      ,member_call_queue=Q
                                                      ,acct_id=AcctId
                                                      ,queue_id=QueueId
                                                      ,my_id=MyId
                                                      ,agent_id=AgentId
                                                     }=State) ->
    lager:debug("agent has taken care of member, we're done"),

    acdc_util:unbind_from_call_events(Call),
    acdc_queue_shared:ack(Pid, Delivery),
    send_member_call_success(Q, AcctId, QueueId, MyId, AgentId),

    {noreply, clear_call_state(State), hibernate};

handle_cast({cancel_member_call}, #state{delivery=undefined}=State) ->
    lager:debug("empty cancel member, no delivery info"),
    {noreply, State};
handle_cast({cancel_member_call}, #state{delivery=Delivery
                                         ,call=Call
                                         ,shared_pid=Pid
                                        }=State) ->
    lager:debug("cancel member_call"),

    _ = maybe_nack(Call, Delivery, Pid),
    {noreply, clear_call_state(State), hibernate};

handle_cast({cancel_member_call, _RejectJObj}, #state{delivery=undefined}=State) ->
    lager:debug("cancel a member_call that I don't have delivery info for"),
    {noreply, State};
handle_cast({cancel_member_call, _RejectJObj}, #state{delivery = #'basic.deliver'{}=Delivery
                                                      ,call=Call
                                                      ,shared_pid=Pid
                                                     }=State) ->
    lager:debug("agent failed to handle the call, nack"),

    _ = maybe_nack(Call, Delivery, Pid),
    {noreply, clear_call_state(State), hibernate};

handle_cast({cancel_member_call, _MemberCallJObj, Delivery}, #state{shared_pid=Pid}=State) ->
    lager:debug("can't handle the member_call, sending it back up"),
    acdc_queue_shared:nack(Pid, Delivery),
    {noreply, State};

handle_cast({send_sync_req, _Type}=Msg, #state{my_q=MyQ}=State) when MyQ =:= 'undefined' orelse MyQ =:= <<>> ->
    fetch_my_queue(),
    lager:debug("replaying ~p, hopefully we have our queue by then", [Msg]),
    gen_listener:cast(self(), Msg),
    {noreply, State};

handle_cast({send_sync_req, Type}, #state{my_q=MyQ
                                          ,my_id=MyId
                                          ,acct_id=AcctId
                                          ,queue_id=QueueId
                                         }=State) ->
    send_sync_req(MyQ, MyId, AcctId, QueueId, Type),
    {noreply, State};

handle_cast({put_member_on_hold, Call}, #state{moh=MOH}=State) ->
    handle_cast({put_member_on_hold, Call, MOH}, State);
handle_cast({put_member_on_hold, Call, MOH}, State) ->
    whapps_call_command:answer(Call),
    whapps_call_command:hold(MOH, Call),
    {noreply, State};

handle_cast({send_sync_resp, Strategy, StrategyState, ReqJObj}, #state{my_id=Id}=State) ->
    publish_sync_resp(Strategy, StrategyState, ReqJObj, Id),
    {noreply, State};

handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {noreply, State}.

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
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {noreply, State}.

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
    {reply, [{fsm_pid, FSM}]}.

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
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec send_member_connect_req/5 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
send_member_connect_req(CallId, AcctId, QueueId, MyQ, MyId) ->
    lager:debug("sending req via ~s", [MyQ]),
    Req = props:filter_undefined(
            [{<<"Account-ID">>, AcctId}
             ,{<<"Queue-ID">>, QueueId}
             ,{<<"Process-ID">>, MyId}
             ,{<<"Server-ID">>, MyQ}
             ,{<<"Call-ID">>, CallId}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    publish(Req, fun wapi_acdc_queue:publish_member_connect_req/1).

-spec send_member_connect_win/8 :: (wh_json:object(), pos_integer(), pos_integer(), whapps_call:call(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
send_member_connect_win(RespJObj, RingTimeout, AgentWrapup, Call, QueueId, MyQ, MyId, CallerExitKey) ->
    CallJSON = whapps_call:to_json(Call),
    Q = wh_json:get_value(<<"Server-ID">>, RespJObj),
    Win = props:filter_undefined(
            [{<<"Call">>, CallJSON}
             ,{<<"Process-ID">>, MyId}
             ,{<<"Agent-Process-ID">>, wh_json:get_value(<<"Agent-Process-ID">>, RespJObj)}
             ,{<<"Queue-ID">>, QueueId}
             ,{<<"Ring-Timeout">>, RingTimeout}
             ,{<<"Wrapup-Timeout">>, AgentWrapup}
             ,{<<"Caller-Exit-Key">>, CallerExitKey}
             | wh_api:default_headers(MyQ, ?APP_NAME, ?APP_VERSION)
            ]),
    publish(Q, Win, fun wapi_acdc_queue:publish_member_connect_win/2).

send_member_call_success(Q, AcctId, QueueId, MyId, AgentId) ->
    Resp = props:filter_undefined(
             [{<<"Account-ID">>, AcctId}
              ,{<<"Queue-ID">>, QueueId}
              ,{<<"Process-ID">>, MyId}
              ,{<<"Agent-ID">>, AgentId}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    publish(Q, Resp, fun wapi_acdc_queue:publish_member_call_success/2).

send_member_call_failure(Q, AcctId, QueueId, MyId, AgentId) ->
    send_member_call_failure(Q, AcctId, QueueId, MyId, AgentId, undefined).
send_member_call_failure(Q, AcctId, QueueId, MyId, AgentId, Reason) ->
    Resp = props:filter_undefined(
             [{<<"Account-ID">>, AcctId}
              ,{<<"Queue-ID">>, QueueId}
              ,{<<"Process-ID">>, MyId}
              ,{<<"Agent-ID">>, AgentId}
              ,{<<"Failure-Reason">>, Reason}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    publish(Q, Resp, fun wapi_acdc_queue:publish_member_call_failure/2).

send_sync_req(MyQ, MyId, AcctId, QueueId, Type) ->
    Resp = props:filter_undefined(
             [{<<"Account-ID">>, AcctId}
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

-spec maybe_nack/3 :: (whapps_call:call(), #'basic.deliver'{}, pid()) -> boolean().
maybe_nack(Call, Delivery, SharedPid) ->
    case is_call_alive(Call) of
        true ->
            lager:debug("call is still active, nack and replay"),
            acdc_util:unbind_from_call_events(Call),
            acdc_queue_shared:nack(SharedPid, Delivery),
            true;
        false ->
            lager:debug("call is probably not active, ack it (so its gone)"),
            acdc_util:unbind_from_call_events(Call),
            acdc_queue_shared:ack(SharedPid, Delivery),
            false
    end.

-spec is_call_alive/1 :: (whapps_call:call() | ne_binary()) -> boolean().
is_call_alive(Call) ->
    case whapps_call_command:b_call_status(Call) of
        {ok, _} -> true;
        {error, _} -> false
    end.

clear_call_state(#state{acct_id=AcctId
                        ,queue_id=QueueId
                       }=State) ->
    _ = acdc_util:queue_presence_update(AcctId, QueueId),

    put(callid, QueueId),
    State#state{call=undefined
                ,member_call_queue=undefined
                ,agent_id=undefined
                ,delivery=undefined
                }.

-spec publish/2 :: (api_terms(), wh_amqp_worker:publish_fun()) -> 'ok'.
-spec publish/3 :: (ne_binary(), api_terms(), fun((ne_binary(), api_terms()) -> 'ok')) -> 'ok'.
publish(Req, F) ->
    case catch F(Req) of
        'ok' -> 'ok';
        {'EXIT', _R} ->
            ST = erlang:get_stacktrace(),
            lager:debug("failed to publish message: ~p", [_R]),
            _ = [lager:debug("st: ~p", [S]) || S <- ST],
            ok
    end.
publish(Q, Req, F) ->
    case catch F(Q, Req) of
        'ok' -> 'ok';
        {'EXIT', _R} ->
            ST = erlang:get_stacktrace(),
            lager:debug("failed to publish message to ~s: ~p", [Q, _R]),
            _ = [lager:debug("st: ~p", [S]) || S <- ST],
            ok
    end.

fetch_my_queue() ->
    Self = self(),
    _ = spawn(fun() -> gen_listener:cast(Self, {queue_name, gen_listener:queue_name(Self)}) end),
    ok.
