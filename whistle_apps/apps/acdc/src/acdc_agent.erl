%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_agent).

-behaviour(gen_listener).

%% API
-export([start_link/2
         ,member_connect_resp/2
         ,member_connect_retry/2
         ,bridge_to_member/3
         ,monitor_call/2
         ,member_connect_accepted/1
         ,channel_hungup/2
         ,originate_execute/2
         ,join_agent/2
         ,send_sync_req/1
         ,send_sync_resp/3, send_sync_resp/4
         ,config/1
         ,send_status_resume/1
         ,add_acdc_queue/2
         ,rm_acdc_queue/2
         ,get_recording_doc_id/1
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
         call :: whapps_call:call()
         ,acdc_queue_id :: ne_binary() % the ACDc Queue ID
         ,msg_queue_id :: ne_binary() % the AMQP Queue ID of the ACDc Queue process
         ,agent_id :: ne_binary()
         ,acct_db :: ne_binary()
         ,acct_id :: ne_binary()
         ,fsm_pid :: pid()
         ,agent_queues :: [ne_binary(),...] | []
         ,last_connect :: wh_now() % last connection
         ,last_attempt :: wh_now() % last attempt to connect
         ,my_id :: ne_binary()
         ,my_q :: ne_binary() % AMQP queue name
         ,timer_ref :: reference()
         ,sync_resp :: wh_json:json_object() % furthest along resp
         ,supervisor :: pid()
         ,record_calls = false :: boolean()
         }).

%%%===================================================================
%%% Defines for different functionality
%%%===================================================================

%% On init, an aget process sends a sync_req and waits SYNC_TIMER_TIMEOUT ms
%% The agent process checks its list of received
-define(SYNC_TIMER_MESSAGE, sync_timeout).
-define(SYNC_TIMER_TIMEOUT, 5000).

%% After receiving sync_resp, if the resp status requires waiting, SYNC_WAIT_TIMER_TIMEOUT
%% pauses the agent process, then restarts the sync process (send sync_req, start
%% SYNC_TIMER_TIMEOUT, collect sync_resp(s), etc
-define(SYNC_WAIT_TIMER_MESSAGE, sync_wait_timeout).
-define(SYNC_WAIT_TIMER_TIMEOUT, 5000).

%% When in the wrapup status, how long does an agent wait before going back to ready
-define(WRAPUP_TIMER_MESSAGE, wrapup_timeout).
-define(WRAPUP_TIMER_TIMEOUT, 60000).

%% When an agent is paused (on break, logged out, etc)
-define(PAUSED_TIMER_MESSAGE, paused_timeout).

-define(BINDINGS(AcctId, AgentId), [{self, []}
                                    ,{acdc_agent, [{account_id, AcctId}
                                                   ,{agent_id, AgentId}
                                                   ,{restrict_to, [sync, stats_req]}
                                                  ]}
                                   ]).

-define(RESPONDERS, [{{acdc_agent_handler, handle_sync_req}
                      ,[{<<"agent">>, <<"sync_req">>}]
                      }
                     ,{{acdc_agent_handler, handle_sync_resp}
                       ,[{<<"agent">>, <<"sync_resp">>}]
                      }
                     ,{{acdc_agent_handler, handle_stats_req}
                       ,[{<<"agent">>, <<"stats_req">>}]
                      }
                     ,{{acdc_agent_handler, handle_call_event}
                       ,[{<<"call_event">>, <<"*">>}]
                      }
                     ,{{acdc_agent_handler, handle_call_event}
                       ,[{<<"error">>, <<"*">>}]
                      }
                     ,{{acdc_agent_handler, handle_member_message}
                       ,[{<<"member">>, <<"*">>}]
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
start_link(Supervisor, AgentJObj) ->
    AgentId = wh_json:get_value(<<"_id">>, AgentJObj),

    case wh_json:get_value(<<"queues">>, AgentJObj) of
        undefined ->
            lager:debug("agent ~s has no queues, ignoring", [AgentId]),
            ignore;
        [] ->
            lager:debug("agent ~s in ~s has no queues, ignoring"),
            ignore;
        Queues ->
            AcctId = wh_json:get_value(<<"pvt_account_id">>, AgentJObj),
            gen_listener:start_link(?MODULE
                                    ,[{bindings, ?BINDINGS(AcctId, AgentId)}
                                      ,{responders, ?RESPONDERS}
                                     ]
                                    ,[Supervisor, AgentJObj, Queues]
                                   )
    end.

-spec member_connect_resp/2 :: (pid(), wh_json:json_object()) -> 'ok'.
member_connect_resp(Srv, ReqJObj) ->
    gen_listener:cast(Srv, {member_connect_resp, ReqJObj}).

member_connect_retry(Srv, WinJObj) ->
    gen_listener:cast(Srv, {member_connect_retry, WinJObj}).

member_connect_accepted(Srv) ->
    gen_listener:cast(Srv, member_connect_accepted).

bridge_to_member(Srv, WinJObj, EPs) ->
    gen_listener:cast(Srv, {bridge_to_member, WinJObj, EPs}).

monitor_call(Srv, MonitorJObj) ->
    gen_listener:cast(Srv, {monitor_call, MonitorJObj}).

-spec channel_hungup/2 :: (pid(), ne_binary()) -> 'ok'.
channel_hungup(Srv, CallId) ->
    gen_listener:cast(Srv, {channel_hungup, CallId}).

originate_execute(Srv, JObj) ->
    gen_listener:cast(Srv, {originate_execute, JObj}).

join_agent(Srv, ACallId) ->
    gen_listener:cast(Srv, {join_agent, ACallId}).

send_sync_req(Srv) ->
    gen_listener:cast(Srv, {send_sync_req}).

send_sync_resp(Srv, Status, ReqJObj) ->
    send_sync_resp(Srv, Status, ReqJObj, []).
send_sync_resp(Srv, Status, ReqJObj, Options) ->
    gen_listener:cast(Srv, {send_sync_resp, Status, ReqJObj, Options}).

-spec config/1 :: (pid()) -> {ne_binary(), ne_binary()}.
config(Srv) ->
    gen_listener:call(Srv, config).

send_status_resume(Srv) ->
    gen_listener:cast(Srv, {send_status_update, resume}).

add_acdc_queue(Srv, Q) ->
    gen_listener:cast(Srv, {queue_login, Q}).

rm_acdc_queue(Srv, Q) ->
    gen_listener:cast(Srv, {queue_logout, Q}).

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
init([Supervisor, AgentJObj, Queues]) ->
    AgentId = wh_json:get_value(<<"_id">>, AgentJObj),
    put(callid, AgentId),

    Self = self(),
    AcctId = wh_json:get_value(<<"pvt_account_id">>, AgentJObj),

    _ = spawn(fun() ->
                      put(amqp_publish_as, Self),

                      gen_listener:cast(Self, {queue_name, gen_listener:queue_name(Self)}),

                      Prop = [{<<"Account-ID">>, AcctId}
                              ,{<<"Agent-ID">>, AgentId}
                              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                             ],
                      [wapi_acdc_queue:publish_agent_available([{<<"Queue-ID">>, QueueId} | Prop]) || QueueId <- Queues]
              end),

    {ok, #state{
       agent_id = AgentId
       ,acct_id = AcctId
       ,acct_db = wh_json:get_value(<<"pvt_account_db">>, AgentJObj)
       ,agent_queues = Queues
       ,my_id = acdc_util:proc_id()
       ,supervisor = Supervisor
       ,record_calls = wh_json:is_true(<<"record_calls">>, AgentJObj, false)
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
                                  ,agent_id=AgentId
                                  }=State) ->
    {reply, {AcctId, AgentId}, State};
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
handle_cast({start_fsm, Supervisor}, #state{acct_id=AcctId
                                            ,agent_id=AgentId
                                           }=State) ->
    {ok, FSMPid} = acdc_agent_sup:start_fsm(Supervisor, AcctId, AgentId),

    lager:debug("started FSM at ~p", [FSMPid]),

    gen_listener:cast(self(), bind_to_member_reqs),

    {noreply, State#state{fsm_pid=FSMPid}};
handle_cast({queue_name, Q}, State) ->
    lager:debug("my queue: ~s", [Q]),
    {noreply, State#state{my_q=Q}};

handle_cast({queue_login, Q}, #state{agent_queues=Qs
                                     ,acct_id=AcctId
                                    }=State) ->
    case lists:member(Q, Qs) of
        true ->
            lager:debug("already logged into queue ~s", [Q]),
            {noreply, State};
        false ->
            lager:debug("adding binding (logging in) to queue ~s", [Q]),
            login_to_queue(AcctId, Q),
            {noreply, State#state{agent_queues=[Q|Qs]}}
    end;

handle_cast({queue_logout, Q}, #state{agent_queues=Qs
                                      ,acct_id=AcctId
                                     }=State) ->
    case lists:member(Q, Qs) of
        true ->
            lager:debug("removing binding (logging out) from queue ~s", [Q]),
            logout_from_queue(AcctId, Q),
            {noreply, State#state{agent_queues=lists:delete(Q, Qs)}};
        false ->
            lager:debug("not logged into queue ~s", [Q]),
            {noreply, State}
    end;

handle_cast(bind_to_member_reqs, #state{agent_queues=Qs
                                        ,acct_id=AcctId
                                       }=State) ->
    lager:debug("binding to queues: ~p", [Qs]),
    _ = [login_to_queue(AcctId, Q) || Q <- Qs],
    {noreply, State};

handle_cast({channel_hungup, CallId}, #state{call=Call
                                             ,record_calls=ShouldRecord
                                            }=State) ->
    case call_id(Call) of
        CallId ->
            lager:debug("member channel hungup, done with this call"),
            acdc_util:unbind_from_call_events(Call),

            maybe_stop_recording(Call, ShouldRecord),

            {noreply, State#state{call=undefined
                                  ,msg_queue_id=undefined
                                  ,acdc_queue_id=undefined
                                 }};
        undefined ->
            lager:debug("undefined call id for channel_hungup, ignoring"),
            {noreply, State};
        _ ->
            lager:debug("other channel ~s hungup", [CallId]),
            acdc_util:unbind_from_call_events(CallId),
            maybe_stop_recording(Call, ShouldRecord),
            {noreply, State}
    end;

handle_cast(member_connect_accepted, #state{msg_queue_id=AmqpQueue
                                            ,call=Call
                                            ,acct_id=AcctId
                                            ,agent_id=AgentId
                                            ,my_id=MyId
                                   }=State) ->
    lager:debug("member bridged to agent!"),
    send_member_connect_accepted(AmqpQueue, call_id(Call), AcctId, AgentId, MyId),
    {noreply, State};

handle_cast({member_connect_resp, _}=Msg, #state{my_q = Q}=State) when
      Q =:= undefined orelse Q =:= <<>> ->
    fetch_my_queue(),
    lager:debug("replaying ~p, hopefully we have our queue by then", [Msg]),
    gen_listener:cast(self(), Msg),
    {noreply, State};
handle_cast({member_connect_resp, ReqJObj}, #state{agent_id=AgentId
                                                   ,last_connect=LastConn
                                                   ,agent_queues=Qs
                                                   ,my_id=MyId
                                                   ,my_q=MyQ
                                                  }=State) ->
    ACDcQueue = wh_json:get_value(<<"Queue-ID">>, ReqJObj),
    case is_valid_queue(ACDcQueue, Qs) of
        false ->
            lager:debug("Queue ~s isn't one of ours", [ACDcQueue]),
            {noreply, State};
        true ->
            lager:debug("responding to member_connect_req"),

            send_member_connect_resp(ReqJObj, MyQ, AgentId, MyId, LastConn),
            {noreply, State#state{acdc_queue_id = ACDcQueue
                                  ,msg_queue_id = wh_json:get_value(<<"Server-ID">>, ReqJObj)
                                 }}
    end;

handle_cast({member_connect_retry, CallId}, #state{my_id=MyId
                                                   ,msg_queue_id=Server
                                                  }=State) when is_binary(CallId) ->
    send_member_connect_retry(Server, CallId, MyId),
    {noreply, State#state{msg_queue_id=undefined
                          ,acdc_queue_id=undefined
                          }};
handle_cast({member_connect_retry, WinJObj}, #state{my_id=MyId}=State) ->
    lager:debug("cannot process this call, sending a retry: ~p", [WinJObj]),
    send_member_connect_retry(WinJObj, MyId),
    {noreply, State};

handle_cast({bridge_to_member, WinJObj, EPs}, #state{fsm_pid=FSM
                                                     ,record_calls=RecordCall
                                                    }=State) ->
    lager:debug("bridging to agent endpoints: ~p", [EPs]),

    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, WinJObj)),
    RingTimeout = wh_json:get_value(<<"Ring-Timeout">>, WinJObj),
    lager:debug("ring agent for ~p", [RingTimeout]),

    ShouldRecord = should_record_endpoints(EPs, RecordCall),

    acdc_util:bind_to_call_events(Call),
    _P = spawn(fun() -> maybe_connect_to_agent(FSM, EPs, Call, RingTimeout) end),

    lager:debug("waiting on successful bridge now: connecting in ~p", [_P]),
    {noreply, State#state{call=Call
                          ,record_calls=ShouldRecord
                         }};

handle_cast({monitor_call, MonitorJObj}, State) ->
    Call = whapps_call:set_call_id(wh_json:get_value(<<"Call-ID">>, MonitorJObj), whapps_call:new()),
    acdc_util:bind_to_call_events(Call),
    lager:debug("monitoring call ~s", [whapps_call:call_id(Call)]),
    {noreply, State#state{call=Call}};

handle_cast({originate_execute, JObj}, State) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    acdc_util:bind_to_call_events(CallId),

    send_originate_execute(JObj),
    {noreply, State};

handle_cast({join_agent, ACallId}, #state{call=Call
                                          ,record_calls=ShouldRecord
                                         }=State) ->
    lager:debug("sending call pickup"),
    whapps_call_command:pickup(ACallId, <<"now">>, Call),

    maybe_start_recording(Call, ShouldRecord),

    {noreply, State};

handle_cast({send_sync_req}=Msg, #state{my_q = Q}=State) when
      Q =:= undefined orelse Q =:= <<>> ->
    fetch_my_queue(),
    lager:debug("replaying ~p, hopefully we have our queue by then", [Msg]),
    gen_listener:cast(self(), Msg),
    {noreply, State};
handle_cast({send_sync_req}, #state{my_id=MyId
                                    ,my_q=MyQ
                                    ,acct_id=AcctId
                                    ,agent_id=AgentId
                                   }=State) ->
    lager:debug("sending sync request"),
    send_sync_request(AcctId, AgentId, MyId, MyQ),
    {noreply, State};

handle_cast({send_sync_resp, Status, ReqJObj, Options}, #state{my_id=MyId
                                                               ,acct_id=AcctId
                                                               ,agent_id=AgentId
                                                              }=State) ->
    lager:debug("sending sync response"),
    send_sync_response(ReqJObj, AcctId, AgentId, MyId, Status, Options),
    {noreply, State};

handle_cast({send_status_update, Status}, #state{acct_id=AcctId
                                                 ,agent_id=AgentId
                                                 }=State) ->
    send_status_update(AcctId, AgentId, Status),
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
%% @spec handle_info(JObj, State) -> {reply, Proplist} |
%%                                   ignore
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, #state{fsm_pid=FSM}) ->
    {reply, [{fsm_pid, FSM}]}.

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
    lager:debug("agent process going down: ~p", [_Reason]).

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
-spec is_valid_queue/2 :: (ne_binary(), [ne_binary()]) -> boolean().
is_valid_queue(Q, Qs) -> lists:member(Q, Qs).

-spec send_member_connect_resp/5 :: (wh_json:json_object(), ne_binary()
                                     ,ne_binary(), ne_binary(), wh_now() | 'undefined'
                                    ) -> 'ok'.
send_member_connect_resp(JObj, MyQ, AgentId, MyId, LastConn) ->
    Queue = wh_json:get_value(<<"Server-ID">>, JObj),
    IdleTime = idle_time(LastConn),
    Resp = props:filter_undefined(
             [{<<"Agent-ID">>, AgentId}
              ,{<<"Idle-Time">>, IdleTime}
              ,{<<"Process-ID">>, MyId}
              ,{<<"Server-ID">>, MyQ}
              | wh_api:default_headers(MyQ, ?APP_NAME, ?APP_VERSION)
             ]),
    lager:debug("sending connect_resp to ~s: ~p", [Queue, Resp]),
    wapi_acdc_queue:publish_member_connect_resp(Queue, Resp).

-spec send_member_connect_retry/2 :: (wh_json:json_object(), ne_binary()) -> 'ok'.
-spec send_member_connect_retry/3 :: (ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
send_member_connect_retry(JObj, MyId) ->
    send_member_connect_retry(wh_json:get_value(<<"Server-ID">>, JObj)
                              ,call_id(JObj)
                              ,MyId).

send_member_connect_retry(Queue, CallId, MyId) ->
    Resp = props:filter_undefined(
             [{<<"Process-ID">>, MyId}
              ,{<<"Call-ID">>, CallId}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    wapi_acdc_queue:publish_member_connect_retry(Queue, Resp).

-spec send_member_connect_accepted/5 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
send_member_connect_accepted(Queue, CallId, AcctId, AgentId, MyId) ->
    Resp = props:filter_undefined([{<<"Call-ID">>, CallId}
                                   ,{<<"Account-ID">>, AcctId}
                                   ,{<<"Agent-ID">>, AgentId}
                                   ,{<<"Process-ID">>, MyId}
                                   | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                                  ]),
    wapi_acdc_queue:publish_member_connect_accepted(Queue, Resp).

-spec send_originate_execute/1 :: (wh_json:json_object()) -> 'ok'.
send_originate_execute(JObj) ->
    Prop = [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
            ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    wapi_dialplan:publish_originate_execute(wh_json:get_value(<<"Server-ID">>, JObj), Prop).

-spec send_sync_request/4 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
send_sync_request(AcctId, AgentId, MyId, MyQ) ->
    Prop = [{<<"Account-ID">>, AcctId}
            ,{<<"Agent-ID">>, AgentId}
            ,{<<"Process-ID">>, MyId}
            | wh_api:default_headers(MyQ, ?APP_NAME, ?APP_VERSION)
           ],
    wapi_acdc_agent:publish_sync_req(Prop).

send_sync_response(ReqJObj, AcctId, AgentId, MyId, Status, Options) ->
    Prop = [{<<"Account-ID">>, AcctId}
            ,{<<"Agent-ID">>, AgentId}
            ,{<<"Process-ID">>, MyId}
            ,{<<"Status">>, wh_util:to_binary(Status)}
            | Options ++ wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    Q = wh_json:get_value(<<"Server-ID">>, ReqJObj),
    lager:debug("sending sync resp to ~s", [Q]),
    wapi_acdc_agent:publish_sync_resp(Q, Prop).

send_status_update(AcctId, AgentId, resume) ->
    Update = props:filter_undefined(
               [{<<"Account-ID">>, AcctId}
                ,{<<"Agent-ID">>, AgentId}
                | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]),
    wapi_acdc_agent:publish_resume(Update).


-spec idle_time/1 :: ('undefined' | wh_now()) -> 'undefined' | integer().
idle_time(undefined) -> undefined;
idle_time(T) -> wh_util:elapsed_s(T).

-spec call_id/1 :: ('undefined' | whapps_call:call() | wh_json:json_object()) ->
                           'undefined' | ne_binary().
-spec call_id/2 :: (whapps_call:call() | wh_json:json_object(), 'undefined' | whapps_call:call()) ->
                           'undefined' | ne_binary().
call_id(undefined) -> undefined;
call_id(Call) ->
    case whapps_call:is_call(Call) of
        true -> whapps_call:call_id(Call);
        false ->
            call_id(Call, wh_json:get_value([<<"Call">>, <<"call_id">>], Call))
    end.

call_id(Call, undefined) ->
    wh_json:get_value([<<"Call-ID">>], Call);
call_id(_, CallId) -> CallId.

-spec maybe_connect_to_agent/4 :: (pid(), list(), whapps_call:call(), integer() | 'undefined') -> 'ok'.
maybe_connect_to_agent(FSM, EPs, Call, Timeout) ->
    put(callid, whapps_call:call_id(Call)),

    ReqId = wh_util:rand_hex_binary(6),
    AcctId = whapps_call:account_id(Call),

    CCVs = props:filter_undefined([{<<"Account-ID">>, AcctId}
                                   ,{<<"Authorizing-ID">>, whapps_call:authorizing_id(Call)}
                                   ,{<<"Request-ID">>, ReqId}
                                  ]),

    Prop = props:filter_undefined(
             [{<<"Msg-ID">>, wh_util:rand_hex_binary(6)}
              ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
              ,{<<"Timeout">>, Timeout}
              ,{<<"Endpoints">>, [wh_json:set_value(<<"Endpoint-Timeout">>, Timeout, EP) || EP <- EPs]}
              ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>, <<"Retain-CID">>
                                                   ,<<"Authorizing-ID">>, <<"Authorizing-Type">>
                                                  ]}
              ,{<<"Account-ID">>, AcctId}
              ,{<<"Resource-Type">>, <<"originate">>}
              ,{<<"Application-Name">>, <<"park">>}
              ,{<<"App-Name">>, ?APP_NAME}
              ,{<<"App-Version">>, ?APP_VERSION}
             ]),

    lager:debug("sending originate request"),

    case whapps_util:amqp_pool_request(Prop
                                       ,fun wapi_resource:publish_originate_req/1
                                       ,fun wapi_dialplan:originate_ready_v/1
                                      ) of
        {ok, JObj} ->
            lager:debug("originate is ready to execute"),
            acdc_agent_fsm:originate_ready(FSM, JObj);
        {error, E} ->
            lager:debug("error originating: ~p", [E]),
            acdc_agent_fsm:originate_failed(FSM, E)
    end.

-spec login_to_queue/2 :: (ne_binary(), ne_binary()) -> 'ok'.
login_to_queue(AcctId, Q) ->
    gen_listener:add_binding(self(), acdc_queue, [{restrict_to, [member_connect_req]}
                                                  ,{queue_id, Q}
                                                  ,{account_id, AcctId}
                                                 ]).

-spec logout_from_queue/2 :: (ne_binary(), ne_binary()) -> 'ok'.
logout_from_queue(AcctId, Q) ->
    gen_listener:rm_binding(self(), acdc_queue, [{restrict_to, [member_connect_req]}
                                                 ,{queue_id, Q}
                                                 ,{account_id, AcctId}
                                                ]).

fetch_my_queue() ->
    Self = self(),
    _ = spawn(fun() -> gen_listener:cast(Self, {queue_name, gen_listener:queue_name(Self)}) end),
    ok.

-spec should_record_endpoints/2 :: (wh_json:json_objects(), boolean()) -> boolean().
should_record_endpoints(_EPs, true) -> true;
should_record_endpoints(EPs, false) ->
    lists:any(fun(EP) ->
                      wh_json:is_true(<<"record_calls">>, EP, false)
              end, EPs).

maybe_stop_recording(_Call, false) -> ok;
maybe_stop_recording(Call, true) ->
    Format = recording_format(),
    MediaName = get_media_name(whapps_call:call_id(Call), Format),

    _ = whapps_call_command:record_call(MediaName, <<"stop">>, Call),
    lager:debug("recording of ~s stopped", [MediaName]),

    save_recording(Call, MediaName, Format).

maybe_start_recording(_Call, false) -> ok;
maybe_start_recording(Call, true) ->
    Format = recording_format(),
    MediaName = get_media_name(whapps_call:call_id(Call), Format),
    lager:debug("recording of ~s started", [MediaName]),

    whapps_call_command:record_call(MediaName, <<"start">>, Call).

recording_format() ->
    whapps_config:get(<<"callflow">>, [<<"call_recording">>, <<"extension">>], <<"mp3">>).

save_recording(Call, MediaName, Format) ->
    {ok, MediaJObj} = store_recording_meta(Call, MediaName, Format),
    lager:debug("stored meta: ~p", [MediaJObj]),

    StoreUrl = store_url(Call, MediaJObj),
    lager:debug("store url: ~s", [StoreUrl]),

    store_recording(MediaName, StoreUrl, Call).

-spec store_recording/3 :: (ne_binary(), ne_binary(), whapps_call:call()) -> 'ok'.
store_recording(MediaName, StoreUrl, Call) ->
    ok = whapps_call_command:store(MediaName, StoreUrl, Call).

-spec store_recording_meta/3 :: (whapps_call:call(), ne_binary(), ne_binary()) ->
                                        {'ok', wh_json:json_object()} |
                                        {'error', any()}.
store_recording_meta(Call, MediaName, Ext) ->
    AcctDb = whapps_call:account_db(Call),
    CallId = whapps_call:call_id(Call),

    MediaDoc = wh_doc:update_pvt_parameters(
                 wh_json:from_list(
                   [{<<"name">>, MediaName}
                    ,{<<"description">>, <<"acdc recording ", MediaName/binary>>}
                    ,{<<"content_type">>, ext_to_mime(Ext)}
                    ,{<<"media_type">>, Ext}
                    ,{<<"media_source">>, <<"recorded">>}
                    ,{<<"source_type">>, wh_util:to_binary(?MODULE)}
                    ,{<<"pvt_type">>, <<"private_media">>}
                    ,{<<"from">>, whapps_call:from(Call)}
                    ,{<<"to">>, whapps_call:to(Call)}
                    ,{<<"caller_id_number">>, whapps_call:caller_id_number(Call)}
                    ,{<<"caller_id_name">>, whapps_call:caller_id_name(Call)}
                    ,{<<"call_id">>, CallId}
                    ,{<<"_id">>, get_recording_doc_id(CallId)}
                   ])
                 ,AcctDb
                ),
    couch_mgr:save_doc(AcctDb, MediaDoc).

ext_to_mime(<<"wav">>) -> <<"audio/x-wav">>;
ext_to_mime(_) -> <<"audio/mp3">>.

get_recording_doc_id(CallId) -> <<"call_recording_", CallId/binary>>.

-spec get_media_name/2 :: (ne_binary(), ne_binary()) -> ne_binary().
get_media_name(CallId, Ext) ->
    <<(get_recording_doc_id(CallId))/binary, ".", Ext/binary>>.

-spec store_url/2 :: (whapps_call:call(), wh_json:json_object()) -> ne_binary().
store_url(Call, JObj) ->
    AccountDb = whapps_call:account_db(Call),
    MediaId = wh_json:get_value(<<"_id">>, JObj),
    MediaName = wh_json:get_value(<<"name">>, JObj),

    Rev = wh_json:get_value(<<"_rev">>, JObj),
    list_to_binary([couch_mgr:get_url(), AccountDb
                    ,"/", MediaId
                    ,"/", MediaName
                    ,"?rev=", Rev
                   ]).
