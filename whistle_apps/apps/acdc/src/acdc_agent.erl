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
-export([start_link/3
         ,member_connect_resp/2
         ,member_connect_retry/2
         ,bridge_to_member/2
         ,monitor_call/2
         ,member_connect_accepted/1
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
         ,agent_db :: ne_binary()
         ,account_id :: ne_binary()
         ,fsm_pid :: pid()
         ,agent_queues :: [ne_binary(),...] | []
         ,endpoints :: wh_json:json_objects()
         ,last_connect :: wh_now() % last connection
         ,last_attempt :: wh_now() % last attempt to connect
         ,my_id :: ne_binary()
         ,my_q :: ne_binary() % AMQP queue name
         ,timer_ref :: reference()
         ,sync_resp :: wh_json:json_object() % furthest along resp
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

-define(BINDINGS(AcctDb, AgentId), [{self, []}
                                    ,{acdc_agent, [{agent_db, AcctDb}
                                                   ,{agent_id, AgentId}
                                                  ]}
                                   ]).

-define(RESPONDERS, [{{acdc_agent_handler, handle_agent_status}
                      ,{<<"agent">>, <<"status_update">>}
                     }
                     ,{{acdc_agent_handler, handle_sync_req}
                       ,{<<"agents">>, <<"sync_req">>}
                      }
                     ,{{acdc_agent_handler, handle_sync_resp}
                       ,{<<"agent">>, <<"sync_resp">>}
                      }
                     ,{{acdc_agent_handler, handle_call_event}
                       ,{<<"call_event">>, <<"*">>}
                      }
                     ,{{acdc_agent_handler, handle_member_message}
                       ,{<<"member">>, <<"*">>}
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
start_link(Supervisor, AcctDb, AgentJObj) ->
    AgentId = wh_json:get_value(<<"_id">>, AgentJObj),

    case wh_json:get_value(<<"queues">>, AgentJObj) of
        undefined ->
            lager:debug("agent ~s in ~s has no queues, ignoring"),
            ignore;
        [] ->
            lager:debug("agent ~s in ~s has no queues, ignoring"),
            ignore;
        Queues ->
            gen_listener:start_link(?MODULE
                                    ,[{bindings, ?BINDINGS(AcctDb, AgentId)}
                                      ,{responders, ?RESPONDERS}
                                     ]
                                    ,[Supervisor, AcctDb, AgentJObj, Queues]
                                   )
    end.

-spec member_connect_resp/2 :: (pid(), wh_json:json_object()) -> 'ok'.
member_connect_resp(Srv, ReqJObj) ->
    gen_listener:cast(Srv, {member_connect_resp, ReqJObj}).

member_connect_retry(Srv, WinJObj) ->
    gen_listener:cast(Srv, {member_connect_retry, WinJObj}).

member_connect_accepted(Srv) ->
    gen_listener:cast(Srv, member_connect_accepted).

bridge_to_member(Srv, WinJObj) ->
    gen_listener:cast(Srv, {bridge_to_member, WinJObj}).

monitor_call(Srv, MonitorJObj) ->
    gen_listener:cast(Srv, {monitor_call, MonitorJObj}).

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
init([Supervisor, AcctDb, AgentJObj, Queues]) ->
    AgentId = wh_json:get_value(<<"_id">>, AgentJObj),
    put(callid, AgentId),

    {ok, FSMPid} = acdc_agent_sup:start_fsm(Supervisor, AcctDb, AgentId),
    link(FSMPid),

    gen_listener:cast(self(), load_endpoints),
    gen_listener:cast(self(), send_sync_event),

    Self = self(),
    _ = spawn(fun() ->
                      gen_listener:cast(Self, {queue_name, gen_listener:queue_name(Self)})
              end),

    {ok, #state{
       agent_db = AcctDb
       ,agent_id = AgentId
       ,account_id = wh_json:get_value(<<"pvt_account_id">>, AgentJObj)
       ,agent_queues = Queues
       ,my_id = list_to_binary([wh_util:to_binary(node()), "-", pid_to_list(self())])
       ,fsm_pid = FSMPid
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
handle_cast({queue_name, Q}, State) ->
    {noreply, State#state{my_q=Q}};

handle_cast(member_connect_accepted, #state{msg_queue_id=AmqpQueue
                                    ,call=Call
                                   }=State) ->
    lager:debug("member bridged to agent!"),
    send_member_connect_accepted(AmqpQueue, call_id(Call)),
    {noreply, State};

handle_cast(load_endpoints, #state{
              agent_db=AcctDb
              ,agent_id=AgentId
             }=State) ->
    lager:debug("loading agent endpoints"),
    {noreply, State#state{endpoints=acdc_util:get_endpoints(AcctDb, AgentId)}};

handle_cast({member_connect_resp, ReqJObj}, #state{
              agent_id=AgentId
              ,last_connect=LastConn
              ,agent_queues=Qs
              ,my_id=MyId
              ,my_q=MyQ
             }=State) ->
    lager:debug("responding to member_connect_req"),

    ACDcQueue = wh_json:get_value(<<"Queue-ID">>, ReqJObj),
    case is_valid_queue(ACDcQueue, Qs) of
        false -> {noreply, State};
        true ->
            send_member_connect_resp(ReqJObj, MyQ, AgentId, MyId, LastConn),
            {noreply, State#state{acdc_queue_id = ACDcQueue
                                  ,msg_queue_id = wh_json:get_value(<<"Server-ID">>, ReqJObj)
                                 }}
    end;

handle_cast({member_connect_retry, WinJObj}, #state{
              my_id=MyId
             }=State) ->
    lager:debug("cannot process this call, sending a retry"),
    send_member_connect_retry(WinJObj, MyId),
    {noreply, State};

handle_cast({bridge_to_member, WinJObj}, #state{endpoints=EPs}=State) ->
    lager:debug("bridging to agent endpoints"),

    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, WinJObj)),
    bind_to_call_events(Call),
    maybe_connect_to_agent(EPs, Call),

    lager:debug("waiting on successful bridge now"),
    {noreply, State#state{call=Call}};

handle_cast({monitor_call, MonitorJObj}, State) ->
    Call = whapps_call:set_call_id(wh_json:get_value(<<"Call-ID">>, MonitorJObj), whapps_call:new()),
    bind_to_call_events(Call),
    lager:debug("monitoring call ~s", [whapps_call:call_id(Call)]),
    {noreply, State#state{call=Call}};

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
handle_info(_Info, #state{}=State) ->
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
              | wh_api:default_headers(MyQ, ?APP_NAME, ?APP_VERSION)
             ]),
    wapi_acdc_queue:publish_member_connect_resp(Queue, Resp).

-spec send_member_connect_retry/2 :: (wh_json:json_object(), ne_binary()) -> 'ok'.
send_member_connect_retry(JObj, MyId) ->
    Queue = wh_json:get_value(<<"Server-ID">>, JObj),
    Resp = props:filter_undefined(
             [{<<"Process-ID">>, MyId}
              ,{<<"Call-ID">>, wh_json:get_value([<<"Call">>, <<"call_id">>], JObj)}
              ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
             ]),
    wapi_acdc_queue:publish_member_connect_retry(Queue, Resp).

-spec send_member_connect_accepted/2 :: (ne_binary(), ne_binary()) -> 'ok'.
send_member_connect_accepted(Queue, CallId) ->
    Resp = props:filter_undefined([{<<"Call-ID">>, CallId}]),
    wapi_acdc_queue:publish_member_connect_retry(Queue, Resp).

-spec idle_time/1 :: ('undefined' | wh_now()) -> 'undefined' | integer().
idle_time(undefined) -> undefined;
idle_time(T) -> wh_util:elapsed_s(T).

-spec call_id/1 :: ('undefined' | whapps_call:call()) -> 'undefined' | ne_binary().
call_id(undefined) -> undefined;
call_id(Call) -> whapps_call:call_id(Call).

%% Handles subscribing/unsubscribing from call events
-spec bind_to_call_events/1 :: (ne_binary() | whapps_call:call()) -> 'ok'.
-spec unbind_from_call_events/1 :: (ne_binary() | whapps_call:call()) -> 'ok'.
bind_to_call_events(?NE_BINARY = CallId) ->
    gen_listener:add_binding(self(), call, [{callid, CallId}
                                            ,{restrict_to, [events]}
                                           ]);
bind_to_call_events(Call) ->
    bind_to_call_events(whapps_call:call_id(Call)).

unbind_from_call_events(?NE_BINARY = CallId) ->
    gen_listener:rm_binding(self(), call, [{callid, CallId}
                                           ,{restrict_to, [events]}
                                          ]);
unbind_from_call_events(Call) ->
    unbind_from_call_events(whapps_call:call_id(Call)).

-spec maybe_connect_to_agent/2 :: (list(), whapps_call:call()) -> 'ok'.
maybe_connect_to_agent(EPs, Call) ->
    whapps_call_command:bridge(EPs, Call).
