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
-module(acdc_queue).

-behaviour(gen_listener).

%% API
-export([start_link/2
         ,accept_member_calls/1
         ,member_connect_req/3
         ,member_connect_win/3, member_connect_win/4
         ,member_connect_monitor/2, member_connect_monitor/3
         ,timeout_member_call/1
         ,exit_member_call/1
         ,finish_member_call/2
         ,cancel_member_call/1, cancel_member_call/2 ,cancel_member_call/3
         ,send_sync_req/2
         ,config/1
        ]).

%% Call Manipulation
-export([put_member_on_hold/3]).

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
         ,queue_db :: ne_binary()
         ,acct_id :: ne_binary()

          %% PIDs of the gang
         ,supervisor :: pid()
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
-spec start_link/2 :: (pid(), wh_json:json_object()) -> startlink_ret().
start_link(Supervisor, QueueJObj) ->
    gen_listener:start_link(?MODULE
                            ,[{bindings, ?BINDINGS}
                              ,{responders, ?RESPONDERS}
                             ]
                            ,[Supervisor, QueueJObj]
                           ).

accept_member_calls(Srv) ->
    gen_listener:cast(Srv, {accept_member_calls}).

member_connect_req(Srv, MemberCallJObj, Delivery) ->
    gen_listener:cast(Srv, {member_connect_req, MemberCallJObj, Delivery}).

member_connect_win(Srv, RespJObj, Timeout) ->
    member_connect_win(Srv, RespJObj, Timeout, <<"#">>).
member_connect_win(Srv, RespJObj, Timeout, CallerExitKey) ->
    gen_listener:cast(Srv, {member_connect_win, RespJObj, Timeout, CallerExitKey}).

member_connect_monitor(Srv, RespJObj) ->
    member_connect_monitor(Srv, RespJObj, <<"#">>).
member_connect_monitor(Srv, RespJObj, CallerExitKey) ->
    gen_listener:cast(Srv, {member_connect_monitor, RespJObj, CallerExitKey}).

timeout_member_call(Srv) ->
    gen_listener:cast(Srv, {timeout_member_call}).
exit_member_call(Srv) ->
    gen_listener:cast(Srv, {exit_member_call}).
finish_member_call(Srv, AcceptJObj) ->
    gen_listener:cast(Srv, {finish_member_call, AcceptJObj}).

cancel_member_call(Srv) ->
    gen_listener:cast(Srv, {cancel_member_call}).
cancel_member_call(Srv, RejectJObj) ->
    gen_listener:cast(Srv, {cancel_member_call, RejectJObj}).
cancel_member_call(Srv, MemberCallJObj, Delivery) ->
    gen_listener:cast(Srv, {cancel_member_call, MemberCallJObj, Delivery}).

-spec send_sync_req/2 :: (pid(), queue_strategy()) -> 'ok'.
send_sync_req(Srv, Type) ->
    gen_listener:cast(Srv, {send_sync_req, Type}).

config(Srv) ->
    gen_listener:call(Srv, config).

put_member_on_hold(Srv, Call, MOH) ->
    gen_listener:cast(Srv, {put_member_on_hold, Call, MOH}).

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
init([Supervisor, QueueJObj]) ->
    QueueId = wh_json:get_value(<<"_id">>, QueueJObj),
    put(callid, QueueId),

    lager:debug("starting queue ~s", [QueueId]),

    gen_listener:cast(self(), {start_fsm, Supervisor, QueueJObj}),

    Self = self(),
    _ = spawn(fun() ->
                      gen_listener:cast(Self, {queue_name, gen_listener:queue_name(Self)})
              end),

    {ok, #state{
       queue_id = QueueId
       ,queue_db = wh_json:get_value(<<"pvt_account_db">>, QueueJObj)
       ,acct_id = wh_json:get_value(<<"pvt_account_id">>, QueueJObj)
       ,my_id = list_to_binary([wh_util:to_binary(node()), "-", pid_to_list(self())])
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
handle_cast({start_fsm, Supervisor, QueueJObj}, #state{}=State) ->
    {ok, FSMPid} = acdc_queue_sup:start_fsm(Supervisor, QueueJObj),

    {noreply, State#state{fsm_pid=FSMPid
                          ,supervisor=Supervisor
                         }};

handle_cast({accept_member_calls}, #state{supervisor=Supervisor
                                          ,fsm_pid=FSMPid
                                          ,shared_pid=undefined
                                          ,acct_id=AcctId
                                          ,queue_id=QueueId
                                         }=State) ->
    {ok, SharedPid} = acdc_queue_sup:start_shared_queue(Supervisor, FSMPid, AcctId, QueueId),
    lager:debug("started shared queue listener: ~p", [SharedPid]),
    {noreply, State#state{shared_pid=SharedPid}};

handle_cast({queue_name, Q}, State) ->
    {noreply, State#state{my_q=Q}};

handle_cast({member_connect_req, MemberCallJObj, Delivery}
            ,#state{
              my_q=MyQ
              ,my_id=MyId
              ,acct_id=AcctId
              ,queue_id=QueueId
             }=State
           ) ->
    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, MemberCallJObj)),

    acdc_util:bind_to_call_events(Call),
    send_member_connect_req(whapps_call:call_id(Call), AcctId, QueueId, MyQ, MyId),

    {noreply, State#state{call=Call
                          ,delivery=Delivery
                          ,member_call_queue=wh_json:get_value(<<"Server-ID">>, MemberCallJObj)
                         }};

handle_cast({member_connect_win, RespJObj, Timeout, CallerExitKey}, #state{my_q=MyQ
                                                                           ,my_id=MyId
                                                                           ,call=Call
                                                                           ,queue_id=QueueId
                                                                          }=State) ->
    lager:debug("agent process won the call, sending the win"),
    send_member_connect_win(RespJObj, Timeout, Call, QueueId, MyQ, MyId, CallerExitKey),
    {noreply, State#state{agent_process=wh_json:get_value(<<"Agent-Process">>, RespJObj)
                          ,agent_id=wh_json:get_value(<<"Agent-ID">>, RespJObj)
                         }};

handle_cast({member_connect_monitor, RespJObj, CallerExitKey}, #state{my_id=MyId
                                                                      ,call=Call
                                                                      ,queue_id=QueueId
                                                                     }=State) ->
    lager:debug("agent process won the call, sending the monitor to another agent process"),
    send_member_connect_monitor(RespJObj, whapps_call:call_id(Call), QueueId, MyId, CallerExitKey),
    {noreply, State};

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

    {noreply, clear_call_state(State)};

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

    {noreply, clear_call_state(State)};

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

    {noreply, clear_call_state(State)};

handle_cast({cancel_member_call}, #state{delivery=undefined}=State) ->
    lager:debug("empty cancel member, no delivery info"),
    {noreply, State};
handle_cast({cancel_member_call}, #state{delivery=Delivery
                                         ,call=Call
                                         ,shared_pid=Pid
                                        }=State) ->
    lager:debug("cancel member_call"),

    case maybe_nack(Call, Delivery, Pid) of
        true -> {noreply, clear_call_state(State)};
        false ->
            {noreply, State}
    end;

handle_cast({cancel_member_call, _RejectJObj}, #state{delivery=undefined}=State) ->
    lager:debug("cancel a member_call that I don't have delivery info for: ~p", [_RejectJObj]),
    {noreply, State};
handle_cast({cancel_member_call, _RejectJObj}, #state{delivery = #'basic.deliver'{}=Delivery
                                                      ,call=Call
                                                      ,shared_pid=Pid
                                                     }=State) ->
    lager:debug("agent failed to handle the call, nack: ~p", [_RejectJObj]),

    case maybe_nack(Call, Delivery, Pid) of
        true -> {noreply, clear_call_state(State)};
        false -> {noreply, State}
    end;

handle_cast({cancel_member_call, _MemberCallJObj, Delivery}, #state{shared_pid=Pid}=State) ->
    lager:debug("can't handle the member_call, sending it back up"),
    acdc_queue_shared:nack(Pid, Delivery),
    {noreply, State};

handle_cast({send_sync_req, Type}, #state{my_q=MyQ
                                          ,my_id=MyId
                                          ,acct_id=AcctId
                                          ,queue_id=QueueId
                                         }=State) ->
    send_sync_req(MyQ, MyId, AcctId, QueueId, Type),
    {noreply, State};

handle_cast({put_member_on_hold, Call, MOH}, State) ->
    whapps_call_command:answer(Call),
    whapps_call_command:hold(MOH, Call),
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
    Req = props:filter_undefined(
            [{<<"Account-ID">>, AcctId}
             ,{<<"Queue-ID">>, QueueId}
             ,{<<"Process-ID">>, MyId}
             ,{<<"Server-ID">>, MyQ}
             ,{<<"Call-ID">>, CallId}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    publish(Req, fun wapi_acdc_queue:publish_member_connect_req/1).

-spec send_member_connect_win/7 :: (wh_json:json_object(), pos_integer(), whapps_call:call(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
send_member_connect_win(RespJObj, Timeout, Call, QueueId, MyQ, MyId, CallerExitKey) ->
    CallJSON = whapps_call:to_json(Call),
    Q = wh_json:get_value(<<"Server-ID">>, RespJObj),
    Win = props:filter_undefined(
            [{<<"Call">>, CallJSON}
             ,{<<"Process-ID">>, MyId}
             ,{<<"Server-ID">>, MyQ}
             ,{<<"Queue-ID">>, QueueId}
             ,{<<"Ring-Timeout">>, Timeout}
             ,{<<"Caller-Exit-Key">>, CallerExitKey}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    publish(Q, Win, fun wapi_acdc_queue:publish_member_connect_win/2).

-spec send_member_connect_monitor/5 :: (wh_json:json_object(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
send_member_connect_monitor(RespJObj, CallId, QueueId, MyId, CallerExitKey) ->
    Q = wh_json:get_value(<<"Server-ID">>, RespJObj),
    Prop = [{<<"Call-ID">>, CallId}
            ,{<<"Process-ID">>, MyId}
            ,{<<"Queue-ID">>, QueueId}
            ,{<<"Caller-Exit-Key">>, CallerExitKey}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    publish(Q, Prop, fun wapi_acdc_queue:publish_member_connect_monitor/2).

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
              | wh_api:default_headers(MyQ, ?APP_NAME, ?APP_VERSION)
             ]),
    publish(Resp, fun wapi_acdc_queue:publish_sync_req/1).

-spec maybe_nack/3 :: (whapps_call:call(), #'basic.deliver'{}, pid()) -> boolean().
maybe_nack(Call, Delivery, SharedPid) ->
    case whapps_call_command:call_status(Call) of
        {ok, _} ->
            lager:debug("call is still active, nack and replay"),
            acdc_util:unbind_from_call_events(Call),
            acdc_queue_shared:nack(SharedPid, Delivery),
            true;
        {error, _} ->
            lager:debug("call is probably not active, ack it (so its gone)"),
            acdc_util:unbind_from_call_events(Call),
            acdc_queue_shared:ack(SharedPid, Delivery),
            false
    end.

clear_call_state(#state{}=State) ->
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
            lager:debug("failed to publish message: ~p", [_R]),
            ok
    end.
publish(Q, Req, F) ->
    case catch F(Q, Req) of
        'ok' -> 'ok';
        {'EXIT', _R} ->
            lager:debug("failed to publish message to ~s: ~p", [Q, _R]),
            ok
    end.
