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
-export([start_link/2]).

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

-type agent_status() :: 'init' |     % agent is starting up
                        'ready' |    % agent is ready to connect
                        'waiting' |  % agent is waiting to see if winner of member call
                        'ringing' |  % agent is trying to connect
                        'answered' | % agent is connected
                        'wrapup' |   % agent is done with call, jot some notes
                        'paused'.    % agent is away
-export_type([agent_status/0]).
-record(state, {
          status = 'init' :: agent_status()
         ,call :: whapps_call:call()
         ,agent_id :: ne_binary()
         ,agent_db :: ne_binary()
         ,account_id :: ne_binary()
         ,agent_queues :: [ne_binary(),...] | []
         ,endpoints :: wh_json:json_objects()
         ,last_connect :: wh_now() % last connection
         ,last_attempt :: wh_now() % last attempt to connect
         ,my_id :: ne_binary()
         ,my_q :: ne_binary()
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
start_link(AcctDb, AgentJObj) ->
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
                                    ,[AcctDb, AgentJObj, Queues]
                                   )
    end.

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
init([AcctDb, AgentJObj, Queues]) ->
    AgentId = wh_json:get_value(<<"_id">>, AgentJObj),
    put(callid, AgentId),

    gen_listener:cast(self(), load_endpoints),
    gen_listener:cast(self(), send_sync_event),

    {ok, #state{
       status = 'init'
       ,agent_db = AcctDb
       ,agent_id = AgentId
       ,account_id = wh_json:get_value(<<"pvt_account_id">>, AgentJObj)
       ,agent_queues = Queues
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
handle_cast(load_endpoints, #state{
              agent_db=AcctDb
              ,agent_id=AgentId
             }=State) ->
    {noreply, State#state{endpoints=acdc_util:get_endpoints(AcctDb, AgentId)}};
handle_cast(send_sync_event, #state{
              account_id=AcctId
              ,agent_id=AgentId
              ,my_id=ProcessId
             }=State) ->
    wapi_agent:publish_sync_req(create_sync_api(AcctId, AgentId, ProcessId)),
    {noreply, State#state{timer_ref=start_sync_timer()}};
handle_cast({recv_sync_resp, RespJObj}, #state{sync_resp=CurrResp}=State) ->
    lager:debug("recv sync resp: ~p", [RespJObj]),
    {noreply, State#state{sync_resp=choose_sync_resp(RespJObj, CurrResp)}};

handle_cast({member_connect_req, JObj, MyQ}, #state{status=ready
                                               ,agent_id=AgentId
                                               ,agent_queues=Qs
                                               ,last_connect=LastConn
                                               ,my_id=MyId
                                              }=State) ->
    %% status may have changed, ensure we're ready to respond
    case is_valid_queue(wh_json:get_value(<<"Queue-ID">>, JObj), Qs) of
        false -> {noreply, State};
        true ->
            send_member_connect_resp(JObj, MyQ, AgentId, MyId, LastConn),
            {noreply, State#state{status=waiting}}
    end;

handle_cast({call_bridged, CallId}, #state{status=ringing}=State) ->
    lager:debug("member bridged to agent on ~s", [CallId]),
    {noreply, State#state{status=answered}};

handle_cast({call_hungup, CallId}, #state{status=ringing, call=Call}=State) ->
    case whapps_call:call_id(Call) =:= CallId of
        false -> {noreply, State};
        true ->
            lager:debug("member and agent finished call"),
            {noreply, State#state{status=wrapup
                                  ,timer_ref=start_wrapup_timer()
                                  ,call=undefined
                                 }}
    end;

handle_cast({member_connect_win, JObj}, #state{status=waiting
                                                   ,endpoints=EPs
                                                   }=State) ->
    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, JObj)),
    bind_to_call_events(Call),
    maybe_connect_to_agent(EPs, Call),

    lager:debug("waiting on successful bridge now"),
    {noreply, State#state{call=Call, status=ringing}};

handle_cast({member_connect_monitor, JObj}, #state{status=waiting}=State) ->
    Call = whapps_call:from_json(wh_json:get_value(<<"Call-ID">>, JObj)),
    bind_to_call_events(Call),

    lager:debug("monitoring for successful bridge now"),
    {noreply, State#state{call=Call, status=ringing}};

handle_cast({member_connect_ignore, JObj}, #state{status=waiting}=State) ->
    lager:debug("ignoring call ~s, going back to ready", [wh_json:get_value(<<"Call-ID">>, JObj)]),
    {noreply, State#state{status=ready}};

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
handle_info({timeout, Ref, ?SYNC_TIMER_MESSAGE}, #state{status=init
                                                        ,timer_ref=Ref
                                                        ,sync_resp=undefined
                                                       }=State) ->
    lager:debug("no sync_resps recv'd, moving to ready"),
    {noreply, State#state{status=ready, timer_ref=undefined}};
handle_info({timeout, Ref, ?SYNC_TIMER_MESSAGE}, #state{status=init
                                                        ,timer_ref=Ref
                                                        ,sync_resp=SyncResp
                                                       }=State) ->
    lager:debug("sync_resp recv timeout reached, using ~p", [SyncResp]),
    State1 = update_state_with_sync_resp(SyncResp, State),
    {noreply, State1};
handle_info({timeout, _Ref, ?SYNC_TIMER_MESSAGE}, #state{status=_Status
                                                       }=State) ->
    lager:debug("sync_resp timer up, not in init(~s)", [_Status]),
    {noreply, State};

handle_info({timeout, Ref, ?WRAPUP_TIMER_MESSAGE}, #state{status=wrapup
                                                          ,timer_ref=Ref
                                                          }=State) ->
    lager:debug("wrapup period ended, moving to ready"),
    {noreply, State#state{status=ready}};
handle_info({timeout, _Ref, ?WRAPUP_TIMER_MESSAGE}, #state{status=_Status
                                                          }=State) ->
    lager:debug("wrapup timer fired, not in status waiting(~s)", [_Status]),
    {noreply, State};

handle_info(_Info, #state{status=_Status}=State) ->
    lager:debug("unhandled message in status ~s: ~p", [_Status, _Info]),
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
handle_event(_JObj, #state{status=Status
                           ,timer_ref=Ref
                           ,call=Call
                           ,my_id=MyId
                          }) ->
    {reply, [{status, Status}
             ,{time_left, time_left(Ref)}
             ,{callid, call_id(Call)}
             ,{my_id, MyId}
            ]}.

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
terminate(_Reason, #state{agent_queues=Qs}) ->
    lager:debug("agent process going down: ~p", [_Reason]),
    rm_queue_bindings(Qs).

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
-spec create_sync_api/3 :: (ne_binary(), ne_binary(), ne_binary()) ->
                                   wh_json:json_object().
create_sync_api(AcctId, AgentId, ProcessId) ->
    wh_json:from_list([{<<"Account-ID">>, AcctId}
                       ,{<<"Agent-ID">>, AgentId}
                       ,{<<"Process-ID">>, ProcessId}
                       ,{<<"Msg-ID">>, wh_util:rand_hex_binary(6)}
                      ]).

-spec start_sync_timer/0 :: () -> reference().
-spec start_sync_wait_timer/0 :: () -> reference().
start_sync_timer() ->
    erlang:start_timer(?SYNC_TIMER_TIMEOUT, self(), ?SYNC_TIMER_MESSAGE).
start_sync_wait_timer() ->
    erlang:start_timer(?SYNC_WAIT_TIMER_TIMEOUT, self(), ?SYNC_WAIT_TIMER_MESSAGE).

-spec start_wrapup_timer/0 :: () -> reference().
-spec start_wrapup_timer/1 :: (pos_integer()) -> reference().
start_wrapup_timer() ->
    start_wrapup_timer(?WRAPUP_TIMER_TIMEOUT).
start_wrapup_timer(TimeLeft) ->
    erlang:start_timer(TimeLeft, self(), ?WRAPUP_TIMER_MESSAGE).

-spec start_paused_timer/1 :: (integer() | 'undefined') -> reference() | 'undefined'.
start_paused_timer(TimeLeft) when is_integer(TimeLeft), TimeLeft > 0 ->
    erlang:start_timer(TimeLeft, self(), ?PAUSED_TIMER_MESSAGE);
start_paused_timer(_) -> undefined.

-spec time_left/1 :: ('undefined' | reference()) -> 'undefined' | integer() | 'false'.
time_left(undefined) -> undefined;
time_left(Ref) when is_reference(Ref) -> erlang:read_timer(Ref).

-spec idle_time/1 :: ('undefined' | wh_now()) -> 'undefined' | integer().
idle_time(undefined) -> undefined;
idle_time(T) -> wh_util:elapsed_s(T).

-spec call_id/1 :: ('undefined' | whapps_call:call()) -> 'undefined' | ne_binary().
call_id(undefined) -> undefined;
call_id(Call) -> whapps_call:call_id(Call).

%% Compare the recv'd sync_resp to the held sync_resp, returning the one 'furthest along'
-spec choose_sync_resp/2 :: (wh_json:json_object(), wh_json:json_object() | 'undefined') ->
                               wh_json:json_object().
choose_sync_resp(RespJObj, undefined) -> RespJObj;
choose_sync_resp(RespJObj, CurrResp) ->
    case status_to_int(wh_json:get_value(<<"Status">>, RespJObj)) >
        status_to_int(wh_json:get_value(<<"Status">>, CurrResp)) of
        true -> RespJObj; % new resp is further along
        false -> CurrResp
    end.

-spec status_to_int/1 :: (ne_binary()) -> 0..6.
status_to_int(<<"ready">>) -> 1;
status_to_int(<<"waiting">>) -> 2;
status_to_int(<<"ringing">>) -> 3;
status_to_int(<<"answered">>) -> 4;
status_to_int(<<"wrapup">>) -> 5;
status_to_int(<<"paused">>) -> 6;
status_to_int(_) -> 0.

%% process the final accepted sync_resp and move the agent process's state into the
%% appropriate status (with bindings and timers if necessary)
-spec update_state_with_sync_resp/2 :: (wh_json:json_object(), #state{}) -> #state{}.
-spec update_state_with_sync_resp/3 :: (wh_json:json_object(), #state{}, ne_binary()) -> #state{}.
update_state_with_sync_resp(SyncResp, State) ->
    update_state_with_sync_resp(SyncResp, State
                                 ,wh_json:get_value(<<"Status">>, SyncResp)
                                ).
update_state_with_sync_resp(_SyncResp, #state{agent_queues=Qs}=State, <<"ready">>) ->
    lager:debug("moving to ready status"),
    _ = add_queue_bindings(Qs),
    State#state{status=ready};
update_state_with_sync_resp(_SyncResp, State, <<"waiting">>) ->
    lager:debug("starting sync timer for status waiting"),
    State#state{status=init
                ,timer_ref=start_sync_wait_timer()
               };
update_state_with_sync_resp(_SyncResp, State, <<"ringing">>) ->
    lager:debug("starting sync timer for status ringing"),
    State#state{status=init
                ,timer_ref=start_sync_wait_timer()
               };
update_state_with_sync_resp(SyncResp, #state{agent_queues=Qs}=State, <<"answered">>) ->
    CallId = wh_json:get_value(<<"Call-ID">>, SyncResp),
    lager:debug("binding to call events for status answered, call ~s", [CallId]),

    %% add binding so we can transition with other agent processes
    bind_to_call_events(CallId),
    _ = add_queue_bindings(Qs),

    State#state{status=answered
                ,call=whapps_call:set_call_id(CallId, whapps_call:new())
               };
update_state_with_sync_resp(SyncResp, #state{agent_queues=Qs}=State, <<"wrapup">>) ->
    TimeLeft = wh_json:get_integer_value(<<"Time-Left">>, SyncResp, ?WRAPUP_TIMER_TIMEOUT),
    lager:debug("sync_resp in wrapup: ~b ms left", [TimeLeft]),

    _ = add_queue_bindings(Qs),

    State#state{status=wrapup
                ,timer_ref=start_wrapup_timer(TimeLeft)
               };
update_state_with_sync_resp(SyncResp, #state{agent_queues=Qs}=State, <<"paused">>) ->
    TimeLeft = wh_json:get_integer_value(<<"Time-Left">>, SyncResp),

    _ = add_queue_bindings(Qs),

    State#state{status=paused
                ,timer_ref=start_paused_timer(TimeLeft)
               };
update_state_with_sync_resp(_SyncResp, #state{agent_queues=Qs}=State, <<"init">>) ->
    lager:debug("sync_resp is also in init, let's startup"),

    _ = add_queue_bindings(Qs),

    State#state{status=ready};
update_state_with_sync_resp(_SyncResp, State, _Status) ->
    lager:debug("starting sync timer for other status: ~s", [_Status]),
    State#state{status=init
                ,timer_ref=start_sync_wait_timer()
               }.

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

%% foreach queue an agent belongs to, add a binding to that queue for member requests
-spec add_queue_bindings/1 :: ([ne_binary()]) -> ['ok'].
-spec add_queue_binding/1 :: (ne_binary()) -> 'ok'.
add_queue_bindings(Qs) ->
    [add_queue_binding(Q) || Q <- Qs].
add_queue_binding(Q) ->
    gen_listener:add_binding(self(), acdc_queue, [{queue_id, Q}]).

-spec rm_queue_bindings/1 :: ([ne_binary()]) -> ['ok'].
-spec rm_queue_binding/1 :: (ne_binary()) -> 'ok'.
rm_queue_bindings(Qs) ->
    [rm_queue_binding(Q) || Q <- Qs].
rm_queue_binding(Q) ->
    gen_listener:rm_binding(self(), acdc_queue, [{queue_id, Q}]).

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

-spec is_valid_queue/2 :: (ne_binary(), [ne_binary()]) -> boolean().
is_valid_queue(Q, Qs) -> lists:member(Q, Qs).

-spec maybe_connect_to_agent/2 :: (list(), whapps_call:call()) -> 'ok'.
maybe_connect_to_agent(EPs, Call) ->
    whapps_call_command:bridge(EPs, Call).
