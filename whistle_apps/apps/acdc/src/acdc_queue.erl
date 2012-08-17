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
-export([start_link/3
         ,member_connect_req/3
         ,member_connect_win/2
         ,finish_member_call/2
         ,cancel_member_call/3
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
         ,queue_db :: ne_binary()
         ,acct_id :: ne_binary()
         ,fsm_pid :: pid()
         ,my_id :: ne_binary()
         ,my_q :: ne_binary()
         ,call :: whapps_call:call()
         ,agent_process :: ne_binary()
         ,agent_id :: ne_binary()
         ,delivery :: #'basic.deliver'{}
         }).

-define(BINDINGS, [{self, []}]).
-define(RESPONDERS, [{{acdc_queue_handler, handle_call_event}, {<<"call_event">>, <<"*">>}}
                     ,{{acdc_queue_handler, handle_member_call}, {<<"member">>, <<"call">>}}
                     ,{{acdc_queue_handler, handle_member_resp}, {<<"member">>, <<"connect_resp">>}}
                     ,{{acdc_queue_handler, handle_member_accepted}, {<<"member">>, <<"connect_accepted">>}}
                     ,{{acdc_queue_handler, handle_member_retry}, {<<"member">>, <<"connect_retry">>}}
                    ]).

-define(SHARED_BINDING_OPTIONS, [{consume_options, [{no_ack, false}]}
                                 ,{basic_qos, 1}
                                ]).
-define(SHARED_QUEUE_BINDINGS(AcctId, QueueId), [{acdc_queue, [{account_id, AcctId}
                                                               ,{queue_id, QueueId}
                                                              ]}
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
start_link(Supervisor, AcctDb, QueueJObj) ->
    gen_listener:start_link(?MODULE
                            ,[{bindings, ?BINDINGS}
                              ,{responders, ?RESPONDERS}
                             ]
                            ,[Supervisor, AcctDb, QueueJObj]
                           ).

member_connect_req(Srv, MemberCallJObj, Delivery) ->
    gen_listener:cast(Srv, {member_connect_req, MemberCallJObj, Delivery}).

member_connect_win(Srv, RespJObj) ->
    gen_listener:cast(Srv, {member_connect_win, RespJObj}).

finish_member_call(Srv, AcceptJObj) ->
    gen_listener:cast(Srv, {finish_member_call, AcceptJObj}).

cancel_member_call(Srv, MemberCallJObj, Delivery) ->
    gen_listener:cast(Srv, {cancel_member_call, MemberCallJObj, Delivery}).

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
init([Supervisor, AcctDb, QueueJObj]) ->
    QueueId = wh_json:get_value(<<"_id">>, QueueJObj),
    put(callid, QueueId),

    {ok, FSMPid} = acdc_queue_sup:start_fsm(Supervisor, AcctDb, QueueId),
    link(FSMPid),

    Self = self(),
    _ = spawn(fun() ->
                      gen_listener:cast(Self, {queue_name, gen_listener:queue_name(Self)})
              end),

    gen_listener:cast(self(), consume_from_shared_queue),

    {ok, #state{
       queue_id = QueueId
       ,queue_db = AcctDb
       ,acct_id = wh_util:format_account_id(AcctDb, raw)
       ,fsm_pid = FSMPid
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
handle_cast({queue_name, Q}, State) ->
    {noreply, State#state{my_q=Q}};
handle_cast(consume_from_shared_queue, #state{
              queue_id=QueueId
              ,acct_id=AcctId
             }=State) ->
    Self = self(),
    spawn(
      fun() ->
              put(callid, QueueId),
              gen_listener:add_queue(Self, shared_queue_name(AcctId, QueueId)
                                     ,?SHARED_BINDING_OPTIONS
                                     ,?SHARED_QUEUE_BINDINGS(AcctId, QueueId)
                                    ),
              lager:debug("bound ~p to shared queue ~s", [Self, shared_queue_name(AcctId, QueueId)])
      end),
    {noreply, State};

handle_cast({member_connect_req, MemberCallJObj, Delivery}, #state{my_q=MyQ
                                                                   ,my_id=MyId
                                                                   ,acct_id=AcctId
                                                                   ,queue_id=QueueId
                                                                  }=State) ->
    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, MemberCallJObj)),

    acdc_util:bind_to_call_events(Call),
    send_member_connect_req(MemberCallJObj, AcctId, QueueId, MyQ, MyId),

    {noreply, State#state{call=Call
                          ,delivery=Delivery
                         }};

handle_cast({member_connect_win, RespJObj}, #state{my_q=MyQ
                                                   ,my_id=MyId
                                                   ,call=Call
                                                   ,queue_id=QueueId
                                                  }=State) ->
    lager:debug("agent process won the call, sending the win"),
    send_member_connect_win(RespJObj, Call, QueueId, MyQ, MyId),
    {noreply, State#state{agent_process=wh_json:get_value(<<"Agent-Process">>, RespJObj)
                          ,agent_id=wh_json:get_value(<<"Agent-ID">>, RespJObj)
                         }};

handle_cast({finish_member_call, _AcceptJObj}, #state{delivery=Delivery}=State) ->
    lager:debug("agent has taken care of member, we're done"),

    gen_listener:ack(self(), Delivery),

    {noreply, State};

handle_cast({cancel_member_call, _MemberCallJObj, Delivery}, State) ->
    lager:debug("can't handle the member_call, sending it back up"),
    gen_listener:nack(self(), Delivery),
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
-spec shared_queue_name/2 :: (ne_binary(), ne_binary()) -> ne_binary().
shared_queue_name(AcctId, QueueId) ->
    <<"acdc.queue.", AcctId/binary, ".", QueueId/binary>>.

-spec send_member_connect_req/5 :: (wh_json:json_object(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
send_member_connect_req(MemberCallJObj, AcctId, QueueId, MyQ, MyId) ->
    Req = props:filter_undefined(
            [{<<"Account-ID">>, AcctId}
             ,{<<"Queue-ID">>, QueueId}
             ,{<<"Process-ID">>, MyId}
             ,{<<"Server-ID">>, MyQ}
             ,{<<"Call-ID">>, whapps_call:call_id(MemberCallJObj)}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    wapi_acdc_queue:publish_member_connect_req(Req).

-spec send_member_connect_win/5 :: (wh_json:json_object(), whapps_call:call(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
send_member_connect_win(RespJObj, Call, QueueId, MyQ, MyId) ->
    CallJSON = whapps_call:to_json(Call),
    Win = props:filter_undefined(
            [{<<"Call">>, CallJSON}
             ,{<<"Process-ID">>, MyId}
             ,{<<"Server-ID">>, MyQ}
             ,{<<"Queue-ID">>, QueueId}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    wapi_acdc_queue:publish_member_connect_win(wh_json:get_value(<<"Server-ID">>, RespJObj), Win).
