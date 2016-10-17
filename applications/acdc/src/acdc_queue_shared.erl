%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   KAZOO-3596: Sponsored by GTNetwork LLC, implemented by SIPLABS LLC
%%%-------------------------------------------------------------------
-module(acdc_queue_shared).
-behaviour(gen_listener).

%% API
-export([start_link/4
        ,ack/2
        ,nack/2
        ,deliveries/1
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

-record(state, {fsm_pid :: pid()
               ,deliveries = [] :: deliveries()
               }).
-type state() :: #state{}.

-define(SHARED_BINDING_OPTIONS(Priority)
       ,[{'consume_options', [{'no_ack', 'false'}
                             ,{'exclusive', 'false'}
                             ]}
        ,{'basic_qos', 1}
        ,{'queue_options', [{'exclusive', 'false'}
                           ,{'arguments', [{<<"x-message-ttl">>, ?MILLISECONDS_IN_DAY}
                                          ,{<<"x-max-length">>, 1000}
                                          ,{<<"x-max-priority">>, Priority}
                                          ]
                            }
                           ]
         }
        ]).

-define(SHARED_QUEUE_BINDINGS(AcctId, QueueId), [{'self', []}]).

-define(RESPONDERS, [{{'acdc_queue_handler', 'handle_member_call'}
                     ,[{<<"member">>, <<"call">>}]
                     }
                    ]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link(server_ref(), ne_binary(), ne_binary(), api_integer()) -> startlink_ret().
start_link(FSMPid, AcctId, QueueId, Priority) ->
    gen_listener:start_link(?SERVER
                           ,[{'bindings', ?SHARED_QUEUE_BINDINGS(AcctId, QueueId)}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', kapi_acdc_queue:shared_queue_name(AcctId, QueueId)}
                             | ?SHARED_BINDING_OPTIONS(Priority)
                            ]
                           ,[FSMPid]
                           ).

-spec ack(server_ref(), gen_listener:basic_deliver()) -> 'ok'.
ack(Srv, Delivery) ->
    gen_listener:ack(Srv, Delivery),
    gen_listener:cast(Srv, {'ack', Delivery}).

-spec nack(server_ref(), gen_listener:basic_deliver()) -> 'ok'.
nack(Srv, Delivery) ->
    gen_listener:nack(Srv, Delivery),
    gen_listener:cast(Srv, {'noack', Delivery}).

-spec deliveries(server_ref()) -> deliveries().
deliveries(Srv) ->
    gen_listener:call(Srv, 'deliveries').

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
-spec init([pid()]) -> {'ok', state()}.
init([FSMPid]) ->
    kz_util:put_callid(?LOG_SYSTEM_ID),

    lager:debug("shared queue proc started, sending messages to FSM ~p", [FSMPid]),
    {'ok', #state{fsm_pid=FSMPid}}.

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
-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call('deliveries', _From, #state{deliveries=Ds}=State) ->
    {'reply', Ds, State};
handle_call(_Request, _From, State) ->
    {'noreply', State}.

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
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast({'delivery', Delivery}, #state{deliveries=Ds}=State) ->
    {'noreply', State#state{deliveries=[Delivery|Ds]}};
handle_cast({'ack', Delivery}, #state{deliveries=Ds}=State) ->
    {'noreply', State#state{deliveries=lists:delete(Delivery, Ds)}};
handle_cast({'noack', Delivery}, #state{deliveries=Ds}=State) ->
    {'noreply', State#state{deliveries=lists:delete(Delivery, Ds)}};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'created_queue',_Q}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

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
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info({'basic.cancel',_,'true'}, State) ->
    lager:debug("recv basic.cancel...no!!!"),
    {'noreply', State};
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
-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(_JObj, #state{fsm_pid=FSM}) ->
    {'reply', [{'fsm_pid', FSM}]}.

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
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{deliveries=Ds}) ->
    _ = [catch amqp_util:basic_nack(Delivery) || Delivery <- Ds],
    lager:debug("acdc_queue_shared terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
