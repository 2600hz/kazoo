%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_queue_shared).

-behaviour(gen_listener).

%% API
-export([start_link/3
         ,ack/2
         ,nack/2
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

-record(state, {fsm_pid :: pid()}).

-define(SHARED_BINDING_OPTIONS, [{consume_options, [{no_ack, false}
                                                    ,{exclusive, false}
                                                   ]}
                                 ,{basic_qos, 1}
                                 ,{queue_options, [{exclusive, false}]}
                                ]).

-define(SHARED_QUEUE_BINDINGS(AcctId, QueueId), [{self, []}]).

-define(RESPONDERS, [{{acdc_queue_handler, handle_member_call}
                      ,[{<<"member">>, <<"call">>}]
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
-spec start_link/3 :: (pid(), ne_binary(), ne_binary()) -> startlink_ret().
start_link(FSMPid, AcctId, QueueId) ->
    gen_listener:start_link(?MODULE
                            ,[{bindings, ?SHARED_QUEUE_BINDINGS(AcctId, QueueId)}
                              ,{responders, ?RESPONDERS}
                              ,{queue_name, wapi_acdc_queue:shared_queue_name(AcctId, QueueId)}
                              | ?SHARED_BINDING_OPTIONS
                             ]
                            ,[FSMPid]
                           ).

-spec ack/2 :: (pid(), #'basic.deliver'{}) -> 'ok'.
ack(Srv, Delivery) ->
    gen_listener:ack(Srv, Delivery).

-spec nack/2 :: (pid(), #'basic.deliver'{}) -> 'ok'.
nack(Srv, Delivery) ->
    gen_listener:nack(Srv, Delivery).

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
init([FSMPid]) ->
    put(callid, ?LOG_SYSTEM_ID),

    lager:debug("shared queue proc started, sending messages to FSM ~p", [FSMPid]),
    {ok, #state{
       fsm_pid=FSMPid
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
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast(_Msg, State) ->
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
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    lager:debug("acdc_queue_shared terminating: ~p", [_Reason]).

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
