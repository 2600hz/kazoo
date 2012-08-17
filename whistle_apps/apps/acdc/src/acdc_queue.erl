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

-record(state, {
          queue_id :: ne_binary()
         ,queue_db :: ne_binary()
         ,account_id :: ne_binary()
         }).

-define(BINDINGS, [{self, []}]).
-define(RESPONDERS, []).

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
start_link(AcctDb, QueueJObj) ->
    gen_listener:start_link(?MODULE
                            ,[{bindings, ?BINDINGS}
                              ,{responders, ?RESPONDERS}
                             ]
                            ,[AcctDb, QueueJObj]
                           ).

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
init([AcctDb, QueueJObj]) ->
    QueueId = wh_json:get_value(<<"_id">>, QueueJObj),
    put(callid, QueueId),

    gen_listener:cast(self(), consume_from_shared_queue),

    {ok, #state{
       queue_id = QueueId
       ,queue_db = AcctDb
       ,account_id = wh_util:format_account_id(AcctDb, raw)
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
handle_cast(consume_from_shared_queue, #state{
              queue_id=QueueId
              ,account_id=AcctId
             }=State) ->
    Self = self(),
    spawn(fun() -> gen_listener:add_queue(Self, shared_queue_name(AcctId, QueueId)
                                          ,?SHARED_BINDING_OPTIONS
                                          ,?SHARED_QUEUE_BINDINGS(AcctId, QueueId)
                                         )
          end),
    {noreply, State};
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
handle_event(_JObj, #state{}=_) ->
    {reply, []}.

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
