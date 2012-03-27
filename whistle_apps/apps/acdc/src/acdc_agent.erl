%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(acdc_agent).

-behaviour(gen_listener).

%% API
-export([start_link/3]).
-export([handle_call_event/2]).
-export([maybe_handle_call/3]).
-export([consume_call_events/1]).

-export([get_agent_id/1]).

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

-define(FUDGE, 2600).

%% By convention, put the options here in macros
-define(BINDINGS, [{self, []}]).
-define(RESPONDERS, [{{?MODULE, handle_call_event}, {<<"*">>, <<"*">>}}]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(ROUTE_OPTIONS, []).

-record(state, {account_db :: 'undefined' | ne_binary()
                ,agent_id :: 'undefined' | ne_binary()
                ,endpoints :: wh_json:json_object()
                ,queues :: [ne_binary(),...]
                ,call_event_consumers = []
                ,caller :: acdc_call:caller()
                ,start = 'undefined' :: 'undefined' | wh_now()
                ,timeout = 0 :: integer()
                ,ref :: 'undefined' | reference()
                ,server_id = 'undefined' :: 'undefined' | ne_binary()
                ,last_call = wh_util:current_tstamp() :: pos_integer()
               }).

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
start_link(AccountDb, AgentId, QueueIDs) ->
    gen_listener:start_link(?MODULE, [{bindings, ?BINDINGS}
                                      ,{responders, ?RESPONDERS}
                                      ,{queue_name, ?QUEUE_NAME}
                                      ,{queue_options, ?QUEUE_OPTIONS}
                                      ,{route_options, ?ROUTE_OPTIONS}
                                     ], [AccountDb, AgentId, QueueIDs]).

-spec handle_call_event/2 :: (wh_json:json_object(), proplist()) -> any().
handle_call_event(JObj, Props) ->
    _ = [Pid ! {amqp_msg, JObj}
         || Pid <- props:get_value(call_event_consumers, Props, [])
                ,is_pid(Pid)
        ].

-spec maybe_handle_call/3 :: (acdc_call:caller(), integer(), ne_binary()) -> boolean().
maybe_handle_call(Caller, Timeout, ServerId) when Timeout =< 0 ->
    Call = acdc_call:call(Caller),

    CallId = whapps_call:is_call(Call) andalso whapps_call:call_id(Call),
    lager:debug("call ~s timed out before an agent answered", [CallId]),
    Result = [{<<"Call-ID">>, CallId}
              ,{<<"Result">>, <<"NO_ANSWER">>}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ],
    wapi_queue:publish_result(ServerId, Result);
maybe_handle_call(Caller, Timeout, ServerId) ->
    Start = erlang:now(),
    case acdc_call:next_agent(Caller) of
        undefined ->
            lager:debug("no agents left in the list, reload and try again"),
            maybe_handle_call(acdc_call:reload_agents(Caller), Timeout - diff_milli(Start), ServerId);
        {Agent, Caller1} ->
            lager:debug("trying agent ~p", [Agent]),
            gen_listener:cast(Agent, {maybe_handle_call, Caller1, Timeout, ServerId})
    end.

-spec consume_call_events/1 :: (pid()) -> 'ok'.
consume_call_events(Srv) ->
    gen_server:cast(Srv, {add_consumer, self()}).

-spec get_agent_id/1 :: (pid()) -> ne_binary().
get_agent_id(Srv) ->
    gen_server:call(Srv, agent_id).

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
init([AccountDb, AgentId, QueueIDs]) ->
    erlang:process_flag(trap_exit, true),
    put(callid, ?LOG_SYSTEM_ID),
    lager:debug("starting new agent process for ~s/~s", [AccountDb, AgentId]),
    {ok, #state{account_db=AccountDb
                ,agent_id=AgentId
                ,endpoints=acdc_util:fetch_owned_by(AgentId, device, AccountDb)
                ,queues=QueueIDs
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
handle_call(agent_id, _From, #state{agent_id=Id}=State) ->
    {reply, Id, State}.

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
handle_cast({maybe_handle_call, Caller, Timeout, ServerId}, #state{account_db=AccountDb
                                                                   ,queues=QueueIDs
                                                                   ,agent_id=AgentId
                                                                   ,start=undefined
                                                                  }=State) ->
    OriginalCallId = get(callid),
    acdc_call:put_callid(Caller),

    Criteria = [fun() ->
                        QueueId = acdc_call:queue_id(Caller),
                        case lists:member(QueueId, QueueIDs) of
                            true -> true;
                            false ->
                                lager:debug("agent does not belong to queue '~s', declined", [QueueId]),
                                false
                        end
                end
                ,fun() ->
                         CallAccountId = acdc_call:account_db(Caller),
                         case AccountDb =:= CallAccountId of
                             true -> true;
                             false ->
                                 lager:debug("agent does not belong to account id '~s', declined", [CallAccountId]),
                                 false
                         end
                 end
                ,fun() ->
                         case acdc_util:get_agent_status(AccountDb, AgentId) of
                             <<"login">> -> true;
                             <<"resume">> -> true;
                             _Else ->
                                 lager:debug("agent is in non-ready state '~s', declined", [_Else]),
                                 false
                         end
                 end
               ],
    case lists:all(fun(F) -> F() end, Criteria) of
        false ->
            maybe_handle_call(Caller, Timeout-500, ServerId),

            put(callid, OriginalCallId),
            {noreply, State};
        true ->
            lager:debug("attempting to ring agent ~s", [AgentId]),
            gen_listener:cast(self(), attempt_agent),
            {noreply, State#state{caller=Caller, start=erlang:now(), timeout=Timeout, server_id=ServerId}}
    end;
handle_cast({maybe_handle_call, Caller, Timeout, ServerId}, State) ->
    OriginalCallId = get(callid),
    acdc_call:put_callid(Caller),

    lager:debug("agent already processing call, declined", []),

    maybe_handle_call(Caller, Timeout-500, ServerId),

    put(callid, OriginalCallId),
    {noreply, State};

handle_cast({add_consumer, Consumer}, #state{call_event_consumers=Consumers}=State) ->
    link(Consumer),
    {noreply, State#state{call_event_consumers=[Consumer|Consumers]}};

handle_cast({remove_consumer, Consumer}, #state{call_event_consumers=Consumers}=State) ->
    {noreply, State#state{call_event_consumers=lists:filter(fun(C) -> C =/= Consumer end, Consumers)}};

handle_cast({bridge_result, Ref, {ok, _}}, #state{server_id=ServerId, caller=Caller, ref=Ref}=State) ->
    lager:debug("agent handled call"),
    Result = [{<<"Call-ID">>, acdc_call:callid(Caller)}
              ,{<<"Result">>, <<"ANSWERED">>}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ],
    wapi_queue:publish_result(ServerId, Result),
    acdc_agents:update_agent(self()),
    {noreply, reset(State)};
handle_cast({bridge_result, Ref, _R}, #state{ref=Ref}=State) ->
    lager:debug("agent failed to handle call"),
    try_next_agent(State),
    acdc_agents:update_agent(self()),
    {noreply, reset(State)};

handle_cast(attempt_agent, #state{caller=Caller, endpoints=EPs}=State) ->
    CallId = acdc_call:callid(Caller),

    gen_listener:add_binding(self(), call, [{callid, CallId}, {restrict_to, [events, cdr]}]),
    lager:debug("added bindings for ~s", [CallId]),
    Self = self(),
    Ref = make_ref(),

    Call = acdc_call:call(Caller),

    spawn_link(fun() ->
                       put(callid, CallId),
                       gen_server:cast(Self, {add_consumer, self()}),
                       Result = (catch bridge_to_endpoints(acdc_util:get_endpoints(Call, EPs)
                                                           ,acdc_call:member_timeout(Caller)
                                                           ,Call
                                                          )),
                       gen_server:cast(Self, {bridge_result, Ref, Result})
               end),
    {noreply, State#state{ref=Ref}}.

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
handle_info({'EXIT', Consumer, _R}, #state{call_event_consumers=Consumers}=State) ->
    {noreply, State#state{call_event_consumers=lists:filter(fun(C) -> C =/= Consumer end, Consumers)}};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, #state{caller=undefined}) ->
    ignore;
handle_event(JObj, #state{call_event_consumers=Consumers, caller=Caller}) ->
    Call = acdc_call:call(Caller),
    CallId = whapps_call:call_id_direct(Call),

    case {whapps_util:get_event_type(JObj), wh_json:get_value(<<"Call-ID">>, JObj)} of
        {{<<"call_event">>, _}, EventCallId} when EventCallId =/= CallId ->
            ignore;
        {_Else, _} ->
            {reply, [{call_event_consumers, Consumers}]}
    end.

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
    lager:debug("agent going down: ~p", [_Reason]).

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
-spec bridge_to_endpoints/3 :: (wh_json:json_objects(), ne_binary(), whapps_call:call()) -> {'ok', wh_json:json_object()} |
                                                                                            {'fail', wh_json:json_object()} |
                                                                                            {'error', term()}.
bridge_to_endpoints([], _, _) ->
    {error, no_endpoints};
bridge_to_endpoints(Endpoints, Timeout, Call) ->
    whapps_call_command:b_bridge(Endpoints, Timeout, <<"simultaneous">>, Call).

-spec reset/1 :: (#state{}) -> #state{}.
reset(#state{call_event_consumers=Consumers, caller=Caller}=State) ->
    _ = [exit(Consumer, exit) || Consumer <- Consumers],
    case acdc_call:is_call(Caller) andalso acdc_call:callid(Caller) of
        CallId when is_binary(CallId) ->
            gen_listener:rm_binding(self(), call, [{callid, CallId}, {restrict_to, [events, cdr]}]);
        _Else ->
            ok
    end,
    State#state{caller=undefined, start=undefined, timeout=0, ref=undefined, call_event_consumers=[]}.

-spec try_next_agent/1 :: (#state{}) -> boolean().
try_next_agent(#state{caller=Caller, timeout=Timeout, start=Start, server_id=ServerId}) ->
    maybe_handle_call(Caller, Timeout - diff_milli(Start), ServerId).

diff_milli(Start) ->
    diff_micro(Start) div 1000.
diff_micro(Start) ->
    timer:now_diff(erlang:now(), Start).
