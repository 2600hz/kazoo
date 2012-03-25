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
-export([start_link/1, handle_call_event/2, update_agent/2
         ,maybe_handle_call/5
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2
         ,terminate/2, code_change/3]).

-include("acdc.hrl").

-define(TIMEOUT_CALL_STATUS, 300000). % check call status every 5 minutes
-define(FUDGE, 2600).

-record(state, {
          acct_db :: ne_binary()
         ,agent_id :: ne_binary()
         ,queues :: [{ne_binary(), wh_json:json_object()},...]
         ,call :: whapps_call:call()
         ,from :: {pid(), reference()}
         ,from_timeout_ref :: reference()
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
start_link(_) ->
    gen_listener:start_link(?MODULE, [{bindings, [{self, []}]}
                                      ,{responders, [{{?MODULE, handle_call_event}, {<<"*">>, <<"*">>}}]}
                                      ,{queue_name, <<>>}
                                      ,{queue_options, []}
                                      ,{consume_options, []}
                                     ], []).

handle_call_event(JObj, Props) ->
    Accept = props:get_value(accept_call_events, Props),

    case wh_util:get_event_type(JObj) of
        {<<"call_event">>, _}=Type when Accept ->
            gen_listener:cast(props:get_value(server, Props), {call_event, Type, JObj});
        {<<"call_detail">>, _}=Type when Accept ->
            gen_listener:cast(props:get_value(server, Props), {call_event, Type, JObj});
        {<<"error">>, <<"dialplan">>}=Type when Accept ->
            gen_listener:cast(props:get_value(server, Props), {call_event, Type, JObj});
        _Type ->
            lager:debug("not sending event of type ~p: ~p", [_Type, Accept])
    end.

update_agent(Srv, IDandInfo) ->
    gen_listener:cast(Srv, {update, IDandInfo}).

maybe_handle_call(Srv, Call, AcctDb, QueueId, Timeout) ->
    gen_listener:call(Srv, {maybe_handle_call, Call, AcctDb, QueueId, Timeout}, Timeout+?FUDGE).

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
init(_) ->
    put(callid, ?LOG_SYSTEM_ID),
    lager:debug("acdc_agent starting"),

    _ = erlang:send_after(?TIMEOUT_CALL_STATUS, self(), call_status),

    {ok, #state{}}.

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
handle_call({maybe_handle_call, _, _, _, _}, _From, #state{from={_,_}}=State) ->
    lager:debug("agent already processing a call"),
    {reply, false, State};
handle_call({maybe_handle_call, Call, AcctDb, QueueId, Timeout}, From, #state{
                                                                   acct_db=DB
                                                                   ,queues=Qs
                                                                   ,agent_id=AgentId
                                                                  }=State) ->
    case AcctDb =:= DB andalso lists:keyfind(QueueId, 1, Qs) of
        false ->
            lager:debug("db ~s doesn't match ~s", [AcctDb, DB]),
            lager:debug("or agent isn't in queue ~s: ~p", [QueueId, Qs]),
            {reply, false, State};
        {_, Q} ->
            %% TODO: no guarantee Agent is still available or will answer the call;
            %% detect that case and move to next agent
            case acdc_util:get_agent_status(AcctDb, AgentId) of
                <<"login">> ->
                    _ = put(callid, whapps_call:call_id(Call)),
                    acdc_util:log_agent_activity(Call, <<"busy">>, AgentId),

                    Ref = erlang:send_after(Timeout, self(), from_timeout),

                    call_binding(Call),

                    gen_listener:cast(self(), {connect_call, Q}),

                    {noreply, State#state{call=Call, from=From, from_timeout_ref=Ref}};
                <<"resume">> ->
                    _ = put(callid, whapps_call:call_id(Call)),
                    acdc_util:log_agent_activity(Call, <<"busy">>, AgentId),

                    Ref = erlang:send_after(Timeout, self(), from_timeout),

                    call_binding(Call),

                    gen_listener:cast(self(), {connect_call, Q}),

                    {noreply, State#state{call=Call, from=From, from_timeout_ref=Ref}};
                _Action ->
                    lager:debug("in non-ready state: ~s", [_Action]),
                    {reply, false, State}
            end
    end.

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
handle_cast({connect_call, Q}, #state{call=Call, agent_id=AgentId}=State) ->
    lager:debug("trying to connect call"),
    call_bridging(Call, AgentId, Q),
    {noreply, State};
handle_cast({update, {AcctDb, AgentId, Info}}, State) ->
    lager:debug("updating agent ~s", [AgentId]),
    {noreply, State#state{
                acct_db=AcctDb
                ,agent_id=AgentId
                ,queues=[begin
                             {ok, Q} = couch_mgr:open_doc(AcctDb, QID),
                             {QID, Q}
                         end || QID <- wh_json:get_value(<<"queues">>, Info, [])
                        ]
               }};

handle_cast({call_event, {<<"call_event">>, <<"LEG_CREATED">>}, JObj}, State) ->
    lager:debug("b-leg created (~s), hope we answer!", [wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj)]),
    {noreply, State};

handle_cast({call_event, {<<"call_event">>, <<"LEG_DESTROYED">>}, JObj}, #state{from={_,_}=From}=State) ->
    lager:debug("b-leg destroyed (~s): ~s(~s)", [wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj)
                                                 ,wh_json:get_value(<<"Hangup-Cause">>, JObj)
                                                 ,wh_json:get_value(<<"Hangup-Code">>, JObj)
                                                ]),
    gen_server:reply(From, false),
    {noreply, clear_call(State)};
handle_cast({call_event, {<<"call_event">>, <<"LEG_DESTROYED">>}, JObj}, State) ->
    lager:debug("b-leg destroyed (~s): ~s(~s)", [wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj)
                                                 ,wh_json:get_value(<<"Hangup-Cause">>, JObj)
                                                 ,wh_json:get_value(<<"Hangup-Code">>, JObj)
                                                ]),
    {noreply, State};

handle_cast({call_event, {<<"call_event">>, <<"CHANNEL_BRIDGE">>}, JObj}, #state{from={_,_}=From}=State) ->
    lager:debug("leg (~s) was bridged! we're handling the call", [wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj)]),
    gen_server:reply(From, true),
    {noreply, clear_from(State)};

handle_cast({call_event, {<<"call_event">>, <<"CHANNEL_DESTROY">>}, JObj}, #state{from={_,_}=From}=State) ->
    lager:debug("received channel destroy for caller, we're done here: ~s", [wh_json:get_value(<<"Hangup-Code">>, JObj)]),

    gen_server:reply(From, false),
    {noreply, clear_call(State)};
handle_cast({call_event, {<<"call_event">>, <<"CHANNEL_DESTROY">>}, _JObj}, State) ->
    lager:debug("received channel destroy for caller, reinstating agent as available"),
    {noreply, clear_call(State)};

handle_cast({call_event, {<<"error">>, <<"dialplan">>}, JObj}, #state{from={_,_}=From}=State) ->
    lager:debug("recieved dialplan error: ~s", [wh_json:get_value(<<"Error-Message">>, JObj)]),
    gen_server:reply(From, down),
    {noreply, clear_call(State)};

handle_cast({call_event, {_Cat, _Name}, _JObj}, State) ->
    lager:debug("unhandled call event: ~s/~s: ~s", [_Cat, _Name, wh_json:get_value(<<"Application-Name">>, _JObj)]),
    {noreply, State};
handle_cast(_Msg, State) ->
    lager:debug("cast: ~p", [_Msg]),
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
handle_info(from_timeout, #state{from={_,_}=From}=State) ->
    gen_listener:reply(From, false),
    {noreply, clear_call(State)};

handle_info(call_status, #state{call=undefined, agent_id=AgentId, acct_db=AcctDb}=State) ->
    _ = erlang:send_after(?TIMEOUT_CALL_STATUS, self(), call_status),

    case acdc_util:get_agent_status(AcctDb, AgentId) of
        <<"busy">> ->
            lager:debug("agent marked as busy, which is wrong as we have no call; setting to resume"),
            acdc_util:log_agent_activity(AcctDb, <<"resume">>, AgentId),
            {noreply, State};
        _ ->
            {noreply, State}
    end;
handle_info(call_status, #state{call=Call}=State) ->
    whapps_call_command:call_status(Call),
    lager:debug("sent call status req"),
    _ = erlang:send_after(?TIMEOUT_CALL_STATUS, self(), call_status),
    {noreply, State};

handle_info(_Info, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
    {noreply, State}.

handle_event(_JObj, #state{call=Call}) ->
    {reply, [{accept_call_events, whapps_call:is_call(Call)}]}.

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
clear_call(#state{from_timeout_ref=Ref, call=Call, acct_db=AcctDb, agent_id=AgentId}=State) ->
    catch(erlang:cancel_timer(Ref)),

    case whapps_call:is_call(Call) of
        true ->
            call_unbinding(Call),
            acdc_util:log_agent_activity(Call, <<"resume">>, AgentId);
        false ->
            acdc_util:log_agent_activity(AcctDb, <<"resume">>, AgentId)
    end,

    put(callid, ?LOG_SYSTEM_ID),
    State#state{
      call = undefined
      ,from = undefined
      ,from_timeout_ref = undefined
     }.

clear_from(#state{from_timeout_ref=Ref}=State) ->
    catch(erlang:cancel_timer(Ref)),

    State#state{
      from = undefined
      ,from_timeout_ref = undefined
     }.

call_binding(Call) ->
    gen_listener:add_binding(self(), call, [{callid, whapps_call:call_id(Call)}, {restrict_to, [events, cdr]}]).

call_unbinding(Call) ->
    gen_listener:rm_binding(self(), call, [{callid, whapps_call:call_id(Call)}, {restrict_to, [events, cdr]}]).

call_bridging(Call, AgentId, Q) ->
    whapps_call_command:bridge(get_endpoints(Call, AgentId, Q), Call).

%% TODO: move to shared place, instead of calling callflow functions directly
get_endpoints(Call, UserId, Q) ->
    MemberTimeout = wh_json:get_integer_value(<<"member_timeout">>, Q, 5) * 1000,

    lager:debug("find devices owned by ~s (timeout after ~bms)", [UserId, MemberTimeout]),

    lists:foldr(fun(EndpointId, Acc) ->
                        lager:debug("ep id: ~s", [EndpointId]),
                        case cf_endpoint:build(EndpointId, Call) of
                            {ok, Endpoints} ->
                                lists:foldl(fun(EP, Acc0) ->
                                                    [wh_json:set_value(<<"Endpoint-Timeout">>, wh_util:to_binary(MemberTimeout), EP)|Acc0]
                                            end, Acc, Endpoints);
                            {error, _E} -> lager:debug("failed to build ep: ~p", [_E]), Acc
                        end
                end, [], cf_attributes:fetch_owned_by(UserId, device, Call)).
