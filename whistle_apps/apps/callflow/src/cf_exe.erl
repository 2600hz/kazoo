%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% @end
%%% Created : 18 Dec 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_exe).

-behaviour(gen_listener).

%% API
-export([start_link/4, relay_amqp/2]).
-export([get_call_info/1]).
-export([callid/1]).
-export([queue_name/1]).
-export([control_queue_name/1]).
-export([continue/1, continue/2]).
-export([branch/2]).
-export([stop/1]).
-export([transfer/1]).
-export([control_usurped/1]).
-export([get_branch_keys/1, get_all_branch_keys/1]).
-export([attempt/1, attempt/2]).
-export([callid_update/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2
         ,terminate/2, code_change/3]).

-include("callflow.hrl").

-define(SERVER, ?MODULE).

-define(CALL_SANITY_CHECK, 30000).

-define(RESPONDERS, [{{?MODULE, relay_amqp}, [{<<"*">>, <<"*">>}]}]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-record(state, {call = #cf_call{} :: #cf_call{}
                ,flow = wh_json:new() :: wh_json:json_object()
                ,cf_module_pid = 'undefined' :: 'undefined' | pid()
                ,status = <<"sane">> :: ne_binary()
                ,ctrl_q = 'undefined' :: 'undefined' | ne_binary()
                ,call_id = <<"0000000000">> :: ne_binary()
                ,sanity_timer = 'undefined' :: 'undefined' | timer:tref()
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
start_link(Flow, ControlQ, CallId, Call) ->    
    Bindings = [{call, [{callid, CallId}]}
                ,{self, []}
               ],
    gen_listener:start_link(?MODULE, [{responders, ?RESPONDERS}
                                      ,{bindings, Bindings}
                                      ,{queue_name, ?QUEUE_NAME}
                                      ,{queue_options, ?QUEUE_OPTIONS}
                                      ,{consume_options, ?CONSUME_OPTIONS}
                                     ], [Flow, ControlQ, CallId, Call]).

-spec get_call_info/1 :: (pid()) -> {ok, #cf_call{}}.
get_call_info(Srv) ->
    gen_server:call(Srv, {call_info}, 500).
    
-spec continue/1 :: (#cf_call{} | pid()) -> 'ok'.
-spec continue/2 :: (ne_binary(), #cf_call{} | pid()) -> 'ok'.
continue(Srv) ->
    continue(<<"_">>, Srv).

continue(Key, #cf_call{cf_pid=Srv}=Call) ->
    gen_server:cast(Srv, {continue, Key, Call});
continue(Key, Srv) ->
    gen_server:cast(Srv, {continue, Key}).

-spec branch/2 :: (wh_json:json_object(), #cf_call{} | pid()) -> 'ok'.
branch(Flow, #cf_call{cf_pid=Srv}) ->
    branch(Flow, Srv);
branch(Flow, Srv) ->
    gen_server:cast(Srv, {branch, Flow}).

-spec stop/1 :: (#cf_call{} | pid()) -> 'ok'.
stop(#cf_call{cf_pid=Srv}) ->
    stop(Srv);
stop(Srv) ->
    gen_server:cast(Srv, {stop}).

-spec transfer/1 :: (#cf_call{} | pid()) -> 'ok'.
transfer(#cf_call{cf_pid=Srv}) ->
    transfer(Srv);
transfer(Srv) ->
    gen_server:cast(Srv, {transfer}).

-spec control_usurped/1 :: (#cf_call{} | pid()) -> 'ok'.
control_usurped(#cf_call{cf_pid=Srv}) ->
    control_usurped(Srv);
control_usurped(Srv) ->
    gen_server:cast(Srv, {control_usurped}).

-spec callid_update/3 :: (ne_binary(), ne_binary(), #cf_call{} | pid()) -> 'ok'.
callid_update(CallId, CtrlQ, #cf_call{cf_pid=Srv}) ->
    callid_update(CallId, CtrlQ, Srv);
callid_update(CallId, CtrlQ, Srv) ->
    put(callid, CallId),
    gen_server:cast(Srv, {callid_update, CallId, CtrlQ}).

-spec callid/1 :: (#cf_call{} | pid()) -> ne_binary().
callid(#cf_call{cf_pid=Srv}) ->
    callid(Srv);
callid(Srv) ->
    CallId = gen_server:call(Srv, {callid}, 500),
    put(callid, CallId),
    CallId.

-spec queue_name/1 :: (#cf_call{} | pid()) -> ne_binary().
queue_name(#cf_call{cf_pid=Srv}) ->
    queue_name(Srv);
queue_name(Srv) ->
    gen_listener:queue_name(Srv).

-spec control_queue_name/1 :: (#cf_call{} | pid()) -> ne_binary().
control_queue_name(#cf_call{cf_pid=Srv}) ->
    control_queue_name(Srv);
control_queue_name(Srv) ->
    gen_server:call(Srv, {control_queue_name}).

-spec get_branch_keys/1 :: (#cf_call{} | pid()) -> {branch_keys, [ne_binary(),...]}.
get_branch_keys(#cf_call{cf_pid=Srv}) ->
    get_branch_keys(Srv);
get_branch_keys(Srv) ->
    gen_server:call(Srv, {get_branch_keys}).

-spec get_all_branch_keys/1 :: (#cf_call{} | pid()) -> {branch_keys, [ne_binary(),...]}.
get_all_branch_keys(#cf_call{cf_pid=Srv}) ->
    get_all_branch_keys(Srv);
get_all_branch_keys(Srv) ->
    gen_server:call(Srv, {get_branch_keys, all}).

-spec attempt/1 :: (#cf_call{} | pid()) -> {attempt_resp, ok} | {attempt_resp, {error, term()}}.
-spec attempt/2 :: (ne_binary(), #cf_call{} | pid()) -> {attempt_resp, ok} | {attempt_resp, {error, term()}}.
attempt(Srv) ->
    attempt(<<"_">>, Srv).
attempt(Key, #cf_call{cf_pid=Srv}) ->
    attempt(Key, Srv);
attempt(Key, Srv) ->
    gen_server:call(Srv, {attempt, Key}).

-spec relay_amqp/2 :: (wh_json:json_object(), proplist()) -> ok.
relay_amqp(JObj, Props) ->
    case props:get_value(cf_module_pid, Props) of
        Pid when is_pid(Pid) ->
            Pid ! {amqp_msg, JObj},
            ok;
        _Else ->
            %% TODO: queue?
            ?LOG("received event to relay while no module running, dropping"),
            ok
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
init([Flow, ControlQ, CallId, Call]) ->
    process_flag(trap_exit, true),
    put(callid, CallId),
    ?LOG_START("executing callflow ~s", [Call#cf_call.flow_id]),
    ?LOG("account id ~s", [Call#cf_call.account_id]),
    ?LOG("request ~s", [Call#cf_call.request]),
    ?LOG("to ~s", [Call#cf_call.to]),
    ?LOG("from ~s", [Call#cf_call.from]),
    ?LOG("CID ~s ~s", [Call#cf_call.cid_name, Call#cf_call.cid_number]),
    ?LOG("inception ~s", [Call#cf_call.inception]),
    ?LOG("authorizing id ~s", [Call#cf_call.authorizing_id]),
    {ok, TRef} = timer:send_after(?CALL_SANITY_CHECK, self(), {call_sanity_check}),
    {ok, #state{call=Call#cf_call{cf_pid=self()}, call_id=CallId, flow=Flow
                ,ctrl_q=ControlQ, sanity_timer=TRef}, 0}.

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
handle_call({call_info}, _From, #state{call=Call}=State) ->
    {reply, {ok, Call}, State};
handle_call({callid}, _From, #state{call_id=CallId}=State) ->
    {reply, CallId, State};
handle_call({control_queue_name}, _From, #state{ctrl_q=CtrlQ}=State) ->
    {reply, CtrlQ, State};
handle_call({get_branch_keys}, _From, #state{flow = Flow}=State) ->
    {struct, Children} = wh_json:get_value(<<"children">>, Flow, wh_json:new()),
    Reply = {branch_keys, lists:delete(<<"_">>, proplists:get_keys(Children))},
    {reply, Reply, State};
handle_call({get_branch_keys, all}, _From, #state{flow = Flow}=State) ->
    {struct, Children} = wh_json:get_value(<<"children">>, Flow, wh_json:new()),
    Reply = {branch_keys, proplists:get_keys(Children)},
    {reply, Reply, State};
handle_call({attempt, Key}, _From, #state{flow = Flow}=State) ->
    case wh_json:get_value([<<"children">>, Key], Flow) of
        undefined ->
            ?LOG("attempted undefined child ~s", [Key]),
            Reply = {attempt_resp, {error, undefined}},
            {reply, Reply, State};
        NewFlow ->
            case wh_json:is_empty(NewFlow) of
                true ->
                    ?LOG("attempted empty child ~s", [Key]),
                    Reply = {attempt_resp, {error, empty}},
                    {reply, Reply, State};
                false ->
                    ?LOG("branching to attempted child ~s", [Key]),
                    Reply = {attempt_resp, ok},
                    {reply, Reply, launch_cf_module(State#state{flow = NewFlow})}
            end
    end;
handle_call(_Request, _From, State) ->
    Reply = {error, unimplemented},
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
handle_cast({continue, Key, #cf_call{}=Call}, State) ->
    ?LOG("threading new cf_call{}"),
    handle_cast({continue, Key}, State#state{call=Call});
handle_cast({continue, Key}, #state{flow=Flow}=State) ->
    ?LOG("continuing to child ~s", [Key]),
    case wh_json:get_value([<<"children">>, Key], Flow) of
        undefined when Key =:= <<"_">> ->
            ?LOG("wildcard child does not exist, we are lost..."),
            {stop, normal, State};
        undefined ->
            ?LOG("requested child does not exist, trying wild card", [Key]),
            ?MODULE:continue(self()),
            {noreply, State};
        NewFlow ->
            case wh_json:is_empty(NewFlow) of
                true ->
                    {stop, normal, State};
                false ->
                    {noreply, launch_cf_module(State#state{flow=NewFlow})}
            end
    end;
handle_cast({stop}, State) ->
    {stop, normal, State};
handle_cast({transfer}, State) ->
    {stop, {shutdown, transfer}, State};
handle_cast({control_usurped}, State) ->
    {stop, {shutdown, control_usurped}, State};
handle_cast({branch, NewFlow}, State) ->
    ?LOG("callflow has been branched"),
    {noreply, launch_cf_module(State#state{flow=NewFlow})};
handle_cast({channel_status_received, JObj}, #state{sanity_timer=RunningTRef}=State) ->
    case wh_json:get_value(<<"Status">>, JObj) of
        <<"active">> ->
            ?LOG("callflow executer is sane... for now"),
            _ = timer:cancel(RunningTRef),
            {ok, TRef} = timer:send_after(?CALL_SANITY_CHECK, self(), {call_sanity_check}),
            {noreply, State#state{status = <<"sane">>, sanity_timer=TRef}};
        _Else ->
            ?LOG("call status is ~s, preparing to stop cf_exe", [_Else]),
            _ = timer:cancel(RunningTRef),
            {ok, TRef} = timer:send_after(?CALL_SANITY_CHECK, self(), {call_sanity_check}),
            {noreply, State#state{status = <<"testing">>, sanity_timer=TRef}}
    end;            
handle_cast({callid_update, NewCallId, NewCtrlQ}, #state{call_id=PrevCallId}=State) ->
    put(callid, NewCallId),
    ?LOG(PrevCallId, "updating callid to ~s, catch you on the flip side", [NewCallId]),
    ?LOG("removing call event bindings for ~s", [PrevCallId]),
    gen_listener:rm_binding(self(), call, [{callid, PrevCallId}]),
    ?LOG("binding to new call events"),
    gen_listener:add_binding(self(), call, [{callid, NewCallId}]),
    ?LOG("updating control q to ~s", [NewCtrlQ]),
    {noreply, State#state{call_id=NewCallId, ctrl_q=NewCtrlQ}};
handle_cast(_, State) ->
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
handle_info({'EXIT', _, normal}, State) ->
    %% handle normal exits so we dont need a guard on the next clause, cleaner looking...
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, #state{cf_module_pid=Pid, call=#cf_call{account_id=AccountId}=Call}=State) ->
    ?LOG(error, "action ~w died unexpectedly, ~p"
         ,[Pid, Reason, {extra_data, [{details, cf_util:call_to_proplist(Call)}
                                      ,{account_id, AccountId}
                                     ]}]),
    ?MODULE:continue(self()),
    {noreply, State};
handle_info({call_sanity_check}, #state{status = <<"testing">>, call=#cf_call{account_id=AccountId}=Call}=State) ->
    ?LOG(info, "callflow executer is insane, shuting down"
         ,[{extra_data, [{details, cf_util:call_to_proplist(Call)}
                         ,{account_id, AccountId}
                        ]}]),
    {stop, {shutdown, insane}, State#state{status = <<"insane">>}};
handle_info({call_sanity_check}, #state{call_id=CallId, call=Call}=State) ->
    ?LOG("ensuring call is active, requesting controlling channel status"),
    spawn(fun() -> cf_call_command:channel_status(CallId, Call) end),
    {ok, TRef} = timer:send_after(?CALL_SANITY_CHECK, self(), {call_sanity_check}),
    {noreply, State#state{status = <<"testing">>, sanity_timer=TRef}};
handle_info(timeout, #state{call_id=CallId, ctrl_q=CtrlQ, call=#cf_call{cf_pid=Self}}=State) ->
    spawn(fun() ->
                  ControllerQ = queue_name(Self),
                  send_controller_queue(ControllerQ, CallId, CtrlQ)
          end),
    {noreply, launch_cf_module(State)};
handle_info(_, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
handle_event(JObj, #state{cf_module_pid=Pid, call_id=CallId}) ->
    case {whapps_util:get_event_type(JObj), wh_json:get_value(<<"Call-ID">>, JObj)}of
        {{<<"call_event">>, <<"channel_status_resp">>}, _} ->
            gen_server:cast(self(), {channel_status_received, JObj}),
            {reply, [{cf_module_pid, Pid}]};
        {{<<"call_event">>, <<"call_status_resp">>}, _} ->
            {reply, [{cf_module_pid, Pid}]};
        {{<<"call_event">>, <<"call_id_update">>},_} ->
            NewCallId = wh_json:get_value(<<"Call-ID">>, JObj),
            NewCtrlQ = wh_json:get_value(<<"Control-Queue">>, JObj),
            callid_update(NewCallId, NewCtrlQ, self()),
            ignore;
        {{<<"call_event">>, <<"control_transfer">>}, _} ->
            transfer(self()),
            ignore;
        {{<<"call_event">>, <<"usurp_control">>}, CallId} ->
            Srv = self(),
            spawn(fun() ->
                          put(callid, CallId),
                          Q = queue_name(Srv),
                          case wh_json:get_value(<<"Controller-Queue">>, JObj) of
                              Q -> ok;
                              _Else -> control_usurped(Srv)
                          end
                  end),
            ignore;
        {{<<"error">>, _}, _} ->
            {reply, [{cf_module_pid, Pid}]};
        {_, CallId} ->
            {reply, [{cf_module_pid, Pid}]};
        {_, _Else} ->
            ?LOG("received event from call ~s while relaying for ~s, dropping", [_Else, CallId]),
            ignore
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
terminate({shutdown, transfer}, #state{sanity_timer=TRef}) ->
    _ = timer:cancel(TRef),
    ?LOG_END("callflow execution has been transfered"),
    ok;
terminate({shutdown, insane}, #state{sanity_timer=TRef}) ->
    _ = timer:cancel(TRef),
    ?LOG_END("call id is not longer present on the media gateways, terminating callflow execution"),
    ok;
terminate({shutdown, control_usurped}, #state{sanity_timer=TRef}) ->
    _ = timer:cancel(TRef),
    ?LOG_END("the call has been usurped by an external process"),
    ok;
terminate(_Reason, #state{sanity_timer=TRef}=State) ->
    _ = timer:cancel(TRef),
    Command = [{<<"Application-Name">>, <<"hangup">>}
               ,{<<"Insert-At">>, <<"now">>}
              ],
    send_command(Command, State),
    ?LOG_END("callflow execution has been stopped: ~p", [_Reason]),
    ok.

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

%%--------------------------------------------------------------------
%% @private
%% this function determines if the callflow module specified at the 
%% current node is 'available' and attempts to launch it if so. 
%% Otherwise it will advance to the next child in the flow
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec launch_cf_module/1 :: (#state{}) -> #state{}.
launch_cf_module(#state{call=#cf_call{last_action=LastAction, account_id=AccountId}=Call, flow=Flow, call_id=CallId}=State) ->
    Module = <<"cf_", (wh_json:get_value(<<"module">>, Flow))/binary>>,
    Data = wh_json:get_value(<<"data">>, Flow),
    {Pid, Action} =
        try
            CFModule = wh_util:to_atom(Module, true),
            ?LOG("moving to action ~s", [Module]),
            spawn_cf_module(CFModule, Data, CallId, Call)
        catch
            _:_ ->
                ?LOG(error, "unknown callflow action: ~p"
                     ,[Module, {extra_data, [{details, cf_util:call_to_proplist(Call)}
                                             ,{account_id, AccountId}
                                            ]}]),
                ?MODULE:continue(self()),
                {undefined, LastAction}
        end,
    State#state{cf_module_pid=Pid, call=Call#cf_call{last_action=Action}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% helper function to spawn a linked callflow module, from the entry
%% point 'handle' having set the callid on the new process first
%% @end
%%--------------------------------------------------------------------
-spec spawn_cf_module/4 :: (atom(), list(), ne_binary(), #cf_call{}) -> {pid(), atom()}.
spawn_cf_module(CFModule, Data, CallId, Call) ->
    {spawn_link(fun() ->
                        put(callid, CallId),
                        CFModule:handle(Data, Call)
                end), CFModule}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% unlike the cf_call_command this send command does not call the 
%% functions of this module to form the headers, nor does it set
%% the reply queue.  Used when this module is terminating to send
%% a hangup command without relying on the (now terminated) cf_exe.
%% @end
%%--------------------------------------------------------------------
-spec send_command/2 :: (proplist(), #state{}) -> ok.
send_command(Command, #state{call_id=CallId, ctrl_q=CtrlQ}) when is_binary(CallId), is_binary(CtrlQ) ->
    Prop = Command ++ [{<<"Call-ID">>, CallId}
                       | wh_api:default_headers(<<>>, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
                      ],
    wapi_dialplan:publish_command(CtrlQ, Prop);    
send_command(_, _) ->
    ok.

-spec send_controller_queue/3 :: (ne_binary(), ne_binary(), ne_binary()) -> ok.
send_controller_queue(ControllerQ, CallId, CtrlQ) ->
    Props = [{<<"Call-ID">>, CallId}
            ,{<<"Controller-Queue">>, ControllerQ}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    wapi_call:publish_controller_queue(CtrlQ, Props).    
    
