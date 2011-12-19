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
-export([callid/1, queue_name/1, control_queue_name/1]).
-export([continue/1, continue/2, branch/2, stop/1, transfer/1]).
-export([get_branch_keys/1, get_all_branch_keys/1, attempt/1, attempt/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2
	 ,terminate/2, code_change/3]).

-include("callflow.hrl").

-define(SERVER, ?MODULE).

-define(CALL_SANITY_CHECK, 60000).

-define(RESPONDERS, [{{?MODULE, relay_amqp}, [{<<"*">>, <<"*">>}]}]).
-define(QUEUE_NAME, <<"">>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-record(state, {call = #cf_call{} :: #cf_call{}
                ,flow = ?EMPTY_JSON_OBJECT :: json_object()
                ,cf_module_pid = undefined :: undefined | pid()
                ,status = <<"sane">> :: ne_binary()
                ,ctrl_q = undefined :: undefined | ne_binary()
                ,call_id = <<"0000000000">> :: ne_binary()
                ,sanity_timer = undefined :: undefined | timer:tref()
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
                ,{self, []}],
    gen_listener:start_link(?MODULE, [{responders, ?RESPONDERS}
				      ,{bindings, Bindings}
				      ,{queue_name, ?QUEUE_NAME}
				      ,{queue_options, ?QUEUE_OPTIONS}
				      ,{consume_options, ?CONSUME_OPTIONS}
				     ], [Flow, ControlQ, CallId, Call]).

-spec callid/1 :: (#cf_call{} | pid()) -> ne_binary().
callid(#cf_call{cf_pid=Srv}) ->
    callid(Srv);
callid(Srv) ->
    CallId = gen_server:call(Srv, {callid}),
    put(callid, CallId),
    CallId.

-spec queue_name/1 :: (#cf_call{} | pid()) -> ne_binary().
queue_name(#cf_call{cf_pid=Srv}) ->
    gen_listener:queue_name(Srv).

-spec control_queue_name/1 :: (#cf_call{} | pid()) -> ne_binary().
control_queue_name(#cf_call{cf_pid=Srv}) ->
    control_queue_name(Srv);
control_queue_name(Srv) ->
    gen_server:call(Srv, {control_queue_name}).

-spec continue/1 :: (#cf_call{} | pid()) -> ok.
-spec continue/2 :: (ne_binary(), #cf_call{} | pid()) -> ok.
continue(Srv) ->
    continue(<<"_">>, Srv).

continue(Key, #cf_call{cf_pid=Srv}) ->
    continue(Key, Srv);
continue(Key, Srv) ->
    gen_server:cast(Srv, {continue, Key}).

-spec branch/2 :: (json_object(), #cf_call{} | pid()) -> ok.
branch(Flow, #cf_call{cf_pid=Srv}) ->
    branch(Flow, Srv);
branch(Flow, Srv) ->
    gen_server:cast(Srv, {branch, Flow}).

-spec stop/1 :: (#cf_call{} | pid()) -> ok.
stop(#cf_call{cf_pid=Srv}) ->
    stop(Srv);
stop(Srv) ->
    gen_server:cast(Srv, {stop}).

-spec transfer/1 :: (#cf_call{} | pid()) -> ok.
transfer(#cf_call{cf_pid=Srv}) ->
    transfer(Srv);
transfer(Srv) ->
    gen_server:cast(Srv, {transfer}).

-spec get_branch_keys/1 :: (#cf_call{} | pid()) -> [ne_binary(),...].
get_branch_keys(#cf_call{cf_pid=Srv}) ->
    get_branch_keys(Srv);
get_branch_keys(Srv) ->
    gen_server:call(Srv, {get_branch_keys}).

-spec get_all_branch_keys/1 :: (#cf_call{} | pid()) -> [ne_binary(),...].
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

-spec relay_amqp/2 :: (json_object(), proplist()) -> ok.
relay_amqp(JObj, Props) ->
    case props:get_value(cf_module_pid, Props) of
        Pid when is_pid(Pid) ->
            Pid ! {amqp_msg, JObj},
            ok;
        _Else ->
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
handle_call({callid}, _From, #state{call_id=CallId}=State) ->
    {reply, CallId, State};
handle_call({control_queue_name}, _From, #state{ctrl_q=CtrlQ}=State) ->
    {reply, CtrlQ, State};
handle_call({get_branch_keys}, _From, #state{flow = Flow}=State) ->
    {struct, Children} = wh_json:get_value(<<"children">>, Flow, ?EMPTY_JSON_OBJECT),
    Reply = {branch_keys, lists:delete(<<"_">>, proplists:get_keys(Children))},
    {reply, Reply, State};
handle_call({get_branch_keys, all}, _From, #state{flow = Flow}=State) ->
    {struct, Children} = wh_json:get_value(<<"children">>, Flow, ?EMPTY_JSON_OBJECT),
    Reply = {branch_keys, proplists:get_keys(Children)},
    {reply, Reply, State};
handle_call({attempt, Key}, _From, #state{flow = Flow}=State) ->
    case wh_json:get_value([<<"children">>, Key], Flow) of
        undefined ->
            ?LOG("attempted undefined child ~s", [Key]),
            Reply = {attempt_resp, {error, undefined}},
            {reply, Reply, State};
        ?EMPTY_JSON_OBJECT ->
            ?LOG("attempted empty child ~s", [Key]),
            Reply = {attempt_resp, {error, empty}},
            {reply, Reply, State};
        NewFlow ->
            ?LOG("branching to attempted child ~s", [Key]),
            Reply = {attempt_resp, ok},
            {reply, Reply, launch_cf_module(State#state{flow = NewFlow})}
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
handle_cast({continue, Key}, #state{flow=Flow}=State) ->
    ?LOG("continuing to child ~s", [Key]),
    case wh_json:get_value([<<"children">>, Key], Flow) of
        undefined when Key =:= <<"_">> ->
            {stop, normal, State};
        undefined ->
            ?LOG("requested child does not exist, trying wild card", [Key]),
            ?MODULE:continue(self()),
            {noreply, State};
        ?EMPTY_JSON_OBJECT ->
            {stop, normal, State};
        NewFlow ->
            {noreply, launch_cf_module(State#state{flow=NewFlow})}
    end;
handle_cast({stop}, State) ->
    {stop, normal, State};
handle_cast({transfer}, State) ->
    {stop, transfer, State};
handle_cast({branch, NewFlow}, State) ->
    ?LOG("callflow has been branched"),
    {noreply, launch_cf_module(State#state{flow=NewFlow})};
handle_cast({call_status_received, _}, State) ->
    ?LOG("callflow executer is sane... for now"),
    {noreply, State#state{status = <<"sane">>}};
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
    ?LOG("action ~w died unexpectedly, ~p", [Pid, Reason]),
    spawn(fun() ->
                  Message = ["Source: ~s(~p)~n"
                             ,"Alert: action died unexpectedly~n"
                             ,"Fault: ~p~n"
                             ,"~n~s"
                            ],
                  Args = [?MODULE, ?LINE, Reason, cf_util:call_info_to_string(Call)],
                  whapps_util:alert(<<"notice">>, lists:flatten(Message), Args, AccountId)
          end),
    ?MODULE:continue(self()),
    {noreply, State};
handle_info({call_sanity_check}, #state{status = <<"testing">>, call=#cf_call{account_id=AccountId}=Call}=State) ->
    ?LOG("call executer is insane, shuting down"),
    spawn(fun() ->
                  Message = ["Source: ~s(~p)~n"
                             ,"Alert: forced channel termination~n"
                             ,"Fault: call sanity check failed~n"
                             ,"~n~s"
                            ],
                  Args = [?MODULE, ?LINE, cf_util:call_info_to_string(Call)],
                  whapps_util:alert(<<"notice">>, lists:flatten(Message), Args, AccountId)
          end),
    {stop, insane, State#state{status = <<"insane">>}};
handle_info({call_sanity_check}, #state{call_id=CallId}=State) ->
    ?LOG("ensuring call is active, requesting call status"),
    Q = ?MODULE:queue_name(self()),
    Req = [{<<"Call-ID">>, CallId}
           | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)],
    wapi_call:publish_status_req(CallId, Req),
    {ok, TRef} = timer:send_after(?CALL_SANITY_CHECK, self(), {call_sanity_check}),
    {noreply, State#state{status = <<"testing">>, sanity_timer=TRef}};
handle_info(timeout, State) ->
    {noreply, launch_cf_module(State)};
handle_info(_, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
handle_event(JObj, #state{cf_module_pid=Pid}) ->
    case whapps_util:get_event_type(JObj) of
        {<<"call_event">>, <<"status_resp">>} ->
            ?LOG("received call status"),
            gen_server:cast(self(), {call_status_received, JObj});
        _Else ->
            ok
    end,
    {reply, [{cf_module_pid, Pid}]}.

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
terminate(transfer, #state{sanity_timer=TRef}) ->
    timer:cancel(TRef),
    ?LOG_END("callflow execution has been transfered"),
    ok;
terminate(_Reason, #state{sanity_timer=TRef}=State) ->
    timer:cancel(TRef),
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
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec launch_cf_module/1 :: (#state{}) -> #state{}.
launch_cf_module(#state{call=#cf_call{last_action=LastAction}=Call, flow=Flow, call_id=CallId}=State) ->
    Module = <<"cf_", (wh_json:get_value(<<"module">>, Flow))/binary>>,
    Data = wh_json:get_value(<<"data">>, Flow),
    {Pid, Action} =
        try
            CFModule = wh_util:to_atom(Module, true),
            ?LOG("moving to action ~s", [Module]),
            spawn_cf_module(CFModule, Data, CallId, Call)
        catch
            _:_ ->
                ?LOG("unknown action ~s, skipping", [Module]),
                ?MODULE:continue(self()),
                {undefined, LastAction}
        end,
    State#state{cf_module_pid=Pid, call=Call#cf_call{last_action=Action}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
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
%% @end
%%--------------------------------------------------------------------
-spec send_command/2 :: (proplist(), #state{}) -> ok.
send_command(Command, #state{call_id=CallId, ctrl_q=CtrlQ}) ->
    Prop = Command ++ [{<<"Call-ID">>, CallId}
                       | wh_api:default_headers(<<>>, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
                      ],
    wapi_dialplan:publish_command(CtrlQ, Prop).
