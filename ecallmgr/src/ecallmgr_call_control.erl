%%%-------------------------------------------------------------------
%%% @copyright (C) 2010, VoIP INC
%%% @doc
%%% Created when a call hits a fetch_handler in ecallmgr_route.
%%% A Control Queue is created by the lookup_route function in the
%%% fetch_handler. On initialization, besides adding itself as the
%%% consumer for the AMQP messages, Call Control creates an empty queue
%%% object (not to be confused with AMQP queues), sets the current
%%% application running on the switch to the empty binary, and records
%%% the timestamp of when the initialization finishes. The process then
%%% enters its loop to wait.
%%%
%%% When receiving an AMQP message, after decoding the JSON into a proplist,
%%% we check if the application is "queue" or not; if it is "queue", we
%%% extract the default headers out, iterate through the Commands portion,
%%% and append the default headers to the application-specific portions, and
%%% insert these commands into the CmdQ. We then check whether the old CmdQ is
%%% empty AND the new CmdQ is not, and that the current App is the empty
%%% binary. If so, we dequeue the next command, execute it, and loop; otherwise
%%% we loop with the CmdQ.
%%% If just a single application is sent in the message, we check the CmdQ's
%%% size and the current App's status; if both are empty, we fire the command
%%% immediately; otherwise we add the command to the CmdQ and loop.
%%%
%%% When receiving an {execute_complete, CALLID, EvtName} tuple from
%%% the corresponding ecallmgr_call_events process tracking the call,
%%% we convert the CurrApp name from Whistle parlance to FS, matching
%%% it against what application name we got from FS via the events
%%% process. If CurrApp is empty, we just loop since the completed
%%% execution probably wasn't related to our stuff (perhaps FS internal);
%%% if the converted Whistle name matches the passed FS name, we know
%%% the CurrApp cmd has finished and can execute the next command in the
%%% queue. If there are no commands in the queue, set CurrApp to 'undefined' and
%%% loop; otherwise take the next command, execute it, and look with it as
%%% the CurrApp. If EvtName and the converted Whistle name don't match,
%%% something else executed that might have been related to the main
%%% application's execute (think set commands, like playback terminators);
%%% we can note the event happened, and continue looping as we were.
%%%
%%% @end
%%%
%%% @contributors
%%% James Aimonetti <james@2600hz.org>
%%% Karl Anderson <karl@2600hz.org>
%%%
%%% Created : 26 Aug 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_call_control).

-behaviour(gen_listener).

%% API
-export([start_link/3]).
-export([handle_call_command/2, handle_conference_command/2, handle_call_events/2]).
-export([queue_name/1, callid/1]).
-export([event_execute_complete/3]).
-export([add_leg/1, rm_leg/1]).
-export([other_legs/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2
         ,terminate/2, code_change/3]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).
-define(KEEP_ALIVE, 5000). %% after hangup, keep alive for 5 seconds

-type insert_at_options() :: 'now' | 'head' | 'tail' | 'flush'.

-record(state, {
          node = 'undefined' :: atom()
         ,callid = <<>> :: binary()
         ,self = 'undefined' :: 'undefined' | pid()
         ,controller_q = 'undefined' :: 'undefined' | ne_binary()
         ,evtpid = 'undefined' :: 'undefined' | pid()
         ,command_q = queue:new() :: queue()
         ,current_app = 'undefined' :: ne_binary() | 'undefined'
         ,start_time = erlang:now() :: wh_now()
         ,is_node_up = 'true' :: boolean()
         ,keep_alive_ref = 'undefined' :: 'undefined' | reference()
         ,other_legs = [] :: [] | [ne_binary(),...]
         ,last_removed_leg = 'undefined' :: 'undefined' | ne_binary()
         ,sanity_check_tref = 'undefined' :: 'undefined' | reference()
         }).

-define(RESPONDERS, [{{?MODULE, handle_call_command}, [{<<"call">>, <<"command">>}]}
                     ,{{?MODULE, handle_conference_command}, [{<<"conference">>, <<"command">>}]}
                     ,{{?MODULE, handle_call_events}, [{<<"call_event">>, <<"*">>}]}]).
-define(QUEUE_NAME, <<"">>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

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
start_link(Node, CallId, WhAppQ) ->
    %% We need to become completely decoupled from ecallmgr_call_events
    %% because the call_events process might have been spun up with A->B
    %% then transfered to A->D, but the route landed in a different 
    %% ecallmgr.  Since our call_events will get a bad session if we
    %% try to handlecall more than once on a UUID we had to leave the
    %% call_events running on another ecallmgr... fun fun
    Bindings = [{call, [{callid, CallId}]}
                ,{dialplan, []}
                ,{self, []}],
    gen_listener:start_link(?MODULE, [{responders, ?RESPONDERS}
                                      ,{bindings, Bindings}
                                      ,{queue_name, ?QUEUE_NAME}
                                      ,{queue_options, ?QUEUE_OPTIONS}
                                      ,{consume_options, ?CONSUME_OPTIONS}
                                     ], [Node, CallId, WhAppQ]).

-spec callid/1 :: (pid()) -> ne_binary().
callid(Srv) ->
    gen_listener:call(Srv, {callid}).

-spec queue_name/1 :: (pid()) -> ne_binary().
queue_name(Srv) ->
    gen_listener:queue_name(Srv).

-spec other_legs/1 :: (pid()) -> [] | [ne_binary(),...].
other_legs(Srv) ->
    gen_listener:call(Srv, {other_legs}).

-spec event_execute_complete/3 :: (pid(), ne_binary(), ne_binary()) -> 'ok'.
event_execute_complete(Srv, CallId, App) ->
    gen_listener:cast(Srv, {event_execute_complete, CallId, App}).

-spec add_leg/1 :: (proplist()) -> pid().
add_leg(Props) ->
    spawn(fun() ->
                  %% if there is a Other-Leg-Unique-ID then that MAY refer to a leg managed
                  %% by call_control, if so add the leg to it
                  CallId = props:get_value(<<"Other-Leg-Unique-ID">>, Props),
                  put(callid, CallId),
                  case is_binary(CallId) andalso ecallmgr_call_control_sup:find_worker(CallId) of
                      false -> ok;
                      {error, _} -> ok;
                      {ok, Srv} -> 
                          gen_listener:cast(Srv, {add_leg, wh_json:from_list(Props)})
                  end
          end).

-spec rm_leg/1 :: (proplist()) -> pid().
rm_leg(Props) ->
    spawn(fun() ->
                  %% if there is a Other-Leg-Unique-ID then that MAY refer to a leg managed
                  %% by call_control, if so remove the leg from it
                  CallId = props:get_value(<<"Other-Leg-Unique-ID">>, Props),
                  put(callid, CallId),
                  case is_binary(CallId) andalso ecallmgr_call_control_sup:find_worker(CallId) of
                      false -> ok;
                      {error, _} -> ok;
                      {ok, Srv} -> 
                          gen_listener:cast(Srv, {rm_leg, wh_json:from_list(Props)})
                  end
          end).

-spec handle_call_command/2 :: (json_object(), proplist()) -> ok.
handle_call_command(JObj, Props) ->
    Srv = props:get_value(server, Props),
    gen_listener:cast(Srv, {dialplan, JObj}).

-spec handle_conference_command/2 :: (json_object(), proplist()) -> ok.
handle_conference_command(JObj, Props) ->
    Srv = props:get_value(server, Props),
    gen_listener:cast(Srv, {dialplan, JObj}).

-spec handle_call_events/2 :: (json_object(), proplist()) -> ok.
handle_call_events(JObj, Props) ->
    Srv = props:get_value(server, Props),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    put(callid, CallId),
    case {wh_json:get_value(<<"Event-Name">>, JObj), wh_util:get_transfer_state(JObj)} of
        {<<"CHANNEL_EXECUTE_COMPLETE">>, _} ->
            Application = wh_json:get_value(<<"Raw-Application-Name">>, JObj),
            ?LOG("control queue ~p channel execute completion for '~s'", [Srv, Application]),
            gen_listener:cast(Srv, {event_execute_complete, CallId, Application});
        {<<"CHANNEL_DESTROY">>, _} ->
            gen_listener:cast(Srv, {channel_destroyed, JObj});
        {<<"CHANNEL_HANGUP">>, undefined} ->
            ok;
        {<<"CHANNEL_HANGUP">>, Transfer} ->
            ?LOG("control queue ~p channel hangup due to a transfer and we are the ~s", [Srv, Transfer]),
            gen_listener:cast(Srv, {Transfer, JObj});
        {<<"CHANNEL_UNBRIDGE">>, undefined} ->
            gen_listener:cast(Srv, {rm_leg, JObj});
        {<<"CHANNEL_UNBRIDGE">>, Transfer} ->
            ?LOG("control queue ~p channel unbridged due to a transfer and we are the ~s", [Srv, Transfer]),
            gen_listener:cast(Srv, {Transfer, JObj});
        {<<"CHANNEL_BRIDGE">>, _} ->
            gen_listener:cast(Srv, {add_leg, JObj});
        {<<"controller_queue">>, _} ->
            ControllerQ = wh_json:get_value(<<"Controller-Queue">>, JObj),
            gen_listener:cast(Srv, {controller_queue, ControllerQ});
        {_, _} ->
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
init([Node, CallId, WhAppQ]) ->
    put(callid, CallId),
    ?LOG_START("starting call control listener"),
    erlang:monitor_node(Node, true),
    TRef = erlang:send_after(?SANITY_CHECK_PERIOD, self(), {sanity_check}),
    {ok, #state{node=Node, callid=CallId, command_q=queue:new(), self=self()
                ,controller_q=WhAppQ, start_time=erlang:now(), sanity_check_tref=TRef}
    }.

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
handle_call({callid}, _From, #state{callid=CallId}=State) ->
    {reply, CallId, State};
handle_call({other_legs}, _From, #state{other_legs=Legs}=State) ->
    {reply, Legs, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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
handle_cast({controller_queue, ControllerQ}, State) ->
    ?LOG("updating controller queue to ~s", [ControllerQ]),
    {noreply, State#state{controller_q=ControllerQ}};
handle_cast({transferer, _}, #state{last_removed_leg=undefined, other_legs=[]}=State) ->    
    %% if the callee preforms a blind transfer then sometimes the new control
    %% listener is built so quickly that it receives the transferer event ment
    %% to tear down the old one.  However, a new control listener will not have
    %% nor removed any legs. This is just pain hackish but its working...
    ?LOG("ignoring transferer as it is a residual event for the other control listener"),
    {noreply, State};
handle_cast({transferer, _}, #state{callid=CallId, controller_q=ControllerQ}=State) ->
    ?LOG("call control has been transfered"),
    spawn(fun() -> publish_control_transfer(ControllerQ, CallId) end),
    {stop, normal, State};
handle_cast({transferee, JObj}, #state{other_legs=Legs, node=Node, callid=PrevCallId, self=Self}=State) ->
    %% TODO: once we are satisfied that this is not breaking anything we can reduce the verbosity...
    OtherLegCallId =  wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj),
    case OtherLegCallId =/= undefined andalso freeswitch:api(Node, uuid_dump, wh_util:to_list(OtherLegCallId)) of
        {ok, Result} ->
            %% this next line makes the whole thing work...
            ?LOG("OK, but... you asked for it. Hold on to your butts!"),
            Props = ecallmgr_util:eventstr_to_proplist(Result),
            NewCallId = props:get_value(<<"Channel-Call-UUID">>, Props),
            spawn(fun() -> publish_callid_update(PrevCallId, NewCallId, queue_name(Self)) end),
            ?LOG("updating callid to ~s", [NewCallId]),
            put(callid, NewCallId),
            ?LOG("removing call event bindings for ~s", [PrevCallId]),
            gen_listener:rm_binding(self(), call, [{callid, PrevCallId}]),
            ?LOG("binding to new call events"),
            gen_listener:add_binding(self(), call, [{callid, NewCallId}]),
            ?LOG("ensuring event listener exists"),
            _ = ecallmgr_call_sup:start_event_process(Node, NewCallId),
            ?LOG("....You did it. You crazy son of a bitch you did it."),
            {noreply, State#state{callid=NewCallId, other_legs=lists:delete(NewCallId, Legs)}};
        _ ->
            {noreply, State}
    end;
handle_cast({add_leg, JObj}, #state{other_legs=Legs, node=Node, callid=CallId}=State) ->
    LegId = case wh_json:get_value(<<"Event-Name">>, JObj) of
                <<"CHANNEL_BRIDGE">> ->
                    wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj);
                _ ->
                    wh_json:get_value(<<"Caller-Unique-ID">>, JObj)
                end,
    case is_atom(LegId) orelse lists:member(LegId, Legs) of
        true -> {noreply, State};
        false ->
            ?LOG("added leg ~s to call", [LegId]),
            _ = spawn(fun() ->
			      _ = put(callid, CallId),
			      publish_leg_addition(JObj)
		      end),
            ?LOG("ensuring event listener for leg ~s exists", [LegId]),
            _ = ecallmgr_call_sup:start_event_process(Node, LegId),
            {noreply, State#state{other_legs=[LegId|Legs]}}
    end;
handle_cast({rm_leg, JObj}, #state{other_legs=Legs, callid=CallId}=State) ->
    LegId = case wh_json:get_value(<<"Event-Name">>, JObj) of
                <<"CHANNEL_UNBRIDGE">> ->
                    wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj);
                _ ->
                    wh_json:get_value(<<"Caller-Unique-ID">>, JObj)
            end,
    case lists:member(LegId, Legs) of
        false -> 
            {noreply, State};
        true ->
            ?LOG("removed leg ~s from call", [LegId]),
            _ = spawn(fun() ->
			      put(callid, CallId),
			      publish_leg_removal(JObj)
		      end),
            {noreply, State#state{other_legs=lists:delete(LegId, Legs), last_removed_leg=LegId}}
    end;
handle_cast({channel_destroyed, _},  #state{command_q=CmdQ, sanity_check_tref=SCTRef}=State) ->
    ?LOG("our channel has been destroyed, executing any post-hangup commands"),
    lists:foreach(fun(Cmd) -> execute_control_request(Cmd, State) end, post_hangup_commands(CmdQ)),
    %% if our sanity check timer is running stop it, it will always return false
    %% now that the channel is gone
    catch (erlang:cancel_timer(SCTRef)), 
    %% if the keep-alive timer is not already running start it
    %% different then get_keep_alive_ref (which resets it)
    KATRef = case State#state.keep_alive_ref of
                 undefined ->
                     erlang:send_after(?KEEP_ALIVE, self(), keep_alive_expired);
                 Else ->
                     Else
             end,
    {noreply, State#state{keep_alive_ref=KATRef}, hibernate};    
handle_cast({dialplan, JObj}
            ,#state{keep_alive_ref=Ref, command_q=CmdQ, current_app=CurrApp, is_node_up=INU}=State) ->
    NewCmdQ = try
                  insert_command(State, wh_util:to_atom(wh_json:get_value(<<"Insert-At">>, JObj, 'tail')), JObj)
              catch _T:_R ->
                      ?LOG("failed to insert command into control queue: ~p:~p", [_T, _R]),
                      CmdQ
              end,
    case INU andalso (not queue:is_empty(NewCmdQ)) andalso CurrApp =:= undefined of
        true ->
            {{value, Cmd}, NewCmdQ1} = queue:out(NewCmdQ),
            execute_control_request(Cmd, State),
            AppName = wh_json:get_value(<<"Application-Name">>, Cmd),
            {noreply, State#state{command_q = NewCmdQ1, current_app = AppName, keep_alive_ref=get_keep_alive_ref(Ref)}, hibernate};
        false ->
            {noreply, State#state{command_q = NewCmdQ, keep_alive_ref=get_keep_alive_ref(Ref)}, hibernate}
    end;
handle_cast({event_execute_complete, CallId, EvtName}
            ,#state{callid=CallId, command_q=CmdQ, current_app=CurrApp, is_node_up=INU}=State) ->
    case lists:member(EvtName, wh_api:convert_whistle_app_name(CurrApp)) of
        false ->
            {noreply, State};
        true ->
            ?LOG("completed execution of command '~s'", [CurrApp]),
            case INU andalso queue:out(CmdQ) of
                false ->
                    %% if the node is down, don't inject the next FS event
                    ?LOG("not continuing until the media node becomes avaliable"),
                    {noreply, State#state{current_app=undefined}, hibernate};
                {empty, _} ->
                    ?LOG("no call commands remain queued, hibernating"),
                    {noreply, State#state{current_app=undefined}, hibernate};
                {{value, Cmd}, CmdQ1} ->
                    execute_control_request(Cmd, State),
                    AppName = wh_json:get_value(<<"Application-Name">>, Cmd),
                    {noreply, State#state{command_q = CmdQ1, current_app = AppName}, hibernate}
            end
    end;
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
handle_info({nodedown, Node}, #state{node=Node, is_node_up=true}=State) ->
    ?LOG_SYS("lost connection to media node ~s, waiting for reconnection", [Node]),
    erlang:monitor_node(Node, false),
    _Ref = erlang:send_after(0, self(), {is_node_up, 100}),
    {noreply, State#state{is_node_up=false}, hibernate};
handle_info({is_node_up, Timeout}, #state{node=Node, is_node_up=false}=State) ->
    case ecallmgr_util:is_node_up(Node) of
        true ->
            erlang:monitor_node(Node, true),
            ?LOG("reconnected to node ~s", [Node]),
            {noreply, State#state{is_node_up=true}, hibernate};
        false ->
            _Ref = case Timeout >= ?MAX_TIMEOUT_FOR_NODE_RESTART of
                          true ->
                              ?LOG("node ~p down, waiting ~p to check again", [Node, ?MAX_TIMEOUT_FOR_NODE_RESTART]),
                              erlang:send_after(?MAX_TIMEOUT_FOR_NODE_RESTART, self(), {is_node_up, ?MAX_TIMEOUT_FOR_NODE_RESTART});
                          false ->
                              ?LOG("node ~p down, waiting ~p to check again", [Node, Timeout]),
                              erlang:send_after(Timeout, self(), {is_node_up, Timeout*2})
                      end,
            {noreply, State}
    end;
handle_info({force_queue_advance, CallId}, #state{callid=CallId, command_q=CmdQ, is_node_up=INU, current_app=CurrApp}=State) ->
    ?LOG("received control queue unconditional advance, skipping wait for command completion of '~s'", [CurrApp]),
    case INU andalso queue:out(CmdQ) of
        false ->
            %% if the node is down, don't inject the next FS event
            ?LOG("not continuing until the media node becomes avaliable"),
            {noreply, State#state{current_app = undefined}, hibernate};
        {empty, _} ->
            ?LOG("no call commands remain queued, hibernating"),
            {noreply, State#state{current_app = undefined}, hibernate};
        {{value, Cmd}, CmdQ1} ->
            execute_control_request(Cmd, State),
            AppName = wh_json:get_value(<<"Application-Name">>, Cmd),
            {noreply, State#state{command_q = CmdQ1, current_app = AppName}, hibernate}
    end;
handle_info(keep_alive_expired, State) ->
    ?LOG("no new commands received after channel destruction, our job here is done"),
    {stop, normal, State};
handle_info({sanity_check}, #state{node=Node, callid=CallId, keep_alive_ref=undefined}=State) ->
    case freeswitch:api(Node, uuid_exists, wh_util:to_list(CallId)) of
        {'ok', <<"true">>} -> 
            ?LOG("listener passed sanity check, call is still up"),
            TRef = erlang:send_after(?SANITY_CHECK_PERIOD, self(), {sanity_check}),
            {'noreply', State#state{sanity_check_tref=TRef}};
        _ ->
            ?LOG("I must be crazy to be in a loony bin like this"),
            {stop, normal, State#state{sanity_check_tref=undefined}}
    end;
handle_info(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, _State) ->
    {reply, [{server, self()}]}.

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
terminate(_Reason, #state{start_time=StartTime,  sanity_check_tref=SCTRef, keep_alive_ref=KATRef}) ->
    catch (erlang:cancel_timer(SCTRef)), 
    catch (erlang:cancel_timer(KATRef)), 
    ?LOG_END("control queue was up for ~p microseconds", [timer:now_diff(erlang:now(), StartTime)]),
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

%% execute all commands in JObj immediately, irregardless of what is running (if anything).
-spec insert_command/3 :: (#state{}, insert_at_options(), json_object()) -> queue().
insert_command(#state{node=Node, callid=CallId, command_q=CommandQ, is_node_up=IsNodeUp}=State, now, JObj) ->
    AName = wh_json:get_value(<<"Application-Name">>, JObj),
    ?LOG("received immediate call command '~s'", [AName]),
    case IsNodeUp andalso AName of
        false ->
            ?LOG("node ~s is not avaliable", [Node]),
            ?LOG("sending execution error for command ~s", [AName]),
            {Mega,Sec,Micro} = erlang:now(),
            Props = [ {<<"Event-Name">>, <<"CHANNEL_EXECUTE_ERROR">>}
                     ,{<<"Event-Date-Timestamp">>, ( (Mega * 1000000 + Sec) * 1000000 + Micro )}
                     ,{<<"Call-ID">>, CallId}
                     ,{<<"Channel-Call-State">>, <<"ERROR">>}
                     ,{<<"Custom-Channel-Vars">>, JObj}
                   ],
            wapi_call:publish_event(CallId, Props),
            CommandQ;
        <<"queue">> ->
            true = wapi_dialplan:queue_v(JObj),
            DefJObj = wh_json:from_list(wh_api:extract_defaults(JObj)),
            lists:foreach(fun(?EMPTY_JSON_OBJECT) -> ok;
                             (CmdJObj) ->
                                  put(callid, CallId),
                                  AppCmd = wh_json:merge_jobjs(DefJObj, CmdJObj),
                                  true = wapi_dialplan:v(AppCmd),
                                  execute_control_request(CmdJObj, State)
                          end, wh_json:get_value(<<"Commands">>, JObj)),
            CommandQ;
        _ ->
            execute_control_request(JObj, State),
            CommandQ
    end;
insert_command(_State, flush, JObj) ->
    ?LOG("received control queue flush command, clearing all waiting commands"),
    insert_command_into_queue(queue:new(), tail, JObj);
insert_command(#state{command_q=CommandQ}, head, JObj) ->
    insert_command_into_queue(CommandQ, head, JObj);
insert_command(#state{command_q=CommandQ}, tail, JObj) ->
    insert_command_into_queue(CommandQ, tail, JObj);
insert_command(Q, Pos, _) ->
    ?LOG("received command for an unknown queue position: ~p", [Pos]),
    Q.

-spec insert_command_into_queue/3 :: (queue(), tail | head, json_object()) -> queue().
insert_command_into_queue(Q, Position, JObj) ->
    InsertFun = queue_insert_fun(Position),
    case wh_json:get_value(<<"Application-Name">>, JObj) of
        <<"queue">> -> %% list of commands that need to be added
            true = wapi_dialplan:queue_v(JObj),
            DefJObj = wh_json:from_list(wh_api:extract_defaults(JObj)), %% each command lacks the default headers
            lists:foldr(fun(?EMPTY_JSON_OBJECT, TmpQ) -> TmpQ;
                           (CmdJObj, TmpQ) ->
                                AppCmd = wh_json:merge_jobjs(DefJObj, CmdJObj),
                                true = wapi_dialplan:v(AppCmd),
                                ?LOG("inserting at the ~s of the control queue call command '~s'"
                                     ,[Position, wh_json:get_value(<<"Application-Name">>, AppCmd)]),
                                InsertFun(AppCmd, TmpQ)
                        end, Q, wh_json:get_value(<<"Commands">>, JObj));
        AppName ->
            true = wapi_dialplan:v(JObj),
            ?LOG("inserting at the ~s of the control queue call command '~s'", [Position, AppName]),
            InsertFun(JObj, Q)
    end.

queue_insert_fun(tail) ->
    fun queue:in/2;
queue_insert_fun(head) ->
    fun queue:in_r/2.

-spec post_hangup_commands/1 :: (queue()) -> json_objects().
post_hangup_commands(CmdQ) ->
    [ JObj || JObj <- queue:to_list(CmdQ),
              begin
                  AppName = wh_json:get_value(<<"Application-Name">>, JObj),
                  case lists:member(AppName, ?POST_HANGUP_COMMANDS) of
                      true -> true;
                      false ->
                          ?LOG("removing command ~s from control queue, not valid after hangup", [AppName]),
                          false
                  end
              end
    ].

-spec execute_control_request/2 :: (json_object(), #state{}) -> 'ok'.
execute_control_request(Cmd, #state{node=Node, callid=CallId}) ->
    put(callid, CallId),

    try
        ?LOG("executing call command '~s'", [wh_json:get_value(<<"Application-Name">>, Cmd)]),
        Mod = wh_util:to_atom(<<"ecallmgr_"
                                     ,(wh_json:get_value(<<"Event-Category">>, Cmd, <<>>))/binary
                                     ,"_"
                                     ,(wh_json:get_value(<<"Event-Name">>, Cmd, <<>>))/binary
                                   >>),
        Mod:exec_cmd(Node, CallId, Cmd, self())
    catch
        _:{error,nosession} ->
            ?LOG("unable to execute command, no session"),
            send_error_resp(CallId, Cmd, <<"Could not execute dialplan action: ", (wh_json:get_value(<<"Application-Name">>, Cmd))/binary>>),
            self() ! {hangup, undefined, CallId},
            ok;
        error:{badmatch, {error, ErrMsg}} ->
            ?LOG("invalid command ~s: ~p", [wh_json:get_value(<<"Application-Name">>, Cmd), ErrMsg]),
            ?LOG("stacktrace: ~w", [erlang:get_stacktrace()]),
            send_error_resp(CallId, Cmd),
            self() ! {force_queue_advance, CallId},
            ok;
        _A:_B ->
            ?LOG("exception (~s) while executing ~s: ~w", [_A, wh_json:get_value(<<"Application-Name">>, Cmd), _B]),
            ?LOG("stacktrace: ~w", [erlang:get_stacktrace()]),
            send_error_resp(CallId, Cmd),
            self() ! {force_queue_advance, CallId},
            ok
    end.

-spec send_error_resp/2 :: (ne_binary(), json_object()) -> 'ok'.
send_error_resp(CallId, Cmd) ->
    send_error_resp(CallId, Cmd, <<"Could not execute dialplan action: ", (wh_json:get_value(<<"Application-Name">>, Cmd))/binary>>).

-spec send_error_resp/3 :: (ne_binary(), json_object(), ne_binary()) -> 'ok'.
send_error_resp(CallId, Cmd, Msg) ->
    Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Cmd, <<>>)}
            ,{<<"Error-Message">>, Msg}
            | wh_api:default_headers(<<>>, <<"error">>, <<"dialplan">>, ?APP_NAME, ?APP_VERSION)
           ],
    {ok, Payload} = wapi_dialplan:error(Resp),
    ?LOG("sending execution error: ~s", [Payload]),
    wapi_dialplan:publish_event(CallId, Payload).

-spec get_keep_alive_ref/1 :: ('undefined' | reference()) -> 'undefined' | reference().
get_keep_alive_ref(undefined) -> undefined;
get_keep_alive_ref(TRef) ->
    _ = case erlang:cancel_timer(TRef) of
            false -> ok;
            _ -> %% flush the receive buffer of expiration messages
                receive keep_alive_expired -> ok
                after 0 -> ok end
        end,
    erlang:send_after(?KEEP_ALIVE, self(), keep_alive_expired).

-spec publish_leg_addition/1 :: (json_object()) -> 'ok'.
publish_leg_addition(JObj) ->
    Props = case wh_json:get_value(<<"Event-Name">>, JObj) of
                <<"CHANNEL_BRIDGE">> ->
                    wh_json:to_proplist(JObj);
                <<"CHANNEL_CREATE">> ->
                    ecallmgr_call_events:swap_call_legs(JObj)
            end,
    Event = ecallmgr_call_events:create_event(<<"LEG_CREATED">>, undefined, Props),
    case props:get_value(<<"Call-ID">>, Event) of
        undefined ->
            ok;
        _Else ->
            ecallmgr_call_events:publish_event(Event)
    end.

-spec publish_leg_removal/1 :: (json_object()) -> 'ok'.
publish_leg_removal(JObj) ->
    Props = case wh_json:get_value(<<"Event-Name">>, JObj) of
                <<"CHANNEL_UNBRIDGE">> ->
                    wh_json:to_proplist(JObj);
                <<"CHANNEL_DESTROY">> ->
                    ecallmgr_call_events:swap_call_legs(JObj)
            end,
    Event = ecallmgr_call_events:create_event(<<"LEG_DESTROYED">>, undefined, Props),
    case props:get_value(<<"Call-ID">>, Event) of
        undefined ->
            ok;
        _Else ->
            ecallmgr_call_events:publish_event(Event)
    end.

-spec publish_callid_update/3 :: (ne_binary(), ne_binary(), ne_binary()) -> ok.
publish_callid_update(PrevCallId, NewCallId, CtrlQ) -> 
    ?LOG(PrevCallId, "sending callid update to ~s", [NewCallId]),
    Update = [{<<"Call-ID">>, NewCallId}
              ,{<<"Replaces-Call-ID">>, PrevCallId}
              ,{<<"Control-Queue">>, CtrlQ}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ],
    wapi_call:publish_callid_update(PrevCallId, Update).

-spec publish_control_transfer/2 :: (ne_binary(), ne_binary()) -> ok.
publish_control_transfer(undefined, CallId) ->
    ?LOG(CallId, "no whapp queue known for control transfer", []),    
    ok;
publish_control_transfer(ControllerQ, CallId) ->
    ?LOG(CallId, "sending control transfer to queue ~s", [ControllerQ]),
    Transfer = [{<<"Call-ID">>, CallId}
                | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
               ],
    wapi_call:publish_control_transfer(ControllerQ, Transfer).
    
