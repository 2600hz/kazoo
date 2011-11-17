%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
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
%%% When receiving an {execute_complete, UUID, EvtName} tuple from
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
%%% Created : 26 Aug 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_call_control).

-behaviour(gen_listener).

%% API
-export([start_link/2, handle_req/2, amqp_queue/1]).

-export([event_execute_complete/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2
	 ,terminate/2, code_change/3]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).
-define(KEEP_ALIVE, 5000). %% after hangup, keep alive for 5 seconds

-type insert_at_options() :: 'now' | 'head' | 'tail' | 'flush'.

-record(state, {
	  node = 'undefined' :: atom()
	 ,uuid = <<>> :: binary()
         ,command_q = queue:new() :: queue()
         ,current_app = 'undefined' :: ne_binary() | 'undefined'
	 ,start_time = erlang:now() :: wh_now()
	 ,is_node_up = 'true' :: boolean()
         ,keep_alive_ref = 'undefined' :: 'undefined' | reference()
	 }).

-define(RESPONDERS, [{?MODULE, [{<<"call">>, <<"command">>}]}
                     ,{?MODULE, [{<<"conference">>, <<"command">>}]}]).
-define(BINDINGS, [{dialplan, []}]).

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
start_link(Node, UUID) ->
    ?LOG(UUID, "starting call control on ~s", [Node]),
    gen_listener:start_link(?MODULE, [{responders, ?RESPONDERS}
				      ,{bindings, ?BINDINGS}]
                            ,[Node, UUID]).

handle_req(JObj, Props) ->
    Srv = props:get_value(server, Props),
    gen_listener:cast(Srv, {dialplan, JObj}).

-spec amqp_queue/1 :: (pid()) -> ne_binary().
amqp_queue(Srv) ->
    gen_listener:queue_name(Srv).

-spec event_execute_complete/3 :: (pid(), ne_binary(), ne_binary()) -> 'ok'.
event_execute_complete(Srv, UUID, App) ->
    gen_listener:cast(Srv, {event_execute_complete, UUID, App}).

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
init([Node, UUID]) ->
    put(callid, UUID),
    erlang:monitor_node(Node, true),
    {ok, #state{node=Node, uuid=UUID, command_q = queue:new()
		,start_time = erlang:now()}
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

handle_cast({event_execute_complete, UUID, EvtName}
	    ,#state{uuid=UUID, command_q=CmdQ, current_app=CurrApp, is_node_up=INU}=State) ->
    case lists:member(EvtName, wh_api:convert_whistle_app_name(CurrApp)) of
        false ->
            {noreply, State};
        true ->
            ?LOG("execution of ~s command complete", [CurrApp]),
	    case INU andalso queue:out(CmdQ) of
		false ->
		    %% if the node is down, don't inject the next FS event
                    ?LOG("not continuing until switch is avaliable"),
		    {noreply, State#state{current_app=undefined}, hibernate};
		{empty, _} ->
                    ?LOG("reached end of queued call commands"),
		    {noreply, State#state{current_app=undefined}, hibernate};
		{{value, Cmd}, CmdQ1} ->
		    execute_control_request(Cmd, State),
                    AppName = wh_json:get_value(<<"Application-Name">>, Cmd),
		    {noreply, State#state{command_q = CmdQ1, current_app = AppName}, hibernate}
	    end
    end.

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
    ?LOG_SYS("lost connection to node ~s, waiting for reconnection", [Node]),
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

handle_info({force_queue_advance, UUID}, #state{uuid=UUID, command_q=CmdQ, is_node_up=INU, current_app=CurrApp}=State) ->
    ?LOG("received control queue unconditional advance, skipping wait for ~s command completion", [CurrApp]),
    case INU andalso queue:out(CmdQ) of
        false ->
            %% if the node is down, don't inject the next FS event
            ?LOG("not continuing until switch is avaliable"),
            {noreply, State#state{current_app = undefined}, hibernate};
        {empty, _} ->
            ?LOG("reached end of queued call commands"),
            {noreply, State#state{current_app = undefined}, hibernate};
        {{value, Cmd}, CmdQ1} ->
            execute_control_request(Cmd, State),
            AppName = wh_json:get_value(<<"Application-Name">>, Cmd),
            {noreply, State#state{command_q = CmdQ1, current_app = AppName}, hibernate}
    end;

handle_info({hangup, _EvtPid, UUID}, #state{uuid=UUID, command_q=CmdQ}=State) ->
    ?LOG("recieved hangup notification"),
    lists:foreach(fun(Cmd) -> execute_control_request(Cmd, State) end, post_hangup_commands(CmdQ)),
    TRef = erlang:send_after(?KEEP_ALIVE, self(), keep_alive_expired),
    {noreply, State#state{keep_alive_ref=TRef}, hibernate};

handle_info(keep_alive_expired, #state{start_time=StartTime}=State) ->
    ?LOG("KeepAlive expired, control queue was up for ~p microseconds", [timer:now_diff(erlang:now(), StartTime)]),
    {stop, normal, State};

handle_info(_Msg, State) ->
    {noreply, State}.

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
terminate(_Reason, _State) ->
    ?LOG("call control ~p termination", [_Reason]),
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
insert_command(#state{node=Node, uuid=UUID, command_q=CommandQ, is_node_up=IsNodeUp}=State, now, JObj) ->
    AName = wh_json:get_value(<<"Application-Name">>, JObj),
    ?LOG("received immediate call command ~s", [AName]),
    case IsNodeUp andalso AName of
	false ->
            ?LOG("node ~s is not avaliable", [Node]),
            ?LOG("sending execution error for command ~s", [AName]),
	    {Mega,Sec,Micro} = erlang:now(),
	    Prop = [ {<<"Event-Name">>, <<"CHANNEL_EXECUTE_ERROR">>}
		     ,{<<"Event-Date-Timestamp">>, ( (Mega * 1000000 + Sec) * 1000000 + Micro )}
		     ,{<<"Call-ID">>, UUID}
		     ,{<<"Channel-Call-State">>, <<"ERROR">>}
		     ,{<<"Custom-Channel-Vars">>, JObj}
		   ],
	    ecallmgr_call_events:publish_msg(Node, UUID, Prop),
	    CommandQ;
	<<"queue">> ->
	    true = wapi_dialplan:queue_v(JObj),
	    DefJObj = wh_json:from_list(wh_api:extract_defaults(JObj)),
	    lists:foreach(fun(?EMPTY_JSON_OBJECT) -> ok;
			     (CmdJObj) ->
				  put(callid, UUID),
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
    ?LOG("received command for an unknown queue position ~p", [Pos]),
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
                                ?LOG("inserting call command ~s at the ~s of the control queue"
                                     ,[wh_json:get_value(<<"Application-Name">>, AppCmd), Position]),
				InsertFun(AppCmd, TmpQ)
			end, Q, wh_json:get_value(<<"Commands">>, JObj));
	AppName ->
	    true = wapi_dialplan:v(JObj),
            ?LOG("inserting call command ~s at the ~s of the control queue", [AppName, Position]),
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
execute_control_request(Cmd, #state{node=Node, uuid=UUID}) ->
    put(callid, UUID),

    try
        ?LOG("executing command ~s", [wh_json:get_value(<<"Application-Name">>, Cmd)]),
        Mod = wh_util:to_atom(<<"ecallmgr_"
                                     ,(wh_json:get_value(<<"Event-Category">>, Cmd, <<>>))/binary
                                     ,"_"
                                     ,(wh_json:get_value(<<"Event-Name">>, Cmd, <<>>))/binary
                                   >>),
        Mod:exec_cmd(Node, UUID, Cmd, self())
    catch
	_:{error,nosession} ->
	    ?LOG("unable to execute command, no session"),
	    send_error_resp(UUID, Cmd, <<"Could not execute dialplan action: ", (wh_json:get_value(<<"Application-Name">>, Cmd))/binary>>),
            self() ! {hangup, undefined, UUID},
	    ok;
	error:{badmatch, {error, ErrMsg}} ->
	    ?LOG("invalid command ~s: ~p", [wh_json:get_value(<<"Application-Name">>, Cmd), ErrMsg]),
	    ?LOG("stacktrace: ~w", [erlang:get_stacktrace()]),
	    send_error_resp(UUID, Cmd),
	    self() ! {force_queue_advance, UUID},
	    ok;
        _A:_B ->
	    ?LOG("exception (~s) while executing ~s: ~w", [_A, wh_json:get_value(<<"Application-Name">>, Cmd), _B]),
	    ?LOG("stacktrace: ~w", [erlang:get_stacktrace()]),
	    send_error_resp(UUID, Cmd),
            self() ! {force_queue_advance, UUID},
            ok
    end.

-spec send_error_resp/2 :: (ne_binary(), json_object()) -> 'ok'.
send_error_resp(UUID, Cmd) ->
    send_error_resp(UUID, Cmd, <<"Could not execute dialplan action: ", (wh_json:get_value(<<"Application-Name">>, Cmd))/binary>>).

-spec send_error_resp/3 :: (ne_binary(), json_object(), ne_binary()) -> 'ok'.
send_error_resp(UUID, Cmd, Msg) ->
    Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Cmd, <<>>)}
	    ,{<<"Error-Message">>, Msg}
	    | wh_api:default_headers(<<>>, <<"error">>, <<"dialplan">>, ?APP_NAME, ?APP_VERSION)
	   ],
    {ok, Payload} = wapi_dialplan:error(Resp),
    ?LOG("sending execution error: ~s", [Payload]),
    wapi_dialplan:publish_event(UUID, Payload).

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
