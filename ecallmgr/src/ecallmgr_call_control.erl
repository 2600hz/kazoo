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
%%% queue. If there are no commands in the queue, set CurrApp to <<>> and
%%% loop; otherwise take the next command, execute it, and look with it as
%%% the CurrApp. If EvtName and the converted Whistle name don't match,
%%% something else executed that might have been related to the main
%%% application's execute (think set commands, like playback terminators);
%%% we can note the event happened, and continue looping as we were.
%%%
%%%
%%%
%%% @end
%%% Created : 26 Aug 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_call_control).

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).
-define(KEEP_ALIVE, 5000). %% after hangup, keep alive for 5 seconds

-record(state, {
	  node = undefined :: atom()
	 ,uuid = <<>> :: binary()
         ,command_q = queue:new() :: queue()
         ,current_app = <<>> :: binary()
         ,amqp_q = <<>> :: binary()
	 ,start_time = erlang:now() :: tuple()
	 ,is_node_up = true :: boolean()
         ,keep_alive_ref = undefined :: undefined | timer:tref()
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
start_link(Node, UUID, CtlQ) ->
    gen_server:start_link(?MODULE, [Node, UUID, CtlQ], []).

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
init([Node, UUID, CtlQueue]) ->
    put(callid, UUID),
    ?LOG_START("starting new call control listener"),
    {ok, #state{node=Node, uuid=UUID, command_q = queue:new()
		,current_app = <<>>, amqp_q = CtlQueue, start_time = erlang:now()}, 0}.

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
handle_info(timeout, #state{node=N, amqp_q=Q}=State) ->
    erlang:monitor_node(N, true),
    amqp_util:basic_consume(Q),
    {noreply, State};

handle_info({nodedown, Node}, #state{node=Node, is_node_up=true}=State) ->
    ?LOG_SYS("lost connection to node ~s, waiting for reconnection", [Node]),
    erlang:monitor_node(Node, false),
    {ok, _} = timer:send_after(0, self(), {is_node_up, 100}),
    {noreply, State#state{is_node_up=false}};

handle_info({is_node_up, Timeout}, #state{node=Node, is_node_up=false}=State) ->
    case ecallmgr_fs_handler:is_node_up(Node) of
	true ->
	    erlang:monitor_node(Node, true),
            ?LOG("reconnected to node ~s", [Node]),
	    {noreply, State#state{is_node_up=true}};
	false ->
	    {ok, _} = case Timeout >= ?MAX_TIMEOUT_FOR_NODE_RESTART of
			  true ->
			      ?LOG("node ~p down, waiting ~p to check again", [Node, ?MAX_TIMEOUT_FOR_NODE_RESTART]),
			      timer:send_after(?MAX_TIMEOUT_FOR_NODE_RESTART, self(), {is_node_up, ?MAX_TIMEOUT_FOR_NODE_RESTART});
			  false ->
			      ?LOG("node ~p down, waiting ~p to check again", [Node, Timeout]),
			      timer:send_after(Timeout, self(), {is_node_up, Timeout*2})
		      end,
	    {noreply, State}
    end;

handle_info({_, #amqp_msg{props=#'P_basic'{content_type = <<"application/json">>}, payload = Payload}}
	    ,#state{keep_alive_ref=Ref, command_q=CmdQ, current_app=CurrApp, is_node_up=INU}=State) ->
    JObj = mochijson2:decode(binary_to_list(Payload)),

    NewCmdQ = try insert_command(State, wh_json:get_value(<<"Insert-At">>, JObj, <<"tail">>), JObj)
	      catch _:_ -> CmdQ end,

    case INU andalso (not queue:is_empty(NewCmdQ)) andalso CurrApp =:= <<>> of
	true ->
	    {{value, Cmd}, NewCmdQ1} = queue:out(NewCmdQ),
	    execute_control_request(Cmd, State),
	    AppName = wh_json:get_value(<<"Application-Name">>, Cmd),
            ?LOG("advance to ~s", [AppName]),
	    {noreply, State#state{command_q = NewCmdQ1, current_app = AppName, keep_alive_ref=get_keep_alive_ref(Ref)}};
	false ->
	    {noreply, State#state{command_q = NewCmdQ, keep_alive_ref=get_keep_alive_ref(Ref)}}
    end;

handle_info({execute_complete, UUID, EvtName}, #state{uuid=UUID, command_q=CmdQ, current_app=CurrApp, is_node_up=INU}=State) ->
    case lists:member(EvtName, wh_api:convert_whistle_app_name(CurrApp)) of
        false ->
            {noreply, State};
        true ->
	    ?LOG("execution of ~s complete, treating as application ~s", [EvtName, CurrApp]),
	    case INU andalso queue:out(CmdQ) of
		false ->
		    %% if the node is down, don't inject the next FS event
		    {noreply, State#state{current_app = <<>>}};
		{empty, _} ->
		    {noreply, State#state{current_app = <<>>}};
		{{value, Cmd}, CmdQ1} ->
		    execute_control_request(Cmd, State),
                    AppName = wh_json:get_value(<<"Application-Name">>, Cmd),
                    ?LOG("advance to ~s", [AppName]),
		    {noreply, State#state{command_q = CmdQ1, current_app = AppName}}
	    end
    end;

handle_info({force_queue_advance, UUID}, #state{uuid=UUID, command_q=CmdQ, is_node_up=INU, current_app=CurrApp}=State) ->
    ?LOG("forcing queue to advance past ~s", [CurrApp]),
    case INU andalso queue:out(CmdQ) of
        false ->
            %% if the node is down, don't inject the next FS event
            {noreply, State#state{current_app = <<>>}};
        {empty, _} ->
            {noreply, State#state{current_app = <<>>}};
        {{value, Cmd}, CmdQ1} ->
            execute_control_request(Cmd, State),
            AppName = wh_json:get_value(<<"Application-Name">>, Cmd),
            ?LOG("advance to ~s", [AppName]),
            {noreply, State#state{command_q = CmdQ1, current_app = AppName}}
    end;

handle_info({hangup, _EvtPid, UUID}, #state{uuid=UUID, command_q=CmdQ}=State) ->
    ?LOG("recieved hangup notification"),
    lists:foreach(fun(Cmd) -> execute_control_request(Cmd, State) end, post_hangup_commands(CmdQ)),
    {ok, TRef} = timer:send_after(?KEEP_ALIVE, keep_alive_expired),
    {noreply, State#state{keep_alive_ref=TRef}};

handle_info(keep_alive_expired, #state{start_time=StartTime}=State) ->
    ?LOG("KeepAlive expired, control queue was up for ~p microseconds", [timer:now_diff(erlang:now(), StartTime)]),
    {stop, normal, State};

handle_info(is_amqp_up, #state{amqp_q=Q}=State) ->
    case restart_amqp_queue(Q) of
	ok -> {noreply, State};
	{error, _} ->
	    {ok, _} = timer:send_after(1000, self(), is_amqp_up),
	    {noreply, State}
    end;

handle_info({amqp_host_down, _}, State) ->
    ?LOG_SYS("lost AMQP connection, attempting to reconnect"),
    {ok, _} = timer:send_after(1000, self(), is_amqp_up),
    {noreply, State};

handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

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

-spec restart_amqp_queue/1 :: (Queue) -> 'ok' | {'error', 'amqp_error'} when
      Queue :: binary().
restart_amqp_queue(Queue) ->
    case amqp_util:new_callctl_queue(Queue) of
	Q when Q =:= Queue ->
	    _ = amqp_util:bind_q_to_callctl(Queue),
	    _ = amqp_util:basic_consume(Queue),
	    ok;
	{error, amqp_error}=E ->
	    E
    end.

%% execute all commands in JObj immediately, irregardless of what is running (if anything).
-spec insert_command/3 :: (State, InsertAt, JObj) -> queue() when
      State :: #state{},
      InsertAt :: binary(),
      JObj :: json_object().
insert_command(State, <<"now">>, JObj) ->
    AName = wh_json:get_value(<<"Application-Name">>, JObj),
    case State#state.is_node_up andalso AName of
	false ->
            ?LOG("node ~s is not avaliable"),
            ?LOG("sending execution error for command ~s", [State#state.node, AName]),
	    {Mega,Sec,Micro} = erlang:now(),
	    Prop = [ {<<"Event-Name">>, <<"CHANNEL_EXECUTE_ERROR">>}
		     ,{<<"Event-Date-Timestamp">>, ( (Mega * 1000000 + Sec) * 1000000 + Micro )}
		     ,{<<"Call-ID">>, State#state.uuid}
		     ,{<<"Channel-Call-State">>, <<"ERROR">>}
		     ,{<<"Custom-Channel-Vars">>, JObj}
		   ],
	    ecallmgr_call_events:publish_msg(State#state.node, State#state.uuid, Prop),
	    State#state.command_q;
	<<"queue">> ->
	    true = wh_api:queue_req_v(JObj),
	    DefProp = wh_api:extract_defaults(JObj), %% each command lacks the default headers
	    lists:foreach(fun(?EMPTY_JSON_OBJECT) -> ok;
			     ({struct, Cmd}) ->
				  AppCmd = {struct, DefProp ++ Cmd},
				  true = wh_api:dialplan_req_v(AppCmd),
                                  execute_control_request(Cmd, State)
			  end, wh_json:get_value(<<"Commands">>, JObj)),
	    State#state.command_q;
	AppName ->
	    ?LOG("executing command ~s immediately, bypassing queue", [AppName]),
            execute_control_request(JObj, State),
	    State#state.command_q
    end;
insert_command(_State, <<"flush">>, JObj) ->
    ?LOG("flushing queue"),
    insert_command_into_queue(queue:new(), fun queue:in/2, JObj);
insert_command(State, <<"head">>, JObj) ->
    case wh_json:get_value(<<"Application-Name">>, JObj) of
	<<"queue">> ->
	    Commands = wh_json:get_value(<<"Commands">>, JObj),
	    JObj2 = wh_json:set_value(<<"Commands">>, lists:reverse(Commands), JObj),
            ?LOG("inserting queued commands at head of queue"),
	    insert_command_into_queue(State#state.command_q, fun queue:in_r/2, JObj2);
	AppName ->
            ?LOG("inserting command ~s at head of queue", [AppName]),
	    insert_command_into_queue(State#state.command_q, fun queue:in_r/2, JObj)
    end;
insert_command(State, <<"tail">>, JObj) ->
    case wh_json:get_value(<<"Application-Name">>, JObj) of
	<<"queue">> ->
	    Commands = wh_json:get_value(<<"Commands">>, JObj),
	    JObj2 = wh_json:set_value(<<"Commands">>, lists:reverse(Commands), JObj),
            ?LOG("inserting queued commands at tail of queue"),
	    insert_command_into_queue(State#state.command_q, fun queue:in/2, JObj2);
        AppName ->
            ?LOG("inserting command ~s at tail of queue", [AppName]),
	    insert_command_into_queue(State#state.command_q, fun queue:in/2, JObj)
    end.

-spec insert_command_into_queue/3 :: (Q, InsertFun, JObj) -> queue() when
      Q :: queue(),
      InsertFun :: fun(),
      JObj :: json_object().
insert_command_into_queue(Q, InsertFun, JObj) ->
    case wh_json:get_value(<<"Application-Name">>, JObj) of
	<<"queue">> -> %% list of commands that need to be added
	    true = wh_api:queue_req_v(JObj),
	    DefProp = wh_api:extract_defaults(JObj), %% each command lacks the default headers
	    lists:foldl(fun(?EMPTY_JSON_OBJECT, TmpQ) -> TmpQ;
			   ({struct, Cmd}, TmpQ) ->
				AppCmd = {struct, DefProp ++ Cmd},
				true = wh_api:dialplan_req_v(AppCmd),
				?LOG("inserting queued command ~s into queue", [wh_json:get_value(<<"Application-Name">>, AppCmd)]),
				InsertFun(AppCmd, TmpQ)
			end, Q, wh_json:get_value(<<"Commands">>, JObj));
	_AppName ->
	    true = wh_api:dialplan_req_v(JObj),
	    InsertFun(JObj, Q)
    end.

-spec post_hangup_commands/1 :: (CmdQ) -> json_objects() when
      CmdQ :: queue().
post_hangup_commands(CmdQ) ->
    ?LOG("removing non post hangup commands from command queue"),
    [ JObj || JObj <- queue:to_list(CmdQ),
	      lists:member(wh_json:get_value(<<"Application-Name">>, JObj), ?POST_HANGUP_COMMANDS)
    ].

-spec execute_control_request/2 :: (Cmd, State) -> 'ok' when
      Cmd :: json_object(),
      State :: #state{}.
execute_control_request(Cmd, #state{node=Node, uuid=UUID}) ->
    try
        ?LOG("executing application ~s", [wh_json:get_value(<<"Application-Name">>, Cmd)]),
        Mod = wh_util:to_atom(<<"ecallmgr_"
                                     ,(wh_json:get_value(<<"Event-Category">>, Cmd, <<>>))/binary
                                     ,"_"
                                     ,(wh_json:get_value(<<"Event-Name">>, Cmd, <<>>))/binary
                                   >>),
        Mod:exec_cmd(Node, UUID, Cmd, self())
    catch
	_:{error,nosession} ->
	    ?LOG("Session in FS down"),
	    Resp = [
		    {<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Cmd, <<>>)}
		    ,{<<"Error-Message">>, <<"Channel was hungup before completing command ", (wh_json:get_value(<<"Application-Name">>, Cmd))/binary>>}
		    | wh_api:default_headers(<<>>, <<"error">>, <<"dialplan">>, ?APP_NAME, ?APP_VERSION)
		   ],
            {ok, Payload} = wh_api:error_resp(Resp),
            amqp_util:callevt_publish(UUID, Payload, event),
            self() ! {hangup, undefined, UUID},
	    ok;
	error:{badmatch, {error, ErrMsg}} ->
	    ?LOG("Matching error: {'error': ~s} when executing ~s", [ErrMsg, wh_json:get_value(<<"Application-Name">>, Cmd)]),
	    ?LOG("Stacktrace: ~w", [erlang:get_stacktrace()]),
	    send_error_resp(UUID, Cmd),
	    self() ! {force_queue_advance, UUID},
	    ok;
        _A:_B ->
	    ?LOG("Exception ~s:~w when executing ~s", [_A, _B, wh_json:get_value(<<"Application-Name">>, Cmd)]),
	    ?LOG("Stacktrace: ~w", [erlang:get_stacktrace()]),
	    send_error_resp(UUID, Cmd),
            self() ! {force_queue_advance, UUID},
            ok
    end.

-spec send_error_resp/2 :: (UUID, Cmd) -> 'ok' when
      UUID :: binary(),
      Cmd :: json_object().
send_error_resp(UUID, Cmd) ->
    Resp = [
	    {<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Cmd, <<>>)}
	    ,{<<"Error-Message">>, <<"Could not execute dialplan action: ", (wh_json:get_value(<<"Application-Name">>, Cmd))/binary>>}
	    | wh_api:default_headers(<<>>, <<"error">>, <<"dialplan">>, ?APP_NAME, ?APP_VERSION)
	   ],
    {ok, Payload} = wh_api:error_resp(Resp),
    amqp_util:callevt_publish(UUID, Payload, event).

-spec get_keep_alive_ref/1 :: (TRef) -> 'undefined' | timer:tref() when
      TRef :: 'undefined' | timer:tref().
get_keep_alive_ref(undefined) -> undefined;
get_keep_alive_ref(TRef) ->
    _ = case timer:cancel(TRef) of
	    {ok, cancel} -> %% flush the receive buffer of expiration messages
		receive keep_alive_expired -> ok
		after 0 -> ok end;
	    _ -> ok
	end,
    {ok, NewTRef} = timer:send_after(?KEEP_ALIVE, keep_alive_expired),
    NewTRef.
