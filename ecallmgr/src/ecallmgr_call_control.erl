%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
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
%%% Created : 26 Aug 2010 by James Aimonetti <james@2600hz.com>
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
    logger:format_log(error, "CONTROL(~p): nodedown ~p~n", [self(), Node]),
    erlang:monitor_node(Node, false),
    {ok, _} = timer:send_after(0, self(), {is_node_up, 100}),
    {noreply, State#state{is_node_up=false}};

handle_info({is_node_up, Timeout}, #state{node=Node, is_node_up=false}=State) ->
    logger:format_log(error, "CONTROL(~p): nodedown ~p, trying ping, then waiting ~p if it fails~n", [self(), Node, Timeout]),
    case ecallmgr_fs_handler:is_node_up(Node) of
	true ->
	    erlang:monitor_node(Node, true),
	    logger:format_log(info, "CONTROL(~p): node_is_up ~p~n", [self(), Node]),
	    {noreply, State#state{is_node_up=true}};
	false ->
	    {ok, _} = case Timeout >= ?MAX_TIMEOUT_FOR_NODE_RESTART of
			  true ->
			      timer:send_after(?MAX_TIMEOUT_FOR_NODE_RESTART, self(), {is_node_up, ?MAX_TIMEOUT_FOR_NODE_RESTART});
			  false ->
			      timer:send_after(Timeout, self(), {is_node_up, Timeout*2})
		      end,
	    {noreply, State}
    end;

handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};

handle_info({_, #amqp_msg{props=#'P_basic'{content_type = <<"application/json">>}, payload = Payload}}
	    ,#state{keep_alive_ref=Ref, command_q=CmdQ, current_app=CurrApp, is_node_up=INU}=State) ->
    JObj = mochijson2:decode(binary_to_list(Payload)),
    logger:format_log(info, "CONTROL(~p): Recv App ~s, insert-at: ~s~n"
		      ,[self(), wh_json:get_value(<<"Application-Name">>, JObj), wh_json:get_value(<<"Insert-At">>, JObj, <<"tail">>)]),

    NewCmdQ = try insert_command(State, wh_json:get_value(<<"Insert-At">>, JObj, <<"tail">>), JObj)
	      catch _:_ -> CmdQ end,

    case INU andalso (not queue:is_empty(NewCmdQ)) andalso CurrApp =:= <<>> of
	true ->
	    {{value, Cmd}, NewCmdQ1} = queue:out(NewCmdQ),
	    execute_control_request(Cmd, State),
	    AppName = wh_json:get_value(<<"Application-Name">>, Cmd),
	    {noreply, State#state{command_q = NewCmdQ1, current_app = AppName, keep_alive_ref=get_keep_alive_ref(Ref)}};
	false ->
	    {noreply, State#state{command_q = NewCmdQ, keep_alive_ref=get_keep_alive_ref(Ref)}}
    end;

handle_info({execute_complete, UUID, EvtName}, #state{uuid=UUID, command_q=CmdQ, current_app=CurrApp, is_node_up=INU}=State) ->
    case lists:member(EvtName, whistle_api:convert_whistle_app_name(CurrApp)) of 
        false -> 
            {noreply, State}; 
        true ->
            logger:format_log(info, "CONTROL(~p): Command ~p execution complete, advance call-id ~p running ~p", [self(), EvtName, UUID, CurrApp]),
	    case INU andalso queue:out(CmdQ) of
		false ->
		    %% if the node is down, don't inject the next FS event
		    {noreply, State#state{current_app = <<>>}};
		{empty, _} ->
		    {noreply, State#state{current_app = <<>>}};
		{{value, Cmd}, CmdQ1} ->
		    execute_control_request(Cmd, State),
		    {noreply, State#state{command_q = CmdQ1, current_app = wh_json:get_value(<<"Application-Name">>, Cmd)}}
	    end       
    end;

handle_info({force_queue_advance, UUID}, #state{uuid=UUID, command_q=CmdQ, is_node_up=INU, current_app=CurrApp}=State) ->
    logger:format_log(info, "CONTROL(~p): Forced queue advance for call-id ~p running ~p", [self(), UUID, CurrApp]),
    case INU andalso queue:out(CmdQ) of
        false ->
            %% if the node is down, don't inject the next FS event
            {noreply, State#state{current_app = <<>>}};
        {empty, _} ->
            {noreply, State#state{current_app = <<>>}};
        {{value, Cmd}, CmdQ1} ->
            execute_control_request(Cmd, State),
            {noreply, State#state{command_q = CmdQ1, current_app = wh_json:get_value(<<"Application-Name">>, Cmd)}}
    end;

handle_info({hangup, _EvtPid, UUID}, #state{uuid=UUID, command_q=CmdQ}=State) ->
    lists:foreach(fun(Cmd) ->
                          execute_control_request(Cmd, State)
		  end, post_hangup_commands(CmdQ)),

    {ok, TRef} = timer:send_after(?KEEP_ALIVE, keep_alive_expired),
    {noreply, State#state{keep_alive_ref=TRef}};

handle_info(keep_alive_expired, #state{start_time=StartTime}=State) ->
    logger:format_log(info, "CONTROL(~p): KeepAlive expired. Proc up for ~p micro~n", [self(), timer:now_diff(erlang:now(), StartTime)]),
    {stop, normal, State};

handle_info(is_amqp_up, #state{amqp_q=Q}=State) ->
    case restart_amqp_queue(Q) of
	ok -> {noreply, State};
	{error, _} ->
	    {ok, _} = timer:send_after(1000, self(), is_amqp_up),
	    {noreply, State}
    end;

handle_info({amqp_host_down, H}, State) ->
    logger:format_log(info, "CONTROL(~p): AmqpHost ~s went down, so we are too~n", [self(), H]),
    {ok, _} = timer:send_after(1000, self(), is_amqp_up),
    {noreply, State};

handle_info(_Msg, State) ->
    logger:format_log(info, "CONTROL(~p): Unhandled message: ~p~n", [self(), _Msg]),
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

-spec(restart_amqp_queue/1 :: (Queue :: binary()) -> 'ok' | tuple('error', term())).
restart_amqp_queue(Queue) ->
    case amqp_util:new_callctl_queue(Queue) of
	Q when Q =:= Queue ->
	    amqp_util:bind_q_to_callctl(Queue),
	    amqp_util:basic_consume(Queue),
	    ok;
	{error, _}=E ->
	    E
    end.

%% execute all commands in JObj immediately, irregardless of what is running (if anything).
-spec(insert_command/3 :: (State :: #state{}, InsertAt :: binary(), JObj :: json_object()) -> queue()).
insert_command(State, <<"now">>, JObj) ->
    case State#state.is_node_up andalso wh_json:get_value(<<"Application-Name">>, JObj) of
	false ->
	    {Mega,Sec,Micro} = erlang:now(),
	    Prop = [ {<<"Event-Name">>, <<"CHANNEL_EXECUTE_ERROR">>}
		     ,{<<"Event-Date-Timestamp">>, ( (Mega * 1000000 + Sec) * 1000000 + Micro )}
		     ,{<<"Call-ID">>, State#state.uuid}
		     ,{<<"Channel-Call-State">>, <<"ERROR">>}
		     ,{<<"Custom-Channel-Vars">>, JObj}
		   ],
	    ecallmgr_call_events:publish_msg(State#state.uuid, Prop),
	    State#state.command_q;
	<<"queue">> ->
	    true = whistle_api:queue_req_v(JObj),
	    DefProp = whistle_api:extract_defaults(JObj), %% each command lacks the default headers
	    lists:foreach(fun(?EMPTY_JSON_OBJECT) -> ok;
			     ({struct, Cmd}) ->
				  AppCmd = {struct, DefProp ++ Cmd},
				  true = whistle_api:dialplan_req_v(AppCmd),
				  AppName = wh_json:get_value(<<"Application-Name">>, AppCmd),
				  logger:format_log(info, "CONTROL.queue: Exec now Cmd: ~p~n", [AppName]),
                                  execute_control_request(Cmd, State)
			  end, wh_json:get_value(<<"Commands">>, JObj)),
	    State#state.command_q;
	AppName ->
	    logger:format_log(info, "CONTROL: Exec now Cmd: ~p~n", [AppName]),
            execute_control_request(JObj, State),
	    State#state.command_q
    end;
insert_command(_State, <<"flush">>, JObj) ->
    insert_command_into_queue(queue:new(), fun queue:in/2, JObj);
insert_command(State, <<"head">>, JObj) ->
    case wh_json:get_value(<<"Application-Name">>, JObj) of
	<<"queue">> ->
	    Commands = wh_json:get_value(<<"Commands">>, JObj),
	    JObj2 = wh_json:set_value(<<"Commands">>, lists:reverse(Commands), JObj),
	    insert_command_into_queue(State#state.command_q, fun queue:in_r/2, JObj2);
	_ ->
	    insert_command_into_queue(State#state.command_q, fun queue:in_r/2, JObj)
    end;
insert_command(State, <<"tail">>, JObj) ->
    case wh_json:get_value(<<"Application-Name">>, JObj) of
	<<"queue">> ->
	    Commands = wh_json:get_value(<<"Commands">>, JObj),
	    JObj2 = wh_json:set_value(<<"Commands">>, lists:reverse(Commands), JObj),
	    insert_command_into_queue(State#state.command_q, fun queue:in/2, JObj2);
	_ ->
	    insert_command_into_queue(State#state.command_q, fun queue:in/2, JObj)
    end.

-spec(insert_command_into_queue/3 :: (Q :: queue(), InsertFun :: fun(), JObj :: json_object()) -> queue()).
insert_command_into_queue(Q, InsertFun, JObj) ->
    case wh_json:get_value(<<"Application-Name">>, JObj) of
	<<"queue">> -> %% list of commands that need to be added
	    true = whistle_api:queue_req_v(JObj),
	    DefProp = whistle_api:extract_defaults(JObj), %% each command lacks the default headers
	    lists:foldl(fun(?EMPTY_JSON_OBJECT, TmpQ) -> TmpQ;
			   ({struct, Cmd}, TmpQ) ->
				AppCmd = {struct, DefProp ++ Cmd},
				true = whistle_api:dialplan_req_v(AppCmd),
				logger:format_log(info, "CONTROL.queue: insert Cmd: ~p~n", [wh_json:get_value(<<"Application-Name">>, AppCmd)]),
				InsertFun(AppCmd, TmpQ)
			end, Q, wh_json:get_value(<<"Commands">>, JObj));
	_AppName ->
	    true = whistle_api:dialplan_req_v(JObj),
	    InsertFun(JObj, Q)
    end.

-spec(post_hangup_commands/1 :: (CmdQ :: queue()) -> json_objects()).
post_hangup_commands(CmdQ) ->
    [ JObj || JObj <- queue:to_list(CmdQ),
	      lists:member(wh_json:get_value(<<"Application-Name">>, JObj), ?POST_HANGUP_COMMANDS)
    ].

execute_control_request(Cmd, #state{node=Node, uuid=UUID}) ->
    try
        Mod = whistle_util:to_atom(<<"ecallmgr_"
                                     ,(wh_json:get_value(<<"Event-Category">>, Cmd, <<>>))/binary
                                     ,"_"
                                     ,(wh_json:get_value(<<"Event-Name">>, Cmd, <<>>))/binary
                                   >>),
        Mod:exec_cmd(Node, UUID, Cmd, self())
    catch
	_:{error,nosession} ->
	    logger:format_log(error, "CONTROL.exe (~p): Error session down for ~p~n~p~n", [self(), UUID, erlang:get_stacktrace()]),
	    Resp = [
		    {<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Cmd, <<>>)}
		    ,{<<"Error-Message">>, <<"Channel was hungup before completing command ", (wh_json:get_value(<<"Application-Name">>, Cmd))/binary>>}
		    | whistle_api:default_headers(<<>>, <<"error">>, <<"dialplan">>, ?APP_NAME, ?APP_VERSION)
		   ],
            {ok, Payload} = whistle_api:error_resp(Resp),
            amqp_util:callevt_publish(UUID, Payload, event),
            self() ! {hangup, undefined, UUID},
	    ok;
        _:_=E ->
            logger:format_log(error, "CONTROL.exe (~p): Error ~p executing request for call ~p~n~p~n", [self(), E, UUID, erlang:get_stacktrace()]),
            Resp = [
		    {<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Cmd, <<>>)}
		    ,{<<"Error-Message">>, <<"Could not execute dialplan action: ", (wh_json:get_value(<<"Application-Name">>, Cmd))/binary>>}
		    | whistle_api:default_headers(<<>>, <<"error">>, <<"dialplan">>, ?APP_NAME, ?APP_VERSION)
		   ],
            {ok, Payload} = whistle_api:error_resp(Resp),
            amqp_util:callevt_publish(UUID, Payload, event),
            self() ! {force_queue_advance, UUID},
            ok
    end.

-spec(get_keep_alive_ref/1 :: (TRef :: undefined | timer:tref()) -> undefined | timer:tref()).
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
