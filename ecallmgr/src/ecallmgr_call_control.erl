%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
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
%%% we can note the event happended, and continue looping as we were.
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

-import(logger, [format_log/3]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE). 

-record(state, {
	  node = undefined :: atom()
	 ,uuid = <<>> :: binary()
         ,command_q = queue:new() :: queue()
         ,current_app = <<>> :: binary()
         ,amqp_h = <<>> :: string()
         ,amqp_q = <<>> :: binary()
	 ,start_time = erlang:now() :: tuple()
	 ,is_node_up = true :: boolean()
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
start_link(Node, UUID, Amqp) ->
    gen_server:start_link(?MODULE, [Node, UUID, Amqp], []).

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
init([Node, UUID, {AmqpHost, CtlQueue}]) ->
    {ok, #state{node=Node, uuid=UUID, command_q = queue:new(), current_app = <<>>, amqp_h = AmqpHost, amqp_q = CtlQueue, start_time = erlang:now()}, 0}.

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
handle_info(timeout, #state{node=N, amqp_h=H, amqp_q=Q}=State) ->
    erlang:monitor_node(N, true),
    amqp_util:basic_consume(H, Q),
    {noreply, State};

handle_info({nodedown, Node}, #state{node=Node}=State) ->
    format_log(error, "CONTROL(~p): nodedown ~p~n", [self(), Node]),
    erlang:monitor_node(Node, false),
    timer:send_after(0, self(), {is_node_up, 100}),
    {noreply, State#state{is_node_up=false}};

handle_info({is_node_up, Timeout}, #state{node=Node}=State) ->
    format_log(error, "CONTROL(~p): nodedown ~p, trying ping, then waiting ~p if it fails~n", [self(), Node, Timeout]),
    case net_adm:ping(Node) of
	pong ->
	    erlang:monitor_node(Node, true),
	    format_log(info, "CONTROL(~p): node_is_up ~p~n", [self(), Node]),
	    {noreply, State#state{is_node_up=true}};
	pang ->
	    case Timeout >= ?MAX_TIMEOUT_FOR_NODE_RESTART of
		true ->
		    timer:send_after(?MAX_TIMEOUT_FOR_NODE_RESTART, self(), {is_node_up, ?MAX_TIMEOUT_FOR_NODE_RESTART});
		false ->
		    timer:send_after(Timeout, self(), {is_node_up, Timeout*2})
	    end,
	    {noreply, State}
    end;

handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};

handle_info({_, #amqp_msg{props=#'P_basic'{content_type = <<"application/json">>}, payload = Payload}}, State) ->
    JObj = mochijson2:decode(binary_to_list(Payload)),
    format_log(info, "CONTROL(~p): Recv App ~p~n", [self(), whapps_json:get_value(<<"Application-Name">>, JObj)]),

    NewCmdQ = case whapps_json:get_value(<<"Application-Name">>, JObj) of
		  <<"queue">> -> %% list of commands that need to be added
		      DefProp = whistle_api:extract_defaults(JObj), %% each command lacks the default headers
		      lists:foldl(fun({struct, []}, TmpQ) -> TmpQ;
				     ({struct, Cmd}, TmpQ) ->
					  AppCmd = DefProp ++ Cmd,
					  format_log(info, "CONTROL.queue: Cmd: ~p~n", [AppCmd]),
					  queue:in({struct, AppCmd}, TmpQ)
				  end, State#state.command_q, whapps_json:get_value(<<"Commands">>, JObj));
		  _AppName ->
		      queue:in(JObj, State#state.command_q)
	      end,
    case State#state.is_node_up andalso (not queue:is_empty(NewCmdQ)) andalso State#state.current_app =:= <<>> of
	true ->
	    {{value, Cmd}, NewCmdQ1} = queue:out(NewCmdQ),
	    ecallmgr_call_command:exec_cmd(State#state.node, State#state.uuid, Cmd, State#state.amqp_h),
	    AppName = whapps_json:get_value(<<"Application-Name">>, Cmd),
	    {noreply, State#state{command_q = NewCmdQ1, current_app = AppName}};
	false ->
	    {noreply, State#state{command_q = NewCmdQ}}
    end;

handle_info({execute_complete, UUID, EvtName}, State) when UUID =:= State#state.uuid ->
    case whistle_api:convert_whistle_app_name(State#state.current_app) of
	<<>> ->
	    {noreply, State};
	EvtName ->
	    case State#state.is_node_up andalso queue:out(State#state.command_q) of
		false ->
		    %% if the node is down, don't inject the next FS event
		    {noreply, State#state{current_app = <<>>}};
		{empty, _} ->
		    {noreply, State#state{current_app = <<>>}};
		{{value, Cmd}, CmdQ1} ->
		    ecallmgr_call_command:exec_cmd(State#state.node, State#state.uuid, Cmd, State#state.amqp_h),
		    {noreply, State#state{command_q = CmdQ1, current_app = whapps_json:get_value(<<"Application-Name">>, Cmd)}}
	    end;
	_OtherEvt ->
	    {noreply, State}
    end;

handle_info({hangup, EvtPid, UUID}, #state{uuid=UUID, amqp_h=H, amqp_q=Q, start_time=StartT}=State) ->
    amqp_util:unbind_q_from_callctl(H, Q),
    amqp_util:delete_queue(H, Q), %% stop receiving messages
    format_log(info, "CONTROL(~p): Received hangup, exiting (Time since process started: ~pms)~n"
	       ,[self(), timer:now_diff(erlang:now(), StartT) div 1000]),
    EvtPid ! {ctl_down, self()},
    {stop, normal, State};

handle_info(is_amqp_up, #state{amqp_h=H, amqp_q=Q}=State) ->
    case restart_amqp_queue(H, Q) of
	ok -> {noreply, State};
	{error, _} ->
	    timer:send_after(1000, self(), is_amqp_up),
	    {noreply, State}
    end;

handle_info({amqp_host_down, H}, State) ->
    format_log(info, "CONTROL(~p): AmqpHost ~s went down, so we are too~n", [self(), H]),
    timer:send_after(1000, self(), is_amqp_up),
    {noreply, State};

handle_info(_Msg, State) ->
    format_log(info, "CONTROL(~p): Unhandled message: ~p~n", [self(), _Msg]),
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

-spec(restart_amqp_queue/2 :: (Host :: string(), Queue :: binary()) -> 'ok' | tuple('error', term())).
restart_amqp_queue(Host, Queue) ->
    case amqp_util:new_callctl_queue(Host, Queue) of
	Q when Q =:= Queue ->
	    amqp_util:bind_q_to_callctl(Host, Queue),
	    amqp_util:basic_consume(Host, Queue),
	    ok;
	{error, _}=E ->
	    E
    end.
