%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Receive route(dialplan) requests from FS, request routes and respond
%%% @end
%%% Created : 23 Mar 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_route).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2, handle_route_req/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(FS_TIMEOUT, 5000).

-include("ecallmgr.hrl").

%% lookups [ {LPid, FS_ReqID, erlang:now()} ]
-record(state, {
	  node = undefined :: atom()
	  ,stats = #handler_stats{} :: tuple()
	  ,lookups = [] :: list(tuple(pid(), binary(), tuple(integer(), integer(), integer())))
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
start_link(Node) ->
    gen_server:start_link(?MODULE, [Node], []).

start_link(Node, _Options) ->
    gen_server:start_link(?MODULE, [Node], []).

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
init([Node]) ->
    process_flag(trap_exit, true),
    Stats = #handler_stats{started = erlang:now()},
    {ok, #state{node=Node, stats=Stats}, 0}.

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
handle_info(timeout, #state{node=Node}=State) ->
    Type = {bind, dialplan},
    erlang:monitor_node(Node, true),
    {foo, Node} ! Type,
    receive
	ok ->
	    ?LOG("Bound ~p to ~p~n", [Type, Node]),
	    {noreply, State};
	{error, Reason} ->
	    ?LOG("Failed to bind: ~p~n", [Reason]),
	    {stop, Reason, State}
    after ?FS_TIMEOUT ->
	    ?LOG("Failed to bind: TIMEOUT"),
	    {stop, timeout, State}
    end;

handle_info({fetch, _Section, _Something, _Key, _Value, ID, [undefined | _Data]}, #state{node=Node}=State) ->
    ?LOG("Fetch unknown section: ~p So: ~p, K: ~p V: ~p ID: ~s", [_Section, _Something, _Key, _Value, ID]),
    _ = freeswitch:fetch_reply(Node, ID, ?EMPTYRESPONSE),
    {noreply, State};

handle_info({fetch, dialplan, _Tag, _Key, _Value, FSID, [CallID | FSData]}, #state{node=Node, stats=Stats, lookups=LUs}=State) ->
    case {props:get_value(<<"Event-Name">>, FSData), props:get_value(<<"Caller-Context">>, FSData)} of
	{<<"REQUEST_PARAMS">>, <<"context_2">>} ->
	    {ok, LookupPid} = ecallmgr_fs_route_sup:start_req(Node, FSID, CallID, FSData),
	    erlang:monitor(process, LookupPid),

	    LookupsReq = Stats#handler_stats.lookups_requested + 1,
	    ?LOG_START(CallID, "Fetch request: FSID: ~p Lookup: ~p Req#: ~p", [FSID, LookupPid, LookupsReq]),
	    {noreply, State#state{lookups=[{LookupPid, FSID, erlang:now()} | LUs]
				  ,stats=Stats#handler_stats{lookups_requested=LookupsReq}}};
	{_Other, _Context} ->
	    ?LOG("Ignoring event ~s in context ~s", [_Other, _Context]),
	    _ = freeswitch:fetch_reply(Node, FSID, ?EMPTYRESPONSE),
	    {noreply, State}
    end;

handle_info({nodedown, Node}, #state{node=Node}=State) ->
    ?LOG("Node ~p down", [Node]),
    freeswitch:close(Node),
    {ok, _} = timer:send_after(0, self(), {is_node_up, 100}),
    {noreply, State};

handle_info({is_node_up, Timeout}, State) when Timeout > ?FS_TIMEOUT ->
    handle_info({is_node_up, ?FS_TIMEOUT}, State);
handle_info({is_node_up, Timeout}, #state{node=Node}=State) ->
    case ecallmgr_fs_handler:is_node_up(Node) of
	true ->
	    ?LOG("Node ~p recovered, restarting", [self(), Node]),
	    {noreply, State, 0};
	false ->
	    ?LOG("Node ~p still down, retrying in ~p ms", [self(), Node, Timeout]),
	    {ok, _} = timer:send_after(Timeout, self(), {is_node_up, Timeout*2}),
	    {noreply, State}
    end;

handle_info(shutdown, #state{node=Node, lookups=LUs}=State) ->
    lists:foreach(fun({Pid, _CallID, _StartTime}) ->
			  case erlang:is_process_alive(Pid) of
			      true -> Pid ! shutdown;
			      false -> ok
			  end
		  end, LUs),
    ?LOG("Commanded to shutdown node ~p", [Node]),
    {stop, normal, State};

%% send diagnostic info
handle_info({diagnostics, Pid}, #state{stats=Stats, lookups=LUs}=State) ->
    ActiveLUs = [ [{fs_route_id, ID}, {started, Started}] || {_, ID, Started} <- LUs],
    Resp = [{active_lookups, ActiveLUs}
	    ,{amqp_host, amqp_manager:get_host()}
	    | ecallmgr_diagnostics:get_diagnostics(Stats) ],
    Pid ! Resp,
    {noreply, State};

handle_info({'DOWN', _Ref, process, LU, _Reason}, #state{lookups=LUs}=State) ->
    ?LOG("Lookup ~p down: ~p", [LU, _Reason]),
    {noreply, State#state{lookups=lists:keydelete(LU, 1, LUs)}};

handle_info({'EXIT', LU, _Reason}, #state{lookups=LUs}=State) ->
    ?LOG("Lookup ~p exited: ~p", [LU, _Reason]),
    {noreply, State#state{lookups=lists:keydelete(LU, 1, LUs)}};

handle_info(Other, State) ->
    ?LOG("Unhandled message: ~p", [Other]),
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
terminate(_Reason, #state{node=Node}) ->
    freeswitch:close(Node).

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
-spec(handle_route_req/4 :: (Node :: atom(), FSID :: binary(), CallID :: binary(), FSData :: proplist()) -> no_return()).
handle_route_req(Node, FSID, CallID, FSData) ->
    put(callid, CallID),
    Pid = spawn_link(fun() ->
			     DefProp = [{<<"Msg-ID">>, FSID}
					,{<<"Destination-Number">>, props:get_value(<<"Caller-Destination-Number">>, FSData)}
					,{<<"Caller-ID-Name">>, props:get_value(<<"Caller-Caller-ID-Name">>, FSData)}
					,{<<"Caller-ID-Number">>, props:get_value(<<"Caller-Caller-ID-Number">>, FSData)}
					,{<<"To">>, ecallmgr_util:get_sip_to(FSData)}
					,{<<"From">>, ecallmgr_util:get_sip_from(FSData)}
					,{<<"Call-ID">>, CallID}
					,{<<"Custom-Channel-Vars">>, {struct, ecallmgr_util:custom_channel_vars(FSData)}}
					| whistle_api:default_headers(<<>>, <<"dialplan">>, <<"route_req">>, ?APP_NAME, ?APP_VERSION)],
			     %% Server-ID will be over-written by the pool worker
			     {ok, RespProp} = ecallmgr_amqp_pool:route_req(DefProp),

			     true = whistle_api:route_resp_v(RespProp),

			     {ok, Xml} = ecallmgr_fs_xml:route_resp_xml(RespProp),
			     ok = freeswitch:fetch_reply(Node, FSID, Xml), % only start control if freeswitch recv'd reply
			     start_control_and_events(Node, CallID, props:get_value(<<"Server-ID">>, RespProp))
		     end),
    {ok, Pid}.

start_control_and_events(Node, CallID, SendTo) ->
    try
	true = is_binary(CtlQ = amqp_util:new_callctl_queue(<<>>)),
	_ = amqp_util:bind_q_to_callctl(CtlQ),
	{ok, CtlPid} = ecallmgr_call_sup:start_control_process(Node, CallID, CtlQ),
	{ok, _EvtPid} = ecallmgr_call_sup:start_event_process(Node, CallID, CtlPid),

	?LOG("Started control(~p) and event(~p) procs", [CtlPid, _EvtPid]),

	CtlProp = [{<<"Msg-ID">>, CallID}
		   ,{<<"Call-ID">>, CallID}
		   ,{<<"Control-Queue">>, CtlQ}
		   | whistle_api:default_headers(CtlQ, <<"dialplan">>, <<"route_win">>, ?APP_NAME, ?APP_VERSION)],
	send_control_queue(SendTo, CtlProp)
    catch
	_:_ -> {error, amqp_error}
    end.

send_control_queue(SendTo, CtlProp) ->
    case whistle_api:route_win(CtlProp) of
	{ok, JSON} ->
	    ?LOG("Sending route_win to ~s: ~s", [SendTo, JSON]),
	    amqp_util:targeted_publish(SendTo, JSON, <<"application/json">>);
	{error, _Msg} ->
	    ?LOG("Sending route_win to ~s failed: ~p", [self(), SendTo, _Msg])
    end.
