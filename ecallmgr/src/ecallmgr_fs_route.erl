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

%% Internal API
-export([init_route_req/5]).

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
    ?LOG_SYS("starting new fs route listener for ~s", [Node]),
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
-spec handle_info/2 :: ('timeout', #state{}) -> handle_info_ret();
		       ({'nodedown', atom()}, #state{}) -> {'noreply', #state{}};
                       ({'is_node_up', non_neg_integer()}, #state{}) -> handle_info_ret();
		       ({'fetch', atom(), _, _, _, binary(), proplist()}, #state{}) ->
			       {'noreply', #state{}};
		       ('shutdown', #state{}) -> {'stop', term(), #state{}};
		       ({'diagnostics', pid()}, #state{}) -> {'noreply', #state{}};
		       ({'DOWN', reference(), 'process', pid(), term()}, #state{}) -> {'noreply', #state{}};
		       ({'EXIT', pid(), term()}, #state{}) -> {'noreply', #state{}}.
handle_info({fetch, _Section, _Something, _Key, _Value, ID, [undefined | _Data]}, #state{node=Node}=State) ->
    ?LOG("fetch unknown section: ~p So: ~p, K: ~p V: ~p ID: ~s", [_Section, _Something, _Key, _Value, ID]),
    _ = freeswitch:fetch_reply(Node, ID, ?EMPTYRESPONSE),
    {noreply, State};

handle_info({fetch, dialplan, _Tag, _Key, _Value, FSID, [CallID | FSData]}, #state{node=Node, stats=Stats, lookups=LUs}=State) ->
    case {props:get_value(<<"Event-Name">>, FSData), props:get_value(<<"Caller-Context">>, FSData)} of
	{<<"REQUEST_PARAMS">>, ?WHISTLE_CONTEXT} ->
	    {ok, LookupPid} = ecallmgr_fs_route_sup:start_req(Node, FSID, CallID, FSData),
	    erlang:monitor(process, LookupPid),

	    LookupsReq = Stats#handler_stats.lookups_requested + 1,
	    ?LOG_START(CallID, "processing fetch request ~s (~b) in PID ~p", [FSID, LookupsReq, LookupPid]),
	    {noreply, State#state{lookups=[{LookupPid, FSID, erlang:now()} | LUs]
				  ,stats=Stats#handler_stats{lookups_requested=LookupsReq}}, hibernate};
	{_Other, _Context} ->
	    ?LOG("ignoring event ~s in context ~s", [_Other, _Context]),
	    _ = freeswitch:fetch_reply(Node, FSID, ?EMPTYRESPONSE),
	    {noreply, State, hibernate}
    end;

handle_info({'DOWN', _Ref, process, LU, _Reason}, #state{lookups=LUs}=State) ->
    ?LOG("lookup task ~p went down, ~p", [LU, _Reason]),
    {noreply, State#state{lookups=lists:keydelete(LU, 1, LUs)}, hibernate};

handle_info({'EXIT', LU, _Reason}, #state{lookups=LUs}=State) ->
    ?LOG("lookup task ~p exited, ~p", [LU, _Reason]),
    {noreply, State#state{lookups=lists:keydelete(LU, 1, LUs)}, hibernate};

handle_info({nodedown, Node}, #state{node=Node}=State) ->
    ?LOG_SYS("lost connection to node ~s, waiting for reconnection", [Node]),
    freeswitch:close(Node),
    _Ref = erlang:send_after(0, self(), {is_node_up, 100}),
    {noreply, State};

handle_info({is_node_up, Timeout}, State) when Timeout > ?FS_TIMEOUT ->
    handle_info({is_node_up, ?FS_TIMEOUT}, State);
handle_info({is_node_up, Timeout}, #state{node=Node}=State) ->
    case ecallmgr_fs_handler:is_node_up(Node) of
	true ->
	    ?LOG("node ~p recovered, restarting", [self(), Node]),
	    {noreply, State, 0};
	false ->
	    ?LOG("node ~p still down, retrying in ~p ms", [self(), Node, Timeout]),
	    _Ref = erlang:send_after(Timeout, self(), {is_node_up, Timeout*2}),
	    {noreply, State}
    end;

handle_info(shutdown, #state{node=Node, lookups=LUs}=State) ->
    lists:foreach(fun({Pid, _CallID, _StartTime}) ->
			  case erlang:is_process_alive(Pid) of
			      true -> Pid ! shutdown;
			      false -> ok
			  end
		  end, LUs),
    ?LOG("commanded to shutdown node ~s", [Node]),
    {stop, normal, State};

handle_info({diagnostics, Pid}, #state{stats=Stats, lookups=LUs}=State) ->
    ActiveLUs = [ [{fs_route_id, ID}, {started, Started}] || {_, ID, Started} <- LUs],
    Resp = [{active_lookups, ActiveLUs}
	    ,{amqp_host, amqp_mgr:get_host()}
	    | ecallmgr_diagnostics:get_diagnostics(Stats) ],
    Pid ! Resp,
    {noreply, State};

handle_info(timeout, #state{node=Node}=State) ->
    Type = {bind, dialplan},
    erlang:monitor_node(Node, true),
    {foo, Node} ! Type,
    receive
	ok ->
	    ?LOG_SYS("bound to dialplan request on ~s", [Node]),
	    {noreply, State};
	{error, Reason} ->
	    ?LOG_SYS("failed to bind to dialplan requests on ~s, ~p", [Node, Reason]),
	    {stop, Reason, State}
    after ?FS_TIMEOUT ->
	    ?LOG_SYS("timed out binding to dialplan requests on ~s", [Node]),
	    {stop, timeout, State}
    end;

handle_info(_Other, State) ->
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
    ?LOG_SYS("fs route ~p termination", [_Reason]),
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
-spec handle_route_req/4 :: (Node, FSID, CallID, FSData) -> {'ok', pid()} when
      Node :: atom(),
      FSID :: binary(),
      CallID :: binary(),
      FSData :: proplist().
handle_route_req(Node, FSID, CallID, FSData) ->
    proc_lib:start_link(?MODULE, init_route_req, [self(), Node, FSID, CallID, FSData]).

-spec init_route_req/5 :: (Parent, Node, FSID, CallID, FSData) -> no_return() when
      Parent :: pid(),
      Node :: atom(),
      FSID :: binary(),
      CallID :: binary(),
      FSData :: proplist().
init_route_req(Parent, Node, FSID, CallID, FSData) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    put(callid, CallID),
    process_route_req(Node, FSID, CallID, FSData).

-spec process_route_req/4 :: (Node, FSID, CallID, FSData) -> ok when
      Node :: atom(),
      FSID :: binary(),
      CallID :: binary(),
      FSData :: proplist().
process_route_req(Node, FSID, CallID, FSData) ->
    DefProp = [{<<"Msg-ID">>, FSID}
	       ,{<<"Caller-ID-Name">>, props:get_value(<<"Caller-Caller-ID-Name">>, FSData)}
	       ,{<<"Caller-ID-Number">>, props:get_value(<<"Caller-Caller-ID-Number">>, FSData)}
	       ,{<<"To">>, ecallmgr_util:get_sip_to(FSData)}
	       ,{<<"From">>, ecallmgr_util:get_sip_from(FSData)}
	       ,{<<"Request">>, ecallmgr_util:get_sip_request(FSData)}
	       ,{<<"Call-ID">>, CallID}
	       ,{<<"Custom-Channel-Vars">>, {struct, ecallmgr_util:custom_channel_vars(FSData)}}
	       | wh_api:default_headers(<<>>, <<"dialplan">>, <<"route_req">>, ?APP_NAME, ?APP_VERSION)],
    %% Server-ID will be over-written by the pool worker

    {ok, AuthZEnabled} = ecallmgr_util:get_setting(authz_enabled, true),
    case wh_util:is_true(AuthZEnabled) of
	true -> authorize_and_route(Node, FSID, CallID, FSData, DefProp);
	false -> route(Node, FSID, CallID, DefProp, undefined)
    end.

-spec authorize_and_route/5 :: (Node, FSID, CallID, FSData, DefProp) -> 'ok' when
      Node :: atom(),
      FSID :: binary(),
      CallID :: binary(),
      FSData :: proplist(),
      DefProp :: proplist().
authorize_and_route(Node, FSID, CallID, FSData, DefProp) ->
    ?LOG("Starting authorization request"),
    {ok, AuthZPid} = ecallmgr_authz:authorize(FSID, CallID, FSData),
    route(Node, FSID, CallID, DefProp, AuthZPid).

-spec route/5 :: (Node, FSID, CallID, DefProp, AuthZPid) -> ok when
      Node :: atom(),
      FSID :: binary(),
      CallID :: binary(),
      DefProp :: proplist(),
      AuthZPid :: pid() | undefined.
route(Node, FSID, CallID, DefProp, AuthZPid) ->
    ?LOG("Starting route request"),
    {ok, RespJObj} = ecallmgr_amqp_pool:route_req(DefProp),
    RouteCCV = wh_json:get_value(<<"Custom-Channel-Vars">>, RespJObj, ?EMPTY_JSON_OBJECT),
    authorize(Node, FSID, CallID, RespJObj, AuthZPid, RouteCCV).

-spec authorize/6 :: (Node, FSID, CallID, RespJObj, AuthZPid, RouteCCV) -> ok when
      Node :: atom(),
      FSID :: binary(),
      CallID :: binary(),
      RespJObj :: json_object(),
      AuthZPid :: pid() | undefined,
      RouteCCV :: json_object().
authorize(Node, FSID, CallID, RespJObj, undefined, RouteCCV) ->
    ?LOG("No authz available, validating route_resp"),
    true = wh_api:route_resp_v(RespJObj),
    reply(Node, FSID, CallID, RespJObj, RouteCCV);
authorize(Node, FSID, CallID, RespJObj, AuthZPid, RouteCCV) ->
    ?LOG("Checking authz_resp"),
    case ecallmgr_authz:is_authorized(AuthZPid) of
	{false, _} ->
	    ?LOG("Authz is false"),
	    reply_forbidden(Node, FSID);
	{true, {struct, CCV}} ->
	    ?LOG("Authz is true"),
	    true = wh_api:route_resp_v(RespJObj),
	    ?LOG("Valid route resp"),
	    RouteCCV1 = lists:foldl(fun({K,V}, RouteCCV0) -> wh_json:set_value(K, V, RouteCCV0) end, RouteCCV, CCV),

	    reply(Node, FSID, CallID, RespJObj, RouteCCV1)
    end.

-spec reply_forbidden/2 :: (Node, FSID) -> ok when
      Node :: atom(),
      FSID :: binary().
reply_forbidden(Node, FSID) ->
    {ok, XML} = ecallmgr_fs_xml:route_resp_xml([{<<"Method">>, <<"error">>}
						,{<<"Route-Error-Code">>, <<"486">>}
						,{<<"Route-Error-Message">>, <<"No more channels">>}
					       ]),
    case freeswitch:fetch_reply(Node, FSID, XML) of
	ok ->
	    %% only start control if freeswitch recv'd reply
	    ?LOG_END("freeswitch accepted our route unauthz");
	{error, Reason} ->
	    ?LOG_END("freeswitch rejected our route unauthz, ~p", [Reason]);
	timeout ->
	    ?LOG_END("received no reply from freeswitch, timeout")
    end.

-spec reply/5 :: (Node, FSID, CallID, RespJObj, CCVs) -> ok when
      Node :: atom(),
      FSID :: binary(),
      CallID :: binary(),
      RespJObj :: json_object(),
      CCVs :: json_object().
reply(Node, FSID, CallID, RespJObj, CCVs) ->
    {ok, XML} = ecallmgr_fs_xml:route_resp_xml(RespJObj),
    ServerQ = wh_json:get_value(<<"Server-ID">>, RespJObj),

    case freeswitch:fetch_reply(Node, FSID, XML) of
	ok ->
	    %% only start control if freeswitch recv'd reply
	    ?LOG("freeswitch accepted our route, starting control and events"),
	    start_control_and_events(Node, CallID, ServerQ, CCVs);
	{error, Reason} ->
	    ?LOG_END("freeswitch rejected our route response, ~p", [Reason]);
	timeout ->
	    ?LOG_END("received no reply from freeswitch, timeout")
    end.

-spec start_control_and_events/4 :: (Node, CallID, SendTo, CCVs) -> ok when
      Node :: atom(),
      CallID :: binary(),
      SendTo :: binary(),
      CCVs :: json_object().
start_control_and_events(Node, CallID, SendTo, CCVs) ->
    try
	true = is_binary(CtlQ = amqp_util:new_callctl_queue(<<>>)),
	_ = amqp_util:bind_q_to_callctl(CtlQ),
	{ok, CtlPid} = ecallmgr_call_sup:start_control_process(Node, CallID, CtlQ),
	{ok, _EvtPid} = ecallmgr_call_sup:start_event_process(Node, CallID, CtlPid),

	CtlProp = [{<<"Msg-ID">>, CallID}
		   ,{<<"Call-ID">>, CallID}
		   ,{<<"Control-Queue">>, CtlQ}
                   ,{<<"Custom-Channel-Vars">>, CCVs}
		   | wh_api:default_headers(CtlQ, <<"dialplan">>, <<"route_win">>, ?APP_NAME, ?APP_VERSION)],
	send_control_queue(SendTo, CtlProp)
    catch
	_:Reason ->
            ?LOG_END("error during control handoff to whapp, ~p", [Reason]),
            {error, amqp_error}
    end.

-spec send_control_queue/2 :: (SendTo, CtlProp) -> ok when
      SendTo :: binary(),
      CtlProp :: proplist().
send_control_queue(SendTo, CtlProp) ->
    case wh_api:route_win(CtlProp) of
	{ok, JSON} ->
	    ?LOG_END("sending route_win to ~s", [SendTo]),
	    amqp_util:targeted_publish(SendTo, JSON, <<"application/json">>);
	{error, _Msg} ->
	    ?LOG_END("sending route_win to ~s failed, ~p", [SendTo, _Msg])
    end.
