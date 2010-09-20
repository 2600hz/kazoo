%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Receive dialplan bindings, search for a match, and create call ctl
%%% and evt queues
%%% @end
%%% Created : 24 Aug 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ecallmgr_route).

-behaviour(gen_server).

%% API
-export([start_link/0, add_fs_node/1, rm_fs_node/1]).
-export([fetch_route/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(proplists, [get_value/2, get_value/3]).
-import(logger, [log/2, format_log/3]).

-include("../include/amqp_client/include/amqp_client.hrl").
-include("freeswitch_xml.hrl").

-define(SERVER, ?MODULE). 

-record(route_state, {fs_nodes=[]}).
-record(handler_state, {fs_node, channel, ticket, app_vsn, lookups=[]}).

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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_fs_node(Node) ->
    gen_server:call(?MODULE, {add_fs_node, Node}).

rm_fs_node(Node) ->
    gen_server:call(?MODULE, {rm_fs_node, Node}).

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
init([]) ->
    process_flag(trap_exit, true),
    {ok, #route_state{}}.

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
%% #route_state{fs_nodes=[{FSNode, HandlerPid}]}
%%--------------------------------------------------------------------
handle_call({add_fs_node, Node}, _From, #route_state{fs_nodes=Nodes}=State) ->
    case lists:keyfind(Node, 1, Nodes) of
	{Node, _HandlerPid} ->
	    format_log(info, "ROUTE(~p): handler(~p) known for ~p~n", [self(), _HandlerPid, Node]),
	    {reply, {error, node_is_known}, State};
	false ->
	    case net_adm:ping(Node) of
		pong ->
		    HPid = setup_fs_conn(Node),
		    link(HPid),
		    format_log(info, "Route(~p): Starting handler(~p) for ~p~n", [self(), HPid, Node]),
		    {reply, ok, State#route_state{fs_nodes=[{Node, HPid} | Nodes]}};
		pang ->
		    format_log(error, "Route(~p): ~p not responding~n", [self(), Node]),
		    {reply, {error, no_connection}, State}
	    end
    end;
handle_call({rm_fs_node, Node}, _From, #route_state{fs_nodes=Nodes}=State) ->
    case lists:keyfind(Node, 1, Nodes) of
	{Node, HPid} ->
	    case erlang:is_process_alive(HPid) of
		true ->
		    HPid ! shutdown,
		    format_log(info, "ROUTE(~p): Shutting down handler ~p for ~p~n", [self(), HPid, Node]),
		    {reply, ok, State#route_state{fs_nodes=lists:keydelete(Node, 1, Nodes)}};
		false ->
		    format_log(error, "ROUTE(~p): Handler ~p already down for ~p~n", [self(), HPid, Node]),
		    {reply, {error, node_down}, State}
	    end;
	false ->
	    format_log(error, "ROUTE(~p): No handler for ~p~n", [self(), Node]),
	    {reply, {error, no_node}, State}
    end;
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
handle_info(_Info, State) ->
    format_log(info, "ROUTE(~p): Unhandled Info: ~p~nState: ~p~n", [self(), _Info, State]),
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
terminate(_Reason, #route_state{fs_nodes=Nodes}) ->
    Self = self(),
    format_log(error, "ROUTE(~p): terminating: ~p~n", [Self, _Reason]),
    lists:foreach(fun({_FSNode, HPid}) ->
			  format_log(error, "ROUTE(~p): terminate handler: ~p(~p)~n", [Self, HPid, _FSNode]),
			  HPid ! shutdown
		  end, Nodes),
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
fetch_route(Node, #handler_state{channel=undefined, ticket=undefined}=State) ->
    {ok, Channel, Ticket} = amqp_manager:open_channel(self()),
    fetch_route(Node, State#handler_state{channel=Channel, ticket=Ticket});
fetch_route(Node, #handler_state{channel=Channel, lookups=LUs}=State) ->
    receive
	{fetch, dialplan, _Tag, _Key, _Value, ID, [UUID | Data]} ->
	    case get_value(<<"Event-Name">>, Data) of
		<<"REQUEST_PARAMS">> ->
		    Self = self(),
		    LookupPid = spawn(fun() -> lookup_route(Node, State, ID, UUID, Self, Data) end),
		    link(LookupPid),
		    format_log(info, "FETCH_ROUTE(~p): fetch route: Id: ~p UUID: ~p Lookup: ~p~n"
			       ,[self(), ID, UUID, LookupPid]),
		    ?MODULE:fetch_route(Node, State#handler_state{lookups=[{LookupPid, erlang:now()}|LUs]});
		_Other ->
		    format_log(info, "FETCH_ROUTE(~p): Ignoring event ~p~n", [self(), _Other]),
		    ?MODULE:fetch_route(Node, State)
	    end;
	{fetch, _Section, _Something, _Key, _Value, ID, [undefined | _Data]} ->
	    format_log(info, "FETCH_ROUTE(~p): fetch unknown: Se: ~p So: ~p, K: ~p V: ~p ID: ~p~nD: ~p~n", [self(), _Section, _Something, _Key, _Value, ID, _Data]),
	    freeswitch:fetch_reply(Node, ID, ?EMPTYRESPONSE),
	    ?MODULE:fetch_route(Node, State);
	{nodedown, Node} ->
	    format_log(error, "FETCH_ROUTE(~p): Node ~p exited", [self(), Node]),
	    ok;
	{xml_response, ID, XML} ->
	    format_log(info, "FETCH_ROUTE(~p): Received XML for ID ~p~n", [self(), ID]),
	    freeswitch:fetch_reply(Node, ID, XML),
	    ?MODULE:fetch_route(Node, State);
	{'EXIT', Channel, noconnection} ->
	    {ok, Channel1, Ticket1} = amqp_manager:open_channel(self()),
	    format_log(error, "FETCH_ROUTE(~p): Channel(~p) went down; replaced with ~p~n", [self(), Channel, Channel1]),
	    ?MODULE:fetch_route(Node, State#handler_state{channel=Channel1, ticket=Ticket1});
	shutdown ->
	    lists:foreach(fun({Pid,_StartTime}) ->
				  case erlang:is_process_alive(Pid) of
				      true -> Pid ! shutdown;
				      false -> ok
				  end
			  end, LUs),
	    format_log(error, "FETCH_ROUTE(~p): shutting down~n", [self()]);
	{lookup_finished, LookupPid} ->
	    case get_value(LookupPid, LUs) of
		StartTime when is_tuple(StartTime) ->
		    format_log(info, "FETCH_ROUTE(~p): lookup (~p) finished in ~p ms~n"
			       ,[self(), LookupPid, timer:now_diff(erlang:now(), StartTime) div 1000]),
		    ?MODULE:fetch_route(Node, State#handler_state{lookups=proplists:delete(LookupPid, LUs)});
		undefined ->
		    format_log(error, "FETCH_ROUTE(~p): unknown lookup ~p~n", [self(), LookupPid]),
		    ?MODULE:fetch_route(Node, State)
	    end;
	Other ->
	    format_log(info, "FETCH_ROUTE(~p): got other response: ~p", [self(), Other]),
	    ?MODULE:fetch_route(Node, State)
    end.

lookup_route(Node, #handler_state{channel=Channel, ticket=Ticket, app_vsn=Vsn}=HState, ID, UUID, FetchPid, Data) ->
    Q = bind_q(Channel, Ticket, ID),
    {EvtQ, CtlQ} = bind_channel_qs(Channel, Ticket, UUID, Node),

    DefProp = [{<<"Msg-ID">>, ID}
	       ,{<<"Caller-ID-Name">>, get_value(<<"Caller-Caller-ID-Name">>, Data)}
	       ,{<<"Caller-ID-Number">>, get_value(<<"Caller-Caller-ID-Number">>, Data)}
	       | whistle_api:default_headers(Q, <<"dialplan">>, <<"routing">>, <<"ecallmgr">>, Vsn)],
    case whistle_api:route_req(lists:umerge([DefProp, Data, [{<<"Call-ID">>, UUID}
							     ,{<<"Event-Queue">>, EvtQ}
							    ]
					    ]
					   )) of
	{ok, JSON} ->
	    format_log(info, "L/U-R(~p): Sending RouteReq JSON over Channel(~p)~n", [self(), Channel]),
	    send_request(Channel, Ticket, JSON),
	    handle_response(ID, UUID, EvtQ, CtlQ, HState, FetchPid),
	    ecallmgr_amqp:delete_queue(Q);
	{error, _Msg} ->
	    format_log(error, "L/U-R(~p): Route Req API error ~p~n", [self(), _Msg])
    end,
    FetchPid ! {lookup_finished, self()}.

send_request(Channel, Ticket, JSON) ->
    {BP, AmqpMsg} = amqp_util:broadcast_publish(Ticket, JSON, <<"application/json">>),
    amqp_channel:cast(Channel, BP, AmqpMsg).

recv_response(ID) ->
    receive
	#'basic.consume_ok'{} ->
	    recv_response(ID);
	{_, #amqp_msg{props = Props, payload = Payload}} ->
	    format_log(info, "L/U-R(~p): Recv Content: ~p Payload: ~s~n"
		       ,[self(), Props#'P_basic'.content_type, binary_to_list(Payload)]),
	    {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
	    case get_value(<<"Msg-ID">>, Prop) of
		ID -> Prop;
		_BadId ->
		    format_log(info, "L/U-R(~p): Recv MsgID ~p when expecting ~p~n", [self(), _BadId, ID]),
		    recv_response(ID)
	    end;
	shutdown -> shutdown;
	_Msg ->
	    format_log(info, "L/U-R(~p): Unexpected: received ~p off rabbit~n", [self(), _Msg]),
	    recv_response(ID)
    after 4000 ->
	    format_log(info, "L/U-R(~p): Failed to receive after 4000ms~n", [self()]),
	    timeout
    end.

bind_q(Channel, Ticket, ID) ->
    amqp_channel:cast(Channel, amqp_util:targeted_exchange(Ticket)),
    #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, amqp_util:new_targeted_queue(Ticket, ID)),
    amqp_channel:cast(Channel, amqp_util:bind_q_to_targeted(Ticket, Queue, Queue)),
    #'basic.consume_ok'{} = amqp_channel:subscribe(Channel, amqp_util:basic_consume(Ticket, Queue), self()),
    Queue.

%% creates the event and control queues for the call, spins up the event handler
%% to pump messages to the queue, and returns the control queue
bind_channel_qs(Channel, Ticket, UUID, Node) ->
    amqp_channel:cast(Channel, amqp_util:callevt_exchange(Ticket)),
    amqp_channel:cast(Channel, amqp_util:callctl_exchange(Ticket)),

    #'queue.declare_ok'{queue = EvtQueue} = amqp_channel:call(Channel, amqp_util:new_callevt_queue(Ticket, UUID)),
    #'queue.declare_ok'{queue = CtlQueue} = amqp_channel:call(Channel, amqp_util:new_callctl_queue(Ticket, UUID)),

    amqp_channel:cast(Channel, amqp_util:bind_q_to_callevt(Ticket, EvtQueue, EvtQueue)),
    amqp_channel:cast(Channel, amqp_util:bind_q_to_callctl(Ticket, CtlQueue, CtlQueue)),

    CtlPid = ecallmgr_call_control:start(Node, UUID, {Channel, Ticket, CtlQueue}),
    ecallmgr_call_events:start(Node, UUID, {Channel, Ticket, EvtQueue}, CtlPid),
    {EvtQueue, CtlQueue}.

send_control_queue(_Channel, _Ticket, _Q, undefined, _EvtQ) ->
    format_log(error, "ROUTE(~p): Cannot send control Q(~p) to undefined server-id~n", [self(), _Q]);
send_control_queue(Channel, Ticket, CtlProp, AppQ, EvtQ) ->
    Payload = [ {<<"Event-Name">>, <<"Call-Control">>}
		,{<<"Event-Queue">>, EvtQ}
		| CtlProp ],
    {BP, AmqpMsg} = amqp_util:targeted_publish(Ticket
					       ,AppQ
					       ,list_to_binary(mochijson2:encode({struct, Payload}))
					       ,<<"application/json">>
					      ),
    %% execute the publish command
    format_log(info, "L/U-R(~p): Sending AppQ(~p) the control Q~n", [self(), AppQ]),
    amqp_channel:cast(Channel, BP, AmqpMsg).

%% Prop = Route Response
generate_xml(<<"bridge">>, Routes, _Prop) ->
    format_log(info, "L/U-R(~p): BRIDGEXML: Routes:~n~p~n", [self(), Routes]),
    %% format the Route based on protocol
    {_Idx, Extensions} = lists:foldl(fun({struct, RouteProp}, {Idx, Acc}) ->
					     Route = get_value(<<"Route">>, RouteProp), %% translate Route to FS-encoded URI
					     BypassMedia = case get_value(<<"Media">>, RouteProp) of
							       <<"bypass">> -> "true";
							       <<"process">> -> "false";
							       _ -> "true" %% auto?
							   end,
					     ChannelVars = get_channel_vars(RouteProp),
					     Ext = io_lib:format(?ROUTE_BRIDGE_EXT, [Idx, BypassMedia, ChannelVars, Route]),
					     {Idx+1, [Ext | Acc]}
				     end, {1, ""}, lists:reverse(Routes)),
    format_log(info, "L/U-R(~p): RoutesXML: ~s~n", [self(), Extensions]),
    lists:flatten(io_lib:format(?ROUTE_BRIDGE_RESPONSE, [Extensions]));
generate_xml(<<"park">>, _Routes, _Prop) ->
    ?ROUTE_PARK_RESPONSE;
generate_xml(<<"error">>, _Routes, Prop) ->
    ErrCode = get_value(<<"Route-Error-Code">>, Prop),
    ErrMsg = list_to_binary([" ", get_value(<<"Route-Error-Message">>, Prop, <<"">>)]),
    format_log(info, "L/U-R(~p): ErrorXML: ~s ~s~n", [self(), ErrCode, ErrMsg]),
    lists:flatten(io_lib:format(?ROUTE_ERROR_RESPONSE, [ErrCode, ErrMsg])).

get_channel_vars(Prop) ->
    Vars = lists:foldr(fun get_channel_vars/2, [], Prop),
    lists:flatten(["{", string:join(lists:map(fun binary_to_list/1, Vars), ","), "}"]).

get_channel_vars({<<"Auth-User">>, V}, Vars) ->
    [ list_to_binary(["sip_auth_username='", V, "'"]) | Vars];
get_channel_vars({<<"Auth-Pass">>, V}, Vars) ->
    [ list_to_binary(["sip_auth_password='", V, "'"]) | Vars];
get_channel_vars({_K, _V}, Vars) ->
    format_log(info, "ROUTE(~p): Unknown channel var ~p::~p~n", [self(), _K, _V]),
    Vars.

%% setup a connection to mod_erlang_event for dialplan requests,
%% or setup a timer to query for the node and return the timer ref
setup_fs_conn(Node) ->
    {ok, Vsn} = application:get_key(ecallmgr, vsn),
    HState = #handler_state{fs_node=Node, app_vsn=list_to_binary(Vsn)},
    {ok, Pid} = freeswitch:start_fetch_handler(Node, dialplan, ?MODULE, fetch_route, HState),
    Pid.

handle_response(ID, UUID, EvtQ, CtlQ, #handler_state{channel=Channel, ticket=Ticket, app_vsn=Vsn}, FetchPid) ->
    T1 = erlang:now(),
    case recv_response(ID) of
	shutdown ->
	    format_log(error, "L/U-R(~p): Shutting down for ID ~p~n", [self(), ID]),
	    shutdown;
	timeout ->
	    FetchPid ! {xml_response, ID, ?ROUTE_NOT_FOUND_RESPONSE};
	Prop ->
	    Xml = generate_xml(get_value(<<"Method">>, Prop), get_value(<<"Routes">>, Prop), Prop),
	    format_log(info, "L/U-R(~p): Sending XML to FS(~p) took ~pms ~n"
		       ,[self(), ID, timer:now_diff(erlang:now(), T1) div 1000]),
	    FetchPid ! {xml_response, ID, Xml},
	    CtlProp = [{<<"Msg-ID">>, UUID} |
	    whistle_api:default_headers(CtlQ, <<"dialplan">>, <<"control">>, <<"ecallmgr">>, Vsn)],
	    send_control_queue(Channel, Ticket
			       , [{<<"Call-ID">>, UUID} |  CtlProp]
			       , get_value(<<"Server-ID">>, Prop), EvtQ)
    end.
