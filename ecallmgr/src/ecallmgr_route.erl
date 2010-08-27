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
-export([start_link/0, lookup_dialplan/2, send_fetch_response/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).


-import(proplists, [get_value/2, get_value/3]).
-import(logger, [log/2, format_log/3]).

-include("../include/amqp_client/include/amqp_client.hrl").
-include("freeswitch_xml.hrl").

-define(SERVER, ?MODULE). 

-record(state, {fs_node, channel, ticket, app_vsn}).

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

send_fetch_response(ID, Response) ->
    gen_server:cast(?MODULE, {send_fetch_response, ID, Response}).

%% see lookup_dialplan/2 after gen_server callbacks

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
    Node = list_to_atom(lists:concat(["freeswitch@", net_adm:localhost()])),
    {ok, Channel, Ticket} = amqp_manager:open_channel(self()),
    {ok, Vsn} = application:get_key(ecallmgr, vsn),
    State = #state{fs_node=Node, channel=Channel, ticket=Ticket, app_vsn=list_to_binary(Vsn)},
    case net_adm:ping(Node) of
	pong ->
	    {ok, Pid} = freeswitch:start_fetch_handler(Node, dialplan, ?MODULE, lookup_dialplan, State),
	    link(Pid);
	_ ->
	    format_log(error, "RSC: Unable to find ~p to talk to freeSWITCH~n", [Node])
    end,
    {ok, State}.

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
handle_cast({send_fetch_response, ID, Response}, #state{fs_node=Node}=State) ->
    freeswitch:fetch_reply(Node, ID, Response),
    {noreply, State};
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

%%%===================================================================
%%% Internal functions
%%%===================================================================
lookup_dialplan(Node, State) ->
    receive
	{fetch, dialplan, _Tag, _Key, _Value, ID, [UUID | Data]} ->
	    format_log(info, "ROUTE: fetch dialplan: Id: ~p UUID: ~p~nData: ~p~n", [ID, UUID, Data]),
	    case get_value(<<"Event-Name">>, Data) of
		<<"REQUEST_PARAMS">> ->
		    spawn(fun() -> lookup_dialplan(Node, State, ID, UUID, Data) end);
		_Other ->
		    format_log(info, "ROUTE(~p): Ignoring event ~p~n", [self(), _Other])
	    end,
	    ?MODULE:lookup_dialplan(Node, State);
	{fetch, _Section, _Something, _Key, _Value, ID, [undefined | _Data]} ->
	    format_log(info, "ROUTE: fetch unknown: Se: ~p So: ~p, K: ~p V: ~p ID: ~p~nD: ~p~n", [_Section, _Something, _Key, _Value, ID, _Data]),
	    freeswitch:fetch_reply(Node, ID, ?EMPTYRESPONSE),
	    ?MODULE:lookup_dialplan(Node, State);
	{nodedown, Node} ->
	    format_log(error, "ROUTE: Node we were serving XML search requests to exited", []),
	    ok;
	Other ->
	    format_log(info, "ROUTE: got other response: ~p", [Other]),
	    ?MODULE:lookup_dialplan(Node, State)
    end.

lookup_dialplan(Node, #state{channel=Channel, ticket=Ticket, app_vsn=Vsn}, ID, UUID, Data) ->
    Q = bind_q(Channel, Ticket, ID),
    {EvtQ, CtlQ} = bind_channel_qs(Channel, Ticket, UUID, Node),

    DefProp = whistle_api:default_headers(Q, <<"dialplan">>, <<"ecallmgr">>, Vsn, ID),
    {ok, JSON} = whistle_api:route_req(lists:umerge([DefProp, Data, [{<<"Call-ID">>, UUID}
								     ,{<<"Event-Queue">>, EvtQ}
								    ]
						    ]
						   )),
    format_log(info, "ROUTE: JSON REQ: ~s~n", [JSON]),

    {BP, AmqpMsg} = amqp_util:broadcast_publish(Ticket, JSON, <<"application/json">>),
    amqp_channel:call(Channel, BP, AmqpMsg),

    case recv_response(ID) of
	timeout ->
	    format_log(info, "ROUTE: recv route timeout~n", []),
	    ?MODULE:send_fetch_response(ID, ?ROUTE_NOT_FOUND_RESPONSE);
	Prop ->
	    Xml = generate_xml(get_value(<<"Method">>, Prop), get_value(<<"Routes">>, Prop)),
	    format_log(info, "ROUTE: Sending XML to FS: ~n~p~n", [Xml]),
	    ?MODULE:send_fetch_response(ID, Xml),
	    CtlProp = whistle_api:default_headers(CtlQ, <<"dialplan">>, <<"ecallmgr">>, Vsn, ID),
	    send_control_queue(Channel, Ticket
			       , [{<<"Call-ID">>, UUID} |  CtlProp]
			       , get_value(<<"Server-ID">>, Prop), EvtQ)
    end.

recv_response(ID) ->
    receive
	#'basic.consume_ok'{} ->
	    recv_response(ID);
	{_, #amqp_msg{props = Props, payload = Payload}} ->
	    format_log(info, "ROUTE: Recv Content: ~p Payload: ~s~n", [Props#'P_basic'.content_type, binary_to_list(Payload)]),
	    {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
	    case get_value(<<"Msg-ID">>, Prop) of
		ID -> Prop;
		_BadId ->
		    format_log(info, "ROUTE: Recv MsgID ~p when expecting ~p~n", [_BadId, ID]),
		    recv_response(ID)
	    end;
	_Msg ->
	    format_log(info, "ROUTE: Unexpected: received ~p off rabbit~n", [_Msg]),
	    recv_response(ID)
    after 4000 ->
	    format_log(info, "ROUTE: Failed to receive after 4000ms~n", []),
	    timeout
    end.

bind_q(Channel, Ticket, ID) ->
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, amqp_util:targeted_exchange(Ticket)),
    #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, amqp_util:new_targeted_queue(Ticket, ID)),
    #'queue.bind_ok'{} = amqp_channel:call(Channel, amqp_util:bind_q_to_targeted(Ticket, Queue, Queue)),
    #'basic.consume_ok'{} = amqp_channel:subscribe(Channel, amqp_util:basic_consume(Ticket, Queue), self()),
    Queue.

%% creates the event and control queues for the call, spins up the event handler
%% to pump messages to the queue, and returns the control queue
bind_channel_qs(Channel, Ticket, UUID, Node) ->
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, amqp_util:callevt_exchange(Ticket)),
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, amqp_util:callctl_exchange(Ticket)),

    #'queue.declare_ok'{queue = EvtQueue} = amqp_channel:call(Channel, amqp_util:new_callevt_queue(Ticket, UUID)),
    #'queue.declare_ok'{queue = CtlQueue} = amqp_channel:call(Channel, amqp_util:new_callctl_queue(Ticket, UUID)),

    #'queue.bind_ok'{} = amqp_channel:call(Channel, amqp_util:bind_q_to_callevt(Ticket, EvtQueue, EvtQueue)),
    #'queue.bind_ok'{} = amqp_channel:call(Channel, amqp_util:bind_q_to_callctl(Ticket, CtlQueue, CtlQueue)),

    CtlPid = ecallmgr_call_control:start(Node, UUID, {Channel, Ticket, CtlQueue}),
    ecallmgr_call_events:start(Node, UUID, {Channel, Ticket, EvtQueue}, CtlPid),
    {EvtQueue, CtlQueue}.

send_control_queue(_Channel, _Ticket, _Q, undefined, _EvtQ) ->
    format_log(error, "ROUTE: Cannot send control Q(~p) to undefined server-id~n", [_Q]);
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
    format_log(info, "ROUTE: Sending AppQ(~p) AmqpMsg:~n~p~n", [AppQ, Payload]),
    format_log(info, "ROUTE: AMQP Send of CallCtl ~p~n", [amqp_channel:call(Channel, BP, AmqpMsg)]).

%% Prop = Route Response
generate_xml(<<"bridge">>, Routes) ->
    format_log(info, "ROUTE: BRIDGEXML: Routes:~n~p~n", [Routes]),
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
    format_log(info, "ROUTE: RoutesXML: ~s~n", [Extensions]),
    lists:flatten(io_lib:format(?ROUTE_BRIDGE_RESPONSE, [Extensions]));
generate_xml(_, _Prop) ->
    ?ROUTE_PARK_RESPONSE.

get_channel_vars(Prop) ->
    Vars = lists:foldr(fun get_channel_vars/2, [], Prop),
    lists:flatten(["{", string:join(lists:map(fun binary_to_list/1, Vars), ","), "}"]).

get_channel_vars({<<"Auth-User">>, V}, Vars) ->
    [ list_to_binary(["sip_auth_username='", V, "'"]) | Vars];
get_channel_vars({<<"Auth-Pass">>, V}, Vars) ->
    [ list_to_binary(["sip_auth_password='", V, "'"]) | Vars];
get_channel_vars({_K, _V}, Vars) ->
    format_log(info, "ROUTE: Unknown channel var ~p::~p~n", [_K, _V]),
    Vars.
