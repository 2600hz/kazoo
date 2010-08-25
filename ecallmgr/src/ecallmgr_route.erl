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
		    spawn(fun() -> lookup_dialplan(State, ID, UUID, Data) end);
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

lookup_dialplan(#state{channel=Channel, ticket=Ticket, app_vsn=Vsn}, ID, UUID, Data) ->
    Q = bind_q(Channel, Ticket, ID),
    {_EvtQ, _CtlQ} = bind_channel_qs(Channel, Ticket, UUID),

    DefProp = whistle_api:default_headers(Q, <<"dialplan">>, <<"ecallmgr">>, Vsn, ID),
    {ok, JSON} = whistle_api:route_req(lists:umerge( [DefProp, Data, [{<<"Call-ID">>, UUID}]] )),
    format_log(info, "JSON REQ: ~s~n", [JSON]),

    {BP, AmqpMsg} = amqp_util:broadcast_publish(Ticket, JSON, <<"application/json">>),
    amqp_channel:call(Channel, BP, AmqpMsg),

    case recv_response(ID) of
	timeout ->
	    format_log(info, "ROUTE: recv route timeout~n", []),
	    ?MODULE:send_fetch_response(ID, ?EMPTYRESPONSE);
	Prop ->
	    format_log(info, "Recv route info: ~p~n", [get_value(<<"Routes">>, Prop, "none")]),
	    ?MODULE:send_fetch_response(ID, ?EMPTYRESPONSE)
    end.

recv_response(ID) ->
    receive
	#'basic.consume_ok'{} ->
	    recv_response(ID);
	{_, #amqp_msg{props = Props, payload = Payload}} ->
	    format_log(info, "Recv Content: ~p Payload: ~s~n", [Props#'P_basic'.content_type, binary_to_list(Payload)]),
	    {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
	    case get_value(<<"Msg-ID">>, Prop) of
		ID -> Prop;
		_BadId ->
		    format_log(info, "Recv Msg ~p when expecting ~p~n", [_BadId, ID]),
		    recv_response(ID)
	    end;
	Msg ->
	    format_log(info, "Received ~p off rabbit~n", [Msg]),
	    recv_response(ID)
    after 2000 ->
	    format_log(info, "Failed to receive after 2000ms~n", []),
	    timeout
    end.

bind_q(Channel, Ticket, ID) ->
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, amqp_util:targeted_exchange(Ticket)),
    #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, amqp_util:new_targeted_queue(Ticket, ID)),
    #'queue.bind_ok'{} = amqp_channel:call(Channel, amqp_util:bind_q_to_targeted(Ticket, Queue, Queue)),
    #'basic.consume_ok'{} = amqp_channel:subscribe(Channel, amqp_util:basic_consume(Ticket, Queue), self()),
    Queue.

bind_channel_qs(Channel, Ticket, UUID) ->
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, amqp_util:callevt_exchange(Ticket)),
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, amqp_util:callctl_exchange(Ticket)),

    #'queue.declare_ok'{queue = EvtQueue} = amqp_channel:call(Channel, amqp_util:new_callevt_queue(Ticket, UUID)),
    #'queue.declare_ok'{queue = CtlQueue} = amqp_channel:call(Channel, amqp_util:new_callctl_queue(Ticket, UUID)),

    #'queue.bind_ok'{} = amqp_channel:call(Channel, amqp_util:bind_q_to_callevt(Ticket, EvtQueue, EvtQueue)),
    #'queue.bind_ok'{} = amqp_channel:call(Channel, amqp_util:bind_q_to_callctl(Ticket, CtlQueue, CtlQueue)),

    %% separate process consumes the callctl messages
    %%#'basic.consume_ok'{} = amqp_channel:subscribe(Channel, amqp_util:basic_consume(Ticket, Queue), self()),
    {EvtQueue, CtlQueue}.
