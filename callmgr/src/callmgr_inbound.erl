%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Receive requests from the BroadcastXc for inbound call control,
%%% things like listening for a call being parked.
%%% @end
%%% Created :  2 Aug 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(callmgr_inbound).

-include("../include/amqp_client/include/amqp_client.hrl").
-include("../../include/fs.hrl").

-behaviour(gen_server).

-import(proplists, [get_value/2, get_value/3]).
-import(logger, [log/2, format_log/3]).

%% API
-export([start_link/0, event_received/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {channel, ticket, queue, tag, fs=#fs_conn{}, reg_events=[]}).

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

event_received(Evt) ->
    gen_server:cast(?MODULE, {event_received, Evt}).

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
    {ok, Channel, Ticket} = amqp_manager:open_channel(self()),
    format_log(info, "CALLMGR_INBND: Channel open to MQ: ~p Ticket: ~p~n", [Channel, Ticket]),

    process_flag(trap_exit, true),

    Exchange = amqp_util:broadcast_exchange(Ticket),
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Exchange),
    format_log(info, "CALLMGR_INBND: Accessing Exchange ~p~n", [Exchange]),

    Queue = list_to_binary([#'exchange.declare'.exchange, ".callmgr_inbound.", net_adm:localhost()]),
    QueueDeclare = amqp_util:new_queue(Ticket, Queue),
    #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, QueueDeclare),

    %% Bind the queue to an exchange
    QueueBind = amqp_util:bind_q_to_broadcast(Ticket, Queue),
    #'queue.bind_ok'{} = amqp_channel:call(Channel, QueueBind),
    format_log(info, "CALLMGR_INBND Bound ~p to ~p~n", [Queue, #'exchange.declare'.exchange]),

    %% Register a consumer to listen to the queue
    BasicConsume = amqp_util:basic_consume(Ticket, Queue),
    #'basic.consume_ok'{consumer_tag = Tag}
        = amqp_channel:subscribe(Channel, BasicConsume, self()),

    format_log(info, "CALLMGR_INBND(~p): Starting FS Event Socket Listener~n", [self()]),
    Fs = freeswitch:new_fs_socket(#fs_conn{}),

    freeswitch:read_loop(Fs#fs_conn.socket, fun callmgr_inbound:process_socket_evt/2),

    {ok, #state{channel=Channel, ticket=Ticket, tag=Tag, queue=Queue, fs=Fs}}.

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
handle_cast({event_received, _Evt}, State) ->
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
handle_info({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State};
%% receive resource requests from Apps
handle_info({_, #amqp_msg{props = Props, payload = Payload}}, State) ->
    format_log(info, "CALLMGR_INBND(~p): Headers: ~p~nPayload: ~p~n", [self(), Props, Payload]),
    case Props#'P_basic'.content_type of
	<<"application/json">> ->
	    process_json_req(State, binary_to_list(Payload)),
	    State;
	<<"erlang/term">> ->
	    log(info, erlang_term),
	    State;
	undefined ->
	    log(info, undefined),
	    try binary_to_term(Payload) of
		_Term -> State
	    catch
		_:_ -> State
	    end;
	_ContentType ->
	    format_log(info, "CALLMGR_INBND(~p): recieved unknown msg type: ~p~n", [self(), _ContentType]),
	    State
    end,
    {noreply, State};
%% catch all so we dont loose state
handle_info(Unhandled, State) ->
    format_log(info, "CALLMGR_INBND(~p): unknown info request: ~p~n", [self(), Unhandled]),
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
process_json_req(State, Json) ->
    {struct, Prop} = mochijson2:decode(Json),
    process_req(get_value(<<"action">>, Prop)
		,get_value(<<"category">>, Prop)
		,get_value(<<"type">>, Prop)
		,get_value(<<"command">>, Prop), State, Prop).

%% process_req(Action, Category, Type, Command, State, Prop)
%%
%% process a register Channel_Park Event Request
process_req(<<"request">>, <<"event">>, <<"CHANNEL_PARK">>, <<"register">>,
	    #state{fs=Fs, reg_events=Reged}=State, Prop) ->
    %% see if app has reged with event already
    Resp_queue_id = binary_to_list(get_value(<<"resp_queue_id">>, Prop)),
    RegKey = lists:concat(["CHANNEL_PARK:", Resp_queue_id]),
    Reged1 = case get_value(RegKey, Reged) of
		 undefined ->
		     [{RegKey, Resp_queue_id} | Reged];
		 Resp_queue_id ->
		     Reged;
		 _Alt_queue_id ->
		     [{RegKey, Resp_queue_id} | proplists:delete(RegKey, Reged)]
	     end,

    %% Add listening event to event socket
    freeswitch:send_cmd(Fs#fs_conn.socket, "event plain CHANNEL_PARK\n\n"),
    State#state{reg_events=Reged1};

process_req(Act, Cat, Type, Cmd, State, Prop) ->
    format_log(info, "Unknown Request: A: ~p Cat: ~p T: ~p Cmd: ~p~nRaw Props: ~p~n"
	       ,[Act, Cat, Type, Cmd, Prop]),
    State.

%% Msg should be a Proplist
send_resp(Channel, Ticket, Exchange, Queue, Msg) ->
    {BasicPublish, AmqpMsg} = amqp_util:basic_publish(Ticket
						      ,Exchange
						      ,Queue
						      ,list_to_binary(mochijson2:encode({struct, Msg}))
						      ,<<"application/json">>
						     ),

    %% execute the publish command
    Res = amqp_channel:call(Channel, BasicPublish, AmqpMsg),

    format_log(info, "Sent Resp on Channel ~p~nMsg: ~p~nTo: ~p~nRes: ~p~n"
	       ,[Channel, AmqpMsg, BasicPublish, Res]),
    Res.

process_socket_evt({ok, Headers, Body}) ->
    process_evt(get_value("Content-type", Headers), Body).

process_evt("text/event-plain", Body) ->
    Evt = freeswitch:event_to_proplist(Body),
    callmgr_inbound:event_received(Evt).
