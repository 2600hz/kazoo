%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Startup Queue on TargetedXc
%%% Receive requests to spin up a call
%%% Ask callmgr_fs to spin up call
%%% Receive callid from callmgr_fs
%%% Create two queues:
%%%   callctl.callid - apps send cmds to FS via this queue
%%%   callevt.callid - apps receive updates from FS via this queue
%%% Create two processes to manage the queues
%%% Send requester callid
%%% @end
%%% Created : 31 Jul 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(callmgr_req).

-behaviour(gen_server).

-include("../include/amqp_client/include/amqp_client.hrl").

-import(logger, [log/2, format_log/3]).
-import(proplists, [get_value/2, get_value/3]).

%% API
-export([start_link/0, recv_callid/2, get_callids_served/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(EXCHANGE, <<"targeted">>).

-record(state, {channel, connection, ticket, queue, tag, callids=0}).

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

recv_callid(CallId, Prop) ->
    gen_server:cast(?MODULE, {recv_callid, CallId, Prop}).

get_callids_served() ->
    gen_server:call(?MODULE, get_callids_served).
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
    format_log(info, "CALLMGR_REQ(~p): Channel open to MQ: ~p Ticket: ~p~n", [self(), Channel, Ticket]),

    process_flag(trap_exit, true),

    Exchange = amqp_util:targeted_exchange(Ticket),
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Exchange),
    format_log(info, "CALLMGR_REQ: Accessing Exchange ~p~n", [Exchange]),

    Queue = list_to_binary([?EXCHANGE, ".callmgr." | net_adm:localhost()]),
    QueueDeclare = amqp_util:new_queue(Ticket, Queue),
    #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, QueueDeclare),

    %% Bind the queue to an exchange
    QueueBind = amqp_util:bind_q_to_targeted(Ticket, Queue, Queue),
    #'queue.bind_ok'{} = amqp_channel:call(Channel, QueueBind),
    format_log(info, "CALLMGR_REQ Bound ~p to ~p~n", [Queue, ?EXCHANGE]),

    %% Register a consumer to listen to the queue
    BasicConsume = amqp_util:basic_consume(Ticket, Queue),
    #'basic.consume_ok'{consumer_tag = Tag}
        = amqp_channel:subscribe(Channel, BasicConsume, self()),

    {ok, #state{channel=Channel, ticket=Ticket, tag=Tag, queue=Queue}}.

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
handle_call(get_callids_served, _From, #state{callids=N}=State) ->
    {reply, N, State};
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
handle_cast({recv_callid, CallId, Prop}, #state{callids=N}=State) ->
    format_log(info, "CALLMGR_REQ: Recv CallId: ~p for ~p~n", [CallId, get_value(<<"resp_queue_id">>, Prop)]),
    send_callid(State, Prop, CallId),
    {noreply, State#state{callids=N+1}};
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
%% cleanly exit if we have been asked to exit
handle_info({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State};
%% receive resource requests from Apps
handle_info({_, #amqp_msg{props = Props, payload = Payload}}, State) ->
    format_log(info, "CALLMGR_REQ: Info Headers: ~p~n", [Props]),
    case Props#'P_basic'.content_type of
	<<"text/xml">> ->
	    log(info, xml),
	    %%notify(State#state.consumers, Props, xmerl_scan:string(binary_to_list(Payload)));
	    State;
	<<"text/plain">> ->
	    log(info, text),
	    %%notify(State#state.consumers, Props, binary_to_list(Payload));
	    State;
	<<"application/json">> ->
	    {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
	    format_log(info, "CALLMGR_REQ JSON: ~p~n", [Prop]),
	    process_req(get_route(Prop), Prop),
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
	    format_log(info, "~p recieved unknown msg type: ~p~n", [self(), _ContentType]),
	    State
    end,
    {noreply, State};
%% catch all so we dont loose state
handle_info(Unhandled, State) ->
    format_log(info, "CALLMGR_REQ ~p unknown info request: ~p~n", [self(), Unhandled]),
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

%% process_req({Action, Category, Type, Command}, Prop)
process_req({<<"request">>
	     ,<<"resource">>
	     ,<<"channel">>
	     ,<<"setup_loopback">>}, Prop) ->
    callmgr_fs:originate_loopback(Prop);
process_req({<<"request">>
	     ,<<"resource">>
	     ,<<"channel">>
	     ,<<"setup">>}, Prop) ->
    {struct, CallInfo} = get_value(<<"call">>, Prop),
    case get_value(<<"from">>, CallInfo, ["NoName", 0]) of
	[Name, Num] ->
	    callmgr_fs:originate(Name, Num, Prop);
	Num ->
	    callmgr_fs:originate("NoName", Num, Prop)
    end.

get_route(Prop) ->
    {get_value(<<"action">>, Prop)
     ,get_value(<<"category">>, Prop)
     ,get_value(<<"type">>, Prop)
     ,get_value(<<"command">>, Prop)
    }.

send_callid(#state{channel=Channel, ticket=Ticket}, Prop, Callid) ->
    Exchange = get_value(<<"resp_exchange_id">>, Prop, <<"targeted">>),
    Queue = get_value(<<"resp_queue_id">>, Prop),

    Msg = [{request_id, get_value(<<"request_id">>, Prop)}
	   ,{app, callmgr_req}
	   ,{action, response}
	   ,{category, resource}
	   ,{type, channel}
	   ,{command, setup_loopback}
	   ,{callid, Callid}
	  ],

    {BasicPublish, AmqpMsg} = amqp_util:basic_publish(Ticket
						      ,Exchange
						      ,Queue
						      ,list_to_binary(mochijson2:encode({struct, Msg}))
						      ,<<"application/json">>
						     ),

    %% execute the publish command
    amqp_channel:call(Channel, BasicPublish, AmqpMsg).
