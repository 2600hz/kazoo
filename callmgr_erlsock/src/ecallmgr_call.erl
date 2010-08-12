%%%-------------------------------------------------------------------
%%% @author James Aimonetti <>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Handle a call's lifecycle
%%% @end
%%% Created : 11 Aug 2010 by James Aimonetti <>
%%%-------------------------------------------------------------------
-module(ecallmgr_call).

-behaviour(gen_server).

-include("../include/amqp_client/include/amqp_client.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(logger, [log/2, format_log/3]).
-import(proplists, [get_value/2, get_value/3]).

-define(SERVER, ?MODULE).

-record(amqp_state, {channel, ticket, evt_queue, ctl_queue}).
-record(state, {callid, amqp=#amqp_state{}, ref}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% (setup) -> create an a-leg and park it
%% (setup_loopback) -> create a loopback call and park it
%% ({callid, CallId}) -> take over processing for a call
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link({Ref, _Init}=Args) when is_reference(Ref) ->
    gen_server:start_link(?MODULE, [Args], []);
start_link(_Args) ->
    format_log(error, "ECALLMGR_CALL(~p): start_link failing with args ~p~n", [self(), _Args]).

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
init([{Ref, [{setup, Opts}]}]) ->
    format_log(info, "ECALLMGR_CALL(~p): Init.setup with ~p~n", [self(), Opts]),
    Cmd = case get_value(<<"effective_caller_id_name">>, Opts) of
	      undefined -> "";
	      Name -> lists:concat(["{effective_caller_id_name=", Name, "}"])
	  end,
    Cmd1 = case get_value(<<"effective_caller_id_number">>, Opts) of
	       undefined -> Cmd;
	       Num -> lists:concat([Cmd, "{effective_caller_id_number=", Num, "}"])
	   end,
    Cmd2 = case get_value(<<"call_string">>, Opts) of
	       undefined -> Cmd1;
	       Call -> lists:concat([Cmd1, Call]) % sofia/gateway/proxy1.switchfreedom.com/+14158867905
	   end,
    Cmd3 = case get_value(<<"park">>, Opts) of
	       undefined -> Cmd2;
	       _Park -> lists:concat([Cmd2, " &park"])
	   end,
    format_log(info, "ECALLMGR_CALL(~p): Init.setup Cmd ~p~n", [self(), Cmd3]),
    {ok, [$+,$O,$K,$ | Callid]} = freeswitch:api(?FS_NODE, originate, Cmd3),
    CallId = string:strip(Callid, right, $\n),

    {ok, Amqp} = start_queues(CallId),

    send_callid(CallId, Amqp, Opts),

    {ok, #state{callid=CallId, ref=Ref, amqp=Amqp}};
init([{Ref, [{setup_loopback, Opts}]}]) ->
    format_log(info, "ECALLMGR_CALL(~p): Init.setup_loopback FS node ~p~n", [self(), ?FS_NODE]),
    {ok, [$+,$O,$K,$ | Callid]} = freeswitch:api(?FS_NODE, originate, "loopback/wait &park"),
    CallId = string:strip(Callid, right, $\n),
    format_log(info, "ECALLMGR_CALL(~p): Init.setup_loopback CallId: ~p~n", [self(), CallId]),
    {ok, Amqp} = start_queues(CallId),

    send_callid(CallId, Amqp, Opts),
    format_log(info, "ECALLMGR_CALL(~p): Init.setup_loopback done~n", [self()]),

    {ok, #state{callid=CallId, ref=Ref, amqp=Amqp}};
init([{Ref, [{callid, CallId, Opts}]}]) ->
    format_log(info, "ECALLMGR_CALL(~p): Init.callid with ~p~n", [self(), CallId]),
    {ok, Amqp} = start_queues(CallId),
    send_callid(CallId, Amqp, Opts),
    {ok, #state{callid=CallId, ref=Ref, amqp=Amqp}};
init(_Args) ->
    format_log(error, "ECALLMGR_CALL(~p): Init: Unknown Args ~p~n", [self(), _Args]),
    {error, unknown_args}.

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
%% start_queues(CallId) -> {ok, #amqp_state{}}.
start_queues(CallId) ->
    {ok, Channel, Ticket} = amqp_manager:open_channel(self()),

    EvtQ = get_evt_queue(CallId, Channel, Ticket),
    CtlQ = get_ctl_queue(CallId, Channel, Ticket),
    {ok, #amqp_state{channel=Channel, ticket=Ticket, evt_queue=EvtQ, ctl_queue=CtlQ}}.

get_evt_queue(CallId, Channel, Ticket) ->
    EvtExchange = amqp_util:callevt_exchange(Ticket),
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, EvtExchange),

    EvtQueueDeclare = amqp_util:new_callevt_queue(Ticket, CallId),
    #'queue.declare_ok'{queue = EvtQueue} = amqp_channel:call(Channel, EvtQueueDeclare),

    %% Bind the queue to an exchange
    EvtQueueBind = amqp_util:bind_q_to_callevt(Ticket, EvtQueue, EvtQueue),
    #'queue.bind_ok'{} = amqp_channel:call(Channel, EvtQueueBind),
    EvtQueue.

get_ctl_queue(CallId, Channel, Ticket) ->
    CtlExchange = amqp_util:callctl_exchange(Ticket),
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, CtlExchange),

    CtlQueueDeclare = amqp_util:new_callctl_queue(Ticket, CallId),
    #'queue.declare_ok'{queue = CtlQueue} = amqp_channel:call(Channel, CtlQueueDeclare),

    %% Bind the queue to an exchange
    CtlQueueBind = amqp_util:bind_q_to_callctl(Ticket, CtlQueue, CtlQueue),
    #'queue.bind_ok'{} = amqp_channel:call(Channel, CtlQueueBind),

    %% Register a consumer to listen to the queue
    CtlBasicConsume = amqp_util:callctl_consume(Ticket, CallId),
    #'basic.consume_ok'{} = amqp_channel:subscribe(Channel, CtlBasicConsume, self()),
    CtlQueue.

send_callid(CallId, Amqp, Prop) ->
    case get_value(<<"respond_to">>, Prop) of
	undefined ->
	    case get_value(<<"resp_queue_id">>, Prop) of
		undefined -> {error, no_response_queue};
		Q -> publish_msg(Amqp, callid_response(CallId, Prop), Q)
	    end;
	Q -> publish_msg(Amqp, callid_response(CallId, Prop), Q)
    end.

%% publish to the evt queue
publish_msg(#amqp_state{channel=Channel, ticket=Ticket, evt_queue=Q}, Msg) ->
    publish_msg(Channel, Ticket, Q, Msg).
%% publish to arbitrary queue
publish_msg(#amqp_state{channel=Channel, ticket=Ticket}, Msg, Q) ->
    publish_msg(Channel, Ticket, Q, Msg).

publish_msg(Channel, Ticket, Queue, Msg) ->
    {BasicPublish, AmqpMsg} = amqp_util:callevt_publish(Ticket
							,Queue
							,list_to_binary(mochijson2:encode({struct, Msg}))
							,<<"application/json">>
						       ),
    %% execute the publish command
    amqp_channel:call(Channel, BasicPublish, AmqpMsg).

callid_response(CallId, Prop) ->
    [{request_id, get_value(<<"request_id">>, Prop)}
     ,{app, ecallmgr_call}
     ,{action, response}
     ,{category, resource}
     ,{type, channel}
     ,{command, get_value(<<"command">>, Prop)}
     ,{callid, CallId}
    ].
