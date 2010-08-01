%%%-------------------------------------------------------------------
%%% @author James Aimonetti <>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Read from the CallCtlXc.CallId queue
%%% Run Commands on FS socket
%%% @end
%%% Created : 31 Jul 2010 by James Aimonetti <>
%%%-------------------------------------------------------------------
-module(callmgr_ctl).

-export([start_link/2, init/3]).

-import(callmgr_logger, [log/2, format_log/3]).

-include("../include/amqp_client/include/amqp_client.hrl").
-include("../../include/fs.hrl").

-record(state, {fs, callid, channel, ticket, tag, queue}).

start_link(Fs, CallId) ->
    proc_lib:spawn_link(callmgr_ctl, init, [self(), Fs, CallId]).

init(_From, Fs, CallId) ->
    case do_init(Fs, CallId) of
	{ok, State} ->
	    cmd_loop(State);
	{error, Reason} ->
	    exit(Reason)
    end.

do_init(Fs, CallId) ->
    {Channel, Ticket, Tag, Queue} = start_queue(CallId),
    case start_socket(Fs) of
	{error, _Reason}=Err -> Err;
	Fs1 ->
	    format_log(info, "CALLMGR_CTL(~p): Started socket and queue for callId: ~p~n", [self(), CallId]),
	    {ok, #state{channel=Channel, ticket=Ticket, tag=Tag, queue=Queue, fs=Fs1, callid=CallId}}
    end.

start_socket(#fs_conn{host=H, port=P, auth=A}=Fs) ->
    format_log(info, "CALLMGR_CTL(~p): Starting FS Event Socket Listener", [self()]),

    case gen_tcp:connect(H, P, [list, {active, false}]) of
	{ok, FsSock} ->
	    inet:setopts(FsSock, [{packet, line}]),
	    format_log(info, "CALLMGR_CTL(~p): Opened FreeSWITCH event socket to ~p~n", [self(), H]),
	    ok = gen_tcp:send(FsSock, lists:concat(["auth ", A, "\n\n"])),
	    Fs#fs_conn{socket=FsSock};
	{error, Reason}=Err ->
	    format_log(error, "CALLMGR_CTL(~p): Unable to open socket: ~p~n", [self(), Reason]),
	    Err
    end.

start_queue(CallId) ->
    {ok, Channel, Ticket} = amqp_manager:open_channel(self()),
    format_log(info, "CALLMGR_CTL(~p): Channel open to MQ: ~p Ticket: ~p~n", [self(), Channel, Ticket]),

    process_flag(trap_exit, true),

    Exchange = amqp_util:callctl_exchange(Ticket),
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Exchange),
    format_log(info, "CALLMGR_CTL(~p): Accessing Exchange ~p~n", [self(), Exchange]),

    QueueDeclare = amqp_util:new_callctl_queue(Ticket, CallId),
    #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, QueueDeclare),

    %% Bind the queue to an exchange
    QueueBind = amqp_util:bind_q_to_callctl(Ticket, Queue, Queue),
    #'queue.bind_ok'{} = amqp_channel:call(Channel, QueueBind),
    format_log(info, "CALLMGR_CTL(~p) Bound ~p to ~p~n", [self(), Queue, "callctlXc"]),

    %% Register a consumer to listen to the queue
    BasicConsume = amqp_util:callctl_consume(Ticket, CallId),
    #'basic.consume_ok'{consumer_tag = Tag}
        = amqp_channel:subscribe(Channel, BasicConsume, self()),

    %% Channel, Ticket, Tag, Queue
    {Channel, Ticket, Tag, Queue}.

cmd_loop(#state{fs=Fs}=State) ->
    receive
	{#'basic.deliver'{}, #amqp_msg{props = Prop, payload = Payload}} ->
	    format_log(info, "CALLMGR_CTL(~p): Recv Header: ~p~nPayload: ~p~n", [self(), Prop, binary_to_list(Payload)]),
	    run_cmd(Fs#fs_conn.socket, prepare_payload(Payload)),
	    cmd_loop(State);
	Other ->
	    format_log(info, "CALLMGR_CTL(~p): Recv Other: ~p~n", [self(), Other]),
	    cmd_loop(State)
    end.

prepare_payload(Payload) ->
    lists:concat([mochijson2:decode(binary_to_list(Payload)), "\n\n"]).

run_cmd(Socket, Cmd) ->
    format_log(info, "CALLMGR_CTL(~p): Run Cmd ||~p||~n", [self(), Cmd]),
    ok = gen_tcp:send(Socket, Cmd).
