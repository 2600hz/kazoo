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

-import(logger, [log/2, format_log/3]).

-include("../include/amqp_client/include/amqp_client.hrl").
-include("../../include/fs.hrl").

-record(state, {fs, callid, channel, ticket, tag, queue}).

start_link(Fs, CallId) ->
    proc_lib:start_link(callmgr_ctl, init, [self(), Fs, CallId]).

init(From, Fs, CallId) ->
    case do_init(CallId, Fs) of
	{ok, State} ->
	    proc_lib:init_ack(From, self()),
	    cmd_loop(State);
	{error, Reason} ->
	    exit(Reason)
    end.

do_init(CallId, Fs) ->
    {Channel, Ticket, Tag, Queue} = start_queue(CallId),
    {ok, #state{channel=Channel, ticket=Ticket, tag=Tag, queue=Queue, callid=CallId, fs=start_socket(Fs)}}.

start_socket(Fs) ->
    %format_log(info, "CALLMGR_CTL(~p): Starting FS Event Socket Listener", [self()]),
    freeswitch:new_fs_socket(Fs).

start_queue(CallId) ->
    {ok, Channel, Ticket} = amqp_manager:open_channel(self()),
    %format_log(info, "CALLMGR_CTL(~p): Channel open to MQ: ~p Ticket: ~p~n", [self(), Channel, Ticket]),

    process_flag(trap_exit, true),

    Exchange = amqp_util:callctl_exchange(Ticket),
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Exchange),
    %format_log(info, "CALLMGR_CTL(~p): Accessing Exchange ~p~n", [self(), Exchange]),

    QueueDeclare = amqp_util:new_callctl_queue(Ticket, CallId),
    #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, QueueDeclare),

    %% Bind the queue to an exchange
    QueueBind = amqp_util:bind_q_to_callctl(Ticket, Queue, Queue),
    #'queue.bind_ok'{} = amqp_channel:call(Channel, QueueBind),
    %format_log(info, "CALLMGR_CTL(~p) Bound ~p to ~p~n", [self(), Queue, "callctlXc"]),

    %% Register a consumer to listen to the queue
    BasicConsume = amqp_util:callctl_consume(Ticket, CallId),
    #'basic.consume_ok'{consumer_tag = Tag}
        = amqp_channel:subscribe(Channel, BasicConsume, self()),
    %format_log(info, "CALLMGR_CTL(~p): BC: ~p at ~p~n", [self(), BasicConsume, erlang:now()]),

    %% Channel, Ticket, Tag, Queue
    {Channel, Ticket, Tag, Queue}.

cmd_loop(#state{fs=Fs}=State) ->
    receive
	close ->
	    stop(State);
	{#'basic.deliver'{}, #amqp_msg{props = Prop, payload = Payload}} ->
	    %format_log(info, "CALLMGR_CTL(~p): Recv Header: ~p~nPayload: ~p~n", [self(), Prop, binary_to_list(Payload)]),
	    Fs1 = process_msg(Fs, Prop#'P_basic'.content_type, Payload),
	    cmd_loop(State#state{fs=Fs1});
	Other ->
	    format_log(info, "CALLMGR_CTL(~p): Recv Other: ~p~n", [self(), Other]),
	    cmd_loop(State)
    end.

stop(#state{fs=Fs, ticket=Ticket, channel=Channel, queue=Queue}) ->
    gen_tcp:close(Fs#fs_conn.socket),

    amqp_manager:close_channel(self()),

    io:format("CALLMGR_CTL(~p): Stopping~n", [self()]),
    stopped.

process_msg(Fs, <<"application/json">>, Payload) ->
    {struct, Msg} = mochijson2:decode(binary_to_list(Payload)),
    Cmd = lists:flatten([proplists:get_value(<<"execute">>, Msg), "\n\n"]),
    run_cmd(Fs, Cmd).

run_cmd(#fs_conn{socket=Socket}=Fs, Cmd) ->
    format_log(info, "CALLMGR_CTL(~p): Run Cmd ||~p||~n", [self(), Cmd]),
    ok = gen_tcp:send(Socket, Cmd),
    freeswitch:clear_socket(Socket),
    Fs.
