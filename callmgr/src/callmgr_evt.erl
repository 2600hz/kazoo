%%%-------------------------------------------------------------------
%%% @author James Aimonetti <>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Read CallId-related events from the FS event socket
%%% Put relevant data onto the CallEvtXc.CallId queue
%%% @end
%%% Created : 31 Jul 2010 by James Aimonetti <>
%%%-------------------------------------------------------------------
-module(callmgr_evt).

-export([start_link/3, init/4]).

-import(logger, [log/2, format_log/3]).
-import(proplists, [get_value/2, get_value/3]).

-include("../include/amqp_client/include/amqp_client.hrl").
-include("../../include/fs.hrl").

-record(state, {fs, callid, channel, ticket, tag, queue, ctl_pid}).

start_link(Fs, CallId, CtlPid) ->
    proc_lib:spawn_link(callmgr_evt, init, [self(), Fs, CallId, CtlPid]).

init(_From, Fs, CallId, CtlPid) ->
    case do_init(Fs, CallId) of
	{ok, State} ->
	    evt_loop(State#state{ctl_pid=CtlPid});
	{error, Reason} ->
	    exit(Reason)
    end.

do_init(Fs, CallId) ->
    case start_socket(Fs, CallId) of
	{error, _Reason}=Err -> Err;
	Fs1 ->
	    {Channel, Ticket, Tag, Queue} = start_queue(CallId),
	    %format_log(info, "CALLMGR_EVT(~p): Started socket and queue for callId: ~p~n", [self(), CallId]),
	    {ok, #state{channel=Channel, ticket=Ticket, tag=Tag, queue=Queue, fs=Fs1, callid=CallId}}
    end.

start_socket(#fs_conn{socket=undefined}=Fs, CallId) ->
    Fs1 = freeswitch:new_fs_socket(Fs),
    format_log(info, "CALLMGR_EVT(~p): Starting FS Event Socket Listener on Port: ~p~n", [self(), Fs1#fs_conn.socket]),
    filter_events(Fs1#fs_conn.socket, CallId),
    Fs1;
start_socket(Fs, CallId) ->
    Fs1 = freeswitch:new_fs_socket(Fs),
    format_log(info, "CALLMGR_EVT(~p): Starting FS Event Socket Listener on Port: ~p (was ~p)~n"
	       ,[self(), Fs1#fs_conn.socket, Fs#fs_conn.socket]),
    filter_events(Fs1#fs_conn.socket, CallId),
    Fs1.

start_queue(CallId) ->
    {ok, Channel, Ticket} = amqp_manager:open_channel(self()),
    %format_log(info, "CALLMGR_EVT(~p): Channel open to MQ: ~p Ticket: ~p~n", [self(), Channel, Ticket]),

    process_flag(trap_exit, true),

    Exchange = amqp_util:callevt_exchange(Ticket),
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Exchange),
    %format_log(info, "CALLMGR_EVT(~p): Accessing Exchange ~p~n", [self(), Exchange]),

    QueueDeclare = amqp_util:new_callevt_queue(Ticket, CallId),
    #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, QueueDeclare),
    %format_log(info, "CALLMGR_EVT(~p): Declared Q: ~p~nQD: ~p~n", [self(), Queue, QueueDeclare]),

    %% Bind the queue to an exchange
    QueueBind = amqp_util:bind_q_to_callevt(Ticket, Queue, Queue),
    #'queue.bind_ok'{} = amqp_channel:call(Channel, QueueBind),
    %format_log(info, "CALLMGR_EVT(~p) Bound ~p to ~p~n", [self(), Queue, "callevtXc"]),

    %% Channel, Ticket, Tag, Queue -> Tag currently unused
    {Channel, Ticket, undefined, Queue}.

evt_loop(#state{fs=(#fs_conn{socket=Socket})=Fs, callid=CallId}=State) ->
    case freeswitch:read_socket(Socket, 5000) of
	{ok, H, B} ->
	    B1 = string:strip(B, right, $\n),
	    process_evt(proplists:get_value("Content-Type", H), B1, State),
	    am_i_up(State);
	{error, timeout} ->
	    am_i_up(State);
	{error, Reason} ->
	    case start_socket(Fs, CallId) of
		{error, Reason1} ->
		    stop(State),
		    throw({Reason, Reason1});
		Fs1 ->
		    am_i_up(State#state{fs=Fs1})
	    end
    end.

process_evt("text/event-plain", EvtStr, State) ->
    Evt = freeswitch:event_to_proplist(EvtStr),
    send_event(Evt, State);
process_evt("api/response", EvtStr, State) ->
    am_i_up_resp(EvtStr, State);
process_evt(CT, _, _) ->
    format_log(info, "CALLMGR_EVT(~p): Unhandled content-type: ~p~n", [self(), CT]).

send_event(Prop, State) ->
    send_event(get_value(<<"Event-Name">>, Prop), Prop, State).

send_event(_, Prop, State) ->
    EvtName = get_value("Event-Name", Prop),
    Msg = [{app, callmgr_evt}
	   ,{action, response}
	   ,{category, event}
	   ,{type, EvtName}
	   ,{command, report}
	   ,{event_name, EvtName}
	   ,{callid, get_value("Unique-ID", Prop)}
	  ],
    send_event_to_queue(Msg, State).

send_event_to_queue(Msg, #state{channel=Channel, ticket=Ticket, queue=Queue}) ->
    {BasicPublish, AmqpMsg} = amqp_util:callevt_publish(Ticket
							,Queue
							,list_to_binary(mochijson2:encode({struct, Msg}))
							,<<"application/json">>
						       ),
    %% execute the publish command
    amqp_channel:call(Channel, BasicPublish, AmqpMsg).

am_i_up(State) ->
    receive
	{'EXIT', _Pid, normal} ->
	    %% FS.UUID exiting, ignore
	    am_i_up_socket(State);
	{'EXIT', _Pid, _Reason} ->
	    format_log(error, "CALLMGR_EVT(~p): Exit caught from ~p: ~p~n", [self(), _Pid, _Reason]),
	    stop(State)
    after 100 ->
	    am_i_up_socket(State)
    end.

am_i_up_socket(#state{fs=Fs, callid=CallId}=State) ->
    case gen_tcp:send(Fs#fs_conn.socket, lists:flatten(["api uuid_exists ", CallId, "\n\n"])) of
	ok ->
	    evt_loop(State);
	{error, closed} ->
	    stop(State)
    end.

am_i_up_resp("false", State) ->
    stop(State);
am_i_up_resp(_, State) ->
    evt_loop(State).

stop(#state{fs=Fs, ctl_pid=Pid}) ->
    gen_tcp:close(Fs#fs_conn.socket),

    amqp_manager:close_channel(self()),

    Pid ! close,
    format_log(info, "CALLMGR_EVT(~p): Stopping, sent close to ~p~n", [self(), Pid]),
    stopped.

filter_events(FsSock, CallId) ->
    ok = gen_tcp:send(FsSock, "events plain all\n\n"),
    freeswitch:clear_socket(FsSock),
    ok = gen_tcp:send(FsSock, lists:concat(["filter Unique-ID ", CallId, "\n\n"])),
    freeswitch:clear_socket(FsSock).
