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

-export([start_link/2, init/3]).

-import(callmgr_logger, [log/2, format_log/3]).
-import(proplists, [get_value/2, get_value/3]).

-include("../include/amqp_client/include/amqp_client.hrl").
-include("../../include/fs.hrl").

-record(state, {fs, callid, channel, ticket, tag, queue}).

start_link(Fs, CallId) ->
    proc_lib:spawn_link(callmgr_evt, init, [self(), Fs, CallId]).

init(_From, Fs, CallId) ->
    case do_init(Fs, CallId) of
	{ok, State} ->
	    evt_loop(State, [], 0);
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

start_socket(#fs_conn{host=H, port=P, auth=A}=Fs, CallId) ->
    %format_log(info, "CALLMGR_EVT(~p): Starting FS Event Socket Listener", [self()]),

    case gen_tcp:connect(H, P, [list, {active, false}]) of
	{ok, FsSock} ->
	    inet:setopts(FsSock, [{packet, line}]),
	    %format_log(info, "CALLMGR_EVT(~p): Opened FreeSWITCH event socket to ~p~n", [self(), H]),
	    ok = gen_tcp:send(FsSock, lists:concat(["auth ", A, "\n\n"])),
	    ok = gen_tcp:send(FsSock, "events plain all\n\n"),
	    ok = gen_tcp:send(FsSock, lists:concat(["filter Unique-ID ", CallId, "\n\n"])),
	    Fs#fs_conn{socket=FsSock};
	{error, _Reason}=Err ->
	    %format_log(error, "CALLMGR_EVT(~p): Unable to open socket: ~p~n", [self(), _Reason]),
	    Err
    end.

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

evt_loop(#state{fs=(#fs_conn{socket=Socket}), callid=CallId}=State, Headers, ContentLength) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, "\n"} ->
            %% End of message. Parse
	    %%log(info, ["COMPLETE HEADER: ", Headers]),

            case ContentLength > 0 of
		true ->
		    inet:setopts(Socket, [{packet, raw}]),
		    {ok, Body} = gen_tcp:recv(Socket, ContentLength),
		    inet:setopts(Socket, [{packet, line}]),
		    Body1 = string:strip(Body, right, $\n),
		    %%%format_log(info, "CALLMGR_EVT(~p): Headers: ~p~nEvt: ~p~n", [self(), Headers, Body1]),
		    process_evt(proplists:get_value("Content-Type", Headers), Body1, State);
		false ->
		    ok
	    end,
	    evt_loop(State, [], 0);
        {ok, Data} ->
	    %% Parse the line
	    KV = split(Data),
	    {K, V} = KV,

            %% Is this a content-length string? If so, we'll need to gather extra data later
            case K =:= "Content-Length" of
		true ->
		    Length = list_to_integer(V);
		false ->
		    Length = ContentLength
            end,

            evt_loop(State, [KV | Headers], Length);
        {error, closed} ->
            case do_init(State#state.fs, CallId) of
		{error, Error} -> exit(Error);
		{ok, State1} -> evt_loop(State1, 0, [])
	    end
    end.

%% Takes K: V\n and returns {K, V}
split(Data) ->
    [K | V] = string:tokens(Data, ": "),
    V1 = case length(V) of
	     0 -> "";
	     1 -> string:strip(hd(V), right, $\n);
	     _ -> lists:map(fun(S) -> string:strip(S, right, $\n) end, V)
	 end,
    {K, V1}.

process_evt("text/event-plain", EvtStr, State) ->
    send_event(event_to_proplist(EvtStr), State);
process_evt(CT, _, _) ->
    format_log(info, "CALLMGR_EVT(~p): Unhandled content-type: ~p~n", [self(), CT]).

event_to_proplist(Str) ->
    L = string:tokens(Str, "\n"),
    lists:map(fun(S) -> [K, V0] = string:tokens(S, ":"),
			V1 = string:strip(V0, left, $ ),
			{V2, []} = mochiweb_util:parse_qs_value(V1),
			{K, V2}
	      end, L).

send_event(Prop, State) ->
    send_event(get_value(<<"Event-Name">>, Prop), Prop, State).

send_event(_, Prop, State) ->
    Msg = [{event_name, get_value("Event-Name", Prop)}],
    send_event_to_queue(Msg, State).

send_event_to_queue(Msg, #state{channel=Channel, ticket=Ticket, queue=Queue}) ->
    {BasicPublish, AmqpMsg} = amqp_util:callevt_publish(Ticket
							,Queue
							,list_to_binary(mochijson2:encode({struct, Msg}))
							,<<"application/json">>
						       ),

    %% execute the publish command
    Res = amqp_channel:call(Channel, BasicPublish, AmqpMsg),

    %format_log(info, "CALLMGR_EVT(~p): Sent Evt to Channel ~p~nEvt: ~p~nTo: ~p~nRes: ~p~n"
    %,[self(), Channel, AmqpMsg, BasicPublish, Res]),
    Res.
