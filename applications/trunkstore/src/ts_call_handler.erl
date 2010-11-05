%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% A handler is spawned for each call that makes it through the routing
%%% stage of the trunk store.
%%% There a number of different scenarios for tracking call progress:
%%%
%%% 1) Outbound Calls
%%%  a) When all the routes have been processed and none have been bridged,
%%%     we have a total network failure and somehow need to let our admins
%%%     know things are not kosher.
%%%  b) A route is bridged; we need to monitor the call progress, noting
%%%     whether it was a flat-rate call or not. If flat-rate, when the call
%%%     finishes, update available trunks accordingly. If the call is per-min
%%%     we need to track duration and probably compute cost against available
%%%     credit, perhaps hanging the call up should they overrun their funds.
%%% 2) Inbound calls
%%%  a) If initial route bridges successfully, track call progress (much like
%%%     1b above). If the route fails to bridge and failover is not config-
%%%     ured, play a sound file about the number being temp. out of service.
%%%  b) If routing fails, but a failover is configured, an outbound leg
%%%     needs to be run through ts_route to find the routing information.
%%%     Will probably put straight into a ts_route:outbound_handler call
%%%     to lookup DID and find a route, updating flags with outbound rate
%%%     costs (since inbound rates will be set already). Probably set up
%%%     a second ts_call_handler to track the outbound leg.
%%%
%%% At the end of a channel's life, format the flag record, and any in-call
%%% data (or failure notices should the call not succeed), sending the
%%% compiled report to appropriate report-receiving places (Couch or another
%%% process, perhaps).
%%% @end
%%% Created :  1 Oct 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ts_call_handler).

-export([start/3, init/3, loop/3]).


-import(props, [get_value/2, get_value/3]).
-import(logger, [format_log/3]).

-include("../include/amqp_client/include/amqp_client.hrl").

-spec(start/3 :: (CallID :: binary(), Flags :: tuple(), AmqpHost :: string()) -> pid()).
start(CallID, Flags, AmqpHost) ->
    spawn_link(ts_call_handler, init, [CallID, Flags, AmqpHost]).

-spec(init/3 :: (CallID :: binary(), Flags :: tuple(), AmqpHost :: string()) -> no_return()).
init(CallID, Flags, AmqpHost) ->
    format_log(info, "TS_CALL(~p): Starting post handler for ~p...~n", [self(), CallID]),
    consume_events(AmqpHost, CallID),
    loop(CallID, Flags, {AmqpHost, <<>>}).

-spec(loop/3 :: (CallID :: binary(), Flags :: tuple(), Amqp :: tuple(Host :: string(), CtlQ :: binary())) -> no_return()).
loop(CallID, Flags, {Host, _CtlQ}=Amqp) ->
    receive
	{ctl_queue, CallID, CtlQ1} ->
	    ?MODULE:loop(CallID, Flags, {Host, CtlQ1});
	{shutdown, CallID} ->
	    amqp_util:delete_callmgr_queue(Host, CallID),
	    format_log(info, "TS_CALL(~p): Recv shutdown...~n", [self()]);
	{_, #amqp_msg{props = _Props, payload = Payload}} ->
	    format_log(info, "TS_CALL(~p): Evt recv:~n~s~n", [self(), Payload]),
	    {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),

	    case get_value(<<"Event-Name">>, Prop) of
		<<"CHANNEL_DESTROY">> ->
		    format_log(info, "TS_CALL(~p): ChanDestroy recv, shutting down...~n", [self()]),
		    ts_responder:rm_post_handler(CallID);
		<<"CHANNEL_HANGUP">> ->
		    format_log(info, "TS_CALL(~p): ChanHangup recv, shutting down...~n", [self()]),
		    ts_responder:rm_post_handler(CallID);
		_EvtName ->
		    format_log(info, "TS_CALL(~p): Evt: ~p AppMsg: ~p~n", [self(), _EvtName, get_value(<<"Application-Response">>, Prop)]),
		    ?MODULE:loop(CallID, Flags, Amqp)
	    end;
	_Other ->
	    format_log(info, "TS_CALL(~p): Received Other: ~p~n", [self(), _Other]),
	    ?MODULE:loop(CallID, Flags, Amqp)
    end.

-spec(consume_events/2 :: (Host :: string(), CallID :: binary()) -> no_return()).
consume_events(Host, CallID) ->
    amqp_util:callevt_exchange(Host),
    EvtQ = amqp_util:new_callevt_queue(Host, list_to_binary(["ts_call.event.", CallID])),
    amqp_util:bind_q_to_callevt(Host, EvtQ, CallID),
    amqp_util:basic_consume(Host, EvtQ).
