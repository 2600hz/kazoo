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

-export([start/4, init/4]).

-import(proplists, [get_value/2, get_value/3, delete/2, is_defined/2]).
-import(logger, [log/2, format_log/3]).

-include("../include/amqp_client/include/amqp_client.hrl").

-spec(start/4 :: (CallID :: binary(), Flags :: tuple(), EvtQ :: binary(), CtlQ :: binary()) -> pid()).
start(CallID, Flags, EvtQ, CtlQ) ->
    spawn(ts_call_handler, init, [CallID, Flags, EvtQ, CtlQ]).

-spec(init/4 :: (CaallID :: binary(), Flags :: tuple(), EvtQ :: binary(), CtlQ :: binary()) -> no_return()).
init(CallID, Flags, EvtQ, CtlQ) ->
    {ok, Channel, Ticket} = amqp_manager:open_channel(self()),
    consume_events(Channel, Ticket, EvtQ),
    loop(CallID, Flags, {Channel, Ticket, CtlQ}).

-spec(loop/3 :: (CallID :: binary(), Flags :: tuple(), Amqp :: tuple(Channel :: pid(), Ticket :: integer(), CtlQ :: binary())) -> no_return()).
loop(CallID, Flags, {Channel, Ticket, CtlQ}=Amqp) ->
    receive
	{_, #amqp_msg{props = _Props, payload = Payload}} ->
	    format_log(info, "TS_CALL(~p): Evt recv:~n~s~n", [self(), Payload]),
	    {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),

	    case get_value(<<"Event-Name">>, Prop) of
		<<"CHANNEL_DESTROY">> ->
		    format_log(info, "TS_CALL(~p): ChanDestroy Done~n", [self()]),
		    done;
		_EvtName ->
		    format_log(info, "TS_CALL(~p): Evt: ~p AppMsg: ~p~n", [self(), _EvtName, get_value(<<"Application-Response">>, Prop)])
	    end;
	_Other ->
	    format_log(info, "TS_CALL(~p): Received Other: ~p~n", [self(), _Other])
    end,
    loop(CallID, Flags, Amqp).

-spec(consume_events/3 :: (Channel :: pid(), Ticket :: integer(), EvtQ :: binary()) -> no_return()).
consume_events(Channel, Ticket, EvtQ) ->
    #'basic.consume_ok'{} = amqp_channel:subscribe(Channel, amqp_util:basic_consume(Ticket, EvtQ), self()).
