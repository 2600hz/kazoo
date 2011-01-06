%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc A handler is spawned for each call that makes it through the
%%% routing stage of the trunk store.  There a number of different scenarios
%%% for tracking call progress:
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

-export([start/3, init/3, loop/4]).

-import(props, [get_value/2, get_value/3]).
-import(logger, [format_log/3]).

-include("../include/amqp_client/include/amqp_client.hrl").
-include("ts.hrl").

-define(WAIT_TO_UPDATE_TIMEOUT, 5000). %% 5 seconds
-define(CALL_HEARTBEAT, 10000). %% 10 seconds, how often to query callmgr about call status

-compile(export_all).

-spec(start/3 :: (CallID :: binary(), Flags :: tuple(), AmqpHost :: string()) -> pid()).
start(CallID, Flags, AmqpHost) ->
    spawn_link(ts_call_handler, init, [CallID, Flags, AmqpHost]).

-spec(init/3 :: (CallID :: binary(), Flags :: tuple(), AmqpHost :: string()) -> no_return()).
init(CallID, Flags, AmqpHost) ->
    format_log(info, "TS_CALL(~p): Starting post handler for ~p...~n", [self(), CallID]),
    EvtQ = consume_events(AmqpHost, CallID),
    loop(CallID, Flags, {AmqpHost, <<>>, EvtQ}, infinity).

-spec(loop/4 :: (CallID :: binary(), Flags :: tuple(), Amqp :: tuple(Host :: string(), CtlQ :: binary(), EvtQ :: binary()), Timeout :: infinity | integer()) -> no_return()).
loop(CallID, Flags, {Host, CtlQ, EvtQ}=Amqp, Timeout) ->
    receive
	{ctl_queue, CallID, CtlQ1} ->
	    ?MODULE:loop(CallID, Flags, {Host, CtlQ1, EvtQ}, Timeout);
	{shutdown, CallID} ->
	    amqp_util:delete_callmgr_queue(Host, CallID),
	    ts_responder:rm_post_handler(CallID),
	    format_log(info, "TS_CALL(~p): Recv shutdown...~n", [self()]);
	{_, #amqp_msg{props = _Props, payload = Payload}} ->
	    format_log(info, "TS_CALL(~p): Recv off amqp: ~s~n", [self(), Payload]),

	    {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),

	    case get_value(<<"Event-Name">>, Prop) of
		<<"CHANNEL_DESTROY">> ->
		    format_log(info, "TS_CALL(~p): ChanDestroy recv, shutting down...~n", [self()]),
		    ?MODULE:loop(CallID, Flags, Amqp, ?CALL_HEARTBEAT);
		<<"CHANNEL_HANGUP_COMPLETE">> ->
		    format_log(info, "TS_CALL(~p): ChanHangupCompl recv, shutting down...~n", [self()]),
		    ?MODULE:loop(CallID, Flags, Amqp, ?CALL_HEARTBEAT);
		<<"cdr">> ->
		    case CtlQ of
			<<>> ->
			    spawn(fun() -> wait_to_update(Prop, Flags, Host, CallID, EvtQ) end);
			_ ->
			    close_down(Prop, Flags, Host, CallID, EvtQ)
			end;
		_EvtName ->
		    format_log(info, "TS_CALL(~p): Evt: ~p AppMsg: ~p~n", [self(), _EvtName, get_value(<<"Application-Response">>, Prop)]),
		    ?MODULE:loop(CallID, Flags, Amqp, Timeout)
	    end;
	_Other ->
	    format_log(info, "TS_CALL(~p): Received Other: ~p~n", [self(), _Other]),
	    ?MODULE:loop(CallID, Flags, Amqp, Timeout)
    after
	Timeout ->
	    ts_responder:rm_post_handler(CallID),
	    format_log(info, "TS_CALL(~p): Timeout ~p hit~n", [self(), Timeout])
    end.

-spec(consume_events/2 :: (Host :: string(), CallID :: binary()) -> binary()).
consume_events(Host, CallID) ->
    amqp_util:callevt_exchange(Host),
    EvtQ = amqp_util:new_callevt_queue(Host, <<>>),
    format_log(info, "TS_CALL(~p): Listening on Q: ~p for call events relating to ~p", [self(), EvtQ, CallID]),
    amqp_util:bind_q_to_callevt(Host, EvtQ, CallID, events),
    amqp_util:bind_q_to_callevt(Host, EvtQ, CallID, cdr),
    amqp_util:basic_consume(Host, EvtQ),
    EvtQ.

%% Duration - billable seconds
-spec(update_account/2 :: (Duration :: integer(), Flags :: #route_flags{}) -> #route_flags{}).
update_account(_, #route_flags{account_doc_id = <<>>}=F) -> F;
update_account(_, #route_flags{callid=CallID, flat_rate_enabled=true, account_doc_id=DocID}=Flags) ->
    ts_acctmgr:release_trunk(DocID, CallID),
    Flags;
update_account(Duration, #route_flags{flat_rate_enabled=false, account_doc_id=DocID, callid=CallID
				      ,rate=R, rate_increment=RI, rate_minimum=RM, surcharge=S}=Flags) ->
    Amount = calculate_cost(R, RI, RM, S, Duration),
    ts_acctmgr:deduct_credit(DocID, Amount),
    ts_acctmgr:release_trunk(DocID, CallID),
    Flags;
update_account(_, Flags) ->
    Flags.

%% R :: rate, per minute, in dollars (0.01, 1 cent per minute)
%% RI :: rate increment, in seconds, bill in this increment AFTER rate minimum is taken from Secs
%% RM :: rate minimum, in seconds, minimum number of seconds to bill for
%% Sur :: surcharge, in dollars, (0.05, 5 cents to connect the call)
%% Secs :: billable seconds
-spec(calculate_cost/5 :: (R :: float() | integer(), RI :: integer(), RM :: integer(), Sur :: float() | integer(), Secs :: integer()) -> float()).
calculate_cost(_, _, _, _, 0) -> 0;
calculate_cost(R, RI, RM, Sur, Secs) ->
    case Secs =< RM of
	true -> Sur + ((RM / 60) * R);
	false -> Sur + ((RM / 60) * R) + ( whistle_util:ceiling((Secs - RM) / RI) * ((RI / 60) * R))
    end.

wait_to_update(Prop, Flags, Host, CallID, EvtQ) ->
    receive
    after ?WAIT_TO_UPDATE_TIMEOUT ->
	    close_down(Prop, Flags, Host, CallID, EvtQ)
    end.

close_down(Prop, Flags, Host, CallID, EvtQ) ->
    format_log(info, "TS_CALL.close_down(~p): CDR: ~p~n", [self(), Prop]),
    update_account(whistle_util:to_integer(get_value(<<"Billing-Seconds">>, Prop)), Flags),

    spawn(fun() -> ts_cdr:store_cdr(Prop, Flags) end),

    format_log(info, "TS_CALL.close_down(~p): Close down ~p on ~p~n", [CallID, EvtQ, Host]),

    amqp_util:delete_callmgr_queue(Host, EvtQ),
    ts_responder:rm_post_handler(CallID).
