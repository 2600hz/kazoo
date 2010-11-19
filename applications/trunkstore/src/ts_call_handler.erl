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

-export([start/3, init/3, loop/3]).

-import(props, [get_value/2, get_value/3]).
-import(logger, [format_log/3]).

-include("../include/amqp_client/include/amqp_client.hrl").
-include("ts.hrl").

-compile(export_all).

-spec(start/3 :: (CallID :: binary(), Flags :: tuple(), AmqpHost :: string()) -> pid()).
start(CallID, Flags, AmqpHost) ->
    spawn_link(ts_call_handler, init, [CallID, Flags, AmqpHost]).

-spec(init/3 :: (CallID :: binary(), Flags :: tuple(), AmqpHost :: string()) -> no_return()).
init(CallID, Flags, AmqpHost) ->
    format_log(info, "TS_CALL(~p): Starting post handler for ~p...~n", [self(), CallID]),
    consume_events(AmqpHost, CallID),
    monitor_account_doc(Flags),
    loop(CallID, Flags, {AmqpHost, <<>>}).

-spec(loop/3 :: (CallID :: binary(), Flags :: tuple(), Amqp :: tuple(Host :: string(), CtlQ :: binary())) -> no_return()).
loop(CallID, Flags, {Host, _CtlQ}=Amqp) ->
    receive
	{ctl_queue, CallID, CtlQ1} ->
	    ?MODULE:loop(CallID, Flags, {Host, CtlQ1});
	{shutdown, CallID} ->
	    amqp_util:delete_callmgr_queue(Host, CallID),
	    format_log(info, "TS_CALL(~p): Recv shutdown...~n", [self()]);
	{document_changes, DocID, Changes} ->
	    Doc0 = Flags#route_flags.account_doc,
	    CurrRev = get_value(<<"_rev">>, Doc0),
	    Doc1 = case get_value(<<"rev">>, hd(Changes)) of
		       undefined -> Doc0;
		       CurrRev -> Doc0;
		       _ -> ts_couch:open_doc(?TS_DB, DocID)
		   end,
	    format_log(info, "TS_CALL(~p): Doc ~p changed from ~p to ~p~n", [self(), DocID, get_value(<<"_rev">>, Doc0), get_value(<<"_rev">>, Doc1)]),
	    ?MODULE:loop(CallID, Flags#route_flags{account_doc=Doc1}, Amqp);
	{_, #amqp_msg{props = _Props, payload = Payload}} ->
	    {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),

	    case get_value(<<"Event-Name">>, Prop) of
		<<"CHANNEL_DESTROY">> ->
		    format_log(info, "TS_CALL(~p): ChanDestroy recv, shutting down...~n", [self()]),
		    update_account(60, Flags),
		    ts_responder:rm_post_handler(CallID);
		<<"CHANNEL_HANGUP_COMPLETE">> ->
		    format_log(info, "TS_CALL(~p): ChanHangupCompl recv, shutting down...~n", [self()]),
		    update_account(60, Flags),
		    ts_couch:rm_change_handler(get_value(<<"_id">>, Flags#route_flags.account_doc)),
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
    EvtQ = amqp_util:new_callevt_queue(Host, <<>>),
    format_log(info, "TS_CALL(~p): Listening on Q: ~p for call events relating to ~p", [self(), EvtQ, CallID]),
    amqp_util:bind_q_to_callevt(Host, EvtQ, CallID),
    amqp_util:basic_consume(Host, EvtQ).

monitor_account_doc(#route_flags{account_doc=Doc}) ->
    ts_couch:add_change_handler(?TS_DB, get_value(<<"_id">>, Doc)).

%% Duration - billable seconds
-spec(update_account/2 :: (Duration :: integer(), Flags :: tuple()) -> tuple()).
update_account(_Duration, #route_flags{flat_rate_enabled=true, trunks_in_use=TinU, account_doc=Doc}=Flags) ->
    format_log(info, "TS_CALL(~p): Updating trunks in use from ~p to ~p~n", [self(), TinU, TinU-1]),
    {Acct0} = get_value(<<"account">>, Doc),
    Acct1 = [{<<"trunks_in_use">>, whistle_util:to_binary(TinU-1)} | proplists:delete(<<"trunks_in_use">>, Acct0)],
    Doc1 = ts_couch:add_to_doc(<<"account">>, {Acct1}, Doc),
    {ok, Doc2} = ts_couch:save_doc(?TS_DB, Doc1),
    format_log(info, "TS_CALL.up_acct(~p): Old Rev: ~p New Rev: ~p~n", [self(), get_value(<<"_rev">>, Doc), get_value(<<"_rev">>, Doc2)]),
    Flags#route_flags{trunks_in_use=TinU-1, account_doc=Doc2};
update_account(Duration, #route_flags{flat_rate_enabled=false, direction = <<"inbound">>, credit_available=CA
					  ,inbound_rate=R, inbound_rate_increment=RI, inbound_rate_minimum=RM, inbound_surcharge=S}=Flags) ->
    Amount = calculate_cost(R, RI, RM, S, Duration),
    Flags#route_flags{account_doc=update_account_balance(Amount, Flags), credit_available=CA-Amount};
update_account(Duration, #route_flags{flat_rate_enabled=false, direction = <<"outbound">>, credit_available=CA
					  ,outbound_rate=R, outbound_rate_increment=RI, outbound_rate_minimum=RM, outbound_surcharge=S}=Flags) ->
    Amount = calculate_cost(R, RI, RM, S, Duration),
    Flags#route_flags{account_doc=update_account_balance(Amount, Flags), credit_available=CA-Amount};
update_account(_Duration, #route_flags{}=Flags) ->
    Flags.

-spec(update_account_balance/2 :: (AmountToDeduct :: float(), Flags :: tuple()) -> proplist()).
update_account_balance(AmountToDeduct, #route_flags{account_doc=Doc, credit_available=CA}) ->
    format_log(info, "TS_CALL(~p): Deducting ~p from ~p~n", [self(), AmountToDeduct, CA]),
    {Acct0} = get_value(<<"account">>, Doc),
    {Credits0} = get_value(<<"credits">>, Acct0),
    Credits1 = [{<<"prepay">>, whistle_util:to_binary(CA - AmountToDeduct)} | proplists:delete(<<"prepay">>, Credits0)],
    Acct1 = [{<<"credits">>, {Credits1}} | proplists:delete(<<"credits">>, Acct0)],
    Doc1 = ts_couch:add_to_doc(<<"account">>, {Acct1}, Doc),
    {ok, Doc2} = ts_couch:save_doc(?TS_DB, Doc1),
    format_log(info, "TS_CALL.up_acct_bal(~p): Old Rev: ~p New Rev: ~p~n", [self(), get_value(<<"_rev">>, Doc), get_value(<<"_rev">>, Doc2)]),
    Doc2.

%% R :: rate, per minute, in dollars (0.01, 1 cent per minute)
%% RI :: rate increment, in seconds, bill in this increment AFTER rate minimum is taken from Secs
%% RM :: rate minimum, in seconds, minimum number of seconds to bill for
%% Sur :: surcharge, in dollars, (0.05, 5 cents to connect the call)
%% Secs :: billable seconds
-spec(calculate_cost/5 :: (R :: float() | integer(), RI :: integer(), RM :: integer(), Sur :: float() | integer(), Secs :: integer()) -> float()).
calculate_cost(R, RI, RM, Sur, Secs) ->
    case Secs =< RM of
	true -> Sur + ((RM / 60) * R);
	false -> Sur + ((RM / 60) * R) + ( whistle_util:ceiling((Secs - RM) / RI) * ((RI / 60) * R))
    end.
