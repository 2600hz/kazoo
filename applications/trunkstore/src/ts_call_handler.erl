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
    monitor_account_doc(Flags),
    loop(CallID, Flags, {AmqpHost, <<>>, EvtQ}, infinity).

-spec(loop/4 :: (CallID :: binary(), Flags :: tuple(), Amqp :: tuple(Host :: string(), CtlQ :: binary(), EvtQ :: binary()), Timeout :: infinity | integer()) -> no_return()).
loop(CallID, Flags, {Host, CtlQ, EvtQ}=Amqp, Timeout) ->
    receive
	{ctl_queue, CallID, CtlQ1} ->
	    ?MODULE:loop(CallID, Flags, {Host, CtlQ1, EvtQ}, Timeout);
	{shutdown, CallID} ->
	    amqp_util:delete_callmgr_queue(Host, CallID),
	    whistle_couch:rm_change_handler(Flags#route_flags.account_doc_id),
	    format_log(info, "TS_CALL(~p): Recv shutdown...~n", [self()]);
	{document_changes, DocID, Changes} ->
	    Doc0 = Flags#route_flags.account_doc,
	    CurrRev = get_value(<<"_rev">>, Doc0),
	    Doc1 = case get_value(<<"rev">>, hd(Changes)) of
		       undefined -> Doc0;
		       CurrRev -> Doc0;
		       _ -> whistle_couch:open_doc(?TS_DB, DocID)
		   end,
	    format_log(info, "TS_CALL(~p): Doc ~p changed from ~p to ~p~n", [self(), DocID, get_value(<<"_rev">>, Doc0), get_value(<<"_rev">>, Doc1)]),
	    ?MODULE:loop(CallID, Flags#route_flags{account_doc=Doc1}, Amqp, Timeout);
	{_, #amqp_msg{props = _Props, payload = Payload}} ->
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

monitor_account_doc(#route_flags{account_doc_id=DocID}) ->
    whistle_couch:add_change_handler(?TS_DB, DocID).

%% Duration - billable seconds
-spec(update_account/2 :: (Duration :: integer(), Flags :: tuple()) -> tuple()).
update_account(Duration, #route_flags{callid=CallID, flat_rate_enabled=true, account_doc=Doc, account_doc_id=DocID, active_calls=ACs}=Flags) ->
    {Acct0} = get_value(<<"account">>, Doc),
    ACs1 = get_value(<<"active_calls">>, Acct0, ACs),

    case lists:member(CallID, ACs1) of
	true ->
	    format_log(info, "TS_CALL(~p): Removing ~p from active ~p~n", [self(), CallID, ACs1]),
	    ACs2 = lists:filter(fun(CallID1) when CallID =:= CallID1 -> false; (_) -> true end, ACs1),
	    Acct1 = [{<<"active_calls">>, ACs2} | proplists:delete(<<"active_calls">>, Acct0)],
	    Doc1 = whistle_couch:add_to_doc(<<"account">>, {Acct1}, Doc),
	    case whistle_couch:save_doc(?TS_DB, Doc1) of
		{ok, Doc2} ->
		    format_log(info, "TS_CALL.up_acct(~p): Old Rev: ~p New Rev: ~p~n", [self(), get_value(<<"_rev">>, Doc), get_value(<<"_rev">>, Doc2)]),
		    Flags#route_flags{account_doc=Doc2};
		{error, conflict} ->
		    format_log(info, "TS_CALL.up_acct(~p): Conflict saving ~p, trying again~n", [self(), DocID]),
		    update_account(Duration, Flags#route_flags{account_doc=whistle_couch:open_doc(?TS_DB, DocID)})
	    end;
	false ->
	    format_log(info, "TS_CALL(~p): Updating trunks not necessary for ~p~n", [self(), CallID]),
	    Flags
    end;
update_account(Duration, #route_flags{flat_rate_enabled=false, direction = <<"inbound">>
					  ,inbound_rate=R, inbound_rate_increment=RI, inbound_rate_minimum=RM, inbound_surcharge=S}=Flags) ->
    Amount = calculate_cost(R, RI, RM, S, Duration),
    Flags#route_flags{account_doc=update_account_balance(Amount, Flags)};
update_account(Duration, #route_flags{flat_rate_enabled=false, direction = <<"outbound">>
					  ,outbound_rate=R, outbound_rate_increment=RI, outbound_rate_minimum=RM, outbound_surcharge=S}=Flags) ->
    Amount = calculate_cost(R, RI, RM, S, Duration),
    Flags#route_flags{account_doc=update_account_balance(Amount, Flags)};
update_account(_Duration, #route_flags{}=Flags) ->
    Flags.

%% only update if the call id is in the list of active calls
-spec(update_account_balance/2 :: (AmountToDeduct :: float(), Flags :: tuple()) -> proplist()).
update_account_balance(AmountToDeduct, #route_flags{account_doc=Doc, callid=CallID, active_calls=ACs}=Flags) ->
    {Acct0} = get_value(<<"account">>, Doc),
    ACs1 = get_value(<<"active_calls">>, Acct0, ACs),

    case lists:member(CallID, ACs1) of
	true ->
	    {Credits0} = get_value(<<"credits">>, Acct0),
	    CA = whistle_util:to_float(get_value(<<"prepay">>, Credits0, 0.0)),
	    format_log(info, "TS_CALL(~p): Deducting ~p from ~p~n", [self(), AmountToDeduct, CA]),

	    Credits1 = [{<<"prepay">>, whistle_util:to_binary(CA - AmountToDeduct)} | proplists:delete(<<"prepay">>, Credits0)],
	    Acct1 = [{<<"credits">>, {Credits1}} | proplists:delete(<<"credits">>, Acct0)],

	    ACs2 = lists:filter(fun(CallID1) when CallID =:= CallID1 -> false; (_) -> true end, ACs1),
	    Acct2 = [{<<"active_calls">>, ACs2} | proplists:delete(<<"active_calls">>, Acct1)],

	    Doc1 = whistle_couch:add_to_doc(<<"account">>, {Acct2}, Doc),
	    case whistle_couch:save_doc(?TS_DB, Doc1) of
		{ok, Doc2} ->
		    format_log(info, "TS_CALL.up_acct_bal(~p): Old Rev: ~p New Rev: ~p~n", [self(), get_value(<<"_rev">>, Doc), get_value(<<"_rev">>, Doc2)]),
		    Doc2;
		{error, conflict} ->
		    format_log(info, "TS_CALL.up_acct_bal(~p): Conflict on ~p, retrying~n", [self(), Flags#route_flags.account_doc_id]),
		    update_account_balance(AmountToDeduct, Flags#route_flags{account_doc=whistle_couch:open_doc(?TS_DB, Flags#route_flags.account_doc_id)})
	    end;
	false ->
	    format_log(info, "TS_CALL.up_acct_bal(~p): No CallID in ActiveCalls; nothing to do~n", [self()]),
	    Doc
    end.

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

wait_to_update(Prop, Flags, Host, CallID, EvtQ) ->
    wait_to_update(Prop, Flags, Host, CallID, EvtQ, erlang:now()).

wait_to_update(Prop, Flags, Host, CallID, EvtQ, Started) ->
    Timeout = ?WAIT_TO_UPDATE_TIMEOUT - (timer:now_diff(erlang:now(), Started) div 1000),
    receive
	{document_changes, DocID, Changes} ->
	    Doc0 = Flags#route_flags.account_doc,
	    CurrRev = get_value(<<"_rev">>, Doc0),
	    Doc1 = case get_value(<<"rev">>, hd(Changes)) of
		       undefined -> Doc0;
		       CurrRev -> Doc0;
		       _ -> whistle_couch:open_doc(?TS_DB, DocID)
		   end,
	    format_log(info, "TS_CALL(~p): Doc ~p changed from ~p to ~p~n", [self(), DocID, get_value(<<"_rev">>, Doc0), get_value(<<"_rev">>, Doc1)]),
	    wait_to_update(Prop, Flags#route_flags{account_doc=Doc1}, Host, CallID, EvtQ, Started)
    after
	Timeout ->
	    close_down(Prop, Flags, Host, CallID, EvtQ)
    end.

close_down(Prop, #route_flags{account_doc_id=DocID}=Flags, Host, CallID, EvtQ) ->
    format_log(info, "TS_CALL.close_down(~p): CDR: ~p~n", [self(), Prop]),
    update_account(whistle_util:to_integer(get_value(<<"Billing-Seconds">>, Prop)), Flags),
    format_log(info, "TS_CALL.close_down(~p): Close down ~p on ~p~n", [CallID, EvtQ, Host]),
    amqp_util:delete_callmgr_queue(Host, EvtQ),
    whistle_couch:rm_change_handler(DocID),
    ts_responder:rm_post_handler(CallID).
