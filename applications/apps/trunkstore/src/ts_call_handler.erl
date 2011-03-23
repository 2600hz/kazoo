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

-behaviour(gen_server).

%% API
-export([start_link/3, start_link/4, get_queue/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(props, [get_value/2, get_value/3]).
-import(logger, [format_log/3]).

-include("ts.hrl").

-define(CALL_ACTIVITY_TIMEOUT, 2 * 60 * 1000). %% 2 mins to check call status

-record(state, {callid = <<>> :: binary()
		,amqp_q = {error, undefined} :: binary() | tuple(error, term())
		,amqp_h = "" :: string()
		,is_amqp_up = true :: boolean()
		,ctl_q = <<>> :: binary() %% the control queue for the call, if we won the route_resp race
		,start_time = 0 :: integer() %% the timestamp of when this process started
		,call_activity_ref = undefined :: undefined | reference()
		,call_status = up :: up | down
                ,route_flags = #route_flags{} :: #route_flags{}
		,leg_number = 1 :: integer() %% a-leg is 1, b-leg is 2, each transfer increments leg number
	       }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(CallID, AmqpHost, RouteFlags) ->
    gen_server:start_link(?MODULE, [CallID, AmqpHost, RouteFlags, 1], []).

start_link(CallID, AmqpHost, RouteFlags, LegNumber) ->
    gen_server:start_link(?MODULE, [CallID, AmqpHost, RouteFlags, LegNumber+1], []).

%% get_queue() -> Queue Name
-spec(get_queue/1 :: (Pid :: pid()) -> tuple(ok, binary())).
get_queue(Pid) ->
    gen_server:call(Pid, get_queue).

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
init([CallID, AmqpHost, RouteFlags, LegNumber]) ->
    process_flag(trap_exit, true),

    {ok, #state{callid = CallID
		,amqp_q = get_amqp_queue(AmqpHost, CallID)
		,amqp_h = AmqpHost
		,route_flags = RouteFlags
		,start_time = ts_util:current_tstamp()
		,leg_number = LegNumber
	       }}.

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
handle_call(get_queue, _, #state{amqp_q=Q}=S) ->
    {reply, {ok, Q}, S}.

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
handle_info(timeout, #state{callid=CallID, amqp_q=Q}=S) when not is_binary(Q) ->
    H = whapps_controller:get_amqp_host(),
    NewQ = get_amqp_queue(H, CallID, Q),

    {noreply, S#state{amqp_h=H, amqp_q=NewQ}, 1000};

handle_info({amqp_host_down, H}, #state{is_amqp_up=true}=State) ->
    format_log(info, "TS_CALL(~p): AmqpHost ~s went down~n", [self(), H]),
    timer:send_after(1000, self(), is_amqp_up),
    {noreply, State#state{amqp_q={error, amqp_host_down}, is_amqp_up=false}};

handle_info(is_amqp_up, #state{callid=CallID, amqp_q={error, _}, is_amqp_up=false}=State) ->
    H = whapps_controller:get_amqp_host(),
    NewQ = get_amqp_queue(H, CallID),
    case is_binary(NewQ) of
	true ->
	    {noreply, State#state{amqp_q = NewQ, is_amqp_up = true}};
	false ->
	    timer:send_after(1000, self(), is_amqp_up),
	    {noreply, State}
    end;

handle_info({timeout, Ref, call_activity_timeout}, #state{call_status=down, call_activity_ref=Ref}=S) ->
    stop_call_activity_ref(Ref),
    format_log(info, "TS_CALL(~p): No status_resp received; assuming call is down and we missed it.~n", [self()]),
    {stop, shutdown, S};
handle_info({timeout, Ref, call_activity_timeout}, #state{call_activity_ref=Ref, amqp_q=Q, amqp_h=H, callid=CallID}=S) when is_binary(Q) ->
    format_log(info, "TS_CALL(~p): Haven't heard from the event stream for a bit, need to check in~n", [self()]),
    stop_call_activity_ref(Ref),

    Prop = [{<<"Call-ID">>, CallID} | whistle_api:default_headers(Q, <<"call_event">>, <<"status_req">>, <<"ts_call_handler">>, <<"0.5.3">>)],
    case whistle_api:call_status_req(Prop) of
	{ok, JSON} ->
	    amqp_util_old:callevt_publish(H, CallID, JSON, status_req);
	{error, E} ->
	    format_log(error, "TS_CALL(~p): sending status_req failed: ~p~n", [self(), E])
    end,
    {noreply, S#state{call_activity_ref=call_activity_ref(), call_status=down}};
handle_info(#'basic.consume_ok'{}, #state{call_activity_ref=Ref}=S) ->
    stop_call_activity_ref(Ref),
    {noreply, S#state{call_activity_ref=call_activity_ref()}};
handle_info({_, #amqp_msg{props = _Props, payload = Payload}}, #state{route_flags=Flags, call_activity_ref=Ref}=S) ->
    format_log(info, "TS_CALL(~p): Recv off amqp: ~s~n", [self(), Payload]),
    stop_call_activity_ref(Ref),

    JObj = mochijson2:decode(binary_to_list(Payload)),

    case whapps_json:get_value(<<"Event-Name">>, JObj) of
	<<"cdr">> ->
	    spawn(fun() ->
			  format_log(info, "TS_CALL(~p): Scenario(~p) for ~p~n", [self(), Flags#route_flags.scenario, Flags#route_flags.callid]),
			  true = whistle_api:call_cdr_v(JObj),
			  close_down_call(JObj, Flags, S#state.leg_number),
			  ts_cdr:store_cdr(JObj, Flags)
		  end),
	    {stop, normal, S};
	<<"route_win">> ->
	    true = whistle_api:route_win_v(JObj),
	    format_log(info, "TS_CALL(~p): route win received~n~p~n", [self(), JObj]),
	    {noreply, S#state{ctl_q=whapps_json:get_value(<<"Control-Queue">>, JObj), call_activity_ref=call_activity_ref(), call_status=up}};
	<<"CHANNEL_BRIDGE">> ->
	    true = whistle_api:call_event_v(JObj),
	    OtherCallID = whapps_json:get_value(<<"Other-Leg-Unique-ID">>, JObj),
	    OtherAcctID = Flags#route_flags.diverted_account_doc_id,
	    AcctID = Flags#route_flags.account_doc_id, % don't lose the old account, in case of a failover route

	    %% if an outbound was re-routed as inbound, diverted_account_doc_id won't be <<>>; otherwise, when the CDR is received, nothing really happens

	    %% try to reserve a trunk for this leg
	    ts_acctmgr:release_trunk(OtherAcctID, Flags#route_flags.callid),
	    ts_acctmgr:reserve_trunk(OtherAcctID, OtherCallID),
	    ts_call_sup:start_proc([OtherCallID
				    ,whapps_controller:get_amqp_host()
				    ,Flags#route_flags{account_doc_id=OtherAcctID
						       ,callid = OtherCallID
						       ,diverted_account_doc_id=AcctID
						       ,direction = <<"inbound">>
						      }
				    ,S#state.leg_number]),
	    format_log(info, "TS_CALL(~p): Bridging to ~s~n", [self(), OtherCallID]),
	    {noreply, S#state{call_activity_ref=call_activity_ref(), call_status=up}};
	<<"status_resp">> ->
	    true = whistle_api:call_status_resp_v(JObj),
	    format_log(info, "TS_CALL(~p): Call is active, despite appearances~n", [self()]),
	    {noreply, S#state{call_activity_ref=call_activity_ref(), call_status=up}};
	_EvtName ->
	    format_log(info, "TS_CALL(~p): Evt: ~p~n", [self(), _EvtName]),
	    {noreply, S#state{call_activity_ref=call_activity_ref(), call_status=up}}
    end;
handle_info(_Info, S) ->
    format_log(error, "TS_CALL(~p): Unhandled info: ~p~n", [self(), _Info]),
    {noreply, S}.

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
terminate(shutdown, #state{route_flags=Flags, amqp_h=H, amqp_q=Q, start_time=StartT, call_activity_ref=Ref}) ->
    stop_call_activity_ref(Ref),
    Duration = get_call_duration([], Flags, StartT),
    format_log(error, "TS_CALL(~p): terminating via shutdown, releasing trunk and billing for ~p seconds"
	       ,[self(), Duration]),
    update_account(Duration, Flags), % charge for minimmum seconds since we apparently messed up
    amqp_util_old:delete_queue(H, Q),
    ok;
terminate(normal, #state{amqp_h=H, amqp_q=Q, start_time=StartTime, call_activity_ref=Ref}) ->
    stop_call_activity_ref(Ref),
    DeltaTime = ts_util:current_tstamp() - StartTime, % one second calls in case the call isn't connected but we have a delay knowing it
    format_log(error, "TS_CALL(~p): terminating normally: took ~p~n", [self(), DeltaTime]),
    amqp_util_old:delete_queue(H, Q),
    ok;
terminate(_Unexpected, #state{amqp_h=H, amqp_q=Q, start_time=StartTime, call_activity_ref=Ref}) ->
    stop_call_activity_ref(Ref),
    DeltaTime = ts_util:current_tstamp() - StartTime, % one second calls in case the call isn't connected but we have a delay knowing it
    format_log(error, "TS_CALL(~p): terminating unexpectedly: took ~p~n~p~n", [self(), DeltaTime, _Unexpected]),
    amqp_util_old:delete_queue(H, Q),
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
-spec(get_amqp_queue/2 :: (AmqpHost :: string(), CallID :: binary()) -> binary()).
get_amqp_queue(AmqpHost, CallID) ->
    get_amqp_queue(AmqpHost, CallID, <<>>).

get_amqp_queue(AmqpHost, CallID, Q) ->
    EvtQ = amqp_util_old:new_callevt_queue(AmqpHost, <<>>),
    format_log(info, "TS_CALL(~p): Listening on Q: ~p for call events relating to ~p~n", [self(), EvtQ, CallID]),

    amqp_util_old:bind_q_to_callevt(AmqpHost, EvtQ, CallID, events),
    amqp_util_old:bind_q_to_callevt(AmqpHost, EvtQ, CallID, cdr),
    amqp_util_old:bind_q_to_targeted(AmqpHost, EvtQ),

    amqp_util_old:basic_consume(AmqpHost, EvtQ),
    amqp_util_old:delete_queue(AmqpHost, Q),
    EvtQ.

%% Duration - billable seconds
-spec(update_account/2 :: (Duration :: integer(), Flags :: #route_flags{}) -> no_return()).
update_account(_, #route_flags{callid=CallID, flat_rate_enabled=true, account_doc_id=DocID}) ->
    ts_acctmgr:release_trunk(DocID, CallID);
update_account(Duration, #route_flags{flat_rate_enabled=false, account_doc_id=DocID, callid=CallID
				      ,rate=R, rate_increment=RI, rate_minimum=RM, surcharge=S
				     }) ->
    Amount = calculate_cost(R, RI, RM, S, Duration),
    ts_acctmgr:release_trunk(DocID, CallID, Amount).

%% R :: rate, per minute, in dollars (0.01, 1 cent per minute)
%% RI :: rate increment, in seconds, bill in this increment AFTER rate minimum is taken from Secs
%% RM :: rate minimum, in seconds, minimum number of seconds to bill for
%% Sur :: surcharge, in dollars, (0.05, 5 cents to connect the call)
%% Secs :: billable seconds
-spec(calculate_cost/5 :: (R :: float() | integer(), RI :: integer(), RM :: integer(), Sur :: float() | integer(), Secs :: integer()) -> float()).
calculate_cost(_, _, _, _, 0) -> 0.0;
calculate_cost(R, 0, RM, Sur, Secs) -> calculate_cost(R, 60, RM, Sur, Secs);
calculate_cost(R, RI, RM, Sur, Secs) ->
    case Secs =< RM of
	true -> Sur + ((RM / 60) * R);
	false -> Sur + ((RM / 60) * R) + ( whistle_util:ceiling((Secs - RM) / RI) * ((RI / 60) * R))
    end.

call_activity_ref() ->
    erlang:start_timer(?CALL_ACTIVITY_TIMEOUT, self(), call_activity_timeout).

stop_call_activity_ref(undefined) ->
    ok;
stop_call_activity_ref(Ref) ->
    erlang:cancel_timer(Ref).

%% Close down the A-Leg of a call
close_down_call(JObj, #route_flags{diverted_account_doc_id = <<>>}=Flags, 1) ->
    Duration = get_call_duration(JObj, Flags),
    update_account(Duration, Flags);
close_down_call(JObj, #route_flags{diverted_account_doc_id = Acct2ID, callid=CallID}=Flags, 1) ->
    Duration = get_call_duration(JObj, Flags),
    update_account(Duration, Flags),

    %% Because the call may have never bridged, we need to go ahead and clear this second trunk
    %% If it did bridge, ts_acctmgr will just error when the B-leg ts_call_handler tries to clear the trunk
    CCVs = whapps_json:get_value(<<"Custom-Channel-Vars">>, JObj),

    {R, RI, RM, S} = get_rate_data(CCVs, Flags),

    Cost = calculate_cost(R, RI, RM, S, Duration),
    ts_acctmgr:release_trunk(Acct2ID, CallID, Cost),
    ts_acctmgr:release_trunk(Acct2ID, <<CallID/binary, "-failover">>, Cost);
close_down_call(_JObj, #route_flags{scenario=inbound}, _LegNo) ->
    ok; %% a-leg takes care of it all, nothing to do
close_down_call(_JObj, #route_flags{scenario=outbound}, _LegNo) ->
    ok; %% a-leg takes care of it all, nothing to do
close_down_call(JObj, #route_flags{scenario=inbound_failover, diverted_account_doc_id=AAcctID}=Flags, _LegNo) ->
    ACallID = whapps_json:get_value([<<"Other-Leg-Call-ID">>], JObj),
    FailCallID = <<ACallID/binary, "-failover">>, % A-leg

    CCVs = whapps_json:get_value(<<"Custom-Channel-Vars">>, JObj),

    case whistle_util:to_boolean(whapps_json:get_value(<<"Failover-Route">>, CCVs, false)) of
	false -> %% inbound route bridged
	    ts_acctmgr:release_trunk(AAcctID, FailCallID, 0);
	true -> %% failover route bridged
	    {R, RI, RM, S} = get_rate_data(CCVs, Flags),
	    Duration = get_call_duration(JObj, Flags),

	    ts_acctmgr:release_trunk(AAcctID, FailCallID, calculate_cost(R, RI, RM, S, Duration))
    end;
close_down_call(JObj, #route_flags{scenario=outbound_inbound, account_doc_id=Acct2ID}=Flags, _LegNo) ->
    BCallID = whapps_json:get_value([<<"Call-ID">>], JObj),
    ACallID = whapps_json:get_value([<<"Other-Leg-Call-ID">>], JObj),

    CCVs = whapps_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    {R, RI, RM, S} = get_rate_data(CCVs, Flags),

    Duration = get_call_duration(JObj, Flags),

    %% copy acct2's A-leg trunk reservation to a B-leg trunk reservation
    ts_acctmgr:copy_reserve_trunk(Acct2ID, ACallID, BCallID, (R * RM + S)),
    ts_acctmgr:release_trunk(Acct2ID, ACallID, 0),
    ts_acctmgr:release_trunk(Acct2ID, BCallID, calculate_cost(R, RI, RM, S, Duration));

close_down_call(JObj, #route_flags{scenario=outbound_inbound_failover, account_doc_id=Acct2ID}=Flags, _LegNumber) ->
    BCallID = whapps_json:get_value([<<"Call-ID">>], JObj),
    ACallID = whapps_json:get_value([<<"Other-Leg-Call-ID">>], JObj),

    CCVs = whapps_json:get_value(<<"Custom-Channel-Vars">>, JObj),

    {R, RI, RM, S} = get_rate_data(CCVs, Flags),

    IsFailoverRoute = whistle_util:to_boolean(whapps_json:get_value(<<"Failover-Route">>, CCVs, false)),

    Duration = get_call_duration(JObj, Flags),

    ts_acctmgr:release_trunk(Acct2ID, ACallID, 0),
    ts_acctmgr:release_trunk(Acct2ID, <<ACallID/binary, "-failover">>, 0),

    case IsFailoverRoute of
	false -> %% inbound route bridged
	    ts_acctmgr:copy_reserve_trunk(Acct2ID, ACallID, BCallID, (R*RM+S));
	true ->
	    ts_acctmgr:copy_reserve_trunk(Acct2ID, <<ACallID/binary, "-failover">>, BCallID, (R*RM+S))
    end,
    ts_acctmgr:release_trunk(Acct2ID, BCallID, calculate_cost(R, RI, RM, S, Duration)).

-spec(get_rate_data/2 :: (CCVs :: json_object(), Flags :: #route_flags{}) -> tuple(float(), integer(), integer(), float())).
get_rate_data({struct, _}=CCVs, #route_flags{}=Flags) ->
    {
      whistle_util:to_float(whapps_json:get_value(<<"Rate">>, CCVs, Flags#route_flags.rate))
      ,whistle_util:to_integer(whapps_json:get_value(<<"Rate-Increment">>, CCVs, Flags#route_flags.rate_increment))
      ,whistle_util:to_integer(whapps_json:get_value(<<"Rate-Minimum">>, CCVs, Flags#route_flags.rate_minimum))
      ,whistle_util:to_float(whapps_json:get_value(<<"Surcharge">>, CCVs, Flags#route_flags.surcharge))
    }.

-spec(get_call_duration/2 :: (JObj :: json_object(), Flags :: #route_flags{}) -> integer()).
get_call_duration(JObj, Flags) ->
    get_call_duration(JObj, Flags, 0).

-spec(get_call_duration/3 :: (JObj :: json_object(), Flags :: #route_flags{}, StartTime :: integer()) -> integer()).
get_call_duration(JObj, #route_flags{rate_minimum=RM}, StartTime) ->
    Guess = case StartTime > 0 andalso (ts_util:current_tstamp() - StartTime) of
		false -> RM;
		%% if no duration from JObj, and elapsed time is > twice the timeout
		X when X >= (2 * ?CALL_ACTIVITY_TIMEOUT) -> X - (2 * ?CALL_ACTIVITY_TIMEOUT);
		Y when Y < RM -> RM;
		Z -> Z
	    end,
    format_log(info, "TS_CALL(~p): get_call_d: BillSecs: ~p, StartTime: ~p, Guess: ~p~n", [self(), whapps_json:get_value(<<"Billing-Seconds">>, JObj), StartTime, Guess]),
    whistle_util:to_integer(whapps_json:get_value(<<"Billing-Seconds">>, JObj, Guess)).
