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
-export([start_link/2, start_link/3, get_queue/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ts.hrl").

-define(CALL_ACTIVITY_TIMEOUT, 2 * 60 * 1000). %% 2 mins to check call status
-define(BILLING_TIMEOUT, 2 * ?CALL_ACTIVITY_TIMEOUT div 1000).

-record(state, {callid = <<>> :: binary()
		,amqp_q = {error, undefined} :: binary() | tuple(error, term())
		,is_amqp_up = true :: boolean()
		,ctl_q = <<>> :: binary() %% the control queue for the call, if we won the route_resp race
		,start_time = 0 :: integer() %% the timestamp of when this process started
		,call_activity_ref = undefined :: undefined | reference()
		,call_status = up :: up | down
                ,route_flags = #route_flags{} :: #route_flags{}
		,leg_number = 1 :: integer() %% a-leg is 1, b-leg is 2, each transfer increments leg number
                ,todays_db = <<>> :: binary()
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
start_link(CallID, RouteFlags) ->
    gen_server:start_link(?MODULE, [CallID, RouteFlags, 1], []).

start_link(CallID, RouteFlags, LegNumber) ->
    gen_server:start_link(?MODULE, [CallID, RouteFlags, LegNumber+1], []).

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
init([CallID, RouteFlags, LegNumber]) ->
    process_flag(trap_exit, true),

    {ok, #state{callid = CallID
		,route_flags = RouteFlags
		,start_time = whistle_util:current_tstamp()
		,leg_number = LegNumber
		,todays_db = ts_util:todays_db_name(?TS_CDR_PREFIX)
	       }, 0}.

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
handle_info(timeout, #state{callid=CallID, amqp_q=Q, todays_db=DB}=S) when not is_binary(Q) ->
    NewQ = get_amqp_queue(CallID),
    couch_mgr:db_create(DB),
    {noreply, S#state{amqp_q=NewQ}, 1000};

handle_info({amqp_host_down, H}, #state{is_amqp_up=true}=State) ->
    logger:debug("~p.~p(~p): AMQP Host ~s went down", [?MODULE, ?LINE, self(), H]),
    {ok, _} = timer:send_after(1000, self(), is_amqp_up),
    {noreply, State#state{amqp_q={error, amqp_host_down}, is_amqp_up=false}};

handle_info(is_amqp_up, #state{callid=CallID, amqp_q={error, _}, is_amqp_up=false}=State) ->
    NewQ = get_amqp_queue(CallID),
    case is_binary(NewQ) of
	true ->
	    {noreply, State#state{amqp_q = NewQ, is_amqp_up = true}};
	false ->
	    {ok, _} = timer:send_after(1000, self(), is_amqp_up),
	    {noreply, State}
    end;

handle_info({timeout, Ref, call_activity_timeout}, #state{callid=CallID, call_status=down, call_activity_ref=Ref}=S) ->
    _ = stop_call_activity_ref(Ref),
    logger:debug("~s | Log | ~p.~p(~p): Call activity timed out, assuming we're down", [CallID, ?MODULE, ?LINE, self()]),
    {stop, shutdown, S};
handle_info({timeout, Ref, call_activity_timeout}, #state{call_activity_ref=Ref, amqp_q=Q, callid=CallID}=S) when is_binary(Q) ->
    logger:debug("~s | Log | ~p.~p(~p): Event stream quiet for too long, checking in", [CallID, ?MODULE, ?LINE, self()]),
    _ = stop_call_activity_ref(Ref),

    Prop = [{<<"Call-ID">>, CallID} | whistle_api:default_headers(Q, <<"call_event">>, <<"status_req">>, <<"ts_call_handler">>, <<"0.5.3">>)],
    case whistle_api:call_status_req(Prop) of
	{ok, JSON} ->
	    amqp_util:callevt_publish(CallID, JSON, status_req);
	{error, E} ->
	    logger:err("~s | Log | ~p.~p(~p): Error validating status request: ~p", [CallID, ?MODULE, ?LINE, self(), E])
    end,
    {noreply, S#state{call_activity_ref=call_activity_ref(), call_status=down}};
handle_info(#'basic.consume_ok'{}, #state{call_activity_ref=Ref}=S) ->
    _ = stop_call_activity_ref(Ref),
    {noreply, S#state{call_activity_ref=call_activity_ref()}};
handle_info({_, #amqp_msg{props = _Props, payload = Payload}}, #state{callid=CallID, route_flags=Flags, call_activity_ref=Ref, leg_number=LegNo, todays_db=DB}=S) ->
    _ = stop_call_activity_ref(Ref),

    JObj = mochijson2:decode(binary_to_list(Payload)),

    case wh_json:get_value(<<"Event-Name">>, JObj) of
	<<"cdr">> ->
	    Self = self(),
	    spawn(fun() ->
			  logger:info("~s | Log | ~p.~p(~p): CDR received: ~s", [CallID, ?MODULE, ?LINE, Self, Payload]),
			  true = whistle_api:call_cdr_v(JObj),
			  close_down_call(JObj, Flags, LegNo),
			  ts_cdr:store_cdr(JObj, Flags, DB)
		  end),
	    {stop, normal, S};
	<<"route_win">> ->
	    true = whistle_api:route_win_v(JObj),
	    logger:info("~s | Log | ~p.~p(~p): Route won: ~s", [CallID, ?MODULE, ?LINE, self(), Payload]),
	    {noreply, S#state{ctl_q=wh_json:get_value(<<"Control-Queue">>, JObj), call_activity_ref=call_activity_ref(), call_status=up}};
	<<"CHANNEL_BRIDGE">> ->
	    true = whistle_api:call_event_v(JObj),
	    OtherCallID = wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj),
	    logger:info("~s | Log | ~p.~p(~p): Bridging to ~s: ~s", [CallID, ?MODULE, ?LINE, self(), OtherCallID, Payload]),
	    OtherAcctID = Flags#route_flags.diverted_account_doc_id,
	    AcctID = Flags#route_flags.account_doc_id, % don't lose the old account, in case of a failover route

	    %% if an outbound was re-routed as inbound, diverted_account_doc_id won't be <<>>; otherwise, when the CDR is received, nothing really happens

	    %% try to reserve a trunk for this leg
	    _ = ts_acctmgr:release_trunk(OtherAcctID, Flags#route_flags.callid, 0),
	    _ = ts_acctmgr:reserve_trunk(OtherAcctID, OtherCallID, (Flags#route_flags.rate * Flags#route_flags.rate_minimum + Flags#route_flags.surcharge)
				     ,Flags#route_flags.flat_rate_enabled),
	    _ = ts_call_sup:start_proc([OtherCallID
				    ,Flags#route_flags{account_doc_id=OtherAcctID
						       ,callid = OtherCallID
						       ,diverted_account_doc_id=AcctID
						       ,direction = <<"inbound">>
						      }
				    ,LegNo]),
	    {noreply, S#state{call_activity_ref=call_activity_ref(), call_status=up}};
	<<"status_resp">> ->
	    true = whistle_api:call_status_resp_v(JObj),
	    logger:info("~s | Log | ~p.~p(~p): Status Response received: ~s", [CallID, ?MODULE, ?LINE, self(), Payload]),
	    {noreply, S#state{call_activity_ref=call_activity_ref(), call_status=up}};
	_EvtName ->
	    logger:err("~s | Log | ~p.~p(~p): Unknown event name: ~s", [CallID, ?MODULE, ?LINE, self(), Payload]),
	    {noreply, S#state{call_activity_ref=call_activity_ref(), call_status=up}}
    end;
handle_info(_Info, #state{callid=CallID}=S) ->
    logger:info("~s | Log | ~p.~p(~p): Unhandled message: ~p", [CallID, ?MODULE, ?LINE, self(), _Info]),
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
terminate(shutdown, #state{callid=CallID, route_flags=Flags, start_time=StartT, call_activity_ref=Ref}) ->
    _ = stop_call_activity_ref(Ref),
    Duration = get_call_duration(?EMPTY_JSON_OBJECT, Flags, StartT),
    logger:info("~s | Log | ~p.~p(~p): Call ending via shutdown, billing for ~p", [CallID, ?MODULE, ?LINE, self(), Duration]),
    update_account(Duration, Flags); % charge for minimmum seconds since we apparently messed up
terminate(normal, #state{callid=CallID, start_time=StartTime, call_activity_ref=Ref}) ->
    _ = stop_call_activity_ref(Ref),
    DeltaTime = whistle_util:current_tstamp() - StartTime, % one second calls in case the call isn't connected but we have a delay knowing it
    logger:info("~s | Log | ~p.~p(~p): Terminating normally, call handler lasted ~p seconds", [CallID, ?MODULE, ?LINE, self(), DeltaTime]);
terminate(_Unexpected, #state{callid=CallID, start_time=StartTime, call_activity_ref=Ref}) ->
    _ = stop_call_activity_ref(Ref),
    DeltaTime = whistle_util:current_tstamp() - StartTime, % one second calls in case the call isn't connected but we have a delay knowing it
    logger:info("~s | Log | ~p.~p(~p): Terminating ~p, call handler lasted ~p seconds", [CallID, ?MODULE, ?LINE, self(), _Unexpected, DeltaTime]).

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
-spec(get_amqp_queue/1 :: (CallID :: binary()) -> binary() | tuple(error, term())).
get_amqp_queue(CallID) ->
    EvtQ = amqp_util:new_callevt_queue(<<>>),

    amqp_util:bind_q_to_callevt(EvtQ, CallID, events),
    amqp_util:bind_q_to_callevt(EvtQ, CallID, cdr),
    amqp_util:bind_q_to_targeted(EvtQ),

    amqp_util:basic_consume(EvtQ),
    EvtQ.

%% Duration - billable seconds
-spec(update_account/2 :: (Duration :: integer(), Flags :: #route_flags{}) -> no_return()).
update_account(_, #route_flags{callid=CallID, flat_rate_enabled=true, account_doc_id=DocID}) ->
    ts_acctmgr:release_trunk(DocID, CallID, 0);
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

-spec(call_activity_ref/0 :: () -> reference()).
call_activity_ref() ->
    erlang:start_timer(?CALL_ACTIVITY_TIMEOUT, self(), call_activity_timeout).

-spec(stop_call_activity_ref/1 :: (Ref :: undefined | reference()) -> ok | integer() | false).
stop_call_activity_ref(undefined) ->
    ok;
stop_call_activity_ref(Ref) ->
    erlang:cancel_timer(Ref).

%% Close down the A-Leg of a call
-spec(close_down_call/3 :: (JObj :: json_object(), Flags :: #route_flags{}, LegNo :: integer()) -> no_return()).
close_down_call(JObj, #route_flags{diverted_account_doc_id = <<>>}=Flags, 1) ->
    Duration = get_call_duration(JObj, Flags),
    update_account(Duration, Flags);
close_down_call(JObj, #route_flags{diverted_account_doc_id = Acct2ID, callid=CallID}=Flags, 1) ->
    Duration = get_call_duration(JObj, Flags),
    update_account(Duration, Flags),

    %% Because the call may have never bridged, we need to go ahead and clear this second trunk
    %% If it did bridge, ts_acctmgr will just error when the B-leg ts_call_handler tries to clear the trunk
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),

    {R, RI, RM, S} = get_rate_data(CCVs, Flags),

    Cost = calculate_cost(R, RI, RM, S, Duration),
    ts_acctmgr:release_trunk(Acct2ID, CallID, Cost),
    ts_acctmgr:release_trunk(Acct2ID, <<CallID/binary, "-failover">>, Cost);
close_down_call(_JObj, #route_flags{scenario=inbound}, _LegNo) ->
    ok; %% a-leg takes care of it all, nothing to do
close_down_call(_JObj, #route_flags{scenario=outbound}, _LegNo) ->
    ok; %% a-leg takes care of it all, nothing to do
close_down_call(JObj, #route_flags{scenario=inbound_failover, diverted_account_doc_id=AAcctID}=Flags, _LegNo) ->
    ACallID = wh_json:get_value([<<"Other-Leg-Call-ID">>], JObj),
    FailCallID = <<ACallID/binary, "-failover">>, % A-leg

    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),

    case whistle_util:to_boolean(wh_json:get_value(<<"Failover-Route">>, CCVs, false)) of
	false -> %% inbound route bridged
	    ts_acctmgr:release_trunk(AAcctID, FailCallID, 0);
	true -> %% failover route bridged
	    {R, RI, RM, S} = get_rate_data(CCVs, Flags),
	    Duration = get_call_duration(JObj, Flags),

	    ts_acctmgr:release_trunk(AAcctID, FailCallID, calculate_cost(R, RI, RM, S, Duration))
    end;
close_down_call(JObj, #route_flags{scenario=outbound_inbound, account_doc_id=Acct2ID}=Flags, _LegNo) ->
    BCallID = wh_json:get_value([<<"Call-ID">>], JObj),
    ACallID = wh_json:get_value([<<"Other-Leg-Call-ID">>], JObj),

    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    {R, RI, RM, S} = get_rate_data(CCVs, Flags),

    Duration = get_call_duration(JObj, Flags),

    %% copy acct2's A-leg trunk reservation to a B-leg trunk reservation
    ts_acctmgr:copy_reserve_trunk(Acct2ID, ACallID, BCallID, (R * RM + S)),
    _ = ts_acctmgr:release_trunk(Acct2ID, ACallID, 0),
    ts_acctmgr:release_trunk(Acct2ID, BCallID, calculate_cost(R, RI, RM, S, Duration));

close_down_call(JObj, #route_flags{scenario=outbound_inbound_failover, account_doc_id=Acct2ID}=Flags, _LegNumber) ->
    BCallID = wh_json:get_value([<<"Call-ID">>], JObj),
    ACallID = wh_json:get_value([<<"Other-Leg-Call-ID">>], JObj),

    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),

    {R, RI, RM, S} = get_rate_data(CCVs, Flags),

    IsFailoverRoute = whistle_util:to_boolean(wh_json:get_value(<<"Failover-Route">>, CCVs, false)),

    Duration = get_call_duration(JObj, Flags),

    _ = ts_acctmgr:release_trunk(Acct2ID, ACallID, 0),
    _ = ts_acctmgr:release_trunk(Acct2ID, <<ACallID/binary, "-failover">>, 0),

    case IsFailoverRoute of
	false -> %% inbound route bridged
	    ts_acctmgr:copy_reserve_trunk(Acct2ID, ACallID, BCallID, (R*RM+S));
	true ->
	    ts_acctmgr:copy_reserve_trunk(Acct2ID, <<ACallID/binary, "-failover">>, BCallID, (R*RM+S))
    end,
    ts_acctmgr:release_trunk(Acct2ID, BCallID, calculate_cost(R, RI, RM, S, Duration)).

-spec(get_rate_data/2 :: (CCVs :: json_object(), Flags :: #route_flags{}) -> tuple(float(), integer(), integer(), float())).
get_rate_data(CCVs, #route_flags{rate=R, rate_increment=RI, rate_minimum=RM, surcharge=S}) ->
    {
      whistle_util:to_float(wh_json:get_value(<<"Rate">>, CCVs, R))
      ,whistle_util:to_integer(wh_json:get_value(<<"Rate-Increment">>, CCVs, RI))
      ,whistle_util:to_integer(wh_json:get_value(<<"Rate-Minimum">>, CCVs, RM))
      ,whistle_util:to_float(wh_json:get_value(<<"Surcharge">>, CCVs, S))
    }.

-spec(get_call_duration/2 :: (JObj :: json_object(), Flags :: #route_flags{}) -> integer()).
get_call_duration(JObj, Flags) ->
    get_call_duration(JObj, Flags, 0).

-spec(get_call_duration/3 :: (JObj :: json_object(), Flags :: #route_flags{}, StartTime :: integer()) -> integer()).
get_call_duration(JObj, #route_flags{rate_minimum=RM}, StartTime) ->
    Now = whistle_util:current_tstamp(),
    Guess = case StartTime > 0 andalso (Now - StartTime) of
		false -> RM;
		%% if no duration from JObj, and elapsed time is > twice the timeout
		X when X >= ?BILLING_TIMEOUT -> X - ?BILLING_TIMEOUT;
		Y when Y < RM -> RM;
		Z -> Z
	    end,
    whistle_util:to_integer(wh_json:get_value(<<"Billing-Seconds">>, JObj, Guess)).
