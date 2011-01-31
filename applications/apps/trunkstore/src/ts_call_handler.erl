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
-export([start_link/3, get_queue/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(props, [get_value/2, get_value/3]).
-import(logger, [format_log/3]).

-include("../include/amqp_client/include/amqp_client.hrl").
-include("ts.hrl").

-record(state, {callid = <<>> :: binary()
		,amqp_q = <<>> :: binary()
		,amqp_h = "" :: string()
		,ctl_q = <<>> :: binary() %% the control queue for the call, if we won the route_resp race
                ,route_flags = #route_flags{} :: #route_flags{}
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
    gen_server:start_link(?MODULE, [CallID, AmqpHost, RouteFlags], []).

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
init([CallID, AmqpHost, RouteFlags]) ->
    {ok, #state{callid = CallID
		,amqp_q = get_amqp_queue(AmqpHost, CallID)
		,amqp_h = AmqpHost
		,route_flags = RouteFlags
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
handle_info({_, #amqp_msg{props = _Props, payload = Payload}}, #state{route_flags=Flags}=S) ->
    format_log(info, "TS_CALL(~p): Recv off amqp: ~s~n", [self(), Payload]),

    {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),

    case get_value(<<"Event-Name">>, Prop) of
	<<"cdr">> ->
	    spawn(fun() ->
			  true = whistle_api:call_cdr_v(Prop),
			  update_account(whistle_util:to_integer(get_value(<<"Billing-Seconds">>, Prop)), Flags)
		  end),
	    {stop, cdr_received, S};
	<<"route_win">> ->
	    true = whistle_api:route_win_v(Prop),	    
	    format_log(info, "TS_CALL(~p): route win received~n~p~n", [self(), Prop]),
	    {noreply, S#state{ctl_q=props:get_value(<<"Control-Queue">>, Prop)}};
	_EvtName ->
	    format_log(info, "TS_CALL(~p): Evt: ~p AppMsg: ~p~n", [self(), _EvtName, get_value(<<"Application-Response">>, Prop)]),
	    {noreply, S}
    end.

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
terminate(_Reason, #state{amqp_h=AmqpHost, amqp_q=Q}) ->
    amqp_util:delete_callmgr_queue(AmqpHost, Q),
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
    EvtQ = amqp_util:new_callevt_queue(AmqpHost, <<>>),
    format_log(info, "TS_CALL(~p): Listening on Q: ~p for call events relating to ~p", [self(), EvtQ, CallID]),

    amqp_util:bind_q_to_callevt(AmqpHost, EvtQ, CallID, events),
    amqp_util:bind_q_to_callevt(AmqpHost, EvtQ, CallID, cdr),
    amqp_util:bind_q_to_targeted(AmqpHost, EvtQ),

    amqp_util:basic_consume(AmqpHost, EvtQ),
    EvtQ.

%% Duration - billable seconds
-spec(update_account/2 :: (Duration :: integer(), Flags :: #route_flags{}) -> no_return()).
update_account(_, #route_flags{callid=CallID, flat_rate_enabled=true, account_doc_id=DocID}) ->
    ts_acctmgr:release_trunk(DocID, CallID);
update_account(Duration, #route_flags{flat_rate_enabled=false, account_doc_id=DocID, callid=CallID
				      ,rate=R, rate_increment=RI, rate_minimum=RM, surcharge=S}) ->
    Amount = calculate_cost(R, RI, RM, S, Duration),
    ts_acctmgr:release_trunk(DocID, CallID, Amount).

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
