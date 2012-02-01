%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Monitor a call for CDR. If per-minute, write to the ledger with the final
%%% cost of the call. Sole listener for call events
%%% @end
%%% Created : 18 Nov 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(j5_call_monitor).

-behaviour(gen_listener).

%% API
-export([start_link/3, handle_call_event/2, authz_won/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2,
         terminate/2, code_change/3]).

-include("jonny5.hrl").

-define(SERVER, ?MODULE).
-define(TIMER_CALL_STATUS, 60000). %% ask for call status every 10 seconds

-record(state, {
          callid = <<>> :: binary()
         ,ledger_db = <<>> :: binary()
         ,call_type = 'per_min' :: call_types()
         ,authz_won = 'false' :: boolean()
         ,timer_ref = 'undefined' :: 'undefined' | reference() % timer ref for sending call_status requests
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
start_link(CallID, LedgerDB, CallType) ->
    gen_listener:start_link(?MODULE
                          ,[{bindings, [{call, [{callid, CallID}, {restrict_to, [events, cdr]}]}, {self, []}]}
                            ,{responders, [{{?MODULE, handle_call_event}, [{<<"*">>, <<"*">>}]}] }
                           ]
                          ,[CallID, LedgerDB, CallType]).

authz_won(Srv) ->
    gen_listener:cast(Srv, authz_won).

handle_call_event(JObj, Props) ->
    Srv = props:get_value(server, Props),
    gen_listener:cast(Srv, {call_event, wh_util:get_event_type(JObj), JObj}).

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
init([CallID, LedgerDB, CallType]) ->
    put(callid, CallID),
    ?LOG_SYS("init complete for call of type ~s", [CallType]),
    {ok, #state{callid=CallID, ledger_db=LedgerDB
                ,call_type=CallType, timer_ref=start_status_timer()
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
handle_call(_, _From, State) ->
    {reply, ok, State}.

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
handle_cast(authz_won, State) ->
    ?LOG("Aww yeah, won the authz. We're responsible for writing to the ledger (don't crash!)"),
    {noreply, State#state{authz_won=true}};

handle_cast({call_event, {<<"call_event">>, <<"CHANNEL_HANGUP_COMPLETE">>}, _JObj}, #state{timer_ref=Ref}=State) ->
    ?LOG("Hangup complete, expecting a CDR any moment"),
    {noreply, State#state{timer_ref=restart_status_timer(Ref)}};

handle_cast({call_event, {<<"call_detail">>, <<"cdr">>}, JObj}, #state{timer_ref=Ref, callid=CallID, ledger_db=DB
                                                                       ,call_type=per_min, authz_won=true}=State) ->
    case wapi_call:cdr_v(JObj) of
        false ->
            ?LOG("CDR failed validation"),
            {noreply, State#state{timer_ref=restart_status_timer(Ref)}};
        true ->
            ?LOG("CDR passed validation"),
            CallID = wh_json:get_value(<<"Call-ID">>, JObj), % assert

            handle_transaction(JObj, DB, per_min),
            {stop, normal, State}
    end;

handle_cast({call_event, {<<"call_detail">>, <<"cdr">>}, JObj}, #state{timer_ref=Ref, callid=CallID, call_type=CallType
                                                                       ,ledger_db=DB, authz_won=true}=State) ->
    case CallID =:= wh_json:get_value(<<"Call-ID">>, JObj) andalso wapi_call:cdr_v(JObj) of
        true ->
            ?LOG("Received CDR, finishing transaction"),

            handle_transaction(JObj, DB, CallType),
            {stop, normal, State};
        false ->
            ?LOG("JSON not for our call leg (recv call-id ~s) or CDR was invalid", [wh_json:get_value(<<"Call-ID">>, JObj)]),
            {noreply, State#state{timer_ref=restart_status_timer(Ref)}}
    end;

handle_cast({call_event, {<<"call_detail">>, <<"cdr">>}, JObj}, #state{timer_ref=Ref, authz_won=false}=State) ->
    ?LOG("CDR received, but we're not the winner of the authz_win, so let's wait a bit and see if the ledger was updated"),
    erlang:send_after(500 + crypto:rand_uniform(500, 1000), self(), {check_ledger, JObj}),
    {noreply, State#state{timer_ref=restart_status_timer(Ref)}};

handle_cast({call_event, {<<"call_event">>, <<"status_resp">>}, JObj}, #state{timer_ref=Ref}=State) ->
    case {wapi_call:channel_status_resp_v(JObj), wapi_call:get_status(JObj)} of
        {true, <<"active">>} ->
            ?LOG("Received active status_resp"),
            {noreply, State#state{timer_ref=restart_status_timer(Ref)}};
        {true, <<"tmpdown">>} ->
            ?LOG("Call tmpdown, starting down timer"),
            _ = erlang:cancel_timer(Ref),
            {noreply, State#state{timer_ref=start_down_timer()}};
        {false, _} ->
            ?LOG("Failed validation of status_resp"),
            {noreply, State}
    end;

handle_cast({call_event, {Cat, Name}, _JObj}, #state{timer_ref=Ref}=State) ->
    ?LOG("Unhandled event ~s:~s: ~s", [Cat, Name, wh_json:get_value(<<"Application-Name">>, _JObj)]),
    {noreply, State#state{timer_ref=restart_status_timer(Ref)}}.

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
handle_info({check_ledger, JObj}, #state{callid=CallID, ledger_db=DB, call_type=CallType}=State) ->
    ?LOG("Checking ledger for final debit/credit"),
    CallID = wh_json:get_value(<<"Call-ID">>, JObj),

    handle_transaction(JObj, DB, CallType),

    {stop, normal, State};

handle_info({timeout, CallStatusRef, call_status}, #state{timer_ref=CallStatusRef, callid=CallID}=State) ->
    ?LOG("Been a while, time to check in on call"),

    Self = self(),
    spawn(fun() ->
                  StatusReq = [{<<"Call-ID">>, CallID}
                               | wh_api:default_headers(gen_listener:queue_name(Self), ?APP_NAME, ?APP_VERSION)
                              ],

                  wapi_call:publish_channel_status_req(CallID, StatusReq)
          end),

    {noreply, State#state{timer_ref=start_status_timer()}};

handle_info({timeout, DownRef, call_status_down}, #state{callid=CallID, timer_ref=DownRef, call_type=per_min, ledger_db=DB}=State) ->
    ?LOG("Per minute call got lost somehow. What to do???"),

    whapps_util:alert(<<"alert">>, ["Source: ~s(~p)~n"
                                    ,"Alert: Per-minute call went down without us receiving the CDR.~n"
                                    ,"No way to bill the proper amount (didn't check for the CDR in the DB.~n"
                                    ,"Call-ID: ~s~n"
                                    ,"Ledger-DB: ~s~n"
                                   ]
                      ,[?MODULE, ?LINE, CallID, DB]),

    {stop, normal, State};

handle_info({timeout, DownRef, call_status_down}, #state{timer_ref=DownRef}=State) ->
    ?LOG("Call is down; status requests have gone unanswered or indicate call is down"),
    {stop, normal, State};

handle_info(_Info, State) ->
    ?LOG("Unhandled message: ~p", [_Info]),
    {noreply, State}.

handle_event(_, _) ->
    {reply, []}.

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
terminate(_Reason, _State) ->
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
%% Return the cost of the call in UNITS (not dollars)
-spec extract_cost/1 :: (wh_json:json_object()) -> integer().
extract_cost(JObj) ->
    BillingSecs = wh_json:get_integer_value(<<"Billing-Seconds">>, JObj),
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),

    Rate = wh_json:get_float_value(<<"Rate">>, CCVs, 0.0),
    RateIncr = wh_json:get_integer_value(<<"Rate-Increment">>, CCVs, 0),
    RateMin = wh_json:get_integer_value(<<"Rate-Minimum">>, CCVs, 0),
    Surcharge = wh_json:get_float_value(<<"Surcharge">>, CCVs, 0.0),

    Cost = whapps_util:calculate_cost(Rate, RateIncr, RateMin, Surcharge, BillingSecs),
    ?LOG("Rating call: ~p at incr: ~p with min: ~p and surcharge: ~p for ~p secs: $~p", [Rate, RateIncr, RateMin, Surcharge, BillingSecs, Cost]),
    wapi_money:dollars_to_units(Cost).

handle_transaction(JObj, DB, CallType) ->
    ?LOG("CDR is for our call-id"),
    CallID = wh_json:get_value(<<"Call-ID">>, JObj),
    BillingSecs = wh_json:get_integer_value(<<"Billing-Seconds">>, JObj),

    PerMinCharge = wapi_money:default_per_min_charge(),
    case extract_cost(JObj) of
        Cost when Cost < PerMinCharge ->
            Credit = PerMinCharge - Cost,
            ?LOG("Crediting back ~p", [Credit]),
            {ok, Transaction} = j5_util:write_credit_to_ledger(DB, CallID, CallType, Credit, BillingSecs, JObj),
            publish_transaction(Transaction, fun wapi_money:publish_credit/1);
        Cost ->
            Debit = Cost - PerMinCharge,
            ?LOG("Debiting an additional ~p", [Debit]),
            {ok, Transaction} = j5_util:write_debit_to_ledger(DB, CallID, CallType, Debit, BillingSecs, JObj),
            publish_transaction(Transaction, fun wapi_money:publish_debit/1)
    end.

publish_transaction(Transaction, PublisherFun) ->
    ?LOG("Publishing transaction to wapi_money"),
    PublisherFun([{<<"Transaction-ID">>, wh_json:get_value(<<"_id">>, Transaction)}
                  ,{<<"Account-ID">>, wh_json:get_value(<<"pvt_account_id">>, Transaction)}
                  ,{<<"Amount">>, wh_json:get_value(<<"amount">>, Transaction)}
                  | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                 ]).

-spec start_status_timer/0 :: () -> reference().
-spec restart_status_timer/1 :: ('undefined' | reference()) -> reference().
restart_status_timer(undefined) ->
    start_status_timer();
restart_status_timer(Ref) ->
    _ = erlang:cancel_timer(Ref),
    start_status_timer().

start_status_timer() ->
    erlang:start_timer(?TIMER_CALL_STATUS, self(), call_status).

start_down_timer() ->
    erlang:start_timer(?TIMER_CALL_STATUS, self(), call_status_down).
