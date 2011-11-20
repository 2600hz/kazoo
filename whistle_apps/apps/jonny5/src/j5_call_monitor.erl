%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Monitor a call for CDR. If per-minute, write to the ledger with the final
%%% cost of the call.
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

-record(state, {
	  callid = <<>> :: binary()
	 ,ledger_db = <<>> :: binary()
	 ,call_type = 'per_min' :: call_types()
         ,authz_won = 'false' :: boolean()
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
			  ,[{bindings, [{call, [{callid, CallID}]}]}
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
    {ok, #state{callid=CallID, ledger_db=LedgerDB, call_type=CallType}}.

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

handle_cast({call_event, {<<"call_event">>, <<"CHANNEL_HANGUP_COMPLETE">>}, _JObj}, State) ->
    ?LOG("Hangup complete, expecting a CDR any moment"),
    {noreply, State, 5000};

handle_cast({call_event, {<<"call_detail">>, <<"cdr">>}, JObj}, #state{callid=CallID, ledger_db=DB, call_type=per_min, authz_won=true}=State) ->
    case wapi_call:cdr_v(JObj) of
	false -> {noreply, State, 5000};
	true ->
	    CallID = wh_json:get_value(<<"Call-ID">>, JObj), % assert
	    BillingSecs = wh_json:get_integer_value(<<"Billing-Seconds">>, JObj),

	    case extract_cost(JObj) of
		Cost when Cost < ?PER_MIN_MIN ->
		    Credit = ?PER_MIN_MIN - Cost,
		    ?LOG("Crediting back ~p", [Credit]),
		    {ok, Transaction} = j5_util:write_credit_to_ledger(DB, CallID, per_min, Credit, BillingSecs, JObj),
		    publish_transaction(Transaction, fun wapi_money:publish_credit/1);
		Cost ->
		    Debit = Cost - ?PER_MIN_MIN,
		    ?LOG("Debiting an additional ~p", [Debit]),
		    {ok, Transaction} = j5_util:write_debit_to_ledger(DB, CallID, per_min, Debit, BillingSecs, JObj),
		    publish_transaction(Transaction, fun wapi_money:publish_debit/1)
	    end,

	    {stop, normal, State}
    end;

handle_cast({call_event, {<<"call_detail">>, <<"cdr">>}, JObj}, #state{callid=CallID, ledger_db=DB, call_type=Type, authz_won=true}=State) ->
    CallID = wh_json:get_value(<<"Call-ID">>, JObj), % assert

    BillingSecs = wh_json:get_integer_value(<<"Billing-Seconds">>, JObj),
    {ok, Transaction} = j5_util:write_credit_to_ledger(DB, CallID, Type, 0, BillingSecs, JObj),
    publish_transaction(Transaction, fun wapi_money:publish_credit/1),

    {stop, normal, State};

handle_cast({call_event, {<<"call_detail">>, <<"cdr">>}, JObj}, #state{authz_won=false}=State) ->
    ?LOG("CDR received, but we're not the winner of the authz_win, so let's wait a bit and see if the ledger was updated"),
    erlang:send_after(1000, self(), {check_ledger, JObj}),
    {noreply, State};

handle_cast({call_event, {Cat, Name}, _JObj}, State) ->
    ?LOG("Unhandled event ~s:~s: ~s", [Cat, Name, wh_json:get_value(<<"Application-Name">>, _JObj)]),
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
handle_info({check_ledger, JObj}, #state{callid=CallID, ledger_db=DB, call_type=Type}=State) ->
    ?LOG("Checking ledger for final debit/credit"),
    CallID = wh_json:get_value(<<"Call-ID">>, JObj),

    BillingSecs = wh_json:get_integer_value(<<"Billing-Seconds">>, JObj),
    {ok, Transaction} = j5_util:write_credit_to_ledger(DB, CallID, Type, 0, BillingSecs, JObj),
    publish_transaction(Transaction, fun wapi_money:publish_credit/1),

    {stop, normal, State};

handle_info(_Info, State) ->
    ?LOG("Unhandled message: ~p", [_Info]),
    {noreply, State}.

handle_event(_, _) ->
    {reply, [{server, self()}]}.

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
-spec extract_cost/1 :: (json_object()) -> integer().
extract_cost(JObj) ->
    BillingSecs = wh_json:get_integer_value(<<"Billing-Seconds">>, JObj),
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),

    Rate = wh_json:get_float_value(<<"Rate">>, CCVs, 0.0),
    RateIncr = wh_json:get_integer_value(<<"Rate-Increment">>, CCVs, 0),
    RateMin = wh_json:get_integer_value(<<"Rate-Minimum">>, CCVs, 0),
    Surcharge = wh_json:get_float_value(<<"Surcharge">>, CCVs, 0.0),

    Cost = whapps_util:calculate_cost(Rate, RateIncr, RateMin, Surcharge, BillingSecs),
    ?LOG("Rating call: ~p at incr: ~p with min: ~p and surcharge: ~p for ~p secs: $~p", [Rate, RateIncr, RateMin, Surcharge, BillingSecs, Cost]),
    ?DOLLARS_TO_UNITS(Cost).

publish_transaction(Transaction, PublisherFun) ->
    ?LOG("Publishing transaction to wapi_money"),
    PublisherFun(wh_json:from_list([{<<"Transaction-ID">>, wh_json:get_value(<<"_id">>, Transaction)}
				    ,{<<"Account-ID">>, wh_json:get_value(<<"pvt_account_id">>, Transaction)}
				    ,{<<"Amount">>, wh_json:get_value(<<"amount">>, Transaction)}
				    | wh_util:default_headers(?APP_NAME, ?APP_VERSION)
				   ])).
