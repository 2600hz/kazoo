%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_call_monitor).

-behaviour(gen_listener).

-export([start_link/3
         ,handle_call_event/2
         ,handle_authz_win/2
        ]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("jonny5.hrl").

-define(SERVER, ?MODULE).
-define(TIMER_CALL_STATUS, 60000). %% ask for call status every 60 seconds

-define(RESPONDERS, [{{?MODULE, handle_authz_win}, [{<<"dialplan">>, <<"authz_win">>}]}
                     ,{{?MODULE, handle_call_event}, [{<<"call_event">>, <<"*">>}]}
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-record(state, {callid = <<>> :: binary()
                ,account_db = <<>> :: binary()
                ,type = 'prepay' :: 'prepay' | 'postpay'
                ,reserve_amount = 0.0 :: float()                    
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
start_link(Type, ReserveAmount, JObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    Bindings = [{call, [{callid, CallId}]}
                ,{authz, []}
                ,{self, []}
               ],
    gen_listener:start_link(?MODULE, [{bindings, Bindings}
                                      ,{responders, ?RESPONDERS}
                                      ,{queue_name, ?QUEUE_NAME}
                                      ,{queue_options, ?QUEUE_OPTIONS}
                                      ,{consume_options, ?CONSUME_OPTIONS}
                                     ], [Type, ReserveAmount, JObj]).

handle_authz_win(_, Props) ->
    Srv = props:get_value(server, Props),
    gen_listener:cast(Srv, authz_win).

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
init([Type, ReserveAmount, JObj]) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    put(callid, CallId),
    lager:debug("started call monitor for a ~s call ~s", [Type, CallId]),
    AccountDb = wh_util:format_account_id(wh_json:get_value(<<"Account-ID">>, JObj), encoded),   
    {ok, #state{callid=CallId, account_db=AccountDb, reserve_amount=ReserveAmount
                ,type=Type %%, timer_ref=start_status_timer()
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
handle_cast(authz_win, State) ->
    lager:debug("qww yeah, won the authz. We're responsible for writing to the ledger (don't crash!)"),
    {noreply, State#state{authz_won=true}};
handle_cast({call_event, {<<"call_event">>, <<"CHANNEL_HANGUP_COMPLETE">>}, _JObj}, State) ->
    io:format("got channel_hangup_complete: ~p~n", [_JObj]),
    {stop, normal, State};
handle_cast(_, State) ->
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
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
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
    io:format("later suckers! ~p ~p~n", [_Reason, _State]),
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
    lager:debug("rating call: ~p at incr: ~p with min: ~p and surcharge: ~p for ~p secs: $~p", [Rate, RateIncr, RateMin, Surcharge, BillingSecs, Cost]),
    wapi_money:dollars_to_units(Cost).

handle_transaction(JObj, DB, CallType) ->
    lager:debug("cdr is for our call-id"),
    CallID = wh_json:get_value(<<"Call-ID">>, JObj),
    BillingSecs = wh_json:get_integer_value(<<"Billing-Seconds">>, JObj),

    PerMinCharge = wapi_money:default_per_min_charge(),
    case extract_cost(JObj) of
        Cost when Cost < PerMinCharge ->
            Credit = PerMinCharge - Cost,
            lager:debug("crediting back ~p", [Credit]),
            {ok, Transaction} = j5_util:write_credit_to_ledger(DB, CallID, CallType, Credit, BillingSecs, JObj),
            publish_transaction(Transaction, fun wapi_money:publish_credit/1);
        Cost ->
            Debit = Cost - PerMinCharge,
            lager:debug("debiting an additional ~p", [Debit]),
            {ok, Transaction} = j5_util:write_debit_to_ledger(DB, CallID, CallType, Debit, BillingSecs, JObj),
            publish_transaction(Transaction, fun wapi_money:publish_debit/1)
    end.

publish_transaction(Transaction, PublisherFun) ->
    lager:debug("Publishing transaction to wapi_money"),
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
