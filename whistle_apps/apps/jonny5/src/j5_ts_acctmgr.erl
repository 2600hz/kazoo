%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Handle serializing account access for trunkstore accounts
%%% @end
%%% Created : 16 Jul 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(j5_ts_acctmgr).

-behaviour(gen_server).

%% API
-export([start_link/1, authz_trunk/3, known_calls/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("jonny5.hrl").

-define(SERVER, ?MODULE). 

-record(state, {
	  my_q = undefined :: binary() | undefined
	 ,is_amqp_up = true :: boolean()
	 ,cache = undefined :: undefined | pid()
         ,cache_ref = make_ref() :: reference()
	 ,acct_id = <<>> :: binary()
         ,trunks_available = {0,0,0.0} :: {non_neg_integer(), non_neg_integer(), float()} %% {TwoWay, Inbound, Prepay}
         ,trunks_in_use = dict:new() :: dict() %% {CallID, Type :: inbound | two_way}
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
start_link(AcctID) ->
    ?LOG_SYS("starting with acct ~s", [AcctID]),
    gen_server:start_link(?MODULE, [AcctID], []).

-spec authz_trunk/3 :: (Pid, JObj, CallDir) -> {boolean(), proplist()} when
      Pid :: pid(),
      JObj :: json_object(),
      CallDir :: inbound | outbound.
authz_trunk(Pid, JObj, CallDir) ->
    gen_server:call(Pid, {authz, JObj, CallDir}).

known_calls(Pid) ->
    gen_server:call(Pid, known_calls).

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
init([AcctID]) ->
    CPid = whereis(j5_cache),
    ?LOG_SYS("CPid: ~p", [CPid]),
    Ref = erlang:monitor(process, CPid),
    ?LOG_SYS("Ref for CPid: ~p", [Ref]),
    wh_cache:store_local(CPid, {j5_authz, AcctID}, self(), 24 * 3600), %% 1 day
    ?LOG_SYS("Stored acct in cache with ~p as the value", [self()]),

    Q = amqp_util:new_queue(),
    ?LOG_SYS("Will listen for call events/cdrs on ~s", [Q]),

    case get_trunks_available(AcctID) of
	{error, not_found} ->
	    ?LOG_SYS("No account found for ~s", [AcctID]),
	    {stop, no_account};
	TrunksAvail ->
	    ?LOG_SYS("Init for ~s complete", [AcctID]),
	    {ok, #state{my_q=Q, is_amqp_up=is_binary(Q)
			,cache=CPid, cache_ref=Ref
			,trunks_available = TrunksAvail
		       }}
    end.

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

handle_call(known_calls, _, #state{trunks_in_use=Dict}=State) ->
    {reply, dict:to_list(Dict), State};
%% pull from inbound, then trunks, then prepay
handle_call({authz, JObj, inbound}, _From, #state{my_q=Q, trunks_available={Two, In, Pre}, trunks_in_use=Dict}=State) ->
    CallID = wh_json:get_value(<<"Call-ID">>, JObj),
    ?LOG_START(CallID, "Authorizing call...", []),

    case In > 0 of
	true ->
	    monitor_call(Q, CallID),
	    ?LOG_END(CallID, "Inbound call authorized using an inbound trunk", []),
	    {reply
	     ,{true, [{<<"Trunk-Type">>, <<"inbound">>}]}
	     , State#state{trunks_available={Two, In-1, Pre}, trunks_in_use=dict:store(CallID, inbound, Dict)}
	    };
	false ->
	    case try_twoway(CallID, State) of
		{{true, _}=Resp, State1} -> ?LOG_END(CallID, "Inbound call authorized using a two-way trunk", []), {reply, Resp, State1};
		{{false, _}=Resp, State2} -> ?LOG_END(CallID, "Inbound call NOT authorized with flat rate trunks", []), {reply, Resp, State2} %% add per-minute check here?
	    end
    end;

handle_call({authz, JObj, outbound}, _From, State) ->
    CallID = wh_json:get_value(<<"Call-ID">>, JObj),
    case try_twoway(CallID, State) of
	{{true, _}=Resp, State1} -> ?LOG_END(CallID, "Outbound call authorized using a two-way trunk", []), {reply, Resp, State1};
	{{false, _}=Resp, State2} -> ?LOG_END(CallID, "Outbound call NOT authorized using flat rate trunks", []), {reply, Resp, State2} %% add per-minute check here?
    end.

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
handle_info({_, #amqp_msg{payload=Payload}}, #state{my_q=Q, trunks_available={Two, In, Pre}, trunks_in_use=Dict}=State) ->
    JObj = mochijson2:decode(Payload),
    CallID = wh_json:get_value(<<"Call-ID">>, JObj),
    ?LOG(CallID, "Recv JSON payload: ~s", [Payload]),

    case process_call_event(CallID, JObj, Dict) of
	{release, inbound, Dict1} ->
	    ?LOG_END(CallID, "Releasing inbound trunk", []),
	    unmonitor_call(Q, CallID),
	    {noreply, State#state{trunks_available={Two, In+1, Pre}, trunks_in_use=Dict1}};
	{release, twoway, Dict2} ->
	    ?LOG_END(CallID, "Releasing two-way trunk", []),
	    unmonitor_call(Q, CallID),
	    {noreply, State#state{trunks_available={Two+1, In, Pre}, trunks_in_use=Dict2}};
	ignore ->
	    ?LOG_END(CallID, "Ignoring event", []),
	    {noreply, State}
    end;

handle_info(_Info, State) ->
    ?LOG_SYS("Unhandled message: ~p", [_Info]),
    {noreply, State}.

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
-spec get_trunks_available/1 :: (AcctID) -> {error, not_found} | {non_neg_integer(), non_neg_integer(), float()} when
      AcctID :: binary().
get_trunks_available(AcctID) ->
    case couch_mgr:open_doc(<<"ts">>, AcctID) of
	{error, not_found}=E ->
	    ?LOG_SYS("No account found in ts: ~s", [AcctID]),
	    E;
	{ok, JObj} ->
	    Acct = wh_json:get_value(<<"account">>, JObj, ?EMPTY_JSON_OBJECT),
	    Credits = wh_json:get_value(<<"credits">>, Acct, ?EMPTY_JSON_OBJECT),

	    Trunks = whistle_util:to_integer(wh_json:get_value(<<"trunks">>, Acct, 0)),
	    InboundTrunks = whistle_util:to_integer(wh_json:get_value(<<"inbound_trunks">>, Acct, 0)),
	    Prepay = whistle_util:to_float(wh_json:get_value(<<"prepay">>, Credits, 0.0)),
	    %% Balance = ?DOLLARS_TO_UNITS(),
	    ?LOG_SYS("Found trunk levels for ~s: ~b two way, ~b inbound, and $ ~p prepay", [AcctID, Trunks, InboundTrunks, Prepay]),
	    {Trunks, InboundTrunks, Prepay}
    end.

-spec try_twoway/2 :: (CallID, State) -> {{boolean(), proplist()}, #state{}} when
      CallID :: binary(),
      State :: #state{}.
try_twoway(_, #state{trunks_available={0, In, Pre}}=State) ->
    {{false, []}, State#state{trunks_available={0, In, Pre}}};
try_twoway(CallID, #state{my_q=Q, trunks_available={Two, In, Pre}, trunks_in_use=Dict}=State) ->
    monitor_call(Q, CallID),
    {{true, [{<<"Trunk-Type">>, <<"two_way">>}]}
     ,State#state{trunks_available={Two-1, In, Pre}, trunks_in_use=dict:store(CallID, twoway, Dict)}
    }.

monitor_call(Q, CallID) ->
    _ = amqp_util:bind_q_to_callevt(Q, CallID),
    _ = amqp_util:bind_q_to_callevt(Q, CallID, cdr),
    ?LOG(CallID, "Monitoring with ~s", [Q]),
    amqp_util:basic_consume(Q).

unmonitor_call(Q, CallID) ->
    R1 = amqp_util:unbind_q_from_callevt(Q, CallID),
    R2 = amqp_util:unbind_q_from_callevt(Q, CallID, cdr),
    ?LOG(CallID, "Un-monitoring: ~p", [R1]),
    ?LOG(CallID, "Un-monitoring: ~p", [R2]),
    amqp_util:basic_consume(Q).

-spec process_call_event/3 :: (CallID, JObj, Dict) -> ignore | {release, twoway | inbound, dict()} when
      CallID :: binary(),
      JObj :: json_object(),
      Dict :: dict().
process_call_event(CallID, JObj, Dict) ->
    case { wh_json:get_value(<<"Application-Name">>, JObj)
	   ,wh_json:get_value(<<"Event-Name">>, JObj)
	   ,wh_json:get_value(<<"Event-Category">>, JObj) } of
	{ <<"bridge">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"call_event">> } ->
	    ?LOG(CallID, "Bridge event received", []),
	    case wh_json:get_value(<<"Application-Response">>, JObj) of
		<<"SUCCESS">> ->
		    ?LOG(CallID, "Bridge event successful", []),
		    ignore;
		Cause ->
		    ?LOG("Failed to bridge: ~s", [Cause]),
		    release_trunk(CallID, Dict)
	    end;

	{ _, <<"CHANNEL_HANGUP">>, <<"call_event">> } ->
	    ?LOG(CallID, "Channel hungup", []),
	    release_trunk(CallID, Dict);

	{ _, _, <<"error">> } ->
	    ?LOG(CallID, "Execution failed", []),
	    release_trunk(CallID, Dict);

	{_, <<"CHANNEL_HANGUP_COMPLETE">>, <<"call_event">>} ->
	    ?LOG(CallID, "Channel hungup complete", []),
	    release_trunk(CallID, Dict);

	{ _, <<"cdr">>, <<"call_detail">> } ->
	    ?LOG(CallID, "CDR received", []),
	    release_trunk(CallID, Dict);

	_E ->
	    ?LOG("Unhandled call event: ~p", [_E]),
	    ignore
    end.

-spec release_trunk/2 :: (CallID, Dict) -> ignore | {release, twoway | inbound, dict()} when
      CallID :: binary(),
      Dict :: dict().
release_trunk(CallID, Dict) ->
    case dict:find(CallID, Dict) of
	error ->
	    ?LOG_SYS(CallID, "Call is unknown to us", []),
	    ignore;
	{ok, TrunkType} ->
	    {release, TrunkType, dict:erase(CallID, Dict)}
    end.
