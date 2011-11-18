%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Handle serializing account access for crossbar accounts
%%% @end
%%% Created : 16 Jul 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(j5_acctmgr).

-behaviour(gen_listener).

%% API
-export([start_link/1, authz_trunk/3, known_calls/1, status/1, refresh/1]).

-export([credit/2, debit/2]).

-export([handle_call_event/2, handle_j5_msg/2, handle_conf_change/2, handle_authz_win/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2
	 ,terminate/2, code_change/3]).

-include("jonny5.hrl").

-define(SERVER, ?MODULE).
-define(SYNC_TIMER, 60000).

-record(state, {
	 acct_id = <<>> :: binary()
         ,acct_rev = <<>> :: binary()
	 ,acct_type = 'account' :: 'account' | 'ts'
	 ,max_two_way = 0 :: non_neg_integer()
         ,max_inbound = 0 :: non_neg_integer()
	 ,two_way = 0 :: non_neg_integer()
         ,inbound = 0 :: non_neg_integer()
         ,prepay = 0 :: non_neg_integer()
         ,trunks_in_use = dict:new() :: dict() %% {CallID, Type :: inbound | two_way}
	 ,start_time = 1 :: pos_integer()
         ,sync_ref :: reference()
	 ,ledger_db = <<>> :: binary() %% where to write credits/debits
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
-spec start_link/1 :: (ne_binary()) -> {'ok', pid()} | 'ignore' | {'error', term()}.
start_link(AcctID) ->
    %% why are we receiving messages for account IDs we don't bind to?
    gen_listener:start_link(?MODULE, [{bindings, [{self, []}
						  ,{jonny5, [{account_id, AcctID}]}
						  ,{conf, [{doc_id, AcctID}, {doc_type, <<"sip_service">>}]}
						 ]}
				      ,{responders, [{ {?MODULE, handle_call_event}, [{<<"call_event">>, <<"*">>} % call events
										      ,{<<"call_detail">>, <<"*">>} % and CDR
										     ]
						     }
						     ,{ {?MODULE, handle_conf_change}, [{<<"configuration">>, <<"*">>}]}
						     ,{ {?MODULE, handle_j5_msg}, [{<<"jonny5">>, <<"*">>}]} % internal J5 sync/status
						     ,{ {?MODULE, handle_authz_win}, [{<<"dialplan">>, <<"authz_win">>}] } % won the authz
						    ]}
				     ], [AcctID]).

-spec status/1 :: (pid()) -> json_object().
status(Srv) ->
    gen_listener:call(Srv, status).

-spec refresh/1 :: (pid()) -> 'ok'.
refresh(Srv) ->
    gen_listener:cast(Srv, refresh).

-spec credit/2 :: (pid(), integer()) -> 'ok'.
credit(Srv, Credit) ->
    gen_listener:cast(Srv, {credit, Credit}).

-spec debit/2 :: (pid(), integer()) -> 'ok'.
debit(Srv, Debit) ->
    gen_listener:cast(Srv, {debit, Debit}).

-spec authz_trunk/3 :: (pid() | ne_binary(), json_object(), 'inbound' | 'outbound') -> {boolean(), proplist()}.
authz_trunk(Pid, JObj, CallDir) when is_pid(Pid) ->
    {Bool, Prop} = gen_server:call(Pid, {authz, JObj, CallDir}),
    {Bool, [{<<"Server-ID">>, gen_listener:queue_name(Pid)} | Prop]};

authz_trunk(AcctID, JObj, CallDir) ->
    case j5_util:fetch_account_handler(AcctID) of
	{ok, AcctPID} ->
	    case erlang:is_process_alive(AcctPID) of
		true ->
		    ?LOG_SYS("Account(~s) AuthZ proc ~p found", [AcctID, AcctPID]),
		    j5_acctmgr:authz_trunk(AcctPID, JObj, CallDir);
		false ->
		    ?LOG_SYS("Account(~s) AuthZ proc ~p not alive", [AcctID, AcctPID]),
		    {ok, AcctPID} = jonny5_acct_sup:start_proc(AcctID),
		    j5_acctmgr:authz_trunk(AcctPID, JObj, CallDir)
	    end;
	{error, not_found} ->
	    ?LOG_SYS("No AuthZ proc for account ~s, starting", [AcctID]),
	    try
		{ok, AcctPID} = jonny5_acct_sup:start_proc(AcctID),
		j5_acctmgr:authz_trunk(AcctPID, JObj, CallDir)
	    catch
		E:R ->
		    ST = erlang:get_stacktrace(),
		    ?LOG_SYS("Error: ~p: ~p", [E, R]),
		    _ = [ ?LOG_SYS("Stacktrace: ~p", [ST1]) || ST1 <- ST],
		    {false, []}
	    end
    end.

known_calls(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, known_calls);
known_calls(AcctID) when is_binary(AcctID) ->
    case j5_util:fetch_account_handler(AcctID) of
	{error, _}=E -> E;
	{ok, AcctPid} when is_pid(AcctPid) -> known_calls(AcctPid)
    end.

handle_call_event(JObj, Props) ->
    Srv = props:get_value(server, Props),
    gen_listener:cast(Srv, {call_event, JObj}).

handle_j5_msg(JObj, Props) ->
    Srv = props:get_value(server, Props),
    gen_listener:cast(Srv, {j5_msg, wh_json:get_value(<<"Event-Name">>, JObj), JObj}).

handle_conf_change(JObj, Props) ->
    Srv = props:get_value(server, Props),
    gen_listener:cast(Srv, {conf_change, wh_json:get_value(<<"Event-Name">>, JObj), JObj}).

handle_authz_win(JObj, Props) ->
    Srv = props:get_value(server, Props),
    gen_listener:cast(Srv, {authz_win, JObj}).

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
    SyncRef = erlang:start_timer(0, self(), sync), % want this to be the first message we get
    j5_util:store_account_handler(AcctID, self()),

    StartTime = wh_util:current_tstamp(),

    put(callid, AcctID),
    case get_trunks_available(AcctID, account) of
	{error, not_found} ->
	    ?LOG_SYS("No account found for ~s", [AcctID]),
	    {stop, no_account};
	{undefined, undefined, undefined, account} ->
	    {TwoWay, Inbound, Prepay, ts} = get_trunks_available(AcctID, ts),
	    ?LOG_SYS("Init for ts ~s complete", [AcctID]),

	    {ok, Rev} = couch_mgr:lookup_doc_rev(<<"ts">>, AcctID),

	    LedgerDB = list_to_binary(["ts%2fusage%2f", AcctID]),
	    couch_mgr:db_create(LedgerDB),

	    {ok, #state{prepay=try_update_value(Prepay, 0)
			,two_way=try_update_value(TwoWay, 0)
			,inbound=try_update_value(Inbound, 0)
			,max_two_way=try_update_value(TwoWay, 0)
			,max_inbound=try_update_value(Inbound, 0)
			,acct_rev=Rev, acct_id=AcctID, acct_type=ts
			,start_time=StartTime, sync_ref=SyncRef
			,ledger_db=LedgerDB
		       }};
	{TwoWay, Inbound, Prepay, account} ->
	    ?LOG_SYS("Init for account ~s complete", [AcctID]),

	    {ok, #state{prepay=try_update_value(Prepay, 0)
			,two_way=try_update_value(TwoWay, 0)
			,inbound=try_update_value(Inbound, 0)
			,max_two_way=try_update_value(TwoWay, 0)
			,max_inbound=try_update_value(Inbound, 0)
			,acct_id=AcctID, acct_type=account
			,start_time=StartTime, sync_ref=SyncRef
			,ledger_db=whapps_util:get_db_name(AcctID, encoded)
		       }};
	{TwoWay, Inbound, Prepay, ts} ->
	    ?LOG_SYS("Init for ts ~s complete", [AcctID]),

	    {ok, Rev} = couch_mgr:lookup_doc_rev(<<"ts">>, AcctID),

	    LedgerDB = list_to_binary(["ts%2fusage%2f", AcctID]),
	    couch_mgr:db_create(LedgerDB),

	    {ok, #state{prepay=try_update_value(Prepay, 0)
			,two_way=try_update_value(TwoWay, 0)
			,inbound=try_update_value(Inbound, 0)
			,max_two_way=try_update_value(TwoWay, 0)
			,max_inbound=try_update_value(Inbound, 0)
			,acct_rev=Rev, acct_id=AcctID, acct_type=ts
			,start_time=StartTime, sync_ref=SyncRef
			,ledger_db=LedgerDB
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
handle_call(status, _, #state{max_two_way=MaxTwo, max_inbound=MaxIn
			      ,two_way=Two, inbound=In, trunks_in_use=Dict
			      ,prepay=Prepay, acct_id=Acct}=State) ->
    {reply, wh_json:from_list([{<<"max_two_way">>, MaxTwo}
			       ,{<<"max_inbound">>, MaxIn}
			       ,{<<"two_way">>, Two}
			       ,{<<"inbound">>, In}
			       ,{<<"prepay">>, ?UNITS_TO_DOLLARS(Prepay)}
			       ,{<<"account_id">>, Acct}
			       ,{<<"trunks">>, [begin
						    {CallID, Type} = case Tuple of
									 {CID, {per_min=T, _}} -> {CID, T};
									 {_,_} -> Tuple
								     end,
						    wh_json:from_list([{<<"callid">>, CallID}, {<<"type">>, Type}])
						end
						|| Tuple <- dict:to_list(Dict)
					       ]}
			      ]), State};
handle_call(known_calls, _, #state{trunks_in_use=Dict}=State) ->
    {reply, dict:to_list(Dict), State};

%% pull from inbound, then two_way, then prepay
handle_call({authz, JObj, inbound}, _From, #state{}=State) ->
    CallID = wh_json:get_value(<<"Call-ID">>, JObj),
    ?LOG_START(CallID, "Authorizing inbound call...", []),

    ToDID = case binary:split(wh_json:get_value(<<"To">>, JObj), <<"@">>) of
		[<<"nouser">>, _] ->
		    [RUser, _] = binary:split(wh_json:get_value(<<"Request">>, JObj, <<"nouser">>), <<"@">>),
		    wh_util:to_e164(RUser);
		[ToUser, _] -> wh_util:to_e164(ToUser)
	    end,

    ?LOG("ToDID: ~s", [ToDID]),

    {Resp, State1} = case is_us48(ToDID) of
			 true -> try_inbound_then_twoway(CallID, State);
			 false -> try_prepay(CallID, State)
		     end,
    {reply, Resp, State1, hibernate};

handle_call({authz, JObj, outbound}, _From, State) ->
    CallID = wh_json:get_value(<<"Call-ID">>, JObj),
    ?LOG_START(CallID, "Authorizing outbound call...", []),

    ToDID = case binary:split(wh_json:get_value(<<"To">>, JObj), <<"@">>) of
		[<<"nouser">>, _] ->
		    [RUser, _] = binary:split(wh_json:get_value(<<"Request">>, JObj, <<"nouser">>), <<"@">>),
		    wh_util:to_e164(RUser);
		[ToUser, _] -> wh_util:to_e164(ToUser)
	    end,

    ?LOG("ToDID: ~s", [ToDID]),

    {Resp, State1} = case is_us48(ToDID) of
			 true -> try_twoway(CallID, State);
			 false -> try_prepay(CallID, State)
		     end,
    {reply, Resp, State1, hibernate}.

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
handle_cast({credit, Credit}, #state{prepay=Prepay}=State) ->
    %% maybe publish an event so all J5s of this account can update
    {noreply, State#state{prepay=Prepay+Credit}};
handle_cast({debit, Debit}, #state{prepay=Prepay}=State) ->
    {noreply, State#state{prepay=Prepay+Debit}};

handle_cast({authz_win, JObj}, State) ->
    ?LOG("Authz won!"),
    ?LOG("~p", [JObj]),
    {noreply, State};
handle_cast(refresh, #state{acct_type=AcctType, acct_id=AcctID, max_two_way=OldTwo, max_inbound=OldIn, prepay=OldPrepay}=State) ->
    case catch(get_trunks_available(AcctID, AcctType)) of
	{Trunks, InboundTrunks, Prepay, _} ->
	    ?LOG("Maybe changing max two way from ~b to ~p", [OldTwo, Trunks]),
	    ?LOG("Maybe changing max inbound from ~b to ~p", [OldIn, InboundTrunks]),
	    ?LOG("Maybe changing prepay from ~b to ~p", [OldPrepay, Prepay]),
	    {noreply, State#state{max_two_way=try_update_value(Trunks, OldTwo)
				  ,max_inbound=try_update_value(InboundTrunks, OldIn)
				  ,prepay=try_update_value(Prepay, OldPrepay)
				 }};
	_E ->
	    ?LOG("Failed to refresh: ~p", [_E]),
	    {noreply, State}
    end;

handle_cast({conf_change, <<"doc_edited">>, JObj}, #state{acct_id=AcctID, acct_type=AcctType
							  ,max_two_way=MTW, max_inbound=MI, prepay=P
							  ,two_way=_TW, inbound=_I
							  ,trunks_in_use=Dict
							 }=State) ->
    ConfAcctID = wh_json:get_value(<<"ID">>, JObj, <<"missing">>),
    Doc = wh_json:get_value(<<"Doc">>, JObj),

    {Trunks, InboundTrunks, Prepay, _} = case AcctType of
					     account when ConfAcctID =:= AcctID ->
						 case get_account_values(Doc) of
						     {undefined, undefined, undefined, account} ->
							 get_ts_values(Doc);
						     Levels -> Levels
						 end;
					     ts when ConfAcctID =:= AcctID ->
						 get_ts_values(Doc);
					     _ ->
						 ?LOG("No change necessary"),
						 ?LOG("Conf acct id: ~s", [ConfAcctID]),
						 {MTW, MI, P}
					 end,

    NMTW = try_update_value(Trunks, MTW),
    NMI = try_update_value(InboundTrunks,MI),

    {NTWIU, NTIIU} = dict:fold(fun(_CallID, two_way, {Two, In}) ->
				       {Two-1, In};
				  (_CallID, inbound, {Two, In}) ->
				       {Two, In-1};
				  (_, _, Acc) -> Acc %% ignore per-min
			       end, {NMTW, NMI}, Dict),

    ?LOG("changing max two way from ~b to ~p", [MTW, NMTW]),
    ?LOG("changing max inbound from ~b to ~p", [MI, NMI]),
    ?LOG("changing two-way in use from ~b to ~p", [_TW, NTWIU]),
    ?LOG("changing inbound in use from ~b to ~p", [_I, NTIIU]),
    ?LOG("Maybe changing prepay from ~b to ~p", [P, Prepay]),

    {noreply, State#state{max_two_way=NMTW
			  ,max_inbound=NMI
			  ,two_way=NTWIU
			  ,inbound=NTIIU
			  ,prepay=try_update_value(Prepay, P)
			 }, hibernate};

handle_cast({j5_msg, <<"sync_req">>, JObj}, State) ->
    spawn(fun() -> send_levels_resp(JObj, State, fun wapi_jonny5:publish_sync_resp/2) end),
    {noreply, State};

handle_cast({j5_msg, <<"status_req">>, JObj}, State) ->
    spawn(fun() -> send_levels_resp(JObj, State, fun wapi_jonny5:publish_status_resp/2) end),
    {noreply, State};

handle_cast({j5_msg, <<"sync_resp">>, JObj}, #state{acct_id=AcctID, max_inbound=MaxIn, max_two_way=MaxTwo
						    ,start_time=StartTime, prepay=Prepay
						   }=State) ->
    try
	true = wapi_jonny5:sync_resp_v(JObj),
	AcctID = wh_json:get_value(<<"Account-ID">>, JObj),

	case j5_util:uptime(StartTime) < wh_json:get_integer_value(<<"Uptime">>, JObj) of
	    true ->
		NewMaxTwo = wh_json:get_integer_value(<<"Max-Two-Way">>, JObj, MaxTwo),
		NewMaxIn = wh_json:get_integer_value(<<"Max-Inbound">>, JObj, MaxIn),
		NewPrepay = ?DOLLARS_TO_UNITS(wh_json:get_float_value(<<"Prepay">>, JObj, ?UNITS_TO_DOLLARS(Prepay))),

		?LOG("Uptime is greater than ours, updating max values"),
		?LOG("MaxTwoWay: from ~b to ~b", [MaxTwo, NewMaxTwo]),
		?LOG("MaxIn: from ~b to ~b", [MaxIn, NewMaxIn]),
		?LOG("Prepay: from ~p to ~p", [Prepay, NewPrepay]),

		{noreply, State#state{
			    max_two_way=NewMaxTwo
			    ,max_inbound=NewMaxIn
			    ,prepay=NewPrepay
			   }};
	    false ->
		{noreply, State}
	end
    catch
	error:{badmatch, BadMatch} ->
	    ?LOG("Badmatch error with ~s", [BadMatch]),
	    {noreply, State};
	_T:_R ->
	    ?LOG("Failed to process sync_resp: ~p ~p", [_T, _R]),
	    {noreply, State}
    end;

handle_cast({j5_msg, _Evt, _JObj}, State) ->
    ?LOG("Unhandled j5 message ~s", [_Evt]),
    {noreply, State};

handle_cast({call_event, JObj}, #state{two_way=Two, inbound=In, trunks_in_use=Dict
						    ,max_inbound=MaxIn, max_two_way=MaxTwo
						   }=State) ->
    CallID = wh_json:get_value(<<"Call-ID">>, JObj),

    case process_call_event(CallID, JObj, Dict) of
	{release, inbound, Dict1} ->
	    ?LOG_END(CallID, "Releasing inbound trunk", []),
	    unmonitor_call(CallID),
	    NewIn = case (In+1) of I when I > MaxIn -> MaxIn; I -> I end,
	    {noreply, State#state{inbound=NewIn, trunks_in_use=Dict1}, hibernate};
	{release, twoway, Dict2} ->
	    ?LOG_END(CallID, "Releasing two-way trunk", []),
	    unmonitor_call(CallID),
	    NewTwo = case (Two+1) of T when T > MaxTwo -> MaxTwo; T -> T end,
	    {noreply, State#state{two_way=NewTwo, trunks_in_use=Dict2}, hibernate};
	ignore ->
	    ?LOG_END(CallID, "Ignoring event", []),
	    {noreply, State}
    end.

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
handle_info({timeout, SyncRef, sync}, #state{start_time=StartTime, sync_ref=SyncRef, acct_id=AcctID}=State) ->
    Self = self(),
    spawn(fun() ->
		  SyncProp = [{<<"Uptime">>, j5_util:uptime(StartTime)}
			      ,{<<"Account-ID">>, AcctID}
			      ,{<<"Server-ID">>, gen_listener:queue_name(Self)}
			      ,{<<"App-Name">>, ?APP_NAME}
			      ,{<<"App-Version">>, ?APP_VERSION}
			     ],
		  wapi_jonny5:publish_sync_req(SyncProp)
	  end),
    {noreply, State#state{sync_ref=erlang:start_timer(?SYNC_TIMER + sync_fudge(), self(), sync)}};

handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    ?LOG_SYS("Unhandled message: ~p", [_Info]),
    {noreply, State}.

handle_event(_JObj, #state{acct_id=_AcctId}=_State) ->
    ?LOG("Acct: ~s received jobj for acctid ~s", [_AcctId, wh_json:get_value(<<"Account-ID">>, _JObj)]),
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
terminate(_Reason, #state{acct_id=AcctID}) ->
    j5_util:store_account_handler(AcctID, undefined).

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

-spec get_trunks_available/2 :: (ne_binary(), 'account' | 'ts') -> {'error', 'not_found'} |
								   {'undefined' | non_neg_integer()
								    ,'undefined' | non_neg_integer()
								    ,'undefined' | float()
								    ,'account' | 'ts'
								   }.
get_trunks_available(AcctID, account) ->
    case couch_mgr:get_results(whapps_util:get_db_name(AcctID, encoded), <<"limits/crossbar_listing">>, [{<<"include_docs">>, true}]) of
	{ok, []} ->
	    ?LOG("No results from view, trying ts doc"),
	    get_trunks_available(AcctID, ts);
	{error, not_found} ->
	    ?LOG("Error loading view, trying ts doc"),
	    get_trunks_available(AcctID, ts);
	{ok, [JObj|_]} ->
	    ?LOG("View result retrieved"),
	    get_account_values(JObj)
    end;
get_trunks_available(AcctID, ts) ->
    case couch_mgr:open_doc(<<"ts">>, AcctID) of
	{error, not_found}=E ->
	    ?LOG_SYS("No account found in ts: ~s", [AcctID]),
	    E;
	{ok, JObj} ->
	    get_ts_values(JObj)
    end.

get_ts_values(JObj) ->
    Acct = wh_json:get_value(<<"account">>, JObj, wh_json:new()),
    Credits = wh_json:get_value(<<"credits">>, Acct, wh_json:new()),

    Trunks = wh_json:get_integer_value(<<"trunks">>, Acct),
    InboundTrunks = wh_json:get_integer_value(<<"inbound_trunks">>, Acct),
    Prepay = ?DOLLARS_TO_UNITS(wh_json:get_float_value(<<"prepay">>, Credits, 0.0)),
    ?LOG_SYS("Found ts trunk levels: ~p two way, ~p inbound, and $ ~p prepay", [Trunks, InboundTrunks, Prepay]),
    {Trunks, InboundTrunks, Prepay, ts}.

get_account_values(JObj) ->
    Trunks = wh_json:get_integer_value(<<"trunks">>, JObj),
    InboundTrunks = wh_json:get_integer_value(<<"inbound_trunks">>, JObj),
    Prepay = ?DOLLARS_TO_UNITS(wh_json:get_float_value(<<"prepay">>, JObj)),

    ?LOG_SYS("Found trunk levels: ~p two way, ~p inbound, and $ ~p prepay", [Trunks, InboundTrunks, Prepay]),
    {Trunks, InboundTrunks, Prepay, account}.

-spec try_inbound_then_twoway/2 :: (ne_binary(), #state{}) -> {{boolean(), proplist()}, #state{}}.
try_inbound_then_twoway(CallID, State) ->
    case try_inbound(CallID, State) of
	{{true, _}, _}=Resp ->
	    ?LOG_END(CallID, "Inbound call authorized with inbound trunk", []),
	    Resp;
	{{false, _}, State2} ->
	    case try_twoway(CallID, State2) of
		{{true, _}, _}=Resp ->
		    ?LOG_END(CallID, "Inbound call authorized using a two-way trunk", []),
		    Resp;
		{{false, _}, State3} ->
		    try_prepay(CallID, State3)
	    end
    end.

-spec try_twoway/2 :: (ne_binary(), #state{}) -> {{boolean(), proplist()}, #state{}}.
try_twoway(_CallID, #state{two_way=T}=State) when T < 1 ->
    ?LOG_SYS(_CallID, "Failed to authz a two-way trunk", []),
    {{false, []}, State#state{two_way=0}};
try_twoway(CallID, #state{two_way=Two, trunks_in_use=Dict, ledger_db=DB}=State) ->
    ?LOG_SYS(CallID, "Authz a two-way trunk", []),
    monitor_call(CallID, DB, two_way),

    {{true, [{<<"Trunk-Type">>, <<"two_way">>}]}
     ,State#state{two_way=Two-1, trunks_in_use=dict:store(CallID, twoway, Dict)}
    }.

-spec try_inbound/2 :: (ne_binary(), #state{}) -> {{boolean(), proplist()}, #state{}}.
try_inbound(_CallID, #state{inbound=I}=State) when I < 1 ->
    ?LOG_SYS(_CallID, "Failed to authz an inbound_only trunk", []),
    {{false, []}, State#state{inbound=0}};
try_inbound(CallID, #state{inbound=In, trunks_in_use=Dict, ledger_db=DB}=State) ->
    ?LOG_SYS(CallID, "Authz an inbound_only trunk", []),
    monitor_call(CallID, DB, inbound),
    {{true, [{<<"Trunk-Type">>, <<"inbound">>}]}
     ,State#state{inbound=In-1, trunks_in_use=dict:store(CallID, inbound, Dict)}
    }.

-spec try_prepay/2 :: (ne_binary(), #state{}) -> {{boolean(), proplist()}, #state{}}.
try_prepay(_CallID, #state{prepay=Pre}=State) when Pre =< ?PER_MIN_MIN ->
    ?LOG_SYS(_CallID, "Failed to authz a per_min trunk", []),

    %% Send Email to account holder warning of low Prepay?

    {{false, [{<<"Error">>, <<"Insufficient Funds">>}]}, State};
try_prepay(CallID, #state{acct_id=AcctId, prepay=Prepay, trunks_in_use=Dict, ledger_db=LedgerDB}=State) ->
    case jonny5_listener:is_blacklisted(AcctId) of
	{true, Reason} ->
	    ?LOG_SYS(CallID, "Authz false for per_min: ~s", [Reason]),
	    {{false, [{<<"Error">>, Reason}]}, State};
	false ->
	    PrepayLeft = Prepay - ?PER_MIN_MIN,
	    ?LOG_SYS(CallID, "Authz a per_min trunk; ~b prepay left, ~b charged up-front", [PrepayLeft, ?PER_MIN_MIN]),
	    {ok, Pid} = monitor_call(CallID, LedgerDB, per_min, ?PER_MIN_MIN),
	    {{true, [{<<"Trunk-Type">>, <<"per_min">>}]}
	     ,State#state{trunks_in_use=dict:store(CallID, {per_min, Pid}, Dict), prepay=PrepayLeft}
	    }
    end.

-spec monitor_call/3 :: (ne_binary(), ne_binary(), call_types()) -> {'ok', pid()}.
monitor_call(CallID, LedgerDB, CallType) ->
    monitor_call(CallID, LedgerDB, CallType, 0).
monitor_call(CallID, LedgerDB, CallType, Debit) ->
    gen_listener:add_binding(self(), call, [{callid, CallID}]),
    j5_util:write_debit_to_ledger(LedgerDB, CallID, CallType, Debit, 0),
    j5_call_monitor_sup:start_monitor(CallID, LedgerDB, CallType, self()).

-spec unmonitor_call/1 :: (ne_binary()) -> 'ok'.
unmonitor_call(CallID) ->
    gen_listener:rm_binding(self(), call, [{callid, CallID}]).

-spec process_call_event/3 :: (ne_binary(), json_object(), dict()) -> 'ignore' | {'release', 'twoway' | 'inbound', dict()}.
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

-spec release_trunk/2 :: (ne_binary(), dict()) -> 'ignore' | {'release', 'twoway' | 'inbound', dict()}.
release_trunk(CallID, Dict) ->
    case dict:find(CallID, Dict) of
	error ->
	    ?LOG_SYS(CallID, "Call is unknown to us", []),
	    ignore;
	{ok, TrunkType} ->
	    {release, TrunkType, dict:erase(CallID, Dict)}
    end.

%% Match +1XXXYYYZZZZ as US-48; all others are not
is_us48(<<"+1", Rest/binary>>) when erlang:byte_size(Rest) =:= 10 -> true;
%% extension dialing
is_us48(Bin) when erlang:byte_size(Bin) < 7 -> true;
is_us48(_) -> false.

-spec send_levels_resp/3 :: (json_object(), #state{}, fun((ne_binary(), proplist() | json_object()) -> 'ok')) -> no_return().
send_levels_resp(JObj, #state{two_way=Two, inbound=In, trunks_in_use=Dict, acct_id=AcctID
			      ,max_inbound=MaxIn, max_two_way=MaxTwo, start_time=StartTime
			      ,prepay=Prepay
			     }, PublishFun) ->
    SyncResp = [{<<"Uptime">>, j5_util:uptime(StartTime)}
		,{<<"Account-ID">>, AcctID}
		,{<<"Prepay">>, ?UNITS_TO_DOLLARS(Prepay)}
		,{<<"Two-Way">>, Two}
		,{<<"Inbound">>, In}
		,{<<"Max-Two-Way">>, MaxTwo}
		,{<<"Max-Inbound">>, MaxIn}
		,{<<"Server-ID">>, <<>>}
		,{<<"Trunks">>, [wh_json:from_list([{<<"Call-ID">>, CallID}, {<<"Type">>, Type}]) || {CallID, Type} <- dict:to_list(Dict)]}
		,{<<"App-Version">>, ?APP_VERSION}
		,{<<"App-Name">>, ?APP_NAME}
		,{<<"Node">>, wh_util:to_binary(node())}
	       ],
    PublishFun(wh_json:get_value(<<"Server-ID">>, JObj), SyncResp).

-spec sync_fudge/0 :: () -> 1..?SYNC_TIMER.
sync_fudge() ->
    crypto:rand_uniform(1, ?SYNC_TIMER).

try_update_value(undefined, Default) ->
    Default;
try_update_value(New, _) ->
    New.
