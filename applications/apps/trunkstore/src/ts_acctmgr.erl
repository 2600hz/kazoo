%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, James Aimonetti
%%% @doc
%%% Manage the account documents and provide specific API access to
%%% their contents for Trunkstore components (ts_route, etc).
%%% @end
%%% Created :  3 Jan 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ts_acctmgr).

-behaviour(gen_server).

%% API
-export([start_link/0, update_views/0]).

%% Data Access API
-export([has_credit/1, has_credit/2 %% has_credit(AcctId[, Amount]) - check if account has > Amount credit (0 if Amount isn't specified)
	 ,has_flatrates/1 %% has_flatrates(AcctId) - check if account has a free flatrate trunk
	 ,reserve_trunk/4 %% reserve_trunk(AcctId, CallID, Amount, FlatRateEnabled) - only reserve if avail_credit > Amt (0 if unspecified)
	 ,release_trunk/3 %% release_trunk(AcctId, CallID[, Amount]) - release trunk, deducting Amt from account balance
	 ,copy_reserve_trunk/4 %% when a failover trunk gets the b-leg callid resolved, copy its reserve doc to the b-leg callid
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ts.hrl").

-define(SERVER, ?MODULE).
-define(TS_ACCTMGR_VIEWS, ["accounts.json", "credit.json", "trunks.json"]).
-define(DOLLARS_TO_UNITS(X), whistle_util:to_integer(X * 100000)). %% $1.00 = 100,000 thousand-ths of a cent
-define(CENTS_TO_UNITS(X), whistle_util:to_integer(X * 1000)). %% 100 cents = 100,000 thousand-ths of a cent
-define(UNITS_TO_DOLLARS(X), whistle_util:to_binary(X / 100000)). %% $1.00 = 100,000 thousand-ths of a cent
-define(TS_USAGE_PREFIX, <<"ts_usage">>).

-define(ACTIVE_CALL_TIMEOUT, 1000).

-record(state, {
	  current_write_db = ""
	  ,current_read_db = "" %% possibly different during transition from yesterday to today
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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

update_views() ->
    gen_server:cast(?SERVER, update_views).

%%%===================================================================
%%% Data Access API
%%%===================================================================
-spec(has_credit/1 :: (Acct :: binary()) -> boolean()).
has_credit(Acct) ->
    has_credit(Acct, 0).

%% Does the account have enough credit to cover Amt
-spec(has_credit/2 :: (Acct :: binary(), Amt :: integer()) -> boolean() | tuple(error, no_account)).
has_credit(<<>>, _) ->
    {error, no_account};
has_credit(Acct, Amt) ->
    gen_server:call(?SERVER, {has_credit, whistle_util:to_binary(Acct), [Amt]}, infinity).

-spec(has_flatrates/1 :: (Acct :: binary()) -> boolean() | tuple(error, no_account)).
has_flatrates(<<>>) ->
    {error, no_account};
has_flatrates(Acct) ->
    gen_server:call(?SERVER, {has_flatrates, whistle_util:to_binary(Acct)}).

%% try to reserve a trunk
%% first try to reserve a flat_rate trunk; if none are available, try a per_min trunk;
%% if the Amt is more than available credit, return error
-spec(reserve_trunk/4 :: (Acct :: binary(), CallID :: binary(), Amt :: float() | integer(), FRE :: boolean()) ->
			      tuple(ok, flat_rate | per_min) | tuple(error, no_account | no_callid | entry_exists | no_funds | not_found)).
reserve_trunk(<<>>, _, _, _) ->
    {error, no_account};
reserve_trunk(_, <<>>, _, _) ->
    {error, no_callid};
reserve_trunk(Acct, CallID, Amt, FRE) ->
    gen_server:call(?SERVER, {reserve_trunk, whistle_util:to_binary(Acct), [CallID, Amt, FRE]}, infinity).

%% when an a-leg CALLID-failover is resolved into a B-leg CallID, transfer the type of trunk to the B-leg CallID
-spec(copy_reserve_trunk/4 :: (AcctID :: binary(), ACallID :: binary(), BCallID :: binary(), Amt :: float() | integer()) -> ok).
copy_reserve_trunk(AcctID, ACallID, BCallID, Amt) ->
    gen_server:call(?SERVER, {copy_reserve_trunk, whistle_util:to_binary(AcctID), [ACallID, BCallID, Amt]}, infinity).

%% release a reserved trunk
%% pass the account and the callid from the reserve_trunk/2 call to release the trunk back to the account
-spec(release_trunk/3 :: (Acct :: binary(), CallID :: binary(), Amt :: float() | integer()) -> ok | tuple(error, no_account | no_callid)).
release_trunk(<<>>, _, _) ->
    {error, no_account};
release_trunk(_, <<>>, _) ->
    {error, no_callid};
release_trunk(Acct, CallID, Amt) ->
    gen_server:cast(?SERVER, {release_trunk, whistle_util:to_binary(Acct), [CallID,Amt]}),
    ok.

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
init(_) ->
    {_, {H,Min,S}} = calendar:universal_time(),

    DB = ts_util:todays_db_name(?TS_USAGE_PREFIX),

    logger:format_log(info, "TS_ACCTMGR: Starting DB ~p~n", [DB]),
    couch_mgr:db_create(DB),

    MillisecsToMidnight = ?MILLISECS_PER_DAY - timer:hms(H,Min,S),
    logger:format_log(info, "TS_ACCTMGR: ms till Midnight: ~p (~p:~p:~p)~n", [MillisecsToMidnight, H, Min, S]),
    {ok, _} = timer:send_after(MillisecsToMidnight, ?EOD),

    {ok, #state{
       current_write_db = DB
       ,current_read_db = DB
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
handle_call({has_credit, AcctId, [Amt]}, From, #state{current_write_db=WDB, current_read_db=RDB}=S) ->
    Self = self(),
    spawn(fun() -> load_account(AcctId, WDB, Self), gen_server:reply(From, has_credit(RDB, AcctId, Amt)) end),
    {noreply, S};

handle_call({has_flatrates, AcctId}, From, #state{current_read_db=RDB}=S) ->
    spawn(fun() -> gen_server:reply(From, has_flatrates(RDB, AcctId)) end),
    {noreply, S};

handle_call({reserve_trunk, AcctId, [CallID, Amt, false]}, From, #state{current_write_db=WDB, current_read_db=RDB}=S) ->
    Self = self(),
    logger:format_log(info, "TS_ACCTMGR(~p): reserve per_min for ~p: ~p: ~p~n", [self(), AcctId, CallID, Amt]),
    spawn(fun() ->
		  spawn(fun() -> load_account(AcctId, WDB, Self) end),
		  case has_credit(RDB, AcctId, Amt) of
		      true ->
			  spawn(fun() -> couch_mgr:save_doc(WDB, reserve_doc(AcctId, CallID, per_min)) end),
			  gen_server:reply(From, {ok, per_min});
		      false ->
			  gen_server:reply(From, {error, no_funds})
		  end
	  end),
    {noreply, S};

handle_call({reserve_trunk, AcctId, [CallID, Amt, true]}, From, #state{current_write_db=WDB, current_read_db=RDB}=S) ->
    Self = self(),
    logger:format_log(info, "TS_ACCTMGR(~p): reserve trunk for ~p: ~p: ~p from ~p~n", [self(), AcctId, CallID, Amt, RDB]),
    spawn(fun() ->
		  spawn(fun() -> load_account(AcctId, WDB, Self) end),

		  case couch_mgr:get_results(RDB, <<"accounts/balance">>, [{<<"key">>, AcctId}, {<<"group">>, <<"true">>}, {<<"stale">>, <<"ok">>}]) of
		      {error, not_found}=E ->
			  gen_server:reply(From, E);
		      {ok, []} ->
			  logger:format_log(info, "TS_ACCTMGR.reserve: no_funds for ~p:~p~n", [AcctId, CallID]),
			  gen_server:reply(From, {error, no_account});
		      {ok, [{struct, [{<<"key">>, _}, {<<"value">>, Funds}] }] } ->
			  case wh_json:get_value(<<"trunks">>, Funds, 0) > 0 of
			      true ->
				  spawn(fun() -> couch_mgr:save_doc(WDB, reserve_doc(AcctId, CallID, flat_rate)) end),
				  logger:format_log(info, "TS_ACCTMGR.reserve: flat_rate for ~p:~p~n", [AcctId, CallID]),
				  gen_server:reply(From, {ok, flat_rate});
			      false ->
				  case wh_json:get_value(<<"credit">>, Funds, 0) > Amt of
				      true ->
					  spawn(fun() -> couch_mgr:save_doc(WDB, reserve_doc(AcctId, CallID, per_min)) end),
					  logger:format_log(info, "TS_ACCTMGR.reserve: per_min for ~p:~p~n", [AcctId, CallID]),
					  gen_server:reply(From, {ok, per_min});
				      false ->
					  logger:format_log(info, "TS_ACCTMGR.reserve: no_funds for ~p:~p~n", [AcctId, CallID]),
					  gen_server:reply(From, {error, no_funds})
				  end
			  end
		  end
	  end),
    {noreply, S};
handle_call({copy_reserve_trunk, AcctID, [ACallID, BCallID, Amt]}, From, #state{current_write_db=WDB, current_read_db=RDB}=S) ->
    spawn(fun() ->
		  case trunk_type(RDB, AcctID, ACallID) of
		      non_existant ->
			  case trunk_type(WDB, AcctID, ACallID) of
			      non_existant -> logger:format_log(error, "TS_ACCTMGR(~p): Can't copy data from ~p to ~p for acct ~p, non_existant~n"
							 ,[self(), ACallID, BCallID, AcctID]);
			      per_min -> couch_mgr:save_doc(WDB, release_doc(AcctID, BCallID, per_min, Amt));
			      flat_rate -> couch_mgr:save_doc(WDB, release_doc(AcctID, BCallID, flat_rate))
			  end;
		      per_min -> couch_mgr:save_doc(WDB, release_doc(AcctID, BCallID, per_min, Amt));
		      flat_rate -> couch_mgr:save_doc(WDB, release_doc(AcctID, BCallID, flat_rate))
		  end,
		  gen_server:reply(From, ok)
	  end),
    {noreply, S}.

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
handle_cast(update_views, #state{current_read_db=RDB}=S) ->
    spawn(fun() -> update_views(RDB) end),
    {noreply, S};
handle_cast({release_trunk, AcctId, [CallID,Amt]}, #state{current_write_db=WDB, current_read_db=RDB}=S) ->
    Self = self(),
    spawn(fun() ->
		  logger:format_log(info, "TS_ACCTMGR(~p): Release trunk for ~p:~p ($~p)~n", [self(), AcctId, CallID, Amt]),

		  load_account(AcctId, WDB, Self),

		  case trunk_type(RDB, AcctId, CallID) of
		      non_existant ->
			  case trunk_type(WDB, AcctId, CallID) of
			      non_existant -> logger:format_log(info, "TS_ACCTMGR(~p): Failed to find trunk for release ~p: ~p~n", [self(), AcctId, CallID]);
			      per_min -> couch_mgr:save_doc(WDB, release_doc(AcctId, CallID, per_min, Amt));
			      flat_rate -> couch_mgr:save_doc(WDB, release_doc(AcctId, CallID, flat_rate))
			  end;
		      per_min -> couch_mgr:save_doc(WDB, release_doc(AcctId, CallID, per_min, Amt));
		      flat_rate -> couch_mgr:save_doc(WDB, release_doc(AcctId, CallID, flat_rate))
		  end
	  end),
    {noreply, S}.

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
handle_info(timeout, #state{current_write_db=WDB}=S) ->
    Self = self(),
    logger:format_log(info, "TS_ACCTMGR(~p): Loading accounts~n", [Self]),
    spawn(fun() -> load_views(WDB), load_accounts_from_ts(WDB, Self) end),
    {noreply, S};
handle_info(?EOD, S) ->
    DB = ts_util:todays_db_name(?TS_USAGE_PREFIX),

    {ok, _} = timer:send_after(?MILLISECS_PER_DAY, ?EOD),

    self() ! reconcile_accounts,

    spawn(fun() -> load_views(DB) end),

    {noreply, S#state{
		current_write_db = DB % all new writes should go in new DB, but old DB is needed still
	       }};
handle_info(reconcile_accounts, #state{current_read_db=RDB, current_write_db=WDB}=S) ->
    Self = self(),
    spawn( fun() -> lists:foreach(fun(Acct) ->
					  transfer_acct(Acct, RDB, WDB),
					  transfer_active_calls(Acct, RDB, WDB)
				  end, get_accts(RDB)),
		    %% once active accounts from yesterday are done, make sure all others are in too
		    load_accounts_from_ts(WDB, Self)
	   end),
    {noreply, S#state{current_read_db=WDB}};
handle_info({document_changes, DocID, Changes}, #state{current_write_db=WDB, current_read_db=RDB}=S) ->
    logger:format_log(info, "TS_ACCTMGR(~p): Changes on ~p. ~p~n", [self(), DocID, Changes]),
    spawn(fun() -> update_from_couch(DocID, WDB, RDB) end),
    {noreply, S};
handle_info({document_deleted, AcctId}, S) ->
    logger:format_log(info, "TS_ACCTMGR(~p): Account Doc ~p deleted~n", [self(), AcctId]),
    {noreply, S};
handle_info({change_handler_terminating, DB, Doc}, S) ->
    logger:format_log(info, "TS_ACCTMGR(~p): Change Handler down for ~p ~p~n", [self(), DB, Doc]),
    {noreply, S};
handle_info(_Info, S) ->
    logger:format_log(info, "TS_ACCTMGR(~p): Uhandled info: ~p~n", [self(), _Info]),
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

-spec(load_accounts_from_ts/2 :: (DB :: binary(), Srv :: pid()) -> no_return()).
load_accounts_from_ts(DB, Srv) ->
    case couch_mgr:get_results(?TS_DB, {"accounts", "list"}, []) of
	{error, _} -> ok;
	{ok, []} -> ok;
	{ok, Accts} when is_list(Accts) ->
	    AcctIds = lists:map(fun({struct, A}) -> props:get_value(<<"id">>, A) end, Accts),
	    lists:foreach(fun(Id) -> load_account(Id, DB, Srv) end, AcctIds)
    end.

-spec(has_credit/3 :: (DB :: binary(), AcctId :: binary(), Amt :: integer() | float()) -> boolean()).
has_credit(DB, AcctId, Amt) ->
    credit_available(DB, AcctId) > ?DOLLARS_TO_UNITS(Amt).

-spec(credit_available/2 :: (DB :: binary(), AcctId :: binary()) -> integer()).
credit_available(DB, AcctId) ->
    case couch_mgr:get_results(DB, <<"credit/credit_available">>, [{<<"group">>, true}, {<<"key">>, AcctId}]) of
	{ok, []} -> 0;
	{ok, [{struct, [{<<"key">>, _}, {<<"value">>, Avail}] }] } -> Avail
    end.

-spec(has_flatrates/2 :: (DB :: binary(), AcctId :: binary()) -> boolean()).
has_flatrates(DB, AcctId) ->
    flatrates_available(DB, AcctId) > 0.

-spec(flatrates_available/2 :: (DB :: binary(), AcctId :: binary()) -> integer()).
flatrates_available(DB, AcctId) ->
    case couch_mgr:get_results(DB, <<"trunks/flat_rates_available">>, [{<<"group">>, true}, {<<"key">>, AcctId}]) of
	{ok, []} -> 0;
	{ok, [{struct, [{<<"key">>, _}, {<<"value">>, Avail}] }] } -> Avail
    end.

-spec(trunk_type/3 :: (RDB :: binary(), AcctId :: binary(), CallID :: binary()) -> flat_rate | per_min | non_existant).
trunk_type(DB, AcctId, CallID) ->
    case couch_mgr:get_results(DB, <<"trunks/trunk_type">>, [ {<<"key">>, [AcctId, CallID]}, {<<"group">>, true}]) of
	{ok, []} -> non_existant;
	{ok, [{struct, [{<<"key">>,_}, {<<"value">>, <<"flat_rate">>}] }] } -> flat_rate;
	{ok, [{struct, [{<<"key">>,_}, {<<"value">>, <<"per_min">>}] }] } -> per_min
    end.

%% -spec(trunk_status/3 :: (DB :: binary(), AcctId :: binary(), CallID :: binary()) -> active | inactive).
%% trunk_status(DB, AcctId, CallID) ->
%%     case couch_mgr:get_results(DB, {"trunks", "trunk_status"}, [ {<<"key">>, [AcctId, CallID]}, {<<"group_level">>, <<"2">>}]) of
%% 	{ok, []} -> in_active;
%% 	{ok, [{struct, [{<<"key">>,_},{<<"value">>,<<"active">>}] }] } -> active;
%% 	{ok, [{struct, [{<<"key">>,_},{<<"value">>,<<"inactive">>}] }] } -> inactive
%%     end.

%% -spec(trunks_available/2 :: (DB :: binary(), AcctId :: binary()) -> integer()).
%% trunks_available(DB, AcctId) ->
%%     case couch_mgr:get_results(DB, {"trunks", "flat_rates_available"}, [{<<"key">>, AcctId}, {<<"group">>, <<"true">>}]) of
%% 	{ok, []} -> 0;
%% 	{ok, [{struct, [{<<"key">>,_},{<<"value">>, Ts}] }] } -> whistle_util:to_integer(Ts)
%%     end.

%% should be the diffs from the last account update to now
account_doc(AcctId, Credit, Trunks) ->
    credit_doc(AcctId, Credit, Trunks, [{<<"_id">>, AcctId}
					,{<<"doc_type">>, <<"account">>}
				       ]).

reserve_doc(AcctId, CallID, flat_rate) ->
    debit_doc(AcctId, [{<<"_id">>, <<"reserve-", CallID/binary, "-", AcctId/binary>>}
		       ,{<<"call_id">>, CallID}
		       ,{<<"trunk_type">>, flat_rate}
		       ,{<<"trunks">>, 1}
		       ,{<<"amount">>, 0}
		       ,{<<"doc_type">>, <<"reserve">>}
		      ]);
reserve_doc(AcctId, CallID, per_min) ->
    debit_doc(AcctId, [{<<"_id">>, <<"reserve-", CallID/binary, "-", AcctId/binary>>}
		       ,{<<"call_id">>, CallID}
		       ,{<<"trunk_type">>, per_min}
		       ,{<<"amount">>, 0}
		       ,{<<"doc_type">>, <<"reserve">>}
		      ]).

release_doc(AcctId, CallID, flat_rate) ->
    credit_doc(AcctId, 0, 1, [{<<"_id">>, <<"release-", CallID/binary, "-", AcctId/binary>>}
			      ,{<<"call_id">>, CallID}
			      ,{<<"trunk_type">>, flat_rate}
			      ,{<<"doc_type">>, <<"release">>}
			     ]).

release_doc(AcctId, CallID, per_min, Amt) ->
    debit_doc(AcctId, [{<<"_id">>, <<"release-", CallID/binary, "-", AcctId/binary>>}
		       ,{<<"call_id">>, CallID}
		       ,{<<"trunk_type">>, per_min}
		       ,{<<"amount">>, ?DOLLARS_TO_UNITS(Amt)}
		       ,{<<"doc_type">>, <<"release">>}
		      ]).

release_error_doc(AcctId, CallID, flat_rate) ->
    credit_doc(AcctId, 0, 1, [{<<"_id">>, <<"release-", CallID/binary, "-", AcctId/binary>>}
			      ,{<<"call_id">>, CallID}
			      ,{<<"trunk_type">>, flat_rate}
			      ,{<<"doc_type">>, <<"release">>}
			      ,{<<"release_error">>, true}
			     ]).

release_error_doc(AcctId, CallID, per_min, Amt) ->
    debit_doc(AcctId, [{<<"_id">>, <<"release-", CallID/binary, "-", AcctId/binary>>}
		       ,{<<"call_id">>, CallID}
		       ,{<<"trunk_type">>, per_min}
		       ,{<<"amount">>, ?DOLLARS_TO_UNITS(Amt)}
		       ,{<<"doc_type">>, <<"release">>}
		       ,{<<"release_error">>, true}
		      ]).

credit_doc(AcctId, Credit, Trunks, Extra) ->
    [{<<"acct_id">>, AcctId}
     ,{<<"amount">>, Credit}
     ,{<<"trunks">>, Trunks}
     ,{<<"type">>, <<"credit">>}
     | Extra
    ].

debit_doc(AcctId, Extra) ->
    [{<<"acct_id">>, AcctId}
     ,{<<"type">>, <<"debit">>}
     | Extra
    ].

-spec(get_accts/1 :: (DB :: binary()) -> list(binary())).
get_accts(DB) ->
    case couch_mgr:get_results(DB, {"accounts", "listing"}, [{<<"group">>, <<"true">>}]) of
	{ok, []} -> [];
	{ok, AcctsDoc} -> lists:map(fun(AcctJObj) -> wh_json:get_value(<<"key">>, AcctJObj) end, AcctsDoc)
    end.

transfer_acct(AcctId, RDB, WDB) ->
    %% read account balance, from RDB
    Bal = credit_available(RDB, AcctId),
    {ok, {struct, Acct}} = couch_mgr:open_doc(RDB, AcctId),
    Acct1 = [ {<<"amount">>, Bal} | lists:keydelete(<<"amount">>, 1, Acct)],

    logger:format_log(info, "TS_ACCTMGR.transfer: Read from ~p and write to ~p: ~p has ~p balance~n", [RDB, WDB, AcctId, ?UNITS_TO_DOLLARS(Bal)]),

    %% create credit entry in WDB for balance/trunks
    {ok, _} = couch_mgr:save_doc(WDB, {struct, lists:keydelete(<<"_rev">>, 1, Acct1)}),

    %% update info_* doc with account balance
    spawn(fun() -> update_account(AcctId, Bal) end).

transfer_active_calls(AcctId, RDB, WDB) ->
    case couch_mgr:get_results(RDB, <<"trunks/trunk_status">>, [{<<"startkey">>, [AcctId]}, {<<"endkey">>, [AcctId, <<"true">>]}, {<<"group_level">>, <<"2">>}]) of
	{ok, []} -> logger:format_log(info, "TS_ACCTMGR.trans_active: No active for ~p~n", [AcctId]);
	{ok, Calls} when is_list(Calls) ->
	    lists:foreach(fun({struct, [{<<"key">>, [_Acct, CallId]}, {<<"value">>, <<"active">>}] }) ->
				  spawn(fun() ->
						case is_call_active(CallId) of
						    true ->
							NewDoc = reserve_doc(AcctId, CallId, trunk_type(RDB, AcctId, CallId)),
							logger:format_log(info, "TS_ACCTMGR.trans_active: Moving ~p over~n", [CallId]),
							couch_mgr:save_doc(WDB, {struct, NewDoc});
						    false ->
							release_trunk_error(AcctId, CallId, RDB)
						end
					end);
			     (_) -> ok
			  end, Calls)
    end.

%% When TS updates an account, find the diff and create the appropriate entry (debit or credit).
update_from_couch(AcctId, WDB, RDB) ->
    {ok, JObj} = couch_mgr:open_doc(?TS_DB, AcctId),

    Acct = wh_json:get_value(<<"account">>, JObj, ?EMPTY_JSON_OBJECT),
    Credits = wh_json:get_value(<<"credits">>, Acct, ?EMPTY_JSON_OBJECT),
    Balance = ?DOLLARS_TO_UNITS(whistle_util:to_float(wh_json:get_value(<<"prepay">>, Credits, 0.0))),
    Trunks = whistle_util:to_integer(wh_json:get_value(<<"trunks">>, Acct, 0)),

    {ok, UsageJObj} = couch_mgr:open_doc(RDB, AcctId),
    T0 = wh_json:get_value(<<"trunks">>, UsageJObj),
    C0 = wh_json:get_value(<<"amount">>, UsageJObj),

    %% account trunks minus what the day started with to get diff
    %% So 5->7 in account, started day with 5, credit 2 trunks
    %%    7->5 in account, started day with 7, debit 2 trunks
    %% same with credit
    case (Trunks - T0) of
	T when T < 0 -> couch_mgr:save_doc(WDB, debit_doc(AcctId, [{<<"trunks">>, T + T0}]));
	T when T =:= 0 -> ok;
	T -> couch_mgr:save_doc(WDB, credit_doc(AcctId, 0, T + T0, []))
    end,

    case (Balance - C0) of
	C when C < 0 -> couch_mgr:save_doc(WDB, debit_doc(AcctId, [{<<"trunks">>, C0 + C}]));
	C when C =:= 0 -> ok;
	C -> couch_mgr:save_doc(WDB, credit_doc(AcctId, C0 + C, 0, []))
    end.

update_account(AcctId, Bal) ->
    {ok, {struct, Doc}} = couch_mgr:open_doc(?TS_DB, AcctId),
    {struct, Acct} = props:get_value(<<"account">>, Doc, ?EMPTY_JSON_OBJECT),
    {struct, Credits} = props:get_value(<<"credits">>, Acct, ?EMPTY_JSON_OBJECT),
    Credits1 = [ {<<"prepay">>, ?UNITS_TO_DOLLARS(Bal)} | lists:keydelete(<<"prepay">>, 1, Credits)],
    Acct1 = [ {<<"credits">>, {struct, Credits1}} | lists:keydelete(<<"credits">>, 1, Acct)],
    Doc1 = [ {<<"account">>, {struct, Acct1}} | lists:keydelete(<<"account">>, 1, Doc)],
    couch_mgr:save_doc(?TS_DB, Doc1).

load_account(AcctId, DB, Srv) ->
    case wh_cache:fetch({ts_acctmgr, AcctId, DB}) of
	{ok, _} -> ok;
	{error, not_found} ->
	    case couch_mgr:open_doc(?TS_DB, AcctId) of
		{error, not_found} -> ok;
		{ok, JObj} ->
		    Acct = wh_json:get_value(<<"account">>, JObj, ?EMPTY_JSON_OBJECT),
		    Credits = wh_json:get_value(<<"credits">>, Acct, ?EMPTY_JSON_OBJECT),
		    Balance = ?DOLLARS_TO_UNITS(whistle_util:to_float(wh_json:get_value(<<"prepay">>, Credits, 0.0))),
		    Trunks = whistle_util:to_integer(wh_json:get_value(<<"trunks">>, Acct, 0)),
		    couch_mgr:save_doc(DB, account_doc(AcctId, Balance, Trunks)),
		    couch_mgr:add_change_handler(?TS_DB, AcctId, Srv),
		    wh_cache:store({ts_acctmgr, AcctId, DB}, true, 5)
	    end
    end.

load_views(DB) ->
    couch_mgr:db_create(DB),
    lists:foreach(fun(Name) ->
			  case couch_mgr:load_doc_from_file(DB, trunkstore, Name) of
			      {ok, _} -> ok;
			      {error, _} -> couch_mgr:update_doc_from_file(DB, trunkstore, Name)
			  end
		  end, ?TS_ACCTMGR_VIEWS).

update_views(DB) ->
    lists:foreach(fun(File) ->
			  case couch_mgr:update_doc_from_file(DB, trunkstore, File) of
			      {ok, _} -> logger:format_log(info, "Updating ~s: success~n", [File]);
			      {error, Reason} -> logger:format_log(info, "Updating ~s: error ~p~n", [File, Reason])
			  end
		  end, ?TS_ACCTMGR_VIEWS).

%% Sample Data importable via #> curl -X POST -d@sample.json.data http://localhost:5984/DB_NAME/_bulk_docs --header "Content-Type: application/json"

is_call_active(CallID) ->
    Q = amqp_util:new_targeted_queue(),
    amqp_util:bind_q_to_targeted(Q),

    Req = [{<<"Call-ID">>, CallID}
	   | whistle_api:default_headers(Q, <<"call_event">>, <<"status_req">>, <<"ts_acctmgr">>, <<>>)],

    {ok, JSON} = whistle_api:call_status_req(Req),
    amqp_util:callevt_publish(CallID, JSON, status_req),

    is_call_active_loop().

is_call_active_loop() ->
    receive
	{_, #amqp_msg{payload = Payload}} ->
	    {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
	    whistle_api:call_status_resp_v(Prop);
	_ ->
	    is_call_active_loop()
    after ?ACTIVE_CALL_TIMEOUT ->
	    false
    end.

release_trunk_error(AcctId, CallID, DB) ->
    logger:format_log(info, "TS_ACCTMGR(~p).release_error: Release trunk for ~p:~p~n", [self(), AcctId, CallID]),

    case trunk_type(DB, AcctId, CallID) of
	non_existant -> logger:format_log(info, "TS_ACCTMGR.release_error: Failed to find trunk for release ~p: ~p~n", [self(), AcctId, CallID]);
	flat_rate -> couch_mgr:save_doc(DB, release_error_doc(AcctId, CallID, flat_rate));
	per_min ->
	    Amt = case ts_cdr:fetch_cdr(binary:replace(DB, ?TS_USAGE_PREFIX, ?TS_CDR_PREFIX), CallID) of
		      {error, not_found} -> 0;
		      {ok, CDR} -> whistle_util:to_integer(wh_json:get_value(<<"Billing-Seconds">>, CDR, 0))
		  end,
	    couch_mgr:save_doc(DB, release_error_doc(AcctId, CallID, per_min, Amt))
    end.
