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
-export([start_link/0]).

%% Data Access API
-export([has_credit/1, has_credit/2 %% has_credit(AcctId[, Amount]) - check if account has > Amount credit (0 if Amount isn't specified)
	 ,reserve_trunk/2, reserve_trunk/3 %% reserve_trunk(AcctId, CallID[, Amount]) - only reserve if avail_credit > Amt (0 if unspecified)
	 ,release_trunk/2, release_trunk/3 %% release_trunk(AcctId, CallID[, Amount]) - release trunk, deducting Amt from account balance
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ts.hrl").

-import(logger, [format_log/3]).

-define(SERVER, ?MODULE).
-define(DOLLARS_TO_UNITS(X), whistle_util:to_integer(X * 100000)). %% $1.00 = 100,000 thousand-ths of a cent
-define(CENTS_TO_UNITS(X), whistle_util:to_integer(X * 1000)). %% 100 cents = 100,000 thousand-ths of a cent
-define(UNITS_TO_DOLLARS(X), whistle_util:to_binary(X / 100000)). %% $1.00 = 100,000 thousand-ths of a cent
-define(MILLISECS_PER_DAY, 1000 * 60 * 60 * 24).
-define(EOD, end_of_day).

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

%%%===================================================================
%%% Data Access API
%%%===================================================================
-spec(has_credit/1 :: (Acct :: binary()) -> boolean()).
has_credit(Acct) ->
    has_credit(Acct, 0).

%% Does the account have enough credit to cover Amt
-spec(has_credit/2 :: (Acct :: binary(), Amt :: integer()) -> boolean()).
has_credit(Acct, Amt) ->
    gen_server:call(?SERVER, {has_credit, whistle_util:to_binary(Acct), [Amt]}, infinity).

%% try to reserve a trunk
%% first try to reserve a flat_rate trunk; if none are available, try a per_min trunk;
%% if the Amt is more than available credit, return error
-spec(reserve_trunk/2 :: (Acct :: binary(), CallID :: binary()) -> tuple(ok, flat_rate | per_min) | tuple(error, no_funds)).
reserve_trunk(Acct, CallID) ->
    reserve_trunk(Acct, CallID, 0).

-spec(reserve_trunk/3 :: (Acct :: binary(), CallID :: binary(), Amt :: float() | integer()) -> tuple(ok, flat_rate | per_min) | tuple(error, term())).
reserve_trunk(Acct, CallID, Amt) ->
    gen_server:call(?SERVER, {reserve_trunk, whistle_util:to_binary(Acct), [CallID, Amt]}, infinity).

%% release a reserved trunk
%% pass the account and the callid from the reserve_trunk/2 call to release the trunk back to the account
-spec(release_trunk/2 :: (Acct :: binary(), CallID :: binary()) -> no_return()).
release_trunk(Acct, CallID) ->
    release_trunk(Acct, CallID, 0).

-spec(release_trunk/3 :: (Acct :: binary(), CallID :: binary(), Amt :: float() | integer()) -> no_return()).
release_trunk(Acct, CallID, Amt) ->
    gen_server:cast(?SERVER, {release_trunk, whistle_util:to_binary(Acct), [CallID,Amt]}).

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
init([]) ->
    {_, {H,Min,S}} = calendar:universal_time(),
    
    DB = todays_db_name(),

    format_log(info, "TS_ACCTMGR: Starting DB ~p~n", [DB]),

    MillisecsToMidnight = ?MILLISECS_PER_DAY - timer:hms(H,Min,S),
    timer:send_after(MillisecsToMidnight, ?EOD),

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
handle_call({has_credit, AcctId, [Amt]}, _From, #state{current_write_db=WDB, current_read_db=RDB}=S) ->
    load_account(AcctId, WDB),
    couch_mgr:add_change_handler(?TS_DB, AcctId),
    {reply, has_credit(RDB, AcctId, Amt), S};
handle_call({reserve_trunk, AcctId, [CallID, Amt]}, _From, #state{current_write_db=WDB, current_read_db=RDB}=S) ->
    format_log(info, "TS_ACCTMGR(~p): Reserve trunk for ~p:~p ($~p)~n", [self(), AcctId, CallID, Amt]),
    load_account(AcctId, WDB),
    couch_mgr:add_change_handler(?TS_DB, AcctId),
    {DebitDoc, Type} = case couch_mgr:get_results(RDB, {"trunks", "flat_rates_available"}, [{<<"key">>, AcctId}, {<<"group">>, <<"true">>}]) of
			   [] ->
			       case has_credit(RDB, AcctId, Amt) of
				   true -> {reserve_doc(AcctId, CallID, per_min), per_min};
				   false -> {[], no_funds}
			       end;
			   [{[{<<"key">>, _}, {<<"value">>, 0}]}] ->
			       case has_credit(RDB, AcctId, Amt) of
				   true -> {reserve_doc(AcctId, CallID, per_min), per_min};
				   false -> no_funds
			       end;
			   [{[{<<"key">>, _}, {<<"value">>, _}]}] ->
			       {reserve_doc(AcctId, CallID, flat_rate), flat_rate}
		       end,
    case Type of
	no_funds -> {reply, {error, no_funds}, S};
	_ ->
	    case couch_mgr:save_doc(WDB, DebitDoc) of
		{ok, _} -> {reply, {ok, Type}, S};
		{error, conflict} -> {reply, {error, entry_exists}, S}
	    end
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
handle_cast({release_trunk, AcctId, [CallID,Amt]}, #state{current_write_db=WDB, current_read_db=RDB}=S) ->
    format_log(info, "TS_ACCTMGR(~p): Release trunk for ~p:~p ($~p)~n", [self(), AcctId, CallID, Amt]),
    load_account(AcctId, WDB),
    couch_mgr:add_change_handler(?TS_DB, AcctId),
    case trunk_type(RDB, AcctId, CallID) of
	non_existant ->
	    case trunk_type(WDB, AcctId, CallID) of
		non_existant -> format_log(info, "TS_ACCTMGR(~p): Failed to find trunk for release ~p: ~p~n", [self(), AcctId, CallID]);
		per_min -> couch_mgr:save_doc(WDB, release_doc(AcctId, CallID, per_min, Amt));
		flat_rate -> couch_mgr:save_doc(WDB, release_doc(AcctId, CallID, flat_rate))
	    end;
	per_min -> couch_mgr:save_doc(WDB, release_doc(AcctId, CallID, per_min, Amt));
	flat_rate -> couch_mgr:save_doc(WDB, release_doc(AcctId, CallID, flat_rate))
    end,
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
    load_views(WDB),
    load_accounts_from_ts(WDB),
    {noreply, S};
handle_info(?EOD, S) ->
    DB = todays_db_name(),

    timer:send_after(?MILLISECS_PER_DAY, ?EOD),

    self() ! reconcile_accounts,

    load_views(DB),
    load_accounts_from_ts(DB),

    {noreply, S#state{
		current_write_db = DB % all new writes should go in new DB, but old DB is needed still
	       }};
handle_info(reconcile_accounts, #state{current_read_db=RDB, current_write_db=WDB}=S) ->
    spawn( fun() -> lists:foreach(fun(Acct) -> transfer_acct(Acct, RDB, WDB), transfer_active_calls(Acct, RDB, WDB) end, get_accts(RDB)) end),
    {noreply, S#state{current_read_db=WDB}};
handle_info({document_changes, DocID, Changes}, #state{current_write_db=WDB, current_read_db=RDB}=S) ->
    format_log(info, "TS_ACCTMGR(~p): Changes on ~p. ~p~n", [self(), DocID, Changes]),
    update_from_couch(DocID, WDB, RDB),
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

-spec(load_accounts_from_ts/1 :: (DB :: binary()) -> no_return()).
load_accounts_from_ts(DB) ->
    case couch_mgr:get_results(?TS_DB, {"accounts", "list"}, []) of
	{error, _} -> ok;
	{ok, []} -> ok;
	{ok, Accts} when is_list(Accts) ->
	    AcctIds = lists:map(fun({struct, A}) -> props:get_value(<<"id">>, A) end, Accts),
	    lists:foreach(fun(Id) -> load_account(Id, DB) end, AcctIds),
	    start_change_handlers(AcctIds)
    end.

start_change_handlers([]) -> ok;
start_change_handlers([I | Ids]) ->
    couch_mgr:add_change_handler(?TS_DB, I),
    start_change_handlers(Ids).

-spec(todays_db_name/0 :: () -> binary()).
todays_db_name() ->
    {{Y,M,D}, _} = calendar:universal_time(),
    iolist_to_binary(io_lib:format("ts_usage_~4B_~2..0B_~2..0B", [Y,M,D])).

-spec(has_credit/3 :: (DB :: binary(), AcctId :: binary(), Amt :: integer() | float()) -> boolean()).
has_credit(DB, AcctId, Amt) ->
    credit_available(DB, AcctId) > ?DOLLARS_TO_UNITS(Amt).

-spec(credit_available/2 :: (DB :: binary(), AcctId :: binary()) -> integer()).
credit_available(DB, AcctId) ->
    case couch_mgr:get_results(DB, {"credit","credit_available"}, [{<<"group">>, <<"true">>}, {<<"key">>, AcctId}]) of
	[] -> 0;
	[{[{<<"key">>, _}, {<<"value">>, Avail}]}] -> Avail
    end.

-spec(trunk_type/3 :: (RDB :: binary(), AcctId :: binary(), CallID :: binary()) -> flat_rate | per_min | non_existant).
trunk_type(DB, AcctId, CallID) ->
    case couch_mgr:get_results(DB, {"trunks", "trunk_type"}, [ {<<"key">>, [AcctId, CallID]}, {<<"group">>, <<"true">>}]) of
	[] -> non_existant;
	[{[{<<"key">>,_}, {<<"value">>, <<"flat_rate">>}]}] -> flat_rate;
	[{[{<<"key">>,_}, {<<"value">>, <<"per_min">>}]}] -> per_min
    end.

-spec(trunk_status/3 :: (DB :: binary(), AcctId :: binary(), CallID :: binary()) -> active | inactive).
trunk_status(DB, AcctId, CallID) ->
    case couch_mgr:get_results(DB, {"trunks", "trunk_status"}, [ {<<"key">>, [AcctId, CallID]}, {<<"group_level">>, <<"2">>}]) of
	[] -> in_active;
	[{[{<<"key">>,_},{<<"value">>,<<"active">>}]}] -> active;
	[{[{<<"key">>,_},{<<"value">>,<<"inactive">>}]}] -> inactive
    end.

-spec(trunks_available/2 :: (DB :: binary(), AcctId :: binary()) -> integer()).
trunks_available(DB, AcctId) ->
    case couch_mgr:get_results(DB, {"trunks", "flat_rates_available"}, [{<<"key">>, AcctId}, {<<"group">>, <<"true">>}]) of
	[] -> 0;
	[{[{<<"key">>,_},{<<"value">>, Ts}]}] -> whistle_util:to_integer(Ts)
    end.

%% should be the diffs from the last account update to now
account_doc(AcctId, Credit, Trunks) ->
    credit_doc(AcctId, Credit, Trunks, [{<<"_id">>, AcctId}]).

reserve_doc(AcctId, CallID, flat_rate) ->
    Extra = [{<<"call_id">>, CallID}
	     ,{<<"trunk_type">>, flat_rate}
	     ,{<<"trunks">>, 1}
	     ,{<<"amount">>, 0}
	    ],
    debit_doc(AcctId, Extra);
reserve_doc(AcctId, CallID, per_min) ->
    Extra = [{<<"call_id">>, CallID}
	     ,{<<"trunk_type">>, per_min}
	     ,{<<"amount">>, 0}
	    ],
    debit_doc(AcctId, Extra).

release_doc(AcctId, CallID, flat_rate) ->
    credit_doc(AcctId, 0, 1, [{<<"call_id">>, CallID}, {<<"trunk_type">>, flat_rate}]).

release_doc(AcctId, CallID, per_min, Amt) ->
    Extra = [{<<"call_id">>, CallID}
	     ,{<<"trunk_type">>, per_min}
	     ,{<<"amount">>, ?DOLLARS_TO_UNITS(Amt)}
	    ],
    debit_doc(AcctId, Extra).

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
	[] -> [];
	AcctsDoc -> lists:map(fun({AcctDoc}) -> props:get_value(<<"key">>, AcctDoc) end, AcctsDoc)
    end.

transfer_acct(AcctId, RDB, WDB) ->
    %% read account balance, from RDB
    Bal = credit_available(RDB, AcctId),
    Acct = couch_mgr:open_doc(RDB, AcctId),
    Acct1 = [ {<<"amount">>, Bal} | lists:keydelete(<<"amount">>, 1, Acct)],

    format_log(info, "TS_ACCTMGR.transfer: ~p has ~p balance~n", [AcctId, ?UNITS_TO_DOLLARS(Bal)]),

    %% create credit entry in WDB for balance/trunks
    couch_mgr:save_doc(WDB, lists:keydelete(<<"_rev">>, 1, Acct1)),

    %% update info_* doc with account balance
    update_account(AcctId, Bal).

transfer_active_calls(AcctId, RDB, WDB) ->
    case couch_mgr:get_results(RDB, {"trunks", "trunk_status"}, [{<<"startkey">>, [AcctId]}, {<<"endkey">>, [AcctId, {[]}]}, {<<"group">>, <<"true">>}]) of
	[] -> ok;
	Calls when is_list(Calls) ->
	    lists:foreach(fun({[{<<"key">>, [_Acct, _CallId, DocId]}, {<<"value">>, <<"active">>}]}) ->
				  D = couch_mgr:open_doc(RDB, DocId),
				  couch_mgr:save_doc(WDB, lists:keydelete(<<"_rev">>, 1, D));
			     (_) -> ok
			  end, Calls)
    end.

update_from_couch(AcctId, WDB, RDB) ->
    Doc = couch_mgr:open_doc(?TS_DB, AcctId),
    couch_mgr:add_change_handler(?TS_DB, AcctId),

    {Acct} = props:get_value(<<"account">>, Doc, {[]}),
    {Credits} = props:get_value(<<"credits">>, Acct, {[]}),
    Balance = ?DOLLARS_TO_UNITS(whistle_util:to_float(props:get_value(<<"prepay">>, Credits, 0.0))),
    Trunks = whistle_util:to_integer(props:get_value(<<"trunks">>, Acct, 0)),

    UsageDoc = couch_mgr:open_doc(RDB, AcctId),
    T0 = props:get_value(<<"trunks">>, UsageDoc),
    C0 = props:get_value(<<"amount">>, UsageDoc),
    
    UD1 = [ {<<"trunks">>, T0 + (Trunks - T0)} | lists:keydelete(<<"trunks">>, 1, UsageDoc)],
    UD2 = [ {<<"amount">>, C0 + (Balance - C0)} | lists:keydelete(<<"amount">>, 1, UD1)],
    couch_mgr:save_doc(WDB, UD2).

update_account(AcctId, Bal) ->
    Doc = couch_mgr:open_doc(?TS_DB, AcctId),
    {Acct} = props:get_value(<<"account">>, Doc, {[]}),
    {Credits} = props:get_value(<<"credits">>, Acct, {[]}),
    Credits1 = [ {<<"prepay">>, ?UNITS_TO_DOLLARS(Bal)} | lists:keydelete(<<"prepay">>, 1, Credits)],
    Acct1 = [ {<<"credits">>, {Credits1}} | lists:keydelete(<<"credits">>, 1, Acct)],
    Doc1 = [ {<<"account">>, {Acct1}} | lists:keydelete(<<"account">>, 1, Doc)],
    couch_mgr:save_doc(?TS_DB, Doc1).

load_account(AcctId, DB) ->
    case couch_mgr:open_doc(?TS_DB, AcctId) of
	{error, not_found} -> ok;
	Doc -> 
	    couch_mgr:add_change_handler(?TS_DB, AcctId),

	    {Acct} = props:get_value(<<"account">>, Doc, {[]}),
	    {Credits} = props:get_value(<<"credits">>, Acct, {[]}),
	    Balance = ?DOLLARS_TO_UNITS(whistle_util:to_float(props:get_value(<<"prepay">>, Credits, 0.0))),
	    Trunks = whistle_util:to_integer(props:get_value(<<"trunks">>, Acct, 0)),
	    couch_mgr:save_doc(DB, account_doc(AcctId, Balance, Trunks))
    end.

load_views(DB) ->
    lists:foreach(fun(Name) ->
			  couch_mgr:load_doc_from_file(DB, trunkstore, Name)
		  end, ["accounts.json", "credit.json", "trunks.json"]).
				     
%% Sample Data importable via #> curl -X POST -d@sample.json.data http://localhost:5984/DB_NAME/_bulk_docs --header "Content-Type: application/json"
