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
-export([has_credit/1, has_credit/2, deduct_credit/2, reserve_trunk/2, reserve_trunk/3, release_trunk/2]).

-export([update_table/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ts.hrl").

-import(logger, [format_log/3]).

-define(SERVER, ?MODULE).
-define(WRITES_THRESHOLD, 4). %% how many writes should a mnesia record have before persisting to couch

%% { CallID, flat_rate | per_min }
-type active_call() :: tuple(binary(), flat_rate | per_min).


-record(ts_acct, {
	  account_id = <<>> :: binary()
         ,account_rev = <<>> :: binary()
         ,reference = undefined :: undefined | reference()
         ,account_credit = 0 :: integer() %% thousand-ths of a cent, so 1000 = $0.01
	 ,delta_credit = 0 :: integer() %% deductions go here
         ,max_trunks = 0 :: integer() %% number of flat-rate trunks purchased
	 ,active_calls = [] :: list(active_call())
	 ,writes_since_sync = 0 :: integer()
	 }).

%% -record(old_ts_acct, {
	 %%  account_id = <<>> :: binary()
         %% ,reference = undefined :: undefined | reference()
         %% ,account_credit = 0 :: integer() %% thousand-ths of a cent, so 1000 = $0.01
         %% ,max_trunks = 0 :: integer() %% number of flat-rate trunks purchased
	 %% ,active_calls = [] :: list(active_call())
	 %% ,writes_since_sync = 0 :: integer()
	 %% }).

-define(DOLLARS_TO_UNITS, 100000). %% $1.00 = 100,000 thousand-ths of a cent
-define(CENTS_TO_UNITS, 1000). %% 100 cents = 100,000 thousand-ths of a cent

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

update_table() ->
    %% old_ts_acct -> ts_acct
    whistle_apps_mnesia:update_table(ts_acct, fun({ts_acct, AcctId, Ref, AC, MT, ACs, WSS}) ->
    						      #ts_acct{
    						   account_id = AcctId
    						   ,reference = Ref
    						   ,account_credit = AC
    						   ,max_trunks = MT
    						   ,active_calls = ACs
    						   ,writes_since_sync = WSS
    						  };
						 (Rec) when is_record(Rec, ts_acct) -> Rec;
						 (Other) -> format_log(info, "Update table: failed to update ~p~n", [Other]), Other
    					      end
    				     , record_info(fields, ts_acct)).

%%%===================================================================
%%% Data Access API
%%%===================================================================
-spec(has_credit/1 :: (Acct :: binary()) -> boolean()).
has_credit(Acct) ->
    has_credit(Acct, 0).

%% Does the account have enough credit to cover Amt
-spec(has_credit/2 :: (Acct :: binary(), Amt :: integer()) -> boolean()).
has_credit(Acct, Amt) ->
    gen_server:call(?SERVER, {has_credit, Acct, [Amt]}, infinity).

%% Deduct the cost of the call from the account, returning the remaining balance; or return an error
-spec(deduct_credit/2 :: (Acct :: binary(), Amt :: float()) -> tuple(ok, float()) | tuple(error, term())).
deduct_credit(Acct, Amt) ->
    gen_server:call(?SERVER, {deduct_credit, Acct, [Amt]}, infinity).

%% try to reserve a trunk
%% first try to reserve a flat_rate trunk; if none are available, try a per_min trunk;
%% if the Amt is more than available credit, return error
-spec(reserve_trunk/2 :: (Acct :: binary(), CallID :: binary()) -> tuple(ok, flat_rate | per_min) | tuple(error, term())).
reserve_trunk(Acct, CallID) ->
    reserve_trunk(Acct, CallID, 0.0).

-spec(reserve_trunk/3 :: (Acct :: binary(), CallID :: binary(), Amt :: float() | integer()) -> tuple(ok, flat_rate | per_min) | tuple(error, term())).
reserve_trunk(Acct, CallID, Amt) ->
    gen_server:call(?SERVER, {reserve_trunk, Acct, [CallID, Amt]}, infinity).

%% release a reserved trunk
%% pass the account and the callid from the reserve_trunk/2 call to release the trunk back to the account
-spec(release_trunk/2 :: (Acct :: binary(), CallID :: binary()) -> no_return()).
release_trunk(Acct, CallID) ->
    gen_server:cast(?SERVER, {release_trunk, Acct, [CallID]}).

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
    R = whistle_apps_mnesia:create_table(ts_acct, [{attributes, record_info(fields, ts_acct)}
						   ,{type, set}
						  ]),
    format_log(info, "TS_ACCTMGR.init: Create table returned ~p~n", [R]),
    {ok, ok}.

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
handle_call({Action, Acct, Args}, _, S) ->
    case get_acct(Acct) of
	{error, _}=E -> {reply, E, S};
	AcctRec when is_record(AcctRec, ts_acct)->
	    format_log(info, "TS_ACCTMGR(~p): Loaded ~p: ~p, running ~p~n", [self(), Acct, AcctRec, Action]),
	    {reply, run(Action, AcctRec, Args), S}
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
handle_cast({Action, Acct, Args}, S) ->
    case get_acct(Acct) of
	{error, _}=E -> {reply, E, S};
	AcctRec when is_record(AcctRec, ts_acct)->
	    format_log(info, "TS_ACCTMGR(~p): Loaded ~p: ~p, running ~p~n", [self(), Acct, AcctRec, Action]),
	    format_log(info, "TS_ACCTMGR(~p): Cast resulted in ~p~n", [self(), run(Action, AcctRec, Args)]),
	    {noreply, S}
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
handle_info({document_changes, DocID, Changes}, S) ->
    format_log(info, "TS_ACCTMGR(~p): Changes on ~p. ~p~n", [self(), DocID, Changes]),
    update_from_couch(DocID, Changes),
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
-spec(get_acct/1 :: (AcctId :: binary()) -> #ts_acct{} | tuple(error, doc_not_found)).
get_acct(AcctId) ->
    case mnesia:transaction(fun() -> mnesia:read(ts_acct, AcctId) end) of
	{atomic, []} ->
	    load_from_couch(AcctId);
	{atomic, [R]} when is_record(R, ts_acct) ->
	    couch_mgr:add_change_handler(?TS_DB, AcctId),
	    R;
	{aborted, Reason} ->
	    format_log(error, "TS_ACCTMGR(~p): get_acct failed: ~p~n", [self(), Reason]),
	    {error, Reason}
    end.

-spec(load_from_couch/1 :: (AcctId :: binary()) -> #ts_acct{} | tuple(error, doc_not_found)).
load_from_couch(AcctId) ->
    format_log(info, "TS_ACCTMGR(~p): load_from_couch ~p~n", [self(), AcctId]),
    case couch_mgr:open_doc(?TS_DB, AcctId) of
	{error, not_found} -> {error, doc_not_found};
	Doc when is_list(Doc) ->
	    couch_mgr:add_change_handler(?TS_DB, AcctId),
	    Rec = build_rec_from_doc(Doc),
	    format_log(info, "TS_ACCTMGR(~p): Creating ~p in m_table~n", [self(), Rec]),
	    case mnesia:transaction(fun() ->
					    case mnesia:read(ts_acct, AcctId) of
						[] -> mnesia:write(Rec);
						[Rec0] when is_record(Rec0, ts_acct) -> Rec0
					    end
				    end) of
		{atomic, ok} -> Rec;
		{atomic, Rec1} when is_record(Rec1, ts_acct) -> Rec1;
		{aborted, {bad_type, _}=E} -> {error, E};
		_Other -> format_log(error, "load from couch other: ~p~n", [_Other]), Rec
	    end
    end.

update_from_couch(AcctId, Changes) ->
    _ = lists:foldl(fun(ChangeProp, AcctRec0) ->
			    NewRev = props:get_value(<<"rev">>, ChangeProp),
			    format_log(info, "up_from_c: mn_rev: ~p c_rev: ~p~n", [AcctRec0#ts_acct.account_rev, NewRev]),
			    case AcctRec0#ts_acct.account_rev of
				NewRev -> AcctRec0;
				_ ->
				    D = couch_mgr:open_doc(?TS_DB, AcctId),
				    AcctRec1 = build_rec_from_doc(D),
				    format_log(info, "up_from_c: m_rec: ~p~nc_rec: ~p~n", [AcctRec0, AcctRec1]),
				    AcctRec01 = AcctRec0#ts_acct{
						  account_rev = AcctRec1#ts_acct.account_rev
						  ,reference = AcctRec1#ts_acct.reference
						  ,account_credit = AcctRec1#ts_acct.account_credit
						  ,max_trunks = AcctRec1#ts_acct.max_trunks
						 },
				    update_rec(AcctRec0, AcctRec01),
				    AcctRec01
			    end
		    end, get_acct(AcctId), Changes).

-spec(save_to_couch/1 :: (#ts_acct{}) -> tuple(ok, proplist()) | tuple(error, conflict | bad_revision)).
save_to_couch(#ts_acct{account_id=AcctId, account_rev=AcctRev, delta_credit=DC}) ->
    Doc = couch_mgr:open_doc(?TS_DB, AcctId),
    DocRev = props:get_value(<<"_rev">>, Doc),
    case DocRev =:= AcctRev of
	false ->
	    format_log(info, "Bad rev: MN ~p Doc: ~p~n", [AcctRev, DocRev]),
	    {error, bad_revision};
	true ->
	    {Acct} = props:get_value(<<"account">>, Doc, {[]}),
	    {Credits} = props:get_value(<<"credits">>, Acct, {[]}),
	    DocCredit = case whistle_util:to_float(props:get_value(<<"prepay">>, Credits, 0)) of
			 0.0 -> 0;
			 F when is_float(F) -> whistle_util:to_integer(F * ?DOLLARS_TO_UNITS) %% convert $450.12 -> 45012000
		     end,

	    Credits1 = [ {<<"prepay">>, DocCredit - DC} | lists:keydelete(<<"prepay">>, 1, Credits)],
	    Acct1 = [ {<<"credits">>, {Credits1}} | lists:keydelete(<<"credits">>, 1, Acct)],
	    Doc1 = [ {<<"account">>, {Acct1}} | lists:keydelete(<<"account">>, 1, Doc) ],
	    format_log(info, "Saving ~p~nWas ~p~n", [Doc1, Doc]),
	    couch_mgr:save_doc(?TS_DB, Doc1)
    end.

-spec(build_rec_from_doc/1 :: (Doc :: proplist()) -> #ts_acct{}).
build_rec_from_doc(Doc) ->
    {Acct} = props:get_value(<<"account">>, Doc, {[]}),
    {Credits} = props:get_value(<<"credits">>, Acct, {[]}),
    Credit0 = props:get_value(<<"prepay">>, Credits, 0),
    Credit = case whistle_util:to_float(Credit0) of
		 0.0 -> 0;
		 F when is_float(F) -> whistle_util:to_integer(F * ?DOLLARS_TO_UNITS) %% convert $450.12 -> 45012000
	     end,

    MaxTrunks = whistle_util:to_integer(props:get_value(<<"trunks">>, Acct, 0)),
    #ts_acct{
	      account_id = props:get_value(<<"_id">>, Doc)
	      ,account_rev = props:get_value(<<"_rev">>, Doc)
	      ,reference = undefined
	      ,account_credit = Credit
	      ,delta_credit = 0
	      ,max_trunks = MaxTrunks
	      ,active_calls = []
	      ,writes_since_sync = 0
	    }.

update_rec(#ts_acct{account_id=AcctId, reference=Ref, writes_since_sync=WSS}=OldRec, NewRec) ->
    {atomic, SavedRec} = mnesia:transaction(fun() ->
						    case mnesia:wread({ts_acct, AcctId}) of % lock
							[#ts_acct{reference = Ref1}] when Ref =:= Ref1 ->
							    mnesia:write(NewRec#ts_acct{reference = erlang:make_ref()
											,writes_since_sync = WSS+1
										       }),
							    NewRec;
							[OtherRec] when is_record(OtherRec, ts_acct) ->
							    %% someone else saved this record; create a merged record
							    format_log(info, "TS_ACCTMGR: Update out of date~nOld: ~p~nCurr: ~p~nNew: ~p~n", [OldRec, OtherRec, NewRec]),
							    
							    OldCalls = OldRec#ts_acct.active_calls,
							    NewCalls = NewRec#ts_acct.active_calls,
							    CallDiff = case NewCalls =:= OldCalls of
									   true -> [];
									   false ->
									       case hd(OldCalls) =/= hd(NewCalls) of
										   true -> [hd(NewCalls)];
										   false ->
										       lists:foldl(fun(K, Acc) ->
													   case lists:member(K, NewCalls) of
													       true -> Acc;
													       false -> [K | Acc]
													   end
												   end, [], OldCalls)
									       end
								       end,
							    DiffDelta = NewRec#ts_acct.delta_credit - OldRec#ts_acct.delta_credit,
							    OtherRec1 = OtherRec#ts_acct{
									  delta_credit = OtherRec#ts_acct.delta_credit + DiffDelta
									  ,active_calls = lists:umerge(OtherRec#ts_acct.active_calls, CallDiff)
									  ,reference = erlang:make_ref()
									  ,writes_since_sync = OtherRec#ts_acct.writes_since_sync + 1
									 },
							    mnesia:write(OtherRec1),
							    OtherRec1;
							Other -> format_log(info, "update_rec other: ~p~n", [Other]),
								 OldRec
						    end
					    end),
    case SavedRec#ts_acct.writes_since_sync > ?WRITES_THRESHOLD of
	true ->
	    case save_to_couch(SavedRec) of
		{error, _}=E -> format_log(info, "Failed to save doc: ~p~n", [E]);
		{ok, Doc} ->
		    NewRec = build_rec_from_doc(Doc),
		    format_log(info, "Saved couch doc.~nOldRec: ~p~nNewRec: ~p~n", [SavedRec, NewRec]),
		    update_rec(NewRec, NewRec)
	    end;
	false -> ok
    end,
    ok.

run(has_credit, #ts_acct{account_credit=0}, _) -> false;
run(has_credit, #ts_acct{account_credit=Credit, delta_credit=Delta}, [Amt]) ->
    (Credit - Delta) > whistle_util:to_integer(Amt * ?DOLLARS_TO_UNITS);
run(deduct_credit, #ts_acct{account_credit=C, delta_credit=Delta}=AcctRec, [Amt]) ->
    Delta1 = Delta + whistle_util:to_integer(Amt * ?DOLLARS_TO_UNITS),
    try
	ok = update_rec(AcctRec, AcctRec#ts_acct{delta_credit=Delta1}),
	{ok, ((C - Delta1) / ?DOLLARS_TO_UNITS)}
    catch
	_:E -> {error, E}
    end;
run(reserve_trunk, #ts_acct{active_calls=ACs}=AcctRec, [CallID, Amt]) ->
    case props:get_value(CallID, ACs) of
	flat_rate -> {error, {callid_exists, flat_rate}};
	per_min -> {error, {callid_exists, per_min}};
	undefined ->
	    case flat_rates_in_use(ACs) < AcctRec#ts_acct.max_trunks of
		true ->
		    update_rec(AcctRec, AcctRec#ts_acct{active_calls=[{CallID, flat_rate} | ACs]}),
		    {ok, flat_rate};
		false ->
		    case run(has_credit, AcctRec, [Amt]) of
			true ->
			    update_rec(AcctRec, AcctRec#ts_acct{active_calls=[{CallID, per_min} | ACs]}),
			    {ok, per_min};
			false ->
			    {error, no_available_trunks}
		    end
	    end
    end;
run(release_trunk, #ts_acct{active_calls=ACs}=AcctRec, [CallID]) ->
    case props:get_value(CallID, ACs) of
	undefined -> ok;
	_ ->
	    ACs1 = lists:keydelete(CallID, 1, ACs),
	    update_rec(AcctRec, AcctRec#ts_acct{active_calls=ACs1})
    end;
run(Action, _AcctRec, _Args) ->
    format_log(error, "TS_ACCTMGR(~p): Unknown action ~p, ~p, ~p~n", [self(), Action, _AcctRec, _Args]),
    {error, unknown_action}.

flat_rates_in_use(ACs) ->
    lists:foldl(fun({_, flat_rate}, Cnt) -> Cnt+1; (_, Cnt) -> Cnt end, 0, ACs).
