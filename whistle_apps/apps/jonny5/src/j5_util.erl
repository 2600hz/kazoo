%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Util functions for shared actions
%%% @end
%%% Created :  2 Nov 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(j5_util).

-export([fetch_all_accounts/0, fetch_account/1, fetch_account_handler/1]).

-export([store_account_handler/2, uptime/1]).

-export([preload_accounts/0, preload_trunkstore/0]).

-export([refresh_all_accounts/0, refresh_account/1]).

-export([write_debit_to_ledger/5, write_credit_to_ledger/5]).

-include("jonny5.hrl").

-spec fetch_all_accounts/0 :: () -> json_objects().
fetch_all_accounts() ->
    {ok, Cache} = jonny5_sup:cache_proc(),
    fetch_all_accounts(Cache).

-spec fetch_all_accounts/1 :: (pid()) -> json_objects().
fetch_all_accounts(Cache) ->
    AcctPids = wh_cache:filter_local(Cache, fun({j5_authz, _}, _) -> true;
						(_, _) -> false
					     end),
    [j5_acctmgr:status(AcctPid) || {{j5_authz, _AcctID}, AcctPid} <- AcctPids, erlang:is_process_alive(AcctPid)].

-spec fetch_account/1 :: (ne_binary()) -> json_object() | {'error', 'not_found'}.
fetch_account(AcctID) ->
    case fetch_account_handler(AcctID) of
	{ok, Pid} ->
	    j5_acctmgr:status(Pid);
	{error, _}=E -> E
    end.

-spec fetch_account_handler/1 :: (ne_binary()) -> {'ok', pid()} | {'error', 'not_found'}.
fetch_account_handler(AcctID) ->
    {ok, Cache} = jonny5_sup:cache_proc(),
    fetch_account_handler(AcctID, Cache).

-spec fetch_account_handler/2 :: (ne_binary(), pid()) -> {'ok', pid()} | {'error', 'not_found'}.
fetch_account_handler(AcctID, Cache) when is_pid(Cache) ->
    wh_cache:fetch_local(Cache, cache_account_handler_key(AcctID)).

-spec store_account_handler/2 :: (ne_binary(), pid() | 'undefined') -> 'ok'.
store_account_handler(AcctID, J5Pid) ->
    {ok, Cache} = jonny5_sup:cache_proc(),
    store_account_handler(AcctID, J5Pid, Cache).

-spec store_account_handler/3 :: (ne_binary(), pid() | 'undefined', pid()) -> 'ok'.
store_account_handler(AcctID, undefined, Cache) ->
    wh_cache:erase_local(Cache, cache_account_handler_key(AcctID));
store_account_handler(AcctID, J5Pid, Cache) when is_pid(J5Pid) ->
    wh_cache:store_local(Cache, cache_account_handler_key(AcctID), J5Pid, infinity).

cache_account_handler_key(AcctID) ->
    {j5_authz, AcctID}.

-spec preload_accounts/0 :: () -> [{'ok', pid()},...].
preload_accounts() ->
    {ok, Accts} = couch_mgr:get_results(<<"accounts">>, <<"accounts/listing_by_id">>, []),
    ?LOG_SYS("Loading ~b accounts", [length(Accts)]),
    preload_accounts(Accts).

preload_accounts(JObjs) ->
    {ok, Cache} = jonny5_sup:cache_proc(),
    [ preload_account(wh_json:get_value(<<"id">>, JObj), Cache) || JObj <- JObjs].

preload_account(AcctID, Cache) ->
    case fetch_account_handler(AcctID, Cache) of
	{ok, _} -> ok;
	{error, not_found} ->
	    jonny5_acct_sup:start_proc(AcctID)
    end.

-spec preload_trunkstore/0 :: () -> [{'ok', pid()},...].
preload_trunkstore() ->
    {ok, TSAccts} = couch_mgr:get_results(<<"ts">>, <<"LookUpDID/DIDsByAcct">>, []), %% crappy way, make new view
    ?LOG_SYS("Loading ~b trunkstore accounts", [length(TSAccts)]),
    preload_accounts(TSAccts).

-spec refresh_all_accounts/0 :: () -> no_return().
refresh_all_accounts() ->
    {ok, Cache} = jonny5_sup:cache_proc(),
    AcctPids = wh_cache:filter_local(Cache, fun({j5_authz, _}, _) -> true;
					       (_, _) -> false
					    end),
    [j5_acctmgr:refresh(AcctPid) || {{j5_authz, _AcctID}, AcctPid} <- AcctPids].

-spec refresh_account/1 :: (ne_binary()) -> 'ok'.
refresh_account(AcctID) ->
    {ok, AcctPid} = fetch_account_handler(AcctID),
    j5_acctmgr:refresh(AcctPid).

-spec uptime/1 :: (pos_integer()) -> pos_integer().
uptime(StartTime) ->
    case wh_util:current_tstamp() - StartTime of
	X when X =< 0 ->
	    1;
	X -> X
    end.

write_debit_to_ledger(DB, CallID, CallType, DebitUnits, Duration) ->
    Timestamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),

    JObj = wh_json:from_list([{<<"Call-ID">>, CallID}
			      ,{<<"Call-Type">>, CallType}
			      ,{<<"Call-Duration">>, Duration}
			      ,{<<"Amount">>, DebitUnits}
			      ,{<<"pvt_type">>, <<"debit">>}
			      ,{<<"pvt_created">>, Timestamp}
			      ,{<<"pvt_modified">>, Timestamp}
			      ,{<<"pvt_version">>, ?APP_VERSION}
			      ,{<<"_id">>, <<CallID/binary, "-", (wh_util:to_binary(Timestamp))/binary>>}
			     ]),
    couch_mgr:save_doc(DB, JObj).

write_credit_to_ledger(DB, CallID, CallType, CreditUnits, Duration) ->
    Timestamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),

    JObj = wh_json:from_list([{<<"Call-ID">>, CallID}
			      ,{<<"Call-Type">>, CallType}
			      ,{<<"Call-Duration">>, Duration}
			      ,{<<"Amount">>, CreditUnits}
			      ,{<<"pvt_type">>, <<"credit">>}
			      ,{<<"pvt_created">>, Timestamp}
			      ,{<<"pvt_modified">>, Timestamp}
			      ,{<<"pvt_version">>, ?APP_VERSION}
			      ,{<<"_id">>, <<CallID/binary, "-", (wh_util:to_binary(Timestamp))/binary>>}
			     ]),
    couch_mgr:save_doc(DB, JObj).
