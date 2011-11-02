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

-export([store_account_handler/2]).

-export([preload_accounts/0, preload_trunkstore/0]).

-export([refresh_all_accounts/0, refresh_account/1]).

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
    [j5_acctmgr:status(AcctPid) || {{j5_authz, _AcctID}, AcctPid} <- AcctPids].

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

store_account_handler(AcctID, J5Pid) ->
    {ok, Cache} = jonny5_sup:cache_proc(),
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

-spec refresh_all_accounts/0 :: () -> 'ok'.
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
