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

-export([store_account_handler/2, uptime/1, current_usage/1]).

-export([preload_accounts/0, preload_trunkstore/0]).

-export([refresh_all_accounts/0, refresh_account/1]).

-export([write_debit_to_ledger/5, write_credit_to_ledger/5
         ,write_debit_to_ledger/6, write_credit_to_ledger/6
        ]).

-include_lib("jonny5/src/jonny5.hrl").

-spec fetch_all_accounts/0 :: () -> wh_json:json_objects().
fetch_all_accounts() ->
    AcctPids = wh_cache:filter_local(?JONNY5_CACHE, fun({j5_authz, _}, _) -> true;
                                                (_, _) -> false
                                             end),
    [j5_acctmgr:status(AcctPid) || {{j5_authz, _AcctID}, AcctPid} <- AcctPids, erlang:is_process_alive(AcctPid)].

-spec fetch_account/1 :: (ne_binary()) -> wh_json:json_object() | {'error', 'not_found'}.
fetch_account(AcctID) ->
    case fetch_account_handler(AcctID) of
        {ok, Pid} -> j5_acctmgr:status(Pid);
        {error, _}=E -> E
    end.

-spec fetch_account_handler/1 :: (ne_binary()) -> {'ok', pid()} | {'error', 'not_found'}.
fetch_account_handler(AcctID) ->
    wh_cache:fetch_local(?JONNY5_CACHE, cache_account_handler_key(AcctID)).

-spec store_account_handler/2 :: (ne_binary(), pid() | 'undefined') -> 'ok'.
store_account_handler(AcctID, undefined) ->
    wh_cache:erase_local(?JONNY5_CACHE, cache_account_handler_key(AcctID));
store_account_handler(AcctID, J5Pid) when is_pid(J5Pid) ->
    wh_cache:store_local(?JONNY5_CACHE, cache_account_handler_key(AcctID), J5Pid, infinity).

cache_account_handler_key(AcctID) ->
    {j5_authz, AcctID}.

-spec preload_accounts/0 :: () -> [{'ok', pid()},...].
preload_accounts() ->
    {ok, Accts} = couch_mgr:get_results(?WH_ACCOUNTS_DB, <<"accounts/listing_by_id">>, []),
    lager:debug("Loading ~b accounts", [length(Accts)]),
    preload_accounts(Accts).

preload_accounts(JObjs) ->
    [ preload_account(wh_json:get_value(<<"id">>, JObj)) || JObj <- JObjs].

preload_account(AcctID) ->
    case fetch_account_handler(AcctID) of
        {ok, _} -> ok;
        {error, not_found} ->
            jonny5_acct_sup:start_proc(AcctID)
    end.

-spec preload_trunkstore/0 :: () -> [{'ok', pid()},...].
preload_trunkstore() ->
    {ok, TSAccts} = couch_mgr:get_results(<<"ts">>, <<"LookUpDID/DIDsByAcct">>, []), %% crappy way, make new view
    lager:debug("Loading ~b trunkstore accounts", [length(TSAccts)]),
    preload_accounts(TSAccts).

-spec refresh_all_accounts/0 :: () -> no_return().
refresh_all_accounts() ->
    AcctPids = wh_cache:filter_local(?JONNY5_CACHE, fun({j5_authz, _}, _) -> true;
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
    write_debit_to_ledger(DB, CallID, CallType, DebitUnits, Duration, wh_json:new()).
write_debit_to_ledger(DB, CallID, CallType, DebitUnits, Duration, JObj) ->
    write_transaction_to_ledger(DB, CallID, CallType, DebitUnits, Duration, JObj, debit).

write_credit_to_ledger(DB, CallID, CallType, CreditUnits, Duration) ->
    write_credit_to_ledger(DB, CallID, CallType, CreditUnits, Duration, wh_json:new()).
write_credit_to_ledger(DB, CallID, CallType, CreditUnits, Duration, JObj) ->
    write_transaction_to_ledger(DB, CallID, CallType, CreditUnits, Duration, JObj, credit).

-spec write_transaction_to_ledger/7 :: (ne_binary(), ne_binary(), call_types(), integer(), integer(), wh_json:json_object(), 'debit' | 'credit') -> {'ok', wh_json:json_object()} | {'error', atom()}.
write_transaction_to_ledger(DB, CallID, CallType, Units, Duration, JObj, DocType) when (CallType =:= twoway orelse CallType =:= inbound)
                                                                                       andalso Units =/= 0 ->
    write_transaction_to_ledger(DB, CallID, CallType, 0, Duration, JObj, DocType);
write_transaction_to_ledger(DB, CallID, CallType, Units, Duration, JObj, DocType) ->
    Timestamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),

    AcctID = wh_json:get_value(<<"Account-ID">>, JObj, DB),
    EvtTimestamp = wh_json:get_value(<<"Timestamp">>, JObj, <<>>),

    ID = mk_id(CallID, AcctID, EvtTimestamp),

    lager:debug("trying to write ~s to ~s for doc ~s", [ID, DB, DocType]),

    TransactionJObj = wh_json:from_list([{<<"call_id">>, CallID}
                                         ,{<<"call_type">>, CallType}
                                         ,{<<"call_duration">>, wh_util:to_integer(Duration)}
                                         ,{<<"amount">>, wh_util:to_integer(Units)}
                                         ,{<<"pvt_account_id">>, wh_util:format_account_id(AcctID, raw)}
                                         ,{<<"pvt_account_db">>, wh_util:format_account_id(AcctID, encoded)}
                                         ,{<<"pvt_type">>, DocType}
                                         ,{<<"pvt_created">>, Timestamp}
                                         ,{<<"pvt_modified">>, Timestamp}
                                         ,{<<"pvt_vsn">>, ?APP_VERSION}
                                         ,{<<"pvt_whapp">>, ?APP_NAME}
                                         ,{<<"_id">>, ID}
                                        ]),
    couch_mgr:save_doc(DB, TransactionJObj).

-spec mk_id/3 :: (ne_binary(), ne_binary(), ne_binary() | pos_integer()) -> ne_binary() | 'undefined'.
mk_id(CallID, AcctID, Tstamp) ->
    Suffix = case wh_util:is_empty(Tstamp) of
                 true -> <<>>;
                 false -> [<<"-">>, (wh_util:to_binary(Tstamp))]
             end,
    case wh_util:is_empty(CallID) of
        true -> mk_id(AcctID, Suffix);
        false -> mk_id(CallID, Suffix)
    end.

-spec mk_id/2 :: (binary(), binary()) -> ne_binary() | 'undefined'.
mk_id(Prefix, Suffix) ->
    case wh_util:is_empty(Prefix) of
        true -> undefined;
        false -> list_to_binary(["transaction-", Prefix, Suffix])
    end.

-spec current_usage/1 :: (ne_binary()) -> integer().
current_usage(AcctID) ->
    DB = wh_util:format_account_id(AcctID, encoded),
    case couch_mgr:get_results(DB, <<"transactions/credit_remaining">>, [{<<"reduce">>, true}]) of
        {ok, []} -> 0;
        {ok, [ViewRes|_]} -> wh_json:get_integer_value(<<"value">>, ViewRes, 0);
        {error, _} -> 0
    end.
