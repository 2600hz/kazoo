%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-module(wh_transactions).

-export([call_charges/2
         ,call_charges/3
         ,call_charges/4
        ]).
-export([filter_by_reason/2]).
-export([fetch_since/3]).
-export([fetch_last/2]).
-export([save/1]).
-export([remove/1]).
-export([to_json/1]).
-export([to_public_json/1]).

-include("whistle_transactions.hrl").

-type wh_transactions() :: [wh_transaction:transaction(), ...].
-export_type([wh_transactions/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec call_charges(ne_binary(), ne_binary()) -> integer().
call_charges(Ledger, CallId) ->
    call_charges(Ledger, CallId, 'true').

-spec call_charges(ne_binary(), ne_binary(), 'true') -> integer();
                  (ne_binary(), ne_binary(), 'false') -> wh_transactions();
                  (ne_binary(), ne_binary(), ne_binary()) -> integer().
call_charges(Ledger, CallId, 'true') ->
    LedgerDb = wh_util:format_account_id(Ledger, 'encoded'),
    ViewOptions = ['reduce'
                   ,'group'
                   ,{'group_level', 1}
                   ,{'startkey', [CallId]}
                   ,{'endkey', [CallId, wh_json:new()]}
                  ],
    case couch_mgr:get_results(LedgerDb, <<"transactions/per_minute_cost">>, ViewOptions) of
        {'ok', []} -> 0;
        {'ok', [JObj]} -> wh_json:get_integer_value(<<"value">>, JObj, 0);
        {'error', _R} ->
            lager:debug("unable to get per minute cost for ~s: ~p", [CallId, _R]),
            0
    end;
call_charges(Ledger, CallId, 'false') ->
    LedgerDb = wh_util:format_account_id(Ledger, 'encoded'),
    ViewOptions = [{'reduce', 'false'}
                   ,{'group', 'false'}
                   ,{'startkey', [CallId]}
                   ,{'endkey', [CallId, wh_json:new()]}
                   ,'include_docs'
                  ],
    case couch_mgr:get_results(LedgerDb, <<"transactions/per_minute_cost">>, ViewOptions) of
        {'ok', []} -> [];
        {'ok', JObjs} ->
            [wh_transaction:from_json(wh_json:get_value(<<"doc">>, JObj))
             || JObj <- JObjs
            ];
        {'error', _R} ->
            lager:debug("unable to get per minute cost for ~s: ~p", [CallId, _R]),
            []
    end;
call_charges(Ledger, CallId, Event) ->
    call_charges(Ledger, CallId, Event, 'true').

-spec call_charges(ne_binary(), ne_binary(), ne_binary(), 'true') -> integer();
                  (ne_binary(), ne_binary(), ne_binary(), 'false') -> wh_transactions().
call_charges(Ledger, CallId, Event, 'true') ->
    LedgerDb = wh_util:format_account_id(Ledger, 'encoded'),
    ViewOptions = ['reduce'
                   ,'group'
                   ,{'group_level', 1}
                   ,{'key', [CallId, Event]}
                  ],
    case couch_mgr:get_results(LedgerDb, <<"transactions/per_minute_cost">>, ViewOptions) of
        {'ok', []} -> 0;
        {'ok', [JObj]} -> wh_json:get_integer_value(<<"value">>, JObj, 0);
        {'error', _R} ->
            lager:debug("unable to get per minute cost for ~s: ~p", [CallId, _R]),
            0
    end;
call_charges(Ledger, CallId, Event, 'false') ->
    LedgerDb = wh_util:format_account_id(Ledger, 'encoded'),
    ViewOptions = [{'reduce', 'false'}
                   ,{'group', 'false'}
                   ,{'key', [CallId, Event]}
                   ,'include_docs'
                  ],
    case couch_mgr:get_results(LedgerDb, <<"transactions/per_minute_cost">>, ViewOptions) of
        {'ok', []} -> [];
        {'ok', JObjs} ->
            [wh_transaction:from_json(wh_json:get_value(<<"doc">>, JObj))
             || JObj <- JObjs
            ];
        {'error', _R} ->
            lager:debug("unable to get per minute cost for ~s: ~p", [CallId, _R]),
            []
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Filter list of transactions by reason
%% @end
%%--------------------------------------------------------------------
-spec filter_by_reason(ne_binary(), wh_transactions()) -> wh_transactions().
filter_by_reason(Reason, Transactions) ->
    lists:foldr(
      fun(Transaction, Acc) ->
              case wh_transaction:is_reason(Reason, Transaction) of
                  'true' -> [Transaction | Acc];
                  'false' -> Acc
              end
      end, [], Transactions).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% fetch last transactions
%% @end
%%--------------------------------------------------------------------
-spec fetch_last(ne_binary(), integer()) -> wh_transaction:wh_transactions().
fetch_last(AccountId, Count) ->
    AccountDB = wh_util:format_account_id(AccountId, 'encoded'),
    ViewOptions = [{'limit', Count}
                   ,'include_docs'
                  ],
    case couch_mgr:get_results(AccountDB, <<"transactions/by_timestamp">>, ViewOptions) of
        {'ok', []} ->
            lager:debug("no transactions for that account ~p", [AccountId]),
            [];
        {'ok', ViewRes} ->
            viewres_to_recordlist(ViewRes)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% fetch last transaction from date to now
%% @end
%%--------------------------------------------------------------------
-spec fetch_since(ne_binary(), integer(), integer()) -> wh_transactions().
fetch_since(AccountId, From, To) ->
    AccountDB = wh_util:format_account_id(AccountId, 'encoded'),
    ViewOptions = [{'startkey', From}
                   ,{'endkey', To}
                   ,'include_docs'
                  ],
    case couch_mgr:get_results(AccountDB, <<"transactions/by_timestamp">>, ViewOptions) of
        {'ok', []} ->
            lager:debug("no transactions for that range from ~p to ~p on ~p", [From, To, AccountId]),
            [];
        {'ok', ViewRes} ->
            viewres_to_recordlist(ViewRes)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Save list of record
%% @end
%%--------------------------------------------------------------------
-spec save(wh_transactions()) -> wh_transactions().
save(L) ->
    save(L, []).

-spec save(wh_transactions(), wh_transactions()) -> wh_transactions().
save([], Acc) ->
    lists:reverse(Acc);
save([Transaction | Transactions], Acc) ->
    case wh_transaction:save(Transaction) of
        {'ok', _}=Ok ->
            save(Transactions, [Ok | Acc]);
        {'error', _} ->
            save(Transactions, [{'error', Transaction} | Acc])
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
remove(Transactions) ->
    remove(Transactions, []).

-spec remove(wh_transactions(), wh_transactions()) -> wh_transactions().
remove([], Acc) ->
    lists:reverse(Acc);
remove([Transaction | Transactions], Acc) ->
    case wh_transaction:remove(Transaction) of
        'ok' ->
            remove(Transactions, ['ok' | Acc]);
        {'error', _} ->
            remove(Transactions, [{'error', Transaction} | Acc])
    end.


%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec to_json/1 :: (wh_transactions()) -> [wh_json:object(), ...].
to_json(Transactions) ->
    [wh_transaction:to_json(Tr) ||  Tr <- Transactions].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec to_public_json/1 :: (wh_transactions()) -> [wh_json:object(), ...].
to_public_json(Transactions) ->
    [wh_transaction:to_public_json(Tr) ||  Tr <- Transactions].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% fetch last transaction
%% @end
%%--------------------------------------------------------------------
-spec viewres_to_recordlist/1 :: (list()) -> wh_transaction:wh_transactions().
viewres_to_recordlist(ViewRes) ->
    [wh_transaction:from_json(wh_json:get_value(<<"doc">>, Tr)) || Tr <- ViewRes].
