%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz INC
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

-include_lib("whistle_transactions/include/whistle_transactions.hrl").

-type wh_transactions() :: wh_transaction:transactions().
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

-spec call_charges(ne_binary(), ne_binary(), ne_binary() | boolean()) -> integer() | wh_transactions().
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

-spec call_charges(ne_binary(), ne_binary(), ne_binary(), boolean()) -> integer() | wh_transactions().
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
filter_by_reason(<<"no_calls">>, Transactions) ->
    lists:foldr(
      fun(Transaction, Acc) ->
              Code = wh_transaction:code(Transaction),
              case Code >= 1000 andalso Code < 2000 of
                  'false' -> [Transaction | Acc];
                  'true' -> Acc
              end
      end, [], Transactions);
filter_by_reason(<<"only_calls">>, Transactions) ->
    lists:foldr(
      fun(Transaction, Acc) ->
              Code = wh_transaction:code(Transaction),
              case Code >= 1000 andalso Code < 2000 of
                  'true' -> [Transaction | Acc];
                  'false' -> Acc
              end
      end, [], Transactions);
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
-spec fetch_last(ne_binary(), integer()) ->
                        {'ok', wh_transactions()} |
                        {'error', any()}.
fetch_last(Account, Count) ->
    ViewOptions = [{'limit', Count}
                   ,'include_docs'
                  ],
    fetch_local(Account, ViewOptions).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% fetch last transaction from date to now
%% @end
%%--------------------------------------------------------------------
-spec fetch_since(ne_binary(), integer(), integer()) ->
                         {'ok', wh_transactions()} |
                         {'error', any()}.
fetch_since(Account, From, To) ->
    case check_range(From, To) of
        {'error', _Reason}=Error -> Error;
        {'ok', ViewOptionsFrom, ViewOptionsTo} ->
            fetch(Account, ViewOptionsFrom, ViewOptionsTo)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec check_range(integer(), integer()) ->
                         {'ok', 'undefined' | wh_proplist(), wh_proplist()} |
                         {'error', ne_binary()}.
check_range(From, To) ->
    {{YearFrom, MonthFrom, _}, _} = calendar:gregorian_seconds_to_datetime(From),
    {{YearTo, MonthTo, _}, _} = calendar:gregorian_seconds_to_datetime(To),
    ViewOptionsFrom = [{'startkey', From}
                       ,{'endkey', To}
                       ,{'year', YearFrom}
                       ,{'month', MonthFrom}
                       ,'include_docs'
                      ],
    ViewOptionsTo = [{'startkey', From}
                      ,{'endkey', To}
                      ,{'year', YearTo}
                      ,{'month', MonthTo}
                      ,'include_docs'
                    ],
    case {YearTo - YearFrom, MonthTo - MonthFrom} of
        {0, 0} -> {'ok', 'undefined', ViewOptionsTo};
        {0, M} when M > 2 ->
            {'error', <<"max range 2 consecutive month">>};
        {0, _M} ->
            {'ok', ViewOptionsFrom, ViewOptionsTo};
        {1, -11} ->
            {'ok', ViewOptionsFrom, ViewOptionsTo};
        {1, _M} ->
            {'error', <<"max range 2 consecutive month">>};
        {_Y, _M} ->
            {'error', <<"max range 2 consecutive month">>}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch(ne_binary(), 'undefined' | wh_proplist(), wh_proplist()) ->
                   {'ok', wh_transactions()} |
                   {'error', any()}.
fetch(Account, ViewOptionsFrom, ViewOptionsTo) ->
    case {fetch_local(Account, ViewOptionsFrom)
          ,fetch_local(Account, ViewOptionsTo)
          ,fetch_bookkeeper(Account, ViewOptionsTo)
         }
    of
        {{'error', _R}=Error, _, _} -> Error;
        {_, {'error', _R}=Error, _} -> Error;
        {_, _, {'error', _R}=Error} -> Error;
        {{'ok', Local1}, {'ok', Local2}, {'ok', Bookkeeper}} ->
            {'ok', de_duplicate_transactions(Local1 ++ Local2, Bookkeeper)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch_local(ne_binary(), 'undefined' | wh_proplist()) ->
                         {'ok', wh_transactions()} |
                         {'error', any()}.
fetch_local(_, 'undefined') -> {'ok', []};
fetch_local(Account, ViewOptions) ->
    case kazoo_modb:get_results(Account, <<"transactions/by_timestamp">>, ViewOptions) of
        {'error', _}=Error -> Error;
        {'ok', []}=R -> R;
        {'ok', ViewRes} ->
            {'ok', viewres_to_recordlist(ViewRes)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch_bookkeeper(ne_binary(), wh_proplist()) ->
                              {'ok', wh_transactions()} |
                              {'error', any()}.
fetch_bookkeeper(Account, ViewOptions) ->
    Bookkeeper = whapps_config:get_atom(<<"services">>, <<"master_account_bookkeeper">>),
    Options = [
        {'from', props:get_value('startkey', ViewOptions)}
        ,{'to', props:get_value('endkey', ViewOptions)}
    ],
    try Bookkeeper:transactions(Account, Options) of
        {'ok', _}=R -> R;
        {'error', _}=Error -> Error
    catch
        _:_ ->
            {'error', <<"error while fetching bookkeeper transactions">>}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec de_duplicate_transactions(wh_transactions(), wh_transactions()) -> wh_transactions().
-spec de_duplicate_transactions(wh_proplist(), wh_proplist(), wh_transactions()) -> wh_transactions().
de_duplicate_transactions(Transactions, BookkeeperTransactions) ->
    PropsTr = transactions_to_props(Transactions),
    PropsBTr = transactions_to_props(BookkeeperTransactions),
    de_duplicate_transactions(PropsTr, PropsBTr, []).

de_duplicate_transactions([], BookkeeperTransactions, Acc) ->
    [Tr || {_, Tr} <- BookkeeperTransactions] ++ Acc;
de_duplicate_transactions([{Key, Value}|Transactions], BookkeeperTransactions, Acc) ->
    case props:is_defined(Key, BookkeeperTransactions) of
        'true' ->
            de_duplicate_transactions(
              Transactions
              ,props:delete(Key, BookkeeperTransactions)
              ,[Value|Acc]
             );
        'false' ->
            de_duplicate_transactions(
              Transactions
              ,BookkeeperTransactions
              ,[Value|Acc]
             )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec transactions_to_props(wh_transactions()) -> wh_proplist().
transactions_to_props(Transactions) ->
    lists:foldl(fun transaction_to_prop_fold/2, [], Transactions).

-spec transaction_to_prop_fold(wh_transaction:transaction(), wh_proplist()) -> wh_proplist().
transaction_to_prop_fold(Transaction, Acc) ->
    Amount = wh_transaction:amount(Transaction),
    Timestamp = wh_transaction:created(Transaction),
    {Date, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    [{{Date, Amount}, Transaction}|Acc].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Save list of record
%% @end
%%--------------------------------------------------------------------
-type save_acc() :: [{'ok' | 'error', wh_transaction:transaction()},...] | [].

-spec save(wh_transactions()) -> save_acc().
-spec save(wh_transactions(), save_acc()) -> save_acc().
save(L) ->
    save(L, []).

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
-type remove_acc() :: ['ok' | {'error', wh_transaction:transaction()},...] | [].

-spec remove(wh_transactions()) -> remove_acc().
-spec remove(wh_transactions(), remove_acc()) -> remove_acc().
remove(Transactions) ->
    remove(Transactions, []).

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
-spec to_json(wh_transactions()) -> wh_json:objects().
to_json(Transactions) ->
    [wh_transaction:to_json(Tr) || Tr <- Transactions].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec to_public_json(wh_transactions()) -> wh_json:objects().
to_public_json(Transactions) ->
    [wh_transaction:to_public_json(Tr) ||  Tr <- Transactions].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% fetch last transaction
%% @end
%%--------------------------------------------------------------------
-spec viewres_to_recordlist(wh_json:objects()) -> wh_transactions().
viewres_to_recordlist(ViewRes) ->
    [wh_transaction:from_json(wh_json:get_value(<<"doc">>, Tr)) || Tr <- ViewRes].
