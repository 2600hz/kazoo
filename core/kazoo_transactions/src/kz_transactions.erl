%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_transactions).

-export([call_charges/2
        ,call_charges/3
        ,call_charges/4
        ]).
-export([filter_by_reason/2
        ,filter_for_per_minute/1
        ]).
-export([fetch_last/2
        ,fetch/3
        ,fetch_local/3
        ,fetch_bookkeeper/3
        ]).
-export([save/1]).
-export([remove/1]).
-export([to_json/1]).
-export([to_public_json/1]).

-include_lib("kazoo_services/include/kazoo_services.hrl").
-include_lib("kazoo_transactions/include/kazoo_transactions.hrl").

-type kz_transactions() :: kz_transaction:transactions().
-export_type([kz_transactions/0]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec call_charges(kz_term:ne_binary(), kz_term:ne_binary()) -> integer().
call_charges(Ledger, CallId) ->
    call_charges(Ledger, CallId, 'true').

-spec call_charges(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary() | boolean()) ->
                          integer() | kz_transactions().
call_charges(Ledger, CallId, 'true') ->
    LedgerDb = kz_util:format_account_id(Ledger, 'encoded'),
    ViewOptions = ['reduce'
                  ,'group'
                  ,{'group_level', 1}
                  ,{'startkey', [CallId]}
                  ,{'endkey', [CallId, kz_json:new()]}
                  ],
    case kz_datamgr:get_results(LedgerDb, <<"transactions/per_minute_cost">>, ViewOptions) of
        {'ok', []} -> 0;
        {'ok', [JObj]} -> kz_json:get_integer_value(<<"value">>, JObj, 0);
        {'error', _R} ->
            lager:debug("unable to get per minute cost for ~s: ~p", [CallId, _R]),
            0
    end;
call_charges(Ledger, CallId, 'false') ->
    LedgerDb = kz_util:format_account_id(Ledger, 'encoded'),
    ViewOptions = [{'reduce', 'false'}
                  ,{'group', 'false'}
                  ,{'startkey', [CallId]}
                  ,{'endkey', [CallId, kz_json:new()]}
                  ,'include_docs'
                  ],
    case kz_datamgr:get_results(LedgerDb, <<"transactions/per_minute_cost">>, ViewOptions) of
        {'ok', []} -> [];
        {'ok', JObjs} ->
            [kz_transaction:from_json(kz_json:get_value(<<"doc">>, JObj))
             || JObj <- JObjs
            ];
        {'error', _R} ->
            lager:debug("unable to get per minute cost for ~s: ~p", [CallId, _R]),
            []
    end;
call_charges(Ledger, CallId, Event) ->
    call_charges(Ledger, CallId, Event, 'true').

-spec call_charges(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), boolean()) -> integer() | kz_transactions().
call_charges(Ledger, CallId, Event, 'true') ->
    LedgerDb = kz_util:format_account_id(Ledger, 'encoded'),
    ViewOptions = ['reduce'
                  ,'group'
                  ,{'group_level', 1}
                  ,{'key', [CallId, Event]}
                  ],
    case kz_datamgr:get_results(LedgerDb, <<"transactions/per_minute_cost">>, ViewOptions) of
        {'ok', []} -> 0;
        {'ok', [JObj]} -> kz_json:get_integer_value(<<"value">>, JObj, 0);
        {'error', _R} ->
            lager:debug("unable to get per minute cost for ~s: ~p", [CallId, _R]),
            0
    end;
call_charges(Ledger, CallId, Event, 'false') ->
    LedgerDb = kz_util:format_account_id(Ledger, 'encoded'),
    ViewOptions = [{'reduce', 'false'}
                  ,{'group', 'false'}
                  ,{'key', [CallId, Event]}
                  ,'include_docs'
                  ],
    case kz_datamgr:get_results(LedgerDb, <<"transactions/per_minute_cost">>, ViewOptions) of
        {'ok', []} -> [];
        {'ok', JObjs} ->
            [kz_transaction:from_json(kz_json:get_value(<<"doc">>, JObj))
             || JObj <- JObjs
            ];
        {'error', _R} ->
            lager:debug("unable to get per minute cost for ~s: ~p", [CallId, _R]),
            []
    end.

%%------------------------------------------------------------------------------
%% @doc Filter list of transactions by reason
%% @end
%%------------------------------------------------------------------------------
-spec filter_by_reason(kz_term:ne_binary(), kz_transactions()) -> kz_transactions().
filter_by_reason(<<"only_bookkeeper">>, Transactions) ->
    {BTTransactions, LTransactions} = lists:partition(fun is_from_braintree/1, Transactions),
    case ?KZ_SERVICE_MASTER_ACCOUNT_BOOKKEEPER of
        'kz_bookkeeper_braintree' -> BTTransactions;
        'kz_bookkeeper_local'     -> LTransactions
    end;
filter_by_reason(<<"no_calls">>, Transactions) -> %% Legacy of only_bookkeeper
    lists:foldr(
      fun(Transaction, Acc) ->
              Code = kz_transaction:code(Transaction),
              case (Code >= 1000) and (Code < 2000) of
                  'false' -> [Transaction | Acc];
                  'true' -> Acc
              end
      end, [], Transactions);
filter_by_reason(<<"only_calls">>, Transactions) ->
    lists:foldr(
      fun(Transaction, Acc) ->
              Code = kz_transaction:code(Transaction),
              case (Code >= 1000) and (Code < 2000) of
                  'true' -> [Transaction | Acc];
                  'false' -> Acc
              end
      end, [], Transactions);
filter_by_reason(Reason, Transactions) ->
    [Transaction || Transaction <- Transactions
                        , kz_transaction:is_reason(Reason, Transaction)
    ].

-spec is_from_braintree(kz_transaction:transaction()) -> boolean().
is_from_braintree(Transaction) ->
    kz_transaction:description(Transaction) =:= <<"braintree transaction">>.

%%------------------------------------------------------------------------------
%% @doc Keep only per minute transactions
%% @end
%%------------------------------------------------------------------------------
-spec filter_for_per_minute(kz_transactions()) -> kz_transactions().
filter_for_per_minute(Transactions) ->
    [Transaction || Transaction <- Transactions
                        , is_per_minute(Transaction)
    ].

-spec is_per_minute(kz_transaction:transaction()) -> boolean().
is_per_minute(Transaction) ->
    case kz_transaction:code(Transaction) of
        ?CODE_PER_MINUTE_CALL -> 'true';
        ?CODE_PER_MINUTE_CALL_SUB_ACCOUNT -> 'true';
        _Code -> 'false'
    end.

%%------------------------------------------------------------------------------
%% @doc fetch last transactions
%% @end
%%------------------------------------------------------------------------------
-spec fetch_last(kz_term:ne_binary(), pos_integer()) ->
                        {'ok', kz_transactions()} |
                        {'error', any()}.
fetch_last(Account, Count) ->
    ViewOptions = [{'limit', Count}
                  ,'include_docs'
                  ],
    fetch_local(Account, [ViewOptions]).

%%------------------------------------------------------------------------------
%% @doc fetch last transactions from From to To
%% @end
%%------------------------------------------------------------------------------
-spec fetch(kz_term:ne_binary(), kz_time:gregorian_seconds(), kz_time:gregorian_seconds()) ->
                   {'ok', kz_transactions()} |
                   {'error', any()}.
fetch(Account, From, To) ->
    ViewOptionsList = get_range(Account, From, To),
    fetch(Account, ViewOptionsList).

-spec fetch(kz_term:ne_binary(), kz_term:proplists()) ->
                   {'ok', kz_transactions()} |
                   {'error', any()}.
fetch(Account, ViewOptionsList) ->
    case {fetch_local(Account, ViewOptionsList)
         ,fetch_bookkeeper(Account, ViewOptionsList)
         }
    of
        {{'error', _R}=Error, _} -> Error;
        {_, {'error', _R}=Error} -> Error;
        {{'ok', Local}, {'ok', Bookkeeper}} ->
            {'ok', de_duplicate_transactions(Local, Bookkeeper)}
    end.

-spec get_range(kz_term:ne_binary(), kz_time:gregorian_seconds(), kz_time:gregorian_seconds()) ->
                       ViewOptionsList :: kz_term:proplists().
get_range(Account, From, To) ->
    [ begin
          {Account, Year, Month} = kazoo_modb_util:split_account_mod(MODb),
          [{'startkey', From}
          ,{'endkey', To}
          ,{'year', Year}
          ,{'month', Month}
          ,'include_docs'
          ]
      end || MODb <- kazoo_modb:get_range(Account, From, To)
    ].

%%------------------------------------------------------------------------------
%% @doc fetch last local transactions from From to To
%% @end
%%------------------------------------------------------------------------------
-spec fetch_local(kz_term:ne_binary(), kz_time:gregorian_seconds(), kz_time:gregorian_seconds()) ->
                         {'ok', kz_transactions()} |
                         {'error', any()}.
fetch_local(Account, From, To) ->
    ViewOptionsList = get_range(Account, From, To),
    fetch_local(Account, ViewOptionsList).

-spec fetch_local(kz_term:ne_binary(), kz_term:proplists()) ->
                         {'ok', kz_transactions()} |
                         {'error', any()}.
fetch_local(_Account, []) -> {'ok', []};
fetch_local(Account, ViewOptionsList) ->
    do_fetch_local(Account, ViewOptionsList, []).

-spec do_fetch_local(kz_term:ne_binary(), kz_term:proplists(), kz_json:objects()) ->
                            {'ok', kz_transactions()} |
                            {'error', any()}.
do_fetch_local(Account, [ViewOptions|ViewOptionsList], Acc) ->
    case kazoo_modb:get_results(Account, <<"transactions/by_timestamp">>, ViewOptions) of
        {'error', _}=Error -> Error;
        {'ok', ViewRes} -> do_fetch_local(Account, ViewOptionsList, ViewRes ++ Acc)
    end;
do_fetch_local(_Account, [], ViewRes) ->
    Transactions = [kz_transaction:from_json(kz_json:get_value(<<"doc">>, JObj))
                    || JObj <- ViewRes
                   ],
    {'ok', Transactions}.

-spec fetch_bookkeeper(kz_term:ne_binary(), kz_term:proplists()) ->
                              {'ok', kz_transactions()} |
                              {'error', any()}.
fetch_bookkeeper(Account, ViewOptionsList) ->
    do_fetch_bookkeeper(Account, ViewOptionsList, []).

-spec do_fetch_bookkeeper(kz_term:ne_binary(), kz_term:proplists(), kz_json:objects()) ->
                                 {'ok', kz_transactions()} |
                                 {'error', any()}.
do_fetch_bookkeeper(Account, [ViewOptions|ViewOptionsList], Acc) ->
    From = props:get_value('startkey', ViewOptions),
    To   = props:get_value('endkey', ViewOptions),
    case fetch_bookkeeper(Account, From, To) of
        {'error', _R}=Error -> Error;
        {'ok', Transactions} ->
            do_fetch_bookkeeper(Account, ViewOptionsList, Transactions ++ Acc)
    end;
do_fetch_bookkeeper(_Account, [], Transactions) ->
    {'ok', Transactions}.

%%------------------------------------------------------------------------------
%% @doc fetch last bookkeeper transactions from From to To
%% @end
%%------------------------------------------------------------------------------
-spec fetch_bookkeeper(kz_term:ne_binary(), kz_time:gregorian_seconds(), kz_time:gregorian_seconds()) ->
                              {'ok', kz_transactions()} |
                              {'error', any()}.
fetch_bookkeeper(Account, From, To) ->
    Bookkeeper = ?KZ_SERVICE_MASTER_ACCOUNT_BOOKKEEPER,
    try Bookkeeper:transactions(Account, From, To) of
        {'ok', _}=R -> R;
        {'error', _}=Error -> Error
    catch
        _:_ ->
            {'error', <<"error while fetching bookkeeper transactions">>}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec de_duplicate_transactions(kz_transactions(), kz_transactions()) -> kz_transactions().
de_duplicate_transactions(Transactions, BookkeeperTransactions) ->
    PropsTr = transactions_to_props(Transactions),
    PropsBTr = transactions_to_props(BookkeeperTransactions),
    de_duplicate_transactions(PropsTr, PropsBTr, []).

-spec de_duplicate_transactions(kz_term:proplist(), kz_term:proplist(), kz_transactions()) -> kz_transactions().
de_duplicate_transactions([], BookkeeperTransactions, Acc) ->
    [Transaction || {_, Transaction} <- BookkeeperTransactions] ++ Acc;
de_duplicate_transactions([{Key, Value}|Transactions], BookkeeperTransactions, Acc) ->
    case props:is_defined(Key, BookkeeperTransactions) of
        'true'  -> de_duplicate_transactions(Transactions, BookkeeperTransactions, Acc);
        'false' -> de_duplicate_transactions(Transactions, BookkeeperTransactions, [Value|Acc])
    end.

-spec transactions_to_props(kz_transactions()) -> kz_term:proplist().
transactions_to_props(Transactions) ->
    lists:foldl(fun transaction_to_prop_fold/2, [], Transactions).

-spec transaction_to_prop_fold(kz_transaction:transaction(), kz_term:proplist()) -> kz_term:proplist().
transaction_to_prop_fold(Transaction, Acc) ->
    Amount = kz_transaction:amount(Transaction),
    Timestamp = kz_transaction:created(Transaction),
    {Date, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    [{{Date, Amount}, Transaction}|Acc].

%%------------------------------------------------------------------------------
%% @doc Save list of record
%% @end
%%------------------------------------------------------------------------------
-type save_acc() :: [{'ok' | 'error', kz_transaction:transaction()}].

-spec save(kz_transactions()) -> save_acc().
save(L) ->
    save(L, []).

-spec save(kz_transactions(), save_acc()) -> save_acc().
save([], Acc) ->
    lists:reverse(Acc);
save([Transaction | Transactions], Acc) ->
    case kz_transaction:save(Transaction) of
        {'ok', _}=Ok ->
            save(Transactions, [Ok | Acc]);
        {'error', _} ->
            save(Transactions, [{'error', Transaction} | Acc])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-type remove_acc() :: ['ok' | {'error', kz_transaction:transaction()}].

-spec remove(kz_transactions()) -> remove_acc().
remove(Transactions) ->
    remove(Transactions, []).

-spec remove(kz_transactions(), remove_acc()) -> remove_acc().
remove([], Acc) ->
    lists:reverse(Acc);
remove([Transaction | Transactions], Acc) ->
    case kz_transaction:remove(Transaction) of
        'ok' ->
            remove(Transactions, ['ok' | Acc]);
        {'error', _} ->
            remove(Transactions, [{'error', Transaction} | Acc])
    end.


%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec to_json(kz_transactions()) -> kz_json:objects().
to_json(Transactions) ->
    [kz_transaction:to_json(Tr) || Tr <- Transactions].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec to_public_json(kz_transactions()) -> kz_json:objects().
to_public_json(Transactions) ->
    [kz_transaction:to_public_json(Tr) ||  Tr <- Transactions].
