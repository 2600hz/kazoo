%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
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

-include_lib("kazoo_transactions/include/kazoo_transactions.hrl").

-type kz_transactions() :: kz_transaction:transactions().
-export_type([kz_transactions/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec call_charges(ne_binary(), ne_binary()) -> integer().
call_charges(Ledger, CallId) ->
    call_charges(Ledger, CallId, 'true').

-spec call_charges(ne_binary(), ne_binary(), ne_binary() | boolean()) -> integer() | kz_transactions().
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

-spec call_charges(ne_binary(), ne_binary(), ne_binary(), boolean()) -> integer() | kz_transactions().
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Filter list of transactions by reason
%% @end
%%--------------------------------------------------------------------
-spec filter_by_reason(ne_binary(), kz_transactions()) -> kz_transactions().
filter_by_reason(<<"only_bookkeeper">>, Transactions) ->
    {BTTransactions, LTransactions} = lists:partition(fun is_from_braintree/1, Transactions),
    case kapps_config:get_atom(<<"services">>, <<"master_account_bookkeeper">>) of
        'kz_bookkeeper_braintree' -> BTTransactions;
        'kz_bookkeeper_local'     -> LTransactions
    end;
filter_by_reason(<<"no_calls">>, Transactions) -> %% Legacy of only_bookkeeper
    lists:foldr(
      fun(Transaction, Acc) ->
              Code = kz_transaction:code(Transaction),
              case Code >= 1000 andalso Code < 2000 of
                  'false' -> [Transaction | Acc];
                  'true' -> Acc
              end
      end, [], Transactions);
filter_by_reason(<<"only_calls">>, Transactions) ->
    lists:foldr(
      fun(Transaction, Acc) ->
              Code = kz_transaction:code(Transaction),
              case Code >= 1000 andalso Code < 2000 of
                  'true' -> [Transaction | Acc];
                  'false' -> Acc
              end
      end, [], Transactions);
filter_by_reason(Reason, Transactions) ->
    [Transaction || Transaction <- Transactions
                        , kz_transaction:is_reason(Reason, Transaction)
    ].

%% @private
-spec is_from_braintree(kz_transaction:transaction()) -> boolean().
is_from_braintree(Transaction) ->
    kz_transaction:description(Transaction) =:= <<"braintree transaction">>.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Keep only per minute transactions
%% @end
%%--------------------------------------------------------------------
-spec filter_for_per_minute(kz_transactions()) -> kz_transactions().
filter_for_per_minute(Transactions) ->
    [Transaction || Transaction <- Transactions
                        , is_per_minute(Transaction)
    ].

%% @private
-spec is_per_minute(kz_transaction:transaction()) -> boolean().
is_per_minute(Transaction) ->
    case kz_transaction:code(Transaction) of
        ?CODE_PER_MINUTE_CALL -> 'true';
        ?CODE_SUB_ACCOUNT_PER_MINUTE_CALL -> 'true';
        _Code -> 'false'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% fetch last transactions
%% @end
%%--------------------------------------------------------------------
-spec fetch_last(ne_binary(), pos_integer()) ->
                        {'ok', kz_transactions()} |
                        {'error', any()}.
fetch_last(Account, Count) ->
    ViewOptions = [{'limit', Count}
                  ,'include_docs'
                  ],
    fetch_local(Account, [ViewOptions]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% fetch last transactions from From to To
%% @end
%%--------------------------------------------------------------------
-spec fetch(ne_binary(), gregorian_seconds(), gregorian_seconds()) ->
                   {'ok', kz_transactions()} |
                   {'error', any()}.
fetch(Account, From, To) ->
    ViewOptionsList = get_range(Account, From, To),
    fetch(Account, ViewOptionsList).

%% @private
-spec fetch(ne_binary(), kz_proplists()) ->
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

%% @private
-spec get_range(ne_binary(), gregorian_seconds(), gregorian_seconds()) ->
                       ViewOptionsList :: kz_proplists().
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% fetch last local transactions from From to To
%% @end
%%--------------------------------------------------------------------
-spec fetch_local(ne_binary(), gregorian_seconds(), gregorian_seconds()) ->
                         {'ok', kz_transactions()} |
                         {'error', any()}.
fetch_local(Account, From, To) ->
    ViewOptionsList = get_range(Account, From, To),
    fetch_local(Account, ViewOptionsList).

%% @private
-spec fetch_local(ne_binary(), kz_proplists()) ->
                         {'ok', kz_transactions()} |
                         {'error', any()}.
fetch_local(_Account, []) -> {'ok', []};
fetch_local(Account, ViewOptionsList) ->
    do_fetch_local(Account, ViewOptionsList, []).

%% @private
-spec do_fetch_local(ne_binary(), kz_proplists(), kz_json:objects()) ->
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

%% @private
-spec fetch_bookkeeper(ne_binary(), kz_proplists()) ->
                              {'ok', kz_transactions()} |
                              {'error', any()}.
fetch_bookkeeper(Account, ViewOptionsList) ->
    do_fetch_bookkeeper(Account, ViewOptionsList, []).

%% @private
-spec do_fetch_bookkeeper(ne_binary(), kz_proplists(), kz_json:objects()) ->
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% fetch last bookkeeper transactions from From to To
%% @end
%%--------------------------------------------------------------------
-spec fetch_bookkeeper(ne_binary(), gregorian_seconds(), gregorian_seconds()) ->
                              {'ok', kz_transactions()} |
                              {'error', any()}.
fetch_bookkeeper(Account, From, To) ->
    Bookkeeper = kapps_config:get_atom(<<"services">>, <<"master_account_bookkeeper">>),
    try Bookkeeper:transactions(Account, From, To) of
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
-spec de_duplicate_transactions(kz_transactions(), kz_transactions()) -> kz_transactions().
-spec de_duplicate_transactions(kz_proplist(), kz_proplist(), kz_transactions()) -> kz_transactions().
de_duplicate_transactions(Transactions, BookkeeperTransactions) ->
    PropsTr = transactions_to_props(Transactions),
    PropsBTr = transactions_to_props(BookkeeperTransactions),
    de_duplicate_transactions(PropsTr, PropsBTr, []).

de_duplicate_transactions([], BookkeeperTransactions, Acc) ->
    [Transaction || {_, Transaction} <- BookkeeperTransactions] ++ Acc;
de_duplicate_transactions([{Key, Value}|Transactions], BookkeeperTransactions, Acc) ->
    case props:is_defined(Key, BookkeeperTransactions) of
        'true'  -> de_duplicate_transactions(Transactions, BookkeeperTransactions, Acc);
        'false' -> de_duplicate_transactions(Transactions, BookkeeperTransactions, [Value|Acc])
    end.

%% @private
-spec transactions_to_props(kz_transactions()) -> kz_proplist().
transactions_to_props(Transactions) ->
    lists:foldl(fun transaction_to_prop_fold/2, [], Transactions).

-spec transaction_to_prop_fold(kz_transaction:transaction(), kz_proplist()) -> kz_proplist().
transaction_to_prop_fold(Transaction, Acc) ->
    Amount = kz_transaction:amount(Transaction),
    Timestamp = kz_transaction:created(Transaction),
    {Date, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    [{{Date, Amount}, Transaction}|Acc].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Save list of record
%% @end
%%--------------------------------------------------------------------
-type save_acc() :: [{'ok' | 'error', kz_transaction:transaction()}].

-spec save(kz_transactions()) -> save_acc().
-spec save(kz_transactions(), save_acc()) -> save_acc().
save(L) ->
    save(L, []).

save([], Acc) ->
    lists:reverse(Acc);
save([Transaction | Transactions], Acc) ->
    case kz_transaction:save(Transaction) of
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
-type remove_acc() :: ['ok' | {'error', kz_transaction:transaction()}].

-spec remove(kz_transactions()) -> remove_acc().
-spec remove(kz_transactions(), remove_acc()) -> remove_acc().
remove(Transactions) ->
    remove(Transactions, []).

remove([], Acc) ->
    lists:reverse(Acc);
remove([Transaction | Transactions], Acc) ->
    case kz_transaction:remove(Transaction) of
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
-spec to_json(kz_transactions()) -> kz_json:objects().
to_json(Transactions) ->
    [kz_transaction:to_json(Tr) || Tr <- Transactions].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec to_public_json(kz_transactions()) -> kz_json:objects().
to_public_json(Transactions) ->
    [kz_transaction:to_public_json(Tr) ||  Tr <- Transactions].
