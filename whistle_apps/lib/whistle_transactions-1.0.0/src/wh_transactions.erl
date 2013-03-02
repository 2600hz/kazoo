%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-module(wh_transactions).

-include_lib("whistle/include/wh_types.hrl").

-export([get_current_balance/1]).
-export([call_charges/2]).
-export([default_reason/0]).
-export([is_valid_reason/1]).
-export([reason_code/1]).
-export([filter_by_reason/2
         ,save/1
         ,fetch_since/2
         ,fetch_last/2
        ]).

-define(REASONS, [{<<"per_minute_call">>, 1001}
                  ,{<<"sub_account_per_minute_call">>, 1002}
                  ,{<<"activation_charge">>, 2001}
                  ,{<<"manual_credit_addition">>, 3001}
                  ,{<<"auto_credit_addition">>, 3002}
                  ,{<<"admin_discretion">>, 3003}
                  ,{<<"unknown">>, 9999}
                 ]).

-type wh_transactions() :: [wh_transaction:transaction(), ...].
-export_type([wh_transactions/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Fetch a transaction from the database
%% @end
%%--------------------------------------------------------------------
-spec get_current_balance/1 :: (ne_binary()) -> integer().
get_current_balance(AccountId) ->
    AccountDB = wh_util:format_account_id(AccountId, encoded),
    case couch_mgr:get_results(AccountDB, <<"transactions/credit_remaining">>, []) of
        {ok, []} ->
            lager:debug("no current balance for ~s", [AccountId]),
            0;
        {ok, [ViewRes|_]} ->
            wh_json:get_integer_value(<<"value">>, ViewRes, 0);
        {error, _R} ->
            lager:warning("unable to get current balance for ~s: ~p", [AccountId, _R]),
            0
    end.

-spec call_charges(ne_binary(), ne_binary()) -> integer().
call_charges(Ledger, CallId) ->
    LedgerDb = wh_util:format_account_id(Ledger, encoded),
    ViewOptions = [reduce
                   ,group
                   ,{<<"key">>, CallId}
                  ],
    case couch_mgr:get_results(LedgerDb, <<"transactions/per_minute_cost">>, ViewOptions) of
        {ok, []} -> 0;
        {ok, [JObj|_]} -> wh_json:get_integer_value(<<"value">>, JObj, 0);
        {error, _R} ->
            lager:debug("unable to get per minute cost for ~s: ~p", [CallId, _R]),
            0
    end.

default_reason() ->
    <<"unknown">>.

is_valid_reason(Reason) ->
    lists:keyfind(Reason, 1, ?REASONS) =/= false.

reason_code(Reason) ->
    {_, Code} = lists:keyfind(Reason, 1, ?REASONS),
    Code.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Filter list of transactions by reason
%% @end
%%--------------------------------------------------------------------
-spec filter_by_reason/2 :: (ne_binary(), wh_transaction:wh_transactions()) -> wh_transaction:wh_transactions().
filter_by_reason(Reason, Transactions) ->
    lists:foldr(
      fun(Transaction, Acc) ->
              case wh_transaction:is_reason(Reason, Transaction) of
                  true -> [Transaction | Acc];
                  false -> Acc
              end
      end, [], Transactions).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Save list of record
%% @end
%%--------------------------------------------------------------------
-spec save/1 :: (wh_transaction:wh_transactions()) -> wh_transaction:wh_transactions().
save(L) ->
    save(L, []).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Save list of record
%% @end
%%--------------------------------------------------------------------
-spec save/2 :: (wh_transaction:wh_transactions(), wh_transaction:wh_transactions()) -> wh_transaction:wh_transactions().
save([], Acc) ->
    lists:reverse(Acc);
save([Transaction | Transactions], Acc) ->
    case wh_transaction:save(Transaction) of
        {ok, _}=Ok ->
            save(Transactions, [Ok | Acc]);
        {error, _} ->
            save(Transactions, [{error, Transaction} | Acc])
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% fetch last transactions
%% @end
%%--------------------------------------------------------------------
-spec fetch_last/2 :: (ne_binary(), integer()) -> wh_transaction:wh_transactions().
fetch_last(AccountId, Num) ->
    AccountDB = wh_util:format_account_id(AccountId, encoded),
    ViewOptions = [{limit, Num}
                   ,include_docs
                  ],
    case couch_mgr:get_results(AccountDB, <<"transactions/by_timestamp">>, ViewOptions) of
        {ok, []} ->
            lager:debug("no transactions for that account ~p", [AccountId]),
            [];
        {ok, ViewRes} ->
            viewres_to_recordlist(ViewRes)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% fetch last transaction from date to now
%% @end
%%--------------------------------------------------------------------
-spec fetch_since/2 :: (ne_binary(), integer()) -> wh_transaction:wh_transactions().
fetch_since(AccountId, Date) ->
    AccountDB = wh_util:format_account_id(AccountId, encoded),
    Now = wh_util:current_tstamp(),
    ViewOptions = [{startkey, Date}
                   ,{endkey, Now}
                   ,include_docs
                  ],
    case couch_mgr:get_results(AccountDB, <<"transactions/by_timestamp">>, ViewOptions) of
        {ok, []} ->
            lager:debug("no transactions for that range from ~p to ~p on ~p", [Date, Now, AccountId]),
            [];
        {ok, ViewRes} ->
            viewres_to_recordlist(ViewRes)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% fetch last transaction
%% @end
%%--------------------------------------------------------------------
-spec viewres_to_recordlist/1 :: (list()) -> wh_transaction:wh_transactions().
viewres_to_recordlist(ViewRes) ->
    L = [wh_json:get_value(<<"doc">>, Tr) || Tr <- ViewRes],
    [wh_transaction:from_json(Tr) || Tr <- L].
