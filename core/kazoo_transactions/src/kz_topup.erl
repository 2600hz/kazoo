%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_topup).

-export([init/2]).
-export([should_topup/1, should_topup/2]).

-include("include/kazoo_transactions.hrl").

-define(KZ_SERVICES_DB, <<"services">>).

-type error() :: 'topup_disabled' |
                 'topup_undefined' |
                 'amount_undefined' |
                 'threshold_undefined' |
                 'balance_above_threshold' |
                 'amount_and_threshold_undefined' |
                 atom().


%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec init(api_binary(), integer()) ->
                  'ok' |
                  {'error', error()}.
init(Account, CurrentBalance) ->
    Balance = wht_util:units_to_dollars(CurrentBalance),
    case get_top_up(Account) of
        {'error', _}=E -> E;
        {'ok', Amount, Threshold} ->
            lager:info("checking if account ~s balance $~w is below top up threshold $~w"
                      ,[Account, Balance, Threshold]),
            AccountId = kz_util:format_account_id(Account, 'raw'),
            maybe_top_up(AccountId, Balance, Amount, Threshold)
    end.

-spec should_topup(ne_binary()) -> boolean().
-spec should_topup(ne_binary(), integer()) -> boolean().
should_topup(AccountId) ->
    case wht_util:current_balance(AccountId) of
        {'ok', CurrentBalance} -> should_topup(AccountId, CurrentBalance);
        {'error', _} -> 'false'
    end.

should_topup(AccountId, CurrentBalance) ->
    Balance = wht_util:units_to_dollars(CurrentBalance),
    case get_top_up(AccountId) of
        {'error', _} -> 'false';
        {'ok', _Amount, Threshold} ->
            lager:info("checking if account ~s balance $~w is below top up threshold $~w"
                      ,[AccountId, Balance, Threshold]),
            case should_topup(AccountId, Balance, Threshold) of
                {'error', _} -> 'false';
                Boolean -> Boolean
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_top_up(api_binary() | kz_account:doc()) ->
                        {'error', error()} |
                        {'ok', integer(), integer()}.
get_top_up(<<_/binary>> = Account) ->
    case kapps_config:get_is_true(?TOPUP_CONFIG, <<"enable">>, 'false') of
        'false' -> {'error', 'topup_disabled'};
        'true' ->
            case kz_account:fetch(Account) of
                {'error', _E}=Error ->
                    lager:error("could not open account ~s: ~p", [Account, _E]),
                    Error;
                {'ok', AccountJObj} -> get_top_up(kz_json:get_value(<<"topup">>, AccountJObj))
            end
    end;
get_top_up('undefined') -> {'error', 'topup_undefined'};
get_top_up(JObj) ->
    case
        {kz_json:get_integer_value(<<"amount">>, JObj)
        ,kz_json:get_integer_value(<<"threshold">>, JObj)
        }
    of
        {'undefined', _} -> {'error', 'amount_undefined'};
        {_, 'undefined'} -> {'error', 'threshold_undefined'};
        {Amount, Threshold} when Amount > 0
                                 andalso Threshold > 0 ->
            {'ok', Amount, Threshold};
        {_, _} -> {'error', 'amount_and_threshold_undefined'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_top_up(ne_binary(), number(), integer(), integer()) ->
                          'ok' |
                          {'error', error()}.
maybe_top_up(AccountId, Balance, Amount, Threshold) ->
    case should_topup(AccountId, Balance, Threshold) of
        'true' -> top_up(AccountId, Amount);
        'false' -> 'ok';
        {'error', _} = Error -> Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec should_topup(ne_binary(), number(), integer()) ->
                          boolean() |
                          {'error', error()}.
should_topup(AccountId, Balance, Threshold) when Balance =< Threshold ->
    To = kz_time:current_tstamp(),
    From = To - ?SECONDS_IN_DAY,
    case kz_transactions:fetch_local(AccountId, From, To) of
        {'error', _Reason} = Error ->
            lager:warning("failed to fetch recent transactions for ~s: ~p", [AccountId, _Reason]),
            Error;
        {'ok', Transactions} ->
            TopupTransactions = kz_transactions:filter_by_reason(<<"topup">>, Transactions),
            is_topup_today(AccountId, TopupTransactions)
    end;
should_topup(_AccountId, _Balance, _Threshold) ->
    lager:warning("balance (~p) is still > to threshold (~p) for account ~s", [_Balance, _Threshold, _AccountId]),
    {'error', 'balance_above_threshold'}.

-spec is_topup_today(ne_binary(), kz_json:objects()) -> boolean().
is_topup_today(_AccountId, []) ->
    lager:info("no top up transactions found for ~s, processing...", [_AccountId]),
    'true';
is_topup_today(_AccountId, _TopupTransactions) ->
    lager:info("today auto top up for ~s already done, skipping...", [_AccountId]),
    'false'.

%%--------------------------------------------------------------------
%% @private
%% @do
%%
%% @end
%%--------------------------------------------------------------------
-spec top_up(ne_binary(), integer()) -> 'ok' | {'error', any()}.
top_up(AccountId, Amount) ->
    Services = kz_services:fetch(AccountId),
    Transaction = kz_transaction:debit(AccountId, wht_util:dollars_to_units(Amount)),
    Transaction1 = kz_transaction:set_reason(<<"topup">>, Transaction),

    lager:info("attemptting to top up account ~s for ~p", [AccountId, Amount]),
    case kz_services:charge_transactions(Services, [Transaction1]) of
        [] ->
            lager:info("account ~s top up successfully for ~p", [AccountId, Amount]),
            case kz_transaction:save(kz_transaction:set_type(<<"credit">>, Transaction1)) of
                {'ok', _} ->
                    lager:info("auto top up transaction for account ~s saved succesfully", [AccountId]);
                {'error', 'conflict'} ->
                    lager:warning("did not write top up transaction for account ~s already exist for today", [AccountId]);
                {'error', _Reason} ->
                    lager:error("failed to write top up transaction ~p , for account ~s (amount: ~p)"
                               ,[_Reason, AccountId, Amount]
                               )
            end;
        [FailedTransaction] ->
            _Reason = kz_json:get_value(<<"failed_reason">>, FailedTransaction),
            lager:error("failed to top up account ~s: ~p", [AccountId, _Reason]),
            {'error', 'bookkeeper_failed'}
    end.
