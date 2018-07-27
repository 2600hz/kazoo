%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_topup).

-export([init/2]).
-export([should_topup/1, should_topup/2]).

-include("transactions.hrl").

-type error() :: 'topup_disabled' |
                 'topup_undefined' |
                 'amount_undefined' |
                 'threshold_undefined' |
                 'balance_above_threshold' |
                 'amount_and_threshold_undefined' |
                 'topup_daily_limit' |
                 atom().


%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init(kz_term:api_binary(), integer()) ->
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec should_topup(kz_term:ne_binary()) -> boolean().
should_topup(AccountId) ->
    case wht_util:current_balance(AccountId) of
        {'ok', CurrentBalance} -> should_topup(AccountId, CurrentBalance);
        {'error', _} -> 'false'
    end.

-spec should_topup(kz_term:ne_binary(), integer()) -> boolean().
should_topup(AccountId, CurrentBalance) ->
    Balance = wht_util:units_to_dollars(CurrentBalance),
    case get_top_up(AccountId) of
        {'error', _} -> 'false';
        {'ok', _Amount, Threshold} ->
            lager:info("checking if account ~s balance $~w is below top up threshold $~w"
                      ,[AccountId, Balance, Threshold]),
            should_topup(AccountId, Balance, Threshold) =:= 'true'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec should_topup(kz_term:ne_binary(), number(), integer()) ->
                          'true' |
                          {'error', error()}.
should_topup(AccountId, Balance, Threshold) when Balance =< Threshold ->
    To = kz_time:now_s(),
    From = To - ?SECONDS_IN_DAY,
    case kz_transactions:fetch_local(AccountId, From, To) of
        {'error', _Reason} = Error ->
            lager:warning("failed to fetch recent transactions for ~s: ~p", [AccountId, _Reason]),
            Error;
        {'ok', Transactions} ->
            TopupTransactions = kz_transactions:filter_by_reason(wht_util:topup(), Transactions),
            is_topup_today(AccountId, TopupTransactions)
    end;
should_topup(_AccountId, _Balance, _Threshold) ->
    lager:warning("balance (~p) is still > to threshold (~p) for account ~s", [_Balance, _Threshold, _AccountId]),
    {'error', 'balance_above_threshold'}.

-spec is_topup_today(kz_term:ne_binary(), kz_json:objects()) -> 'true' | {'error', error()}.
is_topup_today(_AccountId, []) ->
    lager:info("no top up transactions found for ~s, processing...", [_AccountId]),
    'true';
is_topup_today(_AccountId, _TopupTransactions) ->
    lager:info("today auto top up for ~s already done, skipping...", [_AccountId]),
    {'error', 'topup_daily_limit'}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_top_up(kz_term:api_binary() | kzd_accounts:doc()) ->
                        {'error', error()} |
                        {'ok', integer(), integer()}.
get_top_up(<<_/binary>> = Account) ->
    case kapps_config:get_is_true(?TOPUP_CONFIG, <<"enable">>, 'false') of
        'false' -> {'error', 'topup_disabled'};
        'true' ->
            case kzd_accounts:fetch(Account) of
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_top_up(kz_term:ne_binary(), number(), integer(), integer()) ->
                          'ok' |
                          {'error', error()}.
maybe_top_up(AccountId, Balance, Amount, Threshold) ->
    case should_topup(AccountId, Balance, Threshold) of
        'true' -> top_up(AccountId, Amount);
        {'error', _} = Error -> Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec top_up(kz_term:ne_binary(), integer()) -> 'ok' | {'error', any()}.
top_up(AccountId, Amount) ->
    Services = kz_services:fetch(AccountId),
    Transaction = kz_transaction:debit(AccountId, wht_util:dollars_to_units(Amount)),
    Transaction1 = kz_transaction:set_reason(wht_util:topup(), Transaction),

    lager:info("attempting to top up account ~s for ~p", [AccountId, Amount]),
    case kz_services:charge_transactions(Services, [Transaction1]) of
        [] ->
            lager:info("account ~s top up successfully for ~p", [AccountId, Amount]),
            case kz_transaction:save(kz_transaction:set_type(<<"credit">>, Transaction1)) of
                {'ok', _} ->
                    lager:info("auto top up transaction for account ~s saved successfully", [AccountId]);
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
