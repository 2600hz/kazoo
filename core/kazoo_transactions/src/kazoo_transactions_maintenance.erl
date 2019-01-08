%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_transactions_maintenance).

-export([balance/1
        ,balance/3
        ]).
-export([disable_top_up/0]).
-export([enable_top_up/0]).
-export([top_up_status/0, top_up_status/1]).

-include_lib("kazoo_transactions/include/kazoo_transactions.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec balance(kz_term:ne_binary()) -> dollars().
balance(Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    {'ok', Balance} = wht_util:current_balance(AccountId),
    wht_util:units_to_dollars(Balance).

-spec balance(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> dollars().
balance(Account, Year, Month) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    {'ok', Balance} = wht_util:previous_balance(AccountId, Year, Month),
    wht_util:units_to_dollars(Balance).

%%------------------------------------------------------------------------------
%% @doc Enable tauto top up
%% @end
%%------------------------------------------------------------------------------
-spec enable_top_up() -> 'ok'.
enable_top_up() ->
    kapps_config:set(?TOPUP_CONFIG, <<"enable">>, 'true'),
    io:format("auto top up enabled ~n").

%%------------------------------------------------------------------------------
%% @doc Disable tauto top up
%% @end
%%------------------------------------------------------------------------------
-spec disable_top_up() -> 'ok'.
disable_top_up() ->
    kapps_config:set(?TOPUP_CONFIG, <<"enable">>, 'false'),
    io:format("auto top up disabled ~n", []).

-spec top_up_status() -> 'ok'.
top_up_status() ->
    IsEnabled = case kapps_config:get_is_true(?TOPUP_CONFIG, <<"enable">>) of
                    'true' -> "enabled";
                    _ -> "disabled"
                end,
    io:format("auto top up is ~s~n", [IsEnabled]).

-spec top_up_status(kz_term:ne_binary()) -> 'ok'.
top_up_status(AccountId) ->
    {NotifyThreshold, TopupAmount, TopupThreshold} = get_topup_thresholds(AccountId),
    io:format("+-----------------+------------------+------------------+---------------+-------------------------+~n"),
    io:format("| Current Balance | Notify threshold | Top up threshold | Top up Amount | Auto top up in 24 hours |~n"),
    io:format("+=================+==================+==================+===============+=========================+~n"),
    io:format("| ~-15w | ~-16w | ~-16w | ~-13w | ~-23w |~n"
             ,[current_balance(AccountId)
              ,NotifyThreshold
              ,TopupThreshold
              ,TopupAmount
              ,is_topup_today(AccountId)
              ]),
    io:format("+-----------------+------------------+------------------+---------------+-------------------------+~n").

-spec current_balance(kz_term:ne_binary()) -> dollars().
current_balance(AccountId) ->
    case wht_util:current_balance(AccountId) of
        {'ok', Balance} -> wht_util:units_to_dollars(Balance);
        {'error', _} -> 0
    end.

-spec get_topup_thresholds(kz_term:ne_binary()) -> {kz_term:api_integer(), integer() | string(), integer() | string()}.
get_topup_thresholds(AccountId) ->
    case kzd_accounts:fetch(AccountId) of
        {'error', _Reason} ->
            io:format("failed to open account ~p: ~p", [AccountId, _Reason]),
            {'undefined', 'undefined', 'undefined'};
        {'ok', JObj} ->
            {kzd_accounts:low_balance_threshold(JObj)
            ,kz_json:get_integer_value([<<"topup">>, <<"amount">>], JObj, "N/A")
            ,kz_json:get_integer_value([<<"topup">>, <<"threshold">>], JObj, "N/A")
            }
    end.

-spec is_topup_today(kz_term:ne_binary()) -> boolean().
is_topup_today(AccountId) ->
    To = kz_time:now_s(),
    From = To - ?SECONDS_IN_DAY,
    case kz_transactions:fetch_local(AccountId, From, To) of
        {'error', _Reason} ->
            lager:warning("failed to fetch recent transactions for ~s: ~p", [AccountId, _Reason]),
            'false';
        {'ok', Transactions} ->
            TopupTransactions = kz_transactions:filter_by_reason(<<"topup">>, Transactions),
            TopupTransactions =/= []
    end.
