%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-module(kazoo_transactions_maintenance).

-export([disable_top_up/0]).
-export([enable_top_up/0]).
-export([top_up_status/0, top_up_status/1]).

-include("include/kazoo_transactions.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Enable tauto top up
%% @end
%%--------------------------------------------------------------------
-spec enable_top_up() -> 'ok'.
enable_top_up() ->
    kapps_config:set(?TOPUP_CONFIG, <<"enable">>, 'true'),
    io:format("auto top up enabled ~n").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Disable tauto top up
%% @end
%%--------------------------------------------------------------------
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

-spec top_up_status(ne_binary()) -> 'ok'.
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

-spec current_balance(ne_binary()) -> ne_binary().
current_balance(AccountId) ->
    case wht_util:current_balance(AccountId) of
        {'ok', Balance} -> wht_util:units_to_dollars(Balance);
        {'error', _} -> 0
    end.

-spec get_topup_thresholds(ne_binary()) -> {api_integer(), integer() | string(), integer() | string()}.
get_topup_thresholds(AccountId) ->
    case kz_account:fetch(AccountId) of
        {'error', _Reason} ->
            io:format("failed to open account ~p: ~p", [AccountId, _Reason]),
            {'undefined', 'undefined', 'undefined'};
        {'ok', JObj} ->
            {kz_account:low_balance_threshold(JObj)
            ,kz_json:get_integer_value([<<"topup">>, <<"amount">>], JObj, "N/A")
            ,kz_json:get_integer_value([<<"topup">>, <<"threshold">>], JObj, "N/A")
            }
    end.

-spec is_topup_today(ne_binary()) -> boolean().
is_topup_today(AccountId) ->
    To = kz_time:current_tstamp(),
    From = To - ?SECONDS_IN_DAY,
    case kz_transactions:fetch_local(AccountId, From, To) of
        {'error', _Reason} ->
            lager:warning("failed to fetch recent transactions for ~s: ~p", [AccountId, _Reason]),
            'false';
        {'ok', Transactions} ->
            TopupTransactions = kz_transactions:filter_by_reason(<<"topup">>, Transactions),
            TopupTransactions =/= []
    end.
