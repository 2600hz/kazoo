%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_topup).

-export([should_topup/1
        ,should_topup/2
        ]).
-export([get_topup/1]).
-export([maybe_topup/1
        ,maybe_topup/2
        ]).
-export([topup/4]).

-include("services.hrl").

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
-spec should_topup(kz_term:ne_binary()) -> boolean().
should_topup(?NE_BINARY=AccountId) ->
    case kz_currency:available_units(AccountId) of
        {'ok', AvailableUnits} -> should_topup(AccountId, AvailableUnits);
        {'error', _} -> 'false'
    end.

-spec should_topup(kz_term:ne_binary(), kz_currency:units()) -> boolean().
should_topup(?NE_BINARY=AccountId, AvailableUnits) ->
    case get_topup(AccountId) of
        {'error', _} -> 'false';
        {'ok', _ReplinishUnits, ThresholdUnits} ->
            lager:info("checking if account ~s balance $~w is below top up threshold $~w"
                      ,[AccountId
                       ,kz_currency:units_to_dollars(AvailableUnits)
                       ,kz_currency:units_to_dollars(ThresholdUnits)
                       ]),
            should_topup(AccountId, AvailableUnits, ThresholdUnits) =:= 'true'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec should_topup(kz_term:ne_binary(), kz_currency:units(), kz_currency:units()) ->
                          'true' |
                          {'error', error()}.
should_topup(AccountId, AvailableUnits, ThresholdUnits)
  when AvailableUnits =< ThresholdUnits ->
    case do_transactions_have_topup(AccountId)
        orelse do_ledgers_have_topup(AccountId)
    of
        'true' ->
            lager:info("today auto top up for ~s already done, skipping..."
                      ,[AccountId]),
            {'error', 'topup_daily_limit'};
        'false' ->
            lager:info("no top up transactions or ledgers found for ~s, processing..."
                      ,[AccountId]),
            'true'
    end;
should_topup(_AccountId, _AvailableUnits, _ThresholdUnits) ->
    lager:debug("balance (~p) is still > to threshold (~p) for account ~s"
               ,[kz_currency:units_to_dollars(_AvailableUnits)
                ,kz_currency:units_to_dollars(_ThresholdUnits)
                ,_AccountId
                ]
               ),
    {'error', 'balance_above_threshold'}.

-spec do_transactions_have_topup(kz_term:ne_binary() | kz_transactions:transactions()) -> boolean().
do_transactions_have_topup(?NE_BINARY=AccountId) ->
    To = kz_time:now_s(),
    From = To - ?SECONDS_IN_DAY,
    {'ok', Transactions} = kz_transactions:fetch(AccountId, From, To),
    do_transactions_have_topup(Transactions);
do_transactions_have_topup([]) -> 'false';
do_transactions_have_topup([Transaction|Transactions]) ->
    case kz_transaction:executor_module(Transaction) =:= kz_term:to_binary(?MODULE)
        andalso kz_transaction:executor_trigger(Transaction) =:= <<"automatic">>
    of
        'false' -> do_transactions_have_topup(Transactions);
        'true' -> 'true'
    end.

-spec do_ledgers_have_topup(kz_term:ne_binary() | kz_ledgers:ledgers()) -> boolean().
do_ledgers_have_topup(?NE_BINARY=AccountId) ->
    To = kz_time:now_s(),
    From = To - ?SECONDS_IN_DAY,
    {'ok', Ledgers} = kz_ledgers:list_source(AccountId, <<"kazoo-services">>, From, To),
    do_ledgers_have_topup(Ledgers);
do_ledgers_have_topup([]) -> 'false';
do_ledgers_have_topup([Ledger|Ledgers]) ->
    case kz_ledger:source_service(Ledger) =:= <<"kazoo-services">>
        andalso kz_ledger:executor_module(Ledger) =:= kz_term:to_binary(?MODULE)
        andalso kz_ledger:executor_trigger(Ledger) =:= <<"automatic">>
    of
        'false' -> do_ledgers_have_topup(Ledgers);
        'true' -> 'true'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_topup(kz_term:api_ne_binary() | kzd_accounts:doc()) ->
                       {'error', error()} |
                       {'ok', kz_currency:units(), kz_currency:units()}.
get_topup('undefined') -> {'error', 'topup_undefined'};
get_topup(?NE_BINARY = Account) ->
    case kapps_config:get_is_true(?TOPUP_CONFIG, <<"enable">>, 'false') of
        'false' -> {'error', 'topup_disabled'};
        'true' ->
            case kzd_accounts:fetch(Account) of
                {'error', _E}=Error ->
                    lager:error("could not open account ~s: ~p", [Account, _E]),
                    Error;
                {'ok', AccountJObj} -> get_account_topup(AccountJObj)
            end
    end.

-spec get_account_topup(kzd_accounts:doc()) ->
                               {'ok', kz_currency:units(), kz_currency:units()} |
                               {'error', error()}.
get_account_topup(AccountJObj) ->
    case
        {kzd_accounts:topup_amount(AccountJObj)
        ,kzd_accounts:topup_threshold(AccountJObj)
        }
    of
        {'undefined', _} -> {'error', 'amount_undefined'};
        {_, 'undefined'} -> {'error', 'threshold_undefined'};
        {Amount, Threshold} when Amount > 0
                                 andalso Threshold > 0 ->
            {'ok'
            ,kz_currency:dollars_to_units(Amount)
            ,kz_currency:dollars_to_units(Threshold)
            };
        {_, _} -> {'error', 'amount_and_threshold_undefined'}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_topup(kz_term:api_ne_binary()) ->
                         {'ok', kz_transaction:transaction(), kz_ledger:ledger()} |
                         {'error', error()}.
maybe_topup(Account) ->
    {'ok', AvailableUnits} = kz_currency:available_units(Account),
    maybe_topup(Account, AvailableUnits).

-spec maybe_topup(kz_term:api_ne_binary(), kz_currency:units()) ->
                         {'ok', kz_transaction:transaction(), kz_ledger:ledger()} |
                         {'error', error()}.
maybe_topup(Account, AvailableUnits) ->
    case get_topup(Account) of
        {'error', _}=E -> E;
        {'ok', ReplinishUnits, ThresholdUnits} ->
            lager:info("checking if account ~s balance $~w is below top up threshold $~w"
                      ,[Account
                       ,kz_currency:units_to_dollars(AvailableUnits)
                       ,kz_currency:units_to_dollars(ThresholdUnits)
                       ]),
            AccountId = kz_util:format_account_id(Account, 'raw'),
            maybe_topup(AccountId, AvailableUnits, ReplinishUnits, ThresholdUnits)
    end.

-spec maybe_topup(kz_term:ne_binary(), kz_currency:units(), kz_currency:units(), kz_currency:units()) ->
                         {'ok', kz_transaction:transaction(), kz_ledger:ledger()} |
                         {'error', error()}.
maybe_topup(AccountId, AvailableUnits, ReplinishUnits, ThresholdUnits) ->
    case should_topup(AccountId, AvailableUnits, ThresholdUnits) of
        'true' -> topup(AccountId, ReplinishUnits);
        {'error', _} = Error -> Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec topup(kz_term:ne_binary(), kz_currency:units()) ->
                   {'ok', kz_transaction:transaction(), kz_ledger:ledger()} |
                   {'error', any()}.
topup(AccountId, ReplinishUnits) ->
    topup(AccountId, ReplinishUnits, <<"automatic">>).

-spec topup(kz_term:ne_binary(), kz_currency:units(), kz_term:ne_binary()) ->
                   {'ok', kz_transaction:transaction(), kz_ledger:ledger()} | {'error', any()}.
topup(AccountId, ReplinishUnits, Trigger) ->
    topup(AccountId, ReplinishUnits, Trigger, 'undefined').

-spec topup(kz_term:ne_binary(), kz_currency:units(), kz_term:ne_binary(), kz_term:api_object()) ->
                   {'ok', kz_transaction:transaction(), kz_ledger:ledger()} | {'error', any()}.
topup(AccountId, ReplinishUnits, Trigger, Audit) ->
    case create_transaction(AccountId, ReplinishUnits, Trigger, Audit) of
        {'error', _Reason} = Error -> Error;
        {'ok', Transaction} ->
            {'ok', Ledger} = create_ledger(Transaction),
            _ = reset_low_balance_notification(AccountId),
            {'ok', Transaction, Ledger}
    end.

-spec create_transaction(kz_term:ne_binary(), kz_currency:units(), kz_term:ne_binary(), kz_term:api_object()) ->
                                {'ok', kz_json:object()} | {'error', any()}.
create_transaction(AccountId, ReplinishUnits, Trigger, Audit) ->
    Setters =
        props:filter_empty(
          [{fun kz_transaction:set_account/2, AccountId}
          ,{fun kz_transaction:set_description/2, <<"Kazoo services credit replenishment">>}
          ,{fun kz_transaction:set_executor_trigger/2, Trigger}
          ,{fun kz_transaction:set_executor_module/2, kz_term:to_binary(?MODULE)}
          ,{fun kz_transaction:set_audit/2, Audit}
          ,{fun kz_transaction:set_unit_amount/2, ReplinishUnits}
          ]
         ),
    case kz_transaction:sale(kz_transaction:setters(Setters)) of
        {'error', _R} = Error -> Error;
        {'ok', Transaction} ->
            {'ok', Transaction}
    end.

-spec create_ledger(kz_transaction:transaction()) -> {'ok', kz_ledger:ledger()} | {'error', any()}.
create_ledger(Transaction) ->
    Setters =
        props:filter_empty(
          [{fun kz_ledger:set_account/2, kz_transaction:account_id(Transaction)}
          ,{fun kz_ledger:set_source_id/2, kz_transaction:id(Transaction)}
          ,{fun kz_ledger:set_description/2, kz_transaction:description(Transaction)}
          ,{fun kz_ledger:set_metadata/2, kz_transaction:bookkeeper(Transaction)}
          ,{fun kz_ledger:set_executor_trigger/2, kz_transaction:executor_trigger(Transaction)}
          ,{fun kz_ledger:set_executor_module/2, kz_transaction:executor_module(Transaction)}
          ,{fun kz_ledger:set_audit/2, kz_transaction:audit(Transaction)}
          ,{fun kz_ledger:set_unit_amount/2, kz_transaction:unit_amount(Transaction)}
          ,{fun kz_ledger:set_period_start/2, kz_time:now_s()}
          ,{fun kz_ledger:set_source_service/2, <<"kazoo-services">>}
          ]
         ),
    case kz_ledger:credit(kz_ledger:setters(Setters)) of
        {'error', _R} = Error -> Error;
        {'ok', Ledger} ->
            {'ok', Ledger}
    end.

-spec reset_low_balance_notification(kz_term:ne_binary()) -> 'ok'.
reset_low_balance_notification(AccountId) ->
    case kzd_accounts:fetch(AccountId) of
        {'error', _} -> 'ok';
        {'ok', AccountJObj0} ->
            AccountJObj1 = kzd_accounts:reset_low_balance_sent(AccountJObj0),
            AccountJObj2 = kzd_accounts:remove_low_balance_tstamp(AccountJObj1),
            kzd_accounts:save(AccountJObj2)
    end.
