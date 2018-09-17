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

-type topup_return() :: {'ok', kz_transaction:transaction() | 'undefined', kz_ledger:ledger()} |
                        {'error', {'transaction_incomplete', kz_transaction:transaction()}} |
                        {'error', {'ledger_error', kz_transaction:transaction() | 'undefined', any()}} |
                        {'error', any()}.

-define(DEFAULT_DESCRIPTION, <<"Kazoo services credit replenishment">>).
-define(DEFAULT_EXECUTOR_TRIGGER, <<"automatic">>).
-define(EXECUTOR_MODULE, kz_term:to_binary(?MODULE)).
-define(SOURCE_SERVICE, <<"kazoo-topup">>).

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
    case kz_transaction:executor_module(Transaction) =:= ?EXECUTOR_MODULE
        andalso kz_transaction:executor_trigger(Transaction) =:= ?DEFAULT_EXECUTOR_TRIGGER
    of
        'false' -> do_transactions_have_topup(Transactions);
        'true' -> 'true'
    end.

-spec do_ledgers_have_topup(kz_term:ne_binary() | kz_ledgers:ledgers()) -> boolean().
do_ledgers_have_topup(?NE_BINARY=AccountId) ->
    To = kz_time:now_s(),
    From = To - ?SECONDS_IN_DAY,
    {'ok', Ledgers} = kz_ledgers:list_source(AccountId, ?SOURCE_SERVICE, From, To),
    do_ledgers_have_topup(Ledgers);
do_ledgers_have_topup([]) -> 'false';
do_ledgers_have_topup([Ledger|Ledgers]) ->
    case kz_ledger:source_service(Ledger) =:= ?SOURCE_SERVICE
        andalso kz_ledger:executor_module(Ledger) =:= ?EXECUTOR_MODULE
        andalso kz_ledger:executor_trigger(Ledger) =:= ?DEFAULT_EXECUTOR_TRIGGER
    of
        'false' -> do_ledgers_have_topup(Ledgers);
        'true' -> 'true'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_topup(kz_term:api_ne_binary() | kzd_accounts:doc()) ->
                       {'ok', kz_currency:units(), kz_currency:units()} |
                       {'error', error()}.
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
-spec maybe_topup(kz_term:api_ne_binary()) -> topup_return().
maybe_topup(Account) ->
    {'ok', AvailableUnits} = kz_currency:available_units(Account),
    maybe_topup(Account, AvailableUnits).

-spec maybe_topup(kz_term:api_ne_binary(), kz_currency:units()) -> topup_return().
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
                         topup_return().
maybe_topup(AccountId, AvailableUnits, ReplinishUnits, ThresholdUnits) ->
    case should_topup(AccountId, AvailableUnits, ThresholdUnits) of
        'true' -> topup(AccountId, ReplinishUnits);
        {'error', _} = Error -> Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec topup(kz_term:ne_binary(), kz_currency:units()) -> topup_return().
topup(AccountId, ReplinishUnits) ->
    topup(AccountId, ReplinishUnits, ?DEFAULT_EXECUTOR_TRIGGER).

-spec topup(kz_term:ne_binary(), kz_currency:units(), kz_term:ne_binary()) ->
                   topup_return().
topup(AccountId, ReplinishUnits, Trigger) ->
    topup(AccountId, ReplinishUnits, Trigger, 'undefined').

-spec topup(kz_term:ne_binary(), kz_currency:units(), kz_term:ne_binary(), kz_term:api_object()) ->
                   topup_return().
topup(AccountId, ReplinishUnits, Trigger, Audit) ->
    Setters =
        props:filter_empty(
          [{fun kz_transaction:set_account/2, AccountId}
          ,{fun kz_transaction:set_description/2, ?DEFAULT_DESCRIPTION}
          ,{fun kz_transaction:set_executor_trigger/2, Trigger}
          ,{fun kz_transaction:set_executor_module/2, ?EXECUTOR_MODULE}
          ,{fun kz_transaction:set_audit/2, Audit}
          ,{fun kz_transaction:set_unit_amount/2, ReplinishUnits}
          ]
         ),
    case kz_transaction:sale(kz_transaction:setters(Setters)) of
        {'error', 'invalid_bookkeeper'} ->
            create_ledger(AccountId, ReplinishUnits, Trigger, Audit);
        {'error', _Reason} = Error -> Error;
        {'ok', Transaction} ->
            maybe_create_ledger(Transaction)
    end.

-spec maybe_create_ledger(kz_transaction:transaction()) -> topup_return().
maybe_create_ledger(Transaction) ->
    case kz_transaction:status_completed(Transaction) of
        'false' -> {'error', {'transaction_incomplete', Transaction}};
        'true' -> create_ledger(Transaction)
    end.

-spec create_ledger(kz_transaction:transaction()) -> topup_return().
create_ledger(Transaction) ->
    AccountId = kz_transaction:account_id(Transaction),
    ReplinishUnits = kz_transaction:unit_amount(Transaction),
    Trigger = kz_transaction:executor_trigger(Transaction),
    Audit = kz_transaction:audit(Transaction),
    Props = [{[<<"transaction">>, <<"id">>]
             ,kz_transaction:id(Transaction)
             }
            ,{[<<"transaction">>, <<"created">>]
             ,'true'
             }
            ],
    Metadata = kz_json:set_values(Props, kz_json:new()),
    case create_ledger(AccountId, ReplinishUnits, Trigger, Audit, Metadata) of
        {'error', Reason} ->
            {'error', {'ledger_error', Transaction, Reason}};
        {'ok', Ledger} ->
            {'ok', Transaction, Ledger}
    end.

-spec create_ledger(kz_term:ne_binary(), kz_currency:units(), kz_term:ne_binary(), kz_term:api_object()) ->
                           topup_return().
create_ledger(AccountId, ReplinishUnits, Trigger, Audit) ->
    case create_ledger(AccountId, ReplinishUnits, Trigger, Audit, kz_json:new()) of
        {'error', Reason} ->
            {'error', {'ledger_error', 'undefined', Reason}};
        {'ok', Ledger} ->
            {'ok', 'undefined', Ledger}
    end.

-spec create_ledger(kz_term:ne_binary(), kz_currency:units(), kz_term:ne_binary(), kz_term:api_object(), kz_json:object()) ->
                           {'ok', kz_ledger:ledger()} | {'error', any()}.
create_ledger(AccountId, ReplinishUnits, Trigger, Audit, Metadata) ->
    SourceId = kz_json:get_value([<<"transaction">>, <<"id">>]
                                ,Metadata
                                ,kz_binary:rand_hex(5)
                                ),
    Setters =
        props:filter_empty(
          [{fun kz_ledger:set_account/2, AccountId}
          ,{fun kz_ledger:set_source_id/2, SourceId}
          ,{fun kz_ledger:set_description/2, ?DEFAULT_DESCRIPTION}
          ,{fun kz_ledger:set_metadata/2, Metadata}
          ,{fun kz_ledger:set_executor_trigger/2, Trigger}
          ,{fun kz_ledger:set_executor_module/2, ?EXECUTOR_MODULE}
          ,{fun kz_ledger:set_audit/2, Audit}
          ,{fun kz_ledger:set_unit_amount/2, ReplinishUnits}
          ,{fun kz_ledger:set_period_start/2, kz_time:now_s()}
          ,{fun kz_ledger:set_source_service/2, ?SOURCE_SERVICE}
          ]
         ),
    kz_ledger:credit(kz_ledger:setters(Setters)).
