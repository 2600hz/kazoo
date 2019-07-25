%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_ledger).

-export([set_account/2]).
-export([account_id/1
        ,set_account_id/2
        ]).
-export([account_name/1
        ,set_account_name/2
        ]).
-export([amount/1
        ,unit_amount/1
        ,dollar_amount/1
        ,set_unit_amount/2
        ,set_dollar_amount/2
        ]).
-export([description/1
        ,set_description/2
        ]).
-export([executor_trigger/1
        ,set_executor_trigger/2
        ]).
-export([executor_module/1
        ,set_executor_module/2
        ]).
-export([source_id/1
        ,set_source_id/2
        ]).
-export([source_service/1
        ,set_source_service/2
        ]).
-export([usage_quantity/1
        ,set_usage_quantity/2
        ]).
-export([usage_type/1
        ,set_usage_type/2
        ]).
-export([usage_unit/1
        ,set_usage_unit/2
        ]).
-export([period_end/1
        ,set_period_end/2
        ]).
-export([period_start/1
        ,set_period_start/2
        ]).
-export([metadata/1
        ,set_metadata/2
        ]).
-export([audit/1
        ,set_audit/2
        ]).
-export([ledger_type/1
        ,set_ledger_type/2
        ]).
-export([modb/1
        ,set_modb/2
        ,set_modb/4
        ]).
-export([id/1
        ,set_id/2
        ]).
-export([created/1]).
-export([reset/1]).

-export([empty/0]).
-export([setters/1
        ,setters/2
        ]).
-export([public_json/1]).
-export([to_json/1
        ,from_json/1
        ]).
-export([fetch/2
        ,fetch/4
        ]).
-export([credit/1
        ,credit/2
        ]).
-export([debit/1
        ,debit/2
        ]).
-export([save/1
        ,save/2
        ,save/4
        ]).

-include("kazoo_ledgers.hrl").

-record(ledger, {account_id :: kz_term:api_ne_binary()
                ,account_name :: kz_term:api_ne_binary()
                ,amount = 0 :: kz_currency:units()
                ,description :: kz_term:api_binary()
                ,executor_trigger :: kz_term:api_binary()
                ,executor_module :: kz_term:api_binary()
                ,source_id :: kz_term:api_ne_binary()
                ,source_service :: kz_term:api_ne_binary()
                ,usage_type :: kz_term:api_ne_binary()
                ,usage_quantity = 0 :: non_neg_integer()
                ,usage_unit :: kz_term:api_binary()
                ,period_start :: kz_term:api_integer()
                ,period_end :: kz_term:api_integer()
                ,metadata = kz_json:new() :: kz_json:object()
                ,audit = kz_json:new() :: kz_json:object()
                ,ledger_type = kzd_ledgers:type_debit() :: kz_term:ne_binary()
                ,private_fields = kz_json:new() :: kz_json:object()
                ,modb :: kz_term:api_binary()
                }
       ).

-opaque ledger() :: #ledger{}.
-type setter_fun() :: {fun((ledger(), Value) -> ledger()), Value}.
-type setter_funs() :: [setter_fun()].
-export_type([ledger/0
             ,setter_fun/0
             ,setter_funs/0
             ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_account(ledger(), kz_term:ne_binary()) -> ledger().
set_account(Ledger, Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    Setters = [{fun set_account_id/2, AccountId}
              ,{fun set_account_name/2, kzd_accounts:fetch_name(AccountId)}
              ],
    setters(Ledger, Setters).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec account_id(ledger()) -> kz_term:ne_binary().
account_id(#ledger{account_id=AccountId}) ->
    AccountId.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_account_id(ledger(), kz_term:ne_binary()) -> ledger().
set_account_id(Ledger, AccountId) ->
    Ledger#ledger{account_id=AccountId}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec account_name(ledger()) -> kz_term:ne_binary().
account_name(#ledger{account_name=AccountName}) ->
    AccountName.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_account_name(ledger(), kz_term:ne_binary()) -> ledger().
set_account_name(Ledger, AccountName) ->
    Ledger#ledger{account_name=AccountName}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec amount(ledger()) -> integer().
amount(Ledger) ->
    UnitAmount = unit_amount(Ledger),
    case ledger_type(Ledger) =:= kzd_ledgers:type_debit() of
        'true' -> UnitAmount * -1;
        'false' -> UnitAmount
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec unit_amount(ledger()) -> kz_current:units().
unit_amount(#ledger{amount=Amount}) ->
    abs(Amount).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec dollar_amount(ledger()) -> kz_currency:dollars().
dollar_amount(#ledger{amount=Amount}) ->
    kz_currency:units_to_dollars(Amount).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_unit_amount(ledger(), kz_currency:units()) -> ledger().
set_unit_amount(Ledger, Amount) ->
    Ledger#ledger{amount=abs(Amount)}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_dollar_amount(ledger(), kz_currency:dollars()) -> ledger().
set_dollar_amount(Ledger, Amount) ->
    Ledger#ledger{amount=kz_currency:dollars_to_units(Amount)}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec description(ledger()) -> kz_term:api_binary().
description(#ledger{description=Description}) ->
    Description.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_description(ledger(), kz_term:ne_binary()) -> ledger().
set_description(Ledger, Description) ->
    Ledger#ledger{description=Description}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec executor_trigger(ledger()) -> kz_term:api_binary().
executor_trigger(#ledger{executor_trigger=Trigger}) ->
    Trigger.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_executor_trigger(ledger(), kz_term:ne_binary()) -> ledger().
set_executor_trigger(Ledger, Trigger) ->
    Ledger#ledger{executor_trigger=Trigger}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec executor_module(ledger()) -> kz_term:api_binary().
executor_module(#ledger{executor_module=Module}) ->
    Module.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_executor_module(ledger(), kz_term:ne_binary()) -> ledger().
set_executor_module(Ledger, Module) ->
    Ledger#ledger{executor_module=Module}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec source_id(ledger()) -> kz_term:ne_binary().
source_id(#ledger{source_id=SourceId}) ->
    SourceId.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_source_id(ledger(), kz_term:ne_binary()) -> ledger().
set_source_id(Ledger, SourceId) ->
    Ledger#ledger{source_id=SourceId}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec source_service(ledger()) -> kz_term:ne_binary().
source_service(#ledger{source_service=SourceService}) ->
    SourceService.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_source_service(ledger(), kz_term:ne_binary()) -> ledger().
set_source_service(Ledger, SourceService) ->
    Ledger#ledger{source_service=SourceService}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec usage_type(ledger()) -> kz_term:ne_binary().
usage_type(#ledger{usage_type=UsageType}) ->
    UsageType.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_usage_type(ledger(), kz_term:ne_binary()) -> ledger().
set_usage_type(Ledger, UsageType) ->
    Ledger#ledger{usage_type=UsageType}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec usage_quantity(ledger()) -> non_neg_integer().
usage_quantity(#ledger{usage_quantity=UsageQuantity}) ->
    UsageQuantity.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_usage_quantity(ledger(), non_neg_integer()) -> ledger().
set_usage_quantity(Ledger, UsageQuantity) ->
    Ledger#ledger{usage_quantity=UsageQuantity}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec usage_unit(ledger()) -> kz_term:api_binary().
usage_unit(#ledger{usage_unit=UsageUnit}) ->
    UsageUnit.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_usage_unit(ledger(), kz_term:ne_binary()) -> ledger().
set_usage_unit(Ledger, UsageUnit) ->
    Ledger#ledger{usage_unit=UsageUnit}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec period_start(ledger()) -> non_neg_integer().
period_start(#ledger{period_start=PeriodStart}) ->
    PeriodStart.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_period_start(ledger(), non_neg_integer()) -> ledger().
set_period_start(Ledger, PeriodStart) ->
    Ledger#ledger{period_start=PeriodStart}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec period_end(ledger()) -> non_neg_integer().
period_end(#ledger{period_end=PeriodEnd}) ->
    PeriodEnd.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_period_end(ledger(), non_neg_integer()) -> ledger().
set_period_end(Ledger, PeriodEnd) ->
    Ledger#ledger{period_end=PeriodEnd}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec metadata(ledger()) -> kz_json:object().
metadata(#ledger{metadata=Metadata}) ->
    Metadata.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_metadata(ledger(), kz_json:object()) -> ledger().
set_metadata(Ledger, Metadata) ->
    Ledger#ledger{metadata=Metadata}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec audit(ledger()) -> kz_json:object().
audit(#ledger{audit=Audit}) ->
    Audit.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_audit(ledger(), kz_json:object()) -> ledger().
set_audit(Ledger, Audit) ->
    Ledger#ledger{audit=Audit}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec ledger_type(ledger()) -> kz_term:ne_binary().
ledger_type(#ledger{ledger_type=LedgerType}) ->
    LedgerType.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_ledger_type(ledger(), kz_currency:units() | kz_currency:dollars() | kz_term:ne_binary()) ->
                             ledger().
set_ledger_type(Ledger, Amount)
  when is_float(Amount), Amount > 0 ->
    set_ledger_type(Ledger, kzd_ledgers:type_credit());
set_ledger_type(Ledger, Amount)
  when is_integer(Amount), Amount > 0 ->
    set_ledger_type(Ledger, kzd_ledgers:type_credit());
set_ledger_type(Ledger, Amount)
  when is_float(Amount) ->
    set_ledger_type(Ledger, kzd_ledgers:type_debit());
set_ledger_type(Ledger, Amount)
  when is_integer(Amount) ->
    set_ledger_type(Ledger, kzd_ledgers:type_debit());
set_ledger_type(Ledger, LedgerType) ->
    Ledger#ledger{ledger_type=LedgerType}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec modb(ledger()) -> kz_term:ne_binary().
modb(#ledger{modb='undefined'}=Ledger) ->
    kazoo_modb:get_modb(account_id(Ledger));
modb(#ledger{modb=MODb}) ->
    MODb.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_modb(ledger(), kz_term:ne_binary()) -> ledger().
set_modb(Ledger, MODb) ->
    Ledger#ledger{modb=MODb}.

-spec set_modb(ledger(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                      ledger().
set_modb(Ledger, Account, Year, Month) ->
    MODb = kazoo_modb:get_modb(Account, Year, Month),
    set_modb(Ledger, MODb).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec id(ledger()) -> kz_term:api_binary().
id(#ledger{private_fields=PrivateFields}) ->
    kz_doc:id(PrivateFields).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_id(ledger(), kz_term:ne_binary()) -> ledger().
set_id(#ledger{private_fields=PrivateFields}=Ledger, Id) ->
    Ledger#ledger{private_fields=maybe_prefix_id(kz_doc:set_id(PrivateFields, Id))}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec created(ledger()) -> kz_time:gregorian_seconds() | 'undefined'.
created(#ledger{private_fields=PrivateFields}) ->
    kz_doc:created(PrivateFields).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec empty() -> ledger().
empty() ->
    #ledger{}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reset(ledger()) -> ledger().
reset(#ledger{private_fields=PrivateFields}=Ledger) ->
    Origin = kz_json:map(fun remove_pvt/2, PrivateFields),
    Props = [{<<"origin_ledger">>, Origin}],
    Metadata = kz_json:set_values(Props, metadata(Ledger)),
    set_metadata(Ledger#ledger{private_fields=kz_json:new()
                              ,modb='undefined'
                              }
                ,Metadata
                ).

-spec remove_pvt(kz_term:ne_binary(), Value) -> {kz_term:ne_binary(), Value}.
remove_pvt(<<"pvt_", Key/binary>>, Value) -> {Key, Value};
remove_pvt(<<"_", Key/binary>>, Value) -> {Key, Value};
remove_pvt(Key, Value) -> {Key, Value}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec setters(setter_funs()) -> ledger().
setters(Routines) ->
    setters(empty(), Routines).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec setters(ledger(), setter_funs()) -> ledger().
setters(Ledger, Routines) ->
    lists:foldl(fun({Setter, Value}, L) ->
                        Setter(L, Value)
                end
               ,Ledger
               ,Routines
               ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec public_json(ledger()) -> kz_json:object().
public_json(Ledger) ->
    Routines = [fun maybe_prefix_id/1
               ,fun amount_to_dollars/1
               ,fun kz_doc:public_fields/1
               ],
    lists:foldl(fun(F, J) ->
                        F(J)
                end
               ,to_json(Ledger)
               ,Routines
               ).

-spec maybe_prefix_id(kzd_ledgers:doc()) -> kzd_ledgers:doc().
maybe_prefix_id(LedgerJObj) ->
    case kz_doc:id(LedgerJObj) of
        ?MATCH_MODB_PREFIX(_Year, _Month, _Id) -> LedgerJObj;
        Id ->
            Created = kz_doc:created(LedgerJObj),
            {Year, Month, _} = kz_term:to_date(Created),
            kz_doc:set_id(LedgerJObj, kazoo_modb_util:modb_id(Year, Month, Id))
    end.

-spec amount_to_dollars(kzd_ledgers:doc()) -> kzd_ledgers:doc().
amount_to_dollars(LedgerJObj) ->
    Amount = kzd_ledgers:dollar_amount(LedgerJObj, 0),
    case kzd_ledgers:ledger_type(LedgerJObj) =:= kzd_ledgers:type_debit() of
        'true' -> kz_json:set_value(<<"amount">>, Amount * -1, LedgerJObj);
        'false' -> kz_json:set_value(<<"amount">>, abs(Amount), LedgerJObj)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec to_json(ledger()) -> kzd_ledgers:doc().
to_json(#ledger{private_fields=PrivateFields}=Ledger) ->
    Setters = [{fun kzd_ledgers:set_account_id/2, account_id(Ledger)}
              ,{fun kzd_ledgers:set_account_name/2, account_name(Ledger)}
              ,{fun kzd_ledgers:set_unit_amount/2, unit_amount(Ledger)}
              ,{fun kzd_ledgers:set_description/2, description(Ledger)}
              ,{fun kzd_ledgers:set_executor_trigger/2, executor_trigger(Ledger)}
              ,{fun kzd_ledgers:set_executor_module/2, executor_module(Ledger)}
              ,{fun kzd_ledgers:set_source_id/2, source_id(Ledger)}
              ,{fun kzd_ledgers:set_source_service/2, source_service(Ledger)}
              ,{fun kzd_ledgers:set_usage_quantity/2, usage_quantity(Ledger)}
              ,{fun kzd_ledgers:set_usage_type/2, usage_type(Ledger)}
              ,{fun kzd_ledgers:set_usage_unit/2, usage_unit(Ledger)}
              ,{fun kzd_ledgers:set_period_end/2, period_end(Ledger)}
              ,{fun kzd_ledgers:set_period_start/2, period_start(Ledger)}
              ,{fun kzd_ledgers:set_metadata/2, metadata(Ledger)}
              ,{fun kzd_ledgers:set_audit/2, audit(Ledger)}
              ,{fun kzd_ledgers:set_ledger_type/2, ledger_type(Ledger)}
              ],
    LedgerJObj = kz_json:merge(PrivateFields, kz_doc:setters(Setters)),
    Props = [{kz_doc:path_type(), kzd_ledgers:type()}
            ,{kz_doc:path_created(), get_created_timestamp(LedgerJObj)}
            ,{kz_doc:path_modified(), kz_time:now_s()}
            ,{kz_doc:path_account_id(), account_id(Ledger)}
             | maybe_add_id(LedgerJObj)
            ],
    kz_json:set_values(Props, LedgerJObj).

-spec get_created_timestamp(kzd_ledgers:doc()) -> integer().
get_created_timestamp(LedgerJObj) ->
    kz_doc:created(LedgerJObj, kz_time:now_s()).

-spec maybe_add_id(kzd_ledgers:doc()) -> kz_term:proplist().
maybe_add_id(LedgerJObj) ->
    case kz_doc:id(LedgerJObj) of
        'undefined' ->
            {Year, Month, _} = erlang:date(),
            [{kz_doc:path_id(), list_to_binary([kz_term:to_binary(Year)
                                               ,kz_date:pad_month(Month)
                                               ,"-"
                                               ,create_hash(LedgerJObj)
                                               ])
             }
            ];
        _ -> []
    end.

-spec create_hash(kzd_ledgers:doc()) -> kz_term:ne_binary().
create_hash(LedgerJObj) ->
    Props = [{<<"source">>, kzd_ledgers:source(LedgerJObj)}
            ,{<<"account">>, kzd_ledgers:account(LedgerJObj)}
            ,{<<"usage">>, kzd_ledgers:usage(LedgerJObj)}
            ,{<<"period">>, kzd_ledgers:period(LedgerJObj)}
            ,{<<"type">>, kz_doc:type(LedgerJObj)}
            ],
    kz_binary:md5(kz_json:encode(kz_json:from_list(Props))).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec from_json(kzd_ledgers:doc()) -> ledger().
from_json(JObj) ->
    Setters = [{fun set_account_id/2, kzd_ledgers:account_id(JObj)}
              ,{fun set_account_name/2, kzd_ledgers:account_name(JObj)}
              ,{fun set_unit_amount/2, kzd_ledgers:unit_amount(JObj)}
              ,{fun set_description/2, kzd_ledgers:description(JObj)}
              ,{fun set_executor_trigger/2, kzd_ledgers:executor_trigger(JObj)}
              ,{fun set_executor_module/2, kzd_ledgers:executor_module(JObj)}
              ,{fun set_source_id/2, kzd_ledgers:source_id(JObj)}
              ,{fun set_source_service/2, kzd_ledgers:source_service(JObj)}
              ,{fun set_usage_quantity/2, kzd_ledgers:usage_quantity(JObj)}
              ,{fun set_usage_type/2, kzd_ledgers:usage_type(JObj)}
              ,{fun set_usage_unit/2, kzd_ledgers:usage_unit(JObj)}
              ,{fun set_period_end/2, kzd_ledgers:period_end(JObj)}
              ,{fun set_period_start/2, kzd_ledgers:period_start(JObj)}
              ,{fun set_metadata/2, kzd_ledgers:metadata(JObj)}
              ,{fun set_audit/2, kzd_ledgers:audit(JObj)}
              ,{fun set_ledger_type/2, kzd_ledgers:ledger_type(JObj)}
              ,{fun(Ledger, PrivateFields) ->
                        Ledger#ledger{private_fields=PrivateFields}
                end
               ,kz_doc:private_fields(JObj)
               }
              ],
    setters(Setters).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch(kz_term:ne_binary(), kz_term:ne_binary()) ->
                   {'ok', ledger()} |
                   {'error', any()}.
fetch(Account, ?MATCH_MODB_PREFIX(Year, Month, Id)) ->
    fetch(Account, Id, Year, Month).

-spec fetch(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                   {'ok', ledger()} |
                   {'error', any()}.
fetch(Account, Id, Year, Month) ->
    case kazoo_modb:open_doc(Account, Id, Year, Month) of
        {'error', _Reason} = Error -> Error;
        {'ok', JObj} ->
            {'ok', set_modb(from_json(JObj), Account, Year, Month)}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec credit(ledger()) -> {'ok', ledger()} | {'error', any()}.
credit(Ledger) ->
    save(set_ledger_type(Ledger, kzd_ledgers:type_credit())).

-spec credit(ledger(), kz_term:ne_binary()) -> {'ok', ledger()} | {'error', any()}.
credit(Ledger, AccountId) ->
    save(set_ledger_type(Ledger, kzd_ledgers:type_credit()), AccountId).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec debit(ledger()) -> {'ok', ledger()} | {'error', any()}.
debit(Ledger) ->
    save(set_ledger_type(Ledger, kzd_ledgers:type_debit())).

-spec debit(ledger(), kz_term:ne_binary()) -> {'ok', ledger()} | {'error', any()}.
debit(Ledger, AccountId) ->
    save(set_ledger_type(Ledger, kzd_ledgers:type_debit()), AccountId).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec save(ledger(), kz_term:ne_binary()) -> {'ok', ledger()} | {'error', any()}.
save(Ledger, Account) ->
    MODb = kazoo_modb:get_modb(Account),
    save(set_modb(Ledger, MODb)).

-spec save(ledger(), kz_term:ne_binary(), kz_time:year(), kz_time:month()) ->
                  {'ok', ledger()} | {'error', any()}.
save(Ledger, Account, Year, Month) ->
    MODb = kazoo_modb:get_modb(Account, Year, Month),
    save(set_modb(Ledger, MODb)).

-spec save(ledger()) -> {'ok', ledger()} | {'error', any()}.
save(Ledger) ->
    MODb = modb(Ledger),
    {AccountId, _Year, _Month} = kazoo_modb_util:split_account_mod(MODb),

    LedgerJObj = add_account_info(AccountId, to_json(Ledger)),
    case kazoo_modb:save_doc(MODb, LedgerJObj) of
        {'ok', SavedJObj} ->
            lager:debug("created ~s ~s in ~s ~p-~p (~s) for $~w"
                       ,[source_service(Ledger)
                        ,ledger_type(Ledger)
                        ,AccountId
                        ,_Year
                        ,_Month
                        ,kz_doc:id(SavedJObj)
                        ,dollar_amount(Ledger)
                        ]
                       ),
            {'ok', from_json(SavedJObj)};
        {'error', _Reason} = Error ->
            lager:info("unable to create ~s ~s in ~s ~p-~p: ~p"
                      ,[source_service(Ledger)
                       ,ledger_type(Ledger)
                       ,AccountId
                       ,_Year
                       ,_Month
                       ,_Reason
                       ]),
            Error
    end.

-spec add_account_info(kz_term:ne_binary(), kzd_ledgers:doc()) -> kzd_ledgers:doc().
add_account_info(AccountId, LedgerJObj) ->
    Audit = kzd_ledgers:audit(LedgerJObj),
    UpdatedAudit = kz_json:set_value(<<"available_units">>, available_units(AccountId), Audit),

    UpdatedLedger = kzd_ledgers:set_audit(LedgerJObj, UpdatedAudit),
    kz_doc:set_account_id(UpdatedLedger, AccountId).

-spec available_units(kz_term:ne_binary()) -> kz_currency:units() | 'undefined'.
available_units(AccountId) ->
    case kz_currency:available_units(AccountId) of
        {'error', _} -> 'undefined';
        {'ok', Units} -> Units
    end.
