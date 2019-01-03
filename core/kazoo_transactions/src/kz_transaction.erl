%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_transaction).

-export([id/1]).
-export([rev/1]).
-export([description/1, set_description/2]).
-export([call_id/1]).
-export([sub_account_id/1]).
-export([sub_account_name/1]).
-export([set_sub_account_info/2]).
-export([event/1, set_event/2]).
-export([number/1, set_number/2]).
-export([numbers/1, set_numbers/2]).
-export([feature/1, set_feature/2]).
-export([bookkeeper_info/1, set_bookkeeper_info/2]).
-export([metadata/1, set_metadata/2]).
-export([reason/1, set_reason/2]).
-export([code/1, set_code/2]).
-export([amount/1, set_amount_and_type/2]).
-export([type/1, set_type/2]).
-export([created/1]).
-export([modified/1]).
-export([account_id/1]).
-export([account_db/1]).
-export([version/1]).
-export([status/1]).
-export([order_id/1, set_order_id/2]).

-export([new/1]).
-export([debit/2]).
-export([credit/2]).

-export([is_reason/2]).
-export([to_json/1]).
-export([to_public_json/1]).
-export([from_json/1]).
-export([remove/1]).
-export([save/1]).
-export([service_save/1]).
-export([is_per_minute/1]).

-include("transactions.hrl").

-record(kz_transaction, {id :: kz_term:api_ne_binary()
                        ,rev :: kz_term:api_ne_binary()
                        ,description :: kz_term:api_binary()
                        ,call_id :: kz_term:api_ne_binary()
                        ,sub_account_id :: kz_term:api_ne_binary()
                        ,sub_account_name :: kz_term:api_binary()
                        ,event :: kz_term:api_ne_binary()
                        ,number :: kz_term:api_ne_binary()
                        ,numbers :: kz_term:api_ne_binaries()
                        ,feature :: kz_term:api_ne_binary()
                        ,bookkeeper_info :: kz_term:api_object()
                        ,metadata :: kz_term:api_object()
                        ,pvt_status :: kz_term:api_ne_binary()
                        ,pvt_reason :: kz_term:api_ne_binary()
                        ,pvt_code :: kz_term:api_integer()
                        ,pvt_amount = 0 :: units()
                        ,pvt_type :: kz_term:ne_binary()
                        ,pvt_created :: kz_time:gregorian_seconds()
                        ,pvt_modified :: kz_time:gregorian_seconds()
                        ,pvt_account_id :: kz_term:ne_binary()
                        ,pvt_account_db :: kz_term:ne_binary()
                        ,pvt_vsn = 2 :: pos_integer()
                        ,order_id :: kz_term:api_ne_binary()
                        }).

-type transaction() :: #kz_transaction{}.
-type transactions() :: [transaction()].

-export_type([transaction/0
             ,transactions/0
             ,units/0
             ,dollars/0
             ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec id(transaction()) -> kz_term:ne_binary().
id(#kz_transaction{id = Id}) -> Id.
-spec rev(transaction()) -> kz_term:ne_binary().
rev(#kz_transaction{rev = Rev}) -> Rev.
-spec description(transaction()) -> kz_term:api_ne_binary().
description(#kz_transaction{description = Description}) -> Description.
-spec call_id(transaction()) -> kz_term:ne_binary().
call_id(#kz_transaction{call_id = CallId}) -> CallId.
-spec sub_account_id(transaction()) -> kz_term:ne_binary().
sub_account_id(#kz_transaction{sub_account_id = SubAccountId}) -> SubAccountId.
-spec sub_account_name(transaction()) -> kz_term:api_ne_binary().
sub_account_name(#kz_transaction{sub_account_name = SubAccountName}) -> SubAccountName.
-spec event(transaction()) -> kz_term:ne_binary().
event(#kz_transaction{event = Event}) -> Event.
-spec number(transaction()) -> kz_term:ne_binary().
number(#kz_transaction{number = Number}) -> Number.
-spec numbers(transaction()) -> kz_term:ne_binaries().
numbers(#kz_transaction{numbers = Numbers}) -> Numbers.
-spec feature(transaction()) -> kz_term:ne_binary().
feature(#kz_transaction{feature = Feature}) -> Feature.
-spec bookkeeper_info(transaction()) -> kz_json:object().
bookkeeper_info(#kz_transaction{bookkeeper_info = BookkeeperInfo}) -> BookkeeperInfo.
-spec metadata(transaction()) -> kz_term:api_object().
metadata(#kz_transaction{metadata = MetaData}) -> MetaData.
-spec reason(transaction()) -> kz_term:ne_binary().
reason(#kz_transaction{pvt_reason = Reason}) -> Reason.
-spec status(transaction()) -> kz_term:ne_binary().
status(#kz_transaction{pvt_status = Status}) -> Status.
-spec code(transaction()) -> kz_term:api_integer().
code(#kz_transaction{pvt_code = Code}) -> Code.
-spec amount(transaction()) -> units().
amount(#kz_transaction{pvt_amount = Amount}) -> Amount.
-spec type(transaction()) -> kz_term:ne_binary().
type(#kz_transaction{pvt_type = Type}) -> Type.
-spec created(transaction()) -> kz_time:gregorian_seconds().
created(#kz_transaction{pvt_created = Created}) -> Created.
-spec modified(transaction()) -> kz_time:gregorian_seconds().
modified(#kz_transaction{pvt_modified = Modified}) -> Modified.
-spec account_id(transaction()) -> kz_term:ne_binary().
account_id(#kz_transaction{pvt_account_id = AccountId}) -> AccountId.
-spec account_db(transaction()) -> kz_term:ne_binary().
account_db(#kz_transaction{pvt_account_db = AccountDb}) -> AccountDb.
-spec version(transaction()) -> integer().
version(#kz_transaction{pvt_vsn = Version}) -> Version.
-spec order_id(transaction()) -> kz_term:api_ne_binary().
order_id(#kz_transaction{order_id = OrderId}) -> OrderId.

-type new() :: #{account_id => kz_term:ne_binary()
                ,account_db => kz_term:ne_binary()
                ,amount => units()
                ,type => kz_term:ne_binary()
                ,description => kz_term:ne_binary()
                ,bookkeeper_info => kz_json:object()
                ,metadata => kz_json:object()
                ,status => kz_term:ne_binary()
                ,created => kz_time:gregorian_seconds()
                ,modified => kz_time:gregorian_seconds()
                }.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec new(new()) -> transaction().
new(#{account_id := AccountId
     ,account_db := AccountDb
     ,amount := Amount
     ,type := Type
     ,description := Description
     ,bookkeeper_info := Info
     ,metadata := Metadata
     ,status := Status
     ,created := Created
     ,modified := Modfified
     }) ->
    #kz_transaction{pvt_account_id = AccountId
                   ,pvt_account_db = AccountDb
                   ,pvt_amount = Amount
                   ,pvt_type = Type
                   ,description = Description
                   ,bookkeeper_info = Info
                   ,metadata = Metadata
                   ,pvt_status = Status
                   ,pvt_created = Created
                   ,pvt_modified = Modfified
                   }.

-spec new(kz_term:ne_binary(), units(), kz_term:ne_binary()) -> transaction().
new(Ledger, Amount, Type) ->
    #kz_transaction{pvt_type = Type
                   ,pvt_amount = abs(Amount)
                   ,pvt_account_id = kz_util:format_account_id(Ledger)
                   ,pvt_account_db = kz_util:format_account_mod_id(Ledger)
                   ,pvt_created = kz_time:now_s()
                   ,pvt_modified = kz_time:now_s()
                   }.

%%------------------------------------------------------------------------------
%% @doc Create transaction record of type credit (with Amount and Reason).
%% @end
%%------------------------------------------------------------------------------
-spec credit(kz_term:ne_binary(), units()) -> transaction().
credit(Ledger, Amount) ->
    new(Ledger, Amount, <<"credit">>).

%%------------------------------------------------------------------------------
%% @doc Create transaction record of type debit (with Amount and Reason).
%% @end
%%------------------------------------------------------------------------------
-spec debit(kz_term:ne_binary(), units()) -> transaction().
debit(Ledger, Amount) ->
    new(Ledger, Amount, <<"debit">>).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_description(kz_term:ne_binary(), transaction()) -> transaction().
set_description(Desc=?NE_BINARY, T) ->
    T#kz_transaction{description = Desc}.

-spec set_sub_account_info(kz_term:ne_binary(), transaction()) -> transaction().
set_sub_account_info(?MATCH_ACCOUNT_RAW(AccountId), T) ->
    case kzd_accounts:fetch(AccountId) of
        {'error', _R} ->
            lager:error("failed to open account ~s : ~p", [AccountId, _R]),
            T#kz_transaction{sub_account_id = AccountId
                            };
        {'ok', JObj} ->
            T#kz_transaction{sub_account_id = AccountId
                            ,sub_account_name = kz_json:get_value(<<"name">>, JObj)
                            }
    end.

-spec set_event(kz_term:ne_binary(), transaction()) -> transaction().
set_event(Event, T) ->
    T#kz_transaction{event = Event}.
-spec set_number(kz_term:ne_binary(), transaction()) -> transaction().
set_number(Number, T) ->
    T#kz_transaction{number = Number}.
-spec set_numbers(kz_term:ne_binaries(), transaction()) -> transaction().
set_numbers(Numbers, T) ->
    T#kz_transaction{numbers = Numbers}.
-spec set_feature(kz_term:ne_binary(), transaction()) -> transaction().
set_feature(Feature, T) ->
    T#kz_transaction{feature = Feature}.
-spec set_bookkeeper_info(kz_json:object(), transaction()) -> transaction().
set_bookkeeper_info(BookkeeperInfo, T) ->
    T#kz_transaction{bookkeeper_info = BookkeeperInfo}.
-spec set_metadata(kz_json:object(), transaction()) -> transaction().
set_metadata(MetaData, T) ->
    T#kz_transaction{metadata = MetaData}.

-spec set_reason(kz_term:ne_binary(), transaction()) -> transaction().
set_reason(Reason, T) ->
    %%FIXME: check wht_util:is_valid_reason/1
    T#kz_transaction{pvt_reason = Reason
                    ,pvt_code = wht_util:reason_code(Reason)
                    }.

-spec set_code(pos_integer(), transaction()) -> transaction().
set_code(Code, T)
  when is_integer(Code), Code > 0 ->
    T#kz_transaction{pvt_reason = wht_util:code_reason(Code)
                    ,pvt_code = Code
                    }.

-spec set_amount_and_type(integer(), transaction()) -> transaction().
set_amount_and_type(Amount, T)
  when is_integer(Amount), Amount > 0 ->
    T#kz_transaction{pvt_amount = Amount
                    ,pvt_type = <<"credit">>
                    };
set_amount_and_type(Amount, T)
  when is_integer(Amount), Amount < 0 ->
    T#kz_transaction{pvt_amount = abs(Amount)
                    ,pvt_type = <<"debit">>
                    }.

-spec set_type(kz_term:ne_binary(), transaction()) -> transaction().
set_type(Type, T) ->
    T#kz_transaction{pvt_type = Type}.
-spec set_order_id(kz_term:api_binary(), transaction()) -> transaction().
set_order_id(OrderId, T) ->
    T#kz_transaction{order_id = OrderId}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_per_minute(transaction()) -> boolean().
is_per_minute(Transaction) ->
    case code(Transaction) of
        ?CODE_PER_MINUTE_CALL -> 'true';
        ?CODE_PER_MINUTE_CALL_SUB_ACCOUNT -> 'true';
        _Code -> 'false'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_reason(kz_term:ne_binary() | kz_term:ne_binaries(), transaction()) -> boolean().
is_reason(Reason, #kz_transaction{pvt_reason = Reason}) -> 'true';
is_reason([Reason | _], #kz_transaction{pvt_reason = Reason}) -> 'true';
is_reason([_ | Reasons], Transaction) ->
    is_reason(Reasons, Transaction);
is_reason(_, _) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc Transform transaction record to Json object
%% @end
%%------------------------------------------------------------------------------
-spec to_json(transaction()) -> kz_json:object().
to_json(#kz_transaction{}=T) ->
    maybe_correct_transaction(
      kz_json:from_list(
        [{<<"_id">>, T#kz_transaction.id}
        ,{<<"_rev">>, T#kz_transaction.rev}
        ,{<<"description">>, T#kz_transaction.description}
        ,{<<"call_id">>, T#kz_transaction.call_id}
        ,{<<"sub_account_id">>, T#kz_transaction.sub_account_id}
        ,{<<"sub_account_name">>, T#kz_transaction.sub_account_name}
        ,{<<"event">>, T#kz_transaction.event}
        ,{<<"number">>, T#kz_transaction.number}
        ,{<<"numbers">>, T#kz_transaction.numbers}
        ,{<<"feature">>, T#kz_transaction.feature}
        ,{<<"bookkeeper_info">>, T#kz_transaction.bookkeeper_info}
        ,{<<"metadata">>, T#kz_transaction.metadata}
        ,{<<"pvt_status">>, T#kz_transaction.pvt_status}
        ,{<<"pvt_reason">>, T#kz_transaction.pvt_reason}
        ,{<<"pvt_code">>, T#kz_transaction.pvt_code}
        ,{<<"pvt_amount">>, T#kz_transaction.pvt_amount}
        ,{<<"pvt_type">>, T#kz_transaction.pvt_type}
        ,{<<"pvt_created">>, T#kz_transaction.pvt_created}
        ,{<<"pvt_modified">>, T#kz_transaction.pvt_modified}
        ,{<<"pvt_account_id">>, T#kz_transaction.pvt_account_id}
        ,{<<"pvt_account_db">>, T#kz_transaction.pvt_account_db}
        ,{<<"pvt_vsn">>, T#kz_transaction.pvt_vsn}
        ,{<<"order_id">>, T#kz_transaction.order_id}
        ])).

%%------------------------------------------------------------------------------
%% @doc Transform Json Object to transaction record
%% @end
%%------------------------------------------------------------------------------
-spec to_public_json(transaction()) -> kz_json:object().
to_public_json(Transaction) ->
    maybe_correct_transaction(
      clean_jobj(
        to_json(Transaction))).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_correct_transaction(kz_json:object()) -> kz_json:object().
maybe_correct_transaction(JObj) ->
    Routines = [fun maybe_add_sub_account_name/1
               ],
    lists:foldl(fun(F, J) -> F(J) end, JObj, Routines).

-spec maybe_add_sub_account_name(kz_json:object()) -> kz_json:object().
maybe_add_sub_account_name(JObj) ->
    case {kz_json:get_value(<<"sub_account_name">>, JObj)
         ,kz_json:get_value(<<"sub_account_id">>, JObj)
         }
    of
        {'undefined', 'undefined'} -> JObj;
        {'undefined', AccountId} -> add_sub_account_name(AccountId, JObj);
        _ -> JObj
    end.

-spec add_sub_account_name(kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
add_sub_account_name(AccountId, JObj) ->
    case kzd_accounts:fetch_name(AccountId) of
        undefined -> JObj;
        AccountName ->
            kz_json:set_value(<<"sub_account_name">>, AccountName, JObj)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec clean_jobj(kz_json:object()) -> kz_json:object().
clean_jobj(JObj) ->
    CleanKeys = [{<<"_id">>, <<"id">>}
                ,{<<"pvt_amount">>, <<"amount">>, fun wht_util:units_to_dollars/1}
                ,{<<"pvt_reason">>, <<"reason">>}
                ,{<<"pvt_status">>, <<"status">>}
                ,{<<"pvt_type">>, <<"type">>}
                ,{<<"pvt_created">>, <<"created">>}
                ,{<<"pvt_vsn">>, <<"version">>}
                ,{<<"pvt_code">>, <<"code">>}
                ],
    RemoveKeys = [<<"pvt_account_db">>
                 ,<<"pvt_account_id">>
                 ,<<"pvt_modified">>
                 ,<<"_rev">>
                 ],
    kz_json:normalize_jobj(JObj, RemoveKeys, CleanKeys).

%%------------------------------------------------------------------------------
%% @doc Transform Json Object to transaction record
%% @end
%%------------------------------------------------------------------------------
-spec from_json(kz_json:object()) -> transaction().
from_json(JObj) ->
    #kz_transaction{id = kz_doc:id(JObj)
                   ,rev = kz_doc:revision(JObj)
                   ,description = kz_json:get_ne_value(<<"description">>, JObj)
                   ,call_id = kz_json:get_ne_value(<<"call_id">>, JObj)
                   ,sub_account_id = kz_json:get_ne_value(<<"sub_account_id">>, JObj)
                   ,sub_account_name = kz_json:get_ne_value(<<"sub_account_name">>, JObj)
                   ,event = kz_json:get_ne_value(<<"event">>, JObj)
                   ,number = kz_json:get_ne_value(<<"number">>, JObj)
                   ,numbers = kz_json:get_ne_value(<<"numbers">>, JObj)
                   ,feature = kz_json:get_ne_value(<<"feature">>, JObj)
                   ,bookkeeper_info = kz_json:get_ne_value(<<"bookkeeper_info">>, JObj)
                   ,metadata = kz_json:get_ne_value(<<"metadata">>, JObj)
                   ,pvt_reason = kz_json:get_ne_value(<<"pvt_reason">>, JObj)
                   ,pvt_status = kz_json:get_ne_value(<<"pvt_status">>, JObj)
                   ,pvt_code = kz_json:get_integer_value(<<"pvt_code">>, JObj, 0)
                   ,pvt_amount = kz_json:get_integer_value(<<"pvt_amount">>, JObj, 0)
                   ,pvt_type = kz_doc:type(JObj)
                   ,pvt_created = kz_doc:created(JObj)
                   ,pvt_modified = kz_doc:modified(JObj)
                   ,pvt_account_id = kz_doc:account_id(JObj)
                   ,pvt_account_db = kz_doc:account_db(JObj)
                   ,pvt_vsn = kz_doc:vsn(JObj, 1)
                   ,order_id = kz_json:get_ne_value(<<"order_id">>, JObj)
                   }.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec remove(transaction()) -> 'ok' | {'error', any()}.
remove(#kz_transaction{}=Transaction) ->
    case prepare_transaction(Transaction) of
        {'error', _}=E -> E;
        T=#kz_transaction{pvt_account_db = AccountDb} ->
            case kz_datamgr:del_doc(AccountDb, to_json(T)) of
                {'error', _}=E -> E;
                {'ok', _} -> 'ok'
            end
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec save(transaction()) -> {'ok', transaction()} |
                             {'error', any()}.
save(#kz_transaction{}=Transaction) ->
    case prepare_transaction(Transaction) of
        {'error', _}=E -> E;
        T=#kz_transaction{pvt_account_id = AccountId
                         ,pvt_created = Created
                         } ->
            JObj = to_json(T#kz_transaction{pvt_modified = kz_time:now_s()
                                           }),
            case kazoo_modb:save_doc(AccountId, JObj, Created) of
                {'error', _}=E -> E;
                {'ok', J} ->
                    {'ok', from_json(J)}
            end
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec service_save(transaction()) -> {'ok', transaction()} |
                                     {'error', any()}.
service_save(#kz_transaction{}=Transaction) ->
    case prepare_transaction(Transaction) of
        {'error', _}=E -> E;
        #kz_transaction{}=T ->
            service_save_transaction(T)
    end.

-spec service_save_transaction(transaction()) -> {'ok', transaction()} |
                                                 {'error', any()}.
service_save_transaction(#kz_transaction{pvt_account_id = AccountId}=Transaction) ->
    TransactionJObj = to_json(Transaction#kz_transaction{pvt_modified = kz_time:now_s()
                                                        }),
    case kz_services:fetch_services_doc(AccountId, true) of
        {'error', _R}=Error ->
            lager:debug("unable to open account ~s services doc: ~p", [AccountId, _R]),
            Error;
        {'ok', ServicesJObj} ->
            Transactions = kz_json:get_list_value(<<"transactions">>, ServicesJObj, []),
            Props = [{<<"transactions">>, [TransactionJObj|Transactions]}
                    ,{?SERVICES_PVT_IS_DIRTY, 'true'}
                    ],
            case kz_datamgr:save_doc(?KZ_SERVICES_DB, kz_json:set_values(Props, ServicesJObj)) of
                {'error', _R}=Error ->
                    lager:debug("failed to save account ~s services doc for transaction ~s: ~p"
                               ,[AccountId, kz_doc:id(TransactionJObj), _R]),
                    Error;
                {'ok', _} ->
                    {'ok', from_json(TransactionJObj)}
            end
    end.

-spec prepare_transaction(transaction()) -> transaction() | {'error', any()}.
prepare_transaction(#kz_transaction{id = 'undefined'}=Transaction) ->
    prepare_transaction(Transaction#kz_transaction{id = modb_doc_id()});
prepare_transaction(#kz_transaction{pvt_code = Code}=Transaction)
  when ?CODE_PER_MINUTE_CALL =:= Code;
       ?CODE_PER_MINUTE_CALL_SUB_ACCOUNT =:= Code ->
    prepare_call_transaction(Transaction);
prepare_transaction(#kz_transaction{pvt_code = Code}=Transaction)
  when ?CODE_FEATURE_ACTIVATION =:= Code;
       ?CODE_FEATURE_ACTIVATION_SUB_ACCOUNT =:= Code ->
    prepare_feature_activation_transaction(Transaction);
prepare_transaction(#kz_transaction{pvt_code = Code}=Transaction)
  when ?CODE_NUMBER_ACTIVATION =:= Code;
       ?CODE_NUMBER_ACTIVATION_SUB_ACCOUNT =:= Code ->
    prepare_number_activation_transaction(Transaction);
prepare_transaction(#kz_transaction{pvt_code = Code}=Transaction)
  when ?CODE_NUMBERS_ACTIVATION =:= Code;
       ?CODE_NUMBERS_ACTIVATION_SUB_ACCOUNT =:= Code ->
    prepare_numbers_activation_transaction(Transaction);
prepare_transaction(#kz_transaction{pvt_code = Code}=Transaction)
  when ?CODE_MANUAL_ADDITION =:= Code;
       ?CODE_MANUAL_ADDITION_SUB_ACCOUNT =:= Code ->
    prepare_manual_addition_transaction(Transaction);
prepare_transaction(#kz_transaction{pvt_code = Code}=Transaction)
  when ?CODE_DATABASE_ROLLUP =:= Code ->
    prepare_rollup_transaction(Transaction);
prepare_transaction(#kz_transaction{pvt_code = Code}=Transaction)
  when ?CODE_TOPUP =:= Code ->
    prepare_topup_transaction(Transaction);
prepare_transaction(Transaction=#kz_transaction{pvt_account_id = AccountId
                                               ,pvt_account_db = AccountDb
                                               }) ->
    case {AccountId, AccountDb} of
        {?MATCH_ACCOUNT_RAW(_), ?MATCH_MODB_SUFFIX_ENCODED(_,_,_,_,_)} -> Transaction;
        {_, ?MATCH_MODB_SUFFIX_ENCODED(_,_,_,_,_)} -> {error, account_id_missing};
        {?MATCH_ACCOUNT_RAW(_), _} -> {error, account_db_missing}
    end.

-spec prepare_call_transaction(transaction()) -> transaction() |
                                                 {'error', any()}.
prepare_call_transaction(#kz_transaction{call_id = 'undefined'}) ->
    {'error', 'call_id_missing'};
prepare_call_transaction(#kz_transaction{sub_account_id = 'undefined'
                                        ,pvt_code = ?CODE_PER_MINUTE_CALL_SUB_ACCOUNT
                                        }) ->
    {'error', 'sub_account_id_missing'};
prepare_call_transaction(#kz_transaction{event = 'undefined'}) ->
    {'error', 'event_missing'};
prepare_call_transaction(#kz_transaction{call_id = CallId
                                        ,event = Event
                                        }=Transaction) ->
    Id = <<CallId/binary, "-", (kz_term:to_upper_binary(Event))/binary>>,
    Transaction#kz_transaction{id = Id
                              ,event = kz_term:to_lower_binary(Event)
                              }.

-spec prepare_feature_activation_transaction(transaction()) -> transaction() |
                                                               {'error', any()}.
prepare_feature_activation_transaction(#kz_transaction{feature = 'undefined'}) ->
    {'error', 'feature_name_missing'};
prepare_feature_activation_transaction(#kz_transaction{number = 'undefined'}) ->
    {'error', 'number_missing'};
prepare_feature_activation_transaction(#kz_transaction{sub_account_id = 'undefined'
                                                      ,pvt_code = ?CODE_FEATURE_ACTIVATION_SUB_ACCOUNT
                                                      }) ->
    {'error', 'sub_account_id_missing'};
prepare_feature_activation_transaction(Transaction) ->
    Transaction.

-spec prepare_number_activation_transaction(transaction()) -> transaction() |
                                                              {'error', any()}.
prepare_number_activation_transaction(#kz_transaction{number = 'undefined'}) ->
    {'error', 'number_missing'};
prepare_number_activation_transaction(#kz_transaction{sub_account_id = 'undefined'
                                                     ,pvt_code = ?CODE_NUMBER_ACTIVATION_SUB_ACCOUNT
                                                     }) ->
    {'error', 'sub_account_id_missing'};
prepare_number_activation_transaction(Transaction) ->
    Transaction.

-spec prepare_numbers_activation_transaction(transaction()) -> transaction() |
                                                               {'error', any()}.
prepare_numbers_activation_transaction(#kz_transaction{numbers = 'undefined'}) ->
    {'error', 'numbers_missing'};
prepare_numbers_activation_transaction(#kz_transaction{sub_account_id = 'undefined'
                                                      ,pvt_code = ?CODE_NUMBERS_ACTIVATION_SUB_ACCOUNT
                                                      }) ->
    {'error', 'sub_account_id_missing'};
prepare_numbers_activation_transaction(Transaction) ->
    Transaction.

-spec prepare_manual_addition_transaction(transaction()) -> transaction() |
                                                            {'error', any()}.
prepare_manual_addition_transaction(#kz_transaction{bookkeeper_info = 'undefined'}) ->
    {'error', 'bookkeeper_info_missing'};
prepare_manual_addition_transaction(#kz_transaction{sub_account_id = 'undefined'
                                                   ,pvt_code = ?CODE_MANUAL_ADDITION_SUB_ACCOUNT
                                                   }) ->
    {'error', 'sub_account_id_missing'};
prepare_manual_addition_transaction(Transaction) ->
    Transaction.

-spec prepare_rollup_transaction(transaction()) -> transaction().
prepare_rollup_transaction(Transaction) ->
    Transaction#kz_transaction{id = <<"monthly_rollup">>}.

-spec prepare_topup_transaction(transaction()) -> transaction().
prepare_topup_transaction(Transaction) ->
    {_, M, D} = erlang:date(),
    Month = kz_date:pad_month(M),
    Day = kz_date:pad_month(D),
    Id = <<"topup-", Month/binary, Day/binary>>,
    Transaction#kz_transaction{id = Id}.

-spec modb_doc_id() -> kz_term:ne_binary().
modb_doc_id() ->
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(kz_time:now_s()),
    list_to_binary([kz_term:to_binary(Year)
                   ,kz_date:pad_month(Month)
                   ,"-"
                   ,kz_binary:rand_hex(16)
                   ]).
