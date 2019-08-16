%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_transaction).

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
-export([bookkeeper/1]).
-export([bookkeeper_type/1
        ,set_bookkeeper_type/2
        ]).
-export([bookkeeper_vendor_id/1
        ,set_bookkeeper_vendor_id/2
        ]).
-export([bookkeeper_results/1
        ,set_bookkeeper_results/2
        ]).
-export([metadata/1
        ,set_metadata/2
        ]).
-export([audit/1
        ,set_audit/2
        ]).
-export([order_id/1
        ,set_order_id/2
        ]).
-export([status/1
        ,status_pending/0
        ,status_pending/1
        ,status_failed/0
        ,status_failed/1
        ,status_completed/0
        ,status_completed/1
        ,set_status/2
        ]).
-export([transaction_type/1
        ,set_transaction_type/2
        ]).
-export([modb/1
        ,set_modb/2
        ,set_modb/4
        ]).
-export([id/1
        ,set_id/2
        ]).
-export([created/1]).

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
-export([refund/1
        ,refund/2
        ]).
-export([sale/1
        ,sale/2
        ]).
-export([save/1
        ,save/2
        ,save/4
        ]).

-include("kazoo_transactions.hrl").

-define(STATUS_PENDING, <<"pending">>).
-define(STATUS_COMPLETED, <<"completed">>).
-define(STATUS_FAILED, <<"failed">>).

-record(transaction, {account_id :: kz_term:api_ne_binary()
                     ,account_name :: kz_term:api_ne_binary()
                     ,amount = 0 :: kz_currency:units()
                     ,description :: kz_term:api_binary()
                     ,executor_trigger :: kz_term:api_binary()
                     ,executor_module :: kz_term:api_binary()
                     ,bookkeeper_type :: kz_term:api_binary()
                     ,bookkeeper_vendor_id :: kz_term:api_binary()
                     ,bookkeeper_results :: kz_term:api_object()
                     ,metadata = kz_json:new() :: kz_json:object()
                     ,audit = kz_json:new() :: kz_json:object()
                     ,order_id :: kz_term:api_ne_binary()
                     ,status = ?STATUS_PENDING :: kz_term:api_ne_binary()
                     ,transaction_type = kzd_transactions:type_sale() :: kz_term:ne_binary()
                     ,private_fields = kz_json:new() :: kz_json:object()
                     ,modb :: kz_term:api_ne_binary()
                     }).

-type transaction() :: #transaction{}.
-type setter_fun() :: {fun((transaction(), Value) -> transaction()), Value}.
-type setter_funs() :: [setter_fun()].
-export_type([transaction/0
             ,setter_fun/0
             ,setter_funs/0
             ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_account(transaction(), kz_term:ne_binary()) -> transaction().
set_account(Transaction, Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    Setters = [{fun set_account_id/2, AccountId}
              ,{fun set_account_name/2, kzd_accounts:fetch_name(AccountId)}
               | set_bookkeeper(Account)
              ],
    setters(Transaction, Setters).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec account_id(transaction()) -> kz_term:ne_binary().
account_id(#transaction{account_id=AccountId}) ->
    AccountId.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_account_id(transaction(), kz_term:ne_binary()) -> transaction().
set_account_id(Transaction, AccountId) ->
    Transaction#transaction{account_id=AccountId}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec account_name(transaction()) -> kz_term:ne_binary().
account_name(#transaction{account_name=AccountName}) ->
    AccountName.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_account_name(transaction(), kz_term:ne_binary()) -> transaction().
set_account_name(Transaction, AccountName) ->
    Transaction#transaction{account_name=AccountName}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec amount(transaction()) -> kz_currency:units().
amount(Transaction) ->
    UnitAmount = unit_amount(Transaction),
    case transaction_type(Transaction) =:= kzd_transactions:type_refund() of
        'true' -> UnitAmount * -1;
        'false' -> UnitAmount
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec unit_amount(transaction()) -> kz_currency:units().
unit_amount(#transaction{amount=Amount}) ->
    abs(Amount).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec dollar_amount(transaction()) -> kz_currency:dollars().
dollar_amount(#transaction{amount=Amount}) ->
    kz_currency:units_to_dollars(Amount).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_unit_amount(transaction(), kz_currency:units()) -> transaction().
set_unit_amount(Transaction, Amount) ->
    Transaction#transaction{amount=abs(Amount)}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_dollar_amount(transaction(), kz_currency:dollars()) -> transaction().
set_dollar_amount(Transaction, Amount) ->
    Transaction#transaction{amount=kz_currency:dollars_to_units(Amount)}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec description(transaction()) -> kz_term:api_binary().
description(#transaction{description=Description}) ->
    Description.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_description(transaction(), kz_term:ne_binary()) -> transaction().
set_description(Transaction, Description) ->
    Transaction#transaction{description=Description}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec executor_trigger(transaction()) -> kz_term:api_binary().
executor_trigger(#transaction{executor_trigger=Trigger}) ->
    Trigger.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_executor_trigger(transaction(), kz_term:ne_binary()) -> transaction().
set_executor_trigger(Transaction, Trigger) ->
    Transaction#transaction{executor_trigger=Trigger}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec executor_module(transaction()) -> kz_term:api_binary().
executor_module(#transaction{executor_module=Module}) ->
    Module.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_executor_module(transaction(), kz_term:ne_binary()) -> transaction().
set_executor_module(Transaction, Module) ->
    Transaction#transaction{executor_module=Module}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bookkeeper(transaction()) -> kz_json:object().
bookkeeper(Transaction) ->
    kz_json:from_list(
      props:filter_empty(
        [{<<"type">>, bookkeeper_type(Transaction)}
        ,{<<"vendor_id">>, bookkeeper_vendor_id(Transaction)}
        ,{<<"results">>, bookkeeper_results(Transaction)}
        ]
       )
     ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_bookkeeper(kz_term:ne_binary()) -> setter_funs().
set_bookkeeper(?NE_BINARY=Account) ->
    Services = kz_services:fetch(Account),
    [{fun set_bookkeeper_type/2
     ,kz_services:bookkeeper_type(Services)
     }
    ,{fun set_bookkeeper_vendor_id/2
     ,kz_services:bookkeeper_vendor_id(Services)
     }
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bookkeeper_type(transaction()) -> kz_term:ne_binary().
bookkeeper_type(#transaction{bookkeeper_type=Type}) ->
    case kz_term:is_empty(Type) of
        'true' -> kzd_services:default_bookkeeper_type();
        'false' -> Type
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_bookkeeper_type(transaction(), kz_term:ne_binary()) -> transaction().
set_bookkeeper_type(Transaction, Type) ->
    Transaction#transaction{bookkeeper_type=Type}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bookkeeper_vendor_id(transaction()) -> kz_term:api_ne_binary().
bookkeeper_vendor_id(#transaction{bookkeeper_vendor_id=VendorId}=Transaction) ->
    case kz_term:is_empty(VendorId) of
        'true' -> kz_services_reseller:get_id(account_id(Transaction));
        'false' -> VendorId
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_bookkeeper_vendor_id(transaction(), kz_term:ne_binary()) -> transaction().
set_bookkeeper_vendor_id(Transaction, VendorId) ->
    Transaction#transaction{bookkeeper_vendor_id=VendorId}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bookkeeper_results(transaction()) -> kz_term:api_object().
bookkeeper_results(#transaction{bookkeeper_results=Results}) ->
    Results.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_bookkeeper_results(transaction(), kz_json:object()) -> transaction().
set_bookkeeper_results(Transaction, Results) ->
    Transaction#transaction{bookkeeper_results=Results}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec metadata(transaction()) -> kz_json:object().
metadata(#transaction{metadata=Metadata}) ->
    Metadata.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_metadata(transaction(), kz_json:object()) -> transaction().
set_metadata(Transaction, Metadata) ->
    Transaction#transaction{metadata=Metadata}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec audit(transaction()) -> kz_json:object().
audit(#transaction{audit=Audit}) ->
    Audit.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_audit(transaction(), kz_json:object()) -> transaction().
set_audit(Transaction, Audit) ->
    Transaction#transaction{audit=Audit}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec order_id(transaction()) -> kz_term:api_ne_binary().
order_id(#transaction{order_id=OrderId}) ->
    OrderId.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_order_id(transaction(), kz_term:api_binary()) -> transaction().
set_order_id(Transaction, OrderId) ->
    Transaction#transaction{order_id=OrderId}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec status(transaction()) -> kz_term:ne_binary().
status(#transaction{status=Status}) ->
    Status.

-spec status_pending() -> kz_term:ne_binary().
status_pending() ->
    ?STATUS_PENDING.

-spec status_pending(transaction()) -> boolean().
status_pending(Transaction) ->
    status(Transaction) =:= status_pending().

-spec status_failed() -> kz_term:ne_binary().
status_failed() ->
    ?STATUS_FAILED.

-spec status_failed(transaction()) -> boolean().
status_failed(Transaction) ->
    status(Transaction) =:= status_failed().

-spec status_completed() -> kz_term:ne_binary().
status_completed() ->
    ?STATUS_COMPLETED.

-spec status_completed(transaction()) -> boolean().
status_completed(Transaction) ->
    status(Transaction) =:= status_completed().

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_status(transaction(), kz_term:ne_binary()) -> transaction().
set_status(Transaction, Status) ->
    Transaction#transaction{status=Status}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec transaction_type(transaction()) -> kz_term:ne_binary().
transaction_type(#transaction{transaction_type=TransactionType}) ->
    TransactionType.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_transaction_type(transaction(), kz_currency:units() | kz_currency:dollars() | kz_term:ne_binary()) ->
                                  transaction().
set_transaction_type(Transaction, Amount)
  when is_float(Amount), Amount > 0 ->
    set_transaction_type(Transaction, kzd_transactions:type_sale());
set_transaction_type(Transaction, Amount)
  when is_integer(Amount), Amount > 0 ->
    set_transaction_type(Transaction, kzd_transactions:type_sale());
set_transaction_type(Transaction, Amount)
  when is_float(Amount) ->
    set_transaction_type(Transaction, kzd_transactions:type_refund());
set_transaction_type(Transaction, Amount)
  when is_integer(Amount) ->
    set_transaction_type(Transaction, kzd_transactions:type_refund());
set_transaction_type(Transaction, TransactionType) ->
    Transaction#transaction{transaction_type=TransactionType}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec modb(transaction()) -> kz_term:ne_binary().
modb(#transaction{modb='undefined'}=Transaction) ->
    kazoo_modb:get_modb(account_id(Transaction));
modb(#transaction{modb=MODb}) ->
    MODb.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_modb(transaction(), kz_term:ne_binary()) -> transaction().
set_modb(Transaction, MODb) ->
    Transaction#transaction{modb=MODb}.

-spec set_modb(transaction(), kz_term:ne_binary(), kz_time:year(), kz_time:month()) -> transaction().
set_modb(Transaction, Account, Year, Month) ->
    MODb = kazoo_modb:get_modb(Account, Year, Month),
    set_modb(Transaction, MODb).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec id(transaction()) -> kz_term:api_binary().
id(#transaction{private_fields=PrivateFields}) ->
    kz_doc:id(PrivateFields).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_id(transaction(), kz_term:ne_binary()) -> transaction().
set_id(#transaction{private_fields=PrivateFields}=Transaction, Id) ->
    Transaction#transaction{private_fields=kz_doc:set_id(PrivateFields, Id)}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec created(transaction()) -> kz_term:api_binary().
created(#transaction{private_fields=PrivateFields}) ->
    kz_doc:created(PrivateFields).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec empty() -> transaction().
empty() ->
    #transaction{}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec setters(setter_funs()) -> transaction().
setters(Routines) ->
    setters(empty(), Routines).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec setters(transaction(), setter_funs()) -> transaction().
setters(Transaction, Routines) ->
    lists:foldl(fun({Setter, Value}, T) ->
                        Setter(T, Value)
                end
               ,Transaction
               ,Routines
               ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec public_json(transaction()) -> kz_json:object().
public_json(Transaction) ->
    Routines = [fun amount_to_dollars/1
               ,fun kz_doc:public_fields/1
               ],
    lists:foldl(fun(F, J) ->
                        F(J)
                end
               ,to_json(Transaction)
               ,Routines
               ).

-spec amount_to_dollars(kzd_transactions:doc()) -> kzd_transactions:doc().
amount_to_dollars(TransactionJObj) ->
    Amount = kzd_transactions:dollar_amount(TransactionJObj),
    kz_json:set_value(<<"amount">>, Amount, TransactionJObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec to_json(transaction()) -> kzd_transactions:doc().
to_json(#transaction{private_fields=PrivateFields}=Transaction) ->
    Setters = [{fun kzd_transactions:set_account_id/2, account_id(Transaction)}
              ,{fun kzd_transactions:set_account_name/2, account_name(Transaction)}
              ,{fun kzd_transactions:set_unit_amount/2, unit_amount(Transaction)}
              ,{fun kzd_transactions:set_description/2, description(Transaction)}
              ,{fun kzd_transactions:set_executor_trigger/2, executor_trigger(Transaction)}
              ,{fun kzd_transactions:set_executor_module/2, executor_module(Transaction)}
              ,{fun kzd_transactions:set_bookkeeper_type/2, bookkeeper_type(Transaction)}
              ,{fun kzd_transactions:set_bookkeeper_vendor_id/2, bookkeeper_vendor_id(Transaction)}
              ,{fun kzd_transactions:set_bookkeeper_results/2, bookkeeper_results(Transaction)}
              ,{fun kzd_transactions:set_metadata/2, metadata(Transaction)}
              ,{fun kzd_transactions:set_audit/2, audit(Transaction)}
              ,{fun kzd_transactions:set_order_id/2, order_id(Transaction)}
              ,{fun kzd_transactions:set_status/2, status(Transaction)}
              ,{fun kzd_transactions:set_transaction_type/2, transaction_type(Transaction)}
              ],
    TransactionJObj = kz_json:merge(PrivateFields, kz_doc:setters(Setters)),
    Props = [{<<"pvt_type">>, kzd_transactions:type()}
            ,{<<"pvt_created">>, get_created_timestamp(TransactionJObj)}
            ,{<<"pvt_modified">>, kz_time:now_s()}
            ,{<<"pvt_account_id">>, account_id(Transaction)}
             | maybe_add_id(TransactionJObj)
            ],
    kz_json:set_values(Props, TransactionJObj).

-spec get_created_timestamp(kzd_transactions:doc()) -> integer().
get_created_timestamp(TransactionJObj) ->
    kz_json:get_integer_value(<<"pvt_created">>, TransactionJObj, kz_time:now_s()).

-spec maybe_add_id(kzd_transactions:doc()) -> kz_term:proplist().
maybe_add_id(TransactionJObj) ->
    case kz_doc:id(TransactionJObj) of
        'undefined' ->
            [{<<"_id">>, kazoo_modb_util:modb_id()}
            ,{<<"pvt_created">>, kz_time:now_s()}
            ];
        _ -> []
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec from_json(kzd_transactions:doc()) -> transaction().
from_json(JObj) ->
    Setters = [{fun set_account_id/2, kzd_transactions:account_id(JObj)}
              ,{fun set_account_name/2, kzd_transactions:account_name(JObj)}
              ,{fun set_unit_amount/2, kzd_transactions:unit_amount(JObj)}
              ,{fun set_description/2, kzd_transactions:description(JObj)}
              ,{fun set_executor_trigger/2, kzd_transactions:executor_trigger(JObj)}
              ,{fun set_executor_module/2, kzd_transactions:executor_module(JObj)}
              ,{fun set_bookkeeper_type/2, kzd_transactions:bookkeeper_type(JObj)}
              ,{fun set_bookkeeper_vendor_id/2, kzd_transactions:bookkeeper_vendor_id(JObj)}
              ,{fun set_bookkeeper_results/2, kzd_transactions:bookkeeper_results(JObj)}
              ,{fun set_metadata/2, kzd_transactions:metadata(JObj)}
              ,{fun set_audit/2, kzd_transactions:audit(JObj)}
              ,{fun set_order_id/2, kzd_transactions:order_id(JObj)}
              ,{fun set_status/2, kzd_transactions:status(JObj)}
              ,{fun set_transaction_type/2, kzd_transactions:transaction_type(JObj)}
              ,{fun(Transaction, PrivateFields) ->
                        Transaction#transaction{private_fields=PrivateFields}
                end
               ,kz_doc:private_fields(JObj)
               }
              ],
    setters(Setters).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch(kz_term:ne_binary(), kz_term:ne_binary()) -> {'ok', transaction()} |
                                                         kz_datamgr:data_error().
fetch(Account, ?MATCH_MODB_PREFIX(Year, Month, _)=Id) ->
    fetch(Account, Id, Year, Month).

-spec fetch(kz_term:ne_binary(), kz_term:ne_binary(), kz_time:year()|kz_term:ne_binary(), kz_time:month()|kz_term:ne_binary()) ->
                   {'ok', transaction()} | {'error', any()}.
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
-spec refund(transaction()) -> {'ok', transaction()} | {'error', any()}.
refund(Transaction) ->
    save(set_transaction_type(Transaction, kzd_transactions:type_refund())).

-spec refund(transaction(), kz_term:ne_binary()) -> {'ok', transaction()} | {'error', any()}.
refund(Transaction, AccountId) ->
    save(set_transaction_type(Transaction, kzd_transactions:type_refund()), AccountId).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec sale(transaction()) -> {'ok', transaction()} | {'error', any()}.
sale(Transaction) ->
    save(set_transaction_type(Transaction, kzd_transactions:type_sale())).

-spec sale(transaction(), kz_term:ne_binary()) -> {'ok', transaction()} | {'error', any()}.
sale(Transaction, AccountId) ->
    save(set_transaction_type(Transaction, kzd_transactions:type_sale()), AccountId).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec save(transaction(), kz_term:ne_binary()) -> {'ok', transaction()} | {'error', any()}.
save(Transaction, Account) ->
    MODb = kazoo_modb:get_modb(Account),
    save(set_modb(Transaction, MODb)).

-spec save(transaction(), kz_term:ne_binary(), kz_time:year(), kz_time:month()) ->
                  {'ok', transaction()} | {'error', any()}.
save(Transaction, Account, Year, Month) ->
    MODb = kazoo_modb:get_modb(Account, Year, Month),
    save(set_modb(Transaction, MODb)).

-spec save(transaction()) -> {'ok', transaction()} | {'error', any()}.
save(Transaction) ->
    BookkeeperType = bookkeeper_type(Transaction),
    case kz_term:is_empty(BookkeeperType)
        orelse BookkeeperType =:= kzd_services:default_bookkeeper_type()
    of
        'true' -> {'error', 'invalid_bookkeeper'};
        'false' -> do_save(Transaction)
    end.

-spec do_save(transaction()) -> {'ok', transaction()} | {'error', any()}.
do_save(Transaction) ->
    MODb = modb(Transaction),
    {AccountId, _Year, _Month} = kazoo_modb_util:split_account_mod(MODb),
    Props = [{<<"pvt_account_id">>, AccountId}],

    TransactionJObj = kz_json:set_values(Props, to_json(Transaction)),
    IsPending = status_pending(Transaction),

    case kazoo_modb:save_doc(MODb, TransactionJObj) of
        {'ok', SavedJObj} when IsPending ->
            lager:debug("created ~s transaction in ~s ~p-~p for $~w"
                       ,[transaction_type(Transaction)
                        ,AccountId
                        ,_Year
                        ,_Month
                        ,dollar_amount(Transaction)
                        ]
                       ),
            submit_to_bookkeeper(from_json(SavedJObj));
        {'ok', SavedJObj} ->
            lager:debug("updated ~s transaction in ~s ~p-~p"
                       ,[transaction_type(Transaction)
                        ,AccountId
                        ,_Year
                        ,_Month
                        ]
                       ),
            {'ok', from_json(SavedJObj)};
        {'error', _Reason} = Error ->
            lager:info("unable to save ~s transaction in ~s ~p-~p: ~p"
                      ,[transaction_type(Transaction)
                       ,AccountId
                       ,_Year
                       ,_Month
                       ,_Reason
                       ]),
            Error
    end.

-spec submit_to_bookkeeper(transaction()) -> {'ok', transaction()}.
submit_to_bookkeeper(Transaction) ->
    Sale = kzd_transactions:type_sale(),
    Refund = kzd_transactions:type_refund(),
    case transaction_type(Transaction) of
        Sale -> attempt_bookkeeper_sale(Transaction);
        Refund -> attempt_bookkeeper_refund(Transaction)
    end.

-spec attempt_bookkeeper_sale(transaction()) -> {'ok', transaction()} | {'error', any()}.
attempt_bookkeeper_sale(Transaction) ->
    Request = [{<<"Bookkeeper-Type">>, bookkeeper_type(Transaction)}
              ,{<<"Vendor-ID">>, bookkeeper_vendor_id(Transaction)}
              ,{<<"Account-ID">>, account_id(Transaction)}
              ,{<<"Transaction-ID">>, id(Transaction)}
              ,{<<"Transaction-DB">>, modb(Transaction)}
              ,{<<"Amount">>, unit_amount(Transaction)}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    Result = kz_amqp_worker:call(Request
                                ,fun kapi_bookkeepers:publish_sale_req/1
                                ,fun kapi_bookkeepers:sale_resp_v/1
                                ,20 * ?MILLISECONDS_IN_SECOND
                                ),
    handle_bookkeeper_result(Transaction, Result).

-spec attempt_bookkeeper_refund(transaction()) -> {'ok', transaction()} | {'error', any()}.
attempt_bookkeeper_refund(Transaction) ->
    Request = [{<<"Bookkeeper-Type">>, bookkeeper_type(Transaction)}
              ,{<<"Vendor-ID">>, bookkeeper_vendor_id(Transaction)}
              ,{<<"Account-ID">>, account_id(Transaction)}
              ,{<<"Transaction-ID">>, id(Transaction)}
              ,{<<"Transaction-DB">>, modb(Transaction)}
              ,{<<"Amount">>, unit_amount(Transaction)}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    Result = kz_amqp_worker:call(Request
                                ,fun kapi_bookkeepers:publish_refund_req/1
                                ,fun kapi_bookkeepers:refund_resp_v/1
                                ),
    handle_bookkeeper_result(Transaction, Result).

-spec handle_bookkeeper_result(transaction(), kz_amqp_worker:request_return()) ->
                                      {'ok', transaction()} |
                                      {'error', any()}.
handle_bookkeeper_result(Transaction, {'ok', Result}) ->
    RemoveKeys = [<<"Transaction-ID">>
                 ,<<"Transaction-DB">>
                 ],
    BookkeeperResult =
        kz_json:normalize(
          kz_json:delete_keys(RemoveKeys
                             ,kz_api:remove_defaults(Result)
                             )
         ),
    case kz_json:get_ne_binary_value(<<"Status">>, Result, <<"fatal">>) of
        <<"success">> ->
            Setters = [{fun set_status/2, status_completed()}
                      ,{fun set_bookkeeper_results/2, BookkeeperResult}
                      ],
            UpdatedTransaction = setters(Transaction, Setters),
            _ = send_notification(UpdatedTransaction),
            save(UpdatedTransaction);
        <<"error">> ->
            Setters = [{fun set_status/2, status_failed()}
                      ,{fun set_bookkeeper_results/2, BookkeeperResult}
                      ],
            UpdatedTransaction = setters(Transaction, Setters),
            _ = send_notification(UpdatedTransaction),
            save(UpdatedTransaction);
        <<"fatal">> ->
            Setters = [{fun set_status/2, status_failed()}
                      ,{fun set_bookkeeper_results/2, BookkeeperResult}
                      ],
            save(setters(Transaction, Setters))
    end;
handle_bookkeeper_result(Transaction, {'error', 'timeout'}) ->
    BookkeeperResult =
        kz_json:from_list(
          [{<<"message">>, <<"Timeout waiting for bookkeeper result">>}
          ,{<<"status">>, <<"fatal">>}
          ,{<<"reason">>, <<"timeout">>}
          ,{<<"details">>, kz_json:new()}
          ]
         ),
    Setters = [{fun set_status/2, status_failed()}
              ,{fun set_bookkeeper_results/2, BookkeeperResult}
              ],
    UpdatedTransaction = setters(Transaction, Setters),
    _ = send_notification(UpdatedTransaction),
    save(UpdatedTransaction);
handle_bookkeeper_result(Transaction, {'error', _Else}) ->
    lager:info("request to bookkeeper failed: ~p", [_Else]),
    Setters = [{fun set_status/2, status_failed()}],
    UpdatedTransaction = setters(Transaction, Setters),
    _ = send_notification(UpdatedTransaction),
    save(UpdatedTransaction).

-spec send_notification(transaction()) -> 'ok'.
send_notification(Transaction) ->
    lager:debug("sending ~s notification", [status(Transaction)]),
    BookkeeperResult = bookkeeper_results(Transaction),
    Details = kz_json:get_ne_json_value(<<"details">>, BookkeeperResult, kz_json:new()),
    Notification =
        [{<<"Account-ID">>, account_id(Transaction)}
        ,{<<"Amount">>, dollar_amount(Transaction)}
        ,{<<"Response">>, find_response(Transaction)}
        ,{<<"Success">>, status_completed(Transaction)}
        ,{<<"Timestamp">>, created(Transaction)}
        ,{<<"Add-Ons">>, kz_json:get_value(<<"add_ons">>, Details)}
        ,{<<"Billing-Address">>, kz_json:get_value(<<"billing_address">>, Details)}
        ,{<<"Card-Last-Four">>, card_last_four(Transaction, Details)}
        ,{<<"Currency-Code">>, kz_json:get_value(<<"currency_code">>, Details)}
        ,{<<"ID">>, id(Transaction)}
        ,{<<"Purchase-Order">>, kz_json:get_value(<<"purchase_order">>, Details)}
        ,{<<"Tax-Amount">>, kz_json:get_value(<<"tax_amount">>, Details)}
         | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
        ],
    kz_amqp_worker:cast(Notification, fun kapi_notifications:publish_transaction/1).

-spec card_last_four(transaction(), kz_json:object()) -> kz_term:api_ne_binary().
card_last_four(Transaction, BookkeeperDetails) ->
    case kz_json:get_ne_binary_value([<<"card">>, <<"last_four">>], BookkeeperDetails) of
        'undefined' -> default_card_last_four(Transaction);
        LastFour -> LastFour
    end.

-spec default_card_last_four(transaction()) -> kz_term:api_ne_binary().
default_card_last_four(Transaction) ->
    case kz_services_payment_tokens:default(account_id(Transaction), bookkeeper_type(Transaction)) of
        'undefined' -> 'undefined';
        DefaultToken ->
            kz_json:get_ne_binary_value([<<"metadata">>, <<"last_four">>], DefaultToken)
    end.

-spec find_response(transaction()) -> kz_term:ne_binary().
find_response(Transaction) ->
    BookkeeperResult = bookkeeper_results(Transaction),
    Details = kz_json:get_ne_json_value(<<"details">>, BookkeeperResult, kz_json:new()),
    case kz_json:get_ne_binary_value(<<"processor_response_text">>, Details) of
        'undefined' ->
            kz_json:get_ne_binary_value(<<"message">>, BookkeeperResult, status(Transaction));
        Response -> Response
    end.
