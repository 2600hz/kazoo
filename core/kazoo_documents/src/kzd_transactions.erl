%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_transactions).

-export([transaction_types/0
        ,type_refund/0
        ,type_sale/0
        ]).
-export([new/0]).
-export([type/0
        ,type/1
        ,set_type/1
        ]).
-export([account/1
        ,account/2
        ,set_account/2
        ]).
-export([account_id/1
        ,account_id/2
        ,set_account_id/2
        ]).
-export([account_name/1
        ,account_name/2
        ,set_account_name/2
        ]).
-export([unit_amount/1
        ,unit_amount/2
        ,set_unit_amount/2
        ]).
-export([dollar_amount/1
        ,dollar_amount/2
        ,set_dollar_amount/2
        ]).
-export([description/1
        ,description/2
        ,set_description/2
        ]).
-export([executor/1
        ,executor/2
        ,set_executor/2
        ]).
-export([executor_trigger/1
        ,executor_trigger/2
        ,set_executor_trigger/2
        ]).
-export([executor_module/1
        ,executor_module/2
        ,set_executor_module/2
        ]).
-export([bookkeeper/1
        ,bookkeeper/2
        ,set_bookkeeper/2
        ]).
-export([bookkeeper_type/1
        ,bookkeeper_type/2
        ,set_bookkeeper_type/2
        ]).
-export([bookkeeper_vendor_id/1
        ,bookkeeper_vendor_id/2
        ,set_bookkeeper_vendor_id/2
        ]).
-export([bookkeeper_results/1
        ,bookkeeper_results/2
        ,set_bookkeeper_results/2
        ]).
-export([metadata/1
        ,metadata/2
        ,set_metadata/2
        ]).
-export([audit/1
        ,audit/2
        ,set_audit/2
        ]).
-export([order_id/1
        ,order_id/2
        ,set_order_id/2
        ]).
-export([status/1
        ,status/2
        ,set_status/2
        ]).
-export([transaction_type/1
        ,transaction_type/2
        ,set_transaction_type/2
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-type transaction_type() :: kz_term:ne_binary().
-export_type([doc/0
             ,transaction_type/0
             ]).

-define(REFUND, <<"refund">>).
-define(SALE, <<"sale">>).

-define(ACCOUNT, <<"account">>).
-define(ACCOUNT_ID, [?ACCOUNT, <<"id">>]).
-define(ACCOUNT_NAME, [?ACCOUNT, <<"name">>]).
-define(AMOUNT, <<"amount">>).
-define(DESCRIPTION, <<"description">>).
-define(EXECUTOR, <<"executor">>).
-define(EXECUTOR_TRIGGER, [?EXECUTOR, <<"trigger">>]).
-define(EXECUTOR_MODULE, [?EXECUTOR, <<"module">>]).
-define(BOOKKEEPER, <<"bookkeeper">>).
-define(BOOKKEEPER_TYPE, [?BOOKKEEPER, <<"type">>]).
-define(BOOKKEEPER_VENDOR_ID, [?BOOKKEEPER, <<"vendor_id">>]).
-define(BOOKKEEPER_RESULTS, [?BOOKKEEPER, <<"results">>]).
-define(METADATA, <<"metadata">>).
-define(AUDIT, <<"audit">>).
-define(ORDER_ID, <<"order_id">>).
-define(STATUS, <<"status">>).
-define(PVT_TRANSACTION_TYPE, <<"pvt_transaction_type">>).

-define(PVT_TYPE, <<"transaction">>).

-define(SCHEMA, <<"transactions">>).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec transaction_types() -> kz_term:ne_binaries().
transaction_types() ->
    [type_sale(), type_refund()].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec type_refund() -> kz_term:ne_binary().
type_refund() ->
    ?REFUND.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec type_sale() -> kz_term:ne_binary().
type_sale() ->
    ?SALE.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec type() -> kz_term:ne_binary().
type() -> ?PVT_TYPE.

-spec type(kz_json:object()) -> kz_term:ne_binary().
type(JObj) ->
    kz_doc:type(JObj, type()).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_type(doc()) -> doc().
set_type(JObj) ->
    kz_doc:set_type(JObj, type()).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec account(doc()) -> kz_term:api_object().
account(Doc) ->
    account(Doc, 'undefined').

-spec account(doc(), Default) -> kz_json:object() | Default.
account(Doc, Default) ->
    kz_json:get_json_value(?ACCOUNT, Doc, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_account(doc(), kz_json:object()) -> doc().
set_account(Doc, Account) ->
    kz_json:set_value(?ACCOUNT, Account, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec account_id(doc()) -> kz_term:api_binary().
account_id(Doc) ->
    account_id(Doc, 'undefined').

-spec account_id(doc(), Default) -> binary() | Default.
account_id(Doc, Default) ->
    kz_json:get_binary_value(?ACCOUNT_ID, Doc, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_account_id(doc(), binary()) -> doc().
set_account_id(Doc, AccountId) ->
    kz_json:set_value(?ACCOUNT_ID, AccountId, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec account_name(doc()) -> kz_term:api_binary().
account_name(Doc) ->
    account_name(Doc, 'undefined').

-spec account_name(doc(), Default) -> binary() | Default.
account_name(Doc, Default) ->
    kz_json:get_binary_value(?ACCOUNT_NAME, Doc, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_account_name(doc(), binary()) -> doc().
set_account_name(Doc, AccountName) ->
    kz_json:set_value(?ACCOUNT_NAME, AccountName, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec unit_amount(doc()) -> kz_currency:units() | 'undefined'.
unit_amount(Doc) ->
    unit_amount(Doc, 'undefined').

-spec unit_amount(doc(), Default) -> kz_currency:units() | Default.
unit_amount(Doc, Default) ->
    kz_json:get_integer_value(?AMOUNT, Doc, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_unit_amount(doc(), kz_currency:units()) -> doc().
set_unit_amount(Doc, Amount) ->
    kz_json:set_value(?AMOUNT, Amount, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec dollar_amount(doc()) -> kz_currency:dollars() | 'undefined'.
dollar_amount(Doc) ->
    dollar_amount(Doc, 'undefined').

-spec dollar_amount(doc(), Default) -> kz_currency:dollars() | Default.
dollar_amount(Doc, Default) ->
    Units = unit_amount(Doc, Default),
    kz_currency:units_to_dollars(Units).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_dollar_amount(doc(), kz_currency:dollars()) -> doc().
set_dollar_amount(Doc, Amount) ->
    Units = kz_currency:dollars_to_units(Amount),
    kz_json:set_value(?AMOUNT, Units, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec description(doc()) -> kz_term:api_binary().
description(Doc) ->
    description(Doc, 'undefined').

-spec description(doc(), Default) -> binary() | Default.
description(Doc, Default) ->
    kz_json:get_binary_value(?DESCRIPTION, Doc, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_description(doc(), binary()) -> doc().
set_description(Doc, Description) ->
    kz_json:set_value(?DESCRIPTION, Description, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec executor(doc()) -> kz_term:api_object().
executor(Doc) ->
    executor(Doc, 'undefined').

-spec executor(doc(), Default) -> kz_json:object() | Default.
executor(Doc, Default) ->
    kz_json:get_json_value(?EXECUTOR, Doc, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_executor(doc(), kz_json:object()) -> doc().
set_executor(Doc, Executor) ->
    kz_json:set_value(?EXECUTOR, Executor, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec executor_trigger(doc()) -> kz_term:api_binary().
executor_trigger(Doc) ->
    executor_trigger(Doc, 'undefined').

-spec executor_trigger(doc(), Default) -> binary() | Default.
executor_trigger(Doc, Default) ->
    kz_json:get_binary_value(?EXECUTOR_TRIGGER, Doc, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_executor_trigger(doc(), binary()) -> doc().
set_executor_trigger(Doc, Trigger) ->
    kz_json:set_value(?EXECUTOR_TRIGGER, Trigger, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec executor_module(doc()) -> kz_term:api_binary().
executor_module(Doc) ->
    executor_module(Doc, 'undefined').

-spec executor_module(doc(), Default) -> binary() | Default.
executor_module(Doc, Default) ->
    kz_json:get_binary_value(?EXECUTOR_MODULE, Doc, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_executor_module(doc(), binary()) -> doc().
set_executor_module(Doc, Module) ->
    kz_json:set_value(?EXECUTOR_MODULE, Module, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bookkeeper(doc()) -> kz_term:api_object().
bookkeeper(Doc) ->
    bookkeeper(Doc, 'undefined').

-spec bookkeeper(doc(), Default) -> kz_json:object() | Default.
bookkeeper(Doc, Default) ->
    kz_json:get_json_value(?BOOKKEEPER, Doc, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_bookkeeper(doc(), kz_json:object()) -> doc().
set_bookkeeper(Doc, Bookkeeper) ->
    kz_json:set_value(?BOOKKEEPER, Bookkeeper, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bookkeeper_type(doc()) -> kz_term:api_binary().
bookkeeper_type(Doc) ->
    bookkeeper_type(Doc, 'undefined').

-spec bookkeeper_type(doc(), Default) -> binary() | Default.
bookkeeper_type(Doc, Default) ->
    kz_json:get_binary_value(?BOOKKEEPER_TYPE, Doc, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_bookkeeper_type(doc(), binary()) -> doc().
set_bookkeeper_type(Doc, BookkeeperType) ->
    kz_json:set_value(?BOOKKEEPER_TYPE, BookkeeperType, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bookkeeper_vendor_id(doc()) -> kz_term:api_binary().
bookkeeper_vendor_id(Doc) ->
    bookkeeper_vendor_id(Doc, 'undefined').

-spec bookkeeper_vendor_id(doc(), Default) -> binary() | Default.
bookkeeper_vendor_id(Doc, Default) ->
    kz_json:get_ne_binary_value(?BOOKKEEPER_VENDOR_ID, Doc, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_bookkeeper_vendor_id(doc(), binary()) -> doc().
set_bookkeeper_vendor_id(Doc, VendorId) ->
    kz_json:set_value(?BOOKKEEPER_VENDOR_ID, VendorId, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bookkeeper_results(doc()) -> kz_term:api_object().
bookkeeper_results(Doc) ->
    bookkeeper_results(Doc, 'undefined').

-spec bookkeeper_results(doc(), Default) -> kz_json:object() | Default.
bookkeeper_results(Doc, Default) ->
    kz_json:get_ne_json_value(?BOOKKEEPER_RESULTS, Doc, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_bookkeeper_results(doc(), kz_json:object()) -> doc().
set_bookkeeper_results(Doc, Results) ->
    kz_json:set_value(?BOOKKEEPER_RESULTS, Results, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec metadata(doc()) -> kz_term:api_object().
metadata(Doc) ->
    metadata(Doc, 'undefined').

-spec metadata(doc(), Default) -> kz_json:object() | Default.
metadata(Doc, Default) ->
    kz_json:get_json_value(?METADATA, Doc, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_metadata(doc(), kz_json:object()) -> doc().
set_metadata(Doc, Metadata) ->
    kz_json:set_value(?METADATA, Metadata, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec audit(doc()) -> kz_term:api_object().
audit(Doc) ->
    audit(Doc, 'undefined').

-spec audit(doc(), Default) -> kz_json:object() | Default.
audit(Doc, Default) ->
    kz_json:get_json_value(?AUDIT, Doc, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_audit(doc(), kz_json:object()) -> doc().
set_audit(Doc, Audit) ->
    kz_json:set_value(?AUDIT, Audit, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec order_id(doc()) -> kz_term:api_binary().
order_id(Doc) ->
    order_id(Doc, 'undefined').

-spec order_id(doc(), Default) -> binary() | Default.
order_id(Doc, Default) ->
    kz_json:get_binary_value(?ORDER_ID, Doc, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_order_id(doc(), binary()) -> doc().
set_order_id(Doc, OrderId) ->
    kz_json:set_value(?ORDER_ID, OrderId, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec status(doc()) -> kz_term:api_binary().
status(Doc) ->
    status(Doc, 'undefined').

-spec status(doc(), Default) -> binary() | Default.
status(Doc, Default) ->
    kz_json:get_binary_value(?STATUS, Doc, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_status(doc(), binary()) -> doc().
set_status(Doc, Status) ->
    kz_json:set_value(?STATUS, Status, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec transaction_type(doc()) -> kz_term:ne_binary().
transaction_type(Doc) ->
    transaction_type(Doc, type_sale()).

-spec transaction_type(doc(), Default) -> kz_term:ne_binary() | Default.
transaction_type(Doc, Default) ->
    kz_json:get_ne_binary_value(?PVT_TRANSACTION_TYPE, Doc, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_transaction_type(doc(), kz_term:ne_binary()) -> doc().
set_transaction_type(Doc, Type) ->
    kz_json:set_value(?PVT_TRANSACTION_TYPE, Type, Doc).
