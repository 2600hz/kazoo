%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_ledgers).

-export([ledger_types/0
        ,type_credit/0
        ,type_debit/0
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
-export([source/1
        ,source/2
        ,set_source/2
        ]).
-export([source_id/1
        ,source_id/2
        ,set_source_id/2
        ]).
-export([source_service/1
        ,source_service/2
        ,set_source_service/2
        ]).
-export([usage/1
        ,usage/2
        ,set_usage/2
        ]).
-export([usage_quantity/1
        ,usage_quantity/2
        ,set_usage_quantity/2
        ]).
-export([usage_type/1
        ,usage_type/2
        ,set_usage_type/2
        ]).
-export([usage_unit/1
        ,usage_unit/2
        ,set_usage_unit/2
        ]).
-export([period/1
        ,period/2
        ,set_period/2
        ]).
-export([period_end/1
        ,period_end/2
        ,set_period_end/2
        ]).
-export([period_start/1
        ,period_start/2
        ,set_period_start/2
        ]).
-export([metadata/1
        ,metadata/2
        ,set_metadata/2
        ]).
-export([audit/1
        ,audit/2
        ,set_audit/2
        ]).
-export([ledger_type/1
        ,ledger_type/2
        ,set_ledger_type/2
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-type ledger_type() :: kz_term:ne_binary().
-export_type([doc/0
             ,ledger_type/0
             ]).

-define(CREDIT, <<"credit">>).
-define(DEBIT, <<"debit">>).

-define(ACCOUNT, <<"account">>).
-define(ACCOUNT_ID, [?ACCOUNT, <<"id">>]).
-define(ACCOUNT_NAME, [?ACCOUNT, <<"name">>]).
-define(AMOUNT, <<"amount">>).
-define(DESCRIPTION, <<"description">>).
-define(EXECUTOR, <<"executor">>).
-define(EXECUTOR_TRIGGER, [?EXECUTOR, <<"trigger">>]).
-define(EXECUTOR_MODULE, [?EXECUTOR, <<"module">>]).
-define(SOURCE, <<"source">>).
-define(SOURCE_ID, [?SOURCE, <<"id">>]).
-define(SOURCE_SERVICE, [?SOURCE, <<"service">>]).
-define(USAGE, <<"usage">>).
-define(USAGE_QUANTITY, [?USAGE, <<"quantity">>]).
-define(USAGE_TYPE, [?USAGE, <<"type">>]).
-define(USAGE_UNIT, [?USAGE, <<"unit">>]).
-define(PERIOD, <<"period">>).
-define(PERIOD_END, [?PERIOD, <<"end">>]).
-define(PERIOD_START, [?PERIOD, <<"start">>]).
-define(METADATA, <<"metadata">>).
-define(AUDIT, <<"audit">>).
-define(PVT_LEDGER_TYPE, <<"pvt_ledger_type">>).

-define(PVT_TYPE, <<"ledger">>).

-define(SCHEMA, <<"ledgers">>).

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
-spec ledger_types() -> kz_term:ne_binaries().
ledger_types() ->
    [type_debit(), type_credit()].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec type_credit() -> kz_term:ne_binary().
type_credit() ->
    ?CREDIT.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec type_debit() -> kz_term:ne_binary().
type_debit() ->
    ?DEBIT.

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
-spec set_dollar_amount(doc(), integer()) -> doc().
set_dollar_amount(Doc, Amount) ->
    Units = kz_currency:dollars_to_units(Amount),
    kz_json:set_value(?AMOUNT, Units, Doc).

%%------------------------------------------------------------------------------
%% DO NOT USE ME!
%%
%% The API document is guided by the schema but the database document is guided
%% by kz_ledger (which in this case uses `amount` as the key for units. So the API
%% request's `amount` is in currency but the db doc's `amount` is in units.
%%
%% Commented out the code below to show that we know it breaks convention
%%------------------------------------------------------------------------------
%% -spec amount(doc()) -> kz_term:api_number().
%% amount(Doc) ->
%%     amount(Doc, 'undefined').

%% -spec amount(doc(), Default) -> number() | Default.
%% amount(Doc, Default) ->
%%     kz_json:get_float_value([<<"amount">>], Doc, Default).

%% -spec set_amount(doc(), number()) -> doc().
%% set_amount(Doc, Amount) ->
%%     kz_json:set_value([<<"amount">>], Amount, Doc).


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
-spec source(doc()) -> kz_term:api_object().
source(Doc) ->
    source(Doc, 'undefined').

-spec source(doc(), Default) -> kz_json:object() | Default.
source(Doc, Default) ->
    kz_json:get_json_value(?SOURCE, Doc, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_source(doc(), kz_json:object()) -> doc().
set_source(Doc, Source) ->
    kz_json:set_value(?SOURCE, Source, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec source_id(doc()) -> kz_term:api_binary().
source_id(Doc) ->
    source_id(Doc, 'undefined').

-spec source_id(doc(), Default) -> binary() | Default.
source_id(Doc, Default) ->
    kz_json:get_binary_value(?SOURCE_ID, Doc, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_source_id(doc(), binary()) -> doc().
set_source_id(Doc, SourceId) ->
    kz_json:set_value(?SOURCE_ID, SourceId, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec source_service(doc()) -> kz_term:api_binary().
source_service(Doc) ->
    source_service(Doc, 'undefined').

-spec source_service(doc(), Default) -> binary() | Default.
source_service(Doc, Default) ->
    kz_json:get_binary_value(?SOURCE_SERVICE, Doc, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_source_service(doc(), binary()) -> doc().
set_source_service(Doc, SourceService) ->
    kz_json:set_value(?SOURCE_SERVICE, SourceService, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec usage(doc()) -> kz_term:api_object().
usage(Doc) ->
    usage(Doc, 'undefined').

-spec usage(doc(), Default) -> kz_json:object() | Default.
usage(Doc, Default) ->
    kz_json:get_json_value(?USAGE, Doc, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_usage(doc(), kz_json:object()) -> doc().
set_usage(Doc, Usage) ->
    kz_json:set_value(?USAGE, Usage, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec usage_quantity(doc()) -> kz_term:api_integer().
usage_quantity(Doc) ->
    usage_quantity(Doc, 'undefined').

-spec usage_quantity(doc(), Default) -> integer() | Default.
usage_quantity(Doc, Default) ->
    kz_json:get_integer_value(?USAGE_QUANTITY, Doc, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_usage_quantity(doc(), integer()) -> doc().
set_usage_quantity(Doc, UsageQuantity) ->
    kz_json:set_value(?USAGE_QUANTITY, UsageQuantity, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec usage_type(doc()) -> kz_term:api_binary().
usage_type(Doc) ->
    usage_type(Doc, 'undefined').

-spec usage_type(doc(), Default) -> binary() | Default.
usage_type(Doc, Default) ->
    kz_json:get_binary_value(?USAGE_TYPE, Doc, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_usage_type(doc(), binary()) -> doc().
set_usage_type(Doc, UsageType) ->
    kz_json:set_value(?USAGE_TYPE, UsageType, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec usage_unit(doc()) -> kz_term:api_binary().
usage_unit(Doc) ->
    usage_unit(Doc, 'undefined').

-spec usage_unit(doc(), Default) -> binary() | Default.
usage_unit(Doc, Default) ->
    kz_json:get_binary_value(?USAGE_UNIT, Doc, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_usage_unit(doc(), binary()) -> doc().
set_usage_unit(Doc, UsageUnit) ->
    kz_json:set_value(?USAGE_UNIT, UsageUnit, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec period(doc()) -> kz_term:api_object().
period(Doc) ->
    period(Doc, 'undefined').

-spec period(doc(), Default) -> kz_json:object() | Default.
period(Doc, Default) ->
    kz_json:get_json_value(?PERIOD, Doc, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_period(doc(), kz_json:object()) -> doc().
set_period(Doc, Period) ->
    kz_json:set_value(?PERIOD, Period, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec period_end(doc()) -> kz_term:api_integer().
period_end(Doc) ->
    period_end(Doc, 'undefined').

-spec period_end(doc(), Default) -> integer() | Default.
period_end(Doc, Default) ->
    kz_json:get_integer_value(?PERIOD_END, Doc, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_period_end(doc(), integer()) -> doc().
set_period_end(Doc, PeriodEnd) ->
    kz_json:set_value(?PERIOD_END, PeriodEnd, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec period_start(doc()) -> kz_term:api_integer().
period_start(Doc) ->
    period_start(Doc, 'undefined').

-spec period_start(doc(), Default) -> integer() | Default.
period_start(Doc, Default) ->
    kz_json:get_integer_value(?PERIOD_START, Doc, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_period_start(doc(), integer()) -> doc().
set_period_start(Doc, PeriodStart) ->
    kz_json:set_value(?PERIOD_START, PeriodStart, Doc).

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
-spec ledger_type(doc()) -> kz_term:ne_binary().
ledger_type(Doc) ->
    ledger_type(Doc, type_debit()).

-spec ledger_type(doc(), Default) -> kz_term:ne_binary() | Default.
ledger_type(Doc, Default) ->
    kz_json:get_ne_binary_value(?PVT_LEDGER_TYPE, Doc, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_ledger_type(doc(), kz_term:ne_binary()) -> doc().
set_ledger_type(Doc, Type) ->
    kz_json:set_value(?PVT_LEDGER_TYPE, Type, Doc).
