-module(kzd_ledgers).

-export([new/0]).
-export([account/1, account/2, set_account/2]).
-export([account_id/1, account_id/2, set_account_id/2]).
-export([account_name/1, account_name/2, set_account_name/2]).
-export([amount/1, amount/2, set_amount/2]).
-export([description/1, description/2, set_description/2]).
-export([metadata/1, metadata/2, set_metadata/2]).
-export([period/1, period/2, set_period/2]).
-export([period_end/1, period_end/2, set_period_end/2]).
-export([period_start/1, period_start/2, set_period_start/2]).
-export([source/1, source/2, set_source/2]).
-export([source_id/1, source_id/2, set_source_id/2]).
-export([source_service/1, source_service/2, set_source_service/2]).
-export([usage/1, usage/2, set_usage/2]).
-export([usage_quantity/1, usage_quantity/2, set_usage_quantity/2]).
-export([usage_type/1, usage_type/2, set_usage_type/2]).
-export([usage_unit/1, usage_unit/2, set_usage_unit/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec account(doc()) -> api_object().
-spec account(doc(), Default) -> kz_json:object() | Default.
account(Doc) ->
    account(Doc, 'undefined').
account(Doc, Default) ->
    kz_json:get_json_value([<<"account">>], Doc, Default).

-spec set_account(doc(), kz_json:object()) -> doc().
set_account(Doc, Account) ->
    kz_json:set_value([<<"account">>], Account, Doc).

-spec account_id(doc()) -> api_binary().
-spec account_id(doc(), Default) -> binary() | Default.
account_id(Doc) ->
    account_id(Doc, 'undefined').
account_id(Doc, Default) ->
    kz_json:get_binary_value([<<"account">>, <<"id">>], Doc, Default).

-spec set_account_id(doc(), binary()) -> doc().
set_account_id(Doc, AccountId) ->
    kz_json:set_value([<<"account">>, <<"id">>], AccountId, Doc).

-spec account_name(doc()) -> api_binary().
-spec account_name(doc(), Default) -> binary() | Default.
account_name(Doc) ->
    account_name(Doc, 'undefined').
account_name(Doc, Default) ->
    kz_json:get_binary_value([<<"account">>, <<"name">>], Doc, Default).

-spec set_account_name(doc(), binary()) -> doc().
set_account_name(Doc, AccountName) ->
    kz_json:set_value([<<"account">>, <<"name">>], AccountName, Doc).

-spec amount(doc()) -> api_integer().
-spec amount(doc(), Default) -> integer() | Default.
amount(Doc) ->
    amount(Doc, 'undefined').
amount(Doc, Default) ->
    kz_json:get_integer_value([<<"amount">>], Doc, Default).

-spec set_amount(doc(), integer()) -> doc().
set_amount(Doc, Amount) ->
    kz_json:set_value([<<"amount">>], Amount, Doc).

-spec description(doc()) -> api_binary().
-spec description(doc(), Default) -> binary() | Default.
description(Doc) ->
    description(Doc, 'undefined').
description(Doc, Default) ->
    kz_json:get_binary_value([<<"description">>], Doc, Default).

-spec set_description(doc(), binary()) -> doc().
set_description(Doc, Description) ->
    kz_json:set_value([<<"description">>], Description, Doc).

-spec metadata(doc()) -> api_object().
-spec metadata(doc(), Default) -> kz_json:object() | Default.
metadata(Doc) ->
    metadata(Doc, 'undefined').
metadata(Doc, Default) ->
    kz_json:get_json_value([<<"metadata">>], Doc, Default).

-spec set_metadata(doc(), kz_json:object()) -> doc().
set_metadata(Doc, Metadata) ->
    kz_json:set_value([<<"metadata">>], Metadata, Doc).

-spec period(doc()) -> api_object().
-spec period(doc(), Default) -> kz_json:object() | Default.
period(Doc) ->
    period(Doc, 'undefined').
period(Doc, Default) ->
    kz_json:get_json_value([<<"period">>], Doc, Default).

-spec set_period(doc(), kz_json:object()) -> doc().
set_period(Doc, Period) ->
    kz_json:set_value([<<"period">>], Period, Doc).

-spec period_end(doc()) -> api_integer().
-spec period_end(doc(), Default) -> integer() | Default.
period_end(Doc) ->
    period_end(Doc, 'undefined').
period_end(Doc, Default) ->
    kz_json:get_integer_value([<<"period">>, <<"end">>], Doc, Default).

-spec set_period_end(doc(), integer()) -> doc().
set_period_end(Doc, PeriodEnd) ->
    kz_json:set_value([<<"period">>, <<"end">>], PeriodEnd, Doc).

-spec period_start(doc()) -> api_integer().
-spec period_start(doc(), Default) -> integer() | Default.
period_start(Doc) ->
    period_start(Doc, 'undefined').
period_start(Doc, Default) ->
    kz_json:get_integer_value([<<"period">>, <<"start">>], Doc, Default).

-spec set_period_start(doc(), integer()) -> doc().
set_period_start(Doc, PeriodStart) ->
    kz_json:set_value([<<"period">>, <<"start">>], PeriodStart, Doc).

-spec source(doc()) -> api_object().
-spec source(doc(), Default) -> kz_json:object() | Default.
source(Doc) ->
    source(Doc, 'undefined').
source(Doc, Default) ->
    kz_json:get_json_value([<<"source">>], Doc, Default).

-spec set_source(doc(), kz_json:object()) -> doc().
set_source(Doc, Source) ->
    kz_json:set_value([<<"source">>], Source, Doc).

-spec source_id(doc()) -> api_binary().
-spec source_id(doc(), Default) -> binary() | Default.
source_id(Doc) ->
    source_id(Doc, 'undefined').
source_id(Doc, Default) ->
    kz_json:get_binary_value([<<"source">>, <<"id">>], Doc, Default).

-spec set_source_id(doc(), binary()) -> doc().
set_source_id(Doc, SourceId) ->
    kz_json:set_value([<<"source">>, <<"id">>], SourceId, Doc).

-spec source_service(doc()) -> api_binary().
-spec source_service(doc(), Default) -> binary() | Default.
source_service(Doc) ->
    source_service(Doc, 'undefined').
source_service(Doc, Default) ->
    kz_json:get_binary_value([<<"source">>, <<"service">>], Doc, Default).

-spec set_source_service(doc(), binary()) -> doc().
set_source_service(Doc, SourceService) ->
    kz_json:set_value([<<"source">>, <<"service">>], SourceService, Doc).

-spec usage(doc()) -> api_object().
-spec usage(doc(), Default) -> kz_json:object() | Default.
usage(Doc) ->
    usage(Doc, 'undefined').
usage(Doc, Default) ->
    kz_json:get_json_value([<<"usage">>], Doc, Default).

-spec set_usage(doc(), kz_json:object()) -> doc().
set_usage(Doc, Usage) ->
    kz_json:set_value([<<"usage">>], Usage, Doc).

-spec usage_quantity(doc()) -> api_integer().
-spec usage_quantity(doc(), Default) -> integer() | Default.
usage_quantity(Doc) ->
    usage_quantity(Doc, 'undefined').
usage_quantity(Doc, Default) ->
    kz_json:get_integer_value([<<"usage">>, <<"quantity">>], Doc, Default).

-spec set_usage_quantity(doc(), integer()) -> doc().
set_usage_quantity(Doc, UsageQuantity) ->
    kz_json:set_value([<<"usage">>, <<"quantity">>], UsageQuantity, Doc).

-spec usage_type(doc()) -> api_binary().
-spec usage_type(doc(), Default) -> binary() | Default.
usage_type(Doc) ->
    usage_type(Doc, 'undefined').
usage_type(Doc, Default) ->
    kz_json:get_binary_value([<<"usage">>, <<"type">>], Doc, Default).

-spec set_usage_type(doc(), binary()) -> doc().
set_usage_type(Doc, UsageType) ->
    kz_json:set_value([<<"usage">>, <<"type">>], UsageType, Doc).

-spec usage_unit(doc()) -> api_binary().
-spec usage_unit(doc(), Default) -> binary() | Default.
usage_unit(Doc) ->
    usage_unit(Doc, 'undefined').
usage_unit(Doc, Default) ->
    kz_json:get_binary_value([<<"usage">>, <<"unit">>], Doc, Default).

-spec set_usage_unit(doc(), binary()) -> doc().
set_usage_unit(Doc, UsageUnit) ->
    kz_json:set_value([<<"usage">>, <<"unit">>], UsageUnit, Doc).
