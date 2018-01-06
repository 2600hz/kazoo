-module(kzd_ledgers).

-export([new/0]).
-export([account/1, account/2, set_account/2]).
-export([amount/1, amount/2, set_amount/2]).
-export([description/1, description/2, set_description/2]).
-export([metadata/1, metadata/2, set_metadata/2]).
-export([period/1, period/2, set_period/2]).
-export([source/1, source/2, set_source/2]).
-export([usage/1, usage/2, set_usage/2]).


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
    kz_json:get_json_value(<<"account">>, Doc, Default).

-spec set_account(doc(), kz_json:object()) -> doc().
set_account(Doc, Account) ->
    kz_json:set_value(<<"account">>, Account, Doc).

-spec amount(doc()) -> api_integer().
-spec amount(doc(), Default) -> integer() | Default.
amount(Doc) ->
    amount(Doc, 'undefined').
amount(Doc, Default) ->
    kz_json:get_integer_value(<<"amount">>, Doc, Default).

-spec set_amount(doc(), integer()) -> doc().
set_amount(Doc, Amount) ->
    kz_json:set_value(<<"amount">>, Amount, Doc).

-spec description(doc()) -> api_binary().
-spec description(doc(), Default) -> binary() | Default.
description(Doc) ->
    description(Doc, 'undefined').
description(Doc, Default) ->
    kz_json:get_binary_value(<<"description">>, Doc, Default).

-spec set_description(doc(), binary()) -> doc().
set_description(Doc, Description) ->
    kz_json:set_value(<<"description">>, Description, Doc).

-spec metadata(doc()) -> api_object().
-spec metadata(doc(), Default) -> kz_json:object() | Default.
metadata(Doc) ->
    metadata(Doc, 'undefined').
metadata(Doc, Default) ->
    kz_json:get_json_value(<<"metadata">>, Doc, Default).

-spec set_metadata(doc(), kz_json:object()) -> doc().
set_metadata(Doc, Metadata) ->
    kz_json:set_value(<<"metadata">>, Metadata, Doc).

-spec period(doc()) -> api_object().
-spec period(doc(), Default) -> kz_json:object() | Default.
period(Doc) ->
    period(Doc, 'undefined').
period(Doc, Default) ->
    kz_json:get_json_value(<<"period">>, Doc, Default).

-spec set_period(doc(), kz_json:object()) -> doc().
set_period(Doc, Period) ->
    kz_json:set_value(<<"period">>, Period, Doc).

-spec source(doc()) -> api_object().
-spec source(doc(), Default) -> kz_json:object() | Default.
source(Doc) ->
    source(Doc, 'undefined').
source(Doc, Default) ->
    kz_json:get_json_value(<<"source">>, Doc, Default).

-spec set_source(doc(), kz_json:object()) -> doc().
set_source(Doc, Source) ->
    kz_json:set_value(<<"source">>, Source, Doc).

-spec usage(doc()) -> api_object().
-spec usage(doc(), Default) -> kz_json:object() | Default.
usage(Doc) ->
    usage(Doc, 'undefined').
usage(Doc, Default) ->
    kz_json:get_json_value(<<"usage">>, Doc, Default).

-spec set_usage(doc(), kz_json:object()) -> doc().
set_usage(Doc, Usage) ->
    kz_json:set_value(<<"usage">>, Usage, Doc).
