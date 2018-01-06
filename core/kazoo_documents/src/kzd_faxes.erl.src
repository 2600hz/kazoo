-module(kzd_faxes).

-export([new/0]).
-export([attempts/1, attempts/2, set_attempts/2]).
-export([document/1, document/2, set_document/2]).
-export([from_name/1, from_name/2, set_from_name/2]).
-export([from_number/1, from_number/2, set_from_number/2]).
-export([notifications/1, notifications/2, set_notifications/2]).
-export([retries/1, retries/2, set_retries/2]).
-export([to_name/1, to_name/2, set_to_name/2]).
-export([to_number/1, to_number/2, set_to_number/2]).
-export([tx_result/1, tx_result/2, set_tx_result/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec attempts(doc()) -> integer().
-spec attempts(doc(), Default) -> integer() | Default.
attempts(Doc) ->
    attempts(Doc, 0).
attempts(Doc, Default) ->
    kz_json:get_integer_value(<<"attempts">>, Doc, Default).

-spec set_attempts(doc(), integer()) -> doc().
set_attempts(Doc, Attempts) ->
    kz_json:set_value(<<"attempts">>, Attempts, Doc).

-spec document(doc()) -> api_object().
-spec document(doc(), Default) -> kz_json:object() | Default.
document(Doc) ->
    document(Doc, 'undefined').
document(Doc, Default) ->
    kz_json:get_json_value(<<"document">>, Doc, Default).

-spec set_document(doc(), kz_json:object()) -> doc().
set_document(Doc, Document) ->
    kz_json:set_value(<<"document">>, Document, Doc).

-spec from_name(doc()) -> api_binary().
-spec from_name(doc(), Default) -> binary() | Default.
from_name(Doc) ->
    from_name(Doc, 'undefined').
from_name(Doc, Default) ->
    kz_json:get_binary_value(<<"from_name">>, Doc, Default).

-spec set_from_name(doc(), binary()) -> doc().
set_from_name(Doc, FromName) ->
    kz_json:set_value(<<"from_name">>, FromName, Doc).

-spec from_number(doc()) -> api_binary().
-spec from_number(doc(), Default) -> binary() | Default.
from_number(Doc) ->
    from_number(Doc, 'undefined').
from_number(Doc, Default) ->
    kz_json:get_binary_value(<<"from_number">>, Doc, Default).

-spec set_from_number(doc(), binary()) -> doc().
set_from_number(Doc, FromNumber) ->
    kz_json:set_value(<<"from_number">>, FromNumber, Doc).

-spec notifications(doc()) -> api_object().
-spec notifications(doc(), Default) -> kz_json:object() | Default.
notifications(Doc) ->
    notifications(Doc, 'undefined').
notifications(Doc, Default) ->
    kz_json:get_json_value(<<"notifications">>, Doc, Default).

-spec set_notifications(doc(), kz_json:object()) -> doc().
set_notifications(Doc, Notifications) ->
    kz_json:set_value(<<"notifications">>, Notifications, Doc).

-spec retries(doc()) -> integer().
-spec retries(doc(), Default) -> integer() | Default.
retries(Doc) ->
    retries(Doc, 1).
retries(Doc, Default) ->
    kz_json:get_integer_value(<<"retries">>, Doc, Default).

-spec set_retries(doc(), integer()) -> doc().
set_retries(Doc, Retries) ->
    kz_json:set_value(<<"retries">>, Retries, Doc).

-spec to_name(doc()) -> api_binary().
-spec to_name(doc(), Default) -> binary() | Default.
to_name(Doc) ->
    to_name(Doc, 'undefined').
to_name(Doc, Default) ->
    kz_json:get_binary_value(<<"to_name">>, Doc, Default).

-spec set_to_name(doc(), binary()) -> doc().
set_to_name(Doc, ToName) ->
    kz_json:set_value(<<"to_name">>, ToName, Doc).

-spec to_number(doc()) -> api_binary().
-spec to_number(doc(), Default) -> binary() | Default.
to_number(Doc) ->
    to_number(Doc, 'undefined').
to_number(Doc, Default) ->
    kz_json:get_binary_value(<<"to_number">>, Doc, Default).

-spec set_to_number(doc(), binary()) -> doc().
set_to_number(Doc, ToNumber) ->
    kz_json:set_value(<<"to_number">>, ToNumber, Doc).

-spec tx_result(doc()) -> api_object().
-spec tx_result(doc(), Default) -> kz_json:object() | Default.
tx_result(Doc) ->
    tx_result(Doc, 'undefined').
tx_result(Doc, Default) ->
    kz_json:get_json_value(<<"tx_result">>, Doc, Default).

-spec set_tx_result(doc(), kz_json:object()) -> doc().
set_tx_result(Doc, TxResult) ->
    kz_json:set_value(<<"tx_result">>, TxResult, Doc).
