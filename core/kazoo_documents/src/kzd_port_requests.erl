-module(kzd_port_requests).

-export([new/0]).
-export([bill/1, bill/2, set_bill/2]).
-export([comments/1, comments/2, set_comments/2]).
-export([name/1, name/2, set_name/2]).
-export([notifications/1, notifications/2, set_notifications/2]).
-export([numbers/1, numbers/2, set_numbers/2]).
-export([port_state/1, port_state/2, set_port_state/2]).
-export([transfer_date/1, transfer_date/2, set_transfer_date/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec bill(doc()) -> api_object().
-spec bill(doc(), Default) -> kz_json:object() | Default.
bill(Doc) ->
    bill(Doc, 'undefined').
bill(Doc, Default) ->
    kz_json:get_json_value(<<"bill">>, Doc, Default).

-spec set_bill(doc(), kz_json:object()) -> doc().
set_bill(Doc, Bill) ->
    kz_json:set_value(<<"bill">>, Bill, Doc).

-spec comments(doc()) -> api_list().
-spec comments(doc(), Default) -> list() | Default.
comments(Doc) ->
    comments(Doc, 'undefined').
comments(Doc, Default) ->
    kz_json:get_list_value(<<"comments">>, Doc, Default).

-spec set_comments(doc(), list()) -> doc().
set_comments(Doc, Comments) ->
    kz_json:set_value(<<"comments">>, Comments, Doc).

-spec name(doc()) -> api_ne_binary().
-spec name(doc(), Default) -> ne_binary() | Default.
name(Doc) ->
    name(Doc, 'undefined').
name(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"name">>, Doc, Default).

-spec set_name(doc(), ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value(<<"name">>, Name, Doc).

-spec notifications(doc()) -> api_object().
-spec notifications(doc(), Default) -> kz_json:object() | Default.
notifications(Doc) ->
    notifications(Doc, 'undefined').
notifications(Doc, Default) ->
    kz_json:get_json_value(<<"notifications">>, Doc, Default).

-spec set_notifications(doc(), kz_json:object()) -> doc().
set_notifications(Doc, Notifications) ->
    kz_json:set_value(<<"notifications">>, Notifications, Doc).

-spec numbers(doc()) -> api_object().
-spec numbers(doc(), Default) -> kz_json:object() | Default.
numbers(Doc) ->
    numbers(Doc, 'undefined').
numbers(Doc, Default) ->
    kz_json:get_json_value(<<"numbers">>, Doc, Default).

-spec set_numbers(doc(), kz_json:object()) -> doc().
set_numbers(Doc, Numbers) ->
    kz_json:set_value(<<"numbers">>, Numbers, Doc).

-spec port_state(doc()) -> binary().
-spec port_state(doc(), Default) -> binary() | Default.
port_state(Doc) ->
    port_state(Doc, <<"unconfirmed">>).
port_state(Doc, Default) ->
    kz_json:get_binary_value(<<"port_state">>, Doc, Default).

-spec set_port_state(doc(), binary()) -> doc().
set_port_state(Doc, PortState) ->
    kz_json:set_value(<<"port_state">>, PortState, Doc).

-spec transfer_date(doc()) -> api_integer().
-spec transfer_date(doc(), Default) -> integer() | Default.
transfer_date(Doc) ->
    transfer_date(Doc, 'undefined').
transfer_date(Doc, Default) ->
    kz_json:get_integer_value(<<"transfer_date">>, Doc, Default).

-spec set_transfer_date(doc(), integer()) -> doc().
set_transfer_date(Doc, TransferDate) ->
    kz_json:set_value(<<"transfer_date">>, TransferDate, Doc).
