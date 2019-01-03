%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_clicktocall).

-export([new/0]).
-export([auth_required/1, auth_required/2, set_auth_required/2]).
-export([caller_id_number/1, caller_id_number/2, set_caller_id_number/2]).
-export([dial_first/1, dial_first/2, set_dial_first/2]).
-export([extension/1, extension/2, set_extension/2]).
-export([name/1, name/2, set_name/2]).
-export([outbound_callee_id_name/1, outbound_callee_id_name/2, set_outbound_callee_id_name/2]).
-export([outbound_callee_id_number/1, outbound_callee_id_number/2, set_outbound_callee_id_number/2]).
-export([throttle/1, throttle/2, set_throttle/2]).
-export([whitelist/1, whitelist/2, set_whitelist/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"clicktocall">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec auth_required(doc()) -> boolean().
auth_required(Doc) ->
    auth_required(Doc, true).

-spec auth_required(doc(), Default) -> boolean() | Default.
auth_required(Doc, Default) ->
    kz_json:get_boolean_value([<<"auth_required">>], Doc, Default).

-spec set_auth_required(doc(), boolean()) -> doc().
set_auth_required(Doc, AuthRequired) ->
    kz_json:set_value([<<"auth_required">>], AuthRequired, Doc).

-spec caller_id_number(doc()) -> kz_term:api_binary().
caller_id_number(Doc) ->
    caller_id_number(Doc, 'undefined').

-spec caller_id_number(doc(), Default) -> binary() | Default.
caller_id_number(Doc, Default) ->
    kz_json:get_binary_value([<<"caller_id_number">>], Doc, Default).

-spec set_caller_id_number(doc(), binary()) -> doc().
set_caller_id_number(Doc, CallerIdNumber) ->
    kz_json:set_value([<<"caller_id_number">>], CallerIdNumber, Doc).

-spec dial_first(doc()) -> kz_term:api_binary().
dial_first(Doc) ->
    dial_first(Doc, 'undefined').

-spec dial_first(doc(), Default) -> binary() | Default.
dial_first(Doc, Default) ->
    kz_json:get_binary_value([<<"dial_first">>], Doc, Default).

-spec set_dial_first(doc(), binary()) -> doc().
set_dial_first(Doc, DialFirst) ->
    kz_json:set_value([<<"dial_first">>], DialFirst, Doc).

-spec extension(doc()) -> kz_term:api_binary().
extension(Doc) ->
    extension(Doc, 'undefined').

-spec extension(doc(), Default) -> binary() | Default.
extension(Doc, Default) ->
    kz_json:get_binary_value([<<"extension">>], Doc, Default).

-spec set_extension(doc(), binary()) -> doc().
set_extension(Doc, Extension) ->
    kz_json:set_value([<<"extension">>], Extension, Doc).

-spec name(doc()) -> kz_term:api_ne_binary().
name(Doc) ->
    name(Doc, 'undefined').

-spec name(doc(), Default) -> kz_term:ne_binary() | Default.
name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), kz_term:ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

-spec outbound_callee_id_name(doc()) -> kz_term:api_binary().
outbound_callee_id_name(Doc) ->
    outbound_callee_id_name(Doc, 'undefined').

-spec outbound_callee_id_name(doc(), Default) -> binary() | Default.
outbound_callee_id_name(Doc, Default) ->
    kz_json:get_binary_value([<<"outbound_callee_id_name">>], Doc, Default).

-spec set_outbound_callee_id_name(doc(), binary()) -> doc().
set_outbound_callee_id_name(Doc, OutboundCalleeIdName) ->
    kz_json:set_value([<<"outbound_callee_id_name">>], OutboundCalleeIdName, Doc).

-spec outbound_callee_id_number(doc()) -> kz_term:api_binary().
outbound_callee_id_number(Doc) ->
    outbound_callee_id_number(Doc, 'undefined').

-spec outbound_callee_id_number(doc(), Default) -> binary() | Default.
outbound_callee_id_number(Doc, Default) ->
    kz_json:get_binary_value([<<"outbound_callee_id_number">>], Doc, Default).

-spec set_outbound_callee_id_number(doc(), binary()) -> doc().
set_outbound_callee_id_number(Doc, OutboundCalleeIdNumber) ->
    kz_json:set_value([<<"outbound_callee_id_number">>], OutboundCalleeIdNumber, Doc).

-spec throttle(doc()) -> kz_term:api_integer().
throttle(Doc) ->
    throttle(Doc, 'undefined').

-spec throttle(doc(), Default) -> integer() | Default.
throttle(Doc, Default) ->
    kz_json:get_integer_value([<<"throttle">>], Doc, Default).

-spec set_throttle(doc(), integer()) -> doc().
set_throttle(Doc, Throttle) ->
    kz_json:set_value([<<"throttle">>], Throttle, Doc).

-spec whitelist(doc()) -> kz_term:api_ne_binaries().
whitelist(Doc) ->
    whitelist(Doc, 'undefined').

-spec whitelist(doc(), Default) -> kz_term:ne_binaries() | Default.
whitelist(Doc, Default) ->
    kz_json:get_list_value([<<"whitelist">>], Doc, Default).

-spec set_whitelist(doc(), kz_term:ne_binaries()) -> doc().
set_whitelist(Doc, Whitelist) ->
    kz_json:set_value([<<"whitelist">>], Whitelist, Doc).
