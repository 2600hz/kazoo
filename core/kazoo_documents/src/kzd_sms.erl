%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_sms).

-export([new/0]).
-export([body/1, body/2, set_body/2]).
-export([from/1, from/2, set_from/2]).
-export([from_user/1, from_user/2]).
-export([from_realm/1, from_realm/2]).
-export([scheduled/1, scheduled/2, set_scheduled/2]).
-export([to/1, to/2, set_to/2]).
-export([to_user/1, to_user/2]).
-export([to_realm/1, to_realm/2]).
-export([status/1, status/2]).
-export([direction/1, direction/2]).
-export([caller_id_number/1, caller_id_number/2]).
-export([callee_id_number/1, callee_id_number/2]).
-export([account_id/1, account_id/2]).
-export([route_id/1, route_id/2, set_route_id/2]).

-export([type/0, type/1]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"sms">>).
-define(TYPE, <<"sms">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec body(doc()) -> kz_term:api_ne_binary().
body(Doc) ->
    body(Doc, 'undefined').

-spec body(doc(), Default) -> kz_term:ne_binary() | Default.
body(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"body">>], Doc, Default).

-spec set_body(doc(), kz_term:ne_binary()) -> doc().
set_body(Doc, Body) ->
    kz_json:set_value([<<"body">>], Body, Doc).

-spec from(doc()) -> kz_term:api_binary().
from(Doc) ->
    from(Doc, 'undefined').

-spec from(doc(), Default) -> binary() | Default.
from(Doc, Default) ->
    kz_json:get_binary_value([<<"from">>], Doc, Default).

-spec set_from(doc(), binary()) -> doc().
set_from(Doc, From) ->
    kz_json:set_value([<<"from">>], From, Doc).

-spec scheduled(doc()) -> kz_term:api_integer().
scheduled(Doc) ->
    scheduled(Doc, 'undefined').

-spec scheduled(doc(), Default) -> integer() | Default.
scheduled(Doc, Default) ->
    kz_json:get_integer_value([<<"scheduled">>], Doc, Default).

-spec set_scheduled(doc(), integer()) -> doc().
set_scheduled(Doc, Scheduled) ->
    kz_json:set_value([<<"scheduled">>], Scheduled, Doc).

-spec to(doc()) -> kz_term:api_binary().
to(Doc) ->
    to(Doc, 'undefined').

-spec to(doc(), Default) -> binary() | Default.
to(Doc, Default) ->
    kz_json:get_binary_value([<<"to">>], Doc, Default).

-spec set_to(doc(), binary()) -> doc().
set_to(Doc, To) ->
    kz_json:set_value([<<"to">>], To, Doc).

-spec from_user(doc()) -> kz_term:api_binary().
from_user(Doc) ->
    from_user(Doc, 'undefined').

-spec from_user(doc(), Default) -> binary() | Default.
from_user(Doc, Default) ->
    kz_json:get_binary_value([<<"from_user">>], Doc, Default).

-spec from_realm(doc()) -> kz_term:api_binary().
from_realm(Doc) ->
    from_realm(Doc, 'undefined').

-spec from_realm(doc(), Default) -> binary() | Default.
from_realm(Doc, Default) ->
    kz_json:get_binary_value([<<"from_realm">>], Doc, Default).

-spec to_user(doc()) -> kz_term:api_binary().
to_user(Doc) ->
    to_user(Doc, 'undefined').

-spec to_user(doc(), Default) -> binary() | Default.
to_user(Doc, Default) ->
    kz_json:get_binary_value([<<"to_user">>], Doc, Default).

-spec to_realm(doc()) -> kz_term:api_binary().
to_realm(Doc) ->
    to_realm(Doc, 'undefined').

-spec to_realm(doc(), Default) -> binary() | Default.
to_realm(Doc, Default) ->
    kz_json:get_binary_value([<<"to_realm">>], Doc, Default).

-spec status(doc()) -> kz_term:api_binary().
status(Doc) ->
    status(Doc, 'undefined').

-spec status(doc(), Default) -> binary() | Default.
status(Doc, Default) ->
    kz_json:get_binary_value([<<"pvt_status">>], Doc, Default).

-spec direction(doc()) -> kz_term:api_binary().
direction(Doc) ->
    direction(Doc, 'undefined').

-spec direction(doc(), Default) -> binary() | Default.
direction(Doc, Default) ->
    kz_json:get_binary_value([<<"pvt_call">>, <<"Call-Direction">>], Doc, Default).

-spec caller_id_number(doc()) -> kz_term:api_binary().
caller_id_number(Doc) ->
    caller_id_number(Doc, 'undefined').

-spec caller_id_number(doc(), Default) -> binary() | Default.
caller_id_number(Doc, Default) ->
    kz_json:get_binary_value(<<"Caller-ID-Number">>, Doc, Default).

-spec callee_id_number(doc()) -> kz_term:api_binary().
callee_id_number(Doc) ->
    callee_id_number(Doc, 'undefined').

-spec callee_id_number(doc(), Default) -> binary() | Default.
callee_id_number(Doc, Default) ->
    kz_json:get_binary_value(<<"Callee-ID-Number">>, Doc, Default).

-spec account_id(doc()) -> kz_term:api_binary().
account_id(Doc) ->
    account_id(Doc, 'undefined').

-spec account_id(doc(), Default) -> binary() | Default.
account_id(Doc, Default) ->
    kz_json:get_binary_value(<<"Account-ID">>, Doc, Default).

-spec route_id(doc()) -> kz_term:api_binary().
route_id(Doc) ->
    route_id(Doc, 'undefined').

-spec route_id(doc(), Default) -> binary() | Default.
route_id(Doc, Default) ->
    kz_json:get_binary_value(<<"Route-ID">>, Doc, Default).

-spec set_route_id(doc(), kz_term:ne_binary()) -> doc().
set_route_id(Doc, RouteId) ->
    kz_json:set_value(<<"Route-ID">>, RouteId, Doc).

-spec type() -> kz_term:ne_binary().
type() -> ?TYPE.

-spec type(doc()) -> kz_term:ne_binary().
type(Doc) ->
    kz_doc:type(Doc, ?TYPE).
