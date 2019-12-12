%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_api_sms).

-export([new/0]).
-export([body/1, body/2, set_body/2]).
-export([from/1, from/2, set_from/2]).
-export([from_user/1, from_user/2]).
-export([from_realm/1, from_realm/2]).
-export([scheduled/1, scheduled/2, set_scheduled/2]).
-export([to/1, to/2, set_to/2]).
-export([to_user/1, to_user/2]).
-export([to_realm/1, to_realm/2]).
-export([caller_id_number/1, caller_id_number/2]).
-export([callee_id_number/1, callee_id_number/2]).
-export([account_id/1, account_id/2, set_account_id/2]).
-export([application_id/1, application_id/2]).
-export([route_id/1, route_id/2, set_route_id/2]).
-export([route_type/1, route_type/2, set_route_type/2]).
-export([message_id/1, message_id/2, set_message_id/2]).
-export([exchange_id/1, exchange_id/2, set_exchange_id/2]).

-export([originator_properties/1
        ,set_originator_properties/2
        ,originator_property/2
        ,set_originator_property/3
        ,remove_originator_property/2
        ,originator_flags/1
        ,set_originator_flags/2
        ,set_originator_flag/2
        ,remove_originator_flag/2
        ]).

-export([type/0, type/1]).

-include("kz_documents.hrl").

-type payload() :: kz_json:object().
-export_type([payload/0]).

-define(SCHEMA, <<"sms">>).
-define(TYPE, <<"sms">>).

-spec new() -> payload().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec body(payload()) -> kz_term:api_ne_binary().
body(Payload) ->
    body(Payload, 'undefined').

-spec body(payload(), Default) -> kz_term:ne_binary() | Default.
body(Payload, Default) ->
    kz_json:get_ne_binary_value([<<"Body">>], Payload, Default).

-spec set_body(payload(), kz_term:ne_binary()) -> payload().
set_body(Payload, Body) ->
    kz_json:set_value([<<"Body">>], Body, Payload).

-spec from(payload()) -> kz_term:api_binary().
from(Payload) ->
    from(Payload, 'undefined').

-spec from(payload(), Default) -> binary() | Default.
from(Payload, Default) ->
    kz_json:get_binary_value([<<"From">>], Payload, Default).

-spec set_from(payload(), binary()) -> payload().
set_from(Payload, From) ->
    kz_json:set_value([<<"From">>], From, Payload).

-spec scheduled(payload()) -> kz_term:api_integer().
scheduled(Payload) ->
    scheduled(Payload, 'undefined').

-spec scheduled(payload(), Default) -> integer() | Default.
scheduled(Payload, Default) ->
    kz_json:get_integer_value([<<"Scheduled">>], Payload, Default).

-spec set_scheduled(payload(), integer()) -> payload().
set_scheduled(Payload, Scheduled) ->
    kz_json:set_value([<<"Scheduled">>], Scheduled, Payload).

-spec to(payload()) -> kz_term:api_binary().
to(Payload) ->
    to(Payload, 'undefined').

-spec to(payload(), Default) -> binary() | Default.
to(Payload, Default) ->
    kz_json:get_binary_value([<<"To">>], Payload, Default).

-spec set_to(payload(), binary()) -> payload().
set_to(Payload, To) ->
    kz_json:set_value([<<"To">>], To, Payload).

-spec from_user(payload()) -> kz_term:api_binary().
from_user(Payload) ->
    from_user(Payload, 'undefined').

-spec from_user(payload(), Default) -> binary() | Default.
from_user(Payload, Default) ->
    kz_json:get_binary_value([<<"From-User">>], Payload, Default).

-spec from_realm(payload()) -> kz_term:api_binary().
from_realm(Payload) ->
    from_realm(Payload, 'undefined').

-spec from_realm(payload(), Default) -> binary() | Default.
from_realm(Payload, Default) ->
    kz_json:get_binary_value([<<"From-Realm">>], Payload, Default).

-spec to_user(payload()) -> kz_term:api_binary().
to_user(Payload) ->
    to_user(Payload, 'undefined').

-spec to_user(payload(), Default) -> binary() | Default.
to_user(Payload, Default) ->
    kz_json:get_binary_value([<<"To-User">>], Payload, Default).

-spec to_realm(payload()) -> kz_term:api_binary().
to_realm(Payload) ->
    to_realm(Payload, 'undefined').

-spec to_realm(payload(), Default) -> binary() | Default.
to_realm(Payload, Default) ->
    kz_json:get_binary_value([<<"To-Realm">>], Payload, Default).

-spec caller_id_number(payload()) -> kz_term:api_binary().
caller_id_number(Payload) ->
    caller_id_number(Payload, 'undefined').

-spec caller_id_number(payload(), Default) -> binary() | Default.
caller_id_number(Payload, Default) ->
    kz_json:get_binary_value(<<"Caller-ID-Number">>, Payload, Default).

-spec callee_id_number(payload()) -> kz_term:api_binary().
callee_id_number(Payload) ->
    callee_id_number(Payload, 'undefined').

-spec callee_id_number(payload(), Default) -> binary() | Default.
callee_id_number(Payload, Default) ->
    kz_json:get_binary_value(<<"Callee-ID-Number">>, Payload, Default).

-spec account_id(payload()) -> kz_term:api_binary().
account_id(Payload) ->
    account_id(Payload, 'undefined').

-spec account_id(payload(), Default) -> binary() | Default.
account_id(Payload, Default) ->
    kz_json:get_binary_value(<<"Account-ID">>, Payload, Default).

-spec set_account_id(payload(), kz_term:ne_binary()) -> payload().
set_account_id(Payload, AccountId) ->
    kz_json:set_value(<<"Account-ID">>, AccountId, Payload).

-spec application_id(payload()) -> kz_term:api_binary().
application_id(Payload) ->
    application_id(Payload, <<"sms">>).

-spec application_id(payload(), Default) -> binary() | Default.
application_id(Payload, Default) ->
    kz_json:get_binary_value(<<"Application-ID">>, Payload, Default).

-spec route_id(payload()) -> kz_term:api_binary().
route_id(Payload) ->
    route_id(Payload, 'undefined').

-spec route_id(payload(), Default) -> binary() | Default.
route_id(Payload, Default) ->
    kz_json:get_binary_value(<<"Route-ID">>, Payload, Default).

-spec set_route_id(payload(), kz_term:ne_binary()) -> payload().
set_route_id(Payload, RouteId) ->
    kz_json:set_value(<<"Route-ID">>, RouteId, Payload).

-spec route_type(payload()) -> kz_term:api_binary().
route_type(Payload) ->
    route_type(Payload, 'undefined').

-spec route_type(payload(), Default) -> binary() | Default.
route_type(Payload, Default) ->
    kz_json:get_binary_value(<<"Route-Type">>, Payload, Default).

-spec set_route_type(payload(), kz_term:ne_binary()) -> payload().
set_route_type(Payload, RouteType) ->
    kz_json:set_value(<<"Route-Type">>, RouteType, Payload).

-spec type() -> kz_term:ne_binary().
type() -> ?TYPE.

-spec type(payload()) -> kz_term:ne_binary().
type(Payload) ->
    kz_doc:type(Payload, ?TYPE).

-spec originator_properties(payload()) -> payload().
originator_properties(Payload) ->
    kz_json:get_json_value(<<"Originator-Properties">>, Payload, kz_json:new()).

-spec set_originator_properties(payload(), kz_json:object()) -> payload().
set_originator_properties(Payload, JObj) ->
    kz_json:set_value(<<"Originator-Properties">>, JObj, Payload).

-spec originator_property(payload(), kz_term:ne_binary()) -> json_term().
originator_property(Payload, Key) ->
    kz_json:get_value(Key, originator_properties(Payload)).

-spec set_originator_property(payload(), kz_term:ne_binary(), json_term()) -> payload().
set_originator_property(Payload, Key, Value) ->
    set_originator_properties(Payload, kz_json:set_value(Key, Value, originator_properties(Payload))).

-spec remove_originator_property(payload(), kz_term:ne_binary()) -> payload().
remove_originator_property(Payload, Key) ->
    JObj = kz_json:set_value(Key, null, originator_properties(Payload)),
    case kz_json:is_empty(JObj) of
        'true' -> kz_json:set_value(<<"Originator-Properties">>, null, Payload);
        'false' -> set_originator_properties(Payload, JObj)
    end.

-spec originator_flags(payload()) -> kz_term:ne_binaries().
originator_flags(Payload) ->
    kz_json:get_list_value(<<"Originator-Flags">>, Payload, []).

-spec set_originator_flags(payload(), kz_term:ne_binaries()) -> payload().
set_originator_flags(Payload, Flags) ->
    kz_json:set_value(<<"Originator-Flags">>, Flags, Payload).

-spec set_originator_flag(payload(), kz_term:ne_binary()) -> payload().
set_originator_flag(Payload, Flag) ->
    set_originator_flags(Payload, [Flag | originator_flags(Payload)]).

-spec remove_originator_flag(payload(), kz_term:ne_binary()) -> payload().
remove_originator_flag(Payload, Flag) ->
    set_originator_flags(Payload, lists:filter(fun(F) -> F =/= Flag end, originator_flags(Payload))).

-spec message_id(payload()) -> kz_term:api_binary().
message_id(Payload) ->
    message_id(Payload, 'undefined').

-spec message_id(payload(), Default) -> binary() | Default.
message_id(Payload, Default) ->
    kz_json:get_binary_value(<<"Message-ID">>, Payload, Default).

-spec set_message_id(payload(), kz_term:ne_binary()) -> payload().
set_message_id(Payload, MessageId) ->
    kz_json:set_value(<<"Message-ID">>, MessageId, Payload).

-spec exchange_id(payload()) -> kz_term:api_binary().
exchange_id(Payload) ->
    exchange_id(Payload, 'undefined').

-spec exchange_id(payload(), Default) -> binary() | Default.
exchange_id(Payload, Default) ->
    kz_json:get_binary_value(<<"Exchange-ID">>, Payload, Default).

-spec set_exchange_id(payload(), kz_term:ne_binary()) -> payload().
set_exchange_id(Payload, ExhangeId) ->
    kz_json:set_value(<<"Exchange-ID">>, ExhangeId, Payload).
