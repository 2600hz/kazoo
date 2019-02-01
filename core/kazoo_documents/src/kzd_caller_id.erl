%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_caller_id).

-export([new/0]).
-export([asserted/1, asserted/2, set_asserted/2]).
-export([asserted_name/1, asserted_name/2, set_asserted_name/2]).
-export([asserted_number/1, asserted_number/2, set_asserted_number/2]).
-export([asserted_realm/1, asserted_realm/2, set_asserted_realm/2]).
-export([emergency/1, emergency/2, set_emergency/2]).
-export([emergency_name/1, emergency_name/2, set_emergency_name/2]).
-export([emergency_number/1, emergency_number/2, set_emergency_number/2]).
-export([external/1, external/2, set_external/2]).
-export([external_name/1, external_name/2, set_external_name/2]).
-export([external_number/1, external_number/2, set_external_number/2]).
-export([internal/1, internal/2, set_internal/2]).
-export([internal_name/1, internal_name/2, set_internal_name/2]).
-export([internal_number/1, internal_number/2, set_internal_number/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"caller_id">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec asserted(doc()) -> kz_term:api_object().
asserted(Doc) ->
    asserted(Doc, 'undefined').

-spec asserted(doc(), Default) -> kz_json:object() | Default.
asserted(Doc, Default) ->
    kz_json:get_json_value([<<"asserted">>], Doc, Default).

-spec set_asserted(doc(), kz_json:object()) -> doc().
set_asserted(Doc, Asserted) ->
    kz_json:set_value([<<"asserted">>], Asserted, Doc).

-spec asserted_name(doc()) -> kz_term:api_binary().
asserted_name(Doc) ->
    asserted_name(Doc, 'undefined').

-spec asserted_name(doc(), Default) -> binary() | Default.
asserted_name(Doc, Default) ->
    kz_json:get_binary_value([<<"asserted">>, <<"name">>], Doc, Default).

-spec set_asserted_name(doc(), binary()) -> doc().
set_asserted_name(Doc, AssertedName) ->
    kz_json:set_value([<<"asserted">>, <<"name">>], AssertedName, Doc).

-spec asserted_number(doc()) -> kz_term:api_binary().
asserted_number(Doc) ->
    asserted_number(Doc, 'undefined').

-spec asserted_number(doc(), Default) -> binary() | Default.
asserted_number(Doc, Default) ->
    kz_json:get_binary_value([<<"asserted">>, <<"number">>], Doc, Default).

-spec set_asserted_number(doc(), binary()) -> doc().
set_asserted_number(Doc, AssertedNumber) ->
    kz_json:set_value([<<"asserted">>, <<"number">>], AssertedNumber, Doc).

-spec asserted_realm(doc()) -> kz_term:api_binary().
asserted_realm(Doc) ->
    asserted_realm(Doc, 'undefined').

-spec asserted_realm(doc(), Default) -> binary() | Default.
asserted_realm(Doc, Default) ->
    kz_json:get_binary_value([<<"asserted">>, <<"realm">>], Doc, Default).

-spec set_asserted_realm(doc(), binary()) -> doc().
set_asserted_realm(Doc, AssertedRealm) ->
    kz_json:set_value([<<"asserted">>, <<"realm">>], AssertedRealm, Doc).

-spec emergency(doc()) -> kz_term:api_object().
emergency(Doc) ->
    emergency(Doc, 'undefined').

-spec emergency(doc(), Default) -> kz_json:object() | Default.
emergency(Doc, Default) ->
    kz_json:get_json_value([<<"emergency">>], Doc, Default).

-spec set_emergency(doc(), kz_json:object()) -> doc().
set_emergency(Doc, Emergency) ->
    kz_json:set_value([<<"emergency">>], Emergency, Doc).

-spec emergency_name(doc()) -> kz_term:api_binary().
emergency_name(Doc) ->
    emergency_name(Doc, 'undefined').

-spec emergency_name(doc(), Default) -> binary() | Default.
emergency_name(Doc, Default) ->
    kz_json:get_binary_value([<<"emergency">>, <<"name">>], Doc, Default).

-spec set_emergency_name(doc(), binary()) -> doc().
set_emergency_name(Doc, EmergencyName) ->
    kz_json:set_value([<<"emergency">>, <<"name">>], EmergencyName, Doc).

-spec emergency_number(doc()) -> kz_term:api_binary().
emergency_number(Doc) ->
    emergency_number(Doc, 'undefined').

-spec emergency_number(doc(), Default) -> binary() | Default.
emergency_number(Doc, Default) ->
    kz_json:get_binary_value([<<"emergency">>, <<"number">>], Doc, Default).

-spec set_emergency_number(doc(), binary()) -> doc().
set_emergency_number(Doc, EmergencyNumber) ->
    kz_json:set_value([<<"emergency">>, <<"number">>], EmergencyNumber, Doc).

-spec external(doc()) -> kz_term:api_object().
external(Doc) ->
    external(Doc, 'undefined').

-spec external(doc(), Default) -> kz_json:object() | Default.
external(Doc, Default) ->
    kz_json:get_json_value([<<"external">>], Doc, Default).

-spec set_external(doc(), kz_json:object()) -> doc().
set_external(Doc, External) ->
    kz_json:set_value([<<"external">>], External, Doc).

-spec external_name(doc()) -> kz_term:api_binary().
external_name(Doc) ->
    external_name(Doc, 'undefined').

-spec external_name(doc(), Default) -> binary() | Default.
external_name(Doc, Default) ->
    kz_json:get_binary_value([<<"external">>, <<"name">>], Doc, Default).

-spec set_external_name(doc(), binary()) -> doc().
set_external_name(Doc, ExternalName) ->
    kz_json:set_value([<<"external">>, <<"name">>], ExternalName, Doc).

-spec external_number(doc()) -> kz_term:api_binary().
external_number(Doc) ->
    external_number(Doc, 'undefined').

-spec external_number(doc(), Default) -> binary() | Default.
external_number(Doc, Default) ->
    kz_json:get_binary_value([<<"external">>, <<"number">>], Doc, Default).

-spec set_external_number(doc(), binary()) -> doc().
set_external_number(Doc, ExternalNumber) ->
    kz_json:set_value([<<"external">>, <<"number">>], ExternalNumber, Doc).

-spec internal(doc()) -> kz_term:api_object().
internal(Doc) ->
    internal(Doc, 'undefined').

-spec internal(doc(), Default) -> kz_json:object() | Default.
internal(Doc, Default) ->
    kz_json:get_json_value([<<"internal">>], Doc, Default).

-spec set_internal(doc(), kz_json:object()) -> doc().
set_internal(Doc, Internal) ->
    kz_json:set_value([<<"internal">>], Internal, Doc).

-spec internal_name(doc()) -> kz_term:api_binary().
internal_name(Doc) ->
    internal_name(Doc, 'undefined').

-spec internal_name(doc(), Default) -> binary() | Default.
internal_name(Doc, Default) ->
    kz_json:get_binary_value([<<"internal">>, <<"name">>], Doc, Default).

-spec set_internal_name(doc(), binary()) -> doc().
set_internal_name(Doc, InternalName) ->
    kz_json:set_value([<<"internal">>, <<"name">>], InternalName, Doc).

-spec internal_number(doc()) -> kz_term:api_binary().
internal_number(Doc) ->
    internal_number(Doc, 'undefined').

-spec internal_number(doc(), Default) -> binary() | Default.
internal_number(Doc, Default) ->
    kz_json:get_binary_value([<<"internal">>, <<"number">>], Doc, Default).

-spec set_internal_number(doc(), binary()) -> doc().
set_internal_number(Doc, InternalNumber) ->
    kz_json:set_value([<<"internal">>, <<"number">>], InternalNumber, Doc).
