-module(kzd_connectivity).

-export([new/0]).
-export([account/1, account/2, set_account/2]).
-export([account_auth_realm/1, account_auth_realm/2, set_account_auth_realm/2]).
-export([account_caller_id/1, account_caller_id/2, set_account_caller_id/2]).
-export([account_caller_id_cid_name/1, account_caller_id_cid_name/2, set_account_caller_id_cid_name/2]).
-export([account_caller_id_cid_number/1, account_caller_id_cid_number/2, set_account_caller_id_cid_number/2]).
-export([account_emergency_caller_id/1, account_emergency_caller_id/2, set_account_emergency_caller_id/2]).
-export([account_emergency_caller_id_cid_name/1, account_emergency_caller_id_cid_name/2, set_account_emergency_caller_id_cid_name/2]).
-export([account_emergency_caller_id_cid_number/1, account_emergency_caller_id_cid_number/2, set_account_emergency_caller_id_cid_number/2]).
-export([account_trunks/1, account_trunks/2, set_account_trunks/2]).
-export([name/1, name/2, set_name/2]).
-export([servers/1, servers/2, set_servers/2]).


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

-spec account_auth_realm(doc()) -> api_ne_binary().
-spec account_auth_realm(doc(), Default) -> ne_binary() | Default.
account_auth_realm(Doc) ->
    account_auth_realm(Doc, 'undefined').
account_auth_realm(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"account">>, <<"auth_realm">>], Doc, Default).

-spec set_account_auth_realm(doc(), ne_binary()) -> doc().
set_account_auth_realm(Doc, AccountAuthenticationRealm) ->
    kz_json:set_value([<<"account">>, <<"auth_realm">>], AccountAuthenticationRealm, Doc).

-spec account_caller_id(doc()) -> api_object().
-spec account_caller_id(doc(), Default) -> kz_json:object() | Default.
account_caller_id(Doc) ->
    account_caller_id(Doc, 'undefined').
account_caller_id(Doc, Default) ->
    kz_json:get_json_value([<<"account">>, <<"caller_id">>], Doc, Default).

-spec set_account_caller_id(doc(), kz_json:object()) -> doc().
set_account_caller_id(Doc, AccountCallerId) ->
    kz_json:set_value([<<"account">>, <<"caller_id">>], AccountCallerId, Doc).

-spec account_caller_id_cid_name(doc()) -> api_binary().
-spec account_caller_id_cid_name(doc(), Default) -> binary() | Default.
account_caller_id_cid_name(Doc) ->
    account_caller_id_cid_name(Doc, 'undefined').
account_caller_id_cid_name(Doc, Default) ->
    kz_json:get_binary_value([<<"account">>, <<"caller_id">>, <<"cid_name">>], Doc, Default).

-spec set_account_caller_id_cid_name(doc(), binary()) -> doc().
set_account_caller_id_cid_name(Doc, AccountCallerIdCidName) ->
    kz_json:set_value([<<"account">>, <<"caller_id">>, <<"cid_name">>], AccountCallerIdCidName, Doc).

-spec account_caller_id_cid_number(doc()) -> api_binary().
-spec account_caller_id_cid_number(doc(), Default) -> binary() | Default.
account_caller_id_cid_number(Doc) ->
    account_caller_id_cid_number(Doc, 'undefined').
account_caller_id_cid_number(Doc, Default) ->
    kz_json:get_binary_value([<<"account">>, <<"caller_id">>, <<"cid_number">>], Doc, Default).

-spec set_account_caller_id_cid_number(doc(), binary()) -> doc().
set_account_caller_id_cid_number(Doc, AccountCallerIdCidNumber) ->
    kz_json:set_value([<<"account">>, <<"caller_id">>, <<"cid_number">>], AccountCallerIdCidNumber, Doc).

-spec account_emergency_caller_id(doc()) -> api_object().
-spec account_emergency_caller_id(doc(), Default) -> kz_json:object() | Default.
account_emergency_caller_id(Doc) ->
    account_emergency_caller_id(Doc, 'undefined').
account_emergency_caller_id(Doc, Default) ->
    kz_json:get_json_value([<<"account">>, <<"emergency_caller_id">>], Doc, Default).

-spec set_account_emergency_caller_id(doc(), kz_json:object()) -> doc().
set_account_emergency_caller_id(Doc, AccountEmergencyCallerId) ->
    kz_json:set_value([<<"account">>, <<"emergency_caller_id">>], AccountEmergencyCallerId, Doc).

-spec account_emergency_caller_id_cid_name(doc()) -> api_binary().
-spec account_emergency_caller_id_cid_name(doc(), Default) -> binary() | Default.
account_emergency_caller_id_cid_name(Doc) ->
    account_emergency_caller_id_cid_name(Doc, 'undefined').
account_emergency_caller_id_cid_name(Doc, Default) ->
    kz_json:get_binary_value([<<"account">>, <<"emergency_caller_id">>, <<"cid_name">>], Doc, Default).

-spec set_account_emergency_caller_id_cid_name(doc(), binary()) -> doc().
set_account_emergency_caller_id_cid_name(Doc, AccountEmergencyCallerIdCidName) ->
    kz_json:set_value([<<"account">>, <<"emergency_caller_id">>, <<"cid_name">>], AccountEmergencyCallerIdCidName, Doc).

-spec account_emergency_caller_id_cid_number(doc()) -> api_binary().
-spec account_emergency_caller_id_cid_number(doc(), Default) -> binary() | Default.
account_emergency_caller_id_cid_number(Doc) ->
    account_emergency_caller_id_cid_number(Doc, 'undefined').
account_emergency_caller_id_cid_number(Doc, Default) ->
    kz_json:get_binary_value([<<"account">>, <<"emergency_caller_id">>, <<"cid_number">>], Doc, Default).

-spec set_account_emergency_caller_id_cid_number(doc(), binary()) -> doc().
set_account_emergency_caller_id_cid_number(Doc, AccountEmergencyCallerIdCidNumber) ->
    kz_json:set_value([<<"account">>, <<"emergency_caller_id">>, <<"cid_number">>], AccountEmergencyCallerIdCidNumber, Doc).

-spec account_trunks(doc()) -> api_integer().
-spec account_trunks(doc(), Default) -> integer() | Default.
account_trunks(Doc) ->
    account_trunks(Doc, 'undefined').
account_trunks(Doc, Default) ->
    kz_json:get_integer_value([<<"account">>, <<"trunks">>], Doc, Default).

-spec set_account_trunks(doc(), integer()) -> doc().
set_account_trunks(Doc, AccountTrunks) ->
    kz_json:set_value([<<"account">>, <<"trunks">>], AccountTrunks, Doc).

-spec name(doc()) -> api_binary().
-spec name(doc(), Default) -> binary() | Default.
name(Doc) ->
    name(Doc, 'undefined').
name(Doc, Default) ->
    kz_json:get_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

-spec servers(doc()) -> kz_json:objects().
-spec servers(doc(), Default) -> kz_json:objects() | Default.
servers(Doc) ->
    servers(Doc, []).
servers(Doc, Default) ->
    kz_json:get_list_value([<<"servers">>], Doc, Default).

-spec set_servers(doc(), kz_json:objects()) -> doc().
set_servers(Doc, Servers) ->
    kz_json:set_value([<<"servers">>], Servers, Doc).
