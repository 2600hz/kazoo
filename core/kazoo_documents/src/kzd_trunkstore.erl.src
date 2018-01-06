-module(kzd_trunkstore).

-export([new/0]).
-export([account/1, account/2, set_account/2]).
-export([account_auth_realm/1, account_auth_realm/2, set_account_auth_realm/2]).
-export([account_caller_id/1, account_caller_id/2, set_account_caller_id/2]).
-export([accountcaller_id_cid_name/1, accountcaller_id_cid_name/2, set_accountcaller_id_cid_name/2]).
-export([accountcaller_id_cid_number/1, accountcaller_id_cid_number/2, set_accountcaller_id_cid_number/2]).
-export([account_credits/1, account_credits/2, set_account_credits/2]).
-export([accountcredits_prepay/1, accountcredits_prepay/2, set_accountcredits_prepay/2]).
-export([account_emergency_caller_id/1, account_emergency_caller_id/2, set_account_emergency_caller_id/2]).
-export([accountemergency_caller_id_cid_name/1, accountemergency_caller_id_cid_name/2, set_accountemergency_caller_id_cid_name/2]).
-export([accountemergency_caller_id_cid_number/1, accountemergency_caller_id_cid_number/2, set_accountemergency_caller_id_cid_number/2]).
-export([account_trunks/1, account_trunks/2, set_account_trunks/2]).
-export([call_restriction/1, call_restriction/2, set_call_restriction/2]).
-export([name/1, name/2, set_name/2]).
-export([servers/1, servers/2, set_servers/2]).
-export([type/1, type/2, set_type/2]).


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

-spec accountcaller_id_cid_name(doc()) -> api_ne_binary().
-spec accountcaller_id_cid_name(doc(), Default) -> ne_binary() | Default.
accountcaller_id_cid_name(Doc) ->
    accountcaller_id_cid_name(Doc, 'undefined').
accountcaller_id_cid_name(Doc, Default) ->
    kz_json:get_ne_binary_value([[<<"account">>, <<"caller_id">>], <<"cid_name">>], Doc, Default).

-spec set_accountcaller_id_cid_name(doc(), ne_binary()) -> doc().
set_accountcaller_id_cid_name(Doc, AccountcallerIdCidName) ->
    kz_json:set_value([[<<"account">>, <<"caller_id">>], <<"cid_name">>], AccountcallerIdCidName, Doc).

-spec accountcaller_id_cid_number(doc()) -> api_ne_binary().
-spec accountcaller_id_cid_number(doc(), Default) -> ne_binary() | Default.
accountcaller_id_cid_number(Doc) ->
    accountcaller_id_cid_number(Doc, 'undefined').
accountcaller_id_cid_number(Doc, Default) ->
    kz_json:get_ne_binary_value([[<<"account">>, <<"caller_id">>], <<"cid_number">>], Doc, Default).

-spec set_accountcaller_id_cid_number(doc(), ne_binary()) -> doc().
set_accountcaller_id_cid_number(Doc, AccountcallerIdCidNumber) ->
    kz_json:set_value([[<<"account">>, <<"caller_id">>], <<"cid_number">>], AccountcallerIdCidNumber, Doc).

-spec account_credits(doc()) -> api_object().
-spec account_credits(doc(), Default) -> kz_json:object() | Default.
account_credits(Doc) ->
    account_credits(Doc, 'undefined').
account_credits(Doc, Default) ->
    kz_json:get_json_value([<<"account">>, <<"credits">>], Doc, Default).

-spec set_account_credits(doc(), kz_json:object()) -> doc().
set_account_credits(Doc, AccountCredits) ->
    kz_json:set_value([<<"account">>, <<"credits">>], AccountCredits, Doc).

-spec accountcredits_prepay(doc()) -> any().
-spec accountcredits_prepay(doc(), Default) -> any() | Default.
accountcredits_prepay(Doc) ->
    accountcredits_prepay(Doc, 'undefined').
accountcredits_prepay(Doc, Default) ->
    kz_json:get_value([[<<"account">>, <<"credits">>], <<"prepay">>], Doc, Default).

-spec set_accountcredits_prepay(doc(), any()) -> doc().
set_accountcredits_prepay(Doc, AccountcreditsPrepay) ->
    kz_json:set_value([[<<"account">>, <<"credits">>], <<"prepay">>], AccountcreditsPrepay, Doc).

-spec account_emergency_caller_id(doc()) -> api_object().
-spec account_emergency_caller_id(doc(), Default) -> kz_json:object() | Default.
account_emergency_caller_id(Doc) ->
    account_emergency_caller_id(Doc, 'undefined').
account_emergency_caller_id(Doc, Default) ->
    kz_json:get_json_value([<<"account">>, <<"emergency_caller_id">>], Doc, Default).

-spec set_account_emergency_caller_id(doc(), kz_json:object()) -> doc().
set_account_emergency_caller_id(Doc, AccountEmergencyCallerId) ->
    kz_json:set_value([<<"account">>, <<"emergency_caller_id">>], AccountEmergencyCallerId, Doc).

-spec accountemergency_caller_id_cid_name(doc()) -> api_ne_binary().
-spec accountemergency_caller_id_cid_name(doc(), Default) -> ne_binary() | Default.
accountemergency_caller_id_cid_name(Doc) ->
    accountemergency_caller_id_cid_name(Doc, 'undefined').
accountemergency_caller_id_cid_name(Doc, Default) ->
    kz_json:get_ne_binary_value([[<<"account">>, <<"emergency_caller_id">>], <<"cid_name">>], Doc, Default).

-spec set_accountemergency_caller_id_cid_name(doc(), ne_binary()) -> doc().
set_accountemergency_caller_id_cid_name(Doc, AccountemergencyCallerIdCidName) ->
    kz_json:set_value([[<<"account">>, <<"emergency_caller_id">>], <<"cid_name">>], AccountemergencyCallerIdCidName, Doc).

-spec accountemergency_caller_id_cid_number(doc()) -> api_ne_binary().
-spec accountemergency_caller_id_cid_number(doc(), Default) -> ne_binary() | Default.
accountemergency_caller_id_cid_number(Doc) ->
    accountemergency_caller_id_cid_number(Doc, 'undefined').
accountemergency_caller_id_cid_number(Doc, Default) ->
    kz_json:get_ne_binary_value([[<<"account">>, <<"emergency_caller_id">>], <<"cid_number">>], Doc, Default).

-spec set_accountemergency_caller_id_cid_number(doc(), ne_binary()) -> doc().
set_accountemergency_caller_id_cid_number(Doc, AccountemergencyCallerIdCidNumber) ->
    kz_json:set_value([[<<"account">>, <<"emergency_caller_id">>], <<"cid_number">>], AccountemergencyCallerIdCidNumber, Doc).

-spec account_trunks(doc()) -> api_integer().
-spec account_trunks(doc(), Default) -> integer() | Default.
account_trunks(Doc) ->
    account_trunks(Doc, 'undefined').
account_trunks(Doc, Default) ->
    kz_json:get_integer_value([<<"account">>, <<"trunks">>], Doc, Default).

-spec set_account_trunks(doc(), integer()) -> doc().
set_account_trunks(Doc, AccountTrunks) ->
    kz_json:set_value([<<"account">>, <<"trunks">>], AccountTrunks, Doc).

-spec call_restriction(doc()) -> kz_json:object().
-spec call_restriction(doc(), Default) -> kz_json:object() | Default.
call_restriction(Doc) ->
    call_restriction(Doc, kz_json:new()).
call_restriction(Doc, Default) ->
    kz_json:get_json_value(<<"call_restriction">>, Doc, Default).

-spec set_call_restriction(doc(), kz_json:object()) -> doc().
set_call_restriction(Doc, CallRestriction) ->
    kz_json:set_value(<<"call_restriction">>, CallRestriction, Doc).

-spec name(doc()) -> api_binary().
-spec name(doc(), Default) -> binary() | Default.
name(Doc) ->
    name(Doc, 'undefined').
name(Doc, Default) ->
    kz_json:get_binary_value(<<"name">>, Doc, Default).

-spec set_name(doc(), binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value(<<"name">>, Name, Doc).

-spec servers(doc()) -> list().
-spec servers(doc(), Default) -> list() | Default.
servers(Doc) ->
    servers(Doc, []).
servers(Doc, Default) ->
    kz_json:get_list_value(<<"servers">>, Doc, Default).

-spec set_servers(doc(), list()) -> doc().
set_servers(Doc, Servers) ->
    kz_json:set_value(<<"servers">>, Servers, Doc).

-spec type(doc()) -> api_binary().
-spec type(doc(), Default) -> binary() | Default.
type(Doc) ->
    type(Doc, 'undefined').
type(Doc, Default) ->
    kz_json:get_binary_value(<<"type">>, Doc, Default).

-spec set_type(doc(), binary()) -> doc().
set_type(Doc, Type) ->
    kz_json:set_value(<<"type">>, Type, Doc).
