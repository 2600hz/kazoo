-module(kzd_user_auth).

-export([new/0]).
-export([account_name/1, account_name/2, set_account_name/2]).
-export([account_realm/1, account_realm/2, set_account_realm/2]).
-export([credentials/1, credentials/2, set_credentials/2]).
-export([method/1, method/2, set_method/2]).
-export([phone_number/1, phone_number/2, set_phone_number/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec account_name(doc()) -> api_ne_binary().
-spec account_name(doc(), Default) -> ne_binary() | Default.
account_name(Doc) ->
    account_name(Doc, 'undefined').
account_name(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"account_name">>, Doc, Default).

-spec set_account_name(doc(), ne_binary()) -> doc().
set_account_name(Doc, AccountName) ->
    kz_json:set_value(<<"account_name">>, AccountName, Doc).

-spec account_realm(doc()) -> api_ne_binary().
-spec account_realm(doc(), Default) -> ne_binary() | Default.
account_realm(Doc) ->
    account_realm(Doc, 'undefined').
account_realm(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"account_realm">>, Doc, Default).

-spec set_account_realm(doc(), ne_binary()) -> doc().
set_account_realm(Doc, AccountRealm) ->
    kz_json:set_value(<<"account_realm">>, AccountRealm, Doc).

-spec credentials(doc()) -> api_ne_binary().
-spec credentials(doc(), Default) -> ne_binary() | Default.
credentials(Doc) ->
    credentials(Doc, 'undefined').
credentials(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"credentials">>, Doc, Default).

-spec set_credentials(doc(), ne_binary()) -> doc().
set_credentials(Doc, Credentials) ->
    kz_json:set_value(<<"credentials">>, Credentials, Doc).

-spec method(doc()) -> binary().
-spec method(doc(), Default) -> binary() | Default.
method(Doc) ->
    method(Doc, <<"md5">>).
method(Doc, Default) ->
    kz_json:get_binary_value(<<"method">>, Doc, Default).

-spec set_method(doc(), binary()) -> doc().
set_method(Doc, Method) ->
    kz_json:set_value(<<"method">>, Method, Doc).

-spec phone_number(doc()) -> api_ne_binary().
-spec phone_number(doc(), Default) -> ne_binary() | Default.
phone_number(Doc) ->
    phone_number(Doc, 'undefined').
phone_number(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"phone_number">>, Doc, Default).

-spec set_phone_number(doc(), ne_binary()) -> doc().
set_phone_number(Doc, PhoneNumber) ->
    kz_json:set_value(<<"phone_number">>, PhoneNumber, Doc).
