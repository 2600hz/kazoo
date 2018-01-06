-module(kzd_user_auth_recovery).

-export([new/0]).
-export([account_name/1, account_name/2, set_account_name/2]).
-export([account_realm/1, account_realm/2, set_account_realm/2]).
-export([phone_number/1, phone_number/2, set_phone_number/2]).
-export([ui_url/1, ui_url/2, set_ui_url/2]).
-export([username/1, username/2, set_username/2]).


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
    kz_json:get_ne_binary_value([<<"account_name">>], Doc, Default).

-spec set_account_name(doc(), ne_binary()) -> doc().
set_account_name(Doc, AccountName) ->
    kz_json:set_value([<<"account_name">>], AccountName, Doc).

-spec account_realm(doc()) -> api_ne_binary().
-spec account_realm(doc(), Default) -> ne_binary() | Default.
account_realm(Doc) ->
    account_realm(Doc, 'undefined').
account_realm(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"account_realm">>], Doc, Default).

-spec set_account_realm(doc(), ne_binary()) -> doc().
set_account_realm(Doc, AccountRealm) ->
    kz_json:set_value([<<"account_realm">>], AccountRealm, Doc).

-spec phone_number(doc()) -> api_ne_binary().
-spec phone_number(doc(), Default) -> ne_binary() | Default.
phone_number(Doc) ->
    phone_number(Doc, 'undefined').
phone_number(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"phone_number">>], Doc, Default).

-spec set_phone_number(doc(), ne_binary()) -> doc().
set_phone_number(Doc, PhoneNumber) ->
    kz_json:set_value([<<"phone_number">>], PhoneNumber, Doc).

-spec ui_url(doc()) -> api_ne_binary().
-spec ui_url(doc(), Default) -> ne_binary() | Default.
ui_url(Doc) ->
    ui_url(Doc, 'undefined').
ui_url(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"ui_url">>], Doc, Default).

-spec set_ui_url(doc(), ne_binary()) -> doc().
set_ui_url(Doc, UiUrl) ->
    kz_json:set_value([<<"ui_url">>], UiUrl, Doc).

-spec username(doc()) -> api_ne_binary().
-spec username(doc(), Default) -> ne_binary() | Default.
username(Doc) ->
    username(Doc, 'undefined').
username(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"username">>], Doc, Default).

-spec set_username(doc(), ne_binary()) -> doc().
set_username(Doc, Username) ->
    kz_json:set_value([<<"username">>], Username, Doc).
