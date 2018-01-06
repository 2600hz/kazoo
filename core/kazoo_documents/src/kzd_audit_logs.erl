-module(kzd_audit_logs).

-export([new/0]).
-export([audit/1, audit/2, set_audit/2]).
-export([authenticating_user/1, authenticating_user/2, set_authenticating_user/2]).
-export([authenticating_user_account_id/1, authenticating_user_account_id/2, set_authenticating_user_account_id/2]).
-export([authenticating_user_account_name/1, authenticating_user_account_name/2, set_authenticating_user_account_name/2]).
-export([authenticating_user_first_name/1, authenticating_user_first_name/2, set_authenticating_user_first_name/2]).
-export([authenticating_user_last_name/1, authenticating_user_last_name/2, set_authenticating_user_last_name/2]).
-export([authenticating_user_user_id/1, authenticating_user_user_id/2, set_authenticating_user_user_id/2]).
-export([tree/1, tree/2, set_tree/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec audit(doc()) -> api_object().
-spec audit(doc(), Default) -> kz_json:object() | Default.
audit(Doc) ->
    audit(Doc, 'undefined').
audit(Doc, Default) ->
    kz_json:get_json_value(<<"audit">>, Doc, Default).

-spec set_audit(doc(), kz_json:object()) -> doc().
set_audit(Doc, Audit) ->
    kz_json:set_value(<<"audit">>, Audit, Doc).

-spec authenticating_user(doc()) -> api_object().
-spec authenticating_user(doc(), Default) -> kz_json:object() | Default.
authenticating_user(Doc) ->
    authenticating_user(Doc, 'undefined').
authenticating_user(Doc, Default) ->
    kz_json:get_json_value(<<"authenticating_user">>, Doc, Default).

-spec set_authenticating_user(doc(), kz_json:object()) -> doc().
set_authenticating_user(Doc, AuthenticatingUser) ->
    kz_json:set_value(<<"authenticating_user">>, AuthenticatingUser, Doc).

-spec authenticating_user_account_id(doc()) -> api_binary().
-spec authenticating_user_account_id(doc(), Default) -> binary() | Default.
authenticating_user_account_id(Doc) ->
    authenticating_user_account_id(Doc, 'undefined').
authenticating_user_account_id(Doc, Default) ->
    kz_json:get_binary_value([<<"authenticating_user">>, <<"account_id">>], Doc, Default).

-spec set_authenticating_user_account_id(doc(), binary()) -> doc().
set_authenticating_user_account_id(Doc, AuthenticatingUserAccountId) ->
    kz_json:set_value([<<"authenticating_user">>, <<"account_id">>], AuthenticatingUserAccountId, Doc).

-spec authenticating_user_account_name(doc()) -> api_binary().
-spec authenticating_user_account_name(doc(), Default) -> binary() | Default.
authenticating_user_account_name(Doc) ->
    authenticating_user_account_name(Doc, 'undefined').
authenticating_user_account_name(Doc, Default) ->
    kz_json:get_binary_value([<<"authenticating_user">>, <<"account_name">>], Doc, Default).

-spec set_authenticating_user_account_name(doc(), binary()) -> doc().
set_authenticating_user_account_name(Doc, AuthenticatingUserAccountName) ->
    kz_json:set_value([<<"authenticating_user">>, <<"account_name">>], AuthenticatingUserAccountName, Doc).

-spec authenticating_user_first_name(doc()) -> api_binary().
-spec authenticating_user_first_name(doc(), Default) -> binary() | Default.
authenticating_user_first_name(Doc) ->
    authenticating_user_first_name(Doc, 'undefined').
authenticating_user_first_name(Doc, Default) ->
    kz_json:get_binary_value([<<"authenticating_user">>, <<"first_name">>], Doc, Default).

-spec set_authenticating_user_first_name(doc(), binary()) -> doc().
set_authenticating_user_first_name(Doc, AuthenticatingUserFirstName) ->
    kz_json:set_value([<<"authenticating_user">>, <<"first_name">>], AuthenticatingUserFirstName, Doc).

-spec authenticating_user_last_name(doc()) -> api_binary().
-spec authenticating_user_last_name(doc(), Default) -> binary() | Default.
authenticating_user_last_name(Doc) ->
    authenticating_user_last_name(Doc, 'undefined').
authenticating_user_last_name(Doc, Default) ->
    kz_json:get_binary_value([<<"authenticating_user">>, <<"last_name">>], Doc, Default).

-spec set_authenticating_user_last_name(doc(), binary()) -> doc().
set_authenticating_user_last_name(Doc, AuthenticatingUserLastName) ->
    kz_json:set_value([<<"authenticating_user">>, <<"last_name">>], AuthenticatingUserLastName, Doc).

-spec authenticating_user_user_id(doc()) -> api_binary().
-spec authenticating_user_user_id(doc(), Default) -> binary() | Default.
authenticating_user_user_id(Doc) ->
    authenticating_user_user_id(Doc, 'undefined').
authenticating_user_user_id(Doc, Default) ->
    kz_json:get_binary_value([<<"authenticating_user">>, <<"user_id">>], Doc, Default).

-spec set_authenticating_user_user_id(doc(), binary()) -> doc().
set_authenticating_user_user_id(Doc, AuthenticatingUserUserId) ->
    kz_json:set_value([<<"authenticating_user">>, <<"user_id">>], AuthenticatingUserUserId, Doc).

-spec tree(doc()) -> api_ne_binaries().
-spec tree(doc(), Default) -> ne_binaries() | Default.
tree(Doc) ->
    tree(Doc, 'undefined').
tree(Doc, Default) ->
    kz_json:get_list_value(<<"tree">>, Doc, Default).

-spec set_tree(doc(), ne_binaries()) -> doc().
set_tree(Doc, Tree) ->
    kz_json:set_value(<<"tree">>, Tree, Doc).
