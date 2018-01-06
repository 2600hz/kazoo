-module(kzd_audit_logs).

-export([new/0]).
-export([audit/1, audit/2, set_audit/2]).
-export([authenticating_user/1, authenticating_user/2, set_authenticating_user/2]).
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

-spec tree(doc()) -> api_ne_binaries().
-spec tree(doc(), Default) -> ne_binaries() | Default.
tree(Doc) ->
    tree(Doc, 'undefined').
tree(Doc, Default) ->
    kz_json:get_list_value(<<"tree">>, Doc, Default).

-spec set_tree(doc(), ne_binaries()) -> doc().
set_tree(Doc, Tree) ->
    kz_json:set_value(<<"tree">>, Tree, Doc).
