-module(kzd_auth_module_config).

-export([new/0]).
-export([enabled/1, enabled/2, set_enabled/2]).
-export([log_failed_attempts/1, log_failed_attempts/2, set_log_failed_attempts/2]).
-export([log_successful_attempts/1, log_successful_attempts/2, set_log_successful_attempts/2]).
-export([multi_factor/1, multi_factor/2, set_multi_factor/2]).
-export([token_auth_expiry_s/1, token_auth_expiry_s/2, set_token_auth_expiry_s/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec enabled(doc()) -> api_boolean().
-spec enabled(doc(), Default) -> boolean() | Default.
enabled(Doc) ->
    enabled(Doc, 'undefined').
enabled(Doc, Default) ->
    kz_json:get_boolean_value(<<"enabled">>, Doc, Default).

-spec set_enabled(doc(), boolean()) -> doc().
set_enabled(Doc, Enabled) ->
    kz_json:set_value(<<"enabled">>, Enabled, Doc).

-spec log_failed_attempts(doc()) -> api_boolean().
-spec log_failed_attempts(doc(), Default) -> boolean() | Default.
log_failed_attempts(Doc) ->
    log_failed_attempts(Doc, 'undefined').
log_failed_attempts(Doc, Default) ->
    kz_json:get_boolean_value(<<"log_failed_attempts">>, Doc, Default).

-spec set_log_failed_attempts(doc(), boolean()) -> doc().
set_log_failed_attempts(Doc, LogFailedAttempts) ->
    kz_json:set_value(<<"log_failed_attempts">>, LogFailedAttempts, Doc).

-spec log_successful_attempts(doc()) -> api_boolean().
-spec log_successful_attempts(doc(), Default) -> boolean() | Default.
log_successful_attempts(Doc) ->
    log_successful_attempts(Doc, 'undefined').
log_successful_attempts(Doc, Default) ->
    kz_json:get_boolean_value(<<"log_successful_attempts">>, Doc, Default).

-spec set_log_successful_attempts(doc(), boolean()) -> doc().
set_log_successful_attempts(Doc, LogSuccessfulAttempts) ->
    kz_json:set_value(<<"log_successful_attempts">>, LogSuccessfulAttempts, Doc).

-spec multi_factor(doc()) -> api_object().
-spec multi_factor(doc(), Default) -> kz_json:object() | Default.
multi_factor(Doc) ->
    multi_factor(Doc, 'undefined').
multi_factor(Doc, Default) ->
    kz_json:get_json_value(<<"multi_factor">>, Doc, Default).

-spec set_multi_factor(doc(), kz_json:object()) -> doc().
set_multi_factor(Doc, MultiFactor) ->
    kz_json:set_value(<<"multi_factor">>, MultiFactor, Doc).

-spec token_auth_expiry_s(doc()) -> api_integer().
-spec token_auth_expiry_s(doc(), Default) -> integer() | Default.
token_auth_expiry_s(Doc) ->
    token_auth_expiry_s(Doc, 'undefined').
token_auth_expiry_s(Doc, Default) ->
    kz_json:get_integer_value(<<"token_auth_expiry_s">>, Doc, Default).

-spec set_token_auth_expiry_s(doc(), integer()) -> doc().
set_token_auth_expiry_s(Doc, TokenAuthenticationExpiryS) ->
    kz_json:set_value(<<"token_auth_expiry_s">>, TokenAuthenticationExpiryS, Doc).
