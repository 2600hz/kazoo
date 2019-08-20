%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_auth_module_config).

-export([new/0]).
-export([enabled/1, enabled/2, set_enabled/2]).
-export([log_failed_attempts/1, log_failed_attempts/2, set_log_failed_attempts/2]).
-export([log_successful_attempts/1, log_successful_attempts/2, set_log_successful_attempts/2]).
-export([multi_factor/1, multi_factor/2, set_multi_factor/2]).
-export([multi_factor_account_id/1, multi_factor_account_id/2, set_multi_factor_account_id/2]).
-export([multi_factor_configuration_id/1, multi_factor_configuration_id/2, set_multi_factor_configuration_id/2]).
-export([multi_factor_enabled/1, multi_factor_enabled/2, set_multi_factor_enabled/2]).
-export([multi_factor_include_subaccounts/1, multi_factor_include_subaccounts/2, set_multi_factor_include_subaccounts/2]).
-export([token_auth_expiry_s/1, token_auth_expiry_s/2, set_token_auth_expiry_s/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"auth_module_config">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec enabled(doc()) -> kz_term:api_boolean().
enabled(Doc) ->
    enabled(Doc, 'undefined').

-spec enabled(doc(), Default) -> boolean() | Default.
enabled(Doc, Default) ->
    kz_json:get_boolean_value([<<"enabled">>], Doc, Default).

-spec set_enabled(doc(), boolean()) -> doc().
set_enabled(Doc, Enabled) ->
    kz_json:set_value([<<"enabled">>], Enabled, Doc).

-spec log_failed_attempts(doc()) -> kz_term:api_boolean().
log_failed_attempts(Doc) ->
    log_failed_attempts(Doc, 'undefined').

-spec log_failed_attempts(doc(), Default) -> boolean() | Default.
log_failed_attempts(Doc, Default) ->
    kz_json:get_boolean_value([<<"log_failed_attempts">>], Doc, Default).

-spec set_log_failed_attempts(doc(), boolean()) -> doc().
set_log_failed_attempts(Doc, LogFailedAttempts) ->
    kz_json:set_value([<<"log_failed_attempts">>], LogFailedAttempts, Doc).

-spec log_successful_attempts(doc()) -> kz_term:api_boolean().
log_successful_attempts(Doc) ->
    log_successful_attempts(Doc, 'undefined').

-spec log_successful_attempts(doc(), Default) -> boolean() | Default.
log_successful_attempts(Doc, Default) ->
    kz_json:get_boolean_value([<<"log_successful_attempts">>], Doc, Default).

-spec set_log_successful_attempts(doc(), boolean()) -> doc().
set_log_successful_attempts(Doc, LogSuccessfulAttempts) ->
    kz_json:set_value([<<"log_successful_attempts">>], LogSuccessfulAttempts, Doc).

-spec multi_factor(doc()) -> kz_term:api_object().
multi_factor(Doc) ->
    multi_factor(Doc, 'undefined').

-spec multi_factor(doc(), Default) -> kz_json:object() | Default.
multi_factor(Doc, Default) ->
    kz_json:get_json_value([<<"multi_factor">>], Doc, Default).

-spec set_multi_factor(doc(), kz_json:object()) -> doc().
set_multi_factor(Doc, MultiFactor) ->
    kz_json:set_value([<<"multi_factor">>], MultiFactor, Doc).

-spec multi_factor_account_id(doc()) -> kz_term:api_binary().
multi_factor_account_id(Doc) ->
    multi_factor_account_id(Doc, 'undefined').

-spec multi_factor_account_id(doc(), Default) -> binary() | Default.
multi_factor_account_id(Doc, Default) ->
    kz_json:get_binary_value([<<"multi_factor">>, <<"account_id">>], Doc, Default).

-spec set_multi_factor_account_id(doc(), binary()) -> doc().
set_multi_factor_account_id(Doc, MultiFactorAccountId) ->
    kz_json:set_value([<<"multi_factor">>, <<"account_id">>], MultiFactorAccountId, Doc).

-spec multi_factor_configuration_id(doc()) -> kz_term:api_binary().
multi_factor_configuration_id(Doc) ->
    multi_factor_configuration_id(Doc, 'undefined').

-spec multi_factor_configuration_id(doc(), Default) -> binary() | Default.
multi_factor_configuration_id(Doc, Default) ->
    kz_json:get_binary_value([<<"multi_factor">>, <<"configuration_id">>], Doc, Default).

-spec set_multi_factor_configuration_id(doc(), binary()) -> doc().
set_multi_factor_configuration_id(Doc, MultiFactorConfigurationId) ->
    kz_json:set_value([<<"multi_factor">>, <<"configuration_id">>], MultiFactorConfigurationId, Doc).

-spec multi_factor_enabled(doc()) -> kz_term:api_boolean().
multi_factor_enabled(Doc) ->
    multi_factor_enabled(Doc, 'undefined').

-spec multi_factor_enabled(doc(), Default) -> boolean() | Default.
multi_factor_enabled(Doc, Default) ->
    kz_json:get_boolean_value([<<"multi_factor">>, <<"enabled">>], Doc, Default).

-spec set_multi_factor_enabled(doc(), boolean()) -> doc().
set_multi_factor_enabled(Doc, MultiFactorEnabled) ->
    kz_json:set_value([<<"multi_factor">>, <<"enabled">>], MultiFactorEnabled, Doc).

-spec multi_factor_include_subaccounts(doc()) -> kz_term:api_boolean().
multi_factor_include_subaccounts(Doc) ->
    multi_factor_include_subaccounts(Doc, 'undefined').

-spec multi_factor_include_subaccounts(doc(), Default) -> boolean() | Default.
multi_factor_include_subaccounts(Doc, Default) ->
    kz_json:get_boolean_value([<<"multi_factor">>, <<"include_subaccounts">>], Doc, Default).

-spec set_multi_factor_include_subaccounts(doc(), boolean()) -> doc().
set_multi_factor_include_subaccounts(Doc, MultiFactorIncludeSubaccounts) ->
    kz_json:set_value([<<"multi_factor">>, <<"include_subaccounts">>], MultiFactorIncludeSubaccounts, Doc).

-spec token_auth_expiry_s(doc()) -> kz_term:api_integer().
token_auth_expiry_s(Doc) ->
    token_auth_expiry_s(Doc, 'undefined').

-spec token_auth_expiry_s(doc(), Default) -> integer() | Default.
token_auth_expiry_s(Doc, Default) ->
    kz_json:get_integer_value([<<"token_auth_expiry_s">>], Doc, Default).

-spec set_token_auth_expiry_s(doc(), integer()) -> doc().
set_token_auth_expiry_s(Doc, TokenAuthenticationExpiryS) ->
    kz_json:set_value([<<"token_auth_expiry_s">>], TokenAuthenticationExpiryS, Doc).
