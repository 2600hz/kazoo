%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017-2019, 2600Hz
%%% @doc Module to authenticate a user with a multi factor provider.
%%% @author Hesaam Farhang
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_mfa_auth).

-export([authenticate/1
        ,get_configs/1
        ,get_system_configs/0
        ,provider/1, default_provider/0
        ]).

-include("kazoo_auth.hrl").

-type result() :: mfa_result().

-export_type([result/0]).

%%------------------------------------------------------------------------------
%% @doc Read configuration and do authentication with configured MFA provider.
%% @end
%%------------------------------------------------------------------------------
-spec authenticate(kz_term:proplist()) -> result().
authenticate(Claims) ->
    Configs = get_configs(props:get_value(<<"mfa_options">>, Claims)),
    case provider(Configs) of
        {'disabled', _Provider} ->
            ErrorMsg = io_lib:format("provider ~s is disabled", [_Provider]),
            {'error', kz_term:to_binary(ErrorMsg)};
        ?NE_BINARY=Provider ->
            Module = module_name(Provider),
            lager:debug("performing authentication factor with ~s(~p) provider"
                       ,[Provider, Module]
                       ),
            Module:authenticate(Claims, kz_json:get_value(<<"settings">>, Configs, kz_json:new()));
        _ ->
            {'error', 'no_provider'}
    end.

%%------------------------------------------------------------------------------
%% @doc Get MFA provider and checks it's enabled or not.
%% @end
%%------------------------------------------------------------------------------
-spec provider(kz_term:api_object()) -> kz_term:ne_binary() | {'disabled', kz_term:ne_binary()} | {'error', 'no_provider'}.
provider(Configs) ->
    Name = kz_json:get_ne_value(<<"provider_name">>, Configs),
    IsDefined = kz_term:is_not_empty(Name),
    case kz_json:is_true(<<"enabled">>, Configs, 'true') of
        'true' when IsDefined -> Name;
        'false' when IsDefined -> {'disabled', Name};
        _ -> {'error', 'no_provider'}
    end.

%%------------------------------------------------------------------------------
%% @doc Get MFA config from Account, if there was no config account
%% get system default configuration.
%% @end
%%------------------------------------------------------------------------------
-spec get_configs('undefined' | kz_term:proplist() | kz_json:object()) -> kz_term:api_object().
get_configs('undefined') ->
    get_system_configs();
get_configs(Options) when is_list(Options) ->
    AccountId = props:get_value(<<"account_id">>, Options),
    ConfigId = props:get_value(<<"configuration_id">>, Options),
    get_account_configs(AccountId, ConfigId);
get_configs(JObj) ->
    get_configs(kz_json:recursive_to_proplist(JObj)).

-spec get_account_configs(kz_term:api_binary(), kz_term:api_binary()) -> kz_term:api_object().
get_account_configs('undefined', _ConfigId) -> get_system_configs();
get_account_configs(_AccountId, 'undefined') -> get_system_configs();
get_account_configs(AccountId, ConfigId) ->
    AccountDb = kz_util:format_account_db(AccountId),
    case kz_datamgr:open_cache_doc(AccountDb, ConfigId) of
        {'ok', JObj} ->
            lager:debug("fetched authentication factor config from ~s/~s"
                       ,[AccountDb, ConfigId]
                       ),
            kz_json:set_value(<<"from">>, AccountId, JObj);
        {'error', _Reason} ->
            lager:debug("failed to open authentication factor configuration from ~s/~s : ~p"
                       ,[AccountDb, ConfigId, _Reason]
                       ),
            'undefined'
    end.

-spec get_system_configs() -> kz_term:api_object().
get_system_configs() ->
    lager:debug("get authentication factor configuration from system config"),
    DefaultProvider = default_provider(),
    case kz_datamgr:open_cache_doc(?KZ_AUTH_DB, DefaultProvider) of
        {'ok', JObj} -> kz_json:set_value(<<"from">>, <<"system">>, JObj);
        {'error', _R} ->
            lager:debug("failed to open default ~s multi factor provider config", [DefaultProvider]),
            'undefined'
    end.

-spec module_name(kz_term:ne_binary()) -> atom().
module_name(Provider) -> kz_term:to_atom(<<"kz_mfa_", Provider/binary>>, 'true').


%%------------------------------------------------------------------------------
%% @doc Returns the system's default multi factor provider.
%% @end
%%------------------------------------------------------------------------------
-spec default_provider() -> kz_term:ne_binary().
default_provider() ->
    kapps_config:get_binary(?CONFIG_CAT, <<"default_multi_factor_provider">>, <<"duo">>).
