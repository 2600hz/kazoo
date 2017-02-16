%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_mfa_auth).

-export([authenticate/1
        ]).

-include("kazoo_auth.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".mfa">>).

-type result() :: mfa_result().

-export_type([result/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc Read configuration and do authentication with configured MFA provider
%% @end
%%--------------------------------------------------------------------
-spec authenticate(kz_proplist()) -> result().
authenticate(Claims) ->
    Configs = get_configs(props:get_value(<<"mfa_options">>, Claims)),
    case provider(Configs) of
        {'disabled', _Provider} ->
            lager:debug("provider ~s is disabled", [_Provider]),
            {'error', 'provider_disabled'};
        ?NE_BINARY=Provider ->
            Module = module_name(Provider),
            lager:debug("performing authentication factor with ~s(~p) provider"
                       ,[Provider, Module]
                       ),
            Module:authenticate(Claims, Configs);
        _ ->
            lager:debug("no provider is available or configured"),
            {'error', 'no_provider'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Get MFA provider and checks it's enabled or not
%% @end
%%--------------------------------------------------------------------
-spec provider(api_object()) -> api_binary() | {'disabled', api_binary()}.
provider(Configs) ->
    Provider = kz_json:get_value(<<"provider">>, Configs),
    case kz_json:is_true(<<"enabled">>, Configs, 'true') of
        'true' -> Provider;
        'false' -> {'disabled', Provider}
    end.

-spec default_provider() -> api_binary().
default_provider() ->
    kapps_config:get_binary(?MOD_CONFIG_CAT, <<"default_provider">>).

%%--------------------------------------------------------------------
%% @private
%% @doc Get MFA configs from Account, if there was no config account
%% get system default configuration
%% @end
%%--------------------------------------------------------------------
-spec get_configs('undefined' | kz_proplist()) -> api_object().
get_configs('undefined') ->
    get_system_configs();
get_configs(Options) when is_list(Options) ->
    AccountId = props:get_value(<<"account_id">>, Options),
    ConfigId = props:get_value(<<"config_id">>, Options),
    get_account_configs(AccountId, ConfigId).

-spec get_account_configs(api_binary(), api_binary()) -> api_object().
get_account_configs('undefined', _ConfigId) -> get_system_configs();
get_account_configs(_AccountId, 'undefined') -> get_system_configs();
get_account_configs(AccountId, ConfigId) ->
    AccountDb = kz_util:format_account_db(AccountId),
    case kz_datamgr:open_cache_doc(AccountDb, ConfigId) of
        {'ok', JObj} ->
            lager:debug("fetched authentication factor config from ~s/~s"
                       ,[AccountDb, ConfigId]
                       ),
            JObj;
        {'error', _Reason} ->
            lager:debug("failed to open authentication factor configuration from ~s/~s : ~p"
                       ,[AccountDb, ConfigId, _Reason]
                       ),
            get_system_configs()
    end.

-spec get_system_configs() -> api_object().
get_system_configs() ->
    lager:debug("get authentication factor configuration from system config"),
    case default_provider() of
        'undefined' -> 'undefined';
        DefaultProvider ->
            case kapps_config:get_json(?MOD_CONFIG_CAT, [<<"providers">>, DefaultProvider]) of
                'undefined' -> 'undefined';
                JObj ->
                    kz_json:set_value(<<"provider">>, DefaultProvider, JObj)
            end
    end.

-spec module_name(ne_binary()) -> atom().
module_name(Provider) -> kz_term:to_atom(<<"kz_mfa_", Provider/binary>>, 'true').
