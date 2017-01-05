%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz
%%% @doc
%%% kazoo data utils
%%% @end
%%% @contributors
%%%-----------------------------------------------------------------------------
-module(kzs_util).

-export([db_classification/1
        ,db_priority/1
        ,map_keys_to_atoms/1
        ]).

-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").
-include("kz_data.hrl").

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec db_classification(text()) -> db_classifications().
db_classification(Db) when not is_binary(Db) ->
    db_classification(kz_util:to_binary(Db));
db_classification(<<"ts">>) -> 'deprecated';
db_classification(<<"crossbar_schemas">>) -> 'deprecated';
db_classification(<<"registrations">>) -> 'deprecated';
db_classification(<<"crossbar%2Fsessions">>) -> 'deprecated';
db_classification(<<"sms">>) -> 'deprecated';
db_classification(<<"signups">>) -> 'system'; %% Soon to be deprecated
db_classification(?KZ_RATES_DB) -> 'system';
db_classification(?KZ_ALERTS_DB) -> 'system';
db_classification(?KZ_OFFNET_DB) -> 'system';
db_classification(?KZ_ANONYMOUS_CDR_DB) -> 'system';
db_classification(?KZ_DEDICATED_IP_DB) -> 'system';
db_classification(?KZ_CONFIG_DB) -> 'system';
db_classification(?KZ_MEDIA_DB) -> 'system';
db_classification(?KZ_SCHEMA_DB) -> 'system';
db_classification(?KZ_OAUTH_DB) -> 'system';
db_classification(?KZ_AUTH_DB) -> 'system';
db_classification(?KZ_DATA_DB) -> 'system';
db_classification(?KZ_TASKS_DB) -> 'system';
db_classification(?KZ_PROVISIONER_DB) -> 'system'; %% Soon to be deprecated
db_classification(?KZ_ACCOUNTS_DB) -> 'aggregate';
db_classification(?KZ_TOKEN_DB) -> 'aggregate';
db_classification(?KZ_SIP_DB) -> 'aggregate';
db_classification(?KZ_FAXES_DB) -> 'aggregate';
db_classification(?KZ_ACDC_DB) -> 'aggregate';
db_classification(?KZ_SERVICES_DB) -> 'aggregate';
db_classification(?KZ_PORT_REQUESTS_DB) -> 'aggregate';
db_classification(?KZ_WEBHOOKS_DB) -> 'aggregate';
db_classification(<<?KNM_DB_PREFIX, _/binary>>) -> 'numbers';
db_classification(<<?KNM_DB_PREFIX_ENCODED, _/binary>>) -> 'numbers';
db_classification(<<?KNM_DB_PREFIX_encoded, _/binary>>) -> 'numbers';
db_classification(<<"numbers/", _/binary>>) -> 'system_numbers';
db_classification(<<"numbers%2F", _/binary>>) -> 'system_numbers';
db_classification(<<"numbers%2f", _/binary>>) -> 'system_numbers';
db_classification(?MATCH_RESOURCE_SELECTORS_UNENCODED(_AccountId)) -> 'resource_selectors';
db_classification(?MATCH_RESOURCE_SELECTORS_encoded(_AccountId)) -> 'resource_selectors';
db_classification(?MATCH_RESOURCE_SELECTORS_ENCODED(_AccountId)) -> 'resource_selectors';
db_classification(?MATCH_RESOURCE_SELECTORS_RAW(_AccountId)) -> 'resource_selectors';
db_classification(?MATCH_MODB_SUFFIX_UNENCODED(_A,_B,_Rest,_Year,_Month)) -> 'modb';% these only need to match
db_classification(?MATCH_MODB_SUFFIX_ENCODED(_A,_B,_Rest,_Year,_Month)) -> 'modb';%   "account..." then the
db_classification(?MATCH_MODB_SUFFIX_encoded(_A,_B,_Rest,_Year,_Month)) -> 'modb';%   right size.
db_classification(?MATCH_MODB_SUFFIX_RAW(_Account,_Year,_Month)) -> 'modb';%   right size.
db_classification(?MATCH_ACCOUNT_UNENCODED(_AccountId)) -> 'account';
db_classification(?MATCH_ACCOUNT_encoded(_AccountId)) -> 'account';
db_classification(?MATCH_ACCOUNT_ENCODED(_AccountId)) -> 'account';
db_classification(_Database) ->
    lager:warning("unknown type for database ~s", [_Database]),
    lager:debug("unknown database classification: ~p", [erlang:process_info(self(),current_stacktrace)]),
    'undefined'.

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec db_priority(text()) -> non_neg_integer().
db_priority(Db) when not is_binary(Db) ->
    db_priority(kz_util:to_binary(Db));
db_priority(?KZ_CONFIG_DB) -> 0;
db_priority(?KZ_DATA_DB) -> 1;
db_priority(?KZ_OFFNET_DB) -> 2;
db_priority(?KZ_ACCOUNTS_DB) -> 3;
db_priority(?KZ_SIP_DB) -> 4;
db_priority(?KZ_AUTH_DB) -> 5;
db_priority(?KZ_WEBHOOKS_DB) -> 6;
db_priority(?KZ_RATES_DB) -> 7;
db_priority(?KZ_ACDC_DB) -> 8;
db_priority(?KZ_FAXES_DB) -> 9;
db_priority(?KZ_SCHEMA_DB) -> 10;
db_priority(?KZ_SERVICES_DB) -> 11;
db_priority(?KZ_PORT_REQUESTS_DB) -> 12;
db_priority(?KZ_TASKS_DB) -> 13;
db_priority(?KZ_DEDICATED_IP_DB) -> 14;
db_priority(?KZ_ALERTS_DB) -> 15;
db_priority(?KZ_MEDIA_DB) -> 16;
db_priority(?KZ_OAUTH_DB) -> 17;
db_priority(?KZ_TOKEN_DB) -> 18;
db_priority(<<?KNM_DB_PREFIX, _/binary>>) -> 19;
db_priority(<<?KNM_DB_PREFIX_ENCODED, _/binary>>) -> 19;
db_priority(<<?KNM_DB_PREFIX_encoded, _/binary>>) -> 19;
db_priority(<<"numbers/", _/binary>>) -> 20;
db_priority(<<"numbers%2F", _/binary>>) -> 20;
db_priority(<<"numbers%2f", _/binary>>) -> 20;
db_priority(?MATCH_ACCOUNT_UNENCODED(_AccountId)) -> 21;
db_priority(?MATCH_ACCOUNT_encoded(_AccountId)) -> 21;
db_priority(?MATCH_ACCOUNT_ENCODED(_AccountId)) -> 21;
db_priority(?MATCH_MODB_SUFFIX_UNENCODED(_A,_B,_Rest,_Year,_Month)) -> 22;
db_priority(?MATCH_MODB_SUFFIX_ENCODED(_A,_B,_Rest,_Year,_Month)) -> 22;
db_priority(?MATCH_MODB_SUFFIX_encoded(_A,_B,_Rest,_Year,_Month)) -> 22;
db_priority(?MATCH_MODB_SUFFIX_RAW(_Account,_Year,_Month)) -> 22;
db_priority(?MATCH_RESOURCE_SELECTORS_UNENCODED(_AccountId)) -> 23;
db_priority(?MATCH_RESOURCE_SELECTORS_encoded(_AccountId)) -> 23;
db_priority(?MATCH_RESOURCE_SELECTORS_ENCODED(_AccountId)) -> 23;
db_priority(?MATCH_RESOURCE_SELECTORS_RAW(_AccountId)) -> 23;
db_priority(_Database) -> 24.

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec map_keys_to_atoms(map()) -> map().
map_keys_to_atoms(Map) ->
    maps:fold(fun map_keys_to_atoms_fold/3, #{}, Map).

map_keys_to_atoms_fold(K, V, Acc) when is_map(V) ->
    Acc#{kz_util:to_atom(K, 'true') => map_keys_to_atoms(V)};
map_keys_to_atoms_fold(K, V, Acc) ->
    Acc#{kz_util:to_atom(K, 'true') => V}.
