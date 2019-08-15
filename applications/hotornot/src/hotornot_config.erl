%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(hotornot_config).

-export([default_minimum/0
        ,default_increment/0
        ,default_nocharge/0
        ,default_surcharge/0
        ,default_cost/0
        ,default_internal_cost/0

        ,default_ratedeck/0

        ,mobile_rate/0

        ,filter_list/0
        ,should_sort_by_weight/0
        ,rate_version/0, set_rate_version/1

        ,should_account_filter_by_resource/1

        ,should_use_trie/0, use_trie/0, dont_use_trie/0
        ,trie_module/0, use_trie_lru/0
        ,trie_build_timeout_ms/0
        ,lru_expires_s/0

        ,should_publish_alert/1
        ]).

-include("hotornot.hrl").

-spec default_minimum() -> pos_integer().
default_minimum() ->
    kapps_config:get_integer(?APP_NAME, <<"default_rate_minimum">>, ?SECONDS_IN_MINUTE).

-spec default_increment() -> pos_integer().
default_increment() ->
    kapps_config:get_integer(?APP_NAME, <<"default_rate_increment">>, ?SECONDS_IN_MINUTE).

-spec default_nocharge() -> non_neg_integer().
default_nocharge() ->
    kapps_config:get_integer(?APP_NAME, <<"default_rate_nocharge_time">>, 0).

-spec default_surcharge() -> float().
default_surcharge() ->
    kapps_config:get_float(?APP_NAME, <<"default_rate_surcharge">>, 0.0).

-spec default_cost() -> float().
default_cost() ->
    kapps_config:get_float(?APP_NAME, <<"default_rate_cost">>, 0.0).

-spec default_internal_cost() -> float().
default_internal_cost() ->
    kapps_config:get_float(?APP_NAME, <<"default_rate_internal_cost">>, 0.0).

-spec filter_list() -> kz_term:ne_binaries().
filter_list() ->
    kapps_config:get(?APP_NAME, <<"filter_list">>, [<<"direction">>
                                                   ,<<"route_options">>
                                                   ,<<"routes">>
                                                   ]).

-spec default_ratedeck() -> kz_term:ne_binary().
default_ratedeck() ->
    kapps_config:get_ne_binary(?APP_NAME, <<"default_ratedeck">>, ?KZ_RATES_DB).

-spec mobile_rate() -> kz_term:api_object().
mobile_rate() ->
    kapps_config:get_json(?APP_NAME, <<"mobile_rate">>).

-spec should_sort_by_weight() -> boolean().
should_sort_by_weight() ->
    kapps_config:get_is_true(?APP_NAME, <<"sort_by_weight">>, 'true').

-spec should_use_trie() -> boolean().
should_use_trie() ->
    kapps_config:get_is_true(?APP_NAME, <<"use_trie">>, 'false').

-spec use_trie() -> 'ok'.
use_trie() ->
    {'ok', _} = kapps_config:set_default(?APP_NAME, <<"use_trie">>, 'true'),
    {'ok', _} = kapps_config:set_default(?APP_NAME, <<"trie_module">>, <<"hon_trie">>),
    'ok'.

-spec dont_use_trie() -> 'ok'.
dont_use_trie() ->
    {'ok', _} = kapps_config:set_default(?APP_NAME, <<"use_trie">>, 'false'),
    'ok'.

-spec use_trie_lru() -> 'ok'.
use_trie_lru() ->
    use_trie(),
    {'ok', _} = kapps_config:set_default(?APP_NAME, <<"trie_module">>, <<"hon_trie_lru">>),
    'ok'.

-spec trie_module() -> atom().
trie_module() ->
    kapps_config:get_atom(?APP_NAME, <<"trie_module">>, 'hon_trie').

-spec trie_build_timeout_ms() -> pos_integer().
trie_build_timeout_ms() ->
    kapps_config:get_integer(?APP_NAME, <<"trie_build_timeout_ms">>, ?MILLISECONDS_IN_MINUTE).

-spec rate_version() -> kz_term:api_ne_binary().
rate_version() ->
    kapps_config:get_ne_binary(?APP_NAME, <<"rate_version">>).

-spec set_rate_version(kz_term:ne_binary()) -> 'ok'.
set_rate_version(Version) ->
    {'ok', _} = kapps_config:set_string(?APP_NAME, <<"rate_version">>, Version),
    'ok'.

-spec should_account_filter_by_resource(kz_term:ne_binary()) -> boolean().
should_account_filter_by_resource(AccountId) ->
    kapps_account_config:get_from_reseller(AccountId, ?APP_NAME, <<"filter_by_resource_id">>, 'false').

-spec lru_expires_s() -> non_neg_integer().
-ifdef(TEST).
lru_expires_s() -> ?SECONDS_IN_DAY.
-else.
lru_expires_s() ->
    kapps_config:get_integer(?APP_NAME, <<"trie_lru_expires_s">>, ?SECONDS_IN_DAY).
-endif.

-spec should_publish_alert(kz_term:api_ne_binary()) -> boolean().
should_publish_alert('undefined') ->
    kapps_config:get_is_true(?APP_NAME, [<<"should_publish_system_alert">>, <<"both">>], 'true');
should_publish_alert(Direction) ->
    kapps_config:get_is_true(?APP_NAME, [<<"should_publish_system_alert">>, Direction], 'true')
        orelse kapps_config:get_is_true(?APP_NAME, [<<"should_publish_system_alert">>, <<"both">>], 'true').
