%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(webhooks_disabler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("webhooks.hrl").

disabler_test_() ->
    {'setup'
    ,fun start_disabler/0
    ,fun stop_disabler/1
    ,fun(_) ->
             [{"should not disable the hook", ?_test(should_not_disable_hook())}
             ,{"should disable the hook", ?_test(should_disable_hook())}
             ]
     end
    }.

-define(ACCOUNT_ID, <<"account0000000000000000000000001">>).

start_disabler() ->
    {'ok', _} = kz_cache_sup:start_link(?CACHE_NAME),
    kz_fixturedb_util:start_me().

stop_disabler(FixtureDb) ->
    kz_fixturedb_util:stop_me(FixtureDb),
    kz_cache_sup:stop(?CACHE_NAME).

should_not_disable_hook() ->
    kz_cache:flush_local(?CACHE_NAME),

    HookId = kz_binary:rand_hex(16),
    NowMs = kz_time:now_ms(),

    ExpiryMs = webhooks_util:system_expires_time(),
    FailureCount = webhooks_util:system_failure_count(),

    [add_distant_failure(?ACCOUNT_ID, HookId, NowMs, ExpiryMs) || _ <- lists:seq(1, FailureCount)],
    ?assertEqual([], webhooks_disabler:find_failures()).

should_disable_hook() ->
    kz_cache:flush_local(?CACHE_NAME),

    HookId = kz_binary:rand_hex(16),
    NowMs = kz_time:now_ms(),

    ExpiryMs = webhooks_util:system_expires_time(),
    FailureCount = webhooks_util:system_failure_count(),

    [add_recent_failure(?ACCOUNT_ID, HookId, NowMs, ExpiryMs) || _ <- lists:seq(1, FailureCount)],
    ?assertEqual([{{?ACCOUNT_ID, HookId}, FailureCount}], webhooks_disabler:find_failures()).

%% @doc all failures happen outside the window of expiration
%%
%% Presumably they've been flushed by the cache process but if they remain,
%% then webhooks_disabler should skip counting them anyway
add_distant_failure(AccountId, HookId, NowMs, ExpiryMs) ->
    Offset = rand:uniform(ExpiryMs) + ExpiryMs,
    webhooks_util:note_failed_attempt(AccountId, HookId, NowMs - Offset).

add_recent_failure(AccountId, HookId, NowMs, ExpiryMs) ->
    Offset = rand:uniform(ExpiryMs) - 1,
    webhooks_util:note_failed_attempt(AccountId, HookId, NowMs - Offset).
