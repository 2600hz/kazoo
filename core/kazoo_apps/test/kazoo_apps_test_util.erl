%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_apps_test_util).

-export([setup/0
        ,cleanup/1
        ]).

-include_lib("kazoo_fixturedb/include/kz_fixturedb.hrl").
-include("kazoo_apps.hrl").
-include("kazoo_apps_test.hrl").

setup() ->
    ?LOG_DEBUG(":: Setting up Kazoo FixtureDB"),

    {'ok', _} = application:ensure_all_started('kazoo_config'),

    FixturesPid = kazoo_fixturedb:start(),

    {'ok', KappsCachePid} = kz_cache_sup:start_link('kapps_config_cache'),

    {'ok', SubConfig} = get_fixture(?FIXTURE_CHILD_ACCOUNT_ID, ?TEST_CAT),
    {'ok', ResellerOnly} = get_fixture(?FIXTURE_RESELLER_ACCOUNT_ID, ?RESELLER_ONLY),

    #{sub_config => SubConfig
     ,reseller_only_config => ResellerOnly
     ,system_config => get_fixture_value(<<"default">>, ?KZ_CONFIG_DB, ?TEST_CAT)
     ,system_only => get_fixture_value(<<"default">>, ?KZ_CONFIG_DB, ?SYSTEM_ONLY)
     ,kapps_cache_pid => KappsCachePid
     ,fixtures_pid => FixturesPid
     }.

cleanup(#{kapps_cache_pid := KappsCachePid
         ,fixtures_pid := FixturesPid
         }) ->
    try
        process_flag('trap_exit', 'true'),
        exit(KappsCachePid, 'shutdown'),
        exit(FixturesPid, 'shutdown'),

        wait_for_shutdown([KappsCachePid, FixturesPid]),

        _ = application:stop('kazoo_services'),
        _ = application:stop('kazoo_data'),
        _ = application:stop('kazoo_config')
    catch
        _E:_R ->
            ?LOG_DEBUG("failed to shutdown: ~p: ~p~n", [_E, _R])
    after
        ?LOG_DEBUG(":: Stopped Kazoo FixtureDB")
    end;
cleanup(_Msg) ->
    ?LOG_DEBUG("unmatched clause: ~p", [_Msg]).

wait_for_shutdown([]) -> 'ok';
wait_for_shutdown(Pids) ->
    receive
        {'EXIT', Pid, _Reason} ->
            wait_for_shutdown(lists:delete(Pid, Pids));
        Msg ->
            ?LOG_DEBUG("waiting for ~p: ~p", [Pids, Msg]),
            wait_for_shutdown(Pids)
    after 5000 ->
            ?LOG_DEBUG("timed out waiting for ~p", [Pids])
    end.
