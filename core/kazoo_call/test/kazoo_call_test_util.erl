-module(kazoo_call_test_util).

-export([setup_db/0
        ,terminate_db/1
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-spec setup_db() -> pid().
setup_db() ->
    ?LOG_DEBUG(":: Starting Kazoo FixtureDB"),
    {'ok', _} = application:ensure_all_started('kazoo_config'),
    kazoo_fixturedb:start().

-spec terminate_db(pid()) -> any().
terminate_db(Pid) ->
    _DataLink = erlang:exit(Pid, 'normal'),
    Ref = monitor('process', Pid),
    receive
        {'DOWN', Ref, 'process', Pid, _Reason} ->
            _KConfig = application:stop('kazoo_config'),
            ?LOG_DEBUG(":: Stopped Kazoo FixtureDB, data_link: ~p kazoo_config: ~p", [_DataLink, _KConfig])
    after 1000 ->
            _KConfig = application:stop('kazoo_config'),
            ?LOG_DEBUG(":: Stopped Kazoo FixtureDB, data_link: timeout kazoo_config: ~p", [_KConfig])
    end.
