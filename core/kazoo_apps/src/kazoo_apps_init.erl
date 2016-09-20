%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% Init to be done
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kazoo_apps_init).

-export([start_link/0
        ,sanity_checks/0
        ,init/0
        ]).

-include("kazoo_apps.hrl").

-spec start_link() -> startlink_ret().
start_link() ->
    _ = sanity_checks(), %% one day make this true
    _ = kz_util:spawn(fun init/0),
    'ignore'.

-spec init() -> 'ok'.
init() ->
    kz_util:put_callid(?MODULE),
    case kz_config:get_atom('kazoo_apps', 'cookie') of
        [] ->
            lager:warning("failed to set kazoo_apps cookie trying node ~s", [node()]),
            [Name, _Host] = binary:split(kz_util:to_binary(node()), <<"@">>),
            case kz_config:get_atom(kz_util:to_atom(Name, 'true'), 'cookie') of
                [] ->
                    lager:warning("failed to set kazoo_apps cookie for node ~s", [node()]);
                [Cookie|_] ->
                    erlang:set_cookie(node(), Cookie),
                    lager:info("setting kazoo_apps cookie to ~p", [Cookie])
            end;
        [Cookie|_] ->
            erlang:set_cookie(node(), Cookie),
            lager:info("setting kazoo_apps cookie to ~p", [Cookie])
    end,
    set_loglevel().

set_loglevel() ->
    [Console|_] = kz_config:get_atom('log', 'console', ['notice']),
    kz_util:change_console_log_level(Console),
    [Syslog|_] = kz_config:get_atom('log', 'syslog', ['info']),
    kz_util:change_syslog_log_level(Syslog),
    [Error|_] = kz_config:get_atom('log', 'error', ['error']),
    kz_util:change_error_log_level(Error),
    'ok'.


-spec sanity_checks() -> boolean().
sanity_checks() ->
    lists:all(fun(F) -> F() end
             ,[fun does_hostname_resolve_speedily/0
              ,fun is_system_clock_is_utc/0
              ]
             ).

-spec does_hostname_resolve_speedily() -> boolean().
does_hostname_resolve_speedily() ->
    InitTime = time_hostname_resolution(),
    Tests = 20,
    {Min, Max, Total} =
        lists:foldl(fun time_hostname_resolution/2
                   ,{InitTime, InitTime, InitTime}
                   ,lists:seq(1, Tests)
                   ),
    case Max < 5000 of
        'true' -> 'true';
        'false' ->
            lager:warning("hostname results (in us): ~p < ~p < ~p"
                         ,[Min, (Total div (Tests+1)), Max]
                         ),
            lager:critical("hostname resolution is painfully slow!!! all config lookups rely on this being fast"),
            'false'
    end.

-spec time_hostname_resolution(any(), {pos_integer(), pos_integer(), pos_integer()}) ->
                                      {pos_integer(), pos_integer(), pos_integer()}.
time_hostname_resolution(_, {Min, Max, Total}) ->
    case time_hostname_resolution() of
        Time when Time < Min ->
            {Time, Max, Total+Time};
        Time when Time > Max ->
            {Min, Time, Total+Time};
        Time ->
            {Min, Max, Total+Time}
    end.

-spec time_hostname_resolution() -> pos_integer().
time_hostname_resolution() ->
    {Time, _} = timer:tc('kz_network_utils', 'get_hostname', []),
    Time.

-spec is_system_clock_is_utc() -> boolean().
is_system_clock_is_utc() ->
    case {calendar:local_time(), calendar:universal_time()} of
        {UTC, UTC} -> 'true';
        {_Local, _UTC} ->
            lager:warning("local: ~p utc: ~p", [_Local, _UTC]),
            lager:critical("system is not running in UTC and Kazoo expects it"),
            'false'
    end.
