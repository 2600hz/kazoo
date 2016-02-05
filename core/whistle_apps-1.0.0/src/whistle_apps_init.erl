%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2016, 2600Hz
%%% @doc
%%% Init to be done
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(whistle_apps_init).

-export([start_link/0
         ,sanity_checks/0
         ,init/0
        ]).

-include("whistle_apps.hrl").

start_link() ->
    _ = sanity_checks(), %% one day make this true
    _ = wh_util:spawn(?MODULE, 'init', []),
    'ignore'.

init() ->
    wh_util:put_callid(?MODULE),

    case wh_config:get_atom('whistle_apps', 'cookie') of
        [] ->
            lager:warning("failed to set whistle_apps cookie trying node ~s", [node()]),
            [Name, _Host] = binary:split(wh_util:to_binary(node()), <<"@">>),
            case wh_config:get_atom(wh_util:to_atom(Name, 'true'), 'cookie') of
                [] ->
                    lager:warning("failed to set whistle_apps cookie for node ~s", [node()]);
                [Cookie|_] ->
                    erlang:set_cookie(erlang:node(), Cookie),
                    lager:info("setting whistle_apps cookie to ~p", [Cookie])
            end;
        [Cookie|_] ->
            erlang:set_cookie(erlang:node(), Cookie),
            lager:info("setting whistle_apps cookie to ~p", [Cookie])
    end,
    set_loglevel().

set_loglevel() ->
    [Console|_] = wh_config:get_atom('log', 'console', ['notice']),
    wh_util:change_console_log_level(Console),
    [Syslog|_] = wh_config:get_atom('log', 'syslog', ['info']),
    wh_util:change_syslog_log_level(Syslog),
    [Error|_] = wh_config:get_atom('log', 'error', ['error']),
    wh_util:change_error_log_level(Error),
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
            lager:critical("hostname resolution is painfully slow!!! This will cause "),
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
    {Time, _} = timer:tc('wh_network_utils', 'get_hostname', []),
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
