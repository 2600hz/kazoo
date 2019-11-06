%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Init to be done.
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_apps_init).

-export([start_link/0
        ,sanity_checks/0
        ,init/0
        ]).

-include("kazoo_apps.hrl").

-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    _ = sanity_checks(), %% one day make this true
    _ = kz_process:spawn(fun init/0),
    'ignore'.

-spec init() -> 'ok'.
init() ->
    kz_log:put_callid(?MODULE),
    set_cookie(),
    start_distribution(),
    set_loglevel().

-spec set_cookie() -> 'true'.
set_cookie() ->
    Cookie = maybe_cookie_from_env(),
    lager:info("setting ~s cookie to ~p", [?APP, Cookie]),
    erlang:set_cookie(node(), Cookie).

-spec maybe_cookie_from_env() -> atom().
maybe_cookie_from_env() ->
    case os:getenv("KAZOO_COOKIE", "noenv") of
        "noenv" -> cookie_from_ini();
        Cookie -> kz_term:to_atom(Cookie, 'true')
    end.

-spec cookie_from_ini() -> atom().
cookie_from_ini() ->
    case kz_config:get_atom(?APP_NAME, <<"cookie">>) of
        [] ->
            [Name, _Host] = binary:split(kz_term:to_binary(node()), <<"@">>),
            case kz_config:get_atom(Name, <<"cookie">>) of
                [] ->
                    lager:warning("failed to get cookie for node ~s, generating one", [node()]),
                    kz_term:to_atom(kz_binary:rand_hex(16), 'true');
                [Cookie|_] -> Cookie
            end;
        [Cookie|_] -> Cookie
    end.

-spec set_loglevel() -> 'ok'.
set_loglevel() ->
    [Console|_] = kz_config:get_atom(<<"log">>, <<"console">>, ['notice']),
    kz_log:change_console_log_level(Console),
    [Syslog|_] = kz_config:get_atom(<<"log">>, <<"syslog">>, ['info']),
    kz_log:change_syslog_log_level(Syslog),
    [Error|_] = kz_config:get_atom(<<"log">>, <<"error">>, ['error']),
    kz_log:change_error_log_level(Error).

-spec sanity_checks() -> boolean().
sanity_checks() ->
    lists:all(fun(F) -> F() end
             ,[fun does_hostname_resolve_speedily/0
              ,fun is_system_clock_on_utc/0
              ]
             ).

-spec does_hostname_resolve_speedily() -> boolean().
does_hostname_resolve_speedily() ->
    HowManyTests = 100,
    {Min, Max, Total, Timings} = time_hostname_resolutions(HowManyTests),
    Mean = (Total div (HowManyTests+1)),
    Median = lists:nth(HowManyTests div 2, lists:sort(Timings)),

    IsSlow =
        Max > 5000
        andalso Median > 1000
        andalso log_slow_resolution(Min, Max, Mean, Median),
    not IsSlow.

-spec log_slow_resolution(pos_integer(), pos_integer(), pos_integer(), pos_integer()) -> 'true'.
log_slow_resolution(Min, Max, Mean, Median) ->
    lager:warning("hostname resolution(in us): Min/Median/Mean/Max ~p/~p/~p/~p"
                 ,[Min, Median, Mean, Max]
                 ),
    lager:critical("hostname resolution is painfully slow!!! all config lookups rely on this being fast"),
    'true'.


-type resolutions() :: {Min :: pos_integer(), Max :: pos_integer(), Total:: pos_integer(), Timings :: [pos_integer()]}.

-spec time_hostname_resolutions(pos_integer()) -> resolutions().
time_hostname_resolutions(HowManyTests) ->
    time_hostname_resolutions(HowManyTests, time_hostname_resolution()).

-spec time_hostname_resolutions(pos_integer(), pos_integer()) -> resolutions().
time_hostname_resolutions(HowManyTests, InitTime) ->
    lists:foldl(fun time_hostname_resolution/2
               ,{InitTime, InitTime, InitTime, []}
               ,lists:seq(1, HowManyTests)
               ).

-spec time_hostname_resolution(any(), resolutions()) ->
                                      resolutions().
time_hostname_resolution(_TestNo, {Min, Max, Total, Timings}) ->
    case time_hostname_resolution() of
        Time when Time < Min ->
            {Time, Max, Total+Time, [Time | Timings]};
        Time when Time > Max ->
            {Min, Time, Total+Time, [Time | Timings]};
        Time ->
            {Min, Max, Total+Time, [Time | Timings]}
    end.

-spec time_hostname_resolution() -> pos_integer().
time_hostname_resolution() ->
    {Time, _} = timer:tc(fun kz_network_utils:get_hostname/0),
    Time.

-spec is_system_clock_on_utc() -> boolean().
is_system_clock_on_utc() ->
    case {calendar:local_time(), calendar:universal_time()} of
        {UTC, UTC} -> 'true';
        {_Local, _UTC} ->
            lager:warning("local: ~p UTC: ~p", [_Local, _UTC]),
            lager:critical("system is not running in UTC and Kazoo expects it"),
            'false'
    end.

-spec start_distribution() -> 'ok'.
start_distribution() ->
    amqp_dist:add_brokers(kz_amqp_connections:uris()).