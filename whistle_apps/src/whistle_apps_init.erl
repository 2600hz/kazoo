%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Init to be done
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(whistle_apps_init).

-export([start_link/0
         ,init/0
        ]).

-include("whistle_apps.hrl").

start_link() -> spawn(?MODULE, 'init', []), 'ignore'.

init() ->
    put('callid', ?MODULE),
    case wh_config:get_atom('whistle_apps', 'cookie') of
        [] ->
            lager:warning("failed to set whistle_apps cookie ~n", []);
        [Cookie|_] ->
            erlang:set_cookie(erlang:node(), Cookie),
            lager:info("setting whistle_apps cookie to ~p~n", [Cookie])
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
