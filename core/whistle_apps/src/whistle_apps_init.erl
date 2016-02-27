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

start_link() ->
    _ = wh_util:spawn(fun init/0),
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
                    erlang:set_cookie(node(), Cookie),
                    lager:info("setting whistle_apps cookie to ~p", [Cookie])
            end;
        [Cookie|_] ->
            erlang:set_cookie(node(), Cookie),
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
