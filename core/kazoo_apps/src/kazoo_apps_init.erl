%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Init to be done
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kazoo_apps_init).

-export([start_link/0
         ,init/0
        ]).

-include("kazoo_apps.hrl").

start_link() ->
    _ = kz_util:spawn(fun init/0),
    'ignore'.

init() ->
    kz_util:put_callid(?MODULE),
    case kz_config:get_atom('kazoo_apps', 'cookie') of
        [] ->
            lager:warning("failed to set kazoo_apps cookie trying node ~s", [node()]),
            [Name, _Host] = binary:split(kz_term:to_binary(node()), <<"@">>),
            case kz_config:get_atom(kz_term:to_atom(Name, 'true'), 'cookie') of
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
