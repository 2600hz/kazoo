%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2016, 2600Hz
%%% @doc
%%% Init to be done
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_init).

-export([start_link/0
         ,init/0
        ]).

-include("ecallmgr.hrl").

-define(CONSOLE_LOG_LEVEL
        ,{<<"console_log_level">>
          ,fun kz_util:change_console_log_level/1
          ,kz_config:get_atom('log', 'console', ['notice'])
         }).
-define(SYSLOG_LOG_LEVEL
        ,{<<"syslog_log_level">>
          ,fun kz_util:change_syslog_log_level/1
          ,kz_config:get_atom('log', 'syslog', ['info'])
         }).
-define(ERROR_LOG_LEVEL
        ,{<<"error_log_level">>
          ,fun kz_util:change_error_log_level/1
          ,kz_config:get_atom('log', 'error', ['error'])
         }).

-spec start_link() -> startlink_ret().
start_link() ->
    kz_util:spawn(fun init/0),
    'ignore'.

init() ->
    kz_util:put_callid(?MODULE),
    case kz_config:get_atom('ecallmgr', 'cookie') of
        [] ->
            lager:info("no cookie defined for ecallmgr, leaving as ~s", [erlang:get_cookie()]);
        [Cookie|_] ->
            OldCookie = erlang:get_cookie(),
            erlang:set_cookie(erlang:node(), Cookie),
            lager:info("setting ecallmgr cookie to ~p (from ~s)", [Cookie, OldCookie])
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
