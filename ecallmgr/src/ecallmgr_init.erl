%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
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
          ,fun wh_util:change_console_log_level/1
          ,wh_config:get_atom('log', 'console', 'notice')
         }).
-define(SYSLOG_LOG_LEVEL
        ,{<<"syslog_log_level">>
          ,fun wh_util:change_syslog_log_level/1
          ,wh_config:get_atom('log', 'syslog', 'info')
         }).
-define(ERROR_LOG_LEVEL
        ,{<<"error_log_level">>
          ,fun wh_util:change_error_log_level/1
          ,wh_config:get_atom('log', 'error', 'error')
         }).

start_link() -> spawn(?MODULE, 'init', []), 'ignore'.

init() ->
    put('callid', ?MODULE),
    set_loglevel().

set_loglevel() ->
    [Console|_] = wh_config:get_atom('log', 'console', 'notice'),
    wh_util:change_console_log_level(Console),
    [Syslog|_] = wh_config:get_atom('log', 'syslog', 'info'),
    wh_util:change_syslog_log_level(Syslog),
    [Error|_] = wh_config:get_atom('log', 'error', 'error'),
    wh_util:change_error_log_level(Error),
    'ok'.
