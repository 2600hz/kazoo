%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_log).

-export([change_console_log_level/1
        ,change_error_log_level/1
        ,change_syslog_log_level/1
        ]).

-export_type([log_level/0]).

-define(LOG_LEVELS, ['emergency'
                    ,'alert'
                    ,'critical'
                    ,'error'
                    ,'warning'
                    ,'notice'
                    ,'info'
                    ,'debug'
                    ]).
-type log_level() :: 'emergency'
                   | 'alert'
                   | 'critical'
                   | 'error'
                   | 'warning'
                   | 'notice'
                   | 'info'
                   | 'debug'
                   | kz_term:ne_binary().


-spec change_console_log_level(log_level()) -> 'ok'.
change_console_log_level(L) when is_atom(L) ->
    lager:info("updated console_log to level ~s", [L]),
    lager:set_loglevel('lager_console_backend', L);
change_console_log_level(L) ->
    change_console_log_level(kz_term:to_atom(L)).

-spec change_error_log_level(log_level()) -> 'ok'.
change_error_log_level(L) when is_atom(L) ->
    lager:info("updated error_log to level ~s", [L]),
    lager:set_loglevel({'lager_file_backend', "log/error.log"}, L);
change_error_log_level(L) ->
    change_error_log_level(kz_term:to_atom(L)).

-spec change_syslog_log_level(log_level()) -> 'ok'.
change_syslog_log_level(L) when is_atom(L) ->
    lager:info("updated syslog_log to level ~s", [L]),
    lager:set_loglevel({'lager_syslog_backend',{"2600hz",'local0'}}, L);
change_syslog_log_level(L) ->
    change_syslog_log_level(kz_term:to_atom(L)).
