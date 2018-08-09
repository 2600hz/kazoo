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
        ,change_file_log_level/2
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

-define(LOG(Fmt), begin lager:info(Fmt), io:format(Fmt ++ "~n") end).
-define(LOG(Fmt, Args), begin lager:info(Fmt, Args), io:format(Fmt ++ "~n", Args) end).

-spec change_console_log_level(log_level()) -> 'ok'.
change_console_log_level(L) when is_atom(L) ->
    Handlers = ['lager_console_backend'
               ,{'lager_file_backend', "log/console.log"}
               ],
    update_log_level(Handlers, L);
change_console_log_level(L) ->
    change_console_log_level(kz_term:to_atom(L)).

-spec change_error_log_level(log_level()) -> 'ok'.
change_error_log_level(L) when is_atom(L) ->
    update_log_level([{'lager_file_backend', "log/error.log"}], L);
change_error_log_level(L) ->
    change_error_log_level(kz_term:to_atom(L)).

-spec change_syslog_log_level(log_level()) -> 'ok'.
change_syslog_log_level(L) when is_atom(L) ->
    update_log_level([{'lager_syslog_backend',{"2600hz",'local0'}}], L);
change_syslog_log_level(L) ->
    change_syslog_log_level(kz_term:to_atom(L)).

-spec change_file_log_level(kz_term:text(), log_level()) -> 'ok'.
change_file_log_level(File, Level) ->
    update_log_level([{'lager_file_backend', kz_term:to_list(File)}], Level).

update_log_level([], _Level) -> 'ok';
update_log_level([Backend|Backends], Level) ->
    case lager:get_loglevel(Backend) of
        {'error', 'bad_module'} -> 'ok';
        Level ->
            ?LOG("handler ~p already logging at ~p", [Backend, Level]);
        _OldLevel ->
            ?LOG("handler ~p now logging at ~p (was ~p)", [Backend, Level, _OldLevel]),
            lager:set_loglevel(Backend, Level)
    end,
    update_log_level(Backends, Level).
