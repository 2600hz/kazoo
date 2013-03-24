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

    [set_loglevel(K, F, D)
     || {K, F, D} <- [?CONSOLE_LOG_LEVEL
                      ,?ERROR_LOG_LEVEL
                      ,?SYSLOG_LOG_LEVEL
                     ]].

set_loglevel(K, F, D) ->
    try wh_util:to_atom(ecallmgr_config:get(K, D)) of
        L -> F(L), lager:info("updated ~s to level ~s", [K, L])
    catch
        _T:_R -> lager:info("failed to update ~s: ~s:~p", [K, _T, _R])
    end.
