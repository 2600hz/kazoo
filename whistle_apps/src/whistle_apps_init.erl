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
    spawn(?MODULE, 'init', []),
    'ignore'.

init() ->
    put('callid', ?MODULE),
    case wh_config:get_atom('bigcouch', 'cookie') of
        [Cookie|_] ->
            erlang:set_cookie(erlang:node(), Cookie),
            lager:info("setting whistle_apps cookie to ~p~n", [Cookie]);
        [] ->
            lager:warning("failed to set whistle_apps cookie ~n", [])
    end,
    [set_loglevel(K, F, D) || {K, F, D} <- [{<<"console_log_level">>, fun wh_util:change_console_log_level/1, 'notice'}
                                            ,{<<"error_log_level">>, fun wh_util:change_error_log_level/1, 'error'}
                                            ,{<<"syslog_log_level">>, fun wh_util:change_syslog_log_level/1, 'info'}
                                           ]].

set_loglevel(K, F, D) ->
    try whapps_config:get_atom(<<"whapps_controller">>, K, D) of
        L -> F(L), lager:info("updated ~s to level ~s", [K, L])
    catch
        _T:_R -> lager:info("failed to update ~s: ~s:~p", [K, _T, _R])
    end.
