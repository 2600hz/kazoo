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

start_link() ->
    spawn(?MODULE, 'init', []),
    'ignore'.

init() ->
    put('callid', ?MODULE),
    case wh_config:get_atom('bigcouch', 'cookie') of
        [Cookie|_] ->
            erlang:set_cookie(erlang:node(), Cookie),
            lager:info("setting ecallmgr cookie to ~p~n", [Cookie]);
        [] ->
            lager:warning("failed to set ecallmgr cookie ~n", [])
    end,
    [set_loglevel(K, F, D) || {K, F, D} <- [{<<"console_log_level">>, fun wh_util:change_console_log_level/1, 'notice'}
                                            ,{<<"error_log_level">>, fun wh_util:change_error_log_level/1, 'error'}
                                            ,{<<"syslog_log_level">>, fun wh_util:change_syslog_log_level/1, 'info'}
                                           ]].

set_loglevel(K, F, D) ->
    try wh_util:to_atom(ecallmgr_config:get(K, D)) of
        L -> F(L), lager:info("updated ~s to level ~s", [K, L])
    catch
        _T:_R -> lager:info("failed to update ~s: ~s:~p", [K, _T, _R])
    end.
