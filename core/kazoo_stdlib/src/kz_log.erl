%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_log).

-export([change_console_log_level/1
        ,change_error_log_level/1
        ,change_syslog_log_level/1
        ,change_file_log_level/2

        ,log_stacktrace/0, log_stacktrace/1, log_stacktrace/2, log_stacktrace/3
        ,put_callid/1, get_callid/0, find_callid/1
        ,kz_log_md_clear/0, kz_log_md_put/2
        ]).

-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_amqp/include/kz_api_literals.hrl").

-export_type([log_level/0]).

-deprecated({'log_stacktrace', 0, 'next_major_release'}).
-deprecated({'log_stacktrace', 2, 'next_major_release'}).

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

%%------------------------------------------------------------------------------
%% @doc Standardized way of logging the stack-trace.
%% @deprecated `erlang:get_stacktrace/0' used by this function is deprecated
%% in OTP 21, please use the new try/catch syntax and pass stacktrace to
%% {@link kz_log:log_stacktrace/1} instead.
%% @end
%%------------------------------------------------------------------------------
-spec log_stacktrace() -> 'ok'.
log_stacktrace() ->
    try throw('get_stacktrace')
    catch
        ?STACKTRACE(_E, _R, ST)
        log_stacktrace(ST, "log_stacktrace/0 is deprecated: ", [])
        end.

%%------------------------------------------------------------------------------
%% @doc Standardized way of logging the stack-trace.
%% @end
%%------------------------------------------------------------------------------
-spec log_stacktrace(list()) -> 'ok'.
log_stacktrace(ST) ->
    log_stacktrace(ST, "", []).

%%------------------------------------------------------------------------------
%% @doc Standardized way of logging the stack-trace.
%% @deprecated `erlang:get_stacktrace/0' used by this function is deprecated
%% in OTP 21, please use the new try/catch syntax and pass stacktrace to
%% {@link kz_log:log_stacktrace/3} instead.
%% @end
%%------------------------------------------------------------------------------
-spec log_stacktrace(string(), list()) -> 'ok'.
log_stacktrace(Fmt, Args) ->
    try throw('get_stacktrace')
    catch
        ?STACKTRACE(_E, _R, ST)
        log_stacktrace(ST, "log_stacktrace/2 is deprecated: " ++ Fmt, Args)
        end.

%%------------------------------------------------------------------------------
%% @doc Standardized way of logging the stack-trace.
%% @end
%%------------------------------------------------------------------------------
-spec log_stacktrace(list(), string(), list()) -> 'ok'.
log_stacktrace(ST, Fmt, Args) ->
    ?LOG_ERROR("stacktrace: " ++ Fmt, Args),
    _ = [log_stacktrace_mfa(M, F, A, Info)
         || {M, F, A, Info} <- ST
        ],
    'ok'.

log_stacktrace_mfa(M, F, Arity, Info) when is_integer(Arity) ->
    ?LOG_ERROR("st: ~s:~s/~b at (~b)", [M, F, Arity, props:get_value('line', Info, 0)]);
log_stacktrace_mfa(M, F, Args, Info) ->
    ?LOG_ERROR("st: ~s:~s at ~p", [M, F, props:get_value('line', Info, 0)]),
    lists:foreach(fun (Arg) -> ?LOG_ERROR("args: ~p", [Arg]) end, Args).

%%------------------------------------------------------------------------------
%% @doc Given an JSON Object extracts the `Call-ID' into the processes
%% dictionary, failing that the `Msg-ID' and finally a generic.
%% @end
%%------------------------------------------------------------------------------
-spec put_callid(kz_json:object() | kz_term:proplist() | kz_term:ne_binary() | atom()) -> 'ok'.
put_callid(?NE_BINARY = CallId) ->
    _ = kz_log_md_put('callid', CallId),
    _ = erlang:put('callid', CallId),
    'ok';
put_callid(Atom) when is_atom(Atom) ->
    _ = kz_log_md_put('callid', Atom),
    _ = erlang:put('callid', Atom),
    'ok';
put_callid(APITerm) ->
    put_callid(find_callid(APITerm)).

-spec get_callid() -> kz_term:api_ne_binary().
get_callid() -> erlang:get('callid').

-spec find_callid(kz_term:api_terms()) -> kz_term:api_binary().
find_callid(APITerm) when is_list(APITerm) ->
    find_callid(APITerm, fun props:get_first_defined/3);
find_callid(APITerm) ->
    find_callid(APITerm, fun kz_json:get_first_defined/3).

-spec find_callid(kz_term:api_terms(), fun()) -> kz_term:api_binary().
find_callid(APITerm, GetFun) ->
    GetFun([?KEY_LOG_ID, ?KEY_API_CALL_ID, ?KEY_MSG_ID]
          ,APITerm
          ,?DEFAULT_LOG_SYSTEM_ID
          ).

-spec kz_log_md_put(atom(), any()) -> any().
kz_log_md_put(K, V) ->
    lager:md(lists:usort(fun is_kz_log_md_equal/2, [{K, V} | lager:md()])).

is_kz_log_md_equal({K1, _}, {K2, _}) -> K1 =< K2;
is_kz_log_md_equal(K1, K2) -> K1 =< K2.

-define(LAGER_MD_KEY, '__lager_metadata').

-spec kz_log_md_clear() -> 'ok'.
kz_log_md_clear() ->
    %% `lager:md([])' causing dialyzer to complain:
    %% warn_failing_call
    %% `kz_util.erl:408: The call lager:md([]) breaks the contract ([{atom(),any()},...]) -> ok`'
    %% lager:md([]).
    _ = erlang:put(?LAGER_MD_KEY, []),
    'ok'.
