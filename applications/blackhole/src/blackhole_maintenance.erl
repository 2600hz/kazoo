%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(blackhole_maintenance).

-export([start_module/1, start_module/2]).
-export([stop_module/1, stop_module/2]).
-export([running_modules/0
        ,active_sessions/0
        ,active_sessions_by_ip/1
        ,active_sessions_by_account/1
        ]).

-include("blackhole.hrl").
-include_lib("kazoo/include/kz_system_config.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec start_module(kz_term:text()) -> 'ok'.
start_module(ModuleBin) ->
    start_module(ModuleBin, 'true').

-spec start_module(kz_term:text(), kz_term:text() | boolean()) -> 'ok'.
start_module(ModuleBin, Persist) ->
    Req = [{<<"Module">>, ModuleBin}
          ,{<<"Action">>, <<"start">>}
          ,{<<"Persist">>, kz_term:to_boolean(Persist)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kz_amqp_worker:call_collect(Req
                                    ,fun kapi_websockets:publish_module_req/1
                                    ,{'blackhole', fun kapi_websockets:module_resp_v/1, 'true'}
                                    )
    of
        {'ok', JObjs} ->
            io:format("starting ~s:~n", [ModuleBin]),
            lists:foreach(fun print_module_resp/1, JObjs);
        {'timeout', JObjs} ->
            io:format("timed out waiting for responses~n"),
            [io:format("resp: ~s~n", [kz_json:encode(JObj)]) || JObj <- JObjs], 'ok';
        {'error', _E} ->
            io:format("failed to start module ~s: ~p~n", [ModuleBin, _E])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec stop_module(kz_term:text()) -> 'ok'.
stop_module(ModuleBin) ->
    stop_module(ModuleBin, 'true').

-spec stop_module(kz_term:text(), kz_term:text() | boolean()) -> 'ok'.
stop_module(ModuleBin, Persist) ->
    Req = [{<<"Module">>, ModuleBin}
          ,{<<"Action">>, <<"stop">>}
          ,{<<"Persist">>, kz_term:to_boolean(Persist)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kz_amqp_worker:call_collect(Req
                                    ,fun kapi_websockets:publish_module_req/1
                                    ,{'blackhole', fun kapi_websockets:module_resp_v/1, 'true'}
                                    )
    of
        {'ok', JObjs} ->
            io:format("stopping ~s:~n", [ModuleBin]),
            lists:foreach(fun print_module_resp/1, JObjs);
        {'timeout', JObjs} ->
            io:format("timed out waiting for responses~n"),
            [io:format("resp: ~s~n", [kz_json:encode(JObj)]) || JObj <- JObjs], 'ok';
        {'error', _E} ->
            io:format("failed to stop module ~s: ~p~n", [ModuleBin, _E])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec running_modules() -> kz_term:atoms().
running_modules() -> blackhole_bindings:modules_loaded().

-spec print_module_resp(kz_json:object()) -> 'ok'.
print_module_resp(JObj) ->
    Fields = kz_api:remove_defaults(JObj),
    io:format("node ~s returned:~n", [kz_api:node(JObj)]),
    kz_json:foreach(fun print_field_resp/1, Fields).

-spec print_field_resp({kz_json:key(), kz_json:json_term()}) -> 'ok'.
print_field_resp({Field, Value}) ->
    io:format("  ~s: ~s~n", [Field, Value]).

-spec active_sessions() -> 'ok'.
active_sessions() ->
    Contexts = blackhole_tracking:get_contexts(),
    print_sessions(Contexts).

-spec active_sessions_by_ip(kz_term:ne_binary()) -> 'ok'.
active_sessions_by_ip(<<IPAddr/binary>>) ->
    Sessions = blackhole_tracking:get_contexts_by_ip(IPAddr),
    print_sessions(Sessions).

-spec active_sessions_by_account(kz_term:ne_binary()) -> 'ok'.
active_sessions_by_account(<<AccountId/binary>>) ->
    Sessions = blackhole_tracking:get_contexts_by_account_id(AccountId),
    print_sessions(Sessions).

-spec print_sessions([bh_context:context()]) -> 'ok'.
print_sessions([]) -> io:format("no active sessions~n");
print_sessions(Sessions) ->
    io:format("Active websocket connections:~n"),
    io:format("~22s | ~10s | ~12s | ~32s~n", [<<"Peer">>, <<"Uptime">>, <<"PID">>, <<"Account-ID">>]),
    lists:foreach(fun print_session/1, Sessions).

print_session(Context) ->
    io:format("~22s | ~10s | ~12s | ~32s~n"
             ,[bh_context:websocket_session_id(Context)
              ,kz_time:pretty_print_elapsed_s(kz_time:elapsed_s(bh_context:timestamp(Context)))
              ,kz_term:to_binary(bh_context:websocket_pid(Context))
              ,bh_context:auth_account_id(Context)
              ]).
