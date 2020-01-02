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
-export([running_modules/0]).

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
