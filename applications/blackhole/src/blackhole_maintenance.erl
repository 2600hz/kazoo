%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(blackhole_maintenance).


-export([start_module/1, start_module/2]).
-export([stop_module/1, stop_module/2]).
-export([running_modules/0]).

-include("blackhole.hrl").
-include_lib("kazoo/include/kz_system_config.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec start_module(text()) -> 'ok'.
-spec start_module(text(), text() | boolean()) -> 'ok'.
start_module(ModuleBin) ->
    start_module(ModuleBin, 'true').
start_module(ModuleBin, Persist) ->
    Req = [{<<"Module">>, ModuleBin}
          ,{<<"Action">>, <<"start">>}
          ,{<<"Persist">>, kz_term:to_boolean(Persist)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kz_amqp_worker:call_collect(Req
                                    ,fun kapi_blackhole:publish_module_req/1
                                    ,{'blackhole', fun kapi_blackhole:module_resp_v/1, 'true'}
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec stop_module(text()) -> 'ok'.
-spec stop_module(text(), text() | boolean()) -> 'ok'.
stop_module(ModuleBin) ->
    stop_module(ModuleBin, 'true').
stop_module(ModuleBin, Persist) ->
    Req = [{<<"Module">>, ModuleBin}
          ,{<<"Action">>, <<"stop">>}
          ,{<<"Persist">>, kz_term:to_boolean(Persist)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kz_amqp_worker:call_collect(Req
                                    ,fun kapi_blackhole:publish_module_req/1
                                    ,{'blackhole', fun kapi_blackhole:module_resp_v/1, 'true'}
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec running_modules() -> atoms().
running_modules() -> blackhole_bindings:modules_loaded().

-spec print_module_resp(kz_json:object()) -> 'ok'.
print_module_resp(JObj) ->
    Fields = kz_api:remove_defaults(JObj),
    io:format("node ~s returned:~n", [kz_api:node(JObj)]),
    kz_json:foreach(fun print_field_resp/1, Fields).

-spec print_field_resp({kz_json:key(), kz_json:json_term()}) -> 'ok'.
print_field_resp({Field, Value}) ->
    io:format("  ~s: ~s~n", [Field, Value]).
