%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% EDR in-core API
%%% @end
%%% @contributors
%%%    SIPLABS, LLC (Vorontsov Nikita) <info@siplabs.ru>
%%%-------------------------------------------------------------------
-module(kz_edr).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").

-export([fatal/3, fatal/4
        ,error/3, error/4
        ,warn/3, warn/4
        ,info/3, info/4
        ,debug/3, debug/4
        ,trace/3, trace/4
        ]).

-type log_event_return() :: 'ok' |
                            {'error', any()} |
                            {'returned', kz_json:object(), kz_json:object()}.

-type edr_level() :: 'fatal' | 'error' | 'warn' | 'info' | 'debug' | 'trace'.

-spec fatal(binary(), binary(), kz_json:object()) -> log_event_return().
-spec fatal(binary(), binary(), kz_json:object(), api_binary()) -> log_event_return().
fatal(AppName, AppVersion, Body) ->
    log_event(AppName, AppVersion, 'fatal', Body).
fatal(AppName, AppVersion, Body, AccountId) ->
    log_event(AppName, AppVersion, 'fatal', Body, AccountId).

-spec error(binary(), binary(), kz_json:object()) -> log_event_return().
-spec error(binary(), binary(), kz_json:object(), api_binary()) -> log_event_return().
error(AppName, AppVersion, Body) ->
    log_event(AppName, AppVersion, 'error', Body).
error(AppName, AppVersion, Body, AccountId) ->
    log_event(AppName, AppVersion, 'error', Body, AccountId).

-spec warn(binary(), binary(), kz_json:object()) -> log_event_return().
-spec warn(binary(), binary(), kz_json:object(), api_binary()) -> log_event_return().
warn(AppName, AppVersion, Body) ->
    log_event(AppName, AppVersion, 'warn', Body).
warn(AppName, AppVersion, Body, AccountId) ->
    log_event(AppName, AppVersion, 'warn', Body, AccountId).

-spec info(binary(), binary(), kz_json:object()) -> log_event_return().
-spec info(binary(), binary(), kz_json:object(), api_binary()) -> log_event_return().
info(AppName, AppVersion, Body) ->
    log_event(AppName, AppVersion, 'info', Body).
info(AppName, AppVersion, Body, AccountId) ->
    log_event(AppName, AppVersion, 'info', Body, AccountId).

-spec debug(binary(), binary(), kz_json:object()) -> log_event_return().
-spec debug(binary(), binary(), kz_json:object(), api_binary()) -> log_event_return().
debug(AppName, AppVersion, Body) ->
    log_event(AppName, AppVersion, 'debug', Body).
debug(AppName, AppVersion, Body, AccountId) ->
    log_event(AppName, AppVersion, 'debug', Body, AccountId).

-spec trace(binary(), binary(), kz_json:object()) -> log_event_return().
-spec trace(binary(), binary(), kz_json:object(), api_binary()) -> log_event_return().
trace(AppName, AppVersion, Body) ->
    log_event(AppName, AppVersion, 'trace', Body).
trace(AppName, AppVersion, Body, AccountId) ->
    log_event(AppName, AppVersion, 'trace', Body, AccountId).

-spec log_event(binary(), binary(), edr_level(), kz_json:object()) -> log_event_return().
-spec log_event(binary(), binary(), edr_level(), kz_json:object(), api_binary()) -> log_event_return().
log_event(AppName, AppVersion, LogLevel, Body) ->
    log_event(AppName, AppVersion, LogLevel, Body, 'undefined').
log_event(AppName, AppVersion, LogLevel, Body, AccountId) ->
    Req = [{<<"Account-ID">>, AccountId}
          ,{<<"Body">>, Body}
          ,{<<"ID">>, kz_datamgr:get_uuid()}
          ,{<<"Level">>, LogLevel}
          ,{<<"Timestamp">>, kz_time:now_s(kz_time:now())}
           | kz_api:default_headers(<<"edr">>, <<"event">>, AppName, AppVersion)
          ],
    kz_amqp_worker:cast(Req, fun kapi_edr:publish/1).
