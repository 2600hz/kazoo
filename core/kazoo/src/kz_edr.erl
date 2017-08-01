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

-export([log_event/4, log_event/5]).

-type log_event_return() :: 'ok' |
                            {'error', any()} |
                            {'returned', kz_json:object(), kz_json:object()}.

%% TODO: Decide on how log levels will work
-type log_level() :: atom().

-spec log_event(binary(), binary(), log_level(), kz_json:object()) -> log_event_return().
-spec log_event(binary(), binary(), log_level(), kz_json:object(), api_binary()) -> log_event_return().
log_event(AppName, AppVersion, LogLevel, Body) ->
    log_event(AppName, AppVersion, LogLevel, Body, 'undefined').
log_event(AppName, AppVersion, LogLevel, Body, AccountId) ->
    Req = [{<<"Account-ID">>, AccountId}
          ,{<<"Level">>, LogLevel}
          ,{<<"Body">>, Body}
           | kz_api:default_headers(AppName, AppVersion)
          ],
    kz_amqp_worker:cast(Req, fun kapi_edr:publish/1).
