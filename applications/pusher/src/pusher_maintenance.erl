%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2017, 2600Hz
%%% @doc
%%% Maintenance functions for all
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(pusher_maintenance).

-include("pusher.hrl").

-export([add_google_app/2
        ,add_apple_app/2, add_apple_app/3
        ]).
-export([flush/0]).

-spec add_google_app(binary(), binary()) -> 'ok'.
add_google_app(AppId, Secret) ->
    kapps_config:set_node(?CONFIG_CAT, <<"google">>, Secret, AppId),
    'ok'.

-spec add_apple_app(binary(), binary()) -> 'ok' | {'error', any()}.
-spec add_apple_app(binary(), binary(), binary()) -> 'ok' | {'error', any()}.
add_apple_app(AppId, Certfile) ->
    add_apple_app(AppId, Certfile, ?DEFAULT_APNS_HOST).

add_apple_app(AppId, Certfile, Host) ->
    case file:read_file(Certfile) of
        {'ok', Binary} ->
            kapps_config:set_node(?CONFIG_CAT, [<<"apple">>, <<"certificate">>], Binary, AppId),
            kapps_config:set_node(?CONFIG_CAT, [<<"apple">>, <<"host">>], Host, AppId),
            'ok';
        {'error', _} = Err -> Err
    end.

-spec flush() -> any().
flush() ->
    kz_cache:flush_local(?CACHE_NAME).
