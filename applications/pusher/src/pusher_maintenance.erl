%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc Maintenance functions for all
%%% @author Luis Azedo
%%% @end
%%%-----------------------------------------------------------------------------
-module(pusher_maintenance).

-include("pusher.hrl").

-export([add_firebase_app/2
        ,add_apple_app/2, add_apple_app/3
        ]).

-spec add_firebase_app(binary(), binary()) -> 'ok'.
add_firebase_app(AppId, Secret) ->
    kapps_config:set_node(?CONFIG_CAT, [<<"firebase">>, <<"api_key">>], Secret, AppId),
    'ok'.

-spec add_apple_app(binary(), binary()) -> 'ok' | {'error', any()}.
add_apple_app(AppId, Certfile) ->
    add_apple_app(AppId, Certfile, ?DEFAULT_APNS_HOST).

-spec add_apple_app(binary(), binary(), binary()) -> 'ok' | {'error', any()}.
add_apple_app(AppId, Certfile, Host) ->
    case file:read_file(Certfile) of
        {'ok', Binary} ->
            kapps_config:set_node(?CONFIG_CAT, [<<"apple">>, <<"certificate">>], Binary, AppId),
            kapps_config:set_node(?CONFIG_CAT, [<<"apple">>, <<"host">>], Host, AppId),
            'ok';
        {'error', _} = Err -> Err
    end.
