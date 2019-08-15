%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc Maintenance functions for all
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(pusher_maintenance).

-include("pusher.hrl").

-export([add_google_app/2
        ,add_apple_app/2, add_apple_app/3
        ]).

-spec add_google_app(binary(), binary()) -> 'ok'.
add_google_app(AppId, Secret) ->
    kapps_config:set_node(?CONFIG_CAT, <<"google">>, Secret, AppId),
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
