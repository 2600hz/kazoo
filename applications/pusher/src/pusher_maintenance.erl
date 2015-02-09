%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Maintenance functions for all
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(pusher_maintenance).

-include("pusher.hrl").

-export([add_google_app/2, add_apple_app/2]).

-spec add_google_app(binary(), binary()) -> any().
add_google_app(AppId, Secret) ->
    whapps_config:set_node(?CONFIG_CAT, <<"google">>, Secret, AppId),
    'ok'.


-spec add_apple_app(binary(), binary()) -> any().
add_apple_app(AppId, Certfile) ->
    case file:read_file(Certfile) of
        {'ok', Binary} -> whapps_config:set_node(?CONFIG_CAT, <<"apple">>, Binary, AppId),
                          'ok';
        {'error', _} = Err -> Err
    end.
