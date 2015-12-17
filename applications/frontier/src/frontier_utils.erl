%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(frontier_utils).

-include("frontier.hrl").

%% API
-export([extract_realm/1
         ,extract_username/1
         ,is_device_entity/1
         ,is_device/1
         ,is_realm/1
         ,get_entity_type/1
        ]).

-spec get_entity_type(ne_binary()) -> ne_binary().
get_entity_type(Entity) ->
    case binary:split(Entity, <<"@">>) of
        [_, _] -> <<"device">>;
        [_] -> <<"realm">>
    end.

-spec is_device_entity(ne_binary()) -> boolean().
is_device_entity(Entity) ->
    Realm = ?MODULE:extract_realm(Entity),
    Realm =/= Entity.

-spec extract_realm(ne_binary()) -> ne_binary().
extract_realm(Entity) ->
    case binary:split(Entity, <<"@">>) of
        [_, OnRealm] -> OnRealm;
        [JustRealm] -> JustRealm
    end.

-spec extract_username(ne_binary()) -> api_binary().
extract_username(Entity) ->
    case binary:split(Entity, <<"@">>) of
        [Username, _] -> Username;
        [_JustRealm] -> 'undefined'
    end.

-spec is_device(wh_json:object()) -> boolean().
is_device(JObj) ->
    <<"device">> =:= wh_json:get_value([<<"value">>, <<"type">>], JObj).

-spec is_realm(wh_json:object()) -> boolean().
is_realm(JObj) ->
    <<"realm">> =:= wh_json:get_value([<<"value">>, <<"type">>], JObj).
