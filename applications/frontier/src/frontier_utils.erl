%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
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

-spec get_entity_type(kz_term:ne_binary()) -> kz_term:ne_binary().
get_entity_type(Entity) ->
    case binary:split(Entity, <<"@">>) of
        [_, _] -> <<"device">>;
        [_] -> <<"realm">>
    end.

-spec is_device_entity(kz_term:ne_binary()) -> boolean().
is_device_entity(Entity) ->
    Realm = extract_realm(Entity),
    Realm =/= Entity.

-spec extract_realm(kz_term:ne_binary()) -> kz_term:ne_binary().
extract_realm(Entity) ->
    case binary:split(Entity, <<"@">>) of
        [_, OnRealm] -> OnRealm;
        [JustRealm] -> JustRealm
    end.

-spec extract_username(kz_term:ne_binary()) -> kz_term:api_binary().
extract_username(Entity) ->
    case binary:split(Entity, <<"@">>) of
        [Username, _] -> Username;
        [_JustRealm] -> 'undefined'
    end.

-spec is_device(kz_json:object()) -> boolean().
is_device(JObj) ->
    <<"device">> =:= kz_json:get_value([<<"value">>, <<"type">>], JObj).

-spec is_realm(kz_json:object()) -> boolean().
is_realm(JObj) ->
    <<"realm">> =:= kz_json:get_value([<<"value">>, <<"type">>], JObj).
