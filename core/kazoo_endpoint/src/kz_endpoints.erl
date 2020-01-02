%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @author Luis Azedo
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_endpoints).

-export([by_owner_id/3
        ,ignore_early_media/1
        ]).

-include("kazoo_endpoint.hrl").

-spec by_owner_id(kz_term:ne_binary(), kz_json:object(), kapps_call:call()) ->
          kz_json:objects().
by_owner_id(OwnerId, Data, Call) ->
    by_owner_id(OwnerId, Data, Call, ?MODNAME).

by_owner_id(OwnerId, Data, Call, 'kz_endpoint_v4') ->
    lists:foldr(fun(EndpointId, Acc) ->
                        case kz_endpoint_v4:build(EndpointId, Data, Call) of
                            {'ok', Endpoint} -> Endpoint ++ Acc;
                            {'error', _E} -> Acc
                        end
                end
               ,[]
               ,kz_attributes:owned_by(OwnerId, <<"device">>, Call)
               );
by_owner_id(OwnerId, Data, Call, 'kz_endpoint_v5') ->
    case kz_endpoint_v5:build(OwnerId, Data, Call) of
        {'ok', Endpoint} -> Endpoint;
        {'error', _E} -> []
    end.

-spec ignore_early_media(kz_json:objects()) -> kz_term:api_binary().
ignore_early_media(Endpoints) ->
    case lists:any(fun(Endpoint) ->
                           kz_json:is_true(<<"Ignore-Early-Media">>, Endpoint)
                   end, Endpoints)
    of
        'true' -> <<"true">>;
        'false' -> 'undefined'
    end.
