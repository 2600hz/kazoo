%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(kz_endpoints).

-export([by_owner_id/3, ignore_early_media/1]).

-include("kazoo_endpoint.hrl").

-spec by_owner_id(ne_binary(), kz_json:object(), kapps_call:call()) ->
                         kz_json:objects().
by_owner_id(OwnerId, Data, Call) ->
    lists:foldr(fun(EndpointId, Acc) ->
                        case kz_endpoint:build(EndpointId, Data, Call) of
                            {'ok', Endpoint} -> Endpoint ++ Acc;
                            {'error', _E} -> Acc
                        end
                end
               ,[]
               ,kz_attributes:owned_by(OwnerId, <<"device">>, Call)
               ).

-spec ignore_early_media(kz_json:objects()) -> api_binary().
ignore_early_media(Endpoints) ->
    case lists:any(fun(Endpoint) ->
                           kz_json:is_true(<<"Ignore-Early-Media">>, Endpoint)
                   end, Endpoints)
    of
        'true' -> <<"true">>;
        'false' -> 'undefined'
    end.
