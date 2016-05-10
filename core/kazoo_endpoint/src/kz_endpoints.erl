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

-export([by_owner_id/3]).

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
