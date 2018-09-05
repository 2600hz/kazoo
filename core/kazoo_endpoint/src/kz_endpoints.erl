%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @author Luis Azedo
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_endpoints).

-export([by_owner_id/3
        ,ignore_early_media/1
        ,lift_common_properties/1
        ]).

-include("kazoo_endpoint.hrl").

-spec by_owner_id(kz_term:ne_binary(), kz_json:object(), kapps_call:call()) ->
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

-spec ignore_early_media(kz_json:objects()) -> kz_term:api_binary().
ignore_early_media(Endpoints) ->
    case lists:any(fun(Endpoint) ->
                           kz_json:is_true(<<"Ignore-Early-Media">>, Endpoint)
                   end, Endpoints)
    of
        'true' -> <<"true">>;
        'false' -> 'undefined'
    end.

%%%-----------------------------------------------------------------------------
%% @doc Lifts shared key/value pairs out for use in global settings
%%
%% @end
%%%-----------------------------------------------------------------------------
-spec lift_common_properties(kz_json:objects()) ->
                                    {kz_json:object(), kz_json:objects()}.
lift_common_properties([]) -> {[], []};
lift_common_properties([Endpoint]) -> {[], [Endpoint]};
lift_common_properties([Endpoint | Endpoints]) ->
    EndpointProperties = endpoint_properties(Endpoint),
    CommonProperties = lift_common_properties(Endpoints, EndpointProperties),
    {kz_json:expand(CommonProperties), remove_common_properties([Endpoint | Endpoints], CommonProperties)}.

lift_common_properties(_Endpoints, []) -> kz_json:new();
lift_common_properties([], CommonProperties) ->
    kz_json:from_list(CommonProperties);
lift_common_properties([Endpoint|Endpoints], CommonProperties) ->
    EndpointProperties = endpoint_properties(Endpoint),
    lift_common_properties(Endpoints, intersection(CommonProperties, EndpointProperties)).

intersection(CommonProperties, EndpointProperties) ->
    sets:to_list(sets:intersection(sets:from_list(CommonProperties), sets:from_list(EndpointProperties))).

endpoint_properties(Endpoint) ->
    lists:usort(kz_json:to_proplist(kz_json:flatten(Endpoint))).

remove_common_properties(Endpoints, CommonProperties) ->
    kz_json:foldl(fun remove_common_property/3, Endpoints, CommonProperties).

remove_common_property(Path, _Value, Endpoints) ->
    lists:map(fun(E) -> kz_json:delete_key(Path, E, 'prune') end
             ,Endpoints
             ).
