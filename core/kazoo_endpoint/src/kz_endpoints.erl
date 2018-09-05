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
        ,lift_common_properties/1, lift_common_properties/2
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
lift_common_properties(Endpoints) ->
    lift_common_properties(Endpoints, []).

-spec lift_common_properties(kz_json:objects(), kz_json:paths()) ->
                                    {kz_json:object(), kz_json:objects()}.
lift_common_properties([], _Unliftable) -> {kz_json:new(), []};
lift_common_properties([Endpoint], _Unliftable) -> {kz_json:new(), [Endpoint]};
lift_common_properties([Endpoint | Endpoints], Unliftable) ->
    EndpointProperties = endpoint_properties(Endpoint, Unliftable),
    CommonProperties = lift_common_properties(Endpoints, Unliftable, EndpointProperties),

    {kz_json:expand(CommonProperties)
    ,remove_common_properties([Endpoint | Endpoints], CommonProperties)
    }.

lift_common_properties(_Endpoints, _Unliftable, []) -> kz_json:new();
lift_common_properties([], _Unliftable, CommonProperties) ->
    kz_json:from_list(CommonProperties);
lift_common_properties([Endpoint|Endpoints], Unliftable, CommonProperties) ->
    EndpointProperties = endpoint_properties(Endpoint, Unliftable),
    lift_common_properties(Endpoints, Unliftable, intersection(CommonProperties, EndpointProperties)).

intersection(CommonProperties, EndpointProperties) ->
    sets:to_list(sets:intersection(sets:from_list(CommonProperties), sets:from_list(EndpointProperties))).

endpoint_properties(Endpoint, []) ->
    lists:usort(kz_json:to_proplist(kz_json:flatten(Endpoint)));
endpoint_properties(Endpoint, Unliftable) ->
    Properties = endpoint_properties(Endpoint, []),
    lists:foldl(fun remove_unliftable/2, Properties, Unliftable).

remove_unliftable([_|_]=Path, Properties) ->
    lists:filter(fun(Property) -> should_remove_unliftable(Property, Path) end
                ,Properties
                );
remove_unliftable(Key, Properties) ->
    remove_unliftable([Key], Properties).

should_remove_unliftable({Path, _}, Unliftable) ->
    not lists:prefix(Unliftable, Path).

remove_common_properties(Endpoints, CommonProperties) ->
    kz_json:foldl(fun remove_common_property/3, Endpoints, CommonProperties).

remove_common_property(Path, _Value, Endpoints) ->
    lists:map(fun(E) -> kz_json:delete_key(Path, E, 'prune') end
             ,Endpoints
             ).
