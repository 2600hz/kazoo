%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(hon_util).

-export([candidate_rates/1, candidate_rates/2
         ,matching_rates/2, matching_rates/4
         ,sort_rates/1
        ]).

-ifdef(TEST).
-export([build_keys/1]).
-endif.

-include("hotornot.hrl").

-define(MIN_PREFIX_LEN, 1). % how many chars to strip off the e164 DID
-define(BOTH_DIRECTIONS, [<<"inbound">>, <<"outbound">>]).

-spec candidate_rates(ne_binary()) ->
                             {'ok', kz_json:objects()} |
                             {'error', atom()}.
-spec candidate_rates(ne_binary(), binary()) ->
                             {'ok', kz_json:objects()} |
                             {'error', atom()}.
candidate_rates(ToDID) ->
    candidate_rates(ToDID, <<>>).
candidate_rates(ToDID, FromDID) ->
    E164 = knm_converters:normalize(ToDID),
    find_candidate_rates(E164, FromDID).

find_candidate_rates(E164, _FromDID) when byte_size(E164) > ?MIN_PREFIX_LEN ->
    Keys = build_keys(E164),

    lager:debug("searching for prefixes for ~s: ~p", [E164, Keys]),
    case kz_datamgr:get_results(?KZ_RATES_DB
                               ,<<"rates/lookup">>
                               ,[{'keys', Keys}
                                 ,'include_docs'
                                ]
                              )
    of
        {'ok', []}=OK -> OK;
        {'error', _}=E -> E;
        {'ok', ViewRows} ->
            {'ok'
             ,[kz_json:get_value(<<"doc">>, ViewRow)
               || ViewRow <- ViewRows
              ]
            }
    end;
find_candidate_rates(DID, _) ->
    lager:debug("DID ~s is too short", [DID]),
    {'error', 'did_too_short'}.

build_keys(<<"+", E164/binary>>) ->
    build_keys(E164);
build_keys(<<D:1/binary, Rest/binary>>) ->
    build_keys(Rest, D, [kz_util:to_integer(D)]).

build_keys(<<D:1/binary, Rest/binary>>, Prefix, Acc) ->
    build_keys(Rest, <<Prefix/binary, D/binary>>, [kz_util:to_integer(<<Prefix/binary, D/binary>>) | Acc]);
build_keys(<<>>, _, Acc) -> Acc.

%% Given a list of rates, return the list of rates whose routes regexes match the given E164
%% Optionally include direction of the call and options from the client to match against the rate
-spec matching_rates(kz_json:objects(), ne_binary()) ->
                            kz_json:objects().
-spec matching_rates(kz_json:objects(), ne_binary(), api_binary(), trunking_options()) ->
                            kz_json:objects().
matching_rates(Rates, DID) ->
    matching_rates(Rates, DID, 'undefined', []).

matching_rates(Rates, DID, Direction, RouteOptions) ->
    E164 = knm_converters:normalize(DID),
    [Rate || Rate <- Rates,
             matching_rate(Rate, E164, Direction, RouteOptions)
    ].

-spec sort_rates(kz_json:objects()) -> kz_json:objects().
sort_rates(Rates) ->
    lists:usort(fun sort_rate/2, Rates).

%% Private helper functions

%% Return whether the given rate is a candidate for the given DID
%% taking into account direction of the call and options the DID
%% needs to have available
-spec matching_rate(kz_json:object(), ne_binary(), api_binary(), trunking_options()) -> boolean().
matching_rate(Rate, E164, Direction, RouteOptions) ->
    matching_direction(Rate, Direction)
        andalso matching_options(Rate, RouteOptions)
        andalso matching_routes(Rate, E164).

-spec matching_routes(kz_json:object(), ne_binary()) -> boolean().
matching_routes(Rate, E164) ->
    lists:any(fun(Regex) -> re:run(E164, Regex) =/= 'nomatch' end
              ,kz_json:get_value([<<"routes">>], Rate, [])
             ).

-spec matching_direction(kz_json:object(), api_binary()) -> boolean().
matching_direction(_Rate, 'undefined') ->
    'true';
matching_direction(Rate, Direction) ->
    lists:member(Direction
                 ,kz_json:get_value([<<"direction">>], Rate, ?BOTH_DIRECTIONS)
                ).

%% Return true if RateA has lower weight than RateB
-spec sort_rate(kz_json:object(), kz_json:object()) -> boolean().
sort_rate(RateA, RateB) ->
    PrefixA = byte_size(kz_json:get_binary_value(<<"prefix">>, RateA)),
    PrefixB = byte_size(kz_json:get_binary_value(<<"prefix">>, RateB)),

    case PrefixA =:= PrefixB of
        'true' ->
            kz_json:get_integer_value(<<"weight">>, RateA, 100) <
                kz_json:get_integer_value(<<"weight">>, RateB, 100);
        'false' ->
            PrefixA > PrefixB
    end.

%% Route options come from the client device
%% Rate options come from the carrier providing the trunk
%% All Route options must exist in a carrier's options to keep the carrier
%% in the list of carriers capable of handling the call
-spec matching_options(kz_json:object(), trunking_options()) -> boolean().
matching_options(Rate, RouteOptions) ->
    options_match(kz_json:get_value([<<"options">>], Rate, []), RouteOptions).

-spec options_match(trunking_options(), trunking_options()) -> boolean().
options_match([], []) -> 'true';
options_match([], _) -> 'true';
options_match(RateOptions, RouteOptions) ->
    lists:all(fun(RouteOption) ->
                      props:get_value(RouteOption, RateOptions, 'false') =/= 'false'
              end
              ,RouteOptions
             ).
