%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
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

-include("hotornot.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(MIN_PREFIX_LEN, 1). % how many chars to strip off the e164 DID
-define(BOTH_DIRECTIONS, [<<"inbound">>, <<"outbound">>]).

-spec candidate_rates(ne_binary()) ->
                             {'ok', wh_json:objects()} |
                             {'error', atom()}.
-spec candidate_rates(ne_binary(), binary()) ->
                             {'ok', wh_json:objects()} |
                             {'error', atom()}.
candidate_rates(ToDID) ->
    candidate_rates(ToDID, <<>>).
candidate_rates(ToDID, FromDID) ->
    E164 = wnm_util:to_e164(ToDID),
    find_candidate_rates(E164, FromDID).

find_candidate_rates(E164, _FromDID) when byte_size(E164) > ?MIN_PREFIX_LEN ->
    Keys = build_keys(E164),

    lager:debug("searching for prefixes for ~s: ~p", [E164, Keys]),
    case couch_mgr:get_results(?WH_RATES_DB, <<"rates/lookup">>, [{'keys', Keys}
                                                                  ,'include_docs'
                                                                 ])
    of
        {'ok', []}=OK -> OK;
        {'ok', ViewRows} ->
            {'ok', [wh_json:get_value(<<"doc">>, ViewRow) || ViewRow <- ViewRows]};
        {'error', _}=E -> E
    end;
find_candidate_rates(DID, _) ->
    lager:debug("DID ~s is too short", [DID]),
    {'error', 'did_too_short'}.

build_keys(<<"+", E164/binary>>) ->
    build_keys(E164);
build_keys(<<D:1/binary, Rest/binary>>) ->
    build_keys(Rest, D, [wh_util:to_integer(D)]).

build_keys(<<D:1/binary, Rest/binary>>, Prefix, Acc) ->
    build_keys(Rest, <<Prefix/binary, D/binary>>, [wh_util:to_integer(<<Prefix/binary, D/binary>>) | Acc]);
build_keys(<<>>, _, Acc) -> Acc.

%% Given a list of rates, return the list of rates whose routes regexes match the given E164
%% Optionally include direction of the call and options from the client to match against the rate
-spec matching_rates(wh_json:objects(), ne_binary()) ->
                            wh_json:objects().
-spec matching_rates(wh_json:objects(), ne_binary(), api_binary(), trunking_options()) ->
                            wh_json:objects().
matching_rates(Rates, DID) ->
    matching_rates(Rates, DID, 'undefined', []).

matching_rates(Rates, DID, Direction, RouteOptions) ->
    E164 = wnm_util:to_e164(DID),
    [Rate || Rate <- Rates,
             matching_rate(Rate, E164, Direction, RouteOptions)
    ].

-spec sort_rates(wh_json:objects()) -> wh_json:objects().
sort_rates(Rates) ->
    lists:usort(fun sort_rate/2, Rates).

%% Private helper functions

%% Return whether the given rate is a candidate for the given DID
%% taking into account direction of the call and options the DID
%% needs to have available
-spec matching_rate(wh_json:object(), ne_binary(), 'undefined' | ne_binary(), trunking_options()) -> boolean().
matching_rate(Rate, E164, Direction, RouteOptions) ->
    (Direction =:= 'undefined' orelse lists:member(Direction, wh_json:get_value([<<"direction">>], Rate, ?BOTH_DIRECTIONS)))
        andalso options_match(RouteOptions, wh_json:get_value([<<"options">>], Rate, []))
        andalso lists:any(fun(Regex) -> re:run(E164, Regex) =/= 'nomatch' end
                          ,wh_json:get_value([<<"routes">>], Rate, [])
                         ).

%% Return true if RateA has lower weight than RateB
-spec sort_rate(wh_json:object(), wh_json:object()) -> boolean().
sort_rate(RateA, RateB) ->
    PrefixA = byte_size(wh_json:get_binary_value(<<"prefix">>, RateA)),
    PrefixB = byte_size(wh_json:get_binary_value(<<"prefix">>, RateB)),

    case PrefixA =:= PrefixB of
        'true' ->
            wh_json:get_integer_value(<<"weight">>, RateA, 100) >
                wh_json:get_integer_value(<<"weight">>, RateB, 100);
        'false' ->
            PrefixA > PrefixB
    end.

%% Route options come from the client device
%% Rate options come from the carrier providing the trunk
%% All Route options must exist in a carrier's options to keep the carrier
%% in the list of carriers capable of handling the call
-spec options_match(trunking_options(), trunking_options()) -> boolean().
options_match([], []) -> 'true';
options_match([], _) -> 'true';
options_match(RouteOptions, RateOptions) ->
    lists:all(fun(RouteOpt) -> props:get_value(RouteOpt, RateOptions, 'false') =/= 'false' end, RouteOptions).

-ifdef(TEST).
build_keys_test() ->
    ?assertEqual([1], build_keys(<<"1">>)),
    ?assertEqual([12, 1], build_keys(<<"12">>)),
    ?assertEqual([123, 12, 1], build_keys(<<"123">>)).

-endif.

