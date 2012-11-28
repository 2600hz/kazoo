%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
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

-define(MIN_PREFIX_LEN, 1). % how many chars to strip off the e164 DID
-define(BOTH_DIRECTIONS, [<<"inbound">>, <<"outbound">>]).

-spec candidate_rates/1 :: (ne_binary()) ->
                                   {'ok', wh_json:json_objects()} |
                                   {'error', atom()}.
-spec candidate_rates/2 :: (ne_binary(), binary()) ->
                                   {'ok', wh_json:json_objects()} |
                                   {'error', atom()}.
candidate_rates(ToDID) ->
    candidate_rates(ToDID, <<>>).
candidate_rates(ToDID, FromDID) ->
    E164 = wnm_util:to_e164(ToDID),
    find_candidate_rates(E164, FromDID).

find_candidate_rates(E164, _FromDID) when byte_size(E164) > ?MIN_PREFIX_LEN ->
    Start = get_prefix(?MIN_PREFIX_LEN, E164),
    End = get_suffix(E164),

    lager:debug("searching for rates in the range ~s to ~s", [Start, End]),
    case couch_mgr:get_results(?WH_RATES_DB, <<"rates/lookup">>, [{startkey, Start}
                                                                  ,{endkey, End}
                                                                 ]) of
        {ok, []}=OK -> OK;
        {ok, ViewRows} -> {ok, [wh_json:get_value(<<"value">>, ViewRow) || ViewRow <- ViewRows]};
        {error, _}=E -> E
    end;
find_candidate_rates(DID, _) ->
    lager:debug("DID ~s is too short", [DID]),
    {error, did_too_short}.

%% Given a list of rates, return the list of rates whose routes regexes match the given E164
%% Optionally include direction of the call and options from the client to match against the rate
-spec matching_rates/2 :: (wh_json:json_objects(), ne_binary()) -> wh_json:json_objects().
-spec matching_rates/4 :: (wh_json:json_objects(), ne_binary(), 'undefined' | ne_binary(), trunking_options()) -> wh_json:json_objects().

matching_rates(Rates, DID) ->
    matching_rates(Rates, DID, undefined, []).

matching_rates(Rates, DID, Direction, RouteOptions) ->
    E164 = wnm_util:to_e164(DID),
    [Rate || Rate <- Rates,
             matching_rate(Rate, E164, Direction, RouteOptions)
    ].


-spec sort_rates/1 :: (wh_json:json_objects()) -> wh_json:json_objects().
sort_rates(Rates) ->
    lists:usort(fun sort_rate/2, Rates).

%% Private helper functions

%% Return whether the given rate is a candidate for the given DID
%% taking into account direction of the call and options the DID
%% needs to have available
-spec matching_rate/4 :: (wh_json:json_object(), ne_binary(), 'undefined' | ne_binary(), trunking_options()) -> boolean().
matching_rate(Rate, E164, Direction, RouteOptions) ->
    (Direction =:= undefined orelse lists:member(Direction, wh_json:get_value([<<"direction">>], Rate, ?BOTH_DIRECTIONS)))
        andalso options_match(RouteOptions, wh_json:get_value([<<"options">>], Rate, []))
        andalso lists:any(fun(Regex) -> re:run(E164, Regex) =/= nomatch end, wh_json:get_value([<<"routes">>], Rate, [])).

%% Return true if RateA has lower weight than RateB
-spec sort_rate/2 :: (wh_json:json_object(), wh_json:json_object()) -> boolean().
sort_rate(RateA, RateB) ->
    wh_json:get_integer_value(<<"weight">>, RateA, 100) =< wh_json:get_integer_value(<<"weight">>, RateB, 100).

%% Route options come from the client device
%% Rate options come from the carrier providing the trunk
%% All Route options must exist in a carrier's options to keep the carrier
%% in the list of carriers capable of handling the call
-spec options_match/2 :: (trunking_options(), trunking_options()) -> boolean().
options_match([], []) -> true;
options_match([], _) -> true;
options_match(RouteOptions, RateOptions) ->
    lists:all(fun(RouteOpt) -> props:get_value(RouteOpt, RateOptions, false) =/= false end, RouteOptions).

-spec get_prefix/2 :: (pos_integer(), ne_binary()) -> ne_binary().
get_prefix(MinLen, <<"+", Bin/binary>>) -> get_prefix(MinLen, Bin);
get_prefix(MinLen, Bin) -> <<Start:MinLen/binary, _/binary>> = Bin, Start.

-spec get_suffix/1 :: (ne_binary()) -> ne_binary().
get_suffix(<<"+", End/binary>>) -> End;
get_suffix(End) -> End.
