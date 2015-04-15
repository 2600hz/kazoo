%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(hon_util).

-export([candidate_rates/3, candidate_rates/4
         ,matching_rates/2, matching_rates/3
         ,sort_rates/1
         ,rate_dbs/1
        ]).

-ifdef(TEST).
-export([build_keys/1]).
-endif.

-include("hotornot.hrl").

-define(MIN_PREFIX_LEN, 1). % how many chars to strip off the e164 DID

-spec candidate_rates(ne_binary(), api_binary(), ne_binary()) ->
                             {'ok', wh_json:objects()} |
                             {'error', atom()}.
-spec candidate_rates(ne_binary(), api_binary(), ne_binary(), binary()) ->
                             {'ok', wh_json:objects()} |
                             {'error', atom()}.
candidate_rates(AccountId, Direction, ToDID) ->
    candidate_rates(AccountId, Direction, ToDID, <<>>).
candidate_rates(AccountId, Direction, ToDID, FromDID) ->
    E164 = wnm_util:to_e164(ToDID),
    find_candidate_rates(AccountId, Direction, E164, FromDID).

find_candidate_rates(AccountId, Direction, E164, _FromDID) when byte_size(E164) > ?MIN_PREFIX_LEN ->
    Keys = build_keys(Direction, E164),
    RateDbs = rate_dbs(AccountId),
    case lists:foldl(fun acc_candidate_rates/2, {'ok', Keys, 0, []}, RateDbs) of
        {'ok', _, _, Rates} -> {'ok', Rates};
        E -> E
    end;
find_candidate_rates(_, _, DID, _) ->
    lager:debug("DID ~s is too short", [DID]),
    {'error', 'did_too_short'}.

build_keys(Direction, <<"+", E164/binary>>) ->
    build_keys(Direction, E164);
build_keys(Direction, <<D:1/binary, Rest/binary>>) ->
    build_keys(Direction, Rest, D, [[D, Direction]]).

build_keys(Direction, <<D:1/binary, Rest/binary>>, Prefix, Acc) ->
    build_keys(Direction, Rest, <<Prefix/binary, D/binary>>, [[<<Prefix/binary, D/binary>>, Direction] | Acc]);
build_keys(_, <<>>, _, Acc) -> Acc.

-spec rate_dbs(ne_binary()) -> list().
rate_dbs(AccountId) ->
    {ok, A} = couch_mgr:open_doc(?WH_ACCOUNTS_DB, AccountId),
    lists:foldl(fun acc_rate_dbs/2, [], lists:reverse([AccountId | kz_account:tree(A)]))
    ++ [?WH_RATES_DB].

-spec acc_rate_dbs(ne_binary(), list()) -> list().
acc_rate_dbs(AccountId, Acc) ->
    Db = wh_util:format_account_id(<<AccountId/binary, "-", ?WH_RATES_DB/binary>>, 'encoded'),
    case couch_mgr:db_exists(Db) of
        true -> [Db | Acc];
        false -> Acc
    end.

-spec acc_candidate_rates(ne_binary(), {'ok', list(), integer(), list()} | {'error', any()}) ->
    {'ok', list(), integer(), list()} | {'error', any()}.
acc_candidate_rates(RateDb, {'ok', Keys, Lvl, Acc}) ->
    case couch_mgr:get_results(RateDb, <<"rates/lookup">>, [{'keys', Keys}, 'include_docs']) of
        {'ok', ViewRows} ->
            {'ok', Keys, Lvl + 1, Acc ++ [normalize_rate(Rate, Lvl) || Rate <- ViewRows]};
        {'error', _}=E -> E
    end;
acc_candidate_rates(_Db, State) ->
    State.

normalize_rate(Rate, Lvl) ->
    Rate1 = wh_json:get_value(<<"value">>, Rate),
    Rate2 = wh_json:merge_jobjs(Rate1, wh_json:get_value(<<"doc">>, Rate)),
    wh_json:set_value(<<"lvl">>, Lvl, Rate2).

%% Given a list of rates, return the list of rates whose routes regexes match the given E164
%% Optionally include direction of the call and options from the client to match against the rate
-spec matching_rates(wh_json:objects(), ne_binary()) ->
                            wh_json:objects().
-spec matching_rates(wh_json:objects(), ne_binary(), trunking_options()) ->
                            wh_json:objects().
matching_rates(Rates, DID) ->
    matching_rates(Rates, DID, []).

matching_rates(Rates, DID, RouteOptions) ->
    E164 = wnm_util:to_e164(DID),
    [Rate || Rate <- Rates,
             matching_rate(Rate, E164, RouteOptions)
    ].

-spec sort_rates(wh_json:objects()) -> wh_json:objects().
sort_rates(Rates) ->
    lists:usort(fun sort_rate/2, Rates).

%% Private helper functions

%% Return whether the given rate is a candidate for the given DID
%% taking into account direction of the call and options the DID
%% needs to have available
-spec matching_rate(wh_json:object(), ne_binary(), trunking_options()) -> boolean().
matching_rate(Rate, E164, RouteOptions) ->
    matching_options(Rate, RouteOptions)
        andalso matching_routes(Rate, E164).

-spec matching_routes(wh_json:object(), ne_binary()) -> boolean().
matching_routes(Rate, E164) ->
    lists:any(fun(Regex) -> re:run(E164, Regex) =/= 'nomatch' end
              ,wh_json:get_value([<<"routes">>], Rate, [])
             ).

%% Return true if RateA has lower weight than RateB
-spec sort_rate(wh_json:object(), wh_json:object()) -> boolean().
sort_rate(RateA, RateB) ->
    PrefixA = byte_size(wh_json:get_binary_value(<<"prefix">>, RateA)),
    PrefixB = byte_size(wh_json:get_binary_value(<<"prefix">>, RateB)),

    case PrefixA =:= PrefixB of
        'true' ->
            LvlA = wh_json:get_integer_value(<<"lvl">>, RateA),
            LvlB = wh_json:get_integer_value(<<"lvl">>, RateB),
            case LvlA =:= LvlB of
                'true' ->
                    wh_json:get_integer_value(<<"weight">>, RateA, 100) >
                        wh_json:get_integer_value(<<"weight">>, RateB, 100);
                'false' ->
                    LvlA < LvlB
            end;
        'false' ->
            PrefixA > PrefixB
    end.

%% Route options come from the client device
%% Rate options come from the carrier providing the trunk
%% All Route options must exist in a carrier's options to keep the carrier
%% in the list of carriers capable of handling the call
-spec matching_options(wh_json:object(), trunking_options()) -> boolean().
matching_options(Rate, RouteOptions) ->
    options_match(wh_json:get_value([<<"options">>], Rate, []), RouteOptions).

-spec options_match(trunking_options(), trunking_options()) -> boolean().
options_match([], []) -> 'true';
options_match([], _) -> 'true';
options_match(RateOptions, RouteOptions) ->
    lists:all(fun(RouteOpt) ->
                      props:get_value(RouteOpt, RateOptions, 'false') =/= 'false'
              end
              ,RouteOptions
             ).
