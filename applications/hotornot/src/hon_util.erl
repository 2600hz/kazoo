%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(hon_util).

-export([candidate_rates/1
        ,matching_rates/2
        ,sort_rates/1

        ,use_trie/0
        ]).

-ifdef(TEST).
-export([build_keys/1]).
-endif.

-include("hotornot.hrl").

-define(MIN_PREFIX_LEN, 1). % how many chars to strip off the e164 DID
-define(BOTH_DIRECTIONS, [<<"inbound">>, <<"outbound">>]).

-spec use_trie() -> boolean().
use_trie() ->
    kapps_config:get_is_true(?APP_NAME, <<"use_trie">>, 'false').

-spec candidate_rates(ne_binary()) ->
                             {'ok', kz_json:objects()} |
                             {'error', atom()}.
candidate_rates(ToDID) ->
    E164 = knm_converters:normalize(ToDID),
    find_candidate_rates(E164).

-spec find_candidate_rates(ne_binary()) ->
                                  {'ok', kz_json:objects()} |
                                  {'error', atom()}.
find_candidate_rates(E164)
  when byte_size(E164) > ?MIN_PREFIX_LEN ->
    case use_trie() of
        'false' -> fetch_candidate_rates(E164);
        'true' -> find_trie_rates(E164)
    end;
find_candidate_rates(DID) ->
    lager:debug("DID ~s is too short", [DID]),
    {'error', 'did_too_short'}.

-spec find_trie_rates(api_binary()) ->
                             {'ok', kz_json:objects()} |
                             {'error', atom()}.
find_trie_rates(E164) ->
    case hon_trie:match_did(only_numeric(E164)) of
        {'ok', Result} -> {'ok', Result};
        {'error', _E} ->
            lager:warning("got error while searching did in trie, falling back to DB search"),
            fetch_candidate_rates(E164)
    end.

-spec fetch_candidate_rates(ne_binary()) ->
                                   {'ok', kz_json:objects()} |
                                   {'error', atom()}.
fetch_candidate_rates(E164) ->
    Keys = build_keys(E164),

    lager:debug("searching for prefixes for ~s: ~p", [E164, Keys]),
    case Keys =/= []
        andalso kz_datamgr:get_results(?KZ_RATES_DB
                                      ,<<"rates/lookup">>
                                      ,[{'keys', Keys}
                                       ,'include_docs'
                                       ]
                                      )
    of
        'false' -> {'error', 'did_too_short'};
        {'ok', []}=OK -> OK;
        {'error', _}=E -> E;
        {'ok', ViewRows} ->
            {'ok'
            ,[kz_json:get_value(<<"doc">>, ViewRow)
              || ViewRow <- ViewRows
             ]
            }
    end.

-spec build_keys(ne_binary()) -> [integer()].
build_keys(Number) ->
    case only_numeric(Number) of
        <<>> -> [];
        <<D:1/binary, Rest/binary>> ->
            build_keys(Rest, D, [kz_util:to_integer(D)])
    end.

-spec only_numeric(binary()) -> binary().
only_numeric(Number) ->
    << <<N>> || <<N>> <= Number, is_numeric(N)>>.

-spec is_numeric(integer()) -> boolean().
is_numeric(N) ->
    N >= $0
        andalso N =< $9.

-spec build_keys(binary(), ne_binary(), [integer()]) -> [integer()].
build_keys(<<D:1/binary, Rest/binary>>, Prefix, Acc) ->
    build_keys(Rest, <<Prefix/binary, D/binary>>, [kz_util:to_integer(<<Prefix/binary, D/binary>>) | Acc]);
build_keys(<<>>, _, Acc) -> Acc.

-spec matching_rates(kz_json:objects(), kz_json:object()) ->
                            kz_json:objects().
matching_rates(Rates, ReqJObj) ->
    FilterList = kapps_config:get(?APP_NAME, <<"filter_list">>, ?DEFAULT_FILTER_LIST),
    lists:foldl(fun(Filter, Acc) ->
                        lists:filter(fun(R) -> matching_rate(R, Filter, ReqJObj) end, Acc)
                end
               ,Rates
               ,FilterList
               ).

-spec sort_rates(kz_json:objects()) -> kz_json:objects().
sort_rates(Rates) ->
    case kapps_config:get_is_true(?APP_NAME, <<"sort_by_weight">>, 'true') of
        'true' -> lists:usort(fun sort_rate_by_weight/2, Rates);
        'false' -> lists:usort(fun sort_rate_by_cost/2, Rates)
    end.

%% Private helper functions

-spec matching_rate(kz_json:object(), ne_binary(), kz_json:object()) -> boolean().
matching_rate(Rate, <<"direction">>, JObj) ->
    case kz_json:get_value(<<"Direction">>, JObj) of
        'undefined' -> 'true';
        Direction ->
            lists:member(Direction
                        ,lists:flatten([kz_json:get_value(<<"direction">>, Rate, ?BOTH_DIRECTIONS)])
                        )
    end;

matching_rate(Rate, <<"route_options">>, JObj) ->
    RouteOptions = kz_json:get_value(<<"Options">>, JObj, []),
    RouteFlags   = kz_json:get_value(<<"Outbound-Flags">>, JObj, []),
    ResourceFlag = case kz_json:get_value(<<"Account-ID">>, JObj) of
                       'undefined' -> [];
                       AccountId -> maybe_add_resource_flag(JObj, AccountId)
                   end,
    options_match(kz_json:get_value(<<"options">>, Rate, []), RouteOptions++RouteFlags++ResourceFlag);

matching_rate(Rate, <<"routes">>, JObj) ->
    E164 = knm_converters:normalize(kz_json:get_value(<<"To-DID">>, JObj)),
    lists:any(fun(Regex) -> re:run(E164, Regex) =/= 'nomatch' end
             ,kz_json:get_value([<<"routes">>], Rate, [])
             );

matching_rate(Rate, <<"ratedeck_name">>, JObj) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    AccountRatedeck = kz_service_ratedeck_name:get_ratedeck_name(AccountId),
    RatedeckName = kz_json:get_value(<<"ratedeck_name">>, Rate),
    AccountRatedeck =:= RatedeckName;

matching_rate(Rate, <<"reseller">>, JObj) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    ResellerId = kz_services:find_reseller_id(AccountId),
    RateAccountId = kz_json:get_value(<<"account_id">>, Rate),
    RateAccountId =:= ResellerId;

matching_rate(Rate, <<"version">>, _JObj) ->
    RateVersion = kz_json:get_binary_value(<<"rate_version">>, Rate),
    ConfigVersion = kapps_config:get_binary(?APP_NAME, <<"rate_version">>),
    RateVersion =:= ConfigVersion;

matching_rate(_Rate, _FilterType, _ReqJObj) -> 'false'.

%% Return true if RateA has lower weight than RateB
-spec sort_rate_by_weight(kz_json:object(), kz_json:object()) -> boolean().
sort_rate_by_weight(RateA, RateB) ->
    PrefixA = byte_size(kz_json:get_binary_value(<<"prefix">>, RateA)),
    PrefixB = byte_size(kz_json:get_binary_value(<<"prefix">>, RateB)),

    case PrefixA =:= PrefixB of
        'true' ->
            kz_json:get_integer_value(<<"weight">>, RateA, 100) <
                kz_json:get_integer_value(<<"weight">>, RateB, 100);
        'false' ->
            PrefixA > PrefixB
    end.

-spec sort_rate_by_cost(kz_json:object(), kz_json:object()) -> boolean().
sort_rate_by_cost(RateA, RateB) ->
    PrefixA = byte_size(kz_json:get_binary_value(<<"prefix">>, RateA)),
    PrefixB = byte_size(kz_json:get_binary_value(<<"prefix">>, RateB)),

    case PrefixA =:= PrefixB of
        'true' ->
            kz_json:get_float_value(<<"rate_cost">>, RateA, 0) >
                kz_json:get_float_value(<<"rate_cost">>, RateB, 0);
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
options_match(RateOptions, RouteOptions) ->
    lists:all(fun(RouteOption) ->
                      props:get_value(RouteOption, RateOptions, 'false') =/= 'false'
              end
             ,RouteOptions
             ).

-spec maybe_add_resource_flag(kz_json:object(), ne_binary()) -> kz_proplist().
maybe_add_resource_flag(JObj, AccountId) ->
    case kapps_account_config:get_from_reseller(AccountId, ?APP_NAME, <<"filter_by_resource_id">>, 'false') of
        'true' ->
            case kz_json:get_value(<<"Resource-ID">>, JObj) of
                'undefined' -> [];
                ResourceId -> [ResourceId]
            end;
        'false' -> []
    end.
