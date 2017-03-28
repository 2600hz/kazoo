%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(hon_util).

-export([candidate_rates/1, candidate_rates/2, candidate_rates/3
        ,matching_rates/2
        ,sort_rates/1
        ,account_ratedeck/1, account_ratedeck/2
        ]).

-ifdef(TEST).
-export([build_keys/1]).
-endif.

-include("hotornot.hrl").

-define(MIN_PREFIX_LEN, 1). % how many chars to strip off the e164 DID

-type candidate_rates_return() :: {'ok', kzd_rate:docs()} |
                                  {'error', 'did_to_short'} |
                                  kz_datamgr:data_error().

-spec candidate_rates(ne_binary()) ->
                             candidate_rates_return().
-spec candidate_rates(ne_binary(), api_ne_binary()) ->
                             candidate_rates_return().
-spec candidate_rates(ne_binary(), api_ne_binary(), api_ne_binary()) ->
                             candidate_rates_return().
candidate_rates(ToDID) ->
    candidate_rates(ToDID, 'undefined', 'undefined').

candidate_rates(ToDID, AccountId) ->
    candidate_rates(ToDID, AccountId, 'undefined').

candidate_rates(ToDID, AccountId, RatedeckId) ->
    E164 = knm_converters:normalize(ToDID),
    find_candidate_rates(E164, AccountId, RatedeckId).

-spec find_candidate_rates(ne_binary(), api_ne_binary(), api_ne_binary()) ->
                                  candidate_rates_return().
find_candidate_rates(E164, AccountId, RatedeckId)
  when byte_size(E164) > ?MIN_PREFIX_LEN ->
    case hotornot_config:should_use_trie() of
        'false' -> fetch_candidate_rates(E164, AccountId, RatedeckId);
        'true' -> find_trie_rates(E164, AccountId, RatedeckId)
    end;
find_candidate_rates(DID, _AccountId, _RatedeckId) ->
    lager:debug("DID ~s is too short", [DID]),
    {'error', 'did_too_short'}.

-spec find_trie_rates(ne_binary(), api_ne_binary(), api_ne_binary()) ->
                             candidate_rates_return().
find_trie_rates(E164, AccountId, RatedeckId) ->
    case hon_trie:match_did(only_numeric(E164), AccountId, RatedeckId) of
        {'ok', Result} -> {'ok', Result};
        {'error', _E} ->
            lager:warning("got error while searching did in trie, falling back to DB search"),
            Candidates = fetch_candidate_rates(E164, AccountId, RatedeckId),
            maybe_update_trie(RatedeckId, Candidates),
            Candidates
    end.

-spec maybe_update_trie(ne_binary(), candidate_rates_return()) -> 'ok'.
-spec maybe_update_trie(ne_binary(), candidate_rates_return(), atom()) -> 'ok'.
maybe_update_trie(RatedeckId, Candidates) ->
    maybe_update_trie(RatedeckId, Candidates, hotornot_config:trie_module()).
maybe_update_trie(RatedeckId, {'ok', [_|_]=Rates}, 'hon_trie_lru') ->
    hon_trie_lru:cache_rates(RatedeckId, Rates);
maybe_update_trie(_RatedeckId, _Candidates, _Module) ->
    'ok'.

-spec fetch_candidate_rates(ne_binary(), api_ne_binary(), api_ne_binary()) ->
                                   candidate_rates_return().
-spec fetch_candidate_rates(ne_binary(), api_ne_binary(), api_ne_binary(), ne_binaries()) ->
                                   candidate_rates_return().
fetch_candidate_rates(E164, AccountId, RatedeckId) ->
    fetch_candidate_rates(E164, AccountId, RatedeckId, build_keys(E164)).

fetch_candidate_rates(_E164, _AccountId, _RatedeckId, []) ->
    {'error', 'did_too_short'};
fetch_candidate_rates(E164, AccountId, RatedeckId, Keys) ->
    lager:debug("searching for prefixes for ~s: ~p", [E164, Keys]),
    RatedeckDb = account_ratedeck(AccountId, RatedeckId),
    case fetch_rates_from_ratedeck(RatedeckDb, Keys) of
        {'ok', []}=OK -> OK;
        {'error', _}=E -> E;
        {'ok', ViewRows} ->
            {'ok'
            ,[kzd_rate:set_ratedeck(kz_json:get_json_value(<<"doc">>, ViewRow)
                                   ,kzd_ratedeck:format_ratedeck_id(RatedeckDb)
                                   )
              || ViewRow <- ViewRows
             ]
            }
    end.

-spec fetch_rates_from_ratedeck(ne_binary(), [integer()]) ->
                                       kz_datamgr:get_results_return().
fetch_rates_from_ratedeck(RatedeckDb, Keys) ->
    kz_datamgr:get_results(RatedeckDb
                          ,<<"rates/lookup">>
                          ,[{'keys', Keys}
                           ,'include_docs'
                           ]
                          ).

-spec account_ratedeck(api_ne_binary()) -> ne_binary().
-spec account_ratedeck(api_ne_binary(), api_ne_binary()) -> ne_binary().

-ifdef(TEST).
account_ratedeck(_AccountId) -> ?KZ_RATES_DB.
account_ratedeck(_AccountId, _RatedeckId) -> ?KZ_RATES_DB.
-else.
account_ratedeck(AccountId) ->
    account_ratedeck(AccountId, 'undefined').

account_ratedeck('undefined', 'undefined') ->
    lager:info("no account supplied, using default ratedeck"),
    hotornot_config:default_ratedeck();
account_ratedeck('undefined', <<_/binary>> = RatedeckId) ->
    lager:info("using supplied ratedeck ~s", [RatedeckId]),
    kzd_ratedeck:format_ratedeck_db(RatedeckId);
account_ratedeck(AccountId, _RatedeckId) ->
    case kz_service_ratedeck:get_ratedeck(AccountId) of
        'undefined' ->
            lager:debug("failed to find account ~s ratedeck, checking reseller", [AccountId]),
            reseller_ratedeck(AccountId, kz_services:find_reseller_id(AccountId));
        RatedeckId ->
            lager:info("using account ratedeck ~s for account ~s", [RatedeckId, AccountId]),
            kzd_ratedeck:format_ratedeck_db(RatedeckId)
    end.

-spec reseller_ratedeck(ne_binary(), api_ne_binary()) -> ne_binary().
reseller_ratedeck(_AccountId, 'undefined') ->
    lager:debug("no reseller for ~s, using default ratedeck", [_AccountId]),
    hotornot_config:default_ratedeck();
reseller_ratedeck(ResellerId, ResellerId) ->
    lager:debug("account ~s is own reseller, using system setting", [ResellerId]),
    hotornot_config:default_ratedeck();
reseller_ratedeck(_AccountId, ResellerId) ->
    case kz_service_ratedeck:get_ratedeck(ResellerId) of
        'undefined' ->
            lager:debug("failed to find reseller ~s ratedeck, using default", [_AccountId]),
            hotornot_config:default_ratedeck();
        RatedeckId ->
            lager:info("using reseller ~s ratedeck ~s for account ~s"
                      ,[ResellerId, RatedeckId, _AccountId]
                      ),
            kzd_ratedeck:format_ratedeck_db(RatedeckId)
    end.
-endif.

-spec build_keys(ne_binary()) -> [integer()].
build_keys(Number) ->
    case only_numeric(Number) of
        <<>> -> [];
        <<D:1/binary, Rest/binary>> ->
            build_keys(Rest, D, [kz_term:to_integer(D)])
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
    build_keys(Rest, <<Prefix/binary, D/binary>>, [kz_term:to_integer(<<Prefix/binary, D/binary>>) | Acc]);
build_keys(<<>>, _, Acc) -> Acc.

-spec matching_rates(kzd_rate:docs(), kapi_rate:req()) ->
                            kzd_rate:docs().
matching_rates(Rates, RateReq) ->
    FilterList = hotornot_config:filter_list(),
    lists:foldl(fun(Filter, Acc) ->
                        lists:filter(fun(Rate) -> matching_rate(Rate, Filter, RateReq) end, Acc)
                end
               ,Rates
               ,FilterList
               ).

-spec sort_rates(kzd_rate:docs()) -> kzd_rate:docs().
sort_rates(Rates) ->
    case hotornot_config:should_sort_by_weight() of
        'true' -> lists:usort(fun sort_rate_by_weight/2, Rates);
        'false' -> lists:usort(fun sort_rate_by_cost/2, Rates)
    end.

%% Private helper functions

-spec matching_rate(kzd_rate:doc(), ne_binary(), kapi_rate:req()) -> boolean().
matching_rate(Rate, <<"direction">>, RateReq) ->
    case kz_json:get_value(<<"Direction">>, RateReq) of
        'undefined' -> 'true';
        Direction ->
            lists:member(Direction, kzd_rate:direction(Rate))
    end;

matching_rate(Rate, <<"route_options">>, RateReq) ->
    RouteOptions = kz_json:get_value(<<"Options">>, RateReq, []),
    RouteFlags   = kz_json:get_value(<<"Outbound-Flags">>, RateReq, []),
    ResourceFlag = case kz_json:get_value(<<"Account-ID">>, RateReq) of
                       'undefined' -> [];
                       AccountId -> maybe_add_resource_flag(RateReq, AccountId)
                   end,
    options_match(kzd_rate:options(Rate), RouteOptions++RouteFlags++ResourceFlag);

matching_rate(Rate, <<"routes">>, RateReq) ->
    E164 = knm_converters:normalize(kz_json:get_value(<<"To-DID">>, RateReq)),
    lists:any(fun(Regex) -> re:run(E164, Regex) =/= 'nomatch' end
             ,kzd_rate:routes(Rate, [])
             );

matching_rate(Rate, <<"ratedeck_id">>, RateReq) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, RateReq),
    AccountRatedeck = kz_service_ratedeck_name:get_ratedeck_name(AccountId),
    RatedeckName = kzd_rate:ratedeck(Rate),
    AccountRatedeck =:= RatedeckName;

matching_rate(Rate, <<"reseller">>, RateReq) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, RateReq),
    ResellerId = kz_services:find_reseller_id(AccountId),
    RateAccountId = kzd_rate:account_id(Rate),
    RateAccountId =:= ResellerId;

matching_rate(Rate, <<"version">>, _RateReq) ->
    kzd_rate:version(Rate) =:= hotornot_config:rate_version();

matching_rate(_Rate, _FilterType, _RateReq) -> 'false'.

%% Return true if RateA has lower weight than RateB
-spec sort_rate_by_weight(kzd_rate:doc(), kzd_rate:doc()) -> boolean().
sort_rate_by_weight(RateA, RateB) ->
    PrefixA = byte_size(kzd_rate:prefix(RateA)),
    PrefixB = byte_size(kzd_rate:prefix(RateB)),

    case PrefixA =:= PrefixB of
        'true' ->
            kzd_rate:weight(RateA, 100) < kzd_rate:weight(RateB, 100);
        'false' ->
            PrefixA > PrefixB
    end.

-spec sort_rate_by_cost(kzd_rate:doc(), kzd_rate:doc()) -> boolean().
sort_rate_by_cost(RateA, RateB) ->
    PrefixA = byte_size(kzd_rate:prefix(RateA)),
    PrefixB = byte_size(kzd_rate:prefix(RateB)),

    case PrefixA =:= PrefixB of
        'true' ->
            kzd_rate:rate_cost(RateA, 0.0) > kzd_rate:rate_cost(RateB, 0.0);
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

-spec maybe_add_resource_flag(kapi_rate:req(), ne_binary()) -> kz_proplist().
maybe_add_resource_flag(RateReq, AccountId) ->
    case hotornot_config:should_account_filter_by_resource(AccountId) of
        'true' ->
            case kz_json:get_ne_binary_value(<<"Resource-ID">>, RateReq) of
                'undefined' -> [];
                ResourceId -> [ResourceId]
            end;
        'false' -> []
    end.
