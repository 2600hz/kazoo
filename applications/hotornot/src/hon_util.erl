%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(hon_util).

-export([candidate_rates/1, candidate_rates/2, candidate_rates/3
        ,matching_rates/2
        ,sort_rates/1
        ,sort_rates_by_cost/1, sort_rates_by_weight/1
        ,account_ratedeck/1, account_ratedeck/2
        ]).

-include("hotornot.hrl").

-define(MIN_PREFIX_LEN, 1). % how many chars to strip off the e164 DID

-type candidate_rates_return() :: {'ok', kzd_rates:docs()} |
                                  {'error', 'did_to_short'} |
                                  kz_datamgr:data_error().

-spec candidate_rates(kz_term:ne_binary()) ->
          candidate_rates_return().
candidate_rates(ToDID) ->
    candidate_rates(ToDID, 'undefined', 'undefined').

-spec candidate_rates(kz_term:ne_binary(), kz_term:api_ne_binary()) ->
          candidate_rates_return().
candidate_rates(ToDID, AccountId) ->
    candidate_rates(ToDID, AccountId, 'undefined').

-spec candidate_rates(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:api_ne_binary()) ->
          candidate_rates_return().
candidate_rates(ToDID, AccountId, RatedeckId) ->
    E164 = knm_converters:normalize(ToDID),
    find_candidate_rates(E164, AccountId, RatedeckId).

-spec find_candidate_rates(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:api_ne_binary()) ->
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

-spec find_trie_rates(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:api_ne_binary()) ->
          candidate_rates_return().
find_trie_rates(E164, AccountId, RatedeckId) ->
    case hon_trie:match_did(kz_binary:remove_non_numeric(E164), AccountId, RatedeckId) of
        {'ok', Result} -> {'ok', Result};
        {'error', _E} ->
            lager:warning("got error while searching did in trie, falling back to DB search"),
            Candidates = fetch_candidate_rates(E164, AccountId, RatedeckId),
            maybe_update_trie(RatedeckId, Candidates),
            Candidates
    end.

-spec maybe_update_trie(kz_term:ne_binary(), candidate_rates_return()) -> 'ok'.
maybe_update_trie(RatedeckId, Candidates) ->
    maybe_update_trie(RatedeckId, Candidates, hotornot_config:trie_module()).

-spec maybe_update_trie(kz_term:ne_binary(), candidate_rates_return(), atom()) -> 'ok'.
maybe_update_trie(RatedeckId, {'ok', [_|_]=Rates}, 'hon_trie_lru') ->
    hon_trie_lru:cache_rates(RatedeckId, Rates);
maybe_update_trie(_RatedeckId, _Candidates, _Module) ->
    'ok'.

-spec fetch_candidate_rates(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:api_ne_binary()) ->
          candidate_rates_return().
fetch_candidate_rates(E164, AccountId, RatedeckId) ->
    fetch_candidate_rates(E164, AccountId, RatedeckId, kzdb_ratedeck:prefix_keys(E164)).

-spec fetch_candidate_rates(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:api_ne_binary(), kz_term:ne_binaries()) ->
          candidate_rates_return().
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
            ,[kzd_rates:set_ratedeck_id(kz_json:get_json_value(<<"doc">>, ViewRow)
                                       ,kzd_ratedeck:format_ratedeck_id(RatedeckDb)
                                       )
              || ViewRow <- ViewRows
             ]
            }
    end.

-spec fetch_rates_from_ratedeck(kz_term:ne_binary(), [integer()]) ->
          kz_datamgr:get_results_return().
fetch_rates_from_ratedeck(RatedeckDb, Keys) ->
    kz_datamgr:get_results(RatedeckDb
                          ,<<"rates/lookup">>
                          ,[{'keys', Keys}
                           ,'include_docs'
                           ]
                          ).

-ifdef(TEST).

-spec account_ratedeck(kz_term:api_ne_binary()) -> kz_term:ne_binary().
account_ratedeck(_AccountId) -> ?KZ_RATES_DB.

-spec account_ratedeck(kz_term:api_ne_binary(), kz_term:api_ne_binary()) -> kz_term:ne_binary().
account_ratedeck(_AccountId, _RatedeckId) -> ?KZ_RATES_DB.
-else.

-spec account_ratedeck(kz_term:api_ne_binary()) -> kz_term:ne_binary().
account_ratedeck(AccountId) ->
    account_ratedeck(AccountId, 'undefined').

-spec account_ratedeck(kz_term:api_ne_binary(), kz_term:api_ne_binary()) -> kz_term:ne_binary().
account_ratedeck('undefined', 'undefined') ->
    lager:info("no account supplied, using default ratedeck"),
    hotornot_config:default_ratedeck();
account_ratedeck('undefined', <<_/binary>> = RatedeckId) ->
    lager:info("using supplied ratedeck ~s", [RatedeckId]),
    kzd_ratedeck:format_ratedeck_db(RatedeckId);
account_ratedeck(AccountId, _RatedeckId) ->
    case kz_services_ratedecks:id(AccountId) of
        'undefined' ->
            lager:debug("failed to find account ~s ratedeck, checking reseller", [AccountId]),
            reseller_ratedeck(AccountId, kz_services_reseller:get_id(AccountId));
        RatedeckId ->
            lager:info("using account ratedeck ~s for account ~s", [RatedeckId, AccountId]),
            kzd_ratedeck:format_ratedeck_db(RatedeckId)
    end.

-spec reseller_ratedeck(kz_term:ne_binary(), kz_term:api_ne_binary()) -> kz_term:ne_binary().
reseller_ratedeck(_AccountId, 'undefined') ->
    lager:debug("no reseller for ~s, using default ratedeck", [_AccountId]),
    hotornot_config:default_ratedeck();
reseller_ratedeck(ResellerId, ResellerId) ->
    lager:debug("account ~s is own reseller, using system setting", [ResellerId]),
    hotornot_config:default_ratedeck();
reseller_ratedeck(_AccountId, ResellerId) ->
    case kz_services_ratedecks:id(ResellerId) of
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

-spec matching_rates(kzd_rates:docs(), kapi_rate:req()) ->
          kzd_rates:docs().
matching_rates(Rates, RateReq) ->
    FilterList = hotornot_config:filter_list(),
    lists:foldl(fun(Filter, Acc) ->
                        lists:filter(fun(Rate) -> matching_rate(Rate, Filter, RateReq) end, Acc)
                end
               ,Rates
               ,FilterList
               ).

-spec sort_rates(kzd_rates:docs()) -> kzd_rates:docs().
sort_rates(Rates) ->
    case hotornot_config:should_sort_by_weight() of
        'true' -> sort_rates_by_weight(Rates);
        'false' -> sort_rates_by_cost(Rates)
    end.

-spec sort_rates_by_weight(kzd_rates:docs()) -> kzd_rates:docs().
sort_rates_by_weight(Rates) ->
    lists:usort(fun sort_rate_by_weight/2, Rates).

-spec sort_rates_by_cost(kzd_rates:docs()) -> kzd_rates:docs().
sort_rates_by_cost(Rates) ->
    lists:usort(fun sort_rate_by_cost/2, Rates).

%% Private helper functions

-spec matching_rate(kzd_rates:doc(), kz_term:ne_binary(), kapi_rate:req()) -> boolean().
matching_rate(Rate, <<"direction">>, RateReq) ->
    case kapi_rate:direction(RateReq) of
        'undefined' -> 'true';
        Direction ->
            lists:member(Direction, kzd_rates:direction(Rate))
    end;

matching_rate(Rate, <<"route_options">>, RateReq) ->
    RouteOptions = kapi_rate:options(RateReq),
    RouteFlags   = kapi_rate:outbound_flags(RateReq),
    ResourceFlag = case kapi_rate:account_id(RateReq) of
                       'undefined' -> [];
                       AccountId -> maybe_add_resource_flag(RateReq, AccountId)
                   end,
    options_match(kzd_rates:options(Rate), RouteOptions++RouteFlags++ResourceFlag);

matching_rate(Rate, <<"routes">>, RateReq) ->
    ToDID = kapi_rate:to_did(RateReq),
    E164 = knm_converters:normalize(ToDID),
    lists:any(fun(Regex) -> re:run(E164, Regex) =/= 'nomatch' end
             ,kzd_rates:routes(Rate, [])
             );

matching_rate(Rate, <<"caller_id_numbers">>, RateReq) ->
    FromDID = kapi_rate:from_did(RateReq),
    E164 = knm_converters:normalize(FromDID),
    lists:any(fun(Regex) -> re:run(E164, Regex) =/= 'nomatch' end
             ,kzd_rates:caller_id_numbers(Rate, [<<".">>])
             );

matching_rate(Rate, <<"ratedeck_id">>, RateReq) ->
    AccountId = kapi_rate:account_id(RateReq),
    AccountRatedeck = kz_services_ratedecks:name(AccountId),
    RatedeckName = kzd_rates:ratedeck_id(Rate),
    AccountRatedeck =:= RatedeckName;

matching_rate(Rate, <<"reseller">>, RateReq) ->
    AccountId = kapi_rate:account_id(RateReq),
    ResellerId = kz_services_reseller:get_id(AccountId),
    RateAccountId = kzd_rates:account_id(Rate),
    RateAccountId =:= ResellerId;

matching_rate(Rate, <<"version">>, _RateReq) ->
    kzd_rates:rate_version(Rate) =:= hotornot_config:rate_version();

matching_rate(_Rate, _FilterType, _RateReq) -> 'false'.

%% Return true if RateA has lower weight than RateB
-spec sort_rate_by_weight(kzd_rates:doc(), kzd_rates:doc()) -> boolean().
sort_rate_by_weight(RateA, RateB) ->
    PrefixA = byte_size(kz_term:to_binary(kzd_rates:prefix(RateA))),
    PrefixB = byte_size(kz_term:to_binary(kzd_rates:prefix(RateB))),

    case PrefixA =:= PrefixB of
        'true' ->
            kzd_rates:weight(RateA, 100) < kzd_rates:weight(RateB, 100);
        'false' ->
            PrefixA > PrefixB
    end.

-spec sort_rate_by_cost(kzd_rates:doc(), kzd_rates:doc()) -> boolean().
sort_rate_by_cost(RateA, RateB) ->
    PrefixA = byte_size(kz_term:to_binary(kzd_rates:prefix(RateA))),
    PrefixB = byte_size(kz_term:to_binary(kzd_rates:prefix(RateB))),

    case PrefixA =:= PrefixB of
        'true' ->
            kzd_rates:rate_cost(RateA, 0.0) > kzd_rates:rate_cost(RateB, 0.0);
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

-spec maybe_add_resource_flag(kapi_rate:req(), kz_term:ne_binary()) -> kz_term:ne_binaries().
maybe_add_resource_flag(RateReq, AccountId) ->
    case hotornot_config:should_account_filter_by_resource(AccountId) of
        'true' ->
            case kapi_rate:resource_id(RateReq) of
                'undefined' -> [];
                ResourceId -> [ResourceId]
            end;
        'false' -> []
    end.
