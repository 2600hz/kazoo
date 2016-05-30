%%%-------------------------------------------------------------------
%%% @copyright
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_srs_filter_regex_cid_rules).

-export([handle_req/5]).

-include("stepswitch.hrl").

-define(MOD_NAME, <<"filter_regex_cid_rules">>).
-define(DEFAULT_RULES_SOURCE, <<"resource">>).
-define(DEFAULT_DENY_ON_EMPTY_RULES, 'false').
-define(DEFAULT_UPDATE_CID_NUMBER, 'false').

-spec handle_req(stepswitch_resources:resources(), ne_binary(), kapi_offnet_resource:req(), ne_binary(), kz_json:object()) ->
    stepswitch_resources:resources().
handle_req(Resources, _Number, OffnetJObj, DB, Params) ->
    CIDNumber = case ?RULES_HONOR_DIVERSION of
                    'false' -> kapi_offnet_resource:outbound_caller_id_number(OffnetJObj);
                    'true' -> stepswitch_resources:check_diversion_fields(OffnetJObj)
                end,
    FilterSource = kz_json:get_ne_binary_value(<<"source">>, Params, ?DEFAULT_RULES_SOURCE),
    lager:debug("filtering resources by regex cid rules with data from ~s", [FilterSource]),
    filter_by_regex_cid_rules(Resources, CIDNumber, DB, FilterSource, Params).

-spec filter_by_regex_cid_rules(stepswitch_resources:resources(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) ->
    stepswitch_resources:resources().
filter_by_regex_cid_rules(Resources, CIDNumber, _DB, <<"resource">>, Params) ->
    IsDenyOnEmptyRules = kz_json:is_true(<<"deny_on_empty_rules">>, Params,  ?DEFAULT_DENY_ON_EMPTY_RULES),
    IsUpdateCIDNumber = kz_json:is_true(<<"update_cid_number">>, Params,  ?DEFAULT_UPDATE_CID_NUMBER),
    lists:filter(fun(R) ->
                         Rules = stepswitch_resources:get_resrc_cid_rules(R),
                         Id = stepswitch_resources:get_resrc_id(R),
                         case evaluate_rules(Rules, CIDNumber) of
                             {'error', 'empty_rules'} when IsDenyOnEmptyRules ->
                                 lager:debug("resource ~s has empty rules for caller id, skipping", [Id]),
                                 'false';
                             {'error', 'empty_rules'} ->
                                 lager:debug("resource ~s has empty rules for caller id, allow", [Id]),
                                 'true';
                             {'error', 'no_match'} ->
                                 lager:debug("resource ~s does not match caller id number '~s', skipping", [Id, CIDNumber]),
                                 'false';
                             {'ok', CIDMatch} when IsUpdateCIDNumber ->
                                 lager:debug("resource ~s matches caller id match '~s'", [Id, CIDMatch]),
                                 {'true', save_matched_cid_number(R, CIDMatch)};
                             {'ok', CIDMatch} ->
                                 lager:debug("resource ~s matches caller id match '~s'", [Id, CIDMatch]),
                                 'true'
                         end
                 end
                 ,Resources
                ).

-spec evaluate_rules(re:mp(), ne_binary()) ->
                            {'ok', ne_binary()} |
                            {'error', 'empty_rules'} |
                            {'error', 'no_match'}.
evaluate_rules([], _) -> {'error', 'empty_rules'};
evaluate_rules(Rules, Number) -> do_evaluate_rules(Rules, Number).

-spec do_evaluate_rules(re:mp(), ne_binary()) ->
                            {'ok', ne_binary()} |
                            {'error', 'no_match'}.
do_evaluate_rules([], _) -> {'error', 'no_match'};
do_evaluate_rules([Rule|Rules], Number) ->
    case re:run(Number, Rule) of
        {'match', [{Start,End}]} ->
            {'ok', binary:part(Number, Start, End)};
        {'match', CaptureGroups} ->
            %% find the largest matching group if present by sorting the position of the
            %% matching groups by list, reverse so head is largest, then take the head of the list
            {Start, End} = hd(lists:reverse(lists:keysort(2, tl(CaptureGroups)))),
            {'ok', binary:part(Number, Start, End)};
        _ -> do_evaluate_rules(Rules, Number)
    end.

-spec save_matched_cid_number(stepswitch_resources:resource(), binary()) ->
    stepswitch_resources:resource().
save_matched_cid_number(Resource, CIDNumber) ->
    OldSelectors = stepswitch_resources:get_resrc_selectors(Resource),
    NewSelectors = props:set_value('regex_cid_number_match', CIDNumber, OldSelectors),
    stepswitch_resources:set_resrc_selectors(Resource, NewSelectors).
