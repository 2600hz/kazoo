%%%-------------------------------------------------------------------
%%% @copyright
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_srs_filter_regex_rules).

-export([handle_req/5]).

-include("stepswitch.hrl").

-define(MOD_NAME, <<"filter_regex_rules">>).
-define(DEFAULT_RULES_SOURCE, <<"resource">>).
-define(DEFAULT_EMPTY_RULES_ACTION, <<"deny">>).

-spec handle_req(stepswitch_resources:resources(), ne_binary(), kapi_offnet_resource:req(), ne_binary(), kz_json:object()) ->
    stepswitch_resources:resources().
handle_req(Resources, Number, _OffnetJObj, DB, Params) ->
    FilterSource = kz_json:get_ne_binary_value(<<"source">>, Params, ?DEFAULT_RULES_SOURCE),
    EmptyRulesAction = kz_json:get_ne_binary_value(<<"empty_rules_action">>, Params,  ?DEFAULT_EMPTY_RULES_ACTION),
    lager:debug("filtering resources by regex rules with data from ~s", [FilterSource]),
    filter_by_regex_rules(Resources, Number, DB, FilterSource, EmptyRulesAction).

-spec filter_by_regex_rules(stepswitch_resources:resources(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) ->
    stepswitch_resources:resources().
filter_by_regex_rules(Resources, Number, _DB, <<"resource">>, EmptyRulesAction) ->
    lists:filter(fun(R) ->
                         Rules = stepswitch_resources:get_resrc_rules(R),
                         Id = stepswitch_resources:get_resrc_id(R),
                         case evaluate_rules(Rules, Number) of
                             {'error', 'empty_rules'} when EmptyRulesAction =:= <<"allow">> ->
                                 lager:debug("resource ~s has empty rules, allow", [Id]),
                                 'true';
                             {'error', 'empty_rules'} when EmptyRulesAction =:= <<"deny">> ->
                                 lager:debug("resource ~s has empty rules, skipping", [Id]),
                                 'false';
                             {'error', 'no_match'} ->
                                 lager:debug("resource ~s does not match request, skipping", [Id]),
                                 'false';
                             {'ok', Match} ->
                                 lager:debug("resource ~s does match request, match result: ~p", [Id, Match]),
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

