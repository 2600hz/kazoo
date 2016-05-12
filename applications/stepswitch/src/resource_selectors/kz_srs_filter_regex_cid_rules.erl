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
-define(DEFAULT_EMPTY_RULES_ACTION, <<"allow">>).

-spec handle_req(stepswitch_resources:resources(), ne_binary(), kapi_offnet_resource:req(), ne_binary(), kz_json:object()) ->
    stepswitch_resources:resources().
handle_req(Resources, _Number, OffnetJObj, DB, Params) ->
    CIDNumber = case ?RULES_HONOR_DIVERSION of
                    'false' -> kapi_offnet_resource:outbound_caller_id_number(OffnetJObj);
                    'true' -> stepswitch_resources:check_diversion_fields(OffnetJObj)
                end,
    FilterSource = kz_json:get_ne_binary_value(<<"source">>, Params, ?DEFAULT_RULES_SOURCE),
    EmptyRulesAction = kz_json:get_ne_binary_value(<<"empty_rules_action">>, Params,  ?DEFAULT_EMPTY_RULES_ACTION),
    lager:debug("filtering resources by regex cid rules with data from ~s", [FilterSource]),
    filter_by_regex_cid_rules(Resources, CIDNumber, DB, FilterSource, EmptyRulesAction).

-spec filter_by_regex_cid_rules(stepswitch_resources:resources(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) ->
    stepswitch_resources:resources().
filter_by_regex_cid_rules(Resources, CIDNumber, _DB, <<"resource">>, EmptyRulesAction) ->
    lists:filter(fun(R) ->
                         Rules = stepswitch_resources:get_resrc_cid_rules(R),
                         Id = stepswitch_resources:get_resrc_id(R),
                         case evaluate_rules(Rules, CIDNumber) of
                             {'error', 'empty_rules'} when EmptyRulesAction =:= <<"allow">> ->
                                 lager:debug("resource ~s has empty rules for caller idr, allow", [Id]),
                                 'true';
                             {'error', 'empty_rules'} when EmptyRulesAction =:= <<"deny">> ->
                                 lager:debug("resource ~s has empty rules for caller id, skipping", [Id]),
                                 'false';
                             {'ok', CIDMatch} ->
                                 lager:debug("resource ~s matches caller id match '~s'"
                                             ,[Id, CIDMatch]
                                            ),
                                 'true';
                             {'error', 'no_match'} ->
                                 lager:debug("resource ~s does not match caller id number '~s', skipping", [Id, CIDNumber]),
                                 'false'
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

