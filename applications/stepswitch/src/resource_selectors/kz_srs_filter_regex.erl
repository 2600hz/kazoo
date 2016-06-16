%%%-------------------------------------------------------------------
%%% @copyright
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_srs_filter_regex).

-export([handle_req/5]).

-include("stepswitch_resource_selectors.hrl").

-define(MOD_NAME, <<"filter_regex">>).
-define(ALLOWED_FILTER_MODES, [<<"empty_ok">>
                               ,<<"empty_fail">>
                              ]).
-define(DEFAULT_FILTER_MODE, <<"empty_fail">>).

-spec handle_req(stepswitch_resources:resources()
                 ,ne_binary()
                 ,kapi_offnet_resource:req()
                 ,ne_binary()
                 ,kz_json:object()
                ) -> stepswitch_resources:resources().
handle_req(Resources, Number, OffnetJObj, DB, Params) ->
    SourceA = kz_srs_util:get_source(kz_json:get_value(<<"value_a">>, Params)),
    ValueA = kz_srs_util:get_value(SourceA, Resources, Number, OffnetJObj, DB, <<>>),
    'ok' = kz_srs_util:check_value(fun is_binary/1, ValueA),
    SourceB = kz_srs_util:get_source(kz_json:get_value(<<"value_b">>, Params)),
    ValueB = kz_srs_util:get_value(SourceB, Resources, Number, OffnetJObj, DB, []),
    'ok' = kz_srs_util:check_value(fun is_list/1, ValueB),
    Action = kz_srs_util:select_filter_action(Params),
    EmptyMode = kz_srs_util:select_filter_mode(Params, ?ALLOWED_FILTER_MODES, ?DEFAULT_FILTER_MODE),
    filter_by_regex(Resources, ValueA, ValueB, Action, EmptyMode).

-spec filter_by_regex(stepswitch_resources:resources()
                      ,ne_binary()
                      ,kz_proplists()
                      ,atom()
                      ,ne_binary()
                     ) -> stepswitch_resources:resources().
filter_by_regex(Resources, ValueA, Regexes, Action, EmptyMode) ->
    lager:debug("filter resources by ~s with regex rules, and ~s matched", [ValueA, Action]),
    lists:filtermap(fun(R) ->
                            Id = stepswitch_resources:get_resrc_id(R),
                            Rules = props:get_value(Id, Regexes, []),
                            evaluate_rules(Id, Rules, ValueA, Action, EmptyMode)
                    end
                    ,Resources
                   ).

-spec evaluate_rules(ne_binary(), re:mp(), ne_binary(), atom(), ne_binary()) -> boolean().
evaluate_rules(Id, [], _Data, 'keep', <<"empty_fail">>) ->
    lager:debug("resource ~s has empty rules, dropping", [Id]),
    'false';
evaluate_rules(Id, [], _Data, 'keep', <<"empty_ok">>) ->
    lager:debug("resource ~s has empty rules, keeping", [Id]),
    'true';
evaluate_rules(Id, Rules, Data, 'keep', _EmptyMode) ->
    case do_evaluate_rules(Rules, Data) of
        {'error', 'no_match'} ->
            lager:debug("resource ~s does not match request, dropping", [Id]),
            'false';
        {'ok', Match} ->
            lager:debug("resource ~s does match (~p) request, keeping", [Id, Match]),
            'true'
    end;
evaluate_rules(Id, [], _Data, 'drop', <<"empty_fail">>) ->
    lager:debug("resource ~s has empty rules, keeping", [Id]),
    'false';
evaluate_rules(Id, [], _Data, 'drop', <<"empty_ok">>) ->
    lager:debug("resource ~s has empty rules, dropping", [Id]),
    'true';
evaluate_rules(Id, Rules, Data, 'drop', _EmptyMode) ->
    case do_evaluate_rules(Rules, Data) of
        {'error', 'no_match'} ->
            lager:debug("resource ~s does not match request, keeping", [Id]),
            'true';
        {'ok', Match} ->
            lager:debug("resource ~s does match (~p) request, dropping", [Id, Match]),
            'false'
    end.

-spec do_evaluate_rules(re:mp(), ne_binary()) ->
                            {'ok', ne_binary()} |
                            {'error', 'no_match'}.
do_evaluate_rules([], _) -> {'error', 'no_match'};
do_evaluate_rules([Rule|Rules], Data) ->
    case re:run(Data, Rule) of
        {'match', [{Start,End}]} ->
            {'ok', binary:part(Data, Start, End)};
        {'match', CaptureGroups} ->
            %% find the largest matching group if present by sorting the position of the
            %% matching groups by list, reverse so head is largest, then take the head of the list
            {Start, End} = hd(lists:reverse(lists:keysort(2, tl(CaptureGroups)))),
            {'ok', binary:part(Data, Start, End)};
        _ -> do_evaluate_rules(Rules, Data)
    end.
