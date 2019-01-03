%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_srs_filter_list).

-export([handle_req/5]).

-include("stepswitch.hrl").

-define(MOD_NAME, <<"filter_list">>).
-define(ALLOWED_FILTER_MODES, [<<"exact">>
                              ,<<"subset">>
                              ,<<"ne_subset">>
                              ,<<"ne_subset_or_exact">>
                              ,<<"intersect">>
                              ,<<"disjoint">>
                              ]).
-define(DEFAULT_FILTER_MODE, <<"ne_subset_or_exact">>).

-spec handle_req(stepswitch_resources:resources()
                ,kz_term:ne_binary()
                ,kapi_offnet_resource:req()
                ,kz_term:ne_binary()
                ,kz_json:object()
                ) -> stepswitch_resources:resources().
handle_req(Resources, Number, OffnetJObj, DB, Params) ->
    SourceA = kz_srs_util:get_source(kz_json:get_value(<<"value_a">>, Params)),
    ValueA = kz_srs_util:get_value(SourceA, Resources, Number, OffnetJObj, DB, []),
    'ok' = kz_srs_util:check_value(fun is_list/1, ValueA),
    SourceB = kz_srs_util:get_source(kz_json:get_value(<<"value_b">>, Params)),
    ValueB = kz_srs_util:get_value(SourceB, Resources, Number, OffnetJObj, DB, []),
    'ok' = kz_srs_util:check_value(fun is_list/1, ValueB),
    Action = kz_srs_util:select_filter_action(Params),
    Mode = kz_srs_util:select_filter_mode(Params, ?ALLOWED_FILTER_MODES, ?DEFAULT_FILTER_MODE),
    SetA = sets:from_list(ValueA),
    lists:filtermap(fun(R) ->
                            Id = stepswitch_resources:get_resrc_id(R),
                            SetB = sets:from_list(props:get_value(Id, ValueB, [])),
                            Result = filter_list(SetA, SetB, Mode),
                            case Action of
                                'keep' ->
                                    lager:debug("resource ~s (mode ~s), keep: ~s", [Id, Mode, Result]),
                                    Result;
                                'drop' ->
                                    lager:debug("resource ~s (mode ~s), keep: ~s", [Id, Mode, not Result]),
                                    not Result
                            end
                    end
                   ,Resources
                   ).

-spec filter_list(sets:set()
                 ,sets:set()
                 ,kz_term:ne_binary()
                 ) -> boolean().
filter_list(Set, Set, <<"exact">>) -> 'true';
filter_list(SetA, SetB, <<"subset">>) -> sets:is_subset(SetA, SetB);
filter_list(SetA, SetB, <<"ne_subset">>) ->
    sets:size(SetA) > 0
        andalso sets:is_subset(SetA, SetB);
filter_list(SetA, SetB, <<"ne_subset_or_exact">>) ->
    case sets:size(SetA) > 0 of
        'true' -> sets:is_subset(SetA, SetB);
        'false' -> SetA =:= SetB
    end;
filter_list(SetA, SetB, <<"intersect">>) -> not sets:is_disjoint(SetA, SetB);
filter_list(SetA, SetB, <<"disjoint">>) -> sets:is_disjoint(SetA, SetB).
