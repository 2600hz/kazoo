%%%-------------------------------------------------------------------
%%% @copyright
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_srs_filter_prefix).

-export([handle_req/5]).

-include("stepswitch_resource_selectors.hrl").

-define(MOD_NAME, <<"filter_prefix">>).
-define(ALLOWED_FILTER_MODES, [<<"empty_ok">>
			      ,<<"empty_fail">>
                              ]).
-define(DEFAULT_FILTER_MODE, <<"empty_fail">>).

-spec handle_req(stepswitch_resources:resources(), ne_binary(), kapi_offnet_resource:req(), ne_binary(), kz_json:object()) ->
			stepswitch_resources:resources().
handle_req(Resources, Number, OffnetJObj, DB, Params) ->
    SourceA = kz_srs_util:get_source(kz_json:get_value(<<"value_a">>, Params)),
    ValueA = kz_srs_util:get_value(SourceA, Resources, Number, OffnetJObj, DB, <<>>),
    'ok' = kz_srs_util:check_value(fun is_binary/1, ValueA),
    SourceB = maybe_db_type(kz_srs_util:get_source(kz_json:get_value(<<"value_b">>, Params)), Resources, ValueA),
    ValueB = kz_srs_util:get_value(SourceB, Resources, Number, OffnetJObj, DB, []),
    'ok' = kz_srs_util:check_value(fun is_list/1, ValueB),
    Action = kz_srs_util:select_filter_action(Params),
    PrefixMode = kz_srs_util:select_filter_mode(Params, ?ALLOWED_FILTER_MODES, ?DEFAULT_FILTER_MODE),
    case SourceB of
        {'database', _, _} -> filter_by_db_prefix(Resources, ValueB, Action, PrefixMode);
        _ -> filter_by_prefix(Resources, ValueA, ValueB, Action, PrefixMode)
    end.

filter_by_db_prefix(Resources, Result, Action, PrefixMode) ->
    lists:filter(fun(R) ->
                         Id = stepswitch_resources:get_resrc_id(R),
                         Value = props:get_value(Id, Result),
                         match_db_prefixes(Id, Value, Action, PrefixMode)
                 end
		,Resources
                ).

filter_by_prefix(Resources, ValueA, ValueB, Action, PrefixMode) ->
    SetA = sets:from_list(build_prefixes(ValueA)),
    lists:filter(fun(R) ->
                         Id = stepswitch_resources:get_resrc_id(R),
                         SetB = sets:from_list(props:get_value(Id, ValueB, [])),
                         match_prefixes(Id, sets:intersection(SetA, SetB), Action, PrefixMode)
                 end
		,Resources
                ).

maybe_db_type({'database', SelectorName}, Resources, PrefixSrc) ->
    Prefixes = build_prefixes(PrefixSrc),
    Keys = [[stepswitch_resources:get_resrc_id(R), SelectorName, P] || R <- Resources, P <- Prefixes],
    Options = [{'keys', Keys}],
    View = <<"resource_selectors/resource_name_data_listing">>,
    {'database', View, Options};
maybe_db_type(Other, _Resources, _PrefixSrc) -> Other.

-spec build_prefixes(ne_binary()) -> ne_binaries().
build_prefixes(<<"+", Rest/binary>>) -> build_prefixes(Rest);
build_prefixes(<<D:1/binary, Rest/binary>>) ->
    build_prefixes(Rest, D, [D]).

-spec build_prefixes(ne_binary(), ne_binary(), ne_binaries()) -> ne_binaries().
build_prefixes(<<D:1/binary, Rest/binary>>, Prefix, Acc) ->
    build_prefixes(Rest, <<Prefix/binary, D/binary>>, [<<Prefix/binary, D/binary>> | Acc]);
build_prefixes(<<>>, _, Acc) -> Acc.

match_prefixes(Id, Set, 'keep', _PrefixMode) ->
    case sets:size(Set) of
        0 ->
            lager:debug("resource ~s dont match prefix, droping", [Id]),
            'false';
        _ ->
            lager:debug("resource ~s matched prefix, keeping", [Id]),
            'true'
    end;
match_prefixes(Id, Set, 'drop', _PrefixMode) ->
    case sets:size(Set) of
        0 ->
            lager:debug("resource ~s dont match prefix, keeping", [Id]),
            'true';
        _ ->
            lager:debug("resource ~s matched prefix, droping", [Id]),
            'false'
    end.

match_db_prefixes(Id, 'undefined', 'keep', _PrefixMode) ->
    lager:debug("resource ~s dont match prefix, droping", [Id]),
    'false';
match_db_prefixes(Id, [_|_], 'keep', _PrefixMode) ->
    lager:debug("resource ~s matched prefix, keeping", [Id]),
    'true';
match_db_prefixes(Id, 'undefined', 'drop', _PrefixMode) ->
    lager:debug("resource ~s dont match prefix, keeping", [Id]),
    'true';
match_db_prefixes(Id, [_|_], 'drop', _PrefixMode) ->
    lager:debug("resource ~s matched prefix, droping", [Id]),
    'false'.
