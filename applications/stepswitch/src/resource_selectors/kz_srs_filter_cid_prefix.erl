%%%-------------------------------------------------------------------
%%% @copyright
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_srs_filter_cid_prefix).

-export([handle_req/5]).

-include("stepswitch.hrl").

-define(MOD_NAME, <<"filter_cid_prefix">>).
-define(LIST_PREFIX, <<"selectors/cid_prefix_listing">>).

-spec handle_req(stepswitch_resources:resources(), ne_binary(), kapi_offnet_resource:req(), ne_binary(), kz_josn:object()) ->
    stepswitch_resources:resources().
handle_req(Resources, _Number, OffnetJObj, DB, _Params) ->
    CIDNumber = case ?RULES_HONOR_DIVERSION of
                    'false' -> kapi_offnet_resource:outbound_caller_id_number(OffnetJObj);
                    'true' -> stepswitch_resources:check_diversion_fields(OffnetJObj)
                end,
    Keys = build_keys(CIDNumber),
    Options = [{'keys', Keys}],
    case kz_datamgr:get_results(DB, ?LIST_PREFIX, Options) of
        {'ok', []} ->
            lager:info("no matching cid prefix found for: ~p in: ~s",[CIDNumber, DB]),
            [];
        {'ok', Rows} ->
            DBResult = lists:foldl(fun parse_row/2, [], Rows),
            lists:filtermap(fun(Res) ->
                                    Id = stepswitch_resources:get_resrc_id(Res),
                                    case proplists:is_defined(Id, DBResult) of
                                        'true' ->
                                            OldSelectors = stepswitch_resources:get_resrc_selectors(Res),
                                            PrefixMatch = props:get_value(Id, DBResult),
                                            NewSelectors = props:set_value('cid_prefix_match', PrefixMatch, OldSelectors),
                                            {'true', stepswitch_resources:set_resrc_selectors(Res, NewSelectors)};
                                        'false' -> 'false'
                                    end
                            end
                            ,Resources
                           );
        {'error', E} ->
            lager:error("failed to get cid prefixes data from ~s: ~p", [DB, E]),
            []
    end.

build_keys(<<"+", E164/binary>>) ->
    build_keys(E164);
build_keys(<<D:1/binary, Rest/binary>>) ->
    build_keys(Rest, D, [D]).

build_keys(<<D:1/binary, Rest/binary>>, Prefix, Acc) ->
    build_keys(Rest, <<Prefix/binary, D/binary>>, [<<Prefix/binary, D/binary>> | Acc]);
build_keys(<<>>, _, Acc) -> Acc.

parse_row(Row, Acc) ->
    Prefix = kz_json:get_ne_value(<<"key">>, Row),
    ResId = kz_json:get_ne_value(<<"value">>, Row),
    OldPrefix = proplists:get_value(ResId, Acc, <<>>),
    case ResId of
        'undefined' -> Acc;
        _ when byte_size(Prefix) =< byte_size(OldPrefix) -> Acc;
        _ -> props:set_value(ResId, Prefix, Acc)
    end.
