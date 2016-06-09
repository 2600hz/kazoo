%%%-------------------------------------------------------------------
%%% @copyright
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_srs_filter_flags).

-export([handle_req/5]).

-include("stepswitch.hrl").

-define(MOD_NAME, <<"filter_flags">>).
-define(LIST_FLAGS, <<"selectors/flag_listing">>).
-define(LIST_HAS_FLAGS, <<"selectors/has_flag_listing">>).
-define(DEFAULT_FLAGS_SOURCE, <<"resource">>).

-spec handle_req(stepswitch_resources:resources(), ne_binary(), kapi_offnet_resource:req(), ne_binary(), kz_json:object()) ->
   stepswitch_resources:resources().
handle_req(Resources, _Number, OffnetJObj, DB, Params) ->
    FilterSource = kz_json:get_ne_binary_value(<<"source">>, Params, ?DEFAULT_FLAGS_SOURCE),
    lager:debug("filtering resources by flags with data from ~s", [FilterSource]),
    filter_by_flags(Resources, kapi_offnet_resource:flags(OffnetJObj, []), DB, FilterSource).

-spec filter_by_flags(stepswitch_resources:resources(), ne_binaries(), ne_binary(), ne_binary()) -> stepswitch_resources:resources().
filter_by_flags(Resources, [], DB, <<"database">>) ->
    lager:debug("no flags provided, filtering resources that require flags"),
    Keys = lists:map(fun stepswitch_resources:get_resrc_id/1, Resources),
    Options = [{'keys', Keys}, group],
    case kz_datamgr:get_results(DB, ?LIST_HAS_FLAGS, Options) of
        {'ok', []} -> Resources;
        {'ok', Rows} ->
            WithFlags = kz_datamgr:get_result_keys(Rows),
            lists:filter(fun(Res) ->
                                 ResId = stepswitch_resources:get_resrc_id(Res),
                                 not lists:member(ResId, WithFlags)
                         end
                         ,Resources
                        );
        {'error', E} ->
            lager:error("failed to get flags data from ~s: ~p", [DB, E]),
            []
    end;
filter_by_flags(Resources, Flags, DB, <<"database">>) ->
    lager:debug("filtering resources without flags: ~p",[Flags]),
    Options = [{'keys', Flags}],
    case kz_datamgr:get_results(DB, ?LIST_FLAGS, Options) of
        {'ok', []} -> [];
        {'ok', Rows} ->
            Result = parse_rows(Rows),
            ReqSet = sets:from_list(Flags),
            lists:filter(fun(Res) ->
                                 ResId = stepswitch_resources:get_resrc_id(Res),
                                 ResSet = proplists:get_value(ResId, Result, sets:new()),
                                 sets:is_subset(ReqSet, ResSet)
                         end
                         ,Resources
                        );
        {'error', E} ->
            lager:error("failed to get flags data from ~s: ~p", [DB, E]),
            []
    end;
filter_by_flags(Resources, [], _DB, <<"resource">>) ->
    lager:debug("no flags provided, filtering resources that require flags"),
    lists:filter(fun(Res) ->
                         kz_util:is_empty(stepswitch_resources:get_resrc_flags(Res))
                 end
                 ,Resources
                );
filter_by_flags(Resources, Flags, _DB, <<"resource">>) ->
    lager:debug("filtering resources without flags: ~p",[Flags]),
    ReqSet = sets:from_list(Flags),
    lists:filter(fun(Res) ->
                         ResSet = sets:from_list(stepswitch_resources:get_resrc_flags(Res)),
                         sets:is_subset(ReqSet, ResSet)
                 end
                 ,Resources
                ).

-spec parse_rows(kz_json:objects()) -> kz_proplists().
parse_rows(Rows) ->
    lists:foldl(fun(JObj, Acc) ->
                        Flag = kz_json:get_value(<<"key">>, JObj),
                        ResId = kz_json:get_value(<<"value">>, JObj),
                        FlagsSet = proplists:get_value(ResId, Acc, sets:new()),
                        props:set_value(ResId, sets:add_element(Flag, FlagsSet), Acc)
                end
                ,[]
                ,Rows
               ).
