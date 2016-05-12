
%%%-------------------------------------------------------------------
%%% @copyright
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_srs_order_weight).

-export([handle_req/5]).

-include("stepswitch.hrl").

-define(MOD_NAME, <<"order_weight">>).
-define(LIST_WEIGHT, <<"selectors/weight_listing">>).
-define(DEFAULT_WEIGHT_SOURCE, <<"resource">>).
-define(DEFAULT_SORT_ORDER, <<"ascend">>).
-define(DEFAULT_DESC_WEIGHT, 0).
-define(DEFAULT_ASC_WEIGHT, 9999).

-spec handle_req(stepswitch_resources:resources(), ne_binary(), kapi_offnet_resource:req(), ne_binary(), kz_json:object()) ->
    stepswitch_resources:resources().
handle_req([Resource], _Number, _OffnetJObj, _DB, _Params) ->
    Id = stepswitch_resources:get_resrc_id(Resource),
    lager:debug("resources list contains ony one resource (~p), skip sorting", [Id]),
    [Resource];
handle_req(Resources, _Number, _OffnetJObj, DB, Rules) ->
    WeightSource = kz_json:get_ne_binary_value(<<"source">>, Rules, ?DEFAULT_WEIGHT_SOURCE),
    SortOrder = kz_json:get_ne_binary_value(<<"sort_order">>, Rules, ?DEFAULT_SORT_ORDER),
    lager:debug("ordering (~s) resources by weight with data from ~s", [SortOrder, WeightSource]),
    order_by_weight(Resources, SortOrder, WeightSource, DB).

-spec order_by_weight(stepswitch_resources:resources(), ne_binary(), ne_binary(), ne_binary()) -> stepswitch_resources:resources().
order_by_weight(Resources, SortOrder, <<"resource">>, _DB) ->
    lists:sort(fun(R1, R2) ->
                       Weight1 = kz_util:to_integer(stepswitch_resources:get_resrc_weight(R1)),
                       Weight2 = kz_util:to_integer(stepswitch_resources:get_resrc_weight(R2)),
                       case SortOrder of
                           <<"ascend">> -> Weight1 =< Weight2;
                           <<"descend">> -> Weight2 =< Weight1
                       end
               end
               ,Resources
              );
order_by_weight(Resources, SortOrder, <<"database">>, DB) ->
    ResIDs = [ stepswitch_resources:get_resrc_id(R) || R <- Resources ],
    Options = [{'keys', ResIDs}],
    case kz_datamgr:get_results(DB, ?LIST_WEIGHT, Options) of
        {'ok', []} -> Resources;
        {'ok', Result} ->
            Parsed = lists:foldl(fun parse_db_result/2, [], Result),
            lists:sort(fun(R1, R2) ->
                               ID1 = stepswitch_resources:get_resrc_id(R1),
                               ID2 = stepswitch_resources:get_resrc_id(R2),
                               case SortOrder of
                                   <<"ascend">> ->
                                       Weight1 = props:get_value(ID1, Parsed, ?DEFAULT_ASC_WEIGHT),
                                       Weight2 = props:get_value(ID2, Parsed, ?DEFAULT_ASC_WEIGHT),
                                       Weight1 =< Weight2;
                                   <<"descend">> ->
                                       Weight1 = props:get_value(ID1, Parsed, ?DEFAULT_DESC_WEIGHT),
                                       Weight2 = props:get_value(ID2, Parsed, ?DEFAULT_DESC_WEIGHT),
                                       Weight2 =< Weight1
                               end
                       end
                       ,Resources
                      );
        {'error', E} ->
            lager:error("failed to find weight data in ~s: ~p", [DB, E]),
            Resources
    end.

parse_db_result(Row, Acc) ->
    ID = kz_json:get_ne_value(<<"key">>, Row),
    case {kz_json:get_integer_value(<<"value">>, Row), props:get_value(ID, Acc)} of
        {'undefined', _} -> Acc;
        {Value, 'undefined'} -> props:set_value(ID, Value, Acc);
        {Value, OldValue} when Value > OldValue -> props:set_value(ID, Value, Acc);
        {_, _} -> Acc
    end.
