%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_srs_order).

-export([handle_req/5]).

-include("stepswitch.hrl").

-define(MOD_NAME, <<"order_by">>).
-define(DEFAULT_SORT_ORDER, <<"ascend">>).
-define(DEFAULT_DESC_WEIGHT, 0).
-define(DEFAULT_ASC_WEIGHT, 9999).

-spec handle_req(stepswitch_resources:resources(), kz_term:ne_binary(), kapi_offnet_resource:req(), kz_term:ne_binary(), kz_json:object()) ->
                        stepswitch_resources:resources().
handle_req([], _Number, _OffnetJObj, _DB, _Params) ->
    lager:warning("empty resource list", []),
    [];
handle_req([Resource], _Number, _OffnetJObj, _DB, _Params) ->
    Id = stepswitch_resources:get_resrc_id(Resource),
    lager:debug("resource list contains only one resource (~p), skip sorting", [Id]),
    [Resource];
handle_req(Resources, Number, OffnetJObj, DB, Params) ->
    Source = kz_srs_util:get_source(kz_json:get_value(<<"value">>, Params)),
    'ok' = check_source(Source),
    Values = kz_srs_util:get_value(Source, Resources, Number, OffnetJObj, DB, 1),
    SortOrder = kz_json:get_ne_value(<<"direction">>, Params, ?DEFAULT_SORT_ORDER),
    order_by(Resources, Values, SortOrder).

-spec order_by(stepswitch_resources:resources(), kz_json:object(), kz_term:ne_binary()) ->
                      stepswitch_resources:resources().
order_by(Resources, Values, SortOrder) ->
    lists:sort(fun(R1, R2) ->
                       Id1 = stepswitch_resources:get_resrc_id(R1),
                       Id2 = stepswitch_resources:get_resrc_id(R2),
                       Weight1 = kz_json:get_value(Id1, Values, default_weight(SortOrder)),
                       Weight2 = kz_json:get_value(Id2, Values, default_weight(SortOrder)),
                       sort(Weight1, Weight2, SortOrder)
               end
              ,Resources
              ).

-spec default_weight(kz_term:ne_binary()) -> integer().
default_weight(<<"ascend">>) -> ?DEFAULT_ASC_WEIGHT;
default_weight(<<"descend">>) -> ?DEFAULT_DESC_WEIGHT.

-spec check_source(tuple() | any()) -> 'ok'.
check_source({'database', _}) -> 'ok';
check_source({'resource', _}) -> 'ok';
check_source(Source) -> throw({invalid_source, Source}).

-spec sort(integer(), integer(), kz_term:ne_binary()) -> boolean().
sort(Lesser, Greater, <<"ascend">>) -> Lesser =< Greater;
sort(Greater, Lesser, <<"descend">>) -> Lesser =< Greater.
