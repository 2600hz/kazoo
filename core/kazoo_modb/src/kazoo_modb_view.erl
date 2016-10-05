%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Roman Galeev
%%%-------------------------------------------------------------------
-module(kazoo_modb_view).
-export([get_results/6]).
-include_lib("kazoo/include/kz_types.hrl").

-spec id(any()) -> any().
id(X) -> X.

-spec get_results(binary(), binary(), integer(), integer(), integer(), kz_proplists()) -> {integer(), kz_json:objects()}.
get_results(AccountId, View, Start, End, Limit, Options) ->
    CouchOptions = props:get_value('couch_options', Options, []),
    Ascending = props:get_value('ascending', Options, 'false'),
    Mapper = props:get_value('mapper', Options, fun id/1),
    case Ascending of
        'true' ->
            get_results_ascending(AccountId, View, Start, End, Limit, Mapper, CouchOptions);
        'false' ->
            get_results_descending(AccountId, View, Start, End, Limit, Mapper, CouchOptions)
    end.

-spec get_results_descending(binary(), binary(), integer(), integer(), integer(), fun(), []) -> {integer(), kz_json:objects()}.
get_results_descending(AccountId, View, Start, End, Limit, Mapper, CouchOptions) when
      is_binary(AccountId), is_binary(View), is_integer(Start), is_integer(End), is_integer(Limit), Start > End ->
    MODbs = lists:reverse(kazoo_modb:get_range(AccountId, End, Start)),
    CouchOpts = [{'startkey', Start}, {'endkey', End}, 'descending' | CouchOptions],
    {_, LastKey, JObjs} =
        lists:foldl(fun fold_query/2, {Limit, 'undefined', []}, [ {Db, View, CouchOpts, Mapper} || Db <- MODbs ]),
    {LastKey, JObjs}.

-spec get_results_ascending(binary(), binary(), integer(), integer(), integer(), fun(), []) -> {integer(), kz_json:objects()}.
get_results_ascending(AccountId, View, Start, End, Limit, Mapper, CouchOptions) when
      is_binary(AccountId), is_binary(View), is_integer(Start), is_integer(End), is_integer(Limit), Start < End ->
    MODbs = kazoo_modb:get_range(AccountId, Start, End),
    CouchOpts = [{'startkey', Start}, {'endkey', End} | CouchOptions],
    {_, LastKey, JObjs} =
        lists:foldl(fun fold_query/2, {Limit, 'undefined', []}, [ {Db, View, CouchOpts, Mapper} || Db <- MODbs ]),
    {LastKey, JObjs}.

-spec fold_query({binary(), binary(), list(), fun()}, {integer(), integer(), kz_json:objects()}) -> {integer(), integer(), kz_json:objects()}.
fold_query(_, {Limit, LastKey, Res} = Re) when is_integer(Limit), Limit > 0, length(Res) == Limit, LastKey =/= 'undefined' -> Re;
fold_query({Db, View, CouchOpts, Mapper}, {Limit, LastKey, Res}) when is_integer(Limit), Limit > 0 ->
    Queried = erlang:length(Res),
    LimitWithLast = 1 + Limit - Queried,
    DbResults = limited_query(LimitWithLast, Db, View, CouchOpts),
    {NewLastKey, JObjs} = last_key(LastKey, lists:reverse(DbResults), LimitWithLast, erlang:length(DbResults)),
    {Limit, NewLastKey, Res ++ apply_filter(Mapper, JObjs)}.

-spec limited_query(integer(), binary(), binary(), list()) -> list().
limited_query(Limit, _, _, _) when Limit < 0; Limit == 0 -> [];
limited_query(Limit, Db, View, CouchOpts) ->
    case kz_datamgr:get_results(Db, View, [{'limit', Limit} | CouchOpts]) of
        {'ok', JObjs} -> JObjs;
        {'error', 'not_found'} -> [];
        {'error', Error} -> throw(Error)
    end.

-spec apply_filter(fun(), kz_json:objects()) -> kz_json:objects().
apply_filter(Map, Objects) when is_function(Map, 1) ->
    [ Map(Obj) || Obj <- Objects ];
apply_filter(Mapper, Objects) when is_function(Mapper, 2) ->
    lists:foldl(Mapper, [], Objects).

-spec last_key(integer(), kz_json:objects(), integer(), integer()) -> {api_integer(), kz_json:objects()}.
last_key(LastKey, [], _, _) -> {LastKey, []};
last_key(LastKey, JObjs, Limit, Returned) when Returned < Limit -> {LastKey, JObjs};
last_key(_LastKey, [Last|JObjs], Limit, Returned) when Returned == Limit -> {kz_json:get_value(<<"key">>, Last), JObjs}.
