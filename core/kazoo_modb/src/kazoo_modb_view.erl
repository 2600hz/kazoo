%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Roman Galeev
%%%-------------------------------------------------------------------
-module(kazoo_modb_view).
-export([get_results/6, descending/7, ascending/7]).
-include_lib("kazoo/include/kz_types.hrl").

-spec id(any()) -> any().
id(X) -> X.

get_results(AccountId, View, Start, End, Limit, Options) ->
    CouchOptions = props:get_value(couch_options, Options, []),
    Ascending = props:get_value(ascending, Options, false),
    Filter = props:get_value(filter, Options, fun id/1),
    case Ascending of
        true ->
            ascending(AccountId, View, Start, End, Limit, Filter, CouchOptions);
        false ->
            descending(AccountId, View, Start, End, Limit, Filter, CouchOptions)
    end.

-spec descending(binary(), binary(), integer(), integer(), integer(), fun(), []) -> {integer(), kz_json:objects()}.
descending(AccountId, View, Start, End, Limit, Filter, CouchOptions) when
      is_binary(AccountId), is_binary(View), is_integer(Start), is_integer(End), is_integer(Limit), Start > End ->
    MODbs = lists:reverse(kazoo_modb:get_range(AccountId, End, Start)),
    CouchOpts = [{'startkey', Start}, {'endkey', End}, 'descending' | CouchOptions],
    {_, LastKey, JObjs} =
        lists:foldl(fun fold_query/2, {Limit, 'undefined', []}, [ {Db, View, CouchOpts, Filter} || Db <- MODbs ]),
    {LastKey, JObjs}.

-spec ascending(binary(), binary(), integer(), integer(), integer(), fun(), []) -> {integer(), kz_json:objects()}.
ascending(AccountId, View, Start, End, Limit, Filter, CouchOptions) when
      is_binary(AccountId), is_binary(View), is_integer(Start), is_integer(End), is_integer(Limit), Start < End ->
    MODbs = kazoo_modb:get_range(AccountId, Start, End),
    CouchOpts = [{'startkey', Start}, {'endkey', End} | CouchOptions],
    {_, LastKey, JObjs} =
        lists:foldl(fun fold_query/2, {Limit, 'undefined', []}, [ {Db, View, CouchOpts, Filter} || Db <- MODbs ]),
    {LastKey, JObjs}.

-spec fold_query({binary(), binary(), list(), fun()}, {integer(), integer(), kz_json:objects()}) -> {integer(), integer(), kz_json:objects()}.
fold_query({Db, View, CouchOpts, Filter}, {Limit, _LastKey, Res}) when is_integer(Limit), Limit > 0 ->
    Queried = erlang:length(Res),
    LimitWithLast = 1 + Limit - Queried,
    DbResults = limited_query(LimitWithLast, Db, View, CouchOpts),
    {LastKey, JObjs} = last_key(lists:reverse(DbResults), LimitWithLast, erlang:length(DbResults)),
    {Limit, LastKey, Res ++ apply_filter(Filter, JObjs)}.

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
apply_filter(Filter, Objects) when is_function(Filter, 2) ->
    lists:foldl(Filter, [], Objects).

-spec last_key(kz_json:objects(), integer(), integer()) -> {api_integer(), kz_json:objects()}.
last_key([], _, _) -> {'undefined', []};
last_key(JObjs, Limit, Returned) when Returned < Limit -> {'undefined', JObjs};
last_key([Last|JObjs], Limit, Returned) when Returned == Limit -> {kz_json:get_value(<<"key">>, Last), JObjs}.
