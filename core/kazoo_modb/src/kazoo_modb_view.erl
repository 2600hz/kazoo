%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz
%%% @doc
%%% Query range of MODBs (month-only databases) either in ascending (Start>End) or descending (End>Start) order,
%%% limited or unlimited, filtered or unfiltered
%%% @end
%%% @contributors
%%%   Roman Galeev
%%%-------------------------------------------------------------------
-module(kazoo_modb_view).
-export([get_results/5, get_results/6]).
-export_type([mapper_fun/0]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-type mapper_fun() :: fun((kz_json:object()) -> kz_json:object()) |
                      fun((kz_json:object(), kz_json:objects()) -> kz_json:objects()).

-type keymap_fun() :: fun((kz_json:path()) -> kz_json:path()).

-type option() :: {couch_options, kz_datamgr:view_options()} |
                  {mapper, mapper_fun()} |
                  {key_map, kz_json:path()} |
                  {start_key_map, kz_json:path()} |
                  {end_key_map, kz_json:path()}.
-type options() :: [option()].

-type last_key() :: 'undefined' | kz_json:path().

-spec get_results(ne_binary(), ne_binary(), gregorian_seconds(), gregorian_seconds(), pos_integer(), options()) ->
                         {last_key(), kz_json:objects()}.
get_results(?NE_BINARY = AccountId, ?NE_BINARY = View, StartTimestamp, EndTimestamp, Limit, Options) ->
    {StartKey, EndKey} = start_end_keys(StartTimestamp, EndTimestamp, Options),
    CouchOptions = [{'startkey', StartKey}
                   ,{'endkey', EndKey}
                    | props:get_value('couch_options', Options, [])
                   ],
    Mapper = props:get_value('mapper', Options, fun kz_term:identity/1),
    get_ordered(AccountId, View, StartTimestamp, EndTimestamp, Limit, Mapper, CouchOptions).

-spec get_ordered(ne_binary(), ne_binary(), gregorian_seconds(), gregorian_seconds(), pos_integer(), mapper_fun(), kz_datamgr:view_options()) ->
                         {last_key(), kz_json:objects()}.
get_ordered(AccountId, View, StartTimestamp, EndTimestamp, Limit, Mapper, CouchOptions) when StartTimestamp >= EndTimestamp ->
    MODbs = lists:reverse(kazoo_modb:get_range(AccountId, EndTimestamp, StartTimestamp)),
    CouchOpts = ['descending' | CouchOptions],
    {_, LastKey, JObjs} =
        lists:foldl(fun fold_query/2, {Limit, 'undefined', []}, [{Db, View, CouchOpts, Mapper} || Db <- MODbs ]),
    {LastKey, JObjs};

get_ordered(AccountId, View, StartTimestamp, EndTimestamp, Limit, Mapper, CouchOptions) when StartTimestamp < EndTimestamp ->
    MODbs = kazoo_modb:get_range(AccountId, StartTimestamp, EndTimestamp),
    {_, LastKey, JObjs} =
        lists:foldl(fun fold_query/2, {Limit, 'undefined', []}, [{Db, View, CouchOptions, Mapper} || Db <- MODbs]),
    {LastKey, JObjs}.

-spec fold_query({ne_binary(), ne_binary(), kz_datamgr:view_options(), mapper_fun()}
                ,{pos_integer(), last_key(), kz_json:objects()}
                ) ->
                        {pos_integer(), last_key(), kz_json:objects()}.
fold_query(_, {Limit, LastKey, Res} = Acc)
  when is_integer(Limit),
       Limit > 0,
       length(Res) == Limit,
       LastKey =/= 'undefined' ->
    Acc;
fold_query({Db, View, CouchOpts, Mapper}, {Limit, LastKey, Res}) when is_integer(Limit), Limit > 0 ->
    Queried = erlang:length(Res),
    LimitWithLast = 1 + Limit - Queried,
    DbResults = limited_query(LimitWithLast, Db, View, CouchOpts),
    {NewLastKey, JObjs} = last_key(LastKey, lists:reverse(DbResults), LimitWithLast, erlang:length(DbResults)),
    {Limit, NewLastKey, Res ++ apply_filter(Mapper, JObjs)}.

-spec map_keymap(ne_binary() | ne_binaries() | keymap_fun()) -> keymap_fun().
map_keymap(K) when is_binary(K) -> fun(Ts) -> [K, Ts] end;
map_keymap(K) when is_list(K) -> fun(Ts) -> K ++ [Ts] end;
map_keymap(K) when is_function(K) -> K.

-spec limited_query(integer(), ne_binary(), ne_binary(), kz_datamgr:view_options()) -> kz_json:objects().
limited_query(Limit, _, _, _) when Limit =< 0 -> [];
limited_query(Limit, Db, View, CouchOpts) ->
    case kazoo_modb:get_results(Db, View, [{limit, Limit} | CouchOpts]) of
        {ok, JObjs} -> JObjs;
        {error, not_found} -> [];
        {error, Error} -> throw(Error)
    end.

-spec apply_filter(mapper_fun(), kz_json:objects()) -> kz_json:objects().
apply_filter(Map, Objects) when is_function(Map, 1) ->
    [Map(Obj) || Obj <- Objects];
apply_filter(Mapper, Objects) when is_function(Mapper, 2) ->
    lists:foldl(Mapper, [], Objects).

-spec last_key(last_key(), kz_json:objects(), integer(), integer()) -> {last_key(), kz_json:objects()}.
last_key(LastKey, [], _, _) ->
    {LastKey, []};
last_key(LastKey, JObjs, Limit, Returned) when Returned < Limit ->
    {LastKey, JObjs};
last_key(_LastKey, [Last|JObjs], Limit, Returned) when Returned == Limit ->
    {kz_json:get_value(<<"key">>, Last), JObjs}.

%%% unlimited version

-spec get_results(ne_binary(), ne_binary(), gregorian_seconds(), gregorian_seconds(), options()) ->
                         kz_json:objects().
get_results(?NE_BINARY = AccountId, ?NE_BINARY = View, StartTimestamp, EndTimestamp, Options) ->
    {StartKey, EndKey} = start_end_keys(StartTimestamp, EndTimestamp, Options),
    CouchOptions = [{'startkey', StartKey}
                   ,{'endkey', EndKey}
                    | props:get_value('couch_options', Options, [])
                   ],
    Mapper = props:get_value('mapper', Options, fun kz_term:identity/1),
    get_ordered(AccountId, View, StartTimestamp, EndTimestamp, Mapper, CouchOptions).

-spec get_ordered(ne_binary(), ne_binary(), gregorian_seconds(), gregorian_seconds(), mapper_fun(), kz_datamgr:view_options()) ->
                         kz_json:objects().
get_ordered(AccountId, View, StartTimestamp, EndTimestamp, Mapper, CouchOptions) when StartTimestamp >= EndTimestamp ->
    MODbs = lists:reverse(kazoo_modb:get_range(AccountId, EndTimestamp, StartTimestamp)),
    CouchOpts = ['descending' | CouchOptions],
    lists:flatten([ apply_filter(Mapper, unlimited_query(Db, View, CouchOpts)) || Db <- MODbs ]);
get_ordered(AccountId, View, StartTimestamp, EndTimestamp, Mapper, CouchOptions) when StartTimestamp < EndTimestamp ->
    MODbs = kazoo_modb:get_range(AccountId, StartTimestamp, EndTimestamp),
    lists:flatten([ apply_filter(Mapper, unlimited_query(Db, View, CouchOptions)) || Db <- MODbs ]).

-spec unlimited_query(ne_binary(), ne_binary(), kz_datamgr:view_options()) ->
                             kz_json:objects().
unlimited_query(Db, View, CouchOpts) ->
    case kazoo_modb:get_results(Db, View, CouchOpts) of
        {'ok', JObjs} -> JObjs;
        {'error', 'not_found'} -> [];
        {'error', Error} -> throw(Error)
    end.

-spec start_end_keys(gregorian_seconds(), gregorian_seconds(), options()) ->
                            {kz_json:path(), kz_json:path()}.
start_end_keys(StartTimestamp, EndTimestamp, Options) ->
    {StartKeyMap, EndKeyMap} = get_key_maps(Options),
    {StartKeyMap(StartTimestamp), EndKeyMap(EndTimestamp)}.

-spec get_key_maps(options()) -> {keymap_fun(), keymap_fun()}.
get_key_maps(Options) ->
    DefaultKeyMap = fun kz_term:identity/1,
    case props:get_value(key_map, Options) of
        'undefined' ->
            case {props:get_value(start_key_map, Options)
                 ,props:get_value(end_key_map, Options)
                 }
            of
                {'undefined', 'undefined'} -> {DefaultKeyMap, DefaultKeyMap};
                {StartMap, 'undefined'} -> {map_keymap(StartMap), DefaultKeyMap};
                {'undefined', EndMap} -> {DefaultKeyMap, map_keymap(EndMap)};
                {StartMap, EndMap} -> {map_keymap(StartMap), map_keymap(EndMap)}
            end;
        KeyMap -> {map_keymap(KeyMap), map_keymap(KeyMap)}
    end.
