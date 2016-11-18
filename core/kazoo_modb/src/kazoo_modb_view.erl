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

-type mapper_fun() :: fun((kz_json:object()) -> kz_json:object()) |
                      fun((kz_json:object(), kz_json:objects()) -> kz_json:objects()).

-type option() :: {'couch_options', kz_datamgr:view_options()} |
                  {'ascending', boolean()} |
                  {'mapper', mapper_fun()}.
-type options() :: [option()].

-spec get_results(ne_binary(), ne_binary(), kz_json:path(), kz_json:path(), pos_integer(), options()) ->
                         {api_integer(), kz_json:objects()}.
get_results(?NE_BINARY = AccountId, ?NE_BINARY = View, Start, End, Limit, Options) ->
    CouchOptions = props:get_value('couch_options', Options, []),
    Mapper = props:get_value('mapper', Options, fun kz_util:identity/1),
    case props:is_true('ascending', Options, 'false') of
        'true' ->
            get_results_ascending(AccountId, View, Start, End, Limit, Mapper, CouchOptions);
        'false' ->
            get_results_descending(AccountId, View, Start, End, Limit, Mapper, CouchOptions)
    end.

-spec get_results_descending(ne_binary(), ne_binary(), kz_json:path(), kz_json:path(), pos_integer(), mapper_fun(), kz_datamgr:view_options()) ->
                                    {api_integer(), kz_json:objects()}.
get_results_descending(AccountId, View, Start, End, Limit, Mapper, CouchOptions)
  when is_binary(AccountId),
       is_binary(View),
       is_integer(Start),
       is_integer(End),
       is_integer(Limit),
       Start > End ->
    MODbs = lists:reverse(kazoo_modb:get_range(AccountId, End, Start)),
    CouchOpts = [{'startkey', Start}
                ,{'endkey', End}
                ,'descending'
                 | CouchOptions
                ],
    {_, LastKey, JObjs} =
        lists:foldl(fun fold_query/2
                   ,{Limit, 'undefined', []}
                   ,[{Db, View, CouchOpts, Mapper} || Db <- MODbs ]
                   ),
    {LastKey, JObjs}.

-spec get_results_ascending(ne_binary(), ne_binary(), kz_json:path(), kz_json:path(), pos_integer(), mapper_fun(), kz_datamgr:view_options()) ->
                                   {api_integer(), kz_json:objects()}.
get_results_ascending(AccountId, View, Start, End, Limit, Mapper, CouchOptions)
  when is_binary(AccountId),
       is_binary(View),
       is_integer(Limit) ->
    MODbs = kazoo_modb:get_range(AccountId, Start, End),
    CouchOpts = [{'startkey', Start}
                ,{'endkey', End}
                 | CouchOptions
                ],
    {_, LastKey, JObjs} =
        lists:foldl(fun fold_query/2
                   ,{Limit, 'undefined', []}
                   ,[{Db, View, CouchOpts, Mapper} || Db <- MODbs]
                   ),
    {LastKey, JObjs}.

-spec fold_query({ne_binary(), ne_binary(), kz_datamgr:view_options(), mapper_fun()}
                ,{pos_integer(), api_integer(), kz_json:objects()}
                ) ->
                        {pos_integer(), api_integer(), kz_json:objects()}.
fold_query(_, {Limit, LastKey, Res} = Re)
  when is_integer(Limit),
       Limit > 0,
       length(Res) == Limit,
       LastKey =/= 'undefined' ->
    Re;
fold_query({Db, View, CouchOpts, Mapper}, {Limit, LastKey, Res})
  when is_integer(Limit),
       Limit > 0 ->
    Queried = erlang:length(Res),
    LimitWithLast = 1 + Limit - Queried,
    DbResults = limited_query(LimitWithLast, Db, View, CouchOpts),
    {NewLastKey, JObjs} = last_key(LastKey, lists:reverse(DbResults), LimitWithLast, erlang:length(DbResults)),
    {Limit, NewLastKey, Res ++ apply_filter(Mapper, JObjs)}.

-spec limited_query(integer(), ne_binary(), ne_binary(), kz_datamgr:view_options()) ->
                           kz_json:objects().
limited_query(Limit, _, _, _) when Limit =< 0 -> [];
limited_query(Limit, Db, View, CouchOpts) ->
    case kz_datamgr:get_results(Db, View, [{'limit', Limit} | CouchOpts]) of
        {'ok', JObjs} -> JObjs;
        {'error', 'not_found'} -> [];
        {'error', Error} -> throw(Error)
    end.

-spec apply_filter(mapper_fun(), kz_json:objects()) -> kz_json:objects().
apply_filter(Map, Objects) when is_function(Map, 1) ->
    [Map(Obj) || Obj <- Objects];
apply_filter(Mapper, Objects) when is_function(Mapper, 2) ->
    lists:foldl(Mapper, [], Objects).

-spec last_key(api_integer(), kz_json:objects(), integer(), integer()) ->
                      {api_integer(), kz_json:objects()}.
last_key(LastKey, [], _, _) ->
    {LastKey, []};
last_key(LastKey, JObjs, Limit, Returned) when Returned < Limit ->
    {LastKey, JObjs};
last_key(_LastKey, [Last|JObjs], Limit, Returned) when Returned == Limit ->
    {kz_json:get_value(<<"key">>, Last), JObjs}.
