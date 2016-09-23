-module(cb_pager).
-export([descending/7, ascending/7]).

descending(AccountId, View, Start, End, Limit, Filter, Options) when 
        is_binary(AccountId), is_binary(View), is_integer(Start), is_integer(End), is_integer(Limit), Start > End ->
    MODbs = lists:reverse(kazoo_modb:get_range(AccountId, End, Start)),
    CouchOpts = [{'startkey', Start}, {'endkey', End}, 'descending' | Options],
    {_, LastKey, JObjs} =
        lists:foldl(fun fold_query/2, {Limit, undefined, []}, [ {Db, View, CouchOpts, Filter} || Db <- MODbs ]),
    {LastKey, JObjs}.

ascending(AccountId, View, Start, End, Limit, Filter, Options) when 
        is_binary(AccountId), is_binary(View), is_integer(Start), is_integer(End), is_integer(Limit), Start < End ->
    MODbs = kazoo_modb:get_range(AccountId, Start, End),
    CouchOpts = [{'startkey', Start}, {'endkey', End} | Options],
    {_, LastKey, JObjs} =
        lists:foldl(fun fold_query/2, {Limit, undefined, []}, [ {Db, View, CouchOpts, Filter} || Db <- MODbs ]),
    {LastKey, JObjs}.

fold_query({Db, View, CouchOpts, Filter}, {Limit, _LastKey, Res}) when is_integer(Limit), Limit > 0 ->
    Queried = erlang:length(Res),
    LimitWithLast = 1 + Limit - Queried,
    DbResults = limited_query(LimitWithLast, Db, View, CouchOpts),
    {LastKey, JObjs} = last_key(lists:reverse(DbResults), LimitWithLast, erlang:length(DbResults)),
    {Limit, LastKey, Res ++ apply_filter(erlang:fun_info(Filter, arity), Filter, JObjs)}.

limited_query(Limit, _, _, _) when Limit < 0; Limit == 0 -> [];
limited_query(Limit, Db, View, CouchOpts) ->
    case kz_datamgr:get_results(Db, View, [{limit, Limit} | CouchOpts]) of
        {ok, JObjs} -> JObjs;
        {error, not_found} -> [];
        {error, Error} -> throw(Error)
    end.

apply_filter({arity, 1}, Map, Objects) -> lists:foldl(fun(Obj, Acc) -> [ Map(Obj) | Acc ] end, [], Objects);
apply_filter({arity, 2}, Filter, Objects) -> lists:foldl(Filter, [], Objects).

last_key([], _, _) -> {undefined, []};
last_key(JObjs, Limit, Returned) when Returned < Limit -> {undefined, lists:reverse(JObjs)};
last_key([Last|JObjs], Limit, Returned) when Returned == Limit -> {kz_json:get_value(<<"key">>, Last), JObjs}.
