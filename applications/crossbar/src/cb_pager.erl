-module(cb_pager).
-export([descending/6, ascending/6]).

descending(AccountId, View, Start, End, Limit, Filter) when 
        is_binary(AccountId), is_binary(View), is_integer(Start), is_integer(End), is_integer(Limit), Start > End ->
    MODbs = lists:reverse(kazoo_modb:get_range(AccountId, End, Start)),
    CouchOpts = [{'startkey', End}, {'endkey', Start}, 'descending'],
    lists:foldl(fun fold_query/2, {Limit, []}, [ {Db, View, CouchOpts, Filter} || Db <- MODbs ]).

ascending(AccountId, View, Start, End, Limit, Filter) when 
        is_binary(AccountId), is_binary(View), is_integer(Start), is_integer(End), is_integer(Limit), Start < End ->
    MODbs = kazoo_modb:get_range(AccountId, End, Start),
    CouchOpts = [{'startkey', Start}, {'endkey', End}],
    lists:foldl(fun fold_query/2, {Limit, []}, [ {Db, View, CouchOpts, Filter} || Db <- MODbs ]).

fold_query({Db, View, CouchOpts, Filter}, {Limit, Res}) when is_integer(Limit), Limit > 0 ->
    Queried = erlang:length(Res),
    Res ++ apply_filter(erlang:fun_info(Filter, arity), Filter, limited_query(Limit - Queried, Db, View, CouchOpts)).

limited_query(Limit, _, _, _) when Limit < 0; Limit == 0 -> [];
limited_query(Limit, Db, View, CouchOpts) ->
    case kz_datamgr:get_results(Db, View, [{limit, Limit} | CouchOpts]) of
        {ok, JObjs} -> JObjs;
        {error, not_found} -> [];
        {error, Error} -> throw(Error)
    end.

apply_filter({arity, 1}, Map, Objects) -> lists:foldl(fun(Obj, Acc) -> [ Map(Obj) | Acc ] end, [], Objects);
apply_filter({arity, 2}, Filter, Objects) -> lists:foldl(Filter, [], Objects).
