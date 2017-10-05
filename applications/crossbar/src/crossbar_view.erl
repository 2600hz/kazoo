%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   Roman Galeev
%%%   Hesaam Farhang
%%%-------------------------------------------------------------------
-module(crossbar_view).
-export([load/2, load/3
        ,load_modb/2, load_modb/3
        ]).

-include("crossbar.hrl").

-type last_key() :: crossbar_view_util:api_range_key().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Equivalent of load/3, setting Options to an empty list.
%% @end
%%--------------------------------------------------------------------
-spec load(cb_context:context(), ne_binary()) -> cb_context:context().
load(Context, View) ->
    load(Context, View, []).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function attempts to load the context with the results of a view
%% run against the accounts database.
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-spec load(cb_context:context(), ne_binary(), crossbar_view_util:options()) -> cb_context:context().
load(Context, View, Options) ->
    load_view(crossbar_view_util:build_modb_options(Context, View, Options)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Equivalent of load_modb/3, setting Options to an empty list.
%% @end
%%--------------------------------------------------------------------
-spec load_modb(cb_context:context(), ne_binary()) -> cb_context:context().
load_modb(Context, View) ->
    load_modb(Context, View, []).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function attempts to load the context with the results of a view
%% run against the account's MODBs.
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-spec load_modb(cb_context:context(), ne_binary(), crossbar_view_util:options()) -> cb_context:context().
load_modb(Context, View, Options) ->
    load_view(crossbar_view_util:build_modb_options(Context, View, Options)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load view results based on options.
%% @end
%%--------------------------------------------------------------------
load_view(#{should_paginate := 'true'}=Options) ->
    {LastKey, JObjs} = get_ordered(Options),
    format_response(LastKey, JObjs, Options);
load_view(Context) ->
    Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get ordered result from databases. It will figure out last
%% view's JObj key to set as the `next_start_key`.
%% @end
%%--------------------------------------------------------------------
-spec get_ordered(crossbar_view_util:options_map()) -> {last_key(), kz_json:objects()}.
get_ordered(#{databases := Dbs}=Options) ->
    {_, LastKey, JObjs} =
        lists:foldl(fun fold_query/2, {0, 'undefined', []}, [{Db, Options} || Db <- Dbs]),
    {LastKey, JObjs}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Fold over databases and fetch result from each, keeps track of result
%% last key and total counts.
%% @end
%%--------------------------------------------------------------------
fold_query({_, #{limit := Limit}}, {_, LastKey, QueriedJObjs}=Acc)
  when is_integer(Limit),
       Limit > 0,
       length(QueriedJObjs) == Limit,
       LastKey =/= 'undefined' ->
    lager:debug("page size exhausted: ~s", [Limit]),
    Acc;
fold_query({Db, #{view := View
                 ,view_options := ViewOptions
                 ,direction := Direction
                 ,start_key := StartKey
                 ,limit := Limit
                 }=Options}, {TotalQueried, LastKey, QueriedJObjs}) when is_integer(Limit), Limit > 0 ->
    Queried = erlang:length(QueriedJObjs),
    LimitWithLast = 1 + Limit - Queried,

    lager:debug("querying view '~s' from '~s', starting at '~p' with page size ~b and limit ~b in direction ~s"
               ,[View, Db, StartKey, Limit, LimitWithLast, Direction]
               ),

    DbResults = limited_query(LimitWithLast, Db, View, ViewOptions),
    DbResultsLength = erlang:length(DbResults),

    {NewLastKey, JObjs} = last_key(LastKey, lists:reverse(DbResults), LimitWithLast, DbResultsLength),
    FilteredJObj = apply_filter(maps:get(mapper, Options, 'undefined'), JObjs),

    Filtered = length(FilteredJObj),
    NewTotalQueried = TotalQueried + Filtered,

    lager:debug("db_returned: ~b passed_filter: ~p total_queried: ~b next_start_key: ~p"
               ,[DbResultsLength, Filtered, NewTotalQueried, NewLastKey]
               ),
    io:format("~ndb_returned: ~b passed_filter: ~p total_queried: ~b next_start_key: ~p~n"
             ,[DbResultsLength, Filtered, NewTotalQueried, NewLastKey]
             ),
    {NewTotalQueried, NewLastKey, QueriedJObjs ++ FilteredJObj}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Make database request
%% @end
%%--------------------------------------------------------------------
-spec limited_query(pos_integer(), ne_binary(), ne_binary(), kz_datamgr:view_options()) -> kz_json:objects().
limited_query(Limit, _, _, _) when Limit =< 0 -> [];
limited_query(Limit, Db, View, ViewOptions) ->
    Options = [{'limit', Limit} | ViewOptions],

    lager:debug("kz_datamgr:get_results(~s, ~s, ~p)", [Db, View, Options]),
    io:format("~nkz_datamgr:get_results ~s ~s~n~p~n", [Db, View, Options]),

    case kz_datamgr:get_results(Db, View, Options) of
        {'ok', JObjs} -> JObjs;
        {'error', 'not_found'} ->
            lager:debug("either the db ~s or view ~s was not found", [Db, View]),
            [];
        {'error', Error} ->
            lager:debug("failed to get view ~s result from ~s: ~p", [View, Db, Error]),
            []
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Apply filter function if provided while keep maintaining the result order.
%%
%% Filter function can be arity 1 and 2.
%% @end
%%--------------------------------------------------------------------
-spec apply_filter(crossbar_view_util:mapper_fun() | 'undefined', kz_json:objects()) -> kz_json:objects().
apply_filter('undefined', JObjs) ->
    lists:reverse(JObjs);
apply_filter(Map, JObjs) when is_function(Map, 1) ->
    Fun = fun(JObj0, Acc) ->
                  JObj = Map(JObj0),
                  case kz_term:is_empty(JObj) of
                      'true' -> Acc;
                      'false' -> [JObj|Acc]
                  end
          end,
    lists:foldl(Fun, [], JObjs);
apply_filter(Mapper, JObjs) when is_function(Mapper, 2) ->
    [JObj
     || JObj <- lists:foldl(Mapper, [], JObjs),
        not kz_term:is_empty(JObj)
    ].
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Figure out the last key if we have some result and page size is not
%% exhausted yet.
%% @end
%%--------------------------------------------------------------------
-spec last_key(last_key(), kz_json:objects(), integer(), pos_integer()) -> {last_key(), kz_json:objects()}.
last_key(LastKey, [], _, _) ->
    {LastKey, []};
last_key(LastKey, JObjs, Limit, Returned) when Returned < Limit ->
    {LastKey, JObjs};
last_key(_LastKey, [Last|JObjs], Limit, Returned) when Returned == Limit ->
    {kz_json:get_value(<<"key">>, Last), JObjs}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If the last key is known set as the next_start_key in the
%% response envelope.
%% @end
%%--------------------------------------------------------------------
format_response('undefined', JObjs, #{context := Context}) ->
    Envelope = remove_paging(cb_context:resp_envelope(Context)),
    crossbar_doc:handle_datamgr_success(JObjs, cb_context:set_resp_envelope(Context, Envelope));
format_response(NextStartKey, JObjs, #{context := Context, start_key := StartKey}) ->
    Envelope = add_paging(StartKey, length(JObjs), NextStartKey, cb_context:resp_envelope(Context)),
    crossbar_doc:handle_datamgr_success(JObjs, cb_context:set_resp_envelope(Context, Envelope)).

-spec add_paging(crossbar_view_util:range_key(), pos_integer(), crossbar_view_util:range_key(), kz_json:object()) -> kz_json:object().
add_paging(StartKey, PageSize, NextStartKey, JObj) ->
    kz_json:set_values([{<<"start_key">>, StartKey},
                        {<<"page_size">>, PageSize},
                        {<<"next_start_key">>, NextStartKey}
                       ]
                      ,JObj
                      ).

-spec remove_paging(kz_json:object()) -> kz_json:object().
remove_paging(JObj) ->
    kz_json:delete_keys([<<"start_key">>, <<"page_size">>, <<"next_start_key">>], JObj).
