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

        ,build_load_modb_params/3
        ,build_query/5

        ,direction/1, direction/2
        ,time_range/1, time_range/2, time_range/5
        ,start_end_keys/2, start_end_keys/5
        ,suffix_key_fun/1
        ]).

-include("crossbar.hrl").

-define(CB_VIEW_OPTIONS,
        ['ascending', 'created_from'
        ,'created_to', 'databases'
        ,'descending' ,'end_key_map'
        ,'key_map', 'mapper'
        ,'max_range', 'start_key_map'
        ]).

-type direction() :: 'ascending' | 'descending'.

-type time_range() :: {gregorian_seconds(), gregorian_seconds()}.

-type range_key() :: kz_json:json_term().
-type api_range_key() :: kz_json:api_json_term().
-type range_keys() :: {api_range_key(), api_range_key()}.

-type keymap_fun() :: fun((range_key()) -> api_range_key()).
-type keymap() :: 'nil' | api_ne_binary() | ne_binaries() | integer() | keymap_fun().

-type user_mapper_fun() :: 'undefined' |
                           fun((kz_json:object()) -> kz_json:object()) |
                           fun((kz_json:object(), kz_json:objects()) -> kz_json:objects()) |
                           fun((cb_context:context(), kz_json:object(), kz_json:objects()) -> kz_json:objects()).

-type mapper_fun() :: 'undefined' |
                      fun((kz_json:object()) -> kz_json:object()) |
                      fun((kz_json:object(), kz_json:objects()) -> kz_json:objects()).

-type options() :: kazoo_data:view_options() |
                   [{'databases', ne_binaries()} |
                   {'created_from', pos_integer()} |
                   {'created_to', pos_integer()} |
                   {'end_key_map', keymap()} |
                   {'key_map', keymap()} |
                   {'mapper', user_mapper_fun()} |
                   {'max_range', pos_integer()} |
                   {'start_key_map', keymap()}
                   ].

-type load_params() :: #{context => cb_context:context()
                        ,databases => ne_binaries()
                        ,direction => direction()
                        ,page_size => integer()
                        ,mapper => mapper_fun()
                        ,should_paginate => boolean()
                        ,start_key => range_key()
                        ,view => ne_binary()
                        ,view_options => kazoo_data:view_options()
                        }.

-type last_key() :: api_range_key().
-type query_db_params() :: {ne_binary(), load_params()}.
-type query_acc() :: {non_neg_integer(), last_key(), kz_json:objects()}.

-export_type([range_key/0, api_range_key/0, range_keys/0
             ,options/0 , load_params/0
             ,mapper_fun/0 ,user_mapper_fun/0
             ,keymap/0, keymap_fun/0
             ,time_range/0
             ,direction/0
             ]
            ).

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
-spec load(cb_context:context(), ne_binary(), options()) -> cb_context:context().
load(Context, View, Options) ->
    load_view(build_load_modb_params(Context, View, Options)).

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
-spec load_modb(cb_context:context(), ne_binary(), options()) -> cb_context:context().
load_modb(Context, View, Options) ->
    load_view(build_load_modb_params(Context, View, Options)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Generates corssbar_view options map for looking up MODBs view.
%% @end
%%--------------------------------------------------------------------
-spec build_load_modb_params(cb_context:context(), ne_binary(), options()) -> load_params() | cb_context:context().
build_load_modb_params(Context, View, Options) ->
    Direction = direction(Context, Options),
    TimeFilterKey = props:get_ne_binary_value('range_key', Options, <<"created">>),
    HasQSFilter = crossbar_filter:is_defined(Context)
                      andalso not crossbar_filter:is_only_time_filter(Context, TimeFilterKey),
    UserMapper = props:get_value('mapper', Options),
    case time_range(Context, Options, TimeFilterKey) of
        {StartTime, EndTime} ->
            {StartKey, EndKey} = start_end_keys(Context, Options, Direction, StartTime, EndTime),
            maps:from_list(
              props:filter_undefined(
                [{'context', cb_context:set_doc(Context, [])}
                ,{'databases', get_range_modbs(Context, Options, Direction, StartTime, EndTime)}
                ,{'direction', Direction}
                ,{'page_size', get_limit(Context, Options)}
                ,{'mapper', crossbar_filter:build_with_mapper(Context, UserMapper, HasQSFilter)}
                ,{'should_paginate', cb_context:should_paginate(Context)}
                ,{'start_key', StartKey}
                ,{'view', View}
                ,{'view_options', build_query(Options, Direction, StartKey, EndKey, HasQSFilter)}
                ])
             );
        Ctx -> Ctx
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Build CouchDB view options. It sets start/end keys,
%% direction and `include_docs` (if it's not using reduce) and removes
%% CrossbarView Options.
%%
%% NOTE: Do not set start/end keys in your CrossbarView Options,
%% use provided special keys to generate start/end keys based on
%% timestamp.
%% @end
%%--------------------------------------------------------------------
-spec build_query(options(), direction(), api_range_key(), api_range_key(), boolean()) -> kazoo_data:view_options().
build_query(Options, Direction, StartKey, EndKey, HasQSFilter) ->
    DefaultOptions =
        props:filter_undefined(
          [{'startkey', StartKey}
          ,{'endkey', EndKey}
          ,Direction
           | props:delete_keys(['startkey', 'endkey', 'ascending' | ?CB_VIEW_OPTIONS], Options)
          ]),

    IncludeOptions =
        case HasQSFilter of
            'true' -> ['include_docs' | props:delete('include_docs', DefaultOptions)];
            'false' -> DefaultOptions
        end,

    case props:get_first_defined(['reduce', 'group', 'group_level'], IncludeOptions) of
        'undefined' -> IncludeOptions;
        'false' -> IncludeOptions;
        _V -> props:delete('include_docs', IncludeOptions)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Equivalent of `start_end_keys/5`, find time ranges and direction
%% and pass them to `start_end_keys/5`.
%% @end
%%--------------------------------------------------------------------
-spec start_end_keys(cb_context:cb_context(), options()) -> range_keys().
start_end_keys(Context, Options) ->
    {StartTime, EndTime} = time_range(Context, Options),
    Direction = direction(Context, Options),
    start_end_keys(Context, Options, Direction, StartTime, EndTime).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find start/end keys based on requested time range and direction.
%% If `start_key` or `end_key` is present in the request it will be
%% used used instead. Otherwise the keys will built by key map options.
%%
%% See get_range_key_maps/1 to find out possible key map options.
%%
%% The keys will be swapped if direction is descending.
%% @end
%%--------------------------------------------------------------------
-spec start_end_keys(cb_context:cb_context(), options(), direction(), gregorian_seconds(), gregorian_seconds()) -> range_keys().
start_end_keys(Context, Options, Direction, StartTime, EndTime) ->
    {StartKeyMap, EndKeyMap} = get_range_key_maps(Options),
    case {cb_context:req_value(Context, <<"start_key">>)
         ,cb_context:req_value(Context, <<"end_key">>)
         }
    of
        {'undefined', 'undefined'} when Direction =:= 'ascending'  -> {StartKeyMap(StartTime), EndKeyMap(EndTime)};
        {StartKey, 'undefined'}    when Direction =:= 'ascending'  -> {StartKey, EndKeyMap(EndTime)};
        {'undefined', EndKey}      when Direction =:= 'ascending'  -> {StartKeyMap(StartTime), EndKey};
        {StartKey, EndKey}         when Direction =:= 'ascending'  -> {StartKey, EndKey};
        {'undefined', 'undefined'} when Direction =:= 'descending' -> {EndKeyMap(EndTime), StartKeyMap(StartTime)};
        {StartKey, 'undefined'}    when Direction =:= 'descending' -> {StartKey, StartKeyMap(StartTime)};
        {'undefined', EndKey}      when Direction =:= 'descending' -> {EndKeyMap(EndTime), EndKey};
        {StartKey, EndKey}         when Direction =:= 'descending' -> {StartKey, EndKey}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Suffix the Timestamp to the provided key map option. Useful to use
%% generate the keys like [TS, InteractionId] for the end key in
%% cb_cdrs for example.
%% @end
%%--------------------------------------------------------------------
-spec suffix_key_fun(keymap()) -> keymap_fun().
suffix_key_fun('undefined') -> fun(_) -> 'undefined' end;
suffix_key_fun(K) when is_binary(K) -> fun(Ts) -> [Ts, K] end;
suffix_key_fun(K) when is_integer(K) -> fun(Ts) -> [Ts, K] end;
suffix_key_fun(K) when is_list(K) -> fun(Ts) -> [Ts | K] end;
suffix_key_fun(K) when is_function(K, 1) -> K.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Equivalent to direction/2, setting Options to an empty list.
%% @end
%%--------------------------------------------------------------------
-spec direction(cb_context:context()) -> direction().
direction(Context) ->
    direction(Context, []).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find view sort direction from CrossbarView Options or request
%% query string. Default to `descending`.
%% @end
%%--------------------------------------------------------------------
-spec direction(cb_context:context(), options()) -> direction().
direction(Context, Options) ->
    case props:get_value('descending', Options, 'false')
        orelse kz_json:is_true(<<"descending">>, cb_context:query_string(Context))
        orelse kz_json:is_false(<<"ascending">>, cb_context:query_string(Context), 'true')
    of
        'true' -> 'descending';
        'false' -> 'ascending'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Equivalent to time_range/2, setting Options to an empty list.
%% @end
%%--------------------------------------------------------------------
-spec time_range(cb_context:context()) -> time_range() | cb_context:context().
time_range(Context) -> time_range(Context, []).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get view lookup time range from request or CrossbarView Options or
%% return default range based on system configuration(maximum range).
%%
%% Start time (`created_from`) should always be prior to end time (`created_to).
%%
%% Possible option:
%%   - `max_range`: Maximum range allowed
%%   - `range_key`: the key name to look values (created, modified or ...)
%%   - `{RANGE_KEY}_from`: start time
%%   - `{RANGE_KEY}_to`: end time
%% @end
%%--------------------------------------------------------------------
-spec time_range(cb_context:context(), options()) -> time_range() | cb_context:context().
time_range(Context, Options) ->
    time_range(Context, Options, props:get_ne_binary_value('range_key', Options, <<"created">>)).

-spec time_range(cb_context:context(), options(), ne_binary()) -> time_range() | cb_context:context().
time_range(Context, Options, Key) ->
    MaxRange = get_max_range(Options),
    TSTime = kz_time:current_tstamp(),
    RangeTo = get_time_key(Context, <<Key/binary, "_to">>, Options, TSTime),
    RangeFrom = get_time_key(Context, <<Key/binary, "_from">>, Options, RangeTo - MaxRange),
    time_range(Context, MaxRange, Key, RangeFrom, RangeTo).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Checks whether or not start time is prior to end time. Returns a ranged
%% tuple `{start_time, end_time}` or `context` with validation error.
%% @end
%%--------------------------------------------------------------------
-spec time_range(cb_context:context(), pos_integer(), ne_binary(), pos_integer(), pos_integer()) -> time_range() | cb_context:context().
time_range(Context, MaxRange, Key, RangeFrom, RangeTo) ->
    Path = <<Key/binary, "_from">>,
    case RangeTo - RangeFrom of
        N when N < 0 ->
            Msg = kz_json:from_list(
                    [{<<"message">>, <<Path/binary, " is prior to ", Key/binary, "_to">>}
                    ,{<<"cause">>, RangeFrom}
                    ]),
            cb_context:add_validation_error(Path, <<"date_range">>, Msg, Context);
        N when N > MaxRange ->
            Msg = kz_json:from_list(
                    [{<<"message">>, <<Key/binary, "_to is more than "
                                       ,(integer_to_binary(MaxRange))/binary
                                       ," seconds from ", Path/binary>>
                     }
                    ,{<<"cause">>, RangeTo}
                    ]),
            cb_context:add_validation_error(Path, <<"date_range">>, Msg, Context);
        _ ->
            {RangeFrom, RangeTo}
    end.

%%%===================================================================
%%% Load view internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load view results based on options.
%% @end
%%--------------------------------------------------------------------
-spec load_view(load_params() | cb_context:context()) -> cb_context:context().
load_view(#{should_paginate := 'true'}=LoadMap) ->
    {LastKey, JObjs} = get_ordered(LoadMap),
    format_response(LastKey, JObjs, LoadMap);
load_view(#{}=LoadMap) ->
    {LastKey, JObjs} = get_ordered(LoadMap),
    format_response(LastKey, JObjs, LoadMap);
load_view(Context) ->
    Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get ordered result from databases. It will figure out last view's JObj
%% key to set as the `next_start_key` if pagination is requested.
%% @end
%%--------------------------------------------------------------------
-spec get_ordered(load_params()) -> {last_key(), kz_json:objects()}.
get_ordered(#{databases := Dbs}=LoadMap) ->
    {_, LastKey, JObjs} =
        lists:foldl(fun fold_query/2, {0, 'undefined', []}, [{Db, LoadMap} || Db <- Dbs]),
    {LastKey, JObjs}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Fold over databases and fetch result from each and count total result.
%% If pagination is requested keeps track of last key.
%% If `page_size` is not in the options, make unlimited get_results.
%% @end
%%--------------------------------------------------------------------
-spec fold_query(query_db_params(), query_acc()) -> query_acc().
fold_query({_, #{page_size := PageSize}}, {_, LastKey, QueriedJObjs}=Acc)
  when is_integer(PageSize),
       PageSize > 0,
       length(QueriedJObjs) == PageSize,
       LastKey =/= 'undefined' ->
    lager:debug("page size exhausted: ~s", [PageSize]),
    Acc;
fold_query({Db, #{view := View
                 ,view_options := ViewOptions
                 ,direction := Direction
                 ,page_size := PageSize
                 }=LoadMap}, {TotalQueried, LastKey, QueriedJObjs}) when is_integer(PageSize), PageSize > 0 ->
    Queried = erlang:length(QueriedJObjs),
    LimitWithLast = 1 + PageSize - Queried,

    lager:debug("querying view '~s' from '~s', starting at '~p' with page size ~b and limit ~b in direction ~s"
               ,[View, Db, maps:get(start_key, LoadMap, "no_start_key"), PageSize, LimitWithLast, Direction]
               ),

    {Pref1, DbResults} = timer:tc(fun() -> perform_query(LimitWithLast, Db, View, [{'limit', LimitWithLast} | ViewOptions]) end),
    DbResultsLength = erlang:length(DbResults),

    {Pref2, {NewLastKey, JObjs}} = timer:tc(fun() -> last_key(LastKey, lists:reverse(DbResults), LimitWithLast, DbResultsLength) end),
    {Pref3, FilteredJObj} = timer:tc(fun() -> apply_filter(maps:get(mapper, LoadMap, 'undefined'), JObjs) end),

    Filtered = length(FilteredJObj),
    NewTotalQueried = TotalQueried + Filtered,

    io:format("~nquery took: ~pms last_key took: ~pms apply_filter took: ~pms~n"
             ,[Pref1/?MILLISECONDS_IN_SECOND, Pref2/?MILLISECONDS_IN_SECOND, Pref3/?MILLISECONDS_IN_SECOND]),

    lager:debug("db_returned: ~b passed_filter: ~p total_queried: ~b next_start_key: ~p"
               ,[DbResultsLength, Filtered, NewTotalQueried, NewLastKey]
               ),

    {NewTotalQueried, NewLastKey, QueriedJObjs ++ FilteredJObj};
fold_query({Db, #{view := View
                 ,view_options := ViewOptions
                 ,direction := Direction
                 }=LoadMap}, {TotalQueried, LastKey, QueriedJObjs}) ->
    lager:debug("querying view '~s' from '~s', starting at '~p' in direction ~s", [View, Db, maps:get(start_key, LoadMap, "no_start_key"), Direction]),

    {Pref1, DbResults} = timer:tc(fun() -> perform_query('undefined', Db, View, ViewOptions) end),
    DbResultsLength = erlang:length(DbResults),

    {Pref2, FilteredJObj} = timer:tc(fun() -> apply_filter(maps:get(mapper, LoadMap, 'undefined'), DbResults) end),

    Filtered = length(FilteredJObj),
    NewTotalQueried = TotalQueried + Filtered,

    io:format("~nquery took: ~pms apply_filter took: ~pms~n"
             ,[Pref1/?MILLISECONDS_IN_SECOND, Pref2/?MILLISECONDS_IN_SECOND]),

    lager:debug("db_returned: ~b passed_filter: ~p total_queried: ~b"
               ,[DbResultsLength, Filtered, NewTotalQueried]
               ),

    {NewTotalQueried, LastKey, QueriedJObjs ++ FilteredJObj}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Make database request
%% @end
%%--------------------------------------------------------------------
-spec perform_query(api_pos_integer(), ne_binary(), ne_binary(), kz_datamgr:view_options()) -> kz_json:objects().
perform_query(Limit, _, _, _) when is_integer(Limit), Limit =< 0 -> [];
perform_query(_, Db, View, Options) ->
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
-spec apply_filter(mapper_fun(), kz_json:objects()) -> kz_json:objects().
apply_filter('undefined', JObjs) ->
    lists:reverse(JObjs);
apply_filter(Mapper, JObjs) when is_function(Mapper, 1) ->
    Fun = fun(JObj0, Acc) ->
                  JObj = Mapper(JObj0),
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
-spec format_response(last_key(), kz_json:objects(), load_params()) -> cb_context:context().
format_response('undefined', JObjs, #{context := Context}) ->
    Envelope = remove_paging(cb_context:resp_envelope(Context)),
    crossbar_doc:handle_datamgr_success(JObjs, cb_context:set_resp_envelope(Context, Envelope));
format_response(NextStartKey, JObjs, #{context := Context, start_key := StartKey}) ->
    Envelope = add_paging(StartKey, length(JObjs), NextStartKey, cb_context:resp_envelope(Context)),
    crossbar_doc:handle_datamgr_success(JObjs, cb_context:set_resp_envelope(Context, Envelope)).

-spec add_paging(range_key(), pos_integer(), range_key(), kz_json:object()) -> kz_json:object().
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

%%%===================================================================
%%% Build load view parameters internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create ranged view lookup database list using start/end time and
%% direction.
%% @end
%%--------------------------------------------------------------------
-spec get_range_modbs(cb_context:context(), options(), direction(), gregorian_seconds(), gregorian_seconds()) -> ne_binaries().
get_range_modbs(Context, Options, Direction, StartTime, EndTime) ->
    case props:get_value('databases', Options) of
        'undefined' when Direction =:= 'ascending' ->
            kazoo_modb:get_range(cb_context:account_id(Context), StartTime, EndTime);
        'undefined' when Direction =:= 'descending' ->
            lists:reverse(kazoo_modb:get_range(cb_context:account_id(Context), StartTime, EndTime));
        Dbs when Direction =:= 'ascending' ->
            lists:usort(Dbs);
        Dbs when Direction =:= 'descending' ->
            lists:reverse(lists:usort(Dbs))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If pagination available, returns page size.
%%
%% Note: DO NOT ADD ONE (1) TO PAGE_SIZE/LIMIT! Load function will add it.
%% @end
%%--------------------------------------------------------------------
-spec get_limit(cb_context:context(), options()) -> api_pos_integer().
get_limit(Context, Options) ->
    case cb_context:should_paginate(Context) of
        'true' ->
            case props:get_integer_value('limit', Options) of
                'undefined' -> cb_context:pagination_page_size(Context);
                Limit ->
                    lager:debug("got limit from options: ~b", [Limit]),
                    Limit
            end;
        'false' ->
            lager:debug("pagination disabled in context"),
            'undefined'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get time key value from options or request.
%% @end
%%--------------------------------------------------------------------
-spec get_time_key(cb_context:context(), ne_binary(), options(), pos_integer()) -> pos_integer().
get_time_key(Context, Key, Options, Default) ->
    case props:get_integer_value(Key, Options) of
        'undefined' ->
            try kz_term:to_integer(cb_context:req_value(Context, Key, 0)) of
                T when T > 0 -> T;
                _ -> Default
            catch
                _:_ -> Default
            end;
        Value -> Value
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get max_range from option or system config.
%% @end
%%--------------------------------------------------------------------
-spec get_max_range(options()) -> pos_integer().
get_max_range(Options) ->
    case props:get_integer_value('max_range', Options) of
        'undefined' -> ?MAX_RANGE;
        MaxRange -> MaxRange
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Build customized start/end key mapper. If a map option is not
%% present in the options, the timestamp will be used as the key.
%%
%% Possible key maps options:
%%   - `key_map`: use this to map both start/end keys
%%   - `start_key_map`: maps start key only
%%   - `end_key_map`: maps end key only
%%
%% Possible maps type:
%%   - a binary value: to construct keys like [<<"account">>, Timestamp]
%%   - an integer value: to construct keys like [1234, Timestamp]
%%   - a list: to construct keys like [<<"en">>, <<"us">>, Timestamp]
%%   - a function/1: To customize your own key using a function
%% @end
%%--------------------------------------------------------------------
-spec get_range_key_maps(options()) -> {keymap_fun(), keymap_fun()}.
get_range_key_maps(Options) ->
    case props:get_value('key_map', Options) of
        'undefined' ->
            {get_key_map(props:get_value('start_key_map', Options)), get_key_map(props:get_value('end_key_map', Options))};
        KeyMap -> {map_keymap(KeyMap), map_keymap(KeyMap)}
    end.

-spec get_key_map('undefined' | keymap()) -> keymap_fun().
get_key_map('undefined') -> fun kz_term:identity/1;
get_key_map(KeyMap) -> map_keymap(KeyMap).

-spec map_keymap(keymap()) -> keymap_fun().
map_keymap('nil') -> fun(_) -> 'undefined' end;
map_keymap('undefined') -> fun(_) -> 'undefined' end;
map_keymap(K) when is_binary(K) -> fun(Ts) -> [K, Ts] end;
map_keymap(K) when is_integer(K) -> fun(Ts) -> [K, Ts] end;
map_keymap(K) when is_list(K) -> fun(Ts) -> K ++ [Ts] end;
map_keymap(K) when is_function(K, 1) -> K.
