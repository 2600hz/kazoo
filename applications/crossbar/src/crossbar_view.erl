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
        ,load_range/2, load_range/3
        ,load_modb/2, load_modb/3

        ,build_load_range_params/3
        ,build_load_modb_params/3
        ,build_view_query/5

        ,direction/1, direction/2
        ,time_range/1, time_range/2, time_range/5
        ,start_end_keys/2, start_end_keys/5
        ,suffix_key_fun/1

        ,chunk_send_jsons/3

        ,map_doc_fun/0
        ,map_value_fun/0
        ]).

-include("crossbar.hrl").

-define(CB_VIEW_OPTIONS,
        ['ascending', 'created_from'
        ,'created_to', 'databases'
        ,'descending' ,'end_key_map'
        ,'key_map', 'mapper'
        ,'max_range', 'start_key_map'
        ,'chunked_mapper', 'response_type'
        ,'chunk_size', 'cowboy_req'
        ,'is_chunked'
        ]).

-type direction() :: 'ascending' | 'descending'.

-type time_range() :: {gregorian_seconds(), gregorian_seconds()}.

-type range_key() :: kz_json:json_term().
-type api_range_key() :: kz_json:api_json_term().
-type range_keys() :: {api_range_key(), api_range_key()}.

-type keymap_fun() :: fun((range_key()) -> api_range_key()).
-type keymap() :: 'nil' | api_ne_binary() | ne_binaries() | integer() | keymap_fun().

-type user_mapper_fun() :: 'undefined' |
                           fun((kz_json:objects()) -> kz_json:objects()) |
                           fun((kz_json:object(), kz_json:objects()) -> kz_json:objects()) |
                           fun((cb_context:context(), kz_json:object(), kz_json:objects()) -> kz_json:objects()).

-type chunked_mapper_ret() :: {binaries() | kz_json:objects() | non_neg_integer() | 'stop', cb_context:context()}.

-type chunked_mapper_fun() :: 'undefined' |
                              fun((cb_cowboy_payload(), kz_json:objects()) -> chunked_mapper_ret()) |
                              fun((cb_cowboy_payload(), kz_json:objects(), ne_binary()) -> chunked_mapper_ret()).

-type mapper_fun() :: 'undefined' |
                      fun((kz_json:objects()) -> kz_json:objects()) |
                      fun((kz_json:object(), kz_json:objects()) -> kz_json:objects()).

-type options() :: kazoo_data:view_options() |
                   [{'chunked_mapper', chunked_mapper_fun()} |
                    {'chunk_size', pos_integer()} |
                    {'cowboy_req', cowboy_req:req()} |
                    {'created_from', pos_integer()} |
                    {'created_to', pos_integer()} |
                    {'databases', ne_binaries()} |
                    {'end_key_map', keymap()} |
                    {'is_chunked', boolean()} |
                    {'key_map', keymap()} |
                    {'mapper', user_mapper_fun()} |
                    {'max_range', pos_integer()} |
                    {'response_type', 'json' | 'csv'} |
                    {'start_key_map', keymap()}
                   ].

-type load_params() :: #{chunked_mapper => chunked_mapper_fun()
                        ,chunk_size => pos_integer()
                        ,context => cb_context:context()
                        ,cowboy_req => cowboy_req:req()
                        ,databases => ne_binaries()
                        ,direction => direction()
                        ,end_key => range_key()
                        ,end_time => gregorian_seconds()
                        ,has_qs_filter => boolean()
                        ,is_chunked => boolean()
                        ,last_key => last_key()
                        ,mapper => mapper_fun()
                        ,page_size => pos_integer()
                        ,queried_jobjs => kz_json:objects()
                        ,response_type => 'json' | 'csv'
                        ,should_paginate => boolean()
                        ,start_key => range_key()
                        ,start_time => gregorian_seconds()
                        ,started_chunk => boolean()
                        ,total_queried => non_neg_integer()
                        ,view => ne_binary()
                        ,view_options => kazoo_data:view_options()
                        }.

-type last_key() :: api_range_key().

-export_type([range_key/0, api_range_key/0, range_keys/0
             ,options/0, direction/0
             ,mapper_fun/0 ,user_mapper_fun/0
             ,keymap/0, keymap_fun/0
             ,time_range/0
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
%% Equivalent of load/3, setting Options to an empty list.
%% @end
%%--------------------------------------------------------------------
-spec load_range(cb_context:context(), ne_binary()) -> cb_context:context().
load_range(Context, View) ->
    load_range(Context, View, []).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function attempts to load the context with the results of a view
%% run against the accounts database.
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-spec load_range(cb_context:context(), ne_binary(), options()) -> cb_context:context().
load_range(Context, View, Options) ->
    load_view(build_load_range_params(Context, View, Options)).

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
%% Generates corssbar_view options map for looking up view in time range.
%% @end
%%--------------------------------------------------------------------
-spec build_load_range_params(cb_context:context(), ne_binary(), options()) -> load_params() | cb_context:context().
build_load_range_params(Context, View, Options) ->
    Direction = direction(Context, Options),

    TimeFilterKey = props:get_ne_binary_value('range_key', Options, <<"created">>),

    HasQSFilter = crossbar_filter:is_defined(Context)
        andalso not crossbar_filter:is_only_time_filter(Context, TimeFilterKey),
    UserMapper = props:get_value('mapper', Options),

    IsChunked = props:is_true('is_chunked', Options, 'false'),
    Req = props:get_value('cowboy_req', Options),

    case time_range(Context, Options, TimeFilterKey) of
        {StartTime, EndTime}
          when (IsChunked
                andalso Req =/= 'undefined'
               ) orelse not IsChunked ->
            {StartKey, EndKey} = start_end_keys(Context, Options, Direction, StartTime, EndTime),
            maps:from_list(
              props:filter_undefined(
                [{'chunked_mapper', props:get_value('chunked_mapper', Options)}
                ,{'chunk_size', get_chunk_size(Options)}
                ,{'cowboy_req', Req}
                ,{'context', cb_context:setters(Context
                                               ,[{fun cb_context:set_doc/2, []}
                                                ,{fun cb_context:store/3, 'has_qs_filter', HasQSFilter}
                                                ])
                 }
                ,{'databases', props:get_value('databases', Options, [cb_context:account_db(Context)])}
                ,{'direction', Direction}
                ,{'end_key', EndKey}
                ,{'end_time', EndTime}
                ,{'has_qs_filter', HasQSFilter}
                ,{'is_chunked', IsChunked}
                ,{'mapper', crossbar_filter:build_with_mapper(Context, UserMapper, HasQSFilter)}
                ,{'page_size', get_limit(Context, Options)}
                ,{'queried_jobjs', []}
                ,{'response_type', props:get_value('response_type', Options, 'json')}
                ,{'should_paginate', cb_context:should_paginate(Context)}
                ,{'start_time', StartTime}
                ,{'start_key', StartKey}
                ,{'total_queried', 0}
                ,{'view', View}
                ,{'view_options', build_view_query(Options, Direction, StartKey, EndKey, HasQSFilter)}
                ])
             );
        {_, _} ->
            lager:warning("crossbar module has requested a chunked response but did not provide cowboy_req as options"),
            cb_context:add_system_error('internal_error', Context);
        Ctx -> Ctx
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Generates corssbar_view options map for looking up MODBs view.
%% @end
%%--------------------------------------------------------------------
-spec build_load_modb_params(cb_context:context(), ne_binary(), options()) -> load_params() | cb_context:context().
build_load_modb_params(Context, View, Options) ->
    case build_load_range_params(Context, View, Options) of
        #{context := Context1
         ,direction := Direction
         ,start_time := StartTime
         ,end_time := EndTime
         }=LoadMap ->
            LoadMap#{databases => get_range_modbs(Context1, Options, Direction, StartTime, EndTime)};
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
-spec build_view_query(options(), direction(), api_range_key(), api_range_key(), boolean()) -> kazoo_data:view_options().
build_view_query(Options, Direction, StartKey, EndKey, HasQSFilter) ->
    DefaultOptions =
        props:filter_undefined(
          [{'startkey', StartKey}
          ,{'endkey', EndKey}
          ,Direction
           | props:delete_keys(['startkey', 'endkey', 'ascending', 'limit' | ?CB_VIEW_OPTIONS], Options)
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
    TSTime = kz_time:now_s(),
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

-spec map_doc_fun() -> mapper_fun().
map_doc_fun() -> fun(JObj, Acc) -> [kz_json:get_value(<<"doc">>, JObj)|Acc] end.

-spec map_value_fun() -> mapper_fun().
map_value_fun() -> fun(JObj, Acc) -> [kz_json:get_value(<<"value">>, JObj)|Acc] end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Encode the JObj and send it in chunked.
%% @end
%%--------------------------------------------------------------------
-spec chunk_send_jsons(cowboy_req:req(), kz_json:objects(), boolean()) -> boolean().
chunk_send_jsons(_, [], StartedChunk) ->
    StartedChunk;
chunk_send_jsons(Req, [JObj|JObjs], StartedChunk) ->
    try kz_json:encode(JObj) of
        JSON when StartedChunk ->
            'ok' = cowboy_req:chunk(<<",", JSON/binary>>, Req),
            chunk_send_jsons(Req, JObjs, StartedChunk);
        JSON ->
            'ok' = cowboy_req:chunk(JSON, Req),
            chunk_send_jsons(Req, JObjs, 'true')
    catch
        'throw':{'json_encode', {'bad_term', _Term}} ->
            lager:debug("json encoding failed on ~p", [_Term]),
            chunk_send_jsons(Req, JObjs, StartedChunk);
        _E:_R ->
            lager:debug("failed to encode response: ~s: ~p", [_E, _R]),
            chunk_send_jsons(Req, JObjs, StartedChunk)
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
load_view(#{is_chunked := 'true', response_type := 'json', cowboy_req := Req0}=LoadMap0) ->
    io:format("~n JSON chunk LoadMap ~p~n~n", [maps:remove(context, maps:remove(cowboy_req, LoadMap0))]),
    Headers = cowboy_req:get('resp_headers', Req0),
    {'ok', Req1} = cowboy_req:chunked_reply(200, Headers, Req0),

    'ok' = cowboy_req:chunk("{\"data\":[", Req1),
    LoadMap1 = get_results(LoadMap0#{cowboy_req => Req1}),
    'ok' = cowboy_req:chunk("],", Req1),

    format_chunked_response(LoadMap1),
    {Req1, cb_context:store(maps:get(context, LoadMap1), 'is_chunked', 'true')};
load_view(#{is_chunked := 'true', response_type := 'csv', cowboy_req := Req0}=LoadMap0) ->
    io:format("~n CSV chunk LoadMap ~p~n~n", [maps:remove(context, maps:remove(cowboy_req, LoadMap0))]),
    Headers0 = [{<<"content-type">>, <<"application/octet-stream">>}
               ,{<<"content-disposition">>, <<"attachment; filename=\"result.csv\"">>}
               ],
    Headers = props:set_values(Headers0, cowboy_req:get('resp_headers', Req0)),
    {'ok', Req1} = cowboy_req:chunked_reply(200, Headers, Req0),

    LoadMap1 = get_results(LoadMap0#{cowboy_req => Req1}),
    {Req1, cb_context:store(maps:get(context, LoadMap1), 'is_chunked', 'true')};
load_view(#{}=LoadMap) ->
    io:format("~n Regular LoadMap ~p~n~n", [maps:remove(context, LoadMap)]),
    format_response(get_results(LoadMap));
load_view(Context) ->
    Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get ordered result from databases. It will figure out last view's JObj
%% key to set as the `next_start_key` if pagination is requested.
%% @end
%%--------------------------------------------------------------------
-spec get_results(load_params()) -> load_params().
get_results(#{databases := Dbs}=LoadMap) ->
    fold_query(Dbs, LoadMap#{total_queried => 0, last_key => 'undefined', queried_jobjs => []}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Fold over databases and fetch result from each and count total result.
%% If pagination is requested keeps track of last key.
%% If `page_size` is not in the options, make unlimited get_results.
%% @end
%%--------------------------------------------------------------------
-spec fold_query(ne_binaries(), load_params()) -> load_params().
fold_query([], LoadMap) ->
    lager:debug("databases exhausted"),
    LoadMap;
%% query is limited by page_size
fold_query([Db|RestDbs]=Dbs, #{view := View
                              ,view_options := ViewOpts
                              ,direction := Direction
                              ,is_chunked := IsChunked
                              ,chunk_size := ChunkSize
                              ,total_queried := TotalQueried
                              ,context := Context
                              }=LoadMap) ->
    PageSize = maps:get(page_size, LoadMap, 'undefined'),
    LimitWithLast = limit_with_last_key(IsChunked, PageSize, ChunkSize, TotalQueried),

    lager:debug("querying view '~s' from '~s', starting at '~p' with page size ~b and limit ~p in direction ~s"
               ,[View, Db, maps:get(start_key, LoadMap, "no_start_key"), PageSize, LimitWithLast, Direction]
               ),

    ViewOptions = props:filter_undefined([{'limit', LimitWithLast} | ViewOpts]),

    lager:debug("kz_datamgr:get_results(~s, ~s, ~p)", [Db, View, ViewOptions]),
    case kz_datamgr:get_results(Db, View, ViewOptions) of
        {'error', 'not_found'} when [] =:= RestDbs ->
            lager:debug("either the db ~s or view ~s was not found", [Db, View]),
            LoadMap#{context => crossbar_util:response_missing_view(Context)};
        {'error', 'not_found'} ->
            lager:debug("either the db ~s or view ~s was not found", [Db, View]),
            fold_query(RestDbs, LoadMap);
        {'error', Error} ->
            LoadMap#{context => crossbar_doc:handle_datamgr_errors(Error, View, Context)};
        {'ok', JObjs} ->
            handle_query_result(LoadMap, Dbs, JObjs, LimitWithLast)
    end.

-spec handle_query_result(load_params(), ne_binaries(), kz_json:objects(), api_pos_integer()) -> load_params().
handle_query_result(LoadMap, [Db|RestDbs]=Dbs, DbResults, Limit) ->
    DbResultsLength = erlang:length(DbResults),

    {LastKey, JObjs} = last_key(maps:get(last_key, LoadMap, 'undefined'), DbResults, Limit, DbResultsLength),
    FilteredJObj = apply_filter(maps:get(mapper, LoadMap, 'undefined'), JObjs),

    Filtered = length(FilteredJObj),

    lager:debug("db_returned: ~b passed_filter: ~p next_start_key: ~p", [DbResultsLength, Filtered, LastKey]),

    {LengthOrStop, LoadMap2} = maybe_send_chunked(LoadMap#{last_key => LastKey}, Db, FilteredJObj, Filtered),
    case LengthOrStop =/= 'stop'
        andalso check_page_size_and_length(LoadMap2, LengthOrStop, Limit, LastKey)
    of
        'false' -> LoadMap2;
        {'exhausted', LoadMap3} -> LoadMap3;
        {'same_db', LoadMap3} -> fold_query(Dbs, LoadMap3);
        {'next_db', LoadMap3} -> fold_query(RestDbs, LoadMap3)
    end.

-spec check_page_size_and_length(load_params(), non_neg_integer(), non_neg_integer() | 'undefined', last_key()) ->
                                        {'exhausted' | 'next_db' | 'same_db', load_params()}.
%% page_size is exhausted when query is limited by page_size
check_page_size_and_length(#{page_size := PageSize
                            ,total_queried := TotalQueried
                            }=LoadMap, Length, _, LastKey)
  when is_integer(PageSize)
       andalso PageSize > 0
       andalso TotalQueried + Length == PageSize
       andalso LastKey =/= 'undefined' ->
    lager:debug("page size exhausted: ~b", [PageSize]),
    {'exhausted', LoadMap#{total_queried => TotalQueried + Length}};
%% query next chunk from same db to find next_start_key/last_key when query is limited by page_size
check_page_size_and_length(#{page_size := PageSize, total_queried := TotalQueried}=LoadMap, Length, Limit, LastKey)
  when is_integer(PageSize)
       andalso is_integer(Limit)
       andalso PageSize > 0
       andalso Length == Limit
       andalso LastKey =/= 'undefined' ->
    {'same_db', LoadMap#{total_queried => TotalQueried + Length}};
%% query next_db to find last_key
check_page_size_and_length(#{page_size := PageSize, total_queried := TotalQueried}=LoadMap, Length, Limit, LastKey)
  when is_integer(PageSize)
       andalso is_integer(Limit)
       andalso PageSize > 0
       andalso Length == Limit
       andalso LastKey =:= 'undefined' ->
    {'next_db', LoadMap#{total_queried => TotalQueried + Length}};
%% query next_db when it's an unlimited query
check_page_size_and_length(#{total_queried := TotalQueried}=LoadMap, Length, 'undefined', _) ->
    {'next_db', LoadMap#{total_queried => TotalQueried + Length}};
%% query next chunk from same db when query is chunked and unlimited
check_page_size_and_length(#{total_queried := TotalQueried}=LoadMap, Length, Limit, _)
  when Length == Limit ->
    {'same_db', LoadMap#{total_queried => TotalQueried + Length}};
%% just query next_db
check_page_size_and_length(#{total_queried := TotalQueried}=LoadMap, Length, _, _) ->
    {'next_db', LoadMap#{total_queried => TotalQueried + Length}}.

-spec limit_with_last_key(boolean(), api_pos_integer(), pos_integer(), non_neg_integer()) -> api_pos_integer().
limit_with_last_key('false', 'undefined', _, _) ->
    'undefined';
limit_with_last_key('false', PageSize, _, TotalQueried) ->
    1 + PageSize - TotalQueried;
limit_with_last_key('true', 'undefined', ChunkSize, TotalQueried) ->
    1 + ChunkSize - TotalQueried;
limit_with_last_key('true', PageSize, PageSize, TotalQueried) ->
    1 + PageSize - TotalQueried;
limit_with_last_key('true', PageSize, ChunkSize, TotalQueried) when ChunkSize > PageSize ->
    1 + ChunkSize - TotalQueried;
limit_with_last_key('true', PageSize, _ChunkSize, TotalQueried) ->
    1 + PageSize - TotalQueried.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Send view results for this database in chunked if the request is
%% requesting chunked response.
%% @end
%%--------------------------------------------------------------------
-spec maybe_send_chunked(load_params(), ne_binary(), kz_json:objects(), non_neg_integer()) -> {non_neg_integer() | 'stop', load_params()}.
maybe_send_chunked(#{is_chunked := 'true'}=LoadMap, Db, JObjs, _) ->
    send_chunked_mapper(LoadMap, JObjs, Db);
maybe_send_chunked(#{queried_jobjs := QueriedJObjs}=LoadMap, _, JObjs, Length) ->
    {Length, LoadMap#{queried_jobjs => QueriedJObjs ++ JObjs}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Apply chunked mapper function and send result in chunked
%%
%% Please, please set Context `started_chunk` in your code if you're
%% sending the chunk yourself.
%% Also return `no_result` if you sent the chunk.
%%
%% NOTE: YOU HAVE TO CHECK IF CHUNKED IS STARTED OR NOT IN CSV AND
%%       SEND HEADERS!
%% @end
%%--------------------------------------------------------------------
-spec send_chunked_mapper(load_params(), kz_json:objects(), ne_binary()) -> {non_neg_integer() | 'stop', load_params()}.
send_chunked_mapper(#{cowboy_req := Req
                     ,context := Context0
                     ,queried_jobjs := QueriedJObjs
                     }=LoadMap, JObjs, Db) ->
    StartedChunk = maps:get(started_chunk, LoadMap, 'false'),
    Context1 = cb_context:store(Context0, 'started_chunk', StartedChunk),
    ChunkedMapper = maps:get(chunked_mapper, LoadMap, 'undefined'),

    {Resp, Context2} = apply_chunked_mapper(ChunkedMapper, LoadMap#{context => Context1}, JObjs, Db),

    IsContextStartedChunk = cb_context:fetch(Context2, 'started_chunk', StartedChunk),

    case {Resp, maps:get(response_type, LoadMap)} of
        {'stop', _} ->
            {'stop', LoadMap#{context => Context2}};
        {SentLength, 'json'} when is_integer(SentLength) ->
            {SentLength, LoadMap#{started_chunk => IsContextStartedChunk, context => Context2}};
        {RespJObj, 'json'} when is_list(RespJObj) ->
            NewStartedChunk = chunk_send_jsons(Req, RespJObj, IsContextStartedChunk),
            {length(RespJObj), LoadMap#{started_chunk => NewStartedChunk
                                       ,context => Context2
                                       ,queried_jobjs => QueriedJObjs ++ RespJObj
                                       }
            };
        {SentLength, 'csv'} when is_integer(SentLength) ->
            {SentLength, LoadMap#{started_chunk => IsContextStartedChunk, context => Context2}};
        {Resp, 'csv'} when is_list(Resp) ->
            'ok' = cowboy_req:chunk(Resp, Req),
            {length(Resp), LoadMap#{started_chunk => IsContextStartedChunk, context => Context2}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Apply filter function if provided while keep maintaining the result
%% order.
%% Filter function can be arity 1 and 2.
%% @end
%%--------------------------------------------------------------------
-spec apply_filter(mapper_fun(), kz_json:objects()) -> kz_json:objects().
apply_filter('undefined', JObjs) ->
    lists:reverse(JObjs);
apply_filter(Mapper, JObjs) when is_function(Mapper, 1) ->
    %% Can I trust you to return sorted result in the correct direction?
    Mapper(JObjs);
apply_filter(Mapper, JObjs) when is_function(Mapper, 2) ->
    [JObj
     || JObj <- lists:foldl(Mapper, [], JObjs),
        not kz_term:is_empty(JObj)
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Apply user specified mapper.
%%
%% NOTE: Keep the original passed JOBjs ORDER in your mapper!
%% @end
%%--------------------------------------------------------------------
-spec apply_chunked_mapper(chunked_mapper_fun(), load_params(), kz_json:objects(), ne_binary()) -> chunked_mapper_ret().
apply_chunked_mapper('undefined', #{context := Context, response_type := 'json'}, JObjs, _) ->
    {JObjs, Context};
apply_chunked_mapper(Mapper, #{cowboy_req := Req, context := Context}, JObjs, _) when is_function(Mapper, 2) ->
    Mapper({Req, Context}, JObjs);
apply_chunked_mapper(Mapper, #{cowboy_req := Req, context := Context}, JObjs, Db) when is_function(Mapper, 3) ->
    Mapper({Req, Context}, JObjs, Db).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Figure out the last key if we have some result and page size is not
%% exhausted yet.
%% @end
%%--------------------------------------------------------------------
-spec last_key(last_key(), kz_json:objects(), non_neg_integer() | 'undefined', non_neg_integer()) ->
                      {last_key(), kz_json:objects()}.
last_key(LastKey, [], _, _) ->
    {LastKey, []};
last_key(LastKey, JObjs, 'undefined', _) ->
    {LastKey, lists:reverse(JObjs)};
last_key(LastKey, JObjs, Limit, Returned) when Returned < Limit ->
    {LastKey, lists:reverse(JObjs)};
last_key(_LastKey, JObjs, Limit, Returned) when Returned == Limit ->
    [Last|JObjs1] = lists:reverse(JObjs),
    {kz_json:get_value(<<"key">>, Last), JObjs1}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If the last key is known set as the next_start_key in the
%% response envelope.
%% @end
%%--------------------------------------------------------------------
-spec format_response(load_params()) -> cb_context:context().
format_response(#{total_queried := TotalQueried
                 ,queried_jobjs := JObjs
                 ,context := Context
                 }=LoadMap) ->
    NextStartKey = maps:get(last_key, LoadMap, 'undefined'),
    StartKey = maps:get(start_key, LoadMap, 'undefined'),
    Envelope = add_paging(StartKey, TotalQueried, NextStartKey, cb_context:resp_envelope(Context)),
    crossbar_doc:handle_datamgr_success(JObjs, cb_context:set_resp_envelope(Context, Envelope)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Send the reset of the envelope, properly set page size and
%% `next_start_key`.
%% @end
%%--------------------------------------------------------------------
-spec format_chunked_response(load_params()) -> 'ok'.
format_chunked_response(#{total_queried := TotalQueried
                         ,cowboy_req := Req
                         ,context := Context
                         }=LoadMap) ->
    NextStartKey = maps:get(last_key, LoadMap, 'undefined'),
    StartKey = maps:get(start_key, LoadMap, 'undefined'),
    EnvJObj = add_paging(StartKey, TotalQueried, NextStartKey, cb_context:resp_envelope(Context)),
    EnvProps = api_util:create_chunked_resp_envelope(cb_context:set_resp_envelope(Context, EnvJObj)),
    Encoded = [<<"\"", K/binary, "\":\"", (kz_term:to_binary(V))/binary, "\"">>
                   || {K, V} <- EnvProps,
                      kz_term:is_not_empty(V)
              ],
    'ok' = cowboy_req:chunk(<<(kz_binary:join(Encoded))/binary, "}">>, Req).

%% @private
-spec add_paging(api_range_key(), non_neg_integer(), api_range_key(), kz_json:object()) -> kz_json:object().
add_paging(_, _, 'undefined', JObj) ->
    kz_json:delete_keys([<<"start_key">>, <<"page_size">>, <<"next_start_key">>], JObj);
add_paging(StartKey, PageSize, NextStartKey, JObj) ->
    DeleteKeys = [<<"start_key">>, <<"page_size">>, <<"next_start_key">>],
    kz_json:set_values([{<<"start_key">>, StartKey},
                        {<<"page_size">>, PageSize},
                        {<<"next_start_key">>, NextStartKey}
                       ]
                      ,kz_json:delete_keys(DeleteKeys, JObj)
                      ).

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

-spec get_chunk_size(options()) -> api_pos_integer().
get_chunk_size(Options) ->
    case props:get_integer_value('chunk_size', Options) of
        ChunkSize when is_integer(ChunkSize), ChunkSize > 0 -> ChunkSize;
        _ -> kapps_config:get_pos_integer(?CONFIG_CAT, <<"load_view_chunk_size">>, 50)
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
