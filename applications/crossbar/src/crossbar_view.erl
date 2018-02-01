%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, 2600Hz
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
        ,next_chunk/1

        ,build_load_params/3
        ,build_load_range_params/3
        ,build_load_modb_params/3

        ,direction/1, direction/2

        ,start_end_keys/2, start_end_keys/3

        ,time_range/1, time_range/2, time_range/5
        ,ranged_start_end_keys/2, ranged_start_end_keys/5

        ,suffix_key_fun/1

        ,map_doc_fun/0
        ,map_value_fun/0
        ]).

-include("crossbar.hrl").

-define(CB_SPECIFIC_VIEW_OPTIONS,
        ['ascending', 'databases', 'mapper'

         %% non-range query
        ,'end_keymap', 'keymap', 'start_keymap'

         %% chunked query
        ,'chunk_size', 'is_chunked', 'unchunkable'

         %% ranged query
        ,'created_to', 'created_from', 'max_range'
        ,'range_end_keymap', 'range_key_name', 'range_keymap', 'range_start_keymap'
        ]).

-type direction() :: 'ascending' | 'descending'.

-type time_range() :: {kz_time:gregorian_seconds(), kz_time:gregorian_seconds()}.

-type api_range_key() :: 'undefined' | ['undefined'] | kazoo_data:range_key().
-type range_keys() :: {api_range_key(), api_range_key()}.

-type keymap_fun() :: fun((cb_context:context()) -> api_range_key()) |
                      fun((cb_context:context(), kazoo_data:view_options()) -> api_range_key()).
-type keymap() :: api_range_key() | keymap_fun().

-type range_keymap_fun() :: fun((kz_time:gregorian_seconds()) -> api_range_key()).
-type range_keymap() :: 'nil' | api_range_key() | range_keymap_fun().

-type user_mapper_fun() :: 'undefined' |
                           fun((kz_json:objects()) -> kz_json:objects()) |
                           fun((kz_json:object(), kz_json:objects()) -> kz_json:objects()) |
                           fun((cb_context:context(), kz_json:object(), kz_json:objects()) -> kz_json:objects()).

-type mapper_fun() :: 'undefined' |
                      fun((kz_json:objects()) -> kz_json:objects()) |
                      fun((kz_json:object(), kz_json:objects()) -> kz_json:objects()).

-type options() :: kazoo_data:view_options() |
                   [{'databases', kz_term:ne_binaries()} |
                    {'mapper', user_mapper_fun()} |
                    {'max_range', pos_integer()} |

                    %% for non-ranged query
                    {'end_keymap', keymap()} |
                    {'keymap', keymap()} |
                    {'start_keymap', keymap()} |

                    %% for chunked query
                    {'chunk_size', pos_integer()} |
                    {'is_chunked', boolean()} |

                    %% for ranged/modb query
                    {'created_from', pos_integer()} |
                    {'created_to', pos_integer()} |
                    {'range_end_keymap', range_keymap()} |
                    {'range_keymap', range_keymap()} |
                    {'range_key_name', kz_term:ne_binary()} |
                    {'range_start_keymap', range_keymap()}
                   ].

-type load_params() :: #{chunk_size => pos_integer()
                        ,context => cb_context:context()
                        ,databases => kz_term:ne_binaries()
                        ,direction => direction()
                        ,end_key => kazoo_data:range_key()
                        ,end_time => kz_time:gregorian_seconds()
                        ,has_qs_filter => boolean()
                        ,is_chunked => boolean()
                        ,last_key => last_key()
                        ,mapper => mapper_fun()
                        ,page_size => pos_integer()
                        ,queried_jobjs => kz_json:objects()
                        ,should_paginate => boolean()
                        ,start_key => kazoo_data:range_key()
                        ,start_time => kz_time:gregorian_seconds()
                        ,total_queried => non_neg_integer()
                        ,view => kz_term:ne_binary()
                        ,view_options => kazoo_data:view_options()
                        }.

-type last_key() :: api_range_key().

-export_type([range_keys/0, time_range/0
             ,options/0, direction/0
             ,mapper_fun/0 ,user_mapper_fun/0
             ,keymap/0, keymap_fun/0
             ,range_keymap/0, range_keymap_fun/0
             ]
            ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Equivalent of load/3, setting Options to an empty list.
%% @end
%%--------------------------------------------------------------------
-spec load(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
load(Context, View) ->
    load(Context, View, []).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function attempts to load the context with the results of a view
%% run against the accounts database.
%% @end
%%--------------------------------------------------------------------
-spec load(cb_context:context(), kz_term:ne_binary(), options()) -> cb_context:context().
load(Context, View, Options) ->
    load_view(build_load_params(Context, View, Options), Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Equivalent of load/3, setting Options to an empty list.
%% @end
%%--------------------------------------------------------------------
-spec load_range(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
load_range(Context, View) ->
    load_range(Context, View, []).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function attempts to load the context with the timestamped
%% results of a view run against the accounts database.
%% @end
%%--------------------------------------------------------------------
-spec load_range(cb_context:context(), kz_term:ne_binary(), options()) -> cb_context:context().
load_range(Context, View, Options) ->
    load_view(build_load_range_params(Context, View, Options), Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Equivalent of load_modb/3, setting Options to an empty list.
%% @end
%%--------------------------------------------------------------------
-spec load_modb(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
load_modb(Context, View) ->
    load_modb(Context, View, []).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function attempts to load the context with the results of a view
%% run against the account's MODBs.
%% @end
%%--------------------------------------------------------------------
-spec load_modb(cb_context:context(), kz_term:ne_binary(), options()) -> cb_context:context().
load_modb(Context, View, Options) ->
    load_view(build_load_modb_params(Context, View, Options), Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Generates corssbar_view options map for querying view.
%% @end
%%--------------------------------------------------------------------
-spec build_load_params(cb_context:context(), kz_term:ne_binary(), options()) -> load_params() | cb_context:context().
build_load_params(Context, View, Options) ->
    try build_general_load_params(Context, View, Options) of
        #{direction := Direction}=LoadMap ->
            HasQSFilter = crossbar_filter:is_defined(Context),
            UserMapper = props:get_value('mapper', Options),

            {StartKey, EndKey} = start_end_keys(Context, Options, Direction),

            Params = LoadMap#{has_qs_filter => HasQSFilter
                             ,mapper => crossbar_filter:build_with_mapper(Context, UserMapper, HasQSFilter)
                             ,start_key => StartKey
                             ,view_options => build_view_query(Options, Direction, StartKey, EndKey, HasQSFilter)
                             },
            maybe_set_start_end_keys(Params, StartKey, EndKey);
        Ctx -> Ctx
    catch
        _E:_T ->
            lager:debug("exception occurred during building view options for ~s", [View]),
            ST = erlang:get_stacktrace(),
            kz_util:log_stacktrace(ST),
            cb_context:add_system_error('datastore_fault', Context)
    end.

-spec build_load_range_params(cb_context:context(), kz_term:ne_binary(), options()) ->
                                     load_params() | cb_context:context().
build_load_range_params(Context, View, Options) ->
    try build_general_load_params(Context, View, Options) of
        #{direction := Direction}=LoadMap ->
            TimeFilterKey = props:get_ne_binary_value('range_key_name', Options, <<"created">>),
            UserMapper = props:get_value('mapper', Options),

            HasQSFilter = crossbar_filter:is_defined(Context)
                andalso not crossbar_filter:is_only_time_filter(Context, TimeFilterKey),

            case time_range(Context, Options, TimeFilterKey) of
                {StartTime, EndTime} ->
                    {StartKey, EndKey} = ranged_start_end_keys(Context, Options, Direction, StartTime, EndTime),
                    Params = LoadMap#{end_time => EndTime
                                     ,has_qs_filter => HasQSFilter
                                     ,mapper => crossbar_filter:build_with_mapper(Context, UserMapper, HasQSFilter)
                                     ,start_key => StartKey
                                     ,start_time => StartTime
                                     ,view_options => build_view_query(Options, Direction, StartKey, EndKey, HasQSFilter)
                                     },
                    maybe_set_start_end_keys(Params, StartKey, EndKey);
                Ctx -> Ctx
            end;
        Ctx -> Ctx
    catch
        _E:_T ->
            lager:debug("exception occurred during building range view options for ~s", [View]),
            ST = erlang:get_stacktrace(),
            kz_util:log_stacktrace(ST),
            cb_context:add_system_error('datastore_fault', Context)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Generates corssbar_view options map for querying MODBs view.
%% @end
%%--------------------------------------------------------------------
-spec build_load_modb_params(cb_context:context(), kz_term:ne_binary(), options()) ->
                                    load_params() | cb_context:context().
build_load_modb_params(Context, View, Options) ->
    case build_load_range_params(Context, View, Options) of
        #{direction := Direction
         ,start_time := StartTime
         ,end_time := EndTime
         }=LoadMap ->
            LoadMap#{databases => get_range_modbs(Context, Options, Direction, StartTime, EndTime)};
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
-spec build_view_query(options(), direction(), api_range_key(), api_range_key(), boolean()) ->
                              kazoo_data:view_options().
build_view_query(Options, Direction, StartKey, EndKey, HasQSFilter) ->
    DeleteKeys = ['startkey', 'endkey'
                 ,'descending', 'limit'
                  | ?CB_SPECIFIC_VIEW_OPTIONS
                 ],
    DefaultOptions =
        props:filter_undefined(
          [{'startkey', StartKey}
          ,{'endkey', EndKey}
          ,Direction
           | props:delete_keys(DeleteKeys, Options)
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
%% Equivalent of `start_end_keys/3`, set direction
%% @end
%%--------------------------------------------------------------------
-spec start_end_keys(cb_context:context(), options()) -> range_keys().
start_end_keys(Context, Options) ->
    start_end_keys(Context, Options, direction(Context, Options)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find start/end keys based on direction.
%% If `start_key` or `end_key` is present in the request they will be
%% used used instead.
%%
%% See get_key_maps/2 to find out possible key map options.
%%
%% The keys will be swapped if direction is descending.
%% @end
%%--------------------------------------------------------------------
-spec start_end_keys(cb_context:context(), options(), direction()) -> range_keys().
start_end_keys(Context, Options, Direction) ->
    {OptsStartK, OptsEndK} = get_start_end_keys(Context, Options),

    case {cb_context:req_value(Context, <<"start_key">>)
         ,cb_context:req_value(Context, <<"end_key">>)
         }
    of
        {'undefined', 'undefined'} when Direction =:= 'ascending'  -> {OptsStartK, OptsEndK};
        {StartKeyReq, 'undefined'} when Direction =:= 'ascending'  -> {StartKeyReq, OptsEndK};
        {'undefined', EndKeyReq}   when Direction =:= 'ascending'  -> {OptsStartK, EndKeyReq};
        {StartKeyReq, EndKeyReq}   when Direction =:= 'ascending'  -> {StartKeyReq, EndKeyReq};
        {'undefined', 'undefined'} when Direction =:= 'descending' -> {OptsEndK, OptsStartK};
        {StartKeyReq, 'undefined'} when Direction =:= 'descending' -> {StartKeyReq, OptsStartK};
        {'undefined', EndKeyReq}   when Direction =:= 'descending' -> {OptsEndK, EndKeyReq};
        {StartKeyReq, EndKeyReq}   when Direction =:= 'descending' -> {StartKeyReq, EndKeyReq}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Equivalent of `ranged_start_end_keys/5`, find time ranges and direction
%% and pass them to `ranged_start_end_keys/5`.
%% @end
%%--------------------------------------------------------------------
-spec ranged_start_end_keys(cb_context:cb_context(), options()) -> range_keys().
ranged_start_end_keys(Context, Options) ->
    {StartTime, EndTime} = time_range(Context, Options),
    Direction = direction(Context, Options),
    ranged_start_end_keys(Context, Options, Direction, StartTime, EndTime).

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
-spec ranged_start_end_keys(cb_context:cb_context(), options(), direction(), kz_time:gregorian_seconds(), kz_time:gregorian_seconds()) -> range_keys().
ranged_start_end_keys(Context, Options, Direction, StartTime, EndTime) ->
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
-spec suffix_key_fun(range_keymap()) -> range_keymap_fun().
suffix_key_fun('nil') -> fun(_) -> 'undefined' end;
suffix_key_fun('undefined') -> fun kz_term:identity/1;
suffix_key_fun(['undefined']) -> fun kz_term:identity/1;
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
    time_range(Context, Options, props:get_ne_binary_value('range_key_name', Options, <<"created">>)).

-spec time_range(cb_context:context(), options(), kz_term:ne_binary()) -> time_range() | cb_context:context().
time_range(Context, Options, Key) ->
    MaxRange = get_max_range(Options),
    TSTime = kz_time:now_s(),
    RangeTo = get_time_key(Context, <<Key/binary, "_to">>, Options, TSTime),
    RangeFrom = get_time_key(Context, <<Key/binary, "_from">>, Options, RangeTo - MaxRange),
    time_range(Context, MaxRange, Key, RangeFrom, RangeTo).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Checks whether or not end time is prior to start time. Returns a ranged
%% tuple `{start_time, end_time}` or `context` with validation error.
%% @end
%%--------------------------------------------------------------------
-spec time_range(cb_context:context(), pos_integer(), kz_term:ne_binary(), pos_integer(), pos_integer()) ->
                        time_range() | cb_context:context().
time_range(Context, MaxRange, Key, RangeFrom, RangeTo) ->
    Path = <<Key/binary, "_from">>,
    case RangeTo - RangeFrom of
        N when N < 0 ->
            Msg = kz_term:to_binary(io_lib:format("~s_to ~b is prior to ~s ~b", [Key, RangeTo, Path, RangeFrom])),
            JObj = kz_json:from_list([{<<"message">>, Msg}, {<<"cause">>, RangeFrom}]),
            lager:debug("~s", [Msg]),
            cb_context:add_validation_error(Path, <<"date_range">>, JObj, Context);
        N when N > MaxRange ->
            Msg = kz_term:to_binary(io_lib:format("~s_to ~b is more than ~b seconds from ~s ~b", [Key, RangeTo, MaxRange, Path, RangeFrom])),
            JObj = kz_json:from_list([{<<"message">>, Msg}, {<<"cause">>, RangeTo}]),
            lager:debug("~s", [Msg]),
            cb_context:add_validation_error(Path, <<"date_range">>, JObj, Context);
        _ ->
            {RangeFrom, RangeTo}
    end.

-spec map_doc_fun() -> mapper_fun().
map_doc_fun() -> fun(JObj, Acc) -> [kz_json:get_value(<<"doc">>, JObj)|Acc] end.

-spec map_value_fun() -> mapper_fun().
map_value_fun() -> fun(JObj, Acc) -> [kz_json:get_value(<<"value">>, JObj)|Acc] end.

%%%===================================================================
%%% Load view internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load view results based on options. If the request is chunked
%% finish the chunk if it's started and set is_chunked or return
%% the cb_cowboy_payload() back to api_resource and api_util.
%% @end
%%--------------------------------------------------------------------
-spec load_view(load_params() | cb_context:context(), cb_context:context()) -> cb_context:context().
load_view(#{is_chunked := 'true'
           ,direction := Direction
           ,has_qs_filter := HasQSFilter
           }=LoadMap, Context) ->
    Setters = [{fun cb_context:set_doc/2, []}
              ,{fun cb_context:set_resp_status/2, 'success'}
              ,{fun cb_context:store/3, 'is_chunked', 'true'}
              ,{fun cb_context:store/3, 'next_chunk_fun', fun next_chunk/1}
              ,{fun cb_context:store/3, 'chunking_started', 'false'}
              ,{fun cb_context:store/3, 'view_direction', Direction}
              ,{fun cb_context:store/3, 'has_qs_filter', HasQSFilter}
              ,{fun cb_context:store/3, 'load_view_opts', LoadMap}
              ],
    cb_context:setters(Context, Setters);
load_view(#{direction := Direction
           ,has_qs_filter := HasQSFilter
           }=LoadMap, Context) ->
    Setters = [{fun cb_context:set_doc/2, []}
              ,{fun cb_context:set_resp_status/2, 'success'}
              ,{fun cb_context:store/3, 'view_direction', Direction}
              ,{fun cb_context:store/3, 'has_qs_filter', HasQSFilter}
              ],
    format_response(get_results(LoadMap#{context => cb_context:setters(Context, Setters)}));
load_view(ContextError, _) ->
    ContextError.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check page size is exhausted or shall we continue querying same
%% database (if query is chunk), otherwise go to next database.
%% It sets `total_queried` after figure out what to do.
%% @end
%%--------------------------------------------------------------------
-spec next_chunk(map()) -> map().
next_chunk(#{options := #{databases := []}
            ,previous_chunk_length := PrevLength
            ,total_queried := TotalQueried
            }=ChunkMap) ->
    lager:debug("(chunked) databases exhausted"),
    ChunkMap#{total_queried => TotalQueried + PrevLength
             ,chunking_finished => 'true'
             };
%% page_size is exhausted when query is limited by page_size
%% Condition: page_size = total_queried + current_db_results
%%            and the last key has been found.
next_chunk(#{options := #{page_size := PageSize}
            ,last_key := LastKey
            ,total_queried := TotalQueried
            ,previous_chunk_length := PrevLength
            }=ChunkMap)
  when is_integer(PageSize)
       andalso PageSize > 0
       andalso TotalQueried + PrevLength == PageSize
       andalso LastKey =/= 'undefined' ->
    lager:debug("(chunked) page size exhausted: ~b", [PageSize]),
    ChunkMap#{total_queried => TotalQueried + PrevLength
             ,chunking_finished => 'true'
             };
%% query next chunk from same db when query is chunked
%% Condition: the current last_key has been found and it's not equal to the previous last_key
next_chunk(#{options := #{last_key := OldLastKey}=LoadMap
            ,last_key := LastKey
            ,total_queried := TotalQueried
            ,previous_chunk_length := PrevLength
            ,context := Context
            }=ChunkMap)
  when OldLastKey =/= LastKey,
       LastKey =/= 'undefined' ->
    lager:debug("(chunked) db has more chunks to give, querying same db again"),
    chunk_map_roll_in(ChunkMap, get_results(LoadMap#{total_queried => TotalQueried + PrevLength
                                                    ,context => Context
                                                    ,last_key => LastKey
                                                    }));
%% just query next_db if there are any databases left
next_chunk(#{options := #{databases := [_]}
            ,previous_chunk_length := PrevLength
            ,total_queried := TotalQueried
            }=ChunkMap) ->
    lager:debug("(chunked) databases exhausted"),
    ChunkMap#{total_queried => TotalQueried + PrevLength
             ,chunking_finished => 'true'
             };
next_chunk(#{options := #{databases := [_|RestDbs], last_key := LastKey}=LoadMap
            ,total_queried := TotalQueried
            ,previous_chunk_length := PrevLength
            ,context := Context
            }=ChunkMap) ->
    lager:debug("(chunked) querying next db"),
    chunk_map_roll_in(ChunkMap, get_results(LoadMap#{total_queried => TotalQueried + PrevLength
                                                    ,databases => RestDbs
                                                    ,context => Context
                                                    ,last_key => LastKey
                                                    }));
%% starting chunked query
next_chunk(#{context := Context}=ChunkMap) ->
    LoadMap = cb_context:fetch(Context, 'load_view_opts'),
    lager:debug("(chunked) starting chunked query"),
    chunk_map_roll_in(ChunkMap
                     ,get_results(LoadMap#{context => cb_context:store(Context, 'load_view_opts', 'undefined')})
                     ).

-spec chunk_map_roll_in(map(), load_params()) -> map().
chunk_map_roll_in(#{last_key := OldLastKey}=ChunkMap
                 ,#{start_key := StartKey
                   ,last_key := LastKey
                   ,total_queried := TotalQueried
                   ,context := Context
                   }=LoadMap) ->
    ChunkMap#{start_key => StartKey
             ,last_key => LastKey
             ,total_queried => TotalQueried
             ,context => Context
             ,options => maps:remove(context, LoadMap#{last_key => OldLastKey}) %% to be checked in the next iteration
             }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Fold over databases and fetch result from each and count total result.
%% If pagination is requested keeps track of last key.
%% If `page_size` is not in the options, make unlimited get_results.
%%
%% Based on chunked, limited or unlimited query, get the correct
%% Limit for this loop (if it's limited query) and do the query.
%%
%% We use limit (limit + 1) to get an extra object (if available) to
%% get last object's key as the `next_start_key`. If the page size
%% has been satisfied and the last key has been found, return the result,
%% if the last key is not defined, query next DBs until DBs exhausted.
%%
%% If `chunked_size` is lower than sum of the `total_queried` and
%% `current_db_length`, we set the chunk_size as the limit. In this
%% case the db may return up to the limit size result, if the last_key
%% is defined it means the db has more results to give, so we query
%% the same db again, until the page size satisfied or no last_key is
%% defined. In that case if pages size is not exhausted yet we query
%% the next db.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_results(load_params()) -> load_params().
get_results(#{databases := []}=LoadMap) ->
    lager:debug("databases exhausted"),
    LoadMap;
get_results(#{databases := [Db|RestDbs]=Dbs
             ,view := View
             ,view_options := ViewOpts
             ,direction := Direction
             ,is_chunked := IsChunked
             ,chunk_size := ChunkSize
             ,total_queried := TotalQueried
             ,context := Context
             ,last_key := LastKey
             ,page_size := PageSize
             ,start_key := StartKey
             }=LoadMap) ->
    LimitWithLast = limit_with_last_key(IsChunked, PageSize, ChunkSize, TotalQueried),

    lager:debug("querying view '~s' from '~s', starting at '~p' with page size ~p and limit ~p in direction ~s"
               ,[View, Db, StartKey, PageSize, LimitWithLast, Direction]
               ),

    NextStartKey = case LastKey of
                       'undefined' -> props:get_value('startkey', ViewOpts);
                       _ -> LastKey
                   end,
    ViewOptions = props:filter_undefined(
                    [{'limit', LimitWithLast}
                    ,{'startkey', NextStartKey}
                     | props:delete('startkey', ViewOpts)
                    ]),

    lager:debug("kz_datamgr:get_results(~p, ~p, ~p)", [Db, View, ViewOptions]),
    case kz_datamgr:get_results(Db, View, ViewOptions) of
        {'error', 'not_found'} when [] =:= RestDbs ->
            lager:debug("either the db ~s or view ~s was not found", [Db, View]),
            LoadMap#{context => crossbar_util:response_missing_view(Context)};
        {'error', 'not_found'} ->
            lager:debug("either the db ~s or view ~s was not found, querying next db...", [Db, View]),
            get_results(LoadMap#{databases => RestDbs});
        {'error', Error} ->
            lager:debug("failed to query view ~s from db ~s: ~p", [View, Db, Error]),
            LoadMap#{context => crossbar_doc:handle_datamgr_errors(Error, View, Context)};
        {'ok', JObjs} ->
            %% catching crashes when applying users map functions (filter map)
            %% so we can handle errors when request is chunked and chunk is already started
            try handle_query_result(LoadMap, Dbs, JObjs, LimitWithLast)
            catch
                _E:_T ->
                    lager:debug("exception occurred during querying db ~s for view ~s", [Db, View]),
                    ST = erlang:get_stacktrace(),
                    kz_util:log_stacktrace(ST),
                    LoadMap#{context => cb_context:add_system_error('datastore_fault', Context)}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Apply filter to result, find last key
%% Then based on page_size, limit, result length and last key see
%% we're done or shall we continue.
%% @end
%%--------------------------------------------------------------------
-spec handle_query_result(load_params(), kz_term:ne_binaries(), kz_json:objects(), kz_term:api_pos_integer()) ->
                                 load_params().
handle_query_result(#{last_key := LastKey
                     ,mapper := Mapper
                     ,is_chunked := IsChunked
                     ,context := Context
                     }=LoadMap, [Db|RestDbs]=Dbs, Results, Limit) ->
    ResultsLength = erlang:length(Results),

    {NewLastKey, JObjs} = last_key(LastKey, Results, Limit, ResultsLength),

    FilteredJObjs = apply_filter(Mapper, JObjs),

    Filtered = length(FilteredJObjs),

    lager:debug("db_returned: ~b passed_filter: ~p next_start_key: ~p", [ResultsLength, Filtered, NewLastKey]),

    case not IsChunked
        andalso check_page_size_and_length(LoadMap, Filtered, FilteredJObjs, NewLastKey)
    of
        'false' ->
            LoadMap#{last_key => NewLastKey
                    ,context => cb_context:setters(Context
                                                  ,[{fun cb_context:set_resp_data/2, FilteredJObjs}
                                                   ,{fun cb_context:set_account_db/2, Db}
                                                   ]
                                                  )
                    };
        {'exhausted', LoadMap2} -> LoadMap2;
        {'next_db', LoadMap2} -> get_results(LoadMap2#{databases => RestDbs})
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check page size is exhausted or not.
%% @end
%%--------------------------------------------------------------------
-spec check_page_size_and_length(load_params(), non_neg_integer(), kz_json:objects(), last_key()) ->
                                        {'exhausted' | 'next_db', load_params()}.
%% page_size is exhausted when query is limited by page_size
%% Condition: page_size = total_queried + current_db_results
%%            and the last key has been found.
check_page_size_and_length(#{page_size := PageSize
                            ,queried_jobjs := QueriedJObjs
                            ,total_queried := TotalQueried
                            }=LoadMap, Length, FilteredJObjs, LastKey)
  when is_integer(PageSize)
       andalso PageSize > 0
       andalso TotalQueried + Length == PageSize
       andalso LastKey =/= 'undefined' ->
    lager:debug("page size exhausted: ~b", [PageSize]),
    {'exhausted', LoadMap#{total_queried => TotalQueried + Length
                          ,queried_jobjs => QueriedJObjs ++ FilteredJObjs
                          ,last_key => LastKey
                          }
    };
%% just query next_db
check_page_size_and_length(#{total_queried := TotalQueried
                            ,queried_jobjs := QueriedJObjs
                            }=LoadMap, Length, FilteredJObjs, LastKey) ->
    {'next_db', LoadMap#{total_queried => TotalQueried + Length
                        ,queried_jobjs => QueriedJObjs ++ FilteredJObjs
                        ,last_key => LastKey
                        }
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Find out db request limit to use based on chunk size and remaining
%% amount to satisfy page_size
%% @end
%%--------------------------------------------------------------------
-spec limit_with_last_key(boolean(), kz_term:api_pos_integer(), pos_integer(), non_neg_integer()) ->
                                 kz_term:api_pos_integer().
%% non-chunked unlimited request => no limit
limit_with_last_key('false', 'undefined', _, _) ->
    'undefined';
%% non-chunked limited request
limit_with_last_key('false', PageSize, _, TotalQueried) ->
    1 + PageSize - TotalQueried;
%% chunked unlimited request
limit_with_last_key('true', 'undefined', ChunkSize, _) ->
    1 + ChunkSize;
%% same chunk_size and page_size
limit_with_last_key('true', PageSize, PageSize, TotalQueried) ->
    1 + PageSize - TotalQueried;
%% chunk_size is lower than sum of remaining amount to query and page_size, forcing chunk_size
limit_with_last_key('true', PageSize, ChunkSize, TotalQueried) when ChunkSize < (PageSize - TotalQueried) ->
    1 + ChunkSize;
%% chunk_size is bigger than page_size, using page_size
limit_with_last_key('true', PageSize, _ChunkSize, TotalQueried) ->
    1 + PageSize - TotalQueried.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Apply filter function if provided while keep maintaining the result
%% order.
%% Filter function can be arity 1 (operating on list of JObjs) and
%% arity 2 (classical Erlang foldl function, e.g. (JObj, Acc)).
%%
%% For arity 1, the passed JObjs is in the reverse order, reverse
%% you're response at the end.
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
                 ,last_key := NextStartKey
                 ,start_key := StartKey
                 }) ->
    Envelope = add_paging(StartKey, TotalQueried, NextStartKey, cb_context:resp_envelope(Context)),
    crossbar_doc:handle_datamgr_success(JObjs, cb_context:set_resp_envelope(Context, Envelope)).

%% @private
-spec add_paging(api_range_key(), non_neg_integer(), api_range_key(), kz_json:object()) -> kz_json:object().
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
%% @public
%% @doc
%% Generates general corssbar_view options map for querying view.
%% @end
%%--------------------------------------------------------------------
-spec build_general_load_params(cb_context:context(), kz_term:ne_binary(), options()) -> load_params() | cb_context:context().
build_general_load_params(Context, View, Options) ->
    Direction = direction(Context, Options),
    try maps:from_list(
          [{'chunk_size', get_chunk_size(Context, Options)}
          ,{'databases', props:get_value('databases', Options, [cb_context:account_db(Context)])}
          ,{'direction', Direction}
          ,{'is_chunked', is_chunked(Context, Options)}
          ,{'last_key', 'undefined'}
          ,{'page_size', get_page_size(Context, Options)}
          ,{'queried_jobjs', []}
          ,{'should_paginate', cb_context:should_paginate(Context)}
          ,{'total_queried', 0}
          ,{'view', View}
          ])
    catch
        throw:{'error', ErrorMsg} ->
            cb_context:add_system_error(404, 'faulty_request', ErrorMsg, Context)
    end.

-spec is_chunked(cb_context:context(), options()) -> boolean().
is_chunked(Context, Options) ->
    kz_json:is_true(<<"is_chunked">>
                   ,cb_context:query_string(Context)
                   ,props:get_is_true('is_chunked', Options, 'false')
                   )
        andalso not props:get_is_true('unchunkable', Options, 'false').

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create ranged view lookup database list using start/end time and
%% direction.
%% @end
%%--------------------------------------------------------------------
-spec get_range_modbs(cb_context:context(), options(), direction(), kz_time:gregorian_seconds(), kz_time:gregorian_seconds()) ->
                             kz_term:ne_binaries().
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

-spec get_chunk_size(cb_context:context(), options()) -> kz_term:api_pos_integer().
get_chunk_size(Context, Options) ->
    SystemSize = kapps_config:get_pos_integer(?CONFIG_CAT, <<"load_view_chunk_size">>, 50),
    OptionsSize = props:get_integer_value('chunk_size', Options, SystemSize),

    case kz_json:get_value(<<"chunk_size">>, cb_context:query_string(Context)) of
        'undefined' -> OptionsSize;
        Size ->
            try kz_term:to_integer(Size) of
                ChunkSize when ChunkSize > 0,
                               ChunkSize =< SystemSize ->
                    ChunkSize;
                ChunkSize when ChunkSize < 0 ->
                    throw({'error', <<"chunk size must be at least 1">>});
                ChunkSize when ChunkSize > SystemSize ->
                    throw({'error', <<"chunk size must be lower than ", (integer_to_binary(SystemSize))/binary>>})
            catch
                _:_ ->
                    throw({'error', <<"invalid chunk size">>})
            end
    end.

-spec maybe_set_start_end_keys(load_params(), api_range_key(), api_range_key()) -> load_params().
maybe_set_start_end_keys(LoadMap, 'undefined', 'undefined') -> LoadMap;
maybe_set_start_end_keys(LoadMap, StartKey, 'undefined') -> LoadMap#{start_key => StartKey};
maybe_set_start_end_keys(LoadMap, 'undefined', EndKey) -> LoadMap#{end_key => EndKey};
maybe_set_start_end_keys(LoadMap, StartKey, EndKey) -> LoadMap#{start_key => StartKey, end_key => EndKey}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If pagination available, returns page size.
%%
%% Note: DO NOT ADD ONE (1) TO PAGE_SIZE/LIMIT! Load function will add it.
%% @end
%%--------------------------------------------------------------------
-spec get_page_size(cb_context:context(), options()) -> kz_term:api_pos_integer().
get_page_size(Context, Options) ->
    case cb_context:should_paginate(Context) of
        'true' ->
            case props:get_value('limit', Options) of
                'undefined' ->
                    get_page_size_from_request(Context);
                Limit ->
                    lager:debug("got limit from options: ~b", [Limit]),
                    Limit
            end;
        'false' ->
            lager:debug("pagination disabled in context"),
            'undefined'
    end.

-spec get_page_size_from_request(cb_context:context()) -> pos_integer().
get_page_size_from_request(Context) ->
    case cb_context:req_value(Context, <<"page_size">>) of
        'undefined' -> cb_context:pagination_page_size();
        Size ->
            try kz_term:to_integer(Size) of
                PageSize when PageSize > 0 -> PageSize;
                _ ->
                    throw({'error', <<"page size must be at least 1">>})
            catch
                _:_ ->
                    throw({'error', <<"invalid page size">>})
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get time key value from options or request.
%% @end
%%--------------------------------------------------------------------
-spec get_time_key(cb_context:context(), kz_term:ne_binary(), options(), pos_integer()) -> pos_integer().
get_time_key(Context, Key, Options, Default) ->
    case props:get_integer_value(Key, Options) of
        'undefined' ->
            case kz_term:safe_cast(cb_context:req_value(Context, Key), Default, fun kz_term:to_integer/1) of
                T when T > 0 -> T;
                _ -> Default
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
%% Build customized start/end key mapper
%% @end
%%--------------------------------------------------------------------
-spec get_start_end_keys(cb_context:context(), options()) -> {api_range_key(), api_range_key()}.
get_start_end_keys(Context, Options) ->
    case props:get_value('keymap', Options) of
        'undefined' ->
            {map_keymap(Context, Options, props:get_first_defined(['startkey', 'start_keymap'], Options))
            ,map_keymap(Context, Options, props:get_first_defined(['endkey', 'end_keymap'], Options))
            };
        KeyMap ->
            {map_keymap(Context, Options, KeyMap)
            ,map_keymap(Context, Options, KeyMap)
            }
    end.

-spec map_keymap(cb_context:context(), options(), keymap()) -> api_range_key().
map_keymap(Context, _, Fun) when is_function(Fun, 1) -> Fun(Context) ;
map_keymap(Context, Options, Fun) when is_function(Fun, 2) -> Fun(Options, Context);
map_keymap(_, _, ApiRangeKey) -> ApiRangeKey.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Build customized start/end key mapper for ranged query.
%% If a map option is not present in the options, the timestamp
%% will be used as the key.
%%
%% Possible key maps options:
%%   - `range_keymap`: use this to map both start/end keys
%%   - `range_start_keymap`: maps start key only
%%   - `range_end_keymap`: maps end key only
%%
%% Possible maps type:
%%   - a binary value: to construct keys like [<<"account">>, Timestamp]
%%   - an integer value: to construct keys like [1234, Timestamp]
%%   - a list: to construct keys like [<<"en">>, <<"us">>, Timestamp]
%%   - a function/1: To customize your own key using a function
%% @end
%%--------------------------------------------------------------------
-spec get_range_key_maps(options()) -> {range_keymap_fun(), range_keymap_fun()}.
get_range_key_maps(Options) ->
    case props:get_value('range_keymap', Options) of
        'undefined' ->
            {map_range_keymap(props:get_value('range_start_keymap', Options))
            ,map_range_keymap(props:get_value('range_end_keymap', Options))
            };
        KeyMap -> {map_range_keymap(KeyMap), map_range_keymap(KeyMap)}
    end.

-spec map_range_keymap(range_keymap()) -> range_keymap_fun().
map_range_keymap('nil') -> fun(_) -> 'undefined' end;
map_range_keymap('undefined') -> fun kz_term:identity/1;
map_range_keymap(['undefined']) -> fun(Ts) -> [Ts] end;
map_range_keymap(K) when is_binary(K) -> fun(Ts) -> [K, Ts] end;
map_range_keymap(K) when is_integer(K) -> fun(Ts) -> [K, Ts] end;
map_range_keymap(K) when is_list(K) -> fun(Ts) -> K ++ [Ts] end;
map_range_keymap(K) when is_function(K, 1) -> K.
