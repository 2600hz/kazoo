%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_compaction_reporter).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([start_tracking_job/3
        ,start_tracking_job/4
        ,stop_tracking_job/1
        ,set_job_dbs/2
        ,current_db/2
        ,skipped_db/2
        ,finished_db/3
        ,add_found_shards/2
        ,finished_shard/2
        ]).
%% "Mirrors" for SUP commands
-export([status/0, history/0, history/2, job_info/1]).

%% gen_server's callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,code_change/3
        ,terminate/2
        ]).

-define(SERVER, ?MODULE).
-define(COMPACTION_VIEW, <<"compaction_jobs/crossbar_listing">>).

-type call_id() :: kz_term:ne_binary().
-type compaction_stats() :: #{%% Databases
                              'id' => kz_term:ne_binary()
                             ,'found_dbs' => pos_integer() %% Number of dbs found to be compacted
                             ,'compacted_dbs' => non_neg_integer() %% Number of dbs compacted so far
                             ,'queued_dbs' => non_neg_integer() %% remaining dbs to be compacted
                             ,'skipped_dbs' => non_neg_integer() %% dbs skipped because not data_size nor disk-data's ratio thresholds are met.
                             ,'current_db' => kz_term:api_ne_binary()
                             ,'processed_dbs' => kz_term:ne_binaries() %% `Encoded' DBs already processed, avoids processing duplicated events like skipped, finished, etc.
                              %% Shards
                             ,'found_shards' => non_neg_integer() %% Number of shards found so far
                             ,'compacted_shards' => non_neg_integer() %% Number of shards compacted so far
                              %% Storage
                             ,'disk_start' => non_neg_integer() %% disk_size sum of all dbs in bytes before compaction (for history command)
                             ,'disk_end' => non_neg_integer() %% disk_size sum of all dbs in bytes after compaction (for history command)
                             ,'data_start' => non_neg_integer() %% data_size sum of all dbs in bytes before compaction (for history command)
                             ,'data_end' => non_neg_integer() %% data_size sum of all dbs in bytes after compaction (for history command)
                             ,'recovered_disk' => non_neg_integer() %% bytes recovered so far (for status command)
                              %% Worker
                             ,'pid' => pid() %% worker's pid
                             ,'node' => node() %% node where the worker is running
                             ,'started' => kz_time:gregorian_seconds() %% datetime (in seconds) when the compaction started
                             ,'finished' => 'undefined' | kz_time:gregorian_seconds() %% datetime (in seconds) when the compaction ended
                             }.
-type state() :: #{call_id() => compaction_stats()}.


%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    case gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []) of
        {'error', {'already_started', Pid}} ->
            'true' = link(Pid),
            {'ok', Pid};
        Other -> Other
    end.

%%------------------------------------------------------------------------------
%% @doc Start tracking a compaction job
%% @end
%%------------------------------------------------------------------------------
-spec start_tracking_job(pid(), node(), call_id()) -> 'ok'.
start_tracking_job(Pid, Node, CallId) ->
    start_tracking_job(Pid, Node, CallId, []).

%%------------------------------------------------------------------------------
%% @doc Start tracking a compaction job
%% @end
%%------------------------------------------------------------------------------
-spec start_tracking_job(pid(), node(), call_id(), [kt_compactor:db_and_sizes()]) -> 'ok'.
start_tracking_job(Pid, Node, CallId, DbsAndSizes) ->
    gen_server:cast(?SERVER, {'new_job', Pid, Node, CallId, DbsAndSizes}).

%%------------------------------------------------------------------------------
%% @doc Stop tracking compaction job, save current state on db.
%% @end
%%------------------------------------------------------------------------------
-spec stop_tracking_job(call_id()) -> 'ok'.
stop_tracking_job(CallId) ->
    gen_server:cast(?SERVER, {'stop_job', CallId}).

%%------------------------------------------------------------------------------
%% @doc Some jobs like `compact_all' and `compact_node' doesn't know the list of dbs to
%% be compacted at the beginning of the job, so we wait for that job to report the dbs
%% once it has the list of dbs to be compacted prior to start compacting them.
%% @end
%%------------------------------------------------------------------------------
-spec set_job_dbs(call_id(), kt_compactor:dbs_and_sizes()) -> 'ok'.
set_job_dbs(CallId, DbsAndSizes) ->
    gen_server:cast(?SERVER, {'set_job_dbs', CallId, DbsAndSizes}).

%%------------------------------------------------------------------------------
%% @doc Set current db being compacted for the given job id.
%% @end
%%------------------------------------------------------------------------------
-spec current_db(call_id(), kz_term:ne_binary()) -> 'ok'.
current_db(CallId, Db) ->
    gen_server:cast(?SERVER, {'current_db', CallId, normalize_db(Db)}).

%%------------------------------------------------------------------------------
%% @doc Notifies when a database has been skipped by the compactor worker. This happens
%% when not data_size nor disk-data's ratio thresholds are met.
%% @end
%%------------------------------------------------------------------------------
-spec skipped_db(call_id(), kz_term:ne_binary()) -> 'ok'.
skipped_db(CallId, Db) when is_binary(Db) ->
    gen_server:cast(?SERVER, {'skipped_db', CallId, normalize_db(Db)}).

%%------------------------------------------------------------------------------
%% @doc Set db already compacted for the given job id.
%% @end
%%------------------------------------------------------------------------------
-spec finished_db(call_id(), kz_term:ne_binary(), kt_compactor:rows()) -> 'ok'.
finished_db(CallId, Db, Rows) ->
    gen_server:cast(?SERVER, {'finished_db', CallId, normalize_db(Db), Rows}).

%%------------------------------------------------------------------------------
%% @doc Increases `found_shards' value by adding `ShardsCount' to it for the given job id.
%% @end
%%------------------------------------------------------------------------------
-spec add_found_shards(call_id(), non_neg_integer()) -> 'ok'.
add_found_shards(CallId, ShardsCount) when is_number(ShardsCount) ->
    gen_server:cast(?SERVER, {'add_found_shards', CallId, ShardsCount}).

%%------------------------------------------------------------------------------
%% @doc Increases the counter of `compacted_shards' for the given job id.
%% @end
%%------------------------------------------------------------------------------
-spec finished_shard(call_id(), kz_term:ne_binary()) -> 'ok'.
finished_shard(CallId, Shard) ->
    gen_server:cast(?SERVER, {'finished_shard', CallId, Shard}).

%%------------------------------------------------------------------------------
%% @doc Return the status for every compaction job currently running.
%% @end
%%------------------------------------------------------------------------------
-spec status() -> [kz_term:proplist()].
status() ->
    %% Result is a list of proplists or an empty list.
    gen_server:call(?SERVER, 'status').

%%------------------------------------------------------------------------------
%% @doc Returns history for the current Year and Month.
%% @end
%%------------------------------------------------------------------------------
-spec history() -> {'ok', kz_json:json_terms()} | {'error', atom()}.
history() ->
    {Year, Month, _} = erlang:date(),
    history(Year, Month).

%%------------------------------------------------------------------------------
%% @doc Return compaction history for the given year and month (YYYY, MM).
%% @end
%%------------------------------------------------------------------------------
-spec history(kz_time:year(), kz_time:month()) -> {'ok', kz_json:json_terms()} |
                                                  {'error', atom()}.
history(Year, Month) when is_integer(Year)
                          andalso is_integer(Month) ->
    {'ok', AccountId} = kapps_util:get_master_account_id(),
    Opts = [{'year', Year}, {'month', Month}, 'include_docs'],
    kazoo_modb:get_results(AccountId, ?COMPACTION_VIEW, Opts).

%%------------------------------------------------------------------------------
%% @doc Return the information for the given job id
%% @end
%%------------------------------------------------------------------------------
-spec job_info(kz_term:ne_binary()) -> kz_term:proplist() | atom().
job_info(<<JobId/binary>>) ->
    {'ok', AccountId} = kapps_util:get_master_account_id(),
    case kazoo_modb:open_doc(AccountId, JobId) of
        {'ok', JObj} ->
            Int = fun(Key) -> kz_json:get_integer_value(Key, JObj) end,
            Str = fun(Key) -> kz_json:get_string_value(Key, JObj) end,
            DiskStart = Int([<<"storage">>, <<"disk">>, <<"start">>]),
            DiskEnd = Int([<<"storage">>, <<"disk">>, <<"end">>]),
            Start = Int([<<"worker">>, <<"started">>]),
            End = Int([<<"worker">>, <<"finished">>]),
            [{<<"id">>, kz_doc:id(JObj)}
            ,{<<"found_dbs">>, Str([<<"databases">>, <<"found">>])}
            ,{<<"compacted_dbs">>, Str([<<"databases">>, <<"compacted">>])}
            ,{<<"skipped_dbs">>, Str([<<"databases">>, <<"skipped">>])}
            ,{<<"found_shards">>, Str([<<"shards">>, <<"found">>])}
            ,{<<"compacted_shards">>, Str([<<"shards">>, <<"compacted">>])}
            ,{<<"disk_start">>, kz_term:to_binary(DiskStart)}
            ,{<<"disk_end">>, kz_term:to_binary(DiskEnd)}
            ,{<<"data_start">>, Str([<<"storage">>, <<"data">>, <<"start">>])}
            ,{<<"data_end">>, Str([<<"storage">>, <<"data">>, <<"end">>])}
            ,{<<"recovered_disk">>, kz_util:pretty_print_bytes(DiskStart - DiskEnd)}
            ,{<<"node">>, Str([<<"worker">>, <<"node">>])}
            ,{<<"pid">>, Str([<<"worker">>, <<"pid">>])}
            ,{<<"started">>, kz_term:to_list(kz_time:pretty_print_datetime(Start))}
            ,{<<"finished">>, kz_term:to_list(kz_time:pretty_print_datetime(End))}
            ,{<<"exec_time">>
             ,kz_term:to_list(kz_time:pretty_print_elapsed_s(End - Start))
             }
            ];
        {'error', Reason} ->
            Reason
    end.

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    {'ok', #{}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call('status', _From, State) ->
    Ret = maps:fold(fun stats_to_status_fold/3, [], State),
    {'reply', Ret, State};

handle_call(_Request, _From, State) ->
    lager:debug("unhandled call ~p from ~p", [_Request, _From]),
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'new_job', Pid, Node, CallId, DbsAndSizes}, State) ->
    TotalDbs = length(DbsAndSizes),
    Stats = #{'id' => CallId
             ,'found_dbs' => TotalDbs
             ,'compacted_dbs' => 0
             ,'queued_dbs' => TotalDbs
             ,'skipped_dbs' => 0
             ,'current_db' => 'undefined'
             ,'processed_dbs' => []
             ,'found_shards' => 0
             ,'compacted_shards' => 0
             ,'disk_start' => 0
             ,'disk_end' => 0
             ,'data_start' => 0
             ,'data_end' => 0
             ,'recovered_disk' => 0
             ,'pid' => Pid
             ,'node' => Node
             ,'started' => kz_time:now_s()
             ,'finished' => 'undefined'
             },
    {'noreply', State#{CallId => Stats}};

handle_cast({'stop_job', CallId}, State) ->
    NewState =
        case maps:take(CallId, State) of
            'error' ->
                lager:debug("invalid id provided for stopping job tracking: ~p", [CallId]),
                State;
            {Stats = #{'started' := Started}, State1} ->
                Finished = kz_time:now_s(),
                Elapsed = Finished - Started,
                lager:debug("~s finished, took ~s (~ps)"
                           ,[CallId, kz_time:pretty_print_elapsed_s(Elapsed), Elapsed]
                           ),
                'ok' = save_compaction_stats(Stats#{'finished' => Finished}),
                State1
        end,
    {'noreply', NewState};

handle_cast({'set_job_dbs', CallId, DbsAndSizes}, State) ->
    NewState =
        case maps:get(CallId, State, 'undefined') of
            'undefined' ->
                State;
            Stats ->
                TotalDbs = length(DbsAndSizes),
                State#{CallId => Stats#{'found_dbs' => TotalDbs
                                       ,'queued_dbs' => TotalDbs
                                       }}
        end,
    {'noreply', NewState};

handle_cast({'current_db', CallId, Db}, State) ->
    NewState =
        case maps:get(CallId, State, 'undefined') of
            'undefined' -> State;
            Stats -> State#{CallId => Stats#{'current_db' => Db}}
        end,
    {'noreply', NewState};

handle_cast({'skipped_db', CallId, Db}, State) ->
    Stats = maps:get(CallId, State, 'undefined'),
    NewState =
        case Stats =/= 'undefined'
            andalso not lists:member(Db, maps:get('processed_dbs', Stats))
        of
            'false' ->
                State;
            'true' ->
                lager:debug("~p db does not need compaction, skipped", [Db]),
                State#{CallId => Stats#{'skipped_dbs' => maps:get('skipped_dbs', Stats) + 1}}
        end,
    {'noreply', NewState};

handle_cast({'finished_db', CallId, Db, [FRow | _]}, State) ->
    Stats = maps:get(CallId, State, 'undefined'),
    NewState =
        case Stats =/= 'undefined'
            andalso not lists:member(Db, maps:get('processed_dbs', Stats))
        of
            'false' ->
                State;
            'true' ->
                #{'recovered_disk' := CurrentRec
                 ,'disk_start' := DiskStart
                 ,'disk_end' := DiskEnd
                 ,'data_start' := DataStart
                 ,'data_end' := DataEnd
                 ,'found_dbs' := Found
                 ,'skipped_dbs' := Skipped
                 ,'queued_dbs' := Queued
                 ,'processed_dbs' := ProcessedDBs
                 } = Stats,
                [_, _, OldDisk, OldData, NewDisk, NewData] = FRow,
                Recovered = (OldDisk-NewDisk),
                NewQueued = Queued - 1,
                lager:debug("recovered ~p bytes after compacting ~p db", [Recovered, Db]),
                State#{CallId => Stats#{'recovered_disk' => CurrentRec + Recovered
                                       ,'disk_start' => DiskStart + OldDisk
                                       ,'disk_end' => DiskEnd + NewDisk
                                       ,'data_start' => DataStart + OldData
                                       ,'data_end' => DataEnd + NewData
                                       ,'compacted_dbs' => Found - NewQueued - Skipped
                                       ,'queued_dbs' => NewQueued
                                       ,'current_db' => 'undefined'
                                       ,'processed_dbs' => [Db | ProcessedDBs]
                                       }}
        end,
    {'noreply', NewState};

handle_cast({'add_found_shards', CallId, ShardsCount}, State) ->
    NewState =
        case maps:get(CallId, State, 'undefined') of
            'undefined' ->
                State;
            Stats ->
                lager:debug("adding ~p to the number of found shards", [ShardsCount]),
                CurrentShards = maps:get('found_shards', Stats),
                State#{CallId => Stats#{'found_shards' => CurrentShards + ShardsCount}}
        end,
    {'noreply', NewState};

handle_cast({'finished_shard', CallId, _Shard}, State) ->
    NewState =
        case maps:get(CallId, State, 'undefined') of
            'undefined' ->
                State;
            Stats ->
                Compacted = maps:get('compacted_shards', Stats),
                State#{CallId => Stats#{'compacted_shards' => Compacted + 1}}
        end,
    {'noreply', NewState};

handle_cast(_Msg, State) ->
    lager:debug("unhandled cast ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Info, State) ->
    lager:debug("unhandled message ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("~s terminating with reason: ~p~n when state was: ~p"
               ,[?MODULE, _Reason, _State]
               ).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Converts current state into a list of proplists including only some `Keys'.
%% @end
%%------------------------------------------------------------------------------
-spec stats_to_status_fold(kz_term:ne_binary(), compaction_stats(), [kz_term:proplist()]) ->
                                  [kz_term:proplist()].
stats_to_status_fold(_CallId, Stats = #{'queued_dbs' := QueuedDBs}, Acc) ->
    Keys = ['id', 'found_dbs', 'compacted_dbs', 'queued_dbs', 'skipped_dbs', 'current_db',
            'found_shards', 'compacted_shards', 'recovered_disk', 'pid', 'node', 'started'],
    ToBin = fun(Something) -> kz_term:to_binary(Something) end,
    StatsProp = [{ToBin(Key), ToBin(maps:get(Key, Stats))} || Key <- Keys],
    case QueuedDBs =:= 0 of
        'true' ->
            %% This happens when it finishes compacting on the first node so now
            %% `found_dbs = compacted_dbs + skipped_dbs' which means `queued_dbs = 0' and
            %% also it means it is still compacting on other nodes otherwise this status
            %% were not being build.
            MsgProp = [{<<"msg">>, <<"Still compacting on other nodes">>}],
            [StatsProp ++ MsgProp | Acc];
        'false' ->
            [StatsProp | Acc]
    end.

%%------------------------------------------------------------------------------
%% @doc Save compaction job stats on db.
%% @end
%%------------------------------------------------------------------------------
-spec save_compaction_stats(compaction_stats()) -> 'ok'.
save_compaction_stats(#{'id' := Id
                       ,'found_dbs' := FoundDBs
                       ,'compacted_dbs' := CompactedDBs
                       ,'queued_dbs' := QueuedDBs
                       ,'skipped_dbs' := SkippedDBs
                       ,'found_shards' := FoundShards
                       ,'compacted_shards' := CompactedShards
                       ,'disk_start' := DiskStart
                       ,'disk_end' := DiskEnd
                       ,'data_start' := DataStart
                       ,'data_end' := DataEnd
                       ,'pid' := Pid
                       ,'node' := Node
                       ,'started' := Started
                       ,'finished' := Finished
                       } = Stats) ->
    Map = #{<<"_id">> => Id
           ,<<"databases">> => #{<<"found">> => FoundDBs
                                ,<<"compacted">> => CompactedDBs
                                ,<<"queued">> => QueuedDBs
                                ,<<"skipped">> => SkippedDBs
                                }
           ,<<"shards">> => #{<<"found">> => FoundShards
                             ,<<"compacted">> => CompactedShards
                             }
           ,<<"storage">> => #{<<"disk">> =>
                                   #{<<"start">> => DiskStart
                                    ,<<"end">> => DiskEnd
                                    }
                              ,<<"data">> =>
                                   #{<<"start">> => DataStart
                                    ,<<"end">> => DataEnd
                                    }
                              }
           ,<<"worker">> => #{<<"pid">> => kz_term:to_binary(Pid)
                             ,<<"node">> => kz_term:to_binary(Node)
                             ,<<"started">> => Started
                             ,<<"finished">> => Finished
                             }
           ,<<"pvt_type">> => <<"compaction_job">>
           ,<<"pvt_created">> => kz_time:now_s()
           },
    lager:debug("saving stats after compaction job completion: ~p", [Stats]),
    {'ok', AccountId} = kapps_util:get_master_account_id(),
    {'ok', Doc} = kazoo_modb:save_doc(AccountId, kz_json:from_map(Map)),
    lager:debug("created doc after compaction job completion: ~p", [Doc]),
    'ok'.

-spec normalize_db(kz_term:ne_binary()) -> kz_term:ne_binary().
normalize_db(Db) ->
    kz_http_util:urldecode(Db).
