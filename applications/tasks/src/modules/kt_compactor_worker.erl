%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2017, 2600Hz
%%% @doc
%%% Runs the compaction job which consists of:
%%%   1. The node to run it on (and API/Admin connections directly to the node
%%%   2. The database to compact
%%%   3. The heuristic to use
%%%      a. none - force compaction
%%%      b. ratio - check disk/data usage and compact if over a threshold
%%%
%%% The list of shards on the node and the design documents in the db will be generated
%%% and compaction will begin on those shards.
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kt_compactor_worker).

-export([run_compactor/1]).

-export([compact_shard/1
        ,get_db_disk_and_data/2, get_db_disk_and_data/3
        ]).

-export([compactor_node/1
        ,compactor_conn/1
        ,compactor_admin/1
        ,compactor_database/1
        ,compactor_shards/1
        ,compactor_design_docs/1
        ,compactor_heuristic/1
        ,new/5
        ]).

-include("tasks.hrl").
-include("src/modules/kt_compactor.hrl").

-record(compactor, {node :: ne_binary()
                   ,conn :: kz_data:connection()
                   ,admin :: kz_data:connection()
                   ,database :: ne_binary()
                   ,shards :: ne_binaries()
                   ,design_docs = [] :: ne_binaries()
                   ,heuristic :: heuristic()
                   }).

-type compactor() :: #compactor{}.
-export_type([compactor/0]).

-spec run_compactor(compactor()) -> 'ok'.
run_compactor(Compactor) ->
    case should_compact(Compactor) of
        'false' -> 'ok';
        'true' ->
            lager:info("compacting db ~s on node ~s"
                      ,[compactor_database(Compactor), compactor_node(Compactor)]
                      ),
            run_compactor_job(Compactor, compactor_shards(Compactor))
    end.

-spec run_compactor_job(compactor(), ne_binaries()) -> 'ok'.
run_compactor_job(_Compactor, []) -> 'ok';
run_compactor_job(Compactor, Shards) ->
    try lists:split(?MAX_COMPACTING_SHARDS, Shards) of
        {Compact, ShardsLeft} ->
            lager:debug("  compacting ~b shards", [?MAX_COMPACTING_SHARDS]),
            ShardsPidRef = compact_shards(Compactor#compactor{shards=Compact}),
            wait_for_shards(ShardsPidRef),
            run_compactor_job(Compactor#compactor{shards=ShardsLeft}, ShardsLeft)
    catch
        'error':'badarg' ->
            lager:debug("compacting last ~b shards", [length(Shards)]),
            ShardsPidRef = compact_shards(Compactor),
            wait_for_shards(ShardsPidRef)
    end.

-spec wait_for_shards(pid_refs()) -> 'ok'.
-spec wait_for_shards(pid_refs(), pid_ref()) -> 'ok'.
wait_for_shards([]) -> 'ok';
wait_for_shards(PidRefs) ->
    MaxWait = ?MAX_WAIT_FOR_COMPACTION_PIDS,
    receive
        {'DOWN', Ref, 'process', Pid, _Reason} ->
            wait_for_shards(PidRefs, {Pid, Ref})
    after MaxWait ->
            lager:error("timed out waiting for shards, moving on"),
            lager:debug("shards still processing: ~p", [PidRefs])
    end.

wait_for_shards(PidRefs, {Pid, Ref}) ->
    case props:get_value(Pid, PidRefs) of
        Ref -> wait_for_shards(props:delete(Pid, PidRefs));
        _Ref -> wait_for_shards(PidRefs)
    end.

-spec compact_shards(compactor()) -> pid_refs().
compact_shards(Compactor) ->
    [spawn_monitor(?MODULE, 'compact_shard', [Compactor#compactor{shards=[Shard]}])
     || Shard <- compactor_shards(Compactor)
    ].

-spec compact_shard(compactor()) -> 'ok'.
compact_shard(#compactor{shards=[Shard]}=Compactor) ->
    kz_util:put_callid(<<"compact_shard_", Shard/binary>>),

    wait_for_compaction(compactor_admin(Compactor), Shard),

    case get_db_disk_and_data(compactor_admin(Compactor), Shard) of
        'undefined' ->
            lager:debug("beginning shard compaction"),
            start_compacting_shard(Compactor);
        'not_found' -> 'ok';
        {BeforeDisk, BeforeData} ->
            lager:debug("beginning shard compaction: ~p disk/~p data", [BeforeDisk, BeforeData]),
            start_compacting_shard(Compactor)
    end.

-spec start_compacting_shard(compactor()) -> 'ok'.
start_compacting_shard(#compactor{shards=[Shard]}=Compactor) ->
    lager:debug("compacting shard ~s", [Shard]),
    case kz_couch_db:db_compact(compactor_admin(Compactor), Shard) of
        'true' -> continue_compacting_shard(Compactor);
        'false' -> lager:debug("  compaction of shard failed, skipping")
    end.

-spec continue_compacting_shard(compactor()) -> 'ok'.
continue_compacting_shard(#compactor{shards=[Shard]}=Compactor) ->
    wait_for_compaction(compactor_admin(Compactor), Shard),

    %% cleans up old view indexes
    IsCleanup = kz_couch_db:db_view_cleanup(compactor_admin(Compactor), Shard),
    lager:debug("  is shard view cleaning up started ~s: ~s", [Shard, IsCleanup]),

    wait_for_compaction(compactor_admin(Compactor), Shard),

    %% compacts views
    lager:debug("  design doc compaction starting for shard ~s", [Shard]),
    compact_design_docs(compactor_admin(Compactor), Shard, compactor_design_docs(Compactor)),

    case get_db_disk_and_data(compactor_admin(Compactor), Shard) of
        'undefined' -> lager:debug("  finished compacting shard");
        'not_found' -> lager:debug("  finished compacting shard");
        {AfterDisk, AfterData} ->
            lager:debug("  finished compacting shard: ~p disk/~p data", [AfterDisk, AfterData])
    end.

-spec compact_design_docs(kz_data:connection(), ne_binary(), ne_binaries()) -> 'ok'.
compact_design_docs(_Conn, _Shard, []) -> 'ok';
compact_design_docs(Conn, Shard, DDs) ->
    try lists:split(?MAX_COMPACTING_VIEWS, DDs) of
        {Compact, Remaining} ->
            IsStarted = [{DD, kz_couch_view:design_compact(Conn, Shard, DD)} || DD <- Compact],
            lager:debug("  is shard ~s compacting chunk of views started: ~p", [Shard, IsStarted]),
            wait_for_design_compaction(Conn, Shard, Compact),
            compact_design_docs(Conn, Shard, Remaining)
    catch
        'error':'badarg' ->
            IsStarted = [{DD, kz_couch_view:design_compact(Conn, Shard, DD)} || DD <- DDs],
            lager:debug("  is shard ~s compacting last chunk of views started: ~p", [Shard, IsStarted]),
            wait_for_design_compaction(Conn, Shard, DDs)
    end.

-type db_info_resp() :: {'ok', kz_json:object()} |
                        {'error', any()}.
-type design_info_resp() :: {'ok', kz_json:object()} |
                            {'error', any()}.

-spec wait_for_design_compaction(kz_data:connection(), ne_binary(), ne_binaries()) ->
                                        'ok'.
-spec wait_for_design_compaction(kz_data:connection(), ne_binary(), ne_binaries(), ne_binary(), design_info_resp()) ->
                                        'ok'.
wait_for_design_compaction(_, _, []) -> 'ok';
wait_for_design_compaction(AdminConn, Shard, [DD|DDs]) ->
    wait_for_design_compaction(AdminConn, Shard, DDs, DD, kz_couch_view:design_info(AdminConn, Shard, DD)).

wait_for_design_compaction(AdminConn, Shard, DDs, DD, {'error', {'conn_failed', {'error', 'timeout'}}}) ->
    lager:debug("connecting to BigCouch timed out, waiting then retrying"),
    'ok' = timer:sleep(?SLEEP_BETWEEN_POLL),
    wait_for_design_compaction(AdminConn, Shard, DDs, DD, kz_couch_view:design_info(AdminConn, Shard, DD));
wait_for_design_compaction(AdminConn, Shard, DDs, _DD, {'error', 'not_found'}) ->
    wait_for_design_compaction(AdminConn, Shard, DDs);
wait_for_design_compaction(AdminConn, Shard, DDs, _DD, {'error', _E}) ->
    lager:debug("failed design status for '~s/~s': ~p", [Shard, _DD, _E]),
    'ok' = timer:sleep(?SLEEP_BETWEEN_POLL),
    wait_for_design_compaction(AdminConn, Shard, DDs);
wait_for_design_compaction(AdminConn, Shard, DDs, DD, {'ok', DesignInfo}) ->
    case kz_json:is_true(<<"compact_running">>, DesignInfo, 'false') of
        'false' ->
            wait_for_design_compaction(AdminConn, Shard, DDs);
        'true' ->
            'ok' = timer:sleep(?SLEEP_BETWEEN_POLL),
            wait_for_design_compaction(AdminConn, Shard, DDs, DD, kz_couch_view:design_info(AdminConn, Shard, DD))
    end.

-spec wait_for_compaction(kz_data:connection(), ne_binary()) -> 'ok'.
-spec wait_for_compaction(kz_data:connection(), ne_binary(), db_info_resp()) -> 'ok'.
wait_for_compaction(AdminConn, Shard) ->
    wait_for_compaction(AdminConn, Shard, kz_couch_db:db_info(AdminConn, Shard)).

wait_for_compaction(_AdminConn, _Shard, {'error', 'db_not_found'}) ->
    lager:debug("shard '~s' wasn't found", [_Shard]);
wait_for_compaction(AdminConn, Shard, {'error', 'timeout'}) ->
    lager:warning("timed out querying db status; that seems irregular!"),
    'ok' = timer:sleep(?SLEEP_BETWEEN_POLL * 2),
    wait_for_compaction(AdminConn, Shard);
wait_for_compaction(_AdminConn, _Shard, {'error', _E}) ->
    lager:debug("failed to query shard ~s status: ~p", [_Shard, kz_couch_util:format_error(_E)]);
wait_for_compaction(AdminConn, Shard, {'ok', ShardData}) ->
    case kz_json:is_true(<<"compact_running">>, ShardData, 'false') of
        'false' -> lager:debug("compaction has ended");
        'true' ->
            'ok' = timer:sleep(?SLEEP_BETWEEN_POLL),
            wait_for_compaction(AdminConn, Shard)
    end.


-spec should_compact(compactor()) -> boolean().
-spec should_compact(compactor(), heuristic()) -> boolean().
should_compact(Compactor) ->
    should_compact(Compactor, compactor_heuristic(Compactor)).

should_compact(_Compactor, ?HEUR_NONE) -> 'true';
should_compact(Compactor, ?HEUR_RATIO) ->
    case get_db_disk_and_data(compactor_conn(Compactor), compactor_database(Compactor)) of
        {Disk, Data} -> should_compact_ratio(Disk, Data);
        'undefined' ->
            lager:debug("db sizes for ~s not found, skipping", [compactor_database(Compactor)]),
            'false';
        'not_found' ->
            lager:debug("db ~s not found on node ~s, skipping"
                       ,[compactor_database(Compactor), compactor_node(Compactor)]
                       ),
            'false'
    end.

-spec get_db_disk_and_data(kz_data:connection(), ne_binary()) ->
                                  {pos_integer(), pos_integer()} |
                                  'undefined' | 'not_found'.
-spec get_db_disk_and_data(kz_data:connection(), ne_binary(), 0..3) ->
                                  {pos_integer(), pos_integer()} |
                                  'undefined' | 'not_found'.
get_db_disk_and_data(Conn, DbName) ->
    get_db_disk_and_data(Conn, DbName, 0).

get_db_disk_and_data(_Conn, _DbName, 3=_N) ->
    lager:warning("getting db info for ~s failed ~b times", [_DbName, _N]),
    'undefined';
get_db_disk_and_data(Conn, DbName, N) ->
    N > 0
        andalso lager:debug("getting db info re-attempt ~p", [N]),
    case kz_couch_db:db_info(Conn, DbName) of
        {'ok', Info} ->
            {kz_json:get_integer_value(<<"disk_size">>, Info)
            ,kz_json:get_integer_value([<<"other">>, <<"data_size">>], Info)
            };
        {'error', {'conn_failed',{'error','timeout'}}} ->
            lager:debug("timed out asking for info, waiting and trying again"),
            'ok' = timer:sleep(?MILLISECONDS_IN_SECOND),
            get_db_disk_and_data(Conn, DbName, N+1);
        {'error', 'not_found'} ->
            lager:debug("db '~s' not found, skipping", [DbName]),
            'not_found';
        {'error', 'db_not_found'} ->
            lager:debug("shard '~s' not found, skipping", [DbName]),
            'not_found';
        {'error', _E} ->
            lager:debug("failed to lookup ~s info: ~p", [DbName, _E]),
            'undefined'
    end.

-spec should_compact_ratio(integer(), integer()) -> boolean().
should_compact_ratio(Disk, Data) ->
    min_data_met(Data, ?MIN_DATA)
        andalso min_ratio_met(Disk, Data, ?MIN_RATIO).

-spec min_data_met(integer(), integer()) -> boolean().
min_data_met(Data, Min) when Data > Min ->
    lager:debug("data size ~b is larger than minimum ~b", [Data, Min]),
    'true';
min_data_met(_Data, _Min) ->
    lager:debug("data size ~b is under min_data_size threshold ~b", [_Data, _Min]),
    'false'.

-spec min_ratio_met(integer(), integer(), float()) -> boolean().
min_ratio_met(Disk, Data, MinRatio) ->
    case Disk / Data of
        R when R > MinRatio ->
            lager:debug("ratio ~p is greater than min ratio: ~p", [R, MinRatio]),
            'true';
        _R ->
            lager:debug("ratio ~p (~p/~p) is under min threshold ~p", [_R, Disk, Data, MinRatio]),
            'false'
    end.

-spec compactor_node(compactor()) -> ne_binary().
-spec compactor_conn(compactor()) -> kz_data:connection().
-spec compactor_admin(compactor()) -> kz_data:connection().
-spec compactor_database(compactor()) -> ne_binary().
-spec compactor_shards(compactor()) -> ne_binaries().
-spec compactor_design_docs(compactor()) -> ne_binaries().
-spec compactor_heuristic(compactor()) -> heuristic().

compactor_node(#compactor{node=Node}) -> Node.
compactor_conn(#compactor{conn=Conn}) -> Conn.
compactor_admin(#compactor{admin=Admin}) -> Admin.
compactor_database(#compactor{database=Db}) -> Db.
compactor_shards(#compactor{shards=Shards}) -> Shards.
compactor_design_docs(#compactor{design_docs=DesignDocs}) -> DesignDocs.
compactor_heuristic(#compactor{heuristic=Heuristic}) -> Heuristic.

-spec new(ne_binary(), heuristic(), kz_data:connection(), kz_data:connection(), ne_binary()) ->
                 compactor().
new(Node, Heuristic, APIConn, AdminConn, Database) ->
    #compactor{node=Node
              ,database=kz_http_util:urlencode(Database)
              ,conn=APIConn
              ,admin=AdminConn
              ,heuristic=Heuristic
              ,shards=node_shards(Node, Database)
              ,design_docs=db_design_docs(APIConn, Database)
              }.

-spec node_shards(ne_binary(), ne_binary()) -> ne_binaries().
-spec node_shards(ne_binary(), ne_binary(), kz_json:object()) -> ne_binaries().
node_shards(Node, Unencoded) ->
    {'ok', DbInfo} = kz_datamgr:open_doc(kazoo_couch:get_admin_dbs(), Unencoded),
    node_shards(Node, Unencoded, DbInfo).

node_shards(Node, Unencoded, DbInfo) ->
    Suffix = kz_json:get_ne_binary_value(<<"shard_suffix">>, DbInfo),
    Ranges = kz_json:get_list_value([<<"by_node">>, Node], DbInfo, []),
    [kz_http_util:urlencode(<<"shards/", Range/binary, "/", Unencoded/binary, Suffix/binary>>)
     || Range <- Ranges
    ].

-spec db_design_docs(kz_data:connection(), ne_binary()) -> ne_binaries().
db_design_docs(Conn, D) ->
    case kz_couch_view:all_design_docs(Conn, kz_http_util:urlencode(D)) of
        {'ok', Designs} ->
            [encode_design_doc(kz_doc:id(Design)) || Design <- Designs];
        {'error', _} -> []
    end.

-spec encode_design_doc(ne_binary()) -> ne_binary().
encode_design_doc(Design) ->
    binary:replace(Design, <<"_design/">>, <<>>, []).
