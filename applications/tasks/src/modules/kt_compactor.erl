%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_compactor).

%% behaviour: tasks_provider

-export([init/0]).

-export([compact_all/2
        ,compact_node/3
        ,compact_db/3

        ,compact_db/1
        ,compact_node/1

         %% Used to handle auto_compaction triggers. Check init/0 for more info.
        ,do_compact_db/1
        ]).

-export([browse_dbs_for_triggers/1]).

%% Triggerables
-export([help/1, help/2, help/3
        ,output_header/1
        ]).

-ifdef(TEST).
-export([sort_by_disk_size/1
        ,build_compaction_callid/1
        ]).
-endif.

-include("tasks.hrl").
-include("kt_compactor.hrl").

-define(CATEGORY, "compaction").
-define(ACTIONS, [<<"compact_all">>
                 ,<<"compact_node">>
                 ,<<"compact_db">>
                 ]).

-define(OUTPUT_HEADER
       ,[<<"node">>, <<"database">>, <<"before_disk">>, <<"before_data">>, <<"after_disk">>, <<"after_data">>]
       ).

-type rows() :: [kz_csv:row()] | [].
-type db_and_sizes() :: {kz_term:ne_binary(), kt_compactor_worker:db_disk_and_data()}.
-type dbs_and_sizes() :: [db_and_sizes()].

-export_type([rows/0
             ,db_and_sizes/0
             ,dbs_and_sizes/0
             ]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    AdminNodes = kazoo_couch:get_admin_nodes(),
    %% Refresh `nodes | _nodes' db
    _ = kapps_maintenance:refresh(AdminNodes),
    %% Refresh `dbs | _dbs' db needed for the compactor/listing_by_node view.
    _ = kapps_maintenance:refresh(kazoo_couch:get_admin_dbs()),
    set_node_defaults(AdminNodes),

    _ = case ?COMPACT_AUTOMATICALLY of
            'false' -> lager:info("node ~s not configured to compact automatically", [node()]);
            'true' ->
                _ = tasks_bindings:bind(?TRIGGER_AUTO_COMPACTION
                                       ,?MODULE
                                       ,'browse_dbs_for_triggers'
                                       ),
                lager:info("node ~s configured to compact automatically", [node()]),
                %% Need to use `do_compact_db/1' instead of `compact_db/1' because the
                %% the former uses `?HEUR_RATIO' for heuristic and the latter ignores
                %% heuristic and doesn't allow to improve auto compaction job exec time.
                _ = tasks_bindings:bind(?TRIGGER_ALL_DBS, ?MODULE, 'do_compact_db')
        end,

    _ = tasks_bindings:bind(<<"tasks.help">>, ?MODULE, 'help'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".output_header">>, ?MODULE, 'output_header'),
    tasks_bindings:bind_actions(<<"tasks."?CATEGORY>>, ?MODULE, ?ACTIONS).

-spec set_node_defaults(kz_term:ne_binary() | kz_json:object()) -> 'ok'.
set_node_defaults(AdminNodes) when is_binary(AdminNodes) ->
    {'ok', Nodes} = kz_datamgr:all_docs(AdminNodes),
    lists:foreach(fun set_node_defaults/1, Nodes);

set_node_defaults(NodeJObj) ->
    Node = kz_doc:id(NodeJObj),
    set_node_api_port(Node),
    set_node_admin_port(Node).

-spec set_node_api_port(kz_term:ne_binary()) -> 'ok'.
set_node_api_port(Node) ->
    case kapps_config:get_integer(?SYSCONFIG_COUCH, <<"api_port">>, 'undefined', Node) of
        'undefined' ->
            _ = kapps_config:set_node(?SYSCONFIG_COUCH, <<"api_port">>, ?API_PORT, Node),
            lager:debug("api port for ~s set to default: ~p", [Node, ?API_PORT]);
        _Port ->
            lager:debug("api port for ~s already set: ~p", [Node, _Port])
    end.

-spec set_node_admin_port(kz_term:ne_binary()) -> 'ok'.
set_node_admin_port(Node) ->
    case kapps_config:get_integer(?SYSCONFIG_COUCH, <<"admin_port">>, 'undefined', Node) of
        'undefined' ->
            _ = kapps_config:set_node(?SYSCONFIG_COUCH, <<"admin_port">>, ?ADMIN_PORT, Node),
            lager:debug("admin port for ~s set to default: ~p", [Node, ?ADMIN_PORT]);
        _Port ->
            lager:debug("admin port for ~s already set: ~p", [Node, _Port])
    end.

%%% Triggerables
-spec help(kz_json:object()) -> kz_json:object().
help(JObj) -> help(JObj, <<?CATEGORY>>).

-spec help(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category) ->
    lists:foldl(fun(Action, J) -> help(J, Category, Action) end, JObj, ?ACTIONS).

-spec help(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category, Action) ->
    kz_json:set_value([Category, Action], kz_json:from_list(action(Action)), JObj).

-spec action(kz_term:ne_binary()) -> kz_term:proplist().
action(<<"compact_all">>) ->
    [{<<"description">>, <<"Compact all databases">>}
    ,{<<"doc">>, <<"Walks all the databases and compacts them">>}
    ];
action(<<"compact_node">>) ->
    [{<<"description">>, <<"Compact databases on a database node">>}
    ,{<<"doc">>, <<"Compact all databases that are hosted on a given node">>}
    ,{<<"expected_content">>, <<"text/csv">>}
    ,{<<"mandatory">>, [<<"node">>]}
    ,{<<"optional">>, [<<"force">>]}
    ];
action(<<"compact_db">>) ->
    %% prefix is mandatory field
    [{<<"description">>, <<"Compact database">>}
    ,{<<"doc">>, <<"Compact the database on all nodes hosting it">>}
    ,{<<"expected_content">>, <<"text/csv">>}
    ,{<<"mandatory">>, [<<"database">>]}
    ,{<<"optional">>, [<<"force">>]}
    ].

-spec output_header(kz_term:ne_binary()) -> kz_tasks:output_header().
output_header(<<"compact_all">>) -> ?OUTPUT_HEADER;
output_header(<<"compact_node">>) -> ?OUTPUT_HEADER;
output_header(<<"compact_db">>) -> ?OUTPUT_HEADER.

-spec compact_all(kz_tasks:extra_args(), kz_tasks:iterator()) -> kz_tasks:iterator().
compact_all(Extra, 'init') ->
    {'ok', is_allowed(Extra)};
compact_all(_Extra, 'true') ->
    %% Dbs to be compacted will be set at `do_compact_all/0'
    {track_job(<<"compact_all">>, fun do_compact_all/0, []), 'stop'};
compact_all(_Extra, 'false') ->
    {<<"compaction is only allowed by system administrators">>, 'stop'}.

-spec compact_node(kz_tasks:extra_args(), kz_tasks:iterator(), kz_tasks:args()) -> kz_tasks:iterator().
compact_node(Extra, 'init', Args) ->
    compact_node(Extra, is_allowed(Extra), Args);
compact_node(_Extra, 'false', _Args) ->
    {<<"compaction is only allowed by system administrators">>, 'stop'};
compact_node(_Extra, 'true', #{<<"node">> := Node}=Row) ->
    %% Dbs to be compacted will be set at `do_compact_node/4'
    Rows = track_job(<<"compact_node">>
                    ,fun do_compact_node/2
                    ,[Node, heuristic_from_flag(maps:get(<<"force">>, Row))]
                    ),
    {Rows, 'true'}.

-spec compact_db(kz_tasks:extra_args(), kz_tasks:iterator(), kz_tasks:args()) -> kz_tasks:iterator().
compact_db(Extra, 'init', Args) ->
    compact_db(Extra, is_allowed(Extra), Args);
compact_db(_Extra, 'false', _Args) ->
    {<<"compaction is only allowed by system administrators">>, 'stop'};
compact_db(_Extra, 'true', #{<<"database">> := Database}=Row) ->
    Rows = track_job(<<"compact_db">>
                    ,fun do_compact_db/2
                    ,[Database, heuristic_from_flag(maps:get(<<"force">>, Row))]
                    ),
    {Rows, 'true'}.

-spec compact_db(kz_term:ne_binary()) -> 'ok'.
compact_db(?MATCH_ACCOUNT_RAW(AccountId)) ->
    lager:info("adjusting account id ~s to db name", [AccountId]),
    compact_db(kzs_util:format_account_id(AccountId, 'unencoded'));
compact_db(Database) ->
    CallIdBin = kz_term:to_binary(kz_log:get_callid()),
    print_csv(maybe_track_compact_db(Database, ?HEUR_NONE, CallIdBin)).

-spec maybe_track_compact_db(kz_term:ne_binary(), heuristic(), kz_term:ne_binary()) -> rows().
maybe_track_compact_db(Db, Heur, <<"undefined">>) ->
    %% If not callid defined yet, then this function was call directly with a db name so
    %% it creates a new callid to track this db-only compaction job.
    track_job(<<"compact_db">>, fun do_compact_db/2, [Db, Heur], get_dbs_sizes([Db]));
maybe_track_compact_db(Db, Heur, <<"sup_", _/binary>> = SupId) ->
    %% If callid starts with `sup_', then this function was called via a SUP command
    %% so it creates a new callid (remove the `@' sign + anything at the right of it) to
    %% track this db-only compaction job.
    JobType = supid_to_jobtype(SupId),
    track_job(JobType, fun do_compact_db/2, [Db, Heur], get_dbs_sizes([Db]));
maybe_track_compact_db(Db, Heur, CallId) ->
    %% If there is already a callid defined, then do_compact_db/2 will use it for updating
    %% the corresponding compaction's job stats.
    track_job(CallId, fun do_compact_db/2, [Db, Heur], get_dbs_sizes([Db])).

-spec print_csv(iolist()) -> 'ok'.
print_csv([]) -> 'ok';
print_csv(Rows) ->
    log_and_print("~s~n", [kz_binary:join(?OUTPUT_HEADER)]),
    [log_and_print("~s~n", [kz_binary:join(Row)]) || Row <- Rows],
    'ok'.

-spec log_and_print(string(), [binary()]) -> 'ok'.
log_and_print(FormatStr, Values) ->
    %% Writes to *.log files. Useful because it also saves the process' callid this log
    %% line belongs to.
    lager:debug(FormatStr, Values),
    %% Writes to stdout. Useful for SUP commands to show output.
    io:format(FormatStr, Values).

-spec heuristic_from_flag(kz_term:api_ne_binary()) -> heuristic().
heuristic_from_flag(Force) ->
    case kz_term:is_true(Force) of
        'false' -> ?HEUR_RATIO;
        'true' -> ?HEUR_NONE
    end.

-spec is_allowed(kz_tasks:extra_args()) -> boolean().
is_allowed(ExtraArgs) ->
    AuthAccountId = maps:get('auth_account_id', ExtraArgs),
    {'ok', AuthAccountDoc} = kzd_accounts:fetch(AuthAccountId),
    kzd_accounts:is_superduper_admin(AuthAccountDoc).

-spec do_compact_all() -> rows().
do_compact_all() ->
    CallId = kz_log:get_callid(),

    case get_all_dbs_and_sort_by_disk() of
        [] -> lager:info("failed to find any dbs");
        Sorted ->
            lager:info("starting do_compact_all execution, ~p dbs found", [length(Sorted)]),
            'ok' = kt_compaction_reporter:set_job_dbs(CallId, Sorted),
            lists:foldl(fun do_compact_db_fold/2, [], Sorted)
    end.

-spec compact_node(kz_term:ne_binary()) -> 'ok'.
compact_node(Node) ->
    %% Same as compact_db/1 but for nodes instead of dbs.
    CallId = kz_term:to_binary(kz_log:get_callid()),
    print_csv(maybe_track_compact_node(Node, ?HEUR_NONE, CallId)).

-spec maybe_track_compact_node(kz_term:ne_binary(), heuristic(), kz_term:ne_binary()) -> rows().
maybe_track_compact_node(Node, Heur, <<"undefined">>) ->
    %% Dbs to be compacted will be set at `do_compact_node/4'
    track_job(<<"compact_node">>, fun do_compact_node/2, [Node, Heur]);
maybe_track_compact_node(Node, Heur, <<"sup_", _/binary>> = SupId) ->
    %% Triggered via SUP command
    track_job(supid_to_jobtype(SupId), fun do_compact_node/2, [Node, Heur]);
maybe_track_compact_node(Node, Heur, _CallId) ->
    do_compact_node(Node, Heur).

-spec do_compact_node(kz_term:ne_binary(), heuristic()) ->
          rows().
do_compact_node(Node, Heuristic) ->
    #{server := {_App, #server{}=Conn}} = kzs_plan:plan(),

    case get_node_connections(Node, Conn) of
        {'error', _E} ->
            lager:error("failed to get node connections for ~s", [Node]),
            [];
        {APIConn, AdminConn} ->
            do_compact_node(Node, Heuristic, APIConn, AdminConn)
    end.

-spec do_compact_node(kz_term:ne_binary(), heuristic(), kz_data:connection(), kz_data:connection()) ->
          rows().
do_compact_node(Node, Heuristic, APIConn, AdminConn) ->
    case kz_datamgr:get_results(kazoo_couch:get_admin_dbs(APIConn)
                               ,<<"compactor/listing_by_node">>
                               ,[{'startkey', [Node]}
                                ,{'endkey', [Node, kz_json:new()]}
                                ]
                               )
    of
        {'ok', []} -> lager:debug("no databases on node ~s", [Node]), [];
        {'error', _E} -> lager:warning("error getting databases on node ~s: ~p", [Node, _E]), [];
        {'ok', ViewResults} ->
            NodeDBs = [kz_doc:id(ViewResult) || ViewResult <- ViewResults],
            Sorted = sort_by_disk_size(get_dbs_sizes(NodeDBs)),
            'ok' = kt_compaction_reporter:set_job_dbs(kz_log:get_callid(), Sorted),
            SortedWithoutSizes = [Db || {Db, _Sizes} <- Sorted],
            do_compact_node(Node, Heuristic, APIConn, AdminConn, SortedWithoutSizes)
    end.

-spec do_compact_node(kz_term:ne_binary(), heuristic(), kz_data:connection(), kz_data:connection(), kz_term:ne_binaries()) -> rows().
do_compact_node(Node, Heuristic, APIConn, AdminConn, Databases) ->
    CallId = kz_log:get_callid(),
    lists:foldl(fun(Database, Acc) ->
                        lager:debug("setting current_db to ~p on compaction reporter", [Database]),
                        'ok' = kt_compaction_reporter:current_db(CallId, Database),
                        NewAcc = do_compact_node_db(Node, Heuristic, APIConn, AdminConn, Database, Acc),
                        lager:debug("finished compacting ~p db on node ~p", [Database, Node]),
                        'ok' = kt_compaction_reporter:finished_db(CallId, Database, NewAcc),
                        NewAcc
                end
               ,[]
               ,Databases
               ).

-spec do_compact_node_db(kz_term:ne_binary(), heuristic(), kz_data:connection(), kz_data:connection(), kz_term:ne_binary(), rows()) -> rows().
do_compact_node_db(Node, Heuristic, APIConn, AdminConn, Database, Acc) ->
    Compactor = node_compactor(Node, Heuristic, APIConn, AdminConn, Database),
    Shards = kt_compactor_worker:compactor_shards(Compactor),
    lager:info("adding ~p found shards to compaction reporter", [length(Shards)]),
    'ok' = kt_compaction_reporter:add_found_shards(kz_log:get_callid(), length(Shards)),
    do_compact_node_db(Compactor, Acc).

-spec do_compact_node_db(kt_compactor_worker:compactor(), rows()) -> rows().
do_compact_node_db(Compactor, Acc) ->
    [do_compact_node_db(Compactor) | Acc].

-spec do_compact_node_db(kt_compactor_worker:compactor()) -> kz_csv:row().
do_compact_node_db(Compactor) ->
    Conn = kt_compactor_worker:compactor_conn(Compactor),
    Database = kt_compactor_worker:compactor_database(Compactor),

    BeforeCols = db_usage_cols(Conn, Database),
    _ = kt_compactor_worker:run_compactor(Compactor),
    AfterCols = db_usage_cols(Conn, Database),
    [kt_compactor_worker:compactor_node(Compactor)
    ,Database
     | BeforeCols
    ] ++ AfterCols.

-spec do_compact_db(kz_term:ne_binary()) -> rows().
do_compact_db(Database) ->
    lager:debug("about to start compacting ~p db", [Database]),
    do_compact_db(Database, ?HEUR_RATIO).

-spec do_compact_db(kz_term:ne_binary(), heuristic()) -> rows().
do_compact_db(Database, Heuristic) ->
    do_compact_db_by_nodes(Database, Heuristic).

-spec do_compact_db_fold(db_and_sizes() | kz_term:ne_binary(), rows()) -> rows().
do_compact_db_fold({Db, _Sizes}, Rows) ->
    do_compact_db_fold(Db, Rows);
do_compact_db_fold(Database, Rows) ->
    Rows ++ do_compact_db(Database).

-spec do_compact_db_by_nodes(kz_term:ne_binary(), heuristic()) -> rows().
do_compact_db_by_nodes(?MATCH_ACCOUNT_RAW(_)=AccountId, Heuristic) ->
    lager:info("formatting raw account id ~s", [AccountId]),
    do_compact_db_by_nodes(kzs_util:format_account_id(AccountId, 'unencoded'), Heuristic);
do_compact_db_by_nodes(?MATCH_ACCOUNT_ENCODED(_)=AccountDb, Heuristic) ->
    lager:info("formatting unencoded account db ~s", [AccountDb]),
    do_compact_db_by_nodes(kzs_util:format_account_id(AccountDb, 'unencoded'), Heuristic);
do_compact_db_by_nodes(?MATCH_MODB_SUFFIX_RAW(_AccountId, _Year, _Month)=MODB, Heuristic) ->
    lager:info("formatting raw modb ~s", [MODB]),
    do_compact_db_by_nodes(kzs_util:format_account_modb(MODB, 'unencoded'), Heuristic);
do_compact_db_by_nodes(Database, Heuristic) ->
    AdminDbs = kazoo_couch:get_admin_dbs(),
    lager:info("opening in ~s: ~p", [AdminDbs, Database]),
    {'ok', DbInfo} = kz_datamgr:open_doc(AdminDbs, Database),
    kz_json:foldl(fun(Node, _, Rows) ->
                          do_compact_db_by_node(Node, Heuristic, Database, Rows)
                  end
                 ,[]
                 ,kz_json:get_json_value(<<"by_node">>, DbInfo)
                 ).

-spec do_compact_db_by_node(kz_term:ne_binary(), heuristic(), kz_term:ne_binary(), rows()) -> rows().
do_compact_db_by_node(Node, Heuristic, Database, Acc) ->
    lager:debug("about to start compacting ~p db on node ~p", [Database, Node]),
    #{'server' := {_App, #server{}=Conn}} = kzs_plan:plan(),
    case get_node_connections(Node, Conn) of
        {'error', _E} ->
            lager:error("failed to get node connections for ~s", [Node]),
            Acc;
        {APIConn, AdminConn} ->
            do_compact_db_by_node(Node, Heuristic, APIConn, AdminConn, Database, Acc)
    end.

-spec do_compact_db_by_node(kz_term:ne_binary(), heuristic(), kz_data:connection(), kz_data:connection(), kz_term:ne_binary(), rows()) -> rows().
do_compact_db_by_node(Node, Heuristic, APIConn, AdminConn, Database, Acc) ->
    lager:debug("compacting ~p db on node ~p", [Database, Node]),
    case do_compact_node(Node, Heuristic, APIConn, AdminConn, [Database]) of
        [] -> Acc;
        [Row] -> [Row | Acc]
    end.

-spec db_usage_cols(kz_data:connection(), kz_term:ne_binary()) -> [integer()].
db_usage_cols(Conn, Database) ->
    case kt_compactor_worker:get_db_disk_and_data(Conn, Database) of
        'undefined' -> [0, 0];
        'not_found' -> [0, 0];
        {Disk, Data} -> [Disk, Data]
    end.

-spec node_compactor(kz_term:ne_binary(), heuristic(), kz_data:connection(), kz_data:connection(), kz_term:ne_binary()) ->
          kt_compactor_worker:compactor().
node_compactor(Node, Heuristic, APIConn, AdminConn, Database) ->
    kt_compactor_worker:new(Node, Heuristic, APIConn, AdminConn, Database).

-spec get_node_connections(kz_term:ne_binary(), kz_data:connection()) ->
          {kz_data:connection(), kz_data:connection()} |
          {'error', 'no_connection'}.
get_node_connections(Node, #server{options=Options}) ->
    [_, Host] = binary:split(Node, <<"@">>),
    Hostname = kz_term:to_list(Host),

    NodeAPIPort = ?NODE_API_PORT(Node),
    NodeAdminPort = ?NODE_ADMIN_PORT(Node),

    lager:debug("getting connection information for ~s, ~p and ~p", [Host, NodeAPIPort, NodeAdminPort]),
    C1 = couchbeam:server_connection(Hostname, NodeAPIPort, <<"">>, Options),
    C2 = couchbeam:server_connection(Hostname, NodeAdminPort, <<"">>, Options),

    try {kz_couch_util:connection_info(C1),
         kz_couch_util:connection_info(C2)
        }
    of
        {{'error', 'timeout'}, _} ->
            lager:warning("timed out getting connection for ~s, try again", [Host]),
            {'error', 'no_connection'};
        {_, {'error', 'timeout'}} ->
            lager:warning("timed out getting connection for ~s, try again", [Host]),
            {'error', 'no_connection'};
        {{'error', _E}, _} ->
            lager:warning("error getting conn: ~p", [_E]),
            {'error', 'no_connection'};
        {_, {'error', _E}} ->
            lager:warning("error getting admin conn: ~p", [_E]),
            {'error', 'no_connection'};
        {{'ok', N1}, {'ok', N2}} -> {N1, N2}
    catch
        'error':{'case_clause',{'error',{'conn_failed',{'error','econnrefused'}}}} ->
            lager:warning("connection refused when connecting to ~s (on either ~p or ~p)"
                         ,[Host, NodeAPIPort, NodeAdminPort]
                         ),
            {'error', 'no_connection'};
        _E:_R ->
            lager:warning("failed to connect to ~s: ~s: ~p", [Host, _E, _R]),
            {'error', 'no_connection'}
    end.

-spec get_all_dbs_and_sort_by_disk() -> [db_and_sizes()].
get_all_dbs_and_sort_by_disk() ->
    sort_by_disk_size(get_dbs_and_sizes()).

-spec get_dbs_and_sizes() -> [db_and_sizes()].
get_dbs_and_sizes() ->
    {'ok', Dbs} = kz_datamgr:db_info(),
    get_dbs_sizes(Dbs).

-spec get_dbs_sizes(kz_term:ne_binaries()) -> [db_and_sizes()].
get_dbs_sizes(Dbs) ->
    #{'server' := {_App, #server{}=Conn}} = kzs_plan:plan(),
    F = fun(Db, State) ->
                get_db_disk_and_data_fold(Conn, Db, State, ?COMPACTION_LIST_DBS_CHUNK_SIZE)
        end,
    {DbsAndSizes, _} = lists:foldl(F, {[], 0}, Dbs),
    DbsAndSizes.

-spec get_db_disk_and_data_fold(#server{}
                               ,kz_term:ne_binary()
                               ,{[db_and_sizes()], non_neg_integer()}
                               ,pos_integer()
                               ) -> {[db_and_sizes()], pos_integer()}.
get_db_disk_and_data_fold(Conn, UnencDb, {_, Counter} = State, ChunkSize)
  when Counter rem ChunkSize =:= 0 ->
    %% Every `ChunkSize' handled requests, sleep `?COMPACTION_LIST_DBS_PAUSE'ms (give the db a rest).
    lager:debug("~p dbs read, resting for ~p ms", [Counter, ?COMPACTION_LIST_DBS_PAUSE]),
    timer:sleep(?COMPACTION_LIST_DBS_PAUSE),
    do_get_db_disk_and_data_fold(Conn, UnencDb, State);
get_db_disk_and_data_fold(Conn, UnencDb, State, _ChunkSize) ->
    do_get_db_disk_and_data_fold(Conn, UnencDb, State).

-spec do_get_db_disk_and_data_fold(#server{}
                                  ,kz_term:ne_binary()
                                  ,{[db_and_sizes()], non_neg_integer()}
                                  ) -> {[db_and_sizes()], pos_integer()}.
do_get_db_disk_and_data_fold(Conn, UnencDb, {Acc, Counter}) ->
    EncDb = kz_http_util:urlencode(UnencDb),

    case kt_compactor_worker:get_db_disk_and_data(Conn, EncDb) of
        'not_found' ->
            lager:debug("error accessing ~s: not_found", [UnencDb]),
            {Acc, Counter};
        Info ->
            {[{UnencDb, Info} | Acc]
            ,Counter + 1
            }
    end.

-spec sort_by_disk_size([db_and_sizes()]) -> [db_and_sizes()].
sort_by_disk_size(DbsSizes) when is_list(DbsSizes) ->
    lists:sort(fun sort_by_disk_size/2, DbsSizes).

-spec sort_by_disk_size(db_and_sizes(), db_and_sizes()) -> boolean().
sort_by_disk_size({_UnencDb1, {DiskSize1, _}}, {_UnencDb2, {DiskSize2, _}}) ->
    DiskSize1 > DiskSize2;
sort_by_disk_size({_UnencDb1, {_DiskSize1, _}}, {_UnencDb2, _Else}) -> %% Else = 'not_found' | 'undefined'
    'true';
sort_by_disk_size({_UnencDb1, _Else}, {_UnencDb2, {_DiskSize2, _}}) -> %% Else = 'not_found' | 'undefined'
    'false'.

-spec track_job(kz_term:ne_binary(), function(), [term()]) -> rows().
track_job(JobType, Fun, Args) ->
    track_job(JobType, Fun, Args, []).

-spec track_job(kz_term:ne_binary(), function(), [term()], dbs_and_sizes()) -> rows().
track_job(JobType, Fun, Args, Dbs) when is_function(Fun)
                                        andalso is_list(Args) ->
    try
        CallId = build_compaction_callid(JobType),
        kz_log:put_callid(CallId),
        'ok' = kt_compaction_reporter:start_tracking_job(self(), node(), CallId, Dbs),
        Rows = erlang:apply(Fun, Args),
        'ok' = kt_compaction_reporter:stop_tracking_job(CallId),
        kz_log:put_callid('undefined'), % Reset callid
        Rows
    catch
        'error':{'badmatch', {'error','not_found'}} -> []
    end.

%% SupId = <<"sup_0351@fqdn.hostname.com">>, JobType = <<"sup_0351">>.
-spec supid_to_jobtype(kz_term:ne_binary()) -> kz_term:ne_binary().
supid_to_jobtype(SupId) ->
    hd(binary:split(SupId, <<"@">>)).

%% =======================================================================================
%% Start - Automatic Compaction Section
%% =======================================================================================

%%------------------------------------------------------------------------------
%% @doc Entry point for starting the automatic compaction job.
%%
%% This functions gets triggered by the `browse_dbs_ref' based on `browse_dbs_timer'
%% function. By default it triggers the action 1 day after the timer starts.
%% @end
%%------------------------------------------------------------------------------
-spec browse_dbs_for_triggers(atom() | reference()) -> 'ok'.
browse_dbs_for_triggers(Ref) ->
    CallId = build_compaction_callid(<<"cleanup_pass">>),
    kz_log:put_callid(CallId),
    lager:info("starting cleanup pass of databases"),
    Dbs = maybe_list_and_sort_dbs_for_compaction(?COMPACT_AUTOMATICALLY, CallId),
    _Counter = lists:foldl(fun trigger_db_cleanup/2, {length(Dbs), 1}, Dbs),
    'ok' = kt_compaction_reporter:stop_tracking_job(CallId),
    kz_log:put_callid('undefined'), % Reset callid
    lager:info("pass completed for ~p", [Ref]),
    gen_server:cast('kz_tasks_trigger', {'cleanup_finished', Ref}).

-spec build_compaction_callid(kz_term:ne_binary()) -> kz_term:ne_binary().
build_compaction_callid(JobTypeBin) ->
    {Year, Month, _} = erlang:date(),
    %% <<"YYYYMM-jobtype_xxxxxxxx">> = CallId
    <<(integer_to_binary(Year))/binary                                  %% YYYY
     ,(kz_binary:pad_left(integer_to_binary(Month), 2, <<"0">>))/binary %% MM
     ,"-"
     ,JobTypeBin/binary                                                 %% jobtype
     ,"_"
     ,(kz_binary:rand_hex(4))/binary                                    %% xxxxxxxx
    >>.

-spec trigger_db_cleanup(db_and_sizes() | kz_term:ne_binary()
                        ,{pos_integer(), pos_integer()}
                        ) -> {pos_integer(), pos_integer()}.
trigger_db_cleanup({Db, _Sizes}, Acc) ->
    trigger_db_cleanup(Db, Acc);
trigger_db_cleanup(Db, {TotalDbs, Counter}) ->
    lager:debug("triggering ~p db compaction ~p/~p (~p remaining)",
                [Db, Counter, TotalDbs, (TotalDbs - Counter)]),
    cleanup_pass(Db),
    {TotalDbs, Counter + 1}.

-spec maybe_list_and_sort_dbs_for_compaction(boolean(), kz_term:ne_binary()) ->
          [kz_term:ne_binary()].
maybe_list_and_sort_dbs_for_compaction('true', CallId) ->
    lager:debug("auto compaction enabled, getting databases list and sorting them by disk size"),
    Sorted = get_all_dbs_and_sort_by_disk(),
    lager:debug("finished listing and sorting databases (~p found)", [length(Sorted)]),
    'ok' = kt_compaction_reporter:start_tracking_job(self(), node(), CallId, Sorted),
    Sorted;
maybe_list_and_sort_dbs_for_compaction('false', _CallId) ->
    lager:debug("auto compaction disabled, skip sorting dbs by size"),
    {'ok', Dbs} = kz_datamgr:db_info(),
    lager:debug("finished listing databases (~p found)", [length(Dbs)]),
    Dbs.

-spec cleanup_pass(kz_term:ne_binary()) -> boolean().
cleanup_pass(Db) ->
    _ = tasks_bindings:map(db_to_trigger(Db), Db),
    erlang:garbage_collect(self()).

-spec db_to_trigger(kz_term:ne_binary()) -> kz_term:ne_binary().
db_to_trigger(Db) ->
    Classifiers = [{fun kapps_util:is_account_db/1, ?TRIGGER_ACCOUNT}
                  ,{fun kapps_util:is_account_mod/1, ?TRIGGER_ACCOUNT_MOD}
                  ,{fun is_system_db/1, ?TRIGGER_SYSTEM}
                  ],
    db_to_trigger(Db, Classifiers).

db_to_trigger(_Db, []) -> ?TRIGGER_OTHER;
db_to_trigger(Db, [{Classifier, Trigger} | Classifiers]) ->
    case Classifier(Db) of
        'true' -> Trigger;
        'false' -> db_to_trigger(Db, Classifiers)
    end.

-spec is_system_db(kz_term:ne_binary()) -> boolean().
is_system_db(Db) ->
    lists:member(Db, ?KZ_SYSTEM_DBS).
%% =======================================================================================
%% End - Automatic Compaction Section
%% =======================================================================================
