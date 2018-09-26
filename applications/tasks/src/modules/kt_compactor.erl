%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2018, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
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
        ]).

%% Triggerables
-export([help/1, help/2, help/3
        ,output_header/1
        ]).

-include("tasks.hrl").
-include("src/modules/kt_compactor.hrl").

-define(CATEGORY, "compaction").
-define(ACTIONS, [<<"compact_all">>
                 ,<<"compact_node">>
                 ,<<"compact_db">>
                 ]).

-define(OUTPUT_HEADER
       ,[<<"node">>, <<"database">>, <<"before_disk">>, <<"before_data">>, <<"after_disk">>, <<"after_data">>]
       ).

-type rows() :: [kz_csv:row()] | [].

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    kapps_maintenance:refresh_views(kazoo_couch:get_admin_nodes()),
    set_node_defaults(),

    case ?COMPACT_AUTOMATICALLY of
        'false' -> lager:info("node ~s not configured to compact automatically", [node()]);
        'true' ->
            lager:info("node ~s configured to compact automatically", [node()]),
            _ = tasks_bindings:bind(?TRIGGER_ALL_DBS, ?MODULE, 'compact_db')
    end,

    _ = tasks_bindings:bind(<<"tasks.help">>, ?MODULE, 'help'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".output_header">>, ?MODULE, 'output_header'),
    tasks_bindings:bind_actions(<<"tasks."?CATEGORY>>, ?MODULE, ?ACTIONS).

-spec set_node_defaults() -> 'ok'.
set_node_defaults() ->
    {'ok', Nodes} = kz_datamgr:all_docs(kazoo_couch:get_admin_nodes()),
    lists:foreach(fun set_node_defaults/1, Nodes).

-spec set_node_defaults(kz_json:object()) -> 'ok'.
set_node_defaults(NodeJObj) ->
    Node = kz_doc:id(NodeJObj),
    set_node_api_port(Node),
    set_node_admin_port(Node).

-spec set_node_api_port(kz_term:ne_binary()) -> 'ok'.
set_node_api_port(Node) ->
    case kapps_config:get_integer(?SYSCONFIG_COUCH, <<"api_port">>, 'undefined', Node) of
        'undefined' ->
            kapps_config:set_node(?SYSCONFIG_COUCH, <<"api_port">>, ?API_PORT, Node),
            lager:debug("api port for ~s set to default: ~p", [Node, ?API_PORT]);
        _Port ->
            lager:debug("api port for ~s already set: ~p", [Node, _Port])
    end.

-spec set_node_admin_port(kz_term:ne_binary()) -> 'ok'.
set_node_admin_port(Node) ->
    case kapps_config:get_integer(?SYSCONFIG_COUCH, <<"admin_port">>, 'undefined', Node) of
        'undefined' ->
            kapps_config:set_node(?SYSCONFIG_COUCH, <<"admin_port">>, ?ADMIN_PORT, Node),
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
    {do_compact_all(), 'stop'};
compact_all(_Extra, 'false') ->
    {<<"compaction is only allowed by system administrators">>, 'stop'}.

-spec compact_node(kz_tasks:extra_args(), kz_tasks:iterator(), kz_tasks:args()) -> kz_tasks:iterator().
compact_node(Extra, 'init', Args) ->
    compact_node(Extra, is_allowed(Extra), Args);
compact_node(_Extra, 'false', _Args) ->
    {<<"compaction is only allowed by system administrators">>, 'stop'};
compact_node(_Extra, 'true', #{<<"node">> := Node}=Row) ->
    {do_compact_node(Node, heuristic_from_flag(maps:get(<<"force">>, Row))), 'true'}.

-spec compact_db(kz_tasks:extra_args(), kz_tasks:iterator(), kz_tasks:args()) -> kz_tasks:iterator().
compact_db(Extra, 'init', Args) ->
    compact_db(Extra, is_allowed(Extra), Args);
compact_db(_Extra, 'false', _Args) ->
    {<<"compaction is only allowed by system administrators">>, 'stop'};
compact_db(_Extra, 'true', #{<<"database">> := Database}=Row) ->
    {do_compact_db(Database, heuristic_from_flag(maps:get(<<"force">>, Row))), 'true'}.

-spec compact_db(kz_term:ne_binary()) -> 'ok'.
compact_db(Database) ->
    Rows = do_compact_db(Database, ?HEUR_NONE),
    print_csv(Rows).

-spec print_csv(iolist()) -> 'ok'.
print_csv(Rows) ->
    io:format("~s~n", [kz_binary:join(?OUTPUT_HEADER)]),
    [io:format("~s~n", [kz_binary:join(Row)]) || Row <- Rows],
    'ok'.

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
    {'ok', Dbs} = kz_datamgr:db_info(),
    Shuffled = kz_term:shuffle_list(Dbs),
    lists:foldl(fun do_compact_db_fold/2, [], Shuffled).

-spec compact_node(kz_term:ne_binary()) -> 'ok'.
compact_node(Node) ->
    Rows = do_compact_node(Node, ?HEUR_NONE),
    print_csv(Rows).

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
            do_compact_node(Node, Heuristic, APIConn, AdminConn, NodeDBs)
    end.

-spec do_compact_node(kz_term:ne_binary(), heuristic(), kz_data:connection(), kz_data:connection(), kz_term:ne_binaries()) -> rows().
do_compact_node(Node, Heuristic, APIConn, AdminConn, Databases) ->
    lists:foldl(fun(Database, Acc) ->
                        do_compact_node_db(Node, Heuristic, APIConn, AdminConn, Database, Acc)
                end
               ,[]
               ,Databases
               ).

-spec do_compact_node_db(kz_term:ne_binary(), heuristic(), kz_data:connection(), kz_data:connection(), kz_term:ne_binary(), rows()) -> rows().
do_compact_node_db(Node, Heuristic, APIConn, AdminConn, Database, Acc) ->
    Compactor = node_compactor(Node, Heuristic, APIConn, AdminConn, Database),
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
    do_compact_db(Database, ?HEUR_RATIO).

-spec do_compact_db(kz_term:ne_binary(), heuristic()) -> rows().
do_compact_db(Database, Heuristic) ->
    do_compact_db_by_nodes(Database, Heuristic).

-spec do_compact_db_fold(kz_term:ne_binary(), rows()) -> rows().
do_compact_db_fold(Database, Rows) ->
    Rows ++ do_compact_db(Database).

-spec do_compact_db_by_nodes(kz_term:ne_binary(), heuristic()) -> rows().
do_compact_db_by_nodes(?MATCH_ACCOUNT_RAW(_)=AccountId, Heuristic) ->
    lager:info("formatting raw account id ~s", [AccountId]),
    do_compact_db_by_nodes(kz_util:format_account_id(AccountId, 'unencoded'), Heuristic);
do_compact_db_by_nodes(?MATCH_ACCOUNT_ENCODED(_)=AccountDb, Heuristic) ->
    lager:info("formatting unencoded account db ~s", [AccountDb]),
    do_compact_db_by_nodes(kz_util:format_account_id(AccountDb, 'unencoded'), Heuristic);
do_compact_db_by_nodes(?MATCH_MODB_SUFFIX_RAW(_AccountId, _Year, _Month)=MODB, Heuristic) ->
    lager:info("formatting raw modb ~s", [MODB]),
    do_compact_db_by_nodes(kz_util:format_account_modb(MODB, 'unencoded'), Heuristic);
do_compact_db_by_nodes(Database, Heuristic) ->
    lager:debug("opening in ~s: ~s", [kazoo_couch:get_admin_dbs(), Database]),
    {'ok', DbInfo} = kz_datamgr:open_doc(kazoo_couch:get_admin_dbs(), Database),
    kz_json:foldl(fun(Node, _, Rows) ->
                          do_compact_db_by_node(Node, Heuristic, Database, Rows)
                  end
                 ,[]
                 ,kz_json:get_json_value(<<"by_node">>, DbInfo)
                 ).

-spec do_compact_db_by_node(kz_term:ne_binary(), heuristic(), kz_term:ne_binary(), rows()) -> rows().
do_compact_db_by_node(Node, Heuristic, Database, Acc) ->
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
    C1 = couchbeam:server_connection(Hostname, NodeAPIPort, "", Options),
    C2 = couchbeam:server_connection(Hostname, NodeAdminPort, "", Options),

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
