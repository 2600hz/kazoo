%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Utilities to compact BigCouch clusters, nodes, and DBs
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(couch_compactor).

-export([start_link/0
         ,init/1
        ]).

-export([is_compactor_running/0
         ,status/0
        ]).

-export([compact_all/0
         ,compact_node/1
         ,compact_db/1
         ,compact_db/2
        ]).

%% internal API functions
-export([compact_shard/4]).

-include_lib("whistle_couch/include/wh_couch.hrl").
-define(SLEEP_BETWEEN_COMPACTION, 60000).
-define(SLEEP_BETWEEN_POLL, 1000).
-define(MAX_COMPACTING_SHARDS, 10).
-define(MAX_COMPACTING_VIEWS, 5).
-define(SLEEP_BETWEEN_VIEWS, 2000).
-define(MAX_WAIT_FOR_COMPACTION_PID, 360000). % five minutes

start_link() ->
    proc_lib:start_link(?MODULE, init, [self()], infinity, []).

init(Parent) ->
    put(callid, ?LOG_SYSTEM_ID),

    lager:debug("starting compactor"),

    case couch_config:fetch(<<"compact_automatically">>, false) of
        true ->
            lager:debug("just compacting"),
            proc_lib:init_ack(Parent, {ok, self()}),
            compact_all();
        false ->
            lager:debug("auto-compaction not enabled"),
            proc_lib:init_ack(Parent, ignore)
    end.

-spec is_compactor_running/0 :: () -> boolean().
is_compactor_running() ->
    P = whistle_couch_sup:compactor_pid(),
    is_pid(P) andalso is_process_alive(P).

-spec status/0 :: () -> {'ok', wh_json:json_strings()} |
                        {'error', 'not_running' | 'timeout'}.
status() ->
    case is_compactor_running() of
        false -> {'error', 'not_running'};
        true ->
            Self = self(),
            P = whistle_couch_sup:compactor_pid(),
            P ! {status, Self},
            receive
                Msg -> Msg
            after 5000 ->
                    {error, timeout}
            end
    end.

-spec compact_all/0 :: () -> 'done'.
compact_all() ->
    lager:debug("compacting all nodes"),
    {ok, Nodes} = couch_mgr:admin_all_docs(<<"nodes">>),
    _ = [ compact_node(wh_json:get_value(<<"id">>, Node)) || Node <- wh_util:shuffle_list(Nodes)],
    done.

-spec compact_node/1 :: (ne_binary() | atom()) -> 'done'.
compact_node(Node) when is_atom(Node) ->
    compact_node(wh_util:to_binary(Node));
compact_node(NodeBin) ->
    put(callid, NodeBin),
    lager:debug("compacting node ~s", [NodeBin]),
    try get_node_connections(NodeBin) of
        {Conn, AdminConn} ->
            {ok, DBs} = node_dbs(AdminConn),
            Total = length(DBs),
            _ = lists:foldl(fun(DB, Count) ->
                                    lager:debug("compacting database (~p/~p) '~s'", [Count, Total, DB]),
                                    _ = (catch compact_node_db(NodeBin, DB, Conn, AdminConn)),
                                    Count + 1
                            end, 1, wh_util:shuffle_list(DBs)),
            done
    catch
        _:{error,{conn_failed,{error,etimedout}}} ->
            _ = handle_conn_error(NodeBin, etimedout),
            done;
        _:E ->
            _ = handle_conn_error(NodeBin, E),
            done
    end.

-spec handle_conn_error/2 :: (ne_binary(), any()) -> any().
handle_conn_error(NodeBin, Err) ->
    case wh_cache:fetch_local(?WH_COUCH_CACHE, Err) of
        {ok, Cnt} when Cnt < 3 ->
            wh_cache:store_local(?WH_COUCH_CACHE, Err, Cnt);
        {ok, Cnt} ->
            lager:debug("connection error to ~s for the ~b time: ~p", [NodeBin, Cnt, Err]),
            lager:debug("turning compactor off for now"),
            _ = couch_config:store(<<"compact_automatically">>, false),

            lager:debug("alerting admins about the situation"),
            wh_notify:system_alert("Compactor failed to connect to db node ~s: ~p"
                                   ,[NodeBin, Err]
                                   ,[{<<"Attempts">>, Cnt}]
                                  ),
            wh_cache:store_local(?WH_COUCH_CACHE, Err, 0);
        {error, not_found} ->
            wh_cache:store_local(?WH_COUCH_CACHE, Err, 1)
    end.

%% Use compact_db/1 to compact the DB across all known nodes
%% Use compact_db/2 to compact the DB on a specific node
-spec compact_db/1 :: (ne_binary()) -> 'done'.
-spec compact_db/2 :: (ne_binary() | atom(), ne_binary()) -> 'done'.
compact_db(DB) ->
    {ok, Nodes} = couch_mgr:admin_all_docs(<<"nodes">>),
    _ = [ compact_db(wh_json:get_value(<<"id">>, Node), DB) || Node <- Nodes],
    'done'.
compact_db(Node, DB) when is_atom(Node) ->
    compact_db(wh_util:to_binary(Node), DB);
compact_db(NodeBin, DB) ->
    put(callid, NodeBin),
    try get_node_connections(NodeBin) of
        {Conn, AdminConn} ->
            ok = compact_node_db(NodeBin, DB, Conn, AdminConn),
            done
    catch
        _:_ ->
            lager:error("unable to open connection to ~s", [NodeBin]),
            done
    end.

%% Internal Functions ----------------------------------------------------------
-spec compact_node_db/4 :: (ne_binary(), ne_binary(), #server{}, #server{}) -> 'ok'.
compact_node_db(NodeBin, DB, Conn, AdminConn) ->
    DBEncoded = binary:replace(DB, <<"/">>, <<"%2f">>, [global]),
    lager:debug("compacting db ~s node ~s", [DBEncoded, NodeBin]),

    case couch_util:db_exists(Conn, DBEncoded) andalso get_db_shards(NodeBin, AdminConn, DB, DBEncoded) of
        false -> lager:debug("db ~s not on node ~s", [DBEncoded, NodeBin]);
        [] -> lager:debug("no shards found matching ~s", [DBEncoded]);
        Shards ->
            DesignDocs = try get_db_design_docs(Conn, DBEncoded)
                         catch _:_ -> []
                         end,
            compact_node_shards(NodeBin, Shards, AdminConn, DesignDocs),
            ok
    end.

-spec compact_node_shards/4 :: (ne_binary(), [ne_binary(),...], server(), wh_json:json_strings()) -> 'ok'.
compact_node_shards(NodeBin, Shards, AdminConn, DesignDocs) ->
    case catch(lists:split(?MAX_COMPACTING_SHARDS, Shards)) of
        {'EXIT', _} ->
            _ = compact_shards(NodeBin, Shards, AdminConn, DesignDocs),
            wait_for_inter_compaction_timeout(NodeBin),
            ok;
        {Compact, Remaining} ->
            _ = compact_shards(NodeBin, Compact, AdminConn, DesignDocs),
            wait_for_inter_compaction_timeout(NodeBin),
            compact_node_shards(NodeBin, Remaining, AdminConn, DesignDocs)
    end.

wait_for_inter_compaction_timeout(NodeBin) ->
    wait_for_inter_compaction_timeout(
      NodeBin
      ,couch_config:fetch(<<"sleep_between_compaction">>, ?SLEEP_BETWEEN_COMPACTION)
     ).
wait_for_inter_compaction_timeout(NodeBin, Timeout) ->
    Start = erlang:now(),
    receive
        {status, Srv} ->
            Srv ! [{status, inter_compaction_timeout, Timeout}, {node, NodeBin}],
            wait_for_inter_compaction_timeout(NodeBin, Timeout - wh_util:elapsed_ms(Start))
    after Timeout ->
            ok
    end.

-spec compact_shards/4 :: (ne_binary(), [ne_binary(),...], server(), [ne_binary(),...] | []) -> any().
compact_shards(NodeBin, Shards, AdminConn, DesignDocs) ->
    Pids = [spawn_monitor(?MODULE, compact_shard, [NodeBin, AdminConn, Shard, DesignDocs])
            || Shard <- Shards
           ],

    MaxWait = wh_util:to_integer(
                couch_config:fetch(<<"max_wait_for_compaction_pid">>, ?MAX_WAIT_FOR_COMPACTION_PID)
               ),

    [receive {'DOWN', Ref, process, Pid, _} -> ok;
             {status, Srv} -> Srv ! [{node, NodeBin}, {shards, Shards}]
     after MaxWait ->
             lager:debug("tired of waiting on ~p(~p), moving on", [Pid, Ref])
     end || {Pid,Ref} <- Pids
    ].

-spec compact_shard/4 :: (ne_binary(), server(), ne_binary(), [ne_binary(),...] | []) -> 'ok'.
compact_shard(NodeBin, AdminConn, Shard, DesignDocs) ->
    put(callid, NodeBin),
    lager:debug("compacting shard ~s", [Shard]),
    wait_for_compaction(AdminConn, Shard),
    couch_util:db_compact(AdminConn, Shard),
    wait_for_compaction(AdminConn, Shard),

    lager:debug("cleanup views"),
    couch_util:db_view_cleanup(AdminConn, Shard),
    wait_for_compaction(AdminConn, Shard),

    lager:debug("cleanup design docs"),
    compact_design_docs(AdminConn, Shard, DesignDocs),
    wait_for_compaction(AdminConn, Shard),

    lager:debug("finished compacting shard").

-spec compact_design_docs/3 :: (server(), ne_binary(), [ne_binary(),...] | []) -> 'ok'.
compact_design_docs(AdminConn, Shard, DesignDocs) ->
    case catch(lists:split(?MAX_COMPACTING_VIEWS, DesignDocs)) of
        {'EXIT', _} ->
            _ = [begin
                     lager:debug("cleanup design doc ~s", [Design]),
                     couch_util:design_compact(AdminConn, Shard, Design)
                 end
                  || Design <- DesignDocs
                ],
            ok = timer:sleep(couch_config:fetch(<<"sleep_between_views">>, ?SLEEP_BETWEEN_VIEWS)),
            wait_for_compaction(AdminConn, Shard),
            ok;
        {Compact, Remaining} ->
            _ = [begin
                     lager:debug("cleanup design doc ~s", [Design]),
                     couch_util:design_compact(AdminConn, Shard, Design)
                 end
                  || Design <- Compact
                ],
            ok = timer:sleep(couch_config:fetch(<<"sleep_between_views">>, ?SLEEP_BETWEEN_VIEWS)),
            wait_for_compaction(AdminConn, Shard),
            compact_design_docs(AdminConn, Shard, Remaining)
    end.

-spec wait_for_compaction/2 :: (server(), ne_binary()) -> 'ok'.
wait_for_compaction(AdminConn, Shard) ->
    ok = timer:sleep(1500),
    case couch_util:db_info(AdminConn, Shard) of
        {ok, ShardData} ->
            case wh_json:is_true(<<"compact_running">>, ShardData, false) of
                false -> ok;
                true ->
                    lager:debug("waiting for compaction to finish"),
                    ok = timer:sleep(couch_config:fetch(<<"sleep_between_poll">>, ?SLEEP_BETWEEN_POLL)),
                    wait_for_compaction(AdminConn, Shard)
            end;
        {error, db_not_found} ->
            lager:debug("shard not found, skipping");
        {error, _E} ->
            lager:debug("failed to query shard for compaction status: ~p", [_E]),
            ok = timer:sleep(couch_config:fetch(<<"sleep_between_poll">>, ?SLEEP_BETWEEN_POLL)),
            wait_for_compaction(AdminConn, Shard)
    end.

-spec get_db_design_docs/2 :: (#server{}, ne_binary()) -> [ne_binary(),...] | [].
get_db_design_docs(Conn, DBEncoded) ->
    {ok, Designs} = couch_util:all_design_docs(Conn, DBEncoded, []),
    [ binary:replace(wh_json:get_value(<<"id">>, Design), <<"_design/">>, <<>>, [global]) || Design <- Designs ].

-spec get_db_shards/4 :: (ne_binary(), #server{}, ne_binary(), ne_binary()) -> [ne_binary()].
get_db_shards(NodeBin, AdminConn, Db, DBEncoded) ->
    case couch_util:open_cache_doc(AdminConn, <<"dbs">>, Db, []) of
        {ok, Doc} ->
            Suffix = wh_json:get_value(<<"shard_suffix">>, Doc),
            Ranges = wh_json:get_value([<<"by_node">>, NodeBin], Doc, []),
            [<<"shards%2f", Range/binary, "%2f", DBEncoded/binary, (wh_util:to_binary(Suffix))/binary>>
                || Range <- Ranges
            ];
        {error, _E} ->
            %% TODO: check this error for terminal cases
            lager:debug("unable to get shards for ~s atm: ~p", [DBEncoded, _E]),
            ok = timer:sleep(couch_config:fetch(<<"sleep_between_poll">>, ?SLEEP_BETWEEN_POLL)),
            get_db_shards(NodeBin, AdminConn, Db, DBEncoded)
    end.

-spec get_node_connections/1 :: (ne_binary()) -> {#server{}, #server{}}.
get_node_connections(NodeBin) ->
    [_Name, H] = binary:split(NodeBin, <<"@">>),
    Host = wh_util:to_list(H),
    Node = wh_util:to_atom(NodeBin, true),
    lager:debug("trying to contact host ~s (node ~s)", [Host, _Name]),

    {User,Pass} = couch_mgr:get_creds(),
    {Port,AdminPort} = get_ports(Node),

    get_conns(Host, Port, User, Pass, AdminPort).

-spec get_ports/1 :: (atom()) -> {non_neg_integer(), non_neg_integer()}.
-spec get_ports/2 :: (atom(), 'pong' | 'pang') -> {non_neg_integer(), non_neg_integer()}.
get_ports(Node) ->
    Cookie = couch_config:fetch(<<"bigcouch_cookie">>),
    lager:debug("using cookie ~s on node ~s", [Cookie, Node]),
    try
        erlang:set_cookie(Node, wh_util:to_atom(Cookie, true)),
        get_ports(Node, net_adm:ping(Node))
    catch
        _:_R ->
            lager:debug("failed to get the ports for ~s: ~p", [Node, _R]),
            {?DEFAULT_PORT, ?DEFAULT_ADMIN_PORT}
    end.

get_ports(Node, pong) ->
    lager:debug("trying to find ports from node ~s", [Node]),
    Port = case rpc:call(Node, couch_config, get, ["chttpd", "port"]) of
               {badrpc, _} ->
                   lager:debug("failed to get port from RPC"),
                   couch_mgr:get_port();
               P ->
                   lager:debug("got port ~s", [P]),
                   wh_util:to_integer(P)
           end,
    AdminPort = case rpc:call(Node, couch_config, get, ["httpd", "port"]) of
                    {badrpc, _} ->
                        lager:debug("failed to get admin port from RPC"),
                        couch_mgr:get_admin_port();
                    AP ->
                        lager:debug("got admin port ~s", [AP]),
                        wh_util:to_integer(AP)
                end,
    {Port, AdminPort};
get_ports(_Node, pang) ->
    lager:debug("using same ports as couch_mgr"),
    {couch_mgr:get_port(), couch_mgr:get_admin_port()}.

-spec get_conns/5 :: (nonempty_string(), pos_integer(), string(), string(), pos_integer()) -> {#server{}, #server{}}.
get_conns(Host, Port, User, Pass, AdminPort) ->
    {couch_util:get_new_connection(Host, Port, User, Pass),
     couch_util:get_new_connection(Host, AdminPort, User, Pass)
    }.

%% returns list of DBs in unencoded format
-spec node_dbs/1 :: (#server{}) -> {'ok', wh_json:json_strings()}.
node_dbs(AdminConn) ->
    {ok, Dbs} = couch_util:all_docs(AdminConn, <<"dbs">>, []),
    {ok, [wh_json:get_value(<<"id">>, Db) || Db <- Dbs]}.
