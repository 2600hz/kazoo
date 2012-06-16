%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Utilities to compact BigCouch clusters, nodes, and DBs
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(couch_compactor).

-export([start_link/0, init/1]).

-export([compact_all/0
         ,compact_node/1
         ,compact_db/1
         ,compact_db/2
        ]).

%% Conflict resolution-enabled API
-export([compact_all/1
         ,compact_all/2
         ,compact_node/2
         ,compact_node/3
         ,compact_db/3
         ,compact_db/4
        ]).

%% internal API functions
-export([compact_shard/3]).

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

    case {couch_config:fetch(<<"compact_automatically">>, false), couch_config:fetch(<<"conflict_strategy">>, null)} of
        {true, null} ->
            lager:debug("just compacting"),
            proc_lib:init_ack(Parent, {ok, self()}),
            compact_all();
        {true, Strategy} ->
            lager:debug("compacting and removing conflicts"),
            proc_lib:init_ack(Parent, {ok, self()}),
            compact_all(Strategy);
        {false, _Strategy} ->
            lager:debug("auto-compaction not enabled"),
            proc_lib:init_ack(Parent, ignore);
        {null, _Strategy} ->
            lager:debug("auto-compaction not enabled"),
            proc_lib:init_ack(Parent, ignore),
            couch_config:store(<<"compact_automatically">>, false);
        _Other ->
            lager:debug("unexpected values for auto-compaction: ~p", [_Other])
    end.

-spec compact_all/0 :: () -> 'done'.
-spec compact_all/1 :: (couch_conflict:resolution_strategy()) -> 'done'.
-spec compact_all/2 :: (couch_conflict:resolution_strategy(), couch_conflict:merge_fun()) -> 'done'.
compact_all() ->
    lager:debug("compacting all nodes"),
    {ok, Nodes} = couch_mgr:admin_all_docs(<<"nodes">>),
    _ = [ compact_node(wh_json:get_value(<<"id">>, Node)) || Node <- shuffle(Nodes)],
    done.
compact_all(ConflictStrategy) ->
    lager:debug("compacting all nodes"),
    {ok, Nodes} = couch_mgr:admin_all_docs(<<"nodes">>),
    _ = [ compact_node(wh_json:get_value(<<"id">>, Node), ConflictStrategy) || Node <- shuffle(Nodes)],
    done.
compact_all(ConflictStrategy, F) ->
    lager:debug("compacting all nodes"),
    {ok, Nodes} = couch_mgr:admin_all_docs(<<"nodes">>),
    _ = [ compact_node(wh_json:get_value(<<"id">>, Node), ConflictStrategy, F) || Node <- shuffle(Nodes)],
    done.

-spec compact_node/1 :: (ne_binary() | atom()) -> 'done'.
-spec compact_node/2 :: (ne_binary() | atom(), couch_conflict:resolution_strategy()) -> 'done'.
-spec compact_node/3 :: (ne_binary() | atom(), couch_conflict:resolution_strategy(), couch_conflict:merge_fun()) -> 'done'.
compact_node(Node) when is_atom(Node) ->
    compact_node(wh_util:to_binary(Node));
compact_node(NodeBin) ->
    put(callid, NodeBin),
    lager:debug("compacting node ~s", [NodeBin]),
    {Conn, AdminConn} = get_node_connections(NodeBin),
    {ok, DBs} = couch_util:db_info(Conn),
    Total = length(DBs),
    _ = lists:foldl(fun(DB, Count) ->
                            lager:debug("compacting database (~p/~p) '~s'", [Count, Total, DB]),
                            _ = (catch compact_node_db(NodeBin, DB, Conn, AdminConn)),
                            Count + 1
                    end, 1, shuffle(DBs, Total)),
    done.

compact_node(Node, ConflictStrategy) when is_atom(Node) ->
    compact_node(wh_util:to_binary(Node), ConflictStrategy);
compact_node(NodeBin, ConflictStrategy) ->
    put(callid, NodeBin),
    lager:debug("compacting node ~s", [NodeBin]),

    {Conn, _AdminConn} = get_node_connections(NodeBin),
    {ok, DBs} = couch_util:db_info(Conn),
    lager:debug("found ~b DBs to compact", [length(DBs)]),

    _ = [ compact_db(NodeBin, DB, ConflictStrategy) || DB <- DBs ],
    done.
compact_node(Node, ConflictStrategy, F) when is_atom(Node) ->
    compact_node(wh_util:to_binary(Node), ConflictStrategy, F);
compact_node(NodeBin, ConflictStrategy, F) ->
    put(callid, NodeBin),
    lager:debug("compacting node ~s", [NodeBin]),

    {Conn, _AdminConn} = get_node_connections(NodeBin),
    {ok, DBs} = couch_util:db_info(Conn),
    lager:debug("found ~b DBs to compact", [length(DBs)]),

    _ = [ compact_db(NodeBin, DB, ConflictStrategy, F) || DB <- DBs ],
    done.

%% Use compact_db/1 to compact the DB across all known nodes
%% Use compact_db/2 to compact the DB on a specific node
-spec compact_db/1 :: (ne_binary()) -> 'done'.
-spec compact_db/2 :: (ne_binary() | atom(), ne_binary()) -> 'done'.
-spec compact_db/3 :: (ne_binary() | atom(), ne_binary(), couch_conflict:resolution_strategy()) -> 'done'.
-spec compact_db/4 :: (ne_binary() | atom(), ne_binary(), couch_conflict:resolution_strategy(), couch_conflict:merge_fun()) -> 'done'.

compact_db(DB) ->
    {ok, Nodes} = couch_mgr:admin_all_docs(<<"nodes">>),
    _ = [ compact_db(wh_json:get_value(<<"id">>, Node), DB) || Node <- Nodes],
    'done'.
compact_db(Node, DB) when is_atom(Node) ->
    compact_db(wh_util:to_binary(Node), DB);
compact_db(NodeBin, DB) ->
    put(callid, NodeBin),
    {Conn, AdminConn} = get_node_connections(NodeBin),
    ok = compact_node_db(NodeBin, DB, Conn, AdminConn),
    done.

compact_db(Node, DB, ConflictStrategy) when is_atom(Node) ->
    compact_db(wh_util:to_binary(Node), DB, ConflictStrategy);
compact_db(NodeBin, DB, ConflictStrategy) ->
    put(callid, NodeBin),
    {Conn, AdminConn} = get_node_connections(NodeBin),

    _ = couch_conflict:resolve(Conn, DB, couch_conflict:default_view(), ConflictStrategy),

    ok = compact_node_db(NodeBin, DB, Conn, AdminConn),
    done.

compact_db(Node, DB, ConflictStrategy, F) when is_atom(Node) ->
    compact_db(wh_util:to_binary(Node), DB, ConflictStrategy, F);
compact_db(NodeBin, DB, ConflictStrategy, F) ->
    put(callid, NodeBin),
    {Conn, AdminConn} = get_node_connections(NodeBin),

    _ = couch_conflict:resolve(Conn, DB, couch_conflict:default_view(), ConflictStrategy, F),

    ok = compact_node_db(NodeBin, DB, Conn, AdminConn),
    done.

%% Internal Functions ----------------------------------------------------------
-spec compact_node_db/4 :: (ne_binary(), ne_binary(), #server{}, #server{}) -> 'ok'.
compact_node_db(NodeBin, DB, Conn, AdminConn) ->
    DBEncoded = binary:replace(DB, <<"/">>, <<"%2f">>, [global]),
    lager:debug("compacting db ~s node ~s", [DBEncoded, NodeBin]),
    case couch_util:db_exists(Conn, DBEncoded) andalso get_db_shards(AdminConn, DBEncoded) of
        false -> lager:debug("db ~s not on node ~s", [DBEncoded, NodeBin]);
        [] -> lager:debug("no shards found matching ~s", [DBEncoded]);
        Shards ->
            DesignDocs = try get_db_design_docs(Conn, DBEncoded)
                         catch _:_ -> []
                         end,
            compact_node_shards(Shards, AdminConn, DesignDocs),
            ok
    end.

-spec compact_node_shards/3 :: ([ne_binary(),...], server(), [ne_binary(),...] | []) -> 'ok'. 
compact_node_shards(Shards, AdminConn, DesignDocs) ->
    case catch(lists:split(?MAX_COMPACTING_SHARDS, Shards)) of
        {'EXIT', _} -> 
            compact_shards(Shards, AdminConn, DesignDocs),
            ok;
        {Compact, Remaining} ->
            compact_shards(Compact, AdminConn, DesignDocs),
            compact_node_shards(Remaining, AdminConn, DesignDocs)
    end.             

-spec compact_shards/3 :: ([ne_binary(),...], server(), [ne_binary(),...] | []) -> 'ok'. 
compact_shards(Shards, AdminConn, DesignDocs) ->
    Pids = [spawn_monitor(?MODULE, compact_shard, [AdminConn, Shard, DesignDocs])
            || Shard <- Shards
           ],

    MaxWait = wh_util:to_integer(
                couch_config:fetch(<<"max_wait_for_compaction_pid">>, ?MAX_WAIT_FOR_COMPACTION_PID)
               ),

    _ = [receive {'DOWN', Ref, process, Pid, _} -> ok
         after MaxWait ->
                 lager:debug("tired of waiting on ~p(~p), moving on", [Pid, Ref])
         end || {Pid,Ref} <- Pids
        ],
    ok = timer:sleep(couch_config:fetch(<<"sleep_between_compaction">>, ?SLEEP_BETWEEN_COMPACTION)).

-spec compact_shard/3 :: (server(), ne_binary(), [ne_binary(),...] | []) -> 'ok'.
compact_shard(AdminConn, Shard, DesignDocs) ->
    put(callid, ?LOG_SYSTEM_ID),
    lager:debug("compacting shard ~s", [Shard]),
    wait_for_compaction(AdminConn, Shard),
    couch_util:db_compact(AdminConn, Shard),
    wait_for_compaction(AdminConn, Shard),

    lager:debug("cleanup views in shard ~s", [Shard]),
    couch_util:db_view_cleanup(AdminConn, Shard),
    wait_for_compaction(AdminConn, Shard),

    lager:debug("cleanup design docs in shard ~s", [Shard]),
    compact_design_docs(AdminConn, Shard, DesignDocs),
    wait_for_compaction(AdminConn, Shard),

    lager:debug("finished compacting shard ~s", [Shard]).

-spec compact_design_docs/3 :: (server(), ne_binary(), [ne_binary(),...] | []) -> 'ok'.
compact_design_docs(AdminConn, Shard, DesignDocs) ->
    case catch(lists:split(?MAX_COMPACTING_VIEWS, DesignDocs)) of
        {'EXIT', _} -> 
            _ = [begin
                     lager:debug("cleanup design doc ~s in shard ~s", [Design, Shard]),
                     couch_util:design_compact(AdminConn, Shard, Design) 
                 end
                  || Design <- DesignDocs
                ],
            ok = timer:sleep(couch_config:fetch(<<"sleep_between_views">>, ?SLEEP_BETWEEN_VIEWS)),
            wait_for_compaction(AdminConn, Shard),
            ok;
        {Compact, Remaining} ->
            _ = [begin
                     lager:debug("cleanup design doc ~s in shard ~s", [Design, Shard]),
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
                    lager:debug("waiting for compaction of shard ~s", [Shard]),
                    ok = timer:sleep(couch_config:fetch(<<"sleep_between_poll">>, ?SLEEP_BETWEEN_POLL)),
                    wait_for_compaction(AdminConn, Shard)
            end;
        {error, db_not_found} ->
            lager:debug("db shard ~s not found, skipping", [Shard]);
        {error, _E} ->
            lager:debug("failed to query shard for compaction status: ~p", [_E]),
            ok = timer:sleep(couch_config:fetch(<<"sleep_between_poll">>, ?SLEEP_BETWEEN_POLL)),
            wait_for_compaction(AdminConn, Shard)
    end.

-spec get_db_design_docs/2 :: (#server{}, ne_binary()) -> [ne_binary(),...] | [].
get_db_design_docs(Conn, DBEncoded) ->
    {ok, Designs} = couch_util:all_design_docs(Conn, DBEncoded, []),
    [ binary:replace(wh_json:get_value(<<"id">>, Design), <<"_design/">>, <<>>, [global]) || Design <- Designs ].

-spec get_db_shards/2 :: (#server{}, ne_binary()) -> [ne_binary()].
get_db_shards(AdminConn, DBEncoded) ->
    case couch_config:fetch(DBEncoded, undefined, ?WH_COUCH_CACHE) of
        undefined ->
            case couch_util:db_info(AdminConn) of
                {ok, []} -> lager:debug("no shards found on admin conn? That's odd"), [];
                {ok, Shards} ->
                    Encoded = [ ShardEncoded || Shard <- Shards, is_a_shard(ShardEncoded=binary:replace(Shard, <<"/">>, <<"%2f">>, [global]), DBEncoded) ],
                    couch_config:store(DBEncoded, Encoded, ?WH_COUCH_CACHE),
                    lager:debug("cached encoded shards for ~s", [DBEncoded]),
                    Encoded;
                {error, _E} ->
                    %% TODO: check this error for terminal cases
                    lager:debug("unable to get shards atm: ~p", [_E]),
                    ok = timer:sleep(couch_config:fetch(<<"sleep_between_poll">>, ?SLEEP_BETWEEN_POLL)),
                    get_db_shards(AdminConn, DBEncoded)
            end;
        Encoded ->
            lager:debug("pulled encoded shards from cache for ~s", [DBEncoded]),
            Encoded
    end.

-spec is_a_shard/2 :: (ne_binary(), ne_binary()) -> boolean().
is_a_shard(Shard, DB) ->
    binary:match(Shard, <<"%2f", DB/binary, ".">>) =/= nomatch.

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
     couch_util:get_new_connection(Host, AdminPort, User, Pass)}.

-spec shuffle/1 :: (list()) -> list().
-spec shuffle/2 :: (list(), integer()) -> list().
shuffle(List) ->
    shuffle(List, length(List)).
shuffle(List, Len) ->
    randomize(round(math:log(Len) + 0.5), List).

-spec randomize/2 :: (pos_integer(), list()) -> list().
randomize(1, List) ->
    randomize(List);
randomize(T, List) ->
    lists:foldl(fun(_E, Acc) ->
                        randomize(Acc)
                end, randomize(List), lists:seq(1, (T - 1))).

-spec randomize/1 :: (list()) -> list().
randomize(List) ->
    D = lists:keysort(1, [{random:uniform(), A} || A <- List]),
    {_, D1} = lists:unzip(D),
    D1.
