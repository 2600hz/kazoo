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

-export([compact_all/0, compact_node/1, compact_db/1, compact_db/2]).

%% Conflict resolution-enabled API
-export([compact_all/1, compact_all/2, compact_node/2, compact_node/3
         ,compact_db/3, compact_db/4]).

-include_lib("wh_couch.hrl").
-define(SLEEP_BETWEEN_COMPACTION, 60000). %% sleep 60 seconds between shard compactions
-define(SLEEP_BETWEEN_POLL, 5000). %% sleep 5 seconds before polling the shard for compaction status

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
    _ = [ compact_node(wh_json:get_value(<<"id">>, Node)) || Node <- Nodes],
    done.
compact_all(ConflictStrategy) ->
    lager:debug("compacting all nodes"),
    {ok, Nodes} = couch_mgr:admin_all_docs(<<"nodes">>),
    _ = [ compact_node(wh_json:get_value(<<"id">>, Node), ConflictStrategy) || Node <- Nodes],
    done.
compact_all(ConflictStrategy, F) ->
    lager:debug("compacting all nodes"),
    {ok, Nodes} = couch_mgr:admin_all_docs(<<"nodes">>),
    _ = [ compact_node(wh_json:get_value(<<"id">>, Node), ConflictStrategy, F) || Node <- Nodes],
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
    lager:debug("found ~b DBs to compact", [length(DBs)]),
    _ = [ compact_node_db(NodeBin, DB, Conn, AdminConn) || DB <- DBs ],
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
-spec compact_node_db/4 :: (ne_binary(), ne_binary(), server(), server()) -> 'ok'.
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
            _ = [ catch(compact_shard(AdminConn, Shard, DesignDocs)) || Shard <- Shards ],
            ok
    end.

-spec compact_shard/3 :: (server(), ne_binary(), [ne_binary(),...] | []) -> 'ok'.
compact_shard(AdminConn, Shard, DesignDocs) ->
    wait_for_compaction(AdminConn, Shard),
    lager:debug("compacting shard ~s", [Shard]),
    couch_util:db_compact(AdminConn, Shard),
    wait_for_compaction(AdminConn, Shard),

    lager:debug("view cleanup"),
    couch_util:db_view_cleanup(AdminConn, Shard),

    lager:debug("design cleanup"),
    _ = [ couch_util:design_compact(AdminConn, Shard, Design) || Design <- DesignDocs ],
    ok = timer:sleep(couch_config:fetch(<<"sleep_between_compaction">>, ?SLEEP_BETWEEN_COMPACTION)),
    ok.

-spec wait_for_compaction/2 :: (server(), ne_binary()) -> 'ok'.
wait_for_compaction(AdminConn, Shard) ->
    case couch_util:db_info(AdminConn, Shard) of
        {ok, ShardData} ->
            case wh_json:is_true(<<"compact_running">>, ShardData, false) of
                true ->
                    lager:debug("compaction running for shard"),
                    ok = timer:sleep(couch_config:fetch(<<"sleep_between_poll">>, ?SLEEP_BETWEEN_POLL)),
                    wait_for_compaction(AdminConn, Shard);
                false ->
                    lager:debug("compaction is not running for shard")
            end;
        {error, db_not_found} ->
            lager:debug("db shard ~s not found, skipping", [Shard]);
        {error, _E} ->
            lager:debug("failed to query shard for compaction status: ~p", [_E]),
            ok = timer:sleep(couch_config:fetch(<<"sleep_between_poll">>, ?SLEEP_BETWEEN_POLL)),
            wait_for_compaction(AdminConn, Shard)
    end.

-spec get_db_design_docs/2 :: (server(), ne_binary()) -> [ne_binary(),...] | [].
get_db_design_docs(Conn, DBEncoded) ->
    {ok, Designs} = couch_util:all_design_docs(Conn, DBEncoded, []),
    [ binary:replace(wh_json:get_value(<<"id">>, Design), <<"_design/">>, <<>>, [global])
      || Design <- Designs
    ].

-spec get_db_shards/2 :: (server(), ne_binary()) -> [ne_binary()].
get_db_shards(AdminConn, DBEncoded) ->
    case couch_config:fetch(DBEncoded, undefined, ?WH_COUCH_CACHE) of
        undefined ->
            case couch_util:db_info(AdminConn) of
                {ok, []} -> lager:debug("no shards found on admin conn? That's odd"), [];
                {ok, Shards} ->
                    Encoded = [ ShardEncoded
                                || Shard <- Shards,
                                   begin
                                       ShardEncoded = binary:replace(Shard, <<"/">>, <<"%2f">>, [global]),
                                       is_a_shard(ShardEncoded, DBEncoded)
                                   end
                              ],
                    _ = couch_config:store(DBEncoded, Encoded, ?WH_COUCH_CACHE),
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

-spec get_node_connections/1 :: (ne_binary()) -> {server(), server()}.
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

-spec get_conns/5 :: (nonempty_string(), pos_integer(), string(), string(), pos_integer()) -> {server(), server()}.
get_conns(Host, Port, User, Pass, AdminPort) ->
    {couch_util:get_new_connection(Host, Port, User, Pass),
     couch_util:get_new_connection(Host, AdminPort, User, Pass)
    }.
