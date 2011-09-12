%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, James Aimonetti
%%% @doc
%%% Utilities to compact BigCouch clusters, nodes, and DBs
%%% @end
%%% Created :  8 Sep 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(couch_compactor).

-export([compact_db/2, compact_node/1, compact_all/0]).

-include("wh_couch.hrl").
-define(SLEEP_BETWEEN_COMPACTION, 60000). %% sleep 60 seconds between shard compactions
-define(SLEEP_BETWEEN_POLL, 5000). %% sleep 5 seconds before polling the shard for compaction status

-spec compact_all/0 :: () -> 'done'.
compact_all() ->
    ?LOG_SYS("Compacting all nodes"),
    {ok, Nodes} = couch_mgr:admin_all_docs(<<"nodes">>),
    _ = [ compact_node(wh_json:get_value(<<"id">>, Node)) || Node <- Nodes],
    done.

-spec compact_node/1 :: (Node) -> 'done' when
      Node :: atom() | binary().
compact_node(Node) when is_atom(Node) ->
    compact_node(wh_util:to_binary(Node));
compact_node(NodeBin) ->
    put(callid, NodeBin),
    ?LOG("Compacting node"),

    {Conn, AdminConn} = get_node_connections(NodeBin),
    {ok, DBs} = couch_util:db_info(Conn),
    ?LOG("Found ~b DBs to compact", [length(DBs)]),
    _ = [ compact_db(NodeBin, DB, Conn, AdminConn) || DB <- DBs ],
    done.

-spec compact_db/2 :: (Node, DB) -> 'done' when
      Node :: atom() | binary(),
      DB :: binary().
compact_db(Node, DB) when is_atom(Node) ->
    compact_db(wh_util:to_binary(Node), DB);
compact_db(NodeBin, DB) ->
    {Conn, AdminConn} = get_node_connections(NodeBin),
    ok = compact_db(NodeBin, DB, Conn, AdminConn),
    done.

%% Internal Functions
-spec compact_db/4 :: (NodeBin, DB, Conn, AdminConn) -> 'ok' when
      NodeBin :: binary(),
      DB :: binary(),
      Conn :: #server{},
      AdminConn :: #server{}.
compact_db(NodeBin, DB, Conn, AdminConn) ->
    DBEncoded = binary:replace(DB, <<"/">>, <<"%2f">>, [global]),
    put(callid, <<NodeBin/binary, "-", DBEncoded/binary>>),
    ?LOG("Starting DB compaction"),

    Shards = get_db_shards(AdminConn, DBEncoded),
    DesignDocs = get_db_design_docs(Conn, DBEncoded),
    _ = [ compact_shard(AdminConn, Shard, DesignDocs) || Shard <- Shards ],
    ok.

-spec compact_shard/3 :: (AdminConn, Shard, DesignDocs) -> 'ok' when
      AdminConn :: #server{},
      Shard :: binary(),
      DesignDocs :: [binary(),...].
compact_shard(AdminConn, Shard, DesignDocs) ->
    wait_for_compaction(AdminConn, Shard),
    ?LOG("Compacting shard ~s", [Shard]),
    couch_util:db_compact(AdminConn, Shard),
    wait_for_compaction(AdminConn, Shard),

    ?LOG("View cleanup"),
    couch_util:db_view_cleanup(AdminConn, Shard),

    ?LOG("Design cleanup"),
    _ = [ couch_util:design_compact(AdminConn, Shard, Design) || Design <- DesignDocs ],
    ok = timer:sleep(?SLEEP_BETWEEN_COMPACTION),
    ok.

-spec wait_for_compaction/2 :: (AdminConn, Shard) -> 'ok' when
      AdminConn :: #server{},
      Shard :: binary().
wait_for_compaction(AdminConn, Shard) ->
    {ok, ShardData} = couch_util:db_info(AdminConn, Shard),
    case wh_json:is_true(<<"compact_running">>, ShardData, false) of
	true ->
	    ?LOG("Compaction running for shard"),
	    ok = timer:sleep(?SLEEP_BETWEEN_POLL),
	    wait_for_compaction(AdminConn, Shard);
	false ->
	    ?LOG("Compaction is not running for shard"),
	    ok
    end.

-spec get_db_design_docs/2 :: (Conn, DB) -> [binary(),...] | [] when
      Conn :: #server{},
      DB :: binary().
get_db_design_docs(Conn, DBEncoded) ->
    {ok, Designs} = couch_util:all_design_docs(Conn, DBEncoded, []),
    [ binary:replace(wh_json:get_value(<<"id">>, Design), <<"_design/">>, <<>>, [global]) || Design <- Designs ].

-spec get_db_shards/2 :: (AdminConn, DBEncoded) -> [binary(),...] when
      AdminConn :: #server{},
      DBEncoded :: binary().
get_db_shards(AdminConn, DBEncoded) ->
    {ok, Shards} = couch_util:db_info(AdminConn),
    [ ShardEncoded || Shard <- Shards, is_a_shard(ShardEncoded=binary:replace(Shard, <<"/">>, <<"%2f">>, [global]), DBEncoded) ].

-spec is_a_shard/2 :: (Shard, DB) -> boolean() when
      Shard :: binary(),
      DB :: binary().
is_a_shard(Shard, DB) ->
    binary:match(Shard, <<"%2f", DB/binary, ".">>) =/= nomatch.

-spec get_node_connections/1 :: (NodeBin) -> {#server{}, #server{}} when
      NodeBin :: binary().
get_node_connections(NodeBin) ->
    [_Name, H] = binary:split(NodeBin, <<"@">>),
    Host = wh_util:to_list(H),
    Node = wh_util:to_atom(NodeBin, true),
    ?LOG_SYS("Trying to contact host ~s (node ~s)", [Host, _Name]),

    {User,Pass} = couch_mgr:get_creds(),
    {Port,AdminPort} = get_ports(Node),

    get_conns(Host, Port, User, Pass, AdminPort).

-spec get_ports/1 :: (Node) -> {non_neg_integer(), non_neg_integer()} when
      Node :: atom().
-spec get_ports/2 :: (Node, Ping) -> {non_neg_integer(), non_neg_integer()} when
      Node :: atom(),
      Ping :: 'pong' | 'pang'.
get_ports(Node) ->
    Cookie = couch_mgr:get_node_cookie(),
    ?LOG_SYS("Using cookie ~s on node ~s", [Cookie, Node]),
    try
	erlang:set_cookie(Node, Cookie),
	get_ports(Node, net_adm:ping(Node))
    catch
	_:_ ->
	    {?DEFAULT_PORT, ?DEFAULT_ADMIN_PORT}
    end.

get_ports(Node, pong) ->
    ?LOG_SYS("Trying to find ports from node ~s", [Node]),
    Port = case rpc:call(Node, couch_config, get, ["chttpd", "port"]) of
	       {badrpc, _} ->
		   ?LOG_SYS("Failed to get port from RPC"),
		   couch_mgr:get_port();
	       P ->
		   ?LOG_SYS("Got port ~s", [P]),
		   wh_util:to_integer(P)
	   end,
    AdminPort = case rpc:call(Node, couch_config, get, ["httpd", "port"]) of
		    {badrpc, _} ->
			?LOG_SYS("Failed to get admin port from RPC"),
			couch_mgr:get_admin_port();
		    AP ->
			?LOG_SYS("Got admin port ~s", [AP]),
			wh_util:to_integer(AP)
		end,
    {Port, AdminPort};
get_ports(_Node, pang) ->
    ?LOG_SYS("Using same ports as couch_mgr"),
    {couch_mgr:get_port(), couch_mgr:get_admin_port()}.

-spec get_conns/5 :: (Host, Port, User, Pass, AdminPort) -> {#server{}, #server{}} when
      Host :: string(),
      Port :: non_neg_integer(),
      User :: string(),
      Pass :: string(),
      AdminPort :: non_neg_integer().
get_conns(Host, Port, User, Pass, AdminPort) ->
    {couch_util:get_new_connection(Host, Port, User, Pass),
     couch_util:get_new_connection(Host, AdminPort, User, Pass)}.
