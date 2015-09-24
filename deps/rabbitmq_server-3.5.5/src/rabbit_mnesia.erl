%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2007-2015 Pivotal Software, Inc.  All rights reserved.
%%

-module(rabbit_mnesia).

-export([init/0,
         join_cluster/2,
         reset/0,
         force_reset/0,
         update_cluster_nodes/1,
         change_cluster_node_type/1,
         forget_cluster_node/2,
         force_load_next_boot/0,

         status/0,
         is_clustered/0,
         on_running_node/1,
         is_process_alive/1,
         cluster_nodes/1,
         node_type/0,
         dir/0,
         cluster_status_from_mnesia/0,

         init_db_unchecked/2,
         copy_db/1,
         check_cluster_consistency/0,
         ensure_mnesia_dir/0,

         on_node_up/1,
         on_node_down/1
        ]).

%% Used internally in rpc calls
-export([node_info/0, remove_node_if_mnesia_running/1]).

-include("rabbit.hrl").

%%----------------------------------------------------------------------------

-ifdef(use_specs).

-export_type([node_type/0, cluster_status/0]).

-type(node_type() :: disc | ram).
-type(cluster_status() :: {[node()], [node()], [node()]}).

%% Main interface
-spec(init/0 :: () -> 'ok').
-spec(join_cluster/2 :: (node(), node_type())
                        -> 'ok' | {'ok', 'already_member'}).
-spec(reset/0 :: () -> 'ok').
-spec(force_reset/0 :: () -> 'ok').
-spec(update_cluster_nodes/1 :: (node()) -> 'ok').
-spec(change_cluster_node_type/1 :: (node_type()) -> 'ok').
-spec(forget_cluster_node/2 :: (node(), boolean()) -> 'ok').
-spec(force_load_next_boot/0 :: () -> 'ok').

%% Various queries to get the status of the db
-spec(status/0 :: () -> [{'nodes', [{node_type(), [node()]}]} |
                         {'running_nodes', [node()]} |
                         {'partitions', [{node(), [node()]}]}]).
-spec(is_clustered/0 :: () -> boolean()).
-spec(on_running_node/1 :: (pid()) -> boolean()).
-spec(is_process_alive/1 :: (pid()) -> boolean()).
-spec(cluster_nodes/1 :: ('all' | 'disc' | 'ram' | 'running') -> [node()]).
-spec(node_type/0 :: () -> node_type()).
-spec(dir/0 :: () -> file:filename()).
-spec(cluster_status_from_mnesia/0 :: () -> rabbit_types:ok_or_error2(
                                              cluster_status(), any())).

%% Operations on the db and utils, mainly used in `rabbit_upgrade' and `rabbit'
-spec(init_db_unchecked/2 :: ([node()], node_type()) -> 'ok').
-spec(copy_db/1 :: (file:filename()) ->  rabbit_types:ok_or_error(any())).
-spec(check_cluster_consistency/0 :: () -> 'ok').
-spec(ensure_mnesia_dir/0 :: () -> 'ok').

%% Hooks used in `rabbit_node_monitor'
-spec(on_node_up/1 :: (node()) -> 'ok').
-spec(on_node_down/1 :: (node()) -> 'ok').

-endif.

%%----------------------------------------------------------------------------
%% Main interface
%%----------------------------------------------------------------------------

init() ->
    ensure_mnesia_running(),
    ensure_mnesia_dir(),
    case is_virgin_node() of
        true  -> init_from_config();
        false -> NodeType = node_type(),
                 init_db_and_upgrade(cluster_nodes(all), NodeType,
                                     NodeType =:= ram)
    end,
    %% We intuitively expect the global name server to be synced when
    %% Mnesia is up. In fact that's not guaranteed to be the case -
    %% let's make it so.
    ok = rabbit_node_monitor:global_sync(),
    ok.

init_from_config() ->
    FindBadNodeNames = fun
        (Name, BadNames) when is_atom(Name) -> BadNames;
        (Name, BadNames)                    -> [Name | BadNames]
    end,
    {TryNodes, NodeType} =
        case application:get_env(rabbit, cluster_nodes) of
            {ok, {Nodes, Type} = Config}
            when is_list(Nodes) andalso (Type == disc orelse Type == ram) ->
                case lists:foldr(FindBadNodeNames, [], Nodes) of
                    []       -> Config;
                    BadNames -> e({invalid_cluster_node_names, BadNames})
                end;
            {ok, {_, BadType}} when BadType /= disc andalso BadType /= ram ->
                e({invalid_cluster_node_type, BadType});
            {ok, Nodes} when is_list(Nodes) ->
                %% The legacy syntax (a nodes list without the node
                %% type) is unsupported.
                case lists:foldr(FindBadNodeNames, [], Nodes) of
                    [] -> e(cluster_node_type_mandatory);
                    _  -> e(invalid_cluster_nodes_conf)
                end;
            {ok, _} ->
                e(invalid_cluster_nodes_conf)
        end,
    case TryNodes of
        [] -> init_db_and_upgrade([node()], disc, false);
        _  -> auto_cluster(TryNodes, NodeType)
    end.

auto_cluster(TryNodes, NodeType) ->
    case find_auto_cluster_node(nodes_excl_me(TryNodes)) of
        {ok, Node} ->
            rabbit_log:info("Node '~p' selected for auto-clustering~n", [Node]),
            {ok, {_, DiscNodes, _}} = discover_cluster0(Node),
            init_db_and_upgrade(DiscNodes, NodeType, true),
            rabbit_node_monitor:notify_joined_cluster();
        none ->
            rabbit_log:warning(
              "Could not find any node for auto-clustering from: ~p~n"
              "Starting blank node...~n", [TryNodes]),
            init_db_and_upgrade([node()], disc, false)
    end.

%% Make the node join a cluster. The node will be reset automatically
%% before we actually cluster it. The nodes provided will be used to
%% find out about the nodes in the cluster.
%%
%% This function will fail if:
%%
%%   * The node is currently the only disc node of its cluster
%%   * We can't connect to any of the nodes provided
%%   * The node is currently already clustered with the cluster of the nodes
%%     provided
%%
%% Note that we make no attempt to verify that the nodes provided are
%% all in the same cluster, we simply pick the first online node and
%% we cluster to its cluster.
join_cluster(DiscoveryNode, NodeType) ->
    ensure_mnesia_not_running(),
    ensure_mnesia_dir(),
    case is_only_clustered_disc_node() of
        true  -> e(clustering_only_disc_node);
        false -> ok
    end,
    {ClusterNodes, _, _} = discover_cluster([DiscoveryNode]),
    case me_in_nodes(ClusterNodes) of
        false ->
            case check_cluster_consistency(DiscoveryNode, false) of
                {ok, _} ->
                    %% reset the node. this simplifies things and it
                    %% will be needed in this case - we're joining a new
                    %% cluster with new nodes which are not in synch
                    %% with the current node. It also lifts the burden
                    %% of resetting the node from the user.
                    reset_gracefully(),

                    %% Join the cluster
                    rabbit_log:info("Clustering with ~p as ~p node~n",
                                    [ClusterNodes, NodeType]),
                    ok = init_db_with_mnesia(ClusterNodes, NodeType,
                                             true, true),
                    rabbit_node_monitor:notify_joined_cluster(),
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end;
        true ->
            rabbit_log:info("Already member of cluster: ~p~n", [ClusterNodes]),
            {ok, already_member}
    end.

%% return node to its virgin state, where it is not member of any
%% cluster, has no cluster configuration, no local database, and no
%% persisted messages
reset() ->
    ensure_mnesia_not_running(),
    rabbit_log:info("Resetting Rabbit~n", []),
    reset_gracefully().

force_reset() ->
    ensure_mnesia_not_running(),
    rabbit_log:info("Resetting Rabbit forcefully~n", []),
    wipe().

reset_gracefully() ->
    AllNodes = cluster_nodes(all),
    %% Reconnecting so that we will get an up to date nodes.  We don't
    %% need to check for consistency because we are resetting.
    %% Force=true here so that reset still works when clustered with a
    %% node which is down.
    init_db_with_mnesia(AllNodes, node_type(), false, false),
    case is_only_clustered_disc_node() of
        true  -> e(resetting_only_disc_node);
        false -> ok
    end,
    leave_cluster(),
    rabbit_misc:ensure_ok(mnesia:delete_schema([node()]), cannot_delete_schema),
    wipe().

wipe() ->
    %% We need to make sure that we don't end up in a distributed
    %% Erlang system with nodes while not being in an Mnesia cluster
    %% with them. We don't handle that well.
    [erlang:disconnect_node(N) || N <- cluster_nodes(all)],
    %% remove persisted messages and any other garbage we find
    ok = rabbit_file:recursive_delete(filelib:wildcard(dir() ++ "/*")),
    ok = rabbit_node_monitor:reset_cluster_status(),
    ok.

change_cluster_node_type(Type) ->
    ensure_mnesia_not_running(),
    ensure_mnesia_dir(),
    case is_clustered() of
        false -> e(not_clustered);
        true  -> ok
    end,
    {_, _, RunningNodes} = discover_cluster(cluster_nodes(all)),
    %% We might still be marked as running by a remote node since the
    %% information of us going down might not have propagated yet.
    Node = case RunningNodes -- [node()] of
               []        -> e(no_online_cluster_nodes);
               [Node0|_] -> Node0
           end,
    ok = reset(),
    ok = join_cluster(Node, Type).

update_cluster_nodes(DiscoveryNode) ->
    ensure_mnesia_not_running(),
    ensure_mnesia_dir(),
    Status = {AllNodes, _, _} = discover_cluster([DiscoveryNode]),
    case me_in_nodes(AllNodes) of
        true ->
            %% As in `check_consistency/0', we can safely delete the
            %% schema here, since it'll be replicated from the other
            %% nodes
            mnesia:delete_schema([node()]),
            rabbit_node_monitor:write_cluster_status(Status),
            rabbit_log:info("Updating cluster nodes from ~p~n",
                            [DiscoveryNode]),
            init_db_with_mnesia(AllNodes, node_type(), true, true);
        false ->
            e(inconsistent_cluster)
    end,
    ok.

%% We proceed like this: try to remove the node locally. If the node
%% is offline, we remove the node if:
%%   * This node is a disc node
%%   * All other nodes are offline
%%   * This node was, at the best of our knowledge (see comment below)
%%     the last or second to last after the node we're removing to go
%%     down
forget_cluster_node(Node, RemoveWhenOffline) ->
    case lists:member(Node, cluster_nodes(all)) of
        true  -> ok;
        false -> e(not_a_cluster_node)
    end,
    case {RemoveWhenOffline, is_running()} of
        {true,  false} -> remove_node_offline_node(Node);
        {true,   true} -> e(online_node_offline_flag);
        {false, false} -> e(offline_node_no_offline_flag);
        {false,  true} -> rabbit_log:info(
                            "Removing node ~p from cluster~n", [Node]),
                          case remove_node_if_mnesia_running(Node) of
                              ok               -> ok;
                              {error, _} = Err -> throw(Err)
                          end
    end.

remove_node_offline_node(Node) ->
    %% Here `mnesia:system_info(running_db_nodes)' will RPC, but that's what we
    %% want - we need to know the running nodes *now*.  If the current node is a
    %% RAM node it will return bogus results, but we don't care since we only do
    %% this operation from disc nodes.
    case {mnesia:system_info(running_db_nodes) -- [Node], node_type()} of
        {[], disc} ->
            start_mnesia(),
            try
                %% What we want to do here is replace the last node to
                %% go down with the current node.  The way we do this
                %% is by force loading the table, and making sure that
                %% they are loaded.
                rabbit_table:force_load(),
                rabbit_table:wait_for_replicated(),
                forget_cluster_node(Node, false),
                force_load_next_boot()
            after
                stop_mnesia()
            end;
        {_, _} ->
            e(removing_node_from_offline_node)
    end.

%%----------------------------------------------------------------------------
%% Queries
%%----------------------------------------------------------------------------

status() ->
    IfNonEmpty = fun (_,       []) -> [];
                     (Type, Nodes) -> [{Type, Nodes}]
                 end,
    [{nodes, (IfNonEmpty(disc, cluster_nodes(disc)) ++
                  IfNonEmpty(ram, cluster_nodes(ram)))}] ++
        case is_running() of
            true  -> RunningNodes = cluster_nodes(running),
                     [{running_nodes, RunningNodes},
                      {cluster_name,  rabbit_nodes:cluster_name()},
                      {partitions,    mnesia_partitions(RunningNodes)}];
            false -> []
        end.

mnesia_partitions(Nodes) ->
    Replies = rabbit_node_monitor:partitions(Nodes),
    [Reply || Reply = {_, R} <- Replies, R =/= []].

is_running() -> mnesia:system_info(is_running) =:= yes.

is_clustered() -> AllNodes = cluster_nodes(all),
                  AllNodes =/= [] andalso AllNodes =/= [node()].

on_running_node(Pid) -> lists:member(node(Pid), cluster_nodes(running)).

%% This requires the process be in the same running cluster as us
%% (i.e. not partitioned or some random node).
%%
%% See also rabbit_misc:is_process_alive/1 which does not.
is_process_alive(Pid) ->
    on_running_node(Pid) andalso
        rpc:call(node(Pid), erlang, is_process_alive, [Pid]) =:= true.

cluster_nodes(WhichNodes) -> cluster_status(WhichNodes).

%% This function is the actual source of information, since it gets
%% the data from mnesia. Obviously it'll work only when mnesia is
%% running.
cluster_status_from_mnesia() ->
    case is_running() of
        false ->
            {error, mnesia_not_running};
        true ->
            %% If the tables are not present, it means that
            %% `init_db/3' hasn't been run yet. In other words, either
            %% we are a virgin node or a restarted RAM node. In both
            %% cases we're not interested in what mnesia has to say.
            NodeType = case mnesia:system_info(use_dir) of
                           true  -> disc;
                           false -> ram
                       end,
            case rabbit_table:is_present() of
                true  -> AllNodes = mnesia:system_info(db_nodes),
                         DiscCopies = mnesia:table_info(schema, disc_copies),
                         DiscNodes = case NodeType of
                                         disc -> nodes_incl_me(DiscCopies);
                                         ram  -> DiscCopies
                                     end,
                         %% `mnesia:system_info(running_db_nodes)' is safe since
                         %% we know that mnesia is running
                         RunningNodes = mnesia:system_info(running_db_nodes),
                         {ok, {AllNodes, DiscNodes, RunningNodes}};
                false -> {error, tables_not_present}
            end
    end.

cluster_status(WhichNodes) ->
    {AllNodes, DiscNodes, RunningNodes} = Nodes =
        case cluster_status_from_mnesia() of
            {ok, Nodes0} ->
                Nodes0;
            {error, _Reason} ->
                {AllNodes0, DiscNodes0, RunningNodes0} =
                    rabbit_node_monitor:read_cluster_status(),
                %% The cluster status file records the status when the node is
                %% online, but we know for sure that the node is offline now, so
                %% we can remove it from the list of running nodes.
                {AllNodes0, DiscNodes0, nodes_excl_me(RunningNodes0)}
        end,
    case WhichNodes of
        status  -> Nodes;
        all     -> AllNodes;
        disc    -> DiscNodes;
        ram     -> AllNodes -- DiscNodes;
        running -> RunningNodes
    end.

node_info() ->
    {rabbit_misc:otp_release(), rabbit_misc:version(),
     cluster_status_from_mnesia()}.

node_type() ->
    {_AllNodes, DiscNodes, _RunningNodes} =
        rabbit_node_monitor:read_cluster_status(),
    case DiscNodes =:= [] orelse me_in_nodes(DiscNodes) of
        true  -> disc;
        false -> ram
    end.

dir() -> mnesia:system_info(directory).

%%----------------------------------------------------------------------------
%% Operations on the db
%%----------------------------------------------------------------------------

%% Adds the provided nodes to the mnesia cluster, creating a new
%% schema if there is the need to and catching up if there are other
%% nodes in the cluster already. It also updates the cluster status
%% file.
init_db(ClusterNodes, NodeType, CheckOtherNodes) ->
    Nodes = change_extra_db_nodes(ClusterNodes, CheckOtherNodes),
    %% Note that we use `system_info' here and not the cluster status
    %% since when we start rabbit for the first time the cluster
    %% status will say we are a disc node but the tables won't be
    %% present yet.
    WasDiscNode = mnesia:system_info(use_dir),
    case {Nodes, WasDiscNode, NodeType} of
        {[], _, ram} ->
            %% Standalone ram node, we don't want that
            throw({error, cannot_create_standalone_ram_node});
        {[], false, disc} ->
            %% RAM -> disc, starting from scratch
            ok = create_schema();
        {[], true, disc} ->
            %% First disc node up
            maybe_force_load(),
            ok;
        {[_ | _], _, _} ->
            %% Subsequent node in cluster, catch up
            maybe_force_load(),
            ok = rabbit_table:wait_for_replicated(),
            ok = rabbit_table:create_local_copy(NodeType)
    end,
    ensure_schema_integrity(),
    rabbit_node_monitor:update_cluster_status(),
    ok.

init_db_unchecked(ClusterNodes, NodeType) ->
    init_db(ClusterNodes, NodeType, false).

init_db_and_upgrade(ClusterNodes, NodeType, CheckOtherNodes) ->
    ok = init_db(ClusterNodes, NodeType, CheckOtherNodes),
    ok = case rabbit_upgrade:maybe_upgrade_local() of
             ok                    -> ok;
             starting_from_scratch -> rabbit_version:record_desired();
             version_not_available -> schema_ok_or_move()
         end,
    %% `maybe_upgrade_local' restarts mnesia, so ram nodes will forget
    %% about the cluster
    case NodeType of
        ram  -> start_mnesia(),
                change_extra_db_nodes(ClusterNodes, false);
        disc -> ok
    end,
    %% ...and all nodes will need to wait for tables
    rabbit_table:wait_for_replicated(),
    ok.

init_db_with_mnesia(ClusterNodes, NodeType,
                    CheckOtherNodes, CheckConsistency) ->
    start_mnesia(CheckConsistency),
    try
        init_db_and_upgrade(ClusterNodes, NodeType, CheckOtherNodes)
    after
        stop_mnesia()
    end.

ensure_mnesia_dir() ->
    MnesiaDir = dir() ++ "/",
    case filelib:ensure_dir(MnesiaDir) of
        {error, Reason} ->
            throw({error, {cannot_create_mnesia_dir, MnesiaDir, Reason}});
        ok ->
            ok
    end.

ensure_mnesia_running() ->
    case mnesia:system_info(is_running) of
        yes ->
            ok;
        starting ->
            wait_for(mnesia_running),
            ensure_mnesia_running();
        Reason when Reason =:= no; Reason =:= stopping ->
            throw({error, mnesia_not_running})
    end.

ensure_mnesia_not_running() ->
    case mnesia:system_info(is_running) of
        no ->
            ok;
        stopping ->
            wait_for(mnesia_not_running),
            ensure_mnesia_not_running();
        Reason when Reason =:= yes; Reason =:= starting ->
            throw({error, mnesia_unexpectedly_running})
    end.

ensure_schema_integrity() ->
    case rabbit_table:check_schema_integrity() of
        ok ->
            ok;
        {error, Reason} ->
            throw({error, {schema_integrity_check_failed, Reason}})
    end.

copy_db(Destination) ->
    ok = ensure_mnesia_not_running(),
    rabbit_file:recursive_copy(dir(), Destination).

force_load_filename() ->
    filename:join(dir(), "force_load").

force_load_next_boot() ->
    rabbit_file:write_file(force_load_filename(), <<"">>).

maybe_force_load() ->
    case rabbit_file:is_file(force_load_filename()) of
        true  -> rabbit_table:force_load(),
                 rabbit_file:delete(force_load_filename());
        false -> ok
    end.

%% This does not guarantee us much, but it avoids some situations that
%% will definitely end up badly
check_cluster_consistency() ->
    %% We want to find 0 or 1 consistent nodes.
    case lists:foldl(
           fun (Node,  {error, _})    -> check_cluster_consistency(Node, true);
               (_Node, {ok, Status})  -> {ok, Status}
           end, {error, not_found}, nodes_excl_me(cluster_nodes(all)))
    of
        {ok, Status = {RemoteAllNodes, _, _}} ->
            case ordsets:is_subset(ordsets:from_list(cluster_nodes(all)),
                                   ordsets:from_list(RemoteAllNodes)) of
                true  ->
                    ok;
                false ->
                    %% We delete the schema here since we think we are
                    %% clustered with nodes that are no longer in the
                    %% cluster and there is no other way to remove
                    %% them from our schema. On the other hand, we are
                    %% sure that there is another online node that we
                    %% can use to sync the tables with. There is a
                    %% race here: if between this check and the
                    %% `init_db' invocation the cluster gets
                    %% disbanded, we're left with a node with no
                    %% mnesia data that will try to connect to offline
                    %% nodes.
                    mnesia:delete_schema([node()])
            end,
            rabbit_node_monitor:write_cluster_status(Status);
        {error, not_found} ->
            ok;
        {error, _} = E ->
            throw(E)
    end.

check_cluster_consistency(Node, CheckNodesConsistency) ->
    case rpc:call(Node, rabbit_mnesia, node_info, []) of
        {badrpc, _Reason} ->
            {error, not_found};
        {_OTP, _Rabbit, {error, _}} ->
            {error, not_found};
        {OTP, Rabbit, {ok, Status}} when CheckNodesConsistency ->
            case check_consistency(OTP, Rabbit, Node, Status) of
                {error, _} = E -> E;
                {ok, Res}      -> {ok, Res}
            end;
        {OTP, Rabbit, {ok, Status}} ->
            case check_consistency(OTP, Rabbit) of
                {error, _} = E -> E;
                ok             -> {ok, Status}
            end;
        {_OTP, Rabbit, _Hash, _Status} ->
            %% delegate hash checking implies version mismatch
            version_error("Rabbit", rabbit_misc:version(), Rabbit)
    end.

%%--------------------------------------------------------------------
%% Hooks for `rabbit_node_monitor'
%%--------------------------------------------------------------------

on_node_up(Node) ->
    case running_disc_nodes() of
        [Node] -> rabbit_log:info("cluster contains disc nodes again~n");
        _      -> ok
    end.

on_node_down(_Node) ->
    case running_disc_nodes() of
        [] -> rabbit_log:info("only running disc node went down~n");
        _  -> ok
    end.

running_disc_nodes() ->
    {_AllNodes, DiscNodes, RunningNodes} = cluster_status(status),
    ordsets:to_list(ordsets:intersection(ordsets:from_list(DiscNodes),
                                         ordsets:from_list(RunningNodes))).

%%--------------------------------------------------------------------
%% Internal helpers
%%--------------------------------------------------------------------

discover_cluster(Nodes) ->
    case lists:foldl(fun (_,    {ok, Res}) -> {ok, Res};
                         (Node, _)         -> discover_cluster0(Node)
                     end, {error, no_nodes_provided}, Nodes) of
        {ok, Res}        -> Res;
        {error, E}       -> throw({error, E});
        {badrpc, Reason} -> throw({badrpc_multi, Reason, Nodes})
    end.

discover_cluster0(Node) when Node == node() ->
    {error, cannot_cluster_node_with_itself};
discover_cluster0(Node) ->
    rpc:call(Node, rabbit_mnesia, cluster_status_from_mnesia, []).

schema_ok_or_move() ->
    case rabbit_table:check_schema_integrity() of
        ok ->
            ok;
        {error, Reason} ->
            %% NB: we cannot use rabbit_log here since it may not have been
            %% started yet
            rabbit_log:warning("schema integrity check failed: ~p~n"
                               "moving database to backup location "
                               "and recreating schema from scratch~n",
                               [Reason]),
            ok = move_db(),
            ok = create_schema()
    end.

%% We only care about disc nodes since ram nodes are supposed to catch
%% up only
create_schema() ->
    stop_mnesia(),
    rabbit_misc:ensure_ok(mnesia:create_schema([node()]), cannot_create_schema),
    start_mnesia(),
    ok = rabbit_table:create(),
    ensure_schema_integrity(),
    ok = rabbit_version:record_desired().

move_db() ->
    stop_mnesia(),
    MnesiaDir = filename:dirname(dir() ++ "/"),
    {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:universaltime(),
    BackupDir = rabbit_misc:format(
                  "~s_~w~2..0w~2..0w~2..0w~2..0w~2..0w",
                  [MnesiaDir, Year, Month, Day, Hour, Minute, Second]),
    case file:rename(MnesiaDir, BackupDir) of
        ok ->
            %% NB: we cannot use rabbit_log here since it may not have
            %% been started yet
            rabbit_log:warning("moved database from ~s to ~s~n",
                               [MnesiaDir, BackupDir]),
            ok;
        {error, Reason} -> throw({error, {cannot_backup_mnesia,
                                          MnesiaDir, BackupDir, Reason}})
    end,
    ensure_mnesia_dir(),
    start_mnesia(),
    ok.

remove_node_if_mnesia_running(Node) ->
    case is_running() of
        false ->
            {error, mnesia_not_running};
        true ->
            %% Deleting the the schema copy of the node will result in
            %% the node being removed from the cluster, with that
            %% change being propagated to all nodes
            case mnesia:del_table_copy(schema, Node) of
                {atomic, ok} ->
                    rabbit_amqqueue:forget_all_durable(Node),
                    rabbit_node_monitor:notify_left_cluster(Node),
                    ok;
                {aborted, Reason} ->
                    {error, {failed_to_remove_node, Node, Reason}}
            end
    end.

leave_cluster() ->
    case nodes_excl_me(cluster_nodes(all)) of
        []       -> ok;
        AllNodes -> case lists:any(fun leave_cluster/1, AllNodes) of
                        true  -> ok;
                        false -> e(no_running_cluster_nodes)
                    end
    end.

leave_cluster(Node) ->
    case rpc:call(Node,
                  rabbit_mnesia, remove_node_if_mnesia_running, [node()]) of
        ok                          -> true;
        {error, mnesia_not_running} -> false;
        {error, Reason}             -> throw({error, Reason});
        {badrpc, nodedown}          -> false
    end.

wait_for(Condition) ->
    rabbit_log:info("Waiting for ~p...~n", [Condition]),
    timer:sleep(1000).

start_mnesia(CheckConsistency) ->
    case CheckConsistency of
        true  -> check_cluster_consistency();
        false -> ok
    end,
    rabbit_misc:ensure_ok(mnesia:start(), cannot_start_mnesia),
    ensure_mnesia_running().

start_mnesia() ->
    start_mnesia(true).

stop_mnesia() ->
    stopped = mnesia:stop(),
    ensure_mnesia_not_running().

change_extra_db_nodes(ClusterNodes0, CheckOtherNodes) ->
    ClusterNodes = nodes_excl_me(ClusterNodes0),
    case {mnesia:change_config(extra_db_nodes, ClusterNodes), ClusterNodes} of
        {{ok, []}, [_|_]} when CheckOtherNodes ->
            throw({error, {failed_to_cluster_with, ClusterNodes,
                           "Mnesia could not connect to any nodes."}});
        {{ok, Nodes}, _} ->
            Nodes
    end.

check_consistency(OTP, Rabbit) ->
    rabbit_misc:sequence_error(
      [check_otp_consistency(OTP),
       check_rabbit_consistency(Rabbit)]).

check_consistency(OTP, Rabbit, Node, Status) ->
    rabbit_misc:sequence_error(
      [check_otp_consistency(OTP),
       check_rabbit_consistency(Rabbit),
       check_nodes_consistency(Node, Status)]).

check_nodes_consistency(Node, RemoteStatus = {RemoteAllNodes, _, _}) ->
    case me_in_nodes(RemoteAllNodes) of
        true ->
            {ok, RemoteStatus};
        false ->
            {error, {inconsistent_cluster,
                     rabbit_misc:format("Node ~p thinks it's clustered "
                                        "with node ~p, but ~p disagrees",
                                        [node(), Node, Node])}}
    end.

check_version_consistency(This, Remote, Name) ->
    check_version_consistency(This, Remote, Name, fun (A, B) -> A =:= B end).

check_version_consistency(This, Remote, Name, Comp) ->
    case Comp(This, Remote) of
        true  -> ok;
        false -> version_error(Name, This, Remote)
    end.

version_error(Name, This, Remote) ->
    {error, {inconsistent_cluster,
             rabbit_misc:format("~s version mismatch: local node is ~s, "
                                "remote node ~s", [Name, This, Remote])}}.

check_otp_consistency(Remote) ->
    check_version_consistency(rabbit_misc:otp_release(), Remote, "OTP").

check_rabbit_consistency(Remote) ->
    check_version_consistency(
      rabbit_misc:version(), Remote, "Rabbit",
      fun rabbit_misc:version_minor_equivalent/2).

%% This is fairly tricky.  We want to know if the node is in the state
%% that a `reset' would leave it in.  We cannot simply check if the
%% mnesia tables aren't there because restarted RAM nodes won't have
%% tables while still being non-virgin.  What we do instead is to
%% check if the mnesia directory is non existant or empty, with the
%% exception of the cluster status files, which will be there thanks to
%% `rabbit_node_monitor:prepare_cluster_status_file/0'.
is_virgin_node() ->
    case rabbit_file:list_dir(dir()) of
        {error, enoent} ->
            true;
        {ok, []} ->
            true;
        {ok, [File1, File2]} ->
            lists:usort([dir() ++ "/" ++ File1, dir() ++ "/" ++ File2]) =:=
                lists:usort([rabbit_node_monitor:cluster_status_filename(),
                             rabbit_node_monitor:running_nodes_filename()]);
        {ok, _} ->
            false
    end.

find_auto_cluster_node([]) ->
    none;
find_auto_cluster_node([Node | Nodes]) ->
    Fail = fun (Fmt, Args) ->
                   rabbit_log:warning(
                     "Could not auto-cluster with ~s: " ++ Fmt, [Node | Args]),
                   find_auto_cluster_node(Nodes)
           end,
    case rpc:call(Node, rabbit_mnesia, node_info, []) of
        {badrpc, _} = Reason         -> Fail("~p~n", [Reason]);
        %% old delegate hash check
        {_OTP, RMQ, _Hash, _}        -> Fail("version ~s~n", [RMQ]);
        {_OTP, _RMQ, {error, _} = E} -> Fail("~p~n", [E]);
        {OTP, RMQ, _}                -> case check_consistency(OTP, RMQ) of
                                            {error, _} -> Fail("versions ~p~n",
                                                               [{OTP, RMQ}]);
                                            ok         -> {ok, Node}
                                        end
    end.

is_only_clustered_disc_node() ->
    node_type() =:= disc andalso is_clustered() andalso
        cluster_nodes(disc) =:= [node()].

me_in_nodes(Nodes) -> lists:member(node(), Nodes).

nodes_incl_me(Nodes) -> lists:usort([node()|Nodes]).

nodes_excl_me(Nodes) -> Nodes -- [node()].

e(Tag) -> throw({error, {Tag, error_description(Tag)}}).

error_description({invalid_cluster_node_names, BadNames}) ->
    "In the 'cluster_nodes' configuration key, the following node names "
        "are invalid: " ++ lists:flatten(io_lib:format("~p", [BadNames]));
error_description({invalid_cluster_node_type, BadType}) ->
    "In the 'cluster_nodes' configuration key, the node type is invalid "
        "(expected 'disc' or 'ram'): " ++
        lists:flatten(io_lib:format("~p", [BadType]));
error_description(cluster_node_type_mandatory) ->
    "The 'cluster_nodes' configuration key must indicate the node type: "
        "either {[...], disc} or {[...], ram}";
error_description(invalid_cluster_nodes_conf) ->
    "The 'cluster_nodes' configuration key is invalid, it must be of the "
        "form {[Nodes], Type}, where Nodes is a list of node names and "
        "Type is either 'disc' or 'ram'";
error_description(clustering_only_disc_node) ->
    "You cannot cluster a node if it is the only disc node in its existing "
        " cluster. If new nodes joined while this node was offline, use "
        "'update_cluster_nodes' to add them manually.";
error_description(resetting_only_disc_node) ->
    "You cannot reset a node when it is the only disc node in a cluster. "
        "Please convert another node of the cluster to a disc node first.";
error_description(not_clustered) ->
    "Non-clustered nodes can only be disc nodes.";
error_description(no_online_cluster_nodes) ->
    "Could not find any online cluster nodes. If the cluster has changed, "
        "you can use the 'update_cluster_nodes' command.";
error_description(inconsistent_cluster) ->
    "The nodes provided do not have this node as part of the cluster.";
error_description(not_a_cluster_node) ->
    "The node selected is not in the cluster.";
error_description(online_node_offline_flag) ->
    "You set the --offline flag, which is used to remove nodes remotely from "
        "offline nodes, but this node is online.";
error_description(offline_node_no_offline_flag) ->
    "You are trying to remove a node from an offline node. That is dangerous, "
        "but can be done with the --offline flag. Please consult the manual "
        "for rabbitmqctl for more information.";
error_description(removing_node_from_offline_node) ->
    "To remove a node remotely from an offline node, the node you are removing "
        "from must be a disc node and all the other nodes must be offline.";
error_description(no_running_cluster_nodes) ->
    "You cannot leave a cluster if no online nodes are present.".
