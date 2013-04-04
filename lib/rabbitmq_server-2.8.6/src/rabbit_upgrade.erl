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
%% The Initial Developer of the Original Code is VMware, Inc.
%% Copyright (c) 2007-2013 VMware, Inc.  All rights reserved.
%%

-module(rabbit_upgrade).

-export([maybe_upgrade_mnesia/0, maybe_upgrade_local/0]).

-include("rabbit.hrl").

-define(VERSION_FILENAME, "schema_version").
-define(LOCK_FILENAME, "schema_upgrade_lock").

%% -------------------------------------------------------------------

-ifdef(use_specs).

-spec(maybe_upgrade_mnesia/0 :: () -> 'ok').
-spec(maybe_upgrade_local/0 :: () -> 'ok' |
                                     'version_not_available' |
                                     'starting_from_scratch').

-endif.

%% -------------------------------------------------------------------

%% The upgrade logic is quite involved, due to the existence of
%% clusters.
%%
%% Firstly, we have two different types of upgrades to do: Mnesia and
%% everythinq else. Mnesia upgrades must only be done by one node in
%% the cluster (we treat a non-clustered node as a single-node
%% cluster). This is the primary upgrader. The other upgrades need to
%% be done by all nodes.
%%
%% The primary upgrader has to start first (and do its Mnesia
%% upgrades). Secondary upgraders need to reset their Mnesia database
%% and then rejoin the cluster. They can't do the Mnesia upgrades as
%% well and then merge databases since the cookie for each table will
%% end up different and the merge will fail.
%%
%% This in turn means that we need to determine whether we are the
%% primary or secondary upgrader *before* Mnesia comes up. If we
%% didn't then the secondary upgrader would try to start Mnesia, and
%% either hang waiting for a node which is not yet up, or fail since
%% its schema differs from the other nodes in the cluster.
%%
%% Also, the primary upgrader needs to start Mnesia to do its
%% upgrades, but needs to forcibly load tables rather than wait for
%% them (in case it was not the last node to shut down, in which case
%% it would wait forever).
%%
%% This in turn means that maybe_upgrade_mnesia/0 has to be patched
%% into the boot process by prelaunch before the mnesia application is
%% started. By the time Mnesia is started the upgrades have happened
%% (on the primary), or Mnesia has been reset (on the secondary) and
%% rabbit_mnesia:init_db_unchecked/2 can then make the node rejoin the cluster
%% in the normal way.
%%
%% The non-mnesia upgrades are then triggered by
%% rabbit_mnesia:init_db_unchecked/2. Of course, it's possible for a given
%% upgrade process to only require Mnesia upgrades, or only require
%% non-Mnesia upgrades. In the latter case no Mnesia resets and
%% reclusterings occur.
%%
%% The primary upgrader needs to be a disc node. Ideally we would like
%% it to be the last disc node to shut down (since otherwise there's a
%% risk of data loss). On each node we therefore record the disc nodes
%% that were still running when we shut down. A disc node that knows
%% other nodes were up when it shut down, or a ram node, will refuse
%% to be the primary upgrader, and will thus not start when upgrades
%% are needed.
%%
%% However, this is racy if several nodes are shut down at once. Since
%% rabbit records the running nodes, and shuts down before mnesia, the
%% race manifests as all disc nodes thinking they are not the primary
%% upgrader. Therefore the user can remove the record of the last disc
%% node to shut down to get things going again. This may lose any
%% mnesia changes that happened after the node chosen as the primary
%% upgrader was shut down.

%% -------------------------------------------------------------------

ensure_backup_taken() ->
    case filelib:is_file(lock_filename()) of
        false -> case filelib:is_dir(backup_dir()) of
                     false -> ok = take_backup();
                     _     -> ok
                 end;
        true  -> throw({error, previous_upgrade_failed})
    end.

take_backup() ->
    BackupDir = backup_dir(),
    case rabbit_mnesia:copy_db(BackupDir) of
        ok         -> info("upgrades: Mnesia dir backed up to ~p~n",
                           [BackupDir]);
        {error, E} -> throw({could_not_back_up_mnesia_dir, E})
    end.

ensure_backup_removed() ->
    case filelib:is_dir(backup_dir()) of
        true -> ok = remove_backup();
        _    -> ok
    end.

remove_backup() ->
    ok = rabbit_file:recursive_delete([backup_dir()]),
    info("upgrades: Mnesia backup removed~n", []).

maybe_upgrade_mnesia() ->
    AllNodes = rabbit_mnesia:cluster_nodes(all),
    case rabbit_version:upgrades_required(mnesia) of
        {error, starting_from_scratch} ->
            ok;
        {error, version_not_available} ->
            case AllNodes of
                [] -> die("Cluster upgrade needed but upgrading from "
                          "< 2.1.1.~nUnfortunately you will need to "
                          "rebuild the cluster.", []);
                _  -> ok
            end;
        {error, _} = Err ->
            throw(Err);
        {ok, []} ->
            ok;
        {ok, Upgrades} ->
            ensure_backup_taken(),
            ok = case upgrade_mode(AllNodes) of
                     primary   -> primary_upgrade(Upgrades, AllNodes);
                     secondary -> secondary_upgrade(AllNodes)
                 end
    end.

upgrade_mode(AllNodes) ->
    case nodes_running(AllNodes) of
        [] ->
            AfterUs = rabbit_mnesia:cluster_nodes(running) -- [node()],
            case {node_type_legacy(), AfterUs} of
                {disc, []}  ->
                    primary;
                {disc, _}  ->
                    Filename = rabbit_node_monitor:running_nodes_filename(),
                    die("Cluster upgrade needed but other disc nodes shut "
                        "down after this one.~nPlease first start the last "
                        "disc node to shut down.~n~nNote: if several disc "
                        "nodes were shut down simultaneously they may "
                        "all~nshow this message. In which case, remove "
                        "the lock file on one of them and~nstart that node. "
                        "The lock file on this node is:~n~n ~s ", [Filename]);
                {ram, _} ->
                    die("Cluster upgrade needed but this is a ram node.~n"
                        "Please first start the last disc node to shut down.",
                        [])
            end;
        [Another|_] ->
            MyVersion = rabbit_version:desired_for_scope(mnesia),
            ErrFun = fun (ClusterVersion) ->
                             %% The other node(s) are running an
                             %% unexpected version.
                             die("Cluster upgrade needed but other nodes are "
                                 "running ~p~nand I want ~p",
                                 [ClusterVersion, MyVersion])
                     end,
            case rpc:call(Another, rabbit_version, desired_for_scope,
                          [mnesia]) of
                {badrpc, {'EXIT', {undef, _}}} -> ErrFun(unknown_old_version);
                {badrpc, Reason}               -> ErrFun({unknown, Reason});
                CV                             -> case rabbit_version:matches(
                                                         MyVersion, CV) of
                                                      true  -> secondary;
                                                      false -> ErrFun(CV)
                                                  end
            end
    end.

die(Msg, Args) ->
    %% We don't throw or exit here since that gets thrown
    %% straight out into do_boot, generating an erl_crash.dump
    %% and displaying any error message in a confusing way.
    error_logger:error_msg(Msg, Args),
    io:format("~n~n****~n~n" ++ Msg ++ "~n~n****~n~n~n", Args),
    error_logger:logfile(close),
    halt(1).

primary_upgrade(Upgrades, Nodes) ->
    Others = Nodes -- [node()],
    ok = apply_upgrades(
           mnesia,
           Upgrades,
           fun () ->
                   rabbit_table:force_load(),
                   case Others of
                       [] -> ok;
                       _  -> info("mnesia upgrades: Breaking cluster~n", []),
                             [{atomic, ok} = mnesia:del_table_copy(schema, Node)
                              || Node <- Others]
                   end
           end),
    ok.

secondary_upgrade(AllNodes) ->
    %% must do this before we wipe out schema
    NodeType = node_type_legacy(),
    rabbit_misc:ensure_ok(mnesia:delete_schema([node()]),
                          cannot_delete_schema),
    rabbit_misc:ensure_ok(mnesia:start(), cannot_start_mnesia),
    ok = rabbit_mnesia:init_db_unchecked(AllNodes, NodeType),
    ok = rabbit_version:record_desired_for_scope(mnesia),
    ok.

nodes_running(Nodes) ->
    [N || N <- Nodes, rabbit:is_running(N)].

%% -------------------------------------------------------------------

maybe_upgrade_local() ->
    case rabbit_version:upgrades_required(local) of
        {error, version_not_available} -> version_not_available;
        {error, starting_from_scratch} -> starting_from_scratch;
        {error, _} = Err               -> throw(Err);
        {ok, []}                       -> ensure_backup_removed(),
                                          ok;
        {ok, Upgrades}                 -> mnesia:stop(),
                                          ensure_backup_taken(),
                                          ok = apply_upgrades(local, Upgrades,
                                                              fun () -> ok end),
                                          ensure_backup_removed(),
                                          ok
    end.

%% -------------------------------------------------------------------

apply_upgrades(Scope, Upgrades, Fun) ->
    ok = rabbit_file:lock_file(lock_filename()),
    info("~s upgrades: ~w to apply~n", [Scope, length(Upgrades)]),
    rabbit_misc:ensure_ok(mnesia:start(), cannot_start_mnesia),
    Fun(),
    [apply_upgrade(Scope, Upgrade) || Upgrade <- Upgrades],
    info("~s upgrades: All upgrades applied successfully~n", [Scope]),
    ok = rabbit_version:record_desired_for_scope(Scope),
    ok = file:delete(lock_filename()).

apply_upgrade(Scope, {M, F}) ->
    info("~s upgrades: Applying ~w:~w~n", [Scope, M, F]),
    ok = apply(M, F, []).

%% -------------------------------------------------------------------

dir() -> rabbit_mnesia:dir().

lock_filename() -> lock_filename(dir()).
lock_filename(Dir) -> filename:join(Dir, ?LOCK_FILENAME).
backup_dir() -> dir() ++ "-upgrade-backup".

node_type_legacy() ->
    %% This is pretty ugly but we can't start Mnesia and ask it (will
    %% hang), we can't look at the config file (may not include us
    %% even if we're a disc node).  We also can't use
    %% rabbit_mnesia:node_type/0 because that will give false
    %% postivies on Rabbit up to 2.5.1.
    case filelib:is_regular(filename:join(dir(), "rabbit_durable_exchange.DCD")) of
        true  -> disc;
        false -> ram
    end.

%% NB: we cannot use rabbit_log here since it may not have been
%% started yet
info(Msg, Args) -> error_logger:info_msg(Msg, Args).
