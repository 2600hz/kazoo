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
%% Copyright (c) 2007-2014 GoPivotal, Inc.  All rights reserved.
%%
-module(rabbit_test_configs).

-include_lib("amqp_client/include/amqp_client.hrl").

-export([enable_plugins/1]).
-export([cluster/2, cluster_ab/1, cluster_abc/1, start_ab/1, start_abc/1]).
-export([start_connections/1, build_cluster/1]).
-export([ha_policy_all/1, ha_policy_two_pos/1]).
-export([start_nodes/2, start_nodes/3, add_to_cluster/2]).
-export([stop_nodes/1, start_node/1, stop_node/1, kill_node/1, restart_node/1,
         execute/1]).
-export([cover_work_factor/2]).

-import(rabbit_test_util, [set_ha_policy/3, set_ha_policy/4, a2b/1]).
-import(rabbit_misc, [pget/2]).

-define(INITIAL_KEYS, [cover, base, server, plugins]).
-define(NON_RUNNING_KEYS, ?INITIAL_KEYS ++ [nodename, port]).

cluster_ab(InitialCfg)  -> cluster(InitialCfg, [a, b]).
cluster_abc(InitialCfg) -> cluster(InitialCfg, [a, b, c]).
start_ab(InitialCfg)    -> start_nodes(InitialCfg, [a, b]).
start_abc(InitialCfg)   -> start_nodes(InitialCfg, [a, b, c]).

cluster(InitialCfg, NodeNames) ->
    start_connections(build_cluster(start_nodes(InitialCfg, NodeNames))).

start_nodes(InitialCfg, NodeNames) ->
    start_nodes(InitialCfg, NodeNames, 5672).

start_nodes(InitialCfg0, NodeNames, FirstPort) ->
    {ok, Already0} = net_adm:names(),
    Already = [list_to_atom(N) || {N, _P} <- Already0],
    [check_node_not_running(Node, Already) || Node <- NodeNames],
    Ports = lists:seq(FirstPort, length(NodeNames) + FirstPort - 1),
    InitialCfgs = case InitialCfg0 of
                      [{_, _}|_] -> [InitialCfg0 || _ <- NodeNames];
                      _          -> InitialCfg0
                  end,
    Nodes = [[{nodename, N}, {port, P} | strip_non_initial(Cfg)]
             || {N, P, Cfg} <- lists:zip3(NodeNames, Ports, InitialCfgs)],
    [start_node(Node) || Node <- Nodes].

check_node_not_running(Node, Already) ->
    case lists:member(Node, Already) of
        true  -> exit({node_already_running, Node});
        false -> ok
    end.

strip_non_initial(Cfg) ->
    [{K, V} || {K, V} <- Cfg, lists:member(K, ?INITIAL_KEYS)].

strip_running(Cfg) ->
    [{K, V} || {K, V} <- Cfg, lists:member(K, ?NON_RUNNING_KEYS)].

enable_plugins(Cfg) -> enable_plugins(pget(plugins, Cfg), pget(server, Cfg)).

enable_plugins(none, _Server) -> ok;
enable_plugins(Dir, Server) ->
    Env = plugins_env(Dir),
    R = execute(Env, Server ++ "/scripts/rabbitmq-plugins list -m"),
    Plugins = string:tokens(R, "\n"),
    [execute(Env, {Server ++ "/scripts/rabbitmq-plugins enable ~s", [Plugin]})
     || Plugin <- Plugins],
    ok.

plugins_env(none) ->
    [{"RABBITMQ_ENABLED_PLUGINS_FILE", "/does-not-exist"}];
plugins_env(Dir) ->
    [{"RABBITMQ_PLUGINS_DIR",          {"~s/plugins", [Dir]}},
     {"RABBITMQ_PLUGINS_EXPAND_DIR",   {"~s/expand", [Dir]}},
     {"RABBITMQ_ENABLED_PLUGINS_FILE", {"~s/enabled_plugins", [Dir]}}].

start_node(Cfg) ->
    Nodename = pget(nodename, Cfg),
    Port = pget(port, Cfg),
    Base = pget(base, Cfg),
    Server = pget(server, Cfg),
    PidFile = rabbit_misc:format("~s/~s.pid", [Base, Nodename]),
    Linked =
        execute_bg(
          [{"RABBITMQ_MNESIA_BASE", {"~s/rabbitmq-~s-mnesia", [Base,Nodename]}},
           {"RABBITMQ_LOG_BASE",    {"~s", [Base]}},
           {"RABBITMQ_NODENAME",    {"~s", [Nodename]}},
           {"RABBITMQ_NODE_PORT",   {"~B", [Port]}},
           {"RABBITMQ_PID_FILE",    PidFile},
           {"RABBITMQ_CONFIG_FILE", "/some/path/which/does/not/exist"},
           {"RABBITMQ_ALLOW_INPUT", "1"}, %% Needed to make it close on our exit
           %% Bit of a hack - only needed for mgmt tests.
           {"RABBITMQ_SERVER_START_ARGS",
            {"-rabbitmq_management listener [{port,1~B}]", [Port]}},
           {"RABBITMQ_SERVER_ERL_ARGS",
            %% Next two lines are defaults
            {"+K true +A30 +P 1048576 "
             "-kernel inet_default_connect_options [{nodelay,true}] "
             %% Some tests need to be able to make distribution unhappy
             "-pa ~s/../rabbitmq-test/ebin "
             "-proto_dist inet_proxy", [Server]}}
           | plugins_env(pget(plugins, Cfg))],
          Server ++ "/scripts/rabbitmq-server"),
    execute({Server ++ "/scripts/rabbitmqctl -n ~s wait ~s",
             [Nodename, PidFile]}),
    Node = rabbit_nodes:make(Nodename),
    OSPid = rpc:call(Node, os, getpid, []),
    %% The cover system thinks all nodes with the same name are the
    %% same node and will automaticaly re-establish cover as soon as
    %% we see them, so we only want to start cover once per node name
    %% for the entire test run.
    case {pget(cover, Cfg), lists:member(Node, cover:which_nodes())} of
        {true, false} -> cover:start([Node]);
        _             -> ok
    end,
    [{node,       Node},
     {pid_file,   PidFile}, 
     {os_pid,     OSPid},
     {linked_pid, Linked} | Cfg].

build_cluster([First | Rest]) ->
    add_to_cluster([First], Rest).

add_to_cluster([First | _] = Existing, New) ->
    [cluster_with(First, Node) || Node <- New],
    Existing ++ New.

cluster_with(Cfg, NewCfg) ->
    Node = pget(node, Cfg),
    NewNodename = pget(nodename, NewCfg),
    Server = pget(server, Cfg),
    execute({Server ++ "/scripts/rabbitmqctl -n ~s stop_app",
             [NewNodename]}),
    execute({Server ++ "/scripts/rabbitmqctl -n ~s join_cluster ~s",
             [NewNodename, Node]}),
    execute({Server ++ "/scripts/rabbitmqctl -n ~s start_app",
             [NewNodename]}).   

ha_policy_all([Cfg | _] = Cfgs) ->
    set_ha_policy(Cfg, <<".*">>, <<"all">>),
    Cfgs.

ha_policy_two_pos([Cfg | _] = Cfgs) ->
    Members = [a2b(pget(node, C)) || C <- Cfgs],
    TwoNodes = [M || M <- lists:sublist(Members, 2)],
    set_ha_policy(Cfg, <<"^ha.two.">>, {<<"nodes">>, TwoNodes}, []),
    set_ha_policy(Cfg, <<"^ha.auto.">>, {<<"nodes">>, TwoNodes},
                  [{<<"ha-sync-mode">>, <<"automatic">>}]),
    Cfgs.

start_connections(Nodes) -> [start_connection(Node) || Node <- Nodes].

start_connection(Cfg) ->
    Port = pget(port, Cfg),
    {ok, Conn} = amqp_connection:start(#amqp_params_network{port = Port}),
    {ok, Ch} =  amqp_connection:open_channel(Conn),
    [{connection, Conn}, {channel, Ch} | Cfg].

stop_nodes(Nodes) -> [stop_node(Node) || Node <- Nodes].

stop_node(Cfg) ->
    Server = pget(server, Cfg),
    maybe_flush_cover(Cfg),
    catch execute({Server ++ "/scripts/rabbitmqctl -n ~s stop ~s",
                   [pget(nodename, Cfg), pget(pid_file, Cfg)]}),
    strip_running(Cfg).

kill_node(Cfg) ->
    maybe_flush_cover(Cfg),
    catch execute({"kill -9 ~s", [pget(os_pid, Cfg)]}),
    strip_running(Cfg).

restart_node(Cfg) ->
    start_node(stop_node(Cfg)).

maybe_flush_cover(Cfg) ->
    case pget(cover, Cfg) of
        true  -> cover:flush(pget(node, Cfg));
        false -> ok
    end.

%% Cover slows things down enough that if we are sending messages in
%% bulk, we want to send fewer or we'll be here all day...
cover_work_factor(Without, Cfg) ->
    case pget(cover, Cfg) of
        true  -> trunc(Without * 0.1);
        false -> Without
    end.

%%----------------------------------------------------------------------------

execute(Cmd) -> execute([], Cmd).

execute(Env0, Cmd0) ->
    Env = [{K, fmt(V)} || {K, V} <- Env0],
    Cmd = fmt(Cmd0),
    Port = erlang:open_port(
             {spawn, "/usr/bin/env sh -c \"" ++ Cmd ++ "\""},
             [{env, Env}, exit_status,
              stderr_to_stdout, use_stdio]),
    port_receive_loop(Port, "").

port_receive_loop(Port, Stdout) ->
    receive
        {Port, {exit_status, 0}}   -> Stdout;
        {Port, {exit_status, 137}} -> Stdout; %% [0]
        {Port, {exit_status, X}}   -> exit({exit_status, X, Stdout});
        {Port, {data, Out}}        -> %%io:format(user, "~s", [Out]),
                                      port_receive_loop(Port, Stdout ++ Out)
    end.

%% [0] code 137 -> killed with SIGKILL which we do in some tests

execute_bg(Env, Cmd) ->
    spawn_link(fun () ->
                       execute(Env, Cmd),
                       {links, Links} = process_info(self(), links),
                       [unlink(L) || L <- Links]
               end).

fmt({Fmt, Args}) -> rabbit_misc:format(Fmt, Args);
fmt(Str)         -> Str.
