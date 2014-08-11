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
-module(partitions).

-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-import(rabbit_misc, [pget/2]).

-define(CONFIG, [start_abc, fun enable_dist_proxy/1,
                 build_cluster, short_ticktime(1), start_connections]).
%% We set ticktime to 1s and setuptime is 7s so to make sure it
%% passes...
-define(DELAY, 8000).

ignore_with() -> ?CONFIG.
ignore(Cfgs) ->
    [A, B, C] = [pget(node, Cfg) || Cfg <- Cfgs],
    block_unblock([{B, C}]),
    timer:sleep(?DELAY),
    [] = partitions(A),
    [C] = partitions(B),
    [B] = partitions(C),
    ok.

pause_on_down_with() -> ?CONFIG.
pause_on_down([CfgA, CfgB, CfgC] = Cfgs) ->
    A = pget(node, CfgA),
    set_mode(Cfgs, pause_minority),
    true = is_running(A),

    rabbit_test_util:kill(CfgB, sigkill),
    timer:sleep(?DELAY),
    true = is_running(A),

    rabbit_test_util:kill(CfgC, sigkill),
    await_running(A, false),
    ok.

pause_on_blocked_with() -> ?CONFIG.
pause_on_blocked(Cfgs) ->
    [A, B, C] = [pget(node, Cfg) || Cfg <- Cfgs],
    set_mode(Cfgs, pause_minority),
    [(true = is_running(N)) || N <- [A, B, C]],
    block([{A, B}, {A, C}]),
    await_running(A, false),
    [await_running(N, true) || N <- [B, C]],
    unblock([{A, B}, {A, C}]),
    [await_running(N, true) || N <- [A, B, C]],
    Status = rpc:call(B, rabbit_mnesia, status, []),
    [] = pget(partitions, Status),
    ok.

%% Make sure we do not confirm any messages after a partition has
%% happened but before we pause, since any such confirmations would be
%% lies.
%%
%% This test has to use an AB cluster (not ABC) since GM ends up
%% taking longer to detect down slaves when there are more nodes and
%% we close the window by mistake.
%%
%% In general there are quite a few ways to accidentally cause this
%% test to pass since there are a lot of things in the broker that can
%% suddenly take several seconds to time out when TCP connections
%% won't establish.
pause_false_promises_mirrored_with() ->
    [start_ab, fun enable_dist_proxy/1,
     build_cluster, short_ticktime(10), start_connections, ha_policy_all].

pause_false_promises_mirrored(Cfgs) ->
    pause_false_promises(Cfgs).

pause_false_promises_unmirrored_with() ->
    [start_ab, fun enable_dist_proxy/1,
     build_cluster, short_ticktime(10), start_connections].

pause_false_promises_unmirrored(Cfgs) ->
    pause_false_promises(Cfgs).

pause_false_promises([CfgA, CfgB | _] = Cfgs) ->
    [A, B] = [pget(node, Cfg) || Cfg <- Cfgs],
    set_mode([CfgA], pause_minority),
    ChA = pget(channel, CfgA),
    ChB = pget(channel, CfgB),
    amqp_channel:call(ChB, #'queue.declare'{queue   = <<"test">>,
                                            durable = true}),
    amqp_channel:call(ChA, #'confirm.select'{}),
    amqp_channel:register_confirm_handler(ChA, self()),

    %% Cause a partition after 1s
    Self = self(),
    spawn_link(fun () ->
                       timer:sleep(1000),
                       %%io:format(user, "~p BLOCK~n", [calendar:local_time()]),
                       block([{A, B}]),
                       unlink(Self)
               end),

    %% Publish large no of messages, see how many we get confirmed
    [amqp_channel:cast(ChA, #'basic.publish'{routing_key = <<"test">>},
                       #amqp_msg{props = #'P_basic'{delivery_mode = 1}}) ||
        _ <- lists:seq(1, 100000)],
    %%io:format(user, "~p finish publish~n", [calendar:local_time()]),

    %% Time for the partition to be detected. We don't put this sleep
    %% in receive_acks since otherwise we'd have another similar sleep
    %% at the end.
    timer:sleep(30000),
    Confirmed = receive_acks(0),
    %%io:format(user, "~p got acks~n", [calendar:local_time()]),
    await_running(A, false),
    %%io:format(user, "~p A stopped~n", [calendar:local_time()]),

    unblock([{A, B}]),
    await_running(A, true),

    %% But how many made it onto the rest of the cluster?
    #'queue.declare_ok'{message_count = Survived} = 
        amqp_channel:call(ChB, #'queue.declare'{queue   = <<"test">>,
                                                durable = true}),
    %%io:format(user, "~p queue declared~n", [calendar:local_time()]),
    case Confirmed > Survived of
        true  -> ?debugVal({Confirmed, Survived});
        false -> ok
    end,
    ?assert(Confirmed =< Survived),
    ok.

receive_acks(Max) ->
    receive
        #'basic.ack'{delivery_tag = DTag} ->
            receive_acks(DTag)
    after ?DELAY ->
            Max
    end.

prompt_disconnect_detection_with() ->
    [start_ab, fun enable_dist_proxy/1,
     build_cluster, short_ticktime(1), start_connections].

prompt_disconnect_detection([CfgA, CfgB]) ->
    A = pget(node, CfgA),
    B = pget(node, CfgB),
    ChB = pget(channel, CfgB),
    [amqp_channel:call(ChB, #'queue.declare'{}) || _ <- lists:seq(1, 100)],
    block([{A, B}]),
    timer:sleep(?DELAY),
    %% We want to make sure we do not end up waiting for setuptime *
    %% no of queues. Unfortunately that means we need a timeout...
    [] = rpc(CfgA, rabbit_amqqueue, info_all, [<<"/">>], ?DELAY),
    ok.

autoheal_with() -> ?CONFIG.
autoheal(Cfgs) ->
    [A, B, C] = [pget(node, Cfg) || Cfg <- Cfgs],
    set_mode(Cfgs, autoheal),
    Test = fun (Pairs) ->
                   block_unblock(Pairs),
                   [await_running(N, true) || N <- [A, B, C]],
                   [] = partitions(A),
                   [] = partitions(B),
                   [] = partitions(C)
           end,
    Test([{B, C}]),
    Test([{A, C}, {B, C}]),
    Test([{A, B}, {A, C}, {B, C}]),
    ok.

set_mode(Cfgs, Mode) ->
    [set_env(Cfg, rabbit, cluster_partition_handling, Mode) || Cfg <- Cfgs].

set_env(Cfg, App, K, V) ->
    rpc(Cfg, application, set_env, [App, K, V]).

block_unblock(Pairs) ->
    block(Pairs),
    timer:sleep(?DELAY),
    unblock(Pairs).

block(Pairs)   -> [block(X, Y) || {X, Y} <- Pairs].
unblock(Pairs) -> [allow(X, Y) || {X, Y} <- Pairs].

partitions(Node) ->
    rpc:call(Node, rabbit_node_monitor, partitions, []).

block(X, Y) ->
    rpc:call(X, inet_tcp_proxy, block, [Y]),
    rpc:call(Y, inet_tcp_proxy, block, [X]).

allow(X, Y) ->
    rpc:call(X, inet_tcp_proxy, allow, [Y]),
    rpc:call(Y, inet_tcp_proxy, allow, [X]).

await_running  (Node, Bool) -> await(Node, Bool, fun is_running/1).
await_listening(Node, Bool) -> await(Node, Bool, fun is_listening/1).

await(Node, Bool, Fun) ->
    case Fun(Node) of
        Bool -> ok;
        _    -> timer:sleep(100),
                await(Node, Bool, Fun)
    end.

is_running(Node) -> rpc:call(Node, rabbit, is_running, []).

is_listening(Node) ->
    case rpc:call(Node, rabbit_networking, node_listeners, [Node]) of
        []    -> false;
        [_|_] -> true;
        _     -> false
    end.

enable_dist_proxy(Cfgs) ->
    inet_tcp_proxy_manager:start_link(),
    Nodes = [pget(node, Cfg) || Cfg <- Cfgs],
    [ok = rpc:call(Node, inet_tcp_proxy, start, []) || Node <- Nodes],
    [ok = rpc:call(Node, inet_tcp_proxy, reconnect, [Nodes]) || Node <- Nodes],
    Cfgs.

short_ticktime(Time) ->
    fun (Cfgs) ->
            [rpc(Cfg, net_kernel, set_net_ticktime, [Time, 0]) || Cfg <- Cfgs],
            net_kernel:set_net_ticktime(Time, 0),
            Cfgs
    end.

rpc(Cfg, M, F, A) ->
    rpc:call(pget(node, Cfg), M, F, A).

rpc(Cfg, M, F, A, T) ->
    rpc:call(pget(node, Cfg), M, F, A, T).
