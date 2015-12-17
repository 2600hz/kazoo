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

-module(rabbit_autoheal).

-export([init/0, enabled/0, maybe_start/1, rabbit_down/2, node_down/2,
         handle_msg/3]).

%% The named process we are running in.
-define(SERVER, rabbit_node_monitor).

-define(MNESIA_STOPPED_PING_INTERNAL, 200).

-define(AUTOHEAL_STATE_AFTER_RESTART, rabbit_autoheal_state_after_restart).

%%----------------------------------------------------------------------------

%% In order to autoheal we want to:
%%
%% * Find the winning partition
%% * Stop all nodes in other partitions
%% * Wait for them all to be stopped
%% * Start them again
%%
%% To keep things simple, we assume all nodes are up. We don't start
%% unless all nodes are up, and if a node goes down we abandon the
%% whole process. To further keep things simple we also defer the
%% decision as to the winning node to the "leader" - arbitrarily
%% selected as the first node in the cluster.
%%
%% To coordinate the restarting nodes we pick a special node from the
%% winning partition - the "winner". Restarting nodes then stop, and
%% wait for it to tell them it is safe to start again. The winner
%% determines that a node has stopped just by seeing if its rabbit app
%% stops - if a node stops for any other reason it just gets a message
%% it will ignore, and otherwise we carry on.
%%
%% Meanwhile, the leader may continue to receive new autoheal requests:
%% all of them are ignored. The winner notifies the leader when the
%% current autoheal process is finished (ie. when all losers stopped and
%% were asked to start again) or was aborted. When the leader receives
%% the notification or if it looses contact with the winner, it can
%% accept new autoheal requests.
%%
%% The winner and the leader are not necessarily the same node.
%%
%% The leader can be a loser and will restart in this case. It remembers
%% there is an autoheal in progress by temporarily saving the autoheal
%% state to the application environment.
%%
%% == Possible states ==
%%
%% not_healing
%%   - the default
%%
%% {winner_waiting, OutstandingStops, Notify}
%%   - we are the winner and are waiting for all losing nodes to stop
%%   before telling them they can restart
%%
%% {leader_waiting, Winner, Notify}
%%   - we are the leader, and have already assigned the winner and losers.
%%   We are waiting for a confirmation from the winner that the autoheal
%%   process has ended. Meanwhile we can ignore autoheal requests.
%%   Because we may be a loser too, this state is saved to the application
%%   environment and restored on startup.
%%
%% restarting
%%   - we are restarting. Of course the node monitor immediately dies
%%   then so this state does not last long. We therefore send the
%%   autoheal_safe_to_start message to the rabbit_outside_app_process
%%   instead.
%%
%% == Message flow ==
%%
%% 1. Any node (leader included) >> {request_start, node()} >> Leader
%%      When Mnesia detects it is running partitioned or
%%      when a remote node starts, rabbit_node_monitor calls
%%      rabbit_autoheal:maybe_start/1. The message above is sent to the
%%      leader so the leader can take a decision.
%%
%% 2. Leader >> {become_winner, Losers} >> Winner
%%      The leader notifies the winner so the latter can proceed with
%%      the autoheal.
%%
%% 3. Winner >> {winner_is, Winner} >> All losers
%%      The winner notifies losers they must stop.
%%
%% 4. Winner >> autoheal_safe_to_start >> All losers
%%      When either all losers stopped or the autoheal process was
%%      aborted, the winner notifies losers they can start again.
%%
%% 5. Leader >> report_autoheal_status >> Winner
%%      The leader asks the autoheal status to the winner. This only
%%      happens when the leader is a loser too. If this is not the case,
%%      this message is never sent.
%%
%% 6. Winner >> {autoheal_finished, Winner} >> Leader
%%      The winner notifies the leader that the autoheal process was
%%      either finished or aborted (ie. autoheal_safe_to_start was sent
%%      to losers).

%%----------------------------------------------------------------------------

init() ->
    %% We check the application environment for a saved autoheal state
    %% saved during a restart. If this node is a leader, it is used
    %% to determine if it needs to ask the winner to report about the
    %% autoheal progress.
    State = case application:get_env(rabbit, ?AUTOHEAL_STATE_AFTER_RESTART) of
        {ok, S}   -> S;
        undefined -> not_healing
    end,
    ok = application:unset_env(rabbit, ?AUTOHEAL_STATE_AFTER_RESTART),
    case State of
        {leader_waiting, Winner, _} ->
            rabbit_log:info(
              "Autoheal: in progress, requesting report from ~p~n", [Winner]),
            send(Winner, report_autoheal_status);
        _ ->
            ok
    end,
    State.

maybe_start(not_healing) ->
    case enabled() of
        true  -> Leader = leader(),
                 send(Leader, {request_start, node()}),
                 rabbit_log:info("Autoheal request sent to ~p~n", [Leader]),
                 not_healing;
        false -> not_healing
    end;
maybe_start(State) ->
    State.

enabled() ->
    case application:get_env(rabbit, cluster_partition_handling) of
        {ok, autoheal}                         -> true;
        {ok, {pause_if_all_down, _, autoheal}} -> true;
        _                                      -> false
    end.

leader() ->
    [Leader | _] = lists:usort(rabbit_mnesia:cluster_nodes(all)),
    Leader.

%% This is the winner receiving its last notification that a node has
%% stopped - all nodes can now start again
rabbit_down(Node, {winner_waiting, [Node], Notify}) ->
    rabbit_log:info("Autoheal: final node has stopped, starting...~n",[]),
    winner_finish(Notify);

rabbit_down(Node, {winner_waiting, WaitFor, Notify}) ->
    {winner_waiting, WaitFor -- [Node], Notify};

rabbit_down(Winner, {leader_waiting, Winner, Losers}) ->
    abort([Winner], Losers);

rabbit_down(_Node, State) ->
    %% Ignore. Either:
    %%     o  we already cancelled the autoheal process;
    %%     o  we are still waiting the winner's report.
    State.

node_down(_Node, not_healing) ->
    not_healing;

node_down(Node, {winner_waiting, _, Notify}) ->
    abort([Node], Notify);

node_down(Node, _State) ->
    rabbit_log:info("Autoheal: aborting - ~p went down~n", [Node]),
    not_healing.

%% By receiving this message we become the leader
%% TODO should we try to debounce this?
handle_msg({request_start, Node},
           not_healing, Partitions) ->
    rabbit_log:info("Autoheal request received from ~p~n", [Node]),
    case check_other_nodes(Partitions) of
        {error, E} ->
            rabbit_log:info("Autoheal request denied: ~s~n", [fmt_error(E)]),
            not_healing;
        {ok, AllPartitions} ->
            {Winner, Losers} = make_decision(AllPartitions),
            rabbit_log:info("Autoheal decision~n"
                            "  * Partitions: ~p~n"
                            "  * Winner:     ~p~n"
                            "  * Losers:     ~p~n",
                            [AllPartitions, Winner, Losers]),
            case node() =:= Winner of
                true  -> handle_msg({become_winner, Losers},
                                    not_healing, Partitions);
                false -> send(Winner, {become_winner, Losers}),
                         {leader_waiting, Winner, Losers}
            end
    end;

handle_msg({request_start, Node},
           State, _Partitions) ->
    rabbit_log:info("Autoheal request received from ~p when healing; "
                    "ignoring~n", [Node]),
    State;

handle_msg({become_winner, Losers},
           not_healing, _Partitions) ->
    rabbit_log:info("Autoheal: I am the winner, waiting for ~p to stop~n",
                    [Losers]),
    %% The leader said everything was ready - do we agree? If not then
    %% give up.
    Down = Losers -- rabbit_node_monitor:alive_rabbit_nodes(Losers),
    case Down of
        [] -> [send(L, {winner_is, node()}) || L <- Losers],
              {winner_waiting, Losers, Losers};
        _  -> abort(Down, Losers)
    end;

handle_msg({winner_is, Winner}, State = not_healing,
           _Partitions) ->
    %% This node is a loser, nothing else.
    restart_loser(State, Winner),
    restarting;
handle_msg({winner_is, Winner}, State = {leader_waiting, Winner, _},
           _Partitions) ->
    %% This node is the leader and a loser at the same time.
    restart_loser(State, Winner),
    restarting;

handle_msg(_, restarting, _Partitions) ->
    %% ignore, we can contribute no further
    restarting;

handle_msg(report_autoheal_status, not_healing, _Partitions) ->
    %% The leader is asking about the autoheal status to us (the
    %% winner). This happens when the leader is a loser and it just
    %% restarted. We are in the "not_healing" state, so the previous
    %% autoheal process ended: let's tell this to the leader.
    send(leader(), {autoheal_finished, node()}),
    not_healing;

handle_msg(report_autoheal_status, State, _Partitions) ->
    %% Like above, the leader is asking about the autoheal status. We
    %% are not finished with it. There is no need to send anything yet
    %% to the leader: we will send the notification when it is over.
    State;

handle_msg({autoheal_finished, Winner},
           {leader_waiting, Winner, _}, _Partitions) ->
    %% The winner is finished with the autoheal process and notified us
    %% (the leader). We can transition to the "not_healing" state and
    %% accept new requests.
    rabbit_log:info("Autoheal finished according to winner ~p~n", [Winner]),
    not_healing;

handle_msg({autoheal_finished, Winner}, not_healing, _Partitions)
           when Winner =:= node() ->
    %% We are the leader and the winner. The state already transitioned
    %% to "not_healing" at the end of the autoheal process.
    rabbit_log:info("Autoheal finished according to winner ~p~n", [node()]),
    not_healing.

%%----------------------------------------------------------------------------

send(Node, Msg) -> {?SERVER, Node} ! {autoheal_msg, Msg}.

abort(Down, Notify) ->
    rabbit_log:info("Autoheal: aborting - ~p down~n", [Down]),
    %% Make sure any nodes waiting for us start - it won't necessarily
    %% heal the partition but at least they won't get stuck.
    winner_finish(Notify).

winner_finish(Notify) ->
    %% There is a race in Mnesia causing a starting loser to hang
    %% forever if another loser stops at the same time: the starting
    %% node connects to the other node, negotiates the protocol and
    %% attempts to acquire a write lock on the schema on the other node.
    %% If the other node stops between the protocol negotiation and lock
    %% request, the starting node never gets an answer to its lock
    %% request.
    %%
    %% To work around the problem, we make sure Mnesia is stopped on all
    %% losing nodes before sending the "autoheal_safe_to_start" signal.
    wait_for_mnesia_shutdown(Notify),
    [{rabbit_outside_app_process, N} ! autoheal_safe_to_start || N <- Notify],
    send(leader(), {autoheal_finished, node()}),
    not_healing.

wait_for_mnesia_shutdown([Node | Rest] = AllNodes) ->
    case rpc:call(Node, mnesia, system_info, [is_running]) of
        no ->
            wait_for_mnesia_shutdown(Rest);
        Running when
        Running =:= yes orelse
        Running =:= starting orelse
        Running =:= stopping ->
            timer:sleep(?MNESIA_STOPPED_PING_INTERNAL),
            wait_for_mnesia_shutdown(AllNodes);
        _ ->
            wait_for_mnesia_shutdown(Rest)
    end;
wait_for_mnesia_shutdown([]) ->
    ok.

restart_loser(State, Winner) ->
    rabbit_log:warning(
      "Autoheal: we were selected to restart; winner is ~p~n", [Winner]),
    rabbit_node_monitor:run_outside_applications(
      fun () ->
              MRef = erlang:monitor(process, {?SERVER, Winner}),
              rabbit:stop(),
              NextState = receive
                  {'DOWN', MRef, process, {?SERVER, Winner}, _Reason} ->
                      not_healing;
                  autoheal_safe_to_start ->
                      State
              end,
              erlang:demonitor(MRef, [flush]),
              %% During the restart, the autoheal state is lost so we
              %% store it in the application environment temporarily so
              %% init/0 can pick it up.
              %%
              %% This is useful to the leader which is a loser at the
              %% same time: because the leader is restarting, there
              %% is a great chance it misses the "autoheal finished!"
              %% notification from the winner. Thanks to the saved
              %% state, it knows it needs to ask the winner if the
              %% autoheal process is finished or not.
              application:set_env(rabbit,
                ?AUTOHEAL_STATE_AFTER_RESTART, NextState),
              rabbit:start()
      end, true).

make_decision(AllPartitions) ->
    Sorted = lists:sort([{partition_value(P), P} || P <- AllPartitions]),
    [[Winner | _] | Rest] = lists:reverse([P || {_, P} <- Sorted]),
    {Winner, lists:append(Rest)}.

partition_value(Partition) ->
    Connections = [Res || Node <- Partition,
                          Res <- [rpc:call(Node, rabbit_networking,
                                           connections_local, [])],
                          is_list(Res)],
    {length(lists:append(Connections)), length(Partition)}.

%% We have our local understanding of what partitions exist; but we
%% only know which nodes we have been partitioned from, not which
%% nodes are partitioned from each other.
check_other_nodes(LocalPartitions) ->
    Nodes = rabbit_mnesia:cluster_nodes(all),
    {Results, Bad} = rabbit_node_monitor:status(Nodes -- [node()]),
    RemotePartitions = [{Node, proplists:get_value(partitions, Res)}
                        || {Node, Res} <- Results],
    RemoteDown = [{Node, Down}
                  || {Node, Res} <- Results,
                     Down <- [Nodes -- proplists:get_value(nodes, Res)],
                     Down =/= []],
    case {Bad, RemoteDown} of
        {[], []} -> Partitions = [{node(), LocalPartitions} | RemotePartitions],
                    {ok, all_partitions(Partitions, [Nodes])};
        {[], _}  -> {error, {remote_down, RemoteDown}};
        {_,  _}  -> {error, {nodes_down, Bad}}
    end.

all_partitions([], Partitions) ->
    Partitions;
all_partitions([{Node, CantSee} | Rest], Partitions) ->
    {[Containing], Others} =
        lists:partition(fun (Part) -> lists:member(Node, Part) end, Partitions),
    A = Containing -- CantSee,
    B = Containing -- A,
    Partitions1 = case {A, B} of
                      {[], _}  -> Partitions;
                      {_,  []} -> Partitions;
                      _        -> [A, B | Others]
                  end,
    all_partitions(Rest, Partitions1).

fmt_error({remote_down, RemoteDown}) ->
    rabbit_misc:format("Remote nodes disconnected:~n ~p", [RemoteDown]);
fmt_error({nodes_down, NodesDown}) ->
    rabbit_misc:format("Local nodes down: ~p", [NodesDown]).
