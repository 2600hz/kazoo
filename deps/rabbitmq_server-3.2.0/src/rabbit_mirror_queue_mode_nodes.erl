%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License at
%% http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%% License for the specific language governing rights and limitations
%% under the License.
%%
%% The Original Code is RabbitMQ.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2010-2013 GoPivotal, Inc.  All rights reserved.
%%

-module(rabbit_mirror_queue_mode_nodes).

-include("rabbit.hrl").

-behaviour(rabbit_mirror_queue_mode).

-export([description/0, suggested_queue_nodes/5, validate_policy/1]).

-rabbit_boot_step({?MODULE,
                   [{description, "mirror mode nodes"},
                    {mfa,         {rabbit_registry, register,
                                   [ha_mode, <<"nodes">>, ?MODULE]}},
                    {requires,    rabbit_registry},
                    {enables,     kernel_ready}]}).

description() ->
    [{description, <<"Mirror queue to specified nodes">>}].

suggested_queue_nodes(Nodes0, MNode, _SNodes, SSNodes, Poss) ->
    Nodes1 = [list_to_atom(binary_to_list(Node)) || Node <- Nodes0],
    %% If the current master is not in the nodes specified, then what we want
    %% to do depends on whether there are any synchronised slaves. If there
    %% are then we can just kill the current master - the admin has asked for
    %% a migration and we should give it to them. If there are not however
    %% then we must keep the master around so as not to lose messages.
    Nodes = case SSNodes of
                [] -> lists:usort([MNode | Nodes1]);
                _  -> Nodes1
            end,
    Unavailable = Nodes -- Poss,
    Available = Nodes -- Unavailable,
    case Available of
        [] -> %% We have never heard of anything? Not much we can do but
              %% keep the master alive.
              {MNode, []};
        _  -> case lists:member(MNode, Available) of
                  true  -> {MNode, Available -- [MNode]};
                  false -> %% Make sure the new master is synced! In order to
                           %% get here SSNodes must not be empty.
                           [NewMNode | _] = SSNodes,
                           {NewMNode, Available -- [NewMNode]}
              end
    end.

validate_policy([]) ->
    {error, "ha-mode=\"nodes\" list must be non-empty", []};
validate_policy(Nodes) when is_list(Nodes) ->
    case [I || I <- Nodes, not is_binary(I)] of
        []      -> ok;
        Invalid -> {error, "ha-mode=\"nodes\" takes a list of strings, "
                    "~p was not a string", [Invalid]}
    end;
validate_policy(Params) ->
    {error, "ha-mode=\"nodes\" takes a list, ~p given", [Params]}.
