%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(fs_dist).

-export([listen/1, accept/1, accept_connection/5,
         setup/5, close/1, select/1]).

listen(Name) ->
    inet_tcp_dist:listen(Name).

select(FullNode) ->
    [Node, _Host] = split_node(atom_to_list(FullNode), $@, []),
    case Node =:= "freeswitch" of
        true -> inet_tcp_dist:select(FullNode);
        false -> false
    end.

accept(_Listen) ->
    'ok'.
%% inet_tcp_dist:accept(Listen).

accept_connection(_AcceptPid, _Socket, _MyNode, _Allowed, _SetupTime) ->
    'ok'.
%% inet_tcp_dist:accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime).

setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    inet_tcp_dist:setup(Node, Type, MyNode, LongOrShortNames, SetupTime).

close(_Listen) ->
    ok.
%% inet_tcp_dist:close(Listen).

split_node([Chr|T], Chr, Ack) -> [lists:reverse(Ack)|split_node(T, Chr, [])];
split_node([H|T], Chr, Ack)   -> split_node(T, Chr, [H|Ack]);
split_node([], _, Ack)        -> [lists:reverse(Ack)].
