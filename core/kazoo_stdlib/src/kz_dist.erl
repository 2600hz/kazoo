%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_dist).

-export([listen/1, accept/1, accept_connection/5,
         setup/5, close/1, select/1]).

-spec listen(atom()) -> {error, any()} | {ok, term()}.
listen(Name) ->
    inet_tcp_dist:listen(Name).

-spec select(atom()) -> boolean().
select(FullNode) ->
    case split_node(atom_to_list(FullNode), $@, []) of
        [Node, _Host]
          when Node =:= "freeswitch" ->
            inet_tcp_dist:select(FullNode);
        _Else ->
            false
    end.

-spec accept(term()) -> pid().
accept(Listen) ->
    inet_tcp_dist:accept(Listen).

-spec accept_connection(pid(), term(), atom(), term(), term()) -> pid().
accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    inet_tcp_dist:accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime).

-spec setup(atom(), term(), atom(), term(), term()) -> pid().
setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    inet_tcp_dist:setup(Node, Type, MyNode, LongOrShortNames, SetupTime).

-spec close(term()) -> 'ok'.
close(Listen) ->
    inet_tcp_dist:close(Listen).

split_node([Chr|T], Chr, Ack) -> [lists:reverse(Ack)|split_node(T, Chr, [])];
split_node([H|T], Chr, Ack)   -> split_node(T, Chr, [H|Ack]);
split_node([], _, Ack)        -> [lists:reverse(Ack)].
