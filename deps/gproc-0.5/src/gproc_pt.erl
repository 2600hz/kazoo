%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%% @author Ulf Wiger <ulf@wiger.net>
%% @author Dmitry Demeshchuk <demeshchuk@gmail.com>
%%
%% @doc Parse transform utility for gproc users.
%%
%% This module provides some closer syntactical integration for
%% people who are enthusiastic gproc users.
%%
%% Specifically, this module transforms `Pid ! Msg' into
%% `gproc:send(Pid, Msg)', which, apart from accepting any type for
%% `Pid' that `!' understands, is also able to handle a gproc "triple",
%% e.g. `{n, l, Name}' or even `{p, l, Prop}' (in the latter case, the
%% message may be delivered to multiple recipients).
%%
%% Users should be aware that parse transforms may be confusing to
%% the casual reader, since they extend the semantics of possibly
%% ubiquitous constructs (as is the case with this transform). Therefore,
%% you should document clearly that this is happening.
%%
%% Original suggestion by Dimitry Demeschuk.
%% @end
%%
-module(gproc_pt).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    do_transform(Forms).

do_transform([{op, L, '!', Lhs, Rhs}|Fs]) ->
    [NewLhs] = do_transform([Lhs]),
    [NewRhs] = do_transform([Rhs]),
    [{call, L, {remote, L, {atom, L, gproc}, {atom, L, send}},
        [NewLhs, NewRhs]} | do_transform(Fs)];
do_transform([]) ->
    [];
do_transform([F|Fs]) when is_tuple(F) ->
    [list_to_tuple(do_transform(tuple_to_list(F))) | do_transform(Fs)];
do_transform([F|Fs]) ->
    [do_transform(F) | do_transform(Fs)];
do_transform(F) ->
    F.
