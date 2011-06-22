%%% Copyright 2010-2011 Manolis Papadakis <manopapad@gmail.com>,
%%%                     Eirini Arvaniti <eirinibob@gmail.com>
%%%                 and Kostis Sagonas <kostis@cs.ntua.gr>
%%%
%%% This file is part of PropEr.
%%%
%%% PropEr is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% PropEr is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with PropEr.  If not, see <http://www.gnu.org/licenses/>.

%%% @copyright 2010-2011 Manolis Papadakis, Eirini Arvaniti and Kostis Sagonas
%%% @version {@version}
%%% @author Eirini Arvaniti

-module(symb_statem).
-compile(export_all).

-include_lib("proper/include/proper.hrl").

-record(state, {foo = [],
		bar = []}).

initial_state() ->
    #state{}.

command(_S) ->
    oneof([{call,?MODULE,foo,[integer()]},
	   {call,?MODULE,bar,[integer()]}]).

precondition(_, _) ->
    true.

next_state(S = #state{foo=Foo}, V, {call,_,foo,[_Arg]}) ->
    V1 = {call,erlang,element,[1,V]},
    S#state{foo = [V1|Foo]};
next_state(S = #state{bar=Bar}, V, {call,_,bar,[_Arg]}) ->
    V1 = {call,erlang,hd,[V]},
    S#state{foo = [V1|Bar]}.

postcondition(S, {call,_,foo,[_Arg]}, Res) when is_tuple(Res) ->
    lists:all(fun is_integer/1, S#state.foo);
postcondition(S, {call,_,bar,[_Arg]}, Res) when is_list(Res) ->
    lists:all(fun is_integer/1, S#state.bar);
postcondition(_, _, _) ->
    false.

foo(I) when is_integer(I) ->
    erlang:make_tuple(3, I).

bar(I) when is_integer(I) ->
    lists:duplicate(3, I).

prop_simple() ->
    ?FORALL(Cmds, commands(?MODULE),
	    begin
		{H,S,Res} = run_commands(?MODULE, Cmds),
		?WHENFAIL(
		   io:format("H: ~w\nState: ~w\n:Res: ~w\n", [H,S,Res]),
		   Res =:= ok)
	    end).

prop_parallel_simple() ->
    ?FORALL(Cmds, parallel_commands(?MODULE),
	    begin
		{S,P,Res} = run_parallel_commands(?MODULE, Cmds),
		?WHENFAIL(
		   io:format("Seq: ~w\nParallel: ~w\n:Res: ~w\n", [S,P,Res]),
		   Res =:= ok)
	    end).
