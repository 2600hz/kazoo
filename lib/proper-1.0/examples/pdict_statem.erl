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
%%% @author Kresten Krab Thorup, edited by Eirini Arvaniti
%%% @doc Simple statem test for the process dictionary

-module(pdict_statem).
-behaviour(proper_statem).

-export([test/0, test/1]).
-export([initial_state/0, command/1, precondition/2, postcondition/3,
	 next_state/3]).

-include_lib("proper/include/proper.hrl").

-define(KEYS, [a,b,c,d]).

%% A simple statem test for the process dictionary; tests the
%% operations erlang:put/2, erlang:get/1, erlang:erase/1.

test() ->
    test(100).

test(N) ->
    proper:quickcheck(?MODULE:prop_pdict(), N).

prop_pdict() ->
    ?FORALL(Cmds, commands(?MODULE),
	    begin
		{H,S,Res} = run_commands(?MODULE, Cmds),
		clean_up(),
		?WHENFAIL(
		   io:format("History: ~w\nState: ~w\nRes: ~w\n",
			     [H, S, Res]),
		   aggregate(command_names(Cmds), Res =:= ok))
	    end).

clean_up() ->
    lists:foreach(fun(Key) -> erlang:erase(Key) end, ?KEYS).

key() ->
    elements(?KEYS).

initial_state() -> [].

command([]) ->
    {call,erlang,put,[key(), integer()]};
command(Props) ->
    ?LET({Key,Value}, weighted_union([{2, elements(Props)},
				      {1, {key(),integer()}}]),
	 oneof([{call,erlang,put,[Key,Value]},
		{call,erlang,get,[Key]},
		{call,erlang,erase,[Key]}
	       ])).

precondition(_, {call,erlang,put,[_,_]}) ->
    true;
precondition(Props, {call,erlang,get,[Key]}) ->
    proplists:is_defined(Key, Props);
precondition(Props, {call,erlang,erase,[Key]}) ->
    proplists:is_defined(Key, Props);
precondition(_, _) ->
    false.

postcondition(Props, {call,erlang,put,[Key,_]}, undefined) ->
    not proplists:is_defined(Key, Props);
postcondition(Props, {call,erlang,put,[Key,_]}, Old) ->
    {Key,Old} =:= proplists:lookup(Key, Props);
postcondition(Props, {call,erlang,get,[Key]}, Val) ->
    {Key,Val} =:= proplists:lookup(Key, Props);
postcondition(Props, {call,erlang,erase,[Key]}, Val) ->
    {Key,Val} =:= proplists:lookup(Key, Props);
postcondition(_, _, _) ->
    false.

next_state(Props, _Var, {call,erlang,put,[Key,Value]}) ->
    %% correct model
    [{Key,Value}|proplists:delete(Key, Props)];
    %% wrong model
    %% Props ++ [{Key,Value}];
next_state(Props, _Var, {call,erlang,erase,[Key]}) ->
    proplists:delete(Key, Props);
next_state(Props, _Var, {call,erlang,get,[_]}) ->
    Props.
