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

-module(ets_counter).
-include_lib("proper/include/proper.hrl").

-compile(export_all).
-define(KEYS, lists:seq(1,10)).

ets_inc(Key, Inc) ->
    case ets:lookup(counter, Key) of
	[] ->
	    erlang:yield(),
	    ets:insert(counter, {Key,Inc}),
	    Inc;
	[{Key,OldValue}] ->
	    NewValue = OldValue + Inc,
	    ets:insert(counter, {Key,NewValue}),
	    NewValue
    end.

prop_ets_counter() ->
    ?FORALL(Commands, parallel_commands(?MODULE),
	    begin
		set_up(),
		{Seq,P,Result} = run_parallel_commands(?MODULE, Commands),
		clean_up(),
		?WHENFAIL(io:format("Seq: ~w\nPar: ~w\nRes: ~w\n",
				    [Seq, P, Result]),
			  Result =:= ok)
	    end).

set_up() ->
    ets:new(counter, [public, named_table]).

clean_up() ->
    catch ets:delete(counter).

key() ->
    elements(?KEYS).

initial_state() -> [].

precondition(_S, _C) ->
    true.

command(_S) ->
    {call,?MODULE,ets_inc,[key(),non_neg_integer()]}.

postcondition(S, {call,_,ets_inc,[Key, Inc]}, Res) ->
    case proplists:is_defined(Key, S) of
	true ->
	    OldValue = proplists:get_value(Key, S),
	    Res =:= OldValue + Inc;
	false ->
	    Res =:= Inc
    end.

next_state(S, _Res, {call,_,ets_inc,[Key, Inc]}) ->
    case proplists:is_defined(Key, S) of
	 true ->
	     OldValue = proplists:get_value(Key, S),
	     NewValue = OldValue + Inc,
	     [{Key,NewValue}|proplists:delete(Key, S)];
	 false ->
	     [{Key,Inc}|S]
    end.
