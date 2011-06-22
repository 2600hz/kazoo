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
%%% @doc Simple statem test for ets tables

-module(ets_statem).
-behaviour(proper_statem).

-export([initial_state/0, initial_state/1, initial_state/2, command/1,
	 precondition/2, postcondition/3, next_state/3]).
-export([sample_commands/0]).

-include_lib("proper/include/proper.hrl").

-type object()     :: tuple().
-type table_type() :: 'set' | 'ordered_set' | 'bag' | 'duplicate_bag'.

-record(state, {tids   = []  :: [ets:tid()],
		stored = []  :: [object()],      %% list of objects stored in
		                                 %% ets table
		type   = set :: table_type()}).  %% type of ets table

-define(INT_KEYS, lists:seq(0, 2)).
-define(FLOAT_KEYS, [float(Key) || Key <- ?INT_KEYS]).


%%% Generators

key() -> frequency([{2, elements(?INT_KEYS)},
		    {1, elements(?FLOAT_KEYS)}]).

value() ->
    frequency([{5, int()},
	       {1, elements([a, b, c, d])}]).

object() ->
    {key(), value()}.

object(S) ->
    elements(S#state.stored).

key(S) ->
    ?LET(Object, object(S), element(1, Object)).

tid(S) ->
    elements(S#state.tids).


%%% Abstract state machine for ets table

initial_state() ->
    #state{type = set}.

initial_state(Type) ->
    #state{type = Type}.

initial_state(Type, parallel) ->
    #state{tids = [tab], type = Type}.

command(#state{tids = [], type = Type}) ->
    {call,ets,new,[tab, [Type]]};
command(S) ->
    oneof([{call,ets,insert,[tid(S), object()]},
	   {call,ets,delete,[tid(S), key()]}] ++
	  [{call,ets,lookup_element,[tid(S), key(S), range(1, 2)]}
	   || S#state.stored =/= []] ++
	  [{call,ets,update_counter,[tid(S), key(S), int()]}
	   || S#state.stored =/= [],
	      S#state.type =:= set orelse  S#state.type =:= ordered_set]).

precondition(S, {call,_,lookup_element,[_, Key, _]}) ->
    proplists:is_defined(Key, S#state.stored);
precondition(S, {call,_,update_counter,[_, Key, _Incr]}) ->
    proplists:is_defined(Key, S#state.stored) andalso
	case S#state.type of
	    set ->
		Obj = proplists:lookup(Key, S#state.stored),
		is_integer(element(2, Obj));
	    ordered_set ->
		Obj = lists:keyfind(Key, 1, S#state.stored),
		is_integer(element(2, Obj));
	    _ ->
		false
	end;
precondition(_S, {call,_,_,_}) ->
    true.

next_state(S, V, {call,_,new,[_Tab, _Opts]}) ->
    S#state{tids = [V|S#state.tids]};
next_state(S, _V, {call,_,update_counter,[_Tab, Key, Incr]}) ->
    case S#state.type of
	set ->
	    Object = proplists:lookup(Key, S#state.stored),
	    Value = element(2, Object),
	    NewObj =  setelement(2, Object, Value + Incr),
	    S#state{stored=keyreplace(Key, 1, S#state.stored, NewObj)};
	ordered_set ->
	    Object = lists:keyfind(Key, 1, S#state.stored),
	    Value = element(2, Object),
	    NewObj = setelement(2, Object, Value + Incr),
	    S#state{stored=lists:keyreplace(Key, 1, S#state.stored, NewObj)}
    end;
next_state(S, _V, {call,_,insert,[_Tab, Object]}) ->
    case S#state.type of
	set ->
	    Key = element(1, Object),
	    case proplists:is_defined(Key, S#state.stored) of
		false ->
		    S#state{stored = S#state.stored ++ [Object]};
		true ->
		    %% correct model
		    S#state{stored=keyreplace(Key, 1, S#state.stored, Object)}
		    %% error model, run {numtests, 3000} to discover the bug
		    %% S#state{stored=lists:keyreplace(Key, 1, S#state.stored,
		    %% 				    Object)}
	    end;
	ordered_set ->
	    Key = element(1, Object),
	    case lists:keymember(Key, 1, S#state.stored) of
		false ->
		    S#state{stored = S#state.stored ++ [Object]};
		true ->
		    S#state{stored=lists:keyreplace(Key, 1, S#state.stored,
						    Object)}
	    end;
	bag ->
	    case lists:member(Object, S#state.stored) of
		false ->
		    S#state{stored = S#state.stored ++ [Object]};
		true ->
		    S
	    end;
	duplicate_bag ->
	    S#state{stored = S#state.stored ++ [Object]}
    end;
next_state(S, _V, {call,_,delete,[_Tab, Key]}) ->
    case S#state.type of
	ordered_set ->
	    S#state{stored=lists:keydelete(Key, 1, S#state.stored)};
	_ ->
	    S#state{stored=proplists:delete(Key, S#state.stored)}
    end;
next_state(S, _V, {call,_,_,_}) -> S.

postcondition(_S, {call,_,new,[_Tab, _Opts]}, _Res) ->
    true;
postcondition(S, {call,_,update_counter,[_Tab, Key, Incr]}, Res) ->
    Object = case S#state.type of
		 set ->
		     proplists:lookup(Key, S#state.stored);
		 ordered_set ->
		     lists:keyfind(Key, 1, S#state.stored)
	     end,
    Value = element(2, Object),
    Res =:= Value + Incr;
postcondition(_S, {call,_,delete,[_Tab, _Key]}, Res) ->
    Res =:= true;
postcondition(_S, {call,_,insert,[_Tab, _Object]}, Res) ->
    Res =:= true;
postcondition(S, {call,_,lookup_element,[_Tab, Key, Pos]}, Res) ->
    case S#state.type of
	ordered_set ->
	    Res =:= element(Pos, lists:keyfind(Key, 1, S#state.stored));
	set ->
	    Res =:= element(Pos, proplists:lookup(Key, S#state.stored));
	_ ->
	    Res =:= [element(Pos, Tuple)
		     || Tuple <- proplists:lookup_all(Key, S#state.stored)]
    end.


%%% Sample properties

prop_ets() ->
    ?FORALL(Type, noshrink(table_type()),
        ?FORALL(Cmds, commands(?MODULE, initial_state(Type)),
		begin
		    {H,S,Res} = run_commands(?MODULE, Cmds),
		    [ets:delete(Tab) || Tab <- S#state.tids],
		    ?WHENFAIL(
		       io:format("History: ~p\nState: ~p\nRes: ~p\n", [H,S,Res]),
		       collect(Type, Res =:= ok))
		end)).

prop_parallel_ets() ->
    ?FORALL(Type, noshrink(table_type()),
        ?FORALL(Cmds, commands(?MODULE, initial_state(Type, parallel)),
		begin
		    ets:new(tab, [named_table, public, Type]),
		    {Seq,P,Res} = run_commands(?MODULE, Cmds),
		    ets:delete(tab),
		    ?WHENFAIL(
		       io:format("Sequential: ~p\nParallel: ~p\nRes: ~p\n",
				 [Seq,P,Res]),
		       collect(Type, Res =:= ok))
		end)).

%%% Demo commands

sample_commands() ->
    proper_gen:sample(
      ?LET(Type, oneof([set, ordered_set, bag, duplicate_bag]),
	   commands(?MODULE, initial_state(Type)))).


%%% Utility Functions

keyreplace(Key, Pos, List, NewTuple) ->
    keyreplace(Key, Pos, List, NewTuple, []).

keyreplace(_Key, _Pos, [], _NewTuple, Acc) ->
    lists:reverse(Acc);
keyreplace(Key, Pos, [Tuple|Rest], NewTuple, Acc) ->
    case element(Pos, Tuple) =:= Key of
	true ->
	    lists:reverse(Acc) ++ [NewTuple|Rest];
	false ->
	    keyreplace(Key, Pos, Rest, NewTuple, [Tuple|Acc])
    end.
