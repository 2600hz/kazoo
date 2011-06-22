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
-export([initial_state/0, initial_state/1, command/1, precondition/2,
	 postcondition/3, next_state/3]).
-export([set_up/0, clean_up/0]).

-include_lib("proper/include/proper.hrl").

-type object() :: tuple().
-type table_type() :: 'set' | 'ordered_set' | 'bag' | 'duplicate_bag'.

-record(state, {stored = []  :: [object()],     %% list of objects
		                                %% stored in ets table
		type   = set :: table_type()}). %% type of ets table

-define(TAB, table).
-define(INT_KEYS, lists:seq(0,10)).
-define(FLOAT_KEYS, [float(Key) || Key <- ?INT_KEYS]).


%%% Generators

key() -> frequency([{5, integer_key()},
		    {1, float_key()}]).

integer_key() ->
    elements(?INT_KEYS).

float_key() ->
    elements(?FLOAT_KEYS).

int_or_bin() ->
    frequency([{5, integer()}, {1, binary()}]).

object() ->
    oneof([{key(), int_or_bin()},
	   {key(), int_or_bin(), binary()},
	   {key(), int_or_bin(), binary(), binary()}]).

object(S) ->
    elements(S#state.stored).

key(S) ->
    ?LET(Object, object(S), element(1, Object)).

small_int() ->
    resize(10, integer()).


%%% Abstract state machine for ets table

initial_state(Type) ->
    #state{type = Type}.

initial_state() ->
    #state{}.

command(S) ->
    oneof([{call,ets,delete_object,[?TAB, object(S)]} || S#state.stored =/= []] ++
	  [{call,ets,delete,[?TAB, key(S)]} || S#state.stored =/= []] ++
	  [{call,ets,insert,[?TAB, object()]},
	   {call,ets,insert_new,[?TAB, object()]},
	   {call,ets,lookup,[?TAB,key()]}] ++
	  [{call,ets,update_counter,[?TAB,key(S),small_int()]}
	   || S#state.stored =/= [],
	      S#state.type =:= set orelse S#state.type =:= ordered_set]).

precondition(S, {call,_,update_counter,[?TAB,Key,_Incr]}) ->
    Object = case S#state.type of
		 set ->
		     proplists:lookup(Key, S#state.stored);
		 ordered_set ->
		     lists:keyfind(Key, 1, S#state.stored)
	     end,
    is_tuple(Object) andalso is_integer(element(2, Object));
precondition(_S, {call,_,_,_}) ->
    true.

next_state(S, _V, {call,_,update_counter,[?TAB,Key,Incr]}) ->
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
	    S#state{stored = lists:keyreplace(Key, 1, S#state.stored, NewObj)}
    end;
next_state(S, _V, {call,_,insert,[?TAB,Object]}) ->
    case S#state.type of
	set ->
	    Key = element(1, Object),
	    case proplists:is_defined(Key, S#state.stored) of
		false ->
		    S#state{stored = [Object|S#state.stored]};
		true ->
		    S#state{stored = keyreplace(Key, 1, S#state.stored, Object)}
	    end;
	ordered_set ->
	    Key = element(1, Object),
	    case lists:keymember(Key, 1, S#state.stored) of
		false ->
		    S#state{stored = [Object|S#state.stored]};
		true ->
		    S#state{stored = lists:keyreplace(Key, 1, S#state.stored, Object)}
	    end;
	bag ->
	    case lists:member(Object, S#state.stored) of
		false ->
		    S#state{stored = [Object|S#state.stored]};
		true ->
		    S
	    end;
	duplicate_bag ->
	    S#state{stored = [Object|S#state.stored]}
    end;
next_state(S, _V, {call,_,insert_new,[?TAB,Object]}) ->
    Key = element(1, Object),
    case S#state.type of
	ordered_set ->
	    case lists:keymember(Key, 1, S#state.stored) of
		false ->
		    S#state{stored = [Object|S#state.stored]};
		true ->
		    S
	    end;
  	_ ->
	    case proplists:is_defined(Key, S#state.stored) of
		false ->
		    S#state{stored = [Object|S#state.stored]};
		true ->
		    S
	    end
    end;
next_state(S, _V, {call,_,delete_object,[?TAB,Object]}) ->
    case S#state.type of
	duplicate_bag ->
	    S#state{stored = delete_all(Object, S#state.stored)};
	_ ->
	    S#state{stored = lists:delete(Object, S#state.stored)}
    end;
next_state(S, _V, {call,_,delete,[?TAB,Key]}) ->
    case S#state.type of
	ordered_set ->
	    S#state{stored = lists:keydelete(Key, 1, S#state.stored)};
	_ ->
	    S#state{stored = proplists:delete(Key, S#state.stored)}
    end;
next_state(S, _V, {call,_,_,_}) -> S.

postcondition(S, {call,_,update_counter,[?TAB,Key,Incr]}, Res) ->
    Object = case S#state.type of
		 set ->
		     proplists:lookup(Key, S#state.stored);
		 ordered_set ->
		     lists:keyfind(Key, 1, S#state.stored)
	     end,
    Value = element(2, Object),
    Res =:= Value + Incr;
postcondition(_S, {call,_,delete_object,[?TAB,_Object]}, Res) ->
    Res =:= true;
postcondition(_S, {call,_,delete,[?TAB,_Key]}, Res) ->
    Res =:= true;
postcondition(_S, {call,_,insert,[?TAB,_Object]}, Res) ->
    Res =:= true;
postcondition(S, {call,_,insert_new,[?TAB,Object]}, Res) ->
    Key = element(1, Object),
    case S#state.type of
	ordered_set ->
	    Res =:= not lists:keymember(Key, 1, S#state.stored);
	_ ->
	    Res =:= not proplists:is_defined(Key, S#state.stored)
    end;
postcondition(S, {call,_,lookup,[?TAB,Key]}, []) ->
    case S#state.type of
	ordered_set ->
	    not lists:keymember(Key, 1, S#state.stored);
	_ ->
	    not proplists:is_defined(Key, S#state.stored)
    end;
postcondition(S, {call,_,lookup,[?TAB,Key]}, Res) ->
    case S#state.type of
	set ->
	    Res =:= proplists:lookup_all(Key, S#state.stored);
	ordered_set ->
	    Res =:= [lists:keyfind(Key, 1, S#state.stored)];
	_ ->
	    Res =:= lists:reverse(proplists:lookup_all(Key, S#state.stored))
    end.


%%% Sample properties

prop_ets() ->
    ?FORALL(Type, noshrink(table_type()),
        ?FORALL(Cmds, commands(?MODULE, initial_state(Type)),
	    begin
		catch ets:delete(?TAB),
		?TAB = ets:new(?TAB, [Type, public, named_table]),
		{H,S,Res} = run_commands(?MODULE, Cmds),
		?WHENFAIL(
		   io:format("History: ~p\nState: ~p\nRes: ~p\n", [H,S,Res]),
		   collect(Type, Res =:= ok))
	    end)).

prop_parallel_ets() ->
    ?FORALL(Type, noshrink(table_type()),
        ?FORALL(Cmds, parallel_commands(?MODULE, initial_state(Type)),
	    begin
		catch ets:delete(?TAB),
		?TAB = ets:new(?TAB, [Type, public, named_table]),
		{Seq,P,Res} = run_parallel_commands(?MODULE, Cmds),
		?WHENFAIL(
		   io:format("Sequential: ~p\nParallel: ~p\nRes: ~p\n",
			     [Seq,P,Res]),
		   collect(Type, Res =:= ok))
	    end)).


%%% Utility Functions

set_up() ->
    catch ets:delete(?TAB),
    Type = lists:nth(proper_arith:rand_int(4),
		     [set, ordered_set, bag, duplicate_bag]),
    ?TAB = ets:new(?TAB, [Type, public, named_table]).

clean_up() -> ok.

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

delete_all(X, List) ->
    delete_all(X, List, []).

delete_all(_X, [], Acc) ->
    lists:reverse(Acc);
delete_all(X, [H|T], Acc) ->
    case X =:= H of
	true -> delete_all(X, T, Acc);
	false -> delete_all(X, T, [H|Acc])
    end.
