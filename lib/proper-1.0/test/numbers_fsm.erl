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
%%% @doc Tests for fsm transition targets

-module(numbers_fsm).
-compile(export_all).

-include_lib("proper/include/proper.hrl").

-define(STATES, [zero, one, two, three, four]).
-define(KEYS, [a,b,c,d,e,f]).
-define(LOOKUP, [{zero,0}, {one,1}, {two,2}, {three,3}, {four,4}]).


%%% Fsm callbacks

zero(S) ->
    idle_transition() ++
	[{four, {call,?MODULE,dec,[]}},
	 {one, {call,?MODULE,inc,[]}},
	 {history, {call,?MODULE,insert,[key()]}},
	 {history, {call,?MODULE,delete,[key(S)]}}].

one(S) ->
    idle_transition() ++
	[{zero, {call,?MODULE,dec,[]}},
	 {two, {call,?MODULE,inc,[]}},
	 {history, {call,?MODULE,insert,[key()]}},
	 {history, {call,?MODULE,delete,[key(S)]}}].

two(S) ->
    idle_transition() ++
	[{one, {call,?MODULE,dec,[]}},
	 {three, {call,?MODULE,inc,[]}},
	 {history, {call,?MODULE,insert,[key()]}},
	 {history, {call,?MODULE,delete,[key(S)]}}].

three(S) ->
    idle_transition() ++
	[{two, {call,?MODULE,dec,[]}},
	 {four, {call,?MODULE,inc,[]}},
	 {history, {call,?MODULE,insert,[key()]}},
	 {history, {call,?MODULE,delete,[key(S)]}}].

four(S) ->
     idle_transition() ++
	[{three, {call,?MODULE,dec,[]}},
	 {zero, {call,?MODULE,inc,[]}},
	 {history, {call,?MODULE,insert,[key()]}},
	 {history, {call,?MODULE,delete,[key(S)]}}].

num(N, _, _, _S) ->
    idle_transition() ++
	[{{num,N+1,dummy,dummy}, {call,?MODULE,inc,[]}} || N < 4] ++
	[{{num,N-1,dummy,dummy}, {call,?MODULE,dec,[]}} || N > 0] ++
	[{{num,0,dummy,dummy}, {call,?MODULE,inc,[]}} || N =:= 4] ++
	[{{num,4,dummy,dummy}, {call,?MODULE,dec,[]}} || N =:= 0].

idle_transition() ->
    [{history, {call,?MODULE,idle,[]}}].

initial_state() -> zero.
initial_state_data() -> [].

precondition(_, _, S, {call,_,delete,[Key]}) ->
    lists:member(Key, S);
precondition(zero, _To , _S, _Call) ->
    true;
precondition(_From, _To, S, {call,_,dec,_}) ->
    S =/= [];
precondition(_From, _To, S, {call,_,inc,_}) ->
    S =/= [];
precondition(_, _, _, _) ->
    true.

next_state_data(_, _, S, _, {call,_,insert,[Key]}) ->
    [Key|S];
next_state_data(_, _, S, _, {call,_,delete,[Key]}) ->
    lists:delete(Key, S);
next_state_data(_, _, S, _, _) ->
    S.

postcondition(_, _, _, _, _) ->
    true.

weight(_, _, {call,_,insert,_}) -> 2;
weight(_, _, _) -> 1.


%%% Generators

key() ->
    elements(?KEYS).

key(S) ->
    elements(S).

action() ->
    oneof([idle, inc, dec, foo]).

call() ->
    {call,?MODULE,action(),[]}.

mod(X, Y) when X > 0 -> X rem Y;
mod(X, Y) when X < 0 -> Y + X rem Y;
mod(0, _Y) -> 0.

mod_add(X, Y) -> mod(X+Y, 5).
mod_sub(X, Y) -> mod(X-Y, 5).

inc() -> ok.
idle() -> ok.
dec() -> ok.
insert(_) -> ok.
delete(_) -> ok.


%%% Properties

prop_target_states_atom() ->
    ?FORALL([From,Call], [elements(?STATES),call()],
	    begin
		Res = proper_fsm:target_states(?MODULE, From, [], Call),
		{call,_,Action,[]} = Call,
		Target = case Action of
			     idle ->
				 [history];
			     inc ->
				 Sum = mod_add(proplists:get_value(From, ?LOOKUP), 1),
				 [element(1, lists:keyfind(Sum, 2, ?LOOKUP))];
			     dec ->
				 Diff = mod_sub(proplists:get_value(From, ?LOOKUP), 1),
				 [element(1, lists:keyfind(Diff, 2, ?LOOKUP))];
			     foo ->
				 []
			 end,
		collect({From,Action}, Target =:= Res)
	    end).

prop_target_states_tuple() ->
    ?FORALL([From,Call], [{num,range(0,4),dummy,dummy},call()],
	    begin
		{num,N,_,_} = From,
		Res = proper_fsm:target_states(?MODULE, From, dummy, Call),
		{call,_,Action,[]} = Call,
		Target = case Action of
			     idle ->
				 [history];
			     inc ->
				 Sum = mod_add(N, 1),
				 [{num,Sum,dummy,dummy}];
			     dec ->
				 Diff = mod_sub(N, 1),
				 [{num,Diff,dummy,dummy}];
			     foo ->
				 []
			 end,
		collect({From,Action}, Target =:= Res)
	    end).
