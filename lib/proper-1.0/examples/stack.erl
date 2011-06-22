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
%%% @author Manolis Papadakis
%%% @doc Auto-ADT usage example: list-based implementation of a stack, with
%%%      element counting

-module(stack).
-export([is_empty/1, size/1, new/0, push/2, pop/1, safe_pop/1]).
-export_type([stack/1]).

-opaque stack(T) :: {non_neg_integer(),[T]}.

%% NOTE: You don't need to include the proper header if no properties are
%%	 declared in the module.
-include_lib("proper/include/proper.hrl").

%% NOTE: Every instance of the ADT in a spec must have variables as parameters.
%%	 When this would mean singleton variables, use variables starting with
%%	 an underscore.
-spec is_empty(stack(_T)) -> boolean().
is_empty({0, []}) ->
    true;
is_empty({_N, [_Top|_Rest]}) ->
    false.

-spec size(stack(_T)) -> non_neg_integer().
size({N, _Elems}) ->
    N.

-spec new() -> stack(_T).
new() ->
    {0, []}.

-spec push(T, stack(T)) -> stack(T).
push(X, {N,Elems}) ->
    {N+1, [X|Elems]}.

-spec pop(stack(T)) -> {T,stack(T)}.
pop({0, []}) ->
    throw(stack_empty);
pop({N, [Top|Rest]}) when N > 0 ->
    {Top, {N-1,Rest}}.

-spec safe_pop(stack(T)) -> {'ok',T,stack(T)} | 'error'.
safe_pop({0, []}) ->
    error;
safe_pop({N, [Top|Rest]}) when N > 0 ->
    {ok, Top, {N-1,Rest}}.

%%------------------------------------------------------------------------------
%% Properties
%%------------------------------------------------------------------------------

prop_push_pop() ->
    ?FORALL({X,S}, {integer(),stack(integer())},
	    begin
		{Y,_} = pop(push(X,S)),
		X =:= Y
	    end).
