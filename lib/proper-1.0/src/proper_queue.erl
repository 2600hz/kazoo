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
%%% @doc Parametric wrapper to queue module.
%%% @private

-module(proper_queue).

-export([new/0,is_queue/1,is_empty/1,len/1,to_list/1,from_list/1,member/2]).
-export([in/2,in_r/2,out/1,out_r/1]).
-export([get/1,get_r/1,peek/1,peek_r/1,drop/1,drop_r/1]).
-export([reverse/1,join/2,split/2,filter/2]).
-export([cons/2,head/1,tail/1,snoc/2,last/1,daeh/1,init/1,liat/1,lait/1]).

-export_type([queue/1]).

%% When parsed by the typeserver, this becomes opaque (it's declared as a simple
%% type because dialyzer can't handle parametric opaque types yet).
-type queue(_T) :: queue().

%% This header is only included so that the strip_types parse transform will be
%% applied to this file as well.
-include("proper_internal.hrl").


%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

-spec new() -> queue(_T).
new() ->
    queue:new().

-spec is_queue(term()) -> boolean().
is_queue(X) ->
    queue:is_queue(X).

-spec is_empty(queue(_T)) -> boolean().
is_empty(Queue) ->
    queue:is_empty(Queue).

-spec len(queue(_T)) -> non_neg_integer().
len(Queue) ->
    queue:len(Queue).

-spec to_list(queue(T)) -> [T].
to_list(Queue) ->
    queue:to_list(Queue).

-spec from_list([T]) -> queue(T).
from_list(L) ->
    queue:from_list(L).

-spec member(T, queue(T)) -> boolean().
member(X, Queue) ->
    queue:member(X, Queue).

-spec in(T, queue(T)) -> queue(T).
in(X, Queue) ->
    queue:in(X, Queue).

-spec in_r(T, queue(T)) -> queue(T).
in_r(X, Queue) ->
    queue:in_r(X, Queue).

-spec out(queue(T)) -> {'empty' | {'value',T}, queue(T)}.
out(Queue) ->
    queue:out(Queue).

-spec out_r(queue(T)) -> {'empty' | {'value',T}, queue(T)}.
out_r(Queue) ->
    queue:out_r(Queue).

-spec get(queue(T)) -> T.
get(Queue) ->
    queue:get(Queue).

-spec get_r(queue(T)) -> T.
get_r(Queue) ->
    queue:get_r(Queue).

-spec peek(queue(T)) -> 'empty' | {'value',T}.
peek(Queue) ->
    queue:peek(Queue).

-spec peek_r(queue(T)) -> 'empty' | {'value',T}.
peek_r(Queue) ->
    queue:peek_r(Queue).

-spec drop(queue(T)) -> queue(T).
drop(Queue) ->
    queue:drop(Queue).

-spec drop_r(queue(T)) -> queue(T).
drop_r(Queue) ->
    queue:drop_r(Queue).

-spec reverse(queue(T)) -> queue(T).
reverse(Queue) ->
    queue:reverse(Queue).

-spec join(queue(T), queue(T)) -> queue(T).
join(Queue1, Queue2) ->
    queue:join(Queue1, Queue2).

-spec split(non_neg_integer(), queue(T)) -> {queue(T),queue(T)}.
split(N, Queue) ->
    queue:split(N, Queue).

-spec filter(fun((T) -> boolean() | [T]), queue(T)) -> queue(T).
filter(Pred, Queue) ->
    queue:filter(Pred, Queue).

-spec cons(T, queue(T)) -> queue(T).
cons(X, Queue) ->
    queue:cons(X, Queue).

-spec head(queue(T)) -> T.
head(Queue) ->
    queue:head(Queue).

-spec tail(queue(T)) -> queue(T).
tail(Queue) ->
    queue:tail(Queue).

-spec snoc(queue(T), T) -> queue(T).
snoc(Queue, X) ->
    queue:snoc(Queue, X).

-spec daeh(queue(T)) -> T.
daeh(Queue) ->
    queue:daeh(Queue).

-spec last(queue(T)) -> T.
last(Queue) ->
    queue:last(Queue).

-spec liat(queue(T)) -> queue(T).
liat(Queue) ->
    queue:liat(Queue).

-spec lait(queue(T)) -> queue(T).
lait(Queue) ->
    queue:lait(Queue).

-spec init(queue(T)) -> queue(T).
init(Queue) ->
    queue:init(Queue).
