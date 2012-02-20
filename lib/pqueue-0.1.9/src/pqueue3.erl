%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==A Large Priority Queue.==
%%% This priority queue implementation depends on layered tuples, so that tuple
%%% access times can be exploited for quick in/out priority queue operations
%%% when using 64 or more total priorities. This implementation was created
%%% to avoid the slowness within the priority queue used by
%%% both RabbitMQ and Riak
%%% (https://github.com/basho/riak_core/blob/master/src/priority_queue.erl).
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011, Michael Truog <mjtruog at gmail dot com>
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%% 
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in
%%%       the documentation and/or other materials provided with the
%%%       distribution.
%%%     * All advertising materials mentioning features or use of this
%%%       software must display the following acknowledgment:
%%%         This product includes software developed by Michael Truog
%%%     * The name of the author may not be used to endorse or promote
%%%       products derived from this software without specific prior
%%%       written permission
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
%%% DAMAGE.
%%%
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2011 Michael Truog
%%% @version 0.1.9 {@date} {@time}
%%%------------------------------------------------------------------------

-module(pqueue3).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([in/2,         % O(1)
         in/3,         % O(1)
         is_empty/1,   % O(1)
         is_queue/1,   % O(1)
         len/1,        % O(N)
         new/0,        % O(1)
         new/1,        % O(1)
         out/1,        % O(1) amortized, O(N) worst case
         out/2,        % O(1) amortized, O(N) worst case
         pout/1,       % O(1) amortized, O(N) worst case
         to_list/1]).  % O(N)

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-type pqueue3() :: {integer(), integer(), empty | integer(), tuple()}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Append an item to the tail of the 0 priority queue.===
%% O(1)
%% @end
%%-------------------------------------------------------------------------

-spec in(term(), pqueue3()) -> pqueue3().

in(Value, Q) ->
    in(Value, 0, Q).

%%-------------------------------------------------------------------------
%% @doc
%% ===Append an item to the tail of a specific priority queue.===
%% O(1)
%% @end
%%-------------------------------------------------------------------------

-spec in(term(), integer(), pqueue3()) -> pqueue3().

in(_, P, {Size, Offset, _, _})
    when (P + Offset) < 0; (P + Offset) > Size ->
    throw(badarg);
in(Value, P, {Size, Offset, empty, Bins}) ->
    PriorityIndex = P + Offset,
    {Size, Offset, PriorityIndex,
     in_queue(layer_indexes(Size, PriorityIndex), Value, Bins)};
in(Value, P, {Size, Offset, I, Bins})
    when (P + Offset) < I ->
    PriorityIndex = P + Offset,
    {Size, Offset, PriorityIndex,
     in_queue(layer_indexes(Size, PriorityIndex), Value, Bins)};
in(Value, P, {Size, Offset, I, Bins}) ->
    {Size, Offset, I,
     in_queue(layer_indexes(Size, P + Offset), Value, Bins)}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Check if the priority queue is empty.===
%% O(1)
%% @end
%%-------------------------------------------------------------------------

-spec is_empty(pqueue3()) -> 'true' | 'false'.

is_empty({_, _, empty, _}) ->
    true;
is_empty({_, _, _, _} = Q) ->
    case out(Q) of
        {empty, _} ->
            true;
        {{value, _}, _} ->
            false
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Check if the priority queue type is as expected.===
%% O(1)
%% @end
%%-------------------------------------------------------------------------

-spec is_queue(pqueue3()) -> 'true' | 'false'.

is_queue({Size, Offset, I, Bins})
    when is_integer(Size), is_integer(Offset), is_tuple(Bins) ->
    (I =:= empty) or is_integer(I);
is_queue(_) ->
    false.

%%-------------------------------------------------------------------------
%% @doc
%% ===Determine the length of a priority queue.===
%% O(N)
%% @end
%%-------------------------------------------------------------------------

-spec len(pqueue3()) -> non_neg_integer().

len(Q) ->
    len(0, out(Q)).
len(I, {empty, _}) ->
    I;
len(I, {{value, _}, Q}) ->
    len(I + 1, out(Q)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a new priority queue.===
%% O(1)
%% @end
%%-------------------------------------------------------------------------

-spec new() -> pqueue3().

new() ->
    new([]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a new priority queue with customization options.===
%% O(1)
%% @end
%%-------------------------------------------------------------------------

-spec new(list({atom(), term()})) -> pqueue3().

new(Options) ->
    Size = proplists:get_value(priorities, Options, 256),
    MiddleZero = proplists:get_value(middle_priority_zero, Options, true),
    Offset = if
        MiddleZero =:= true ->
            erlang:round((Size / 2) + 0.5) - 1;
        true ->
            0
    end,
    {Size, Offset, empty, create(layer_sizes(Size))}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Take an item from the head of the priority queue.===
%% O(1) amortized, O(N) worst case
%% @end
%%-------------------------------------------------------------------------

-spec out(pqueue3()) ->
    {{'value', term()}, pqueue3()} | {'empty', pqueue3()}.

out({_, _, empty, _} = Q) ->
    {empty, Q};
out({Size, Offset, I, Bins}) ->
    {Result, NewI, NewBins} = out_check(
        I, Size, out_queue(layer_indexes(Size, I), Bins)
    ),
    {Result, {Size, Offset, NewI, NewBins}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Take an item of a specific priority from the head of the queue.===
%% O(1) amortized, O(N) worst case
%% @end
%%-------------------------------------------------------------------------

-spec out(integer(), pqueue3()) ->
    {{'value', term()}, pqueue3()} | {'empty', pqueue3()}.

out(P, {Size, Offset, _, _})
    when (P + Offset) < 0; (P + Offset) > Size ->
    throw(badarg);
out(_, {_, _, empty, _} = Q) ->
    {empty, Q};
out(P, {Size, Offset, I, Bins}) ->
    {Result, NewBins} = out_queue(layer_indexes(Size, P + Offset), Bins),
    {Result, {Size, Offset, I, NewBins}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Take an item from the head of the priority queue.===
%% Includes the priority in the return value.
%% O(1) amortized, O(N) worst case
%% @end
%%-------------------------------------------------------------------------

-spec pout(pqueue3()) ->
    {{'value', term(), integer()}, pqueue3()} | {'empty', pqueue3()}.

pout({_, _, empty, _} = Q) ->
    {empty, Q};
pout({Size, Offset, I, Bins}) ->
    {Result, NewI, NewBins} = pout_check(
        I, Size, Offset, out_queue(layer_indexes(Size, I), Bins)
    ),
    {Result, {Size, Offset, NewI, NewBins}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Convert the priority queue to a list.===
%% O(N)
%% @end
%%-------------------------------------------------------------------------

-spec to_list(pqueue3()) -> list(term()).

to_list(Q) ->
    to_list([], out(Q)).
to_list(L, {empty, _}) ->
    lists:reverse(L);
to_list(L, {{value, Value}, Q}) ->
    to_list([Value | L], out(Q)).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

create([]) ->
    queue:new();

create([I | Is]) ->
    erlang:make_tuple(I + 1, create(Is)).

in_queue({I1}, Value, Bins1) ->
    erlang:setelement(I1, Bins1, queue:in(Value, erlang:element(I1, Bins1)));

in_queue({I1, I2}, Value, Bins1) ->
    Bins2 = erlang:element(I1, Bins1),
    erlang:setelement(I1, Bins1,
    erlang:setelement(I2, Bins2, queue:in(Value, erlang:element(I2, Bins2))));

in_queue({I1, I2, I3}, Value, Bins1) ->
    Bins2 = erlang:element(I1, Bins1),
    Bins3 = erlang:element(I2, Bins2),
    erlang:setelement(I1, Bins1,
    erlang:setelement(I2, Bins2,
    erlang:setelement(I3, Bins3, queue:in(Value, erlang:element(I3, Bins3)))));

in_queue({I1, I2, I3, I4}, Value, Bins1) ->
    Bins2 = erlang:element(I1, Bins1),
    Bins3 = erlang:element(I2, Bins2),
    Bins4 = erlang:element(I3, Bins3),
    erlang:setelement(I1, Bins1,
    erlang:setelement(I2, Bins2,
    erlang:setelement(I3, Bins3,
    erlang:setelement(I4, Bins4, queue:in(Value, erlang:element(I4, Bins4)))))).

pout_check(Size, Size, _, {empty, Bins}) ->
    {empty, empty, Bins};
pout_check(I, _, Offset, {{value, Value}, Bins}) ->
    {{value, Value, I - Offset}, I, Bins};
pout_check(I, Size, Offset, {empty, Bins}) ->
    NewI = I + 1,
    pout_check(NewI, Size, Offset, out_queue(layer_indexes(Size, NewI), Bins)).

out_check(Size, Size, {empty, Bins}) ->
    {empty, empty, Bins};
out_check(I, _, {{value, _} = Result, Bins}) ->
    {Result, I, Bins};
out_check(I, Size, {empty, Bins}) ->
    NewI = I + 1,
    out_check(NewI, Size, out_queue(layer_indexes(Size, NewI), Bins)).

out_queue({I1}, Bins1) ->
    {Result, NewQueue} = queue:out(erlang:element(I1, Bins1)),
    {Result,
     erlang:setelement(I1, Bins1, NewQueue)};

out_queue({I1, I2}, Bins1) ->
    Bins2 = erlang:element(I1, Bins1),
    {Result, NewQueue} = queue:out(erlang:element(I2, Bins2)),
    {Result,
     erlang:setelement(I1, Bins1,
     erlang:setelement(I2, Bins2, NewQueue))};

out_queue({I1, I2, I3}, Bins1) ->
    Bins2 = erlang:element(I1, Bins1),
    Bins3 = erlang:element(I2, Bins2),
    {Result, NewQueue} = queue:out(erlang:element(I3, Bins3)),
    {Result,
     erlang:setelement(I1, Bins1,
     erlang:setelement(I2, Bins2,
     erlang:setelement(I3, Bins3, NewQueue)))};

out_queue({I1, I2, I3, I4}, Bins1) ->
    Bins2 = erlang:element(I1, Bins1),
    Bins3 = erlang:element(I2, Bins2),
    Bins4 = erlang:element(I3, Bins3),
    {Result, NewQueue} = queue:out(erlang:element(I4, Bins4)),
    {Result,
     erlang:setelement(I1, Bins1,
     erlang:setelement(I2, Bins2,
     erlang:setelement(I3, Bins3,
     erlang:setelement(I4, Bins4, NewQueue))))}.

layer_indexes(Size, PriorityIndex) ->
    if
        Size =< 127 ->
            {PriorityIndex + 1};
        Size =< 255 ->
            <<I1:4, I2:4>> = <<PriorityIndex:8>>,
            {I1 + 1, I2 + 1};
        Size =< 511 ->
            <<I1:4, I2:5>> = <<PriorityIndex:9>>,
            {I1 + 1, I2 + 1};
        Size =< 1023 ->
            <<I1:3, I2:3, I3:4>> = <<PriorityIndex:10>>,
            {I1 + 1, I2 + 1, I3 + 1};
        Size =< 2047 ->
            <<I1:3, I2:4, I3:4>> = <<PriorityIndex:11>>,
            {I1 + 1, I2 + 1, I3 + 1};
        Size =< 4095 ->
            <<I1:4, I2:4, I3:4>> = <<PriorityIndex:12>>,
            {I1 + 1, I2 + 1, I3 + 1};
        Size =< 8191 ->
            <<I1:4, I2:4, I3:5>> = <<PriorityIndex:13>>,
            {I1 + 1, I2 + 1, I3 + 1};
        Size =< 16383 ->
            <<I1:4, I2:5, I3:5>> = <<PriorityIndex:14>>,
            {I1 + 1, I2 + 1, I3 + 1};
        Size =< 32767 ->
            <<I1:3, I2:4, I3:4, I4:4>> = <<PriorityIndex:15>>,
            {I1 + 1, I2 + 1, I3 + 1, I4 + 1};
        Size =< 65535 ->
            <<I1:4, I2:4, I3:4, I4:4>> = <<PriorityIndex:16>>,
            {I1 + 1, I2 + 1, I3 + 1, I4 + 1}
    end.

layer_sizes(Size) ->
    if
        Size =< 127 ->
            <<I1:7>> = <<127:7>>,
            [I1];
        Size =< 255 ->
            <<I1:4, I2:4>> = <<255:8>>,
            [I1, I2];
        Size =< 511 ->
            <<I1:4, I2:5>> = <<511:9>>,
            [I1, I2];
        Size =< 1023 ->
            <<I1:3, I2:3, I3:4>> = <<1023:10>>,
            [I1, I2, I3];
        Size =< 2047 ->
            <<I1:3, I2:4, I3:4>> = <<2047:11>>,
            [I1, I2, I3];
        Size =< 4095 ->
            <<I1:4, I2:4, I3:4>> = <<4095:12>>,
            [I1, I2, I3];
        Size =< 8191 ->
            <<I1:4, I2:4, I3:5>> = <<8191:13>>,
            [I1, I2, I3];
        Size =< 16383 ->
            <<I1:4, I2:5, I3:5>> = <<16383:14>>,
            [I1, I2, I3];
        Size =< 32767 ->
            <<I1:3, I2:4, I3:4, I4:4>> = <<32767:15>>,
            [I1, I2, I3, I4];
        Size =< 65535 ->
            <<I1:4, I2:4, I3:4, I4:4>> = <<65535:16>>,
            [I1, I2, I3, I4]
    end.

