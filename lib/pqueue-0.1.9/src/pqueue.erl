%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Static Priority Queue.==
%%% This priority queue implementation depends on a static number of priorities
%%% (-20 (high) to 20 (low)) so that tuple access times can be exploited for
%%% quick in/out priority queue operations.  This implementation was created to
%%% avoid the slowness within the priority queue used by both RabbitMQ and Riak
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

-module(pqueue).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([in/2,         % O(1)
         in/3,         % O(1)
         is_empty/1,   % O(1)
         is_queue/1,   % O(1)
         join/2,       % O(N) typically (?)
         len/1,        % O(N)
         new/0,        % O(1)
         out/1,        % O(1) amortized, O(N) worst case
         out/2,        % O(1) amortized, O(N) worst case
         pout/1,       % O(1) amortized, O(N) worst case
         to_list/1,    % O(N)
         test/0]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-type pqueue() ::
    {integer(),
     {queue(), queue(), queue(), queue(), queue(), queue(), queue()},
     {queue(), queue(), queue(), queue(), queue(), queue(), queue()},
     {queue(), queue(), queue(), queue(), queue(), queue()},
     queue(),
     {queue(), queue(), queue(), queue(), queue(), queue()},
     {queue(), queue(), queue(), queue(), queue(), queue(), queue()},
     {queue(), queue(), queue(), queue(), queue(), queue(), queue()}} |
    {'empty',
     {queue(), queue(), queue(), queue(), queue(), queue(), queue()},
     {queue(), queue(), queue(), queue(), queue(), queue(), queue()},
     {queue(), queue(), queue(), queue(), queue(), queue()},
     queue(),
     {queue(), queue(), queue(), queue(), queue(), queue()},
     {queue(), queue(), queue(), queue(), queue(), queue(), queue()},
     {queue(), queue(), queue(), queue(), queue(), queue(), queue()}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Append an item to the tail of the 0 priority queue.===
%% O(1)
%% @end
%%-------------------------------------------------------------------------

-spec in(term(), pqueue()) -> pqueue().

in(X, Q) ->
    in(X, 0, Q).

%%-------------------------------------------------------------------------
%% @doc
%% ===Append an item to the tail of a specific priority queue.===
%% O(1)
%% @end
%%-------------------------------------------------------------------------

-spec in(term(), integer(), pqueue()) -> pqueue().

in(_, P, _)
    when P < -20; P > 20 ->
    throw(badarg);
in(X, P, {empty, _, _, _, _, _, _, _} = Q) ->
    in_higher(P, Q, X);
in(X, P, {Pc, _, _, _, _, _, _, _} = Q)
    when P < Pc ->
    in_higher(P, Q, X);
in(X, P, Q) ->
    in_lower(P, Q, X).

%%-------------------------------------------------------------------------
%% @doc
%% ===Check if the priority queue is empty.===
%% O(1)
%% @end
%%-------------------------------------------------------------------------

-spec is_empty(pqueue()) -> 'true' | 'false'.

is_empty({empty, _, _, _, _, _, _, _}) ->
    true;
is_empty({_,
         {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
         {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
         {Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
         Q0,
         {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6},
         {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
         {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}}) ->
    queue:is_empty(Qn20) and queue:is_empty(Qn19) and queue:is_empty(Qn18) and
    queue:is_empty(Qn17) and queue:is_empty(Qn16) and queue:is_empty(Qn15) and
    queue:is_empty(Qn14) and
    queue:is_empty(Qn13) and queue:is_empty(Qn12) and queue:is_empty(Qn11) and
    queue:is_empty(Qn10) and queue:is_empty(Qn9) and queue:is_empty(Qn8) and
    queue:is_empty(Qn7) and
    queue:is_empty(Qn6) and queue:is_empty(Qn5) and queue:is_empty(Qn4) and
    queue:is_empty(Qn3) and queue:is_empty(Qn2) and queue:is_empty(Qn1) and
    queue:is_empty(Q0) and
    queue:is_empty(Qp1) and queue:is_empty(Qp2) and queue:is_empty(Qp3) and
    queue:is_empty(Qp4) and queue:is_empty(Qp5) and queue:is_empty(Qp6) and
    queue:is_empty(Qp7) and queue:is_empty(Qp8) and queue:is_empty(Qp9) and
    queue:is_empty(Qp10) and queue:is_empty(Qp11) and queue:is_empty(Qp12) and
    queue:is_empty(Qp13) and
    queue:is_empty(Qp14) and queue:is_empty(Qp15) and queue:is_empty(Qp16) and
    queue:is_empty(Qp17) and queue:is_empty(Qp18) and queue:is_empty(Qp19) and
    queue:is_empty(Qp20).

%%-------------------------------------------------------------------------
%% @doc
%% ===Check if the priority queue type is as expected.===
%% O(1)
%% @end
%%-------------------------------------------------------------------------

-spec is_queue(pqueue()) -> 'true' | 'false'.

is_queue({Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14})
    when tuple_size(Qsn14) == 7, tuple_size(Qsn7) == 7, tuple_size(Qsn1) == 6,
         tuple_size(Qsp14) == 7, tuple_size(Qsp7) == 7, tuple_size(Qsp1) == 6 ->
    (((Pc =:= empty) or is_integer(Pc)) and queue:is_queue(Q0));
is_queue(_) ->
    false.

%%-------------------------------------------------------------------------
%% @doc
%% ===Join two priority queues.===
%% O(N)
%% @end
%%-------------------------------------------------------------------------

-spec join(pqueue(), pqueue()) -> pqueue().

join({P1c,
      {Q1_n20, Q1_n19, Q1_n18, Q1_n17, Q1_n16, Q1_n15, Q1_n14},
      {Q1_n13, Q1_n12, Q1_n11, Q1_n10, Q1_n9, Q1_n8, Q1_n7},
      {Q1_n6, Q1_n5, Q1_n4, Q1_n3, Q1_n2, Q1_n1},
      Q1_0,
      {Q1_p1, Q1_p2, Q1_p3, Q1_p4, Q1_p5, Q1_p6},
      {Q1_p7, Q1_p8, Q1_p9, Q1_p10, Q1_p11, Q1_p12, Q1_p13},
      {Q1_p14, Q1_p15, Q1_p16, Q1_p17, Q1_p18, Q1_p19, Q1_p20}},
     {P2c,
      {Q2_n20, Q2_n19, Q2_n18, Q2_n17, Q2_n16, Q2_n15, Q2_n14},
      {Q2_n13, Q2_n12, Q2_n11, Q2_n10, Q2_n9, Q2_n8, Q2_n7},
      {Q2_n6, Q2_n5, Q2_n4, Q2_n3, Q2_n2, Q2_n1},
      Q2_0,
      {Q2_p1, Q2_p2, Q2_p3, Q2_p4, Q2_p5, Q2_p6},
      {Q2_p7, Q2_p8, Q2_p9, Q2_p10, Q2_p11, Q2_p12, Q2_p13},
      {Q2_p14, Q2_p15, Q2_p16, Q2_p17, Q2_p18, Q2_p19, Q2_p20}}) ->
    {erlang:min(P1c, P2c),
     {queue:join(Q1_n20, Q2_n20), queue:join(Q1_n19, Q2_n19),
      queue:join(Q1_n18, Q2_n18), queue:join(Q1_n17, Q2_n17),
      queue:join(Q1_n16, Q2_n16), queue:join(Q1_n15, Q2_n15),
      queue:join(Q1_n14, Q2_n14)},
     {queue:join(Q1_n13, Q2_n13), queue:join(Q1_n12, Q2_n12),
      queue:join(Q1_n11, Q2_n11), queue:join(Q1_n10, Q2_n10),
      queue:join(Q1_n9, Q2_n9), queue:join(Q1_n8, Q2_n8),
      queue:join(Q1_n7, Q2_n7)},
     {queue:join(Q1_n6, Q2_n6), queue:join(Q1_n5, Q2_n5),
      queue:join(Q1_n4, Q2_n4), queue:join(Q1_n3, Q2_n3),
      queue:join(Q1_n2, Q2_n2), queue:join(Q1_n1, Q2_n1)},
     queue:join(Q1_0, Q2_0),
     {queue:join(Q1_p1, Q2_p1), queue:join(Q1_p2, Q2_p2),
      queue:join(Q1_p3, Q2_p3), queue:join(Q1_p4, Q2_p4),
      queue:join(Q1_p5, Q2_p5), queue:join(Q1_p6, Q2_p6)},
     {queue:join(Q1_p7, Q2_p7), queue:join(Q1_p8, Q2_p8),
      queue:join(Q1_p9, Q2_p9), queue:join(Q1_p10, Q2_p10),
      queue:join(Q1_p11, Q2_p11), queue:join(Q1_p12, Q2_p12),
      queue:join(Q1_p13, Q2_p13)},
     {queue:join(Q1_p14, Q2_p14), queue:join(Q1_p15, Q2_p15),
      queue:join(Q1_p16, Q2_p16), queue:join(Q1_p17, Q2_p17),
      queue:join(Q1_p18, Q2_p18), queue:join(Q1_p19, Q2_p19),
      queue:join(Q1_p20, Q2_p20)}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Determine the length of a priority queue.===
%% O(N)
%% @end
%%-------------------------------------------------------------------------

-spec len(pqueue()) -> non_neg_integer().

len({_,
     {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
     {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
     {Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
     Q0,
     {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6},
     {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
     {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}}) ->
    queue:len(Qn20) + queue:len(Qn19) + queue:len(Qn18) + queue:len(Qn17) +
    queue:len(Qn16) + queue:len(Qn15) + queue:len(Qn14) +
    queue:len(Qn13) + queue:len(Qn12) + queue:len(Qn11) + queue:len(Qn10) +
    queue:len(Qn9) + queue:len(Qn8) + queue:len(Qn7) +
    queue:len(Qn6) + queue:len(Qn5) + queue:len(Qn4) + queue:len(Qn3) +
    queue:len(Qn2) + queue:len(Qn1) +
    queue:len(Q0) +
    queue:len(Qp1) + queue:len(Qp2) + queue:len(Qp3) + queue:len(Qp4) +
    queue:len(Qp5) + queue:len(Qp6) +
    queue:len(Qp7) + queue:len(Qp8) + queue:len(Qp9) + queue:len(Qp10) +
    queue:len(Qp11) + queue:len(Qp12) + queue:len(Qp13) +
    queue:len(Qp14) + queue:len(Qp15) + queue:len(Qp16) + queue:len(Qp17) +
    queue:len(Qp18) + queue:len(Qp19) + queue:len(Qp20).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a new priority queue.===
%% O(1)
%% @end
%%-------------------------------------------------------------------------

-spec new() -> pqueue().

new() ->
    {empty,                              % current priority
     erlang:make_tuple(7, queue:new()),  % priority [-20..-14]
     erlang:make_tuple(7, queue:new()),  % priority [-13.. -7]
     erlang:make_tuple(6, queue:new()),  % priority [ -6.. -1]
     queue:new(),                        % priority 0 (default)
     erlang:make_tuple(6, queue:new()),  % priority [  1..  6]
     erlang:make_tuple(7, queue:new()),  % priority [  7.. 13]
     erlang:make_tuple(7, queue:new())}. % priority [ 14.. 20]

%%-------------------------------------------------------------------------
%% @doc
%% ===Take an item from the head of the priority queue.===
%% O(1) amortized, O(N) worst case
%% @end
%%-------------------------------------------------------------------------

-spec out(pqueue()) ->
    {{'value', term()}, pqueue()} | {'empty', pqueue()}.

out({empty, _, _, _, _, _, _, _} = Q) ->
    {empty, Q};
out({Pc, _, _, _, _, _, _, _} = Q) ->
    out_current(Pc, Q, nopriority).

%%-------------------------------------------------------------------------
%% @doc
%% ===Take an item of a specific priority from the head of the queue.===
%% O(1) amortized, O(N) worst case
%% @end
%%-------------------------------------------------------------------------

-spec out(integer(), pqueue()) ->
    {{'value', term()}, pqueue()} | {'empty', pqueue()}.

out(P, _)
    when P < -20; P > 20 ->
    throw(badarg);
out(_, {empty, _, _, _, _, _, _, _} = Q) ->
    {empty, Q};
out(P, Q) ->
    out_specific(P, Q).

%%-------------------------------------------------------------------------
%% @doc
%% ===Take an item from the head of the priority queue.===
%% Includes the priority in the return value.
%% O(1) amortized, O(N) worst case
%% @end
%%-------------------------------------------------------------------------

-spec pout(pqueue()) ->
    {{'value', term(), integer()}, pqueue()} | {'empty', pqueue()}.

pout({empty, _, _, _, _, _, _, _} = Q) ->
    {empty, Q};
pout({Pc, _, _, _, _, _, _, _} = Q) ->
    out_current(Pc, Q, priority).

%%-------------------------------------------------------------------------
%% @doc
%% ===Convert the priority queue to a list.===
%% O(N)
%% @end
%%-------------------------------------------------------------------------

-spec to_list(pqueue()) -> list(term()).

to_list({_,
         {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
         {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
         {Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
         Q0,
         {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6},
         {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
         {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}}) ->
    queue:to_list(Qn20) ++ queue:to_list(Qn19) ++ queue:to_list(Qn18) ++
    queue:to_list(Qn17) ++ queue:to_list(Qn16) ++ queue:to_list(Qn15) ++
    queue:to_list(Qn14) ++
    queue:to_list(Qn13) ++ queue:to_list(Qn12) ++ queue:to_list(Qn11) ++
    queue:to_list(Qn10) ++ queue:to_list(Qn9) ++ queue:to_list(Qn8) ++
    queue:to_list(Qn7) ++
    queue:to_list(Qn6) ++ queue:to_list(Qn5) ++ queue:to_list(Qn4) ++
    queue:to_list(Qn3) ++ queue:to_list(Qn2) ++ queue:to_list(Qn1) ++
    queue:to_list(Q0) ++
    queue:to_list(Qp1) ++ queue:to_list(Qp2) ++ queue:to_list(Qp3) ++
    queue:to_list(Qp4) ++ queue:to_list(Qp5) ++ queue:to_list(Qp6) ++
    queue:to_list(Qp7) ++ queue:to_list(Qp8) ++ queue:to_list(Qp9) ++
    queue:to_list(Qp10) ++ queue:to_list(Qp11) ++ queue:to_list(Qp12) ++
    queue:to_list(Qp13) ++
    queue:to_list(Qp14) ++ queue:to_list(Qp15) ++ queue:to_list(Qp16) ++
    queue:to_list(Qp17) ++ queue:to_list(Qp18) ++ queue:to_list(Qp19) ++
    queue:to_list(Qp20).

%%-------------------------------------------------------------------------
%% @doc
%% ===Regression test.===
%% @end
%%-------------------------------------------------------------------------

test() ->
    Q0 = pqueue:new(),
    true = pqueue:is_queue(Q0),
    Q1 = pqueue:in(20, 20, Q0),
    Q2 = pqueue:in(19, 19, Q1),
    Q3 = pqueue:in(18, 18, Q2),
    Q4 = pqueue:in(17, 17, Q3),
    Q5 = pqueue:in(16, 16, Q4),
    Q6 = pqueue:in(15, 15, Q5),
    Q7 = pqueue:in(14, 14, Q6),
    Q8 = pqueue:in(13, 13, Q7),
    Q9 = pqueue:in(12, 12, Q8),
    Q10 = pqueue:in(11, 11, Q9),
    Q11 = pqueue:in(10, 10, Q10),
    Q12 = pqueue:in(9, 9, Q11),
    Q13 = pqueue:in(8, 8, Q12),
    Q14 = pqueue:in(7, 7, Q13),
    Q15 = pqueue:in(6, 6, Q14),
    Q16 = pqueue:in(5, 5, Q15),
    Q17 = pqueue:in(4, 4, Q16),
    Q18 = pqueue:in(3, 3, Q17),
    Q19 = pqueue:in(2, 2, Q18),
    Q20 = pqueue:in(1, 1, Q19),
    Q21 = pqueue:in(0, 0, Q20),
    Q22 = pqueue:in(-1, -1, Q21),
    Q23 = pqueue:in(-2, -2, Q22),
    Q24 = pqueue:in(-3, -3, Q23),
    Q25 = pqueue:in(-4, -4, Q24),
    Q26 = pqueue:in(-5, -5, Q25),
    Q27 = pqueue:in(-6, -6, Q26),
    Q28 = pqueue:in(-7, -7, Q27),
    Q29 = pqueue:in(-8, -8, Q28),
    Q30 = pqueue:in(-9, -9, Q29),
    Q31 = pqueue:in(-10, -10, Q30),
    Q32 = pqueue:in(-11, -11, Q31),
    Q33 = pqueue:in(-12, -12, Q32),
    Q34 = pqueue:in(-13, -13, Q33),
    Q35 = pqueue:in(-14, -14, Q34),
    Q36 = pqueue:in(-15, -15, Q35),
    Q37 = pqueue:in(-16, -16, Q36),
    Q38 = pqueue:in(-17, -17, Q37),
    Q39 = pqueue:in(-18, -18, Q38),
    Q40 = pqueue:in(-19, -19, Q39),
    Q41 = pqueue:in(-20, -20, Q40),
    Q42 = pqueue:in(-20, -20, Q41),
    Q43 = pqueue:in(-19, -19, Q42),
    Q44 = pqueue:in(-18, -18, Q43),
    Q45 = pqueue:in(-17, -17, Q44),
    Q46 = pqueue:in(-16, -16, Q45),
    Q47 = pqueue:in(-15, -15, Q46),
    Q48 = pqueue:in(-14, -14, Q47),
    Q49 = pqueue:in(-13, -13, Q48),
    Q50 = pqueue:in(-12, -12, Q49),
    Q51 = pqueue:in(-11, -11, Q50),
    Q52 = pqueue:in(-10, -10, Q51),
    Q53 = pqueue:in(-9, -9, Q52),
    Q54 = pqueue:in(-8, -8, Q53),
    Q55 = pqueue:in(-7, -7, Q54),
    Q56 = pqueue:in(-6, -6, Q55),
    Q57 = pqueue:in(-5, -5, Q56),
    Q58 = pqueue:in(-4, -4, Q57),
    Q59 = pqueue:in(-3, -3, Q58),
    Q60 = pqueue:in(-2, -2, Q59),
    Q61 = pqueue:in(-1, -1, Q60),
    Q62 = pqueue:in(0, 0, Q61),
    Q63 = pqueue:in(1, 1, Q62),
    Q64 = pqueue:in(2, 2, Q63),
    Q65 = pqueue:in(3, 3, Q64),
    Q66 = pqueue:in(4, 4, Q65),
    Q67 = pqueue:in(5, 5, Q66),
    Q68 = pqueue:in(6, 6, Q67),
    Q69 = pqueue:in(7, 7, Q68),
    Q70 = pqueue:in(8, 8, Q69),
    Q71 = pqueue:in(9, 9, Q70),
    Q72 = pqueue:in(10, 10, Q71),
    Q73 = pqueue:in(11, 11, Q72),
    Q74 = pqueue:in(12, 12, Q73),
    Q75 = pqueue:in(13, 13, Q74),
    Q76 = pqueue:in(14, 14, Q75),
    Q77 = pqueue:in(15, 15, Q76),
    Q78 = pqueue:in(16, 16, Q77),
    Q79 = pqueue:in(17, 17, Q78),
    Q80 = pqueue:in(18, 18, Q79),
    Q81 = pqueue:in(19, 19, Q80),
    Q82 = pqueue:in(20, 20, Q81),
    true = pqueue:is_queue(Q82),
    82 = pqueue:len(Q82),
    [-20, -20, -19, -19, -18, -18, -17, -17, -16, -16, -15, -15, -14, -14,
     -13, -13, -12, -12, -11, -11, -10, -10, -9, -9, -8, -8, -7, -7, -6, -6,
     -5, -5, -4, -4, -3, -3, -2, -2, -1, -1, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4,
     5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14,
     15, 15, 16, 16, 17, 17, 18, 18, 19, 19, 20, 20] = pqueue:to_list(Q82),
    {{value, -20}, Q83} = pqueue:out(Q82),
    {{value, -20}, Q84} = pqueue:out(Q83),
    {{value, -19}, Q85} = pqueue:out(Q84),
    {{value, -19}, Q86} = pqueue:out(Q85),
    {{value, -18}, Q87} = pqueue:out(Q86),
    {{value, -18}, Q88} = pqueue:out(Q87),
    {{value, 0}, Q89} = pqueue:out(0, Q88),
    {{value, 0}, Q90} = pqueue:out(0, Q89),
    {empty, _} = pqueue:out(0, Q90),
    {{value, -17, -17}, Q91} = pqueue:pout(Q90),
    {{value, -17, -17}, Q92} = pqueue:pout(Q91),
    {{value, -16, -16}, Q93} = pqueue:pout(Q92),
    {{value, -16, -16}, Q94} = pqueue:pout(Q93),
    {{value, -15, -15}, Q95} = pqueue:pout(Q94),
    {{value, -15, -15}, Q96} = pqueue:pout(Q95),
    {{value, -14, -14}, Q97} = pqueue:pout(Q96),
    {{value, -14, -14}, Q98} = pqueue:pout(Q97),
    {{value, -13, -13}, Q99} = pqueue:pout(Q98),
    {{value, -13, -13}, Q100} = pqueue:pout(Q99),
    {{value, -12, -12}, Q101} = pqueue:pout(Q100),
    {{value, -12, -12}, Q102} = pqueue:pout(Q101),
    {{value, -11, -11}, Q103} = pqueue:pout(Q102),
    {{value, -11, -11}, Q104} = pqueue:pout(Q103),
    {{value, -10, -10}, Q105} = pqueue:pout(Q104),
    {{value, -10, -10}, Q106} = pqueue:pout(Q105),
    {{value, -9, -9}, Q107} = pqueue:pout(Q106),
    {{value, -9, -9}, Q108} = pqueue:pout(Q107),
    {{value, -8, -8}, Q109} = pqueue:pout(Q108),
    {{value, -8, -8}, Q110} = pqueue:pout(Q109),
    {{value, -7, -7}, Q111} = pqueue:pout(Q110),
    {{value, -7, -7}, Q112} = pqueue:pout(Q111),
    {{value, -6, -6}, Q113} = pqueue:pout(Q112),
    {{value, -6, -6}, Q114} = pqueue:pout(Q113),
    {{value, -5, -5}, Q115} = pqueue:pout(Q114),
    {{value, -5, -5}, Q116} = pqueue:pout(Q115),
    {{value, -4, -4}, Q117} = pqueue:pout(Q116),
    {{value, -4, -4}, Q118} = pqueue:pout(Q117),
    {{value, -3, -3}, Q119} = pqueue:pout(Q118),
    {{value, -3, -3}, Q120} = pqueue:pout(Q119),
    {{value, -2, -2}, Q121} = pqueue:pout(Q120),
    {{value, -2, -2}, Q122} = pqueue:pout(Q121),
    {{value, -1, -1}, Q123} = pqueue:pout(Q122),
    {{value, -1, -1}, Q124} = pqueue:pout(Q123),
    {{value, 1, 1}, Q125} = pqueue:pout(Q124),
    {{value, 1, 1}, Q126} = pqueue:pout(Q125),
    {{value, 2, 2}, Q127} = pqueue:pout(Q126),
    {{value, 2, 2}, Q128} = pqueue:pout(Q127),
    {{value, 3, 3}, Q129} = pqueue:pout(Q128),
    {{value, 3, 3}, Q130} = pqueue:pout(Q129),
    {{value, 4, 4}, Q131} = pqueue:pout(Q130),
    {{value, 4, 4}, Q132} = pqueue:pout(Q131),
    {{value, 5, 5}, Q133} = pqueue:pout(Q132),
    {{value, 5, 5}, Q134} = pqueue:pout(Q133),
    {{value, 6, 6}, Q135} = pqueue:pout(Q134),
    {{value, 6, 6}, Q136} = pqueue:pout(Q135),
    {{value, 7, 7}, Q137} = pqueue:pout(Q136),
    {{value, 7, 7}, Q138} = pqueue:pout(Q137),
    {{value, 8, 8}, Q139} = pqueue:pout(Q138),
    {{value, 8, 8}, Q140} = pqueue:pout(Q139),
    {{value, 9, 9}, Q141} = pqueue:pout(Q140),
    {{value, 9, 9}, Q142} = pqueue:pout(Q141),
    {{value, 10, 10}, Q143} = pqueue:pout(Q142),
    {{value, 10, 10}, Q144} = pqueue:pout(Q143),
    {{value, 11, 11}, Q145} = pqueue:pout(Q144),
    {{value, 11, 11}, Q146} = pqueue:pout(Q145),
    {{value, 12, 12}, Q147} = pqueue:pout(Q146),
    {{value, 12, 12}, Q148} = pqueue:pout(Q147),
    {{value, 13, 13}, Q149} = pqueue:pout(Q148),
    {{value, 13, 13}, Q150} = pqueue:pout(Q149),
    {{value, 14, 14}, Q151} = pqueue:pout(Q150),
    {{value, 14, 14}, Q152} = pqueue:pout(Q151),
    {{value, 15, 15}, Q153} = pqueue:pout(Q152),
    {{value, 15, 15}, Q154} = pqueue:pout(Q153),
    {{value, 16, 16}, Q155} = pqueue:pout(Q154),
    {{value, 16, 16}, Q156} = pqueue:pout(Q155),
    {{value, 17, 17}, Q157} = pqueue:pout(Q156),
    {{value, 17, 17}, Q158} = pqueue:pout(Q157),
    {{value, 18, 18}, Q159} = pqueue:pout(Q158),
    {{value, 18, 18}, Q160} = pqueue:pout(Q159),
    {{value, 19, 19}, Q161} = pqueue:pout(Q160),
    {{value, 19, 19}, Q162} = pqueue:pout(Q161),
    {{value, 20, 20}, Q163} = pqueue:pout(Q162),
    {{value, 20, 20}, Q164} = pqueue:pout(Q163),
    true = pqueue:is_empty(Q164),
    {empty, Q165} = pqueue:pout(Q164),
    true = pqueue:is_empty(Q165),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

in_higher(-20, {_,
                {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
                Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {-20,
     {queue:in(X, Qn20), Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
     Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14};
in_higher(-19, {_,
                {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
                Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {-19,
     {Qn20, queue:in(X, Qn19), Qn18, Qn17, Qn16, Qn15, Qn14},
     Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14};
in_higher(-18, {_,
                {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
                Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {-18,
     {Qn20, Qn19, queue:in(X, Qn18), Qn17, Qn16, Qn15, Qn14},
     Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14};
in_higher(-17, {_,
                {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
                Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {-17,
     {Qn20, Qn19, Qn18, queue:in(X, Qn17), Qn16, Qn15, Qn14},
     Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14};
in_higher(-16, {_,
                {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
                Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {-16,
     {Qn20, Qn19, Qn18, Qn17, queue:in(X, Qn16), Qn15, Qn14},
     Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14};
in_higher(-15, {_,
                {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
                Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {-15,
     {Qn20, Qn19, Qn18, Qn17, Qn16, queue:in(X, Qn15), Qn14},
     Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14};
in_higher(-14, {_,
                {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
                Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {-14,
     {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, queue:in(X, Qn14)},
     Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14};
in_higher(-13, {_, Qsn14,
                {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
                Qsn1, Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {-13, Qsn14,
     {queue:in(X, Qn13), Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
     Qsn1, Q0, Qsp1, Qsp7, Qsp14};
in_higher(-12, {_, Qsn14,
                {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
                Qsn1, Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {-12, Qsn14,
     {Qn13, queue:in(X, Qn12), Qn11, Qn10, Qn9, Qn8, Qn7},
     Qsn1, Q0, Qsp1, Qsp7, Qsp14};
in_higher(-11, {_, Qsn14,
                {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
                Qsn1, Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {-11, Qsn14,
     {Qn13, Qn12, queue:in(X, Qn11), Qn10, Qn9, Qn8, Qn7},
     Qsn1, Q0, Qsp1, Qsp7, Qsp14};
in_higher(-10, {_, Qsn14,
                {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
                Qsn1, Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {-10, Qsn14,
     {Qn13, Qn12, Qn11, queue:in(X, Qn10), Qn9, Qn8, Qn7},
     Qsn1, Q0, Qsp1, Qsp7, Qsp14};
in_higher(-9, {_, Qsn14,
               {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
               Qsn1, Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {-9, Qsn14,
     {Qn13, Qn12, Qn11, Qn10, queue:in(X, Qn9), Qn8, Qn7},
     Qsn1, Q0, Qsp1, Qsp7, Qsp14};
in_higher(-8, {_, Qsn14,
               {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
               Qsn1, Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {-8, Qsn14,
     {Qn13, Qn12, Qn11, Qn10, Qn9, queue:in(X, Qn8), Qn7},
     Qsn1, Q0, Qsp1, Qsp7, Qsp14};
in_higher(-7, {_, Qsn14,
               {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
               Qsn1, Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {-7, Qsn14,
     {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, queue:in(X, Qn7)},
     Qsn1, Q0, Qsp1, Qsp7, Qsp14};
in_higher(-6, {_, Qsn14, Qsn7,
               {Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
               Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {-6, Qsn14, Qsn7,
     {queue:in(X, Qn6), Qn5, Qn4, Qn3, Qn2, Qn1},
     Q0, Qsp1, Qsp7, Qsp14};
in_higher(-5, {_, Qsn14, Qsn7,
               {Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
               Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {-5, Qsn14, Qsn7,
     {Qn6, queue:in(X, Qn5), Qn4, Qn3, Qn2, Qn1},
     Q0, Qsp1, Qsp7, Qsp14};
in_higher(-4, {_, Qsn14, Qsn7,
               {Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
               Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {-4, Qsn14, Qsn7,
     {Qn6, Qn5, queue:in(X, Qn4), Qn3, Qn2, Qn1},
     Q0, Qsp1, Qsp7, Qsp14};
in_higher(-3, {_, Qsn14, Qsn7,
               {Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
               Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {-3, Qsn14, Qsn7,
     {Qn6, Qn5, Qn4, queue:in(X, Qn3), Qn2, Qn1},
     Q0, Qsp1, Qsp7, Qsp14};
in_higher(-2, {_, Qsn14, Qsn7,
               {Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
               Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {-2, Qsn14, Qsn7,
     {Qn6, Qn5, Qn4, Qn3, queue:in(X, Qn2), Qn1},
     Q0, Qsp1, Qsp7, Qsp14};
in_higher(-1, {_, Qsn14, Qsn7,
               {Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
               Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {-1, Qsn14, Qsn7,
     {Qn6, Qn5, Qn4, Qn3, Qn2, queue:in(X, Qn1)},
     Q0, Qsp1, Qsp7, Qsp14};
in_higher(0, {_, Qsn14, Qsn7, Qsn1,
              Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {0, Qsn14, Qsn7, Qsn1,
     queue:in(X, Q0),
     Qsp1, Qsp7, Qsp14};
in_higher(1, {_, Qsn14, Qsn7, Qsn1, Q0,
              {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6},
              Qsp7, Qsp14}, X) ->
    {1, Qsn14, Qsn7, Qsn1, Q0,
     {queue:in(X, Qp1), Qp2, Qp3, Qp4, Qp5, Qp6},
     Qsp7, Qsp14};
in_higher(2, {_, Qsn14, Qsn7, Qsn1, Q0,
              {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6},
              Qsp7, Qsp14}, X) ->
    {2, Qsn14, Qsn7, Qsn1, Q0,
     {Qp1, queue:in(X, Qp2), Qp3, Qp4, Qp5, Qp6},
     Qsp7, Qsp14};
in_higher(3, {_, Qsn14, Qsn7, Qsn1, Q0,
              {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6},
              Qsp7, Qsp14}, X) ->
    {3, Qsn14, Qsn7, Qsn1, Q0,
     {Qp1, Qp2, queue:in(X, Qp3), Qp4, Qp5, Qp6},
     Qsp7, Qsp14};
in_higher(4, {_, Qsn14, Qsn7, Qsn1, Q0,
              {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6},
              Qsp7, Qsp14}, X) ->
    {4, Qsn14, Qsn7, Qsn1, Q0,
     {Qp1, Qp2, Qp3, queue:in(X, Qp4), Qp5, Qp6},
     Qsp7, Qsp14};
in_higher(5, {_, Qsn14, Qsn7, Qsn1, Q0,
              {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6},
              Qsp7, Qsp14}, X) ->
    {5, Qsn14, Qsn7, Qsn1, Q0,
     {Qp1, Qp2, Qp3, Qp4, queue:in(X, Qp5), Qp6},
     Qsp7, Qsp14};
in_higher(6, {_, Qsn14, Qsn7, Qsn1, Q0,
              {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6},
              Qsp7, Qsp14}, X) ->
    {6, Qsn14, Qsn7, Qsn1, Q0,
     {Qp1, Qp2, Qp3, Qp4, Qp5, queue:in(X, Qp6)},
     Qsp7, Qsp14};
in_higher(7, {_, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
              {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
              Qsp14}, X) ->
    {7, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
     {queue:in(X, Qp7), Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
     Qsp14};
in_higher(8, {_, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
              {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
              Qsp14}, X) ->
    {8, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
     {Qp7, queue:in(X, Qp8), Qp9, Qp10, Qp11, Qp12, Qp13},
     Qsp14};
in_higher(9, {_, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
              {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
              Qsp14}, X) ->
    {9, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
     {Qp7, Qp8, queue:in(X, Qp9), Qp10, Qp11, Qp12, Qp13},
     Qsp14};
in_higher(10, {_, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
               {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
               Qsp14}, X) ->
    {10, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
     {Qp7, Qp8, Qp9, queue:in(X, Qp10), Qp11, Qp12, Qp13},
     Qsp14};
in_higher(11, {_, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
               {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
               Qsp14}, X) ->
    {11, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
     {Qp7, Qp8, Qp9, Qp10, queue:in(X, Qp11), Qp12, Qp13},
     Qsp14};
in_higher(12, {_, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
               {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
               Qsp14}, X) ->
    {12, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
     {Qp7, Qp8, Qp9, Qp10, Qp11, queue:in(X, Qp12), Qp13},
     Qsp14};
in_higher(13, {_, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
               {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
               Qsp14}, X) ->
    {13, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
     {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, queue:in(X, Qp13)},
     Qsp14};
in_higher(14, {_, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
               {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}}, X) ->
    {14, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
     {queue:in(X, Qp14), Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}};
in_higher(15, {_, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
               {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}}, X) ->
    {15, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
     {Qp14, queue:in(X, Qp15), Qp16, Qp17, Qp18, Qp19, Qp20}};
in_higher(16, {_, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
               {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}}, X) ->
    {16, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
     {Qp14, Qp15, queue:in(X, Qp16), Qp17, Qp18, Qp19, Qp20}};
in_higher(17, {_, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
               {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}}, X) ->
    {17, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
     {Qp14, Qp15, Qp16, queue:in(X, Qp17), Qp18, Qp19, Qp20}};
in_higher(18, {_, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
               {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}}, X) ->
    {18, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
     {Qp14, Qp15, Qp16, Qp17, queue:in(X, Qp18), Qp19, Qp20}};
in_higher(19, {_, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
               {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}}, X) ->
    {19, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
     {Qp14, Qp15, Qp16, Qp17, Qp18, queue:in(X, Qp19), Qp20}};
in_higher(20, {_, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
               {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}}, X) ->
    {20, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
     {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, queue:in(X, Qp20)}}.

in_lower(-20, {Pc,
               {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
               Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {Pc,
     {queue:in(X, Qn20), Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
     Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14};
in_lower(-19, {Pc,
               {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
               Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {Pc,
     {Qn20, queue:in(X, Qn19), Qn18, Qn17, Qn16, Qn15, Qn14},
     Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14};
in_lower(-18, {Pc,
               {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
               Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {Pc,
     {Qn20, Qn19, queue:in(X, Qn18), Qn17, Qn16, Qn15, Qn14},
     Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14};
in_lower(-17, {Pc,
               {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
               Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {Pc,
     {Qn20, Qn19, Qn18, queue:in(X, Qn17), Qn16, Qn15, Qn14},
     Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14};
in_lower(-16, {Pc,
               {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
               Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {Pc,
     {Qn20, Qn19, Qn18, Qn17, queue:in(X, Qn16), Qn15, Qn14},
     Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14};
in_lower(-15, {Pc,
               {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
               Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {Pc,
     {Qn20, Qn19, Qn18, Qn17, Qn16, queue:in(X, Qn15), Qn14},
     Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14};
in_lower(-14, {Pc,
               {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
               Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {Pc,
     {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, queue:in(X, Qn14)},
     Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14};
in_lower(-13, {Pc, Qsn14,
               {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
               Qsn1, Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {Pc, Qsn14,
     {queue:in(X, Qn13), Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
     Qsn1, Q0, Qsp1, Qsp7, Qsp14};
in_lower(-12, {Pc, Qsn14,
               {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
               Qsn1, Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {Pc, Qsn14,
     {Qn13, queue:in(X, Qn12), Qn11, Qn10, Qn9, Qn8, Qn7},
     Qsn1, Q0, Qsp1, Qsp7, Qsp14};
in_lower(-11, {Pc, Qsn14,
               {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
               Qsn1, Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {Pc, Qsn14,
     {Qn13, Qn12, queue:in(X, Qn11), Qn10, Qn9, Qn8, Qn7},
     Qsn1, Q0, Qsp1, Qsp7, Qsp14};
in_lower(-10, {Pc, Qsn14,
               {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
               Qsn1, Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {Pc, Qsn14,
     {Qn13, Qn12, Qn11, queue:in(X, Qn10), Qn9, Qn8, Qn7},
     Qsn1, Q0, Qsp1, Qsp7, Qsp14};
in_lower(-9, {Pc, Qsn14,
              {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
              Qsn1, Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {Pc, Qsn14,
     {Qn13, Qn12, Qn11, Qn10, queue:in(X, Qn9), Qn8, Qn7},
     Qsn1, Q0, Qsp1, Qsp7, Qsp14};
in_lower(-8, {Pc, Qsn14,
              {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
              Qsn1, Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {Pc, Qsn14,
     {Qn13, Qn12, Qn11, Qn10, Qn9, queue:in(X, Qn8), Qn7},
     Qsn1, Q0, Qsp1, Qsp7, Qsp14};
in_lower(-7, {Pc, Qsn14,
              {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
              Qsn1, Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {Pc, Qsn14,
     {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, queue:in(X, Qn7)},
     Qsn1, Q0, Qsp1, Qsp7, Qsp14};
in_lower(-6, {Pc, Qsn14, Qsn7,
              {Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
              Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {Pc, Qsn14, Qsn7,
     {queue:in(X, Qn6), Qn5, Qn4, Qn3, Qn2, Qn1},
     Q0, Qsp1, Qsp7, Qsp14};
in_lower(-5, {Pc, Qsn14, Qsn7,
              {Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
              Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {Pc, Qsn14, Qsn7,
     {Qn6, queue:in(X, Qn5), Qn4, Qn3, Qn2, Qn1},
     Q0, Qsp1, Qsp7, Qsp14};
in_lower(-4, {Pc, Qsn14, Qsn7,
              {Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
              Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {Pc, Qsn14, Qsn7,
     {Qn6, Qn5, queue:in(X, Qn4), Qn3, Qn2, Qn1},
     Q0, Qsp1, Qsp7, Qsp14};
in_lower(-3, {Pc, Qsn14, Qsn7,
              {Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
              Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {Pc, Qsn14, Qsn7,
     {Qn6, Qn5, Qn4, queue:in(X, Qn3), Qn2, Qn1},
     Q0, Qsp1, Qsp7, Qsp14};
in_lower(-2, {Pc, Qsn14, Qsn7,
              {Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
              Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {Pc, Qsn14, Qsn7,
     {Qn6, Qn5, Qn4, Qn3, queue:in(X, Qn2), Qn1},
     Q0, Qsp1, Qsp7, Qsp14};
in_lower(-1, {Pc, Qsn14, Qsn7,
              {Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
              Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {Pc, Qsn14, Qsn7,
     {Qn6, Qn5, Qn4, Qn3, Qn2, queue:in(X, Qn1)},
     Q0, Qsp1, Qsp7, Qsp14};
in_lower(0, {Pc, Qsn14, Qsn7, Qsn1,
             Q0, Qsp1, Qsp7, Qsp14}, X) ->
    {Pc, Qsn14, Qsn7, Qsn1,
     queue:in(X, Q0),
     Qsp1, Qsp7, Qsp14};
in_lower(1, {Pc, Qsn14, Qsn7, Qsn1, Q0,
             {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6},
             Qsp7, Qsp14}, X) ->
    {Pc, Qsn14, Qsn7, Qsn1, Q0,
     {queue:in(X, Qp1), Qp2, Qp3, Qp4, Qp5, Qp6},
     Qsp7, Qsp14};
in_lower(2, {Pc, Qsn14, Qsn7, Qsn1, Q0,
             {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6},
             Qsp7, Qsp14}, X) ->
    {Pc, Qsn14, Qsn7, Qsn1, Q0,
     {Qp1, queue:in(X, Qp2), Qp3, Qp4, Qp5, Qp6},
     Qsp7, Qsp14};
in_lower(3, {Pc, Qsn14, Qsn7, Qsn1, Q0,
             {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6},
             Qsp7, Qsp14}, X) ->
    {Pc, Qsn14, Qsn7, Qsn1, Q0,
     {Qp1, Qp2, queue:in(X, Qp3), Qp4, Qp5, Qp6},
     Qsp7, Qsp14};
in_lower(4, {Pc, Qsn14, Qsn7, Qsn1, Q0,
             {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6},
             Qsp7, Qsp14}, X) ->
    {Pc, Qsn14, Qsn7, Qsn1, Q0,
     {Qp1, Qp2, Qp3, queue:in(X, Qp4), Qp5, Qp6},
     Qsp7, Qsp14};
in_lower(5, {Pc, Qsn14, Qsn7, Qsn1, Q0,
             {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6},
             Qsp7, Qsp14}, X) ->
    {Pc, Qsn14, Qsn7, Qsn1, Q0,
     {Qp1, Qp2, Qp3, Qp4, queue:in(X, Qp5), Qp6},
     Qsp7, Qsp14};
in_lower(6, {Pc, Qsn14, Qsn7, Qsn1, Q0,
             {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6},
             Qsp7, Qsp14}, X) ->
    {Pc, Qsn14, Qsn7, Qsn1, Q0,
     {Qp1, Qp2, Qp3, Qp4, Qp5, queue:in(X, Qp6)},
     Qsp7, Qsp14};
in_lower(7, {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
             {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
             Qsp14}, X) ->
    {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
     {queue:in(X, Qp7), Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
     Qsp14};
in_lower(8, {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
             {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
             Qsp14}, X) ->
    {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
     {Qp7, queue:in(X, Qp8), Qp9, Qp10, Qp11, Qp12, Qp13},
     Qsp14};
in_lower(9, {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
             {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
             Qsp14}, X) ->
    {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
     {Qp7, Qp8, queue:in(X, Qp9), Qp10, Qp11, Qp12, Qp13},
     Qsp14};
in_lower(10, {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
              {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
              Qsp14}, X) ->
    {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
     {Qp7, Qp8, Qp9, queue:in(X, Qp10), Qp11, Qp12, Qp13},
     Qsp14};
in_lower(11, {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
              {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
              Qsp14}, X) ->
    {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
     {Qp7, Qp8, Qp9, Qp10, queue:in(X, Qp11), Qp12, Qp13},
     Qsp14};
in_lower(12, {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
              {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
              Qsp14}, X) ->
    {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
     {Qp7, Qp8, Qp9, Qp10, Qp11, queue:in(X, Qp12), Qp13},
     Qsp14};
in_lower(13, {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
              {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
              Qsp14}, X) ->
    {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
     {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, queue:in(X, Qp13)},
     Qsp14};
in_lower(14, {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
              {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}}, X) ->
    {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
     {queue:in(X, Qp14), Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}};
in_lower(15, {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
              {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}}, X) ->
    {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
     {Qp14, queue:in(X, Qp15), Qp16, Qp17, Qp18, Qp19, Qp20}};
in_lower(16, {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
              {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}}, X) ->
    {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
     {Qp14, Qp15, queue:in(X, Qp16), Qp17, Qp18, Qp19, Qp20}};
in_lower(17, {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
              {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}}, X) ->
    {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
     {Qp14, Qp15, Qp16, queue:in(X, Qp17), Qp18, Qp19, Qp20}};
in_lower(18, {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
              {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}}, X) ->
    {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
     {Qp14, Qp15, Qp16, Qp17, queue:in(X, Qp18), Qp19, Qp20}};
in_lower(19, {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
              {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}}, X) ->
    {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
     {Qp14, Qp15, Qp16, Qp17, Qp18, queue:in(X, Qp19), Qp20}};
in_lower(20, {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
              {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}}, X) ->
    {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
     {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, queue:in(X, Qp20)}}.

out_current(-20,
            {_, {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
             Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14} = Q, ReturnType) ->
    {Value, NewQn20} = queue:out(Qn20),
    if
        Value =:= empty ->
            out_current(-19, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, -20};
                true ->
                    Value
            end,
            {NewValue,
             {-20,
              {NewQn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
              Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}}
    end;
out_current(-19,
            {_, {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
             Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14} = Q, ReturnType) ->
    {Value, NewQn19} = queue:out(Qn19),
    if
        Value =:= empty ->
            out_current(-18, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, -19};
                true ->
                    Value
            end,
            {NewValue,
             {-19,
              {Qn20, NewQn19, Qn18, Qn17, Qn16, Qn15, Qn14},
              Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}}
    end;
out_current(-18,
            {_, {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
             Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14} = Q, ReturnType) ->
    {Value, NewQn18} = queue:out(Qn18),
    if
        Value =:= empty ->
            out_current(-17, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, -18};
                true ->
                    Value
            end,
            {NewValue,
             {-18,
              {Qn20, Qn19, NewQn18, Qn17, Qn16, Qn15, Qn14},
              Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}}
    end;
out_current(-17,
            {_, {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
             Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14} = Q, ReturnType) ->
    {Value, NewQn17} = queue:out(Qn17),
    if
        Value =:= empty ->
            out_current(-16, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, -17};
                true ->
                    Value
            end,
            {NewValue,
             {-17,
              {Qn20, Qn19, Qn18, NewQn17, Qn16, Qn15, Qn14},
              Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}}
    end;
out_current(-16,
            {_, {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
             Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14} = Q, ReturnType) ->
    {Value, NewQn16} = queue:out(Qn16),
    if
        Value =:= empty ->
            out_current(-15, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, -16};
                true ->
                    Value
            end,
            {NewValue,
             {-16,
              {Qn20, Qn19, Qn18, Qn17, NewQn16, Qn15, Qn14},
              Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}}
    end;
out_current(-15,
            {_, {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
             Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14} = Q, ReturnType) ->
    {Value, NewQn15} = queue:out(Qn15),
    if
        Value =:= empty ->
            out_current(-14, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, -15};
                true ->
                    Value
            end,
            {NewValue,
             {-15,
              {Qn20, Qn19, Qn18, Qn17, Qn16, NewQn15, Qn14},
              Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}}
    end;
out_current(-14,
            {_, {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
             Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14} = Q, ReturnType) ->
    {Value, NewQn14} = queue:out(Qn14),
    if
        Value =:= empty ->
            out_current(-13, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, -14};
                true ->
                    Value
            end,
            {NewValue,
             {-14,
              {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, NewQn14},
              Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}}
    end;
out_current(-13,
            {_, Qsn14,
             {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
             Qsn1, Q0, Qsp1, Qsp7, Qsp14} = Q, ReturnType) ->
    {Value, NewQn13} = queue:out(Qn13),
    if
        Value =:= empty ->
            out_current(-12, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, -13};
                true ->
                    Value
            end,
            {NewValue,
             {-13, Qsn14,
              {NewQn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
              Qsn1, Q0, Qsp1, Qsp7, Qsp14}}
    end;
out_current(-12,
            {_, Qsn14,
             {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
             Qsn1, Q0, Qsp1, Qsp7, Qsp14} = Q, ReturnType) ->
    {Value, NewQn12} = queue:out(Qn12),
    if
        Value =:= empty ->
            out_current(-11, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, -12};
                true ->
                    Value
            end,
            {NewValue,
             {-12, Qsn14,
              {Qn13, NewQn12, Qn11, Qn10, Qn9, Qn8, Qn7},
              Qsn1, Q0, Qsp1, Qsp7, Qsp14}}
    end;
out_current(-11,
            {_, Qsn14,
             {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
             Qsn1, Q0, Qsp1, Qsp7, Qsp14} = Q, ReturnType) ->
    {Value, NewQn11} = queue:out(Qn11),
    if
        Value =:= empty ->
            out_current(-10, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, -11};
                true ->
                    Value
            end,
            {NewValue,
             {-11, Qsn14,
              {Qn13, Qn12, NewQn11, Qn10, Qn9, Qn8, Qn7},
              Qsn1, Q0, Qsp1, Qsp7, Qsp14}}
    end;
out_current(-10,
            {_, Qsn14,
             {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
             Qsn1, Q0, Qsp1, Qsp7, Qsp14} = Q, ReturnType) ->
    {Value, NewQn10} = queue:out(Qn10),
    if
        Value =:= empty ->
            out_current(-9, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, -10};
                true ->
                    Value
            end,
            {NewValue,
             {-10, Qsn14,
              {Qn13, Qn12, Qn11, NewQn10, Qn9, Qn8, Qn7},
              Qsn1, Q0, Qsp1, Qsp7, Qsp14}}
    end;
out_current(-9,
            {_, Qsn14,
             {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
             Qsn1, Q0, Qsp1, Qsp7, Qsp14} = Q, ReturnType) ->
    {Value, NewQn9} = queue:out(Qn9),
    if
        Value =:= empty ->
            out_current(-8, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, -9};
                true ->
                    Value
            end,
            {NewValue,
             {-9, Qsn14,
              {Qn13, Qn12, Qn11, Qn10, NewQn9, Qn8, Qn7},
              Qsn1, Q0, Qsp1, Qsp7, Qsp14}}
    end;
out_current(-8,
            {_, Qsn14,
             {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
             Qsn1, Q0, Qsp1, Qsp7, Qsp14} = Q, ReturnType) ->
    {Value, NewQn8} = queue:out(Qn8),
    if
        Value =:= empty ->
            out_current(-7, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, -8};
                true ->
                    Value
            end,
            {NewValue,
             {-8, Qsn14,
              {Qn13, Qn12, Qn11, Qn10, Qn9, NewQn8, Qn7},
              Qsn1, Q0, Qsp1, Qsp7, Qsp14}}
    end;
out_current(-7,
            {_, Qsn14,
             {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
             Qsn1, Q0, Qsp1, Qsp7, Qsp14} = Q, ReturnType) ->
    {Value, NewQn7} = queue:out(Qn7),
    if
        Value =:= empty ->
            out_current(-6, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, -7};
                true ->
                    Value
            end,
            {NewValue,
             {-7, Qsn14,
              {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, NewQn7},
              Qsn1, Q0, Qsp1, Qsp7, Qsp14}}
    end;
out_current(-6,
            {_, Qsn14, Qsn7,
             {Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
             Q0, Qsp1, Qsp7, Qsp14} = Q, ReturnType) ->
    {Value, NewQn6} = queue:out(Qn6),
    if
        Value =:= empty ->
            out_current(-5, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, -6};
                true ->
                    Value
            end,
            {NewValue,
             {-6, Qsn14, Qsn7,
              {NewQn6, Qn5, Qn4, Qn3, Qn2, Qn1},
              Q0, Qsp1, Qsp7, Qsp14}}
    end;
out_current(-5,
            {_, Qsn14, Qsn7,
             {Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
             Q0, Qsp1, Qsp7, Qsp14} = Q, ReturnType) ->
    {Value, NewQn5} = queue:out(Qn5),
    if
        Value =:= empty ->
            out_current(-4, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, -5};
                true ->
                    Value
            end,
            {NewValue,
             {-5, Qsn14, Qsn7,
              {Qn6, NewQn5, Qn4, Qn3, Qn2, Qn1},
              Q0, Qsp1, Qsp7, Qsp14}}
    end;
out_current(-4,
            {_, Qsn14, Qsn7,
             {Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
             Q0, Qsp1, Qsp7, Qsp14} = Q, ReturnType) ->
    {Value, NewQn4} = queue:out(Qn4),
    if
        Value =:= empty ->
            out_current(-3, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, -4};
                true ->
                    Value
            end,
            {NewValue,
             {-4, Qsn14, Qsn7,
              {Qn6, Qn5, NewQn4, Qn3, Qn2, Qn1},
              Q0, Qsp1, Qsp7, Qsp14}}
    end;
out_current(-3,
            {_, Qsn14, Qsn7,
             {Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
             Q0, Qsp1, Qsp7, Qsp14} = Q, ReturnType) ->
    {Value, NewQn3} = queue:out(Qn3),
    if
        Value =:= empty ->
            out_current(-2, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, -3};
                true ->
                    Value
            end,
            {NewValue,
             {-3, Qsn14, Qsn7,
              {Qn6, Qn5, Qn4, NewQn3, Qn2, Qn1},
              Q0, Qsp1, Qsp7, Qsp14}}
    end;
out_current(-2,
            {_, Qsn14, Qsn7,
             {Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
             Q0, Qsp1, Qsp7, Qsp14} = Q, ReturnType) ->
    {Value, NewQn2} = queue:out(Qn2),
    if
        Value =:= empty ->
            out_current(-1, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, -2};
                true ->
                    Value
            end,
            {NewValue,
             {-2, Qsn14, Qsn7,
              {Qn6, Qn5, Qn4, Qn3, NewQn2, Qn1},
              Q0, Qsp1, Qsp7, Qsp14}}
    end;
out_current(-1,
            {_, Qsn14, Qsn7,
             {Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
             Q0, Qsp1, Qsp7, Qsp14} = Q, ReturnType) ->
    {Value, NewQn1} = queue:out(Qn1),
    if
        Value =:= empty ->
            out_current(0, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, -1};
                true ->
                    Value
            end,
            {NewValue,
             {-1, Qsn14, Qsn7,
              {Qn6, Qn5, Qn4, Qn3, Qn2, NewQn1},
              Q0, Qsp1, Qsp7, Qsp14}}
    end;
out_current(0,
            {_, Qsn14, Qsn7, Qsn1,
             Q0, Qsp1, Qsp7, Qsp14} = Q, ReturnType) ->
    {Value, NewQ0} = queue:out(Q0),
    if
        Value =:= empty ->
            out_current(1, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, 0};
                true ->
                    Value
            end,
            {NewValue,
             {0, Qsn14, Qsn7, Qsn1,
              NewQ0,
              Qsp1, Qsp7, Qsp14}}
    end;
out_current(1,
            {_, Qsn14, Qsn7, Qsn1, Q0,
             {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6},
             Qsp7, Qsp14} = Q, ReturnType) ->
    {Value, NewQp1} = queue:out(Qp1),
    if
        Value =:= empty ->
            out_current(2, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, 1};
                true ->
                    Value
            end,
            {NewValue,
             {1, Qsn14, Qsn7, Qsn1, Q0,
              {NewQp1, Qp2, Qp3, Qp4, Qp5, Qp6},
              Qsp7, Qsp14}}
    end;
out_current(2,
            {_, Qsn14, Qsn7, Qsn1, Q0,
             {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6},
             Qsp7, Qsp14} = Q, ReturnType) ->
    {Value, NewQp2} = queue:out(Qp2),
    if
        Value =:= empty ->
            out_current(3, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, 2};
                true ->
                    Value
            end,
            {NewValue,
             {2, Qsn14, Qsn7, Qsn1, Q0,
              {Qp1, NewQp2, Qp3, Qp4, Qp5, Qp6},
              Qsp7, Qsp14}}
    end;
out_current(3,
            {_, Qsn14, Qsn7, Qsn1, Q0,
             {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6},
             Qsp7, Qsp14} = Q, ReturnType) ->
    {Value, NewQp3} = queue:out(Qp3),
    if
        Value =:= empty ->
            out_current(4, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, 3};
                true ->
                    Value
            end,
            {NewValue,
             {3, Qsn14, Qsn7, Qsn1, Q0,
              {Qp1, Qp2, NewQp3, Qp4, Qp5, Qp6},
              Qsp7, Qsp14}}
    end;
out_current(4,
            {_, Qsn14, Qsn7, Qsn1, Q0,
             {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6},
             Qsp7, Qsp14} = Q, ReturnType) ->
    {Value, NewQp4} = queue:out(Qp4),
    if
        Value =:= empty ->
            out_current(5, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, 4};
                true ->
                    Value
            end,
            {NewValue,
             {4, Qsn14, Qsn7, Qsn1, Q0,
              {Qp1, Qp2, Qp3, NewQp4, Qp5, Qp6},
              Qsp7, Qsp14}}
    end;
out_current(5,
            {_, Qsn14, Qsn7, Qsn1, Q0,
             {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6},
             Qsp7, Qsp14} = Q, ReturnType) ->
    {Value, NewQp5} = queue:out(Qp5),
    if
        Value =:= empty ->
            out_current(6, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, 5};
                true ->
                    Value
            end,
            {NewValue,
             {5, Qsn14, Qsn7, Qsn1, Q0,
              {Qp1, Qp2, Qp3, Qp4, NewQp5, Qp6},
              Qsp7, Qsp14}}
    end;
out_current(6,
            {_, Qsn14, Qsn7, Qsn1, Q0,
             {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6},
             Qsp7, Qsp14} = Q, ReturnType) ->
    {Value, NewQp6} = queue:out(Qp6),
    if
        Value =:= empty ->
            out_current(7, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, 6};
                true ->
                    Value
            end,
            {NewValue,
             {6, Qsn14, Qsn7, Qsn1, Q0,
              {Qp1, Qp2, Qp3, Qp4, Qp5, NewQp6},
              Qsp7, Qsp14}}
    end;
out_current(7,
            {_, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
             {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
             Qsp14} = Q, ReturnType) ->
    {Value, NewQp7} = queue:out(Qp7),
    if
        Value =:= empty ->
            out_current(8, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, 7};
                true ->
                    Value
            end,
            {NewValue,
             {7, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
              {NewQp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
              Qsp14}}
    end;
out_current(8,
            {_, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
             {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
             Qsp14} = Q, ReturnType) ->
    {Value, NewQp8} = queue:out(Qp8),
    if
        Value =:= empty ->
            out_current(9, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, 8};
                true ->
                    Value
            end,
            {NewValue,
             {8, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
              {Qp7, NewQp8, Qp9, Qp10, Qp11, Qp12, Qp13},
              Qsp14}}
    end;
out_current(9,
            {_, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
             {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
             Qsp14} = Q, ReturnType) ->
    {Value, NewQp9} = queue:out(Qp9),
    if
        Value =:= empty ->
            out_current(10, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, 9};
                true ->
                    Value
            end,
            {NewValue,
             {9, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
              {Qp7, Qp8, NewQp9, Qp10, Qp11, Qp12, Qp13},
              Qsp14}}
    end;
out_current(10,
            {_, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
             {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
             Qsp14} = Q, ReturnType) ->
    {Value, NewQp10} = queue:out(Qp10),
    if
        Value =:= empty ->
            out_current(11, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, 10};
                true ->
                    Value
            end,
            {NewValue,
             {10, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
              {Qp7, Qp8, Qp9, NewQp10, Qp11, Qp12, Qp13},
              Qsp14}}
    end;
out_current(11,
            {_, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
             {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
             Qsp14} = Q, ReturnType) ->
    {Value, NewQp11} = queue:out(Qp11),
    if
        Value =:= empty ->
            out_current(12, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, 11};
                true ->
                    Value
            end,
            {NewValue,
             {11, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
              {Qp7, Qp8, Qp9, Qp10, NewQp11, Qp12, Qp13},
              Qsp14}}
    end;
out_current(12,
            {_, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
             {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
             Qsp14} = Q, ReturnType) ->
    {Value, NewQp12} = queue:out(Qp12),
    if
        Value =:= empty ->
            out_current(13, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, 12};
                true ->
                    Value
            end,
            {NewValue,
             {12, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
              {Qp7, Qp8, Qp9, Qp10, Qp11, NewQp12, Qp13},
              Qsp14}}
    end;
out_current(13,
            {_, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
             {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
             Qsp14} = Q, ReturnType) ->
    {Value, NewQp13} = queue:out(Qp13),
    if
        Value =:= empty ->
            out_current(14, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, 13};
                true ->
                    Value
            end,
            {NewValue,
             {13, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
              {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, NewQp13},
              Qsp14}}
    end;
out_current(14,
            {_, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
             {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}} = Q, ReturnType) ->
    {Value, NewQp14} = queue:out(Qp14),
    if
        Value =:= empty ->
            out_current(15, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, 14};
                true ->
                    Value
            end,
            {NewValue,
             {14, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
              {NewQp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}}}
    end;
out_current(15,
            {_, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
             {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}} = Q, ReturnType) ->
    {Value, NewQp15} = queue:out(Qp15),
    if
        Value =:= empty ->
            out_current(16, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, 15};
                true ->
                    Value
            end,
            {NewValue,
             {15, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
              {Qp14, NewQp15, Qp16, Qp17, Qp18, Qp19, Qp20}}}
    end;
out_current(16,
            {_, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
             {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}} = Q, ReturnType) ->
    {Value, NewQp16} = queue:out(Qp16),
    if
        Value =:= empty ->
            out_current(17, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, 16};
                true ->
                    Value
            end,
            {NewValue,
             {16, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
              {Qp14, Qp15, NewQp16, Qp17, Qp18, Qp19, Qp20}}}
    end;
out_current(17,
            {_, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
             {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}} = Q, ReturnType) ->
    {Value, NewQp17} = queue:out(Qp17),
    if
        Value =:= empty ->
            out_current(18, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, 17};
                true ->
                    Value
            end,
            {NewValue,
             {17, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
              {Qp14, Qp15, Qp16, NewQp17, Qp18, Qp19, Qp20}}}
    end;
out_current(18,
            {_, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
             {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}} = Q, ReturnType) ->
    {Value, NewQp18} = queue:out(Qp18),
    if
        Value =:= empty ->
            out_current(19, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, 18};
                true ->
                    Value
            end,
            {NewValue,
             {18, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
              {Qp14, Qp15, Qp16, Qp17, NewQp18, Qp19, Qp20}}}
    end;
out_current(19,
            {_, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
             {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}} = Q, ReturnType) ->
    {Value, NewQp19} = queue:out(Qp19),
    if
        Value =:= empty ->
            out_current(20, Q, ReturnType);
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, 19};
                true ->
                    Value
            end,
            {NewValue,
             {19, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
              {Qp14, Qp15, Qp16, Qp17, Qp18, NewQp19, Qp20}}}
    end;
out_current(20,
            {_, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
             {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20} = Qsp14}, ReturnType) ->
    {Value, NewQp20} = queue:out(Qp20),
    if
        Value =:= empty ->
            {empty, {empty, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}};
        true ->
            NewValue = if
                ReturnType =:= priority ->
                    {value, Contents} = Value,
                    {value, Contents, 20};
                true ->
                    Value
            end,
            {NewValue,
             {20, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
              {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, NewQp20}}}
    end.

out_specific(-20,
             {Pc,
              {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
              Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}) ->
    {Value, NewQn20} = queue:out(Qn20),
    {Value,
     {Pc,
      {NewQn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
      Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}};
out_specific(-19,
             {Pc,
              {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
              Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}) ->
    {Value, NewQn19} = queue:out(Qn19),
    {Value,
     {Pc,
      {Qn20, NewQn19, Qn18, Qn17, Qn16, Qn15, Qn14},
      Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}};
out_specific(-18,
             {Pc,
              {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
              Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}) ->
    {Value, NewQn18} = queue:out(Qn18),
    {Value,
     {Pc,
      {Qn20, Qn19, NewQn18, Qn17, Qn16, Qn15, Qn14},
      Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}};
out_specific(-17,
             {Pc,
              {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
              Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}) ->
    {Value, NewQn17} = queue:out(Qn17),
    {Value,
     {Pc,
      {Qn20, Qn19, Qn18, NewQn17, Qn16, Qn15, Qn14},
      Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}};
out_specific(-16,
             {Pc,
              {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
              Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}) ->
    {Value, NewQn16} = queue:out(Qn16),
    {Value,
     {Pc,
      {Qn20, Qn19, Qn18, Qn17, NewQn16, Qn15, Qn14},
      Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}};
out_specific(-15,
             {Pc,
              {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
              Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}) ->
    {Value, NewQn15} = queue:out(Qn15),
    {Value,
     {Pc,
      {Qn20, Qn19, Qn18, Qn17, Qn16, NewQn15, Qn14},
      Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}};
out_specific(-14,
             {Pc,
              {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, Qn14},
              Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}) ->
    {Value, NewQn14} = queue:out(Qn14),
    {Value,
     {Pc,
      {Qn20, Qn19, Qn18, Qn17, Qn16, Qn15, NewQn14},
      Qsn7, Qsn1, Q0, Qsp1, Qsp7, Qsp14}};
out_specific(-13,
             {Pc, Qsn14,
              {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
              Qsn1, Q0, Qsp1, Qsp7, Qsp14}) ->
    {Value, NewQn13} = queue:out(Qn13),
    {Value,
     {Pc, Qsn14,
      {NewQn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
      Qsn1, Q0, Qsp1, Qsp7, Qsp14}};
out_specific(-12,
             {Pc, Qsn14,
              {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
              Qsn1, Q0, Qsp1, Qsp7, Qsp14}) ->
    {Value, NewQn12} = queue:out(Qn12),
    {Value,
     {Pc, Qsn14,
      {Qn13, NewQn12, Qn11, Qn10, Qn9, Qn8, Qn7},
      Qsn1, Q0, Qsp1, Qsp7, Qsp14}};
out_specific(-11,
             {Pc, Qsn14,
              {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
              Qsn1, Q0, Qsp1, Qsp7, Qsp14}) ->
    {Value, NewQn11} = queue:out(Qn11),
    {Value,
     {Pc, Qsn14,
      {Qn13, Qn12, NewQn11, Qn10, Qn9, Qn8, Qn7},
      Qsn1, Q0, Qsp1, Qsp7, Qsp14}};
out_specific(-10,
             {Pc, Qsn14,
              {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
              Qsn1, Q0, Qsp1, Qsp7, Qsp14}) ->
    {Value, NewQn10} = queue:out(Qn10),
    {Value,
     {Pc, Qsn14,
      {Qn13, Qn12, Qn11, NewQn10, Qn9, Qn8, Qn7},
      Qsn1, Q0, Qsp1, Qsp7, Qsp14}};
out_specific(-9,
             {Pc, Qsn14,
              {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
              Qsn1, Q0, Qsp1, Qsp7, Qsp14}) ->
    {Value, NewQn9} = queue:out(Qn9),
    {Value,
     {Pc, Qsn14,
      {Qn13, Qn12, Qn11, Qn10, NewQn9, Qn8, Qn7},
      Qsn1, Q0, Qsp1, Qsp7, Qsp14}};
out_specific(-8,
             {Pc, Qsn14,
              {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
              Qsn1, Q0, Qsp1, Qsp7, Qsp14}) ->
    {Value, NewQn8} = queue:out(Qn8),
    {Value,
     {Pc, Qsn14,
      {Qn13, Qn12, Qn11, Qn10, Qn9, NewQn8, Qn7},
      Qsn1, Q0, Qsp1, Qsp7, Qsp14}};
out_specific(-7,
             {Pc, Qsn14,
              {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, Qn7},
              Qsn1, Q0, Qsp1, Qsp7, Qsp14}) ->
    {Value, NewQn7} = queue:out(Qn7),
    {Value,
     {Pc, Qsn14,
      {Qn13, Qn12, Qn11, Qn10, Qn9, Qn8, NewQn7},
      Qsn1, Q0, Qsp1, Qsp7, Qsp14}};
out_specific(-6,
             {Pc, Qsn14, Qsn7,
              {Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
              Q0, Qsp1, Qsp7, Qsp14}) ->
    {Value, NewQn6} = queue:out(Qn6),
    {Value,
     {Pc, Qsn14, Qsn7,
      {NewQn6, Qn5, Qn4, Qn3, Qn2, Qn1},
      Q0, Qsp1, Qsp7, Qsp14}};
out_specific(-5,
             {Pc, Qsn14, Qsn7,
              {Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
              Q0, Qsp1, Qsp7, Qsp14}) ->
    {Value, NewQn5} = queue:out(Qn5),
    {Value,
     {Pc, Qsn14, Qsn7,
      {Qn6, NewQn5, Qn4, Qn3, Qn2, Qn1},
      Q0, Qsp1, Qsp7, Qsp14}};
out_specific(-4,
             {Pc, Qsn14, Qsn7,
              {Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
              Q0, Qsp1, Qsp7, Qsp14}) ->
    {Value, NewQn4} = queue:out(Qn4),
    {Value,
     {Pc, Qsn14, Qsn7,
      {Qn6, Qn5, NewQn4, Qn3, Qn2, Qn1},
      Q0, Qsp1, Qsp7, Qsp14}};
out_specific(-3,
             {Pc, Qsn14, Qsn7,
              {Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
              Q0, Qsp1, Qsp7, Qsp14}) ->
    {Value, NewQn3} = queue:out(Qn3),
    {Value,
     {Pc, Qsn14, Qsn7,
      {Qn6, Qn5, Qn4, NewQn3, Qn2, Qn1},
      Q0, Qsp1, Qsp7, Qsp14}};
out_specific(-2,
             {Pc, Qsn14, Qsn7,
              {Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
              Q0, Qsp1, Qsp7, Qsp14}) ->
    {Value, NewQn2} = queue:out(Qn2),
    {Value,
     {Pc, Qsn14, Qsn7,
      {Qn6, Qn5, Qn4, Qn3, NewQn2, Qn1},
      Q0, Qsp1, Qsp7, Qsp14}};
out_specific(-1,
             {Pc, Qsn14, Qsn7,
              {Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
              Q0, Qsp1, Qsp7, Qsp14}) ->
    {Value, NewQn1} = queue:out(Qn1),
    {Value,
     {Pc, Qsn14, Qsn7,
      {Qn6, Qn5, Qn4, Qn3, Qn2, NewQn1},
      Q0, Qsp1, Qsp7, Qsp14}};
out_specific(0,
             {Pc, Qsn14, Qsn7, Qsn1,
              Q0, Qsp1, Qsp7, Qsp14}) ->
    {Value, NewQ0} = queue:out(Q0),
    {Value,
     {Pc, Qsn14, Qsn7, Qsn1,
      NewQ0,
      Qsp1, Qsp7, Qsp14}};
out_specific(1,
             {Pc, Qsn14, Qsn7, Qsn1, Q0,
              {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6},
              Qsp7, Qsp14}) ->
    {Value, NewQp1} = queue:out(Qp1),
    {Value,
     {Pc, Qsn14, Qsn7, Qsn1, Q0,
      {NewQp1, Qp2, Qp3, Qp4, Qp5, Qp6},
      Qsp7, Qsp14}};
out_specific(2,
             {Pc, Qsn14, Qsn7, Qsn1, Q0,
              {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6},
              Qsp7, Qsp14}) ->
    {Value, NewQp2} = queue:out(Qp2),
    {Value,
     {Pc, Qsn14, Qsn7, Qsn1, Q0,
      {Qp1, NewQp2, Qp3, Qp4, Qp5, Qp6},
      Qsp7, Qsp14}};
out_specific(3,
             {Pc, Qsn14, Qsn7, Qsn1, Q0,
              {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6},
              Qsp7, Qsp14}) ->
    {Value, NewQp3} = queue:out(Qp3),
    {Value,
     {Pc, Qsn14, Qsn7, Qsn1, Q0,
      {Qp1, Qp2, NewQp3, Qp4, Qp5, Qp6},
      Qsp7, Qsp14}};
out_specific(4,
             {Pc, Qsn14, Qsn7, Qsn1, Q0,
              {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6},
              Qsp7, Qsp14}) ->
    {Value, NewQp4} = queue:out(Qp4),
    {Value,
     {Pc, Qsn14, Qsn7, Qsn1, Q0,
      {Qp1, Qp2, Qp3, NewQp4, Qp5, Qp6},
      Qsp7, Qsp14}};
out_specific(5,
             {Pc, Qsn14, Qsn7, Qsn1, Q0,
              {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6},
              Qsp7, Qsp14}) ->
    {Value, NewQp5} = queue:out(Qp5),
    {Value,
     {Pc, Qsn14, Qsn7, Qsn1, Q0,
      {Qp1, Qp2, Qp3, Qp4, NewQp5, Qp6},
      Qsp7, Qsp14}};
out_specific(6,
             {Pc, Qsn14, Qsn7, Qsn1, Q0,
              {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6},
              Qsp7, Qsp14}) ->
    {Value, NewQp6} = queue:out(Qp6),
    {Value,
     {Pc, Qsn14, Qsn7, Qsn1, Q0,
      {Qp1, Qp2, Qp3, Qp4, Qp5, NewQp6},
      Qsp7, Qsp14}};
out_specific(7,
             {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
              {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
              Qsp14}) ->
    {Value, NewQp7} = queue:out(Qp7),
    {Value,
     {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
      {NewQp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
      Qsp14}};
out_specific(8,
             {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
              {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
              Qsp14}) ->
    {Value, NewQp8} = queue:out(Qp8),
    {Value,
     {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
      {Qp7, NewQp8, Qp9, Qp10, Qp11, Qp12, Qp13},
      Qsp14}};
out_specific(9,
             {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
              {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
              Qsp14}) ->
    {Value, NewQp9} = queue:out(Qp9),
    {Value,
     {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
      {Qp7, Qp8, NewQp9, Qp10, Qp11, Qp12, Qp13},
      Qsp14}};
out_specific(10,
             {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
              {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
              Qsp14}) ->
    {Value, NewQp10} = queue:out(Qp10),
    {Value,
     {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
      {Qp7, Qp8, Qp9, NewQp10, Qp11, Qp12, Qp13},
      Qsp14}};
out_specific(11,
             {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
              {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
              Qsp14}) ->
    {Value, NewQp11} = queue:out(Qp11),
    {Value,
     {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
      {Qp7, Qp8, Qp9, Qp10, NewQp11, Qp12, Qp13},
      Qsp14}};
out_specific(12,
             {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
              {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
              Qsp14}) ->
    {Value, NewQp12} = queue:out(Qp12),
    {Value,
     {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
      {Qp7, Qp8, Qp9, Qp10, Qp11, NewQp12, Qp13},
      Qsp14}};
out_specific(13,
             {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
              {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, Qp13},
              Qsp14}) ->
    {Value, NewQp13} = queue:out(Qp13),
    {Value,
     {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1,
      {Qp7, Qp8, Qp9, Qp10, Qp11, Qp12, NewQp13},
      Qsp14}};
out_specific(14,
             {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
              {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}}) ->
    {Value, NewQp14} = queue:out(Qp14),
    {Value,
     {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
      {NewQp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}}};
out_specific(15,
             {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
              {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}}) ->
    {Value, NewQp15} = queue:out(Qp15),
    {Value,
     {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
      {Qp14, NewQp15, Qp16, Qp17, Qp18, Qp19, Qp20}}};
out_specific(16,
             {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
              {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}}) ->
    {Value, NewQp16} = queue:out(Qp16),
    {Value,
     {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
      {Qp14, Qp15, NewQp16, Qp17, Qp18, Qp19, Qp20}}};
out_specific(17,
             {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
              {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}}) ->
    {Value, NewQp17} = queue:out(Qp17),
    {Value,
     {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
      {Qp14, Qp15, Qp16, NewQp17, Qp18, Qp19, Qp20}}};
out_specific(18,
             {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
              {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}}) ->
    {Value, NewQp18} = queue:out(Qp18),
    {Value,
     {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
      {Qp14, Qp15, Qp16, Qp17, NewQp18, Qp19, Qp20}}};
out_specific(19,
             {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
              {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}}) ->
    {Value, NewQp19} = queue:out(Qp19),
    {Value,
     {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
      {Qp14, Qp15, Qp16, Qp17, Qp18, NewQp19, Qp20}}};
out_specific(20,
             {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
              {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, Qp20}}) ->
    {Value, NewQp20} = queue:out(Qp20),
    {Value,
     {Pc, Qsn14, Qsn7, Qsn1, Q0, Qsp1, Qsp7,
      {Qp14, Qp15, Qp16, Qp17, Qp18, Qp19, NewQp20}}}.
