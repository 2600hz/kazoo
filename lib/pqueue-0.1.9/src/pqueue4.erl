%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Static Priority Queue.==
%%% This priority queue implementation depends on a static number of priorities
%%% (-128 (high) to 128 (low)) so that tuple access times can be exploited for
%%% quick in/out priority queue operations.  This implementation was created to
%%% avoid the slowness within the priority queue used by both RabbitMQ and Riak
%%% (https://github.com/basho/riak_core/blob/master/src/priority_queue.erl).
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011-2012, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2012 Michael Truog
%%% @version 0.2.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(pqueue4).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([filter/3,     % O(N)
         in/2,         % O(1)
         in/3,         % O(1)
         is_empty/1,   % O(1)
         is_queue/1,   % O(1)
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

-type pqueue4() ::
    {integer() | 'empty',
     {queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue(),
      queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue()},
     {queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue(),
      queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue()},
     {queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue(),
      queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue()},
     {queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue(),
      queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue()},
     {queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue(),
      queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue()},
     {queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue(),
      queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue()},
     {queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue(),
      queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue()},
     {queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue(),
      queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue()},
     queue(),
     {queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue(),
      queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue()},
     {queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue(),
      queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue()},
     {queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue(),
      queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue()},
     {queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue(),
      queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue()},
     {queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue(),
      queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue()},
     {queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue(),
      queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue()},
     {queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue(),
      queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue()},
     {queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue(),
      queue(), queue(), queue(), queue(), queue(), queue(), queue(), queue()}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Filter a specific priority within the priority queue.===
%% O(N)
%% @end
%%-------------------------------------------------------------------------

-spec filter(fun((any()) -> boolean()), integer(), pqueue4()) -> pqueue4().

filter(F, P, Q) when is_function(F, 1) ->
    filter_priority(P, F, Q).

%%-------------------------------------------------------------------------
%% @doc
%% ===Append an item to the tail of the 0 priority queue.===
%% O(1)
%% @end
%%-------------------------------------------------------------------------

-spec in(term(), pqueue4()) -> pqueue4().

in(X, Q) ->
    in(X, 0, Q).

%%-------------------------------------------------------------------------
%% @doc
%% ===Append an item to the tail of a specific priority queue.===
%% O(1)
%% @end
%%-------------------------------------------------------------------------

-spec in(term(), integer(), pqueue4()) -> pqueue4().

in(_, P, _)
    when P < -128; P > 128 ->
    throw(badarg);
in(X, P, {empty, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _} = Q) ->
    in_higher(P, Q, X);
in(X, P, {Pc, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _} = Q)
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

-spec is_empty(pqueue4()) -> 'true' | 'false'.

is_empty({empty, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _}) ->
    true;
is_empty({_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _} = Q) ->
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

-spec is_queue(pqueue4()) -> 'true' | 'false'.

is_queue({Pc,
          Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
          Q0,
          Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128})
    when tuple_size(Qn128) == 16, tuple_size(Qn112) == 16,
         tuple_size(Qn96) == 16, tuple_size(Qn80) == 16,
         tuple_size(Qn64) == 16, tuple_size(Qn48) == 16,
         tuple_size(Qn32) == 16, tuple_size(Qn16) == 16,
         tuple_size(Qp16) == 16, tuple_size(Qp32) == 16,
         tuple_size(Qp48) == 16, tuple_size(Qp64) == 16,
         tuple_size(Qp80) == 16, tuple_size(Qp96) == 16,
         tuple_size(Qp112) == 16, tuple_size(Qp128) == 16 ->
    (((Pc =:= empty) or is_integer(Pc)) and queue:is_queue(Q0));
is_queue(_) ->
    false.

%%-------------------------------------------------------------------------
%% @doc
%% ===Determine the length of a priority queue.===
%% O(N)
%% @end
%%-------------------------------------------------------------------------

-spec len(pqueue4()) -> non_neg_integer().

len({_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _} = Q) ->
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

-spec new() -> pqueue4().

new() ->
    {empty,                               % current priority
     erlang:make_tuple(16, queue:new()),  % priority [-128..-113]
     erlang:make_tuple(16, queue:new()),  % priority [-112.. -97]
     erlang:make_tuple(16, queue:new()),  % priority [ -96.. -81]
     erlang:make_tuple(16, queue:new()),  % priority [ -80.. -65]
     erlang:make_tuple(16, queue:new()),  % priority [ -64.. -49]
     erlang:make_tuple(16, queue:new()),  % priority [ -48.. -33]
     erlang:make_tuple(16, queue:new()),  % priority [ -32.. -17]
     erlang:make_tuple(16, queue:new()),  % priority [ -16..  -1]
     queue:new(),                         % priority 0 (default)
     erlang:make_tuple(16, queue:new()),  % priority [   1..  16]
     erlang:make_tuple(16, queue:new()),  % priority [  17..  32]
     erlang:make_tuple(16, queue:new()),  % priority [  33..  48]
     erlang:make_tuple(16, queue:new()),  % priority [  49..  64]
     erlang:make_tuple(16, queue:new()),  % priority [  65..  80]
     erlang:make_tuple(16, queue:new()),  % priority [  81..  96]
     erlang:make_tuple(16, queue:new()),  % priority [  97.. 112]
     erlang:make_tuple(16, queue:new())}. % priority [ 113.. 128]

%%-------------------------------------------------------------------------
%% @doc
%% ===Take an item from the head of the priority queue.===
%% O(1) amortized, O(N) worst case
%% @end
%%-------------------------------------------------------------------------

-spec out(pqueue4()) ->
    {{'value', term()}, pqueue4()} | {'empty', pqueue4()}.

out({empty, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _} = Q) ->
    {empty, Q};
out({Pc, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _} = Q) ->
    out_current(Pc, Q).

%%-------------------------------------------------------------------------
%% @doc
%% ===Take an item of a specific priority from the head of the queue.===
%% O(1) amortized, O(N) worst case
%% @end
%%-------------------------------------------------------------------------

-spec out(integer(), pqueue4()) ->
    {{'value', term()}, pqueue4()} | {'empty', pqueue4()}.

out(P, _)
    when P < -128; P > 128 ->
    throw(badarg);
out(_, {empty, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _} = Q) ->
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

-spec pout(pqueue4()) ->
    {{'value', term(), integer()}, pqueue4()} | {'empty', pqueue4()}.

pout({empty, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _} = Q) ->
    {empty, Q};
pout({Pc, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _} = Q) ->
    out_current_p(Pc, Q).

%%-------------------------------------------------------------------------
%% @doc
%% ===Convert the priority queue to a list.===
%% O(N)
%% @end
%%-------------------------------------------------------------------------

-spec to_list(pqueue4()) -> list(term()).

to_list(Q) ->
    to_list([], out(Q)).
to_list(L, {empty, _}) ->
    lists:reverse(L);
to_list(L, {{value, Value}, Q}) ->
    to_list([Value | L], out(Q)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Regression test.===
%% @end
%%-------------------------------------------------------------------------

test() ->
    Q0 = pqueue4:new(),
    true = pqueue4:is_queue(Q0),
    Q1 = pqueue4:in(20, 20, Q0),
    Q2 = pqueue4:in(19, 19, Q1),
    Q3 = pqueue4:in(18, 18, Q2),
    Q4 = pqueue4:in(17, 17, Q3),
    Q5 = pqueue4:in(16, 16, Q4),
    Q6 = pqueue4:in(15, 15, Q5),
    Q7 = pqueue4:in(14, 14, Q6),
    Q8 = pqueue4:in(13, 13, Q7),
    Q9 = pqueue4:in(12, 12, Q8),
    Q10 = pqueue4:in(11, 11, Q9),
    Q11 = pqueue4:in(10, 10, Q10),
    Q12 = pqueue4:in(9, 9, Q11),
    Q13 = pqueue4:in(8, 8, Q12),
    Q14 = pqueue4:in(7, 7, Q13),
    Q15 = pqueue4:in(6, 6, Q14),
    Q16 = pqueue4:in(5, 5, Q15),
    Q17 = pqueue4:in(4, 4, Q16),
    Q18 = pqueue4:in(3, 3, Q17),
    Q19 = pqueue4:in(2, 2, Q18),
    Q20 = pqueue4:in(1, 1, Q19),
    Q21 = pqueue4:in(0, 0, Q20),
    Q22 = pqueue4:in(-1, -1, Q21),
    Q23 = pqueue4:in(-2, -2, Q22),
    Q24 = pqueue4:in(-3, -3, Q23),
    Q25 = pqueue4:in(-4, -4, Q24),
    Q26 = pqueue4:in(-5, -5, Q25),
    Q27 = pqueue4:in(-6, -6, Q26),
    Q28 = pqueue4:in(-7, -7, Q27),
    Q29 = pqueue4:in(-8, -8, Q28),
    Q30 = pqueue4:in(-9, -9, Q29),
    Q31 = pqueue4:in(-10, -10, Q30),
    Q32 = pqueue4:in(-11, -11, Q31),
    Q33 = pqueue4:in(-12, -12, Q32),
    Q34 = pqueue4:in(-13, -13, Q33),
    Q35 = pqueue4:in(-14, -14, Q34),
    Q36 = pqueue4:in(-15, -15, Q35),
    Q37 = pqueue4:in(-16, -16, Q36),
    Q38 = pqueue4:in(-17, -17, Q37),
    Q39 = pqueue4:in(-18, -18, Q38),
    Q40 = pqueue4:in(-19, -19, Q39),
    Q41 = pqueue4:in(-20, -20, Q40),
    Q42 = pqueue4:in(-20, -20, Q41),
    Q43 = pqueue4:in(-19, -19, Q42),
    Q44 = pqueue4:in(-18, -18, Q43),
    Q45 = pqueue4:in(-17, -17, Q44),
    Q46 = pqueue4:in(-16, -16, Q45),
    Q47 = pqueue4:in(-15, -15, Q46),
    Q48 = pqueue4:in(-14, -14, Q47),
    Q49 = pqueue4:in(-13, -13, Q48),
    Q50 = pqueue4:in(-12, -12, Q49),
    Q51 = pqueue4:in(-11, -11, Q50),
    Q52 = pqueue4:in(-10, -10, Q51),
    Q53 = pqueue4:in(-9, -9, Q52),
    Q54 = pqueue4:in(-8, -8, Q53),
    Q55 = pqueue4:in(-7, -7, Q54),
    Q56 = pqueue4:in(-6, -6, Q55),
    Q57 = pqueue4:in(-5, -5, Q56),
    Q58 = pqueue4:in(-4, -4, Q57),
    Q59 = pqueue4:in(-3, -3, Q58),
    Q60 = pqueue4:in(-2, -2, Q59),
    Q61 = pqueue4:in(-1, -1, Q60),
    Q62 = pqueue4:in(0, 0, Q61),
    Q63 = pqueue4:in(1, 1, Q62),
    Q64 = pqueue4:in(2, 2, Q63),
    Q65 = pqueue4:in(3, 3, Q64),
    Q66 = pqueue4:in(4, 4, Q65),
    Q67 = pqueue4:in(5, 5, Q66),
    Q68 = pqueue4:in(6, 6, Q67),
    Q69 = pqueue4:in(7, 7, Q68),
    Q70 = pqueue4:in(8, 8, Q69),
    Q71 = pqueue4:in(9, 9, Q70),
    Q72 = pqueue4:in(10, 10, Q71),
    Q73 = pqueue4:in(11, 11, Q72),
    Q74 = pqueue4:in(12, 12, Q73),
    Q75 = pqueue4:in(13, 13, Q74),
    Q76 = pqueue4:in(14, 14, Q75),
    Q77 = pqueue4:in(15, 15, Q76),
    Q78 = pqueue4:in(16, 16, Q77),
    Q79 = pqueue4:in(17, 17, Q78),
    Q80 = pqueue4:in(18, 18, Q79),
    Q81 = pqueue4:in(19, 19, Q80),
    Q82 = pqueue4:in(20, 20, Q81),
    true = pqueue4:is_queue(Q82),
    82 = pqueue4:len(Q82),
    [-20, -20, -19, -19, -18, -18, -17, -17, -16, -16, -15, -15, -14, -14,
     -13, -13, -12, -12, -11, -11, -10, -10, -9, -9, -8, -8, -7, -7, -6, -6,
     -5, -5, -4, -4, -3, -3, -2, -2, -1, -1, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4,
     5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14,
     15, 15, 16, 16, 17, 17, 18, 18, 19, 19, 20, 20] = pqueue4:to_list(Q82),
    {{value, -20}, Q83} = pqueue4:out(Q82),
    {{value, -20}, Q84} = pqueue4:out(Q83),
    {{value, -19}, Q85} = pqueue4:out(Q84),
    {{value, -19}, Q86} = pqueue4:out(Q85),
    {{value, -18}, Q87} = pqueue4:out(Q86),
    {{value, -18}, Q88} = pqueue4:out(Q87),
    {{value, 0}, Q89} = pqueue4:out(0, Q88),
    {{value, 0}, Q90} = pqueue4:out(0, Q89),
    {empty, _} = pqueue4:out(0, Q90),
    {{value, -17, -17}, Q91} = pqueue4:pout(Q90),
    {{value, -17, -17}, Q92} = pqueue4:pout(Q91),
    {{value, -16, -16}, Q93} = pqueue4:pout(Q92),
    {{value, -16, -16}, Q94} = pqueue4:pout(Q93),
    {{value, -15, -15}, Q95} = pqueue4:pout(Q94),
    {{value, -15, -15}, Q96} = pqueue4:pout(Q95),
    {{value, -14, -14}, Q97} = pqueue4:pout(Q96),
    {{value, -14, -14}, Q98} = pqueue4:pout(Q97),
    {{value, -13, -13}, Q99} = pqueue4:pout(Q98),
    {{value, -13, -13}, Q100} = pqueue4:pout(Q99),
    {{value, -12, -12}, Q101} = pqueue4:pout(Q100),
    {{value, -12, -12}, Q102} = pqueue4:pout(Q101),
    {{value, -11, -11}, Q103} = pqueue4:pout(Q102),
    {{value, -11, -11}, Q104} = pqueue4:pout(Q103),
    {{value, -10, -10}, Q105} = pqueue4:pout(Q104),
    {{value, -10, -10}, Q106} = pqueue4:pout(Q105),
    {{value, -9, -9}, Q107} = pqueue4:pout(Q106),
    {{value, -9, -9}, Q108} = pqueue4:pout(Q107),
    {{value, -8, -8}, Q109} = pqueue4:pout(Q108),
    {{value, -8, -8}, Q110} = pqueue4:pout(Q109),
    {{value, -7, -7}, Q111} = pqueue4:pout(Q110),
    {{value, -7, -7}, Q112} = pqueue4:pout(Q111),
    {{value, -6, -6}, Q113} = pqueue4:pout(Q112),
    {{value, -6, -6}, Q114} = pqueue4:pout(Q113),
    {{value, -5, -5}, Q115} = pqueue4:pout(Q114),
    {{value, -5, -5}, Q116} = pqueue4:pout(Q115),
    {{value, -4, -4}, Q117} = pqueue4:pout(Q116),
    {{value, -4, -4}, Q118} = pqueue4:pout(Q117),
    {{value, -3, -3}, Q119} = pqueue4:pout(Q118),
    {{value, -3, -3}, Q120} = pqueue4:pout(Q119),
    {{value, -2, -2}, Q121} = pqueue4:pout(Q120),
    {{value, -2, -2}, Q122} = pqueue4:pout(Q121),
    {{value, -1, -1}, Q123} = pqueue4:pout(Q122),
    {{value, -1, -1}, Q124} = pqueue4:pout(Q123),
    {{value, 1, 1}, Q125} = pqueue4:pout(Q124),
    {{value, 1, 1}, Q126} = pqueue4:pout(Q125),
    {{value, 2, 2}, Q127} = pqueue4:pout(Q126),
    {{value, 2, 2}, Q128} = pqueue4:pout(Q127),
    {{value, 3, 3}, Q129} = pqueue4:pout(Q128),
    {{value, 3, 3}, Q130} = pqueue4:pout(Q129),
    {{value, 4, 4}, Q131} = pqueue4:pout(Q130),
    {{value, 4, 4}, Q132} = pqueue4:pout(Q131),
    {{value, 5, 5}, Q133} = pqueue4:pout(Q132),
    {{value, 5, 5}, Q134} = pqueue4:pout(Q133),
    {{value, 6, 6}, Q135} = pqueue4:pout(Q134),
    {{value, 6, 6}, Q136} = pqueue4:pout(Q135),
    {{value, 7, 7}, Q137} = pqueue4:pout(Q136),
    {{value, 7, 7}, Q138} = pqueue4:pout(Q137),
    {{value, 8, 8}, Q139} = pqueue4:pout(Q138),
    {{value, 8, 8}, Q140} = pqueue4:pout(Q139),
    {{value, 9, 9}, Q141} = pqueue4:pout(Q140),
    {{value, 9, 9}, Q142} = pqueue4:pout(Q141),
    {{value, 10, 10}, Q143} = pqueue4:pout(Q142),
    {{value, 10, 10}, Q144} = pqueue4:pout(Q143),
    {{value, 11, 11}, Q145} = pqueue4:pout(Q144),
    {{value, 11, 11}, Q146} = pqueue4:pout(Q145),
    {{value, 12, 12}, Q147} = pqueue4:pout(Q146),
    {{value, 12, 12}, Q148} = pqueue4:pout(Q147),
    {{value, 13, 13}, Q149} = pqueue4:pout(Q148),
    {{value, 13, 13}, Q150} = pqueue4:pout(Q149),
    {{value, 14, 14}, Q151} = pqueue4:pout(Q150),
    {{value, 14, 14}, Q152} = pqueue4:pout(Q151),
    {{value, 15, 15}, Q153} = pqueue4:pout(Q152),
    {{value, 15, 15}, Q154} = pqueue4:pout(Q153),
    {{value, 16, 16}, Q155} = pqueue4:pout(Q154),
    {{value, 16, 16}, Q156} = pqueue4:pout(Q155),
    {{value, 17, 17}, Q157} = pqueue4:pout(Q156),
    {{value, 17, 17}, Q158} = pqueue4:pout(Q157),
    {{value, 18, 18}, Q159} = pqueue4:pout(Q158),
    {{value, 18, 18}, Q160} = pqueue4:pout(Q159),
    {{value, 19, 19}, Q161} = pqueue4:pout(Q160),
    {{value, 19, 19}, Q162} = pqueue4:pout(Q161),
    {{value, 20, 20}, Q163} = pqueue4:pout(Q162),
    {{value, 20, 20}, Q164} = pqueue4:pout(Q163),
    true = pqueue4:is_empty(Q164),
    {empty, Q165} = pqueue4:pout(Q164),
    true = pqueue4:is_empty(Q165),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

-define(FILTER_P_Qn128(P, V),
filter_priority(P, F,
                {Pc,
                 {Qn128, Qn127, Qn126, Qn125, Qn124, Qn123, Qn122, Qn121,
                  Qn120, Qn119, Qn118, Qn117, Qn116, Qn115, Qn114, Qn113},
                 Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
                 Q0,
                 Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}) ->
    {Pc,
     V,
     Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(FILTER_P_Qn112(P, V),
filter_priority(P, F,
                {Pc,
                 Qn128,
                 {Qn112, Qn111, Qn110, Qn109, Qn108, Qn107, Qn106, Qn105,
                  Qn104, Qn103, Qn102, Qn101, Qn100, Qn99, Qn98, Qn97},
                 Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
                 Q0,
                 Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}) ->
    {Pc,
     Qn128,
     V,
     Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(FILTER_P_Qn96(P, V),
filter_priority(P, F,
                {Pc,
                 Qn128, Qn112,
                 {Qn96, Qn95, Qn94, Qn93, Qn92, Qn91, Qn90, Qn89,
                  Qn88, Qn87, Qn86, Qn85, Qn84, Qn83, Qn82, Qn81},
                 Qn80, Qn64, Qn48, Qn32, Qn16,
                 Q0,
                 Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}) ->
    {Pc,
     Qn128, Qn112,
     V,
     Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(FILTER_P_Qn80(P, V),
filter_priority(P, F,
                {Pc,
                 Qn128, Qn112, Qn96,
                 {Qn80, Qn79, Qn78, Qn77, Qn76, Qn75, Qn74, Qn73,
                  Qn72, Qn71, Qn70, Qn69, Qn68, Qn67, Qn66, Qn65},
                 Qn64, Qn48, Qn32, Qn16,
                 Q0,
                 Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}) ->
    {Pc,
     Qn128, Qn112, Qn96,
     V,
     Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(FILTER_P_Qn64(P, V),
filter_priority(P, F,
                {Pc,
                 Qn128, Qn112, Qn96, Qn80,
                 {Qn64, Qn63, Qn62, Qn61, Qn60, Qn59, Qn58, Qn57,
                  Qn56, Qn55, Qn54, Qn53, Qn52, Qn51, Qn50, Qn49},
                 Qn48, Qn32, Qn16,
                 Q0,
                 Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}) ->
    {Pc,
     Qn128, Qn112, Qn96, Qn80,
     V,
     Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(FILTER_P_Qn48(P, V),
filter_priority(P, F,
                {Pc,
                 Qn128, Qn112, Qn96, Qn80, Qn64,
                 {Qn48, Qn47, Qn46, Qn45, Qn44, Qn43, Qn42, Qn41,
                  Qn40, Qn39, Qn38, Qn37, Qn36, Qn35, Qn34, Qn33},
                 Qn32, Qn16,
                 Q0,
                 Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}) ->
    {Pc,
     Qn128, Qn112, Qn96, Qn80, Qn64,
     V,
     Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(FILTER_P_Qn32(P, V),
filter_priority(P, F,
                {Pc,
                 Qn128, Qn112, Qn96, Qn80, Qn64, Qn48,
                 {Qn32, Qn31, Qn30, Qn29, Qn28, Qn27, Qn26, Qn25,
                  Qn24, Qn23, Qn22, Qn21, Qn20, Qn19, Qn18, Qn17},
                 Qn16,
                 Q0,
                 Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}) ->
    {Pc,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48,
     V,
     Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(FILTER_P_Qn16(P, V),
filter_priority(P, F,
                {Pc,
                 Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32,
                 {Qn16, Qn15, Qn14, Qn13, Qn12, Qn11, Qn10, Qn9,
                  Qn8, Qn7, Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
                 Q0,
                 Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}) ->
    {Pc,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32,
     V,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(FILTER_P_Qp16(P, V),
filter_priority(P, F,
                {Pc,
                 Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
                 Q0,
                 {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6, Qp7, Qp8,
                  Qp9, Qp10, Qp11, Qp12, Qp13, Qp14, Qp15, Qp16},
                 Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}) ->
    {Pc,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     V,
     Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(FILTER_P_Qp32(P, V),
filter_priority(P, F,
                {Pc,
                 Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
                 Q0,
                 Qp16,
                 {Qp17, Qp18, Qp19, Qp20, Qp21, Qp22, Qp23, Qp24,
                  Qp25, Qp26, Qp27, Qp28, Qp29, Qp30, Qp31, Qp32},
                 Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}) ->
    {Pc,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16,
     V,
     Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(FILTER_P_Qp48(P, V),
filter_priority(P, F,
                {Pc,
                 Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
                 Q0,
                 Qp16, Qp32,
                 {Qp33, Qp34, Qp35, Qp36, Qp37, Qp38, Qp39, Qp40,
                  Qp41, Qp42, Qp43, Qp44, Qp45, Qp46, Qp47, Qp48},
                 Qp64, Qp80, Qp96, Qp112, Qp128}) ->
    {Pc,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32,
     V,
     Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(FILTER_P_Qp64(P, V),
filter_priority(P, F,
                {Pc,
                 Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
                 Q0,
                 Qp16, Qp32, Qp48,
                 {Qp49, Qp50, Qp51, Qp52, Qp53, Qp54, Qp55, Qp56,
                  Qp57, Qp58, Qp59, Qp60, Qp61, Qp62, Qp63, Qp64},
                 Qp80, Qp96, Qp112, Qp128}) ->
    {Pc,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48,
     V,
     Qp80, Qp96, Qp112, Qp128}).
-define(FILTER_P_Qp80(P, V),
filter_priority(P, F,
                {Pc,
                 Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
                 Q0,
                 Qp16, Qp32, Qp48, Qp64,
                 {Qp65, Qp66, Qp67, Qp68, Qp69, Qp70, Qp71, Qp72,
                  Qp73, Qp74, Qp75, Qp76, Qp77, Qp78, Qp79, Qp80},
                 Qp96, Qp112, Qp128}) ->
    {Pc,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64,
     V,
     Qp96, Qp112, Qp128}).
-define(FILTER_P_Qp96(P, V),
filter_priority(P, F,
                {Pc,
                 Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
                 Q0,
                 Qp16, Qp32, Qp48, Qp64, Qp80,
                 {Qp81, Qp82, Qp83, Qp84, Qp85, Qp86, Qp87, Qp88,
                  Qp89, Qp90, Qp91, Qp92, Qp93, Qp94, Qp95, Qp96},
                 Qp112, Qp128}) ->
    {Pc,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80,
     V,
     Qp112, Qp128}).
-define(FILTER_P_Qp112(P, V),
filter_priority(P, F,
                {Pc,
                 Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
                 Q0,
                 Qp16, Qp32, Qp48, Qp64, Qp80, Qp96,
                 {Qp97, Qp98, Qp99, Qp100, Qp101, Qp102, Qp103, Qp104,
                  Qp105, Qp106, Qp107, Qp108, Qp109, Qp110, Qp111, Qp112},
                 Qp128}) ->
    {Pc,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96,
     V,
     Qp128}).
-define(FILTER_P_Qp128(P, V),
filter_priority(P, F,
                {Pc,
                 Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
                 Q0,
                 Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112,
                 {Qp113, Qp114, Qp115, Qp116, Qp117, Qp118, Qp119, Qp120,
                  Qp121, Qp122, Qp123, Qp124, Qp125, Qp126, Qp127, Qp128}}) ->
    {Pc,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112,
     V}).

?FILTER_P_Qn128(-128,
                {queue:filter(F, Qn128), Qn127, Qn126, Qn125, Qn124,
                 Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                 Qn117, Qn116, Qn115, Qn114, Qn113});
?FILTER_P_Qn128(-127,
                {Qn128, queue:filter(F, Qn127), Qn126, Qn125, Qn124,
                 Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                 Qn117, Qn116, Qn115, Qn114, Qn113});
?FILTER_P_Qn128(-126,
                {Qn128, Qn127, queue:filter(F, Qn126), Qn125, Qn124,
                 Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                 Qn117, Qn116, Qn115, Qn114, Qn113});
?FILTER_P_Qn128(-125,
                {Qn128, Qn127, Qn126, queue:filter(F, Qn125), Qn124,
                 Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                 Qn117, Qn116, Qn115, Qn114, Qn113});
?FILTER_P_Qn128(-124,
                {Qn128, Qn127, Qn126, Qn125, queue:filter(F, Qn124),
                 Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                 Qn117, Qn116, Qn115, Qn114, Qn113});
?FILTER_P_Qn128(-123,
                {Qn128, Qn127, Qn126, Qn125, Qn124,
                 queue:filter(F, Qn123), Qn122, Qn121, Qn120, Qn119, Qn118,
                 Qn117, Qn116, Qn115, Qn114, Qn113});
?FILTER_P_Qn128(-122,
                {Qn128, Qn127, Qn126, Qn125, Qn124,
                 Qn123, queue:filter(F, Qn122), Qn121, Qn120, Qn119, Qn118,
                 Qn117, Qn116, Qn115, Qn114, Qn113});
?FILTER_P_Qn128(-121,
                {Qn128, Qn127, Qn126, Qn125, Qn124,
                 Qn123, Qn122, queue:filter(F, Qn121), Qn120, Qn119, Qn118,
                 Qn117, Qn116, Qn115, Qn114, Qn113});
?FILTER_P_Qn128(-120,
                {Qn128, Qn127, Qn126, Qn125, Qn124,
                 Qn123, Qn122, Qn121, queue:filter(F, Qn120), Qn119, Qn118,
                 Qn117, Qn116, Qn115, Qn114, Qn113});
?FILTER_P_Qn128(-119,
                {Qn128, Qn127, Qn126, Qn125, Qn124,
                 Qn123, Qn122, Qn121, Qn120, queue:filter(F, Qn119), Qn118,
                 Qn117, Qn116, Qn115, Qn114, Qn113});
?FILTER_P_Qn128(-118,
                {Qn128, Qn127, Qn126, Qn125, Qn124,
                 Qn123, Qn122, Qn121, Qn120, Qn119, queue:filter(F, Qn118),
                 Qn117, Qn116, Qn115, Qn114, Qn113});
?FILTER_P_Qn128(-117,
                {Qn128, Qn127, Qn126, Qn125, Qn124,
                 Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                 queue:filter(F, Qn117), Qn116, Qn115, Qn114, Qn113});
?FILTER_P_Qn128(-116,
                {Qn128, Qn127, Qn126, Qn125, Qn124,
                 Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                 Qn117, queue:filter(F, Qn116), Qn115, Qn114, Qn113});
?FILTER_P_Qn128(-115,
                {Qn128, Qn127, Qn126, Qn125, Qn124,
                 Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                 Qn117, Qn116, queue:filter(F, Qn115), Qn114, Qn113});
?FILTER_P_Qn128(-114,
                {Qn128, Qn127, Qn126, Qn125, Qn124,
                 Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                 Qn117, Qn116, Qn115, queue:filter(F, Qn114), Qn113});
?FILTER_P_Qn128(-113,
                {Qn128, Qn127, Qn126, Qn125, Qn124,
                 Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                 Qn117, Qn116, Qn115, Qn114, queue:filter(F, Qn113)});
?FILTER_P_Qn112(-112,
                {queue:filter(F, Qn112), Qn111, Qn110, Qn109, Qn108,
                 Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                 Qn101, Qn100, Qn99, Qn98, Qn97});
?FILTER_P_Qn112(-111,
                {Qn112, queue:filter(F, Qn111), Qn110, Qn109, Qn108,
                 Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                 Qn101, Qn100, Qn99, Qn98, Qn97});
?FILTER_P_Qn112(-110,
                {Qn112, Qn111, queue:filter(F, Qn110), Qn109, Qn108,
                 Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                 Qn101, Qn100, Qn99, Qn98, Qn97});
?FILTER_P_Qn112(-109,
                {Qn112, Qn111, Qn110, queue:filter(F, Qn109), Qn108,
                 Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                 Qn101, Qn100, Qn99, Qn98, Qn97});
?FILTER_P_Qn112(-108,
                {Qn112, Qn111, Qn110, Qn109, queue:filter(F, Qn108),
                 Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                 Qn101, Qn100, Qn99, Qn98, Qn97});
?FILTER_P_Qn112(-107,
                {Qn112, Qn111, Qn110, Qn109, Qn108,
                 queue:filter(F, Qn107), Qn106, Qn105, Qn104, Qn103, Qn102,
                 Qn101, Qn100, Qn99, Qn98, Qn97});
?FILTER_P_Qn112(-106,
                {Qn112, Qn111, Qn110, Qn109, Qn108,
                 Qn107, queue:filter(F, Qn106), Qn105, Qn104, Qn103, Qn102,
                 Qn101, Qn100, Qn99, Qn98, Qn97});
?FILTER_P_Qn112(-105,
                {Qn112, Qn111, Qn110, Qn109, Qn108,
                 Qn107, Qn106, queue:filter(F, Qn105), Qn104, Qn103, Qn102,
                 Qn101, Qn100, Qn99, Qn98, Qn97});
?FILTER_P_Qn112(-104,
                {Qn112, Qn111, Qn110, Qn109, Qn108,
                 Qn107, Qn106, Qn105, queue:filter(F, Qn104), Qn103, Qn102,
                 Qn101, Qn100, Qn99, Qn98, Qn97});
?FILTER_P_Qn112(-103,
                {Qn112, Qn111, Qn110, Qn109, Qn108,
                 Qn107, Qn106, Qn105, Qn104, queue:filter(F, Qn103), Qn102,
                 Qn101, Qn100, Qn99, Qn98, Qn97});
?FILTER_P_Qn112(-102,
                {Qn112, Qn111, Qn110, Qn109, Qn108,
                 Qn107, Qn106, Qn105, Qn104, Qn103, queue:filter(F, Qn102),
                 Qn101, Qn100, Qn99, Qn98, Qn97});
?FILTER_P_Qn112(-101,
                {Qn112, Qn111, Qn110, Qn109, Qn108,
                 Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                 queue:filter(F, Qn101), Qn100, Qn99, Qn98, Qn97});
?FILTER_P_Qn112(-100,
                {Qn112, Qn111, Qn110, Qn109, Qn108,
                 Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                 Qn101, queue:filter(F, Qn100), Qn99, Qn98, Qn97});
?FILTER_P_Qn112(-99,
                {Qn112, Qn111, Qn110, Qn109, Qn108,
                 Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                 Qn101, Qn100, queue:filter(F, Qn99), Qn98, Qn97});
?FILTER_P_Qn112(-98,
                {Qn112, Qn111, Qn110, Qn109, Qn108,
                 Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                 Qn101, Qn100, Qn99, queue:filter(F, Qn98), Qn97});
?FILTER_P_Qn112(-97,
                {Qn112, Qn111, Qn110, Qn109, Qn108,
                 Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                 Qn101, Qn100, Qn99, Qn98, queue:filter(F, Qn97)});
?FILTER_P_Qn96(-96,
               {queue:filter(F, Qn96), Qn95, Qn94, Qn93, Qn92,
                Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                Qn85, Qn84, Qn83, Qn82, Qn81});
?FILTER_P_Qn96(-95,
               {Qn96, queue:filter(F, Qn95), Qn94, Qn93, Qn92,
                Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                Qn85, Qn84, Qn83, Qn82, Qn81});
?FILTER_P_Qn96(-94,
               {Qn96, Qn95, queue:filter(F, Qn94), Qn93, Qn92,
                Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                Qn85, Qn84, Qn83, Qn82, Qn81});
?FILTER_P_Qn96(-93,
               {Qn96, Qn95, Qn94, queue:filter(F, Qn93), Qn92,
                Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                Qn85, Qn84, Qn83, Qn82, Qn81});
?FILTER_P_Qn96(-92,
               {Qn96, Qn95, Qn94, Qn93, queue:filter(F, Qn92),
                Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                Qn85, Qn84, Qn83, Qn82, Qn81});
?FILTER_P_Qn96(-91,
               {Qn96, Qn95, Qn94, Qn93, Qn92,
                queue:filter(F, Qn91), Qn90, Qn89, Qn88, Qn87, Qn86,
                Qn85, Qn84, Qn83, Qn82, Qn81});
?FILTER_P_Qn96(-90,
               {Qn96, Qn95, Qn94, Qn93, Qn92,
                Qn91, queue:filter(F, Qn90), Qn89, Qn88, Qn87, Qn86,
                Qn85, Qn84, Qn83, Qn82, Qn81});
?FILTER_P_Qn96(-89,
               {Qn96, Qn95, Qn94, Qn93, Qn92,
                Qn91, Qn90, queue:filter(F, Qn89), Qn88, Qn87, Qn86,
                Qn85, Qn84, Qn83, Qn82, Qn81});
?FILTER_P_Qn96(-88,
               {Qn96, Qn95, Qn94, Qn93, Qn92,
                Qn91, Qn90, Qn89, queue:filter(F, Qn88), Qn87, Qn86,
                Qn85, Qn84, Qn83, Qn82, Qn81});
?FILTER_P_Qn96(-87,
               {Qn96, Qn95, Qn94, Qn93, Qn92,
                Qn91, Qn90, Qn89, Qn88, queue:filter(F, Qn87), Qn86,
                Qn85, Qn84, Qn83, Qn82, Qn81});
?FILTER_P_Qn96(-86,
               {Qn96, Qn95, Qn94, Qn93, Qn92,
                Qn91, Qn90, Qn89, Qn88, Qn87, queue:filter(F, Qn86),
                Qn85, Qn84, Qn83, Qn82, Qn81});
?FILTER_P_Qn96(-85,
               {Qn96, Qn95, Qn94, Qn93, Qn92,
                Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                queue:filter(F, Qn85), Qn84, Qn83, Qn82, Qn81});
?FILTER_P_Qn96(-84,
               {Qn96, Qn95, Qn94, Qn93, Qn92,
                Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                Qn85, queue:filter(F, Qn84), Qn83, Qn82, Qn81});
?FILTER_P_Qn96(-83,
               {Qn96, Qn95, Qn94, Qn93, Qn92,
                Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                Qn85, Qn84, queue:filter(F, Qn83), Qn82, Qn81});
?FILTER_P_Qn96(-82,
               {Qn96, Qn95, Qn94, Qn93, Qn92,
                Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                Qn85, Qn84, Qn83, queue:filter(F, Qn82), Qn81});
?FILTER_P_Qn96(-81,
               {Qn96, Qn95, Qn94, Qn93, Qn92,
                Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                Qn85, Qn84, Qn83, Qn82, queue:filter(F, Qn81)});
?FILTER_P_Qn80(-80,
               {queue:filter(F, Qn80), Qn79, Qn78, Qn77, Qn76,
                Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                Qn69, Qn68, Qn67, Qn66, Qn65});
?FILTER_P_Qn80(-79,
               {Qn80, queue:filter(F, Qn79), Qn78, Qn77, Qn76,
                Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                Qn69, Qn68, Qn67, Qn66, Qn65});
?FILTER_P_Qn80(-78,
               {Qn80, Qn79, queue:filter(F, Qn78), Qn77, Qn76,
                Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                Qn69, Qn68, Qn67, Qn66, Qn65});
?FILTER_P_Qn80(-77,
               {Qn80, Qn79, Qn78, queue:filter(F, Qn77), Qn76,
                Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                Qn69, Qn68, Qn67, Qn66, Qn65});
?FILTER_P_Qn80(-76,
               {Qn80, Qn79, Qn78, Qn77, queue:filter(F, Qn76),
                Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                Qn69, Qn68, Qn67, Qn66, Qn65});
?FILTER_P_Qn80(-75,
               {Qn80, Qn79, Qn78, Qn77, Qn76,
                queue:filter(F, Qn75), Qn74, Qn73, Qn72, Qn71, Qn70,
                Qn69, Qn68, Qn67, Qn66, Qn65});
?FILTER_P_Qn80(-74,
               {Qn80, Qn79, Qn78, Qn77, Qn76,
                Qn75, queue:filter(F, Qn74), Qn73, Qn72, Qn71, Qn70,
                Qn69, Qn68, Qn67, Qn66, Qn65});
?FILTER_P_Qn80(-73,
               {Qn80, Qn79, Qn78, Qn77, Qn76,
                Qn75, Qn74, queue:filter(F, Qn73), Qn72, Qn71, Qn70,
                Qn69, Qn68, Qn67, Qn66, Qn65});
?FILTER_P_Qn80(-72,
               {Qn80, Qn79, Qn78, Qn77, Qn76,
                Qn75, Qn74, Qn73, queue:filter(F, Qn72), Qn71, Qn70,
                Qn69, Qn68, Qn67, Qn66, Qn65});
?FILTER_P_Qn80(-71,
               {Qn80, Qn79, Qn78, Qn77, Qn76,
                Qn75, Qn74, Qn73, Qn72, queue:filter(F, Qn71), Qn70,
                Qn69, Qn68, Qn67, Qn66, Qn65});
?FILTER_P_Qn80(-70,
               {Qn80, Qn79, Qn78, Qn77, Qn76,
                Qn75, Qn74, Qn73, Qn72, Qn71, queue:filter(F, Qn70),
                Qn69, Qn68, Qn67, Qn66, Qn65});
?FILTER_P_Qn80(-69,
               {Qn80, Qn79, Qn78, Qn77, Qn76,
                Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                queue:filter(F, Qn69), Qn68, Qn67, Qn66, Qn65});
?FILTER_P_Qn80(-68,
               {Qn80, Qn79, Qn78, Qn77, Qn76,
                Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                Qn69, queue:filter(F, Qn68), Qn67, Qn66, Qn65});
?FILTER_P_Qn80(-67,
               {Qn80, Qn79, Qn78, Qn77, Qn76,
                Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                Qn69, Qn68, queue:filter(F, Qn67), Qn66, Qn65});
?FILTER_P_Qn80(-66,
               {Qn80, Qn79, Qn78, Qn77, Qn76,
                Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                Qn69, Qn68, Qn67, queue:filter(F, Qn66), Qn65});
?FILTER_P_Qn80(-65,
               {Qn80, Qn79, Qn78, Qn77, Qn76,
                Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                Qn69, Qn68, Qn67, Qn66, queue:filter(F, Qn65)});
?FILTER_P_Qn64(-64,
               {queue:filter(F, Qn64), Qn63, Qn62, Qn61, Qn60,
                Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                Qn53, Qn52, Qn51, Qn50, Qn49});
?FILTER_P_Qn64(-63,
               {Qn64, queue:filter(F, Qn63), Qn62, Qn61, Qn60,
                Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                Qn53, Qn52, Qn51, Qn50, Qn49});
?FILTER_P_Qn64(-62,
               {Qn64, Qn63, queue:filter(F, Qn62), Qn61, Qn60,
                Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                Qn53, Qn52, Qn51, Qn50, Qn49});
?FILTER_P_Qn64(-61,
               {Qn64, Qn63, Qn62, queue:filter(F, Qn61), Qn60,
                Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                Qn53, Qn52, Qn51, Qn50, Qn49});
?FILTER_P_Qn64(-60,
               {Qn64, Qn63, Qn62, Qn61, queue:filter(F, Qn60),
                Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                Qn53, Qn52, Qn51, Qn50, Qn49});
?FILTER_P_Qn64(-59,
               {Qn64, Qn63, Qn62, Qn61, Qn60,
                queue:filter(F, Qn59), Qn58, Qn57, Qn56, Qn55, Qn54,
                Qn53, Qn52, Qn51, Qn50, Qn49});
?FILTER_P_Qn64(-58,
               {Qn64, Qn63, Qn62, Qn61, Qn60,
                Qn59, queue:filter(F, Qn58), Qn57, Qn56, Qn55, Qn54,
                Qn53, Qn52, Qn51, Qn50, Qn49});
?FILTER_P_Qn64(-57,
               {Qn64, Qn63, Qn62, Qn61, Qn60,
                Qn59, Qn58, queue:filter(F, Qn57), Qn56, Qn55, Qn54,
                Qn53, Qn52, Qn51, Qn50, Qn49});
?FILTER_P_Qn64(-56,
               {Qn64, Qn63, Qn62, Qn61, Qn60,
                Qn59, Qn58, Qn57, queue:filter(F, Qn56), Qn55, Qn54,
                Qn53, Qn52, Qn51, Qn50, Qn49});
?FILTER_P_Qn64(-55,
               {Qn64, Qn63, Qn62, Qn61, Qn60,
                Qn59, Qn58, Qn57, Qn56, queue:filter(F, Qn55), Qn54,
                Qn53, Qn52, Qn51, Qn50, Qn49});
?FILTER_P_Qn64(-54,
               {Qn64, Qn63, Qn62, Qn61, Qn60,
                Qn59, Qn58, Qn57, Qn56, Qn55, queue:filter(F, Qn54),
                Qn53, Qn52, Qn51, Qn50, Qn49});
?FILTER_P_Qn64(-53,
               {Qn64, Qn63, Qn62, Qn61, Qn60,
                Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                queue:filter(F, Qn53), Qn52, Qn51, Qn50, Qn49});
?FILTER_P_Qn64(-52,
               {Qn64, Qn63, Qn62, Qn61, Qn60,
                Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                Qn53, queue:filter(F, Qn52), Qn51, Qn50, Qn49});
?FILTER_P_Qn64(-51,
               {Qn64, Qn63, Qn62, Qn61, Qn60,
                Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                Qn53, Qn52, queue:filter(F, Qn51), Qn50, Qn49});
?FILTER_P_Qn64(-50,
               {Qn64, Qn63, Qn62, Qn61, Qn60,
                Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                Qn53, Qn52, Qn51, queue:filter(F, Qn50), Qn49});
?FILTER_P_Qn64(-49,
               {Qn64, Qn63, Qn62, Qn61, Qn60,
                Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                Qn53, Qn52, Qn51, Qn50, queue:filter(F, Qn49)});
?FILTER_P_Qn48(-48,
               {queue:filter(F, Qn48), Qn47, Qn46, Qn45, Qn44,
                Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                Qn37, Qn36, Qn35, Qn34, Qn33});
?FILTER_P_Qn48(-47,
               {Qn48, queue:filter(F, Qn47), Qn46, Qn45, Qn44,
                Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                Qn37, Qn36, Qn35, Qn34, Qn33});
?FILTER_P_Qn48(-46,
               {Qn48, Qn47, queue:filter(F, Qn46), Qn45, Qn44,
                Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                Qn37, Qn36, Qn35, Qn34, Qn33});
?FILTER_P_Qn48(-45,
               {Qn48, Qn47, Qn46, queue:filter(F, Qn45), Qn44,
                Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                Qn37, Qn36, Qn35, Qn34, Qn33});
?FILTER_P_Qn48(-44,
               {Qn48, Qn47, Qn46, Qn45, queue:filter(F, Qn44),
                Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                Qn37, Qn36, Qn35, Qn34, Qn33});
?FILTER_P_Qn48(-43,
               {Qn48, Qn47, Qn46, Qn45, Qn44,
                queue:filter(F, Qn43), Qn42, Qn41, Qn40, Qn39, Qn38,
                Qn37, Qn36, Qn35, Qn34, Qn33});
?FILTER_P_Qn48(-42,
               {Qn48, Qn47, Qn46, Qn45, Qn44,
                Qn43, queue:filter(F, Qn42), Qn41, Qn40, Qn39, Qn38,
                Qn37, Qn36, Qn35, Qn34, Qn33});
?FILTER_P_Qn48(-41,
               {Qn48, Qn47, Qn46, Qn45, Qn44,
                Qn43, Qn42, queue:filter(F, Qn41), Qn40, Qn39, Qn38,
                Qn37, Qn36, Qn35, Qn34, Qn33});
?FILTER_P_Qn48(-40,
               {Qn48, Qn47, Qn46, Qn45, Qn44,
                Qn43, Qn42, Qn41, queue:filter(F, Qn40), Qn39, Qn38,
                Qn37, Qn36, Qn35, Qn34, Qn33});
?FILTER_P_Qn48(-39,
               {Qn48, Qn47, Qn46, Qn45, Qn44,
                Qn43, Qn42, Qn41, Qn40, queue:filter(F, Qn39), Qn38,
                Qn37, Qn36, Qn35, Qn34, Qn33});
?FILTER_P_Qn48(-38,
               {Qn48, Qn47, Qn46, Qn45, Qn44,
                Qn43, Qn42, Qn41, Qn40, Qn39, queue:filter(F, Qn38),
                Qn37, Qn36, Qn35, Qn34, Qn33});
?FILTER_P_Qn48(-37,
               {Qn48, Qn47, Qn46, Qn45, Qn44,
                Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                queue:filter(F, Qn37), Qn36, Qn35, Qn34, Qn33});
?FILTER_P_Qn48(-36,
               {Qn48, Qn47, Qn46, Qn45, Qn44,
                Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                Qn37, queue:filter(F, Qn36), Qn35, Qn34, Qn33});
?FILTER_P_Qn48(-35,
               {Qn48, Qn47, Qn46, Qn45, Qn44,
                Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                Qn37, Qn36, queue:filter(F, Qn35), Qn34, Qn33});
?FILTER_P_Qn48(-34,
               {Qn48, Qn47, Qn46, Qn45, Qn44,
                Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                Qn37, Qn36, Qn35, queue:filter(F, Qn34), Qn33});
?FILTER_P_Qn48(-33,
               {Qn48, Qn47, Qn46, Qn45, Qn44,
                Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                Qn37, Qn36, Qn35, Qn34, queue:filter(F, Qn33)});
?FILTER_P_Qn32(-32,
               {queue:filter(F, Qn32), Qn31, Qn30, Qn29, Qn28,
                Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                Qn21, Qn20, Qn19, Qn18, Qn17});
?FILTER_P_Qn32(-31,
               {Qn32, queue:filter(F, Qn31), Qn30, Qn29, Qn28,
                Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                Qn21, Qn20, Qn19, Qn18, Qn17});
?FILTER_P_Qn32(-30,
               {Qn32, Qn31, queue:filter(F, Qn30), Qn29, Qn28,
                Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                Qn21, Qn20, Qn19, Qn18, Qn17});
?FILTER_P_Qn32(-29,
               {Qn32, Qn31, Qn30, queue:filter(F, Qn29), Qn28,
                Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                Qn21, Qn20, Qn19, Qn18, Qn17});
?FILTER_P_Qn32(-28,
               {Qn32, Qn31, Qn30, Qn29, queue:filter(F, Qn28),
                Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                Qn21, Qn20, Qn19, Qn18, Qn17});
?FILTER_P_Qn32(-27,
               {Qn32, Qn31, Qn30, Qn29, Qn28,
                queue:filter(F, Qn27), Qn26, Qn25, Qn24, Qn23, Qn22,
                Qn21, Qn20, Qn19, Qn18, Qn17});
?FILTER_P_Qn32(-26,
               {Qn32, Qn31, Qn30, Qn29, Qn28,
                Qn27, queue:filter(F, Qn26), Qn25, Qn24, Qn23, Qn22,
                Qn21, Qn20, Qn19, Qn18, Qn17});
?FILTER_P_Qn32(-25,
               {Qn32, Qn31, Qn30, Qn29, Qn28,
                Qn27, Qn26, queue:filter(F, Qn25), Qn24, Qn23, Qn22,
                Qn21, Qn20, Qn19, Qn18, Qn17});
?FILTER_P_Qn32(-24,
               {Qn32, Qn31, Qn30, Qn29, Qn28,
                Qn27, Qn26, Qn25, queue:filter(F, Qn24), Qn23, Qn22,
                Qn21, Qn20, Qn19, Qn18, Qn17});
?FILTER_P_Qn32(-23,
               {Qn32, Qn31, Qn30, Qn29, Qn28,
                Qn27, Qn26, Qn25, Qn24, queue:filter(F, Qn23), Qn22,
                Qn21, Qn20, Qn19, Qn18, Qn17});
?FILTER_P_Qn32(-22,
               {Qn32, Qn31, Qn30, Qn29, Qn28,
                Qn27, Qn26, Qn25, Qn24, Qn23, queue:filter(F, Qn22),
                Qn21, Qn20, Qn19, Qn18, Qn17});
?FILTER_P_Qn32(-21,
               {Qn32, Qn31, Qn30, Qn29, Qn28,
                Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                queue:filter(F, Qn21), Qn20, Qn19, Qn18, Qn17});
?FILTER_P_Qn32(-20,
               {Qn32, Qn31, Qn30, Qn29, Qn28,
                Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                Qn21, queue:filter(F, Qn20), Qn19, Qn18, Qn17});
?FILTER_P_Qn32(-19,
               {Qn32, Qn31, Qn30, Qn29, Qn28,
                Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                Qn21, Qn20, queue:filter(F, Qn19), Qn18, Qn17});
?FILTER_P_Qn32(-18,
               {Qn32, Qn31, Qn30, Qn29, Qn28,
                Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                Qn21, Qn20, Qn19, queue:filter(F, Qn18), Qn17});
?FILTER_P_Qn32(-17,
               {Qn32, Qn31, Qn30, Qn29, Qn28,
                Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                Qn21, Qn20, Qn19, Qn18, queue:filter(F, Qn17)});
?FILTER_P_Qn16(-16,
               {queue:filter(F, Qn16), Qn15, Qn14, Qn13, Qn12,
                Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                Qn5, Qn4, Qn3, Qn2, Qn1});
?FILTER_P_Qn16(-15,
               {Qn16, queue:filter(F, Qn15), Qn14, Qn13, Qn12,
                Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                Qn5, Qn4, Qn3, Qn2, Qn1});
?FILTER_P_Qn16(-14,
               {Qn16, Qn15, queue:filter(F, Qn14), Qn13, Qn12,
                Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                Qn5, Qn4, Qn3, Qn2, Qn1});
?FILTER_P_Qn16(-13,
               {Qn16, Qn15, Qn14, queue:filter(F, Qn13), Qn12,
                Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                Qn5, Qn4, Qn3, Qn2, Qn1});
?FILTER_P_Qn16(-12,
               {Qn16, Qn15, Qn14, Qn13, queue:filter(F, Qn12),
                Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                Qn5, Qn4, Qn3, Qn2, Qn1});
?FILTER_P_Qn16(-11,
               {Qn16, Qn15, Qn14, Qn13, Qn12,
                queue:filter(F, Qn11), Qn10, Qn9, Qn8, Qn7, Qn6,
                Qn5, Qn4, Qn3, Qn2, Qn1});
?FILTER_P_Qn16(-10,
               {Qn16, Qn15, Qn14, Qn13, Qn12,
                Qn11, queue:filter(F, Qn10), Qn9, Qn8, Qn7, Qn6,
                Qn5, Qn4, Qn3, Qn2, Qn1});
?FILTER_P_Qn16(-9,
               {Qn16, Qn15, Qn14, Qn13, Qn12,
                Qn11, Qn10, queue:filter(F, Qn9), Qn8, Qn7, Qn6,
                Qn5, Qn4, Qn3, Qn2, Qn1});
?FILTER_P_Qn16(-8,
               {Qn16, Qn15, Qn14, Qn13, Qn12,
                Qn11, Qn10, Qn9, queue:filter(F, Qn8), Qn7, Qn6,
                Qn5, Qn4, Qn3, Qn2, Qn1});
?FILTER_P_Qn16(-7,
               {Qn16, Qn15, Qn14, Qn13, Qn12,
                Qn11, Qn10, Qn9, Qn8, queue:filter(F, Qn7), Qn6,
                Qn5, Qn4, Qn3, Qn2, Qn1});
?FILTER_P_Qn16(-6,
               {Qn16, Qn15, Qn14, Qn13, Qn12,
                Qn11, Qn10, Qn9, Qn8, Qn7, queue:filter(F, Qn6),
                Qn5, Qn4, Qn3, Qn2, Qn1});
?FILTER_P_Qn16(-5,
               {Qn16, Qn15, Qn14, Qn13, Qn12,
                Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                queue:filter(F, Qn5), Qn4, Qn3, Qn2, Qn1});
?FILTER_P_Qn16(-4,
               {Qn16, Qn15, Qn14, Qn13, Qn12,
                Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                Qn5, queue:filter(F, Qn4), Qn3, Qn2, Qn1});
?FILTER_P_Qn16(-3,
               {Qn16, Qn15, Qn14, Qn13, Qn12,
                Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                Qn5, Qn4, queue:filter(F, Qn3), Qn2, Qn1});
?FILTER_P_Qn16(-2,
               {Qn16, Qn15, Qn14, Qn13, Qn12,
                Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                Qn5, Qn4, Qn3, queue:filter(F, Qn2), Qn1});
?FILTER_P_Qn16(-1,
               {Qn16, Qn15, Qn14, Qn13, Qn12,
                Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                Qn5, Qn4, Qn3, Qn2, queue:filter(F, Qn1)});
filter_priority(0, F,
                {Pc,
                 Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
                 Q0,
                 Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}) ->
    {Pc,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     queue:filter(F, Q0),
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128};
?FILTER_P_Qp16(1,
               {queue:filter(F, Qp1), Qp2, Qp3, Qp4, Qp5,
                Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                Qp12, Qp13, Qp14, Qp15, Qp16});
?FILTER_P_Qp16(2,
               {Qp1, queue:filter(F, Qp2), Qp3, Qp4, Qp5,
                Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                Qp12, Qp13, Qp14, Qp15, Qp16});
?FILTER_P_Qp16(3,
               {Qp1, Qp2, queue:filter(F, Qp3), Qp4, Qp5,
                Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                Qp12, Qp13, Qp14, Qp15, Qp16});
?FILTER_P_Qp16(4,
               {Qp1, Qp2, Qp3, queue:filter(F, Qp4), Qp5,
                Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                Qp12, Qp13, Qp14, Qp15, Qp16});
?FILTER_P_Qp16(5,
               {Qp1, Qp2, Qp3, Qp4, queue:filter(F, Qp5),
                Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                Qp12, Qp13, Qp14, Qp15, Qp16});
?FILTER_P_Qp16(6,
               {Qp1, Qp2, Qp3, Qp4, Qp5,
                queue:filter(F, Qp6), Qp7, Qp8, Qp9, Qp10, Qp11,
                Qp12, Qp13, Qp14, Qp15, Qp16});
?FILTER_P_Qp16(7,
               {Qp1, Qp2, Qp3, Qp4, Qp5,
                Qp6, queue:filter(F, Qp7), Qp8, Qp9, Qp10, Qp11,
                Qp12, Qp13, Qp14, Qp15, Qp16});
?FILTER_P_Qp16(8,
               {Qp1, Qp2, Qp3, Qp4, Qp5,
                Qp6, Qp7, queue:filter(F, Qp8), Qp9, Qp10, Qp11,
                Qp12, Qp13, Qp14, Qp15, Qp16});
?FILTER_P_Qp16(9,
               {Qp1, Qp2, Qp3, Qp4, Qp5,
                Qp6, Qp7, Qp8, queue:filter(F, Qp9), Qp10, Qp11,
                Qp12, Qp13, Qp14, Qp15, Qp16});
?FILTER_P_Qp16(10,
               {Qp1, Qp2, Qp3, Qp4, Qp5,
                Qp6, Qp7, Qp8, Qp9, queue:filter(F, Qp10), Qp11,
                Qp12, Qp13, Qp14, Qp15, Qp16});
?FILTER_P_Qp16(11,
               {Qp1, Qp2, Qp3, Qp4, Qp5,
                Qp6, Qp7, Qp8, Qp9, Qp10, queue:filter(F, Qp11),
                Qp12, Qp13, Qp14, Qp15, Qp16});
?FILTER_P_Qp16(12,
               {Qp1, Qp2, Qp3, Qp4, Qp5,
                Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                queue:filter(F, Qp12), Qp13, Qp14, Qp15, Qp16});
?FILTER_P_Qp16(13,
               {Qp1, Qp2, Qp3, Qp4, Qp5,
                Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                Qp12, queue:filter(F, Qp13), Qp14, Qp15, Qp16});
?FILTER_P_Qp16(14,
               {Qp1, Qp2, Qp3, Qp4, Qp5,
                Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                Qp12, Qp13, queue:filter(F, Qp14), Qp15, Qp16});
?FILTER_P_Qp16(15,
               {Qp1, Qp2, Qp3, Qp4, Qp5,
                Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                Qp12, Qp13, Qp14, queue:filter(F, Qp15), Qp16});
?FILTER_P_Qp16(16,
               {Qp1, Qp2, Qp3, Qp4, Qp5,
                Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                Qp12, Qp13, Qp14, Qp15, queue:filter(F, Qp16)});
?FILTER_P_Qp32(17,
               {queue:filter(F, Qp17), Qp18, Qp19, Qp20, Qp21,
                Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                Qp28, Qp29, Qp30, Qp31, Qp32});
?FILTER_P_Qp32(18,
               {Qp17, queue:filter(F, Qp18), Qp19, Qp20, Qp21,
                Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                Qp28, Qp29, Qp30, Qp31, Qp32});
?FILTER_P_Qp32(19,
               {Qp17, Qp18, queue:filter(F, Qp19), Qp20, Qp21,
                Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                Qp28, Qp29, Qp30, Qp31, Qp32});
?FILTER_P_Qp32(20,
               {Qp17, Qp18, Qp19, queue:filter(F, Qp20), Qp21,
                Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                Qp28, Qp29, Qp30, Qp31, Qp32});
?FILTER_P_Qp32(21,
               {Qp17, Qp18, Qp19, Qp20, queue:filter(F, Qp21),
                Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                Qp28, Qp29, Qp30, Qp31, Qp32});
?FILTER_P_Qp32(22,
               {Qp17, Qp18, Qp19, Qp20, Qp21,
                queue:filter(F, Qp22), Qp23, Qp24, Qp25, Qp26, Qp27,
                Qp28, Qp29, Qp30, Qp31, Qp32});
?FILTER_P_Qp32(23,
               {Qp17, Qp18, Qp19, Qp20, Qp21,
                Qp22, queue:filter(F, Qp23), Qp24, Qp25, Qp26, Qp27,
                Qp28, Qp29, Qp30, Qp31, Qp32});
?FILTER_P_Qp32(24,
               {Qp17, Qp18, Qp19, Qp20, Qp21,
                Qp22, Qp23, queue:filter(F, Qp24), Qp25, Qp26, Qp27,
                Qp28, Qp29, Qp30, Qp31, Qp32});
?FILTER_P_Qp32(25,
               {Qp17, Qp18, Qp19, Qp20, Qp21,
                Qp22, Qp23, Qp24, queue:filter(F, Qp25), Qp26, Qp27,
                Qp28, Qp29, Qp30, Qp31, Qp32});
?FILTER_P_Qp32(26,
               {Qp17, Qp18, Qp19, Qp20, Qp21,
                Qp22, Qp23, Qp24, Qp25, queue:filter(F, Qp26), Qp27,
                Qp28, Qp29, Qp30, Qp31, Qp32});
?FILTER_P_Qp32(27,
               {Qp17, Qp18, Qp19, Qp20, Qp21,
                Qp22, Qp23, Qp24, Qp25, Qp26, queue:filter(F, Qp27),
                Qp28, Qp29, Qp30, Qp31, Qp32});
?FILTER_P_Qp32(28,
               {Qp17, Qp18, Qp19, Qp20, Qp21,
                Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                queue:filter(F, Qp28), Qp29, Qp30, Qp31, Qp32});
?FILTER_P_Qp32(29,
               {Qp17, Qp18, Qp19, Qp20, Qp21,
                Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                Qp28, queue:filter(F, Qp29), Qp30, Qp31, Qp32});
?FILTER_P_Qp32(30,
               {Qp17, Qp18, Qp19, Qp20, Qp21,
                Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                Qp28, Qp29, queue:filter(F, Qp30), Qp31, Qp32});
?FILTER_P_Qp32(31,
               {Qp17, Qp18, Qp19, Qp20, Qp21,
                Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                Qp28, Qp29, Qp30, queue:filter(F, Qp31), Qp32});
?FILTER_P_Qp32(32,
               {Qp17, Qp18, Qp19, Qp20, Qp21,
                Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                Qp28, Qp29, Qp30, Qp31, queue:filter(F, Qp32)});
?FILTER_P_Qp48(33,
               {queue:filter(F, Qp33), Qp34, Qp35, Qp36, Qp37,
                Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                Qp44, Qp45, Qp46, Qp47, Qp48});
?FILTER_P_Qp48(34,
               {Qp33, queue:filter(F, Qp34), Qp35, Qp36, Qp37,
                Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                Qp44, Qp45, Qp46, Qp47, Qp48});
?FILTER_P_Qp48(35,
               {Qp33, Qp34, queue:filter(F, Qp35), Qp36, Qp37,
                Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                Qp44, Qp45, Qp46, Qp47, Qp48});
?FILTER_P_Qp48(36,
               {Qp33, Qp34, Qp35, queue:filter(F, Qp36), Qp37,
                Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                Qp44, Qp45, Qp46, Qp47, Qp48});
?FILTER_P_Qp48(37,
               {Qp33, Qp34, Qp35, Qp36, queue:filter(F, Qp37),
                Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                Qp44, Qp45, Qp46, Qp47, Qp48});
?FILTER_P_Qp48(38,
               {Qp33, Qp34, Qp35, Qp36, Qp37,
                queue:filter(F, Qp38), Qp39, Qp40, Qp41, Qp42, Qp43,
                Qp44, Qp45, Qp46, Qp47, Qp48});
?FILTER_P_Qp48(39,
               {Qp33, Qp34, Qp35, Qp36, Qp37,
                Qp38, queue:filter(F, Qp39), Qp40, Qp41, Qp42, Qp43,
                Qp44, Qp45, Qp46, Qp47, Qp48});
?FILTER_P_Qp48(40,
               {Qp33, Qp34, Qp35, Qp36, Qp37,
                Qp38, Qp39, queue:filter(F, Qp40), Qp41, Qp42, Qp43,
                Qp44, Qp45, Qp46, Qp47, Qp48});
?FILTER_P_Qp48(41,
               {Qp33, Qp34, Qp35, Qp36, Qp37,
                Qp38, Qp39, Qp40, queue:filter(F, Qp41), Qp42, Qp43,
                Qp44, Qp45, Qp46, Qp47, Qp48});
?FILTER_P_Qp48(42,
               {Qp33, Qp34, Qp35, Qp36, Qp37,
                Qp38, Qp39, Qp40, Qp41, queue:filter(F, Qp42), Qp43,
                Qp44, Qp45, Qp46, Qp47, Qp48});
?FILTER_P_Qp48(43,
               {Qp33, Qp34, Qp35, Qp36, Qp37,
                Qp38, Qp39, Qp40, Qp41, Qp42, queue:filter(F, Qp43),
                Qp44, Qp45, Qp46, Qp47, Qp48});
?FILTER_P_Qp48(44,
               {Qp33, Qp34, Qp35, Qp36, Qp37,
                Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                queue:filter(F, Qp44), Qp45, Qp46, Qp47, Qp48});
?FILTER_P_Qp48(45,
               {Qp33, Qp34, Qp35, Qp36, Qp37,
                Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                Qp44, queue:filter(F, Qp45), Qp46, Qp47, Qp48});
?FILTER_P_Qp48(46,
               {Qp33, Qp34, Qp35, Qp36, Qp37,
                Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                Qp44, Qp45, queue:filter(F, Qp46), Qp47, Qp48});
?FILTER_P_Qp48(47,
               {Qp33, Qp34, Qp35, Qp36, Qp37,
                Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                Qp44, Qp45, Qp46, queue:filter(F, Qp47), Qp48});
?FILTER_P_Qp48(48,
               {Qp33, Qp34, Qp35, Qp36, Qp37,
                Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                Qp44, Qp45, Qp46, Qp47, queue:filter(F, Qp48)});
?FILTER_P_Qp64(49,
               {queue:filter(F, Qp49), Qp50, Qp51, Qp52, Qp53,
                Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                Qp60, Qp61, Qp62, Qp63, Qp64});
?FILTER_P_Qp64(50,
               {Qp49, queue:filter(F, Qp50), Qp51, Qp52, Qp53,
                Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                Qp60, Qp61, Qp62, Qp63, Qp64});
?FILTER_P_Qp64(51,
               {Qp49, Qp50, queue:filter(F, Qp51), Qp52, Qp53,
                Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                Qp60, Qp61, Qp62, Qp63, Qp64});
?FILTER_P_Qp64(52,
               {Qp49, Qp50, Qp51, queue:filter(F, Qp52), Qp53,
                Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                Qp60, Qp61, Qp62, Qp63, Qp64});
?FILTER_P_Qp64(53,
               {Qp49, Qp50, Qp51, Qp52, queue:filter(F, Qp53),
                Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                Qp60, Qp61, Qp62, Qp63, Qp64});
?FILTER_P_Qp64(54,
               {Qp49, Qp50, Qp51, Qp52, Qp53,
                queue:filter(F, Qp54), Qp55, Qp56, Qp57, Qp58, Qp59,
                Qp60, Qp61, Qp62, Qp63, Qp64});
?FILTER_P_Qp64(55,
               {Qp49, Qp50, Qp51, Qp52, Qp53,
                Qp54, queue:filter(F, Qp55), Qp56, Qp57, Qp58, Qp59,
                Qp60, Qp61, Qp62, Qp63, Qp64});
?FILTER_P_Qp64(56,
               {Qp49, Qp50, Qp51, Qp52, Qp53,
                Qp54, Qp55, queue:filter(F, Qp56), Qp57, Qp58, Qp59,
                Qp60, Qp61, Qp62, Qp63, Qp64});
?FILTER_P_Qp64(57,
               {Qp49, Qp50, Qp51, Qp52, Qp53,
                Qp54, Qp55, Qp56, queue:filter(F, Qp57), Qp58, Qp59,
                Qp60, Qp61, Qp62, Qp63, Qp64});
?FILTER_P_Qp64(58,
               {Qp49, Qp50, Qp51, Qp52, Qp53,
                Qp54, Qp55, Qp56, Qp57, queue:filter(F, Qp58), Qp59,
                Qp60, Qp61, Qp62, Qp63, Qp64});
?FILTER_P_Qp64(59,
               {Qp49, Qp50, Qp51, Qp52, Qp53,
                Qp54, Qp55, Qp56, Qp57, Qp58, queue:filter(F, Qp59),
                Qp60, Qp61, Qp62, Qp63, Qp64});
?FILTER_P_Qp64(60,
               {Qp49, Qp50, Qp51, Qp52, Qp53,
                Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                queue:filter(F, Qp60), Qp61, Qp62, Qp63, Qp64});
?FILTER_P_Qp64(61,
               {Qp49, Qp50, Qp51, Qp52, Qp53,
                Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                Qp60, queue:filter(F, Qp61), Qp62, Qp63, Qp64});
?FILTER_P_Qp64(62,
               {Qp49, Qp50, Qp51, Qp52, Qp53,
                Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                Qp60, Qp61, queue:filter(F, Qp62), Qp63, Qp64});
?FILTER_P_Qp64(63,
               {Qp49, Qp50, Qp51, Qp52, Qp53,
                Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                Qp60, Qp61, Qp62, queue:filter(F, Qp63), Qp64});
?FILTER_P_Qp64(64,
               {Qp49, Qp50, Qp51, Qp52, Qp53,
                Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                Qp60, Qp61, Qp62, Qp63, queue:filter(F, Qp64)});
?FILTER_P_Qp80(65,
               {queue:filter(F, Qp65), Qp66, Qp67, Qp68, Qp69,
                Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                Qp76, Qp77, Qp78, Qp79, Qp80});
?FILTER_P_Qp80(66,
               {Qp65, queue:filter(F, Qp66), Qp67, Qp68, Qp69,
                Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                Qp76, Qp77, Qp78, Qp79, Qp80});
?FILTER_P_Qp80(67,
               {Qp65, Qp66, queue:filter(F, Qp67), Qp68, Qp69,
                Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                Qp76, Qp77, Qp78, Qp79, Qp80});
?FILTER_P_Qp80(68,
               {Qp65, Qp66, Qp67, queue:filter(F, Qp68), Qp69,
                Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                Qp76, Qp77, Qp78, Qp79, Qp80});
?FILTER_P_Qp80(69,
               {Qp65, Qp66, Qp67, Qp68, queue:filter(F, Qp69),
                Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                Qp76, Qp77, Qp78, Qp79, Qp80});
?FILTER_P_Qp80(70,
               {Qp65, Qp66, Qp67, Qp68, Qp69,
                queue:filter(F, Qp70), Qp71, Qp72, Qp73, Qp74, Qp75,
                Qp76, Qp77, Qp78, Qp79, Qp80});
?FILTER_P_Qp80(71,
               {Qp65, Qp66, Qp67, Qp68, Qp69,
                Qp70, queue:filter(F, Qp71), Qp72, Qp73, Qp74, Qp75,
                Qp76, Qp77, Qp78, Qp79, Qp80});
?FILTER_P_Qp80(72,
               {Qp65, Qp66, Qp67, Qp68, Qp69,
                Qp70, Qp71, queue:filter(F, Qp72), Qp73, Qp74, Qp75,
                Qp76, Qp77, Qp78, Qp79, Qp80});
?FILTER_P_Qp80(73,
               {Qp65, Qp66, Qp67, Qp68, Qp69,
                Qp70, Qp71, Qp72, queue:filter(F, Qp73), Qp74, Qp75,
                Qp76, Qp77, Qp78, Qp79, Qp80});
?FILTER_P_Qp80(74,
               {Qp65, Qp66, Qp67, Qp68, Qp69,
                Qp70, Qp71, Qp72, Qp73, queue:filter(F, Qp74), Qp75,
                Qp76, Qp77, Qp78, Qp79, Qp80});
?FILTER_P_Qp80(75,
               {Qp65, Qp66, Qp67, Qp68, Qp69,
                Qp70, Qp71, Qp72, Qp73, Qp74, queue:filter(F, Qp75),
                Qp76, Qp77, Qp78, Qp79, Qp80});
?FILTER_P_Qp80(76,
               {Qp65, Qp66, Qp67, Qp68, Qp69,
                Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                queue:filter(F, Qp76), Qp77, Qp78, Qp79, Qp80});
?FILTER_P_Qp80(77,
               {Qp65, Qp66, Qp67, Qp68, Qp69,
                Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                Qp76, queue:filter(F, Qp77), Qp78, Qp79, Qp80});
?FILTER_P_Qp80(78,
               {Qp65, Qp66, Qp67, Qp68, Qp69,
                Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                Qp76, Qp77, queue:filter(F, Qp78), Qp79, Qp80});
?FILTER_P_Qp80(79,
               {Qp65, Qp66, Qp67, Qp68, Qp69,
                Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                Qp76, Qp77, Qp78, queue:filter(F, Qp79), Qp80});
?FILTER_P_Qp80(80,
               {Qp65, Qp66, Qp67, Qp68, Qp69,
                Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                Qp76, Qp77, Qp78, Qp79, queue:filter(F, Qp80)});
?FILTER_P_Qp96(81,
               {queue:filter(F, Qp81), Qp82, Qp83, Qp84, Qp85,
                Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                Qp92, Qp93, Qp94, Qp95, Qp96});
?FILTER_P_Qp96(82,
               {Qp81, queue:filter(F, Qp82), Qp83, Qp84, Qp85,
                Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                Qp92, Qp93, Qp94, Qp95, Qp96});
?FILTER_P_Qp96(83,
               {Qp81, Qp82, queue:filter(F, Qp83), Qp84, Qp85,
                Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                Qp92, Qp93, Qp94, Qp95, Qp96});
?FILTER_P_Qp96(84,
               {Qp81, Qp82, Qp83, queue:filter(F, Qp84), Qp85,
                Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                Qp92, Qp93, Qp94, Qp95, Qp96});
?FILTER_P_Qp96(85,
               {Qp81, Qp82, Qp83, Qp84, queue:filter(F, Qp85),
                Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                Qp92, Qp93, Qp94, Qp95, Qp96});
?FILTER_P_Qp96(86,
               {Qp81, Qp82, Qp83, Qp84, Qp85,
                queue:filter(F, Qp86), Qp87, Qp88, Qp89, Qp90, Qp91,
                Qp92, Qp93, Qp94, Qp95, Qp96});
?FILTER_P_Qp96(87,
               {Qp81, Qp82, Qp83, Qp84, Qp85,
                Qp86, queue:filter(F, Qp87), Qp88, Qp89, Qp90, Qp91,
                Qp92, Qp93, Qp94, Qp95, Qp96});
?FILTER_P_Qp96(88,
               {Qp81, Qp82, Qp83, Qp84, Qp85,
                Qp86, Qp87, queue:filter(F, Qp88), Qp89, Qp90, Qp91,
                Qp92, Qp93, Qp94, Qp95, Qp96});
?FILTER_P_Qp96(89,
               {Qp81, Qp82, Qp83, Qp84, Qp85,
                Qp86, Qp87, Qp88, queue:filter(F, Qp89), Qp90, Qp91,
                Qp92, Qp93, Qp94, Qp95, Qp96});
?FILTER_P_Qp96(90,
               {Qp81, Qp82, Qp83, Qp84, Qp85,
                Qp86, Qp87, Qp88, Qp89, queue:filter(F, Qp90), Qp91,
                Qp92, Qp93, Qp94, Qp95, Qp96});
?FILTER_P_Qp96(91,
               {Qp81, Qp82, Qp83, Qp84, Qp85,
                Qp86, Qp87, Qp88, Qp89, Qp90, queue:filter(F, Qp91),
                Qp92, Qp93, Qp94, Qp95, Qp96});
?FILTER_P_Qp96(92,
               {Qp81, Qp82, Qp83, Qp84, Qp85,
                Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                queue:filter(F, Qp92), Qp93, Qp94, Qp95, Qp96});
?FILTER_P_Qp96(93,
               {Qp81, Qp82, Qp83, Qp84, Qp85,
                Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                Qp92, queue:filter(F, Qp93), Qp94, Qp95, Qp96});
?FILTER_P_Qp96(94,
               {Qp81, Qp82, Qp83, Qp84, Qp85,
                Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                Qp92, Qp93, queue:filter(F, Qp94), Qp95, Qp96});
?FILTER_P_Qp96(95,
               {Qp81, Qp82, Qp83, Qp84, Qp85,
                Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                Qp92, Qp93, Qp94, queue:filter(F, Qp95), Qp96});
?FILTER_P_Qp96(96,
               {Qp81, Qp82, Qp83, Qp84, Qp85,
                Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                Qp92, Qp93, Qp94, Qp95, queue:filter(F, Qp96)});
?FILTER_P_Qp112(97,
                {queue:filter(F, Qp97), Qp98, Qp99, Qp100, Qp101,
                 Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                 Qp108, Qp109, Qp110, Qp111, Qp112});
?FILTER_P_Qp112(98,
                {Qp97, queue:filter(F, Qp98), Qp99, Qp100, Qp101,
                 Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                 Qp108, Qp109, Qp110, Qp111, Qp112});
?FILTER_P_Qp112(99,
                {Qp97, Qp98, queue:filter(F, Qp99), Qp100, Qp101,
                 Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                 Qp108, Qp109, Qp110, Qp111, Qp112});
?FILTER_P_Qp112(100,
                {Qp97, Qp98, Qp99, queue:filter(F, Qp100), Qp101,
                 Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                 Qp108, Qp109, Qp110, Qp111, Qp112});
?FILTER_P_Qp112(101,
                {Qp97, Qp98, Qp99, Qp100, queue:filter(F, Qp101),
                 Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                 Qp108, Qp109, Qp110, Qp111, Qp112});
?FILTER_P_Qp112(102,
                {Qp97, Qp98, Qp99, Qp100, Qp101,
                 queue:filter(F, Qp102), Qp103, Qp104, Qp105, Qp106, Qp107,
                 Qp108, Qp109, Qp110, Qp111, Qp112});
?FILTER_P_Qp112(103,
                {Qp97, Qp98, Qp99, Qp100, Qp101,
                 Qp102, queue:filter(F, Qp103), Qp104, Qp105, Qp106, Qp107,
                 Qp108, Qp109, Qp110, Qp111, Qp112});
?FILTER_P_Qp112(104,
                {Qp97, Qp98, Qp99, Qp100, Qp101,
                 Qp102, Qp103, queue:filter(F, Qp104), Qp105, Qp106, Qp107,
                 Qp108, Qp109, Qp110, Qp111, Qp112});
?FILTER_P_Qp112(105,
                {Qp97, Qp98, Qp99, Qp100, Qp101,
                 Qp102, Qp103, Qp104, queue:filter(F, Qp105), Qp106, Qp107,
                 Qp108, Qp109, Qp110, Qp111, Qp112});
?FILTER_P_Qp112(106,
                {Qp97, Qp98, Qp99, Qp100, Qp101,
                 Qp102, Qp103, Qp104, Qp105, queue:filter(F, Qp106), Qp107,
                 Qp108, Qp109, Qp110, Qp111, Qp112});
?FILTER_P_Qp112(107,
                {Qp97, Qp98, Qp99, Qp100, Qp101,
                 Qp102, Qp103, Qp104, Qp105, Qp106, queue:filter(F, Qp107),
                 Qp108, Qp109, Qp110, Qp111, Qp112});
?FILTER_P_Qp112(108,
                {Qp97, Qp98, Qp99, Qp100, Qp101,
                 Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                 queue:filter(F, Qp108), Qp109, Qp110, Qp111, Qp112});
?FILTER_P_Qp112(109,
                {Qp97, Qp98, Qp99, Qp100, Qp101,
                 Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                 Qp108, queue:filter(F, Qp109), Qp110, Qp111, Qp112});
?FILTER_P_Qp112(110,
                {Qp97, Qp98, Qp99, Qp100, Qp101,
                 Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                 Qp108, Qp109, queue:filter(F, Qp110), Qp111, Qp112});
?FILTER_P_Qp112(111,
                {Qp97, Qp98, Qp99, Qp100, Qp101,
                 Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                 Qp108, Qp109, Qp110, queue:filter(F, Qp111), Qp112});
?FILTER_P_Qp112(112,
                {Qp97, Qp98, Qp99, Qp100, Qp101,
                 Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                 Qp108, Qp109, Qp110, Qp111, queue:filter(F, Qp112)});
?FILTER_P_Qp128(113,
                {queue:filter(F, Qp113), Qp114, Qp115, Qp116, Qp117,
                 Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                 Qp124, Qp125, Qp126, Qp127, Qp128});
?FILTER_P_Qp128(114,
                {Qp113, queue:filter(F, Qp114), Qp115, Qp116, Qp117,
                 Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                 Qp124, Qp125, Qp126, Qp127, Qp128});
?FILTER_P_Qp128(115,
                {Qp113, Qp114, queue:filter(F, Qp115), Qp116, Qp117,
                 Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                 Qp124, Qp125, Qp126, Qp127, Qp128});
?FILTER_P_Qp128(116,
                {Qp113, Qp114, Qp115, queue:filter(F, Qp116), Qp117,
                 Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                 Qp124, Qp125, Qp126, Qp127, Qp128});
?FILTER_P_Qp128(117,
                {Qp113, Qp114, Qp115, Qp116, queue:filter(F, Qp117),
                 Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                 Qp124, Qp125, Qp126, Qp127, Qp128});
?FILTER_P_Qp128(118,
                {Qp113, Qp114, Qp115, Qp116, Qp117,
                 queue:filter(F, Qp118), Qp119, Qp120, Qp121, Qp122, Qp123,
                 Qp124, Qp125, Qp126, Qp127, Qp128});
?FILTER_P_Qp128(119,
                {Qp113, Qp114, Qp115, Qp116, Qp117,
                 Qp118, queue:filter(F, Qp119), Qp120, Qp121, Qp122, Qp123,
                 Qp124, Qp125, Qp126, Qp127, Qp128});
?FILTER_P_Qp128(120,
                {Qp113, Qp114, Qp115, Qp116, Qp117,
                 Qp118, Qp119, queue:filter(F, Qp120), Qp121, Qp122, Qp123,
                 Qp124, Qp125, Qp126, Qp127, Qp128});
?FILTER_P_Qp128(121,
                {Qp113, Qp114, Qp115, Qp116, Qp117,
                 Qp118, Qp119, Qp120, queue:filter(F, Qp121), Qp122, Qp123,
                 Qp124, Qp125, Qp126, Qp127, Qp128});
?FILTER_P_Qp128(122,
                {Qp113, Qp114, Qp115, Qp116, Qp117,
                 Qp118, Qp119, Qp120, Qp121, queue:filter(F, Qp122), Qp123,
                 Qp124, Qp125, Qp126, Qp127, Qp128});
?FILTER_P_Qp128(123,
                {Qp113, Qp114, Qp115, Qp116, Qp117,
                 Qp118, Qp119, Qp120, Qp121, Qp122, queue:filter(F, Qp123),
                 Qp124, Qp125, Qp126, Qp127, Qp128});
?FILTER_P_Qp128(124,
                {Qp113, Qp114, Qp115, Qp116, Qp117,
                 Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                 queue:filter(F, Qp124), Qp125, Qp126, Qp127, Qp128});
?FILTER_P_Qp128(125,
                {Qp113, Qp114, Qp115, Qp116, Qp117,
                 Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                 Qp124, queue:filter(F, Qp125), Qp126, Qp127, Qp128});
?FILTER_P_Qp128(126,
                {Qp113, Qp114, Qp115, Qp116, Qp117,
                 Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                 Qp124, Qp125, queue:filter(F, Qp126), Qp127, Qp128});
?FILTER_P_Qp128(127,
                {Qp113, Qp114, Qp115, Qp116, Qp117,
                 Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                 Qp124, Qp125, Qp126, queue:filter(F, Qp127), Qp128});
?FILTER_P_Qp128(128,
                {Qp113, Qp114, Qp115, Qp116, Qp117,
                 Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                 Qp124, Qp125, Qp126, Qp127, queue:filter(F, Qp128)}).

-define(IN_HIGHER_Qn128(P, V),
in_higher(P,
          {_,
           {Qn128, Qn127, Qn126, Qn125, Qn124, Qn123, Qn122, Qn121,
            Qn120, Qn119, Qn118, Qn117, Qn116, Qn115, Qn114, Qn113},
           Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
           Q0,
           Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}, X) ->
    {P,
     V,
     Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(IN_HIGHER_Qn112(P, V),
in_higher(P,
          {_,
           Qn128,
           {Qn112, Qn111, Qn110, Qn109, Qn108, Qn107, Qn106, Qn105,
            Qn104, Qn103, Qn102, Qn101, Qn100, Qn99, Qn98, Qn97},
           Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
           Q0,
           Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}, X) ->
    {P,
     Qn128,
     V,
     Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(IN_HIGHER_Qn96(P, V),
in_higher(P,
          {_,
           Qn128, Qn112,
           {Qn96, Qn95, Qn94, Qn93, Qn92, Qn91, Qn90, Qn89,
            Qn88, Qn87, Qn86, Qn85, Qn84, Qn83, Qn82, Qn81},
           Qn80, Qn64, Qn48, Qn32, Qn16,
           Q0,
           Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}, X) ->
    {P,
     Qn128, Qn112,
     V,
     Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(IN_HIGHER_Qn80(P, V),
in_higher(P,
          {_,
           Qn128, Qn112, Qn96,
           {Qn80, Qn79, Qn78, Qn77, Qn76, Qn75, Qn74, Qn73,
            Qn72, Qn71, Qn70, Qn69, Qn68, Qn67, Qn66, Qn65},
           Qn64, Qn48, Qn32, Qn16,
           Q0,
           Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}, X) ->
    {P,
     Qn128, Qn112, Qn96,
     V,
     Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(IN_HIGHER_Qn64(P, V),
in_higher(P,
          {_,
           Qn128, Qn112, Qn96, Qn80,
           {Qn64, Qn63, Qn62, Qn61, Qn60, Qn59, Qn58, Qn57,
            Qn56, Qn55, Qn54, Qn53, Qn52, Qn51, Qn50, Qn49},
           Qn48, Qn32, Qn16,
           Q0,
           Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}, X) ->
    {P,
     Qn128, Qn112, Qn96, Qn80,
     V,
     Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(IN_HIGHER_Qn48(P, V),
in_higher(P,
          {_,
           Qn128, Qn112, Qn96, Qn80, Qn64,
           {Qn48, Qn47, Qn46, Qn45, Qn44, Qn43, Qn42, Qn41,
            Qn40, Qn39, Qn38, Qn37, Qn36, Qn35, Qn34, Qn33},
           Qn32, Qn16,
           Q0,
           Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}, X) ->
    {P,
     Qn128, Qn112, Qn96, Qn80, Qn64,
     V,
     Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(IN_HIGHER_Qn32(P, V),
in_higher(P,
          {_,
           Qn128, Qn112, Qn96, Qn80, Qn64, Qn48,
           {Qn32, Qn31, Qn30, Qn29, Qn28, Qn27, Qn26, Qn25,
            Qn24, Qn23, Qn22, Qn21, Qn20, Qn19, Qn18, Qn17},
           Qn16,
           Q0,
           Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}, X) ->
    {P,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48,
     V,
     Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(IN_HIGHER_Qn16(P, V),
in_higher(P,
          {_,
           Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32,
           {Qn16, Qn15, Qn14, Qn13, Qn12, Qn11, Qn10, Qn9,
            Qn8, Qn7, Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
           Q0,
           Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}, X) ->
    {P,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32,
     V,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(IN_HIGHER_Qp16(P, V),
in_higher(P,
          {_,
           Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
           Q0,
           {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6, Qp7, Qp8,
            Qp9, Qp10, Qp11, Qp12, Qp13, Qp14, Qp15, Qp16},
           Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}, X) ->
    {P,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     V,
     Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(IN_HIGHER_Qp32(P, V),
in_higher(P,
          {_,
           Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
           Q0,
           Qp16,
           {Qp17, Qp18, Qp19, Qp20, Qp21, Qp22, Qp23, Qp24,
            Qp25, Qp26, Qp27, Qp28, Qp29, Qp30, Qp31, Qp32},
           Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}, X) ->
    {P,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16,
     V,
     Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(IN_HIGHER_Qp48(P, V),
in_higher(P,
          {_,
           Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
           Q0,
           Qp16, Qp32,
           {Qp33, Qp34, Qp35, Qp36, Qp37, Qp38, Qp39, Qp40,
            Qp41, Qp42, Qp43, Qp44, Qp45, Qp46, Qp47, Qp48},
           Qp64, Qp80, Qp96, Qp112, Qp128}, X) ->
    {P,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32,
     V,
     Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(IN_HIGHER_Qp64(P, V),
in_higher(P,
          {_,
           Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
           Q0,
           Qp16, Qp32, Qp48,
           {Qp49, Qp50, Qp51, Qp52, Qp53, Qp54, Qp55, Qp56,
            Qp57, Qp58, Qp59, Qp60, Qp61, Qp62, Qp63, Qp64},
           Qp80, Qp96, Qp112, Qp128}, X) ->
    {P,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48,
     V,
     Qp80, Qp96, Qp112, Qp128}).
-define(IN_HIGHER_Qp80(P, V),
in_higher(P,
          {_,
           Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
           Q0,
           Qp16, Qp32, Qp48, Qp64,
           {Qp65, Qp66, Qp67, Qp68, Qp69, Qp70, Qp71, Qp72,
            Qp73, Qp74, Qp75, Qp76, Qp77, Qp78, Qp79, Qp80},
           Qp96, Qp112, Qp128}, X) ->
    {P,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64,
     V,
     Qp96, Qp112, Qp128}).
-define(IN_HIGHER_Qp96(P, V),
in_higher(P,
          {_,
           Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
           Q0,
           Qp16, Qp32, Qp48, Qp64, Qp80,
           {Qp81, Qp82, Qp83, Qp84, Qp85, Qp86, Qp87, Qp88,
            Qp89, Qp90, Qp91, Qp92, Qp93, Qp94, Qp95, Qp96},
           Qp112, Qp128}, X) ->
    {P,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80,
     V,
     Qp112, Qp128}).
-define(IN_HIGHER_Qp112(P, V),
in_higher(P,
          {_,
           Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
           Q0,
           Qp16, Qp32, Qp48, Qp64, Qp80, Qp96,
           {Qp97, Qp98, Qp99, Qp100, Qp101, Qp102, Qp103, Qp104,
            Qp105, Qp106, Qp107, Qp108, Qp109, Qp110, Qp111, Qp112},
           Qp128}, X) ->
    {P,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96,
     V,
     Qp128}).
-define(IN_HIGHER_Qp128(P, V),
in_higher(P,
          {_,
           Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
           Q0,
           Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112,
           {Qp113, Qp114, Qp115, Qp116, Qp117, Qp118, Qp119, Qp120,
            Qp121, Qp122, Qp123, Qp124, Qp125, Qp126, Qp127, Qp128}}, X) ->
    {P,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112,
     V}).

?IN_HIGHER_Qn128(-128,
                 {queue:in(X, Qn128), Qn127, Qn126, Qn125, Qn124,
                  Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                  Qn117, Qn116, Qn115, Qn114, Qn113});
?IN_HIGHER_Qn128(-127,
                 {Qn128, queue:in(X, Qn127), Qn126, Qn125, Qn124,
                  Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                  Qn117, Qn116, Qn115, Qn114, Qn113});
?IN_HIGHER_Qn128(-126,
                 {Qn128, Qn127, queue:in(X, Qn126), Qn125, Qn124,
                  Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                  Qn117, Qn116, Qn115, Qn114, Qn113});
?IN_HIGHER_Qn128(-125,
                 {Qn128, Qn127, Qn126, queue:in(X, Qn125), Qn124,
                  Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                  Qn117, Qn116, Qn115, Qn114, Qn113});
?IN_HIGHER_Qn128(-124,
                 {Qn128, Qn127, Qn126, Qn125, queue:in(X, Qn124),
                  Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                  Qn117, Qn116, Qn115, Qn114, Qn113});
?IN_HIGHER_Qn128(-123,
                 {Qn128, Qn127, Qn126, Qn125, Qn124,
                  queue:in(X, Qn123), Qn122, Qn121, Qn120, Qn119, Qn118,
                  Qn117, Qn116, Qn115, Qn114, Qn113});
?IN_HIGHER_Qn128(-122,
                 {Qn128, Qn127, Qn126, Qn125, Qn124,
                  Qn123, queue:in(X, Qn122), Qn121, Qn120, Qn119, Qn118,
                  Qn117, Qn116, Qn115, Qn114, Qn113});
?IN_HIGHER_Qn128(-121,
                 {Qn128, Qn127, Qn126, Qn125, Qn124,
                  Qn123, Qn122, queue:in(X, Qn121), Qn120, Qn119, Qn118,
                  Qn117, Qn116, Qn115, Qn114, Qn113});
?IN_HIGHER_Qn128(-120,
                 {Qn128, Qn127, Qn126, Qn125, Qn124,
                  Qn123, Qn122, Qn121, queue:in(X, Qn120), Qn119, Qn118,
                  Qn117, Qn116, Qn115, Qn114, Qn113});
?IN_HIGHER_Qn128(-119,
                 {Qn128, Qn127, Qn126, Qn125, Qn124,
                  Qn123, Qn122, Qn121, Qn120, queue:in(X, Qn119), Qn118,
                  Qn117, Qn116, Qn115, Qn114, Qn113});
?IN_HIGHER_Qn128(-118,
                 {Qn128, Qn127, Qn126, Qn125, Qn124,
                  Qn123, Qn122, Qn121, Qn120, Qn119, queue:in(X, Qn118),
                  Qn117, Qn116, Qn115, Qn114, Qn113});
?IN_HIGHER_Qn128(-117,
                 {Qn128, Qn127, Qn126, Qn125, Qn124,
                  Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                  queue:in(X, Qn117), Qn116, Qn115, Qn114, Qn113});
?IN_HIGHER_Qn128(-116,
                 {Qn128, Qn127, Qn126, Qn125, Qn124,
                  Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                  Qn117, queue:in(X, Qn116), Qn115, Qn114, Qn113});
?IN_HIGHER_Qn128(-115,
                 {Qn128, Qn127, Qn126, Qn125, Qn124,
                  Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                  Qn117, Qn116, queue:in(X, Qn115), Qn114, Qn113});
?IN_HIGHER_Qn128(-114,
                 {Qn128, Qn127, Qn126, Qn125, Qn124,
                  Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                  Qn117, Qn116, Qn115, queue:in(X, Qn114), Qn113});
?IN_HIGHER_Qn128(-113,
                 {Qn128, Qn127, Qn126, Qn125, Qn124,
                  Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                  Qn117, Qn116, Qn115, Qn114, queue:in(X, Qn113)});
?IN_HIGHER_Qn112(-112,
                 {queue:in(X, Qn112), Qn111, Qn110, Qn109, Qn108,
                  Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                  Qn101, Qn100, Qn99, Qn98, Qn97});
?IN_HIGHER_Qn112(-111,
                 {Qn112, queue:in(X, Qn111), Qn110, Qn109, Qn108,
                  Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                  Qn101, Qn100, Qn99, Qn98, Qn97});
?IN_HIGHER_Qn112(-110,
                 {Qn112, Qn111, queue:in(X, Qn110), Qn109, Qn108,
                  Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                  Qn101, Qn100, Qn99, Qn98, Qn97});
?IN_HIGHER_Qn112(-109,
                 {Qn112, Qn111, Qn110, queue:in(X, Qn109), Qn108,
                  Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                  Qn101, Qn100, Qn99, Qn98, Qn97});
?IN_HIGHER_Qn112(-108,
                 {Qn112, Qn111, Qn110, Qn109, queue:in(X, Qn108),
                  Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                  Qn101, Qn100, Qn99, Qn98, Qn97});
?IN_HIGHER_Qn112(-107,
                 {Qn112, Qn111, Qn110, Qn109, Qn108,
                  queue:in(X, Qn107), Qn106, Qn105, Qn104, Qn103, Qn102,
                  Qn101, Qn100, Qn99, Qn98, Qn97});
?IN_HIGHER_Qn112(-106,
                 {Qn112, Qn111, Qn110, Qn109, Qn108,
                  Qn107, queue:in(X, Qn106), Qn105, Qn104, Qn103, Qn102,
                  Qn101, Qn100, Qn99, Qn98, Qn97});
?IN_HIGHER_Qn112(-105,
                 {Qn112, Qn111, Qn110, Qn109, Qn108,
                  Qn107, Qn106, queue:in(X, Qn105), Qn104, Qn103, Qn102,
                  Qn101, Qn100, Qn99, Qn98, Qn97});
?IN_HIGHER_Qn112(-104,
                 {Qn112, Qn111, Qn110, Qn109, Qn108,
                  Qn107, Qn106, Qn105, queue:in(X, Qn104), Qn103, Qn102,
                  Qn101, Qn100, Qn99, Qn98, Qn97});
?IN_HIGHER_Qn112(-103,
                 {Qn112, Qn111, Qn110, Qn109, Qn108,
                  Qn107, Qn106, Qn105, Qn104, queue:in(X, Qn103), Qn102,
                  Qn101, Qn100, Qn99, Qn98, Qn97});
?IN_HIGHER_Qn112(-102,
                 {Qn112, Qn111, Qn110, Qn109, Qn108,
                  Qn107, Qn106, Qn105, Qn104, Qn103, queue:in(X, Qn102),
                  Qn101, Qn100, Qn99, Qn98, Qn97});
?IN_HIGHER_Qn112(-101,
                 {Qn112, Qn111, Qn110, Qn109, Qn108,
                  Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                  queue:in(X, Qn101), Qn100, Qn99, Qn98, Qn97});
?IN_HIGHER_Qn112(-100,
                 {Qn112, Qn111, Qn110, Qn109, Qn108,
                  Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                  Qn101, queue:in(X, Qn100), Qn99, Qn98, Qn97});
?IN_HIGHER_Qn112(-99,
                 {Qn112, Qn111, Qn110, Qn109, Qn108,
                  Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                  Qn101, Qn100, queue:in(X, Qn99), Qn98, Qn97});
?IN_HIGHER_Qn112(-98,
                 {Qn112, Qn111, Qn110, Qn109, Qn108,
                  Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                  Qn101, Qn100, Qn99, queue:in(X, Qn98), Qn97});
?IN_HIGHER_Qn112(-97,
                 {Qn112, Qn111, Qn110, Qn109, Qn108,
                  Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                  Qn101, Qn100, Qn99, Qn98, queue:in(X, Qn97)});
?IN_HIGHER_Qn96(-96,
                {queue:in(X, Qn96), Qn95, Qn94, Qn93, Qn92,
                 Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                 Qn85, Qn84, Qn83, Qn82, Qn81});
?IN_HIGHER_Qn96(-95,
                {Qn96, queue:in(X, Qn95), Qn94, Qn93, Qn92,
                 Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                 Qn85, Qn84, Qn83, Qn82, Qn81});
?IN_HIGHER_Qn96(-94,
                {Qn96, Qn95, queue:in(X, Qn94), Qn93, Qn92,
                 Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                 Qn85, Qn84, Qn83, Qn82, Qn81});
?IN_HIGHER_Qn96(-93,
                {Qn96, Qn95, Qn94, queue:in(X, Qn93), Qn92,
                 Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                 Qn85, Qn84, Qn83, Qn82, Qn81});
?IN_HIGHER_Qn96(-92,
                {Qn96, Qn95, Qn94, Qn93, queue:in(X, Qn92),
                 Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                 Qn85, Qn84, Qn83, Qn82, Qn81});
?IN_HIGHER_Qn96(-91,
                {Qn96, Qn95, Qn94, Qn93, Qn92,
                 queue:in(X, Qn91), Qn90, Qn89, Qn88, Qn87, Qn86,
                 Qn85, Qn84, Qn83, Qn82, Qn81});
?IN_HIGHER_Qn96(-90,
                {Qn96, Qn95, Qn94, Qn93, Qn92,
                 Qn91, queue:in(X, Qn90), Qn89, Qn88, Qn87, Qn86,
                 Qn85, Qn84, Qn83, Qn82, Qn81});
?IN_HIGHER_Qn96(-89,
                {Qn96, Qn95, Qn94, Qn93, Qn92,
                 Qn91, Qn90, queue:in(X, Qn89), Qn88, Qn87, Qn86,
                 Qn85, Qn84, Qn83, Qn82, Qn81});
?IN_HIGHER_Qn96(-88,
                {Qn96, Qn95, Qn94, Qn93, Qn92,
                 Qn91, Qn90, Qn89, queue:in(X, Qn88), Qn87, Qn86,
                 Qn85, Qn84, Qn83, Qn82, Qn81});
?IN_HIGHER_Qn96(-87,
                {Qn96, Qn95, Qn94, Qn93, Qn92,
                 Qn91, Qn90, Qn89, Qn88, queue:in(X, Qn87), Qn86,
                 Qn85, Qn84, Qn83, Qn82, Qn81});
?IN_HIGHER_Qn96(-86,
                {Qn96, Qn95, Qn94, Qn93, Qn92,
                 Qn91, Qn90, Qn89, Qn88, Qn87, queue:in(X, Qn86),
                 Qn85, Qn84, Qn83, Qn82, Qn81});
?IN_HIGHER_Qn96(-85,
                {Qn96, Qn95, Qn94, Qn93, Qn92,
                 Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                 queue:in(X, Qn85), Qn84, Qn83, Qn82, Qn81});
?IN_HIGHER_Qn96(-84,
                {Qn96, Qn95, Qn94, Qn93, Qn92,
                 Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                 Qn85, queue:in(X, Qn84), Qn83, Qn82, Qn81});
?IN_HIGHER_Qn96(-83,
                {Qn96, Qn95, Qn94, Qn93, Qn92,
                 Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                 Qn85, Qn84, queue:in(X, Qn83), Qn82, Qn81});
?IN_HIGHER_Qn96(-82,
                {Qn96, Qn95, Qn94, Qn93, Qn92,
                 Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                 Qn85, Qn84, Qn83, queue:in(X, Qn82), Qn81});
?IN_HIGHER_Qn96(-81,
                {Qn96, Qn95, Qn94, Qn93, Qn92,
                 Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                 Qn85, Qn84, Qn83, Qn82, queue:in(X, Qn81)});
?IN_HIGHER_Qn80(-80,
                {queue:in(X, Qn80), Qn79, Qn78, Qn77, Qn76,
                 Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                 Qn69, Qn68, Qn67, Qn66, Qn65});
?IN_HIGHER_Qn80(-79,
                {Qn80, queue:in(X, Qn79), Qn78, Qn77, Qn76,
                 Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                 Qn69, Qn68, Qn67, Qn66, Qn65});
?IN_HIGHER_Qn80(-78,
                {Qn80, Qn79, queue:in(X, Qn78), Qn77, Qn76,
                 Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                 Qn69, Qn68, Qn67, Qn66, Qn65});
?IN_HIGHER_Qn80(-77,
                {Qn80, Qn79, Qn78, queue:in(X, Qn77), Qn76,
                 Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                 Qn69, Qn68, Qn67, Qn66, Qn65});
?IN_HIGHER_Qn80(-76,
                {Qn80, Qn79, Qn78, Qn77, queue:in(X, Qn76),
                 Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                 Qn69, Qn68, Qn67, Qn66, Qn65});
?IN_HIGHER_Qn80(-75,
                {Qn80, Qn79, Qn78, Qn77, Qn76,
                 queue:in(X, Qn75), Qn74, Qn73, Qn72, Qn71, Qn70,
                 Qn69, Qn68, Qn67, Qn66, Qn65});
?IN_HIGHER_Qn80(-74,
                {Qn80, Qn79, Qn78, Qn77, Qn76,
                 Qn75, queue:in(X, Qn74), Qn73, Qn72, Qn71, Qn70,
                 Qn69, Qn68, Qn67, Qn66, Qn65});
?IN_HIGHER_Qn80(-73,
                {Qn80, Qn79, Qn78, Qn77, Qn76,
                 Qn75, Qn74, queue:in(X, Qn73), Qn72, Qn71, Qn70,
                 Qn69, Qn68, Qn67, Qn66, Qn65});
?IN_HIGHER_Qn80(-72,
                {Qn80, Qn79, Qn78, Qn77, Qn76,
                 Qn75, Qn74, Qn73, queue:in(X, Qn72), Qn71, Qn70,
                 Qn69, Qn68, Qn67, Qn66, Qn65});
?IN_HIGHER_Qn80(-71,
                {Qn80, Qn79, Qn78, Qn77, Qn76,
                 Qn75, Qn74, Qn73, Qn72, queue:in(X, Qn71), Qn70,
                 Qn69, Qn68, Qn67, Qn66, Qn65});
?IN_HIGHER_Qn80(-70,
                {Qn80, Qn79, Qn78, Qn77, Qn76,
                 Qn75, Qn74, Qn73, Qn72, Qn71, queue:in(X, Qn70),
                 Qn69, Qn68, Qn67, Qn66, Qn65});
?IN_HIGHER_Qn80(-69,
                {Qn80, Qn79, Qn78, Qn77, Qn76,
                 Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                 queue:in(X, Qn69), Qn68, Qn67, Qn66, Qn65});
?IN_HIGHER_Qn80(-68,
                {Qn80, Qn79, Qn78, Qn77, Qn76,
                 Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                 Qn69, queue:in(X, Qn68), Qn67, Qn66, Qn65});
?IN_HIGHER_Qn80(-67,
                {Qn80, Qn79, Qn78, Qn77, Qn76,
                 Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                 Qn69, Qn68, queue:in(X, Qn67), Qn66, Qn65});
?IN_HIGHER_Qn80(-66,
                {Qn80, Qn79, Qn78, Qn77, Qn76,
                 Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                 Qn69, Qn68, Qn67, queue:in(X, Qn66), Qn65});
?IN_HIGHER_Qn80(-65,
                {Qn80, Qn79, Qn78, Qn77, Qn76,
                 Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                 Qn69, Qn68, Qn67, Qn66, queue:in(X, Qn65)});
?IN_HIGHER_Qn64(-64,
                {queue:in(X, Qn64), Qn63, Qn62, Qn61, Qn60,
                 Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                 Qn53, Qn52, Qn51, Qn50, Qn49});
?IN_HIGHER_Qn64(-63,
                {Qn64, queue:in(X, Qn63), Qn62, Qn61, Qn60,
                 Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                 Qn53, Qn52, Qn51, Qn50, Qn49});
?IN_HIGHER_Qn64(-62,
                {Qn64, Qn63, queue:in(X, Qn62), Qn61, Qn60,
                 Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                 Qn53, Qn52, Qn51, Qn50, Qn49});
?IN_HIGHER_Qn64(-61,
                {Qn64, Qn63, Qn62, queue:in(X, Qn61), Qn60,
                 Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                 Qn53, Qn52, Qn51, Qn50, Qn49});
?IN_HIGHER_Qn64(-60,
                {Qn64, Qn63, Qn62, Qn61, queue:in(X, Qn60),
                 Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                 Qn53, Qn52, Qn51, Qn50, Qn49});
?IN_HIGHER_Qn64(-59,
                {Qn64, Qn63, Qn62, Qn61, Qn60,
                 queue:in(X, Qn59), Qn58, Qn57, Qn56, Qn55, Qn54,
                 Qn53, Qn52, Qn51, Qn50, Qn49});
?IN_HIGHER_Qn64(-58,
                {Qn64, Qn63, Qn62, Qn61, Qn60,
                 Qn59, queue:in(X, Qn58), Qn57, Qn56, Qn55, Qn54,
                 Qn53, Qn52, Qn51, Qn50, Qn49});
?IN_HIGHER_Qn64(-57,
                {Qn64, Qn63, Qn62, Qn61, Qn60,
                 Qn59, Qn58, queue:in(X, Qn57), Qn56, Qn55, Qn54,
                 Qn53, Qn52, Qn51, Qn50, Qn49});
?IN_HIGHER_Qn64(-56,
                {Qn64, Qn63, Qn62, Qn61, Qn60,
                 Qn59, Qn58, Qn57, queue:in(X, Qn56), Qn55, Qn54,
                 Qn53, Qn52, Qn51, Qn50, Qn49});
?IN_HIGHER_Qn64(-55,
                {Qn64, Qn63, Qn62, Qn61, Qn60,
                 Qn59, Qn58, Qn57, Qn56, queue:in(X, Qn55), Qn54,
                 Qn53, Qn52, Qn51, Qn50, Qn49});
?IN_HIGHER_Qn64(-54,
                {Qn64, Qn63, Qn62, Qn61, Qn60,
                 Qn59, Qn58, Qn57, Qn56, Qn55, queue:in(X, Qn54),
                 Qn53, Qn52, Qn51, Qn50, Qn49});
?IN_HIGHER_Qn64(-53,
                {Qn64, Qn63, Qn62, Qn61, Qn60,
                 Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                 queue:in(X, Qn53), Qn52, Qn51, Qn50, Qn49});
?IN_HIGHER_Qn64(-52,
                {Qn64, Qn63, Qn62, Qn61, Qn60,
                 Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                 Qn53, queue:in(X, Qn52), Qn51, Qn50, Qn49});
?IN_HIGHER_Qn64(-51,
                {Qn64, Qn63, Qn62, Qn61, Qn60,
                 Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                 Qn53, Qn52, queue:in(X, Qn51), Qn50, Qn49});
?IN_HIGHER_Qn64(-50,
                {Qn64, Qn63, Qn62, Qn61, Qn60,
                 Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                 Qn53, Qn52, Qn51, queue:in(X, Qn50), Qn49});
?IN_HIGHER_Qn64(-49,
                {Qn64, Qn63, Qn62, Qn61, Qn60,
                 Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                 Qn53, Qn52, Qn51, Qn50, queue:in(X, Qn49)});
?IN_HIGHER_Qn48(-48,
                {queue:in(X, Qn48), Qn47, Qn46, Qn45, Qn44,
                 Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                 Qn37, Qn36, Qn35, Qn34, Qn33});
?IN_HIGHER_Qn48(-47,
                {Qn48, queue:in(X, Qn47), Qn46, Qn45, Qn44,
                 Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                 Qn37, Qn36, Qn35, Qn34, Qn33});
?IN_HIGHER_Qn48(-46,
                {Qn48, Qn47, queue:in(X, Qn46), Qn45, Qn44,
                 Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                 Qn37, Qn36, Qn35, Qn34, Qn33});
?IN_HIGHER_Qn48(-45,
                {Qn48, Qn47, Qn46, queue:in(X, Qn45), Qn44,
                 Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                 Qn37, Qn36, Qn35, Qn34, Qn33});
?IN_HIGHER_Qn48(-44,
                {Qn48, Qn47, Qn46, Qn45, queue:in(X, Qn44),
                 Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                 Qn37, Qn36, Qn35, Qn34, Qn33});
?IN_HIGHER_Qn48(-43,
                {Qn48, Qn47, Qn46, Qn45, Qn44,
                 queue:in(X, Qn43), Qn42, Qn41, Qn40, Qn39, Qn38,
                 Qn37, Qn36, Qn35, Qn34, Qn33});
?IN_HIGHER_Qn48(-42,
                {Qn48, Qn47, Qn46, Qn45, Qn44,
                 Qn43, queue:in(X, Qn42), Qn41, Qn40, Qn39, Qn38,
                 Qn37, Qn36, Qn35, Qn34, Qn33});
?IN_HIGHER_Qn48(-41,
                {Qn48, Qn47, Qn46, Qn45, Qn44,
                 Qn43, Qn42, queue:in(X, Qn41), Qn40, Qn39, Qn38,
                 Qn37, Qn36, Qn35, Qn34, Qn33});
?IN_HIGHER_Qn48(-40,
                {Qn48, Qn47, Qn46, Qn45, Qn44,
                 Qn43, Qn42, Qn41, queue:in(X, Qn40), Qn39, Qn38,
                 Qn37, Qn36, Qn35, Qn34, Qn33});
?IN_HIGHER_Qn48(-39,
                {Qn48, Qn47, Qn46, Qn45, Qn44,
                 Qn43, Qn42, Qn41, Qn40, queue:in(X, Qn39), Qn38,
                 Qn37, Qn36, Qn35, Qn34, Qn33});
?IN_HIGHER_Qn48(-38,
                {Qn48, Qn47, Qn46, Qn45, Qn44,
                 Qn43, Qn42, Qn41, Qn40, Qn39, queue:in(X, Qn38),
                 Qn37, Qn36, Qn35, Qn34, Qn33});
?IN_HIGHER_Qn48(-37,
                {Qn48, Qn47, Qn46, Qn45, Qn44,
                 Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                 queue:in(X, Qn37), Qn36, Qn35, Qn34, Qn33});
?IN_HIGHER_Qn48(-36,
                {Qn48, Qn47, Qn46, Qn45, Qn44,
                 Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                 Qn37, queue:in(X, Qn36), Qn35, Qn34, Qn33});
?IN_HIGHER_Qn48(-35,
                {Qn48, Qn47, Qn46, Qn45, Qn44,
                 Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                 Qn37, Qn36, queue:in(X, Qn35), Qn34, Qn33});
?IN_HIGHER_Qn48(-34,
                {Qn48, Qn47, Qn46, Qn45, Qn44,
                 Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                 Qn37, Qn36, Qn35, queue:in(X, Qn34), Qn33});
?IN_HIGHER_Qn48(-33,
                {Qn48, Qn47, Qn46, Qn45, Qn44,
                 Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                 Qn37, Qn36, Qn35, Qn34, queue:in(X, Qn33)});
?IN_HIGHER_Qn32(-32,
                {queue:in(X, Qn32), Qn31, Qn30, Qn29, Qn28,
                 Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                 Qn21, Qn20, Qn19, Qn18, Qn17});
?IN_HIGHER_Qn32(-31,
                {Qn32, queue:in(X, Qn31), Qn30, Qn29, Qn28,
                 Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                 Qn21, Qn20, Qn19, Qn18, Qn17});
?IN_HIGHER_Qn32(-30,
                {Qn32, Qn31, queue:in(X, Qn30), Qn29, Qn28,
                 Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                 Qn21, Qn20, Qn19, Qn18, Qn17});
?IN_HIGHER_Qn32(-29,
                {Qn32, Qn31, Qn30, queue:in(X, Qn29), Qn28,
                 Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                 Qn21, Qn20, Qn19, Qn18, Qn17});
?IN_HIGHER_Qn32(-28,
                {Qn32, Qn31, Qn30, Qn29, queue:in(X, Qn28),
                 Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                 Qn21, Qn20, Qn19, Qn18, Qn17});
?IN_HIGHER_Qn32(-27,
                {Qn32, Qn31, Qn30, Qn29, Qn28,
                 queue:in(X, Qn27), Qn26, Qn25, Qn24, Qn23, Qn22,
                 Qn21, Qn20, Qn19, Qn18, Qn17});
?IN_HIGHER_Qn32(-26,
                {Qn32, Qn31, Qn30, Qn29, Qn28,
                 Qn27, queue:in(X, Qn26), Qn25, Qn24, Qn23, Qn22,
                 Qn21, Qn20, Qn19, Qn18, Qn17});
?IN_HIGHER_Qn32(-25,
                {Qn32, Qn31, Qn30, Qn29, Qn28,
                 Qn27, Qn26, queue:in(X, Qn25), Qn24, Qn23, Qn22,
                 Qn21, Qn20, Qn19, Qn18, Qn17});
?IN_HIGHER_Qn32(-24,
                {Qn32, Qn31, Qn30, Qn29, Qn28,
                 Qn27, Qn26, Qn25, queue:in(X, Qn24), Qn23, Qn22,
                 Qn21, Qn20, Qn19, Qn18, Qn17});
?IN_HIGHER_Qn32(-23,
                {Qn32, Qn31, Qn30, Qn29, Qn28,
                 Qn27, Qn26, Qn25, Qn24, queue:in(X, Qn23), Qn22,
                 Qn21, Qn20, Qn19, Qn18, Qn17});
?IN_HIGHER_Qn32(-22,
                {Qn32, Qn31, Qn30, Qn29, Qn28,
                 Qn27, Qn26, Qn25, Qn24, Qn23, queue:in(X, Qn22),
                 Qn21, Qn20, Qn19, Qn18, Qn17});
?IN_HIGHER_Qn32(-21,
                {Qn32, Qn31, Qn30, Qn29, Qn28,
                 Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                 queue:in(X, Qn21), Qn20, Qn19, Qn18, Qn17});
?IN_HIGHER_Qn32(-20,
                {Qn32, Qn31, Qn30, Qn29, Qn28,
                 Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                 Qn21, queue:in(X, Qn20), Qn19, Qn18, Qn17});
?IN_HIGHER_Qn32(-19,
                {Qn32, Qn31, Qn30, Qn29, Qn28,
                 Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                 Qn21, Qn20, queue:in(X, Qn19), Qn18, Qn17});
?IN_HIGHER_Qn32(-18,
                {Qn32, Qn31, Qn30, Qn29, Qn28,
                 Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                 Qn21, Qn20, Qn19, queue:in(X, Qn18), Qn17});
?IN_HIGHER_Qn32(-17,
                {Qn32, Qn31, Qn30, Qn29, Qn28,
                 Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                 Qn21, Qn20, Qn19, Qn18, queue:in(X, Qn17)});
?IN_HIGHER_Qn16(-16,
                {queue:in(X, Qn16), Qn15, Qn14, Qn13, Qn12,
                 Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                 Qn5, Qn4, Qn3, Qn2, Qn1});
?IN_HIGHER_Qn16(-15,
                {Qn16, queue:in(X, Qn15), Qn14, Qn13, Qn12,
                 Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                 Qn5, Qn4, Qn3, Qn2, Qn1});
?IN_HIGHER_Qn16(-14,
                {Qn16, Qn15, queue:in(X, Qn14), Qn13, Qn12,
                 Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                 Qn5, Qn4, Qn3, Qn2, Qn1});
?IN_HIGHER_Qn16(-13,
                {Qn16, Qn15, Qn14, queue:in(X, Qn13), Qn12,
                 Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                 Qn5, Qn4, Qn3, Qn2, Qn1});
?IN_HIGHER_Qn16(-12,
                {Qn16, Qn15, Qn14, Qn13, queue:in(X, Qn12),
                 Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                 Qn5, Qn4, Qn3, Qn2, Qn1});
?IN_HIGHER_Qn16(-11,
                {Qn16, Qn15, Qn14, Qn13, Qn12,
                 queue:in(X, Qn11), Qn10, Qn9, Qn8, Qn7, Qn6,
                 Qn5, Qn4, Qn3, Qn2, Qn1});
?IN_HIGHER_Qn16(-10,
                {Qn16, Qn15, Qn14, Qn13, Qn12,
                 Qn11, queue:in(X, Qn10), Qn9, Qn8, Qn7, Qn6,
                 Qn5, Qn4, Qn3, Qn2, Qn1});
?IN_HIGHER_Qn16(-9,
                {Qn16, Qn15, Qn14, Qn13, Qn12,
                 Qn11, Qn10, queue:in(X, Qn9), Qn8, Qn7, Qn6,
                 Qn5, Qn4, Qn3, Qn2, Qn1});
?IN_HIGHER_Qn16(-8,
                {Qn16, Qn15, Qn14, Qn13, Qn12,
                 Qn11, Qn10, Qn9, queue:in(X, Qn8), Qn7, Qn6,
                 Qn5, Qn4, Qn3, Qn2, Qn1});
?IN_HIGHER_Qn16(-7,
                {Qn16, Qn15, Qn14, Qn13, Qn12,
                 Qn11, Qn10, Qn9, Qn8, queue:in(X, Qn7), Qn6,
                 Qn5, Qn4, Qn3, Qn2, Qn1});
?IN_HIGHER_Qn16(-6,
                {Qn16, Qn15, Qn14, Qn13, Qn12,
                 Qn11, Qn10, Qn9, Qn8, Qn7, queue:in(X, Qn6),
                 Qn5, Qn4, Qn3, Qn2, Qn1});
?IN_HIGHER_Qn16(-5,
                {Qn16, Qn15, Qn14, Qn13, Qn12,
                 Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                 queue:in(X, Qn5), Qn4, Qn3, Qn2, Qn1});
?IN_HIGHER_Qn16(-4,
                {Qn16, Qn15, Qn14, Qn13, Qn12,
                 Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                 Qn5, queue:in(X, Qn4), Qn3, Qn2, Qn1});
?IN_HIGHER_Qn16(-3,
                {Qn16, Qn15, Qn14, Qn13, Qn12,
                 Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                 Qn5, Qn4, queue:in(X, Qn3), Qn2, Qn1});
?IN_HIGHER_Qn16(-2,
                {Qn16, Qn15, Qn14, Qn13, Qn12,
                 Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                 Qn5, Qn4, Qn3, queue:in(X, Qn2), Qn1});
?IN_HIGHER_Qn16(-1,
                {Qn16, Qn15, Qn14, Qn13, Qn12,
                 Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                 Qn5, Qn4, Qn3, Qn2, queue:in(X, Qn1)});
in_higher(0,
          {_,
           Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
           Q0,
           Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}, X) ->
    {0,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     queue:in(X, Q0),
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128};
?IN_HIGHER_Qp16(1,
                {queue:in(X, Qp1), Qp2, Qp3, Qp4, Qp5,
                 Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                 Qp12, Qp13, Qp14, Qp15, Qp16});
?IN_HIGHER_Qp16(2,
                {Qp1, queue:in(X, Qp2), Qp3, Qp4, Qp5,
                 Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                 Qp12, Qp13, Qp14, Qp15, Qp16});
?IN_HIGHER_Qp16(3,
                {Qp1, Qp2, queue:in(X, Qp3), Qp4, Qp5,
                 Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                 Qp12, Qp13, Qp14, Qp15, Qp16});
?IN_HIGHER_Qp16(4,
                {Qp1, Qp2, Qp3, queue:in(X, Qp4), Qp5,
                 Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                 Qp12, Qp13, Qp14, Qp15, Qp16});
?IN_HIGHER_Qp16(5,
                {Qp1, Qp2, Qp3, Qp4, queue:in(X, Qp5),
                 Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                 Qp12, Qp13, Qp14, Qp15, Qp16});
?IN_HIGHER_Qp16(6,
                {Qp1, Qp2, Qp3, Qp4, Qp5,
                 queue:in(X, Qp6), Qp7, Qp8, Qp9, Qp10, Qp11,
                 Qp12, Qp13, Qp14, Qp15, Qp16});
?IN_HIGHER_Qp16(7,
                {Qp1, Qp2, Qp3, Qp4, Qp5,
                 Qp6, queue:in(X, Qp7), Qp8, Qp9, Qp10, Qp11,
                 Qp12, Qp13, Qp14, Qp15, Qp16});
?IN_HIGHER_Qp16(8,
                {Qp1, Qp2, Qp3, Qp4, Qp5,
                 Qp6, Qp7, queue:in(X, Qp8), Qp9, Qp10, Qp11,
                 Qp12, Qp13, Qp14, Qp15, Qp16});
?IN_HIGHER_Qp16(9,
                {Qp1, Qp2, Qp3, Qp4, Qp5,
                 Qp6, Qp7, Qp8, queue:in(X, Qp9), Qp10, Qp11,
                 Qp12, Qp13, Qp14, Qp15, Qp16});
?IN_HIGHER_Qp16(10,
                {Qp1, Qp2, Qp3, Qp4, Qp5,
                 Qp6, Qp7, Qp8, Qp9, queue:in(X, Qp10), Qp11,
                 Qp12, Qp13, Qp14, Qp15, Qp16});
?IN_HIGHER_Qp16(11,
                {Qp1, Qp2, Qp3, Qp4, Qp5,
                 Qp6, Qp7, Qp8, Qp9, Qp10, queue:in(X, Qp11),
                 Qp12, Qp13, Qp14, Qp15, Qp16});
?IN_HIGHER_Qp16(12,
                {Qp1, Qp2, Qp3, Qp4, Qp5,
                 Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                 queue:in(X, Qp12), Qp13, Qp14, Qp15, Qp16});
?IN_HIGHER_Qp16(13,
                {Qp1, Qp2, Qp3, Qp4, Qp5,
                 Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                 Qp12, queue:in(X, Qp13), Qp14, Qp15, Qp16});
?IN_HIGHER_Qp16(14,
                {Qp1, Qp2, Qp3, Qp4, Qp5,
                 Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                 Qp12, Qp13, queue:in(X, Qp14), Qp15, Qp16});
?IN_HIGHER_Qp16(15,
                {Qp1, Qp2, Qp3, Qp4, Qp5,
                 Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                 Qp12, Qp13, Qp14, queue:in(X, Qp15), Qp16});
?IN_HIGHER_Qp16(16,
                {Qp1, Qp2, Qp3, Qp4, Qp5,
                 Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                 Qp12, Qp13, Qp14, Qp15, queue:in(X, Qp16)});
?IN_HIGHER_Qp32(17,
                {queue:in(X, Qp17), Qp18, Qp19, Qp20, Qp21,
                 Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                 Qp28, Qp29, Qp30, Qp31, Qp32});
?IN_HIGHER_Qp32(18,
                {Qp17, queue:in(X, Qp18), Qp19, Qp20, Qp21,
                 Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                 Qp28, Qp29, Qp30, Qp31, Qp32});
?IN_HIGHER_Qp32(19,
                {Qp17, Qp18, queue:in(X, Qp19), Qp20, Qp21,
                 Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                 Qp28, Qp29, Qp30, Qp31, Qp32});
?IN_HIGHER_Qp32(20,
                {Qp17, Qp18, Qp19, queue:in(X, Qp20), Qp21,
                 Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                 Qp28, Qp29, Qp30, Qp31, Qp32});
?IN_HIGHER_Qp32(21,
                {Qp17, Qp18, Qp19, Qp20, queue:in(X, Qp21),
                 Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                 Qp28, Qp29, Qp30, Qp31, Qp32});
?IN_HIGHER_Qp32(22,
                {Qp17, Qp18, Qp19, Qp20, Qp21,
                 queue:in(X, Qp22), Qp23, Qp24, Qp25, Qp26, Qp27,
                 Qp28, Qp29, Qp30, Qp31, Qp32});
?IN_HIGHER_Qp32(23,
                {Qp17, Qp18, Qp19, Qp20, Qp21,
                 Qp22, queue:in(X, Qp23), Qp24, Qp25, Qp26, Qp27,
                 Qp28, Qp29, Qp30, Qp31, Qp32});
?IN_HIGHER_Qp32(24,
                {Qp17, Qp18, Qp19, Qp20, Qp21,
                 Qp22, Qp23, queue:in(X, Qp24), Qp25, Qp26, Qp27,
                 Qp28, Qp29, Qp30, Qp31, Qp32});
?IN_HIGHER_Qp32(25,
                {Qp17, Qp18, Qp19, Qp20, Qp21,
                 Qp22, Qp23, Qp24, queue:in(X, Qp25), Qp26, Qp27,
                 Qp28, Qp29, Qp30, Qp31, Qp32});
?IN_HIGHER_Qp32(26,
                {Qp17, Qp18, Qp19, Qp20, Qp21,
                 Qp22, Qp23, Qp24, Qp25, queue:in(X, Qp26), Qp27,
                 Qp28, Qp29, Qp30, Qp31, Qp32});
?IN_HIGHER_Qp32(27,
                {Qp17, Qp18, Qp19, Qp20, Qp21,
                 Qp22, Qp23, Qp24, Qp25, Qp26, queue:in(X, Qp27),
                 Qp28, Qp29, Qp30, Qp31, Qp32});
?IN_HIGHER_Qp32(28,
                {Qp17, Qp18, Qp19, Qp20, Qp21,
                 Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                 queue:in(X, Qp28), Qp29, Qp30, Qp31, Qp32});
?IN_HIGHER_Qp32(29,
                {Qp17, Qp18, Qp19, Qp20, Qp21,
                 Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                 Qp28, queue:in(X, Qp29), Qp30, Qp31, Qp32});
?IN_HIGHER_Qp32(30,
                {Qp17, Qp18, Qp19, Qp20, Qp21,
                 Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                 Qp28, Qp29, queue:in(X, Qp30), Qp31, Qp32});
?IN_HIGHER_Qp32(31,
                {Qp17, Qp18, Qp19, Qp20, Qp21,
                 Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                 Qp28, Qp29, Qp30, queue:in(X, Qp31), Qp32});
?IN_HIGHER_Qp32(32,
                {Qp17, Qp18, Qp19, Qp20, Qp21,
                 Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                 Qp28, Qp29, Qp30, Qp31, queue:in(X, Qp32)});
?IN_HIGHER_Qp48(33,
                {queue:in(X, Qp33), Qp34, Qp35, Qp36, Qp37,
                 Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                 Qp44, Qp45, Qp46, Qp47, Qp48});
?IN_HIGHER_Qp48(34,
                {Qp33, queue:in(X, Qp34), Qp35, Qp36, Qp37,
                 Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                 Qp44, Qp45, Qp46, Qp47, Qp48});
?IN_HIGHER_Qp48(35,
                {Qp33, Qp34, queue:in(X, Qp35), Qp36, Qp37,
                 Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                 Qp44, Qp45, Qp46, Qp47, Qp48});
?IN_HIGHER_Qp48(36,
                {Qp33, Qp34, Qp35, queue:in(X, Qp36), Qp37,
                 Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                 Qp44, Qp45, Qp46, Qp47, Qp48});
?IN_HIGHER_Qp48(37,
                {Qp33, Qp34, Qp35, Qp36, queue:in(X, Qp37),
                 Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                 Qp44, Qp45, Qp46, Qp47, Qp48});
?IN_HIGHER_Qp48(38,
                {Qp33, Qp34, Qp35, Qp36, Qp37,
                 queue:in(X, Qp38), Qp39, Qp40, Qp41, Qp42, Qp43,
                 Qp44, Qp45, Qp46, Qp47, Qp48});
?IN_HIGHER_Qp48(39,
                {Qp33, Qp34, Qp35, Qp36, Qp37,
                 Qp38, queue:in(X, Qp39), Qp40, Qp41, Qp42, Qp43,
                 Qp44, Qp45, Qp46, Qp47, Qp48});
?IN_HIGHER_Qp48(40,
                {Qp33, Qp34, Qp35, Qp36, Qp37,
                 Qp38, Qp39, queue:in(X, Qp40), Qp41, Qp42, Qp43,
                 Qp44, Qp45, Qp46, Qp47, Qp48});
?IN_HIGHER_Qp48(41,
                {Qp33, Qp34, Qp35, Qp36, Qp37,
                 Qp38, Qp39, Qp40, queue:in(X, Qp41), Qp42, Qp43,
                 Qp44, Qp45, Qp46, Qp47, Qp48});
?IN_HIGHER_Qp48(42,
                {Qp33, Qp34, Qp35, Qp36, Qp37,
                 Qp38, Qp39, Qp40, Qp41, queue:in(X, Qp42), Qp43,
                 Qp44, Qp45, Qp46, Qp47, Qp48});
?IN_HIGHER_Qp48(43,
                {Qp33, Qp34, Qp35, Qp36, Qp37,
                 Qp38, Qp39, Qp40, Qp41, Qp42, queue:in(X, Qp43),
                 Qp44, Qp45, Qp46, Qp47, Qp48});
?IN_HIGHER_Qp48(44,
                {Qp33, Qp34, Qp35, Qp36, Qp37,
                 Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                 queue:in(X, Qp44), Qp45, Qp46, Qp47, Qp48});
?IN_HIGHER_Qp48(45,
                {Qp33, Qp34, Qp35, Qp36, Qp37,
                 Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                 Qp44, queue:in(X, Qp45), Qp46, Qp47, Qp48});
?IN_HIGHER_Qp48(46,
                {Qp33, Qp34, Qp35, Qp36, Qp37,
                 Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                 Qp44, Qp45, queue:in(X, Qp46), Qp47, Qp48});
?IN_HIGHER_Qp48(47,
                {Qp33, Qp34, Qp35, Qp36, Qp37,
                 Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                 Qp44, Qp45, Qp46, queue:in(X, Qp47), Qp48});
?IN_HIGHER_Qp48(48,
                {Qp33, Qp34, Qp35, Qp36, Qp37,
                 Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                 Qp44, Qp45, Qp46, Qp47, queue:in(X, Qp48)});
?IN_HIGHER_Qp64(49,
                {queue:in(X, Qp49), Qp50, Qp51, Qp52, Qp53,
                 Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                 Qp60, Qp61, Qp62, Qp63, Qp64});
?IN_HIGHER_Qp64(50,
                {Qp49, queue:in(X, Qp50), Qp51, Qp52, Qp53,
                 Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                 Qp60, Qp61, Qp62, Qp63, Qp64});
?IN_HIGHER_Qp64(51,
                {Qp49, Qp50, queue:in(X, Qp51), Qp52, Qp53,
                 Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                 Qp60, Qp61, Qp62, Qp63, Qp64});
?IN_HIGHER_Qp64(52,
                {Qp49, Qp50, Qp51, queue:in(X, Qp52), Qp53,
                 Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                 Qp60, Qp61, Qp62, Qp63, Qp64});
?IN_HIGHER_Qp64(53,
                {Qp49, Qp50, Qp51, Qp52, queue:in(X, Qp53),
                 Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                 Qp60, Qp61, Qp62, Qp63, Qp64});
?IN_HIGHER_Qp64(54,
                {Qp49, Qp50, Qp51, Qp52, Qp53,
                 queue:in(X, Qp54), Qp55, Qp56, Qp57, Qp58, Qp59,
                 Qp60, Qp61, Qp62, Qp63, Qp64});
?IN_HIGHER_Qp64(55,
                {Qp49, Qp50, Qp51, Qp52, Qp53,
                 Qp54, queue:in(X, Qp55), Qp56, Qp57, Qp58, Qp59,
                 Qp60, Qp61, Qp62, Qp63, Qp64});
?IN_HIGHER_Qp64(56,
                {Qp49, Qp50, Qp51, Qp52, Qp53,
                 Qp54, Qp55, queue:in(X, Qp56), Qp57, Qp58, Qp59,
                 Qp60, Qp61, Qp62, Qp63, Qp64});
?IN_HIGHER_Qp64(57,
                {Qp49, Qp50, Qp51, Qp52, Qp53,
                 Qp54, Qp55, Qp56, queue:in(X, Qp57), Qp58, Qp59,
                 Qp60, Qp61, Qp62, Qp63, Qp64});
?IN_HIGHER_Qp64(58,
                {Qp49, Qp50, Qp51, Qp52, Qp53,
                 Qp54, Qp55, Qp56, Qp57, queue:in(X, Qp58), Qp59,
                 Qp60, Qp61, Qp62, Qp63, Qp64});
?IN_HIGHER_Qp64(59,
                {Qp49, Qp50, Qp51, Qp52, Qp53,
                 Qp54, Qp55, Qp56, Qp57, Qp58, queue:in(X, Qp59),
                 Qp60, Qp61, Qp62, Qp63, Qp64});
?IN_HIGHER_Qp64(60,
                {Qp49, Qp50, Qp51, Qp52, Qp53,
                 Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                 queue:in(X, Qp60), Qp61, Qp62, Qp63, Qp64});
?IN_HIGHER_Qp64(61,
                {Qp49, Qp50, Qp51, Qp52, Qp53,
                 Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                 Qp60, queue:in(X, Qp61), Qp62, Qp63, Qp64});
?IN_HIGHER_Qp64(62,
                {Qp49, Qp50, Qp51, Qp52, Qp53,
                 Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                 Qp60, Qp61, queue:in(X, Qp62), Qp63, Qp64});
?IN_HIGHER_Qp64(63,
                {Qp49, Qp50, Qp51, Qp52, Qp53,
                 Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                 Qp60, Qp61, Qp62, queue:in(X, Qp63), Qp64});
?IN_HIGHER_Qp64(64,
                {Qp49, Qp50, Qp51, Qp52, Qp53,
                 Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                 Qp60, Qp61, Qp62, Qp63, queue:in(X, Qp64)});
?IN_HIGHER_Qp80(65,
                {queue:in(X, Qp65), Qp66, Qp67, Qp68, Qp69,
                 Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                 Qp76, Qp77, Qp78, Qp79, Qp80});
?IN_HIGHER_Qp80(66,
                {Qp65, queue:in(X, Qp66), Qp67, Qp68, Qp69,
                 Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                 Qp76, Qp77, Qp78, Qp79, Qp80});
?IN_HIGHER_Qp80(67,
                {Qp65, Qp66, queue:in(X, Qp67), Qp68, Qp69,
                 Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                 Qp76, Qp77, Qp78, Qp79, Qp80});
?IN_HIGHER_Qp80(68,
                {Qp65, Qp66, Qp67, queue:in(X, Qp68), Qp69,
                 Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                 Qp76, Qp77, Qp78, Qp79, Qp80});
?IN_HIGHER_Qp80(69,
                {Qp65, Qp66, Qp67, Qp68, queue:in(X, Qp69),
                 Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                 Qp76, Qp77, Qp78, Qp79, Qp80});
?IN_HIGHER_Qp80(70,
                {Qp65, Qp66, Qp67, Qp68, Qp69,
                 queue:in(X, Qp70), Qp71, Qp72, Qp73, Qp74, Qp75,
                 Qp76, Qp77, Qp78, Qp79, Qp80});
?IN_HIGHER_Qp80(71,
                {Qp65, Qp66, Qp67, Qp68, Qp69,
                 Qp70, queue:in(X, Qp71), Qp72, Qp73, Qp74, Qp75,
                 Qp76, Qp77, Qp78, Qp79, Qp80});
?IN_HIGHER_Qp80(72,
                {Qp65, Qp66, Qp67, Qp68, Qp69,
                 Qp70, Qp71, queue:in(X, Qp72), Qp73, Qp74, Qp75,
                 Qp76, Qp77, Qp78, Qp79, Qp80});
?IN_HIGHER_Qp80(73,
                {Qp65, Qp66, Qp67, Qp68, Qp69,
                 Qp70, Qp71, Qp72, queue:in(X, Qp73), Qp74, Qp75,
                 Qp76, Qp77, Qp78, Qp79, Qp80});
?IN_HIGHER_Qp80(74,
                {Qp65, Qp66, Qp67, Qp68, Qp69,
                 Qp70, Qp71, Qp72, Qp73, queue:in(X, Qp74), Qp75,
                 Qp76, Qp77, Qp78, Qp79, Qp80});
?IN_HIGHER_Qp80(75,
                {Qp65, Qp66, Qp67, Qp68, Qp69,
                 Qp70, Qp71, Qp72, Qp73, Qp74, queue:in(X, Qp75),
                 Qp76, Qp77, Qp78, Qp79, Qp80});
?IN_HIGHER_Qp80(76,
                {Qp65, Qp66, Qp67, Qp68, Qp69,
                 Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                 queue:in(X, Qp76), Qp77, Qp78, Qp79, Qp80});
?IN_HIGHER_Qp80(77,
                {Qp65, Qp66, Qp67, Qp68, Qp69,
                 Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                 Qp76, queue:in(X, Qp77), Qp78, Qp79, Qp80});
?IN_HIGHER_Qp80(78,
                {Qp65, Qp66, Qp67, Qp68, Qp69,
                 Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                 Qp76, Qp77, queue:in(X, Qp78), Qp79, Qp80});
?IN_HIGHER_Qp80(79,
                {Qp65, Qp66, Qp67, Qp68, Qp69,
                 Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                 Qp76, Qp77, Qp78, queue:in(X, Qp79), Qp80});
?IN_HIGHER_Qp80(80,
                {Qp65, Qp66, Qp67, Qp68, Qp69,
                 Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                 Qp76, Qp77, Qp78, Qp79, queue:in(X, Qp80)});
?IN_HIGHER_Qp96(81,
                {queue:in(X, Qp81), Qp82, Qp83, Qp84, Qp85,
                 Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                 Qp92, Qp93, Qp94, Qp95, Qp96});
?IN_HIGHER_Qp96(82,
                {Qp81, queue:in(X, Qp82), Qp83, Qp84, Qp85,
                 Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                 Qp92, Qp93, Qp94, Qp95, Qp96});
?IN_HIGHER_Qp96(83,
                {Qp81, Qp82, queue:in(X, Qp83), Qp84, Qp85,
                 Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                 Qp92, Qp93, Qp94, Qp95, Qp96});
?IN_HIGHER_Qp96(84,
                {Qp81, Qp82, Qp83, queue:in(X, Qp84), Qp85,
                 Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                 Qp92, Qp93, Qp94, Qp95, Qp96});
?IN_HIGHER_Qp96(85,
                {Qp81, Qp82, Qp83, Qp84, queue:in(X, Qp85),
                 Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                 Qp92, Qp93, Qp94, Qp95, Qp96});
?IN_HIGHER_Qp96(86,
                {Qp81, Qp82, Qp83, Qp84, Qp85,
                 queue:in(X, Qp86), Qp87, Qp88, Qp89, Qp90, Qp91,
                 Qp92, Qp93, Qp94, Qp95, Qp96});
?IN_HIGHER_Qp96(87,
                {Qp81, Qp82, Qp83, Qp84, Qp85,
                 Qp86, queue:in(X, Qp87), Qp88, Qp89, Qp90, Qp91,
                 Qp92, Qp93, Qp94, Qp95, Qp96});
?IN_HIGHER_Qp96(88,
                {Qp81, Qp82, Qp83, Qp84, Qp85,
                 Qp86, Qp87, queue:in(X, Qp88), Qp89, Qp90, Qp91,
                 Qp92, Qp93, Qp94, Qp95, Qp96});
?IN_HIGHER_Qp96(89,
                {Qp81, Qp82, Qp83, Qp84, Qp85,
                 Qp86, Qp87, Qp88, queue:in(X, Qp89), Qp90, Qp91,
                 Qp92, Qp93, Qp94, Qp95, Qp96});
?IN_HIGHER_Qp96(90,
                {Qp81, Qp82, Qp83, Qp84, Qp85,
                 Qp86, Qp87, Qp88, Qp89, queue:in(X, Qp90), Qp91,
                 Qp92, Qp93, Qp94, Qp95, Qp96});
?IN_HIGHER_Qp96(91,
                {Qp81, Qp82, Qp83, Qp84, Qp85,
                 Qp86, Qp87, Qp88, Qp89, Qp90, queue:in(X, Qp91),
                 Qp92, Qp93, Qp94, Qp95, Qp96});
?IN_HIGHER_Qp96(92,
                {Qp81, Qp82, Qp83, Qp84, Qp85,
                 Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                 queue:in(X, Qp92), Qp93, Qp94, Qp95, Qp96});
?IN_HIGHER_Qp96(93,
                {Qp81, Qp82, Qp83, Qp84, Qp85,
                 Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                 Qp92, queue:in(X, Qp93), Qp94, Qp95, Qp96});
?IN_HIGHER_Qp96(94,
                {Qp81, Qp82, Qp83, Qp84, Qp85,
                 Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                 Qp92, Qp93, queue:in(X, Qp94), Qp95, Qp96});
?IN_HIGHER_Qp96(95,
                {Qp81, Qp82, Qp83, Qp84, Qp85,
                 Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                 Qp92, Qp93, Qp94, queue:in(X, Qp95), Qp96});
?IN_HIGHER_Qp96(96,
                {Qp81, Qp82, Qp83, Qp84, Qp85,
                 Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                 Qp92, Qp93, Qp94, Qp95, queue:in(X, Qp96)});
?IN_HIGHER_Qp112(97,
                 {queue:in(X, Qp97), Qp98, Qp99, Qp100, Qp101,
                  Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                  Qp108, Qp109, Qp110, Qp111, Qp112});
?IN_HIGHER_Qp112(98,
                 {Qp97, queue:in(X, Qp98), Qp99, Qp100, Qp101,
                  Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                  Qp108, Qp109, Qp110, Qp111, Qp112});
?IN_HIGHER_Qp112(99,
                 {Qp97, Qp98, queue:in(X, Qp99), Qp100, Qp101,
                  Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                  Qp108, Qp109, Qp110, Qp111, Qp112});
?IN_HIGHER_Qp112(100,
                 {Qp97, Qp98, Qp99, queue:in(X, Qp100), Qp101,
                  Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                  Qp108, Qp109, Qp110, Qp111, Qp112});
?IN_HIGHER_Qp112(101,
                 {Qp97, Qp98, Qp99, Qp100, queue:in(X, Qp101),
                  Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                  Qp108, Qp109, Qp110, Qp111, Qp112});
?IN_HIGHER_Qp112(102,
                 {Qp97, Qp98, Qp99, Qp100, Qp101,
                  queue:in(X, Qp102), Qp103, Qp104, Qp105, Qp106, Qp107,
                  Qp108, Qp109, Qp110, Qp111, Qp112});
?IN_HIGHER_Qp112(103,
                 {Qp97, Qp98, Qp99, Qp100, Qp101,
                  Qp102, queue:in(X, Qp103), Qp104, Qp105, Qp106, Qp107,
                  Qp108, Qp109, Qp110, Qp111, Qp112});
?IN_HIGHER_Qp112(104,
                 {Qp97, Qp98, Qp99, Qp100, Qp101,
                  Qp102, Qp103, queue:in(X, Qp104), Qp105, Qp106, Qp107,
                  Qp108, Qp109, Qp110, Qp111, Qp112});
?IN_HIGHER_Qp112(105,
                 {Qp97, Qp98, Qp99, Qp100, Qp101,
                  Qp102, Qp103, Qp104, queue:in(X, Qp105), Qp106, Qp107,
                  Qp108, Qp109, Qp110, Qp111, Qp112});
?IN_HIGHER_Qp112(106,
                 {Qp97, Qp98, Qp99, Qp100, Qp101,
                  Qp102, Qp103, Qp104, Qp105, queue:in(X, Qp106), Qp107,
                  Qp108, Qp109, Qp110, Qp111, Qp112});
?IN_HIGHER_Qp112(107,
                 {Qp97, Qp98, Qp99, Qp100, Qp101,
                  Qp102, Qp103, Qp104, Qp105, Qp106, queue:in(X, Qp107),
                  Qp108, Qp109, Qp110, Qp111, Qp112});
?IN_HIGHER_Qp112(108,
                 {Qp97, Qp98, Qp99, Qp100, Qp101,
                  Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                  queue:in(X, Qp108), Qp109, Qp110, Qp111, Qp112});
?IN_HIGHER_Qp112(109,
                 {Qp97, Qp98, Qp99, Qp100, Qp101,
                  Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                  Qp108, queue:in(X, Qp109), Qp110, Qp111, Qp112});
?IN_HIGHER_Qp112(110,
                 {Qp97, Qp98, Qp99, Qp100, Qp101,
                  Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                  Qp108, Qp109, queue:in(X, Qp110), Qp111, Qp112});
?IN_HIGHER_Qp112(111,
                 {Qp97, Qp98, Qp99, Qp100, Qp101,
                  Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                  Qp108, Qp109, Qp110, queue:in(X, Qp111), Qp112});
?IN_HIGHER_Qp112(112,
                 {Qp97, Qp98, Qp99, Qp100, Qp101,
                  Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                  Qp108, Qp109, Qp110, Qp111, queue:in(X, Qp112)});
?IN_HIGHER_Qp128(113,
                 {queue:in(X, Qp113), Qp114, Qp115, Qp116, Qp117,
                  Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                  Qp124, Qp125, Qp126, Qp127, Qp128});
?IN_HIGHER_Qp128(114,
                 {Qp113, queue:in(X, Qp114), Qp115, Qp116, Qp117,
                  Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                  Qp124, Qp125, Qp126, Qp127, Qp128});
?IN_HIGHER_Qp128(115,
                 {Qp113, Qp114, queue:in(X, Qp115), Qp116, Qp117,
                  Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                  Qp124, Qp125, Qp126, Qp127, Qp128});
?IN_HIGHER_Qp128(116,
                 {Qp113, Qp114, Qp115, queue:in(X, Qp116), Qp117,
                  Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                  Qp124, Qp125, Qp126, Qp127, Qp128});
?IN_HIGHER_Qp128(117,
                 {Qp113, Qp114, Qp115, Qp116, queue:in(X, Qp117),
                  Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                  Qp124, Qp125, Qp126, Qp127, Qp128});
?IN_HIGHER_Qp128(118,
                 {Qp113, Qp114, Qp115, Qp116, Qp117,
                  queue:in(X, Qp118), Qp119, Qp120, Qp121, Qp122, Qp123,
                  Qp124, Qp125, Qp126, Qp127, Qp128});
?IN_HIGHER_Qp128(119,
                 {Qp113, Qp114, Qp115, Qp116, Qp117,
                  Qp118, queue:in(X, Qp119), Qp120, Qp121, Qp122, Qp123,
                  Qp124, Qp125, Qp126, Qp127, Qp128});
?IN_HIGHER_Qp128(120,
                 {Qp113, Qp114, Qp115, Qp116, Qp117,
                  Qp118, Qp119, queue:in(X, Qp120), Qp121, Qp122, Qp123,
                  Qp124, Qp125, Qp126, Qp127, Qp128});
?IN_HIGHER_Qp128(121,
                 {Qp113, Qp114, Qp115, Qp116, Qp117,
                  Qp118, Qp119, Qp120, queue:in(X, Qp121), Qp122, Qp123,
                  Qp124, Qp125, Qp126, Qp127, Qp128});
?IN_HIGHER_Qp128(122,
                 {Qp113, Qp114, Qp115, Qp116, Qp117,
                  Qp118, Qp119, Qp120, Qp121, queue:in(X, Qp122), Qp123,
                  Qp124, Qp125, Qp126, Qp127, Qp128});
?IN_HIGHER_Qp128(123,
                 {Qp113, Qp114, Qp115, Qp116, Qp117,
                  Qp118, Qp119, Qp120, Qp121, Qp122, queue:in(X, Qp123),
                  Qp124, Qp125, Qp126, Qp127, Qp128});
?IN_HIGHER_Qp128(124,
                 {Qp113, Qp114, Qp115, Qp116, Qp117,
                  Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                  queue:in(X, Qp124), Qp125, Qp126, Qp127, Qp128});
?IN_HIGHER_Qp128(125,
                 {Qp113, Qp114, Qp115, Qp116, Qp117,
                  Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                  Qp124, queue:in(X, Qp125), Qp126, Qp127, Qp128});
?IN_HIGHER_Qp128(126,
                 {Qp113, Qp114, Qp115, Qp116, Qp117,
                  Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                  Qp124, Qp125, queue:in(X, Qp126), Qp127, Qp128});
?IN_HIGHER_Qp128(127,
                 {Qp113, Qp114, Qp115, Qp116, Qp117,
                  Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                  Qp124, Qp125, Qp126, queue:in(X, Qp127), Qp128});
?IN_HIGHER_Qp128(128,
                 {Qp113, Qp114, Qp115, Qp116, Qp117,
                  Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                  Qp124, Qp125, Qp126, Qp127, queue:in(X, Qp128)}).

-define(IN_LOWER_Qn128(P, V),
in_lower(P,
          {Pc,
           {Qn128, Qn127, Qn126, Qn125, Qn124, Qn123, Qn122, Qn121,
            Qn120, Qn119, Qn118, Qn117, Qn116, Qn115, Qn114, Qn113},
           Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
           Q0,
           Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}, X) ->
    {Pc,
     V,
     Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(IN_LOWER_Qn112(P, V),
in_lower(P,
          {Pc,
           Qn128,
           {Qn112, Qn111, Qn110, Qn109, Qn108, Qn107, Qn106, Qn105,
            Qn104, Qn103, Qn102, Qn101, Qn100, Qn99, Qn98, Qn97},
           Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
           Q0,
           Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}, X) ->
    {Pc,
     Qn128,
     V,
     Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(IN_LOWER_Qn96(P, V),
in_lower(P,
          {Pc,
           Qn128, Qn112,
           {Qn96, Qn95, Qn94, Qn93, Qn92, Qn91, Qn90, Qn89,
            Qn88, Qn87, Qn86, Qn85, Qn84, Qn83, Qn82, Qn81},
           Qn80, Qn64, Qn48, Qn32, Qn16,
           Q0,
           Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}, X) ->
    {Pc,
     Qn128, Qn112,
     V,
     Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(IN_LOWER_Qn80(P, V),
in_lower(P,
          {Pc,
           Qn128, Qn112, Qn96,
           {Qn80, Qn79, Qn78, Qn77, Qn76, Qn75, Qn74, Qn73,
            Qn72, Qn71, Qn70, Qn69, Qn68, Qn67, Qn66, Qn65},
           Qn64, Qn48, Qn32, Qn16,
           Q0,
           Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}, X) ->
    {Pc,
     Qn128, Qn112, Qn96,
     V,
     Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(IN_LOWER_Qn64(P, V),
in_lower(P,
          {Pc,
           Qn128, Qn112, Qn96, Qn80,
           {Qn64, Qn63, Qn62, Qn61, Qn60, Qn59, Qn58, Qn57,
            Qn56, Qn55, Qn54, Qn53, Qn52, Qn51, Qn50, Qn49},
           Qn48, Qn32, Qn16,
           Q0,
           Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}, X) ->
    {Pc,
     Qn128, Qn112, Qn96, Qn80,
     V,
     Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(IN_LOWER_Qn48(P, V),
in_lower(P,
          {Pc,
           Qn128, Qn112, Qn96, Qn80, Qn64,
           {Qn48, Qn47, Qn46, Qn45, Qn44, Qn43, Qn42, Qn41,
            Qn40, Qn39, Qn38, Qn37, Qn36, Qn35, Qn34, Qn33},
           Qn32, Qn16,
           Q0,
           Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}, X) ->
    {Pc,
     Qn128, Qn112, Qn96, Qn80, Qn64,
     V,
     Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(IN_LOWER_Qn32(P, V),
in_lower(P,
          {Pc,
           Qn128, Qn112, Qn96, Qn80, Qn64, Qn48,
           {Qn32, Qn31, Qn30, Qn29, Qn28, Qn27, Qn26, Qn25,
            Qn24, Qn23, Qn22, Qn21, Qn20, Qn19, Qn18, Qn17},
           Qn16,
           Q0,
           Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}, X) ->
    {Pc,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48,
     V,
     Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(IN_LOWER_Qn16(P, V),
in_lower(P,
          {Pc,
           Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32,
           {Qn16, Qn15, Qn14, Qn13, Qn12, Qn11, Qn10, Qn9,
            Qn8, Qn7, Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
           Q0,
           Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}, X) ->
    {Pc,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32,
     V,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(IN_LOWER_Qp16(P, V),
in_lower(P,
          {Pc,
           Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
           Q0,
           {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6, Qp7, Qp8,
            Qp9, Qp10, Qp11, Qp12, Qp13, Qp14, Qp15, Qp16},
           Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}, X) ->
    {Pc,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     V,
     Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(IN_LOWER_Qp32(P, V),
in_lower(P,
          {Pc,
           Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
           Q0,
           Qp16,
           {Qp17, Qp18, Qp19, Qp20, Qp21, Qp22, Qp23, Qp24,
            Qp25, Qp26, Qp27, Qp28, Qp29, Qp30, Qp31, Qp32},
           Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}, X) ->
    {Pc,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16,
     V,
     Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(IN_LOWER_Qp48(P, V),
in_lower(P,
          {Pc,
           Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
           Q0,
           Qp16, Qp32,
           {Qp33, Qp34, Qp35, Qp36, Qp37, Qp38, Qp39, Qp40,
            Qp41, Qp42, Qp43, Qp44, Qp45, Qp46, Qp47, Qp48},
           Qp64, Qp80, Qp96, Qp112, Qp128}, X) ->
    {Pc,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32,
     V,
     Qp64, Qp80, Qp96, Qp112, Qp128}).
-define(IN_LOWER_Qp64(P, V),
in_lower(P,
          {Pc,
           Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
           Q0,
           Qp16, Qp32, Qp48,
           {Qp49, Qp50, Qp51, Qp52, Qp53, Qp54, Qp55, Qp56,
            Qp57, Qp58, Qp59, Qp60, Qp61, Qp62, Qp63, Qp64},
           Qp80, Qp96, Qp112, Qp128}, X) ->
    {Pc,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48,
     V,
     Qp80, Qp96, Qp112, Qp128}).
-define(IN_LOWER_Qp80(P, V),
in_lower(P,
          {Pc,
           Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
           Q0,
           Qp16, Qp32, Qp48, Qp64,
           {Qp65, Qp66, Qp67, Qp68, Qp69, Qp70, Qp71, Qp72,
            Qp73, Qp74, Qp75, Qp76, Qp77, Qp78, Qp79, Qp80},
           Qp96, Qp112, Qp128}, X) ->
    {Pc,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64,
     V,
     Qp96, Qp112, Qp128}).
-define(IN_LOWER_Qp96(P, V),
in_lower(P,
          {Pc,
           Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
           Q0,
           Qp16, Qp32, Qp48, Qp64, Qp80,
           {Qp81, Qp82, Qp83, Qp84, Qp85, Qp86, Qp87, Qp88,
            Qp89, Qp90, Qp91, Qp92, Qp93, Qp94, Qp95, Qp96},
           Qp112, Qp128}, X) ->
    {Pc,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80,
     V,
     Qp112, Qp128}).
-define(IN_LOWER_Qp112(P, V),
in_lower(P,
          {Pc,
           Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
           Q0,
           Qp16, Qp32, Qp48, Qp64, Qp80, Qp96,
           {Qp97, Qp98, Qp99, Qp100, Qp101, Qp102, Qp103, Qp104,
            Qp105, Qp106, Qp107, Qp108, Qp109, Qp110, Qp111, Qp112},
           Qp128}, X) ->
    {Pc,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96,
     V,
     Qp128}).
-define(IN_LOWER_Qp128(P, V),
in_lower(P,
          {Pc,
           Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
           Q0,
           Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112,
           {Qp113, Qp114, Qp115, Qp116, Qp117, Qp118, Qp119, Qp120,
            Qp121, Qp122, Qp123, Qp124, Qp125, Qp126, Qp127, Qp128}}, X) ->
    {Pc,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     Q0,
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112,
     V}).

?IN_LOWER_Qn128(-128,
                {queue:in(X, Qn128), Qn127, Qn126, Qn125, Qn124,
                 Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                 Qn117, Qn116, Qn115, Qn114, Qn113});
?IN_LOWER_Qn128(-127,
                {Qn128, queue:in(X, Qn127), Qn126, Qn125, Qn124,
                 Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                 Qn117, Qn116, Qn115, Qn114, Qn113});
?IN_LOWER_Qn128(-126,
                {Qn128, Qn127, queue:in(X, Qn126), Qn125, Qn124,
                 Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                 Qn117, Qn116, Qn115, Qn114, Qn113});
?IN_LOWER_Qn128(-125,
                {Qn128, Qn127, Qn126, queue:in(X, Qn125), Qn124,
                 Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                 Qn117, Qn116, Qn115, Qn114, Qn113});
?IN_LOWER_Qn128(-124,
                {Qn128, Qn127, Qn126, Qn125, queue:in(X, Qn124),
                 Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                 Qn117, Qn116, Qn115, Qn114, Qn113});
?IN_LOWER_Qn128(-123,
                {Qn128, Qn127, Qn126, Qn125, Qn124,
                 queue:in(X, Qn123), Qn122, Qn121, Qn120, Qn119, Qn118,
                 Qn117, Qn116, Qn115, Qn114, Qn113});
?IN_LOWER_Qn128(-122,
                {Qn128, Qn127, Qn126, Qn125, Qn124,
                 Qn123, queue:in(X, Qn122), Qn121, Qn120, Qn119, Qn118,
                 Qn117, Qn116, Qn115, Qn114, Qn113});
?IN_LOWER_Qn128(-121,
                {Qn128, Qn127, Qn126, Qn125, Qn124,
                 Qn123, Qn122, queue:in(X, Qn121), Qn120, Qn119, Qn118,
                 Qn117, Qn116, Qn115, Qn114, Qn113});
?IN_LOWER_Qn128(-120,
                {Qn128, Qn127, Qn126, Qn125, Qn124,
                 Qn123, Qn122, Qn121, queue:in(X, Qn120), Qn119, Qn118,
                 Qn117, Qn116, Qn115, Qn114, Qn113});
?IN_LOWER_Qn128(-119,
                {Qn128, Qn127, Qn126, Qn125, Qn124,
                 Qn123, Qn122, Qn121, Qn120, queue:in(X, Qn119), Qn118,
                 Qn117, Qn116, Qn115, Qn114, Qn113});
?IN_LOWER_Qn128(-118,
                {Qn128, Qn127, Qn126, Qn125, Qn124,
                 Qn123, Qn122, Qn121, Qn120, Qn119, queue:in(X, Qn118),
                 Qn117, Qn116, Qn115, Qn114, Qn113});
?IN_LOWER_Qn128(-117,
                {Qn128, Qn127, Qn126, Qn125, Qn124,
                 Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                 queue:in(X, Qn117), Qn116, Qn115, Qn114, Qn113});
?IN_LOWER_Qn128(-116,
                {Qn128, Qn127, Qn126, Qn125, Qn124,
                 Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                 Qn117, queue:in(X, Qn116), Qn115, Qn114, Qn113});
?IN_LOWER_Qn128(-115,
                {Qn128, Qn127, Qn126, Qn125, Qn124,
                 Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                 Qn117, Qn116, queue:in(X, Qn115), Qn114, Qn113});
?IN_LOWER_Qn128(-114,
                {Qn128, Qn127, Qn126, Qn125, Qn124,
                 Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                 Qn117, Qn116, Qn115, queue:in(X, Qn114), Qn113});
?IN_LOWER_Qn128(-113,
                {Qn128, Qn127, Qn126, Qn125, Qn124,
                 Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                 Qn117, Qn116, Qn115, Qn114, queue:in(X, Qn113)});
?IN_LOWER_Qn112(-112,
                {queue:in(X, Qn112), Qn111, Qn110, Qn109, Qn108,
                 Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                 Qn101, Qn100, Qn99, Qn98, Qn97});
?IN_LOWER_Qn112(-111,
                {Qn112, queue:in(X, Qn111), Qn110, Qn109, Qn108,
                 Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                 Qn101, Qn100, Qn99, Qn98, Qn97});
?IN_LOWER_Qn112(-110,
                {Qn112, Qn111, queue:in(X, Qn110), Qn109, Qn108,
                 Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                 Qn101, Qn100, Qn99, Qn98, Qn97});
?IN_LOWER_Qn112(-109,
                {Qn112, Qn111, Qn110, queue:in(X, Qn109), Qn108,
                 Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                 Qn101, Qn100, Qn99, Qn98, Qn97});
?IN_LOWER_Qn112(-108,
                {Qn112, Qn111, Qn110, Qn109, queue:in(X, Qn108),
                 Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                 Qn101, Qn100, Qn99, Qn98, Qn97});
?IN_LOWER_Qn112(-107,
                {Qn112, Qn111, Qn110, Qn109, Qn108,
                 queue:in(X, Qn107), Qn106, Qn105, Qn104, Qn103, Qn102,
                 Qn101, Qn100, Qn99, Qn98, Qn97});
?IN_LOWER_Qn112(-106,
                {Qn112, Qn111, Qn110, Qn109, Qn108,
                 Qn107, queue:in(X, Qn106), Qn105, Qn104, Qn103, Qn102,
                 Qn101, Qn100, Qn99, Qn98, Qn97});
?IN_LOWER_Qn112(-105,
                {Qn112, Qn111, Qn110, Qn109, Qn108,
                 Qn107, Qn106, queue:in(X, Qn105), Qn104, Qn103, Qn102,
                 Qn101, Qn100, Qn99, Qn98, Qn97});
?IN_LOWER_Qn112(-104,
                {Qn112, Qn111, Qn110, Qn109, Qn108,
                 Qn107, Qn106, Qn105, queue:in(X, Qn104), Qn103, Qn102,
                 Qn101, Qn100, Qn99, Qn98, Qn97});
?IN_LOWER_Qn112(-103,
                {Qn112, Qn111, Qn110, Qn109, Qn108,
                 Qn107, Qn106, Qn105, Qn104, queue:in(X, Qn103), Qn102,
                 Qn101, Qn100, Qn99, Qn98, Qn97});
?IN_LOWER_Qn112(-102,
                {Qn112, Qn111, Qn110, Qn109, Qn108,
                 Qn107, Qn106, Qn105, Qn104, Qn103, queue:in(X, Qn102),
                 Qn101, Qn100, Qn99, Qn98, Qn97});
?IN_LOWER_Qn112(-101,
                {Qn112, Qn111, Qn110, Qn109, Qn108,
                 Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                 queue:in(X, Qn101), Qn100, Qn99, Qn98, Qn97});
?IN_LOWER_Qn112(-100,
                {Qn112, Qn111, Qn110, Qn109, Qn108,
                 Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                 Qn101, queue:in(X, Qn100), Qn99, Qn98, Qn97});
?IN_LOWER_Qn112(-99,
                {Qn112, Qn111, Qn110, Qn109, Qn108,
                 Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                 Qn101, Qn100, queue:in(X, Qn99), Qn98, Qn97});
?IN_LOWER_Qn112(-98,
                {Qn112, Qn111, Qn110, Qn109, Qn108,
                 Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                 Qn101, Qn100, Qn99, queue:in(X, Qn98), Qn97});
?IN_LOWER_Qn112(-97,
                {Qn112, Qn111, Qn110, Qn109, Qn108,
                 Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                 Qn101, Qn100, Qn99, Qn98, queue:in(X, Qn97)});
?IN_LOWER_Qn96(-96,
               {queue:in(X, Qn96), Qn95, Qn94, Qn93, Qn92,
                Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                Qn85, Qn84, Qn83, Qn82, Qn81});
?IN_LOWER_Qn96(-95,
               {Qn96, queue:in(X, Qn95), Qn94, Qn93, Qn92,
                Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                Qn85, Qn84, Qn83, Qn82, Qn81});
?IN_LOWER_Qn96(-94,
               {Qn96, Qn95, queue:in(X, Qn94), Qn93, Qn92,
                Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                Qn85, Qn84, Qn83, Qn82, Qn81});
?IN_LOWER_Qn96(-93,
               {Qn96, Qn95, Qn94, queue:in(X, Qn93), Qn92,
                Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                Qn85, Qn84, Qn83, Qn82, Qn81});
?IN_LOWER_Qn96(-92,
               {Qn96, Qn95, Qn94, Qn93, queue:in(X, Qn92),
                Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                Qn85, Qn84, Qn83, Qn82, Qn81});
?IN_LOWER_Qn96(-91,
               {Qn96, Qn95, Qn94, Qn93, Qn92,
                queue:in(X, Qn91), Qn90, Qn89, Qn88, Qn87, Qn86,
                Qn85, Qn84, Qn83, Qn82, Qn81});
?IN_LOWER_Qn96(-90,
               {Qn96, Qn95, Qn94, Qn93, Qn92,
                Qn91, queue:in(X, Qn90), Qn89, Qn88, Qn87, Qn86,
                Qn85, Qn84, Qn83, Qn82, Qn81});
?IN_LOWER_Qn96(-89,
               {Qn96, Qn95, Qn94, Qn93, Qn92,
                Qn91, Qn90, queue:in(X, Qn89), Qn88, Qn87, Qn86,
                Qn85, Qn84, Qn83, Qn82, Qn81});
?IN_LOWER_Qn96(-88,
               {Qn96, Qn95, Qn94, Qn93, Qn92,
                Qn91, Qn90, Qn89, queue:in(X, Qn88), Qn87, Qn86,
                Qn85, Qn84, Qn83, Qn82, Qn81});
?IN_LOWER_Qn96(-87,
               {Qn96, Qn95, Qn94, Qn93, Qn92,
                Qn91, Qn90, Qn89, Qn88, queue:in(X, Qn87), Qn86,
                Qn85, Qn84, Qn83, Qn82, Qn81});
?IN_LOWER_Qn96(-86,
               {Qn96, Qn95, Qn94, Qn93, Qn92,
                Qn91, Qn90, Qn89, Qn88, Qn87, queue:in(X, Qn86),
                Qn85, Qn84, Qn83, Qn82, Qn81});
?IN_LOWER_Qn96(-85,
               {Qn96, Qn95, Qn94, Qn93, Qn92,
                Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                queue:in(X, Qn85), Qn84, Qn83, Qn82, Qn81});
?IN_LOWER_Qn96(-84,
               {Qn96, Qn95, Qn94, Qn93, Qn92,
                Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                Qn85, queue:in(X, Qn84), Qn83, Qn82, Qn81});
?IN_LOWER_Qn96(-83,
               {Qn96, Qn95, Qn94, Qn93, Qn92,
                Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                Qn85, Qn84, queue:in(X, Qn83), Qn82, Qn81});
?IN_LOWER_Qn96(-82,
               {Qn96, Qn95, Qn94, Qn93, Qn92,
                Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                Qn85, Qn84, Qn83, queue:in(X, Qn82), Qn81});
?IN_LOWER_Qn96(-81,
               {Qn96, Qn95, Qn94, Qn93, Qn92,
                Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                Qn85, Qn84, Qn83, Qn82, queue:in(X, Qn81)});
?IN_LOWER_Qn80(-80,
               {queue:in(X, Qn80), Qn79, Qn78, Qn77, Qn76,
                Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                Qn69, Qn68, Qn67, Qn66, Qn65});
?IN_LOWER_Qn80(-79,
               {Qn80, queue:in(X, Qn79), Qn78, Qn77, Qn76,
                Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                Qn69, Qn68, Qn67, Qn66, Qn65});
?IN_LOWER_Qn80(-78,
               {Qn80, Qn79, queue:in(X, Qn78), Qn77, Qn76,
                Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                Qn69, Qn68, Qn67, Qn66, Qn65});
?IN_LOWER_Qn80(-77,
               {Qn80, Qn79, Qn78, queue:in(X, Qn77), Qn76,
                Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                Qn69, Qn68, Qn67, Qn66, Qn65});
?IN_LOWER_Qn80(-76,
               {Qn80, Qn79, Qn78, Qn77, queue:in(X, Qn76),
                Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                Qn69, Qn68, Qn67, Qn66, Qn65});
?IN_LOWER_Qn80(-75,
               {Qn80, Qn79, Qn78, Qn77, Qn76,
                queue:in(X, Qn75), Qn74, Qn73, Qn72, Qn71, Qn70,
                Qn69, Qn68, Qn67, Qn66, Qn65});
?IN_LOWER_Qn80(-74,
               {Qn80, Qn79, Qn78, Qn77, Qn76,
                Qn75, queue:in(X, Qn74), Qn73, Qn72, Qn71, Qn70,
                Qn69, Qn68, Qn67, Qn66, Qn65});
?IN_LOWER_Qn80(-73,
               {Qn80, Qn79, Qn78, Qn77, Qn76,
                Qn75, Qn74, queue:in(X, Qn73), Qn72, Qn71, Qn70,
                Qn69, Qn68, Qn67, Qn66, Qn65});
?IN_LOWER_Qn80(-72,
               {Qn80, Qn79, Qn78, Qn77, Qn76,
                Qn75, Qn74, Qn73, queue:in(X, Qn72), Qn71, Qn70,
                Qn69, Qn68, Qn67, Qn66, Qn65});
?IN_LOWER_Qn80(-71,
               {Qn80, Qn79, Qn78, Qn77, Qn76,
                Qn75, Qn74, Qn73, Qn72, queue:in(X, Qn71), Qn70,
                Qn69, Qn68, Qn67, Qn66, Qn65});
?IN_LOWER_Qn80(-70,
               {Qn80, Qn79, Qn78, Qn77, Qn76,
                Qn75, Qn74, Qn73, Qn72, Qn71, queue:in(X, Qn70),
                Qn69, Qn68, Qn67, Qn66, Qn65});
?IN_LOWER_Qn80(-69,
               {Qn80, Qn79, Qn78, Qn77, Qn76,
                Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                queue:in(X, Qn69), Qn68, Qn67, Qn66, Qn65});
?IN_LOWER_Qn80(-68,
               {Qn80, Qn79, Qn78, Qn77, Qn76,
                Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                Qn69, queue:in(X, Qn68), Qn67, Qn66, Qn65});
?IN_LOWER_Qn80(-67,
               {Qn80, Qn79, Qn78, Qn77, Qn76,
                Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                Qn69, Qn68, queue:in(X, Qn67), Qn66, Qn65});
?IN_LOWER_Qn80(-66,
               {Qn80, Qn79, Qn78, Qn77, Qn76,
                Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                Qn69, Qn68, Qn67, queue:in(X, Qn66), Qn65});
?IN_LOWER_Qn80(-65,
               {Qn80, Qn79, Qn78, Qn77, Qn76,
                Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                Qn69, Qn68, Qn67, Qn66, queue:in(X, Qn65)});
?IN_LOWER_Qn64(-64,
               {queue:in(X, Qn64), Qn63, Qn62, Qn61, Qn60,
                Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                Qn53, Qn52, Qn51, Qn50, Qn49});
?IN_LOWER_Qn64(-63,
               {Qn64, queue:in(X, Qn63), Qn62, Qn61, Qn60,
                Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                Qn53, Qn52, Qn51, Qn50, Qn49});
?IN_LOWER_Qn64(-62,
               {Qn64, Qn63, queue:in(X, Qn62), Qn61, Qn60,
                Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                Qn53, Qn52, Qn51, Qn50, Qn49});
?IN_LOWER_Qn64(-61,
               {Qn64, Qn63, Qn62, queue:in(X, Qn61), Qn60,
                Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                Qn53, Qn52, Qn51, Qn50, Qn49});
?IN_LOWER_Qn64(-60,
               {Qn64, Qn63, Qn62, Qn61, queue:in(X, Qn60),
                Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                Qn53, Qn52, Qn51, Qn50, Qn49});
?IN_LOWER_Qn64(-59,
               {Qn64, Qn63, Qn62, Qn61, Qn60,
                queue:in(X, Qn59), Qn58, Qn57, Qn56, Qn55, Qn54,
                Qn53, Qn52, Qn51, Qn50, Qn49});
?IN_LOWER_Qn64(-58,
               {Qn64, Qn63, Qn62, Qn61, Qn60,
                Qn59, queue:in(X, Qn58), Qn57, Qn56, Qn55, Qn54,
                Qn53, Qn52, Qn51, Qn50, Qn49});
?IN_LOWER_Qn64(-57,
               {Qn64, Qn63, Qn62, Qn61, Qn60,
                Qn59, Qn58, queue:in(X, Qn57), Qn56, Qn55, Qn54,
                Qn53, Qn52, Qn51, Qn50, Qn49});
?IN_LOWER_Qn64(-56,
               {Qn64, Qn63, Qn62, Qn61, Qn60,
                Qn59, Qn58, Qn57, queue:in(X, Qn56), Qn55, Qn54,
                Qn53, Qn52, Qn51, Qn50, Qn49});
?IN_LOWER_Qn64(-55,
               {Qn64, Qn63, Qn62, Qn61, Qn60,
                Qn59, Qn58, Qn57, Qn56, queue:in(X, Qn55), Qn54,
                Qn53, Qn52, Qn51, Qn50, Qn49});
?IN_LOWER_Qn64(-54,
               {Qn64, Qn63, Qn62, Qn61, Qn60,
                Qn59, Qn58, Qn57, Qn56, Qn55, queue:in(X, Qn54),
                Qn53, Qn52, Qn51, Qn50, Qn49});
?IN_LOWER_Qn64(-53,
               {Qn64, Qn63, Qn62, Qn61, Qn60,
                Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                queue:in(X, Qn53), Qn52, Qn51, Qn50, Qn49});
?IN_LOWER_Qn64(-52,
               {Qn64, Qn63, Qn62, Qn61, Qn60,
                Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                Qn53, queue:in(X, Qn52), Qn51, Qn50, Qn49});
?IN_LOWER_Qn64(-51,
               {Qn64, Qn63, Qn62, Qn61, Qn60,
                Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                Qn53, Qn52, queue:in(X, Qn51), Qn50, Qn49});
?IN_LOWER_Qn64(-50,
               {Qn64, Qn63, Qn62, Qn61, Qn60,
                Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                Qn53, Qn52, Qn51, queue:in(X, Qn50), Qn49});
?IN_LOWER_Qn64(-49,
               {Qn64, Qn63, Qn62, Qn61, Qn60,
                Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                Qn53, Qn52, Qn51, Qn50, queue:in(X, Qn49)});
?IN_LOWER_Qn48(-48,
               {queue:in(X, Qn48), Qn47, Qn46, Qn45, Qn44,
                Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                Qn37, Qn36, Qn35, Qn34, Qn33});
?IN_LOWER_Qn48(-47,
               {Qn48, queue:in(X, Qn47), Qn46, Qn45, Qn44,
                Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                Qn37, Qn36, Qn35, Qn34, Qn33});
?IN_LOWER_Qn48(-46,
               {Qn48, Qn47, queue:in(X, Qn46), Qn45, Qn44,
                Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                Qn37, Qn36, Qn35, Qn34, Qn33});
?IN_LOWER_Qn48(-45,
               {Qn48, Qn47, Qn46, queue:in(X, Qn45), Qn44,
                Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                Qn37, Qn36, Qn35, Qn34, Qn33});
?IN_LOWER_Qn48(-44,
               {Qn48, Qn47, Qn46, Qn45, queue:in(X, Qn44),
                Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                Qn37, Qn36, Qn35, Qn34, Qn33});
?IN_LOWER_Qn48(-43,
               {Qn48, Qn47, Qn46, Qn45, Qn44,
                queue:in(X, Qn43), Qn42, Qn41, Qn40, Qn39, Qn38,
                Qn37, Qn36, Qn35, Qn34, Qn33});
?IN_LOWER_Qn48(-42,
               {Qn48, Qn47, Qn46, Qn45, Qn44,
                Qn43, queue:in(X, Qn42), Qn41, Qn40, Qn39, Qn38,
                Qn37, Qn36, Qn35, Qn34, Qn33});
?IN_LOWER_Qn48(-41,
               {Qn48, Qn47, Qn46, Qn45, Qn44,
                Qn43, Qn42, queue:in(X, Qn41), Qn40, Qn39, Qn38,
                Qn37, Qn36, Qn35, Qn34, Qn33});
?IN_LOWER_Qn48(-40,
               {Qn48, Qn47, Qn46, Qn45, Qn44,
                Qn43, Qn42, Qn41, queue:in(X, Qn40), Qn39, Qn38,
                Qn37, Qn36, Qn35, Qn34, Qn33});
?IN_LOWER_Qn48(-39,
               {Qn48, Qn47, Qn46, Qn45, Qn44,
                Qn43, Qn42, Qn41, Qn40, queue:in(X, Qn39), Qn38,
                Qn37, Qn36, Qn35, Qn34, Qn33});
?IN_LOWER_Qn48(-38,
               {Qn48, Qn47, Qn46, Qn45, Qn44,
                Qn43, Qn42, Qn41, Qn40, Qn39, queue:in(X, Qn38),
                Qn37, Qn36, Qn35, Qn34, Qn33});
?IN_LOWER_Qn48(-37,
               {Qn48, Qn47, Qn46, Qn45, Qn44,
                Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                queue:in(X, Qn37), Qn36, Qn35, Qn34, Qn33});
?IN_LOWER_Qn48(-36,
               {Qn48, Qn47, Qn46, Qn45, Qn44,
                Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                Qn37, queue:in(X, Qn36), Qn35, Qn34, Qn33});
?IN_LOWER_Qn48(-35,
               {Qn48, Qn47, Qn46, Qn45, Qn44,
                Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                Qn37, Qn36, queue:in(X, Qn35), Qn34, Qn33});
?IN_LOWER_Qn48(-34,
               {Qn48, Qn47, Qn46, Qn45, Qn44,
                Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                Qn37, Qn36, Qn35, queue:in(X, Qn34), Qn33});
?IN_LOWER_Qn48(-33,
               {Qn48, Qn47, Qn46, Qn45, Qn44,
                Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                Qn37, Qn36, Qn35, Qn34, queue:in(X, Qn33)});
?IN_LOWER_Qn32(-32,
               {queue:in(X, Qn32), Qn31, Qn30, Qn29, Qn28,
                Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                Qn21, Qn20, Qn19, Qn18, Qn17});
?IN_LOWER_Qn32(-31,
               {Qn32, queue:in(X, Qn31), Qn30, Qn29, Qn28,
                Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                Qn21, Qn20, Qn19, Qn18, Qn17});
?IN_LOWER_Qn32(-30,
               {Qn32, Qn31, queue:in(X, Qn30), Qn29, Qn28,
                Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                Qn21, Qn20, Qn19, Qn18, Qn17});
?IN_LOWER_Qn32(-29,
               {Qn32, Qn31, Qn30, queue:in(X, Qn29), Qn28,
                Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                Qn21, Qn20, Qn19, Qn18, Qn17});
?IN_LOWER_Qn32(-28,
               {Qn32, Qn31, Qn30, Qn29, queue:in(X, Qn28),
                Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                Qn21, Qn20, Qn19, Qn18, Qn17});
?IN_LOWER_Qn32(-27,
               {Qn32, Qn31, Qn30, Qn29, Qn28,
                queue:in(X, Qn27), Qn26, Qn25, Qn24, Qn23, Qn22,
                Qn21, Qn20, Qn19, Qn18, Qn17});
?IN_LOWER_Qn32(-26,
               {Qn32, Qn31, Qn30, Qn29, Qn28,
                Qn27, queue:in(X, Qn26), Qn25, Qn24, Qn23, Qn22,
                Qn21, Qn20, Qn19, Qn18, Qn17});
?IN_LOWER_Qn32(-25,
               {Qn32, Qn31, Qn30, Qn29, Qn28,
                Qn27, Qn26, queue:in(X, Qn25), Qn24, Qn23, Qn22,
                Qn21, Qn20, Qn19, Qn18, Qn17});
?IN_LOWER_Qn32(-24,
               {Qn32, Qn31, Qn30, Qn29, Qn28,
                Qn27, Qn26, Qn25, queue:in(X, Qn24), Qn23, Qn22,
                Qn21, Qn20, Qn19, Qn18, Qn17});
?IN_LOWER_Qn32(-23,
               {Qn32, Qn31, Qn30, Qn29, Qn28,
                Qn27, Qn26, Qn25, Qn24, queue:in(X, Qn23), Qn22,
                Qn21, Qn20, Qn19, Qn18, Qn17});
?IN_LOWER_Qn32(-22,
               {Qn32, Qn31, Qn30, Qn29, Qn28,
                Qn27, Qn26, Qn25, Qn24, Qn23, queue:in(X, Qn22),
                Qn21, Qn20, Qn19, Qn18, Qn17});
?IN_LOWER_Qn32(-21,
               {Qn32, Qn31, Qn30, Qn29, Qn28,
                Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                queue:in(X, Qn21), Qn20, Qn19, Qn18, Qn17});
?IN_LOWER_Qn32(-20,
               {Qn32, Qn31, Qn30, Qn29, Qn28,
                Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                Qn21, queue:in(X, Qn20), Qn19, Qn18, Qn17});
?IN_LOWER_Qn32(-19,
               {Qn32, Qn31, Qn30, Qn29, Qn28,
                Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                Qn21, Qn20, queue:in(X, Qn19), Qn18, Qn17});
?IN_LOWER_Qn32(-18,
               {Qn32, Qn31, Qn30, Qn29, Qn28,
                Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                Qn21, Qn20, Qn19, queue:in(X, Qn18), Qn17});
?IN_LOWER_Qn32(-17,
               {Qn32, Qn31, Qn30, Qn29, Qn28,
                Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                Qn21, Qn20, Qn19, Qn18, queue:in(X, Qn17)});
?IN_LOWER_Qn16(-16,
               {queue:in(X, Qn16), Qn15, Qn14, Qn13, Qn12,
                Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                Qn5, Qn4, Qn3, Qn2, Qn1});
?IN_LOWER_Qn16(-15,
               {Qn16, queue:in(X, Qn15), Qn14, Qn13, Qn12,
                Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                Qn5, Qn4, Qn3, Qn2, Qn1});
?IN_LOWER_Qn16(-14,
               {Qn16, Qn15, queue:in(X, Qn14), Qn13, Qn12,
                Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                Qn5, Qn4, Qn3, Qn2, Qn1});
?IN_LOWER_Qn16(-13,
               {Qn16, Qn15, Qn14, queue:in(X, Qn13), Qn12,
                Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                Qn5, Qn4, Qn3, Qn2, Qn1});
?IN_LOWER_Qn16(-12,
               {Qn16, Qn15, Qn14, Qn13, queue:in(X, Qn12),
                Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                Qn5, Qn4, Qn3, Qn2, Qn1});
?IN_LOWER_Qn16(-11,
               {Qn16, Qn15, Qn14, Qn13, Qn12,
                queue:in(X, Qn11), Qn10, Qn9, Qn8, Qn7, Qn6,
                Qn5, Qn4, Qn3, Qn2, Qn1});
?IN_LOWER_Qn16(-10,
               {Qn16, Qn15, Qn14, Qn13, Qn12,
                Qn11, queue:in(X, Qn10), Qn9, Qn8, Qn7, Qn6,
                Qn5, Qn4, Qn3, Qn2, Qn1});
?IN_LOWER_Qn16(-9,
               {Qn16, Qn15, Qn14, Qn13, Qn12,
                Qn11, Qn10, queue:in(X, Qn9), Qn8, Qn7, Qn6,
                Qn5, Qn4, Qn3, Qn2, Qn1});
?IN_LOWER_Qn16(-8,
               {Qn16, Qn15, Qn14, Qn13, Qn12,
                Qn11, Qn10, Qn9, queue:in(X, Qn8), Qn7, Qn6,
                Qn5, Qn4, Qn3, Qn2, Qn1});
?IN_LOWER_Qn16(-7,
               {Qn16, Qn15, Qn14, Qn13, Qn12,
                Qn11, Qn10, Qn9, Qn8, queue:in(X, Qn7), Qn6,
                Qn5, Qn4, Qn3, Qn2, Qn1});
?IN_LOWER_Qn16(-6,
               {Qn16, Qn15, Qn14, Qn13, Qn12,
                Qn11, Qn10, Qn9, Qn8, Qn7, queue:in(X, Qn6),
                Qn5, Qn4, Qn3, Qn2, Qn1});
?IN_LOWER_Qn16(-5,
               {Qn16, Qn15, Qn14, Qn13, Qn12,
                Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                queue:in(X, Qn5), Qn4, Qn3, Qn2, Qn1});
?IN_LOWER_Qn16(-4,
               {Qn16, Qn15, Qn14, Qn13, Qn12,
                Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                Qn5, queue:in(X, Qn4), Qn3, Qn2, Qn1});
?IN_LOWER_Qn16(-3,
               {Qn16, Qn15, Qn14, Qn13, Qn12,
                Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                Qn5, Qn4, queue:in(X, Qn3), Qn2, Qn1});
?IN_LOWER_Qn16(-2,
               {Qn16, Qn15, Qn14, Qn13, Qn12,
                Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                Qn5, Qn4, Qn3, queue:in(X, Qn2), Qn1});
?IN_LOWER_Qn16(-1,
               {Qn16, Qn15, Qn14, Qn13, Qn12,
                Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                Qn5, Qn4, Qn3, Qn2, queue:in(X, Qn1)});
in_lower(0,
         {Pc,
          Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
          Q0,
          Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}, X) ->
    {Pc,
     Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
     queue:in(X, Q0),
     Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128};
?IN_LOWER_Qp16(1,
               {queue:in(X, Qp1), Qp2, Qp3, Qp4, Qp5,
                Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                Qp12, Qp13, Qp14, Qp15, Qp16});
?IN_LOWER_Qp16(2,
               {Qp1, queue:in(X, Qp2), Qp3, Qp4, Qp5,
                Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                Qp12, Qp13, Qp14, Qp15, Qp16});
?IN_LOWER_Qp16(3,
               {Qp1, Qp2, queue:in(X, Qp3), Qp4, Qp5,
                Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                Qp12, Qp13, Qp14, Qp15, Qp16});
?IN_LOWER_Qp16(4,
               {Qp1, Qp2, Qp3, queue:in(X, Qp4), Qp5,
                Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                Qp12, Qp13, Qp14, Qp15, Qp16});
?IN_LOWER_Qp16(5,
               {Qp1, Qp2, Qp3, Qp4, queue:in(X, Qp5),
                Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                Qp12, Qp13, Qp14, Qp15, Qp16});
?IN_LOWER_Qp16(6,
               {Qp1, Qp2, Qp3, Qp4, Qp5,
                queue:in(X, Qp6), Qp7, Qp8, Qp9, Qp10, Qp11,
                Qp12, Qp13, Qp14, Qp15, Qp16});
?IN_LOWER_Qp16(7,
               {Qp1, Qp2, Qp3, Qp4, Qp5,
                Qp6, queue:in(X, Qp7), Qp8, Qp9, Qp10, Qp11,
                Qp12, Qp13, Qp14, Qp15, Qp16});
?IN_LOWER_Qp16(8,
               {Qp1, Qp2, Qp3, Qp4, Qp5,
                Qp6, Qp7, queue:in(X, Qp8), Qp9, Qp10, Qp11,
                Qp12, Qp13, Qp14, Qp15, Qp16});
?IN_LOWER_Qp16(9,
               {Qp1, Qp2, Qp3, Qp4, Qp5,
                Qp6, Qp7, Qp8, queue:in(X, Qp9), Qp10, Qp11,
                Qp12, Qp13, Qp14, Qp15, Qp16});
?IN_LOWER_Qp16(10,
               {Qp1, Qp2, Qp3, Qp4, Qp5,
                Qp6, Qp7, Qp8, Qp9, queue:in(X, Qp10), Qp11,
                Qp12, Qp13, Qp14, Qp15, Qp16});
?IN_LOWER_Qp16(11,
               {Qp1, Qp2, Qp3, Qp4, Qp5,
                Qp6, Qp7, Qp8, Qp9, Qp10, queue:in(X, Qp11),
                Qp12, Qp13, Qp14, Qp15, Qp16});
?IN_LOWER_Qp16(12,
               {Qp1, Qp2, Qp3, Qp4, Qp5,
                Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                queue:in(X, Qp12), Qp13, Qp14, Qp15, Qp16});
?IN_LOWER_Qp16(13,
               {Qp1, Qp2, Qp3, Qp4, Qp5,
                Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                Qp12, queue:in(X, Qp13), Qp14, Qp15, Qp16});
?IN_LOWER_Qp16(14,
               {Qp1, Qp2, Qp3, Qp4, Qp5,
                Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                Qp12, Qp13, queue:in(X, Qp14), Qp15, Qp16});
?IN_LOWER_Qp16(15,
               {Qp1, Qp2, Qp3, Qp4, Qp5,
                Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                Qp12, Qp13, Qp14, queue:in(X, Qp15), Qp16});
?IN_LOWER_Qp16(16,
               {Qp1, Qp2, Qp3, Qp4, Qp5,
                Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                Qp12, Qp13, Qp14, Qp15, queue:in(X, Qp16)});
?IN_LOWER_Qp32(17,
               {queue:in(X, Qp17), Qp18, Qp19, Qp20, Qp21,
                Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                Qp28, Qp29, Qp30, Qp31, Qp32});
?IN_LOWER_Qp32(18,
               {Qp17, queue:in(X, Qp18), Qp19, Qp20, Qp21,
                Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                Qp28, Qp29, Qp30, Qp31, Qp32});
?IN_LOWER_Qp32(19,
               {Qp17, Qp18, queue:in(X, Qp19), Qp20, Qp21,
                Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                Qp28, Qp29, Qp30, Qp31, Qp32});
?IN_LOWER_Qp32(20,
               {Qp17, Qp18, Qp19, queue:in(X, Qp20), Qp21,
                Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                Qp28, Qp29, Qp30, Qp31, Qp32});
?IN_LOWER_Qp32(21,
               {Qp17, Qp18, Qp19, Qp20, queue:in(X, Qp21),
                Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                Qp28, Qp29, Qp30, Qp31, Qp32});
?IN_LOWER_Qp32(22,
               {Qp17, Qp18, Qp19, Qp20, Qp21,
                queue:in(X, Qp22), Qp23, Qp24, Qp25, Qp26, Qp27,
                Qp28, Qp29, Qp30, Qp31, Qp32});
?IN_LOWER_Qp32(23,
               {Qp17, Qp18, Qp19, Qp20, Qp21,
                Qp22, queue:in(X, Qp23), Qp24, Qp25, Qp26, Qp27,
                Qp28, Qp29, Qp30, Qp31, Qp32});
?IN_LOWER_Qp32(24,
               {Qp17, Qp18, Qp19, Qp20, Qp21,
                Qp22, Qp23, queue:in(X, Qp24), Qp25, Qp26, Qp27,
                Qp28, Qp29, Qp30, Qp31, Qp32});
?IN_LOWER_Qp32(25,
               {Qp17, Qp18, Qp19, Qp20, Qp21,
                Qp22, Qp23, Qp24, queue:in(X, Qp25), Qp26, Qp27,
                Qp28, Qp29, Qp30, Qp31, Qp32});
?IN_LOWER_Qp32(26,
               {Qp17, Qp18, Qp19, Qp20, Qp21,
                Qp22, Qp23, Qp24, Qp25, queue:in(X, Qp26), Qp27,
                Qp28, Qp29, Qp30, Qp31, Qp32});
?IN_LOWER_Qp32(27,
               {Qp17, Qp18, Qp19, Qp20, Qp21,
                Qp22, Qp23, Qp24, Qp25, Qp26, queue:in(X, Qp27),
                Qp28, Qp29, Qp30, Qp31, Qp32});
?IN_LOWER_Qp32(28,
               {Qp17, Qp18, Qp19, Qp20, Qp21,
                Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                queue:in(X, Qp28), Qp29, Qp30, Qp31, Qp32});
?IN_LOWER_Qp32(29,
               {Qp17, Qp18, Qp19, Qp20, Qp21,
                Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                Qp28, queue:in(X, Qp29), Qp30, Qp31, Qp32});
?IN_LOWER_Qp32(30,
               {Qp17, Qp18, Qp19, Qp20, Qp21,
                Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                Qp28, Qp29, queue:in(X, Qp30), Qp31, Qp32});
?IN_LOWER_Qp32(31,
               {Qp17, Qp18, Qp19, Qp20, Qp21,
                Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                Qp28, Qp29, Qp30, queue:in(X, Qp31), Qp32});
?IN_LOWER_Qp32(32,
               {Qp17, Qp18, Qp19, Qp20, Qp21,
                Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                Qp28, Qp29, Qp30, Qp31, queue:in(X, Qp32)});
?IN_LOWER_Qp48(33,
               {queue:in(X, Qp33), Qp34, Qp35, Qp36, Qp37,
                Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                Qp44, Qp45, Qp46, Qp47, Qp48});
?IN_LOWER_Qp48(34,
               {Qp33, queue:in(X, Qp34), Qp35, Qp36, Qp37,
                Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                Qp44, Qp45, Qp46, Qp47, Qp48});
?IN_LOWER_Qp48(35,
               {Qp33, Qp34, queue:in(X, Qp35), Qp36, Qp37,
                Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                Qp44, Qp45, Qp46, Qp47, Qp48});
?IN_LOWER_Qp48(36,
               {Qp33, Qp34, Qp35, queue:in(X, Qp36), Qp37,
                Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                Qp44, Qp45, Qp46, Qp47, Qp48});
?IN_LOWER_Qp48(37,
               {Qp33, Qp34, Qp35, Qp36, queue:in(X, Qp37),
                Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                Qp44, Qp45, Qp46, Qp47, Qp48});
?IN_LOWER_Qp48(38,
               {Qp33, Qp34, Qp35, Qp36, Qp37,
                queue:in(X, Qp38), Qp39, Qp40, Qp41, Qp42, Qp43,
                Qp44, Qp45, Qp46, Qp47, Qp48});
?IN_LOWER_Qp48(39,
               {Qp33, Qp34, Qp35, Qp36, Qp37,
                Qp38, queue:in(X, Qp39), Qp40, Qp41, Qp42, Qp43,
                Qp44, Qp45, Qp46, Qp47, Qp48});
?IN_LOWER_Qp48(40,
               {Qp33, Qp34, Qp35, Qp36, Qp37,
                Qp38, Qp39, queue:in(X, Qp40), Qp41, Qp42, Qp43,
                Qp44, Qp45, Qp46, Qp47, Qp48});
?IN_LOWER_Qp48(41,
               {Qp33, Qp34, Qp35, Qp36, Qp37,
                Qp38, Qp39, Qp40, queue:in(X, Qp41), Qp42, Qp43,
                Qp44, Qp45, Qp46, Qp47, Qp48});
?IN_LOWER_Qp48(42,
               {Qp33, Qp34, Qp35, Qp36, Qp37,
                Qp38, Qp39, Qp40, Qp41, queue:in(X, Qp42), Qp43,
                Qp44, Qp45, Qp46, Qp47, Qp48});
?IN_LOWER_Qp48(43,
               {Qp33, Qp34, Qp35, Qp36, Qp37,
                Qp38, Qp39, Qp40, Qp41, Qp42, queue:in(X, Qp43),
                Qp44, Qp45, Qp46, Qp47, Qp48});
?IN_LOWER_Qp48(44,
               {Qp33, Qp34, Qp35, Qp36, Qp37,
                Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                queue:in(X, Qp44), Qp45, Qp46, Qp47, Qp48});
?IN_LOWER_Qp48(45,
               {Qp33, Qp34, Qp35, Qp36, Qp37,
                Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                Qp44, queue:in(X, Qp45), Qp46, Qp47, Qp48});
?IN_LOWER_Qp48(46,
               {Qp33, Qp34, Qp35, Qp36, Qp37,
                Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                Qp44, Qp45, queue:in(X, Qp46), Qp47, Qp48});
?IN_LOWER_Qp48(47,
               {Qp33, Qp34, Qp35, Qp36, Qp37,
                Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                Qp44, Qp45, Qp46, queue:in(X, Qp47), Qp48});
?IN_LOWER_Qp48(48,
               {Qp33, Qp34, Qp35, Qp36, Qp37,
                Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                Qp44, Qp45, Qp46, Qp47, queue:in(X, Qp48)});
?IN_LOWER_Qp64(49,
               {queue:in(X, Qp49), Qp50, Qp51, Qp52, Qp53,
                Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                Qp60, Qp61, Qp62, Qp63, Qp64});
?IN_LOWER_Qp64(50,
               {Qp49, queue:in(X, Qp50), Qp51, Qp52, Qp53,
                Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                Qp60, Qp61, Qp62, Qp63, Qp64});
?IN_LOWER_Qp64(51,
               {Qp49, Qp50, queue:in(X, Qp51), Qp52, Qp53,
                Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                Qp60, Qp61, Qp62, Qp63, Qp64});
?IN_LOWER_Qp64(52,
               {Qp49, Qp50, Qp51, queue:in(X, Qp52), Qp53,
                Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                Qp60, Qp61, Qp62, Qp63, Qp64});
?IN_LOWER_Qp64(53,
               {Qp49, Qp50, Qp51, Qp52, queue:in(X, Qp53),
                Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                Qp60, Qp61, Qp62, Qp63, Qp64});
?IN_LOWER_Qp64(54,
               {Qp49, Qp50, Qp51, Qp52, Qp53,
                queue:in(X, Qp54), Qp55, Qp56, Qp57, Qp58, Qp59,
                Qp60, Qp61, Qp62, Qp63, Qp64});
?IN_LOWER_Qp64(55,
               {Qp49, Qp50, Qp51, Qp52, Qp53,
                Qp54, queue:in(X, Qp55), Qp56, Qp57, Qp58, Qp59,
                Qp60, Qp61, Qp62, Qp63, Qp64});
?IN_LOWER_Qp64(56,
               {Qp49, Qp50, Qp51, Qp52, Qp53,
                Qp54, Qp55, queue:in(X, Qp56), Qp57, Qp58, Qp59,
                Qp60, Qp61, Qp62, Qp63, Qp64});
?IN_LOWER_Qp64(57,
               {Qp49, Qp50, Qp51, Qp52, Qp53,
                Qp54, Qp55, Qp56, queue:in(X, Qp57), Qp58, Qp59,
                Qp60, Qp61, Qp62, Qp63, Qp64});
?IN_LOWER_Qp64(58,
               {Qp49, Qp50, Qp51, Qp52, Qp53,
                Qp54, Qp55, Qp56, Qp57, queue:in(X, Qp58), Qp59,
                Qp60, Qp61, Qp62, Qp63, Qp64});
?IN_LOWER_Qp64(59,
               {Qp49, Qp50, Qp51, Qp52, Qp53,
                Qp54, Qp55, Qp56, Qp57, Qp58, queue:in(X, Qp59),
                Qp60, Qp61, Qp62, Qp63, Qp64});
?IN_LOWER_Qp64(60,
               {Qp49, Qp50, Qp51, Qp52, Qp53,
                Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                queue:in(X, Qp60), Qp61, Qp62, Qp63, Qp64});
?IN_LOWER_Qp64(61,
               {Qp49, Qp50, Qp51, Qp52, Qp53,
                Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                Qp60, queue:in(X, Qp61), Qp62, Qp63, Qp64});
?IN_LOWER_Qp64(62,
               {Qp49, Qp50, Qp51, Qp52, Qp53,
                Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                Qp60, Qp61, queue:in(X, Qp62), Qp63, Qp64});
?IN_LOWER_Qp64(63,
               {Qp49, Qp50, Qp51, Qp52, Qp53,
                Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                Qp60, Qp61, Qp62, queue:in(X, Qp63), Qp64});
?IN_LOWER_Qp64(64,
               {Qp49, Qp50, Qp51, Qp52, Qp53,
                Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                Qp60, Qp61, Qp62, Qp63, queue:in(X, Qp64)});
?IN_LOWER_Qp80(65,
               {queue:in(X, Qp65), Qp66, Qp67, Qp68, Qp69,
                Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                Qp76, Qp77, Qp78, Qp79, Qp80});
?IN_LOWER_Qp80(66,
               {Qp65, queue:in(X, Qp66), Qp67, Qp68, Qp69,
                Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                Qp76, Qp77, Qp78, Qp79, Qp80});
?IN_LOWER_Qp80(67,
               {Qp65, Qp66, queue:in(X, Qp67), Qp68, Qp69,
                Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                Qp76, Qp77, Qp78, Qp79, Qp80});
?IN_LOWER_Qp80(68,
               {Qp65, Qp66, Qp67, queue:in(X, Qp68), Qp69,
                Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                Qp76, Qp77, Qp78, Qp79, Qp80});
?IN_LOWER_Qp80(69,
               {Qp65, Qp66, Qp67, Qp68, queue:in(X, Qp69),
                Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                Qp76, Qp77, Qp78, Qp79, Qp80});
?IN_LOWER_Qp80(70,
               {Qp65, Qp66, Qp67, Qp68, Qp69,
                queue:in(X, Qp70), Qp71, Qp72, Qp73, Qp74, Qp75,
                Qp76, Qp77, Qp78, Qp79, Qp80});
?IN_LOWER_Qp80(71,
               {Qp65, Qp66, Qp67, Qp68, Qp69,
                Qp70, queue:in(X, Qp71), Qp72, Qp73, Qp74, Qp75,
                Qp76, Qp77, Qp78, Qp79, Qp80});
?IN_LOWER_Qp80(72,
               {Qp65, Qp66, Qp67, Qp68, Qp69,
                Qp70, Qp71, queue:in(X, Qp72), Qp73, Qp74, Qp75,
                Qp76, Qp77, Qp78, Qp79, Qp80});
?IN_LOWER_Qp80(73,
               {Qp65, Qp66, Qp67, Qp68, Qp69,
                Qp70, Qp71, Qp72, queue:in(X, Qp73), Qp74, Qp75,
                Qp76, Qp77, Qp78, Qp79, Qp80});
?IN_LOWER_Qp80(74,
               {Qp65, Qp66, Qp67, Qp68, Qp69,
                Qp70, Qp71, Qp72, Qp73, queue:in(X, Qp74), Qp75,
                Qp76, Qp77, Qp78, Qp79, Qp80});
?IN_LOWER_Qp80(75,
               {Qp65, Qp66, Qp67, Qp68, Qp69,
                Qp70, Qp71, Qp72, Qp73, Qp74, queue:in(X, Qp75),
                Qp76, Qp77, Qp78, Qp79, Qp80});
?IN_LOWER_Qp80(76,
               {Qp65, Qp66, Qp67, Qp68, Qp69,
                Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                queue:in(X, Qp76), Qp77, Qp78, Qp79, Qp80});
?IN_LOWER_Qp80(77,
               {Qp65, Qp66, Qp67, Qp68, Qp69,
                Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                Qp76, queue:in(X, Qp77), Qp78, Qp79, Qp80});
?IN_LOWER_Qp80(78,
               {Qp65, Qp66, Qp67, Qp68, Qp69,
                Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                Qp76, Qp77, queue:in(X, Qp78), Qp79, Qp80});
?IN_LOWER_Qp80(79,
               {Qp65, Qp66, Qp67, Qp68, Qp69,
                Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                Qp76, Qp77, Qp78, queue:in(X, Qp79), Qp80});
?IN_LOWER_Qp80(80,
               {Qp65, Qp66, Qp67, Qp68, Qp69,
                Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                Qp76, Qp77, Qp78, Qp79, queue:in(X, Qp80)});
?IN_LOWER_Qp96(81,
               {queue:in(X, Qp81), Qp82, Qp83, Qp84, Qp85,
                Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                Qp92, Qp93, Qp94, Qp95, Qp96});
?IN_LOWER_Qp96(82,
               {Qp81, queue:in(X, Qp82), Qp83, Qp84, Qp85,
                Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                Qp92, Qp93, Qp94, Qp95, Qp96});
?IN_LOWER_Qp96(83,
               {Qp81, Qp82, queue:in(X, Qp83), Qp84, Qp85,
                Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                Qp92, Qp93, Qp94, Qp95, Qp96});
?IN_LOWER_Qp96(84,
               {Qp81, Qp82, Qp83, queue:in(X, Qp84), Qp85,
                Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                Qp92, Qp93, Qp94, Qp95, Qp96});
?IN_LOWER_Qp96(85,
               {Qp81, Qp82, Qp83, Qp84, queue:in(X, Qp85),
                Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                Qp92, Qp93, Qp94, Qp95, Qp96});
?IN_LOWER_Qp96(86,
               {Qp81, Qp82, Qp83, Qp84, Qp85,
                queue:in(X, Qp86), Qp87, Qp88, Qp89, Qp90, Qp91,
                Qp92, Qp93, Qp94, Qp95, Qp96});
?IN_LOWER_Qp96(87,
               {Qp81, Qp82, Qp83, Qp84, Qp85,
                Qp86, queue:in(X, Qp87), Qp88, Qp89, Qp90, Qp91,
                Qp92, Qp93, Qp94, Qp95, Qp96});
?IN_LOWER_Qp96(88,
               {Qp81, Qp82, Qp83, Qp84, Qp85,
                Qp86, Qp87, queue:in(X, Qp88), Qp89, Qp90, Qp91,
                Qp92, Qp93, Qp94, Qp95, Qp96});
?IN_LOWER_Qp96(89,
               {Qp81, Qp82, Qp83, Qp84, Qp85,
                Qp86, Qp87, Qp88, queue:in(X, Qp89), Qp90, Qp91,
                Qp92, Qp93, Qp94, Qp95, Qp96});
?IN_LOWER_Qp96(90,
               {Qp81, Qp82, Qp83, Qp84, Qp85,
                Qp86, Qp87, Qp88, Qp89, queue:in(X, Qp90), Qp91,
                Qp92, Qp93, Qp94, Qp95, Qp96});
?IN_LOWER_Qp96(91,
               {Qp81, Qp82, Qp83, Qp84, Qp85,
                Qp86, Qp87, Qp88, Qp89, Qp90, queue:in(X, Qp91),
                Qp92, Qp93, Qp94, Qp95, Qp96});
?IN_LOWER_Qp96(92,
               {Qp81, Qp82, Qp83, Qp84, Qp85,
                Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                queue:in(X, Qp92), Qp93, Qp94, Qp95, Qp96});
?IN_LOWER_Qp96(93,
               {Qp81, Qp82, Qp83, Qp84, Qp85,
                Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                Qp92, queue:in(X, Qp93), Qp94, Qp95, Qp96});
?IN_LOWER_Qp96(94,
               {Qp81, Qp82, Qp83, Qp84, Qp85,
                Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                Qp92, Qp93, queue:in(X, Qp94), Qp95, Qp96});
?IN_LOWER_Qp96(95,
               {Qp81, Qp82, Qp83, Qp84, Qp85,
                Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                Qp92, Qp93, Qp94, queue:in(X, Qp95), Qp96});
?IN_LOWER_Qp96(96,
               {Qp81, Qp82, Qp83, Qp84, Qp85,
                Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                Qp92, Qp93, Qp94, Qp95, queue:in(X, Qp96)});
?IN_LOWER_Qp112(97,
                {queue:in(X, Qp97), Qp98, Qp99, Qp100, Qp101,
                 Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                 Qp108, Qp109, Qp110, Qp111, Qp112});
?IN_LOWER_Qp112(98,
                {Qp97, queue:in(X, Qp98), Qp99, Qp100, Qp101,
                 Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                 Qp108, Qp109, Qp110, Qp111, Qp112});
?IN_LOWER_Qp112(99,
                {Qp97, Qp98, queue:in(X, Qp99), Qp100, Qp101,
                 Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                 Qp108, Qp109, Qp110, Qp111, Qp112});
?IN_LOWER_Qp112(100,
                {Qp97, Qp98, Qp99, queue:in(X, Qp100), Qp101,
                 Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                 Qp108, Qp109, Qp110, Qp111, Qp112});
?IN_LOWER_Qp112(101,
                {Qp97, Qp98, Qp99, Qp100, queue:in(X, Qp101),
                 Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                 Qp108, Qp109, Qp110, Qp111, Qp112});
?IN_LOWER_Qp112(102,
                {Qp97, Qp98, Qp99, Qp100, Qp101,
                 queue:in(X, Qp102), Qp103, Qp104, Qp105, Qp106, Qp107,
                 Qp108, Qp109, Qp110, Qp111, Qp112});
?IN_LOWER_Qp112(103,
                {Qp97, Qp98, Qp99, Qp100, Qp101,
                 Qp102, queue:in(X, Qp103), Qp104, Qp105, Qp106, Qp107,
                 Qp108, Qp109, Qp110, Qp111, Qp112});
?IN_LOWER_Qp112(104,
                {Qp97, Qp98, Qp99, Qp100, Qp101,
                 Qp102, Qp103, queue:in(X, Qp104), Qp105, Qp106, Qp107,
                 Qp108, Qp109, Qp110, Qp111, Qp112});
?IN_LOWER_Qp112(105,
                {Qp97, Qp98, Qp99, Qp100, Qp101,
                 Qp102, Qp103, Qp104, queue:in(X, Qp105), Qp106, Qp107,
                 Qp108, Qp109, Qp110, Qp111, Qp112});
?IN_LOWER_Qp112(106,
                {Qp97, Qp98, Qp99, Qp100, Qp101,
                 Qp102, Qp103, Qp104, Qp105, queue:in(X, Qp106), Qp107,
                 Qp108, Qp109, Qp110, Qp111, Qp112});
?IN_LOWER_Qp112(107,
                {Qp97, Qp98, Qp99, Qp100, Qp101,
                 Qp102, Qp103, Qp104, Qp105, Qp106, queue:in(X, Qp107),
                 Qp108, Qp109, Qp110, Qp111, Qp112});
?IN_LOWER_Qp112(108,
                {Qp97, Qp98, Qp99, Qp100, Qp101,
                 Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                 queue:in(X, Qp108), Qp109, Qp110, Qp111, Qp112});
?IN_LOWER_Qp112(109,
                {Qp97, Qp98, Qp99, Qp100, Qp101,
                 Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                 Qp108, queue:in(X, Qp109), Qp110, Qp111, Qp112});
?IN_LOWER_Qp112(110,
                {Qp97, Qp98, Qp99, Qp100, Qp101,
                 Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                 Qp108, Qp109, queue:in(X, Qp110), Qp111, Qp112});
?IN_LOWER_Qp112(111,
                {Qp97, Qp98, Qp99, Qp100, Qp101,
                 Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                 Qp108, Qp109, Qp110, queue:in(X, Qp111), Qp112});
?IN_LOWER_Qp112(112,
                {Qp97, Qp98, Qp99, Qp100, Qp101,
                 Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                 Qp108, Qp109, Qp110, Qp111, queue:in(X, Qp112)});
?IN_LOWER_Qp128(113,
                {queue:in(X, Qp113), Qp114, Qp115, Qp116, Qp117,
                 Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                 Qp124, Qp125, Qp126, Qp127, Qp128});
?IN_LOWER_Qp128(114,
                {Qp113, queue:in(X, Qp114), Qp115, Qp116, Qp117,
                 Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                 Qp124, Qp125, Qp126, Qp127, Qp128});
?IN_LOWER_Qp128(115,
                {Qp113, Qp114, queue:in(X, Qp115), Qp116, Qp117,
                 Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                 Qp124, Qp125, Qp126, Qp127, Qp128});
?IN_LOWER_Qp128(116,
                {Qp113, Qp114, Qp115, queue:in(X, Qp116), Qp117,
                 Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                 Qp124, Qp125, Qp126, Qp127, Qp128});
?IN_LOWER_Qp128(117,
                {Qp113, Qp114, Qp115, Qp116, queue:in(X, Qp117),
                 Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                 Qp124, Qp125, Qp126, Qp127, Qp128});
?IN_LOWER_Qp128(118,
                {Qp113, Qp114, Qp115, Qp116, Qp117,
                 queue:in(X, Qp118), Qp119, Qp120, Qp121, Qp122, Qp123,
                 Qp124, Qp125, Qp126, Qp127, Qp128});
?IN_LOWER_Qp128(119,
                {Qp113, Qp114, Qp115, Qp116, Qp117,
                 Qp118, queue:in(X, Qp119), Qp120, Qp121, Qp122, Qp123,
                 Qp124, Qp125, Qp126, Qp127, Qp128});
?IN_LOWER_Qp128(120,
                {Qp113, Qp114, Qp115, Qp116, Qp117,
                 Qp118, Qp119, queue:in(X, Qp120), Qp121, Qp122, Qp123,
                 Qp124, Qp125, Qp126, Qp127, Qp128});
?IN_LOWER_Qp128(121,
                {Qp113, Qp114, Qp115, Qp116, Qp117,
                 Qp118, Qp119, Qp120, queue:in(X, Qp121), Qp122, Qp123,
                 Qp124, Qp125, Qp126, Qp127, Qp128});
?IN_LOWER_Qp128(122,
                {Qp113, Qp114, Qp115, Qp116, Qp117,
                 Qp118, Qp119, Qp120, Qp121, queue:in(X, Qp122), Qp123,
                 Qp124, Qp125, Qp126, Qp127, Qp128});
?IN_LOWER_Qp128(123,
                {Qp113, Qp114, Qp115, Qp116, Qp117,
                 Qp118, Qp119, Qp120, Qp121, Qp122, queue:in(X, Qp123),
                 Qp124, Qp125, Qp126, Qp127, Qp128});
?IN_LOWER_Qp128(124,
                {Qp113, Qp114, Qp115, Qp116, Qp117,
                 Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                 queue:in(X, Qp124), Qp125, Qp126, Qp127, Qp128});
?IN_LOWER_Qp128(125,
                {Qp113, Qp114, Qp115, Qp116, Qp117,
                 Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                 Qp124, queue:in(X, Qp125), Qp126, Qp127, Qp128});
?IN_LOWER_Qp128(126,
                {Qp113, Qp114, Qp115, Qp116, Qp117,
                 Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                 Qp124, Qp125, queue:in(X, Qp126), Qp127, Qp128});
?IN_LOWER_Qp128(127,
                {Qp113, Qp114, Qp115, Qp116, Qp117,
                 Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                 Qp124, Qp125, Qp126, queue:in(X, Qp127), Qp128});
?IN_LOWER_Qp128(128,
                {Qp113, Qp114, Qp115, Qp116, Qp117,
                 Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                 Qp124, Qp125, Qp126, Qp127, queue:in(X, Qp128)}).

-define(OUT_CURRENT_Qn128(P, V1, V2, V3),
out_current(P,
            {_,
             {Qn128, Qn127, Qn126, Qn125, Qn124, Qn123, Qn122, Qn121,
              Qn120, Qn119, Qn118, Qn117, Qn116, Qn115, Qn114, Qn113},
             Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
             Q0,
             Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128} = Q) ->
    {Value, V2} = queue:out(V1),
    if
        Value =:= empty ->
            out_current(P + 1, Q);
        true ->
            {Value,
             {P,
              V3,
              Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}
    end).
-define(OUT_CURRENT_Qn112(P, V1, V2, V3),
out_current(P,
            {_,
             Qn128,
             {Qn112, Qn111, Qn110, Qn109, Qn108, Qn107, Qn106, Qn105,
              Qn104, Qn103, Qn102, Qn101, Qn100, Qn99, Qn98, Qn97},
             Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
             Q0,
             Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128} = Q) ->
    {Value, V2} = queue:out(V1),
    if
        Value =:= empty ->
            out_current(P + 1, Q);
        true ->
            {Value,
             {P,
              Qn128,
              V3,
              Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}
    end).
-define(OUT_CURRENT_Qn96(P, V1, V2, V3),
out_current(P,
            {_,
             Qn128, Qn112,
             {Qn96, Qn95, Qn94, Qn93, Qn92, Qn91, Qn90, Qn89,
              Qn88, Qn87, Qn86, Qn85, Qn84, Qn83, Qn82, Qn81},
             Qn80, Qn64, Qn48, Qn32, Qn16,
             Q0,
             Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128} = Q) ->
    {Value, V2} = queue:out(V1),
    if
        Value =:= empty ->
            out_current(P + 1, Q);
        true ->
            {Value,
             {P,
              Qn128, Qn112,
              V3,
              Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}
    end).
-define(OUT_CURRENT_Qn80(P, V1, V2, V3),
out_current(P,
            {_,
             Qn128, Qn112, Qn96,
             {Qn80, Qn79, Qn78, Qn77, Qn76, Qn75, Qn74, Qn73,
              Qn72, Qn71, Qn70, Qn69, Qn68, Qn67, Qn66, Qn65},
             Qn64, Qn48, Qn32, Qn16,
             Q0,
             Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128} = Q) ->
    {Value, V2} = queue:out(V1),
    if
        Value =:= empty ->
            out_current(P + 1, Q);
        true ->
            {Value,
             {P,
              Qn128, Qn112, Qn96,
              V3,
              Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}
    end).
-define(OUT_CURRENT_Qn64(P, V1, V2, V3),
out_current(P,
            {_,
             Qn128, Qn112, Qn96, Qn80,
             {Qn64, Qn63, Qn62, Qn61, Qn60, Qn59, Qn58, Qn57,
              Qn56, Qn55, Qn54, Qn53, Qn52, Qn51, Qn50, Qn49},
             Qn48, Qn32, Qn16,
             Q0,
             Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128} = Q) ->
    {Value, V2} = queue:out(V1),
    if
        Value =:= empty ->
            out_current(P + 1, Q);
        true ->
            {Value,
             {P,
              Qn128, Qn112, Qn96, Qn80,
              V3,
              Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}
    end).
-define(OUT_CURRENT_Qn48(P, V1, V2, V3),
out_current(P,
            {_,
             Qn128, Qn112, Qn96, Qn80, Qn64,
             {Qn48, Qn47, Qn46, Qn45, Qn44, Qn43, Qn42, Qn41,
              Qn40, Qn39, Qn38, Qn37, Qn36, Qn35, Qn34, Qn33},
             Qn32, Qn16,
             Q0,
             Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128} = Q) ->
    {Value, V2} = queue:out(V1),
    if
        Value =:= empty ->
            out_current(P + 1, Q);
        true ->
            {Value,
             {P,
              Qn128, Qn112, Qn96, Qn80, Qn64,
              V3,
              Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}
    end).
-define(OUT_CURRENT_Qn32(P, V1, V2, V3),
out_current(P,
            {_,
             Qn128, Qn112, Qn96, Qn80, Qn64, Qn48,
             {Qn32, Qn31, Qn30, Qn29, Qn28, Qn27, Qn26, Qn25,
              Qn24, Qn23, Qn22, Qn21, Qn20, Qn19, Qn18, Qn17},
             Qn16,
             Q0,
             Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128} = Q) ->
    {Value, V2} = queue:out(V1),
    if
        Value =:= empty ->
            out_current(P + 1, Q);
        true ->
            {Value,
             {P,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48,
              V3,
              Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}
    end).
-define(OUT_CURRENT_Qn16(P, V1, V2, V3),
out_current(P,
            {_,
             Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32,
             {Qn16, Qn15, Qn14, Qn13, Qn12, Qn11, Qn10, Qn9,
              Qn8, Qn7, Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
             Q0,
             Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128} = Q) ->
    {Value, V2} = queue:out(V1),
    if
        Value =:= empty ->
            out_current(P + 1, Q);
        true ->
            {Value,
             {P,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32,
              V3,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}
    end).
-define(OUT_CURRENT_Qp16(P, V1, V2, V3),
out_current(P,
            {_,
             Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
             Q0,
             {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6, Qp7, Qp8,
              Qp9, Qp10, Qp11, Qp12, Qp13, Qp14, Qp15, Qp16},
             Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128} = Q) ->
    {Value, V2} = queue:out(V1),
    if
        Value =:= empty ->
            out_current(P + 1, Q);
        true ->
            {Value,
             {P,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              V3,
              Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}
    end).
-define(OUT_CURRENT_Qp32(P, V1, V2, V3),
out_current(P,
            {_,
             Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
             Q0,
             Qp16,
             {Qp17, Qp18, Qp19, Qp20, Qp21, Qp22, Qp23, Qp24,
              Qp25, Qp26, Qp27, Qp28, Qp29, Qp30, Qp31, Qp32},
             Qp48, Qp64, Qp80, Qp96, Qp112, Qp128} = Q) ->
    {Value, V2} = queue:out(V1),
    if
        Value =:= empty ->
            out_current(P + 1, Q);
        true ->
            {Value,
             {P,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16,
              V3,
              Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}
    end).
-define(OUT_CURRENT_Qp48(P, V1, V2, V3),
out_current(P,
            {_,
             Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
             Q0,
             Qp16, Qp32,
             {Qp33, Qp34, Qp35, Qp36, Qp37, Qp38, Qp39, Qp40,
              Qp41, Qp42, Qp43, Qp44, Qp45, Qp46, Qp47, Qp48},
             Qp64, Qp80, Qp96, Qp112, Qp128} = Q) ->
    {Value, V2} = queue:out(V1),
    if
        Value =:= empty ->
            out_current(P + 1, Q);
        true ->
            {Value,
             {P,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32,
              V3,
              Qp64, Qp80, Qp96, Qp112, Qp128}}
    end).
-define(OUT_CURRENT_Qp64(P, V1, V2, V3),
out_current(P,
            {_,
             Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
             Q0,
             Qp16, Qp32, Qp48,
             {Qp49, Qp50, Qp51, Qp52, Qp53, Qp54, Qp55, Qp56,
              Qp57, Qp58, Qp59, Qp60, Qp61, Qp62, Qp63, Qp64},
             Qp80, Qp96, Qp112, Qp128} = Q) ->
    {Value, V2} = queue:out(V1),
    if
        Value =:= empty ->
            out_current(P + 1, Q);
        true ->
            {Value,
             {P,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48,
              V3,
              Qp80, Qp96, Qp112, Qp128}}
    end).
-define(OUT_CURRENT_Qp80(P, V1, V2, V3),
out_current(P,
            {_,
             Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
             Q0,
             Qp16, Qp32, Qp48, Qp64,
             {Qp65, Qp66, Qp67, Qp68, Qp69, Qp70, Qp71, Qp72,
              Qp73, Qp74, Qp75, Qp76, Qp77, Qp78, Qp79, Qp80},
             Qp96, Qp112, Qp128} = Q) ->
    {Value, V2} = queue:out(V1),
    if
        Value =:= empty ->
            out_current(P + 1, Q);
        true ->
            {Value,
             {P,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64,
              V3,
              Qp96, Qp112, Qp128}}
    end).
-define(OUT_CURRENT_Qp96(P, V1, V2, V3),
out_current(P,
            {_,
             Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
             Q0,
             Qp16, Qp32, Qp48, Qp64, Qp80,
             {Qp81, Qp82, Qp83, Qp84, Qp85, Qp86, Qp87, Qp88,
              Qp89, Qp90, Qp91, Qp92, Qp93, Qp94, Qp95, Qp96},
             Qp112, Qp128} = Q) ->
    {Value, V2} = queue:out(V1),
    if
        Value =:= empty ->
            out_current(P + 1, Q);
        true ->
            {Value,
             {P,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80,
              V3,
              Qp112, Qp128}}
    end).
-define(OUT_CURRENT_Qp112(P, V1, V2, V3),
out_current(P,
            {_,
             Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
             Q0,
             Qp16, Qp32, Qp48, Qp64, Qp80, Qp96,
             {Qp97, Qp98, Qp99, Qp100, Qp101, Qp102, Qp103, Qp104,
              Qp105, Qp106, Qp107, Qp108, Qp109, Qp110, Qp111, Qp112},
             Qp128} = Q) ->
    {Value, V2} = queue:out(V1),
    if
        Value =:= empty ->
            out_current(P + 1, Q);
        true ->
            {Value,
             {P,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96,
              V3,
              Qp128}}
    end).
-define(OUT_CURRENT_Qp128(P, V1, V2, V3),
out_current(P,
            {_,
             Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
             Q0,
             Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112,
             {Qp113, Qp114, Qp115, Qp116, Qp117, Qp118, Qp119, Qp120,
              Qp121, Qp122, Qp123, Qp124, Qp125, Qp126, Qp127, Qp128}} = Q) ->
    {Value, V2} = queue:out(V1),
    if
        Value =:= empty ->
            out_current(P + 1, Q);
        true ->
            {Value,
             {P,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112,
              V3}}
    end).

?OUT_CURRENT_Qn128(-128,
                   Qn128, NewQn128,
                   {NewQn128, Qn127, Qn126, Qn125, Qn124,
                    Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                    Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_CURRENT_Qn128(-127,
                   Qn127, NewQn127,
                   {Qn128, NewQn127, Qn126, Qn125, Qn124,
                    Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                    Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_CURRENT_Qn128(-126,
                   Qn126, NewQn126,
                   {Qn128, Qn127, NewQn126, Qn125, Qn124,
                    Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                    Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_CURRENT_Qn128(-125,
                   Qn125, NewQn125,
                   {Qn128, Qn127, Qn126, NewQn125, Qn124,
                    Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                    Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_CURRENT_Qn128(-124,
                   Qn124, NewQn124,
                   {Qn128, Qn127, Qn126, Qn125, NewQn124,
                    Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                    Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_CURRENT_Qn128(-123,
                   Qn123, NewQn123,
                   {Qn128, Qn127, Qn126, Qn125, Qn124,
                    NewQn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                    Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_CURRENT_Qn128(-122,
                   Qn122, NewQn122,
                   {Qn128, Qn127, Qn126, Qn125, Qn124,
                    Qn123, NewQn122, Qn121, Qn120, Qn119, Qn118,
                    Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_CURRENT_Qn128(-121,
                   Qn121, NewQn121,
                   {Qn128, Qn127, Qn126, Qn125, Qn124,
                    Qn123, Qn122, NewQn121, Qn120, Qn119, Qn118,
                    Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_CURRENT_Qn128(-120,
                   Qn120, NewQn120,
                   {Qn128, Qn127, Qn126, Qn125, Qn124,
                    Qn123, Qn122, Qn121, NewQn120, Qn119, Qn118,
                    Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_CURRENT_Qn128(-119,
                   Qn119, NewQn119,
                   {Qn128, Qn127, Qn126, Qn125, Qn124,
                    Qn123, Qn122, Qn121, Qn120, NewQn119, Qn118,
                    Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_CURRENT_Qn128(-118,
                   Qn118, NewQn118,
                   {Qn128, Qn127, Qn126, Qn125, Qn124,
                    Qn123, Qn122, Qn121, Qn120, Qn119, NewQn118,
                    Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_CURRENT_Qn128(-117,
                   Qn117, NewQn117,
                   {Qn128, Qn127, Qn126, Qn125, Qn124,
                    Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                    NewQn117, Qn116, Qn115, Qn114, Qn113});
?OUT_CURRENT_Qn128(-116,
                   Qn116, NewQn116,
                   {Qn128, Qn127, Qn126, Qn125, Qn124,
                    Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                    Qn117, NewQn116, Qn115, Qn114, Qn113});
?OUT_CURRENT_Qn128(-115,
                   Qn115, NewQn115,
                   {Qn128, Qn127, Qn126, Qn125, Qn124,
                    Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                    Qn117, Qn116, NewQn115, Qn114, Qn113});
?OUT_CURRENT_Qn128(-114,
                   Qn114, NewQn114,
                   {Qn128, Qn127, Qn126, Qn125, Qn124,
                    Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                    Qn117, Qn116, Qn115, NewQn114, Qn113});
?OUT_CURRENT_Qn128(-113,
                   Qn113, NewQn113,
                   {Qn128, Qn127, Qn126, Qn125, Qn124,
                    Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                    Qn117, Qn116, Qn115, Qn114, NewQn113});
?OUT_CURRENT_Qn112(-112,
                   Qn112, NewQn112,
                   {NewQn112, Qn111, Qn110, Qn109, Qn108,
                    Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                    Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_CURRENT_Qn112(-111,
                   Qn111, NewQn111,
                   {Qn112, NewQn111, Qn110, Qn109, Qn108,
                    Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                    Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_CURRENT_Qn112(-110,
                   Qn110, NewQn110,
                   {Qn112, Qn111, NewQn110, Qn109, Qn108,
                    Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                    Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_CURRENT_Qn112(-109,
                   Qn109, NewQn109,
                   {Qn112, Qn111, Qn110, NewQn109, Qn108,
                    Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                    Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_CURRENT_Qn112(-108,
                   Qn108, NewQn108,
                   {Qn112, Qn111, Qn110, Qn109, NewQn108,
                    Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                    Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_CURRENT_Qn112(-107,
                   Qn107, NewQn107,
                   {Qn112, Qn111, Qn110, Qn109, Qn108,
                    NewQn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                    Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_CURRENT_Qn112(-106,
                   Qn106, NewQn106,
                   {Qn112, Qn111, Qn110, Qn109, Qn108,
                    Qn107, NewQn106, Qn105, Qn104, Qn103, Qn102,
                    Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_CURRENT_Qn112(-105,
                   Qn105, NewQn105,
                   {Qn112, Qn111, Qn110, Qn109, Qn108,
                    Qn107, Qn106, NewQn105, Qn104, Qn103, Qn102,
                    Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_CURRENT_Qn112(-104,
                   Qn104, NewQn104,
                   {Qn112, Qn111, Qn110, Qn109, Qn108,
                    Qn107, Qn106, Qn105, NewQn104, Qn103, Qn102,
                    Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_CURRENT_Qn112(-103,
                   Qn103, NewQn103,
                   {Qn112, Qn111, Qn110, Qn109, Qn108,
                    Qn107, Qn106, Qn105, Qn104, NewQn103, Qn102,
                    Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_CURRENT_Qn112(-102,
                   Qn102, NewQn102,
                   {Qn112, Qn111, Qn110, Qn109, Qn108,
                    Qn107, Qn106, Qn105, Qn104, Qn103, NewQn102,
                    Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_CURRENT_Qn112(-101,
                   Qn101, NewQn101,
                   {Qn112, Qn111, Qn110, Qn109, Qn108,
                    Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                    NewQn101, Qn100, Qn99, Qn98, Qn97});
?OUT_CURRENT_Qn112(-100,
                   Qn100, NewQn100,
                   {Qn112, Qn111, Qn110, Qn109, Qn108,
                    Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                    Qn101, NewQn100, Qn99, Qn98, Qn97});
?OUT_CURRENT_Qn112(-99,
                   Qn99, NewQn99,
                   {Qn112, Qn111, Qn110, Qn109, Qn108,
                    Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                    Qn101, Qn100, NewQn99, Qn98, Qn97});
?OUT_CURRENT_Qn112(-98,
                   Qn98, NewQn98,
                   {Qn112, Qn111, Qn110, Qn109, Qn108,
                    Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                    Qn101, Qn100, Qn99, NewQn98, Qn97});
?OUT_CURRENT_Qn112(-97,
                   Qn97, NewQn97,
                   {Qn112, Qn111, Qn110, Qn109, Qn108,
                    Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                    Qn101, Qn100, Qn99, Qn98, NewQn97});
?OUT_CURRENT_Qn96(-96,
                  Qn96, NewQn96,
                  {NewQn96, Qn95, Qn94, Qn93, Qn92,
                   Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                   Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_CURRENT_Qn96(-95,
                  Qn95, NewQn95,
                  {Qn96, NewQn95, Qn94, Qn93, Qn92,
                   Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                   Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_CURRENT_Qn96(-94,
                  Qn94, NewQn94,
                  {Qn96, Qn95, NewQn94, Qn93, Qn92,
                   Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                   Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_CURRENT_Qn96(-93,
                  Qn93, NewQn93,
                  {Qn96, Qn95, Qn94, NewQn93, Qn92,
                   Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                   Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_CURRENT_Qn96(-92,
                  Qn92, NewQn92,
                  {Qn96, Qn95, Qn94, Qn93, NewQn92,
                   Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                   Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_CURRENT_Qn96(-91,
                  Qn91, NewQn91,
                  {Qn96, Qn95, Qn94, Qn93, Qn92,
                   NewQn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                   Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_CURRENT_Qn96(-90,
                  Qn90, NewQn90,
                  {Qn96, Qn95, Qn94, Qn93, Qn92,
                   Qn91, NewQn90, Qn89, Qn88, Qn87, Qn86,
                   Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_CURRENT_Qn96(-89,
                  Qn89, NewQn89,
                  {Qn96, Qn95, Qn94, Qn93, Qn92,
                   Qn91, Qn90, NewQn89, Qn88, Qn87, Qn86,
                   Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_CURRENT_Qn96(-88,
                  Qn88, NewQn88,
                  {Qn96, Qn95, Qn94, Qn93, Qn92,
                   Qn91, Qn90, Qn89, NewQn88, Qn87, Qn86,
                   Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_CURRENT_Qn96(-87,
                  Qn87, NewQn87,
                  {Qn96, Qn95, Qn94, Qn93, Qn92,
                   Qn91, Qn90, Qn89, Qn88, NewQn87, Qn86,
                   Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_CURRENT_Qn96(-86,
                  Qn86, NewQn86,
                  {Qn96, Qn95, Qn94, Qn93, Qn92,
                   Qn91, Qn90, Qn89, Qn88, Qn87, NewQn86,
                   Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_CURRENT_Qn96(-85,
                  Qn85, NewQn85,
                  {Qn96, Qn95, Qn94, Qn93, Qn92,
                   Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                   NewQn85, Qn84, Qn83, Qn82, Qn81});
?OUT_CURRENT_Qn96(-84,
                  Qn84, NewQn84,
                  {Qn96, Qn95, Qn94, Qn93, Qn92,
                   Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                   Qn85, NewQn84, Qn83, Qn82, Qn81});
?OUT_CURRENT_Qn96(-83,
                  Qn83, NewQn83,
                  {Qn96, Qn95, Qn94, Qn93, Qn92,
                   Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                   Qn85, Qn84, NewQn83, Qn82, Qn81});
?OUT_CURRENT_Qn96(-82,
                  Qn82, NewQn82,
                  {Qn96, Qn95, Qn94, Qn93, Qn92,
                   Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                   Qn85, Qn84, Qn83, NewQn82, Qn81});
?OUT_CURRENT_Qn96(-81,
                  Qn81, NewQn81,
                  {Qn96, Qn95, Qn94, Qn93, Qn92,
                   Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                   Qn85, Qn84, Qn83, Qn82, NewQn81});
?OUT_CURRENT_Qn80(-80,
                  Qn80, NewQn80,
                  {NewQn80, Qn79, Qn78, Qn77, Qn76,
                   Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                   Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_CURRENT_Qn80(-79,
                  Qn79, NewQn79,
                  {Qn80, NewQn79, Qn78, Qn77, Qn76,
                   Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                   Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_CURRENT_Qn80(-78,
                  Qn78, NewQn78,
                  {Qn80, Qn79, NewQn78, Qn77, Qn76,
                   Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                   Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_CURRENT_Qn80(-77,
                  Qn77, NewQn77,
                  {Qn80, Qn79, Qn78, NewQn77, Qn76,
                   Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                   Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_CURRENT_Qn80(-76,
                  Qn76, NewQn76,
                  {Qn80, Qn79, Qn78, Qn77, NewQn76,
                   Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                   Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_CURRENT_Qn80(-75,
                  Qn75, NewQn75,
                  {Qn80, Qn79, Qn78, Qn77, Qn76,
                   NewQn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                   Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_CURRENT_Qn80(-74,
                  Qn74, NewQn74,
                  {Qn80, Qn79, Qn78, Qn77, Qn76,
                   Qn75, NewQn74, Qn73, Qn72, Qn71, Qn70,
                   Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_CURRENT_Qn80(-73,
                  Qn73, NewQn73,
                  {Qn80, Qn79, Qn78, Qn77, Qn76,
                   Qn75, Qn74, NewQn73, Qn72, Qn71, Qn70,
                   Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_CURRENT_Qn80(-72,
                  Qn72, NewQn72,
                  {Qn80, Qn79, Qn78, Qn77, Qn76,
                   Qn75, Qn74, Qn73, NewQn72, Qn71, Qn70,
                   Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_CURRENT_Qn80(-71,
                  Qn71, NewQn71,
                  {Qn80, Qn79, Qn78, Qn77, Qn76,
                   Qn75, Qn74, Qn73, Qn72, NewQn71, Qn70,
                   Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_CURRENT_Qn80(-70,
                  Qn70, NewQn70,
                  {Qn80, Qn79, Qn78, Qn77, Qn76,
                   Qn75, Qn74, Qn73, Qn72, Qn71, NewQn70,
                   Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_CURRENT_Qn80(-69,
                  Qn69, NewQn69,
                  {Qn80, Qn79, Qn78, Qn77, Qn76,
                   Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                   NewQn69, Qn68, Qn67, Qn66, Qn65});
?OUT_CURRENT_Qn80(-68,
                  Qn68, NewQn68,
                  {Qn80, Qn79, Qn78, Qn77, Qn76,
                   Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                   Qn69, NewQn68, Qn67, Qn66, Qn65});
?OUT_CURRENT_Qn80(-67,
                  Qn67, NewQn67,
                  {Qn80, Qn79, Qn78, Qn77, Qn76,
                   Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                   Qn69, Qn68, NewQn67, Qn66, Qn65});
?OUT_CURRENT_Qn80(-66,
                  Qn66, NewQn66,
                  {Qn80, Qn79, Qn78, Qn77, Qn76,
                   Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                   Qn69, Qn68, Qn67, NewQn66, Qn65});
?OUT_CURRENT_Qn80(-65,
                  Qn65, NewQn65,
                  {Qn80, Qn79, Qn78, Qn77, Qn76,
                   Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                   Qn69, Qn68, Qn67, Qn66, NewQn65});
?OUT_CURRENT_Qn64(-64,
                  Qn64, NewQn64,
                  {NewQn64, Qn63, Qn62, Qn61, Qn60,
                   Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                   Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_CURRENT_Qn64(-63,
                  Qn63, NewQn63,
                  {Qn64, NewQn63, Qn62, Qn61, Qn60,
                   Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                   Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_CURRENT_Qn64(-62,
                  Qn62, NewQn62,
                  {Qn64, Qn63, NewQn62, Qn61, Qn60,
                   Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                   Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_CURRENT_Qn64(-61,
                  Qn61, NewQn61,
                  {Qn64, Qn63, Qn62, NewQn61, Qn60,
                   Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                   Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_CURRENT_Qn64(-60,
                  Qn60, NewQn60,
                  {Qn64, Qn63, Qn62, Qn61, NewQn60,
                   Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                   Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_CURRENT_Qn64(-59,
                  Qn59, NewQn59,
                  {Qn64, Qn63, Qn62, Qn61, Qn60,
                   NewQn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                   Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_CURRENT_Qn64(-58,
                  Qn58, NewQn58,
                  {Qn64, Qn63, Qn62, Qn61, Qn60,
                   Qn59, NewQn58, Qn57, Qn56, Qn55, Qn54,
                   Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_CURRENT_Qn64(-57,
                  Qn57, NewQn57,
                  {Qn64, Qn63, Qn62, Qn61, Qn60,
                   Qn59, Qn58, NewQn57, Qn56, Qn55, Qn54,
                   Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_CURRENT_Qn64(-56,
                  Qn56, NewQn56,
                  {Qn64, Qn63, Qn62, Qn61, Qn60,
                   Qn59, Qn58, Qn57, NewQn56, Qn55, Qn54,
                   Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_CURRENT_Qn64(-55,
                  Qn55, NewQn55,
                  {Qn64, Qn63, Qn62, Qn61, Qn60,
                   Qn59, Qn58, Qn57, Qn56, NewQn55, Qn54,
                   Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_CURRENT_Qn64(-54,
                  Qn54, NewQn54,
                  {Qn64, Qn63, Qn62, Qn61, Qn60,
                   Qn59, Qn58, Qn57, Qn56, Qn55, NewQn54,
                   Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_CURRENT_Qn64(-53,
                  Qn53, NewQn53,
                  {Qn64, Qn63, Qn62, Qn61, Qn60,
                   Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                   NewQn53, Qn52, Qn51, Qn50, Qn49});
?OUT_CURRENT_Qn64(-52,
                  Qn52, NewQn52,
                  {Qn64, Qn63, Qn62, Qn61, Qn60,
                   Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                   Qn53, NewQn52, Qn51, Qn50, Qn49});
?OUT_CURRENT_Qn64(-51,
                  Qn51, NewQn51,
                  {Qn64, Qn63, Qn62, Qn61, Qn60,
                   Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                   Qn53, Qn52, NewQn51, Qn50, Qn49});
?OUT_CURRENT_Qn64(-50,
                  Qn50, NewQn50,
                  {Qn64, Qn63, Qn62, Qn61, Qn60,
                   Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                   Qn53, Qn52, Qn51, NewQn50, Qn49});
?OUT_CURRENT_Qn64(-49,
                  Qn49, NewQn49,
                  {Qn64, Qn63, Qn62, Qn61, Qn60,
                   Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                   Qn53, Qn52, Qn51, Qn50, NewQn49});
?OUT_CURRENT_Qn48(-48,
                  Qn48, NewQn48,
                  {NewQn48, Qn47, Qn46, Qn45, Qn44,
                   Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                   Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_CURRENT_Qn48(-47,
                  Qn47, NewQn47,
                  {Qn48, NewQn47, Qn46, Qn45, Qn44,
                   Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                   Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_CURRENT_Qn48(-46,
                  Qn46, NewQn46,
                  {Qn48, Qn47, NewQn46, Qn45, Qn44,
                   Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                   Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_CURRENT_Qn48(-45,
                  Qn45, NewQn45,
                  {Qn48, Qn47, Qn46, NewQn45, Qn44,
                   Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                   Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_CURRENT_Qn48(-44,
                  Qn44, NewQn44,
                  {Qn48, Qn47, Qn46, Qn45, NewQn44,
                   Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                   Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_CURRENT_Qn48(-43,
                  Qn43, NewQn43,
                  {Qn48, Qn47, Qn46, Qn45, Qn44,
                   NewQn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                   Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_CURRENT_Qn48(-42,
                  Qn42, NewQn42,
                  {Qn48, Qn47, Qn46, Qn45, Qn44,
                   Qn43, NewQn42, Qn41, Qn40, Qn39, Qn38,
                   Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_CURRENT_Qn48(-41,
                  Qn41, NewQn41,
                  {Qn48, Qn47, Qn46, Qn45, Qn44,
                   Qn43, Qn42, NewQn41, Qn40, Qn39, Qn38,
                   Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_CURRENT_Qn48(-40,
                  Qn40, NewQn40,
                  {Qn48, Qn47, Qn46, Qn45, Qn44,
                   Qn43, Qn42, Qn41, NewQn40, Qn39, Qn38,
                   Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_CURRENT_Qn48(-39,
                  Qn39, NewQn39,
                  {Qn48, Qn47, Qn46, Qn45, Qn44,
                   Qn43, Qn42, Qn41, Qn40, NewQn39, Qn38,
                   Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_CURRENT_Qn48(-38,
                  Qn38, NewQn38,
                  {Qn48, Qn47, Qn46, Qn45, Qn44,
                   Qn43, Qn42, Qn41, Qn40, Qn39, NewQn38,
                   Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_CURRENT_Qn48(-37,
                  Qn37, NewQn37,
                  {Qn48, Qn47, Qn46, Qn45, Qn44,
                   Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                   NewQn37, Qn36, Qn35, Qn34, Qn33});
?OUT_CURRENT_Qn48(-36,
                  Qn36, NewQn36,
                  {Qn48, Qn47, Qn46, Qn45, Qn44,
                   Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                   Qn37, NewQn36, Qn35, Qn34, Qn33});
?OUT_CURRENT_Qn48(-35,
                  Qn35, NewQn35,
                  {Qn48, Qn47, Qn46, Qn45, Qn44,
                   Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                   Qn37, Qn36, NewQn35, Qn34, Qn33});
?OUT_CURRENT_Qn48(-34,
                  Qn34, NewQn34,
                  {Qn48, Qn47, Qn46, Qn45, Qn44,
                   Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                   Qn37, Qn36, Qn35, NewQn34, Qn33});
?OUT_CURRENT_Qn48(-33,
                  Qn33, NewQn33,
                  {Qn48, Qn47, Qn46, Qn45, Qn44,
                   Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                   Qn37, Qn36, Qn35, Qn34, NewQn33});
?OUT_CURRENT_Qn32(-32,
                  Qn32, NewQn32,
                  {NewQn32, Qn31, Qn30, Qn29, Qn28,
                   Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                   Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_CURRENT_Qn32(-31,
                  Qn31, NewQn31,
                  {Qn32, NewQn31, Qn30, Qn29, Qn28,
                   Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                   Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_CURRENT_Qn32(-30,
                  Qn30, NewQn30,
                  {Qn32, Qn31, NewQn30, Qn29, Qn28,
                   Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                   Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_CURRENT_Qn32(-29,
                  Qn29, NewQn29,
                  {Qn32, Qn31, Qn30, NewQn29, Qn28,
                   Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                   Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_CURRENT_Qn32(-28,
                  Qn28, NewQn28,
                  {Qn32, Qn31, Qn30, Qn29, NewQn28,
                   Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                   Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_CURRENT_Qn32(-27,
                  Qn27, NewQn27,
                  {Qn32, Qn31, Qn30, Qn29, Qn28,
                   NewQn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                   Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_CURRENT_Qn32(-26,
                  Qn26, NewQn26,
                  {Qn32, Qn31, Qn30, Qn29, Qn28,
                   Qn27, NewQn26, Qn25, Qn24, Qn23, Qn22,
                   Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_CURRENT_Qn32(-25,
                  Qn25, NewQn25,
                  {Qn32, Qn31, Qn30, Qn29, Qn28,
                   Qn27, Qn26, NewQn25, Qn24, Qn23, Qn22,
                   Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_CURRENT_Qn32(-24,
                  Qn24, NewQn24,
                  {Qn32, Qn31, Qn30, Qn29, Qn28,
                   Qn27, Qn26, Qn25, NewQn24, Qn23, Qn22,
                   Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_CURRENT_Qn32(-23,
                  Qn23, NewQn23,
                  {Qn32, Qn31, Qn30, Qn29, Qn28,
                   Qn27, Qn26, Qn25, Qn24, NewQn23, Qn22,
                   Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_CURRENT_Qn32(-22,
                  Qn22, NewQn22,
                  {Qn32, Qn31, Qn30, Qn29, Qn28,
                   Qn27, Qn26, Qn25, Qn24, Qn23, NewQn22,
                   Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_CURRENT_Qn32(-21,
                  Qn21, NewQn21,
                  {Qn32, Qn31, Qn30, Qn29, Qn28,
                   Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                   NewQn21, Qn20, Qn19, Qn18, Qn17});
?OUT_CURRENT_Qn32(-20,
                  Qn20, NewQn20,
                  {Qn32, Qn31, Qn30, Qn29, Qn28,
                   Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                   Qn21, NewQn20, Qn19, Qn18, Qn17});
?OUT_CURRENT_Qn32(-19,
                  Qn19, NewQn19,
                  {Qn32, Qn31, Qn30, Qn29, Qn28,
                   Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                   Qn21, Qn20, NewQn19, Qn18, Qn17});
?OUT_CURRENT_Qn32(-18,
                  Qn18, NewQn18,
                  {Qn32, Qn31, Qn30, Qn29, Qn28,
                   Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                   Qn21, Qn20, Qn19, NewQn18, Qn17});
?OUT_CURRENT_Qn32(-17,
                  Qn17, NewQn17,
                  {Qn32, Qn31, Qn30, Qn29, Qn28,
                   Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                   Qn21, Qn20, Qn19, Qn18, NewQn17});
?OUT_CURRENT_Qn16(-16,
                  Qn16, NewQn16,
                  {NewQn16, Qn15, Qn14, Qn13, Qn12,
                   Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                   Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_CURRENT_Qn16(-15,
                  Qn15, NewQn15,
                  {Qn16, NewQn15, Qn14, Qn13, Qn12,
                   Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                   Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_CURRENT_Qn16(-14,
                  Qn14, NewQn14,
                  {Qn16, Qn15, NewQn14, Qn13, Qn12,
                   Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                   Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_CURRENT_Qn16(-13,
                  Qn13, NewQn13,
                  {Qn16, Qn15, Qn14, NewQn13, Qn12,
                   Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                   Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_CURRENT_Qn16(-12,
                  Qn12, NewQn12,
                  {Qn16, Qn15, Qn14, Qn13, NewQn12,
                   Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                   Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_CURRENT_Qn16(-11,
                  Qn11, NewQn11,
                  {Qn16, Qn15, Qn14, Qn13, Qn12,
                   NewQn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                   Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_CURRENT_Qn16(-10,
                  Qn10, NewQn10,
                  {Qn16, Qn15, Qn14, Qn13, Qn12,
                   Qn11, NewQn10, Qn9, Qn8, Qn7, Qn6,
                   Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_CURRENT_Qn16(-9,
                  Qn9, NewQn9,
                  {Qn16, Qn15, Qn14, Qn13, Qn12,
                   Qn11, Qn10, NewQn9, Qn8, Qn7, Qn6,
                   Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_CURRENT_Qn16(-8,
                  Qn8, NewQn8,
                  {Qn16, Qn15, Qn14, Qn13, Qn12,
                   Qn11, Qn10, Qn9, NewQn8, Qn7, Qn6,
                   Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_CURRENT_Qn16(-7,
                  Qn7, NewQn7,
                  {Qn16, Qn15, Qn14, Qn13, Qn12,
                   Qn11, Qn10, Qn9, Qn8, NewQn7, Qn6,
                   Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_CURRENT_Qn16(-6,
                  Qn6, NewQn6,
                  {Qn16, Qn15, Qn14, Qn13, Qn12,
                   Qn11, Qn10, Qn9, Qn8, Qn7, NewQn6,
                   Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_CURRENT_Qn16(-5,
                  Qn5, NewQn5,
                  {Qn16, Qn15, Qn14, Qn13, Qn12,
                   Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                   NewQn5, Qn4, Qn3, Qn2, Qn1});
?OUT_CURRENT_Qn16(-4,
                  Qn4, NewQn4,
                  {Qn16, Qn15, Qn14, Qn13, Qn12,
                   Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                   Qn5, NewQn4, Qn3, Qn2, Qn1});
?OUT_CURRENT_Qn16(-3,
                  Qn3, NewQn3,
                  {Qn16, Qn15, Qn14, Qn13, Qn12,
                   Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                   Qn5, Qn4, NewQn3, Qn2, Qn1});
?OUT_CURRENT_Qn16(-2,
                  Qn2, NewQn2,
                  {Qn16, Qn15, Qn14, Qn13, Qn12,
                   Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                   Qn5, Qn4, Qn3, NewQn2, Qn1});
?OUT_CURRENT_Qn16(-1,
                  Qn1, NewQn1,
                  {Qn16, Qn15, Qn14, Qn13, Qn12,
                   Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                   Qn5, Qn4, Qn3, Qn2, NewQn1});
out_current(0,
            {_,
             Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
             Q0,
             Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128} = Q) ->
    {Value, NewQ0} = queue:out(Q0),
    if
        Value =:= empty ->
            out_current(1, Q);
        true ->
            {Value,
             {0,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              NewQ0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}
    end;
?OUT_CURRENT_Qp16(1,
                  Qp1, NewQp1,
                  {NewQp1, Qp2, Qp3, Qp4, Qp5,
                   Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                   Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_CURRENT_Qp16(2,
                  Qp2, NewQp2,
                  {Qp1, NewQp2, Qp3, Qp4, Qp5,
                   Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                   Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_CURRENT_Qp16(3,
                  Qp3, NewQp3,
                  {Qp1, Qp2, NewQp3, Qp4, Qp5,
                   Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                   Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_CURRENT_Qp16(4,
                  Qp4, NewQp4,
                  {Qp1, Qp2, Qp3, NewQp4, Qp5,
                   Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                   Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_CURRENT_Qp16(5,
                  Qp5, NewQp5,
                  {Qp1, Qp2, Qp3, Qp4, NewQp5,
                   Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                   Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_CURRENT_Qp16(6,
                  Qp6, NewQp6,
                  {Qp1, Qp2, Qp3, Qp4, Qp5,
                   NewQp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                   Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_CURRENT_Qp16(7,
                  Qp7, NewQp7,
                  {Qp1, Qp2, Qp3, Qp4, Qp5,
                   Qp6, NewQp7, Qp8, Qp9, Qp10, Qp11,
                   Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_CURRENT_Qp16(8,
                  Qp8, NewQp8,
                  {Qp1, Qp2, Qp3, Qp4, Qp5,
                   Qp6, Qp7, NewQp8, Qp9, Qp10, Qp11,
                   Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_CURRENT_Qp16(9,
                  Qp9, NewQp9,
                  {Qp1, Qp2, Qp3, Qp4, Qp5,
                   Qp6, Qp7, Qp8, NewQp9, Qp10, Qp11,
                   Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_CURRENT_Qp16(10,
                  Qp10, NewQp10,
                  {Qp1, Qp2, Qp3, Qp4, Qp5,
                   Qp6, Qp7, Qp8, Qp9, NewQp10, Qp11,
                   Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_CURRENT_Qp16(11,
                  Qp11, NewQp11,
                  {Qp1, Qp2, Qp3, Qp4, Qp5,
                   Qp6, Qp7, Qp8, Qp9, Qp10, NewQp11,
                   Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_CURRENT_Qp16(12,
                  Qp12, NewQp12,
                  {Qp1, Qp2, Qp3, Qp4, Qp5,
                   Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                   NewQp12, Qp13, Qp14, Qp15, Qp16});
?OUT_CURRENT_Qp16(13,
                  Qp13, NewQp13,
                  {Qp1, Qp2, Qp3, Qp4, Qp5,
                   Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                   Qp12, NewQp13, Qp14, Qp15, Qp16});
?OUT_CURRENT_Qp16(14,
                  Qp14, NewQp14,
                  {Qp1, Qp2, Qp3, Qp4, Qp5,
                   Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                   Qp12, Qp13, NewQp14, Qp15, Qp16});
?OUT_CURRENT_Qp16(15,
                  Qp15, NewQp15,
                  {Qp1, Qp2, Qp3, Qp4, Qp5,
                   Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                   Qp12, Qp13, Qp14, NewQp15, Qp16});
?OUT_CURRENT_Qp16(16,
                  Qp16, NewQp16,
                  {Qp1, Qp2, Qp3, Qp4, Qp5,
                   Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                   Qp12, Qp13, Qp14, Qp15, NewQp16});
?OUT_CURRENT_Qp32(17,
                  Qp17, NewQp17,
                  {NewQp17, Qp18, Qp19, Qp20, Qp21,
                   Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                   Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_CURRENT_Qp32(18,
                  Qp18, NewQp18,
                  {Qp17, NewQp18, Qp19, Qp20, Qp21,
                   Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                   Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_CURRENT_Qp32(19,
                  Qp19, NewQp19,
                  {Qp17, Qp18, NewQp19, Qp20, Qp21,
                   Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                   Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_CURRENT_Qp32(20,
                  Qp20, NewQp20,
                  {Qp17, Qp18, Qp19, NewQp20, Qp21,
                   Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                   Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_CURRENT_Qp32(21,
                  Qp21, NewQp21,
                  {Qp17, Qp18, Qp19, Qp20, NewQp21,
                   Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                   Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_CURRENT_Qp32(22,
                  Qp22, NewQp22,
                  {Qp17, Qp18, Qp19, Qp20, Qp21,
                   NewQp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                   Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_CURRENT_Qp32(23,
                  Qp23, NewQp23,
                  {Qp17, Qp18, Qp19, Qp20, Qp21,
                   Qp22, NewQp23, Qp24, Qp25, Qp26, Qp27,
                   Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_CURRENT_Qp32(24,
                  Qp24, NewQp24,
                  {Qp17, Qp18, Qp19, Qp20, Qp21,
                   Qp22, Qp23, NewQp24, Qp25, Qp26, Qp27,
                   Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_CURRENT_Qp32(25,
                  Qp25, NewQp25,
                  {Qp17, Qp18, Qp19, Qp20, Qp21,
                   Qp22, Qp23, Qp24, NewQp25, Qp26, Qp27,
                   Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_CURRENT_Qp32(26,
                  Qp26, NewQp26,
                  {Qp17, Qp18, Qp19, Qp20, Qp21,
                   Qp22, Qp23, Qp24, Qp25, NewQp26, Qp27,
                   Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_CURRENT_Qp32(27,
                  Qp27, NewQp27,
                  {Qp17, Qp18, Qp19, Qp20, Qp21,
                   Qp22, Qp23, Qp24, Qp25, Qp26, NewQp27,
                   Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_CURRENT_Qp32(28,
                  Qp28, NewQp28,
                  {Qp17, Qp18, Qp19, Qp20, Qp21,
                   Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                   NewQp28, Qp29, Qp30, Qp31, Qp32});
?OUT_CURRENT_Qp32(29,
                  Qp29, NewQp29,
                  {Qp17, Qp18, Qp19, Qp20, Qp21,
                   Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                   Qp28, NewQp29, Qp30, Qp31, Qp32});
?OUT_CURRENT_Qp32(30,
                  Qp30, NewQp30,
                  {Qp17, Qp18, Qp19, Qp20, Qp21,
                   Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                   Qp28, Qp29, NewQp30, Qp31, Qp32});
?OUT_CURRENT_Qp32(31,
                  Qp31, NewQp31,
                  {Qp17, Qp18, Qp19, Qp20, Qp21,
                   Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                   Qp28, Qp29, Qp30, NewQp31, Qp32});
?OUT_CURRENT_Qp32(32,
                  Qp32, NewQp32,
                  {Qp17, Qp18, Qp19, Qp20, Qp21,
                   Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                   Qp28, Qp29, Qp30, Qp31, NewQp32});
?OUT_CURRENT_Qp48(33,
                  Qp33, NewQp33,
                  {NewQp33, Qp34, Qp35, Qp36, Qp37,
                   Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                   Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_CURRENT_Qp48(34,
                  Qp34, NewQp34,
                  {Qp33, NewQp34, Qp35, Qp36, Qp37,
                   Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                   Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_CURRENT_Qp48(35,
                  Qp35, NewQp35,
                  {Qp33, Qp34, NewQp35, Qp36, Qp37,
                   Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                   Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_CURRENT_Qp48(36,
                  Qp36, NewQp36,
                  {Qp33, Qp34, Qp35, NewQp36, Qp37,
                   Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                   Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_CURRENT_Qp48(37,
                  Qp37, NewQp37,
                  {Qp33, Qp34, Qp35, Qp36, NewQp37,
                   Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                   Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_CURRENT_Qp48(38,
                  Qp38, NewQp38,
                  {Qp33, Qp34, Qp35, Qp36, Qp37,
                   NewQp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                   Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_CURRENT_Qp48(39,
                  Qp39, NewQp39,
                  {Qp33, Qp34, Qp35, Qp36, Qp37,
                   Qp38, NewQp39, Qp40, Qp41, Qp42, Qp43,
                   Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_CURRENT_Qp48(40,
                  Qp40, NewQp40,
                  {Qp33, Qp34, Qp35, Qp36, Qp37,
                   Qp38, Qp39, NewQp40, Qp41, Qp42, Qp43,
                   Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_CURRENT_Qp48(41,
                  Qp41, NewQp41,
                  {Qp33, Qp34, Qp35, Qp36, Qp37,
                   Qp38, Qp39, Qp40, NewQp41, Qp42, Qp43,
                   Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_CURRENT_Qp48(42,
                  Qp42, NewQp42,
                  {Qp33, Qp34, Qp35, Qp36, Qp37,
                   Qp38, Qp39, Qp40, Qp41, NewQp42, Qp43,
                   Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_CURRENT_Qp48(43,
                  Qp43, NewQp43,
                  {Qp33, Qp34, Qp35, Qp36, Qp37,
                   Qp38, Qp39, Qp40, Qp41, Qp42, NewQp43,
                   Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_CURRENT_Qp48(44,
                  Qp44, NewQp44,
                  {Qp33, Qp34, Qp35, Qp36, Qp37,
                   Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                   NewQp44, Qp45, Qp46, Qp47, Qp48});
?OUT_CURRENT_Qp48(45,
                  Qp45, NewQp45,
                  {Qp33, Qp34, Qp35, Qp36, Qp37,
                   Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                   Qp44, NewQp45, Qp46, Qp47, Qp48});
?OUT_CURRENT_Qp48(46,
                  Qp46, NewQp46,
                  {Qp33, Qp34, Qp35, Qp36, Qp37,
                   Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                   Qp44, Qp45, NewQp46, Qp47, Qp48});
?OUT_CURRENT_Qp48(47,
                  Qp47, NewQp47,
                  {Qp33, Qp34, Qp35, Qp36, Qp37,
                   Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                   Qp44, Qp45, Qp46, NewQp47, Qp48});
?OUT_CURRENT_Qp48(48,
                  Qp48, NewQp48,
                  {Qp33, Qp34, Qp35, Qp36, Qp37,
                   Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                   Qp44, Qp45, Qp46, Qp47, NewQp48});
?OUT_CURRENT_Qp64(49,
                  Qp49, NewQp49,
                  {NewQp49, Qp50, Qp51, Qp52, Qp53,
                   Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                   Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_CURRENT_Qp64(50,
                  Qp50, NewQp50,
                  {Qp49, NewQp50, Qp51, Qp52, Qp53,
                   Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                   Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_CURRENT_Qp64(51,
                  Qp51, NewQp51,
                  {Qp49, Qp50, NewQp51, Qp52, Qp53,
                   Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                   Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_CURRENT_Qp64(52,
                  Qp52, NewQp52,
                  {Qp49, Qp50, Qp51, NewQp52, Qp53,
                   Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                   Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_CURRENT_Qp64(53,
                  Qp53, NewQp53,
                  {Qp49, Qp50, Qp51, Qp52, NewQp53,
                   Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                   Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_CURRENT_Qp64(54,
                  Qp54, NewQp54,
                  {Qp49, Qp50, Qp51, Qp52, Qp53,
                   NewQp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                   Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_CURRENT_Qp64(55,
                  Qp55, NewQp55,
                  {Qp49, Qp50, Qp51, Qp52, Qp53,
                   Qp54, NewQp55, Qp56, Qp57, Qp58, Qp59,
                   Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_CURRENT_Qp64(56,
                  Qp56, NewQp56,
                  {Qp49, Qp50, Qp51, Qp52, Qp53,
                   Qp54, Qp55, NewQp56, Qp57, Qp58, Qp59,
                   Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_CURRENT_Qp64(57,
                  Qp57, NewQp57,
                  {Qp49, Qp50, Qp51, Qp52, Qp53,
                   Qp54, Qp55, Qp56, NewQp57, Qp58, Qp59,
                   Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_CURRENT_Qp64(58,
                  Qp58, NewQp58,
                  {Qp49, Qp50, Qp51, Qp52, Qp53,
                   Qp54, Qp55, Qp56, Qp57, NewQp58, Qp59,
                   Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_CURRENT_Qp64(59,
                  Qp59, NewQp59,
                  {Qp49, Qp50, Qp51, Qp52, Qp53,
                   Qp54, Qp55, Qp56, Qp57, Qp58, NewQp59,
                   Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_CURRENT_Qp64(60,
                  Qp60, NewQp60,
                  {Qp49, Qp50, Qp51, Qp52, Qp53,
                   Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                   NewQp60, Qp61, Qp62, Qp63, Qp64});
?OUT_CURRENT_Qp64(61,
                  Qp61, NewQp61,
                  {Qp49, Qp50, Qp51, Qp52, Qp53,
                   Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                   Qp60, NewQp61, Qp62, Qp63, Qp64});
?OUT_CURRENT_Qp64(62,
                  Qp62, NewQp62,
                  {Qp49, Qp50, Qp51, Qp52, Qp53,
                   Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                   Qp60, Qp61, NewQp62, Qp63, Qp64});
?OUT_CURRENT_Qp64(63,
                  Qp63, NewQp63,
                  {Qp49, Qp50, Qp51, Qp52, Qp53,
                   Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                   Qp60, Qp61, Qp62, NewQp63, Qp64});
?OUT_CURRENT_Qp64(64,
                  Qp64, NewQp64,
                  {Qp49, Qp50, Qp51, Qp52, Qp53,
                   Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                   Qp60, Qp61, Qp62, Qp63, NewQp64});
?OUT_CURRENT_Qp80(65,
                  Qp65, NewQp65,
                  {NewQp65, Qp66, Qp67, Qp68, Qp69,
                   Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                   Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_CURRENT_Qp80(66,
                  Qp66, NewQp66,
                  {Qp65, NewQp66, Qp67, Qp68, Qp69,
                   Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                   Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_CURRENT_Qp80(67,
                  Qp67, NewQp67,
                  {Qp65, Qp66, NewQp67, Qp68, Qp69,
                   Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                   Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_CURRENT_Qp80(68,
                  Qp68, NewQp68,
                  {Qp65, Qp66, Qp67, NewQp68, Qp69,
                   Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                   Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_CURRENT_Qp80(69,
                  Qp69, NewQp69,
                  {Qp65, Qp66, Qp67, Qp68, NewQp69,
                   Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                   Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_CURRENT_Qp80(70,
                  Qp70, NewQp70,
                  {Qp65, Qp66, Qp67, Qp68, Qp69,
                   NewQp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                   Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_CURRENT_Qp80(71,
                  Qp71, NewQp71,
                  {Qp65, Qp66, Qp67, Qp68, Qp69,
                   Qp70, NewQp71, Qp72, Qp73, Qp74, Qp75,
                   Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_CURRENT_Qp80(72,
                  Qp72, NewQp72,
                  {Qp65, Qp66, Qp67, Qp68, Qp69,
                   Qp70, Qp71, NewQp72, Qp73, Qp74, Qp75,
                   Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_CURRENT_Qp80(73,
                  Qp73, NewQp73,
                  {Qp65, Qp66, Qp67, Qp68, Qp69,
                   Qp70, Qp71, Qp72, NewQp73, Qp74, Qp75,
                   Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_CURRENT_Qp80(74,
                  Qp74, NewQp74,
                  {Qp65, Qp66, Qp67, Qp68, Qp69,
                   Qp70, Qp71, Qp72, Qp73, NewQp74, Qp75,
                   Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_CURRENT_Qp80(75,
                  Qp75, NewQp75,
                  {Qp65, Qp66, Qp67, Qp68, Qp69,
                   Qp70, Qp71, Qp72, Qp73, Qp74, NewQp75,
                   Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_CURRENT_Qp80(76,
                  Qp76, NewQp76,
                  {Qp65, Qp66, Qp67, Qp68, Qp69,
                   Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                   NewQp76, Qp77, Qp78, Qp79, Qp80});
?OUT_CURRENT_Qp80(77,
                  Qp77, NewQp77,
                  {Qp65, Qp66, Qp67, Qp68, Qp69,
                   Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                   Qp76, NewQp77, Qp78, Qp79, Qp80});
?OUT_CURRENT_Qp80(78,
                  Qp78, NewQp78,
                  {Qp65, Qp66, Qp67, Qp68, Qp69,
                   Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                   Qp76, Qp77, NewQp78, Qp79, Qp80});
?OUT_CURRENT_Qp80(79,
                  Qp79, NewQp79,
                  {Qp65, Qp66, Qp67, Qp68, Qp69,
                   Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                   Qp76, Qp77, Qp78, NewQp79, Qp80});
?OUT_CURRENT_Qp80(80,
                  Qp80, NewQp80,
                  {Qp65, Qp66, Qp67, Qp68, Qp69,
                   Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                   Qp76, Qp77, Qp78, Qp79, NewQp80});
?OUT_CURRENT_Qp96(81,
                  Qp81, NewQp81,
                  {NewQp81, Qp82, Qp83, Qp84, Qp85,
                   Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                   Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_CURRENT_Qp96(82,
                  Qp82, NewQp82,
                  {Qp81, NewQp82, Qp83, Qp84, Qp85,
                   Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                   Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_CURRENT_Qp96(83,
                  Qp83, NewQp83,
                  {Qp81, Qp82, NewQp83, Qp84, Qp85,
                   Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                   Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_CURRENT_Qp96(84,
                  Qp84, NewQp84,
                  {Qp81, Qp82, Qp83, NewQp84, Qp85,
                   Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                   Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_CURRENT_Qp96(85,
                  Qp85, NewQp85,
                  {Qp81, Qp82, Qp83, Qp84, NewQp85,
                   Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                   Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_CURRENT_Qp96(86,
                  Qp86, NewQp86,
                  {Qp81, Qp82, Qp83, Qp84, Qp85,
                   NewQp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                   Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_CURRENT_Qp96(87,
                  Qp87, NewQp87,
                  {Qp81, Qp82, Qp83, Qp84, Qp85,
                   Qp86, NewQp87, Qp88, Qp89, Qp90, Qp91,
                   Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_CURRENT_Qp96(88,
                  Qp88, NewQp88,
                  {Qp81, Qp82, Qp83, Qp84, Qp85,
                   Qp86, Qp87, NewQp88, Qp89, Qp90, Qp91,
                   Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_CURRENT_Qp96(89,
                  Qp89, NewQp89,
                  {Qp81, Qp82, Qp83, Qp84, Qp85,
                   Qp86, Qp87, Qp88, NewQp89, Qp90, Qp91,
                   Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_CURRENT_Qp96(90,
                  Qp90, NewQp90,
                  {Qp81, Qp82, Qp83, Qp84, Qp85,
                   Qp86, Qp87, Qp88, Qp89, NewQp90, Qp91,
                   Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_CURRENT_Qp96(91,
                  Qp91, NewQp91,
                  {Qp81, Qp82, Qp83, Qp84, Qp85,
                   Qp86, Qp87, Qp88, Qp89, Qp90, NewQp91,
                   Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_CURRENT_Qp96(92,
                  Qp92, NewQp92,
                  {Qp81, Qp82, Qp83, Qp84, Qp85,
                   Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                   NewQp92, Qp93, Qp94, Qp95, Qp96});
?OUT_CURRENT_Qp96(93,
                  Qp93, NewQp93,
                  {Qp81, Qp82, Qp83, Qp84, Qp85,
                   Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                   Qp92, NewQp93, Qp94, Qp95, Qp96});
?OUT_CURRENT_Qp96(94,
                  Qp94, NewQp94,
                  {Qp81, Qp82, Qp83, Qp84, Qp85,
                   Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                   Qp92, Qp93, NewQp94, Qp95, Qp96});
?OUT_CURRENT_Qp96(95,
                  Qp95, NewQp95,
                  {Qp81, Qp82, Qp83, Qp84, Qp85,
                   Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                   Qp92, Qp93, Qp94, NewQp95, Qp96});
?OUT_CURRENT_Qp96(96,
                  Qp96, NewQp96,
                  {Qp81, Qp82, Qp83, Qp84, Qp85,
                   Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                   Qp92, Qp93, Qp94, Qp95, NewQp96});
?OUT_CURRENT_Qp112(97,
                   Qp97, NewQp97,
                   {NewQp97, Qp98, Qp99, Qp100, Qp101,
                    Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                    Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_CURRENT_Qp112(98,
                   Qp98, NewQp98,
                   {Qp97, NewQp98, Qp99, Qp100, Qp101,
                    Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                    Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_CURRENT_Qp112(99,
                   Qp99, NewQp99,
                   {Qp97, Qp98, NewQp99, Qp100, Qp101,
                    Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                    Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_CURRENT_Qp112(100,
                   Qp100, NewQp100,
                   {Qp97, Qp98, Qp99, NewQp100, Qp101,
                    Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                    Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_CURRENT_Qp112(101,
                   Qp101, NewQp101,
                   {Qp97, Qp98, Qp99, Qp100, NewQp101,
                    Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                    Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_CURRENT_Qp112(102,
                   Qp102, NewQp102,
                   {Qp97, Qp98, Qp99, Qp100, Qp101,
                    NewQp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                    Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_CURRENT_Qp112(103,
                   Qp103, NewQp103,
                   {Qp97, Qp98, Qp99, Qp100, Qp101,
                    Qp102, NewQp103, Qp104, Qp105, Qp106, Qp107,
                    Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_CURRENT_Qp112(104,
                   Qp104, NewQp104,
                   {Qp97, Qp98, Qp99, Qp100, Qp101,
                    Qp102, Qp103, NewQp104, Qp105, Qp106, Qp107,
                    Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_CURRENT_Qp112(105,
                   Qp105, NewQp105,
                   {Qp97, Qp98, Qp99, Qp100, Qp101,
                    Qp102, Qp103, Qp104, NewQp105, Qp106, Qp107,
                    Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_CURRENT_Qp112(106,
                   Qp106, NewQp106,
                   {Qp97, Qp98, Qp99, Qp100, Qp101,
                    Qp102, Qp103, Qp104, Qp105, NewQp106, Qp107,
                    Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_CURRENT_Qp112(107,
                   Qp107, NewQp107,
                   {Qp97, Qp98, Qp99, Qp100, Qp101,
                    Qp102, Qp103, Qp104, Qp105, Qp106, NewQp107,
                    Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_CURRENT_Qp112(108,
                   Qp108, NewQp108,
                   {Qp97, Qp98, Qp99, Qp100, Qp101,
                    Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                    NewQp108, Qp109, Qp110, Qp111, Qp112});
?OUT_CURRENT_Qp112(109,
                   Qp109, NewQp109,
                   {Qp97, Qp98, Qp99, Qp100, Qp101,
                    Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                    Qp108, NewQp109, Qp110, Qp111, Qp112});
?OUT_CURRENT_Qp112(110,
                   Qp110, NewQp110,
                   {Qp97, Qp98, Qp99, Qp100, Qp101,
                    Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                    Qp108, Qp109, NewQp110, Qp111, Qp112});
?OUT_CURRENT_Qp112(111,
                   Qp111, NewQp111,
                   {Qp97, Qp98, Qp99, Qp100, Qp101,
                    Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                    Qp108, Qp109, Qp110, NewQp111, Qp112});
?OUT_CURRENT_Qp112(112,
                   Qp112, NewQp112,
                   {Qp97, Qp98, Qp99, Qp100, Qp101,
                    Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                    Qp108, Qp109, Qp110, Qp111, NewQp112});
?OUT_CURRENT_Qp128(113,
                   Qp113, NewQp113,
                   {NewQp113, Qp114, Qp115, Qp116, Qp117,
                    Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                    Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_CURRENT_Qp128(114,
                   Qp114, NewQp114,
                   {Qp113, NewQp114, Qp115, Qp116, Qp117,
                    Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                    Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_CURRENT_Qp128(115,
                   Qp115, NewQp115,
                   {Qp113, Qp114, NewQp115, Qp116, Qp117,
                    Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                    Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_CURRENT_Qp128(116,
                   Qp116, NewQp116,
                   {Qp113, Qp114, Qp115, NewQp116, Qp117,
                    Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                    Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_CURRENT_Qp128(117,
                   Qp117, NewQp117,
                   {Qp113, Qp114, Qp115, Qp116, NewQp117,
                    Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                    Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_CURRENT_Qp128(118,
                   Qp118, NewQp118,
                   {Qp113, Qp114, Qp115, Qp116, Qp117,
                    NewQp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                    Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_CURRENT_Qp128(119,
                   Qp119, NewQp119,
                   {Qp113, Qp114, Qp115, Qp116, Qp117,
                    Qp118, NewQp119, Qp120, Qp121, Qp122, Qp123,
                    Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_CURRENT_Qp128(120,
                   Qp120, NewQp120,
                   {Qp113, Qp114, Qp115, Qp116, Qp117,
                    Qp118, Qp119, NewQp120, Qp121, Qp122, Qp123,
                    Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_CURRENT_Qp128(121,
                   Qp121, NewQp121,
                   {Qp113, Qp114, Qp115, Qp116, Qp117,
                    Qp118, Qp119, Qp120, NewQp121, Qp122, Qp123,
                    Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_CURRENT_Qp128(122,
                   Qp122, NewQp122,
                   {Qp113, Qp114, Qp115, Qp116, Qp117,
                    Qp118, Qp119, Qp120, Qp121, NewQp122, Qp123,
                    Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_CURRENT_Qp128(123,
                   Qp123, NewQp123,
                   {Qp113, Qp114, Qp115, Qp116, Qp117,
                    Qp118, Qp119, Qp120, Qp121, Qp122, NewQp123,
                    Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_CURRENT_Qp128(124,
                   Qp124, NewQp124,
                   {Qp113, Qp114, Qp115, Qp116, Qp117,
                    Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                    NewQp124, Qp125, Qp126, Qp127, Qp128});
?OUT_CURRENT_Qp128(125,
                   Qp125, NewQp125,
                   {Qp113, Qp114, Qp115, Qp116, Qp117,
                    Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                    Qp124, NewQp125, Qp126, Qp127, Qp128});
?OUT_CURRENT_Qp128(126,
                   Qp126, NewQp126,
                   {Qp113, Qp114, Qp115, Qp116, Qp117,
                    Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                    Qp124, Qp125, NewQp126, Qp127, Qp128});
?OUT_CURRENT_Qp128(127,
                   Qp127, NewQp127,
                   {Qp113, Qp114, Qp115, Qp116, Qp117,
                    Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                    Qp124, Qp125, Qp126, NewQp127, Qp128});
out_current(128,
            {_,
             Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
             Q0,
             Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112,
             {Qp113, Qp114, Qp115, Qp116, Qp117, Qp118, Qp119, Qp120,
              Qp121, Qp122, Qp123, Qp124, Qp125, Qp126, Qp127, Qp128}}) ->
    {Value, NewQp128} = queue:out(Qp128),
    if
        Value =:= empty ->
            {empty,
             {empty,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112,
              {Qp113, Qp114, Qp115, Qp116, Qp117, Qp118, Qp119, Qp120,
               Qp121, Qp122, Qp123, Qp124, Qp125, Qp126, Qp127, NewQp128}}};
        true ->
            {Value,
             {128,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112,
              {Qp113, Qp114, Qp115, Qp116, Qp117, Qp118, Qp119, Qp120,
               Qp121, Qp122, Qp123, Qp124, Qp125, Qp126, Qp127, NewQp128}}}
    end.

-define(OUT_CURRENT_P_Qn128(P, V1, V2, V3),
out_current_p(P,
              {_,
               {Qn128, Qn127, Qn126, Qn125, Qn124, Qn123, Qn122, Qn121,
                Qn120, Qn119, Qn118, Qn117, Qn116, Qn115, Qn114, Qn113},
               Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
               Q0,
               Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128} = Q) ->
    case queue:out(V1) of
        {empty, _} ->
            out_current_p(P + 1, Q);
        {{value, X}, V2} ->
            {{value, X, P},
             {P,
              V3,
              Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}
    end).
-define(OUT_CURRENT_P_Qn112(P, V1, V2, V3),
out_current_p(P,
              {_,
               Qn128,
               {Qn112, Qn111, Qn110, Qn109, Qn108, Qn107, Qn106, Qn105,
                Qn104, Qn103, Qn102, Qn101, Qn100, Qn99, Qn98, Qn97},
               Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
               Q0,
               Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128} = Q) ->
    case queue:out(V1) of
        {empty, _} ->
            out_current_p(P + 1, Q);
        {{value, X}, V2} ->
            {{value, X, P},
             {P,
              Qn128,
              V3,
              Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}
    end).
-define(OUT_CURRENT_P_Qn96(P, V1, V2, V3),
out_current_p(P,
              {_,
               Qn128, Qn112,
               {Qn96, Qn95, Qn94, Qn93, Qn92, Qn91, Qn90, Qn89,
                Qn88, Qn87, Qn86, Qn85, Qn84, Qn83, Qn82, Qn81},
               Qn80, Qn64, Qn48, Qn32, Qn16,
               Q0,
               Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128} = Q) ->
    case queue:out(V1) of
        {empty, _} ->
            out_current_p(P + 1, Q);
        {{value, X}, V2} ->
            {{value, X, P},
             {P,
              Qn128, Qn112,
              V3,
              Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}
    end).
-define(OUT_CURRENT_P_Qn80(P, V1, V2, V3),
out_current_p(P,
              {_,
               Qn128, Qn112, Qn96,
               {Qn80, Qn79, Qn78, Qn77, Qn76, Qn75, Qn74, Qn73,
                Qn72, Qn71, Qn70, Qn69, Qn68, Qn67, Qn66, Qn65},
               Qn64, Qn48, Qn32, Qn16,
               Q0,
               Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128} = Q) ->
    case queue:out(V1) of
        {empty, _} ->
            out_current_p(P + 1, Q);
        {{value, X}, V2} ->
            {{value, X, P},
             {P,
              Qn128, Qn112, Qn96,
              V3,
              Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}
    end).
-define(OUT_CURRENT_P_Qn64(P, V1, V2, V3),
out_current_p(P,
              {_,
               Qn128, Qn112, Qn96, Qn80,
               {Qn64, Qn63, Qn62, Qn61, Qn60, Qn59, Qn58, Qn57,
                Qn56, Qn55, Qn54, Qn53, Qn52, Qn51, Qn50, Qn49},
               Qn48, Qn32, Qn16,
               Q0,
               Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128} = Q) ->
    case queue:out(V1) of
        {empty, _} ->
            out_current_p(P + 1, Q);
        {{value, X}, V2} ->
            {{value, X, P},
             {P,
              Qn128, Qn112, Qn96, Qn80,
              V3,
              Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}
    end).
-define(OUT_CURRENT_P_Qn48(P, V1, V2, V3),
out_current_p(P,
              {_,
               Qn128, Qn112, Qn96, Qn80, Qn64,
               {Qn48, Qn47, Qn46, Qn45, Qn44, Qn43, Qn42, Qn41,
                Qn40, Qn39, Qn38, Qn37, Qn36, Qn35, Qn34, Qn33},
               Qn32, Qn16,
               Q0,
               Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128} = Q) ->
    case queue:out(V1) of
        {empty, _} ->
            out_current_p(P + 1, Q);
        {{value, X}, V2} ->
            {{value, X, P},
             {P,
              Qn128, Qn112, Qn96, Qn80, Qn64,
              V3,
              Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}
    end).
-define(OUT_CURRENT_P_Qn32(P, V1, V2, V3),
out_current_p(P,
              {_,
               Qn128, Qn112, Qn96, Qn80, Qn64, Qn48,
               {Qn32, Qn31, Qn30, Qn29, Qn28, Qn27, Qn26, Qn25,
                Qn24, Qn23, Qn22, Qn21, Qn20, Qn19, Qn18, Qn17},
               Qn16,
               Q0,
               Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128} = Q) ->
    case queue:out(V1) of
        {empty, _} ->
            out_current_p(P + 1, Q);
        {{value, X}, V2} ->
            {{value, X, P},
             {P,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48,
              V3,
              Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}
    end).
-define(OUT_CURRENT_P_Qn16(P, V1, V2, V3),
out_current_p(P,
              {_,
               Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32,
               {Qn16, Qn15, Qn14, Qn13, Qn12, Qn11, Qn10, Qn9,
                Qn8, Qn7, Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
               Q0,
               Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128} = Q) ->
    case queue:out(V1) of
        {empty, _} ->
            out_current_p(P + 1, Q);
        {{value, X}, V2} ->
            {{value, X, P},
             {P,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32,
              V3,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}
    end).
-define(OUT_CURRENT_P_Qp16(P, V1, V2, V3),
out_current_p(P,
              {_,
               Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
               Q0,
               {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6, Qp7, Qp8,
                Qp9, Qp10, Qp11, Qp12, Qp13, Qp14, Qp15, Qp16},
               Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128} = Q) ->
    case queue:out(V1) of
        {empty, _} ->
            out_current_p(P + 1, Q);
        {{value, X}, V2} ->
            {{value, X, P},
             {P,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              V3,
              Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}
    end).
-define(OUT_CURRENT_P_Qp32(P, V1, V2, V3),
out_current_p(P,
              {_,
               Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
               Q0,
               Qp16,
               {Qp17, Qp18, Qp19, Qp20, Qp21, Qp22, Qp23, Qp24,
                Qp25, Qp26, Qp27, Qp28, Qp29, Qp30, Qp31, Qp32},
               Qp48, Qp64, Qp80, Qp96, Qp112, Qp128} = Q) ->
    case queue:out(V1) of
        {empty, _} ->
            out_current_p(P + 1, Q);
        {{value, X}, V2} ->
            {{value, X, P},
             {P,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16,
              V3,
              Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}
    end).
-define(OUT_CURRENT_P_Qp48(P, V1, V2, V3),
out_current_p(P,
              {_,
               Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
               Q0,
               Qp16, Qp32,
               {Qp33, Qp34, Qp35, Qp36, Qp37, Qp38, Qp39, Qp40,
                Qp41, Qp42, Qp43, Qp44, Qp45, Qp46, Qp47, Qp48},
               Qp64, Qp80, Qp96, Qp112, Qp128} = Q) ->
    case queue:out(V1) of
        {empty, _} ->
            out_current_p(P + 1, Q);
        {{value, X}, V2} ->
            {{value, X, P},
             {P,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32,
              V3,
              Qp64, Qp80, Qp96, Qp112, Qp128}}
    end).
-define(OUT_CURRENT_P_Qp64(P, V1, V2, V3),
out_current_p(P,
              {_,
               Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
               Q0,
               Qp16, Qp32, Qp48,
               {Qp49, Qp50, Qp51, Qp52, Qp53, Qp54, Qp55, Qp56,
                Qp57, Qp58, Qp59, Qp60, Qp61, Qp62, Qp63, Qp64},
               Qp80, Qp96, Qp112, Qp128} = Q) ->
    case queue:out(V1) of
        {empty, _} ->
            out_current_p(P + 1, Q);
        {{value, X}, V2} ->
            {{value, X, P},
             {P,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48,
              V3,
              Qp80, Qp96, Qp112, Qp128}}
    end).
-define(OUT_CURRENT_P_Qp80(P, V1, V2, V3),
out_current_p(P,
              {_,
               Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
               Q0,
               Qp16, Qp32, Qp48, Qp64,
               {Qp65, Qp66, Qp67, Qp68, Qp69, Qp70, Qp71, Qp72,
                Qp73, Qp74, Qp75, Qp76, Qp77, Qp78, Qp79, Qp80},
               Qp96, Qp112, Qp128} = Q) ->
    case queue:out(V1) of
        {empty, _} ->
            out_current_p(P + 1, Q);
        {{value, X}, V2} ->
            {{value, X, P},
             {P,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64,
              V3,
              Qp96, Qp112, Qp128}}
    end).
-define(OUT_CURRENT_P_Qp96(P, V1, V2, V3),
out_current_p(P,
              {_,
               Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
               Q0,
               Qp16, Qp32, Qp48, Qp64, Qp80,
               {Qp81, Qp82, Qp83, Qp84, Qp85, Qp86, Qp87, Qp88,
                Qp89, Qp90, Qp91, Qp92, Qp93, Qp94, Qp95, Qp96},
               Qp112, Qp128} = Q) ->
    case queue:out(V1) of
        {empty, _} ->
            out_current_p(P + 1, Q);
        {{value, X}, V2} ->
            {{value, X, P},
             {P,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80,
              V3,
              Qp112, Qp128}}
    end).
-define(OUT_CURRENT_P_Qp112(P, V1, V2, V3),
out_current_p(P,
              {_,
               Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
               Q0,
               Qp16, Qp32, Qp48, Qp64, Qp80, Qp96,
               {Qp97, Qp98, Qp99, Qp100, Qp101, Qp102, Qp103, Qp104,
                Qp105, Qp106, Qp107, Qp108, Qp109, Qp110, Qp111, Qp112},
               Qp128} = Q) ->
    case queue:out(V1) of
        {empty, _} ->
            out_current_p(P + 1, Q);
        {{value, X}, V2} ->
            {{value, X, P},
             {P,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96,
              V3,
              Qp128}}
    end).
-define(OUT_CURRENT_P_Qp128(P, V1, V2, V3),
out_current_p(P,
              {_,
               Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
               Q0,
               Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112,
               {Qp113, Qp114, Qp115, Qp116, Qp117, Qp118, Qp119, Qp120,
                Qp121, Qp122, Qp123, Qp124, Qp125, Qp126, Qp127, Qp128}} = Q) ->
    case queue:out(V1) of
        {empty, _} ->
            out_current_p(P + 1, Q);
        {{value, X}, V2} ->
            {{value, X, P},
             {P,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112,
              V3}}
    end).

?OUT_CURRENT_P_Qn128(-128,
                     Qn128, NewQn128,
                     {NewQn128, Qn127, Qn126, Qn125, Qn124,
                      Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                      Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_CURRENT_P_Qn128(-127,
                     Qn127, NewQn127,
                     {Qn128, NewQn127, Qn126, Qn125, Qn124,
                      Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                      Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_CURRENT_P_Qn128(-126,
                     Qn126, NewQn126,
                     {Qn128, Qn127, NewQn126, Qn125, Qn124,
                      Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                      Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_CURRENT_P_Qn128(-125,
                     Qn125, NewQn125,
                     {Qn128, Qn127, Qn126, NewQn125, Qn124,
                      Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                      Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_CURRENT_P_Qn128(-124,
                     Qn124, NewQn124,
                     {Qn128, Qn127, Qn126, Qn125, NewQn124,
                      Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                      Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_CURRENT_P_Qn128(-123,
                     Qn123, NewQn123,
                     {Qn128, Qn127, Qn126, Qn125, Qn124,
                      NewQn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                      Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_CURRENT_P_Qn128(-122,
                     Qn122, NewQn122,
                     {Qn128, Qn127, Qn126, Qn125, Qn124,
                      Qn123, NewQn122, Qn121, Qn120, Qn119, Qn118,
                      Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_CURRENT_P_Qn128(-121,
                     Qn121, NewQn121,
                     {Qn128, Qn127, Qn126, Qn125, Qn124,
                      Qn123, Qn122, NewQn121, Qn120, Qn119, Qn118,
                      Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_CURRENT_P_Qn128(-120,
                     Qn120, NewQn120,
                     {Qn128, Qn127, Qn126, Qn125, Qn124,
                      Qn123, Qn122, Qn121, NewQn120, Qn119, Qn118,
                      Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_CURRENT_P_Qn128(-119,
                     Qn119, NewQn119,
                     {Qn128, Qn127, Qn126, Qn125, Qn124,
                      Qn123, Qn122, Qn121, Qn120, NewQn119, Qn118,
                      Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_CURRENT_P_Qn128(-118,
                     Qn118, NewQn118,
                     {Qn128, Qn127, Qn126, Qn125, Qn124,
                      Qn123, Qn122, Qn121, Qn120, Qn119, NewQn118,
                      Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_CURRENT_P_Qn128(-117,
                     Qn117, NewQn117,
                     {Qn128, Qn127, Qn126, Qn125, Qn124,
                      Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                      NewQn117, Qn116, Qn115, Qn114, Qn113});
?OUT_CURRENT_P_Qn128(-116,
                     Qn116, NewQn116,
                     {Qn128, Qn127, Qn126, Qn125, Qn124,
                      Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                      Qn117, NewQn116, Qn115, Qn114, Qn113});
?OUT_CURRENT_P_Qn128(-115,
                     Qn115, NewQn115,
                     {Qn128, Qn127, Qn126, Qn125, Qn124,
                      Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                      Qn117, Qn116, NewQn115, Qn114, Qn113});
?OUT_CURRENT_P_Qn128(-114,
                     Qn114, NewQn114,
                     {Qn128, Qn127, Qn126, Qn125, Qn124,
                      Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                      Qn117, Qn116, Qn115, NewQn114, Qn113});
?OUT_CURRENT_P_Qn128(-113,
                     Qn113, NewQn113,
                     {Qn128, Qn127, Qn126, Qn125, Qn124,
                      Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                      Qn117, Qn116, Qn115, Qn114, NewQn113});
?OUT_CURRENT_P_Qn112(-112,
                     Qn112, NewQn112,
                     {NewQn112, Qn111, Qn110, Qn109, Qn108,
                      Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                      Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_CURRENT_P_Qn112(-111,
                     Qn111, NewQn111,
                     {Qn112, NewQn111, Qn110, Qn109, Qn108,
                      Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                      Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_CURRENT_P_Qn112(-110,
                     Qn110, NewQn110,
                     {Qn112, Qn111, NewQn110, Qn109, Qn108,
                      Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                      Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_CURRENT_P_Qn112(-109,
                     Qn109, NewQn109,
                     {Qn112, Qn111, Qn110, NewQn109, Qn108,
                      Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                      Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_CURRENT_P_Qn112(-108,
                     Qn108, NewQn108,
                     {Qn112, Qn111, Qn110, Qn109, NewQn108,
                      Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                      Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_CURRENT_P_Qn112(-107,
                     Qn107, NewQn107,
                     {Qn112, Qn111, Qn110, Qn109, Qn108,
                      NewQn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                      Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_CURRENT_P_Qn112(-106,
                     Qn106, NewQn106,
                     {Qn112, Qn111, Qn110, Qn109, Qn108,
                      Qn107, NewQn106, Qn105, Qn104, Qn103, Qn102,
                      Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_CURRENT_P_Qn112(-105,
                     Qn105, NewQn105,
                     {Qn112, Qn111, Qn110, Qn109, Qn108,
                      Qn107, Qn106, NewQn105, Qn104, Qn103, Qn102,
                      Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_CURRENT_P_Qn112(-104,
                     Qn104, NewQn104,
                     {Qn112, Qn111, Qn110, Qn109, Qn108,
                      Qn107, Qn106, Qn105, NewQn104, Qn103, Qn102,
                      Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_CURRENT_P_Qn112(-103,
                     Qn103, NewQn103,
                     {Qn112, Qn111, Qn110, Qn109, Qn108,
                      Qn107, Qn106, Qn105, Qn104, NewQn103, Qn102,
                      Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_CURRENT_P_Qn112(-102,
                     Qn102, NewQn102,
                     {Qn112, Qn111, Qn110, Qn109, Qn108,
                      Qn107, Qn106, Qn105, Qn104, Qn103, NewQn102,
                      Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_CURRENT_P_Qn112(-101,
                     Qn101, NewQn101,
                     {Qn112, Qn111, Qn110, Qn109, Qn108,
                      Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                      NewQn101, Qn100, Qn99, Qn98, Qn97});
?OUT_CURRENT_P_Qn112(-100,
                     Qn100, NewQn100,
                     {Qn112, Qn111, Qn110, Qn109, Qn108,
                      Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                      Qn101, NewQn100, Qn99, Qn98, Qn97});
?OUT_CURRENT_P_Qn112(-99,
                     Qn99, NewQn99,
                     {Qn112, Qn111, Qn110, Qn109, Qn108,
                      Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                      Qn101, Qn100, NewQn99, Qn98, Qn97});
?OUT_CURRENT_P_Qn112(-98,
                     Qn98, NewQn98,
                     {Qn112, Qn111, Qn110, Qn109, Qn108,
                      Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                      Qn101, Qn100, Qn99, NewQn98, Qn97});
?OUT_CURRENT_P_Qn112(-97,
                     Qn97, NewQn97,
                     {Qn112, Qn111, Qn110, Qn109, Qn108,
                      Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                      Qn101, Qn100, Qn99, Qn98, NewQn97});
?OUT_CURRENT_P_Qn96(-96,
                    Qn96, NewQn96,
                    {NewQn96, Qn95, Qn94, Qn93, Qn92,
                     Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                     Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_CURRENT_P_Qn96(-95,
                    Qn95, NewQn95,
                    {Qn96, NewQn95, Qn94, Qn93, Qn92,
                     Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                     Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_CURRENT_P_Qn96(-94,
                    Qn94, NewQn94,
                    {Qn96, Qn95, NewQn94, Qn93, Qn92,
                     Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                     Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_CURRENT_P_Qn96(-93,
                    Qn93, NewQn93,
                    {Qn96, Qn95, Qn94, NewQn93, Qn92,
                     Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                     Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_CURRENT_P_Qn96(-92,
                    Qn92, NewQn92,
                    {Qn96, Qn95, Qn94, Qn93, NewQn92,
                     Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                     Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_CURRENT_P_Qn96(-91,
                    Qn91, NewQn91,
                    {Qn96, Qn95, Qn94, Qn93, Qn92,
                     NewQn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                     Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_CURRENT_P_Qn96(-90,
                    Qn90, NewQn90,
                    {Qn96, Qn95, Qn94, Qn93, Qn92,
                     Qn91, NewQn90, Qn89, Qn88, Qn87, Qn86,
                     Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_CURRENT_P_Qn96(-89,
                    Qn89, NewQn89,
                    {Qn96, Qn95, Qn94, Qn93, Qn92,
                     Qn91, Qn90, NewQn89, Qn88, Qn87, Qn86,
                     Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_CURRENT_P_Qn96(-88,
                    Qn88, NewQn88,
                    {Qn96, Qn95, Qn94, Qn93, Qn92,
                     Qn91, Qn90, Qn89, NewQn88, Qn87, Qn86,
                     Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_CURRENT_P_Qn96(-87,
                    Qn87, NewQn87,
                    {Qn96, Qn95, Qn94, Qn93, Qn92,
                     Qn91, Qn90, Qn89, Qn88, NewQn87, Qn86,
                     Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_CURRENT_P_Qn96(-86,
                    Qn86, NewQn86,
                    {Qn96, Qn95, Qn94, Qn93, Qn92,
                     Qn91, Qn90, Qn89, Qn88, Qn87, NewQn86,
                     Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_CURRENT_P_Qn96(-85,
                    Qn85, NewQn85,
                    {Qn96, Qn95, Qn94, Qn93, Qn92,
                     Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                     NewQn85, Qn84, Qn83, Qn82, Qn81});
?OUT_CURRENT_P_Qn96(-84,
                    Qn84, NewQn84,
                    {Qn96, Qn95, Qn94, Qn93, Qn92,
                     Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                     Qn85, NewQn84, Qn83, Qn82, Qn81});
?OUT_CURRENT_P_Qn96(-83,
                    Qn83, NewQn83,
                    {Qn96, Qn95, Qn94, Qn93, Qn92,
                     Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                     Qn85, Qn84, NewQn83, Qn82, Qn81});
?OUT_CURRENT_P_Qn96(-82,
                    Qn82, NewQn82,
                    {Qn96, Qn95, Qn94, Qn93, Qn92,
                     Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                     Qn85, Qn84, Qn83, NewQn82, Qn81});
?OUT_CURRENT_P_Qn96(-81,
                    Qn81, NewQn81,
                    {Qn96, Qn95, Qn94, Qn93, Qn92,
                     Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                     Qn85, Qn84, Qn83, Qn82, NewQn81});
?OUT_CURRENT_P_Qn80(-80,
                    Qn80, NewQn80,
                    {NewQn80, Qn79, Qn78, Qn77, Qn76,
                     Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                     Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_CURRENT_P_Qn80(-79,
                    Qn79, NewQn79,
                    {Qn80, NewQn79, Qn78, Qn77, Qn76,
                     Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                     Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_CURRENT_P_Qn80(-78,
                    Qn78, NewQn78,
                    {Qn80, Qn79, NewQn78, Qn77, Qn76,
                     Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                     Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_CURRENT_P_Qn80(-77,
                    Qn77, NewQn77,
                    {Qn80, Qn79, Qn78, NewQn77, Qn76,
                     Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                     Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_CURRENT_P_Qn80(-76,
                    Qn76, NewQn76,
                    {Qn80, Qn79, Qn78, Qn77, NewQn76,
                     Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                     Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_CURRENT_P_Qn80(-75,
                    Qn75, NewQn75,
                    {Qn80, Qn79, Qn78, Qn77, Qn76,
                     NewQn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                     Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_CURRENT_P_Qn80(-74,
                    Qn74, NewQn74,
                    {Qn80, Qn79, Qn78, Qn77, Qn76,
                     Qn75, NewQn74, Qn73, Qn72, Qn71, Qn70,
                     Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_CURRENT_P_Qn80(-73,
                    Qn73, NewQn73,
                    {Qn80, Qn79, Qn78, Qn77, Qn76,
                     Qn75, Qn74, NewQn73, Qn72, Qn71, Qn70,
                     Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_CURRENT_P_Qn80(-72,
                    Qn72, NewQn72,
                    {Qn80, Qn79, Qn78, Qn77, Qn76,
                     Qn75, Qn74, Qn73, NewQn72, Qn71, Qn70,
                     Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_CURRENT_P_Qn80(-71,
                    Qn71, NewQn71,
                    {Qn80, Qn79, Qn78, Qn77, Qn76,
                     Qn75, Qn74, Qn73, Qn72, NewQn71, Qn70,
                     Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_CURRENT_P_Qn80(-70,
                    Qn70, NewQn70,
                    {Qn80, Qn79, Qn78, Qn77, Qn76,
                     Qn75, Qn74, Qn73, Qn72, Qn71, NewQn70,
                     Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_CURRENT_P_Qn80(-69,
                    Qn69, NewQn69,
                    {Qn80, Qn79, Qn78, Qn77, Qn76,
                     Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                     NewQn69, Qn68, Qn67, Qn66, Qn65});
?OUT_CURRENT_P_Qn80(-68,
                    Qn68, NewQn68,
                    {Qn80, Qn79, Qn78, Qn77, Qn76,
                     Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                     Qn69, NewQn68, Qn67, Qn66, Qn65});
?OUT_CURRENT_P_Qn80(-67,
                    Qn67, NewQn67,
                    {Qn80, Qn79, Qn78, Qn77, Qn76,
                     Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                     Qn69, Qn68, NewQn67, Qn66, Qn65});
?OUT_CURRENT_P_Qn80(-66,
                    Qn66, NewQn66,
                    {Qn80, Qn79, Qn78, Qn77, Qn76,
                     Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                     Qn69, Qn68, Qn67, NewQn66, Qn65});
?OUT_CURRENT_P_Qn80(-65,
                    Qn65, NewQn65,
                    {Qn80, Qn79, Qn78, Qn77, Qn76,
                     Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                     Qn69, Qn68, Qn67, Qn66, NewQn65});
?OUT_CURRENT_P_Qn64(-64,
                    Qn64, NewQn64,
                    {NewQn64, Qn63, Qn62, Qn61, Qn60,
                     Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                     Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_CURRENT_P_Qn64(-63,
                    Qn63, NewQn63,
                    {Qn64, NewQn63, Qn62, Qn61, Qn60,
                     Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                     Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_CURRENT_P_Qn64(-62,
                    Qn62, NewQn62,
                    {Qn64, Qn63, NewQn62, Qn61, Qn60,
                     Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                     Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_CURRENT_P_Qn64(-61,
                    Qn61, NewQn61,
                    {Qn64, Qn63, Qn62, NewQn61, Qn60,
                     Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                     Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_CURRENT_P_Qn64(-60,
                    Qn60, NewQn60,
                    {Qn64, Qn63, Qn62, Qn61, NewQn60,
                     Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                     Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_CURRENT_P_Qn64(-59,
                    Qn59, NewQn59,
                    {Qn64, Qn63, Qn62, Qn61, Qn60,
                     NewQn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                     Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_CURRENT_P_Qn64(-58,
                    Qn58, NewQn58,
                    {Qn64, Qn63, Qn62, Qn61, Qn60,
                     Qn59, NewQn58, Qn57, Qn56, Qn55, Qn54,
                     Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_CURRENT_P_Qn64(-57,
                    Qn57, NewQn57,
                    {Qn64, Qn63, Qn62, Qn61, Qn60,
                     Qn59, Qn58, NewQn57, Qn56, Qn55, Qn54,
                     Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_CURRENT_P_Qn64(-56,
                    Qn56, NewQn56,
                    {Qn64, Qn63, Qn62, Qn61, Qn60,
                     Qn59, Qn58, Qn57, NewQn56, Qn55, Qn54,
                     Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_CURRENT_P_Qn64(-55,
                    Qn55, NewQn55,
                    {Qn64, Qn63, Qn62, Qn61, Qn60,
                     Qn59, Qn58, Qn57, Qn56, NewQn55, Qn54,
                     Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_CURRENT_P_Qn64(-54,
                    Qn54, NewQn54,
                    {Qn64, Qn63, Qn62, Qn61, Qn60,
                     Qn59, Qn58, Qn57, Qn56, Qn55, NewQn54,
                     Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_CURRENT_P_Qn64(-53,
                    Qn53, NewQn53,
                    {Qn64, Qn63, Qn62, Qn61, Qn60,
                     Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                     NewQn53, Qn52, Qn51, Qn50, Qn49});
?OUT_CURRENT_P_Qn64(-52,
                    Qn52, NewQn52,
                    {Qn64, Qn63, Qn62, Qn61, Qn60,
                     Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                     Qn53, NewQn52, Qn51, Qn50, Qn49});
?OUT_CURRENT_P_Qn64(-51,
                    Qn51, NewQn51,
                    {Qn64, Qn63, Qn62, Qn61, Qn60,
                     Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                     Qn53, Qn52, NewQn51, Qn50, Qn49});
?OUT_CURRENT_P_Qn64(-50,
                    Qn50, NewQn50,
                    {Qn64, Qn63, Qn62, Qn61, Qn60,
                     Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                     Qn53, Qn52, Qn51, NewQn50, Qn49});
?OUT_CURRENT_P_Qn64(-49,
                    Qn49, NewQn49,
                    {Qn64, Qn63, Qn62, Qn61, Qn60,
                     Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                     Qn53, Qn52, Qn51, Qn50, NewQn49});
?OUT_CURRENT_P_Qn48(-48,
                    Qn48, NewQn48,
                    {NewQn48, Qn47, Qn46, Qn45, Qn44,
                     Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                     Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_CURRENT_P_Qn48(-47,
                    Qn47, NewQn47,
                    {Qn48, NewQn47, Qn46, Qn45, Qn44,
                     Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                     Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_CURRENT_P_Qn48(-46,
                    Qn46, NewQn46,
                    {Qn48, Qn47, NewQn46, Qn45, Qn44,
                     Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                     Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_CURRENT_P_Qn48(-45,
                    Qn45, NewQn45,
                    {Qn48, Qn47, Qn46, NewQn45, Qn44,
                     Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                     Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_CURRENT_P_Qn48(-44,
                    Qn44, NewQn44,
                    {Qn48, Qn47, Qn46, Qn45, NewQn44,
                     Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                     Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_CURRENT_P_Qn48(-43,
                    Qn43, NewQn43,
                    {Qn48, Qn47, Qn46, Qn45, Qn44,
                     NewQn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                     Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_CURRENT_P_Qn48(-42,
                    Qn42, NewQn42,
                    {Qn48, Qn47, Qn46, Qn45, Qn44,
                     Qn43, NewQn42, Qn41, Qn40, Qn39, Qn38,
                     Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_CURRENT_P_Qn48(-41,
                    Qn41, NewQn41,
                    {Qn48, Qn47, Qn46, Qn45, Qn44,
                     Qn43, Qn42, NewQn41, Qn40, Qn39, Qn38,
                     Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_CURRENT_P_Qn48(-40,
                    Qn40, NewQn40,
                    {Qn48, Qn47, Qn46, Qn45, Qn44,
                     Qn43, Qn42, Qn41, NewQn40, Qn39, Qn38,
                     Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_CURRENT_P_Qn48(-39,
                    Qn39, NewQn39,
                    {Qn48, Qn47, Qn46, Qn45, Qn44,
                     Qn43, Qn42, Qn41, Qn40, NewQn39, Qn38,
                     Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_CURRENT_P_Qn48(-38,
                    Qn38, NewQn38,
                    {Qn48, Qn47, Qn46, Qn45, Qn44,
                     Qn43, Qn42, Qn41, Qn40, Qn39, NewQn38,
                     Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_CURRENT_P_Qn48(-37,
                    Qn37, NewQn37,
                    {Qn48, Qn47, Qn46, Qn45, Qn44,
                     Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                     NewQn37, Qn36, Qn35, Qn34, Qn33});
?OUT_CURRENT_P_Qn48(-36,
                    Qn36, NewQn36,
                    {Qn48, Qn47, Qn46, Qn45, Qn44,
                     Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                     Qn37, NewQn36, Qn35, Qn34, Qn33});
?OUT_CURRENT_P_Qn48(-35,
                    Qn35, NewQn35,
                    {Qn48, Qn47, Qn46, Qn45, Qn44,
                     Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                     Qn37, Qn36, NewQn35, Qn34, Qn33});
?OUT_CURRENT_P_Qn48(-34,
                    Qn34, NewQn34,
                    {Qn48, Qn47, Qn46, Qn45, Qn44,
                     Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                     Qn37, Qn36, Qn35, NewQn34, Qn33});
?OUT_CURRENT_P_Qn48(-33,
                    Qn33, NewQn33,
                    {Qn48, Qn47, Qn46, Qn45, Qn44,
                     Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                     Qn37, Qn36, Qn35, Qn34, NewQn33});
?OUT_CURRENT_P_Qn32(-32,
                    Qn32, NewQn32,
                    {NewQn32, Qn31, Qn30, Qn29, Qn28,
                     Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                     Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_CURRENT_P_Qn32(-31,
                    Qn31, NewQn31,
                    {Qn32, NewQn31, Qn30, Qn29, Qn28,
                     Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                     Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_CURRENT_P_Qn32(-30,
                    Qn30, NewQn30,
                    {Qn32, Qn31, NewQn30, Qn29, Qn28,
                     Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                     Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_CURRENT_P_Qn32(-29,
                    Qn29, NewQn29,
                    {Qn32, Qn31, Qn30, NewQn29, Qn28,
                     Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                     Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_CURRENT_P_Qn32(-28,
                    Qn28, NewQn28,
                    {Qn32, Qn31, Qn30, Qn29, NewQn28,
                     Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                     Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_CURRENT_P_Qn32(-27,
                    Qn27, NewQn27,
                    {Qn32, Qn31, Qn30, Qn29, Qn28,
                     NewQn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                     Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_CURRENT_P_Qn32(-26,
                    Qn26, NewQn26,
                    {Qn32, Qn31, Qn30, Qn29, Qn28,
                     Qn27, NewQn26, Qn25, Qn24, Qn23, Qn22,
                     Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_CURRENT_P_Qn32(-25,
                    Qn25, NewQn25,
                    {Qn32, Qn31, Qn30, Qn29, Qn28,
                     Qn27, Qn26, NewQn25, Qn24, Qn23, Qn22,
                     Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_CURRENT_P_Qn32(-24,
                    Qn24, NewQn24,
                    {Qn32, Qn31, Qn30, Qn29, Qn28,
                     Qn27, Qn26, Qn25, NewQn24, Qn23, Qn22,
                     Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_CURRENT_P_Qn32(-23,
                    Qn23, NewQn23,
                    {Qn32, Qn31, Qn30, Qn29, Qn28,
                     Qn27, Qn26, Qn25, Qn24, NewQn23, Qn22,
                     Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_CURRENT_P_Qn32(-22,
                    Qn22, NewQn22,
                    {Qn32, Qn31, Qn30, Qn29, Qn28,
                     Qn27, Qn26, Qn25, Qn24, Qn23, NewQn22,
                     Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_CURRENT_P_Qn32(-21,
                    Qn21, NewQn21,
                    {Qn32, Qn31, Qn30, Qn29, Qn28,
                     Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                     NewQn21, Qn20, Qn19, Qn18, Qn17});
?OUT_CURRENT_P_Qn32(-20,
                    Qn20, NewQn20,
                    {Qn32, Qn31, Qn30, Qn29, Qn28,
                     Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                     Qn21, NewQn20, Qn19, Qn18, Qn17});
?OUT_CURRENT_P_Qn32(-19,
                    Qn19, NewQn19,
                    {Qn32, Qn31, Qn30, Qn29, Qn28,
                     Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                     Qn21, Qn20, NewQn19, Qn18, Qn17});
?OUT_CURRENT_P_Qn32(-18,
                    Qn18, NewQn18,
                    {Qn32, Qn31, Qn30, Qn29, Qn28,
                     Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                     Qn21, Qn20, Qn19, NewQn18, Qn17});
?OUT_CURRENT_P_Qn32(-17,
                    Qn17, NewQn17,
                    {Qn32, Qn31, Qn30, Qn29, Qn28,
                     Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                     Qn21, Qn20, Qn19, Qn18, NewQn17});
?OUT_CURRENT_P_Qn16(-16,
                    Qn16, NewQn16,
                    {NewQn16, Qn15, Qn14, Qn13, Qn12,
                     Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                     Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_CURRENT_P_Qn16(-15,
                    Qn15, NewQn15,
                    {Qn16, NewQn15, Qn14, Qn13, Qn12,
                     Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                     Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_CURRENT_P_Qn16(-14,
                    Qn14, NewQn14,
                    {Qn16, Qn15, NewQn14, Qn13, Qn12,
                     Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                     Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_CURRENT_P_Qn16(-13,
                    Qn13, NewQn13,
                    {Qn16, Qn15, Qn14, NewQn13, Qn12,
                     Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                     Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_CURRENT_P_Qn16(-12,
                    Qn12, NewQn12,
                    {Qn16, Qn15, Qn14, Qn13, NewQn12,
                     Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                     Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_CURRENT_P_Qn16(-11,
                    Qn11, NewQn11,
                    {Qn16, Qn15, Qn14, Qn13, Qn12,
                     NewQn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                     Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_CURRENT_P_Qn16(-10,
                    Qn10, NewQn10,
                    {Qn16, Qn15, Qn14, Qn13, Qn12,
                     Qn11, NewQn10, Qn9, Qn8, Qn7, Qn6,
                     Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_CURRENT_P_Qn16(-9,
                    Qn9, NewQn9,
                    {Qn16, Qn15, Qn14, Qn13, Qn12,
                     Qn11, Qn10, NewQn9, Qn8, Qn7, Qn6,
                     Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_CURRENT_P_Qn16(-8,
                    Qn8, NewQn8,
                    {Qn16, Qn15, Qn14, Qn13, Qn12,
                     Qn11, Qn10, Qn9, NewQn8, Qn7, Qn6,
                     Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_CURRENT_P_Qn16(-7,
                    Qn7, NewQn7,
                    {Qn16, Qn15, Qn14, Qn13, Qn12,
                     Qn11, Qn10, Qn9, Qn8, NewQn7, Qn6,
                     Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_CURRENT_P_Qn16(-6,
                    Qn6, NewQn6,
                    {Qn16, Qn15, Qn14, Qn13, Qn12,
                     Qn11, Qn10, Qn9, Qn8, Qn7, NewQn6,
                     Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_CURRENT_P_Qn16(-5,
                    Qn5, NewQn5,
                    {Qn16, Qn15, Qn14, Qn13, Qn12,
                     Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                     NewQn5, Qn4, Qn3, Qn2, Qn1});
?OUT_CURRENT_P_Qn16(-4,
                    Qn4, NewQn4,
                    {Qn16, Qn15, Qn14, Qn13, Qn12,
                     Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                     Qn5, NewQn4, Qn3, Qn2, Qn1});
?OUT_CURRENT_P_Qn16(-3,
                    Qn3, NewQn3,
                    {Qn16, Qn15, Qn14, Qn13, Qn12,
                     Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                     Qn5, Qn4, NewQn3, Qn2, Qn1});
?OUT_CURRENT_P_Qn16(-2,
                    Qn2, NewQn2,
                    {Qn16, Qn15, Qn14, Qn13, Qn12,
                     Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                     Qn5, Qn4, Qn3, NewQn2, Qn1});
?OUT_CURRENT_P_Qn16(-1,
                    Qn1, NewQn1,
                    {Qn16, Qn15, Qn14, Qn13, Qn12,
                     Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                     Qn5, Qn4, Qn3, Qn2, NewQn1});
out_current_p(0,
              {_,
               Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
               Q0,
               Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128} = Q) ->
    case queue:out(Q0) of
        {empty, _} ->
            out_current_p(1, Q);
        {{value, X}, NewQ0} ->
            {{value, X, 0},
             {0,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              NewQ0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}
    end;
?OUT_CURRENT_P_Qp16(1,
                    Qp1, NewQp1,
                    {NewQp1, Qp2, Qp3, Qp4, Qp5,
                     Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                     Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_CURRENT_P_Qp16(2,
                    Qp2, NewQp2,
                    {Qp1, NewQp2, Qp3, Qp4, Qp5,
                     Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                     Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_CURRENT_P_Qp16(3,
                    Qp3, NewQp3,
                    {Qp1, Qp2, NewQp3, Qp4, Qp5,
                     Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                     Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_CURRENT_P_Qp16(4,
                    Qp4, NewQp4,
                    {Qp1, Qp2, Qp3, NewQp4, Qp5,
                     Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                     Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_CURRENT_P_Qp16(5,
                    Qp5, NewQp5,
                    {Qp1, Qp2, Qp3, Qp4, NewQp5,
                     Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                     Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_CURRENT_P_Qp16(6,
                    Qp6, NewQp6,
                    {Qp1, Qp2, Qp3, Qp4, Qp5,
                     NewQp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                     Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_CURRENT_P_Qp16(7,
                    Qp7, NewQp7,
                    {Qp1, Qp2, Qp3, Qp4, Qp5,
                     Qp6, NewQp7, Qp8, Qp9, Qp10, Qp11,
                     Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_CURRENT_P_Qp16(8,
                    Qp8, NewQp8,
                    {Qp1, Qp2, Qp3, Qp4, Qp5,
                     Qp6, Qp7, NewQp8, Qp9, Qp10, Qp11,
                     Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_CURRENT_P_Qp16(9,
                    Qp9, NewQp9,
                    {Qp1, Qp2, Qp3, Qp4, Qp5,
                     Qp6, Qp7, Qp8, NewQp9, Qp10, Qp11,
                     Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_CURRENT_P_Qp16(10,
                    Qp10, NewQp10,
                    {Qp1, Qp2, Qp3, Qp4, Qp5,
                     Qp6, Qp7, Qp8, Qp9, NewQp10, Qp11,
                     Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_CURRENT_P_Qp16(11,
                    Qp11, NewQp11,
                    {Qp1, Qp2, Qp3, Qp4, Qp5,
                     Qp6, Qp7, Qp8, Qp9, Qp10, NewQp11,
                     Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_CURRENT_P_Qp16(12,
                    Qp12, NewQp12,
                    {Qp1, Qp2, Qp3, Qp4, Qp5,
                     Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                     NewQp12, Qp13, Qp14, Qp15, Qp16});
?OUT_CURRENT_P_Qp16(13,
                    Qp13, NewQp13,
                    {Qp1, Qp2, Qp3, Qp4, Qp5,
                     Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                     Qp12, NewQp13, Qp14, Qp15, Qp16});
?OUT_CURRENT_P_Qp16(14,
                    Qp14, NewQp14,
                    {Qp1, Qp2, Qp3, Qp4, Qp5,
                     Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                     Qp12, Qp13, NewQp14, Qp15, Qp16});
?OUT_CURRENT_P_Qp16(15,
                    Qp15, NewQp15,
                    {Qp1, Qp2, Qp3, Qp4, Qp5,
                     Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                     Qp12, Qp13, Qp14, NewQp15, Qp16});
?OUT_CURRENT_P_Qp16(16,
                    Qp16, NewQp16,
                    {Qp1, Qp2, Qp3, Qp4, Qp5,
                     Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                     Qp12, Qp13, Qp14, Qp15, NewQp16});
?OUT_CURRENT_P_Qp32(17,
                    Qp17, NewQp17,
                    {NewQp17, Qp18, Qp19, Qp20, Qp21,
                     Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                     Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_CURRENT_P_Qp32(18,
                    Qp18, NewQp18,
                    {Qp17, NewQp18, Qp19, Qp20, Qp21,
                     Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                     Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_CURRENT_P_Qp32(19,
                    Qp19, NewQp19,
                    {Qp17, Qp18, NewQp19, Qp20, Qp21,
                     Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                     Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_CURRENT_P_Qp32(20,
                    Qp20, NewQp20,
                    {Qp17, Qp18, Qp19, NewQp20, Qp21,
                     Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                     Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_CURRENT_P_Qp32(21,
                    Qp21, NewQp21,
                    {Qp17, Qp18, Qp19, Qp20, NewQp21,
                     Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                     Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_CURRENT_P_Qp32(22,
                    Qp22, NewQp22,
                    {Qp17, Qp18, Qp19, Qp20, Qp21,
                     NewQp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                     Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_CURRENT_P_Qp32(23,
                    Qp23, NewQp23,
                    {Qp17, Qp18, Qp19, Qp20, Qp21,
                     Qp22, NewQp23, Qp24, Qp25, Qp26, Qp27,
                     Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_CURRENT_P_Qp32(24,
                    Qp24, NewQp24,
                    {Qp17, Qp18, Qp19, Qp20, Qp21,
                     Qp22, Qp23, NewQp24, Qp25, Qp26, Qp27,
                     Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_CURRENT_P_Qp32(25,
                    Qp25, NewQp25,
                    {Qp17, Qp18, Qp19, Qp20, Qp21,
                     Qp22, Qp23, Qp24, NewQp25, Qp26, Qp27,
                     Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_CURRENT_P_Qp32(26,
                    Qp26, NewQp26,
                    {Qp17, Qp18, Qp19, Qp20, Qp21,
                     Qp22, Qp23, Qp24, Qp25, NewQp26, Qp27,
                     Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_CURRENT_P_Qp32(27,
                    Qp27, NewQp27,
                    {Qp17, Qp18, Qp19, Qp20, Qp21,
                     Qp22, Qp23, Qp24, Qp25, Qp26, NewQp27,
                     Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_CURRENT_P_Qp32(28,
                    Qp28, NewQp28,
                    {Qp17, Qp18, Qp19, Qp20, Qp21,
                     Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                     NewQp28, Qp29, Qp30, Qp31, Qp32});
?OUT_CURRENT_P_Qp32(29,
                    Qp29, NewQp29,
                    {Qp17, Qp18, Qp19, Qp20, Qp21,
                     Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                     Qp28, NewQp29, Qp30, Qp31, Qp32});
?OUT_CURRENT_P_Qp32(30,
                    Qp30, NewQp30,
                    {Qp17, Qp18, Qp19, Qp20, Qp21,
                     Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                     Qp28, Qp29, NewQp30, Qp31, Qp32});
?OUT_CURRENT_P_Qp32(31,
                    Qp31, NewQp31,
                    {Qp17, Qp18, Qp19, Qp20, Qp21,
                     Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                     Qp28, Qp29, Qp30, NewQp31, Qp32});
?OUT_CURRENT_P_Qp32(32,
                    Qp32, NewQp32,
                    {Qp17, Qp18, Qp19, Qp20, Qp21,
                     Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                     Qp28, Qp29, Qp30, Qp31, NewQp32});
?OUT_CURRENT_P_Qp48(33,
                    Qp33, NewQp33,
                    {NewQp33, Qp34, Qp35, Qp36, Qp37,
                     Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                     Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_CURRENT_P_Qp48(34,
                    Qp34, NewQp34,
                    {Qp33, NewQp34, Qp35, Qp36, Qp37,
                     Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                     Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_CURRENT_P_Qp48(35,
                    Qp35, NewQp35,
                    {Qp33, Qp34, NewQp35, Qp36, Qp37,
                     Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                     Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_CURRENT_P_Qp48(36,
                    Qp36, NewQp36,
                    {Qp33, Qp34, Qp35, NewQp36, Qp37,
                     Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                     Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_CURRENT_P_Qp48(37,
                    Qp37, NewQp37,
                    {Qp33, Qp34, Qp35, Qp36, NewQp37,
                     Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                     Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_CURRENT_P_Qp48(38,
                    Qp38, NewQp38,
                    {Qp33, Qp34, Qp35, Qp36, Qp37,
                     NewQp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                     Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_CURRENT_P_Qp48(39,
                    Qp39, NewQp39,
                    {Qp33, Qp34, Qp35, Qp36, Qp37,
                     Qp38, NewQp39, Qp40, Qp41, Qp42, Qp43,
                     Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_CURRENT_P_Qp48(40,
                    Qp40, NewQp40,
                    {Qp33, Qp34, Qp35, Qp36, Qp37,
                     Qp38, Qp39, NewQp40, Qp41, Qp42, Qp43,
                     Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_CURRENT_P_Qp48(41,
                    Qp41, NewQp41,
                    {Qp33, Qp34, Qp35, Qp36, Qp37,
                     Qp38, Qp39, Qp40, NewQp41, Qp42, Qp43,
                     Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_CURRENT_P_Qp48(42,
                    Qp42, NewQp42,
                    {Qp33, Qp34, Qp35, Qp36, Qp37,
                     Qp38, Qp39, Qp40, Qp41, NewQp42, Qp43,
                     Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_CURRENT_P_Qp48(43,
                    Qp43, NewQp43,
                    {Qp33, Qp34, Qp35, Qp36, Qp37,
                     Qp38, Qp39, Qp40, Qp41, Qp42, NewQp43,
                     Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_CURRENT_P_Qp48(44,
                    Qp44, NewQp44,
                    {Qp33, Qp34, Qp35, Qp36, Qp37,
                     Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                     NewQp44, Qp45, Qp46, Qp47, Qp48});
?OUT_CURRENT_P_Qp48(45,
                    Qp45, NewQp45,
                    {Qp33, Qp34, Qp35, Qp36, Qp37,
                     Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                     Qp44, NewQp45, Qp46, Qp47, Qp48});
?OUT_CURRENT_P_Qp48(46,
                    Qp46, NewQp46,
                    {Qp33, Qp34, Qp35, Qp36, Qp37,
                     Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                     Qp44, Qp45, NewQp46, Qp47, Qp48});
?OUT_CURRENT_P_Qp48(47,
                    Qp47, NewQp47,
                    {Qp33, Qp34, Qp35, Qp36, Qp37,
                     Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                     Qp44, Qp45, Qp46, NewQp47, Qp48});
?OUT_CURRENT_P_Qp48(48,
                    Qp48, NewQp48,
                    {Qp33, Qp34, Qp35, Qp36, Qp37,
                     Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                     Qp44, Qp45, Qp46, Qp47, NewQp48});
?OUT_CURRENT_P_Qp64(49,
                    Qp49, NewQp49,
                    {NewQp49, Qp50, Qp51, Qp52, Qp53,
                     Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                     Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_CURRENT_P_Qp64(50,
                    Qp50, NewQp50,
                    {Qp49, NewQp50, Qp51, Qp52, Qp53,
                     Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                     Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_CURRENT_P_Qp64(51,
                    Qp51, NewQp51,
                    {Qp49, Qp50, NewQp51, Qp52, Qp53,
                     Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                     Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_CURRENT_P_Qp64(52,
                    Qp52, NewQp52,
                    {Qp49, Qp50, Qp51, NewQp52, Qp53,
                     Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                     Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_CURRENT_P_Qp64(53,
                    Qp53, NewQp53,
                    {Qp49, Qp50, Qp51, Qp52, NewQp53,
                     Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                     Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_CURRENT_P_Qp64(54,
                    Qp54, NewQp54,
                    {Qp49, Qp50, Qp51, Qp52, Qp53,
                     NewQp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                     Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_CURRENT_P_Qp64(55,
                    Qp55, NewQp55,
                    {Qp49, Qp50, Qp51, Qp52, Qp53,
                     Qp54, NewQp55, Qp56, Qp57, Qp58, Qp59,
                     Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_CURRENT_P_Qp64(56,
                    Qp56, NewQp56,
                    {Qp49, Qp50, Qp51, Qp52, Qp53,
                     Qp54, Qp55, NewQp56, Qp57, Qp58, Qp59,
                     Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_CURRENT_P_Qp64(57,
                    Qp57, NewQp57,
                    {Qp49, Qp50, Qp51, Qp52, Qp53,
                     Qp54, Qp55, Qp56, NewQp57, Qp58, Qp59,
                     Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_CURRENT_P_Qp64(58,
                    Qp58, NewQp58,
                    {Qp49, Qp50, Qp51, Qp52, Qp53,
                     Qp54, Qp55, Qp56, Qp57, NewQp58, Qp59,
                     Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_CURRENT_P_Qp64(59,
                    Qp59, NewQp59,
                    {Qp49, Qp50, Qp51, Qp52, Qp53,
                     Qp54, Qp55, Qp56, Qp57, Qp58, NewQp59,
                     Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_CURRENT_P_Qp64(60,
                    Qp60, NewQp60,
                    {Qp49, Qp50, Qp51, Qp52, Qp53,
                     Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                     NewQp60, Qp61, Qp62, Qp63, Qp64});
?OUT_CURRENT_P_Qp64(61,
                    Qp61, NewQp61,
                    {Qp49, Qp50, Qp51, Qp52, Qp53,
                     Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                     Qp60, NewQp61, Qp62, Qp63, Qp64});
?OUT_CURRENT_P_Qp64(62,
                    Qp62, NewQp62,
                    {Qp49, Qp50, Qp51, Qp52, Qp53,
                     Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                     Qp60, Qp61, NewQp62, Qp63, Qp64});
?OUT_CURRENT_P_Qp64(63,
                    Qp63, NewQp63,
                    {Qp49, Qp50, Qp51, Qp52, Qp53,
                     Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                     Qp60, Qp61, Qp62, NewQp63, Qp64});
?OUT_CURRENT_P_Qp64(64,
                    Qp64, NewQp64,
                    {Qp49, Qp50, Qp51, Qp52, Qp53,
                     Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                     Qp60, Qp61, Qp62, Qp63, NewQp64});
?OUT_CURRENT_P_Qp80(65,
                    Qp65, NewQp65,
                    {NewQp65, Qp66, Qp67, Qp68, Qp69,
                     Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                     Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_CURRENT_P_Qp80(66,
                    Qp66, NewQp66,
                    {Qp65, NewQp66, Qp67, Qp68, Qp69,
                     Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                     Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_CURRENT_P_Qp80(67,
                    Qp67, NewQp67,
                    {Qp65, Qp66, NewQp67, Qp68, Qp69,
                     Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                     Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_CURRENT_P_Qp80(68,
                    Qp68, NewQp68,
                    {Qp65, Qp66, Qp67, NewQp68, Qp69,
                     Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                     Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_CURRENT_P_Qp80(69,
                    Qp69, NewQp69,
                    {Qp65, Qp66, Qp67, Qp68, NewQp69,
                     Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                     Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_CURRENT_P_Qp80(70,
                    Qp70, NewQp70,
                    {Qp65, Qp66, Qp67, Qp68, Qp69,
                     NewQp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                     Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_CURRENT_P_Qp80(71,
                    Qp71, NewQp71,
                    {Qp65, Qp66, Qp67, Qp68, Qp69,
                     Qp70, NewQp71, Qp72, Qp73, Qp74, Qp75,
                     Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_CURRENT_P_Qp80(72,
                    Qp72, NewQp72,
                    {Qp65, Qp66, Qp67, Qp68, Qp69,
                     Qp70, Qp71, NewQp72, Qp73, Qp74, Qp75,
                     Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_CURRENT_P_Qp80(73,
                    Qp73, NewQp73,
                    {Qp65, Qp66, Qp67, Qp68, Qp69,
                     Qp70, Qp71, Qp72, NewQp73, Qp74, Qp75,
                     Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_CURRENT_P_Qp80(74,
                    Qp74, NewQp74,
                    {Qp65, Qp66, Qp67, Qp68, Qp69,
                     Qp70, Qp71, Qp72, Qp73, NewQp74, Qp75,
                     Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_CURRENT_P_Qp80(75,
                    Qp75, NewQp75,
                    {Qp65, Qp66, Qp67, Qp68, Qp69,
                     Qp70, Qp71, Qp72, Qp73, Qp74, NewQp75,
                     Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_CURRENT_P_Qp80(76,
                    Qp76, NewQp76,
                    {Qp65, Qp66, Qp67, Qp68, Qp69,
                     Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                     NewQp76, Qp77, Qp78, Qp79, Qp80});
?OUT_CURRENT_P_Qp80(77,
                    Qp77, NewQp77,
                    {Qp65, Qp66, Qp67, Qp68, Qp69,
                     Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                     Qp76, NewQp77, Qp78, Qp79, Qp80});
?OUT_CURRENT_P_Qp80(78,
                    Qp78, NewQp78,
                    {Qp65, Qp66, Qp67, Qp68, Qp69,
                     Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                     Qp76, Qp77, NewQp78, Qp79, Qp80});
?OUT_CURRENT_P_Qp80(79,
                    Qp79, NewQp79,
                    {Qp65, Qp66, Qp67, Qp68, Qp69,
                     Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                     Qp76, Qp77, Qp78, NewQp79, Qp80});
?OUT_CURRENT_P_Qp80(80,
                    Qp80, NewQp80,
                    {Qp65, Qp66, Qp67, Qp68, Qp69,
                     Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                     Qp76, Qp77, Qp78, Qp79, NewQp80});
?OUT_CURRENT_P_Qp96(81,
                    Qp81, NewQp81,
                    {NewQp81, Qp82, Qp83, Qp84, Qp85,
                     Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                     Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_CURRENT_P_Qp96(82,
                    Qp82, NewQp82,
                    {Qp81, NewQp82, Qp83, Qp84, Qp85,
                     Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                     Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_CURRENT_P_Qp96(83,
                    Qp83, NewQp83,
                    {Qp81, Qp82, NewQp83, Qp84, Qp85,
                     Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                     Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_CURRENT_P_Qp96(84,
                    Qp84, NewQp84,
                    {Qp81, Qp82, Qp83, NewQp84, Qp85,
                     Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                     Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_CURRENT_P_Qp96(85,
                    Qp85, NewQp85,
                    {Qp81, Qp82, Qp83, Qp84, NewQp85,
                     Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                     Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_CURRENT_P_Qp96(86,
                    Qp86, NewQp86,
                    {Qp81, Qp82, Qp83, Qp84, Qp85,
                     NewQp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                     Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_CURRENT_P_Qp96(87,
                    Qp87, NewQp87,
                    {Qp81, Qp82, Qp83, Qp84, Qp85,
                     Qp86, NewQp87, Qp88, Qp89, Qp90, Qp91,
                     Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_CURRENT_P_Qp96(88,
                    Qp88, NewQp88,
                    {Qp81, Qp82, Qp83, Qp84, Qp85,
                     Qp86, Qp87, NewQp88, Qp89, Qp90, Qp91,
                     Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_CURRENT_P_Qp96(89,
                    Qp89, NewQp89,
                    {Qp81, Qp82, Qp83, Qp84, Qp85,
                     Qp86, Qp87, Qp88, NewQp89, Qp90, Qp91,
                     Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_CURRENT_P_Qp96(90,
                    Qp90, NewQp90,
                    {Qp81, Qp82, Qp83, Qp84, Qp85,
                     Qp86, Qp87, Qp88, Qp89, NewQp90, Qp91,
                     Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_CURRENT_P_Qp96(91,
                    Qp91, NewQp91,
                    {Qp81, Qp82, Qp83, Qp84, Qp85,
                     Qp86, Qp87, Qp88, Qp89, Qp90, NewQp91,
                     Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_CURRENT_P_Qp96(92,
                    Qp92, NewQp92,
                    {Qp81, Qp82, Qp83, Qp84, Qp85,
                     Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                     NewQp92, Qp93, Qp94, Qp95, Qp96});
?OUT_CURRENT_P_Qp96(93,
                    Qp93, NewQp93,
                    {Qp81, Qp82, Qp83, Qp84, Qp85,
                     Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                     Qp92, NewQp93, Qp94, Qp95, Qp96});
?OUT_CURRENT_P_Qp96(94,
                    Qp94, NewQp94,
                    {Qp81, Qp82, Qp83, Qp84, Qp85,
                     Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                     Qp92, Qp93, NewQp94, Qp95, Qp96});
?OUT_CURRENT_P_Qp96(95,
                    Qp95, NewQp95,
                    {Qp81, Qp82, Qp83, Qp84, Qp85,
                     Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                     Qp92, Qp93, Qp94, NewQp95, Qp96});
?OUT_CURRENT_P_Qp96(96,
                    Qp96, NewQp96,
                    {Qp81, Qp82, Qp83, Qp84, Qp85,
                     Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                     Qp92, Qp93, Qp94, Qp95, NewQp96});
?OUT_CURRENT_P_Qp112(97,
                     Qp97, NewQp97,
                     {NewQp97, Qp98, Qp99, Qp100, Qp101,
                      Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                      Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_CURRENT_P_Qp112(98,
                     Qp98, NewQp98,
                     {Qp97, NewQp98, Qp99, Qp100, Qp101,
                      Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                      Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_CURRENT_P_Qp112(99,
                     Qp99, NewQp99,
                     {Qp97, Qp98, NewQp99, Qp100, Qp101,
                      Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                      Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_CURRENT_P_Qp112(100,
                     Qp100, NewQp100,
                     {Qp97, Qp98, Qp99, NewQp100, Qp101,
                      Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                      Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_CURRENT_P_Qp112(101,
                     Qp101, NewQp101,
                     {Qp97, Qp98, Qp99, Qp100, NewQp101,
                      Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                      Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_CURRENT_P_Qp112(102,
                     Qp102, NewQp102,
                     {Qp97, Qp98, Qp99, Qp100, Qp101,
                      NewQp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                      Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_CURRENT_P_Qp112(103,
                     Qp103, NewQp103,
                     {Qp97, Qp98, Qp99, Qp100, Qp101,
                      Qp102, NewQp103, Qp104, Qp105, Qp106, Qp107,
                      Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_CURRENT_P_Qp112(104,
                     Qp104, NewQp104,
                     {Qp97, Qp98, Qp99, Qp100, Qp101,
                      Qp102, Qp103, NewQp104, Qp105, Qp106, Qp107,
                      Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_CURRENT_P_Qp112(105,
                     Qp105, NewQp105,
                     {Qp97, Qp98, Qp99, Qp100, Qp101,
                      Qp102, Qp103, Qp104, NewQp105, Qp106, Qp107,
                      Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_CURRENT_P_Qp112(106,
                     Qp106, NewQp106,
                     {Qp97, Qp98, Qp99, Qp100, Qp101,
                      Qp102, Qp103, Qp104, Qp105, NewQp106, Qp107,
                      Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_CURRENT_P_Qp112(107,
                     Qp107, NewQp107,
                     {Qp97, Qp98, Qp99, Qp100, Qp101,
                      Qp102, Qp103, Qp104, Qp105, Qp106, NewQp107,
                      Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_CURRENT_P_Qp112(108,
                     Qp108, NewQp108,
                     {Qp97, Qp98, Qp99, Qp100, Qp101,
                      Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                      NewQp108, Qp109, Qp110, Qp111, Qp112});
?OUT_CURRENT_P_Qp112(109,
                     Qp109, NewQp109,
                     {Qp97, Qp98, Qp99, Qp100, Qp101,
                      Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                      Qp108, NewQp109, Qp110, Qp111, Qp112});
?OUT_CURRENT_P_Qp112(110,
                     Qp110, NewQp110,
                     {Qp97, Qp98, Qp99, Qp100, Qp101,
                      Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                      Qp108, Qp109, NewQp110, Qp111, Qp112});
?OUT_CURRENT_P_Qp112(111,
                     Qp111, NewQp111,
                     {Qp97, Qp98, Qp99, Qp100, Qp101,
                      Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                      Qp108, Qp109, Qp110, NewQp111, Qp112});
?OUT_CURRENT_P_Qp112(112,
                     Qp112, NewQp112,
                     {Qp97, Qp98, Qp99, Qp100, Qp101,
                      Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                      Qp108, Qp109, Qp110, Qp111, NewQp112});
?OUT_CURRENT_P_Qp128(113,
                     Qp113, NewQp113,
                     {NewQp113, Qp114, Qp115, Qp116, Qp117,
                      Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                      Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_CURRENT_P_Qp128(114,
                     Qp114, NewQp114,
                     {Qp113, NewQp114, Qp115, Qp116, Qp117,
                      Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                      Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_CURRENT_P_Qp128(115,
                     Qp115, NewQp115,
                     {Qp113, Qp114, NewQp115, Qp116, Qp117,
                      Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                      Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_CURRENT_P_Qp128(116,
                     Qp116, NewQp116,
                     {Qp113, Qp114, Qp115, NewQp116, Qp117,
                      Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                      Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_CURRENT_P_Qp128(117,
                     Qp117, NewQp117,
                     {Qp113, Qp114, Qp115, Qp116, NewQp117,
                      Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                      Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_CURRENT_P_Qp128(118,
                     Qp118, NewQp118,
                     {Qp113, Qp114, Qp115, Qp116, Qp117,
                      NewQp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                      Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_CURRENT_P_Qp128(119,
                     Qp119, NewQp119,
                     {Qp113, Qp114, Qp115, Qp116, Qp117,
                      Qp118, NewQp119, Qp120, Qp121, Qp122, Qp123,
                      Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_CURRENT_P_Qp128(120,
                     Qp120, NewQp120,
                     {Qp113, Qp114, Qp115, Qp116, Qp117,
                      Qp118, Qp119, NewQp120, Qp121, Qp122, Qp123,
                      Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_CURRENT_P_Qp128(121,
                     Qp121, NewQp121,
                     {Qp113, Qp114, Qp115, Qp116, Qp117,
                      Qp118, Qp119, Qp120, NewQp121, Qp122, Qp123,
                      Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_CURRENT_P_Qp128(122,
                     Qp122, NewQp122,
                     {Qp113, Qp114, Qp115, Qp116, Qp117,
                      Qp118, Qp119, Qp120, Qp121, NewQp122, Qp123,
                      Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_CURRENT_P_Qp128(123,
                     Qp123, NewQp123,
                     {Qp113, Qp114, Qp115, Qp116, Qp117,
                      Qp118, Qp119, Qp120, Qp121, Qp122, NewQp123,
                      Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_CURRENT_P_Qp128(124,
                     Qp124, NewQp124,
                     {Qp113, Qp114, Qp115, Qp116, Qp117,
                      Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                      NewQp124, Qp125, Qp126, Qp127, Qp128});
?OUT_CURRENT_P_Qp128(125,
                     Qp125, NewQp125,
                     {Qp113, Qp114, Qp115, Qp116, Qp117,
                      Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                      Qp124, NewQp125, Qp126, Qp127, Qp128});
?OUT_CURRENT_P_Qp128(126,
                     Qp126, NewQp126,
                     {Qp113, Qp114, Qp115, Qp116, Qp117,
                      Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                      Qp124, Qp125, NewQp126, Qp127, Qp128});
?OUT_CURRENT_P_Qp128(127,
                     Qp127, NewQp127,
                     {Qp113, Qp114, Qp115, Qp116, Qp117,
                      Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                      Qp124, Qp125, Qp126, NewQp127, Qp128});
out_current_p(128,
              {_,
               Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
               Q0,
               Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112,
               {Qp113, Qp114, Qp115, Qp116, Qp117, Qp118, Qp119, Qp120,
                Qp121, Qp122, Qp123, Qp124, Qp125, Qp126, Qp127, Qp128}}) ->
    case queue:out(Qp128) of
        {empty, _} ->
            {empty,
             {empty,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112,
              {Qp113, Qp114, Qp115, Qp116, Qp117, Qp118, Qp119, Qp120,
               Qp121, Qp122, Qp123, Qp124, Qp125, Qp126, Qp127, Qp128}}};
        {{value, X}, NewQp128} ->
            {{value, X, 128},
             {128,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112,
              {Qp113, Qp114, Qp115, Qp116, Qp117, Qp118, Qp119, Qp120,
               Qp121, Qp122, Qp123, Qp124, Qp125, Qp126, Qp127, NewQp128}}}
    end.

-define(OUT_SPECIFIC_Qn128(P, V1, V2, V3),
out_specific(P,
             {Pc,
              {Qn128, Qn127, Qn126, Qn125, Qn124, Qn123, Qn122, Qn121,
               Qn120, Qn119, Qn118, Qn117, Qn116, Qn115, Qn114, Qn113},
              Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}) ->
    {Value, V2} = queue:out(V1),
    {Value,
     {Pc,
      V3,
      Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
      Q0,
      Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}).
-define(OUT_SPECIFIC_Qn112(P, V1, V2, V3),
out_specific(P,
             {Pc,
              Qn128,
              {Qn112, Qn111, Qn110, Qn109, Qn108, Qn107, Qn106, Qn105,
               Qn104, Qn103, Qn102, Qn101, Qn100, Qn99, Qn98, Qn97},
              Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}) ->
    {Value, V2} = queue:out(V1),
    {Value,
     {Pc,
      Qn128,
      V3,
      Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
      Q0,
      Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}).
-define(OUT_SPECIFIC_Qn96(P, V1, V2, V3),
out_specific(P,
             {Pc,
              Qn128, Qn112,
              {Qn96, Qn95, Qn94, Qn93, Qn92, Qn91, Qn90, Qn89,
               Qn88, Qn87, Qn86, Qn85, Qn84, Qn83, Qn82, Qn81},
              Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}) ->
    {Value, V2} = queue:out(V1),
    {Value,
     {Pc,
      Qn128, Qn112,
      V3,
      Qn80, Qn64, Qn48, Qn32, Qn16,
      Q0,
      Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}).
-define(OUT_SPECIFIC_Qn80(P, V1, V2, V3),
out_specific(P,
             {Pc,
              Qn128, Qn112, Qn96,
              {Qn80, Qn79, Qn78, Qn77, Qn76, Qn75, Qn74, Qn73,
               Qn72, Qn71, Qn70, Qn69, Qn68, Qn67, Qn66, Qn65},
              Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}) ->
    {Value, V2} = queue:out(V1),
    {Value,
     {Pc,
      Qn128, Qn112, Qn96,
      V3,
      Qn64, Qn48, Qn32, Qn16,
      Q0,
      Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}).
-define(OUT_SPECIFIC_Qn64(P, V1, V2, V3),
out_specific(P,
             {Pc,
              Qn128, Qn112, Qn96, Qn80,
              {Qn64, Qn63, Qn62, Qn61, Qn60, Qn59, Qn58, Qn57,
               Qn56, Qn55, Qn54, Qn53, Qn52, Qn51, Qn50, Qn49},
              Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}) ->
    {Value, V2} = queue:out(V1),
    {Value,
     {Pc,
      Qn128, Qn112, Qn96, Qn80,
      V3,
      Qn48, Qn32, Qn16,
      Q0,
      Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}).
-define(OUT_SPECIFIC_Qn48(P, V1, V2, V3),
out_specific(P,
             {Pc,
              Qn128, Qn112, Qn96, Qn80, Qn64,
              {Qn48, Qn47, Qn46, Qn45, Qn44, Qn43, Qn42, Qn41,
               Qn40, Qn39, Qn38, Qn37, Qn36, Qn35, Qn34, Qn33},
              Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}) ->
    {Value, V2} = queue:out(V1),
    {Value,
     {Pc,
      Qn128, Qn112, Qn96, Qn80, Qn64,
      V3,
      Qn32, Qn16,
      Q0,
      Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}).
-define(OUT_SPECIFIC_Qn32(P, V1, V2, V3),
out_specific(P,
             {Pc,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48,
              {Qn32, Qn31, Qn30, Qn29, Qn28, Qn27, Qn26, Qn25,
               Qn24, Qn23, Qn22, Qn21, Qn20, Qn19, Qn18, Qn17},
              Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}) ->
    {Value, V2} = queue:out(V1),
    {Value,
     {Pc,
      Qn128, Qn112, Qn96, Qn80, Qn64, Qn48,
      V3,
      Qn16,
      Q0,
      Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}).
-define(OUT_SPECIFIC_Qn16(P, V1, V2, V3),
out_specific(P,
             {Pc,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32,
              {Qn16, Qn15, Qn14, Qn13, Qn12, Qn11, Qn10, Qn9,
               Qn8, Qn7, Qn6, Qn5, Qn4, Qn3, Qn2, Qn1},
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}) ->
    {Value, V2} = queue:out(V1),
    {Value,
     {Pc,
      Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32,
      V3,
      Q0,
      Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}).
-define(OUT_SPECIFIC_Qp16(P, V1, V2, V3),
out_specific(P,
             {Pc,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              {Qp1, Qp2, Qp3, Qp4, Qp5, Qp6, Qp7, Qp8,
               Qp9, Qp10, Qp11, Qp12, Qp13, Qp14, Qp15, Qp16},
              Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}) ->
    {Value, V2} = queue:out(V1),
    {Value,
     {Pc,
      Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
      Q0,
      V3,
      Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}).
-define(OUT_SPECIFIC_Qp32(P, V1, V2, V3),
out_specific(P,
             {Pc,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16,
              {Qp17, Qp18, Qp19, Qp20, Qp21, Qp22, Qp23, Qp24,
               Qp25, Qp26, Qp27, Qp28, Qp29, Qp30, Qp31, Qp32},
              Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}) ->
    {Value, V2} = queue:out(V1),
    {Value,
     {Pc,
      Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
      Q0,
      Qp16,
      V3,
      Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}}).
-define(OUT_SPECIFIC_Qp48(P, V1, V2, V3),
out_specific(P,
             {Pc,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32,
              {Qp33, Qp34, Qp35, Qp36, Qp37, Qp38, Qp39, Qp40,
               Qp41, Qp42, Qp43, Qp44, Qp45, Qp46, Qp47, Qp48},
              Qp64, Qp80, Qp96, Qp112, Qp128}) ->
    {Value, V2} = queue:out(V1),
    {Value,
     {Pc,
      Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
      Q0,
      Qp16, Qp32,
      V3,
      Qp64, Qp80, Qp96, Qp112, Qp128}}).
-define(OUT_SPECIFIC_Qp64(P, V1, V2, V3),
out_specific(P,
             {Pc,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48,
              {Qp49, Qp50, Qp51, Qp52, Qp53, Qp54, Qp55, Qp56,
               Qp57, Qp58, Qp59, Qp60, Qp61, Qp62, Qp63, Qp64},
              Qp80, Qp96, Qp112, Qp128}) ->
    {Value, V2} = queue:out(V1),
    {Value,
     {Pc,
      Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
      Q0,
      Qp16, Qp32, Qp48,
      V3,
      Qp80, Qp96, Qp112, Qp128}}).
-define(OUT_SPECIFIC_Qp80(P, V1, V2, V3),
out_specific(P,
              {Pc,
               Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
               Q0,
               Qp16, Qp32, Qp48, Qp64,
               {Qp65, Qp66, Qp67, Qp68, Qp69, Qp70, Qp71, Qp72,
                Qp73, Qp74, Qp75, Qp76, Qp77, Qp78, Qp79, Qp80},
               Qp96, Qp112, Qp128}) ->
    {Value, V2} = queue:out(V1),
    {Value,
     {Pc,
      Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
      Q0,
      Qp16, Qp32, Qp48, Qp64,
      V3,
      Qp96, Qp112, Qp128}}).
-define(OUT_SPECIFIC_Qp96(P, V1, V2, V3),
out_specific(P,
             {Pc,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80,
              {Qp81, Qp82, Qp83, Qp84, Qp85, Qp86, Qp87, Qp88,
               Qp89, Qp90, Qp91, Qp92, Qp93, Qp94, Qp95, Qp96},
              Qp112, Qp128}) ->
    {Value, V2} = queue:out(V1),
    {Value,
     {Pc,
      Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
      Q0,
      Qp16, Qp32, Qp48, Qp64, Qp80,
      V3,
      Qp112, Qp128}}).
-define(OUT_SPECIFIC_Qp112(P, V1, V2, V3),
out_specific(P,
             {Pc,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96,
              {Qp97, Qp98, Qp99, Qp100, Qp101, Qp102, Qp103, Qp104,
               Qp105, Qp106, Qp107, Qp108, Qp109, Qp110, Qp111, Qp112},
              Qp128}) ->
    {Value, V2} = queue:out(V1),
    {Value,
     {Pc,
      Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
      Q0,
      Qp16, Qp32, Qp48, Qp64, Qp80, Qp96,
      V3,
      Qp128}}).
-define(OUT_SPECIFIC_Qp128(P, V1, V2, V3),
out_specific(P,
             {Pc,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112,
              {Qp113, Qp114, Qp115, Qp116, Qp117, Qp118, Qp119, Qp120,
               Qp121, Qp122, Qp123, Qp124, Qp125, Qp126, Qp127, Qp128}}) ->
    {Value, V2} = queue:out(V1),
    {Value,
     {Pc,
      Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
      Q0,
      Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112,
      V3}}).

?OUT_SPECIFIC_Qn128(-128,
                    Qn128, NewQn128,
                    {NewQn128, Qn127, Qn126, Qn125, Qn124,
                     Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                     Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_SPECIFIC_Qn128(-127,
                    Qn127, NewQn127,
                    {Qn128, NewQn127, Qn126, Qn125, Qn124,
                     Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                     Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_SPECIFIC_Qn128(-126,
                    Qn126, NewQn126,
                    {Qn128, Qn127, NewQn126, Qn125, Qn124,
                     Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                     Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_SPECIFIC_Qn128(-125,
                    Qn125, NewQn125,
                    {Qn128, Qn127, Qn126, NewQn125, Qn124,
                     Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                     Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_SPECIFIC_Qn128(-124,
                    Qn124, NewQn124,
                    {Qn128, Qn127, Qn126, Qn125, NewQn124,
                     Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                     Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_SPECIFIC_Qn128(-123,
                    Qn123, NewQn123,
                    {Qn128, Qn127, Qn126, Qn125, Qn124,
                     NewQn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                     Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_SPECIFIC_Qn128(-122,
                    Qn122, NewQn122,
                    {Qn128, Qn127, Qn126, Qn125, Qn124,
                     Qn123, NewQn122, Qn121, Qn120, Qn119, Qn118,
                     Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_SPECIFIC_Qn128(-121,
                    Qn121, NewQn121,
                    {Qn128, Qn127, Qn126, Qn125, Qn124,
                     Qn123, Qn122, NewQn121, Qn120, Qn119, Qn118,
                     Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_SPECIFIC_Qn128(-120,
                    Qn120, NewQn120,
                    {Qn128, Qn127, Qn126, Qn125, Qn124,
                     Qn123, Qn122, Qn121, NewQn120, Qn119, Qn118,
                     Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_SPECIFIC_Qn128(-119,
                    Qn119, NewQn119,
                    {Qn128, Qn127, Qn126, Qn125, Qn124,
                     Qn123, Qn122, Qn121, Qn120, NewQn119, Qn118,
                     Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_SPECIFIC_Qn128(-118,
                    Qn118, NewQn118,
                    {Qn128, Qn127, Qn126, Qn125, Qn124,
                     Qn123, Qn122, Qn121, Qn120, Qn119, NewQn118,
                     Qn117, Qn116, Qn115, Qn114, Qn113});
?OUT_SPECIFIC_Qn128(-117,
                    Qn117, NewQn117,
                    {Qn128, Qn127, Qn126, Qn125, Qn124,
                     Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                     NewQn117, Qn116, Qn115, Qn114, Qn113});
?OUT_SPECIFIC_Qn128(-116,
                    Qn116, NewQn116,
                    {Qn128, Qn127, Qn126, Qn125, Qn124,
                     Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                     Qn117, NewQn116, Qn115, Qn114, Qn113});
?OUT_SPECIFIC_Qn128(-115,
                    Qn115, NewQn115,
                    {Qn128, Qn127, Qn126, Qn125, Qn124,
                     Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                     Qn117, Qn116, NewQn115, Qn114, Qn113});
?OUT_SPECIFIC_Qn128(-114,
                    Qn114, NewQn114,
                    {Qn128, Qn127, Qn126, Qn125, Qn124,
                     Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                     Qn117, Qn116, Qn115, NewQn114, Qn113});
?OUT_SPECIFIC_Qn128(-113,
                    Qn113, NewQn113,
                    {Qn128, Qn127, Qn126, Qn125, Qn124,
                     Qn123, Qn122, Qn121, Qn120, Qn119, Qn118,
                     Qn117, Qn116, Qn115, Qn114, NewQn113});
?OUT_SPECIFIC_Qn112(-112,
                    Qn112, NewQn112,
                    {NewQn112, Qn111, Qn110, Qn109, Qn108,
                     Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                     Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_SPECIFIC_Qn112(-111,
                    Qn111, NewQn111,
                    {Qn112, NewQn111, Qn110, Qn109, Qn108,
                     Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                     Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_SPECIFIC_Qn112(-110,
                    Qn110, NewQn110,
                    {Qn112, Qn111, NewQn110, Qn109, Qn108,
                     Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                     Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_SPECIFIC_Qn112(-109,
                    Qn109, NewQn109,
                    {Qn112, Qn111, Qn110, NewQn109, Qn108,
                     Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                     Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_SPECIFIC_Qn112(-108,
                    Qn108, NewQn108,
                    {Qn112, Qn111, Qn110, Qn109, NewQn108,
                     Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                     Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_SPECIFIC_Qn112(-107,
                    Qn107, NewQn107,
                    {Qn112, Qn111, Qn110, Qn109, Qn108,
                     NewQn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                     Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_SPECIFIC_Qn112(-106,
                    Qn106, NewQn106,
                    {Qn112, Qn111, Qn110, Qn109, Qn108,
                     Qn107, NewQn106, Qn105, Qn104, Qn103, Qn102,
                     Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_SPECIFIC_Qn112(-105,
                    Qn105, NewQn105,
                    {Qn112, Qn111, Qn110, Qn109, Qn108,
                     Qn107, Qn106, NewQn105, Qn104, Qn103, Qn102,
                     Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_SPECIFIC_Qn112(-104,
                    Qn104, NewQn104,
                    {Qn112, Qn111, Qn110, Qn109, Qn108,
                     Qn107, Qn106, Qn105, NewQn104, Qn103, Qn102,
                     Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_SPECIFIC_Qn112(-103,
                    Qn103, NewQn103,
                    {Qn112, Qn111, Qn110, Qn109, Qn108,
                     Qn107, Qn106, Qn105, Qn104, NewQn103, Qn102,
                     Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_SPECIFIC_Qn112(-102,
                    Qn102, NewQn102,
                    {Qn112, Qn111, Qn110, Qn109, Qn108,
                     Qn107, Qn106, Qn105, Qn104, Qn103, NewQn102,
                     Qn101, Qn100, Qn99, Qn98, Qn97});
?OUT_SPECIFIC_Qn112(-101,
                    Qn101, NewQn101,
                    {Qn112, Qn111, Qn110, Qn109, Qn108,
                     Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                     NewQn101, Qn100, Qn99, Qn98, Qn97});
?OUT_SPECIFIC_Qn112(-100,
                    Qn100, NewQn100,
                    {Qn112, Qn111, Qn110, Qn109, Qn108,
                     Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                     Qn101, NewQn100, Qn99, Qn98, Qn97});
?OUT_SPECIFIC_Qn112(-99,
                    Qn99, NewQn99,
                    {Qn112, Qn111, Qn110, Qn109, Qn108,
                     Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                     Qn101, Qn100, NewQn99, Qn98, Qn97});
?OUT_SPECIFIC_Qn112(-98,
                    Qn98, NewQn98,
                    {Qn112, Qn111, Qn110, Qn109, Qn108,
                     Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                     Qn101, Qn100, Qn99, NewQn98, Qn97});
?OUT_SPECIFIC_Qn112(-97,
                    Qn97, NewQn97,
                    {Qn112, Qn111, Qn110, Qn109, Qn108,
                     Qn107, Qn106, Qn105, Qn104, Qn103, Qn102,
                     Qn101, Qn100, Qn99, Qn98, NewQn97});
?OUT_SPECIFIC_Qn96(-96,
                   Qn96, NewQn96,
                   {NewQn96, Qn95, Qn94, Qn93, Qn92,
                    Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                    Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_SPECIFIC_Qn96(-95,
                   Qn95, NewQn95,
                   {Qn96, NewQn95, Qn94, Qn93, Qn92,
                    Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                    Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_SPECIFIC_Qn96(-94,
                   Qn94, NewQn94,
                   {Qn96, Qn95, NewQn94, Qn93, Qn92,
                    Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                    Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_SPECIFIC_Qn96(-93,
                   Qn93, NewQn93,
                   {Qn96, Qn95, Qn94, NewQn93, Qn92,
                    Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                    Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_SPECIFIC_Qn96(-92,
                   Qn92, NewQn92,
                   {Qn96, Qn95, Qn94, Qn93, NewQn92,
                    Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                    Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_SPECIFIC_Qn96(-91,
                   Qn91, NewQn91,
                   {Qn96, Qn95, Qn94, Qn93, Qn92,
                    NewQn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                    Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_SPECIFIC_Qn96(-90,
                   Qn90, NewQn90,
                   {Qn96, Qn95, Qn94, Qn93, Qn92,
                    Qn91, NewQn90, Qn89, Qn88, Qn87, Qn86,
                    Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_SPECIFIC_Qn96(-89,
                   Qn89, NewQn89,
                   {Qn96, Qn95, Qn94, Qn93, Qn92,
                    Qn91, Qn90, NewQn89, Qn88, Qn87, Qn86,
                    Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_SPECIFIC_Qn96(-88,
                   Qn88, NewQn88,
                   {Qn96, Qn95, Qn94, Qn93, Qn92,
                    Qn91, Qn90, Qn89, NewQn88, Qn87, Qn86,
                    Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_SPECIFIC_Qn96(-87,
                   Qn87, NewQn87,
                   {Qn96, Qn95, Qn94, Qn93, Qn92,
                    Qn91, Qn90, Qn89, Qn88, NewQn87, Qn86,
                    Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_SPECIFIC_Qn96(-86,
                   Qn86, NewQn86,
                   {Qn96, Qn95, Qn94, Qn93, Qn92,
                    Qn91, Qn90, Qn89, Qn88, Qn87, NewQn86,
                    Qn85, Qn84, Qn83, Qn82, Qn81});
?OUT_SPECIFIC_Qn96(-85,
                   Qn85, NewQn85,
                   {Qn96, Qn95, Qn94, Qn93, Qn92,
                    Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                    NewQn85, Qn84, Qn83, Qn82, Qn81});
?OUT_SPECIFIC_Qn96(-84,
                   Qn84, NewQn84,
                   {Qn96, Qn95, Qn94, Qn93, Qn92,
                    Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                    Qn85, NewQn84, Qn83, Qn82, Qn81});
?OUT_SPECIFIC_Qn96(-83,
                   Qn83, NewQn83,
                   {Qn96, Qn95, Qn94, Qn93, Qn92,
                    Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                    Qn85, Qn84, NewQn83, Qn82, Qn81});
?OUT_SPECIFIC_Qn96(-82,
                   Qn82, NewQn82,
                   {Qn96, Qn95, Qn94, Qn93, Qn92,
                    Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                    Qn85, Qn84, Qn83, NewQn82, Qn81});
?OUT_SPECIFIC_Qn96(-81,
                   Qn81, NewQn81,
                   {Qn96, Qn95, Qn94, Qn93, Qn92,
                    Qn91, Qn90, Qn89, Qn88, Qn87, Qn86,
                    Qn85, Qn84, Qn83, Qn82, NewQn81});
?OUT_SPECIFIC_Qn80(-80,
                   Qn80, NewQn80,
                   {NewQn80, Qn79, Qn78, Qn77, Qn76,
                    Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                    Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_SPECIFIC_Qn80(-79,
                   Qn79, NewQn79,
                   {Qn80, NewQn79, Qn78, Qn77, Qn76,
                    Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                    Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_SPECIFIC_Qn80(-78,
                   Qn78, NewQn78,
                   {Qn80, Qn79, NewQn78, Qn77, Qn76,
                    Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                    Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_SPECIFIC_Qn80(-77,
                   Qn77, NewQn77,
                   {Qn80, Qn79, Qn78, NewQn77, Qn76,
                    Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                    Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_SPECIFIC_Qn80(-76,
                   Qn76, NewQn76,
                   {Qn80, Qn79, Qn78, Qn77, NewQn76,
                    Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                    Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_SPECIFIC_Qn80(-75,
                   Qn75, NewQn75,
                   {Qn80, Qn79, Qn78, Qn77, Qn76,
                    NewQn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                    Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_SPECIFIC_Qn80(-74,
                   Qn74, NewQn74,
                   {Qn80, Qn79, Qn78, Qn77, Qn76,
                    Qn75, NewQn74, Qn73, Qn72, Qn71, Qn70,
                    Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_SPECIFIC_Qn80(-73,
                   Qn73, NewQn73,
                   {Qn80, Qn79, Qn78, Qn77, Qn76,
                    Qn75, Qn74, NewQn73, Qn72, Qn71, Qn70,
                    Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_SPECIFIC_Qn80(-72,
                   Qn72, NewQn72,
                   {Qn80, Qn79, Qn78, Qn77, Qn76,
                    Qn75, Qn74, Qn73, NewQn72, Qn71, Qn70,
                    Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_SPECIFIC_Qn80(-71,
                   Qn71, NewQn71,
                   {Qn80, Qn79, Qn78, Qn77, Qn76,
                    Qn75, Qn74, Qn73, Qn72, NewQn71, Qn70,
                    Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_SPECIFIC_Qn80(-70,
                   Qn70, NewQn70,
                   {Qn80, Qn79, Qn78, Qn77, Qn76,
                    Qn75, Qn74, Qn73, Qn72, Qn71, NewQn70,
                    Qn69, Qn68, Qn67, Qn66, Qn65});
?OUT_SPECIFIC_Qn80(-69,
                   Qn69, NewQn69,
                   {Qn80, Qn79, Qn78, Qn77, Qn76,
                    Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                    NewQn69, Qn68, Qn67, Qn66, Qn65});
?OUT_SPECIFIC_Qn80(-68,
                   Qn68, NewQn68,
                   {Qn80, Qn79, Qn78, Qn77, Qn76,
                    Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                    Qn69, NewQn68, Qn67, Qn66, Qn65});
?OUT_SPECIFIC_Qn80(-67,
                   Qn67, NewQn67,
                   {Qn80, Qn79, Qn78, Qn77, Qn76,
                    Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                    Qn69, Qn68, NewQn67, Qn66, Qn65});
?OUT_SPECIFIC_Qn80(-66,
                   Qn66, NewQn66,
                   {Qn80, Qn79, Qn78, Qn77, Qn76,
                    Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                    Qn69, Qn68, Qn67, NewQn66, Qn65});
?OUT_SPECIFIC_Qn80(-65,
                   Qn65, NewQn65,
                   {Qn80, Qn79, Qn78, Qn77, Qn76,
                    Qn75, Qn74, Qn73, Qn72, Qn71, Qn70,
                    Qn69, Qn68, Qn67, Qn66, NewQn65});
?OUT_SPECIFIC_Qn64(-64,
                   Qn64, NewQn64,
                   {NewQn64, Qn63, Qn62, Qn61, Qn60,
                    Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                    Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_SPECIFIC_Qn64(-63,
                   Qn63, NewQn63,
                   {Qn64, NewQn63, Qn62, Qn61, Qn60,
                    Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                    Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_SPECIFIC_Qn64(-62,
                   Qn62, NewQn62,
                   {Qn64, Qn63, NewQn62, Qn61, Qn60,
                    Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                    Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_SPECIFIC_Qn64(-61,
                   Qn61, NewQn61,
                   {Qn64, Qn63, Qn62, NewQn61, Qn60,
                    Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                    Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_SPECIFIC_Qn64(-60,
                   Qn60, NewQn60,
                   {Qn64, Qn63, Qn62, Qn61, NewQn60,
                    Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                    Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_SPECIFIC_Qn64(-59,
                   Qn59, NewQn59,
                   {Qn64, Qn63, Qn62, Qn61, Qn60,
                    NewQn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                    Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_SPECIFIC_Qn64(-58,
                   Qn58, NewQn58,
                   {Qn64, Qn63, Qn62, Qn61, Qn60,
                    Qn59, NewQn58, Qn57, Qn56, Qn55, Qn54,
                    Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_SPECIFIC_Qn64(-57,
                   Qn57, NewQn57,
                   {Qn64, Qn63, Qn62, Qn61, Qn60,
                    Qn59, Qn58, NewQn57, Qn56, Qn55, Qn54,
                    Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_SPECIFIC_Qn64(-56,
                   Qn56, NewQn56,
                   {Qn64, Qn63, Qn62, Qn61, Qn60,
                    Qn59, Qn58, Qn57, NewQn56, Qn55, Qn54,
                    Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_SPECIFIC_Qn64(-55,
                   Qn55, NewQn55,
                   {Qn64, Qn63, Qn62, Qn61, Qn60,
                    Qn59, Qn58, Qn57, Qn56, NewQn55, Qn54,
                    Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_SPECIFIC_Qn64(-54,
                   Qn54, NewQn54,
                   {Qn64, Qn63, Qn62, Qn61, Qn60,
                    Qn59, Qn58, Qn57, Qn56, Qn55, NewQn54,
                    Qn53, Qn52, Qn51, Qn50, Qn49});
?OUT_SPECIFIC_Qn64(-53,
                   Qn53, NewQn53,
                   {Qn64, Qn63, Qn62, Qn61, Qn60,
                    Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                    NewQn53, Qn52, Qn51, Qn50, Qn49});
?OUT_SPECIFIC_Qn64(-52,
                   Qn52, NewQn52,
                   {Qn64, Qn63, Qn62, Qn61, Qn60,
                    Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                    Qn53, NewQn52, Qn51, Qn50, Qn49});
?OUT_SPECIFIC_Qn64(-51,
                   Qn51, NewQn51,
                   {Qn64, Qn63, Qn62, Qn61, Qn60,
                    Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                    Qn53, Qn52, NewQn51, Qn50, Qn49});
?OUT_SPECIFIC_Qn64(-50,
                   Qn50, NewQn50,
                   {Qn64, Qn63, Qn62, Qn61, Qn60,
                    Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                    Qn53, Qn52, Qn51, NewQn50, Qn49});
?OUT_SPECIFIC_Qn64(-49,
                   Qn49, NewQn49,
                   {Qn64, Qn63, Qn62, Qn61, Qn60,
                    Qn59, Qn58, Qn57, Qn56, Qn55, Qn54,
                    Qn53, Qn52, Qn51, Qn50, NewQn49});
?OUT_SPECIFIC_Qn48(-48,
                   Qn48, NewQn48,
                   {NewQn48, Qn47, Qn46, Qn45, Qn44,
                    Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                    Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_SPECIFIC_Qn48(-47,
                   Qn47, NewQn47,
                   {Qn48, NewQn47, Qn46, Qn45, Qn44,
                    Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                    Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_SPECIFIC_Qn48(-46,
                   Qn46, NewQn46,
                   {Qn48, Qn47, NewQn46, Qn45, Qn44,
                    Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                    Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_SPECIFIC_Qn48(-45,
                   Qn45, NewQn45,
                   {Qn48, Qn47, Qn46, NewQn45, Qn44,
                    Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                    Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_SPECIFIC_Qn48(-44,
                   Qn44, NewQn44,
                   {Qn48, Qn47, Qn46, Qn45, NewQn44,
                    Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                    Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_SPECIFIC_Qn48(-43,
                   Qn43, NewQn43,
                   {Qn48, Qn47, Qn46, Qn45, Qn44,
                    NewQn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                    Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_SPECIFIC_Qn48(-42,
                   Qn42, NewQn42,
                   {Qn48, Qn47, Qn46, Qn45, Qn44,
                    Qn43, NewQn42, Qn41, Qn40, Qn39, Qn38,
                    Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_SPECIFIC_Qn48(-41,
                   Qn41, NewQn41,
                   {Qn48, Qn47, Qn46, Qn45, Qn44,
                    Qn43, Qn42, NewQn41, Qn40, Qn39, Qn38,
                    Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_SPECIFIC_Qn48(-40,
                   Qn40, NewQn40,
                   {Qn48, Qn47, Qn46, Qn45, Qn44,
                    Qn43, Qn42, Qn41, NewQn40, Qn39, Qn38,
                    Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_SPECIFIC_Qn48(-39,
                   Qn39, NewQn39,
                   {Qn48, Qn47, Qn46, Qn45, Qn44,
                    Qn43, Qn42, Qn41, Qn40, NewQn39, Qn38,
                    Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_SPECIFIC_Qn48(-38,
                   Qn38, NewQn38,
                   {Qn48, Qn47, Qn46, Qn45, Qn44,
                    Qn43, Qn42, Qn41, Qn40, Qn39, NewQn38,
                    Qn37, Qn36, Qn35, Qn34, Qn33});
?OUT_SPECIFIC_Qn48(-37,
                   Qn37, NewQn37,
                   {Qn48, Qn47, Qn46, Qn45, Qn44,
                    Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                    NewQn37, Qn36, Qn35, Qn34, Qn33});
?OUT_SPECIFIC_Qn48(-36,
                   Qn36, NewQn36,
                   {Qn48, Qn47, Qn46, Qn45, Qn44,
                    Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                    Qn37, NewQn36, Qn35, Qn34, Qn33});
?OUT_SPECIFIC_Qn48(-35,
                   Qn35, NewQn35,
                   {Qn48, Qn47, Qn46, Qn45, Qn44,
                    Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                    Qn37, Qn36, NewQn35, Qn34, Qn33});
?OUT_SPECIFIC_Qn48(-34,
                   Qn34, NewQn34,
                   {Qn48, Qn47, Qn46, Qn45, Qn44,
                    Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                    Qn37, Qn36, Qn35, NewQn34, Qn33});
?OUT_SPECIFIC_Qn48(-33,
                   Qn33, NewQn33,
                   {Qn48, Qn47, Qn46, Qn45, Qn44,
                    Qn43, Qn42, Qn41, Qn40, Qn39, Qn38,
                    Qn37, Qn36, Qn35, Qn34, NewQn33});
?OUT_SPECIFIC_Qn32(-32,
                   Qn32, NewQn32,
                   {NewQn32, Qn31, Qn30, Qn29, Qn28,
                    Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                    Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_SPECIFIC_Qn32(-31,
                   Qn31, NewQn31,
                   {Qn32, NewQn31, Qn30, Qn29, Qn28,
                    Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                    Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_SPECIFIC_Qn32(-30,
                   Qn30, NewQn30,
                   {Qn32, Qn31, NewQn30, Qn29, Qn28,
                    Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                    Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_SPECIFIC_Qn32(-29,
                   Qn29, NewQn29,
                   {Qn32, Qn31, Qn30, NewQn29, Qn28,
                    Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                    Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_SPECIFIC_Qn32(-28,
                   Qn28, NewQn28,
                   {Qn32, Qn31, Qn30, Qn29, NewQn28,
                    Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                    Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_SPECIFIC_Qn32(-27,
                   Qn27, NewQn27,
                   {Qn32, Qn31, Qn30, Qn29, Qn28,
                    NewQn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                    Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_SPECIFIC_Qn32(-26,
                   Qn26, NewQn26,
                   {Qn32, Qn31, Qn30, Qn29, Qn28,
                    Qn27, NewQn26, Qn25, Qn24, Qn23, Qn22,
                    Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_SPECIFIC_Qn32(-25,
                   Qn25, NewQn25,
                   {Qn32, Qn31, Qn30, Qn29, Qn28,
                    Qn27, Qn26, NewQn25, Qn24, Qn23, Qn22,
                    Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_SPECIFIC_Qn32(-24,
                   Qn24, NewQn24,
                   {Qn32, Qn31, Qn30, Qn29, Qn28,
                    Qn27, Qn26, Qn25, NewQn24, Qn23, Qn22,
                    Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_SPECIFIC_Qn32(-23,
                   Qn23, NewQn23,
                   {Qn32, Qn31, Qn30, Qn29, Qn28,
                    Qn27, Qn26, Qn25, Qn24, NewQn23, Qn22,
                    Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_SPECIFIC_Qn32(-22,
                   Qn22, NewQn22,
                   {Qn32, Qn31, Qn30, Qn29, Qn28,
                    Qn27, Qn26, Qn25, Qn24, Qn23, NewQn22,
                    Qn21, Qn20, Qn19, Qn18, Qn17});
?OUT_SPECIFIC_Qn32(-21,
                   Qn21, NewQn21,
                   {Qn32, Qn31, Qn30, Qn29, Qn28,
                    Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                    NewQn21, Qn20, Qn19, Qn18, Qn17});
?OUT_SPECIFIC_Qn32(-20,
                   Qn20, NewQn20,
                   {Qn32, Qn31, Qn30, Qn29, Qn28,
                    Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                    Qn21, NewQn20, Qn19, Qn18, Qn17});
?OUT_SPECIFIC_Qn32(-19,
                   Qn19, NewQn19,
                   {Qn32, Qn31, Qn30, Qn29, Qn28,
                    Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                    Qn21, Qn20, NewQn19, Qn18, Qn17});
?OUT_SPECIFIC_Qn32(-18,
                   Qn18, NewQn18,
                   {Qn32, Qn31, Qn30, Qn29, Qn28,
                    Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                    Qn21, Qn20, Qn19, NewQn18, Qn17});
?OUT_SPECIFIC_Qn32(-17,
                   Qn17, NewQn17,
                   {Qn32, Qn31, Qn30, Qn29, Qn28,
                    Qn27, Qn26, Qn25, Qn24, Qn23, Qn22,
                    Qn21, Qn20, Qn19, Qn18, NewQn17});
?OUT_SPECIFIC_Qn16(-16,
                   Qn16, NewQn16,
                   {NewQn16, Qn15, Qn14, Qn13, Qn12,
                    Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                    Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_SPECIFIC_Qn16(-15,
                   Qn15, NewQn15,
                   {Qn16, NewQn15, Qn14, Qn13, Qn12,
                    Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                    Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_SPECIFIC_Qn16(-14,
                   Qn14, NewQn14,
                   {Qn16, Qn15, NewQn14, Qn13, Qn12,
                    Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                    Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_SPECIFIC_Qn16(-13,
                   Qn13, NewQn13,
                   {Qn16, Qn15, Qn14, NewQn13, Qn12,
                    Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                    Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_SPECIFIC_Qn16(-12,
                   Qn12, NewQn12,
                   {Qn16, Qn15, Qn14, Qn13, NewQn12,
                    Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                    Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_SPECIFIC_Qn16(-11,
                   Qn11, NewQn11,
                   {Qn16, Qn15, Qn14, Qn13, Qn12,
                    NewQn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                    Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_SPECIFIC_Qn16(-10,
                   Qn10, NewQn10,
                   {Qn16, Qn15, Qn14, Qn13, Qn12,
                    Qn11, NewQn10, Qn9, Qn8, Qn7, Qn6,
                    Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_SPECIFIC_Qn16(-9,
                   Qn9, NewQn9,
                   {Qn16, Qn15, Qn14, Qn13, Qn12,
                    Qn11, Qn10, NewQn9, Qn8, Qn7, Qn6,
                    Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_SPECIFIC_Qn16(-8,
                   Qn8, NewQn8,
                   {Qn16, Qn15, Qn14, Qn13, Qn12,
                    Qn11, Qn10, Qn9, NewQn8, Qn7, Qn6,
                    Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_SPECIFIC_Qn16(-7,
                   Qn7, NewQn7,
                   {Qn16, Qn15, Qn14, Qn13, Qn12,
                    Qn11, Qn10, Qn9, Qn8, NewQn7, Qn6,
                    Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_SPECIFIC_Qn16(-6,
                   Qn6, NewQn6,
                   {Qn16, Qn15, Qn14, Qn13, Qn12,
                    Qn11, Qn10, Qn9, Qn8, Qn7, NewQn6,
                    Qn5, Qn4, Qn3, Qn2, Qn1});
?OUT_SPECIFIC_Qn16(-5,
                   Qn5, NewQn5,
                   {Qn16, Qn15, Qn14, Qn13, Qn12,
                    Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                    NewQn5, Qn4, Qn3, Qn2, Qn1});
?OUT_SPECIFIC_Qn16(-4,
                   Qn4, NewQn4,
                   {Qn16, Qn15, Qn14, Qn13, Qn12,
                    Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                    Qn5, NewQn4, Qn3, Qn2, Qn1});
?OUT_SPECIFIC_Qn16(-3,
                   Qn3, NewQn3,
                   {Qn16, Qn15, Qn14, Qn13, Qn12,
                    Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                    Qn5, Qn4, NewQn3, Qn2, Qn1});
?OUT_SPECIFIC_Qn16(-2,
                   Qn2, NewQn2,
                   {Qn16, Qn15, Qn14, Qn13, Qn12,
                    Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                    Qn5, Qn4, Qn3, NewQn2, Qn1});
?OUT_SPECIFIC_Qn16(-1,
                   Qn1, NewQn1,
                   {Qn16, Qn15, Qn14, Qn13, Qn12,
                    Qn11, Qn10, Qn9, Qn8, Qn7, Qn6,
                    Qn5, Qn4, Qn3, Qn2, NewQn1});
out_specific(0,
             {Pc,
              Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
              Q0,
              Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}) ->
    {Value, NewQ0} = queue:out(Q0),
    {Value,
     {Pc,
      Qn128, Qn112, Qn96, Qn80, Qn64, Qn48, Qn32, Qn16,
      NewQ0,
      Qp16, Qp32, Qp48, Qp64, Qp80, Qp96, Qp112, Qp128}};
?OUT_SPECIFIC_Qp16(1,
                   Qp1, NewQp1,
                   {NewQp1, Qp2, Qp3, Qp4, Qp5,
                    Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                    Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_SPECIFIC_Qp16(2,
                   Qp2, NewQp2,
                   {Qp1, NewQp2, Qp3, Qp4, Qp5,
                    Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                    Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_SPECIFIC_Qp16(3,
                   Qp3, NewQp3,
                   {Qp1, Qp2, NewQp3, Qp4, Qp5,
                    Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                    Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_SPECIFIC_Qp16(4,
                   Qp4, NewQp4,
                   {Qp1, Qp2, Qp3, NewQp4, Qp5,
                    Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                    Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_SPECIFIC_Qp16(5,
                   Qp5, NewQp5,
                   {Qp1, Qp2, Qp3, Qp4, NewQp5,
                    Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                    Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_SPECIFIC_Qp16(6,
                   Qp6, NewQp6,
                   {Qp1, Qp2, Qp3, Qp4, Qp5,
                    NewQp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                    Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_SPECIFIC_Qp16(7,
                   Qp7, NewQp7,
                   {Qp1, Qp2, Qp3, Qp4, Qp5,
                    Qp6, NewQp7, Qp8, Qp9, Qp10, Qp11,
                    Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_SPECIFIC_Qp16(8,
                   Qp8, NewQp8,
                   {Qp1, Qp2, Qp3, Qp4, Qp5,
                    Qp6, Qp7, NewQp8, Qp9, Qp10, Qp11,
                    Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_SPECIFIC_Qp16(9,
                   Qp9, NewQp9,
                   {Qp1, Qp2, Qp3, Qp4, Qp5,
                    Qp6, Qp7, Qp8, NewQp9, Qp10, Qp11,
                    Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_SPECIFIC_Qp16(10,
                   Qp10, NewQp10,
                   {Qp1, Qp2, Qp3, Qp4, Qp5,
                    Qp6, Qp7, Qp8, Qp9, NewQp10, Qp11,
                    Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_SPECIFIC_Qp16(11,
                   Qp11, NewQp11,
                   {Qp1, Qp2, Qp3, Qp4, Qp5,
                    Qp6, Qp7, Qp8, Qp9, Qp10, NewQp11,
                    Qp12, Qp13, Qp14, Qp15, Qp16});
?OUT_SPECIFIC_Qp16(12,
                   Qp12, NewQp12,
                   {Qp1, Qp2, Qp3, Qp4, Qp5,
                    Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                    NewQp12, Qp13, Qp14, Qp15, Qp16});
?OUT_SPECIFIC_Qp16(13,
                   Qp13, NewQp13,
                   {Qp1, Qp2, Qp3, Qp4, Qp5,
                    Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                    Qp12, NewQp13, Qp14, Qp15, Qp16});
?OUT_SPECIFIC_Qp16(14,
                   Qp14, NewQp14,
                   {Qp1, Qp2, Qp3, Qp4, Qp5,
                    Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                    Qp12, Qp13, NewQp14, Qp15, Qp16});
?OUT_SPECIFIC_Qp16(15,
                   Qp15, NewQp15,
                   {Qp1, Qp2, Qp3, Qp4, Qp5,
                    Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                    Qp12, Qp13, Qp14, NewQp15, Qp16});
?OUT_SPECIFIC_Qp16(16,
                   Qp16, NewQp16,
                   {Qp1, Qp2, Qp3, Qp4, Qp5,
                    Qp6, Qp7, Qp8, Qp9, Qp10, Qp11,
                    Qp12, Qp13, Qp14, Qp15, NewQp16});
?OUT_SPECIFIC_Qp32(17,
                   Qp17, NewQp17,
                   {NewQp17, Qp18, Qp19, Qp20, Qp21,
                    Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                    Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_SPECIFIC_Qp32(18,
                   Qp18, NewQp18,
                   {Qp17, NewQp18, Qp19, Qp20, Qp21,
                    Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                    Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_SPECIFIC_Qp32(19,
                   Qp19, NewQp19,
                   {Qp17, Qp18, NewQp19, Qp20, Qp21,
                    Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                    Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_SPECIFIC_Qp32(20,
                   Qp20, NewQp20,
                   {Qp17, Qp18, Qp19, NewQp20, Qp21,
                    Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                    Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_SPECIFIC_Qp32(21,
                   Qp21, NewQp21,
                   {Qp17, Qp18, Qp19, Qp20, NewQp21,
                    Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                    Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_SPECIFIC_Qp32(22,
                   Qp22, NewQp22,
                   {Qp17, Qp18, Qp19, Qp20, Qp21,
                    NewQp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                    Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_SPECIFIC_Qp32(23,
                   Qp23, NewQp23,
                   {Qp17, Qp18, Qp19, Qp20, Qp21,
                    Qp22, NewQp23, Qp24, Qp25, Qp26, Qp27,
                    Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_SPECIFIC_Qp32(24,
                   Qp24, NewQp24,
                   {Qp17, Qp18, Qp19, Qp20, Qp21,
                    Qp22, Qp23, NewQp24, Qp25, Qp26, Qp27,
                    Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_SPECIFIC_Qp32(25,
                   Qp25, NewQp25,
                   {Qp17, Qp18, Qp19, Qp20, Qp21,
                    Qp22, Qp23, Qp24, NewQp25, Qp26, Qp27,
                    Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_SPECIFIC_Qp32(26,
                   Qp26, NewQp26,
                   {Qp17, Qp18, Qp19, Qp20, Qp21,
                    Qp22, Qp23, Qp24, Qp25, NewQp26, Qp27,
                    Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_SPECIFIC_Qp32(27,
                   Qp27, NewQp27,
                   {Qp17, Qp18, Qp19, Qp20, Qp21,
                    Qp22, Qp23, Qp24, Qp25, Qp26, NewQp27,
                    Qp28, Qp29, Qp30, Qp31, Qp32});
?OUT_SPECIFIC_Qp32(28,
                   Qp28, NewQp28,
                   {Qp17, Qp18, Qp19, Qp20, Qp21,
                    Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                    NewQp28, Qp29, Qp30, Qp31, Qp32});
?OUT_SPECIFIC_Qp32(29,
                   Qp29, NewQp29,
                   {Qp17, Qp18, Qp19, Qp20, Qp21,
                    Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                    Qp28, NewQp29, Qp30, Qp31, Qp32});
?OUT_SPECIFIC_Qp32(30,
                   Qp30, NewQp30,
                   {Qp17, Qp18, Qp19, Qp20, Qp21,
                    Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                    Qp28, Qp29, NewQp30, Qp31, Qp32});
?OUT_SPECIFIC_Qp32(31,
                   Qp31, NewQp31,
                   {Qp17, Qp18, Qp19, Qp20, Qp21,
                    Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                    Qp28, Qp29, Qp30, NewQp31, Qp32});
?OUT_SPECIFIC_Qp32(32,
                   Qp32, NewQp32,
                   {Qp17, Qp18, Qp19, Qp20, Qp21,
                    Qp22, Qp23, Qp24, Qp25, Qp26, Qp27,
                    Qp28, Qp29, Qp30, Qp31, NewQp32});
?OUT_SPECIFIC_Qp48(33,
                   Qp33, NewQp33,
                   {NewQp33, Qp34, Qp35, Qp36, Qp37,
                    Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                    Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_SPECIFIC_Qp48(34,
                   Qp34, NewQp34,
                   {Qp33, NewQp34, Qp35, Qp36, Qp37,
                    Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                    Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_SPECIFIC_Qp48(35,
                   Qp35, NewQp35,
                   {Qp33, Qp34, NewQp35, Qp36, Qp37,
                    Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                    Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_SPECIFIC_Qp48(36,
                   Qp36, NewQp36,
                   {Qp33, Qp34, Qp35, NewQp36, Qp37,
                    Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                    Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_SPECIFIC_Qp48(37,
                   Qp37, NewQp37,
                   {Qp33, Qp34, Qp35, Qp36, NewQp37,
                    Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                    Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_SPECIFIC_Qp48(38,
                   Qp38, NewQp38,
                   {Qp33, Qp34, Qp35, Qp36, Qp37,
                    NewQp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                    Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_SPECIFIC_Qp48(39,
                   Qp39, NewQp39,
                   {Qp33, Qp34, Qp35, Qp36, Qp37,
                    Qp38, NewQp39, Qp40, Qp41, Qp42, Qp43,
                    Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_SPECIFIC_Qp48(40,
                   Qp40, NewQp40,
                   {Qp33, Qp34, Qp35, Qp36, Qp37,
                    Qp38, Qp39, NewQp40, Qp41, Qp42, Qp43,
                    Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_SPECIFIC_Qp48(41,
                   Qp41, NewQp41,
                   {Qp33, Qp34, Qp35, Qp36, Qp37,
                    Qp38, Qp39, Qp40, NewQp41, Qp42, Qp43,
                    Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_SPECIFIC_Qp48(42,
                   Qp42, NewQp42,
                   {Qp33, Qp34, Qp35, Qp36, Qp37,
                    Qp38, Qp39, Qp40, Qp41, NewQp42, Qp43,
                    Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_SPECIFIC_Qp48(43,
                   Qp43, NewQp43,
                   {Qp33, Qp34, Qp35, Qp36, Qp37,
                    Qp38, Qp39, Qp40, Qp41, Qp42, NewQp43,
                    Qp44, Qp45, Qp46, Qp47, Qp48});
?OUT_SPECIFIC_Qp48(44,
                   Qp44, NewQp44,
                   {Qp33, Qp34, Qp35, Qp36, Qp37,
                    Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                    NewQp44, Qp45, Qp46, Qp47, Qp48});
?OUT_SPECIFIC_Qp48(45,
                   Qp45, NewQp45,
                   {Qp33, Qp34, Qp35, Qp36, Qp37,
                    Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                    Qp44, NewQp45, Qp46, Qp47, Qp48});
?OUT_SPECIFIC_Qp48(46,
                   Qp46, NewQp46,
                   {Qp33, Qp34, Qp35, Qp36, Qp37,
                    Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                    Qp44, Qp45, NewQp46, Qp47, Qp48});
?OUT_SPECIFIC_Qp48(47,
                   Qp47, NewQp47,
                   {Qp33, Qp34, Qp35, Qp36, Qp37,
                    Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                    Qp44, Qp45, Qp46, NewQp47, Qp48});
?OUT_SPECIFIC_Qp48(48,
                   Qp48, NewQp48,
                   {Qp33, Qp34, Qp35, Qp36, Qp37,
                    Qp38, Qp39, Qp40, Qp41, Qp42, Qp43,
                    Qp44, Qp45, Qp46, Qp47, NewQp48});
?OUT_SPECIFIC_Qp64(49,
                   Qp49, NewQp49,
                   {NewQp49, Qp50, Qp51, Qp52, Qp53,
                    Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                    Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_SPECIFIC_Qp64(50,
                   Qp50, NewQp50,
                   {Qp49, NewQp50, Qp51, Qp52, Qp53,
                    Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                    Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_SPECIFIC_Qp64(51,
                   Qp51, NewQp51,
                   {Qp49, Qp50, NewQp51, Qp52, Qp53,
                    Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                    Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_SPECIFIC_Qp64(52,
                   Qp52, NewQp52,
                   {Qp49, Qp50, Qp51, NewQp52, Qp53,
                    Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                    Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_SPECIFIC_Qp64(53,
                   Qp53, NewQp53,
                   {Qp49, Qp50, Qp51, Qp52, NewQp53,
                    Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                    Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_SPECIFIC_Qp64(54,
                   Qp54, NewQp54,
                   {Qp49, Qp50, Qp51, Qp52, Qp53,
                    NewQp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                    Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_SPECIFIC_Qp64(55,
                   Qp55, NewQp55,
                   {Qp49, Qp50, Qp51, Qp52, Qp53,
                    Qp54, NewQp55, Qp56, Qp57, Qp58, Qp59,
                    Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_SPECIFIC_Qp64(56,
                   Qp56, NewQp56,
                   {Qp49, Qp50, Qp51, Qp52, Qp53,
                    Qp54, Qp55, NewQp56, Qp57, Qp58, Qp59,
                    Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_SPECIFIC_Qp64(57,
                   Qp57, NewQp57,
                   {Qp49, Qp50, Qp51, Qp52, Qp53,
                    Qp54, Qp55, Qp56, NewQp57, Qp58, Qp59,
                    Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_SPECIFIC_Qp64(58,
                   Qp58, NewQp58,
                   {Qp49, Qp50, Qp51, Qp52, Qp53,
                    Qp54, Qp55, Qp56, Qp57, NewQp58, Qp59,
                    Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_SPECIFIC_Qp64(59,
                   Qp59, NewQp59,
                   {Qp49, Qp50, Qp51, Qp52, Qp53,
                    Qp54, Qp55, Qp56, Qp57, Qp58, NewQp59,
                    Qp60, Qp61, Qp62, Qp63, Qp64});
?OUT_SPECIFIC_Qp64(60,
                   Qp60, NewQp60,
                   {Qp49, Qp50, Qp51, Qp52, Qp53,
                    Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                    NewQp60, Qp61, Qp62, Qp63, Qp64});
?OUT_SPECIFIC_Qp64(61,
                   Qp61, NewQp61,
                   {Qp49, Qp50, Qp51, Qp52, Qp53,
                    Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                    Qp60, NewQp61, Qp62, Qp63, Qp64});
?OUT_SPECIFIC_Qp64(62,
                   Qp62, NewQp62,
                   {Qp49, Qp50, Qp51, Qp52, Qp53,
                    Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                    Qp60, Qp61, NewQp62, Qp63, Qp64});
?OUT_SPECIFIC_Qp64(63,
                   Qp63, NewQp63,
                   {Qp49, Qp50, Qp51, Qp52, Qp53,
                    Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                    Qp60, Qp61, Qp62, NewQp63, Qp64});
?OUT_SPECIFIC_Qp64(64,
                   Qp64, NewQp64,
                   {Qp49, Qp50, Qp51, Qp52, Qp53,
                    Qp54, Qp55, Qp56, Qp57, Qp58, Qp59,
                    Qp60, Qp61, Qp62, Qp63, NewQp64});
?OUT_SPECIFIC_Qp80(65,
                   Qp65, NewQp65,
                   {NewQp65, Qp66, Qp67, Qp68, Qp69,
                    Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                    Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_SPECIFIC_Qp80(66,
                   Qp66, NewQp66,
                   {Qp65, NewQp66, Qp67, Qp68, Qp69,
                    Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                    Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_SPECIFIC_Qp80(67,
                   Qp67, NewQp67,
                   {Qp65, Qp66, NewQp67, Qp68, Qp69,
                    Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                    Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_SPECIFIC_Qp80(68,
                   Qp68, NewQp68,
                   {Qp65, Qp66, Qp67, NewQp68, Qp69,
                    Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                    Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_SPECIFIC_Qp80(69,
                   Qp69, NewQp69,
                   {Qp65, Qp66, Qp67, Qp68, NewQp69,
                    Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                    Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_SPECIFIC_Qp80(70,
                   Qp70, NewQp70,
                   {Qp65, Qp66, Qp67, Qp68, Qp69,
                    NewQp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                    Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_SPECIFIC_Qp80(71,
                   Qp71, NewQp71,
                   {Qp65, Qp66, Qp67, Qp68, Qp69,
                    Qp70, NewQp71, Qp72, Qp73, Qp74, Qp75,
                    Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_SPECIFIC_Qp80(72,
                   Qp72, NewQp72,
                   {Qp65, Qp66, Qp67, Qp68, Qp69,
                    Qp70, Qp71, NewQp72, Qp73, Qp74, Qp75,
                    Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_SPECIFIC_Qp80(73,
                   Qp73, NewQp73,
                   {Qp65, Qp66, Qp67, Qp68, Qp69,
                    Qp70, Qp71, Qp72, NewQp73, Qp74, Qp75,
                    Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_SPECIFIC_Qp80(74,
                   Qp74, NewQp74,
                   {Qp65, Qp66, Qp67, Qp68, Qp69,
                    Qp70, Qp71, Qp72, Qp73, NewQp74, Qp75,
                    Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_SPECIFIC_Qp80(75,
                   Qp75, NewQp75,
                   {Qp65, Qp66, Qp67, Qp68, Qp69,
                    Qp70, Qp71, Qp72, Qp73, Qp74, NewQp75,
                    Qp76, Qp77, Qp78, Qp79, Qp80});
?OUT_SPECIFIC_Qp80(76,
                   Qp76, NewQp76,
                   {Qp65, Qp66, Qp67, Qp68, Qp69,
                    Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                    NewQp76, Qp77, Qp78, Qp79, Qp80});
?OUT_SPECIFIC_Qp80(77,
                   Qp77, NewQp77,
                   {Qp65, Qp66, Qp67, Qp68, Qp69,
                    Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                    Qp76, NewQp77, Qp78, Qp79, Qp80});
?OUT_SPECIFIC_Qp80(78,
                   Qp78, NewQp78,
                   {Qp65, Qp66, Qp67, Qp68, Qp69,
                    Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                    Qp76, Qp77, NewQp78, Qp79, Qp80});
?OUT_SPECIFIC_Qp80(79,
                   Qp79, NewQp79,
                   {Qp65, Qp66, Qp67, Qp68, Qp69,
                    Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                    Qp76, Qp77, Qp78, NewQp79, Qp80});
?OUT_SPECIFIC_Qp80(80,
                   Qp80, NewQp80,
                   {Qp65, Qp66, Qp67, Qp68, Qp69,
                    Qp70, Qp71, Qp72, Qp73, Qp74, Qp75,
                    Qp76, Qp77, Qp78, Qp79, NewQp80});
?OUT_SPECIFIC_Qp96(81,
                   Qp81, NewQp81,
                   {NewQp81, Qp82, Qp83, Qp84, Qp85,
                    Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                    Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_SPECIFIC_Qp96(82,
                   Qp82, NewQp82,
                   {Qp81, NewQp82, Qp83, Qp84, Qp85,
                    Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                    Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_SPECIFIC_Qp96(83,
                   Qp83, NewQp83,
                   {Qp81, Qp82, NewQp83, Qp84, Qp85,
                    Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                    Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_SPECIFIC_Qp96(84,
                   Qp84, NewQp84,
                   {Qp81, Qp82, Qp83, NewQp84, Qp85,
                    Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                    Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_SPECIFIC_Qp96(85,
                   Qp85, NewQp85,
                   {Qp81, Qp82, Qp83, Qp84, NewQp85,
                    Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                    Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_SPECIFIC_Qp96(86,
                   Qp86, NewQp86,
                   {Qp81, Qp82, Qp83, Qp84, Qp85,
                    NewQp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                    Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_SPECIFIC_Qp96(87,
                   Qp87, NewQp87,
                   {Qp81, Qp82, Qp83, Qp84, Qp85,
                    Qp86, NewQp87, Qp88, Qp89, Qp90, Qp91,
                    Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_SPECIFIC_Qp96(88,
                   Qp88, NewQp88,
                   {Qp81, Qp82, Qp83, Qp84, Qp85,
                    Qp86, Qp87, NewQp88, Qp89, Qp90, Qp91,
                    Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_SPECIFIC_Qp96(89,
                   Qp89, NewQp89,
                   {Qp81, Qp82, Qp83, Qp84, Qp85,
                    Qp86, Qp87, Qp88, NewQp89, Qp90, Qp91,
                    Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_SPECIFIC_Qp96(90,
                   Qp90, NewQp90,
                   {Qp81, Qp82, Qp83, Qp84, Qp85,
                    Qp86, Qp87, Qp88, Qp89, NewQp90, Qp91,
                    Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_SPECIFIC_Qp96(91,
                   Qp91, NewQp91,
                   {Qp81, Qp82, Qp83, Qp84, Qp85,
                    Qp86, Qp87, Qp88, Qp89, Qp90, NewQp91,
                    Qp92, Qp93, Qp94, Qp95, Qp96});
?OUT_SPECIFIC_Qp96(92,
                   Qp92, NewQp92,
                   {Qp81, Qp82, Qp83, Qp84, Qp85,
                    Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                    NewQp92, Qp93, Qp94, Qp95, Qp96});
?OUT_SPECIFIC_Qp96(93,
                   Qp93, NewQp93,
                   {Qp81, Qp82, Qp83, Qp84, Qp85,
                    Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                    Qp92, NewQp93, Qp94, Qp95, Qp96});
?OUT_SPECIFIC_Qp96(94,
                   Qp94, NewQp94,
                   {Qp81, Qp82, Qp83, Qp84, Qp85,
                    Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                    Qp92, Qp93, NewQp94, Qp95, Qp96});
?OUT_SPECIFIC_Qp96(95,
                   Qp95, NewQp95,
                   {Qp81, Qp82, Qp83, Qp84, Qp85,
                    Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                    Qp92, Qp93, Qp94, NewQp95, Qp96});
?OUT_SPECIFIC_Qp96(96,
                   Qp96, NewQp96,
                   {Qp81, Qp82, Qp83, Qp84, Qp85,
                    Qp86, Qp87, Qp88, Qp89, Qp90, Qp91,
                    Qp92, Qp93, Qp94, Qp95, NewQp96});
?OUT_SPECIFIC_Qp112(97,
                    Qp97, NewQp97,
                    {NewQp97, Qp98, Qp99, Qp100, Qp101,
                     Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                     Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_SPECIFIC_Qp112(98,
                    Qp98, NewQp98,
                    {Qp97, NewQp98, Qp99, Qp100, Qp101,
                     Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                     Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_SPECIFIC_Qp112(99,
                    Qp99, NewQp99,
                    {Qp97, Qp98, NewQp99, Qp100, Qp101,
                     Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                     Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_SPECIFIC_Qp112(100,
                    Qp100, NewQp100,
                    {Qp97, Qp98, Qp99, NewQp100, Qp101,
                     Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                     Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_SPECIFIC_Qp112(101,
                    Qp101, NewQp101,
                    {Qp97, Qp98, Qp99, Qp100, NewQp101,
                     Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                     Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_SPECIFIC_Qp112(102,
                    Qp102, NewQp102,
                    {Qp97, Qp98, Qp99, Qp100, Qp101,
                     NewQp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                     Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_SPECIFIC_Qp112(103,
                    Qp103, NewQp103,
                    {Qp97, Qp98, Qp99, Qp100, Qp101,
                     Qp102, NewQp103, Qp104, Qp105, Qp106, Qp107,
                     Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_SPECIFIC_Qp112(104,
                    Qp104, NewQp104,
                    {Qp97, Qp98, Qp99, Qp100, Qp101,
                     Qp102, Qp103, NewQp104, Qp105, Qp106, Qp107,
                     Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_SPECIFIC_Qp112(105,
                    Qp105, NewQp105,
                    {Qp97, Qp98, Qp99, Qp100, Qp101,
                     Qp102, Qp103, Qp104, NewQp105, Qp106, Qp107,
                     Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_SPECIFIC_Qp112(106,
                    Qp106, NewQp106,
                    {Qp97, Qp98, Qp99, Qp100, Qp101,
                     Qp102, Qp103, Qp104, Qp105, NewQp106, Qp107,
                     Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_SPECIFIC_Qp112(107,
                    Qp107, NewQp107,
                    {Qp97, Qp98, Qp99, Qp100, Qp101,
                     Qp102, Qp103, Qp104, Qp105, Qp106, NewQp107,
                     Qp108, Qp109, Qp110, Qp111, Qp112});
?OUT_SPECIFIC_Qp112(108,
                    Qp108, NewQp108,
                    {Qp97, Qp98, Qp99, Qp100, Qp101,
                     Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                     NewQp108, Qp109, Qp110, Qp111, Qp112});
?OUT_SPECIFIC_Qp112(109,
                    Qp109, NewQp109,
                    {Qp97, Qp98, Qp99, Qp100, Qp101,
                     Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                     Qp108, NewQp109, Qp110, Qp111, Qp112});
?OUT_SPECIFIC_Qp112(110,
                    Qp110, NewQp110,
                    {Qp97, Qp98, Qp99, Qp100, Qp101,
                     Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                     Qp108, Qp109, NewQp110, Qp111, Qp112});
?OUT_SPECIFIC_Qp112(111,
                    Qp111, NewQp111,
                    {Qp97, Qp98, Qp99, Qp100, Qp101,
                     Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                     Qp108, Qp109, Qp110, NewQp111, Qp112});
?OUT_SPECIFIC_Qp112(112,
                    Qp112, NewQp112,
                    {Qp97, Qp98, Qp99, Qp100, Qp101,
                     Qp102, Qp103, Qp104, Qp105, Qp106, Qp107,
                     Qp108, Qp109, Qp110, Qp111, NewQp112});
?OUT_SPECIFIC_Qp128(113,
                    Qp113, NewQp113,
                    {NewQp113, Qp114, Qp115, Qp116, Qp117,
                     Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                     Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_SPECIFIC_Qp128(114,
                    Qp114, NewQp114,
                    {Qp113, NewQp114, Qp115, Qp116, Qp117,
                     Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                     Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_SPECIFIC_Qp128(115,
                    Qp115, NewQp115,
                    {Qp113, Qp114, NewQp115, Qp116, Qp117,
                     Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                     Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_SPECIFIC_Qp128(116,
                    Qp116, NewQp116,
                    {Qp113, Qp114, Qp115, NewQp116, Qp117,
                     Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                     Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_SPECIFIC_Qp128(117,
                    Qp117, NewQp117,
                    {Qp113, Qp114, Qp115, Qp116, NewQp117,
                     Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                     Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_SPECIFIC_Qp128(118,
                    Qp118, NewQp118,
                    {Qp113, Qp114, Qp115, Qp116, Qp117,
                     NewQp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                     Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_SPECIFIC_Qp128(119,
                    Qp119, NewQp119,
                    {Qp113, Qp114, Qp115, Qp116, Qp117,
                     Qp118, NewQp119, Qp120, Qp121, Qp122, Qp123,
                     Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_SPECIFIC_Qp128(120,
                    Qp120, NewQp120,
                    {Qp113, Qp114, Qp115, Qp116, Qp117,
                     Qp118, Qp119, NewQp120, Qp121, Qp122, Qp123,
                     Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_SPECIFIC_Qp128(121,
                    Qp121, NewQp121,
                    {Qp113, Qp114, Qp115, Qp116, Qp117,
                     Qp118, Qp119, Qp120, NewQp121, Qp122, Qp123,
                     Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_SPECIFIC_Qp128(122,
                    Qp122, NewQp122,
                    {Qp113, Qp114, Qp115, Qp116, Qp117,
                     Qp118, Qp119, Qp120, Qp121, NewQp122, Qp123,
                     Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_SPECIFIC_Qp128(123,
                    Qp123, NewQp123,
                    {Qp113, Qp114, Qp115, Qp116, Qp117,
                     Qp118, Qp119, Qp120, Qp121, Qp122, NewQp123,
                     Qp124, Qp125, Qp126, Qp127, Qp128});
?OUT_SPECIFIC_Qp128(124,
                    Qp124, NewQp124,
                    {Qp113, Qp114, Qp115, Qp116, Qp117,
                     Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                     NewQp124, Qp125, Qp126, Qp127, Qp128});
?OUT_SPECIFIC_Qp128(125,
                    Qp125, NewQp125,
                    {Qp113, Qp114, Qp115, Qp116, Qp117,
                     Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                     Qp124, NewQp125, Qp126, Qp127, Qp128});
?OUT_SPECIFIC_Qp128(126,
                    Qp126, NewQp126,
                    {Qp113, Qp114, Qp115, Qp116, Qp117,
                     Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                     Qp124, Qp125, NewQp126, Qp127, Qp128});
?OUT_SPECIFIC_Qp128(127,
                    Qp127, NewQp127,
                    {Qp113, Qp114, Qp115, Qp116, Qp117,
                     Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                     Qp124, Qp125, Qp126, NewQp127, Qp128});
?OUT_SPECIFIC_Qp128(128,
                    Qp128, NewQp128,
                    {Qp113, Qp114, Qp115, Qp116, Qp117,
                     Qp118, Qp119, Qp120, Qp121, Qp122, Qp123,
                     Qp124, Qp125, Qp126, Qp127, NewQp128}).

