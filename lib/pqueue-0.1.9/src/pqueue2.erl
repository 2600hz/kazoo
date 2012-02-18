%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Skew Heap Priority Queue.==
%%% Ulf Wiger suggested pursuing a skew heap as an optimal Erlang priority
%%% queue implementation. Unfortunately, testing has shown this solution to
%%% be more than 2 times slower than pqueue.
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

-module(pqueue2).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([in/2,
         in/3,
         is_empty/1,
         is_queue/1,
         len/1,
         new/0,
         out/1,
         out/2,
         pout/1,
         to_list/1,
         test/0]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-type pqueue2() ::
    empty |
    {integer(), pqueue2(), pqueue2(), element, term()} |
    {integer(), pqueue2(), pqueue2(), queue, queue()}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Append an item to the tail of the 0 priority queue.===
%% @end
%%-------------------------------------------------------------------------

-spec in(term(), pqueue2()) -> pqueue2().

in(Value, H) ->
    in(Value, 0, H).

%%-------------------------------------------------------------------------
%% @doc
%% ===Append an item to the tail of a specific priority queue.===
%% @end
%%-------------------------------------------------------------------------

-spec in(term(), integer(), pqueue2()) -> pqueue2().

in(Value, P, H) ->
    merge({P, empty, empty, element, Value}, H).

%%-------------------------------------------------------------------------
%% @doc
%% ===Check if the priority queue is empty.===
%% @end
%%-------------------------------------------------------------------------

-spec is_empty(pqueue2()) -> 'true' | 'false'.

is_empty(empty) ->
    true;
is_empty({_, empty, empty, queue, Queue}) ->
    queue:is_empty(Queue);
is_empty(_) ->
    false.

%%-------------------------------------------------------------------------
%% @doc
%% ===Check if the priority queue type is as expected.===
%% @end
%%-------------------------------------------------------------------------

-spec is_queue(pqueue2()) -> 'true' | 'false'.

is_queue(empty) ->
    true;
is_queue({P, _, _, element, _})
    when is_integer(P) ->
    true;
is_queue({P, _, _, queue, Queue})
    when is_integer(P) ->
    queue:is_queue(Queue);
is_queue(_) ->
    false.

%%-------------------------------------------------------------------------
%% @doc
%% ===Determine the length of a priority queue.===
%% @end
%%-------------------------------------------------------------------------

-spec len(pqueue2()) -> non_neg_integer().

len(H) ->
    len(0, out(H)).
len(I, {empty, _}) ->
    I;
len(I, {{value, _}, H}) ->
    len(I + 1, out(H)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a new priority queue.===
%% @end
%%-------------------------------------------------------------------------

-spec new() -> pqueue2().

new() ->
    empty.

%%-------------------------------------------------------------------------
%% @doc
%% ===Take an item from the head of the priority queue.===
%% @end
%%-------------------------------------------------------------------------

-spec out(pqueue2()) ->
    {{'value', term()}, pqueue2()} | {'empty', pqueue2()}.

out(empty) ->
    {empty, empty};
out({_, HL, HR, element, Value}) ->
    {{value, Value}, merge(HL, HR)};
out({P, HL, HR, queue, Queue}) ->
    case queue:out(Queue) of
        {{value, _} = Result, NewQueue} ->
            {Result, {P, HL, HR, queue, NewQueue}};
        {empty, _} ->
            out(merge(HL, HR))
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Take an item of a specific priority from the head of the queue.===
%% @end
%%-------------------------------------------------------------------------

-spec out(integer(), pqueue2()) ->
    {{'value', term()}, pqueue2()} | {'empty', pqueue2()}.

out(_, empty) ->
    {empty, empty};
out(P, {P1, _, _, _, _} = H) when P < P1 ->
    {empty, H};
out(P, {P1, HL1, HR1, T, D}) when P > P1 ->
    case out(P, HL1) of
        {{value, _} = Result, HL2} ->
            {Result, {P1, HL2, HR1, T, D}};
        {empty, HL2} ->
            case out(P, HR1) of
                {{value, _} = Result, HR2} ->
                    {Result, {P1, HL2, HR2, T, D}};
                {empty, HR2} ->
                    {empty, {P1, HL2, HR2, T, D}}
            end
    end;
out(P, {P, HL, HR, element, Value}) ->
    {{value, Value}, merge(HL, HR)};
out(P, {P, HL, HR, queue, Queue}) ->
    case queue:out(Queue) of
        {{value, _} = Result, NewQueue} ->
            {Result, {P, HL, HR, queue, NewQueue}};
        {empty, _} ->
            out(P, merge(HL, HR))
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Take an item from the head of the priority queue.===
%% Includes the priority in the return value.
%% @end
%%-------------------------------------------------------------------------

-spec pout(pqueue2()) ->
    {{'value', term(), integer()}, pqueue2()} | {'empty', pqueue2()}.

pout(empty) ->
    {empty, empty};
pout({P, HL, HR, element, Value}) ->
    {{value, Value, P}, merge(HL, HR)};
pout({P, HL, HR, queue, Queue}) ->
    case queue:out(Queue) of
        {{value, Value}, NewQueue} ->
            {{value, Value, P}, {P, HL, HR, queue, NewQueue}};
        {empty, _} ->
            pout(merge(HL, HR))
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Convert the priority queue to a list.===
%% @end
%%-------------------------------------------------------------------------

-spec to_list(pqueue2()) -> list(term()).

to_list(H) ->
    to_list([], out(H)).
to_list(L, {empty, _}) ->
    lists:reverse(L);
to_list(L, {{value, Value}, H}) ->
    to_list([Value | L], out(H)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Regression test.===
%% @end
%%-------------------------------------------------------------------------

test() ->
    Q0 = pqueue2:new(),
    true = pqueue2:is_queue(Q0),
    Q1 = pqueue2:in(20, 20, Q0),
    Q2 = pqueue2:in(19, 19, Q1),
    Q3 = pqueue2:in(18, 18, Q2),
    Q4 = pqueue2:in(17, 17, Q3),
    Q5 = pqueue2:in(16, 16, Q4),
    Q6 = pqueue2:in(15, 15, Q5),
    Q7 = pqueue2:in(14, 14, Q6),
    Q8 = pqueue2:in(13, 13, Q7),
    Q9 = pqueue2:in(12, 12, Q8),
    Q10 = pqueue2:in(11, 11, Q9),
    Q11 = pqueue2:in(10, 10, Q10),
    Q12 = pqueue2:in(9, 9, Q11),
    Q13 = pqueue2:in(8, 8, Q12),
    Q14 = pqueue2:in(7, 7, Q13),
    Q15 = pqueue2:in(6, 6, Q14),
    Q16 = pqueue2:in(5, 5, Q15),
    Q17 = pqueue2:in(4, 4, Q16),
    Q18 = pqueue2:in(3, 3, Q17),
    Q19 = pqueue2:in(2, 2, Q18),
    Q20 = pqueue2:in(1, 1, Q19),
    Q21 = pqueue2:in(0, 0, Q20),
    Q22 = pqueue2:in(-1, -1, Q21),
    Q23 = pqueue2:in(-2, -2, Q22),
    Q24 = pqueue2:in(-3, -3, Q23),
    Q25 = pqueue2:in(-4, -4, Q24),
    Q26 = pqueue2:in(-5, -5, Q25),
    Q27 = pqueue2:in(-6, -6, Q26),
    Q28 = pqueue2:in(-7, -7, Q27),
    Q29 = pqueue2:in(-8, -8, Q28),
    Q30 = pqueue2:in(-9, -9, Q29),
    Q31 = pqueue2:in(-10, -10, Q30),
    Q32 = pqueue2:in(-11, -11, Q31),
    Q33 = pqueue2:in(-12, -12, Q32),
    Q34 = pqueue2:in(-13, -13, Q33),
    Q35 = pqueue2:in(-14, -14, Q34),
    Q36 = pqueue2:in(-15, -15, Q35),
    Q37 = pqueue2:in(-16, -16, Q36),
    Q38 = pqueue2:in(-17, -17, Q37),
    Q39 = pqueue2:in(-18, -18, Q38),
    Q40 = pqueue2:in(-19, -19, Q39),
    Q41 = pqueue2:in(-20, -20, Q40),
    Q42 = pqueue2:in(-20, -20, Q41),
    Q43 = pqueue2:in(-19, -19, Q42),
    Q44 = pqueue2:in(-18, -18, Q43),
    Q45 = pqueue2:in(-17, -17, Q44),
    Q46 = pqueue2:in(-16, -16, Q45),
    Q47 = pqueue2:in(-15, -15, Q46),
    Q48 = pqueue2:in(-14, -14, Q47),
    Q49 = pqueue2:in(-13, -13, Q48),
    Q50 = pqueue2:in(-12, -12, Q49),
    Q51 = pqueue2:in(-11, -11, Q50),
    Q52 = pqueue2:in(-10, -10, Q51),
    Q53 = pqueue2:in(-9, -9, Q52),
    Q54 = pqueue2:in(-8, -8, Q53),
    Q55 = pqueue2:in(-7, -7, Q54),
    Q56 = pqueue2:in(-6, -6, Q55),
    Q57 = pqueue2:in(-5, -5, Q56),
    Q58 = pqueue2:in(-4, -4, Q57),
    Q59 = pqueue2:in(-3, -3, Q58),
    Q60 = pqueue2:in(-2, -2, Q59),
    Q61 = pqueue2:in(-1, -1, Q60),
    Q62 = pqueue2:in(0, 0, Q61),
    Q63 = pqueue2:in(1, 1, Q62),
    Q64 = pqueue2:in(2, 2, Q63),
    Q65 = pqueue2:in(3, 3, Q64),
    Q66 = pqueue2:in(4, 4, Q65),
    Q67 = pqueue2:in(5, 5, Q66),
    Q68 = pqueue2:in(6, 6, Q67),
    Q69 = pqueue2:in(7, 7, Q68),
    Q70 = pqueue2:in(8, 8, Q69),
    Q71 = pqueue2:in(9, 9, Q70),
    Q72 = pqueue2:in(10, 10, Q71),
    Q73 = pqueue2:in(11, 11, Q72),
    Q74 = pqueue2:in(12, 12, Q73),
    Q75 = pqueue2:in(13, 13, Q74),
    Q76 = pqueue2:in(14, 14, Q75),
    Q77 = pqueue2:in(15, 15, Q76),
    Q78 = pqueue2:in(16, 16, Q77),
    Q79 = pqueue2:in(17, 17, Q78),
    Q80 = pqueue2:in(18, 18, Q79),
    Q81 = pqueue2:in(19, 19, Q80),
    Q82 = pqueue2:in(20, 20, Q81),
    true = pqueue2:is_queue(Q82),
    82 = pqueue2:len(Q82),
    [-20, -20, -19, -19, -18, -18, -17, -17, -16, -16, -15, -15, -14, -14,
     -13, -13, -12, -12, -11, -11, -10, -10, -9, -9, -8, -8, -7, -7, -6, -6,
     -5, -5, -4, -4, -3, -3, -2, -2, -1, -1, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4,
     5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14,
     15, 15, 16, 16, 17, 17, 18, 18, 19, 19, 20, 20] = pqueue2:to_list(Q82),
    {{value, -20}, Q83} = pqueue2:out(Q82),
    {{value, -20}, Q84} = pqueue2:out(Q83),
    {{value, -19}, Q85} = pqueue2:out(Q84),
    {{value, -19}, Q86} = pqueue2:out(Q85),
    {{value, -18}, Q87} = pqueue2:out(Q86),
    {{value, -18}, Q88} = pqueue2:out(Q87),
    {{value, 0}, Q89} = pqueue2:out(0, Q88),
    {{value, 0}, Q90} = pqueue2:out(0, Q89),
    {empty, _} = pqueue2:out(0, Q90),
    {{value, -17, -17}, Q91} = pqueue2:pout(Q90),
    {{value, -17, -17}, Q92} = pqueue2:pout(Q91),
    {{value, -16, -16}, Q93} = pqueue2:pout(Q92),
    {{value, -16, -16}, Q94} = pqueue2:pout(Q93),
    {{value, -15, -15}, Q95} = pqueue2:pout(Q94),
    {{value, -15, -15}, Q96} = pqueue2:pout(Q95),
    {{value, -14, -14}, Q97} = pqueue2:pout(Q96),
    {{value, -14, -14}, Q98} = pqueue2:pout(Q97),
    {{value, -13, -13}, Q99} = pqueue2:pout(Q98),
    {{value, -13, -13}, Q100} = pqueue2:pout(Q99),
    {{value, -12, -12}, Q101} = pqueue2:pout(Q100),
    {{value, -12, -12}, Q102} = pqueue2:pout(Q101),
    {{value, -11, -11}, Q103} = pqueue2:pout(Q102),
    {{value, -11, -11}, Q104} = pqueue2:pout(Q103),
    {{value, -10, -10}, Q105} = pqueue2:pout(Q104),
    {{value, -10, -10}, Q106} = pqueue2:pout(Q105),
    {{value, -9, -9}, Q107} = pqueue2:pout(Q106),
    {{value, -9, -9}, Q108} = pqueue2:pout(Q107),
    {{value, -8, -8}, Q109} = pqueue2:pout(Q108),
    {{value, -8, -8}, Q110} = pqueue2:pout(Q109),
    {{value, -7, -7}, Q111} = pqueue2:pout(Q110),
    {{value, -7, -7}, Q112} = pqueue2:pout(Q111),
    {{value, -6, -6}, Q113} = pqueue2:pout(Q112),
    {{value, -6, -6}, Q114} = pqueue2:pout(Q113),
    {{value, -5, -5}, Q115} = pqueue2:pout(Q114),
    {{value, -5, -5}, Q116} = pqueue2:pout(Q115),
    {{value, -4, -4}, Q117} = pqueue2:pout(Q116),
    {{value, -4, -4}, Q118} = pqueue2:pout(Q117),
    {{value, -3, -3}, Q119} = pqueue2:pout(Q118),
    {{value, -3, -3}, Q120} = pqueue2:pout(Q119),
    {{value, -2, -2}, Q121} = pqueue2:pout(Q120),
    {{value, -2, -2}, Q122} = pqueue2:pout(Q121),
    {{value, -1, -1}, Q123} = pqueue2:pout(Q122),
    {{value, -1, -1}, Q124} = pqueue2:pout(Q123),
    {{value, 1, 1}, Q125} = pqueue2:pout(Q124),
    {{value, 1, 1}, Q126} = pqueue2:pout(Q125),
    {{value, 2, 2}, Q127} = pqueue2:pout(Q126),
    {{value, 2, 2}, Q128} = pqueue2:pout(Q127),
    {{value, 3, 3}, Q129} = pqueue2:pout(Q128),
    {{value, 3, 3}, Q130} = pqueue2:pout(Q129),
    {{value, 4, 4}, Q131} = pqueue2:pout(Q130),
    {{value, 4, 4}, Q132} = pqueue2:pout(Q131),
    {{value, 5, 5}, Q133} = pqueue2:pout(Q132),
    {{value, 5, 5}, Q134} = pqueue2:pout(Q133),
    {{value, 6, 6}, Q135} = pqueue2:pout(Q134),
    {{value, 6, 6}, Q136} = pqueue2:pout(Q135),
    {{value, 7, 7}, Q137} = pqueue2:pout(Q136),
    {{value, 7, 7}, Q138} = pqueue2:pout(Q137),
    {{value, 8, 8}, Q139} = pqueue2:pout(Q138),
    {{value, 8, 8}, Q140} = pqueue2:pout(Q139),
    {{value, 9, 9}, Q141} = pqueue2:pout(Q140),
    {{value, 9, 9}, Q142} = pqueue2:pout(Q141),
    {{value, 10, 10}, Q143} = pqueue2:pout(Q142),
    {{value, 10, 10}, Q144} = pqueue2:pout(Q143),
    {{value, 11, 11}, Q145} = pqueue2:pout(Q144),
    {{value, 11, 11}, Q146} = pqueue2:pout(Q145),
    {{value, 12, 12}, Q147} = pqueue2:pout(Q146),
    {{value, 12, 12}, Q148} = pqueue2:pout(Q147),
    {{value, 13, 13}, Q149} = pqueue2:pout(Q148),
    {{value, 13, 13}, Q150} = pqueue2:pout(Q149),
    {{value, 14, 14}, Q151} = pqueue2:pout(Q150),
    {{value, 14, 14}, Q152} = pqueue2:pout(Q151),
    {{value, 15, 15}, Q153} = pqueue2:pout(Q152),
    {{value, 15, 15}, Q154} = pqueue2:pout(Q153),
    {{value, 16, 16}, Q155} = pqueue2:pout(Q154),
    {{value, 16, 16}, Q156} = pqueue2:pout(Q155),
    {{value, 17, 17}, Q157} = pqueue2:pout(Q156),
    {{value, 17, 17}, Q158} = pqueue2:pout(Q157),
    {{value, 18, 18}, Q159} = pqueue2:pout(Q158),
    {{value, 18, 18}, Q160} = pqueue2:pout(Q159),
    {{value, 19, 19}, Q161} = pqueue2:pout(Q160),
    {{value, 19, 19}, Q162} = pqueue2:pout(Q161),
    {{value, 20, 20}, Q163} = pqueue2:pout(Q162),
    {{value, 20, 20}, Q164} = pqueue2:pout(Q163),
    true = pqueue2:is_empty(Q164),
    {empty, Q165} = pqueue2:pout(Q164),
    true = pqueue2:is_empty(Q165),
    % test case 1, based on proper testing
    C1V0 = pqueue2:in(-18, pqueue2:new()),
    C1V1 = pqueue2:in(9, C1V0),
    C1V2 = pqueue2:in(-10, -4, C1V1),
    C1V3 = pqueue2:in(-29, C1V2),
    C1V4 = pqueue2:in(11, C1V3),
    5 = pqueue2:len(C1V4),
    [-10, -18, 9, -29, 11] = pqueue2:to_list(C1V4),
    % test case 2, based on proper testing
    C2V0 = pqueue2:in(-4, -15, pqueue2:new()),
    C2V1 = pqueue2:in(13, C2V0),
    C2V2 = pqueue2:in(2, C2V1),
    [-4, 13, 2] = to_list(C2V2),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

merge(empty, empty) ->
    empty;
merge(empty, {_, _, _, _, _} = H) ->
    H;
merge({_, _, _, _, _} = H, empty) ->
    H;
merge({P1, HL1, HR1, T, D}, {P2, _, _, _, _} = H2) when P1 < P2 ->
    {P1, HL1, merge(HR1, H2), T, D};
merge({P1, _, _, _, _} = H1, {P2, HL2, HR2, T, D}) when P1 > P2 ->
    {P2, HL2, merge(H1, HR2), T, D};
merge({P, HL1, HR1, element, Value1}, {P, HL2, HR2, element, Value2}) ->
    {P, merge(HL1, HR1), merge(HL2, HR2), queue,
     queue:from_list([Value2, Value1])};
merge({P, HL1, HR1, queue, Queue}, {P, HL2, HR2, element, Value}) ->
    {P, merge(HL1, HR1), merge(HL2, HR2), queue, queue:in(Value, Queue)};
merge({P, HL1, HR1, element, Value}, {P, HL2, HR2, queue, Queue}) ->
    {P, merge(HL1, HR1), merge(HL2, HR2), queue, queue:in(Value, Queue)}.

