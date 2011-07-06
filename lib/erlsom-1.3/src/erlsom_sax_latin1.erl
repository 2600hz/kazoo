%%% Copyright (C) 2006 - 2008 Willem de Jong
%%%
%%% This file is part of Erlsom.
%%%
%%% Erlsom is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as 
%%% published by the Free Software Foundation, either version 3 of 
%%% the License, or (at your option) any later version.
%%%
%%% Erlsom is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public 
%%% License along with Erlsom.  If not, see 
%%% <http://www.gnu.org/licenses/>.
%%%
%%% Author contact: w.a.de.jong@gmail.com

%%% ====================================================================
%%% An XML parser, using the SAX model.
%%% ====================================================================

%% this file exists several times, but with different names: 
%% erlsom_sax_utf8, erlsom_sax_latin1 etc.
%% The only difference to the content of these 2 files is the definition below:
%% it can be UTF8, LAT1, U16B or U16L. (The names have been chosen so that the 
%% number of bytes in the file will be the same in either case, so that it is 
%% easy to see whether the files are the same, although this check is obviously 
%% rather primitive.)

-define(LAT1, true).
-ifdef(UTF8).
-module(erlsom_sax_utf8).
-define(BINARY, true).
-define(STR1(X), <<X>>).
-define(STR2(X1, X2), <<X1, X2>>).
-define(STR3(X1, X2, X3), <<X1, X2, X3>>).
-define(STR4(X1, X2, X3, X4), <<X1, X2, X3, X4>>).
-define(STR5(X1, X2, X3, X4, X5), <<X1, X2, X3, X4, X5>>).
-define(STR6(X1, X2, X3, X4, X5, X6), <<X1, X2, X3, X4, X5, X6>>).
-define(STR7(X1, X2, X3, X4, X5, X6, X7), <<X1, X2, X3, X4, X5, X6, X7>>).
-define(STR8(X1, X2, X3, X4, X5, X6, X7, X8), <<X1, X2, X3, X4, X5, X6, X7, X8>>).
-define(DONTCARE_T(Y), <<_, Y/binary>>).
-define(STR1_T(X, Y), <<X, Y/binary>>).
-define(STR2_T(X1, X2, Y), <<X1, X2, Y/binary>>).
-define(STR3_T(X1, X2, X3, Y), <<X1, X2, X3, Y/binary>>).
-define(STR4_T(X1, X2, X3, X4, Y), <<X1, X2, X3, X4, Y/binary>>).
-define(STR7_T(X1, X2, X3, X4, X5, X6, X7, Y), <<X1, X2, X3, X4, X5, X6, X7, Y/binary>>).
-define(STR8_T(X1, X2, X3, X4, X5, X6, X7, X8, Y), <<X1, X2, X3, X4, X5, X6, X7, X8, Y/binary>>).
-define(STR9_T(X1, X2, X3, X4, X5, X6, X7, X8, X9, Y), <<X1, X2, X3, X4, X5, X6, X7, X8, X9, Y/binary>>).
-define(BOM1(X), <<16#EF, 16#BB, 16#BF, X/binary>>).
-define(BOM2, <<16#EF, 16#BB>>).
-define(BOM3, <<16#EF>>).
-endif.

-ifdef(U16B).
-module(erlsom_sax_utf16be).
-define(BINARY, true).
-define(STR1(X), <<0, X>>).
-define(STR2(X1, X2), <<0, X1, 0, X2>>).
-define(STR3(X1, X2, X3), <<0, X1, 0, X2, 0, X3>>).
-define(STR4(X1, X2, X3, X4), <<0, X1, 0, X2, 0, X3, 0, X4>>).
-define(STR5(X1, X2, X3, X4, X5), <<0, X1, 0, X2, 0, X3, 0, X4, 0, X5>>).
-define(STR6(X1, X2, X3, X4, X5, X6), <<0, X1, 0, X2, 0, X3, 0, X4, 0, X5, 0, X6>>).
-define(STR7(X1, X2, X3, X4, X5, X6, X7), <<0, X1, 0, X2, 0, X3, 0, X4, 0, X5, 0, X6, 0, X7>>).
-define(STR8(X1, X2, X3, X4, X5, X6, X7, X8), <<0, X1, 0, X2, 0, X3, 0, X4, 0, X5, 0, X6, 0, X7, 0, X8>>).
-define(DONTCARE_T(Y), <<_, _, Y/binary>>).
-define(STR1_T(X, Y), <<0, X, Y/binary>>).
-define(STR2_T(X1, X2, Y), <<0, X1, 0, X2, Y/binary>>).
-define(STR3_T(X1, X2, X3, Y), <<0, X1, 0, X2, 0, X3, Y/binary>>).
-define(STR4_T(X1, X2, X3, X4, Y), <<0, X1, 0, X2, 0, X3, 0, X4, Y/binary>>).
-define(STR7_T(X1, X2, X3, X4, X5, X6, X7, Y), <<0, X1, 0, X2, 0, X3, 0, X4, 0, X5, 0, X6, 0, X7, Y/binary>>).
-define(STR8_T(X1, X2, X3, X4, X5, X6, X7, X8, Y), <<0, X1, 0, X2, 0, X3, 0, X4, 0, X5, 0, X6, 0, X7, 0, X8, Y/binary>>).
-define(STR9_T(X1, X2, X3, X4, X5, X6, X7, X8, X9, Y), 
               <<0, X1, 0, X2, 0, X3, 0, X4, 0, X5, 0, X6, 0, X7, 0, X8, 0, X9, Y/binary>>).
-define(BOM1(X), <<16#FE, 16#FF, X/binary>>).
-define(BOM2, <<16#FE>>).
-define(BOM3, no_match).
-endif.

-ifdef(U16L).
-module(erlsom_sax_utf16le).
-define(BINARY, true).
-define(STR1(X), <<X, 0>>).
-define(STR2(X1, X2), <<X1, 0, X2, 0>>).
-define(STR3(X1, X2, X3), <<X1, 0, X2, 0, X3, 0>>).
-define(STR4(X1, X2, X3, X4), <<X1, 0, X2, 0, X3, 0, X4, 0>>).
-define(STR5(X1, X2, X3, X4, X5), <<X1, 0, X2, 0, X3, 0, X4, 0, X5, 0>>).
-define(STR6(X1, X2, X3, X4, X5, X6), <<X1, 0, X2, 0, X3, 0, X4, 0, X5, 0, X6, 0>>).
-define(STR7(X1, X2, X3, X4, X5, X6, X7), <<X1, 0, X2, 0, X3, 0, X4, 0, X5, 0, X6, 0, X7, 0>>).
-define(STR8(X1, X2, X3, X4, X5, X6, X7, X8), <<X1, 0, X2, 0, X3, 0, X4, 0, X5, 0, X6, 0, X7, 0, X8, 0>>).
-define(DONTCARE_T(Y), <<_, _, Y/binary>>).
-define(STR1_T(X, Y), <<X, 0, Y/binary>>).
-define(STR2_T(X1, X2, Y), <<X1, 0, X2, 0, Y/binary>>).
-define(STR3_T(X1, X2, X3, Y), <<X1, 0, X2, 0, X3, 0, Y/binary>>).
-define(STR4_T(X1, X2, X3, X4, Y), <<X1, 0, X2, 0, X3, 0, X4, 0, Y/binary>>).
-define(STR7_T(X1, X2, X3, X4, X5, X6, X7, Y), <<X1, 0, X2, 0, X3, 0, X4, 0, X5, 0, X6, 0, X7, 0, Y/binary>>).
-define(STR8_T(X1, X2, X3, X4, X5, X6, X7, X8, Y), <<X1, 0, X2, 0, X3, 0, X4, 0, X5, 0, X6, 0, X7, 0, X8, 0, Y/binary>>).
-define(STR9_T(X1, X2, X3, X4, X5, X6, X7, X8, X9, Y), 
               <<X1, 0, X2, 0, X3, 0, X4, 0, X5, 0, X6, 0, X7, 0, X8, 0, X9, 0, Y/binary>>).
-define(BOM1(X), <<16#FF, 16#FE, X/binary>>).
-define(BOM2, <<16#FF>>).
-define(BOM3, no_match).
-endif.

-ifdef(LAT1).
-module(erlsom_sax_latin1).
-define(BINARY, true).
-define(STR1(X), <<X>>).
-define(STR2(X1, X2), <<X1, X2>>).
-define(STR3(X1, X2, X3), <<X1, X2, X3>>).
-define(STR4(X1, X2, X3, X4), <<X1, X2, X3, X4>>).
-define(STR5(X1, X2, X3, X4, X5), <<X1, X2, X3, X4, X5>>).
-define(STR6(X1, X2, X3, X4, X5, X6), <<X1, X2, X3, X4, X5, X6>>).
-define(STR7(X1, X2, X3, X4, X5, X6, X7), <<X1, X2, X3, X4, X5, X6, X7>>).
-define(STR8(X1, X2, X3, X4, X5, X6, X7, X8), <<X1, X2, X3, X4, X5, X6, X7, X8>>).
-define(DONTCARE_T(Y), <<_, Y/binary>>).
-define(STR1_T(X, Y), <<X, Y/binary>>).
-define(STR2_T(X1, X2, Y), <<X1, X2, Y/binary>>).
-define(STR3_T(X1, X2, X3, Y), <<X1, X2, X3, Y/binary>>).
-define(STR4_T(X1, X2, X3, X4, Y), <<X1, X2, X3, X4, Y/binary>>).
-define(STR7_T(X1, X2, X3, X4, X5, X6, X7, Y), <<X1, X2, X3, X4, X5, X6, X7, Y/binary>>).
-define(STR8_T(X1, X2, X3, X4, X5, X6, X7, X8, Y), <<X1, X2, X3, X4, X5, X6, X7, X8, Y/binary>>).
-define(STR9_T(X1, X2, X3, X4, X5, X6, X7, X8, X9, Y), <<X1, X2, X3, X4, X5, X6, X7, X8, X9, Y/binary>>).
-define(BOM1(X), [no_match | X]).
-define(BOM2, no_match).
-define(BOM3, no_match2).
-endif.

-ifdef(LIST).
-module(erlsom_sax_list).
-define(EMPTY, []).
-define(STR1(X), [X]).
-define(STR2(X1, X2), [X1, X2]).
-define(STR3(X1, X2, X3), [X1, X2, X3]).
-define(STR4(X1, X2, X3, X4), [X1, X2, X3, X4]).
-define(STR5(X1, X2, X3, X4, X5), [X1, X2, X3, X4, X5]).
-define(STR6(X1, X2, X3, X4, X5, X6), [X1, X2, X3, X4, X5, X6]).
-define(STR7(X1, X2, X3, X4, X5, X6, X7), [X1, X2, X3, X4, X5, X6, X7]).
-define(STR8(X1, X2, X3, X4, X5, X6, X7, X8), [X1, X2, X3, X4, X5, X6, X7, X8]).
-define(DONTCARE_T(Y), [_ | Y]).
-define(STR1_T(X, Y), [X | Y]).
-define(STR2_T(X1, X2, Y), [X1, X2 | Y]).
-define(STR3_T(X1, X2, X3, Y), [X1, X2, X3 | Y]).
-define(STR4_T(X1, X2, X3, X4, Y), [X1, X2, X3, X4 | Y]).
-define(STR7_T(X1, X2, X3, X4, X5, X6, X7, Y), [X1, X2, X3, X4, X5, X6, X7 |Y]).
-define(STR8_T(X1, X2, X3, X4, X5, X6, X7, X8, Y), [X1, X2, X3, X4, X5, X6, X7, X8 | Y]).
-define(BOM1(X), [65279 | X]).
-define(BOM2, no_match).
-define(BOM3, no_match2).
-endif.

-ifdef(BINARY).
-define(EMPTY, <<>>).
-endif.

%% these are only here to save some typing
-define(CF3(A, B, C), erlsom_sax_lib:continueFun(A, B, C)).
-define(CF4(A, B, C, D), erlsom_sax_lib:continueFun(A, B, C, D)).
-define(CF4_2(A, B, C, D), erlsom_sax_lib:continueFun2(A, B, C, D)).
-define(CF5(A, B, C, D, E), erlsom_sax_lib:continueFun(A, B, C, D, E)).
-define(CF6(A, B, C, D, E, F), erlsom_sax_lib:continueFun(A, B, C, D, E, F)).
-define(CF6_2(A, B, C, D, E, F), erlsom_sax_lib:continueFun2(A, B, C, D, E, F)).

-include_lib("erlsom_sax.hrl").
-export([parse/2]).

parse(Xml, State) ->
  State2 = wrapCallback(startDocument, State),
  {State3, Tail} = parseProlog(Xml, State2),
  State4 = wrapCallback(endDocument, State3),
  {ok, State4#erlsom_sax_state.user_state, Tail}.

%% returns {State, Tail}
parseProlog(?EMPTY, State) ->
  ?CF3(?EMPTY, State, fun parseProlog/2);
parseProlog(?STR1($<), State) ->
  ?CF3(?STR1($<), State, fun parseProlog/2);
parseProlog(?STR2_T($<, $?, Tail), State) ->
  {processinginstruction, Target, Data, Tail2, State2} = 
    parseProcessingInstruction(Tail, State),
  State3 = wrapCallback({processingInstruction, Target, lists:reverse(Data)}, State2),
  parseProlog(Tail2, State3);
parseProlog(?STR2_T($<, $!, Tail) = T, State) ->
  case Tail of
    ?STR2_T($-, $-, Tail2) -> 
      {comment, Tail3, State2} = parseComment(Tail2, State),
      parseProlog(Tail3, State2);
    ?STR7_T($D, $O, $C, $T, $Y, $P, $E, Tail2) -> 
      {dtd, Tail3, State2} = parseDTD(Tail2, State),
      parseProlog(Tail3, State2);
    ?STR6($D, $O, $C, $T, $Y, $P) -> ?CF3(T, State, fun parseProlog/2);
    ?STR5($D, $O, $C, $T, $Y) -> ?CF3(T, State, fun parseProlog/2);
    ?STR4($D, $O, $C, $T) -> ?CF3(T, State, fun parseProlog/2);
    ?STR3($D, $O, $C) -> ?CF3(T, State, fun parseProlog/2);
    ?STR2($D, $O) -> ?CF3(T, State, fun parseProlog/2);
    ?STR1($D) -> ?CF3(T, State, fun parseProlog/2);
    ?STR1($-) -> ?CF3(T, State, fun parseProlog/2);
    ?EMPTY -> ?CF3(T, State, fun parseProlog/2);
    _ -> throw({error, "Malformed: Illegal character in prolog"})
  end;
parseProlog(T = ?STR1_T($<, _Tail), State) ->
  parseContent(T, State);
%% whitespace in the prolog is ignored
parseProlog(?STR1_T(NextChar, Tail), State) 
  when ?is_whitespace(NextChar) ->
  parseProlog(Tail, State);
%% non-breaking space, used as byte order mark
parseProlog(?BOM1(Tail), State) ->
  parseProlog(Tail, State);
parseProlog(?BOM2, State) ->
  ?CF3(?BOM2, State, fun parseProlog/2);
parseProlog(?BOM3, State) ->
  ?CF3(?BOM3, State, fun parseProlog/2);
parseProlog(_Tail, _) ->
  throw({error, "Malformed: Illegal character in prolog"}).

-ifdef(UTF8).
%% Decode the next character
%% Tail = the rest of the XML
%% returns {Char, Tail2, State2}
decodeChar(Tail, State) -> 
  case Tail of
    ?EMPTY -> ?CF3(?EMPTY, State, fun decodeChar/2);
    <<C1, Tail2/binary>> when C1 < 16#80 ->
      {C1, Tail2, State};
    <<C1, C2, Tail2/binary>> when C1 band 16#E0 =:= 16#C0,
                                  C2 band 16#C0 =:= 16#80 ->
      {decode2(C1, C2), Tail2, State};
    <<C1>> when C1 band 16#E0 =:= 16#C0 ->
      ?CF3(<<C1>>, State, fun decodeChar/2);
    <<C1, C2, C3, Tail2/binary>> when C1 band 16#F0 =:= 16#E0,
                                      C2 band 16#C0 =:= 16#80,
                                      C3 band 16#C0 =:= 16#80 ->
      {decode3(C1, C2, C3), Tail2, State};
    <<C1, C2>> when C1 band 16#F0 =:= 16#E0,
                    C2 band 16#C0 =:= 16#80 ->
      ?CF3(<<C1, C2>>, State, fun decodeChar/2);
    <<C1>> when C1 band 16#F0 =:= 16#E0 ->
      ?CF3(<<C1>>, State, fun decodeChar/2);
    <<C1,C2,C3,C4, Tail2/binary>> when C1 band 16#F8 =:= 16#F0,
                                       C2 band 16#C0 =:= 16#80,
                                       C3 band 16#C0 =:= 16#80,
                                       C4 band 16#C0 =:= 16#80 ->
      {decode4(C1, C2, C3, C4), Tail2, State};
    <<C1,C2,C3>> when C1 band 16#F8 =:= 16#F0,
                      C2 band 16#C0 =:= 16#80,
                      C3 band 16#C0 =:= 16#80 ->
      ?CF3(<<C1, C2, C3>>, State, fun decodeChar/2);
    <<C1,C2>> when C1 band 16#F8 =:= 16#F0,
                   C2 band 16#C0 =:= 16#80 ->
      ?CF3(<<C1, C2>>, State, fun decodeChar/2);
    <<C1>> when C1 band 16#F8 =:= 16#F0 ->
      ?CF3(<<C1>>, State, fun decodeChar/2);
    <<_C1, _C2>> ->
      throw({error, "Decoding error: illegal UTF-8 encoding"})
  end.

%% decodes an UTF-8 encoded character that consists of 2 bytes.
decode2(C1, C2) ->
  case ((C1 band 16#1F) bsl 6) bor (C2 band 16#3F) of
    C when 16#80 =< C ->
      C;
    _ ->
      %% Bad range.
      throw({error, "Decoding error: illegal UTF-8 encoding"})
  end.

decode3(C1, C2, C3) ->
  case ((((C1 band 16#0F) bsl 6) bor (C2 band 16#3F)) bsl 6) bor
       (C3 band 16#3F) of
    C when 16#800 =< C ->
      C;
    _ ->
      %% Bad range.
      throw({error, "Decoding error: illegal UTF-8 encoding"})
  end.

decode4(C1, C2, C3, C4) ->
  case ((((((C1 band 16#0F) bsl 6) bor (C2 band 16#3F)) bsl 6) bor
       (C3 band 16#3F)) bsl 6) bor (C4 band 16#3F) of
    C when 16#10000 =< C ->
      C;
    _ ->
      %% Bad range.
      throw({error, "Decoding error: illegal UTF-8 encoding"})
  end.
-endif.

-ifdef(U16B).
decodeChar(Tail, State) -> 
  case Tail of
    ?EMPTY -> ?CF3(Tail, State, fun decodeChar/2);
    <<_>> -> 
      %% incomplete
      ?CF3(Tail, State, fun decodeChar/2);
    <<C1, C2, Tail2/binary>> when C1 < 16#D8; C1 > 16#DF ->
      {C1 * 256 + C2, Tail2, State};
    <<_Hi1, _Hi2, _Lo1>> -> 
      %% incomplete
      ?CF3(Tail, State, fun decodeChar/2);
    <<Hi1, Hi2, Lo1, Lo2, Tail2/binary>> 
      when Hi1 >= 16#D8, Hi1 < 16#DC, Lo1 >= 16#DC, Lo1 < 16#E0 ->
        %% Surrogate pair
        Hi = Hi1 * 256 + Hi2,
        Lo = Lo1 * 256 + Lo2,
        Ch = ((Hi band 16#3FF) bsl 10) + (Lo band 16#3FF) + 16#10000,
        {Ch, Tail2, State};
    <<Hi1, _Hi2>> when Hi1 >= 16#D8, Hi1 < 16#DC ->
      %% Surrogate pair, incomplete
      ?CF3(Tail, State, fun decodeChar/2);
    _ -> 
      {error,not_utf16be}
  end.
-endif.

-ifdef(U16L).
decodeChar(Tail, State) -> 
  case Tail of
    ?EMPTY -> ?CF3(Tail, State, fun decodeChar/2);
    <<_>> -> 
      %% incomplete
      ?CF3(Tail, State, fun decodeChar/2);
    <<C1, C2, Tail2/binary>> when C2 < 16#D8; C2 > 16#DF ->
      {C2 * 256 + C1, Tail2, State};
    <<_Hi1, _Hi2, _Lo1>> -> 
      %% incomplete
      ?CF3(Tail, State, fun decodeChar/2);
    <<Hi1, Hi2, Lo1, Lo2, Tail2/binary>> 
      when Hi2 >= 16#D8, Hi2 < 16#DC, Lo2 >= 16#DC, Lo2 < 16#E0 ->
        %% Surrogate pair
        Hi = Hi2 * 256 + Hi1,
        Lo = Lo2 * 256 + Lo1,
        Ch = ((Hi band 16#3FF) bsl 10) + (Lo band 16#3FF) + 16#10000,
        {Ch, Tail2, State};
    <<_Hi1, Hi2>> when Hi2 >= 16#D8, Hi2 < 16#DC ->
      %% Surrogate pair, incomplete
      ?CF3(Tail, State, fun decodeChar/2);
    _ -> 
      {error,not_utf16le}
  end.
-endif.


-ifdef(LAT1).
decodeChar(Tail, State) -> 
  case Tail of
    ?EMPTY -> ?CF3(Tail, State, fun decodeChar/2);
    ?STR1_T(C, T) -> {C, T, State}
  end.
-endif.

-ifdef(LIST).
decodeChar(Tail, State) -> 
  case Tail of
    ?EMPTY -> ?CF3(Tail, State, fun decodeChar/2);
    ?STR1_T(C, T) -> {C, T, State}
  end.
-endif.

%% returns {cdata, CData, Tail}
parseCDATA(Head, Tail0, State) ->
  case Tail0 of
    ?STR3_T($], $], $>, Tail) -> 
      {cdata, lists:reverse(Head), Tail, State};
    ?STR2($], $]) -> 
      ?CF4(Head, ?STR2($], $]), State, fun parseCDATA/3);
    ?STR1($]) -> 
      ?CF4(Head, ?STR1($]), State, fun parseCDATA/3);
    ?STR1_T(NextChar, Tail) when NextChar < 16#80 ->
      parseCDATA([NextChar | Head], Tail, State);
    ?EMPTY -> 
      ?CF4(Head, ?EMPTY, State, fun parseCDATA/3);
    _ ->
      {Char, Tail2, State2} = decodeChar(Tail0, State),
      parseCDATA([Char | Head], Tail2, State2)
  end.

%% returns {dtd, Tail} 
parseDTD(?STR1_T($[, Tail), State) ->
  {intSubset, Tail2, State2} = parseIntSubset(Tail, State),
  parseDTD(Tail2, State2);
parseDTD(?STR1_T($>, Tail), State) ->
  {dtd, Tail, State};
parseDTD(?DONTCARE_T(Tail), State) ->
  parseDTD(Tail, State);
parseDTD(?EMPTY, State) -> ?CF3(?EMPTY, State, fun parseDTD/2).

%% returns {intSubset, Tail} 
parseIntSubset(?STR1_T($], Tail), State) ->
  {intSubset, Tail, State};
parseIntSubset(?STR1_T(NextChar, Tail), State)
  when ?is_whitespace(NextChar) ->
  parseIntSubset(Tail, State);
%% get rid of whitespace
parseIntSubset(?STR8_T($<, $!, $E, $N, $T, $I, $T, $Y, Tail), State) ->
  case parseEntity(Tail, State) of
    {Tail2, State2} -> parseIntSubset(Tail2, State2);
    Other -> Other
  end;
parseIntSubset(?STR7($<, $!, $E, $N, $T, $I, $T) = T, State) -> ?CF3(T, State, fun parseIntSubset/2);
parseIntSubset(?STR6($<, $!, $E, $N, $T, $I) = T, State) -> ?CF3(T, State, fun parseIntSubset/2);
parseIntSubset(?STR5($<, $!, $E, $N, $T) = T, State) -> ?CF3(T, State, fun parseIntSubset/2);
parseIntSubset(?STR4($<, $!, $E, $N) = T, State) -> ?CF3(T, State, fun parseIntSubset/2);
parseIntSubset(?STR3($<, $!, $E) = T, State) -> ?CF3(T, State, fun parseIntSubset/2);
parseIntSubset(?STR2($<, $!) = T, State) -> ?CF3(T, State, fun parseIntSubset/2);
parseIntSubset(?STR1($<) = T, State) -> ?CF3(T, State, fun parseIntSubset/2);
%% comments (starting with <--)
parseIntSubset(?STR4_T($<, $!, $-, $-, Tail), State) -> 
  {comment, Tail2, State2} = parseComment(Tail, State),
  parseIntSubset(Tail2, State2);
parseIntSubset(?STR3($<, $!, $-) = T, State) -> ?CF3(T, State, fun parseIntSubset/2);
%% parameter entities (starting with %)
parseIntSubset(?STR1_T($%, Tail), State) -> %%
  {Head, Tail2, State2} = parseReference([], parameter, Tail, State),
  parseIntSubset(Head ++ Tail2, State2);
%% all other things starting with <
parseIntSubset(?STR1_T($<, Tail), State) -> 
  parseMarkupDecl(Tail, State);
parseIntSubset(?EMPTY, State) -> ?CF3(?EMPTY, State, fun parseIntSubset/2).

parseMarkupDecl(?STR1_T($>, Tail), State) -> 
  parseIntSubset(Tail, State);
parseMarkupDecl(?STR1_T($", Tail), State) -> %"
  {value, _, Tail2, State2} = parseLiteralValue(Tail, $", [], definition, State), %"
  parseMarkupDecl(Tail2, State2);
parseMarkupDecl(?STR1_T($', Tail), State) -> %'
  {value, _, Tail2, State2} = parseLiteralValue(Tail, $', [], definition, State), %'
  parseMarkupDecl(Tail2, State2);
parseMarkupDecl(?STR1_T(_, Tail), State) -> 
  parseMarkupDecl(Tail, State);
parseMarkupDecl(?EMPTY, State) -> ?CF3(?EMPTY, State, fun parseMarkupDecl/2).


%% returns:
%% {Tail2, State2}, where the parsed entity has been added to the State
parseEntity(?STR1_T(NextChar, Tail), State)
  when ?is_whitespace(NextChar) ->
  parseEntity(Tail, State);
parseEntity(?STR1_T(NextChar, _) = Tail, State) when ?is_namestart_char(NextChar) ->
  parseEntityName(Tail, State);
parseEntity(?EMPTY, State) ->
  ?CF3(?EMPTY, State, fun parseEntity/2);
parseEntity(Tail, State) ->
  parseEntityName(Tail, State).
  %% {Char, _Tail2, State2} = decodeChar(Tail, State),
  %% case Char of
    %% _ when ?is_namestart_char2(Char) ->
      %% parseEntityName(Tail, State2);
    %% _ ->
      %% throw({error, "Malformed: Illegal character in entity name"})
  %% end.

parseEntityName(Tail, State) ->
  CurrentEntity = State#erlsom_sax_state.current_entity,
  {Type, Tail2, State2} = getType(Tail, State),
  {Name, Tail3, State3} = parseNameNoNamespaces(Tail2, State2),
  {value, Value, Tail4, State4} = 
    parseLiteral(definition, Tail3, State3#erlsom_sax_state{current_entity = Name}),
  %% this is a bit of a hack - parseliteral may return an encoded value, 
  %% but that is not what we want here.
  ValueAsList = case State4#erlsom_sax_state.output of
                  'utf8' -> erlsom_ucs:decode_utf8(Value);
                  _ -> Value
                end,
  {Tail5, State5} = parseEndHook(Tail4, State4),
  case Type of
    general ->
      EntitiesSoFar = State5#erlsom_sax_state.entities,
      State6 = State5#erlsom_sax_state{
        %% if an entity is declared twice, the first definition should be used.
        %% Therefore, add new ones to the back of the list.
        entities = EntitiesSoFar ++ [{Name, ValueAsList}],
        current_entity = CurrentEntity};
    parameter ->
      EntitiesSoFar = State5#erlsom_sax_state.par_entities,
      State6 = State5#erlsom_sax_state{
        par_entities = EntitiesSoFar ++ [{Name, ValueAsList}]}
  end,
  {Tail5, State6}.

getType(?STR1_T(NextChar, Tail), State)
  when ?is_whitespace(NextChar) ->
  getType(Tail, State);
getType(?STR2_T($%, NextChar, Tail), State)
  when ?is_whitespace(NextChar) ->
  {parameter, Tail, State};
getType(?EMPTY, State) ->
  ?CF3(?EMPTY, State, fun getType/2);
getType(Tail, State) ->
  {general, Tail, State}.


%% returns {comment, Tail}
parseComment(?STR1_T($-, Tail), State) ->
  case Tail of
    ?STR2_T($-, $>, Tail2) -> {comment, Tail2, State};
    ?STR1($-) -> ?CF3(?STR2($-, $-), State, fun parseComment/2);
    ?EMPTY -> ?CF3(?STR1($-), State, fun parseComment/2);
    ?STR1_T($-, _) -> throw({error, "Malformed: -- not allowed in comment"});
    _ -> parseComment(Tail, State)
  end;
parseComment(?DONTCARE_T(Tail), State) ->
  parseComment(Tail, State);
parseComment(?EMPTY, State) ->
  ?CF3(?EMPTY, State, fun parseComment/2).

%% returns {processinginstruction, Target, Data, Tail}
parseProcessingInstruction(Tail, State) ->
  {Target, Tail2, State2} = parseNameNoNamespaces(Tail, State),
  {Data, Tail3, State3} = parsePIData([], Tail2, State2),
  {processinginstruction, Target, Data, Tail3, State3}.

%% returns {Data, Tail}
parsePIData(Head, Tail, State) ->
  case Tail of
    ?STR2_T($?, $>, Tail2) -> {Head, Tail2, State};
    ?STR1($?) -> ?CF4(Head, ?STR1($?), State, fun parsePIData/3);
    ?STR1_T(NextChar, Tail2) when NextChar < 16#80 ->
      parsePIData([NextChar | Head], Tail2, State);
    ?EMPTY -> 
      ?CF4(Head, ?EMPTY, State, fun parsePIData/3);
    _ -> 
      {Char, Tail2, State2} = decodeChar(Tail, State),
      parsePIData([Char | Head], Tail2, State2)
  end.

%% function to call the Callback function for all elements in a list of 'new namespaces'.
%% returns State
mapStartPrefixMappingCallback([{Prefix, Uri} | Tail], State) ->
  mapStartPrefixMappingCallback(Tail, wrapCallback({startPrefixMapping, Prefix, Uri}, State));

mapStartPrefixMappingCallback([], State) ->
  State.

%% function to call the Callback function for all elements in a list of 'new namespaces'.
%% returns State
mapEndPrefixMappingCallback([{Prefix, _Uri} | Tail], State) ->
  mapEndPrefixMappingCallback(Tail, wrapCallback({endPrefixMapping, Prefix}, State));

mapEndPrefixMappingCallback([], State) ->
  State.

%% the '<' is already removed
%% returns {starttag, StartTag, Attributes, Tail}
%% or {emptyelement, StartTag, Attributes, Tail}
%%
%% where StartTag = {Prefix, LocalName, QualifiedName}
%%
parseStartTag(Tail, State) ->
  parseTagName(Tail, State).

%% parseTagName
%% returns {Name, Tail}, where
%% Name = {Prefix, LocalName, QualifiedName}
%%
%% To do: introduce a parameter that indicates whether we are using 
%% namespaces. 
parseTagName(?STR1_T(Char, Tail), State) 
  when ?is_namestart_char(Char) ->
  %% this should differentiate between 'with namespaces'and 'without'
  %% for the moment the assumption is 'with', therfore a name cannot
  %% start with a ':'.
  parseTagName([Char], Tail, State);
parseTagName(?EMPTY, State) ->
  ?CF3(?EMPTY, State, fun parseTagName/2);
parseTagName(Tail, State) ->
  {Char, Tail2, State2} = decodeChar(Tail, State),
  case Char of
    _ when ?is_namestart_char2(Char) ->
      parseTagName([Char], Tail2, State2);
    _ ->
      throw({error, "Malformed: Illegal character in tag"})
  end.

parseTagName(Head, ?STR1_T(NextChar, Tail), State) 
  when ?is_name_char(NextChar) ->
  parseTagName([NextChar | Head], Tail, State);
parseTagName(Head, ?STR1_T($:, Tail), State) ->
  parseTagName(Head, [], Tail, State);
parseTagName(Head, ?STR1_T($>, Tail), State) ->
  LocalName = lists:reverse(Head),
  {starttag, {[], LocalName, LocalName}, [], Tail, State};
parseTagName(Head, ?STR2_T($/, $>, Tail), State) ->
  LocalName = lists:reverse(Head),
  {emptyelement, {[], LocalName, LocalName}, [], Tail, State};
parseTagName(Head, ?STR1_T(NextChar, Tail), State) 
  when ?is_whitespace(NextChar) ->
  LocalName = lists:reverse(Head),
  parseAttributes({[], LocalName, LocalName}, [], Tail, State);
parseTagName(Head, ?STR1($/), State) ->
  ?CF4(Head, ?STR1($/), State, fun parseTagName/3);
parseTagName(Head, ?EMPTY, State) ->
  ?CF4(Head, ?EMPTY, State, fun parseTagName/3);
parseTagName(Head, Tail, State) ->
  {Char, Tail2, State2} = decodeChar(Tail, State),
  if 
    ?is_name_char2(Char) ->
      parseTagName([Char | Head], Tail2, State2);
    true ->
      throw({error, "Malformed: Illegal character in tag"})
  end.

%% should there be another check on the first character of the local name?
parseTagName(Prefix, Head, ?STR1_T(NextChar, Tail), State)
  when ?is_name_char(NextChar) ->
  parseTagName(Prefix, [NextChar | Head], Tail, State);
parseTagName(Prefix, Head, ?STR1_T(NextChar, Tail), State)
  when ?is_whitespace(NextChar) ->
  Pf = lists:reverse(Prefix), 
  Hd = lists:reverse(Head), 
  parseAttributes({Pf, Hd, lists:append([Pf, ":", Hd])}, [], Tail, State);
parseTagName(Prefix, Head, ?STR1_T($>, Tail), State) ->
  Pf = lists:reverse(Prefix), 
  Hd = lists:reverse(Head), 
  {starttag, {Pf, Hd, lists:append([Pf, ":", Hd])}, [], Tail, State};
parseTagName(Prefix, Head, ?STR2_T($/, $>, Tail), State) ->
  Pf = lists:reverse(Prefix), 
  Hd = lists:reverse(Head), 
  {emptyelement, {Pf, Hd, lists:append([Pf, ":", Hd])}, [], Tail, State};
parseTagName(Prefix, Head, ?EMPTY, State) ->
  ?CF5(Prefix, Head, ?EMPTY, State, fun parseTagName/4);
parseTagName(Prefix, Head, ?STR1($/), State) ->
  ?CF5(Prefix, Head, ?STR1($/), State, fun parseTagName/4);
parseTagName(Prefix, Head, Tail, State) ->
  {Char, Tail2, State2} = decodeChar(Tail, State),
  if 
    ?is_name_char2(Char) ->
      parseTagName(Prefix, [Char | Head], Tail2, State2);
    true ->
      throw({error, "Malformed: Illegal character in tag"})
  end.

parseAttrName(Head, ?STR1_T($:, Tail), State) ->
  %% Head is the prefix
  parseAttrName(Head, [], Tail, State);
parseAttrName(Head, ?STR1_T(NextChar, Tail), State)
  when ?is_name_char(NextChar) ->
  parseAttrName([NextChar | Head], Tail, State);
parseAttrName(Head, ?STR1_T($=, Tail), State) ->
  LocalName = lists:reverse(Head),
  {{[], LocalName, LocalName}, Tail, State};
parseAttrName(Head, ?STR1_T(NextChar, Tail), State)
  when ?is_whitespace(NextChar) ->
  LocalName = lists:reverse(Head),
  {Tail2, State2} = parseEqualSign(Tail, State),
  {{[], LocalName, LocalName}, Tail2, State2};
parseAttrName(Head, ?EMPTY, State) ->
  ?CF4(Head, ?EMPTY, State, fun parseAttrName/3);
parseAttrName(Head, Tail, State) ->
  {Char, Tail2, State2} = decodeChar(Tail, State),
  if 
    ?is_name_char2(Char) ->
      parseAttrName([Char | Head], Tail2, State2);
    true ->
      throw({error, "Malformed: Illegal character in atribute name"})
  end.

%% should there be another check on the first character of the local name?
parseAttrName(Prefix, Head, ?STR1_T(NextChar, Tail), State)
  when ?is_name_char(NextChar) ->
  parseAttrName(Prefix, [NextChar | Head], Tail, State);
parseAttrName(Prefix, Head, ?STR1_T($=, Tail), State) ->
  Pf = lists:reverse(Prefix), 
  Hd = lists:reverse(Head), 
  {{Pf, Hd, lists:append([Pf, ":", Hd])}, Tail, State};
parseAttrName(Prefix, Head, ?STR1_T(NextChar, Tail), State)
  when ?is_whitespace(NextChar) ->
  Pf = lists:reverse(Prefix), 
  Hd = lists:reverse(Head), 
  {Tail2, State2} = parseEqualSign(Tail, State),
  {{Pf, Hd, lists:append([Pf, ":", Hd])}, Tail2, State2};
parseAttrName(Prefix, Head, ?EMPTY, State) ->
  ?CF5(Prefix, Head, ?EMPTY, State, fun parseAttrName/4);
parseAttrName(Prefix, Head, Tail, State) ->
  {Char, Tail2, State2} = decodeChar(Tail, State),
  if 
    ?is_name_char2(Char) ->
      parseAttrName(Prefix, [Char | Head], Tail2, State2);
    true ->
      throw({error, "Malformed: Illegal character in atribute name"})
  end.

%% returns {Name, Tail, State}
parseNameNoNamespaces(?STR1_T(Char, Tail), State)
  when ?is_namestart_char(Char) ->
  parseNameNoNamespaces([Char], Tail, State);
parseNameNoNamespaces(Tail, State) ->
  {Char, Tail2, State2} = decodeChar(Tail, State),
  if 
    ?is_namestart_char2(Char) ->
      parseNameNoNamespaces([Char], Tail2, State2);
    true ->
      throw({error, "Malformed: Illegal character in name"})
  end.
      
parseNameNoNamespaces(Head, ?STR1_T(NextChar, Tail), State)
  when ?is_name_char(NextChar) ->
  parseNameNoNamespaces([NextChar | Head], Tail, State);
parseNameNoNamespaces(Head, ?STR1_T($:, Tail), State) ->
  parseNameNoNamespaces([$: | Head], Tail, State);
parseNameNoNamespaces(Head, T = ?STR1_T($>, _), State) ->
  {lists:reverse(Head), T, State};
parseNameNoNamespaces(Head, T = ?STR1_T($?, _), State) ->
  {lists:reverse(Head), T, State};
parseNameNoNamespaces(Head, T = ?STR1_T($=, _), State) ->
  {lists:reverse(Head), T, State};
parseNameNoNamespaces(Head, T = ?STR1_T(NextChar, _), State)
  when ?is_whitespace(NextChar) ->
  {lists:reverse(Head), T, State};
parseNameNoNamespaces(Head, ?EMPTY, State) ->
  ?CF4(Head, ?EMPTY, State, fun parseNameNoNamespaces/3);
parseNameNoNamespaces(Head, Tail, State) ->
  {Char, Tail2, State2} = decodeChar(Tail, State),
  if 
    ?is_name_char2(Char) ->
      parseNameNoNamespaces([Char | Head], Tail2, State2);
    true ->
      throw({error, "Malformed: Illegal character in name"})
  end.

%% returns: {attributes, Attributes, Tail}}
%% Attributes = list of {Name, Value} tuples, and
%% Name = {Prefix, LocalName, QualifiedName}.
parseAttributes(StartTag, Attributes, ?STR1_T($>, Tail), State) ->
  {starttag, StartTag, Attributes, Tail, State};
parseAttributes(StartTag, Attributes, ?STR1($/), State) ->
  ?CF5(StartTag, Attributes, ?STR1($/), State, fun parseAttributes/4);
parseAttributes(StartTag, Attributes, ?STR2_T($/, $>, Tail), State) ->
  {emptyelement, StartTag, Attributes, Tail, State};
parseAttributes(StartTag, Attributes, ?STR1_T(NextChar, Tail), State)
  when ?is_whitespace(NextChar) ->
  parseAttributes(StartTag, Attributes, Tail, State);
parseAttributes(StartTag, Attributes, ?STR1_T(NextChar, Tail), State) 
  when ?is_namestart_char(NextChar) ->
    {AttributeName, Tail2, State2} = parseAttrName([NextChar], Tail, State),
    %% {attribute, Attribute, Tail3, State3} = 
      %% parseAttribute([NextChar], Tail, State),
    {value, Value, Tail3, State3} = parseLiteral(attribute, Tail2, State2),
      %% {attribute, {AttributeName, Value}, Tail2, State2};
      %% parseAttributeValue(AttributeName, Tail2, State2),
    parseAttributes(StartTag, [{AttributeName, Value} | Attributes], Tail3, State3);
parseAttributes(StartTag, Attributes, ?EMPTY, State) ->
  ?CF5(StartTag, Attributes, ?EMPTY, State, fun parseAttributes/4);
parseAttributes(StartTag, Attributes, Tail, State) ->
  {Char, Tail2, State2} = decodeChar(Tail, State),
  case Char of 
    _ when ?is_namestart_char2(Char) ->
      {AttributeName, Tail3, State3} = parseAttrName([Char], Tail2, State2),
      {value, Value, Tail4, State4} = parseLiteral(attribute, Tail3, State3),
      %% {attribute, Attribute, Tail3, State3} = 
      %% parseAttribute([Char], Tail2, State2),
      parseAttributes(StartTag, [{AttributeName, Value} | Attributes], Tail4, State4);
    _ ->
      throw({error, "Malformed: Illegal character in name"})
  end.

  
%% returns {value, Value, Tail, State}
%% depending on the context (attribute or definition) the
%% handling of entities is slightly different.
parseLiteral(Context, ?STR1_T($", Tail), State) -> %"
  parseLiteralValue(Tail, $", Context, State); %"
parseLiteral(Context, ?STR1_T($', Tail), State) -> %'  
  parseLiteralValue(Tail, $', Context, State); %'
parseLiteral(Context, ?STR1_T(NextChar, Tail), State) 
  when ?is_whitespace(NextChar) ->
  parseLiteral(Context, Tail, State);
parseLiteral(Context, ?EMPTY, State) -> 
  ?CF4(Context, ?EMPTY, State, fun parseLiteral/3);
parseLiteral(_C, _T, _) ->
  throw({error, "Malformed: Illegal character in literal value"}).

%% TODO: this can be generalized, for example parsing up to the = sign 
%% in an attribute value is exactly the same.
parseEndHook(?STR1_T($>, Tail), State) ->
  {Tail, State};
parseEndHook(?STR1_T(NextChar, Tail), State)
  when ?is_whitespace(NextChar) ->
  parseEndHook(Tail, State);
parseEndHook(?EMPTY, State) -> 
  ?CF3(?EMPTY, State, fun parseEndHook/2);
parseEndHook(_Tail, _) ->
  throw({error, "Malformed: Illegal character in entity definition"}).

parseEqualSign(?STR1_T($=, Tail), State) ->
  {Tail, State};
parseEqualSign(?STR1_T(NextChar, Tail), State)
  when ?is_whitespace(NextChar) ->
  parseEqualSign(Tail, State);
parseEqualSign(?EMPTY, State) -> 
  ?CF3(?EMPTY, State, fun parseEqualSign/2);
parseEqualSign(_Tail, _) ->
  throw({error, "Malformed: Illegal character in attribute name"}).

%% previous char was '<'
parseContentLT(?STR1_T($!, Tail), State) ->
  case Tail of
    ?STR2_T($-, $-, Tail3) ->
      {comment, Tail4, State2} = parseComment(Tail3, State), 
      parseContent(Tail4, State2);
    ?STR1($-) ->
      ?CF3(?STR2($!, $-), State, fun parseContentLT/2);
    ?EMPTY ->
      ?CF3(?STR1($!), State, fun parseContentLT/2);
    ?STR7_T($[, $C, $D, $A, $T, $A, $[, Tail3) ->
      {cdata, CData, Tail4, State2} = parseCDATA([], Tail3, State),
      %% call callback -
      %% If Cdata is preceded and/or followed by text there will be 2 or 3 
      %% events, but that is legal according to the sax doc.
      State3 = wrapCallback({characters, encodeOutput(CData, State)}, State2),
      parseContent(Tail4, State3);
    ?STR6($[, $C, $D, $A, $T, $A) ->
      ?CF3(?STR7($!, $[, $C, $D, $A, $T, $A), State, fun parseContentLT/2);
    ?STR5($[, $C, $D, $A, $T) ->
      ?CF3(?STR6($!, $[, $C, $D, $A, $T), State, fun parseContentLT/2);
    ?STR4($[, $C, $D, $A) ->
      ?CF3(?STR5($!, $[, $C, $D, $A), State, fun parseContentLT/2);
    ?STR3($[, $C, $D) ->
      ?CF3(?STR4($!, $[, $C, $D), State, fun parseContentLT/2);
    ?STR2($[, $C) ->
      ?CF3(?STR3($!, $[, $C), State, fun parseContentLT/2);
    ?STR1($[) ->
      ?CF3(?STR2($!, $[), State, fun parseContentLT/2)
  end;

parseContentLT(?STR1_T($?, Tail), State) ->
  {processinginstruction, Target, Data, Tail3, State2} = 
    parseProcessingInstruction(Tail, State),
  State3 = wrapCallback({processingInstruction, Target, lists:reverse(Data)}, State2),
  parseContent(Tail3, State3);

parseContentLT(?STR1_T($/, Tail), State) ->
  %% this should be the endTag
  [{QName, Uri, LocalName, Prefix, OldNamespaces, NewNamespaces} | EndTags2] = 
    State#erlsom_sax_state.endtags,
  case parseEndTag(Tail, QName, State) of
    {ok, Tail3, State2} -> 
      %% Call the call back functions for the end tag
      State3 = wrapCallback({endElement, Uri, LocalName, Prefix}, State2),
      State4 = mapEndPrefixMappingCallback(NewNamespaces, State3),
      State5 = State4#erlsom_sax_state{namespaces = OldNamespaces, endtags = EndTags2},
      parseContent(Tail3, State5);
    error -> 
      throw({error, "Malformed: Tags don't match"})
  end;

parseContentLT(?EMPTY, State) ->
  ?CF3(?EMPTY, State, fun parseContentLT/2);

parseContentLT(Tail, State) ->
  Namespaces = State#erlsom_sax_state.namespaces,
  case parseStartTag(Tail, State) of
    {emptyelement, {Prefix, _LocalName, _QName}=StartTag, Attributes, Tail2, State2} ->
      {{Uri, LocalName, QName}, Attributes2, NewNamespaces} = 
        createStartTagEvent(StartTag, Namespaces, Attributes),
      %% Call the call back functions
      State3 = mapStartPrefixMappingCallback(NewNamespaces, State2),
      State4 = wrapCallback({startElement, Uri, LocalName, Prefix, Attributes2}, State3),
      State5 = wrapCallback({endElement, Uri, LocalName, QName}, State4),
      State6 = mapEndPrefixMappingCallback(NewNamespaces, State5),
      parseContent(Tail2, State6);
    {starttag, {Prefix, _LocalName, QName} = StartTag, Attributes, Tail2, State2} ->
      EndTags = State#erlsom_sax_state.endtags,
        {{Uri, LocalName, Prefix}, Attributes2, NewNamespaces} = 
      createStartTagEvent(StartTag, Namespaces, Attributes),
      %% Call the call back function
      State3 = mapStartPrefixMappingCallback(NewNamespaces, State2),
      State4 = wrapCallback({startElement, Uri, LocalName, Prefix, Attributes2}, State3),
      State5 = State4#erlsom_sax_state{namespaces = NewNamespaces ++ Namespaces, 
                     endtags = [{QName, Uri, LocalName, Prefix, Namespaces, NewNamespaces} | EndTags]},
      %% TODO: check the order of the namespaces
      parseContent(Tail2, State5)
  end.

parseContent(?STR1_T($<, Tail), State) ->
  parseContentLT(Tail, State);

parseContent(?EMPTY, #erlsom_sax_state{endtags = EndTags} = State) ->
  case EndTags of 
    [] ->
      %% This is the return value. The second element is what
      %% follows the XML document, as a list.
      {State, []};
    _ ->
      ?CF3(?EMPTY, State, fun parseContent/2)
  end;

parseContent(T, #erlsom_sax_state{endtags = EndTags} = State) ->
  case EndTags of
    [] ->
      %% This is the return value. The second element is what
      %% follows the XML document, as a list.
      {State, decode(T)};
    _ ->
     {Tail2, State2} = parseText(T, State),
     parseContentLT(Tail2, State2)
  end.

parseText(Tail, #erlsom_sax_state{output = 'utf8'} = State) ->
  parseTextBinary(<<>>, Tail, State);
parseText(Tail, State) ->
  parseText([], Tail, State).

parseText(Head, ?STR1_T($<, Tail), State) ->
  State2 = wrapCallback({ignorableWhitespace, lists:reverse(Head)}, State),
  {Tail, State2};
parseText(Head, ?STR1_T(NextChar, Tail), State)
  when ?is_whitespace(NextChar) -> 
  parseText([NextChar | Head], Tail, State);
parseText(Head, ?EMPTY, State) ->
  ?CF4(Head, ?EMPTY, State, fun parseText/3);
parseText(Head, Tail, State) ->
  parseTextNoIgnore(Head, Tail, State).

parseTextNoIgnore(Head, ?STR1_T($<, Tail), State) ->
  State2 = wrapCallback({characters, lists:reverse(Head)}, State),
  {Tail, State2};
parseTextNoIgnore(Head, ?STR1_T($&, Tail), State) ->
  {Head2, Tail2, State2} = parseReference([], element, Tail, State),
  parseTextNoIgnore(Head2 ++ Head, Tail2, State2);
parseTextNoIgnore(Head, ?STR1_T(NextChar, Tail), State) 
  when NextChar < 16#80 ->
  parseTextNoIgnore([NextChar|Head], Tail, State);
parseTextNoIgnore(Head, ?EMPTY, State) ->
  ?CF4(Head, ?EMPTY, State, fun parseTextNoIgnore/3);
parseTextNoIgnore(Head, Tail, State) ->
  {Char, Tail2, State2} = decodeChar(Tail, State),
  parseTextNoIgnore([Char | Head], Tail2, State2).


parseTextBinary(Head, ?STR1_T($<, Tail), State) ->
  State2 = wrapCallback({ignorableWhitespace, Head}, State),
  {Tail, State2};
parseTextBinary(Head, ?STR1_T(NextChar, Tail), State)
  when ?is_whitespace(NextChar) -> 
  parseTextBinary(<<Head/binary, NextChar>>, Tail, State);
parseTextBinary(Head, ?EMPTY, State) ->
  ?CF4(Head, ?EMPTY, State, fun parseTextBinary/3);
parseTextBinary(Head, Tail, State) ->
  parseTextNoIgnoreBinary(Head, Tail, State).

parseTextNoIgnoreBinary(Head, ?STR1_T($<, Tail), State) ->
  State2 = wrapCallback({characters, Head}, State),
  {Tail, State2};
parseTextNoIgnoreBinary(Head, ?STR1_T($&, Tail), State) ->
  {Head2, Tail2, State2} = parseReference([], element, Tail, State),
  %% parseReference returns a list
  Head2Binary = list_to_binary(erlsom_ucs:to_utf8(lists:reverse(Head2))),
  parseTextNoIgnoreBinary(<<Head/binary,  Head2Binary/binary>>, Tail2, State2);
parseTextNoIgnoreBinary(Head, ?STR1_T(NextChar, Tail), State)
  when NextChar < 16#80 ->
  parseTextNoIgnoreBinary(<<Head/binary, NextChar>>, Tail, State);
parseTextNoIgnoreBinary(Head, ?EMPTY, State) ->
  ?CF4(Head, ?EMPTY, State, fun parseTextNoIgnoreBinary/3);
parseTextNoIgnoreBinary(Head, Tail, State) ->
  {Char, Tail2, State2} = decodeChar(Tail, State),
  EncodedChar = erlsom_ucs:char_to_utf8(Char),
  parseTextNoIgnoreBinary(<<Head/binary, EncodedChar/binary>>, Tail2, State2).

%% entity refernces in attribute values differ fundamentally from
%% refernces in elements and in entity definitions
%% Context can be: element, attribute, definition
parseReference(Head, Context, ?STR1_T($;, Tail), State) ->
  translateReference(lists:reverse(Head), Context, Tail, State);
parseReference(Head, Context, ?STR1_T(NextChar, Tail), State)
  when NextChar < 16#80 ->
  parseReference([NextChar | Head], Context, Tail, State);
parseReference(Head, Context, ?EMPTY, State) ->
  ?CF5(Head, Context, ?EMPTY, State, fun parseReference/4);
parseReference(Head, Context, Tail, State) ->
  {Char, Tail2, State2} = decodeChar(Tail, State),
  parseReference([Char | Head], Context, Tail2, State2).
  
%% returns: {Head2, Tail2, State2}
%% Character entities are added to the 'head' (the bit that was parsed already),
%% other entities are added to the tail (they still have to be parsed).
%% The problem here is that we have to make sure that we don't get into an infinite 
%% loop. This solved as follows:
%% We proceed by parsing only the entity (while registring in the state that we 
%% are parsing this particular entity). However, we replace the continuation function 
%% by something that simply returns (in stead of calling the function that it was 
%% working on recursively). We then proceed. 
%% Before starting to work on this entity, we need to check that we are not already 
%% parsing this entity (because that would mean an infinite loop).
translateReference(Reference, Context, Tail, State) ->
  %% in the context of a definition, character references have to be replaced
  %% (and others not). 
  case Reference of
    [$#, $x | Tail1] -> %% hex number of char's code point
      %% unfortunately this function accepts illegal values
      %% to do: replace by something that throws an error in case of
      %% an illegal value
      {[httpd_util:hexlist_to_integer(Tail1)], Tail, State};
    [$# | Tail1] -> %% dec number of char's code point
      case catch list_to_integer(Tail1) of
        {'EXIT', _} -> throw({error, "Malformed: Illegal character in reference"});
	%% to do: check on legal character.
	Other -> {[Other], Tail, State}
      end;
    _ -> 
      translateReferenceNonCharacter(Reference, Context, Tail, State)
  end.

translateReferenceNonCharacter(Reference, Context, Tail, State) ->
  case Context of 
    definition ->
      %% check on circular definition
      CurrentEntity = State#erlsom_sax_state.current_entity,
      Relations = [{CurrentEntity, Reference} | State#erlsom_sax_state.entity_relations],
      case erlsom_sax_lib:findCycle(Relations) of
        true -> throw({error, "Malformed: Cycle in entity definitions"});
        _ -> ok
      end,
      %% dont't replace
      {lists:reverse("&" ++ Reference ++ ";"), Tail, State#erlsom_sax_state{entity_relations = Relations}};
    attribute ->
      {Translation, _Type} = nowFinalyTranslate(Reference, Context, State),
      %% replace, add to the parsed text (head)
      {Translation, Tail, State};
    _ -> %% element or parameter
      {Translation, Type} = nowFinalyTranslate(Reference, Context, State),
      case Type of 
        user_defined ->
          %% replace, encode again and put back into the input stream (Tail)
          TEncoded = encode(Translation),
          {[], combine(TEncoded, Tail), State};
        _ ->
          {Translation, Tail, State}
      end
  end.

nowFinalyTranslate(Reference, Context, State) ->
  case Reference of
    "amp" -> {[$&], other};
    "lt" -> {[$<], other};
    "gt" -> {[$>], other};
    "apos" -> {[39], other}; %% apostrof
    "quot" -> {[34], other}; %% quote
  _ -> 
    ListOfEntities = case Context of 
      parameter -> State#erlsom_sax_state.par_entities;
      element -> State#erlsom_sax_state.entities
    end,
    case lists:keysearch(Reference, 1, ListOfEntities) of
      {value, {_, EntityText}} -> 
        {EntityText, user_defined};
      _ ->
        throw({error, "Malformed: unknown reference: " ++ Reference})
    end
  end.

%% TODO: proper encoding
-ifdef(BINARY).
combine(Head, Tail) ->
  <<Head/binary, Tail/binary>>.
-endif.

-ifdef(UTF8).
encode(List) ->
  list_to_binary(erlsom_ucs:to_utf8(List)).
-endif.

-ifdef(U16B).
encode(List) ->
  list_to_binary(xmerl_ucs:to_utf16be(List)).
-endif.

-ifdef(U16L).
encode(List) ->
  list_to_binary(xmerl_ucs:to_utf16le(List)).
-endif.

-ifdef(LAT1).
encode(List) ->
  list_to_binary(List).
-endif.

-ifdef(LIST).
encode(List) ->
  List.

combine(Head, Tail) ->
  Head ++ Tail.
-endif.


encodeOutput(List, #erlsom_sax_state{output = 'utf8'}) ->
  list_to_binary(erlsom_ucs:to_utf8(List));
encodeOutput(List, _) ->
  List.

%%parseText(Tail, #erlsom_sax_state{output = 'utf8'} = State) ->
  %%parseTextBinary(?EMPTY, Tail, State);
%%parseText(Tail, State) ->
  %%parseText([], Tail, State).
parseLiteralValue(Tail, Quote, Context, #erlsom_sax_state{output = 'utf8'} = State) ->
  parseLiteralValueBinary(Tail, Quote, <<>>, Context, State);
parseLiteralValue(Tail, Quote, Context, State) ->
  parseLiteralValue(Tail, Quote, [], Context, State).

parseLiteralValue(?STR1_T(Quote, Tail), Quote, Head, _Context, State) ->
  {value, lists:reverse(Head), Tail, State};
parseLiteralValue(?STR1_T($&, Tail), Quote, Head, Context, State) ->
  {Reference, Tail2, State2} = parseReference([], Context, Tail, State),
  parseLiteralValue(Tail2, Quote, Reference ++ Head, Context, State2);
parseLiteralValue(?STR1_T($<, Tail), Quote, Head, Context, State) ->
  case Context of
    attribute -> 
      throw({error, "Malformed: < not allowed in literal value"});
    _ -> 
      parseLiteralValue(Tail, Quote, [$< | Head], Context, State)
  end;
parseLiteralValue(?STR1_T($%, Tail), Quote, Head, Context, State) -> %%
  case Context of
    definition -> 
      %% this is weird, but it follows the implementation of MS 
      %% Internet Explorer...
      %% Can't find this in the standard, but it is an unlikely thing 
      %% to happen in a bonafide xml, and it avoids some problems with 
      %% circular definitions
      throw({error, "Malformed: cannot use % in entity definition (?)"});
    _ -> 
      parseLiteralValue(Tail, Quote, [$% | Head], Context, State)
  end;
parseLiteralValue(?STR1_T(NextChar, Tail), Quote, Head, Context, State)
  when NextChar < 16#80 ->
  parseLiteralValue(Tail, Quote, [NextChar | Head], Context, State);
parseLiteralValue(?EMPTY, Quote, Head, Context, State) ->
  ?CF6_2(?EMPTY, Quote, Head, Context, State, fun parseLiteralValue/5);
parseLiteralValue(Tail, Quote, Head, Context, State) ->
  {Char, Tail2, State2} = decodeChar(Tail, State),
  parseLiteralValue(Tail2, Quote, [Char | Head], Context, State2).

parseLiteralValueBinary(?STR1_T(Quote, Tail), Quote, Head, _Context, State) ->
  {value, Head, Tail, State};
parseLiteralValueBinary(?STR1_T($&, Tail), Quote, Head, Context, State) ->
  {Head2, Tail2, State2} = parseReference([], element, Tail, State),
  %% parseReference returns a list (only 1 char long - in case of a 
  %% user defined entity this will be put in front of tail!)
  Head2Binary = list_to_binary(erlsom_ucs:to_utf8(lists:reverse(Head2))),
  parseLiteralValueBinary(Tail2, Quote, <<Head/binary,  Head2Binary/binary>>, Context, State2);
parseLiteralValueBinary(?STR1_T($<, Tail), Quote, Head, Context, State) ->
  case Context of
    attribute -> 
      throw({error, "Malformed: < not allowed in literal value"});
    _ -> 
      parseLiteralValueBinary(Tail, Quote, <<Head/binary, $<>>, Context, State)
  end;
parseLiteralValueBinary(?STR1_T($%, Tail), Quote, Head, Context, State) -> %%
  case Context of
    definition -> 
      throw({error, "Malformed: cannot use % in entity definition (?)"});
    _ -> 
      parseLiteralValueBinary(Tail, Quote, <<Head/binary, $%>>, Context, State)
  end;
parseLiteralValueBinary(?STR1_T(NextChar, Tail), Quote, Head, Context, State)
  when NextChar < 16#80 ->
  parseLiteralValueBinary(Tail, Quote, <<Head/binary, NextChar>>, Context, State);
parseLiteralValueBinary(?EMPTY, Quote, Head, Context, State) ->
  ?CF6_2(?EMPTY, Quote, Head, Context, State, fun parseLiteralValueBinary/5);
parseLiteralValueBinary(Tail, Quote, Head, Context, State) ->
  {Char, Tail2, State2} = decodeChar(Tail, State),
  EncodedChar = erlsom_ucs:char_to_utf8(Char),
  parseLiteralValueBinary(Tail2, Quote, <<Head/binary, EncodedChar/binary>>, Context, State2).

%% the start tag is decoded (it is a list of unicode code points)
parseEndTag(?STR1_T(A, Tail1), [A | Tail2], State)
  when A < 16#80 ->
  parseEndTag(Tail1, Tail2, State);
parseEndTag(?STR1_T($>, Tail), [], State) ->
  {ok, Tail, State};
parseEndTag(?STR1_T(NextChar, Tail), [], State) 
  when ?is_whitespace(NextChar) ->
  {Tail2, State2} = removeWS(Tail, State),
  {ok, Tail2, State2};
parseEndTag(?EMPTY, StartTag, State) ->
  ?CF4_2(?EMPTY, StartTag, State, fun parseEndTag/3);
parseEndTag(Tail, [B | StartTagTail], State) ->
  {Char, Tail2, State2} = decodeChar(Tail, State),
  if 
    Char =:= B ->
      parseEndTag(Tail2, StartTagTail, State2);
    true -> error
  end;
parseEndTag(_Tail, [], _State) ->
  error.

removeWS(?STR1_T($>, T), State) ->
  {T, State};
removeWS(?STR1_T(C, T), State) 
  when ?is_whitespace(C) ->
  removeWS(T, State);
removeWS(?EMPTY, State) ->
  ?CF3(?EMPTY, State, fun removeWS/2);
removeWS(_, _) ->
  throw({error, "Malformed: Unexpected character in end tag"}).

%% StartTag = {Prefix, LocalName, QualifiedName}
%% Attributes = list of Attribute
%% Attribute = {{Prefix, LocalName} Value}
%%
%% returns: {Name, Attributes2, NewNamespaces}
%% Name = {URI, LocalName, QualifiedName}
%% Attributes2 = list of Attribute2
%% Atrribute2 = #attribute
%% NewNamespaces = list of {Prefix, URI} (prefix can be []).
%%
%% Namespaces are in such an order that namespace of the 'closest ancestors' 
%% are in front. That way the right element will be found, even if a prefix is 
%% used more than once in the document.
%%
createStartTagEvent(StartTag, Namespaces, Attributes) ->
  
  %% find the namespace definitions in the attributes
  {NewNamespaces, OtherAttributes} = lookForNamespaces([], [], Attributes),
  AllNamespaces = NewNamespaces ++ Namespaces,

  %% add the Uri to the tag name (if applicable)
  Name = tagNameTuple(StartTag, AllNamespaces),

  %% add the URIs to the attribute names (if applicable)
  Attributes2 = attributeNameTuples([], OtherAttributes, AllNamespaces),

  {Name, Attributes2, NewNamespaces}.

%% returns {Namespaces, OtherAttributes}, where 
%%   Namespaces = a list of tuples {Prefix, URI} 
%%   OtherAttributes = a list of tuples {Name, Value}
%%
lookForNamespaces(Namespaces, OtherAttributes, [Head | Tail]) ->
  {{Prefix, LocalName, _QName}, Value} = Head,
  if 
    Prefix == "xmlns" ->
      lookForNamespaces([{LocalName, decodeIfRequired(Value)} | Namespaces], 
                         OtherAttributes, Tail);
    Prefix == [],  LocalName == "xmlns" ->
      lookForNamespaces([{[], decodeIfRequired(Value)} | Namespaces], 
                        OtherAttributes, Tail);
    true -> 
      lookForNamespaces(Namespaces, [Head | OtherAttributes], Tail)
  end;
  
lookForNamespaces(Namespaces, OtherAttributes, []) -> 
  {Namespaces, OtherAttributes}.
 
decodeIfRequired(URI) when is_list(URI) ->
  URI;
decodeIfRequired(URI) when is_binary(URI) ->
  {Value, _} = erlsom_ucs:from_utf8(URI),
  Value.

%% StartTag = {Prefix, LocalName, QualifiedName} 
%% Namespaces = list of {Prefix, URI} (prefix can be []).
%%
%% Returns {Uri, LocalName, Prefix}
%%
%% TODO: error if not found? special treatment of 'xml:lang'?
tagNameTuple(StartTag, Namespaces) ->
  {Prefix, LocalName, _QName} = StartTag,
  case lists:keysearch(Prefix, 1, Namespaces) of
    {value, {Prefix, Uri}} -> {Uri, LocalName, Prefix};
    false -> {[], LocalName, Prefix}
  end.
      
%% Attributes = list of Attribute
%% Attribute = {{Prefix, LocalName} Value}
%% Namespaces = list of {Prefix, URI} (prefix can be []).
%%
%% Returns a list of #attribute records
attributeNameTuples(ProcessedAttributes, 
                    [{AttributeName, Value} | Attributes], Namespaces) ->
  {Uri, LocalName, Prefix} = attributeNameTuple(AttributeName, Namespaces),
  attributeNameTuples([#attribute{localName= LocalName,
                                  prefix = Prefix,
				  uri = Uri,
				  value = Value} | ProcessedAttributes], 
                      Attributes, Namespaces);

attributeNameTuples(ProcessedAttributes, [], _) ->
  ProcessedAttributes.

%% AttributeName = {Prefix, LocalName, QualifiedName}
%% Namespaces = list of {Prefix, URI} (prefix can be []).
%%
%% Returns {Uri, LocalName, Prefix}.
%% Difference with TagNameTuple: attributes without prefix do NOT belong
%% to the default namespace.
attributeNameTuple(AttributeName, Namespaces) ->
  {Prefix, LocalName, _} = AttributeName,
  if 
    Prefix == [] -> {[], LocalName, []};
    true -> 
      case lists:keysearch(Prefix, 1, Namespaces) of
        {value, {Prefix, Uri}} ->
	    {Uri, LocalName, Prefix};
        false ->
            case Prefix of
              "xml" -> {"http://www.w3.org/XML/1998/namespace", LocalName, Prefix};
              _ -> {[], LocalName, Prefix}
            end
      end
  end.

wrapCallback(Event, #erlsom_sax_state{callback = Callback, user_state = UserState} = State) ->
  State#erlsom_sax_state{user_state = Callback(Event, UserState)}.

-ifdef(UTF8).
decode(Bin) ->
  {Value, _} = erlsom_ucs:from_utf8(Bin),
  Value.
-endif.

-ifdef(LAT1).
decode(Bin) ->
  {Value, _} = erlsom_ucs:from_utf8(Bin),
  Value.
-endif.

-ifdef(U16B).
decode(Bin) ->
  {Value, _} = erlsom_ucs:from_utf16be(Bin),
  Value.
-endif.

-ifdef(U16L).
decode(Bin) ->
  {Value, _} = erlsom_ucs:from_utf16le(Bin),
  Value.
-endif.

-ifdef(LIST).
decode(List) ->
  List.
-endif.
