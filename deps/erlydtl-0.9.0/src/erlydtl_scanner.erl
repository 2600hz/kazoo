%%%%% THIS IS A SLEX GENERATED FILE %%%%%

%%%-------------------------------------------------------------------
%%% File:      erlydtl_scanner.slex
%%% @author    Andreas Stenius <kaos@astekk.se>
%%% @copyright 2013 Andreas Stenius
%%% @doc
%%% erlydtl scanner
%%% @end
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2013 Andreas Stenius
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%% @since 2013-11-05 by Andreas Stenius
%%%
%%% Rules based on the original erlydtl_scanner by Robert Saccon and Evan Miller.
%%%-------------------------------------------------------------------
-module(erlydtl_scanner).

%% This file was generated 2014-02-17 11:20:49 UTC by slex 0.2.1.
%% http://github.com/erlydtl/slex
-slex_source(["src/erlydtl_scanner.slex"]).

-export([scan/1, scan/4]).

-compile(nowarn_unused_vars).

-export([resume/1, format_error/1]).

-record(scanner_state,
	{template = [], scanned = [], pos = {1, 1},
	 state = in_text}).

resume(#scanner_state{template = Template,
		      scanned = Scanned, pos = Pos, state = State}) ->
    scan(Template, Scanned, Pos, State).

return_error(Error, P, T, S, St) ->
    {error, {P, erlydtl_scanner, Error},
     #scanner_state{template = T,
		    scanned = post_process(S, err), pos = P, state = St}}.

return_error(Error, P) ->
    {error, {P, erlydtl_scanner, Error}}.

to_atom(L) when is_list(L) -> list_to_atom(L).

to_keyword(L, P) -> {to_atom(L ++ "_keyword"), P, L}.

atomize(L, T) -> setelement(3, T, to_atom(L)).

is_keyword(Class, {_, _, L} = T) ->
    L1 = lists:reverse(L),
    case is_keyword(Class, L1) of
      true -> to_keyword(L1, element(2, T));
      false -> atomize(L1, T)
    end;
is_keyword([C | Cs], L) ->
    is_keyword(C, L) orelse is_keyword(Cs, L);
is_keyword(all, L) -> is_keyword([any, open, close], L);
is_keyword(open_tag, L) -> is_keyword([any, open], L);
is_keyword(close_tag, L) -> is_keyword([any, close], L);
is_keyword(any, "in") -> true;
is_keyword(any, "not") -> true;
is_keyword(any, "or") -> true;
is_keyword(any, "and") -> true;
is_keyword(any, "as") -> true;
is_keyword(any, "by") -> true;
is_keyword(any, "with") -> true;
is_keyword(close, "only") -> true;
is_keyword(close, "parsed") -> true;
is_keyword(close, "noop") -> true;
is_keyword(close, "reversed") -> true;
is_keyword(close, "openblock") -> true;
is_keyword(close, "closeblock") -> true;
is_keyword(close, "openvariable") -> true;
is_keyword(close, "closevariable") -> true;
is_keyword(close, "openbrace") -> true;
is_keyword(close, "closebrace") -> true;
is_keyword(close, "opencomment") -> true;
is_keyword(close, "closecomment") -> true;
is_keyword(open, "autoescape") -> true;
is_keyword(open, "endautoescape") -> true;
is_keyword(open, "block") -> true;
is_keyword(open, "endblock") -> true;
is_keyword(open, "comment") -> true;
is_keyword(open, "endcomment") -> true;
is_keyword(open, "cycle") -> true;
is_keyword(open, "extends") -> true;
is_keyword(open, "filter") -> true;
is_keyword(open, "endfilter") -> true;
is_keyword(open, "firstof") -> true;
is_keyword(open, "for") -> true;
is_keyword(open, "empty") -> true;
is_keyword(open, "endfor") -> true;
is_keyword(open, "if") -> true;
is_keyword(open, "elif") -> true;
is_keyword(open, "else") -> true;
is_keyword(open, "endif") -> true;
is_keyword(open, "ifchanged") -> true;
is_keyword(open, "endifchanged") -> true;
is_keyword(open, "ifequal") -> true;
is_keyword(open, "endifequal") -> true;
is_keyword(open, "ifnotequal") -> true;
is_keyword(open, "endifnotequal") -> true;
is_keyword(open, "include") -> true;
is_keyword(open, "now") -> true;
is_keyword(open, "regroup") -> true;
is_keyword(open, "endregroup") -> true;
is_keyword(open, "spaceless") -> true;
is_keyword(open, "endspaceless") -> true;
is_keyword(open, "ssi") -> true;
is_keyword(open, "templatetag") -> true;
is_keyword(open, "widthratio") -> true;
is_keyword(open, "call") -> true;
is_keyword(open, "endwith") -> true;
is_keyword(open, "trans") -> true;
is_keyword(open, "blocktrans") -> true;
is_keyword(open, "endblocktrans") -> true;
is_keyword(_, _) -> false.

format_error({illegal_char, C}) ->
    io_lib:format("Illegal character '~s'", [[C]]);
format_error({eof, Where}) ->
    io_lib:format("Unexpected end of file ~s",
		  [format_where(Where)]).

format_where(in_comment) -> "in comment";
format_where(in_code) -> "in code block".

scan(Template) when is_list(Template) ->
    scan(Template, [], {1, 1}, in_text);
scan(Template) when is_binary(Template) ->
    scan(binary_to_list(Template)).

scan("{{" ++ T, S, {R, C} = P, in_text) ->
    scan(T,
	 [{open_var, P, "{{"} | post_process(S, open_var)],
	 {R, C + 2}, {in_code, "}}"});
scan("{%" ++ T, S, {R, C} = P, in_text) ->
    scan(T,
	 [{open_tag, P, "{%"} | post_process(S, open_tag)],
	 {R, C + 2}, {in_code, "%}"});
scan("<!--{{" ++ T, S, {R, C} = P, in_text) ->
    scan(T,
	 [{open_var, P, "<!--{{"} | post_process(S, open_var)],
	 {R, C + 6}, {in_code, "}}-->"});
scan("<!--{%" ++ T, S, {R, C} = P, in_text) ->
    scan(T,
	 [{open_tag, P, "<!--{%"} | post_process(S, open_tag)],
	 {R, C + 6}, {in_code, "%}-->"});
scan("{#" ++ T, S, {R, C}, in_text) ->
    scan(T, S, {R, C + 2}, {in_comment, "#}"});
scan("<!--{#" ++ T, S, {R, C}, in_text) ->
    scan(T, S, {R, C + 6}, {in_comment, "#}-->"});
scan("#}-->" ++ T, S, {R, C}, {_, "#}-->"}) ->
    scan(T, S, {R, C + 5}, in_text);
scan("#}" ++ T, S, {R, C}, {_, "#}"}) ->
    scan(T, S, {R, C + 2}, in_text);
scan([H | T], S, {R, C}, {in_comment, E} = St) ->
    scan(T, S,
	 case H of
	   $\n -> {R + 1, 1};
	   _ -> {R, C + 1}
	 end,
	 St);
scan([H | T], S, {R, C} = P, in_text = St) ->
    scan(T,
	 case S of
	   [{string, _, L} = M | Ss] ->
	       [setelement(3, M, [H | L]) | Ss];
	   _ -> [{string, P, [H]} | post_process(S, string)]
	 end,
	 case H of
	   $\n -> {R + 1, 1};
	   _ -> {R, C + 1}
	 end,
	 St);
scan("\"" ++ T, S, {R, C} = P, {in_code, E}) ->
    scan(T,
	 [{string_literal, P, "\""} | post_process(S,
						   string_literal)],
	 {R, C + 1}, {in_double_quote, E});
scan("'" ++ T, S, {R, C} = P, {in_code, E}) ->
    scan(T,
	 [{string_literal, P, "\""} | post_process(S,
						   string_literal)],
	 {R, C + 1}, {in_single_quote, E});
scan("\"" ++ T, S, {R, C} = P, {in_double_quote, E}) ->
    scan(T,
	 case S of
	   [{string_literal, _, L} = M | Ss] ->
	       [setelement(3, M, "\"" ++ L) | Ss];
	   _ ->
	       [{string_literal, P, "\""} | post_process(S,
							 string_literal)]
	 end,
	 {R, C + 1}, {in_code, E});
scan("'" ++ T, S, {R, C} = P, {in_single_quote, E}) ->
    scan(T,
	 case S of
	   [{string_literal, _, L} = M | Ss] ->
	       [setelement(3, M, "\"" ++ L) | Ss];
	   _ ->
	       [{string_literal, P, "\""} | post_process(S,
							 string_literal)]
	 end,
	 {R, C + 1}, {in_code, E});
scan("\\" ++ T, S, {R, C} = P, {in_double_quote, E}) ->
    scan(T,
	 case S of
	   [{string_literal, _, L} = M | Ss] ->
	       [setelement(3, M, "\\" ++ L) | Ss];
	   _ ->
	       [{string_literal, P, "\\"} | post_process(S,
							 string_literal)]
	 end,
	 {R, C + 1}, {in_double_quote_escape, E});
scan("\\" ++ T, S, {R, C} = P, {in_single_quote, E}) ->
    scan(T,
	 case S of
	   [{string_literal, _, L} = M | Ss] ->
	       [setelement(3, M, "\\" ++ L) | Ss];
	   _ ->
	       [{string_literal, P, "\\"} | post_process(S,
							 string_literal)]
	 end,
	 {R, C + 1}, {in_single_quote_escape, E});
scan([H | T], S, {R, C} = P,
     {in_double_quote, E} = St) ->
    scan(T,
	 case S of
	   [{string_literal, _, L} = M | Ss] ->
	       [setelement(3, M, [H | L]) | Ss];
	   _ ->
	       [{string_literal, P, [H]} | post_process(S,
							string_literal)]
	 end,
	 case H of
	   $\n -> {R + 1, 1};
	   _ -> {R, C + 1}
	 end,
	 St);
scan([H | T], S, {R, C} = P,
     {in_single_quote, E} = St) ->
    scan(T,
	 case S of
	   [{string_literal, _, L} = M | Ss] ->
	       [setelement(3, M, [H | L]) | Ss];
	   _ ->
	       [{string_literal, P, [H]} | post_process(S,
							string_literal)]
	 end,
	 case H of
	   $\n -> {R + 1, 1};
	   _ -> {R, C + 1}
	 end,
	 St);
scan([H | T], S, {R, C} = P,
     {in_double_quote_escape, E}) ->
    scan(T,
	 case S of
	   [{string_literal, _, L} = M | Ss] ->
	       [setelement(3, M, [H | L]) | Ss];
	   _ ->
	       [{string_literal, P, [H]} | post_process(S,
							string_literal)]
	 end,
	 case H of
	   $\n -> {R + 1, 1};
	   _ -> {R, C + 1}
	 end,
	 {in_double_quote, E});
scan([H | T], S, {R, C} = P,
     {in_single_quote_escape, E}) ->
    scan(T,
	 case S of
	   [{string_literal, _, L} = M | Ss] ->
	       [setelement(3, M, [H | L]) | Ss];
	   _ ->
	       [{string_literal, P, [H]} | post_process(S,
							string_literal)]
	 end,
	 case H of
	   $\n -> {R + 1, 1};
	   _ -> {R, C + 1}
	 end,
	 {in_single_quote, E});
scan("}}-->" ++ T, S, {R, C} = P, {_, "}}-->"}) ->
    scan(T,
	 [{close_var, P, "}}-->"} | post_process(S, close_var)],
	 {R, C + 5}, in_text);
scan("%}-->" ++ T, S, {R, C} = P, {_, "%}-->"}) ->
    scan(T,
	 [{close_tag, P, "%}-->"} | post_process(S, close_tag)],
	 {R, C + 5}, in_text);
scan("}}" ++ T, S, {R, C} = P, {_, "}}"}) ->
    scan(T,
	 [{close_var, P, "}}"} | post_process(S, close_var)],
	 {R, C + 2}, in_text);
scan("%}" ++ T, S, {R, C} = P, {_, "%}"} = St) ->
    case S of
      [{identifier, _, "mitabrev"}, {open_tag, _, '{%'}
       | Ss] ->
	  scan(T, [{string, {R, C + 2}, ""} | Ss], {R, C + 2},
	       {in_verbatim, undefined});
      [{identifier, _, Tag}, {identifier, _, verbatim},
       {open_tag, _, '{%'}
       | Ss] ->
	  scan(T, [{string, {R, C + 2}, ""} | Ss], {R, C + 2},
	       {in_verbatim, Tag});
      _ ->
	  scan(T,
	       [{close_tag, P, "%}"} | post_process(S, close_tag)],
	       {R, C + 2}, in_text)
    end;
scan("{%" ++ T, S, {R, C} = P, {in_verbatim, E} = St) ->
    scan(T, S, {R, C + 2}, {in_verbatim_code, {E, "%{"}});
scan(" " ++ T, S, {R, C} = P,
     {in_verbatim_code, E} = St) ->
    {Tag, Backtrack} = E,
    scan(T, S, {R, C + 1},
	 {in_verbatim_code, {Tag, [$  | Backtrack]}});
scan("endverbatim%}" ++ T, S, {R, C} = P,
     {in_verbatim_code, E} = St)
    when element(1, E) =:= undefined ->
    scan(T, S, {R, C + 13}, in_text);
scan("endverbatim " ++ T, S, {R, C} = P,
     {in_verbatim_code, E} = St) ->
    {Tag, Backtrack} = E,
    scan(T, S, {R, C + 12},
	 {in_endverbatim_code,
	  {Tag, lists:reverse("endverbatim ", Backtrack), ""}});
scan(" " ++ T, S, {R, C} = P,
     {in_endverbatim_code, E} = St)
    when element(3, E) =:= "" ->
    {Tag, Backtrack, EndTag} = E,
    scan(T, S, {R, C + 1},
	 {in_endverbatim_code, {Tag, [$  | Backtrack], EndTag}});
scan([H | T], S, {R, C} = P,
     {in_endverbatim_code, E} = St)
    when H >= $a andalso H =< $z orelse
	   H >= $0 andalso H =< $9 orelse H =:= $_ ->
    {Tag, Backtrack, EndTag} = E,
    scan(T, S, {R, C + 1},
	 {in_endverbatim_code,
	  {Tag, [H | Backtrack], [H | EndTag]}});
scan(" " ++ T, S, {R, C} = P,
     {in_endverbatim_code, E} = St)
    when element(1, E) =:= element(3, E) ->
    {Tag, Backtrack, Tag} = E,
    scan(T, S, {R, C + 1},
	 {in_endverbatim_code, {Tag, [$  | Backtrack], Tag}});
scan("%}" ++ T, S, {R, C} = P,
     {in_endverbatim_code, E} = St)
    when element(1, E) =:= element(3, E) ->
    scan(T, S, {R, C + 2}, in_text);
scan("%}" ++ T, S, {R, C} = P,
     {in_endverbatim_code, E} = St)
    when element(1, E) =:= undefined andalso
	   element(3, E) =:= "" ->
    scan(T, S, {R, C + 2}, in_text);
scan([H | T], S, {R, C} = P,
     {in_endverbatim_code, E} = St) ->
    {Tag, Backtrack, _} = E,
    scan(T,
	 case S of
	   [{string, _, L} = M | Ss] ->
	       [setelement(3, M, [H | Backtrack] ++ L) | Ss];
	   _ -> [{string, P, [H | Backtrack]} | S]
	 end,
	 case H of
	   $\n -> {R + 1, 1};
	   _ -> {R, C + 1}
	 end,
	 {in_verbatim, Tag});
scan([H | T], S, {R, C} = P,
     {in_verbatim_code, E} = St) ->
    {Tag, Backtrack} = E,
    scan(T,
	 case S of
	   [{string, _, L} = M | Ss] ->
	       [setelement(3, M, [H | Backtrack] ++ L) | Ss];
	   _ -> [{string, P, [H | Backtrack]} | S]
	 end,
	 case H of
	   $\n -> {R + 1, 1};
	   _ -> {R, C + 1}
	 end,
	 {in_verbatim, Tag});
scan([H | T], S, {R, C} = P, {in_verbatim, E} = St) ->
    scan(T,
	 case S of
	   [{string, _, L} = M | Ss] ->
	       [setelement(3, M, [H | L]) | Ss];
	   _ -> [{string, P, [H]} | S]
	 end,
	 case H of
	   $\n -> {R + 1, 1};
	   _ -> {R, C + 1}
	 end,
	 {in_verbatim, E});
scan("==" ++ T, S, {R, C} = P, {_, E}) ->
    scan(T, [{'==', P} | post_process(S, '==')], {R, C + 2},
	 {in_code, E});
scan("!=" ++ T, S, {R, C} = P, {_, E}) ->
    scan(T, [{'!=', P} | post_process(S, '!=')], {R, C + 2},
	 {in_code, E});
scan(">=" ++ T, S, {R, C} = P, {_, E}) ->
    scan(T, [{'>=', P} | post_process(S, '>=')], {R, C + 2},
	 {in_code, E});
scan("<=" ++ T, S, {R, C} = P, {_, E}) ->
    scan(T, [{'<=', P} | post_process(S, '<=')], {R, C + 2},
	 {in_code, E});
scan(">" ++ T, S, {R, C} = P, {_, E}) ->
    scan(T, [{'>', P} | post_process(S, '>')], {R, C + 1},
	 {in_code, E});
scan("<" ++ T, S, {R, C} = P, {_, E}) ->
    scan(T, [{'<', P} | post_process(S, '<')], {R, C + 1},
	 {in_code, E});
scan("(" ++ T, S, {R, C} = P, {_, E}) ->
    scan(T, [{'(', P} | post_process(S, '(')], {R, C + 1},
	 {in_code, E});
scan(")" ++ T, S, {R, C} = P, {_, E}) ->
    scan(T, [{')', P} | post_process(S, ')')], {R, C + 1},
	 {in_code, E});
scan("," ++ T, S, {R, C} = P, {_, E}) ->
    scan(T, [{',', P} | post_process(S, ',')], {R, C + 1},
	 {in_code, E});
scan("|" ++ T, S, {R, C} = P, {_, E}) ->
    scan(T, [{'|', P} | post_process(S, '|')], {R, C + 1},
	 {in_code, E});
scan("=" ++ T, S, {R, C} = P, {_, E}) ->
    scan(T, [{'=', P} | post_process(S, '=')], {R, C + 1},
	 {in_code, E});
scan(":" ++ T, S, {R, C} = P, {_, E}) ->
    scan(T, [{':', P} | post_process(S, ':')], {R, C + 1},
	 {in_code, E});
scan("." ++ T, S, {R, C} = P, {_, E}) ->
    scan(T, [{'.', P} | post_process(S, '.')], {R, C + 1},
	 {in_code, E});
scan("_(" ++ T, S, {R, C} = P, {_, E}) ->
    scan(T, [{'(', P}, {'_', P} | post_process(S, '_')],
	 {R, C + 2}, {in_code, E});
scan(" " ++ T, S, {R, C}, {_, E}) ->
    scan(T, S, {R, C + 1}, {in_code, E});
scan([H | T], S, {R, C} = P, {in_code, E})
    when H >= $a andalso H =< $z orelse
	   H >= $A andalso H =< $Z orelse H == $_ ->
    scan(T,
	 [{identifier, P, [H]} | post_process(S, identifier)],
	 case H of
	   $\n -> {R + 1, 1};
	   _ -> {R, C + 1}
	 end,
	 {in_identifier, E});
scan([H | T], S, {R, C} = P, {in_code, E})
    when H >= $0 andalso H =< $9 orelse H == $- ->
    scan(T,
	 [{number_literal, P, [H]} | post_process(S,
						  number_literal)],
	 case H of
	   $\n -> {R + 1, 1};
	   _ -> {R, C + 1}
	 end,
	 {in_number, E});
scan([H | T], S, {R, C} = P, {in_code, E} = St) ->
    return_error({illegal_char, H}, P, [H | T], S, St);
scan([H | T], S, {R, C} = P, {in_number, E} = St)
    when H >= $0 andalso H =< $9 ->
    scan(T,
	 case S of
	   [{number_literal, _, L} = M | Ss] ->
	       [setelement(3, M, [H | L]) | Ss];
	   _ ->
	       [{number_literal, P, [H]} | post_process(S,
							number_literal)]
	 end,
	 case H of
	   $\n -> {R + 1, 1};
	   _ -> {R, C + 1}
	 end,
	 St);
scan([H | T], S, {R, C} = P, {in_number, E} = St) ->
    return_error({illegal_char, H}, P, [H | T], S, St);
scan([H | T], S, {R, C} = P, {in_identifier, E})
    when H >= $a andalso H =< $z orelse
	   H >= $A andalso H =< $Z orelse
	     H >= $0 andalso H =< $9 orelse H == $_ ->
    scan(T,
	 case S of
	   [{identifier, _, L} = M | Ss] ->
	       [setelement(3, M, [H | L]) | Ss];
	   _ ->
	       [{identifier, P, [H]} | post_process(S, identifier)]
	 end,
	 case H of
	   $\n -> {R + 1, 1};
	   _ -> {R, C + 1}
	 end,
	 {in_identifier, E});
scan([H | T], S, {R, C} = P, {in_identifier, E} = St) ->
    return_error({illegal_char, H}, P, [H | T], S, St);
scan([], S, {R, C} = P, in_text = St) ->
    {ok, lists:reverse(post_process(S, eof))};
scan([], S, {R, C} = P, {in_comment, E} = St) ->
    return_error({eof, in_comment}, P);
scan([], S, {R, C} = P, {_, E} = St) ->
    return_error({eof, in_code}, P).

post_process(_, {string, _, L} = T, _) ->
    setelement(3, T, begin L1 = lists:reverse(L), L1 end);
post_process(_, {string_literal, _, L} = T, _) ->
    setelement(3, T, begin L1 = lists:reverse(L), L1 end);
post_process(_, {number_literal, _, L} = T, _) ->
    setelement(3, T, begin L1 = lists:reverse(L), L1 end);
post_process(_, {open_var, _, L} = T, _) ->
    setelement(3, T, begin L1 = to_atom(L), L1 end);
post_process(_, {close_var, _, L} = T, _) ->
    setelement(3, T, begin L1 = to_atom(L), L1 end);
post_process(_, {open_tag, _, L} = T, _) ->
    setelement(3, T, begin L1 = to_atom(L), L1 end);
post_process(_, {close_tag, _, L} = T, _) ->
    setelement(3, T, begin L1 = to_atom(L), L1 end);
post_process([{open_tag, _, _} | _],
	     {identifier, _, L} = T, close_tag) ->
    is_keyword(all, T);
post_process([{open_tag, _, _} | _],
	     {identifier, _, L} = T, _) ->
    is_keyword(open_tag, T);
post_process(_, {identifier, _, L} = T, close_tag) ->
    is_keyword(close_tag, T);
post_process(_, {identifier, _, L} = T, _) ->
    is_keyword(any, T);
post_process(_, T, _) -> T.

post_process([S | Ss], N) ->
    [post_process(Ss, S, N) | Ss];
post_process(T, N) -> post_process(undefined, T, N).
