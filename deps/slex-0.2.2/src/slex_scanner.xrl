%%%-------------------------------------------------------------------
%%% File:      slex_scanner.xrl
%%% @author    Andreas Stenius <kaos@astekk.se>
%%% @copyright 2013 Andreas Stenius
%%% @doc
%%% slex scanner
%%% @end
%%%
%%% Copyright 2013 Andreas Stenius
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @since 2013-11-05 by Andreas Stenius
%%%-------------------------------------------------------------------


Definitions.

KEYWORDS = (any|skip|until|\+|-|:|,|\.)
CODE = (form|expr)(.|\\\n)+end
IDENTIFIER = [a-zA-Z][a-zA-Z0-9_]*
NUMBER = [0-9]+
SYMBOLS = (\\.|[^a-zA-Z0-9'":.,+\-\s\t\n\\])(\\.|[^.,\s\t\n\\])*

Rules.

\%\%\%.* : {token, {comment, TokenLine, tl(TokenChars)}}.
\%\%[^\%].*\n : skip_token.
(\s|\t|\n)+ : skip_token.
{KEYWORDS} : {token, {keyword(TokenChars), TokenLine}}.
{CODE} : parse_code(TokenLine, TokenChars).
{IDENTIFIER} : {token, {identifier, TokenLine, list_to_atom(TokenChars)}}.
{NUMBER} : {token, {number, TokenLine, list_to_integer(TokenChars)}}.
{SYMBOLS} : {token, {string, TokenLine, unescape(TokenChars)}}.
'(\\.|[^'\n\\])*' : {token, {string, TokenLine, string_token($', TokenChars)}}.
"(\\.|[^"\n\\])*" : {token, {string, TokenLine, string_token($", TokenChars)}}.

Erlang code.

keyword([$.]) -> '.';
keyword([$.|Cs]) -> list_to_atom(Cs);
keyword(Cs) -> list_to_atom(Cs).

string_token(Q, Cs) ->
  unescape(string:strip(Cs, both, Q)).

unescape($n) -> $\n;
unescape($r) -> $\r;
unescape($t) -> $\t;
unescape($s) -> $\s;
unescape(C) when is_integer(C) -> C;
unescape(Cs) when is_list(Cs) -> unescape(Cs, []).

unescape([], Acc) -> lists:reverse(Acc);
unescape([$\\,C|Cs], Acc) -> unescape(Cs, [unescape(C)|Acc]);
unescape([C|Cs], Acc) -> unescape(Cs, [C|Acc]).

parse_code(Line, Code) ->
    Res =
      case erl_scan:string(
           unescape(string:substr(Code, 5, string:len(Code) - 7))
           ++ ".") of
        {ok, Tokens, _} ->
            ParseFun = case Code of
                "form" ++ _ -> parse_form;
                "expr" ++ _ -> parse_exprs
            end,
            apply(erl_parse, ParseFun, [Tokens]);
          Err -> Err
      end,
    case Res of
      {ok, Parsed} -> {token, {code, Line, Parsed}};
      {error, {ELine,EMod,EDesc}} ->
          {error, io_lib:format("~4s:~b: ~s",
            [Code, ELine, EMod:format_error(EDesc)])}
    end.
