Definitions.

A = [a-zA-Z][a-zA-Z0-9_:@\-\.]*[a-zA-Z0-9]
D = [0-9]
S = [\s\t]

Rules.

= : {token,{'=',TokenLine}}.
\] : {token,{']',TokenLine}}.
{S}*\[ : {token,{'[',TokenLine}}.
{S}*{A}+{S}*\] : {token,{key,TokenLine,to_key(TokenChars,TokenLen)},"]"}.
{S}*{A}+{S}*= : {token,{key,TokenLine,to_key(TokenChars,TokenLen)},"="}.
{S}*{A}+ : {token,{value,TokenLine,to_atom(TokenChars)}}.
{S}*{D}+ : {token,{value,TokenLine,to_integer(TokenChars)}}.
{S}*{D}+\.{D}+ : {token,{value,TokenLine,to_float(TokenChars)}}.
{S}*".+" : {token,{value,TokenLine,to_string(TokenChars)}}.
{S}*[^=\[;""\n]+ : {token,{value,TokenLine,string:strip(TokenChars)}}.
;.* : skip_token.
[\000-\s]+ : skip_token.

Erlang code.

-compile({inline, to_key/2}).
to_key(TokenChars, TokenLen) ->
    S = string:substr(TokenChars, 1, TokenLen-1),
    K = string:strip(S),
    list_to_atom(K).

-compile({inline, to_atom/1}).
to_atom(TokenChars) ->
    list_to_atom(string:strip(TokenChars, left)).

-compile({inline, to_integer/1}).
to_integer(TokenChars) ->
    list_to_integer(string:strip(TokenChars, left)).

-compile({inline, to_float/1}).
to_float(TokenChars) ->
    list_to_float(string:strip(TokenChars, left)).

-compile({inline, to_string/1}).
to_string(TokenChars) ->
    S = string:strip(TokenChars, left),
    string:substr(S, 2, length(S)-2).
