Nonterminals sections section properties property.

Terminals key value '[' ']' '='.

Rootsymbol sections.

sections -> section : ['$1'].
sections -> section sections : ['$1' | '$2'].

section -> '[' key ']' properties : {value_of('$2'), '$4'}.

properties -> property : ['$1'].
properties -> property properties : ['$1' | '$2'].

property -> key '=' value : {value_of('$1'), value_of('$3')}.

Erlang code.

-compile({inline, value_of/1}).
value_of(Token) ->
    element(3, Token).
