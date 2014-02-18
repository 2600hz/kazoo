-module(erlydtl_extension_test).

-export([scan/1, parse/1, compile_ast/3]).
-include("erlydtl_ext.hrl").

%% look for a foo identifer followed by a #
scan(#scanner_state{ template="#" ++ T, 
		     scanned=[{identifier, Loc, foo}|Scanned],
		     pos={L,C} }=S) ->
    %% return new state with the hash dropped, and the foo identifer replaced with bar
    {ok, S#scanner_state{ template=T,
			  scanned=[{identifier, Loc, "rab"}|Scanned],
			  pos={L, C+1} }};
scan(#scanner_state{ template="#" ++ _T, pos={L, C} }) ->
    %% give error when # not follows foo
    {error, {L,?MODULE,lists:concat(["Unexpected '#' in code at column ", C])}};
scan(_) -> 
    %% for anything else, fallback to the error message from erlydtl_scanner..
    undefined.

parse(State) ->
    erlydtl_extension_testparser:resume(State).

%% {{ varA or varB }} is equivalent to {% if varA %}{{ varA }}{% else %}{{ varB }}{% endif %}
compile_ast({value_or, {Value1, Value2}}, Context, TreeWalker) ->
    {{V1_Ast, V1_Info}, TW1} = erlydtl_compiler:value_ast(Value1, false, false, Context, TreeWalker),
    {{V2_Ast, V2_Info}, TW2} = erlydtl_compiler:value_ast(Value2, false, false, Context, TW1),
    {{erl_syntax:case_expr(V1_Ast,
                           [erl_syntax:clause([erl_syntax:atom(undefined)], none, [V2_Ast]),
                            erl_syntax:clause([erl_syntax:underscore()], none, [V1_Ast])
                           ]), erlydtl_compiler:merge_info(V1_Info, V2_Info)}, TW2}.
