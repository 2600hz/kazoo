-module(kz_ast_util).

-export([module_ast/1
        ,add_module_ast/3
        ,binary_match_to_binary/1
        ]).

-include_lib("kazoo_ast/include/kz_ast.hrl").

-spec module_ast(atom()) ->
                        {atom(), [erl_parse:abstract_form()]} |
                        'undefined'.
module_ast(M) ->
    case code:which(M) of
        'non_existing' -> 'undefined';
        'preloaded' -> 'undefined';
        Beam ->
            {'ok', {Module, [{'abstract_code', AST}]}} = beam_lib:chunks(Beam, ['abstract_code']),
            {Module, AST}
    end.

add_module_ast(Fs, Module, AST) ->
    ast_functions(Module, AST) ++ Fs.

ast_functions(Module, {'raw_abstract_v1', Attributes}) ->
    [{Module, F, Arity, Clauses}
     || {'function', _Line, F, Arity, Clauses} <- Attributes
    ].

binary_match_to_binary(?ATOM(A)) -> kz_util:to_binary(A);
binary_match_to_binary(?BINARY_MATCH(Match)) ->
    binary_match_to_binary(Match);
binary_match_to_binary(Match) when is_list(Match) ->
    iolist_to_binary(
      [binary_part_to_binary(BP) || BP <- Match]
     ).

binary_part_to_binary(?BINARY_STRING(V)) -> V;
binary_part_to_binary(?BINARY_VAR(N)) -> [${, kz_util:to_binary(N), $}];
binary_part_to_binary(?SUB_BINARY(V)) -> V;
binary_part_to_binary(?BINARY_MATCH(Ms)) -> binary_match_to_binary(Ms).
