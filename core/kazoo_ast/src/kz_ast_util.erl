-module(kz_ast_util).

-export([module_ast/1
        ,add_module_ast/3
        ,binary_match_to_binary/1

        ,schema_path/1
        ,ensure_file_exists/1
        ,create_schema/1

        ,project_apps/0, app_modules/1
        ]).

-include_lib("kazoo_ast/include/kz_ast.hrl").
-include_lib("kazoo/include/kz_types.hrl").


-type ast() :: [erl_parse:abstract_form()].

-spec module_ast(atom()) -> {atom(), ast()} | 'undefined'.
module_ast(M) ->
    case code:which(M) of
        'non_existing' -> 'undefined';
        'preloaded' -> 'undefined';
        Beam ->
            {'ok', {Module, [{'abstract_code', AST}]}} = beam_lib:chunks(Beam, ['abstract_code']),
            {Module, AST}
    end.

-spec add_module_ast(list(), module(), {'raw_abstract_v1',ast()}) -> ast().
add_module_ast(Fs, Module, AST) ->
    ast_functions(Module, AST) ++ Fs.

ast_functions(Module, {'raw_abstract_v1', Attributes}) ->
    [{Module, F, Arity, Clauses}
     || {'function', _Line, F, Arity, Clauses} <- Attributes
    ].

-spec binary_match_to_binary(ast()) -> binary().
binary_match_to_binary(?ATOM(A)) -> kz_util:to_binary(A);
binary_match_to_binary(?BINARY_STRING(V)) ->
    kz_util:to_binary(V);
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

-spec schema_path(binary()) -> binary().
schema_path(Base) ->
    filename:join([code:priv_dir('crossbar')
                  ,<<"couchdb">>
                  ,<<"schemas">>
                  ,Base
                  ]).

-spec ensure_file_exists(binary()) -> 'ok' | {'ok', any()}.
ensure_file_exists(Path) ->
    case filelib:is_regular(Path) of
        'false' -> create_schema(Path);
        'true' -> 'ok'
    end.

-spec create_schema(binary()) -> {'ok', any()}.
create_schema(Path) ->
    Skel = schema_path(<<"skel.json">>),
    {'ok', _} = file:copy(Skel, Path).

-spec project_apps() -> [atom()].
project_apps() ->
    Core = siblings_of('kazoo'),
    Apps = siblings_of('sysconf'),
    Core ++ Apps.

siblings_of(App) ->
    [dir_to_app_name(Dir)
     || Dir <- filelib:wildcard(filename:join([code:lib_dir(App), "..", "*"])),
        filelib:is_dir(Dir)
    ].

dir_to_app_name(Dir) ->
    kz_util:to_atom(filename:basename(Dir), 'true').

-spec app_modules(atom()) -> [atom()].
app_modules(App) ->
    case application:get_key(App, 'modules') of
        {'ok', Modules} -> Modules;
        'undefined' ->
            'ok' = application:load(App),
            app_modules(App)
    end.
