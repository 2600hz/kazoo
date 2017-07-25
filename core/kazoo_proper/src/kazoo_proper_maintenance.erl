-module(kazoo_proper_maintenance).

-export([run_tests/0
        ,run_test/1
        ]).

-include("kazoo_proper.hrl").

-spec run_tests() -> 'no_return'.
run_tests() ->
    _ = [run_test(M) || M <- modules()],
    'no_return'.

-spec run_test(atom() | ne_binary()) -> 'no_return'.
run_test(Module) when is_atom(Module) ->
    Exports = Module:module_info('exports'),
    _ = quickcheck_exports(Module, Exports),
    'no_return';
run_test(ModuleBin) ->
    run_test(kz_term:to_atom(ModuleBin)).

quickcheck_exports(Module, Exports) ->
    _ = [quickcheck_export(Module, Export) || Export <- Exports],
    'ok'.

quickcheck_export(Module, {'correct', 0}) ->
    io:format("quickchecking ~s:correct/0~n", [Module]),
    proper:quickcheck(Module:correct());
quickcheck_export(Module, {'correct_parallel', 0}) ->
    io:format("quickchecking ~s:correct_parallel/0~n", [Module]),
    proper:quickcheck(Module:correct_parallel());
quickcheck_export(_Module, _FunArity) ->
    'ok'.

modules() ->
    case application:get_key('kazoo_proper', 'modules') of
        {'ok', Modules} -> Modules;
        'undefined' ->
            'ok' = application:load('kazoo_proper'),
            modules()
    end.
