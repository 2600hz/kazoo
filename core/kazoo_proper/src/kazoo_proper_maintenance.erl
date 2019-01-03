%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_proper_maintenance).

-export([run_modules/0
        ,run_module/1
        ,run_seq_modules/0
        ,run_seq_module/1
        ]).

-include("kazoo_proper.hrl").

-spec run_modules() -> 'no_return'.
run_modules() ->
    _ = [run_module(M) || M <- modules()],
    'no_return'.

-spec run_module(atom() | kz_term:ne_binary()) -> 'no_return'.
run_module(Module) when is_atom(Module) ->
    Exports = Module:module_info('exports'),
    _ = quickcheck_exports(Module, Exports),
    'no_return';
run_module(ModuleBin) ->
    run_module(kz_term:to_atom(ModuleBin)).

-spec quickcheck_exports(module(), [{function(), arity()}]) -> 'ok'.
quickcheck_exports(Module, Exports) ->
    _ = [quickcheck_export(Module, Export) || Export <- Exports],
    'ok'.

-spec quickcheck_export(module(), {function(), arity()}) -> 'true'.
quickcheck_export(Module, {'correct', 0}) ->
    io:format("quickchecking ~s:correct/0~n", [Module]),
    'true' = proper:quickcheck(Module:correct(), ['noshrink']);
quickcheck_export(Module, {'correct_parallel', 0}) ->
    io:format("quickchecking ~s:correct_parallel/0~n", [Module]),
    'true' = proper:quickcheck(Module:correct_parallel());
quickcheck_export(_Module, _FunArity) ->
    'true'.

-spec run_seq_modules() -> 'no_return'.
run_seq_modules() ->
    _ = [run_seq_module(M) || M <- modules()],
    'no_return'.

-spec run_seq_module(atom() | kz_term:ne_binary()) -> 'no_return'.
run_seq_module(Module) when is_atom(Module) ->
    Exports = Module:module_info('exports'),
    _ = seq_exports(Module, Exports),
    'no_return';
run_seq_module(ModuleBin) ->
    run_seq_module(kz_term:to_atom(ModuleBin)).

-spec seq_exports(module(), [{function(), arity()}]) -> 'ok'.
seq_exports(Module, Exports) ->
    _ = [seq_export(Module, Export) || Export <- Exports],
    'ok'.

-spec seq_export(module(), {function(), arity()}) -> any().
seq_export(Module, {'seq', 0}) ->
    io:format("run ~s:seq/0~n", [Module]),
    (catch Module:seq());
seq_export(_Module, _FunArity) ->
    'true'.

-spec modules() -> [module()].
modules() ->
    case application:get_key('kazoo_proper', 'modules') of
        {'ok', Modules} -> Modules;
        'undefined' ->
            'ok' = application:load('kazoo_proper'),
            modules()
    end.
