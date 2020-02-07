%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
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
    Module = kz_module:ensure_loaded(Module),
    _ = quickcheck_exports(Module),
    'no_return';
run_module(ModuleBin) ->
    run_module(kz_term:to_atom(ModuleBin)).

-spec quickcheck_exports(module()) -> 'ok'.
quickcheck_exports(Module) ->
    _ = [quickcheck_export(Module, Function)
         || Function <- ['correct', 'correct_parallel'],
            kz_module:is_exported(Module, Function, 0)
        ],
    'ok'.

-spec quickcheck_export(module(), atom()) -> 'true'.
quickcheck_export(Module, Function) ->
    io:format("quick-checking ~s:~s/0~n", [Module, Function]),
    'true' = proper:quickcheck(Module:Function()).

-spec run_seq_modules() -> 'no_return'.
run_seq_modules() ->
    _ = [run_seq_module(M) || M <- modules()],
    'no_return'.

-spec run_seq_module(atom() | kz_term:ne_binary()) -> 'no_return'.
run_seq_module(Module) when is_atom(Module) ->
    run_seq_module(Module, kz_module:is_exported(Module, 'seq', 0));
run_seq_module(ModuleBin) ->
    run_seq_module(kz_term:to_atom(ModuleBin)).

run_seq_module(_Module, 'false') -> 'no_return';
run_seq_module(Module, 'true') ->
    StartTimeMs = kz_time:now_ms(),
    Module:seq(),
    ?SUP_LOG_DEBUG("~s:seq() ran in ~pms", [Module, kz_time:elapsed_ms(StartTimeMs)]),
    'no_return'.

-spec modules() -> [module()].
modules() ->
    case application:get_key('kazoo_proper', 'modules') of
        {'ok', Modules} -> Modules;
        'undefined' ->
            'ok' = application:load('kazoo_proper'),
            modules()
    end.
