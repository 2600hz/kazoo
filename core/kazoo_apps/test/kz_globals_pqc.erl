-module(kz_globals_pqc).

-ifdef(PROPER).

-include_lib("proper/include/proper.hrl").

-include_lib("kazoo_caches/include/kazoo_caches.hrl").

-behaviour(proper_statem).

-export([command/1
        ,initial_state/0
        ,next_state/3
        ,postcondition/3
        ,precondition/2

        ,correct/0
        ,correct_parallel/0
        ]).

-endif.
