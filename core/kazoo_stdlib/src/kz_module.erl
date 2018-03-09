%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_module).

-export([is_exported/3
        ,ensure_loaded/1
        ]).

%% @doc lifted from erlang ML
%% http://erlang.2086793.n4.nabble.com/What-is-the-fastest-way-to-check-if-a-function-is-defined-in-a-module-tp4723096p4723108.html
-spec is_exported(module() | kz_term:text(), kz_term:text(), arity()) -> boolean().
is_exported(Module, Function, Arity)
  when is_atom(Module),
       is_atom(Function),
       is_integer(Arity),
       Arity >= 0 ->
    case ensure_loaded(Module) of
        'false' -> 'false';
        Module -> erlang:function_exported(Module, Function, Arity)
    end;
is_exported(Module, Function, Arity) ->
    try is_exported(kz_term:to_atom(Module)
                   ,kz_term:to_atom(Function)
                   ,kz_term:to_integer(Arity)
                   )
    catch
        'error':'badarg' -> 'false'
    end.

%%------------------------------------------------------------------------------
%% @doc Given a module name try to verify its existence, loading it into the
%% the Erlang VM if possible.
%% @end
%%------------------------------------------------------------------------------
-spec ensure_loaded(kz_term:text()) -> module() | 'false'.
ensure_loaded('undefined') -> 'false';
ensure_loaded("undefined") -> 'false';
ensure_loaded(<<"undefined">>) -> 'false';
ensure_loaded(Mod) ->
    Module = kz_term:to_atom(Mod, 'true'),
    case erlang:module_loaded(Module) of
        'true' -> Module;
        'false' ->
            case code:ensure_loaded(Module) of
                {'module', Module} -> Module;
                {'error', _Why} ->
                    lager:debug("module ~s not found: ~p", [Module, _Why]),
                    'false'
            end
    end.
