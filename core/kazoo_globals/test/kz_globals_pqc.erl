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

-define(SERVER, ?MODULE).

correct() ->
    ?FORALL(Cmds
           ,commands(?MODULE)
           ,?TRAPEXIT(
               begin
                   kz_globals:start_link(),
                   {History, State, Result} = run_commands(?MODULE, Cmds),
                   kz_globals:stop(),
                   ?WHENFAIL(io:format("Final State: ~p\nFailing Cmds: ~p\n"
                                      ,[State, zip(Cmds, History)]
                                      )
                            ,aggregate(command_names(Cmds), Result =:= 'ok')
                            )
               end
              )
           ).

correct_parallel() ->
    ?FORALL(Cmds
           ,parallel_commands(?MODULE),
            begin
                kz_globals:start_link(),
                {Sequential, Parallel, Result} = run_parallel_commands(?MODULE, Cmds),
                kz_globals:stop(),

                ?WHENFAIL(io:format("Failing Cmds: ~p\nS: ~p\nP: ~p\n"
                                   ,[Cmds, Sequential, Parallel]
                                   )
                         ,aggregate(command_names(Cmds), Result =:= 'ok')
                         )
            end).

initial_state() ->
    [].

command(_Registry) ->
    oneof([{'call', 'kz_globals', 'whereis_name', [name()]}
           ,{'call', 'kz_globals', 'register_name', [name(), self(), crypto:strong_rand_bytes(10)]}
           ,{'call', 'kz_globals', 'unregister_name', [name()]}
           ,{'call', 'kz_globals', 'registered', []}
          ]).

name() ->
    oneof([atom_for_name
          ,{tuple, for, name}
          ,<<"binary for name">>
          ,"list for name"
          ]).

-define(REG(Name, Pid, State, Extra), {Name, Pid, State, Extra}).

next_state(Registry, _V
          ,{'call', 'kz_globals', 'whereis_name', [_Name]}
          ) ->
    Registry;
next_state(Registry, _V
          ,{'call', 'kz_globals', 'register_name', [Name, Pid, Extra]}
          ) ->
    case lists:keysearch(Name, 1, Registry) of
        'false' -> [?REG(Name, Pid, 'local', Extra) | Registry];
        {'value', ?REG(_, _, _, _)} -> Registry
    end;
next_state(Registry, _V
          ,{'call', 'kz_globals', 'unregister_name', [Name]}
          ) ->
    case lists:keytake(Name, 1, Registry) of
        'false' -> Registry;
        {'value', _Entry, NewRegistry} -> NewRegistry
    end;
next_state(Registry, _V
          ,{'call', 'kz_globals', 'registered', []}
          ) ->
    Registry.

%% precondiiton([], {'call', 'kz_globals', 'unregister_name', [_Name]}) -> 'false';
precondition(_Registry, _Call) -> 'true'.

postcondition(Registry
             ,{'call', 'kz_globals', 'whereis_name', [Name]}
             ,WhereIsIt
             ) ->
    case lists:keysearch(Name, 1, Registry) of
        'false' -> 'undefined' =:= WhereIsIt;
        {'value', ?REG(Name, Pid, _, _)} -> WhereIsIt =:= Pid
    end;
postcondition(Registry
             ,{'call', 'kz_globals', 'register_name', [Name, Pid, Extra]}
             ,WasRegistered
             ) ->
    case lists:keysearch(Name, 1, Registry) of
        'false' -> WasRegistered =:= 'no';
        {'value', ?REG(Name, Pid, _, Extra)} -> WasRegistered =:= 'yes';
        {'value', ?REG(Name, _, _, _)} -> WasRegistered =:= 'no'
    end;
postcondition(_Registry
             ,{'call', 'kz_globals', 'unregister_name', [_Name]}
             ,'ok'
             ) ->
    'true';
postcondition(Registry
             ,{'call', 'kz_globals', 'registered', []}
             ,Names
             ) ->
    RegNames = [Name || ?REG(Name, _Pid, _State, _Extra) <- Registry],
    compare_names(RegNames, Names).

compare_names([], []) -> 'true';
compare_names(RegNames, [Name | Names]) ->
    case lists:member(Name, RegNames) of
        'false' -> 'false';
        'true' -> compare_names(lists:delete(Name, RegNames), Names)
    end.

-endif.
