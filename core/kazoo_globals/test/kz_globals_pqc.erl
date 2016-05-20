-module(kz_globals_pqc).

-ifdef(PROPER).

-include_lib("proper/include/proper.hrl").

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
                   kz_globals:flush(),
                   {History, State, Result} = run_commands(?MODULE, Cmds),

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
                kz_globals:flush(),
                {Sequential, Parallel, Result} = run_parallel_commands(?MODULE, Cmds),

                ?WHENFAIL(io:format("Failing Cmds: ~p\nS: ~p\nP: ~p\n"
                                   ,[Cmds, Sequential, Parallel]
                                   )
                         ,aggregate(command_names(Cmds), Result =:= 'ok')
                         )
            end).

initial_state() -> [].

command(_Registry) ->
    oneof([{'call', 'kz_globals', 'whereis_name', [name()]}
           ,{'call', 'kz_globals', 'register_name', [name(), self()]}
           ,{'call', 'kz_globals', 'unregister_name', [name()]}
           ,{'call', 'kz_globals', 'registered', []}
          ]).

name() ->
    %% oneof([kz_util:rand_hex_binary(4)
    %%        || _ <- lists:seq(1,5)
    %%       ]).
    oneof([atom_for_name
          ,{tuple, for, name}
          ,<<"binary for name">>
          ,"list for name"
          ]).

-define(REG(Name, Pid, State), {Name, Pid, State}).

next_state(Registry, _V
          ,{'call', 'kz_globals', 'whereis_name', [_Name]}
          ) ->
    Registry;
next_state(Registry, _V
          ,{'call', 'kz_globals', 'register_name', [Name, Pid]}
          ) ->
    NewReg =
        case lists:keysearch(Name, 1, Registry) of
            'false' ->
                [?REG(Name, Pid, 'local') | Registry];
            {'value', ?REG(Name, _, _)} ->
                Registry
        end,
    NewReg;
next_state(Registry, _V
          ,{'call', 'kz_globals', 'unregister_name', [Name]}
          ) ->
    NewReg =
        case lists:keytake(Name, 1, Registry) of
            'false' ->
                Registry;
            {'value', _Entry, NewRegistry} ->
                NewRegistry
        end,
    NewReg;
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
        'false' ->
            'undefined' =:= WhereIsIt;
        {'value', ?REG(Name, Pid, _)} ->
            WhereIsIt =:= Pid
    end;
postcondition(Registry
             ,{'call', 'kz_globals', 'register_name', [Name, _Pid]}
             ,WasRegistered
             ) ->
    case lists:keysearch(Name, 1, Registry) of
        'false' ->
            'yes' =:= WasRegistered;
        {'value', ?REG(Name, _, _)} ->
            'no' =:= WasRegistered
    end;
postcondition(_Registry
             ,{'call', 'kz_globals', 'unregister_name', [_Name]}
             ,Unregister
             ) ->
    'ok' =:= Unregister;
postcondition(Registry
             ,{'call', 'kz_globals', 'registered', []}
             ,Names
             ) ->
    RegNames = [Name || ?REG(Name, _Pid, _State) <- Registry],
    compare_names(RegNames, Names).

compare_names([], []) -> 'true';
compare_names([], _) -> 'false';
compare_names(_, []) -> 'false';
compare_names(RegNames, [Name | Names]) ->
    case lists:member(Name, RegNames) of
        'false' -> 'false';
        'true' -> compare_names(lists:delete(Name, RegNames), Names)
    end.

-endif.
