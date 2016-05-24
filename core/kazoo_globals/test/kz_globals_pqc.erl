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

%% test helpers
-export([remote_register/3]).

-define(LOCAL_NODE, 0).
-define(REMOTE_NODES, 1).

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

initial_state() ->
    lists:foldl(fun add_node/2
               ,maps:new()
               ,lists:seq(?LOCAL_NODE,?REMOTE_NODES)
               ).
add_node(N, M) ->
    M#{N => []}.

command(_Registry) ->
    oneof([{'call', 'kz_globals', 'whereis_name', [name()]}
          ,{'call', 'kz_globals', 'register_name', [name(), self()]}
          ,{'call', 'kz_globals', 'unregister_name', [name()]}
          ,{'call', 'kz_globals', 'registered', []}
          ,{'call', ?MODULE, 'remote_register', [remote_node(), name(), self()]}
          %% ,{'call', ?MODULE, 'remote_unregister', [remote_node(), name(), self()]},
          ]).

name() ->
    oneof(['atom_for_name'
          ,{'tuple', 'for', 'name'}
          ,<<"binary for name">>
          ,"list for name"
          ]).

remote_node() ->
    integer(1,?REMOTE_NODES).

-define(REG(Name, Pid, State), {Name, Pid, State}).

name_exists(Name, Registries) ->
    lists:any(fun(R) ->
                      'false' =/= lists:keysearch(Name, 1, R)
              end
             ,maps:values(Registries)
             ).

%% Remote Node updates
next_state(Registries, _V
           ,{'call', ?MODULE, 'remote_register', [Remote, Name, Pid]}
          ) ->
    case name_exists(Name, Registries) of
        'true' -> Registries;
        'false' ->
            maps:fold(fun(Node, Registry, Acc) when Remote =:= Node ->
                              Acc#{Node => [?REG(Name, Pid, 'local') | Registry]};
                         (Node, Registry, Acc) ->
                              Acc#{Node => [?REG(Name, Pid, 'remote') | Registry]}
                      end
                      ,maps:new()
                      ,Registries
                     )
    end;

%% Local Node State Changes
next_state(Registries, _V
          ,{'call', 'kz_globals', 'whereis_name', [_Name]}
          ) ->
    Registries;
next_state(Registries, _V
          ,{'call', 'kz_globals', 'register_name', [Name, Pid]}
          ) ->
    case name_exists(Name, Registries) of
        'true' -> Registries;
        'false' ->
            maps:fold(fun(Node, Registry, Acc) when ?LOCAL_NODE =:= Node ->
                              Acc#{Node => [?REG(Name, Pid, 'local') | Registry]};
                         (Node, Registry, Acc) ->
                              Acc#{Node => [?REG(Name, Pid, 'remote') | Registry]}
                      end
                     ,maps:new()
                     ,Registries
                     )
    end;
next_state(#{?LOCAL_NODE := LocalRegistry}=Registries, _V
          ,{'call', 'kz_globals', 'unregister_name', [Name]}
          ) ->
    case lists:keysearch(Name, 1, LocalRegistry) of
        'false' -> Registries;
        {'value', ?REG(Name, _Pid, 'local')} ->
            maps:fold(fun(Node, Registry, Acc) ->
                              Acc#{Node => lists:keydelete(Name, 1, Registry)}
                      end
                      ,maps:new()
                      ,Registries
                     );
        {'value', ?REG(Name, _Pid, _State)} -> Registries
    end;
next_state(Registries, _V
          ,{'call', 'kz_globals', 'registered', []}
          ) ->
    Registries.

precondition(Registries
             ,{'call', ?MODULE, 'remote_register', [Node, Name, _Pid]}
            ) ->
    #{Node := Registry} = Registries,
    case lists:keysearch(Name, 1, Registry) of
        {'value', ?REG(Name, _P, _State)} -> 'false';
        'false' -> 'true'
    end;
precondition(#{?LOCAL_NODE := Registry}
             ,{'call', 'kz_globals', 'unregister_name', [Name]}
            ) ->
    lager:debug("skpping unreg of ~p", [Name]),
    case lists:keysearch(Name, 1, Registry) of
        {'value', ?REG(Name, _Pid, 'local')} -> 'true';
        _ -> 'false'
    end;
precondition(_Registry, _Call) -> 'true'.

postcondition(Registries
              ,{'call', ?MODULE, 'remote_register', [_Remote, Name, _Pid]}
              ,WasRegistered
              ) ->
    case name_exists(Name, Registries) of
        'true' -> 'no' =:= WasRegistered;
        'false' -> 'yes' =:= WasRegistered
    end;

%% Local Node Changes
postcondition(#{?LOCAL_NODE := Registry}
             ,{'call', 'kz_globals', 'whereis_name', [Name]}
             ,WhereIsIt
             ) ->
    case lists:keysearch(Name, 1, Registry) of
        'false' ->
            'undefined' =:= WhereIsIt;
        {'value', ?REG(Name, Pid, 'local')} ->
            WhereIsIt =:= Pid;
        {'value', ?REG(Name, _Pid, 'remote')} ->
            'true'
    end;
postcondition(#{?LOCAL_NODE := Registry}
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
postcondition(#{?LOCAL_NODE := Registry}
             ,{'call', 'kz_globals', 'registered', []}
             ,Names
             ) ->
    lager:debug("comparing ~p to reg ~p", [Names, Registry]),
    compare_names(lists:keysort(1, Registry), lists:sort(Names)).

compare_names([], []) -> 'true';
compare_names([], _) -> 'false';
compare_names(_, []) -> 'false';
compare_names([?REG(Name, _, 'local')|Registry], [Name | Names]) ->
    compare_names(Registry, Names);
compare_names([?REG(Name, _, 'remote')|Registry], [Name | Names]) ->
    compare_names(Registry, Names);
compare_names([?REG(Name, _, 'pending')|Registry], Names) ->
    lager:debug("ignoring provisional ~p", [Name]),
    compare_names(Registry, Names);
compare_names([?REG(_N, _, _)|_Reg], _Names) ->
    lager:debug("failed to find ~p in ~p (~p)", [_N, _Names, _Reg]),
    'false'.

remote_register(Remote, Name, Pid) ->
    Payload = [{<<"Name">>, Name}
              ,{<<"State">>, 'pending'}
              ,{<<"Node">>, list_to_binary([ Remote+$0, "@remote.host"])}
               | kz_api:default_headers(<<"testing">>, <<"4.0.0">>)
              ],
    case kz_amqp_worker:call_collect(Payload, fun kapi_globals:publish_register/1) of
        {'error', _} -> 'no';
        {_, []} -> 'yes';
        {_, JObjs} ->
            case lists:all(fun kapi_globals:is_pending/1, JObjs) of
                'true' -> remote_register_success(Remote, Name);
                'false' -> 'no'
            end
    end.

remote_register_success(Remote, Name) ->
    Zone = list_to_binary(["zone", $0+Remote]),
    Payload = [{<<"Name">>, Name}
              ,{<<"State">>, 'local'}
              ,{<<"Node">>, list_to_binary([ Remote+$0, "@remote.host"])}
              ,{<<"Zone">>, Zone}
               | kz_api:default_headers(<<>>, <<"testing">>, <<"4.0.0">>)
              ],
    JObj = kz_json:from_list(Payload),
    Global = kz_global:from_jobj(JObj, Zone),
    gen_listener:call('kz_globals', {'insert_remote', Global}),
    'yes'.

-endif.
