%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2020-, 2600Hz
%%% @doc An attempt to capture the account update interleaving via property tests
%%%
%%% Also served to introduce PropEr to devs. Never got it to reproduce the issue
%%% on systems that pqc_cb_accounts errored on though.
%%%
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_kzs_cache).
-behaviour(proper_statem).

-export([command/1
        ,initial_state/0
        ,next_state/3
        ,postcondition/3
        ,precondition/2

        ,correct/0
        ,correct_parallel/0

        ,create/0, update/1, get/1, save/1
        ,run_counterexample/0, run_counterexample/1
        ]).

-include_lib("proper/include/proper.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-define(DB, <<"account%2F94%2F71%2Ff6b1ee6079282d3b5559532fa6d9">>).
-define(ID, <<"9471f6b1ee6079282d3b5559532fa6d9">>).

-spec correct() -> any().
correct() ->
    ?FORALL(Cmds
           ,commands(?MODULE)
           ,begin
                init(),
                {History, State, Result} = run_commands(?MODULE, Cmds),
                terminate(),
                ?WHENFAIL(io:format("Final State: ~p\nFailing Cmds: ~p\n"
                                   ,[State, zip(Cmds, History)]
                                   )
                         ,aggregate(command_names(Cmds), Result =:= 'ok')
                         )
            end
           ).

-spec correct_parallel() -> any().
correct_parallel() ->
    ?FORALL(Cmds
           ,parallel_commands(?MODULE)
           ,begin
                init(),
                {History, State, Result} = run_parallel_commands(?MODULE, Cmds),
                terminate(),
                ?WHENFAIL(io:format("=======~n"
                                    "Failing command sequence:~n~p~n"
                                    "At state: ~p~n"
                                    "=======~n"
                                    "Result: ~p~n"
                                    "History: ~p~n"
                                   ,[Cmds,State,Result,History]
                                   ),
                          aggregate(command_names(Cmds), Result =:= 'ok')
                         )
            end
           ).

init() ->
    kz_datamgr:db_create(?DB),
    _ = kz_datamgr:del_doc(?KZ_ACCOUNTS_DB, ?ID),

    kz_datamgr:flush_cache_docs(?DB),
    kz_datamgr:flush_cache_doc(?KZ_ACCOUNTS_DB, ?ID).

terminate() ->
    _ = kz_datamgr:del_doc(?KZ_ACCOUNTS_DB, ?ID),
    kz_datamgr:db_delete(?DB),

    kz_datamgr:flush_cache_docs(?DB),
    kz_datamgr:flush_cache_doc(?KZ_ACCOUNTS_DB, ?ID).

-spec run_counterexample() -> any().
run_counterexample() ->
    run_counterexample(proper:counterexample()).

-spec run_counterexample(any()) -> any().
run_counterexample('undefined') -> 'undefined';
run_counterexample([SeqSteps]) ->
    init(),
    run_counterexample(SeqSteps, initial_state()).

run_counterexample(SeqSteps, State) ->
    process_flag('trap_exit', 'true'),

    try lists:foldl(fun transition_if/2
                   ,{1, State, #{}}
                   ,SeqSteps
                   )
    catch
        'throw':T -> {'throw', T}
    after
        terminate()
    end.

transition_if({'set', Var, Call}, {Step, State, Env}) ->
    {'call', M, F, As} = Call,
    Resp = erlang:apply(M, F, fix_args(As, Env)),
    io:format('user', "~w: at state ~s: ~w -> ~s~n", [Step, State, Call, Resp]),

    case postcondition(State, Call, Resp) of
        'true' ->
            {Step+1, next_state(State, Resp, Call), Env#{Var =>Resp}};
        'false' ->
            io:format("failed on step ~p~n", [Step]),
            throw({'failed_postcondition', State, Call, Resp})
    end.

fix_args(Args, Env) ->
    lists:foldr(fun(A, Acc) -> fix_arg(A, Acc, Env) end, [], Args).

fix_arg(Arg, Acc, Env) ->
    case maps:get(Arg, Env, 'undefined') of
        'undefined' -> [Arg | Acc];
        EnvArg -> [EnvArg | Acc]
    end.

-spec create() -> kz_term:ne_binary().
create() ->
    {'ok', D} = kzd_accounts:save(doc()),
    kz_doc:revision(D).

-spec update(kz_term:ne_binary()) -> kz_term:ne_binary().
update(_Rev) ->
    case kzd_accounts:update(?ID, doc_updates('undefined')) of
        {'ok', D} -> kz_doc:revision(D);
        {'error', _} -> 'undefined'
    end.

-spec save(kz_term:ne_binary()) -> kz_term:api_ne_binary().
save(Rev) ->
    case kzd_accounts:save(doc(Rev)) of
        {'ok', D} -> kz_doc:revision(D);
        {'error', _} -> 'undefined'
    end.

-spec get(kz_term:api_ne_binary()) -> kz_term:api_ne_binary().
get(_Rev) ->
    case kzd_accounts:fetch(?ID) of
        {'ok', D} -> check_account_doc(D, kz_doc:is_deleted(D));
        {'error', 'not_found'} -> 'undefined'
    end.

check_account_doc(_D, 'true') ->
    'undefined';
check_account_doc(D, 'false') ->
    kz_doc:revision(D).

doc() ->
    doc('undefined').

doc(Rev) ->
    kz_json:set_values(
      [{<<"_id">>, ?ID}
      ,{<<"foo">>, <<"bar">>}
      ,{<<"pvt_account_db">>, ?DB}
       | doc_updates(Rev)
      ]
     ,kzd_accounts:new()
     ).

doc_updates(Rev) ->
    [{<<"_rev">>, Rev}
     | [{[kz_term:to_binary(Key)], kz_binary:rand_hex(16)} || Key <- lists:seq(1,50)]
    ].

-spec initial_state() -> 'undefined'.
initial_state() ->
    'undefined'.

-spec command(kz_term:api_ne_biary()) -> proper_types:type().
command('undefined') ->
    oneof([{'call', ?MODULE, 'create', []}
          ,{'call', ?MODULE, 'get', ['undefined']}
          ]);
command(Rev) ->
    oneof([{'call', ?MODULE, 'update', [Rev]}
          ,{'call', ?MODULE, 'save', [Rev]}
          ,{'call', ?MODULE, 'get', [Rev]}
          ,{'call', 'timer', 'sleep', [range(0,50)]}
          ]).

-spec next_state(kz_term:api_ne_binary(), any(), tuple()) -> kz_term:api_ne_binary().
next_state(_OldRev
          ,NewRev
          ,{'call', ?MODULE, 'create', []}
          ) ->
    NewRev;
next_state(_OldRev
          ,NewRev
          ,{'call', ?MODULE, 'update', [_]}
          ) ->
    NewRev;
next_state(_OldRev
          ,NewRev
          ,{'call', ?MODULE, 'save', [_]}
          ) ->
    NewRev;
next_state(Rev, _V, {'call', ?MODULE, 'get', [_]}) ->
    Rev;
next_state(Rev, _V, {'call', 'timer', 'sleep', [_]}) ->
    Rev.

-spec precondition(any(), any()) -> 'true'.
precondition(_Method, _Call) -> 'true'.

-spec postcondition(kz_term:api_ne_binary(), tuple(), kz_term:ne_binary()) -> boolean().
postcondition(NoRev
             ,{'call', ?MODULE, 'create', []}
             ,Rev
             ) ->
    NoRev =:= 'undefined'
        andalso is_binary(Rev);
postcondition(OldRev
             ,{'call', ?MODULE, 'update', [_OldRev]}
             ,NewRev
             ) ->
    [OldN, _OldRand] = binary:split(OldRev, <<"-">>),
    [NewN, _NewRand] = binary:split(NewRev, <<"-">>),

    kz_term:to_integer(NewN) =:= kz_term:to_integer(OldN) + 1;
postcondition(OldRev
             ,{'call', ?MODULE, 'save', [_OldRev]}
             ,NewRev
             ) ->
    [OldN, _OldRand] = binary:split(OldRev, <<"-">>),
    [NewN, _NewRand] = binary:split(NewRev, <<"-">>),

    kz_term:to_integer(NewN) =:= kz_term:to_integer(OldN) + 1;
postcondition(OldRev
             ,{'call', ?MODULE, 'get', [_OldRev]}
             ,GetRev
             ) ->
    OldRev =:= GetRev;
postcondition(_Rev, {'call', 'timer', 'sleep', [_]}, _Res) -> 'true'.
