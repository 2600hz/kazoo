%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_tracers).

%% To generate the flame.svg:
%% flamegraph.riak-color.pl is found in the eflame dep folder
%% cat /path/to/eflame.trace.out | grep -v 'SLEEP' | ./flamegraph.riak-color.pl > /var/www/html/flame.svg

-export([add_trace/1, add_trace/2
        ,gen_load/1, gen_load/2
        ]).

-include_lib("kazoo_data/src/kz_data.hrl").

-spec add_trace(pid()) -> 'ok'.
add_trace(Pid) ->
    add_trace(Pid, 100*1000).

-spec add_trace(pid(), pos_integer()) -> 'ok'.
add_trace(Pid, CollectFor) ->
    spawn(fun() ->
                  io:format("started trace for ~p in ~p~n", [Pid, self()]),
                  dbg:stop_clear(),
                  BinFile = kz_term:to_list(<<"/tmp/eflame.trace">>),
                  eflame2:write_trace('global_calls_plus_new_procs'
                                     ,BinFile
                                     ,Pid
                                     ,CollectFor
                                     ),
                  io:format("trace for ~p done~n", [Pid]),
                  eflame2:format_trace(BinFile, BinFile ++ ".out"),
                  io:format("trace formatted to ~s.out~n", [BinFile])
          end),
    'ok'.

-spec gen_load(non_neg_integer()) -> 'ok'.
gen_load(N) ->
    gen_load(N, 1000).

-spec gen_load(non_neg_integer(), non_neg_integer()) -> 'ok'.
gen_load(N, D) ->
    Start = os:timestamp(),
    _ = rand:seed('exsplus', Start),

    {PointerTab, MonitorTab} = gen_listener:call(?CACHE_NAME, {'tables'}),
    Tables = [?CACHE_NAME, PointerTab, MonitorTab],
    table_status(Tables),

    PidRefs = [spawn_monitor(fun() -> do_load_gen(D) end) || _ <- lists:seq(1, N)],
    wait_for_refs(Start
                 ,cache_data()
                 ,Tables
                 ,PidRefs
                 ).

wait_for_refs(Start, MaxMailbox, Tables, []) ->
    case cache_data() of
        {0, _} ->
            io:format("finished test after ~pms: ~p~n", [kz_time:elapsed_ms(Start), MaxMailbox]),
            table_status(Tables);
        _ -> timer:sleep(1000),
             wait_for_refs(Start, MaxMailbox, Tables, [])
    end;
wait_for_refs(Start, {M,_G}=MG, Tables, [{Pid, Ref}|R]=PRs) ->
    receive
        {'DOWN', Ref, 'process', Pid, _Reason} ->
            wait_for_refs(Start, MG, Tables, R)
    after 1000 ->
            case cache_data() of
                {N, F}=NF when N > M ->
                    io:format("new max message queue size ~p (~p)~n", [N, F]),
                    table_status(Tables),
                    wait_for_refs(Start, NF, Tables, PRs);
                _ ->
                    wait_for_refs(Start, MG, Tables, PRs)
            end
    end.

table_status(Ts) ->
    io:format("tables: ~p ~p ~p~n", [{T, ets:info(T, 'size')} || T <- Ts]).

do_load_gen(Ds) ->
    AccountId = kz_binary:rand_hex(16),
    AccountDb = kz_util:format_account_db(AccountId),
    'true' = kz_datamgr:db_create(AccountDb),

    io:format("building ~p with ~p docs~n", [AccountDb, Ds]),

    Docs = [new_doc(AccountDb, Doc) || Doc <- lists:seq(1,Ds)],

    Start = os:timestamp(),
    _ = rand:seed('exsplus', Start),

    case rand:uniform(100) of
        42 ->
            io:format("unlucky account ~s getting deleted early: ", [AccountDb]);
        _N ->
            catch do_stuff_to_docs(Start, AccountDb, Docs)
    end,

    kz_datamgr:db_delete(AccountDb),
    timer:sleep(Ds * 10),
    verify_no_docs(Docs),
    io:format("cleaned up ~s~n", [AccountDb]).

verify_no_docs(Docs) ->
    case lists:all(fun verify_no_doc/1, Docs) of
        'true' ->
            io:format("all docs successfully flushed~n");
        'false' ->
            io:format("flushing failed~n")
    end.

verify_no_doc(Doc) ->
    case kz_cache:peek_local(?CACHE_NAME
                            ,{'couch_util'
                             ,kz_doc:account_db(Doc)
                             ,kz_doc:id(Doc)
                             }
                            )
    of
        {'error', 'not_found'} -> 'true';
        {'ok', _} -> 'false'
    end.

new_doc(AccountDb, Ref) ->
    BaseDoc = kz_json:from_list([{<<"_id">>, kz_binary:rand_hex(16)}
                                ,{<<"ref">>, Ref}
                                ,{<<"pvt_type">>, <<"load_test">>}
                                 | [{kz_binary:rand_hex(8)
                                    ,kz_binary:rand_hex(8)
                                    }
                                    || _ <- lists:seq(1, 12)
                                   ]
                                ]),
    Doc = kz_doc:update_pvt_parameters(BaseDoc, AccountDb),

    {'ok', Saved} = kz_datamgr:save_doc(AccountDb, Doc),
    {'ok', _Loaded} = kz_datamgr:open_cache_doc(AccountDb, kz_doc:id(Saved)),
    Saved.

do_stuff_to_docs(Start, _AccountDb, []) ->
    wait_for_cache(Start);
do_stuff_to_docs(Start, AccountDb, Docs) ->
    Ops = [{op(), Doc} || Doc <- Docs],
    perform_ops(Start, AccountDb, Ops).

perform_ops(Start, AccountDb, Ops) ->
    do_stuff_to_docs(Start
                    ,AccountDb
                    ,lists:foldl(fun(Op, Acc) -> perform_op(Op, Acc, AccountDb) end
                                ,[]
                                ,Ops
                                )
                    ).

cache_data() ->
    [{'message_queue_len', N}
    ,{'current_function', F}
    ] = erlang:process_info(whereis(?CACHE_NAME)
                           ,['message_queue_len', 'current_function']
                           ),
    {N, F}.

wait_for_cache(Start) ->
    wait_for_cache(Start, {0, 'ok'}).

wait_for_cache(Start, {N, _}=NF) ->
    case cache_data() of
        {M, G}=MG when M > N ->
            io:format("~p new max msg queue size ~p (~p)~n", [Start, M, G]),
            timer:sleep(1000),
            wait_for_cache(Start, MG);
        {0, _F} ->
            io:format("~pms done (in ~p)~n", [kz_time:elapsed_ms(Start), _F]);
        _ ->
            timer:sleep(1000),
            wait_for_cache(Start, NF)
    end.

perform_op({'edit', Doc}, Acc, AccountDb) ->
    Inc = kz_json:get_integer_value(<<"inc">>, Doc, 0),
    Edited = kz_json:set_value(<<"inc">>, Inc+1, Doc),
    {'ok', Saved} = kz_datamgr:save_doc(AccountDb, Edited),
    {'ok', _Loaded} = kz_datamgr:open_cache_doc(AccountDb, kz_doc:id(Saved)),
    [Saved | Acc];
perform_op({'delete', Doc}, Acc, AccountDb) ->
    _ = kz_datamgr:del_doc(AccountDb, Doc),
    Acc;
perform_op({'noop', Doc}, Acc, _AccountDb) ->
    [Doc | Acc].

op() ->
    case rand:uniform(3) of
        1 -> 'edit';
        2 -> 'delete';
        3 -> 'noop'
    end.
