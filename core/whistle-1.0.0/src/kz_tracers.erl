-module(kz_tracers).

%% To generate the flame.svg:
%% flamegraph.riak-color.pl is found in the eflame dep folder
%% cat /path/to/eflame.trace.out | grep -v 'SLEEP' | ./flamegraph.riak-color.pl > /var/www/html/flame.svg

-export([add_trace/1, add_trace/2
         ,gen_load/1
        ]).

add_trace(Pid) ->
    add_trace(Pid, 100*1000).

add_trace(Pid, CollectFor) ->
    spawn(fun() ->
                  io:format("started trace for ~p in ~p~n", [Pid, self()]),
                  BinFile = wh_util:to_list(<<"/home/james/eflame.trace">>),
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

gen_load(N) ->
    Start = os:timestamp(),
    PidRefs = [spawn_monitor(fun() -> do_load_gen(X) end) || X <- lists:seq(1, N)],
    wait_for_refs(Start, PidRefs).

wait_for_refs(Start, []) ->
    io:format("finished test after ~pms", [wh_util:elapsed_ms(Start)]);
wait_for_refs(Start, [{Pid, Ref}|R]=PRs) ->
    receive
        {'DOWN', Ref, 'process', Pid, _Reason} ->
            wait_for_refs(Start, R)
    after 1000 ->
            cache_status(Start),
            wait_for_refs(Start, PRs)
    end.

do_load_gen(1) ->
    add_trace(whereis(whistle_couch_cache)),
    do_load_gen();
do_load_gen(_) ->
    do_load_gen().

do_load_gen() ->
    Start = os:timestamp(),
    AccountId = wh_util:rand_hex_binary(16),
    AccountDb = wh_util:format_account_db(AccountId),
    'true' = couch_mgr:db_create(AccountDb),

    io:format("building ~p~n", [AccountDb]),

    Docs = [new_doc(AccountDb, Doc) || Doc <- lists:seq(1,1000)],
    {'ok', Saved} = couch_mgr:save_docs(AccountDb, Docs),
    _Result = (catch do_stuff_to_docs(Start, AccountDb, Saved)),
    couch_mgr:db_delete(AccountDb),
    io:format("deleted ~s~nafter ~p~n", [AccountDb, _Result]).

new_doc(AccountDb, Ref) ->
    wh_doc:update_pvt_parameters(
      wh_json:from_list([{<<"_id">>, wh_util:rand_hex_binary(16)}
                        ,{<<"ref">>, Ref}
                        ,{<<"pvt_type">>, <<"load_test">>}
                         | [{wh_util:rand_hex_binary(8)
                             ,wh_util:rand_hex_binary(8)
                            }
                            || _ <- lists:seq(1, 12)
                           ]
                        ])
                                ,AccountDb
     ).

do_stuff_to_docs(Start, _AccountDb, []) ->
    wait_for_cache(Start);
do_stuff_to_docs(Start, AccountDb, Docs) ->
    cache_status(Start),
    Ops = [{op(), Doc} || Doc <- Docs],
    io:format("doing stuff to ~p docs~n", [length(Docs)]),
    perform_ops(Start, AccountDb, Ops).

perform_ops(Start, AccountDb, Ops) ->
    do_stuff_to_docs(Start
                    ,AccountDb
                    ,lists:foldl(fun(Op, Acc) -> perform_op(Op, Acc, AccountDb) end
                                ,[]
                                ,Ops
                                )
                    ).

cache_status(Start) ->
    io:format("~p proc info: ~p~n"
             ,[wh_util:elapsed_ms(Start)
              ,erlang:process_info(whereis(whistle_couch_cache)
                                  ,[message_queue_len, current_function]
                                  )
              ]
             ).

wait_for_cache(Start) ->
    case erlang:process_info(whereis(whistle_couch_cache)
                            ,[message_queue_len
                             ,current_function
                             ]
                            )
    of
        [{message_queue_len, 0}
         ,{current_function, _F}
        ] ->
            io:format("~pms done (in ~p)~n", [wh_util:elapsed_ms(Start), _F]);
        [{message_queue_len, Len}
         ,{current_function, F}
        ] ->
            io:format("~pms msg left ~p in ~p~n", [wh_util:elapsed_ms(Start), Len, F]),
            timer:sleep(1000),
            wait_for_cache(Start)
    end.

perform_op({'edit', Doc}, Acc, AccountDb) ->
    Inc = wh_json:get_integer_value(<<"inc">>, Doc, 0),
    Edited = wh_json:set_value(<<"inc">>, Inc+1, Doc),
    {'ok', Saved} = couch_mgr:ensure_saved(AccountDb, Edited),
    {'ok', _Loaded} = couch_mgr:open_cache_doc(AccountDb, wh_doc:id(Saved)),
    [Saved | Acc];
perform_op({'delete', Doc}, Acc, AccountDb) ->
    couch_mgr:del_doc(AccountDb, Doc),
    Acc;
perform_op({'noop', Doc}, Acc, _AccountDb) ->
    [Doc | Acc].

op() ->
    case random:uniform(3) of
        1 -> 'edit';
        2 -> 'delete';
        3 -> 'noop'
    end.
