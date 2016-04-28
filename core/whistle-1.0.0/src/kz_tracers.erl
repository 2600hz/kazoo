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
                  dbg:stop_clear(),
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
    wait_for_refs(Start, {0, 'ok'}, PidRefs).

wait_for_refs(Start, MaxMailbox, []) ->
    case cache_data() of
        [{message_queue_len, 0} | _] ->
            io:format("finished test after ~pms: ~p~n", [wh_util:elapsed_ms(Start), MaxMailbox]);
        _ -> timer:sleep(1000),
             wait_for_refs(Start, MaxMailbox, [])
    end;
wait_for_refs(Start, {M, G}, [{Pid, Ref}|R]=PRs) ->
    receive
        {'DOWN', Ref, 'process', Pid, _Reason} ->
            wait_for_refs(Start, {M, G}, R)
    after 1000 ->
            case cache_data() of
                [{message_queue_len, N}
                 ,{current_function, F}
                ] when N > M ->
                    io:format("new max message queue size ~p (~p)~n", [N, F]),
                    wait_for_refs(Start, {N, F}, PRs);
                _ ->
                    wait_for_refs(Start, {M, G}, PRs)
            end
    end.

do_load_gen(1) ->
    %% add_trace(whereis(whistle_couch_cache)),
    do_load_gen();
do_load_gen(_) ->
    do_load_gen().

do_load_gen() ->
    AccountId = wh_util:rand_hex_binary(16),
    AccountDb = wh_util:format_account_db(AccountId),
    'true' = couch_mgr:db_create(AccountDb),

    io:format("building ~p~n", [AccountDb]),

    Docs = [new_doc(AccountDb, Doc) || Doc <- lists:seq(1,1000)],

    Start = os:timestamp(),

    _Result = (catch do_stuff_to_docs(Start, AccountDb, Docs)),
    couch_mgr:db_delete(AccountDb),
    io:format("deleted ~s~nafter ~p~n", [AccountDb, _Result]).

new_doc(AccountDb, Ref) ->
    Doc = wh_doc:update_pvt_parameters(
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
           ),
    {'ok', Saved} = couch_mgr:save_doc(AccountDb, Doc),
    {'ok', _Loaded} = couch_mgr:open_cache_doc(AccountDb, wh_doc:id(Saved)),
    Saved.

do_stuff_to_docs(Start, _AccountDb, []) ->
    wait_for_cache(Start);
do_stuff_to_docs(Start, AccountDb, Docs) ->
    cache_status(Start),
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

cache_status(Start) ->
    io:format("~p proc info: ~p~n"
             ,[wh_util:elapsed_ms(Start)
              ,cache_data()
              ]
             ).

cache_data() ->
    erlang:process_info(whereis(whistle_couch_cache)
                       ,[message_queue_len, current_function]
                       ).

wait_for_cache(Start) ->
    wait_for_cache(Start, {0, 'ok'}).

wait_for_cache(Start, {N, F}) ->
    case cache_data() of
        [{message_queue_len, M}
         ,{current_function, G}
        ] when M > N ->
            io:format("~p new max msg queue size ~p (~p)~n", [Start, M, G]),
            timer:sleep(1000),
            wait_for_cache(Start, {M, G});
        [{message_queue_len, 0}
        ,{current_function, _F}
        ] ->
            io:format("~pms done (in ~p)~n", [wh_util:elapsed_ms(Start), _F]);
        _ ->
            timer:sleep(1000),
            wait_for_cache(Start, {N, F})
    end.

perform_op({'edit', Doc}, Acc, AccountDb) ->
    Inc = wh_json:get_integer_value(<<"inc">>, Doc, 0),
    Edited = wh_json:set_value(<<"inc">>, Inc+1, Doc),
    {'ok', Saved} = couch_mgr:save_doc(AccountDb, Edited),
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
