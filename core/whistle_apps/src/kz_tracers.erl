-module(kz_tracers).

%% To generate the flame.svg:
%% flamegraph.riak-color.pl is found in the eflame dep folder
%% cat /path/to/eflame.trace.out | grep -v 'SLEEP' | ./flamegraph.riak-color.pl > /var/www/html/flame.svg

-export([add_trace/1, add_trace/2
         ,gen_load/0
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

gen_load() ->
    spawn_monitor(fun do_load_gen/0).

do_load_gen() ->
    add_trace(whereis(whistle_couch_cache)),

    Start = os:timestamp(),
    AccountId = wh_util:rand_hex_binary(16),
    AccountDb = wh_util:format_account_db(AccountId),
    'true' = kz_datamgr:db_create(AccountDb),

    io:format("building ~p~n", [AccountDb]),

    Docs = [new_doc(AccountDb, Doc) || Doc <- lists:seq(1,400)],
    {'ok', Saved} = kz_datamgr:save_docs(AccountDb, Docs),
    _Result = (catch do_stuff_to_docs(Start, AccountDb, Saved)),
    kz_datamgr:db_delete(AccountDb),
    io:format("deleted ~s~nafter ~p~n", [AccountDb, _Result]).

new_doc(AccountDb, Ref) ->
    wh_doc:update_pvt_parameters(
      wh_json:from_list([{<<"_id">>, wh_util:rand_hex_binary(16)}
                        ,{<<"ref">>, Ref}
                         ,{<<"pvt_type">>, <<"load_test">>}
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
    do_stuff_to_docs(
      Start
      ,AccountDb
      ,lists:foldl(fun(Op, Acc) -> perform_op(Op, Acc, AccountDb) end
                  ,[]
                  ,Ops
                  )
     ).

cache_status(Start) ->
    io:format("~p proc info: ~p messages~n"
             ,[wh_util:elapsed_ms(Start)
               ,erlang:process_info(whereis(whistle_couch_cache)
                                   ,[message_queue_len]
                                   )
              ]
             ).

wait_for_cache(Start) ->
    case erlang:process_info(whereis(whistle_couch_cache)
                            ,[message_queue_len]
                            )
    of
        [{message_queue_len, 0}] ->
            io:format("~p done~n", [wh_util:elapsed_ms(Start)]);
        [{message_queue_len, Len}] ->
            io:format("~p left ~p~n", [wh_util:elapsed_ms(Start), Len]),
            timer:sleep(1000),
            wait_for_cache(Start)
    end.

perform_op({'edit', Doc}, Acc, AccountDb) ->
    Inc = wh_json:get_integer_value(<<"inc">>, Doc, 0),
    Edited = wh_json:set_value(<<"inc">>, Inc+1, Doc),
    {'ok', Saved} = kz_datamgr:save_doc(AccountDb, Edited),
    {'ok', _Loaded} = kz_datamgr:open_cache_doc(AccountDb, wh_doc:id(Saved)),
    [Saved | Acc];
perform_op({'delete', Doc}, Acc, AccountDb) ->
    kz_datamgr:del_doc(AccountDb, Doc),
    Acc;
perform_op({'noop', Doc}, Acc, _AccountDb) ->
    [Doc | Acc].

op() ->
    case random:uniform(3) of
        1 -> 'edit';
        2 -> 'delete';
        3 -> 'noop'
    end.
