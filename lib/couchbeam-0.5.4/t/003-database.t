#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./t 
%%
%% This file is part of couchbeam released under the MIT license. 
%% See the NOTICE for more information.



main(_) ->
    etap:plan(10),
    start_app(),
    case (catch test()) of
        ok ->
            stop_test(),
            etap:end_tests();
        Other ->
            stop_test(),
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail()
    end,
    ok.

start_app() ->
    couchbeam:start(),
    Server = couchbeam:server_connection(),
    catch couchbeam:delete_db(Server, "couchbeam_testdb"),
    catch couchbeam:delete_db(Server, "couchbeam_testdb2"),
    catch couchbeam:delete_db(Server, "couchbeam_testdb3"),
    ok.
    
stop_test() ->
    Server = couchbeam:server_connection(),
    catch couchbeam:delete_db(Server, "couchbeam_testdb"),
    catch couchbeam:delete_db(Server, "couchbeam_testdb2"),
    catch couchbeam:delete_db(Server, "couchbeam_testdb3"),
    ok.
    
test() ->
    Server = couchbeam:server_connection(),
    etap:is(case couchbeam:create_db(Server, "couchbeam_testdb") of
        {ok, _} ->true;
        _ -> false
    end, true, "db created"),
    Res0 = couchbeam:create_db(Server, "couchbeam_testdb"),
    etap:is(Res0, {error, db_exists}, "database already loaded ok"),
    etap:is(case couchbeam:create_db(Server, "couchbeam_testdb2") of
        {ok, _} -> true;
        _ -> false
    end, true, "db2 created ok"),
    {ok, AllDbs} = couchbeam:all_dbs(Server),
    etap:ok(is_list(AllDbs), "all_dbs return a list"),
    etap:ok(lists:member(<<"couchbeam_testdb">>, AllDbs), "couchbeam_testdb exists ok "),
    etap:ok(couchbeam:db_exists(Server, "couchbeam_testdb"), "is_db exists ok "),
    etap:ok(lists:member(<<"couchbeam_testdb2">>, AllDbs), "couchbeam_testdb2 exists ok"),
    etap:is(case couchbeam:delete_db(Server, "couchbeam_testdb2") of
        {ok, _} -> true;
        _ -> false
    end,  true, "delete couchbeam_testdb2 ok"),
    {ok, AllDbs1} = couchbeam:all_dbs(Server),
    etap:not_ok(lists:member(<<"couchbeam_testdb2">>, AllDbs1), 
            "couchbeam_testdb2 don't exists ok"),
    etap:not_ok(couchbeam:db_exists(Server, "couchbeam_testdb2"), 
        "is_db2not exists ok "),

    ok.
