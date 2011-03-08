#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./t
%%
%% This file is part of couchbeam released under the MIT license. 
%% See the NOTICE for more information.


main(_) ->
    etap:plan(7),
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
    ok.
    
stop_test() ->
    Server = couchbeam:server_connection(),

    catch couchbeam:delete_db(Server, "couchbeam_testdb"),
    catch couchbeam:delete_db(Server, "couchbeam_testdb2"),
    ok.
    

test() ->
    Server = couchbeam:server_connection(),

    {ok, Db} = couchbeam:create_db(Server, "couchbeam_testdb"),

    DesignDoc = {[
        {<<"_id">>, <<"_design/couchbeam">>},
        {<<"language">>,<<"javascript">>},
        {<<"views">>,
            {[{<<"test">>,
                {[{<<"map">>,
                    <<"function (doc) {\n if (doc.type == \"test\") {\n emit(doc._id, doc);\n}\n}">>
                }]}
            },{<<"test2">>,
                {[{<<"map">>,
                    <<"function (doc) {\n if (doc.type == \"test2\") {\n emit(doc._id, null);\n}\n}">>
                }]}
            }]}
        }
    ]},

    Doc = {[
        {<<"type">>, <<"test">>}
    ]},

    couchbeam:save_docs(Db, [DesignDoc, Doc, Doc]),
    couchbeam:ensure_full_commit(Db),

    {ok, AllDocs} = couchbeam:all_docs(Db),

    {ok, {Rst}} = couchbeam_view:fetch(AllDocs),

    TotalRows = proplists:get_value(<<"total_rows">>, Rst),
    etap:is(TotalRows, 3, "total_rows ok"),

    Rows = proplists:get_value(<<"rows">>, Rst),
    etap:is(length(Rows), 3, "number of rows ok"),


    {ok, View} = couchbeam:view(Db, "couchbeam/test"),

    {ok, {Rst2}} = couchbeam_view:fetch(View),
    TotalRows2 = proplists:get_value(<<"total_rows">>, Rst2),
    etap:is(TotalRows2, 2, "total_rows ok"),

    Rows2 = proplists:get_value(<<"rows">>, Rst2),
    etap:is(length(Rows2), 2, "number of rows ok"),

    {ok, View1} = couchbeam:view(Db, "couchbeam/test",
        [{"include_docs",true}]),

    {ok, {FirstRow}} = couchbeam_view:first(View1),
    {Doc1} = proplists:get_value(<<"doc">>, FirstRow),
    
    etap:is(proplists:get_value(<<"type">>, Doc1), <<"test">>, "first with
include docs ok"),

    Fun = fun(Row, AttIn) -> [Row|AttIn] end,
    Att = couchbeam_view:fold(View, Fun),
    etap:is(length(Att), 2, "fold ok"),

    Tid =  ets:new(couchbeam_test, [set, private]),
    Fun1 = fun({Row}) -> 
        Key = proplists:get_value(<<"key">>, Row),
        true = ets:insert(Tid, {Key, Row})
    end,
    couchbeam_view:foreach(View, Fun1),
    etap:is(ets:info(Tid, size), 2, "foreach ok"),
    ok.



