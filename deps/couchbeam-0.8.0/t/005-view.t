#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./t
%%
%% This file is part of couchbeam released under the MIT license.
%% See the NOTICE for more information.


main(_) ->
    etap:plan(5),
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

    {ok, AllDocs} = couchbeam_view:fetch(Db),

    etap:is(length(AllDocs), 3, "total_rows ok"),


    {ok, Rst2} = couchbeam_view:fetch(Db, {"couchbeam", "test"}),
    etap:is(length(Rst2), 2, "total_rows ok"),


    {ok, {FirstRow}} = couchbeam_view:first(Db, {"couchbeam", "test"},
        [include_docs]),
    {Doc1} = proplists:get_value(<<"doc">>, FirstRow),

    etap:is(proplists:get_value(<<"type">>, Doc1), <<"test">>,
        "first with include docs ok"),


    Docs = [
        {[{<<"_id">>, <<"test1">>}, {<<"type">>, <<"test">>}, {<<"value">>, 1}]},
        {[{<<"_id">>, <<"test2">>}, {<<"type">>, <<"test">>}, {<<"value">>, 2}]},
        {[{<<"_id">>, <<"test3">>}, {<<"type">>, <<"test">>}, {<<"value">>, 3}]},
        {[{<<"_id">>, <<"test4">>}, {<<"type">>, <<"test">>}, {<<"value">>, 4}]}
    ],

    couchbeam:save_docs(Db, Docs),
    couchbeam:ensure_full_commit(Db),

    case os:getenv("TRAVIS") of
    false ->
        {ok, Rst3} = couchbeam_view:fetch(Db, {"couchbeam", "test"},
            [{start_key, <<"test">>}]),

        etap:is(length(Rst3), 4, "total_rows with start_key ok"),

        {ok, Rst4} = couchbeam_view:fetch(Db, {"couchbeam", "test"},
            [{start_key, <<"test">>}, {end_key, <<"test3">>}]),

        etap:is(length(Rst4), 3, "total_rows with end_keys ok");
    _ ->

        {ok, Rst3} = couchbeam_view:fetch(Db, {"couchbeam", "test"},
            [{startkey, <<"test">>}]),

        etap:is(length(Rst3), 4, "total_rows with start_key ok"),

        {ok, Rst4} = couchbeam_view:fetch(Db, {"couchbeam", "test"},
            [{startkey, <<"test">>}, {endkey, <<"test3">>}]),

        etap:is(length(Rst4), 3, "total_rows with end_keys ok")
    end,


    ok.
