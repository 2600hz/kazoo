#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./t
%%
%% This file is part of couchbeam released under the MIT license. 
%% See the NOTICE for more information.


main(_) ->
    etap:plan(28),
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
    
    {ok, Doc} = couchbeam:save_doc(Db, {[{<<"test">>, <<"blah">>}]}),
    etap:ok(case Doc of
        {_} -> true;
        _ -> false
    end, "save doc ok"),
    etap:ok(case couchbeam:save_doc(Db, 
            {[{<<"_id">>,<<"test">>}, {<<"test">>,<<"blah">>}]}) of
        {ok, {Props}} ->
            case proplists:get_value(<<"_id">>, Props) of
                <<"test">> -> true;
                _ -> false 
            end;
        _ -> false
    end, "save do with id ok"),
    {error, Error} = couchbeam:save_doc(Db, 
            {[{<<"_id">>,<<"test">>}, {<<"test">>,<<"blah">>}]}),
    etap:is(Error, conflict, "conflict raised"),

    Rev = couchbeam:lookup_doc_rev(Db, "test"),

    {ok, {Doc1}} = couchbeam:open_doc(Db, "test"),
    etap:is(proplists:get_value(<<"_rev">>, Doc1), Rev, "fetch rev ok"),
    etap:is(proplists:get_value(<<"test">>, Doc1), <<"blah">>, "fetch doc ok"),
    couchbeam:save_doc(Db, 
        {[{<<"_id">>,<<"test2">>}, {<<"test">>,<<"blah">>}]}),
    
    {ok, Doc2} = couchbeam:open_doc(Db, "test2"),
    etap:ok(case Doc2 of
        {_} -> true;
        _ -> false
        end, "test2 has been created"),
    etap:ok(case couchbeam:delete_doc(Db, Doc2) of
        {ok, _} -> true;
        _ -> false
        end, "doc2 has been deleted"),
    etap:is(couchbeam:open_doc(Db, "test2"), {error, not_found}, "test2 not found"),
       Doc4 = {[{<<"a">>, 1}]},

    etap:is(couchbeam_doc:get_value(<<"a">>, Doc4), 1, "get value ok"),
    etap:is(couchbeam_doc:get_value("a", Doc4), 1, "get value from string ok"),
    etap:is(couchbeam_doc:get_value("b", Doc4), undefined, "get undefined value ok"),
    etap:is(couchbeam_doc:get_value("b", Doc4, nil), nil, "get undefined value with Conn ok"),
    Doc5 = couchbeam_doc:set_value("b", 1, Doc4),
    etap:is(couchbeam_doc:get_value("b", Doc5), 1, "set value ok"),
    Doc6 = couchbeam_doc:set_value("b", 0, Doc5),
    etap:is(couchbeam_doc:get_value("b", Doc6), 0, "update value ok"),
    Doc7 = couchbeam_doc:delete_value("b", Doc6),
    etap:is(couchbeam_doc:get_value("b", Doc7), undefined, "delete value ok"),
    Doc8 = couchbeam_doc:extend([{<<"b">>, 1}, {<<"c">>, 1}], Doc7),
    etap:is(couchbeam_doc:get_value("b", Doc8), 1, "set value ok"),
    etap:is(couchbeam_doc:get_value("c", Doc8), 1, "set value ok"),
    Doc81 = couchbeam_doc:extend([{<<"c">>, 3}, {<<"d">>, 1}], Doc8),
    etap:is(couchbeam_doc:get_value("c", Doc81), 3, "set value ok"),
    etap:is(couchbeam_doc:get_value("d", Doc81), 1, "set value ok"),
    
    Doc9 = {[{<<"_id">>, <<"~!@#$%^&*()_+-=[]{}|;':,./<> ?">>}]},
    {ok, Doc10} = couchbeam:save_doc(Db, Doc9),
    {ok, Doc101} = couchbeam:open_doc(Db, <<"~!@#$%^&*()_+-=[]{}|;':,./<> ?">>),
    etap:ok(case Doc10 of
        {_} -> true;
        _ -> false
        end, "doc with special char created ok"),
    etap:is(couchbeam_doc:get_value(<<"_id">>, Doc101), <<"~!@#$%^&*()_+-=[]{}|;':,./<> ?">>, "doc with special char created ok 2"),
    
    Doc11 = {[{<<"f">>, 1}]},
    etap:not_ok(couchbeam_doc:is_saved(Doc11), "document isn't saved ok"),
    etap:is(couchbeam_doc:get_id(Doc11), undefined, "document id is undefined ok"),
    etap:is(couchbeam_doc:get_rev(Doc11), undefined, "document rev is undefined ok"),

    {ok, Doc12} = couchbeam:save_doc(Db, Doc11),
    etap:ok(couchbeam_doc:is_saved(Doc12), "document saved ok"),
    etap:isnt(couchbeam_doc:get_id(Doc12), undefined, "document id  defined ok"),
    etap:isnt(couchbeam_doc:get_rev(Doc12), undefined, "document rev is defined ok"),
    
    {ok, Doc13} = couchbeam:save_doc(Db, {[]}),
    {ok, Doc14} = couchbeam:save_doc(Db, {[]}),
    couchbeam:delete_docs(Db, [Doc13, Doc14]),
    
    etap:is(couchbeam:open_doc(Db, couchbeam_doc:get_id(Doc13)), 
        {error, not_found}, "bulk docs delete ok"),
    ok.
    
