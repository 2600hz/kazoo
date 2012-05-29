#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./t
%%
%% This file is part of couchbeam released under the MIT license. 
%% See the NOTICE for more information.

-include_lib("kernel/include/file.hrl").

main(_) ->
    etap:plan(13),
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
    Doc = {[
        {<<"_id">>, <<"test">>}
    ]},
    {ok, Doc1} = couchbeam:save_doc(Db, Doc),
    RevDoc1 = couchbeam_doc:get_value(<<"_rev">>, Doc1),
    io:format("rev ~p~n", [RevDoc1]),
    {ok, {Res}} = couchbeam:put_attachment(Db,"test", "test", "test", 
            [{rev, RevDoc1}]),

    RevDoc11 = proplists:get_value(<<"rev">>, Res),
    etap:is(RevDoc1 =:= RevDoc11, false, "put attachment ok"),


    {ok, Attachment} = couchbeam:fetch_attachment(Db, "test", "test"),
    etap:is(Attachment, <<"test">>, "fetch attachment ok"),

    {ok, Doc2} = couchbeam:open_doc(Db, "test"),
    RevDoc2 = couchbeam_doc:get_value(<<"_rev">>, Doc2),


    etap:is(case couchbeam:delete_attachment(Db, Doc2, "test") of
        {ok,  {_}} -> true;
        _ -> false
    end, true, "delete attachment ok"),

    Doc3 = {[
        {<<"_id">>, <<"test2">>}
    ]},
    Doc4 = couchbeam_attachments:add_inline(Doc3, "test", "test.txt"),
    Doc5 = couchbeam_attachments:add_inline(Doc4, "test2", "test2.txt"),
    {ok, _} = couchbeam:save_doc(Db, Doc5),
    {ok, Attachment1} = couchbeam:fetch_attachment(Db, "test2", "test.txt"),
    {ok, Attachment2} = couchbeam:fetch_attachment(Db, "test2", "test2.txt"),
    etap:is(Attachment1, <<"test">>, "fetch attachment ok"),
    etap:is(Attachment2, <<"test2">>, "fetch attachment ok"),

    {ok, Doc6} = couchbeam:open_doc(Db, "test2"),
    Doc7 = couchbeam_attachments:delete_inline(Doc6, "test2.txt"),
    {ok, _} = couchbeam:save_doc(Db, Doc7),
    etap:is(couchbeam:fetch_attachment(Db, "test2", "test2.txt"), 
            {error, not_found}, "inline attachment deleted"),

    {ok, Attachment4} = couchbeam:fetch_attachment(Db, "test2", "test.txt"),
    etap:is(Attachment4, <<"test">>, "fetch attachment ok"),
    
    {ok, Doc8} = couchbeam:save_doc(Db, {[]}),

    {ok, FileInfo} = file:read_file_info("t/1M"),
    FileSize = FileInfo#file_info.size,
    {ok, Fd} = file:open("t/1M", [read]),
    {ok, Res2} = couchbeam:put_attachment(Db, couchbeam_doc:get_id(Doc8), 
        "1M", fun() ->
            case file:read(Fd, 4096) of
                {ok, Data} ->  {ok, iolist_to_binary(Data)};
                _ -> eof
            end
        end, [{content_length, FileSize}, {rev, couchbeam_doc:get_rev(Doc8)}]),
    file:close(Fd),

    {ok, Doc9} = couchbeam:open_doc(Db, couchbeam_doc:get_id(Doc8)),
    Attachements = couchbeam_doc:get_value(<<"_attachments">>, Doc9),
    etap:isnt(Attachements, undefined, "attachment stream ok"),
    Attachment5 = couchbeam_doc:get_value(<<"1M">>, Attachements),
    etap:isnt(Attachment5, undefined, "attachment 1M uploaded ok"),
    etap:is(couchbeam_doc:get_value(<<"length">>, Attachment5), FileSize, "attachment 1M size ok"),
    
    {ok, Bin} = couchbeam:fetch_attachment(Db, couchbeam_doc:get_id(Doc8), "1M"),
    etap:is(iolist_size(Bin), FileSize, "fetch streammed attachment ok"),
    

    {ok, Res3}= couchbeam:put_attachment(Db, "test/2", "test", "test"),
    {ok, Attachment10} = couchbeam:fetch_attachment(Db, "test/2", "test"),
    etap:is(Attachment10, <<"test">>, "fetch attachment with encoded id ok"),  

    {ok, Res4}= couchbeam:put_attachment(Db, "test3", "test", "test"),
    {ok, Attachment11} = couchbeam:fetch_attachment(Db, "test3", "test"),
    etap:is(Attachment11, <<"test">>, "fetch attachment with clength ok"),
     
    ok.
