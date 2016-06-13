%%%-------------------------------------------------------------------
%%% @Copyright (C) 2010-2016, 2600Hz
%%% @doc
%%% Test utilities for manipulating Kazoo/Kazoo documents
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_doc_tests).

-include_lib("eunit/include/eunit.hrl").

latest_attachment_id_test_() ->
    JObj1 = json_fixture(<<"doc_no_attachment.json">>),
    JObj2 = json_fixture(<<"doc_one_attachment.json">>),
    JObj3 = json_fixture(<<"doc_multiple_attachments.json">>),
    [?_assertEqual('undefined', kz_doc:latest_attachment_id(JObj1))
    ,?_assertEqual(<<"csv.csv">>, kz_doc:latest_attachment_id(JObj2))
    ,?_assertEqual(<<"newest">>, kz_doc:latest_attachment_id(JObj3))
    ].

%%% Internals

-spec json_fixture(file:name()) -> kz_json:object().
json_fixture(Filename) ->
    kz_json:load_fixture_from_file('kazoo', <<"fixtures">>, Filename).
