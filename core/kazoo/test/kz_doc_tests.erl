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
-define(APP, kazoo).

latest_attachment_id_test_() ->
    {ok,JObj1} = kz_json:fixture(?APP, <<"fixtures/doc_no_attachment.json">>),
    {ok,JObj2} = kz_json:fixture(?APP, <<"fixtures/doc_one_attachment.json">>),
    {ok,JObj3} = kz_json:fixture(?APP, <<"fixtures/doc_multiple_attachments.json">>),
    [?_assertEqual('undefined', kz_doc:latest_attachment_id(JObj1))
    ,?_assertEqual(<<"csv.csv">>, kz_doc:latest_attachment_id(JObj2))
    ,?_assertEqual(<<"newest">>, kz_doc:latest_attachment_id(JObj3))
    ].
