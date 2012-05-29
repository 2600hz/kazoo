#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./t
%%
%% This file is part of couchbeam released under the MIT license.
%% See the NOTICE for more information.


main(_) ->
    etap:plan(1),
    couchbeam:start(),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail()
    end,
    ok.


test() ->
    Server = couchbeam:server_connection(),
    {ok, {Data}} = couchbeam:server_info(Server),
    etap:is(proplists:get_value(<<"couchdb">>, Data), <<"Welcome">>, "message ok"),
    ok.
