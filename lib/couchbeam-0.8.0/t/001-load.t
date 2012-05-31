#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./t
%%
%% This file is part of couchbeam released under the MIT license. 
%% See the NOTICE for more information.

main(_) ->
    etap:plan(2),
    couchbeam:start(),
    etap:loaded_ok(couchbeam, "Module 'couchbeam' loaded"),
    etap:can_ok(couchbeam, server_info),
    etap:end_tests(),
    ok.
