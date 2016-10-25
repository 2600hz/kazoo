#!/usr/bin/env escript
%%! +A0 -pa deps/coveralls/ebin
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

main(_) ->
    All = filelib:wildcard("*/*/*.coverdata"),
    ok = code:add_pathsa([filename:join(filename:dirname(Path), "ebin") || Path <- All]),
    Id = os:getenv("TRAVIS_JOB_ID"),
    coveralls:convert_and_send_file(All, Id, "travis-ci").
