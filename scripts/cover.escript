#!/usr/bin/env escript
%%! +A0 -pa deps/coveralls/ebin
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

main(_) ->
    All = filelib:wildcard("**/*/*.coverdata"),
    'ok' = code:add_pathsa([filename:join(filename:dirname(Path), "ebin") || Path <- All]),
    {ServiceName, JobId} = get_ci_job_id(os:getenv("TRAVIS_JOB_ID"), os:getenv("CIRCLECI")),
    'true' = is_list(JobId),
    try coveralls:convert_and_send_file(All, JobId, ServiceName, "")
    catch
        _E:_R -> io:format("\ncoveralls ~p:\n\t~p\n", [_E, _R])
    end.

get_ci_job_id("true", _) ->
    {"travis-ci", os:getenv("TRAVIS_JOB_ID")};
get_ci_job_id(_, "true") ->
    {"circle-ci", os:getenv("CIRCLE_BUILD_NUM")}.
