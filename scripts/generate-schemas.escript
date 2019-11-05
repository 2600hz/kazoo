#!/usr/bin/env escript
%%! +A0
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").

main(_) ->
    lists:foreach(fun run_generator/1
                 ,[fun cf_data_usage:to_schema_docs/0
                  ,fun kp_data_usage:to_schema_docs/0
                  ,fun kapps_config_usage:to_schema_docs/0
                  ,fun conference_schema_builder:to_schema/0
                  ,fun kapi_schemas:to_schemas/0
                  ,fun maintenance_docs:to_docs/0
                  ]
                 ).

run_generator(F) ->
    try F()
    catch
        ?STACKTRACE('throw', 'no_type', ST)
        [{M, _F, _A, Props}|_] = ST,
        CompileOpts = M:module_info(compile),
        SrcModule = props:get_value(source, CompileOpts),
        Line = props:get_value('line', Props),
        io:format("~s:~p: no type found when running ~p~n", [SrcModule, Line, F]),
        [io:format("~p~n", [S]) || S <- ST],
        exit(1)
        end.
