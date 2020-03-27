#!/usr/bin/env escript
%%! +A0
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

main(Args) ->
    lists:foreach(fun run_generator/1
                 ,[{fun cf_data_usage:to_schema_docs/1
                   ,[kz_term:to_atom(M, 'true') || Arg <- Args, M <- [module_name(Arg)], M =/= 'undefined']
                   }
                  ,fun kp_data_usage:to_schema_docs/0
                  ,fun kapps_config_usage:to_schema_docs/0
                  ,fun conference_schema_builder:to_schema/0
                  ,fun kapi_schemas:to_schemas/0
                  ]
                 ).

run_generator({F, Args}) ->
    try F(Args)
    catch
        'throw':'no_type' ->
            ST = erlang:get_stacktrace(),
            handle_stacktrace(F, ST)
    end;
run_generator(F) ->
    try F()
    catch
        'throw':'no_type' ->
            ST = erlang:get_stacktrace(),
            handle_stacktrace(F, ST)
    end.

handle_stacktrace(F, [{M, _Fun, _A, Props}|_] = ST) ->
    CompileOpts = M:module_info('compile'),
    SrcModule = props:get_value('source', CompileOpts),
    Line = props:get_value('line', Props),
    io:format("~s:~p: no type found when running ~p~n", [SrcModule, Line, F]),
    [io:format("~p~n", [S]) || S <- ST],
    exit(1).

module_name(Arg) ->
    module_name(Arg, filename:extension(Arg)).

module_name(Arg, ".erl") -> filename:basename(Arg, ".erl");
module_name(_Arg, _Ext) -> 'undefined'.
