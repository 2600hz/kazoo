#!/usr/bin/env escript
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

main(_) ->
    case raw_json_usage:process_project() of
        [] -> 'ok';
        BadModules ->
            [output_bad_module(M, Ls) || {M, Ls} <- BadModules],
            erlang:halt(1)
    end.

output_bad_module(M, [L]) ->
    io:format("module ~p has raw JSON on line ~p~n", [M, L]);
output_bad_module(M, Ls) ->
    Lines = kz_util:join_binary(Ls),
    io:format("module ~p has raw JSON on lines ~s~n", [M, Lines]).
