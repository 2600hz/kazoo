#!/usr/bin/env escript
%%! -sname kazoo_sup_build_autocomplete
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

%% API

main([]) ->
    usage(),
    halt(255);
main(Paths) ->
    Files = lists:flatmap(fun find_modules/1, Paths),
    [print(find(File)) || File <- Files].

%% Internals

print([]) -> 'ok';
print([{M,F,_A,Vs}|Rest]) ->
    Vars = commas(Vs),
    io:format("~s:~s(~s)\n", [M, F, Vars]),
    print(Rest);
print(_R) ->
    io:format("_R = ~p\n", [_R]).


commas([]) -> "";
commas([Var]) -> atom_to_list(Var);
commas([Var|Rest]) ->
    [atom_to_list(Var), ", ", commas(Rest)].

find(File) ->
    {ok, {Module, [{exports, FAs0}]}} = beam_lib:chunks(File, [exports]),
    FAs = [{F,A} || {F,A} <- FAs0, F =/= module_info],
    {ok, {Module, [{abstract_code, AST}]}} = beam_lib:chunks(File, [abstract_code]),
    %% io:format("AST = ~p\n", [AST]),
    {raw_abstract_v1, Ts} = AST,
    Clauses = [{F,A,Cs} || {function,_,F,A,Cs} <- Ts, lists:member({F,A}, FAs)],
    %% io:format("Clauses = ~p\n", [Clauses]),
    Ps = lists:flatmap(
           fun ({F,A,Cs}) ->
                   %% io:format("F ~p A ~p\n", [F,A]),
                   %% Clauses = [Clauses || {function,_,F,A,Clauses} <- Ts],
                   %% io:format("Clauses = ~p\n", [Clauses]),
                   R = [ [V || {var,_,V} <- Vars]
                        || {clause,_,Vars,_,_} <- Cs],
                   %% io:format("R = ~p\n", [R]),
                   [{Module,F,A,Vs} || Vs <- R]
           end, Clauses),
    Ps.

find_modules(Path) ->
    case filelib:is_dir(Path) of
        'false' -> [];
        'true' ->
            AccFiles = fun (File, Acc) -> [File|Acc] end,
            filelib:fold_files(Path, ".+_maintenance\\.beam", true, AccFiles, [])
    end.

usage() ->
    %% ok = io:setopts([{encoding, unicode}]),
    Arg0 = escript:script_name(),
    io:format("Usage: ~s  <path to dir>+\n", [filename:basename(Arg0)]).

%% End of Module
