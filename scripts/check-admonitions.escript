#!/usr/bin/env escript
%%! +A0 -sname kazoo_admonitions
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

%% API

main(Files) ->
    process_files([File || File <- Files, filename:extension(File) =:= ".md"]).

process_files([]) ->
    print_help();
process_files(MDs) ->
    {Halt, Files} = lists:foldl(fun process_file/2, {0, []}, MDs),
    print_report(Halt, Files).

print_report(Halt, []) ->
    halt(Halt);
print_report(Halt, Files) ->
    io:format("fixed admonitions in the following files:~n"),
    _ = [io:format(" - ~s~n", [F]) || F <- Files],
    halt(Halt).

process_file(MD, Acc) ->
    {'ok', Contents} = file:read_file(MD),
    process_file(MD, Acc, Contents, re:run(Contents, "!!! \\w+\n\\w+")).

process_file(_MD, {Changed, Files}, _Contents, 'nomatch') ->
    {Changed, Files};
process_file(MD, {Changed, Files}, Contents, {'match', _Matches}) ->
    Updated = re:replace(Contents, "(!!! \\w+\n)(\\w+)", "\\1    \\2", ['global']),
    'ok' = file:write_file(MD, Updated),
    {Changed+1, [MD | Files]}.

-spec print_help() -> no_return().
print_help() ->
    Script = escript:script_name(),
    io:format("Usage: ERL_LIBS=deps:core:applications ~s file.md [file.md]+~n", [Script]),
    halt().
