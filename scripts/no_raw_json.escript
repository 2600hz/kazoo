#!/usr/bin/env escript
%%! +A1
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

main([]) ->
    io:format("checking raw JSON {[]} usage: "),
    ModulesWithRawJSON = raw_json_usage:process_project(),
    handle_potential_usage(ModulesWithRawJSON);
main(Files) ->
    io:format("checking raw JSON {[]} usage: "),
    ModulesWithRawJSON = lists:foldl(fun process_file/2, [], Files),
    handle_potential_usage(ModulesWithRawJSON).

process_file(File, Acc) ->
    process_file(File, Acc, filename:extension(kz_term:to_binary(File))).

process_file(File, Acc, <<".erl">>) ->
    ModuleName = filename:basename(File, ".erl"),
    Acc ++ raw_json_usage:process_module(kz_term:to_atom(ModuleName, 'true'));
process_file(_File, Acc, _Ext) -> Acc.

handle_potential_usage(ModulesWithRawJSON) ->
    ExitCode = lists:foldl(fun handle_potential_usage/2
                          ,0
                          ,ModulesWithRawJSON
                          ),
    io:format("~n"),
    erlang:halt(ExitCode).

handle_potential_usage({Module, Lines}, ExitCode) ->
    IODevice = find_source(Module),
    ExitWith =
        case lists:foldl(fun using_json_macro/2, {Module, IODevice, 1, 0}, Lines) of
            {Module, IODevice, _CurrentLine, 0} ->
                ExitCode;
            {Module, IODevice, _CurrentLine, _RawLines} ->
                1
        end,
    file:close(IODevice),
    ExitWith.

using_json_macro(Line, {Module, IODevice, CurrentLine, RawLines}) when CurrentLine < Line ->
    {'ok', _Data} = file:read_line(IODevice),
    using_json_macro(Line, {Module, IODevice, CurrentLine+1, RawLines});
using_json_macro(Line, {Module, IODevice, Line, RawLines}) ->
    {'ok', LineData} = file:read_line(IODevice),

    case has_raw_empty_json(Module, Line, RawLines, LineData)
        orelse has_raw_json_object(Module, Line, RawLines, LineData)
    of
        'true' -> {Module, IODevice, Line+1, RawLines+1};
        'false' -> {Module, IODevice, Line+1, RawLines}
    end.

has_raw_empty_json(Module, Line, RawLines, LineData) ->
    case re:run(LineData, empty_json_regex(), [{'capture', 'all', 'binary'}]) of
        'nomatch' -> 'false';
        {'match', Matches} ->
            output_raw_matches(Module, Line, RawLines, LineData, Matches),
            'true'
    end.

has_raw_json_object(Module, Line, RawLines, LineData) ->
    case re:run(LineData, json_object_regex(), [{'capture', 'all', 'binary'}]) of
        'nomatch' -> 'false';
        {'match', Matches} ->
            output_raw_matches(Module, Line, RawLines, LineData, Matches),
            'true'
    end.

empty_json_regex() ->
    EmptyBin = iolist_to_binary(io_lib:format("~w", [kz_json:new()])),
    << <<"\\", C>> || <<C>> <= EmptyBin >>.

json_object_regex() ->
    ObjectList = lists:flatten(io_lib:format("~w", [kz_json:from_list([{<<".+">>, <<".+">>}])])),
    json_object_regex(ObjectList, []).

json_object_regex([], Regex) -> iolist_to_binary(lists:reverse(Regex));
json_object_regex("<<" ++ Rest, Regex) ->
    {Pattern, RestOfRest} = extract_binary_pattern(Rest),
    json_object_regex(RestOfRest, [Pattern | Regex]);
json_object_regex([C | Rest], Regex) ->
    json_object_regex(Rest, [<<"\\", C>> | Regex]).

extract_binary_pattern(ObjectChunk) ->
    extract_binary_pattern(ObjectChunk, [], []).

extract_binary_pattern(">>" ++ Rest, Chars, Pattern) ->
    {lists:reverse([list_to_integer(lists:reverse(Chars)) | Pattern]), Rest};
extract_binary_pattern("," ++ Rest, Chars, Pattern) ->
    extract_binary_pattern(Rest, [], [list_to_integer(lists:reverse(Chars)) | Pattern]);
extract_binary_pattern([C | Rest], Chars, Pattern) ->
    extract_binary_pattern(Rest, [C | Chars], Pattern).

find_source(Module) ->
    find_source(Module, code:which(Module)).

find_source(Module, BeamFile) when is_list(BeamFile) ->
    AppDir = filename:dirname(filename:dirname(BeamFile)),
    SrcFile = find_source_file(Module, AppDir),

    case file:open(SrcFile, ['read', 'binary', 'raw', 'read_ahead']) of
        {'ok', IODevice} -> IODevice;
        {'error', 'enoent'} ->
            io:format("failed to find module ~s source file ~s in ~s for beam ~s~n", [Module, SrcFile, AppDir, BeamFile]),
            throw({'error', 'enoent'})
    end.

find_source_file(Module, AppDir) ->
    SrcSearch = filename:join([AppDir, "*", [kz_term:to_list(Module), ".erl"]]),
    case filelib:wildcard(SrcSearch) of
        [] -> find_source_sub_dir(Module, AppDir);
        [SrcFile] -> SrcFile
    end.

find_source_sub_dir(Module, AppDir) ->
    SrcSearch = filename:join([AppDir, "*", "*", [kz_term:to_list(Module), ".erl"]]),
    case filelib:wildcard(SrcSearch) of
        [] -> 'undefined';
        [SrcFile] -> SrcFile
    end.

output_raw_matches(Module, Line, 0, LineData, Matches) ->
    File = props:get_value('source', Module:module_info('compile'), Module),
    Format = "~s:~p: ~s~n",
    output(File, Line, Matches, LineData, Format);
output_raw_matches(Module, Line, _RawLines, LineData, Matches) ->
    MLen = integer_to_list(length(atom_to_list(Module))),
    Format = "~" ++ MLen ++ "s ~p : ~s~n",
    output("", Line, Matches, LineData, Format).

output(Module, Line, Match, LineData, Format) when not is_list(Match) ->
    output(Module, Line, [Match], LineData, Format);
output(Module, Line, Matches, LineData, Format) when is_list(LineData) ->
    output(Module, Line, Matches, list_to_binary(LineData), Format);
output(Module, Line, Matches, LineData, Format) ->
    ColorizedLineData = lists:foldl(fun colorize_match/2, LineData, Matches),
    io:format(Format, [Module, Line, ColorizedLineData]).

colorize_match(Match, LineData) ->
    WithoutLF = binary:part(LineData, 0, size(LineData)-1),
    binary:replace(WithoutLF, Match, <<"\e[31m", Match/binary, "\e[0m">>, ['global']).
