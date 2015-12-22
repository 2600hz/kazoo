#!/usr/bin/env escript
%%! -sname kazoo_script2any
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

%% API

main([ScriptName]) ->
    case lists:suffix(".script", ScriptName) of
        false ->
            io:format(standard_error, "Filename must end with '.script'\n", []),
            halt(2);
        true -> ok
    end,

    BinScriptName = iolist_to_binary(ScriptName),
    BEAMShouldBeDoingThis_LetsPatchThisAlready = byte_size(BinScriptName) - length(".script"),
    <<TargetName:BEAMShouldBeDoingThis_LetsPatchThisAlready/binary, ".script">> = BinScriptName,

    {ok, Terms} = file:script(ScriptName),
    Data = [io_lib:format("~100000p.\n", [Term]) || Term <- Terms],
    ok = file:write_file(TargetName, Data);

main(_) ->
    io:format("Usage:\n\t$0  <path to a .script file>\n"),
    halt(1).
