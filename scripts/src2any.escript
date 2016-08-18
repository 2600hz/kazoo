#!/usr/bin/env escript
%%! -sname kazoo_src2any
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

-define(EXT, ".src").

%% API

main([ScriptName]) ->
    not lists:suffix(?EXT, ScriptName) andalso usage(),

    BinScriptName = iolist_to_binary(ScriptName),
    BEAMShouldBeDoingThis_LetsPatchThisAlready = byte_size(BinScriptName) - length(?EXT),
    <<TargetName:BEAMShouldBeDoingThis_LetsPatchThisAlready/binary, ?EXT>> = BinScriptName,

    {ok, Terms} = file:script(ScriptName),
    Data = [io_lib:format("~100000p.\n", [Term]) || Term <- Terms],
    ok = file:write_file(TargetName, Data);

main(_) ->
    usage().

usage() ->
    io:format(standard_error, "Usage:\n\t$0  <path to a ~s file>\n", [?EXT]),
    halt(1).
