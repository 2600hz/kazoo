%% Copyright © 2016 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(kzos).

%% kzos: 

-export([cmd/3]).

%% API

%% @public
-spec cmd(file:filename_all(), [nonempty_string(),...], timeout()) ->
                 {non_neg_integer(), [string()]} |
                 {'error', 'command_not_found' | 'timeout'}.
cmd(Dir, [Exe|Args]=_Cmd, Timeout) ->
						%lager:debug("$ cd ~s; `~s` (~p)", [Dir, _Cmd, Timeout]),
    case os:find_executable(Exe) of
        'false' ->
						%lager:error("Command ~s is not installed on this system", [Exe]),
            {'error', 'command_not_found'};
        Executable ->
            Port = open_port({'spawn_executable', Executable}
                            ,['exit_status'
                             ,'use_stdio'
                             ,'stderr_to_stdout'
                             ,{'args', Args}
                             ,{'cd', Dir}
                             ,{'line', 64 * 64}
                             ]),
            cmd_loop(Port, Timeout, [])
    end.

%% Internals

%% @private
-spec cmd_loop(port(), timeout(), [string()]) -> {non_neg_integer(), [string()]} |
                                                 {'error', 'timeout'}.
cmd_loop(Port, Timeout, Acc) ->
    receive
        {Port, {'data', {'eol', Line}}} ->
            cmd_loop(Port, Timeout, [Line|Acc]);
        {Port, {'exit_status', Code}} ->
            {Code, lists:reverse(Acc)}
    after Timeout ->
            {'error', 'timeout'}
    end.

%% End of Module.
