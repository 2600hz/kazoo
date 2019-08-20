%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_convert_maintenance).

-export([read_tiff_info/1]).
-export([read_metadata/1]).
-export([convert_fax_file/2, convert_fax_file/3, convert_fax_file/4]).
-export([versions_in_use/0]).

-include("kz_fax_converter.hrl").

-spec read_tiff_info(any()) -> 'ok'.
read_tiff_info(File) when is_binary(File) ->
    print_tiff_info(kz_fax_converter:get_tiff_info(File));
read_tiff_info(File) ->
    read_tiff_info(kz_term:to_binary(File)).

-spec print_tiff_info(map() | {kz_term:ne_binary(), any(), maps:iterator()}) -> 'ok'.
print_tiff_info({'error', Reason, Message}) ->
    io:format("Command failed with ~p error: ~s ~n", [Reason, Message]);
print_tiff_info(Metadata) ->
    Func = fun (Key, Value, _Acc) when is_binary(Value) ->
                   io:format("~s: ~s ~n", [Key, Value]);
               (Key, Value, _Acc) ->
                   io:format("~s: ~p ~n", [Key, Value])
           end,
    maps:fold(Func, 0, Metadata).

-spec read_metadata(any()) -> 'ok'.
read_metadata(File) when is_binary(File) ->
    print_metadata(kz_fax_converter:read_metadata(File));
read_metadata(File) ->
    read_metadata(kz_term:to_binary(File)).

-spec print_metadata(kz_term:proplist()) -> 'ok'.
print_metadata(Metadata) ->
    _ = lists:foreach(fun({Key, Value}) -> io:format("~s: ~p ~n", [Key, Value]) end, Metadata),
    'ok'.

-spec convert_fax_file(any(), any(), any(), any()) -> 'ok'.
convert_fax_file(FromFile, ToFormat, WorkDir, ToFilename) ->
    Options = [{<<"tmp_dir">>, WorkDir}
              ,{<<"to_filename">>, ToFilename}
              ],
    do_convert(FromFile
              ,ToFormat
              ,Options
              ).

-spec convert_fax_file(any(), any(), any()) -> 'ok'.
convert_fax_file(FromFile, ToFormat, WorkDir) ->
    Options = [{<<"tmp_dir">>, WorkDir}],
    do_convert(FromFile
              ,ToFormat
              ,Options
              ).

-spec convert_fax_file(any(), any()) -> 'ok'.
convert_fax_file(FromFile, ToFormat) ->
    convert_fax_file(FromFile, ToFormat, ?TMP_DIR).

-spec do_convert(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) ->
                        'ok'|'error'.
do_convert(FromFile, ToFormat, Options) ->
    FromMime = kz_mime:from_filename(FromFile),
    {'ok', Content} = file:read_file(FromFile),
    ToMime = kz_mime:from_extension(ToFormat),
    case kz_convert:fax(FromMime, ToMime, Content, Options) of
        { 'ok', OutputFile } ->
            io:format("Successfully converted ~s to ~s~n", [FromFile, OutputFile]);
        { 'error', Msg } ->
            io:format("Failed to convert file ~s with error: ~s~n", [FromFile, Msg])
    end.

-spec versions_in_use() -> 'no_return'.
versions_in_use() ->
    AllCmds =
        [?CONVERT_IMAGE_COMMAND
        ,?CONVERT_OPENOFFICE_COMMAND
        ,?CONVERT_PDF_COMMAND
        ,?CONVERT_TIFF_COMMAND
        ,?VALIDATE_PDF_COMMAND
        ],
    Executables = find_commands(AllCmds),
    lists:foreach(fun print_cmd_version/1, Executables),
    'no_return'.

print_cmd_version(Exe) ->
    Options = ['exit_status'
              ,'use_stdio'
              ,'stderr_to_stdout'
              ,{'args', ["--version"]}
              ],
    Port = open_port({'spawn_executable', Exe}, Options),
    listen_to_port(Port, Exe).

listen_to_port(Port, Exe) ->
    receive
        {Port, {'data', Str0}} ->
            [Str|_] = string:tokens(Str0, "\n"),
            io:format("* ~s:\n\t~s\n", [Exe, Str]),
            lager:debug("version for ~s: ~s", [Exe, Str]);
        {Port, {'exit_status', 0}} -> 'ok';
        {Port, {'exit_status', _}} -> print_no_executable(Exe)
    end.

find_commands(Cmds) ->
    Commands =
        lists:usort(
          [binary_to_list(hd(binary:split(Cmd, <<$\s>>)))
           || Cmd <- Cmds
          ]),
    lists:usort(
      [Exe
       || Cmd <- Commands,
          Exe <- [cmd_to_executable(Cmd)],
          Exe =/= 'false'
      ]).

print_no_executable(Exe) ->
    io:format("* ~s:\n\tERROR! missing executable\n", [Exe]),
    lager:error("missing executable: ~s", [Exe]).

cmd_to_executable("/"++_=Exe) -> Exe;
cmd_to_executable(Cmd) ->
    case os:find_executable(Cmd) of
        'false' ->
            print_no_executable(Cmd),
            'false';
        Exe -> Exe
    end.
