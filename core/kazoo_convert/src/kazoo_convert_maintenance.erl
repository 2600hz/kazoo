%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_convert_maintenance).

-export([read_tiff_info/1]).
-export([read_metadata/1]).
-export([convert_audio_file/2, convert_audio_file/3, convert_audio_file/4]).
-export([convert_fax_file/2, convert_fax_file/3, convert_fax_file/4]).
-export([versions_in_use/0]).

-include("kz_audio_converter.hrl").
-include("kz_fax_converter.hrl").

-spec read_tiff_info(any()) -> 'ok'.
read_tiff_info(Filename) when is_binary(Filename) ->
    print_tiff_info(kz_fax_converter:get_tiff_info(Filename));
read_tiff_info(Filename) ->
    read_tiff_info(kz_term:to_binary(Filename)).

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
read_metadata(Filename) when not is_binary(Filename) ->
    read_metadata(kz_term:to_binary(Filename));
read_metadata(Filename) ->
    read_metadata(Filename, kz_mime:from_filename(Filename)).

-spec read_metadata(any(), kz_term:ne_binary()) -> 'ok'.
read_metadata(Filename, MimeType) when not is_binary(Filename) ->
    read_metadata(kz_term:to_binary(Filename), MimeType);
read_metadata(Filename, ?MP3_MIME) ->
    print_metadata(kz_audio_converter:read_metadata(Filename));
read_metadata(Filename, ?WAV_MIME) ->
    print_metadata(kz_audio_converter:read_metadata(Filename));
read_metadata(Filename, MimeType) when ?WAV_COMPATIBLE(MimeType) ->
    print_metadata(kz_audio_converter:read_metadata(Filename));
read_metadata(Filename, MimeType) ->
    print_metadata(kz_fax_converter:read_metadata(Filename, MimeType)).

-spec print_metadata(kz_term:proplist()) -> 'ok'.
print_metadata(Metadata) ->
    _ = lists:foreach(fun({Key, Value}) -> io:format("~s: ~p ~n", [Key, Value]) end, Metadata),
    'ok'.

-spec convert_audio_file(any(), any(), any(), any()) -> 'ok'.
convert_audio_file(FromFilename, ToFormat, WorkDir, ToFilename) ->
    Options = [{<<"tmp_dir">>, WorkDir}
              ,{<<"to_filename">>, ToFilename}
              ],
    do_convert(FromFilename
              ,ToFormat
              ,Options
              ).

-spec convert_audio_file(any(), any(), any()) -> 'ok'.
convert_audio_file(FromFilename, ToFormat, WorkDir) ->
    Options = [{<<"tmp_dir">>, WorkDir}],
    do_convert(FromFilename
              ,ToFormat
              ,Options
              ).

-spec convert_audio_file(any(), any()) -> 'ok'.
convert_audio_file(FromFilename, ToFormat) ->
    convert_audio_file(FromFilename, ToFormat, ?TMP_DIR).

-spec convert_fax_file(any(), any(), any(), any()) -> 'ok'.
convert_fax_file(FromFilename, ToFormat, WorkDir, ToFilename) ->
    Options = [{<<"tmp_dir">>, WorkDir}
              ,{<<"to_filename">>, ToFilename}
              ],
    do_convert(FromFilename
              ,ToFormat
              ,Options
              ).

-spec convert_fax_file(any(), any(), any()) -> 'ok'.
convert_fax_file(FromFilename, ToFormat, WorkDir) ->
    Options = [{<<"tmp_dir">>, WorkDir}],
    do_convert(FromFilename
              ,ToFormat
              ,Options
              ).

-spec convert_fax_file(any(), any()) -> 'ok'.
convert_fax_file(FromFilename, ToFormat) ->
    convert_fax_file(FromFilename, ToFormat, ?TMP_DIR).

-spec do_convert(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) ->
                        'ok'|'error'.
do_convert(FromFilename, ToFormat, Options) ->
    FromMime = kz_mime:from_filename(FromFilename),
    {'ok', Content} = file:read_file(FromFilename),
    ToMime = kz_mime:from_extension(ToFormat),
    case do_convert_mime(FromMime, ToMime, Content, Options) of
        { 'ok', OutputFile } ->
            io:format("Successfully converted ~s to ~s~n", [FromFilename, OutputFile]);
        { 'error', Msg } ->
            io:format("Failed to convert file ~s with error: ~s~n", [FromFilename, Msg])
    end.

-spec do_convert_mime(kz_term:ne_binary(), kz_term:ne_binary(), kz_convert:content(), kz_term:proplist()) ->
                        'ok'|'error'.
do_convert_mime(?MP3_MIME, ToMime, Content, Options) ->
    kz_convert:audio(?MP3_MIME, ToMime, Content, Options);
do_convert_mime(?WAV_MIME, ToMime, Content, Options) ->
    kz_convert:audio(?WAV_MIME, ToMime, Content, Options);
do_convert_mime(FromMime, ToMime, Content, Options) when ?WAV_COMPATIBLE(FromMime) ->
    kz_convert:audio(FromMime, ToMime, Content, Options);
do_convert_mime(FromMime, ToMime, Content, Options) ->
    kz_convert:fax(FromMime, ToMime, Content, Options).

-spec versions_in_use() -> 'no_return'.
versions_in_use() ->
    AllCmds =
        [?CONVERT_AUDIO_COMMAND
        ,?CONVERT_IMAGE_COMMAND
        ,?CONVERT_OPENOFFICE_COMMAND
        ,?CONVERT_PDF_COMMAND
        ,?CONVERT_TIFF_COMMAND
        ,?VALIDATE_AUDIO_COMMAND
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
