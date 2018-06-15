%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_convert_maintenance).

-export([read_metadata/1]).
-export([convert_fax_file/2, convert_fax_file/3]).
-export([versions_in_use/0]).

-include_lib("kazoo_convert/include/kz_convert.hrl").

-spec read_metadata(any()) -> 'ok'.
read_metadata(File) when is_binary(File) ->
    print_metadata(kz_fax_converter:read_metadata(File));
read_metadata(File) ->
    read_metadata(kz_term:to_binary(File)).

-spec print_metadata(kz_term:proplist()) -> 'ok'.
print_metadata(Metadata) ->
    _ = lists:foreach(fun({Key, Value}) -> io:format("~s ~p ~n", [Key, Value]) end, Metadata).

-spec convert_fax_file(any(), any(), any()) -> 'ok'.
convert_fax_file(FromFile, ToFormat, WorkDir)
  when is_binary(FromFile),
       is_binary(ToFormat),
       is_binary(WorkDir) ->
    Options = [{"tmp_dir", WorkDir}],
    {'ok', Content} = file:read_file(FromFile),
    Type = format_to_mimetype(ToFormat),
    do_convert(FromFile
              ,kz_mime:from_filename(FromFile)
              ,Type
              ,Content
              ,Options
              );
convert_fax_file(FromFile, ToFormat, WorkDir) ->
    convert_fax_file(kz_term:to_binary(FromFile)
                    ,kz_term:to_binary(ToFormat)
                    ,kz_term:to_binary(WorkDir)
                    ).
-spec convert_fax_file(any(), any()) -> 'ok'.
convert_fax_file(FromFile, ToFormat)
  when is_binary(FromFile),
       is_binary(ToFormat) ->
    convert_fax_file(FromFile, ToFormat, ?TMP_DIR);
convert_fax_file(FromFile, ToFormat) ->
    convert_fax_file(kz_term:to_binary(FromFile)
                    ,kz_term:to_binary(ToFormat)
                    ,?TMP_DIR
                    ).

-spec do_convert(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) ->
                        'ok'|'error'.
do_convert(FromFile, FromMime, ToMime, Content, Options) ->
    case kz_convert:fax(FromMime, ToMime, Content, Options) of
        { 'ok', OutputFile } ->
            io:format("Successfully converterd ~s to ~s~n", [FromFile, OutputFile]);
        { 'error', Msg } ->
            io:format("Failed to convert file ~s with error: ~s~n", [FromFile, Msg])
    end.


-spec format_to_mimetype(kz_term:ne_binary()) ->
                                kz_term:ne_binary().
format_to_mimetype(<<"TIFF">>) -> ?TIFF_MIME;
format_to_mimetype(<<"tiff">>) -> ?TIFF_MIME;
format_to_mimetype(<<"pdf">>) -> ?PDF_MIME;
format_to_mimetype(Format) ->
    Format.

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
