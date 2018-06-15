%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_fax_converter).

-behaviour(gen_kz_converter).

-export([convert/4
        ,do_openoffice_to_pdf/2
        ,read_metadata/1
        ]).

-include_lib("kazoo_convert/include/kz_convert.hrl").

-type fax_converted() :: {'ok', any()}|
                         {'error', any()}.

-type fax_convert_funs() :: [fun((kz_term:ne_binary(), map()) -> fax_converted())].

%%------------------------------------------------------------------------------
%% @doc Converts the data or file specified in `Content' from the `To' mime-type to the
%% `From' mime-type.
%%
%% Arguments Description:
%% <ul>
%%   <li><strong>From:</strong> is a mime-type binary that specifies the format of
%%   the Content passed in to convert.</li>
%%   <li><strong>To:</strong> is a mime-type binary that specifies the format the
%%   Content is to be converted.</li>
%%   <li><strong>Content:</strong> content can be filepath to the source file or
%%   a binary containing the contents of the file to be converted.</li>
%%   <li><strong>Options:</strong> a proplist of the converter options</li>
%% </ul>
%%
%% Options:
%% <ul>
%%   <li><strong>job_id:</strong> the unique ID of the job (like a fax job_id).
%%   Used for naming the output file with the extension derived from the `To' format</li>
%%   <li><strong>output_type:</strong> return the converted doc as a raw `binary' containing
%%   the contents of the file or `path' to receive a path to the converted file in the response.
%%   The default is `path'.</li>
%%   <li><strong>tmp_dir:</strong> the working directory where the conversion will take place.</li>
%%   <li><strong>return_metadata:</strong>Include a third option in the output tuple which is a Proplist of metadata about the file.</li>
%%   <li><strong>to_filename:</strong>The user requested destination file name for the converted file, if a full path is provided this will
%%   be copied to the specified path, if a relative path is specified, it will be copied to the `tmp_dir' using the file name specified</li>
%% </ul>
%%
%% read_metadata:
%% <ul>
%%   <li><strong>page_count:</strong> the count of pages in a tiff.</li>
%%   <li><strong>size:</strong> the file size in bytes.</li>
%%   <li><strong>mime-type:</strong> the files mime-type</li>
%%   <li><strong>filetype:</strong> the files extension</li>
%% </ul>
%%
%% Replacement Modules:
%% <ul>
%%   <li>The behavior `gen_kz_converter' specifies the interface of the function convert/4  and read_metadata/1, and return the existing format.</li>
%%   <li>The converter (and any alternative modules) should always delete any files created in the process,
%%   including the input file if the {'file', FilePath} `Content' format is specified. If `output_type' is `path' the file converted file will be returned and deletion of this file will be
%%   the responsibility of the caller. </li>
%%   <li>`binary' and `path' formats in the requested `output_type' must be supported.</li>
%%   <li>Input content formats `{file, FilePath}' and a binary containing the files content must be supported.</li>
%%   <li>Any files created in the process should be stored in the specified tmp_dir or `/tmp' by default.</li>
%%   <li>The user specified filename is respected</li>
%%</ul>
%%
%% @end
%%------------------------------------------------------------------------------
-spec convert(kz_term:ne_binary(), kz_term:ne_binary(), binary()|{'file', kz_term:ne_binary()}, kz_term:proplist()) ->
                     gen_kz_converter:converted().
convert(From, To, Content, Opts) ->
    Options = maps:from_list(
                [{<<"from_format">>, From}
                ,{<<"to_format">>, To}
                ,{<<"job_id">>, props:get_value(<<"job_id">>, Opts, kz_binary:rand_hex(12))}
                 | props:delete_keys([<<"job_id">>], Opts)
                ]),
    Filename = save_file(Content, Options),
    lager:info("converting document ~s from ~s to ~s", [Filename, From, To]),
    case run_convert(eval_format(From, To), To, Filename, Options) of
        {'ok', _}=Ok ->
            lager:info("succesfully converted file: ~s to format: ~s", [Filename, To]),
            Ok;
        {'ok', _, _}=Ok ->
            lager:info("succesfully converted file: ~s to format: ~s", [Filename, To]),
            Ok;
        {'error', Message}=Error ->
            lager:error("conversion failed with error: ~p", [Message]),
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc Collects the fax related metadata from a file
%%
%% Properties returned:
%% <ul>
%%   <li><strong>page_count:</strong> the count of pages if file is a tiff.</li>
%%   <li><strong>size:</strong> the file size in bytes.</li>
%%   <li><strong>mime-type:</strong> the files mime-type</li>
%%   <li><strong>filetype:</strong> the files extension</li>
%% </ul>
%% @end
%%------------------------------------------------------------------------------
-spec read_metadata(kz_term:ne_binary()) -> kz_term:proplist().
read_metadata(Filename) ->
    read_metadata(Filename, kz_mime:from_filename(Filename)).

%%%=============================================================================
%%% conversion functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec eval_format(kz_term:ne_binary(), kz_term:ne_binary()) -> fax_convert_funs() | {'error', kz_term:ne_binary()}.
eval_format(<<"image/", _SubType/binary>>, ?TIFF_MIME) ->
    [fun image_to_tiff/2
    ];
eval_format(Format, Format) ->
    [];
eval_format(?TIFF_MIME, ?PDF_MIME) ->
    [fun tiff_to_pdf/2
    ];
eval_format(?PDF_MIME, ?TIFF_MIME) ->
    [fun pdf_to_tiff/2
    ];
eval_format(<<?OPENXML_MIME_PREFIX, _/binary>>, ?TIFF_MIME) ->
    [fun openoffice_to_pdf/2
    ,fun pdf_to_tiff/2
    ];
eval_format(CT, ?TIFF_MIME) when ?OPENOFFICE_COMPATIBLE(CT) ->
    [fun openoffice_to_pdf/2
    ,fun pdf_to_tiff/2
    ];
eval_format(<<?OPENOFFICE_MIME_PREFIX, _/binary>>, ?TIFF_MIME) ->
    [fun openoffice_to_pdf/2
    ,fun pdf_to_tiff/2
    ];
eval_format(<<?OPENXML_MIME_PREFIX, _/binary>>, ?PDF_MIME) ->
    [fun openoffice_to_pdf/2
    ];
eval_format(<<?OPENOFFICE_MIME_PREFIX, _/binary>>, ?PDF_MIME) ->
    [fun openoffice_to_pdf/2
    ];
eval_format(CT, ?PDF_MIME) when ?OPENOFFICE_COMPATIBLE(CT) ->
    [fun openoffice_to_pdf/2
    ];
eval_format(FromFormat, ToFormat) ->
    {'error', <<"invalid conversion requested: ", FromFormat/binary, " to: ", ToFormat/binary>>}.

-spec run_convert({'error', kz_term:ne_binary()} | fax_convert_funs()
                 ,kz_term:ne_binary()
                 ,kz_term:ne_binary()
                 ,map()) -> gen_kz_converter:converted().
run_convert({'error', _}=Error, _ToFormat, _FilePath, _Options) ->
    Error;
run_convert([Operation|Operations], ToFormat, FilePath, Options) ->
    case Operation(FilePath, Options) of
        {'ok', OutputPath} ->
            maybe_delete_previous_file(FilePath, OutputPath),
            run_convert(Operations, ToFormat, OutputPath, Options);
        Error -> Error
    end;
run_convert([], ToFormat, FilePath, Options) ->
    case validate_output(ToFormat, FilePath, Options) of
        {'ok', _} ->
            format_response(ToFormat, FilePath, Options);
        Error -> Error
    end.

-spec image_to_tiff(kz_term:ne_binary(), map()) -> fax_converted().
image_to_tiff(FromPath, Options) ->
    convert_file(?CONVERT_IMAGE_COMMAND, FromPath, <<".tiff">>, Options).

-spec tiff_to_pdf(kz_term:ne_binary(), map()) -> fax_converted().
tiff_to_pdf(FromPath, Options) ->
    convert_file(?CONVERT_TIFF_COMMAND, FromPath, <<".pdf">>, Options).

-spec pdf_to_tiff(kz_term:ne_binary(), map()) -> fax_converted().
pdf_to_tiff(FromPath, Options) ->
    convert_file(?CONVERT_PDF_COMMAND, FromPath, <<".tiff">>, Options).

-spec openoffice_to_pdf(kz_term:ne_binary(), map()) -> fax_converted().
openoffice_to_pdf(FromPath, Options) ->
    case ?ENABLE_OPENOFFICE of
        'true' ->
            case ?SERIALIZE_OPENOFFICE of
                'true' -> kz_openoffice_server:add(FromPath, Options);
                'false' -> do_openoffice_to_pdf(FromPath, Options)
            end;
        'false' ->
            {'error', <<"openoffice compatible conversion is unsupported">>}
    end.

-spec do_openoffice_to_pdf(kz_term:ne_binary(), map()) -> fax_converted().
do_openoffice_to_pdf(FromPath, Options) ->
    convert_file(?CONVERT_OPENOFFICE_COMMAND, FromPath, <<".pdf">>, Options).

-spec convert_file(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), map()) -> fax_converted().
convert_file(Command, FromPath, Ext, #{<<"job_id">> := JobId, <<"tmp_dir">> := TmpDir}) ->
    ToPath = filename:join(TmpDir, <<JobId/binary, Ext/binary>>),
    BatchPath = filename:join(TmpDir, <<(filename:rootname(filename:basename(FromPath)))/binary, Ext/binary>>),
    case run_convert_command(Command, FromPath, ToPath, TmpDir) of
        {'ok', _} -> maybe_rename_file(BatchPath, ToPath);
        Else -> Else
    end.

-spec run_convert_command(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                                 fax_converted().
run_convert_command(Command, FromPath, ToPath, TmpDir) ->
    lager:debug("converting file with command: ~s", [Command]),
    Args = [{<<"FROM">>, FromPath}
           ,{<<"TO">>, ToPath}
           ,{<<"WORKDIR">>, TmpDir}
           ],
    Options = [{<<"timeout">>, ?CONVERT_TIMEOUT}
              ,{<<"absolute_timeout">>, ?CONVERT_TIMEOUT}
              ],
    case kz_os:cmd(Command, Args, Options) of
        {'ok', _} ->
            lager:debug("successfully converted file ~s", [FromPath]),
            {'ok', ToPath};
        {'error', Reason, Msg} ->
            lager:debug("failed to convert file with reason: ~p, output: ~p", [Reason, Msg]),
            _ = file:delete(ToPath),
            {'error', <<"convert command failed">>}
    end.

%%%=============================================================================
%%% validate functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_output(kz_term:ne_binary(), kz_term:ne_binary(), map()) ->
                             fax_converted().
validate_output(?TIFF_MIME, Filename, #{<<"tmp_dir">> := TmpDir}) ->
    OutputFile = filename:join(TmpDir, <<(kz_binary:rand_hex(16))/binary, ".pdf">>),
    run_validate_command(?VALIDATE_TIFF_COMMAND, Filename, OutputFile, TmpDir);
validate_output(?PDF_MIME, Filename, #{<<"tmp_dir">> := TmpDir}) ->
    OutputFile = filename:join(TmpDir, <<(kz_binary:rand_hex(16))/binary, ".pdf">>),
    run_validate_command(?VALIDATE_PDF_COMMAND, Filename, OutputFile, TmpDir);
validate_output(Mime, _FilePath, _Options) ->
    {'ok', <<"unsupported mime type", Mime/binary>>}.

-spec run_validate_command(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                                  fax_converted().
run_validate_command(Command, FromPath, ToPath, TmpDir) ->
    lager:debug("validating file with command: ~s", [Command]),
    Args = [{<<"FROM">>, FromPath}
           ,{<<"TO">>, ToPath}
           ,{<<"WORKDIR">>, TmpDir}
           ,{<<"FILE">>, FromPath}
           ],
    Options = [{<<"timeout">>, ?CONVERT_TIMEOUT}
              ,{<<"absolute_timeout">>, ?CONVERT_TIMEOUT}
              ],
    case kz_os:cmd(Command, Args, Options) of
        {'ok', _}=Ok ->
            kz_util:delete_file(ToPath),
            Ok;
        {'error', Reason, Msg} ->
            lager:debug("failed to validate file: ~s with reason: ~s error: ~p", [FromPath, Reason, Msg]),
            _ = file:delete(ToPath),
            kz_util:delete_file(FromPath),
            {'error', <<"file validation failed">>}
    end.

%%%=============================================================================
%%% return handling functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec format_response(kz_term:ne_binary(), kz_term:ne_binary(), map()) ->
                             gen_kz_converter:converted().
format_response(ToFormat, FilePath, #{<<"output_type">> := 'binary'}=Options) ->
    Metadata = maybe_read_metadata(ToFormat, FilePath, Options),
    case format_output(FilePath, Options) of
        {'ok', Content} when Metadata =/= [] ->
            {'ok', Content, Metadata};
        {'ok', _}=Ok -> Ok;
        Error -> Error
    end;
format_response(ToFormat, FilePath, Options) ->
    case maybe_user_filename(FilePath, Options) of
        {'ok', NewPath} ->
            Metadata = maybe_read_metadata(ToFormat, NewPath, Options),
            case format_output(NewPath, Options) of
                {'ok', Content} when Metadata =/= [] ->
                    {'ok', Content, Metadata};
                {'ok', _}=Ok -> Ok;
                Error -> Error
            end;
        Error -> Error
    end.

-spec maybe_user_filename(kz_term:ne_binary(), map()) ->
                                 {'ok', kz_term:ne_binary()} |
                                 {'error', kz_term:ne_binary()}.
maybe_user_filename(FilePath, #{<<"to_filename">> := UserPath, <<"tmp_dir">> := TmpDir}) ->
    case filename:pathtype(UserPath) of
        'absolute' ->
            rename_file(FilePath, UserPath);
        'relative' ->
            rename_file(FilePath, filename:join(TmpDir, UserPath));
        _ ->
            {'error', <<"invalid filename ", UserPath/binary>>}
    end;
maybe_user_filename(FilePath, _Options) ->
    {'ok', FilePath}.

-spec format_output(kz_term:ne_binary(), map()) ->
                           {'ok', binary()} |
                           {'error', kz_term:ne_binary()}.
format_output(FilePath, #{<<"output_type">> := 'binary'}) ->
    case file:read_file(FilePath) of
        {'ok', _}=Ok ->
            kz_util:delete_file(FilePath),
            Ok;
        {'error', Reason} ->
            lager:debug("failed to format output file with reason ~p", [Reason]),
            {'error', <<"failed to format output file">>}
    end;
format_output(FilePath, #{<<"output_type">> := 'path'}) ->
    {'ok', FilePath};
format_output(FilePath, _Options) ->
    {'ok', FilePath}.

%%%=============================================================================
%%% metadata functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_read_metadata(kz_term:ne_binary(), kz_term:ne_binary(), map()) -> kz_term:proplist().
maybe_read_metadata(MimeType, FilePath, #{<<"read_metadata">> := 'true'}) ->
    read_metadata(FilePath, MimeType);
maybe_read_metadata(_, _, _) ->
    [].

-spec read_metadata(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:proplist().
read_metadata(Filename, MimeType) ->
    [{<<"page_count">>, count_pages_command(MimeType, Filename)}
    ,{<<"size">>, filelib:file_size(kz_term:to_list(Filename))}
    ,{<<"mimetype">>, MimeType}
    ,{<<"filetype">>, filetype_from_filename(Filename)}
    ].

-spec count_pages_command(kz_term:ne_binary(), kz_term:ne_binary()) -> integer().
count_pages_command(?TIFF_MIME, Filename) ->
    Options = [{<<"timeout">>, ?CONVERT_TIMEOUT}
              ,{<<"absolute_timeout">>, ?CONVERT_TIMEOUT}
              ,{<<"read_mode">>, 'stream'}
              ],
    case kz_os:cmd(?COUNT_TIFF_PAGES_CMD, [{<<"FILE">>, Filename}], Options) of
        {'ok', Result} ->
            kz_term:to_integer(Result);
        _ -> 0
    end;
count_pages_command(_MimeType, _Filename) -> 0.

-spec filetype_from_filename(kz_term:ne_binary()) -> kz_term:ne_binary().
filetype_from_filename(Filename) ->
    filetype_from_extension(filename:extension(Filename)).

-spec filetype_from_extension(kz_term:ne_binary()) -> kz_term:ne_binary().
filetype_from_extension(<<$., Ext/binary>>) -> Ext.

%%%=============================================================================
%%% util functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec save_file({'file', kz_term:ne_binary()}|kz_term:ne_binary(), map()) -> kz_term:ne_binary().
save_file({'file', FilePath}, _Options) ->
    FilePath;
save_file(Content, #{<<"tmp_dir">> := TmpDir
                    ,<<"job_id">> := JobId
                    ,<<"from_format">> := FromFormat
                    }) ->
    Ext = kz_mime:to_extension(FromFormat),
    FilePath = filename:join(TmpDir, <<JobId/binary, ".", Ext/binary>>),
    kz_util:write_file(FilePath, Content),
    FilePath.

-spec maybe_delete_previous_file(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
maybe_delete_previous_file(Filename, Filename) ->
    'ok';
maybe_delete_previous_file(OldFilename, _NewFilename) ->
    kz_util:delete_file(OldFilename).

-spec maybe_rename_file(kz_term:ne_binary(), kz_term:ne_binary()) ->
                               {'ok', kz_term:ne_binary()}|
                               {'error', kz_term:ne_binary()}.
maybe_rename_file(TmpPath, NewPath) ->
    case filelib:is_file(NewPath) of
        'true' -> {'ok', NewPath};
        'false' -> rename_file(TmpPath, NewPath)
    end.

-spec rename_file(kz_term:ne_binary(), kz_term:ne_binary()) ->
                         {'ok', kz_term:ne_binary()}|
                         {'error', kz_term:ne_binary()}.
rename_file(FromPath, ToPath) ->
    lager:info("renaming file from ~s to ~s", [FromPath, ToPath]),
    case filelib:is_file(FromPath) of
        'true' ->
            case file:rename(FromPath, ToPath) of
                'ok' -> {'ok', ToPath};
                {'error', _} -> {'error', <<"failed to rename file to ", ToPath/binary>>}
            end;
        'false' -> {'error', <<"cannot rename from file: ", FromPath/binary, ", file does not exist">>}
    end.
