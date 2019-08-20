%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_fax_converter).

-behaviour(gen_kz_converter).

-export([convert/4
        ,do_openoffice_to_pdf/2
        ,read_metadata/1
        ,get_tiff_info/1
        ]).

-include("kz_fax_converter.hrl").

-type fax_converted() :: {'ok', any()}|
                         {'error', any()}.

-type fax_convert_funs() :: [fun((kz_term:ne_binary(), map()) -> fax_converted())].

%%------------------------------------------------------------------------------
%% @doc Converts the data or file specified in `Content' from the `From' mime-type to the
%% `To' mime-type.
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
%%   <li><strong>read_metadata:</strong>Include a third option in the output tuple which is a Proplist of metadata about the file.</li>
%%   <li><strong>to_filename:</strong>The user requested destination file name for the converted file, if a full path is provided this will
%%   be copied to the specified path, if a relative path is specified, it will be copied to the `tmp_dir' using the file name specified</li>
%% </ul>
%%
%% @end
%%------------------------------------------------------------------------------
-spec convert(kz_term:ne_binary(), kz_term:ne_binary(), binary()|{'file', kz_term:ne_binary()}, map() | kz_term:proplist()) ->
                     gen_kz_converter:converted().
convert(From, To, Content, #{<<"from_format">> := From, <<"to_format">> := To, <<"job_id">> := _ }=Options) ->
    Filename = save_file(Content, Options),
    lager:info("converting document ~s from ~s to ~s", [Filename, From, To]),
    case run_convert(eval_format(From, To), To, Filename, Options) of
        {'ok', _}=Ok ->
            lager:info("successfully converted file: ~s to format: ~s", [Filename, To]),
            Ok;
        {'ok', _, _}=Ok ->
            lager:info("successfully converted file: ~s to format: ~s", [Filename, To]),
            Ok;
        {'error', Message}=Error ->
            lager:error("conversion failed with error: ~p", [Message]),
            Error
    end;
convert(From, To, Content, Options) when is_map(Options) ->
    case maps:is_key(<<"job_id">>, Options) of
        true -> convert(From, To, Content, Options#{<<"from_format">> => From, <<"to_format">> => To});
        false -> convert(From, To, Content, Options#{<<"from_format">> => From, <<"to_format">> => To, <<"job_id">> => kz_binary:rand_hex(12)})
    end;
convert(From, To, Content, Opts) ->
    Options = maps:from_list(
                [{<<"from_format">>, From}
                ,{<<"to_format">>, To}
                ,{<<"job_id">>, props:get_value(<<"job_id">>, Opts, kz_binary:rand_hex(12))}
                 | props:delete_keys([<<"job_id">>], Opts)
                ]),
    convert(From, To, Content, Options).

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
run_convert({'error', _}=Error, _ToFormat, FilePath, _Options) ->
    _ = file:delete(FilePath),
    Error;
run_convert([Operation|Operations], ToFormat, FilePath, Options) ->
    case Operation(FilePath, Options) of
        {'ok', OutputPath} ->
            maybe_delete_previous_file(FilePath, OutputPath),
            run_convert(Operations, ToFormat, OutputPath, Options);
        Error ->
            _ = file:delete(FilePath),
            Error
    end;
run_convert([], ToFormat, FilePath, Options) ->
    case validate_output(ToFormat, FilePath, Options) of
        {'ok', _} ->
            format_response(ToFormat, FilePath, Options);
        Error ->
            _ = file:delete(FilePath),
            Error
    end.

-spec image_to_tiff(kz_term:ne_binary(), map()) -> fax_converted().
image_to_tiff(FromPath, #{<<"from_format">> := <<"image/tiff">>, <<"tmp_dir">> := TmpDir, <<"job_id">> := JobId }=Options) ->
    Info = get_tiff_info(FromPath),
    case select_tiff_command(Info) of
        'noop' ->
            rename_file(FromPath, filename:join(TmpDir, <<JobId/binary, ".tiff">>));
        {'resample', Command} ->
            case convert_file(Command, FromPath, <<".tiff">>, Options) of
                {'ok', Converted} ->
                    handle_resample(Converted, Options);
                Error -> Error
            end;
        {'convert', Command} ->
            convert_file(Command, FromPath, <<".tiff">>, Options)
    end;
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
            {'ok', ToPath};
        {'error', Reason, Msg} ->
            lager:debug("failed to convert file with reason: ~p, output: ~p", [Reason, Msg]),
            _ = file:delete(ToPath),
            {'error', <<"convert command failed">>}
    end.

-spec handle_resample(kz_term:ne_binary(), map()) -> fax_converted().
handle_resample(FromPath, #{<<"tmp_dir">> := TmpDir}=Options) ->
    case rename_file(FromPath, filename:join(TmpDir, <<(kz_binary:rand_hex(12))/binary, ".tiff">>)) of
        {'ok', NewPath} ->
            lager:debug("resampled file to ~p, ensuring valid fax format", [NewPath]),
            image_to_tiff(NewPath, Options);
        Error -> Error
    end.

-spec select_tiff_command(map()) ->
                                 {'convert', kz_term:ne_binary()} |
                                 {'resample', kz_term:ne_binary()} |
                                 'noop'.
select_tiff_command(#{<<"res_x">> := X, <<"res_y">> := Y}=Map)
  when X =:= 0
       orelse Y =:= 0 ->
    lager:debug("file is unknown dpi, re-sampling info: ~p", [Map]),
    {'resample', ?RESAMPLE_IMAGE_COMMAND};
select_tiff_command(#{<<"res_x">> := X, <<"res_y">> := Y}=Map)
  when X > 204
       orelse Y > 200 ->
    lager:debug("file is too high a dpi, re-sampling info: ~p", [Map]),
    {'resample', ?RESAMPLE_IMAGE_COMMAND};
select_tiff_command(#{<<"length">> := Height}=Map) when Height > 2200 ->
    lager:debug("file is too long, resizing with info: ~p", [Map]),
    {'convert', ?LARGE_TIFF_COMMAND};
select_tiff_command(#{<<"width">> := Width}=Map) when Width > 1728 ->
    lager:debug("file is too wide, resizing with info: ~p", [Map]),
    {'convert', ?LARGE_TIFF_COMMAND};
select_tiff_command(#{<<"width">> := Width}=Map) when Width < 1728 ->
    lager:debug("file is smaller than page, centering with info: ~p", [Map]),
    {'convert', ?SMALL_TIFF_COMMAND};
select_tiff_command(#{<<"scheme">> := <<"CCITT Group 3">>, <<"has_pages">> := 'true'}=Map) ->
    lager:debug("file has pages and is valid format for group 3, not going to convert info: ~p", [Map]),
    'noop';
select_tiff_command(#{<<"scheme">> := <<"CCITT Group 4">>, <<"has_pages">> := 'true'}=Map) ->
    lager:debug("file has pages and is valid format for group 4, not going to convert info: ~p", [Map]),
    'noop';
select_tiff_command(Map) ->
    lager:debug("file has no pages or is not ccitt fax encoding, re-sampling with info: ~p", [Map]),
    {'resample', ?CONVERT_IMAGE_COMMAND}.

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
            _ = file:delete(ToPath),
            Ok;
        {'error', Reason, Msg} ->
            lager:debug("failed to validate file: ~s with reason: ~s error: ~p", [FromPath, Reason, Msg]),
            _ = file:delete(ToPath),
            {'error', <<"file validation failed">>}
    end.

%%%=============================================================================
%%% output formatting functions
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
%% @doc read the diff metadata to help with conversion command selection
%% @end
%%------------------------------------------------------------------------------
-spec get_tiff_info(kz_term:ne_binary()) -> map()|{'error', any(), kz_term:ne_binary()}.
get_tiff_info(FilePath) ->
    Args = [{<<"FILE">>, FilePath}
           ],
    case kz_os:cmd(?TIFF_INFO_CMD, Args) of
        {'ok', Data} ->
            parse_tiff_info([L || L <- binary:split(Data, <<"\n">>, ['global']), L =/= <<>>], #{<<"has_pages">> => 'false'});
        Error -> Error
    end.

parse_tiff_info([], Acc) ->
    Acc;
parse_tiff_info([Line|Rest], Acc) ->
    case Line of
        <<"Width: ", Width/binary>> ->
            parse_tiff_info(Rest, Acc#{<<"width">> => kz_term:to_integer(Width)});
        <<"Length: ", Length/binary>> ->
            parse_tiff_info(Rest, Acc#{<<"length">> => kz_term:to_integer(Length)});
        <<"X: ", X/binary>> ->
            parse_tiff_info(Rest, Acc#{<<"res_x">> => kz_term:to_integer(X)});
        <<"Y: ", Y/binary>> ->
            parse_tiff_info(Rest, Acc#{<<"res_y">> => kz_term:to_integer(Y)});
        <<"Compression Scheme: ", Scheme/binary>> ->
            parse_tiff_info(Rest, Acc#{<<"scheme">> => Scheme});
        <<"Page Number: ", _/binary>> ->
            Acc#{<<"has_pages">> => 'true'};
        _Else ->
            parse_tiff_info(Rest, Acc)
    end.
%%------------------------------------------------------------------------------
%% @doc read metadata about a file to provide information like size and page count
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
