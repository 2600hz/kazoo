%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_fax_converter_convertapi).

-behaviour(gen_kz_converter).

-export([convert/4
        ,read_metadata/1
        ]).

-include_lib("kazoo_convert/include/kz_convert.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".convertapi">>).
-define(TRY_OPENOFFICE, kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"try_openoffice">>, <<"for_msoffice_files_also">>)).
-define(CONVERTAPI_URL, kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"api_url">>, <<"https://v2.convertapi.com">>)).
-define(CONVERTAPI_SECRET, kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"secret">>)).
-define(CONVERTAPI_TIMEOUT, kapps_config:get_pos_integer(?MOD_CONFIG_CAT, <<"timeout">>, 60)).
-define(CONVERTAPI_PDF_VERSION, kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"pdf_version">>, <<"1.7">>)).
-define(CONVERTAPI_RESOLUTION, kapps_config:get_pos_integer(?MOD_CONFIG_CAT, <<"resolution">>, 200)).

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
%%   <li><strong>try_openoffice:</strong> try conversion using locally installer Libre Office (Open Office). Possible values `for_openoffice_files_only', `for_msoffice_files_also', `never' Default is `for_msoffice_files_also'.</li>
%% </ul>
%%
%% @end
%%------------------------------------------------------------------------------
-spec convert(kz_term:ne_binary(), kz_term:ne_binary(), binary()|{'file', kz_term:ne_binary()}, map() | kz_term:proplist()) ->
                     gen_kz_converter:converted().
convert(From, To, Content, Opts) when not is_map(Opts) ->
    Options = maps:from_list(
                [{<<"from_format">>, From}
                ,{<<"to_format">>, To}
                ,{<<"job_id">>, props:get_value(<<"job_id">>, Opts, kz_binary:rand_hex(12))}
                 | props:delete_keys([<<"job_id">>], Opts)
                ]),
    convert(From, To, Content, Options);
convert(Format, Format, Content, Options) ->
    kz_fax_converter:convert(Format, Format, Content, Options); %% Case when From and To format is same
convert(<<"image/", _SubType/binary>>=From, ?TIFF_MIME, Content, Options) ->
    kz_fax_converter:convert(From, ?TIFF_MIME, Content, Options);
convert(<<"image/", _SubType/binary>>=From, ?PDF_MIME, Content, Options) ->
    kz_fax_converter:convert(From, ?PDF_MIME, Content, Options);%% необходимо добавить аналогичный в kz_fax_converter
convert(?PDF_MIME, ?TIFF_MIME, Content, Options) ->
    kz_fax_converter:convert(?PDF_MIME, ?TIFF_MIME, Content, Options);
convert(?TIFF_MIME, ?PDF_MIME, Content, Options) ->
    kz_fax_converter:convert(?TIFF_MIME, ?PDF_MIME, Content, Options);
convert(<<?OPENOFFICE_MIME_PREFIX, _/binary>>=From, ?TIFF_MIME, Content, Options) ->
    maybe_convert_using_openoffice(From, ?TIFF_MIME, Content, Options);
convert(<<?OPENOFFICE_MIME_PREFIX, _/binary>>=From, ?PDF_MIME, Content, Options) ->
    maybe_convert_using_openoffice(From, ?PDF_MIME, Content, Options);
convert(<<?OPENXML_MIME_PREFIX, _/binary>>=From, ?TIFF_MIME, Content, Options) ->
    maybe_convert_using_openoffice(From, ?TIFF_MIME, Content, Options);
convert(<<?OPENXML_MIME_PREFIX, _/binary>>=From, ?PDF_MIME, Content, Options) ->
    maybe_convert_using_openoffice(From, ?PDF_MIME, Content, Options);
convert(CT, ?TIFF_MIME, Content, Options) when ?OPENOFFICE_COMPATIBLE(CT) ->
    maybe_convert_using_openoffice(CT, ?TIFF_MIME, Content, Options);
convert(CT, ?PDF_MIME, Content, Options) when ?OPENOFFICE_COMPATIBLE(CT) ->
    maybe_convert_using_openoffice(CT, ?PDF_MIME, Content, Options);
convert(From, To, Content, Options) ->
    maybe_convert_via_convertapi(From, To, Content, Options).


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
    kz_fax_converter:read_metadata(Filename).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_convert_using_openoffice(kz_term:ne_binary(), kz_term:ne_binary(), binary()|{'file', kz_term:ne_binary()}, map()) ->
                                            gen_kz_converter:converted().
maybe_convert_using_openoffice(From, To, Content, #{<<"try_openoffice">> := <<"newer">>}=Options) ->
    maybe_convert_via_convertapi(From, To, Content, Options);
maybe_convert_using_openoffice(From, To, Content, #{<<"try_openoffice">> := <<"for_msoffice_files_also">>}=Options) ->
    case kz_fax_converter:convert(From, To, Content, Options) of
        {'ok', _}=Ok -> Ok;
        {'ok', _, _}=Ok -> Ok;
        {'error', _} ->
            lager:debug("trying convert using convertapi"),
            maybe_convert_via_convertapi(From, To, Content, Options)
    end;
maybe_convert_using_openoffice(<<?OPENOFFICE_MIME_PREFIX, _/binary>>=From, To, Content, #{<<"try_openoffice">> := <<"for_openoffice_files_only">>}=Options) ->
    case kz_fax_converter:convert(From, To, Content, Options) of
        {'ok', _}=Ok -> Ok;
        {'ok', _, _}=Ok -> Ok;
        {'error', _} ->
            lager:debug("trying convert using convertapi"),
            maybe_convert_via_convertapi(From, To, Content, Options)
    end;
maybe_convert_using_openoffice(From, To, Content, #{<<"try_openoffice">> := <<"for_openoffice_files_only">>}=Options) ->
    maybe_convert_via_convertapi(From, To, Content, Options);
maybe_convert_using_openoffice(_From, _To, _Content, #{<<"try_openoffice">> := UnsupportedOption}) ->
    {'error', <<"invalid conversion requested: Unsupported \"try_openoffice\" value: ", UnsupportedOption/binary>>};
maybe_convert_using_openoffice(From, To, Content, Options) ->
    case ?TRY_OPENOFFICE of
        <<"never">> -> maybe_convert_using_openoffice(From, To, Content, maps:put(<<"try_openoffice">>, <<"never">>, Options));
        <<"for_msoffice_files_also">> -> maybe_convert_using_openoffice(From, To, Content, maps:put(<<"try_openoffice">>, <<"for_msoffice_files_also">>, Options));
        <<"for_openoffice_files_only">> -> maybe_convert_using_openoffice(From, To, Content, maps:put(<<"try_openoffice">>, <<"for_open_office_files_only">>, Options));
        Value ->
            lager:debug("unsuported \"try_openoffice\" config value: ~p. Will be used \"for_msoffice_files_also\"", [Value]),
            maybe_convert_using_openoffice(From, To, Content, maps:put(<<"try_openoffice">>, <<"for_msoffice_files_also">>, Options))

    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_convert_via_convertapi(kz_term:ne_binary(), kz_term:ne_binary(), binary()|{'file', kz_term:ne_binary()}, map()) ->
                                          gen_kz_converter:converted().
maybe_convert_via_convertapi(From, To, Content, Options) ->
    case ?CONVERTAPI_SECRET of
        'undefined' ->
            JobId = maps:get(<<"job_id">>, Options),
            lager:debug("converapi secret is not defined. Cannot process JobId : ~s", [JobId]),
            {'error', <<"convertapi secret not defined. Failed JobId: ", JobId/binary>>};
        Secret -> convert_via_convertapi(From, To, Content, Options, Secret)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec convert_via_convertapi(kz_term:ne_binary(), kz_term:ne_binary(), binary()|{'file', kz_term:ne_binary()}, map(), kz_term:ne_binary()) ->
                                    gen_kz_converter:converted().
convert_via_convertapi(From
                      ,To
                      ,Content
                      ,#{<<"from_format">> := From
                        ,<<"to_format">> := To
                        ,<<"job_id">> := _
                        }=Options
                      ,Secret
                      ) when is_binary(Content) ->
    case run_convert(eval_format(From, To), To, Content, Options, Secret) of
        {'ok', _}=Ok ->
            lager:info("successfully converted content: ~s to format: ~s", [From, To]),
            Ok;
        {'error', Message}=Error ->
            lager:error("conversion failed with error: ~p", [Message]),
            Error
    end;
convert_via_convertapi(From, To, Content, Options, Secret) when is_binary(Content) ->
    convert_via_convertapi(From, To, Content, Options#{<<"from_format">> => From, <<"to_format">> => To, <<"job_id">> => kz_binary:rand_hex(12) }, Secret);
convert_via_convertapi(From, To, UserPath, Options, Secret) ->
    case read_file(UserPath, Options) of
        {'ok', Content} -> convert_via_convertapi(From, To, Content, Options, Secret);
        {'error', Message}=Error ->
            lager:error("conversion failed with error: ~p", [Message]),
            Error
    end.

-spec run_convert({'error', kz_term:ne_binary()} | fax_convert_funs()
                 ,kz_term:ne_binary()
                 ,kz_term:ne_binary()
                 ,map()
                 ,kz_term:ne_binary()) -> gen_kz_converter:converted().
run_convert({'error', _}=Error, _ToFormat, _Content, _Options, _Secret) ->
    Error;
run_convert([Operation|Operations], ToFormat, Content, Options, Secret) ->
    case Operation(Content, Options, Secret) of
        {'ok', ConvertedContent} ->
            run_convert(Operations, ToFormat, ConvertedContent, Options, Secret);
        Error -> Error
    end;
run_convert([], ToFormat, Content, Options, _Secret) ->
    format_response(ToFormat, Content, Options).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec format_response(kz_term:ne_binary(), kz_term:ne_binary(), map()) ->
                             gen_kz_converter:converted().
format_response(Format, Content, Options) ->
    kz_fax_converter:convert(Format, Format, Content, Options). %% Let parse and apply options by kz_fax_converter

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec eval_format(kz_term:ne_binary(), kz_term:ne_binary()) -> fax_convert_funs() | {'error', kz_term:ne_binary()}.
eval_format(_FromFormat, ?PDF_MIME) ->
    [fun convert_to_pdf/3
    ];
eval_format(_FromFormat, ?TIFF_MIME) ->
    [fun convert_to_pdf/3
    ,fun pdf_to_tiff/3
    ];
eval_format(FromFormat, ToFormat) ->
    {'error', <<"invalid conversion requested: ", FromFormat/binary, " to: ", ToFormat/binary>>}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec convert_to_pdf(kz_term:ne_binary(), map(), kz_term:ne_binary()) -> fax_converted().
convert_to_pdf(Content, #{<<"from_format">> := From, <<"job_id">> := JobId }, Secret) ->
    Extension = kz_mime:to_extension(From),
    FileName = list_to_binary([JobId, ".", Extension]),
    RequestURL = list_to_binary([?CONVERTAPI_URL, "/", Extension, "/to/pdf?",
                                 "Timeout=", list_to_binary(integer_to_list(?CONVERTAPI_TIMEOUT)),
                                 "&PdfVersion=", ?CONVERTAPI_PDF_VERSION,
                                 "&PdfResolution=", list_to_binary(integer_to_list(?CONVERTAPI_RESOLUTION)),
                                 "&Secret=", Secret]),
    Boundary = kz_binary:rand_hex(12),
    ContentTypeHeaderValue = list_to_binary(["multipart/form-data; boundary=", Boundary]),
    Headers = [{"Content-Type", ContentTypeHeaderValue}],
    Body = iolist_to_binary(["--", Boundary
                            ,"\r\nContent-Disposition: form-data; name=\"File\"; filename=\"", FileName, "\""
                            ,"\r\nContent-Type: ", From
                            ,"\r\n\r\n", Content
                            ,"\r\n--", Boundary, "--\r\n"]),
    lager:debug("attemting to convert document using convertapi for JobId: ~s", [JobId]),
    case kz_http:post(kz_term:to_list(RequestURL), Headers, Body) of
        {'ok', 200, _RespHeaders, RespBody} ->
            JObj = kz_json:decode(RespBody),
            case kz_json:is_defined(<<"Files">>, JObj) of
                'true' ->
                    lager:debug("jobid ~s converted successfully using convertapi", [JobId]),
                    [File|_] = kz_json:get_list_value(<<"Files">>, JObj),
                    FileDataBase64 = kz_json:get_value(<<"FileData">>, File),
                    {'ok', base64:decode(FileDataBase64)};
                'false' ->
                    lager:error("we got convertapi responce without files for jobid ~s : ~p", [JobId, JObj]),
                    {'error', <<"we got responce without files">>}
            end;
        Response ->
            lager:debug("unexpected convertapi response: ~p", [Response]),
            {'error', <<"can not convert file">>}
    end.

-spec pdf_to_tiff(kz_term:ne_binary(), map(), kz_term:ne_binary()) ->  fax_converted().
pdf_to_tiff(Content, Options, _Secret) ->
    kz_fax_converter:convert(?PDF_MIME, ?TIFF_MIME, Content, Options#{<<"read_metadata">> => 'false', <<"output_type">> => 'binary' }).


%%%=============================================================================
%%% util functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec read_file({'file', kz_term:ne_binary()}|kz_term:ne_binary(), map()) ->
                       {'ok', kz_term:ne_binary()}|
                       {'error', kz_term:ne_binary()}.
read_file({'file', UserPath}, #{<<"tmp_dir">> := TmpDir}) ->
    case filename:pathtype(UserPath) of
        'absolute' ->
            file:read_file(UserPath);
        'relative' ->
            file:read_file(filename:join(TmpDir, UserPath));
        _ ->
            {'error', <<"invalid filename ", UserPath/binary>>}
    end;
read_file(Content, _Options) ->
    Content.
