%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(fax_converters_generic).
-export([prepare_contents/4
        ]).

-include("fax.hrl").

-spec prepare_contents(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                              {'ok', kz_term:ne_binary()} |
                              {'error', kz_term:ne_binary()}.
prepare_contents(<<"application/pdf">>, JobId, RespContent, TmpDir) ->
    InputFile = list_to_binary([TmpDir, JobId, ".pdf"]),
    OutputFile = list_to_binary([TmpDir, JobId, ".tiff"]),
    kz_util:write_file(InputFile, RespContent),
    Cmd = io_lib:format(?CONVERT_PDF_COMMAND, [OutputFile, InputFile]),
    lager:debug("attempting to convert pdf: ~s", [Cmd]),
    try "success" = os:cmd(Cmd) of
        "success" -> {'ok', OutputFile}
    catch
        Type:Exception ->
            lager:debug("could not covert file: ~p:~p", [Type, Exception]),
            {'error', <<"can not convert file, try uploading a tiff">>}
    end;

prepare_contents(<<"image/", SubType/binary>>, JobId, RespContent, TmpDir) ->
    InputFile = list_to_binary([TmpDir, JobId, "-input.", SubType]),
    OutputFile = list_to_binary([TmpDir, JobId, ".tiff"]),
    kz_util:write_file(InputFile, RespContent),
    Cmd = io_lib:format(?CONVERT_IMAGE_COMMAND, [InputFile, OutputFile]),
    lager:debug("attempting to convert ~s file to tiff group 4 format: ~s", [SubType, Cmd]),
    try "success" = os:cmd(Cmd) of
        "success" -> {'ok', OutputFile}
    catch
        Type:Exception ->
            lager:debug("could not covert file: ~p:~p", [Type, Exception]),
            {'error', <<"can not convert file, try uploading a tiff">>}
    end;

prepare_contents(<<?OPENXML_MIME_PREFIX, _/binary>> = CT, JobId, RespContent, TmpDir) ->
    convert_openoffice_document(CT, TmpDir, JobId, RespContent);

prepare_contents(<<?OPENOFFICE_MIME_PREFIX, _/binary>> = CT, JobId, RespContent, TmpDir) ->
    convert_openoffice_document(CT, TmpDir, JobId, RespContent);

prepare_contents(CT, JobId, RespContent, TmpDir)
  when ?OPENOFFICE_COMPATIBLE(CT) ->
    convert_openoffice_document(CT, TmpDir, JobId, RespContent);

prepare_contents(CT, _JobId, _RespContent, _TmpDir) ->
    lager:debug("unsupported file type: ~p", [CT]),
    {'error', list_to_binary(["file type '", CT, "' is unsupported"])}.

-spec convert_openoffice_document(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                                         {'ok', kz_term:ne_binary()} |
                                         {'error', kz_term:ne_binary()}.
convert_openoffice_document(CT, TmpDir, JobId, RespContent) ->
    Extension = kz_mime:to_extension(CT),
    InputFile = list_to_binary([TmpDir, JobId, ".", Extension]),
    OutputFile = list_to_binary([TmpDir, JobId, ".tiff"]),
    kz_util:write_file(InputFile, RespContent),
    OpenOfficeServer = kapps_config:get_binary(?CONFIG_CAT, <<"openoffice_server">>, <<"'socket,host=localhost,port=2002;urp;StarOffice.ComponentContext'">>),
    Cmd = io_lib:format(?CONVERT_OO_COMMAND, [OpenOfficeServer, InputFile, OutputFile]),
    lager:debug("attemting to convert openoffice document: ~s", [Cmd]),
    try "success" = os:cmd(Cmd) of
        "success" -> {'ok', OutputFile}
    catch
        Type:Exception ->
            lager:debug("could not covert file: ~p:~p", [Type, Exception]),
            {'error', <<"can not convert file, try uploading a tiff">>}
    end.
