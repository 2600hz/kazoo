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
    end.
