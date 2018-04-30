%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(fax_converters_convertapi).
-export([prepare_contents/5
        ]).

-include("fax.hrl").

-define(CONVERTAPI_URL, kapps_config:get_ne_binary(?CONFIG_CAT, <<"convertapi_url">>, <<"https://v2.convertapi.com">>)).

-spec prepare_contents(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                              {'ok', kz_term:ne_binary()} |
                              {'error', kz_term:ne_binary()}.
prepare_contents(CT, TmpDir, JobId, RespContent, Secret) ->
    Extension = kz_mime:to_extension(CT),
    FileName = list_to_binary([JobId, ".", Extension]),
    RequestURL = list_to_binary([?CONVERTAPI_URL, "/", Extension, "/to/tiff?ImageResolutionH=204&ImageResolutionV=392&ScaleImage=true&ScaleProportions=true&ScaleIfLarger=true&ImageHeight=4312&ImageWidth=1728&Secret=", Secret]),
    Boundary = kz_term:to_hex_binary(crypto:strong_rand_bytes(24)),
    ContentTypeHeaderValue = list_to_binary(["multipart/form-data; boundary=", Boundary]),
    Headers = [{"Content-Type", ContentTypeHeaderValue}],
    Body = iolist_to_binary(["--", Boundary
                            ,"\r\nContent-Disposition: form-data; name=\"File\"; filename=\"", FileName, "\""
                            ,"\r\nContent-Type: ", CT
                            ,"\r\n\r\n", RespContent
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
                    fax_converters_generic:prepare_contents(<<"image/tiff">>, JobId, base64:decode(FileDataBase64), TmpDir);
                'false' ->
                    lager:error("we got convertapi responce without files for jobid ~s : ~p", [JobId, JObj]),
                    {'error', <<"we got responce without files">>}
            end;
        _Response ->
            lager:debug("unexpected canvertapi response sending update_job_status: ~p", [_Response]),
            {'error', <<"can not convert file">>}
    end.
