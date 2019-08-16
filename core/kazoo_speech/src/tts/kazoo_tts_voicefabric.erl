%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_tts_voicefabric).

-behaviour(gen_tts_provider).

-export([create/4
        ,set_api_key/1
        ]).

-include("kazoo_speech.hrl").

-define(VOICEFABRIC_VOICE_MAPPINGS
       ,[{<<"male/ru-ru">>, <<"Владимир8000"/utf8>>}
        ,{<<"female/ru-ru">>, <<"Юлия8000"/utf8>>}
        ,{<<"female/en-us">>, <<"Carol8000">>}
        ,{<<"male/ru-ru/vladimir">>, <<"Владимир8000"/utf8>>}
        ,{<<"female/ru-ru/julia">>, <<"Юлия8000"/utf8>>}
        ,{<<"female/ru-ru/anna">>, <<"Анна8000"/utf8>>}
        ,{<<"female/ru-ru/viktoria">>, <<"Виктория8000"/utf8>>}
        ,{<<"male/ru-ru/alexander">>, <<"Александр8000"/utf8>>}
        ,{<<"female/ru-ru/maria">>, <<"Мария8000"/utf8>>}
        ,{<<"female/ru-ru/lidia">>, <<"Лидия8000"/utf8>>}
        ,{<<"female/en-US/carol">>, <<"Carol8000">>}
        ,{<<"female/en-US/asel">>, <<"Asel8000">>}
        ,{<<"male/ru-ru/vladimir/22050">>, <<"Владимир"/utf8>>}
        ,{<<"female/ru-ru/julia/22050">>, <<"Юлия"/utf8>>}
        ,{<<"female/ru-ru/anna/22050">>, <<"Анна"/utf8>>}
        ,{<<"female/ru-ru/viktoria/22050">>, <<"Виктория"/utf8>>}
        ,{<<"male/ru-ru/alexander/22050">>, <<"Александр"/utf8>>}
        ,{<<"female/ru-ru/maria/22050">>, <<"Мария"/utf8>>}
        ,{<<"female/ru-ru/lidia/22050">>, <<"Лидия"/utf8>>}
        ,{<<"female/en-US/carol/22050">>, <<"Carol">>}
        ,{<<"female/en-US/asel/22050">>, <<"Asel">>}
        ]
       ).

-define(VOICEFABRIC_TTS_URL, kapps_config:get_string(?MOD_CONFIG_CAT, <<"tts_url_voicefabric">>, <<"https://voicefabric.ru/WSServer/ws/tts">>)).

-spec set_api_key(kz_term:ne_binary()) -> 'ok'.
set_api_key(Key) ->
    {'ok', _} = kapps_config:set_default(?MOD_CONFIG_CAT, <<"tts_api_key">>, Key),
    'ok'.

-spec create(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> create_resp().
create(Text, Voice, _Format, Options) ->
    lager:debug("getting ~ts from VoiceFabric", [Text]),
    VoiceMappings = ?VOICEFABRIC_VOICE_MAPPINGS,
    case props:get_value(kz_term:to_lower_binary(Voice), VoiceMappings) of
        'undefined' ->
            {'error', 'invalid_voice'};
        VFabricVoice ->
            BaseUrl = ?VOICEFABRIC_TTS_URL,
            Data = [{<<"apikey">>, ?TTS_API_KEY}
                   ,{<<"ttsVoice">>, VFabricVoice}
                   ,{<<"textFormat">>, <<"text/plain">>}
                   ,{<<"text">>, Text}
                   ],
            ArgsEncode = kapps_config:get_binary(?MOD_CONFIG_CAT, <<"tts_args_encode">>, <<"multipart">>),
            case voicefabric_request_body(ArgsEncode, Data) of
                {'ok', Headers, Body} ->
                    lager:debug("sending ~ts", [Body]),

                    case props:get_value('receiver', Options) of
                        Pid when is_pid(Pid) ->
                            HTTPOptions = props:delete('receiver', Options),
                            Response = kz_http:async_req(Pid, 'post', BaseUrl, Headers, Body, HTTPOptions),
                            create_response(Response);
                        _ ->
                            HTTPOptions = props:delete('receiver', Options),
                            Response = kz_http:post(BaseUrl, Headers, Body, HTTPOptions),
                            create_response(Response)
                    end;
                {'error', Reason} ->
                    lager:warning(Reason),
                    {'error', 'tts_provider_failure', Reason}
            end
    end.

-spec voicefabric_request_body(kz_term:ne_binary(), list()) ->
                                      {'ok', list(), kz_term:ne_binary()} |
                                      {'error', kz_term:ne_binary()}.
voicefabric_request_body(<<"urlencode">>, Data) ->
    Headers = [{"Content-Type", "application/x-www-form-urlencoded"}],
    Body = kz_http_util:props_to_querystring(Data),
    {'ok', Headers, Body};
voicefabric_request_body(<<"multipart">>, Data) ->
    Boundary = iolist_to_binary([<<"--bound--">>
                                ,kz_datamgr:get_uuid()
                                ,<<"--bound--">>
                                ]),
    Headers = [{"Content-Type"
               ,"multipart/form-data; charset=UTF-8; boundary=" ++ erlang:binary_to_list(Boundary)
               }
              ],
    Body = iolist_to_binary([[["--", Boundary
                              ,"\r\nContent-Disposition: form-data;"
                              ," name=\"", Key
                              ,"\"\r\n\r\n", Val
                              ,"\r\n"
                              ]
                              || {Key, Val} <- Data
                             ]
                            ,"--"
                            ,Boundary
                            ,"--"
                            ]),
    {'ok', Headers, Body};
voicefabric_request_body(ArgsEncode, _Data) ->
    {'error', <<"voicefabric: unknown args encode method: ", ArgsEncode/binary>>}.

-spec create_response(kz_http:ret()) ->
                             kz_http:req_id() |
                             {'ok', kz_term:ne_binary(), kz_term:ne_binary()} |
                             {'error', 'tts_provider_failure', binary()}.
create_response({'error', _R}) ->
    lager:warning("creating speech file failed with error ~p", [_R]),
    {'error', 'tts_provider_failure', <<"unexpected error encountered accessing provider">>};
create_response({'http_req_id', ReqID}) ->
    lager:debug("speech file streaming as ~p", [ReqID]),
    {'async', ReqID, 'undefined'};
create_response({'ok', 200, Headers, Content}) ->
    _ = [lager:debug("hdr: ~p", [H]) || H <- Headers],
    lager:debug("converting media"),
    {'ok', Rate} = voicefabric_get_media_rate(Headers),
    RawFile = kazoo_speech_util:tmp_file_name(<<"raw">>),
    WavFile = kazoo_speech_util:tmp_file_name(<<"wav">>),
    kz_util:write_file(RawFile, Content),
    From = ["raw -r ", Rate, " -e signed-integer -b 16"],
    To = "wav",
    Cmd = binary_to_list(iolist_to_binary(["sox -t ", From, " ", RawFile, " -t ", To, " ", WavFile])),
    lager:debug("os cmd: ~ts", [Cmd]),
    CmdOut = os:cmd(Cmd),
    CmdOut =:= []
        orelse lager:debug("cmd out: ~ts", [CmdOut]),
    kz_util:delete_file(RawFile),
    lager:debug("reading file"),
    case file:read_file(WavFile) of
        {'ok', WavContent} ->
            kz_util:delete_file(WavFile),
            lager:debug("media converted"),
            NewHeaders = props:set_values([{"content-type", "audio/wav"}
                                          ,{"content-length", integer_to_list(byte_size(WavContent))}
                                          ]
                                         ,Headers
                                         ),
            lager:debug("corrected headers"),
            create_default_response({'ok', 200, NewHeaders, WavContent});
        {'error', _Reason} ->
            lager:debug("failed: ~p", [_Reason]),
            {'error', 'tts_provider_failure', <<"converting failed">>}
    end;
create_response({'ok', _Code, RespHeaders, Content}) ->
    lager:warning("creating speech file failed with code ~p: ~p", [_Code, Content]),
    _ = [lager:debug("hdr: ~p", [H]) || H <- RespHeaders],
    {'error', 'tts_provider_failure', Content}.

-spec voicefabric_get_media_rate(kz_term:proplist()) -> {'ok', kz_term:ne_binary()}.
voicefabric_get_media_rate(Headers1) ->
    Headers = [{kz_term:to_lower_binary(X), kz_term:to_binary(Y)}
               || {X, Y} <- Headers1
              ],
    <<"audio/raw; ", Params/binary>> =
        props:get_value(<<"content-type">>, Headers),
    [<<"rate=", Rate/binary>>] =
        [X || X <- re:split(Params, "; "), voicefabric_filter_rate(X)],
    {'ok', Rate}.

-spec voicefabric_filter_rate(kz_term:ne_binary()) -> boolean().
voicefabric_filter_rate(<<"rate=", _/binary>>) -> 'true';
voicefabric_filter_rate(_)                     -> 'false'.

-spec create_default_response({'ok', 200, kz_term:proplist(), binary()}) ->
                                     {'ok', kz_term:ne_binary(), binary()}.
create_default_response({'ok', 200, Headers, Content}) ->
    ContentType = props:get_value("content-type", Headers),
    ContentLength = props:get_value("content-length", Headers),
    lager:debug("created speech file ~s of length ~s", [ContentType, ContentLength]),
    {'ok', kz_term:to_binary(ContentType), Content}.
