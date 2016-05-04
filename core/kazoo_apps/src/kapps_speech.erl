%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @doc
%%% For TTS (Text To Speech), use the create/* methods
%%% To do ASR (Automatic Speech Recognition), there are two options:
%%%  1. asr_freeform/* -> takes an arbitrary file and tries to transcribe
%%%     it as best the engine can
%%%  2. asr_commands/* -> For greater accuracy, include a list of expected
%%%     words, and the ASR engine will try to determine which command is
%%%     said in the audio file.
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%   SIPLABS LLC (Ilya Ashchepkov)
%%%-------------------------------------------------------------------
-module(kapps_speech).

-include("kazoo_apps.hrl").
-include("kapps_speech.hrl").

-export([create/1
         ,create/2
         ,create/3
         ,create/4
         ,create/5
        ]).
-export([asr_freeform/1
         ,asr_freeform/2
         ,asr_freeform/3
         ,asr_freeform/4
        ]).
-export([asr_commands/2
         ,asr_commands/3
         ,asr_commands/4
         ,asr_commands/5
        ]).

-type provider_errors() :: 'invalid_voice' | 'unknown_provider'.
-type provider_return() :: {'error', provider_errors()} |
                           kz_http:ret().
-type create_resp() :: provider_return() |
                       {'ok', ne_binary(), ne_binary()} | %% {'ok', ContentType, BinaryData}
                       {'error', 'tts_provider_failure', binary()}.

-type asr_resp() :: kz_http:req_id() |
                    {'ok', kz_json:object()} | %% {'ok', JObj}
                    {'error', provider_errors()} |
                    {'error',  'asr_provider_failure', kz_json:object()}.

%%------------------------------------------------------------------------------
%% Create a tts audio file using a configured provider
%%------------------------------------------------------------------------------
-spec create(ne_binary()) -> create_resp().
create(Text) ->
    create(Text, <<"female/en-us">>).

-spec create(ne_binary(), ne_binary()) -> create_resp().
create(Text, Voice) ->
    create(Text, Voice, <<"wav">>).

-spec create(ne_binary(), ne_binary(), ne_binary()) -> create_resp().
create(Text, Voice, Format) ->
    create(Text, Voice, Format, []).

-spec create(ne_binary(), ne_binary(), ne_binary(), kz_proplist()) -> create_resp().
create(Text, Voice, Format, Options) ->
    Provider = kapps_config:get_binary(?MOD_CONFIG_CAT, <<"tts_provider">>, <<"ispeech">>),
    create(Provider, Text, Voice, Format, Options).

-spec create(maybe(binary()), ne_binary(), ne_binary(), ne_binary(), kz_proplist()) -> create_resp().
create('undefined', Text, Voice, Format, Options) ->
    create(Text, Voice, Format, Options);
create(<<"ispeech">> = Engine, Text, Voice, Format, Opts) ->
    VoiceMappings = ?ISPEECH_VOICE_MAPPINGS,
    case props:get_value(kz_util:to_lower_binary(Voice), VoiceMappings) of
        'undefined' ->
            {'error', 'invalid_voice'};
        ISpeechVoice ->
            BaseUrl = ?ISPEECH_TTS_URL,

            Props = [{<<"text">>, Text}
                     ,{<<"voice">>, ISpeechVoice}
                     ,{<<"format">>, Format}
                     ,{<<"action">>, <<"convert">>}
                     ,{<<"apikey">>, kapps_config:get_binary(?MOD_CONFIG_CAT, <<"tts_api_key">>, <<>>)}
                     ,{<<"speed">>, kapps_config:get_integer(?MOD_CONFIG_CAT, <<"tts_speed">>, 0)}
                     ,{<<"startpadding">>, kapps_config:get_integer(?MOD_CONFIG_CAT, <<"tts_start_padding">>, 1)}
                     ,{<<"endpadding">>, kapps_config:get_integer(?MOD_CONFIG_CAT, <<"tts_end_padding">>, 0)}
                    ],
            Headers = [{"Content-Type", "application/json; charset=UTF-8"}],
            Body = kz_json:encode(kz_json:from_list(Props)),

            lager:debug("sending TTS request to ~s", [BaseUrl]),

            case props:get_value('receiver', Opts) of
                Pid when is_pid(Pid) ->
                    HTTPOptions = props:delete('receiver', Opts),
                    Response = kz_http:async_req(Pid, 'post', BaseUrl, Headers, Body, HTTPOptions),
                    create_response(Engine, Response);
                _ ->
                    HTTPOptions = props:delete('receiver', Opts),
                    Response = kz_http:post(BaseUrl, Headers, Body, HTTPOptions),
                    create_response(Engine, Response)
            end
    end;
create(<<"voicefabric">> = Engine, Text, Voice, <<"wav">> = _Format, Options) ->
    create_voicefabric(Engine, Text, Voice, Options);
create(_, _, _, _, _) ->
    {'error', 'unknown_provider'}.

-spec create_voicefabric(ne_binary(), binary(), ne_binary(), kz_proplist()) ->
                                create_resp().
create_voicefabric(Engine, Text, Voice, Opts) ->
    lager:debug("getting ~ts from VoiceFabric", [Text]),
    VoiceMappings = ?VOICEFABRIC_VOICE_MAPPINGS,
    case props:get_value(kz_util:to_lower_binary(Voice), VoiceMappings) of
        'undefined' ->
            {'error', 'invalid_voice'};
        VFabricVoice ->
            BaseUrl = ?VOICEFABRIC_TTS_URL,
            ApiKey = kapps_config:get_binary(?MOD_CONFIG_CAT, <<"tts_api_key">>),
            Data = [{<<"apikey">>, ApiKey}
                    ,{<<"ttsVoice">>, VFabricVoice}
                    ,{<<"textFormat">>, <<"text/plain">>}
                    ,{<<"text">>, Text}
                   ],
            ArgsEncode = kapps_config:get_binary(?MOD_CONFIG_CAT, <<"tts_args_encode">>, <<"multipart">>),
            case voicefabric_request_body(ArgsEncode, Data) of
                {'ok', Headers, Body} ->
                    lager:debug("sending ~ts", [Body]),

                    case props:get_value('receiver', Opts) of
                        Pid when is_pid(Pid) ->
                            HTTPOptions = props:delete('receiver', Opts),
                            Response = kz_http:async_req(Pid, 'post', BaseUrl, Headers, Body, HTTPOptions),
                            create_response(Engine, Response);
                        _ ->
                            HTTPOptions = props:delete('receiver', Opts),
                            Response = kz_http:post(BaseUrl, Headers, Body, HTTPOptions),
                            create_response(Engine, Response)
                    end;
                {'error', Reason} ->
                    lager:warning(Reason),
                    {'error', 'tts_provider_failure', Reason}
            end
    end.

-spec voicefabric_request_body(ne_binary(), list()) ->
                                      {'ok', list(), ne_binary()} |
                                      {'error', ne_binary()}.
voicefabric_request_body(<<"urlencode">>, Data) ->
    Headers = [{"Content-Type", "application/x-www-form-urlencoded"}],
    Body = props:to_querystring(Data),
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
    Body = iolist_to_binary([[<<"--", Boundary/binary,
                                "\r\nContent-Disposition: form-data;"
                                " name=\"", Key/binary
                                , "\"\r\n\r\n", Val/binary, "\r\n"
                              >>
                              || {Key, Val} <- Data
                             ]
                             ,"--"
                             ,Boundary
                             ,"--"
                            ]),
    {'ok', Headers, Body};
voicefabric_request_body(ArgsEncode, _Data) ->
    {'error', <<"voicefabric: unknown args encode method: ", ArgsEncode/binary>>}.

%%------------------------------------------------------------------------------
%% Transcribe the audio binary
%%------------------------------------------------------------------------------
-spec asr_freeform(binary()) -> asr_resp().
-spec asr_freeform(binary(), ne_binary()) -> asr_resp().
-spec asr_freeform(binary(), ne_binary(), ne_binary()) -> asr_resp().
-spec asr_freeform(binary(), ne_binary(), ne_binary(), kz_proplist()) -> asr_resp().
asr_freeform(Content) ->
    asr_freeform(Content, <<"application/wav">>).
asr_freeform(Content, ContentType) ->
    asr_freeform(Content, ContentType, <<"en-US">>).
asr_freeform(Content, ContentType, Locale) ->
    asr_freeform(Content, ContentType, Locale, []).
asr_freeform(Content, ContentType, Locale, Options) ->
    Provider = kapps_config:get_binary(?MOD_CONFIG_CAT, <<"asr_provider">>, <<>>),
    case kz_util:is_empty(Provider) of
        'true' -> {'error', 'no_asr_provider'};
        'false' -> maybe_convert_content(Content, ContentType, Locale, Options)
    end.

-spec maybe_convert_content(binary(), ne_binary(), ne_binary(), kz_proplist()) -> provider_return().
maybe_convert_content(Content, ContentType, Locale, Options) ->
    ContentTypes = kapps_config:get(?MOD_CONFIG_CAT
                                     ,<<"asr_content_types">>
                                     ,[<<"application/mpeg">>
                                       ,<<"application/wav">>
                                      ]),
    case lists:member(ContentType, ContentTypes) of
        'true' -> attempt_asr_freeform(Content, ContentType, Locale, Options);
        'false' ->
            ConvertTo = kapps_config:get_binary(?MOD_CONFIG_CAT
                                                 ,<<"asr_prefered_content_type">>
                                                 ,<<"application/mpeg">>
                                                ),
            case convert_content(Content, ContentType, ConvertTo) of
                'error' -> {'error', 'unsupported_content_type'};
                Converted ->
                    attempt_asr_freeform(Converted, ConvertTo, Locale, Options)
            end
    end.

-spec attempt_asr_freeform(binary(), ne_binary(), ne_binary(), kz_proplist()) -> provider_return().
attempt_asr_freeform(Content, ContentType, Locale, Options) ->
    Provider = kapps_config:get_binary(?MOD_CONFIG_CAT, <<"asr_provider">>, <<>>),
    case attempt_asr_freeform(Provider, Content, ContentType, Locale, Options) of
        {'error', _R}=E ->
            lager:debug("asr failed with error ~p", [_R]),
            E;
        {'http_req_id', ReqID} ->
            lager:debug("streaming response ~p to provided option: ~p"
                        ,[ReqID, props:get_value('receiver', Options)]
                       ),
            {'ok', ReqID};
        {'ok', 200, _Headers, Content2} ->
            lager:debug("asr of media succeeded: ~s", [Content2]),
            {'ok', kz_json:decode(Content2)};
        {'ok', _Code, _Hdrs, Content2} ->
            lager:debug("asr of media failed with code ~p", [_Code]),
            lager:debug("resp: ~s", [Content2]),
            {'error', 'asr_provider_failure', kz_json:decode(Content2)}
    end.

-spec attempt_asr_freeform(maybe(binary()), binary(), ne_binary(), ne_binary(), kz_proplist()) ->
                                  provider_return().
attempt_asr_freeform(_, <<>>, _, _, _) -> {'error', 'no_content'};
attempt_asr_freeform(<<"ispeech">>, Bin, ContentType, Locale, Opts) ->
    BaseUrl = kapps_config:get_string(?MOD_CONFIG_CAT
                                       ,<<"asr_url">>
                                       ,<<"http://api.ispeech.org/api/rest">>
                                      ),
    lager:debug("sending request to ~s", [BaseUrl]),
    Props = [{<<"apikey">>, kapps_config:get_binary(?MOD_CONFIG_CAT, <<"asr_api_key">>, <<>>)}
             ,{<<"action">>, <<"recognize">>}
             ,{<<"freeform">>, <<"1">>}
             ,{<<"content-type">>, ContentType}
             ,{<<"output">>, <<"json">>}
             ,{<<"locale">>, Locale}
             ,{<<"audio">>, base64:encode(Bin)}
            ],
    Headers = [{"Content-Type", "application/x-www-form-urlencoded"}],
    Body = props:to_querystring(Props),
    lager:debug("req body: ~s", [Body]),
    case props:get_value('receiver', Opts) of
        Pid when is_pid(Pid) ->
            HTTPOptions = props:delete('receiver', Opts),
            kz_http:async_req(Pid, 'post', BaseUrl, Headers, Body, HTTPOptions);
        _ ->
            HTTPOptions = props:delete('receiver', Opts),
            kz_http:post(BaseUrl, Headers, Body, HTTPOptions)
    end;
attempt_asr_freeform(_, _, _, _, _) ->
    {'error', 'unknown_provider'}.

%%------------------------------------------------------------------------------
%% Transcribe the audio binary
%%------------------------------------------------------------------------------
-spec asr_commands(ne_binary(), ne_binaries()) -> asr_resp().
-spec asr_commands(ne_binary(), ne_binaries(), ne_binary()) -> asr_resp().
-spec asr_commands(ne_binary(), ne_binaries(), ne_binary(), ne_binary()) -> asr_resp().
-spec asr_commands(ne_binary(), ne_binaries(), ne_binary(), ne_binary(), kz_proplist()) -> asr_resp().
-spec asr_commands(ne_binary(), ne_binary(), ne_binaries(), ne_binary(), ne_binary(), kz_proplist()) -> provider_return().
asr_commands(Bin, Commands) ->
    asr_commands(Bin, Commands, <<"application/wav">>).
asr_commands(Bin, Commands, ContentType) ->
    asr_commands(Bin, Commands, ContentType, <<"en-US">>).
asr_commands(Bin, Commands, ContentType, Locale) ->
    asr_commands(Bin, Commands, ContentType, Locale, []).
asr_commands(Bin, Commands, ContentType, Locale, Options) ->
    Provider = kapps_config:get_binary(?MOD_CONFIG_CAT, <<"asr_provider">>, <<"ispeech">>),
    case asr_commands(Provider, Bin, Commands, ContentType, Locale, Options) of
        {'error', _R}=E ->
            lager:debug("asr failed with error ~p", [_R]),
            E;
        {'http_req_id', ReqID} ->
            lager:debug("streaming response ~p to provided option: ~p", [ReqID, props:get_value(receiver, Options)]),
            {'ok', ReqID};
        {'ok', 200, _Headers, Content} ->
            lager:debug("asr of media succeeded: ~s", [Content]),
            {'ok', kz_json:decode(Content)};
        {'ok', _Code, _Hdrs, Content} ->
            lager:debug("asr of media failed with code ~s", [_Code]),
            lager:debug("resp: ~s", [Content]),
            {'error', 'asr_provider_failure', kz_json:decode(Content)}
    end.

asr_commands(<<"ispeech">>, Bin, Commands, ContentType, Locale, Opts) ->
    BaseUrl = kapps_config:get_string(?MOD_CONFIG_CAT, <<"asr_url">>, <<"http://api.ispeech.org/api/json">>),

    Commands1 = kz_util:join_binary(Commands, <<"|">>),

    lager:debug("sending request to ~s", [BaseUrl]),

    Props = [{<<"apikey">>, kapps_config:get_binary(?MOD_CONFIG_CAT, <<"asr_api_key">>, <<>>)}
             ,{<<"action">>, <<"recognize">>}
             ,{<<"alias">>, <<"command1|YESNOMAYBE">>}
             ,{<<"YESNOMAYBE">>, Commands1}
             ,{<<"command1">>, <<"say %YESNOMAYBE%">>}
             ,{<<"content-type">>, ContentType}
             ,{<<"output">>, <<"json">>}
             ,{<<"locale">>, Locale}
             ,{<<"audio">>, base64:encode(Bin)}
            ],
    Headers = [{"Content-Type", "application/json"}],

    Body = kz_json:encode(kz_json:from_list(Props)),
    lager:debug("req body: ~s", [Body]),

    case props:get_value('receiver', Opts) of
        Pid when is_pid(Pid) ->
            HTTPOptions = props:delete('receiver', Opts),
            kz_http:async_req(Pid, 'post', BaseUrl, Headers, Body, HTTPOptions);
        _ ->
            HTTPOptions = props:delete('receiver', Opts),
            kz_http:post(BaseUrl, Headers, Body, HTTPOptions)
    end;
asr_commands(_, _, _, _, _, _) ->
    {'error', 'unknown_provider'}.

-spec create_response(ne_binary(), kz_http:ret()) ->
                             kz_http:req_id() |
                             {'ok', ne_binary(), ne_binary()} |
                             {'error', 'tts_provider_failure', binary()}.
create_response(_Engine, {'error', _R}) ->
    lager:warning("creating speech file failed with error ~p", [_R]),
    {'error', 'tts_provider_failure', <<"unexpected error encountered accessing provider">>};
create_response(_Engine, {'http_req_id', ReqID}) ->
    lager:debug("speech file streaming as ~p", [ReqID]),
    {'ok', ReqID};
create_response(<<"voicefabric">> = _Engine, {'ok', 200, Headers, Content}) ->
    _ = [lager:debug("hdr: ~p", [H]) || H <- Headers],
    lager:debug("converting media"),
    {'ok', Rate} = voicefabric_get_media_rate(Headers),
    RawFile = tmp_file_name(<<"raw">>),
    WavFile = tmp_file_name(<<"wav">>),
    kz_util:write_file(RawFile, Content),
    From = ["raw -r ", Rate, " -e signed-integer -b 16"],
    To = "wav",
    Cmd = binary_to_list(iolist_to_binary(["sox -t ", From, " ", RawFile, " -t ", To, " ", WavFile])),
    lager:debug("os cmd: ~ts", [Cmd]),
    CmdOut = os:cmd(Cmd),
    CmdOut =:= [] orelse lager:debug("cmd out: ~ts", [CmdOut]),
    kz_util:delete_file(RawFile),
    lager:debug("reading file"),
    case file:read_file(WavFile) of
        {'ok', WavContent} ->
            kz_util:delete_file(WavFile),
            lager:debug("media converted"),
            NewHeaders = props:set_values([{"Content-Type", "audio/wav"}
                                           ,{"Content-Length", integer_to_list(byte_size(WavContent))}
                                          ]
                                          ,Headers
                                         ),
            lager:debug("corrected headers"),
            create_response(<<"default">>, {'ok', 200, NewHeaders, WavContent});
        {'error', _Reason} ->
            lager:debug("failed: ~p", [_Reason]),
            {'error', 'tts_provider_failure', <<"converting failed">>}
    end;
create_response(_Engine, {'ok', 200, Headers, Content}) ->
    ContentType = props:get_value("Content-Type", Headers),
    ContentLength = props:get_value("Content-Length", Headers),
    lager:debug("created speech file ~s of length ~s", [ContentType, ContentLength]),
    {'ok', kz_util:to_binary(ContentType), Content};
create_response(Engine, {'ok', _Code, RespHeaders, Content}) ->
    lager:warning("creating speech file failed with code ~p: ~p", [_Code, Content]),
    _ = [lager:debug("hdr: ~p", [H]) || H <- RespHeaders],
    {'error', 'tts_provider_failure', create_error_response(Engine, RespHeaders, Content)}.

-spec voicefabric_get_media_rate(kz_proplist()) -> {'ok', ne_binary()}.
voicefabric_get_media_rate(Headers1) ->
    Headers = [{kz_util:to_lower_binary(X), kz_util:to_binary(Y)}
               || {X, Y} <- Headers1
              ],
    case props:get_value(<<"content-type">>, Headers) of
        <<"audio/raw; ", Params/binary>> ->
            [<<"rate=", Rate/binary>>] =
                lists:filter(fun voicefabric_filter_rate/1
                             ,re:split(Params, "; ")
                            ),
            {'ok', Rate}
    end.

-spec voicefabric_filter_rate(ne_binary()) -> boolean().
voicefabric_filter_rate(<<"rate=", _/binary>>) -> 'true';
voicefabric_filter_rate(_)                     -> 'false'.

-spec create_error_response(ne_binary(), kz_proplist(), binary()) -> binary().
create_error_response(<<"ispeech">>, _RespHeaders, Content) ->
    kz_json:get_value(<<"message">>, kz_json:decode(Content));
create_error_response(_Engine, _RespHeaders, Content) ->
    Content.

-spec convert_content(binary(), ne_binary(), ne_binary()) -> binary() | 'error'.
convert_content(Content, <<"audio/mpeg">>, <<"application/wav">> = _ContentType) ->
    Mp3File = tmp_file_name(<<"mp3">>),
    WavFile = tmp_file_name(<<"wav">>),
    kz_util:write_file(Mp3File, Content),
    Cmd = io_lib:format("lame --decode ~s ~s &> /dev/null && echo -n \"success\"", [Mp3File, WavFile]),
    _ = os:cmd(Cmd),
    kz_util:delete_file(Mp3File),
    case file:read_file(WavFile) of
        {'ok', WavContent} ->
            kz_util:delete_file(WavFile),
            WavContent;
        {'error', _R} ->
            lager:info("unable to convert mpeg to wav: ~p", [_R]),
            'error'
    end;
convert_content(_, ContentType, ConvertTo) ->
    lager:info("unsupported conversion from %s to %s", [ContentType, ConvertTo]),
    'error'.

-spec tmp_file_name(ne_binary()) -> string().
tmp_file_name(Ext) ->
    Prefix = kz_util:rand_hex_binary(10),
    Name = filename:join([?TMP_PATH
                          ,<<Prefix/binary, "_voicemail.", Ext/binary>>
                         ]),
    kz_util:to_list(Name).
