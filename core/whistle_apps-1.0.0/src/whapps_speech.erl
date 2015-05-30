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
-module(whapps_speech).

-include("whistle_apps.hrl").
-include("whapps_speech.hrl").

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
                           ibrowse_ret() |
                           {'ibrowse_req_id', ibrowse_req_id()}.
-type create_resp() :: {'ok', ibrowse_req_id()} |
                       provider_return() |
                       {'ok', ne_binary(), ne_binary()} | %% {'ok', ContentType, BinaryData}
                       {'error', 'tts_provider_failure', binary()}.

-type asr_resp() :: {'ok', ibrowse_req_id()} |
                    {'ok', wh_json:object()} | %% {'ok', JObj}
                    {'error', provider_errors()} |
                    {'error',  'asr_provider_failure', wh_json:object()}.

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

-spec create(ne_binary(), ne_binary(), ne_binary(), wh_proplist()) -> create_resp().
create(Text, Voice, Format, Options) ->
    Provider = whapps_config:get_binary(?MOD_CONFIG_CAT, <<"tts_provider">>, <<"ispeech">>),
    create(Provider, Text, Voice, Format, Options).

-spec create(api_binary(), ne_binary(), ne_binary(), ne_binary(), wh_proplist()) -> create_resp().
create('undefined', Text, Voice, Format, Options) ->
    create(Text, Voice, Format, Options);
create(<<"ispeech">> = Engine, Text, Voice, Format, Options) ->
    VoiceMappings = ?ISPEECH_VOICE_MAPPINGS,
    case props:get_value(wh_util:to_lower_binary(Voice), VoiceMappings) of
        'undefined' ->
            {'error', 'invalid_voice'};
        ISpeechVoice ->
            BaseUrl = ?ISPEECH_TTS_URL,

            Props = [{<<"text">>, Text}
                     ,{<<"voice">>, ISpeechVoice}
                     ,{<<"format">>, Format}
                     ,{<<"action">>, <<"convert">>}
                     ,{<<"apikey">>, whapps_config:get_binary(?MOD_CONFIG_CAT, <<"tts_api_key">>, <<>>)}
                     ,{<<"speed">>, whapps_config:get_integer(?MOD_CONFIG_CAT, <<"tts_speed">>, 0)}
                     ,{<<"startpadding">>, whapps_config:get_integer(?MOD_CONFIG_CAT, <<"tts_start_padding">>, 1)}
                     ,{<<"endpadding">>, whapps_config:get_integer(?MOD_CONFIG_CAT, <<"tts_end_padding">>, 0)}
                    ],
            Headers = [{"Content-Type", "application/json; charset=UTF-8"}],
            HTTPOptions = [{'response_format', 'binary'} | Options],
            Body = wh_json:encode(wh_json:from_list(Props)),

            lager:debug("sending TTS request to ~s", [BaseUrl]),

            Response = ibrowse:send_req(BaseUrl, Headers, 'post', Body, HTTPOptions),
            create_response(Engine, Response)
    end;
create(<<"voicefabric">> = Engine, Text, Voice, <<"wav">> = _Format, Options) ->
    create_voicefabric(Engine, Text, Voice, Options);
create(_, _, _, _, _) ->
    {'error', 'unknown_provider'}.

-spec create_voicefabric(ne_binary(), binary(), ne_binary(), wh_proplist()) ->
                                create_resp().
create_voicefabric(Engine, Text, Voice, Options) ->
    lager:debug("getting ~ts from VoiceFabric", [Text]),
    VoiceMappings = ?VOICEFABRIC_VOICE_MAPPINGS,
    case props:get_value(wh_util:to_lower_binary(Voice), VoiceMappings) of
        'undefined' ->
            {'error', 'invalid_voice'};
        VFabricVoice ->
            BaseUrl = ?VOICEFABRIC_TTS_URL,
            ApiKey = whapps_config:get_binary(?MOD_CONFIG_CAT, <<"tts_api_key">>),
            Data = [{<<"apikey">>, ApiKey}
                    ,{<<"ttsVoice">>, VFabricVoice}
                    ,{<<"textFormat">>, <<"text/plain">>}
                    ,{<<"text">>, Text}
                   ],
            ArgsEncode = whapps_config:get_binary(?MOD_CONFIG_CAT, <<"tts_args_encode">>, <<"multipart">>),
            case voicefabric_request_body(ArgsEncode, Data) of
                {'ok', Headers, Body} ->
                    HTTPOptions = [{'response_format', 'binary'} | Options],
                    lager:debug("sending ~ts", [Body]),
                    Response = ibrowse:send_req(BaseUrl, Headers, 'post', Body, HTTPOptions),
                    create_response(Engine, Response);
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
                                 ,couch_mgr:get_uuid()
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
-spec asr_freeform(binary(), ne_binary(), ne_binary(), wh_proplist()) -> asr_resp().
asr_freeform(Content) ->
    asr_freeform(Content, <<"application/wav">>).
asr_freeform(Content, ContentType) ->
    asr_freeform(Content, ContentType, <<"en-US">>).
asr_freeform(Content, ContentType, Locale) ->
    asr_freeform(Content, ContentType, Locale, []).
asr_freeform(Content, ContentType, Locale, Options) ->
    Provider = whapps_config:get_binary(?MOD_CONFIG_CAT, <<"asr_provider">>, <<>>),
    case wh_util:is_empty(Provider) of
        'true' -> {'error', 'no_asr_provider'};
        'false' -> maybe_convert_content(Content, ContentType, Locale, Options)
    end.

-spec maybe_convert_content(binary(), ne_binary(), ne_binary(), wh_proplist()) -> provider_return().
maybe_convert_content(Content, ContentType, Locale, Options) ->
    ContentTypes = whapps_config:get(?MOD_CONFIG_CAT
                                     ,<<"asr_content_types">>
                                     ,[<<"application/mpeg">>
                                       ,<<"application/wav">>
                                      ]),
    case lists:member(ContentType, ContentTypes) of
        'true' -> attempt_asr_freeform(Content, ContentType, Locale, Options);
        'false' ->
            ConvertTo = whapps_config:get_binary(?MOD_CONFIG_CAT
                                                 ,<<"asr_prefered_content_type">>
                                                 ,<<"application/mpeg">>
                                                ),
            case convert_content(Content, ContentType, ConvertTo) of
                'error' -> {'error', 'unsupported_content_type'};
                Converted ->
                    attempt_asr_freeform(Converted, ContentType, Locale, Options)
            end
    end.

-spec attempt_asr_freeform(binary(), ne_binary(), ne_binary(), wh_proplist()) -> provider_return().
attempt_asr_freeform(Content, ContentType, Locale, Options) ->
    Provider = whapps_config:get_binary(?MOD_CONFIG_CAT, <<"asr_provider">>, <<>>),
    case attempt_asr_freeform(Provider, Content, ContentType, Locale, Options) of
        {'error', _R}=E ->
            lager:debug("asr failed with error ~p", [_R]),
            E;
        {'ibrowse_req_id', ReqID} ->
            lager:debug("streaming response ~p to provided option: ~p", [ReqID, props:get_value(stream_to, Options)]),
            {'ok', ReqID};
        {'ok', "200", _Headers, Content} ->
            lager:debug("asr of media succeeded: ~s", [Content]),
            {'ok', wh_json:decode(Content)};
        {'ok', Code, _Hdrs, Content} ->
            lager:debug("asr of media failed with code ~s", [Code]),
            lager:debug("resp: ~s", [Content]),
            {'error', 'asr_provider_failure', wh_json:decode(Content)}
    end.

-spec attempt_asr_freeform(api_binary(), binary(), ne_binary(), ne_binary(), wh_proplist()) -> provider_return().
attempt_asr_freeform(_, <<>>, _, _, _) -> {'error', 'no_content'};
attempt_asr_freeform(<<"ispeech">>, Bin, ContentType, Locale, Options) ->
    BaseUrl = whapps_config:get_string(?MOD_CONFIG_CAT, <<"asr_url">>, <<"http://api.ispeech.org/api/json">>),
    lager:debug("sending request to ~s", [BaseUrl]),
    Props = [{<<"apikey">>, whapps_config:get_binary(?MOD_CONFIG_CAT, <<"asr_api_key">>, <<>>)}
             ,{<<"action">>, <<"recognize">>}
             ,{<<"freeform">>, <<"1">>}
             ,{<<"content-type">>, ContentType}
             ,{<<"output">>, <<"json">>}
             ,{<<"locale">>, Locale}
             ,{<<"audio">>, base64:encode(Bin)}
            ],
    Headers = [{"Content-Type", "application/json"}],
    HTTPOptions = [{'response_format', 'binary'} | Options],
    Body = wh_json:encode(wh_json:from_list(Props)),
    lager:debug("req body: ~s", [Body]),
    ibrowse:send_req(BaseUrl, Headers, 'post', Body, HTTPOptions);
attempt_asr_freeform(_, _, _, _, _) ->
    {'error', 'unknown_provider'}.

%%------------------------------------------------------------------------------
%% Transcribe the audio binary
%%------------------------------------------------------------------------------
-spec asr_commands(ne_binary(), ne_binaries()) -> asr_resp().
-spec asr_commands(ne_binary(), ne_binaries(), ne_binary()) -> asr_resp().
-spec asr_commands(ne_binary(), ne_binaries(), ne_binary(), ne_binary()) -> asr_resp().
-spec asr_commands(ne_binary(), ne_binaries(), ne_binary(), ne_binary(), wh_proplist()) -> asr_resp().
-spec asr_commands(ne_binary(), ne_binary(), ne_binaries(), ne_binary(), ne_binary(), wh_proplist()) -> provider_return().
asr_commands(Bin, Commands) ->
    asr_commands(Bin, Commands, <<"application/wav">>).
asr_commands(Bin, Commands, ContentType) ->
    asr_commands(Bin, Commands, ContentType, <<"en-US">>).
asr_commands(Bin, Commands, ContentType, Locale) ->
    asr_commands(Bin, Commands, ContentType, Locale, []).
asr_commands(Bin, Commands, ContentType, Locale, Options) ->
    Provider = whapps_config:get_binary(?MOD_CONFIG_CAT, <<"asr_provider">>, <<"ispeech">>),
    case asr_commands(Provider, Bin, Commands, ContentType, Locale, Options) of
        {'error', _R}=E ->
            lager:debug("asr failed with error ~p", [_R]),
            E;
        {'ibrowse_req_id', ReqID} ->
            lager:debug("streaming response ~p to provided option: ~p", [ReqID, props:get_value(stream_to, Options)]),
            {'ok', ReqID};
        {'ok', "200", _Headers, Content} ->
            lager:debug("asr of media succeeded: ~s", [Content]),
            {'ok', wh_json:decode(Content)};
        {'ok', Code, _Hdrs, Content} ->
            lager:debug("asr of media failed with code ~s", [Code]),
            lager:debug("resp: ~s", [Content]),
            {'error', 'asr_provider_failure', wh_json:decode(Content)}
    end.

asr_commands(<<"ispeech">>, Bin, Commands, ContentType, Locale, Options) ->
    BaseUrl = whapps_config:get_string(?MOD_CONFIG_CAT, <<"asr_url">>, <<"http://api.ispeech.org/api/json">>),

    Commands1 = wh_util:join_binary(Commands, <<"|">>),

    lager:debug("sending request to ~s", [BaseUrl]),

    Props = [{<<"apikey">>, whapps_config:get_binary(?MOD_CONFIG_CAT, <<"asr_api_key">>, <<>>)}
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
    HTTPOptions = [{'response_format', 'binary'} | Options],

    Body = wh_json:encode(wh_json:from_list(Props)),
    lager:debug("req body: ~s", [Body]),

    ibrowse:send_req(BaseUrl, Headers, 'post', Body, HTTPOptions);
asr_commands(_, _, _, _, _, _) ->
    {'error', 'unknown_provider'}.

-spec create_response(ne_binary(), ibrowse_ret()) ->
                             {'ok', ibrowse_req_id()} |
                             {'ok', ne_binary(), ne_binary()} |
                             {'error', 'tts_provider_failure', binary()}.
create_response(_Engine, {'error', _R}) ->
    lager:warning("creating speech file failed with error ~p", [_R]),
    {'error', 'tts_provider_failure', <<"unexpected error encountered accessing provider">>};
create_response(_Engine, {'ibrowse_req_id', ReqID}) ->
    lager:debug("speech file streaming as ~p", [ReqID]),
    {'ok', ReqID};
create_response(<<"voicefabric">> = _Engine, {'ok', "200", Headers, Content}) ->
    _ = [lager:debug("hdr: ~p", [H]) || H <- Headers],
    lager:debug("converting media"),
    {'ok', Rate} = voicefabric_get_media_rate(Headers),
    RawFile = tmp_file_name(<<"raw">>),
    WavFile = tmp_file_name(<<"wav">>),
    wh_util:write_file(RawFile, Content),
    From = ["raw -r ", Rate, " -e signed-integer -b 16"],
    To = "wav",
    Cmd = binary_to_list(iolist_to_binary(["sox -t ", From, " ", RawFile, " -t ", To, " ", WavFile])),
    lager:debug("os cmd: ~ts", [Cmd]),
    CmdOut = os:cmd(Cmd),
    CmdOut =:= [] orelse lager:debug("cmd out: ~ts", [CmdOut]),
    wh_util:delete_file(RawFile),
    lager:debug("reading file"),
    case file:read_file(WavFile) of
        {'ok', WavContent} ->
            wh_util:delete_file(WavFile),
            lager:debug("media converted"),
            NewHeaders = props:set_values([{"Content-Type", "audio/wav"}
                                           ,{"Content-Length", integer_to_list(byte_size(WavContent))}
                                          ]
                                          ,Headers
                                         ),
            lager:debug("corrected headers"),
            create_response(<<"default">>, {'ok', "200", NewHeaders, WavContent});
        {'error', _Reason} ->
            lager:debug("failed: ~p", [_Reason]),
            {'error', 'tts_provider_failure', <<"converting failed">>}
    end;
create_response(_Engine, {'ok', "200", Headers, Content}) ->
    ContentType = props:get_value("Content-Type", Headers),
    ContentLength = props:get_value("Content-Length", Headers),
    lager:debug("created speech file ~s of length ~s", [ContentType, ContentLength]),
    {'ok', wh_util:to_binary(ContentType), Content};
create_response(Engine, {'ok', Code, RespHeaders, Content}) ->
    lager:warning("creating speech file failed with code ~s: ~s", [Code, Content]),
    _ = [lager:debug("hdr: ~p", [H]) || H <- RespHeaders],
    {'error', 'tts_provider_failure', create_error_response(Engine, RespHeaders, Content)}.

-spec voicefabric_get_media_rate(wh_proplist()) -> {'ok', ne_binary()}.
voicefabric_get_media_rate(Headers1) ->
    Headers = [{wh_util:to_lower_binary(X), wh_util:to_binary(Y)}
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

-spec create_error_response(ne_binary(), wh_proplist(), binary()) -> binary().
create_error_response(<<"ispeech">>, _RespHeaders, Content) ->
    wh_json:get_value(<<"message">>, wh_json:decode(Content));
create_error_response(_Engine, _RespHeaders, Content) ->
    Content.

-spec convert_content(binary(), ne_binary(), ne_binary()) -> binary() | 'error'.
convert_content(Content, <<"audio/mpeg">>, <<"application/wav">> = _ContentType) ->
    Mp3File = tmp_file_name(<<"mp3">>),
    WavFile = tmp_file_name(<<"wav">>),
    wh_util:write_file(Mp3File, Content),
    Cmd = io_lib:format("lame --decode ~s ~s &> /dev/null && echo -n \"success\"", [Mp3File, WavFile]),
    _ = os:cmd(Cmd),
    wh_util:delete_file(Mp3File),
    case file:read_file(WavFile) of
        {'ok', WavContent} ->
            wh_util:delete_file(WavFile),
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
    Prefix = wh_util:rand_hex_binary(10),
    Name = filename:join([?TMP_PATH
                          ,<<Prefix/binary, "_voicemail.", Ext/binary>>
                         ]),
    wh_util:to_list(Name).
