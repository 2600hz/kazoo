-module(kazoo_tts_google).

-behaviour(gen_tts_provider).

-export([create/4
        ,get_supported_voices/0
        ,decode_responce/3
        ,set_api_key/1
        ]).

-include("kazoo_speech.hrl").
-include("kazoo_tts_google.hrl").

-spec set_api_key(kz_term:ne_binary()) -> 'ok'.
set_api_key(Key) ->
    {'ok', _} = kapps_config:set_default(?MOD_CONFIG_CAT, <<"tts_api_key">>, Key),
    'ok'.

-spec get_supported_voices() -> [voice_desc()].
get_supported_voices() ->
    [#voice_desc{voice_name='en-US-Wavenet-C'
                ,language_code='en-US'
                ,gender='female'
                }].

-spec create(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> create_resp().
create(Text, Voice, Format, Options) ->
    case props:get_value(kz_term:to_lower_binary(Format), ?GOOGLE_FORMAT_MAPPINGS) of
        'undefined' ->
            {'error', 'invalid_format'};
        GoogleFormat ->
            ContentType = props:get_value(GoogleFormat, ?GOOGLE_TO_CONTENT_TYPE_MAPPINGS),
            EngineData = kz_json:set_value(<<"content_type">>, ContentType, []),
            MappedVoice = getMappedVoice(Voice),
            make_request(Text, MappedVoice, GoogleFormat, Options, EngineData)
    end.

-spec getMappedVoice(kz_term:ne_binary()) -> kz_term:api_binary().
getMappedVoice(Voice) ->
    case lists:keymember(Voice, #voiceDesc.name, ?GOOGLE_TTS_VOICE_MAPPINGS)
        orelse binary:match(Voice, <<"/">>) of
        true -> Voice;
        'nomatch' -> {'error'};
        _ ->
            [Gender, Language] = binary:split(kz_term:to_lower_binary(Voice), <<"/">>),
            VoiceFiltered = lists:filter(fun(VoiceDesc) ->
                                                 case kz_term:to_lower_binary(VoiceDesc#voiceDesc.ssmlGender) == Gender
                                                     andalso lists:filter(fun(LangCode) -> Language == kz_term:to_lower_binary(LangCode) end , VoiceDesc#voiceDesc.languageCodes) of
                                                     false -> false;
                                                     [] -> false;
                                                     [_] -> true
                                                 end
                                         end, ?GOOGLE_TTS_VOICE_MAPPINGS),
            case VoiceFiltered of
                [] -> 'undefined';
                [VoiceToReturn | _] -> VoiceToReturn#voiceDesc.name
            end
    end.

-spec make_request(kz_term:ne_binary(), kz_term:ne_binary(), audioEncoding(), kz_term:proplist(), kz_json:object()) -> create_resp().
make_request(_Text, 'undefined', _GoogleFormat, _Options, _EngineData) ->
    {'error', 'tts_voice_not_exist', <<"specified voice is not supported by Google TTS engine">>};
make_request(Text, Voice, GoogleFormat, Options, EngineData) ->
    BaseUrl = ?GOOGLE_TTS_URL,
    Headers = [{"Content-Type", "application/json; charset=UTF-8"}
              ,{"X-Goog-Api-Key", ?GOOGLE_TTS_KEY}
              ,{"User-Agent", kz_term:to_list(node())}
              ],
    VoiceDesc = lists:keyfind(Voice, #voiceDesc.name, ?GOOGLE_TTS_VOICE_MAPPINGS),
    [LanguageCode | _] = VoiceDesc#voiceDesc.languageCodes,

    Req = kz_json:from_list([{<<"input">>, kazoo_tts_google_utils:getJson(#synthesisInput{text=Text})}
                            ,{<<"voice">>, kazoo_tts_google_utils:getJson(#voiceSelectionParams{languageCode=LanguageCode, name=Voice})}
                            ,{<<"audioConfig">>, kazoo_tts_google_utils:getJson(#audioConfig{audioEncoding=GoogleFormat})}
                            ]),
    Body = kz_json:encode(Req),

    lager:debug("sending TTS request to ~s", [BaseUrl]),

    HTTPOptions = props:delete('receiver', Options),
    case props:get_value('receiver', Options) of
        Pid when is_pid(Pid) ->
            Response = kz_http:async_req(Pid, 'post', BaseUrl, Headers, Body, HTTPOptions),
            create_response(Response, EngineData);
        _ ->
            Response = kz_http:post(BaseUrl, Headers, Body, HTTPOptions),
            create_response(Response, EngineData)
    end.

-spec decode_responce(kz_term:ne_binary(), kz_json:object()) -> {kz_term:ne_binary(), kz_term:ne_binary()}.
decode_responce(Content, EngineData) ->
    {BinaryContent, NewRequestMeta} = decode_responce(Content, EngineData, []),
    ContentType = kz_json:get_binary_value(<<"content_type">>, NewRequestMeta),
    {BinaryContent, ContentType}.

-spec decode_responce(kz_term:ne_binary(), kz_json:object(), kz_json:object()) -> {kz_term:ne_binary(), kz_json:object()}.
decode_responce(Content, EngineData, RequestMeta) ->
    Base64Result = kz_json:get_binary_value(<<"audioContent">>, kz_json:decode(Content)),
    ContentType = kz_json:get_binary_value(<<"content_type">>, EngineData),
    NewRequestMeta = kz_json:set_value(<<"content_type">>, ContentType, RequestMeta),
    {base64:decode(Base64Result), NewRequestMeta}.

-spec create_response(kz_http:ret(), kz_json:object()) ->
                             kz_http:req_id() |
                             {'ok', kz_term:ne_binary(), kz_term:ne_binary()} |
                             {'error', 'tts_provider_failure', binary()}.
create_response({'error', _R}, _EngineData) ->
    lager:warning("creating speech file failed with error ~p", [_R]),
    {'error', 'tts_provider_failure', <<"unexpected error encountered accessing provider">>};
create_response({'http_req_id', ReqID}, EngineData) ->
    lager:debug("speech file streaming as ~p", [ReqID]),
    {'ok', ReqID, EngineData};
create_response({'ok', 200, _Headers, Content}, EngineData) ->
    {BinaryContent, ContentType} = decode_responce(Content, EngineData),
    lager:debug("created speech file ~s", [ContentType]),
    {'ok', kz_term:to_binary(ContentType), BinaryContent};
create_response({'ok', _Code, RespHeaders, Content}, _EngineData) ->
    lager:warning("creating speech file failed with code ~p: ~p", [_Code, Content]),
    _ = [lager:debug("hdr: ~p", [H]) || H <- RespHeaders],
    {'error', 'tts_provider_failure', kz_json:get_value([<<"error">>, <<"message">>], kz_json:decode(Content))}.
