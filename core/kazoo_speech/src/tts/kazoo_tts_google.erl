%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_tts_google).

-behaviour(gen_tts_provider).

-export([create/4
        ,decode/3
        ,get_supported_voices/0
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
    [#voice_desc{voice_name = <<"en-US-Wavenet-C">>
                ,language_code = <<"en-US">>
                ,gender = 'female'
                }
    ].

-spec create(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> create_resp().
create(Text, Voice, Format, Options) ->
    case props:get_value(kz_term:to_lower_binary(Format), ?GOOGLE_FORMAT_MAPPINGS) of
        'undefined' ->
            {'error', 'invalid_format'};
        GoogleFormat ->
            ContentType = props:get_value(GoogleFormat, ?GOOGLE_TO_CONTENT_TYPE_MAPPINGS),
            EngineData = kz_json:from_list([{<<"content_type">>, ContentType}]),
            MappedVoice = get_mapped_voice(Voice),
            make_request(Text, MappedVoice, GoogleFormat, Options, EngineData)
    end.

-type mapped_voice() :: {kz_term:ne_binary(), kz_term:ne_binary()} |
                        'undefined'.
-spec get_mapped_voice(kz_term:ne_binary()) -> mapped_voice().
get_mapped_voice(Voice) ->
    case lists:keyfind(Voice, #voiceDesc.name, ?GOOGLE_TTS_VOICE_MAPPINGS) of
        #voiceDesc{languageCodes=[Code|_]} -> {Voice, Code};
        'false' ->
            get_mapped_voice(Voice, binary:match(Voice, <<"/">>))
    end.

-spec get_mapped_voice(kz_term:ne_binary(), 'nomatch' | tuple()) ->
                              mapped_voice().
get_mapped_voice(_Voice, 'nomatch') -> 'undefined';
get_mapped_voice(Voice, _Match) ->
    case get_matched_voice(Voice) of
        [] -> 'undefined';
        [#voiceDesc{name=Name, languageCodes=[Code|_]} | _] -> {Name, Code}
    end.

-spec get_matched_voice(kz_term:ne_binary()) -> [#voiceDesc{}].
get_matched_voice(Voice) ->
    [Gender, Language] = binary:split(kz_term:to_lower_binary(Voice), <<"/">>),

    lists:filter(fun(VoiceDesc) -> should_filter_voice(VoiceDesc, Gender, Language) end
                ,?GOOGLE_TTS_VOICE_MAPPINGS
                ).

should_filter_voice(#voiceDesc{ssmlGender=SSMLGender}=VoiceDesc, Gender, Language) ->
    case kz_term:to_lower_binary(SSMLGender) =:= Gender of
        'true' -> should_filter_language(VoiceDesc, Language);
        'false' -> 'false'
    end.

should_filter_language(#voiceDesc{languageCodes=LangCodes}, Language) ->
    lists:any(fun(LangCode) -> Language =:= kz_term:to_lower_binary(LangCode) end
             ,LangCodes
             ).

-spec make_request(kz_term:ne_binary(), mapped_voice(), audioEncoding(), kz_term:proplist(), kz_json:object()) -> create_resp().
make_request(_Text, 'undefined', _GoogleFormat, _Options, _EngineData) ->
    {'error', 'tts_voice_not_exist', <<"specified voice is not supported by Google TTS engine">>};
make_request(Text, {Voice, Language}, GoogleFormat, Options, EngineData) ->
    BaseUrl = ?GOOGLE_TTS_URL,
    Headers = req_headers(),

    Body = build_request_body(Text, Voice, Language, GoogleFormat),

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

-spec req_headers() -> kz_http:headers().
req_headers() ->
    [{"Content-Type", "application/json; charset=UTF-8"}
    ,{"X-Goog-Api-Key", ?GOOGLE_TTS_KEY}
    ,{"User-Agent", kz_term:to_list(node())}
    ].

-spec build_request_body(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), audioEncoding()) ->
                                kz_term:ne_binary().
build_request_body(Text, Voice, Language, GoogleFormat) ->
    Req = kz_json:set_values([{<<"input">>, kazoo_tts_google_utils:getJson(#synthesisInput{text=Text})}
                             ,{<<"voice">>, kazoo_tts_google_utils:getJson(#voiceSelectionParams{languageCode=Language, name=Voice})}
                             ,{<<"audioConfig">>, kazoo_tts_google_utils:getJson(#audioConfig{audioEncoding=GoogleFormat})}
                             ]
                            ,kz_json:new()
                            ),
    kz_json:encode(Req).

-spec decode(kz_term:ne_binary(), kz_json:object(), kz_json:object()) -> {kz_term:ne_binary(), kz_json:object()}.
decode(Contents, RequestMeta, EngineData) ->
    ContentType = kz_json:get_binary_value(<<"content_type">>, EngineData),
    NewRequestMeta = kz_json:set_value(<<"content_type">>, ContentType, RequestMeta),
    {decode(Contents), NewRequestMeta}.

-spec decode(kz_term:ne_binary()) -> kz_term:ne_binary().
decode(Contents) ->
    Base64Result = kz_json:get_binary_value(<<"audioContent">>, kz_json:decode(Contents)),
    base64:decode(Base64Result).

-spec create_response(kz_http:ret(), kz_json:object()) -> create_resp().
create_response({'error', _R}, _EngineData) ->
    lager:warning("creating speech file failed with error ~p", [_R]),
    {'error', 'tts_provider_failure', <<"unexpected error encountered accessing provider">>};
create_response({'http_req_id', ReqID}, EngineData) ->
    lager:debug("speech file streaming as ~p", [ReqID]),
    {'async', ReqID, EngineData};
create_response({'ok', 200, _Headers, Content}, EngineData) ->
    ContentType = kz_json:get_binary_value(<<"content_type">>, EngineData),
    lager:debug("created speech file ~s", [ContentType]),
    {'ok', ContentType, decode(Content)};
create_response({'ok', _Code, _RespHeaders, Content}, _EngineData) ->
    lager:warning("creating speech file failed with code ~p: ~p", [_Code, Content]),
    _ = [lager:debug("hdr: ~p", [H]) || H <- _RespHeaders],
    {'error', 'tts_provider_failure', kz_json:get_value([<<"error">>, <<"message">>], kz_json:decode(Content))}.
