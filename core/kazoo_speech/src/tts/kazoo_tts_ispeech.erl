%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_tts_ispeech).

-behaviour(gen_tts_provider).

-export([create/4
        ,set_api_key/1
        ]).

-include("kazoo_speech.hrl").

-define(ISPEECH_VOICE_MAPPINGS
       ,[{<<"female/en-us">>, <<"usenglishfemale">>}
        ,{<<"male/en-us">>, <<"usenglishmale">>}
        ,{<<"female/en-ca">>, <<"caenglishfemale">>}
        ,{<<"female/en-au">>, <<"auenglishfemale">>}
        ,{<<"female/en-gb">>, <<"ukenglishfemale">>}
        ,{<<"male/en-gb">>, <<"ukenglishmale">>}
        ,{<<"female/es-us">>, <<"usspanishfemale">>}
        ,{<<"male/es-us">>, <<"usspanishmale">>}
        ,{<<"female/us-us">>, <<"usspanishfemale">>}
        ,{<<"female/zh-cn">>, <<"chchinesefemale">>}
        ,{<<"male/zh-cn">>, <<"chchinesemale">>}
        ,{<<"female/zh-hk">>, <<"hkchinesefemale">>}
        ,{<<"female/zh-tw">>, <<"twchinesefemale">>}
        ,{<<"female/ja-jp">>, <<"jpjapanesefemale">>}
        ,{<<"male/ja-jp">>, <<"jpjapanesemale">>}
        ,{<<"female/ko-kr">>, <<"krkoreanfemale">>}
        ,{<<"male/ko-kr">>, <<"krkoreanmale">>}
        ,{<<"female/da-dk">>, <<"eurdanishfemale">>}
        ,{<<"female/de-de">>, <<"eurgermanfemale">>}
        ,{<<"male/de-de">>, <<"eurgermanmale">>}
        ,{<<"female/ca-es">>, <<"eurcatalanfemale">>}
        ,{<<"female/es-es">>, <<"eurspanishfemale">>}
        ,{<<"male/es-es">>, <<"eurspanishmale">>}
        ,{<<"female/fi-fi">>, <<"eurfinnishfemale">>}
        ,{<<"female/fr-ca">>, <<"cafrenchfemale">>}
        ,{<<"male/fr-ca">>, <<"cafrenchmale">>}
        ,{<<"female/fr-fr">>, <<"eurfrenchfemale">>}
        ,{<<"male/fr-fr">>, <<"eurfrenchmale">>}
        ,{<<"female/it-it">>, <<"euritalianfemale">>}
        ,{<<"male/it-it">>, <<"euritalianmale">>}
        ,{<<"female/nb-no">>, <<"eurnorwegianfemale">>}
        ,{<<"female/nl-nl">>, <<"eurdutchfemale">>}
        ,{<<"female/pl-pl">>, <<"eurpolishfemale">>}
        ,{<<"female/pt-br">>, <<"brportuguesefemale">>}
        ,{<<"female/pt-pt">>, <<"eurportuguesefemale">>}
        ,{<<"male/pt-pt">>, <<"eurportuguesemale">>}
        ,{<<"female/ru-ru">>, <<"rurussianfemale">>}
        ,{<<"male/ru-ru">>, <<"rurussianmale">>}
        ,{<<"female/sv-se">>, <<"swswedishfemale">>}
        ,{<<"female/hu-hu">>, <<"huhungarianfemale">>}
        ,{<<"female/cs-cz">>, <<"eurczechfemale">>}
        ,{<<"female/tr-tr">>, <<"eurturkishfemale">>}
        ,{<<"male/tr-tr">>, <<"eurturkishmale">>}
        ]
       ).
-define(ISPEECH_TTS_URL, kapps_config:get_string(?MOD_CONFIG_CAT, <<"tts_url_ispeech">>, <<"http://api.ispeech.org/api/json">>)).

-spec set_api_key(kz_term:ne_binary()) -> 'ok'.
set_api_key(Key) ->
    {'ok', _} = kapps_config:set_default(?MOD_CONFIG_CAT, <<"tts_api_key">>, Key),
    'ok'.

-spec create(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> create_resp().
create(Text, Voice, Format, Options) ->
    VoiceMappings = ?ISPEECH_VOICE_MAPPINGS,
    case props:get_value(kz_term:to_lower_binary(Voice), VoiceMappings) of
        'undefined' ->
            {'error', 'invalid_voice'};
        ISpeechVoice ->
            make_request(Text, ISpeechVoice, Format, Options)
    end.

-spec make_request(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> create_resp().
make_request(Text, ISpeechVoice, Format, Options) ->
    BaseUrl = ?ISPEECH_TTS_URL,

    Props = [{<<"text">>, Text}
            ,{<<"voice">>, ISpeechVoice}
            ,{<<"format">>, Format}
            ,{<<"action">>, <<"convert">>}
            ,{<<"apikey">>, ?TTS_API_KEY}
            ,{<<"speed">>, kapps_config:get_integer(?MOD_CONFIG_CAT, <<"tts_speed">>, 0)}
            ,{<<"startpadding">>, kapps_config:get_integer(?MOD_CONFIG_CAT, <<"tts_start_padding">>, 1)}
            ,{<<"endpadding">>, kapps_config:get_integer(?MOD_CONFIG_CAT, <<"tts_end_padding">>, 0)}
            ],
    Headers = [{"Content-Type", "application/json; charset=UTF-8"}],
    Body = kz_json:encode(kz_json:from_list(Props)),

    lager:debug("sending TTS request to ~s", [BaseUrl]),

    HTTPOptions = props:delete('receiver', Options),
    case props:get_value('receiver', Options) of
        Pid when is_pid(Pid) ->
            Response = kz_http:async_req(Pid, 'post', BaseUrl, Headers, Body, HTTPOptions),
            create_response(Response);
        _ ->
            Response = kz_http:post(BaseUrl, Headers, Body, HTTPOptions),
            create_response(Response)
    end.

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
    ContentType = props:get_value("content-type", Headers),
    ContentLength = props:get_value("content-length", Headers),
    lager:debug("created speech file ~s of length ~s", [ContentType, ContentLength]),
    {'ok', kz_term:to_binary(ContentType), Content};
create_response({'ok', _Code, RespHeaders, Content}) ->
    lager:warning("creating speech file failed with code ~p: ~p", [_Code, Content]),
    _ = [lager:debug("hdr: ~p", [H]) || H <- RespHeaders],
    {'error', 'tts_provider_failure', kz_json:get_value(<<"message">>, kz_json:decode(Content))}.
