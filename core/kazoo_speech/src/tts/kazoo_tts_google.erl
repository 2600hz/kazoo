%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-, 2600Hz
%%% @doc
%%% @author Remco van Vugt <r.vanvugt@raffel.nl>
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_tts_google).

-behaviour(gen_tts_provider).

-export([create/4
        ,set_api_key/1
        ]).

-include("kazoo_speech.hrl").

-define(GOOGLE_VOICE_MAPPINGS
       ,[{<<"female/en-us">>, <<"en-US-Standard-C">>}
        ,{<<"male/en-us">>, <<"en-US-Standard-D">>}
        ,{<<"female/en-gb">>, <<"en-GB-Standard-C">>}
        ,{<<"male/en-gb">>, <<"en-GB-Standard-D">>}
        ,{<<"female/de-de">>, <<"de-DE-Standard-A">>}
        ,{<<"male/de-de">>, <<"de-DE-Standard-B">>}
        ,{<<"female/fr-fr">>, <<"fr-FR-Standard-C">>}
        ,{<<"male/fr-fr">>, <<"fr-FR-Standard-D">>}
        ,{<<"female/nl-nl">>, <<"nl-NL-Standard-A">>}
        ]
       ).

-define(GOOGLE_VOICE_LANGUAGECODES
       ,[{<<"female/en-us">>, <<"en-US">>}
        ,{<<"male/en-us">>, <<"en-US">>}
        ,{<<"female/en-gb">>, <<"en-GB">>}
        ,{<<"male/en-gb">>, <<"en-GB">>}
        ,{<<"female/de-de">>, <<"de-DE">>}
        ,{<<"male/de-de">>, <<"de-DE">>}
        ,{<<"female/fr-fr">>, <<"fr-FR">>}
        ,{<<"male/fr-fr">>, <<"fr-FR">>}
        ,{<<"female/nl-nl">>, <<"nl-NL">>}
        ]
       ).

-define(GOOGLE_TTS_URL, kapps_config:get_string(?MOD_CONFIG_CAT, <<"tts_url_google">>, <<"https://texttospeech.googleapis.com/v1beta1/text:synthesize">>)).
-define(GOOGLE_API_KEY, kapps_config:get_string(?MOD_CONFIG_CAT, <<"tts_api_key">>, <<>>)).

-spec set_api_key(ne_binary()) -> 'ok'.
set_api_key(Key) ->
    {'ok', _} = kapps_config:set_default(?MOD_CONFIG_CAT, <<"tts_api_key">>, Key),
    'ok'.

-spec create(ne_binary(), ne_binary(), ne_binary(), kz_proplist()) -> create_resp().
create(Text, Voice, Format, Options) ->
    VoiceMappings = ?GOOGLE_VOICE_MAPPINGS,
    case props:get_value(kz_term:to_lower_binary(Voice), VoiceMappings) of
        'undefined' ->
            {'error', 'invalid_voice'};
        GoogleVoice ->
            LanguageCode = props:get_value(kz_term:to_lower_binary(Voice), ?GOOGLE_VOICE_LANGUAGECODES),
            make_request(Text, GoogleVoice, LanguageCode, Format, Options)
    end.

-spec make_request(ne_binary(), ne_binary(), ne_binary(), ne_binary(), kz_proplist()) -> create_resp().
make_request(Text, GoogleVoice, LanguageCode, _Format, Options) ->


    ApiKey = ?GOOGLE_API_KEY,
    Url = ?GOOGLE_TTS_URL,
    BaseUrl = lists:flatten([Url, "?key=", ApiKey]),

    Props = [{<<"audioConfig">>, kz_json:from_list([{<<"audioEncoding">>, <<"LINEAR16">> }
                                                   ,{<<"pitch">>, <<"0.00">>}
                                                   ,{<<"speakingRate">>, <<"0.85">>}
                                                   ])}
            ,{<<"input">>, kz_json:from_list([{<<"text">>, Text}])}
            ,{<<"voice">>, kz_json:from_list([{<<"languageCode">>, LanguageCode}
                                             ,{<<"name">>, GoogleVoice}
                                             ])}
            ],       
    Headers = [{"Content-Type", "application/json"}],
    Body = kz_json:encode(kz_json:from_list(Props)),
    lager:info("sending TTS request to ~s", [BaseUrl]),

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
                             {'ok', ne_binary(), ne_binary()} |
                             {'error', 'tts_provider_failure', binary()}.
create_response({'error', _R}) ->
    lager:warning("creating speech file failed with error ~p", [_R]),
    {'error', 'tts_provider_failure', <<"unexpected error encountered accessing provider">>};
create_response({'http_req_id', ReqID}) ->
    lager:debug("speech file streaming as ~p", [ReqID]),
    {'ok', ReqID};
create_response({'ok', 200, Headers, Content}) ->
    ContentLength = props:get_value("content-length", Headers),

    % Google returns JSON with base64 encoded audio in response
    ContentType = "audio/wav",
    lager:debug("created speech file ~s of length ~s", [ContentType, ContentLength]),
    AudioContent = base64:decode(kz_json:get_value(<<"audioContent">>, kz_json:decode(Content))),   

    {'ok', kz_term:to_binary(ContentType), AudioContent};
create_response({'ok', _Code, RespHeaders, Content}) ->
    lager:warning("creating speech file failed with code ~p: ~p", [_Code, Content]),
    _ = [lager:info("hdr: ~p", [H]) || H <- RespHeaders],
    {'error', 'tts_provider_failure', kz_json:get_value(<<"message">>, kz_json:decode(Content))}.
