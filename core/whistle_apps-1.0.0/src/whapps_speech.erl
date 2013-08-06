%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
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
%%%-------------------------------------------------------------------
-module(whapps_speech).

-include("whistle_apps.hrl").

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

-define(MOD_CONFIG_CAT, <<"speech">>).

-type provider_errors() :: 'invalid_voice' | 'unknown_provider'.
-type provider_return() :: {'error', provider_errors()} |
                           ibrowse_ret() |
                           {'ibrowse_req_id', ibrowse_req_id()}.
-type create_resp() :: {'ok', ibrowse_req_id()} |
                       {'ok', ne_binary(), ne_binary()} | %% {'ok', ContentType, BinaryData}
                       {'error', provider_errors() | 'tts_provider_failure'}.

-type asr_resp() :: {'ok', ibrowse_req_id()} |
                    {'ok', wh_json:object()} | %% {'ok', JObj}
                    {'error', provider_errors()} |
                    {'error',  'asr_provider_failure', wh_json:object()}.

%%------------------------------------------------------------------------------
%% Create a tts audio file using a configured provider
%%------------------------------------------------------------------------------
-spec create(ne_binary()) -> create_resp().
create(Text) ->
    create(Text, <<"female/en-US">>).

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
create(<<"ispeech">>, Text, Voice, Format, Options) ->
    VoiceMappings = [{<<"female/en-US">>, <<"usenglishfemale">>}
                     ,{<<"male/en-US">>, <<"usenglishmale">>}
                     ,{<<"female/en-CA">>, <<"caenglishfemale">>}
                     ,{<<"female/en-AU">>, <<"auenglishfemale">>}
                     ,{<<"female/en-GB">>, <<"ukenglishfemale">>}
                     ,{<<"male/en-GB">>, <<"ukenglishmale">>}
                     ,{<<"female/es-US">>, <<"usspanishfemale">>}
                     ,{<<"male/es-US">>, <<"usspanishmale">>}
                     ,{<<"female/us-US">>, <<"usspanishfemale">>}
                     ,{<<"female/zh-CN">>, <<"chchinesefemale">>}
                     ,{<<"male/zh-CN">>, <<"chchinesemale">>}
                     ,{<<"female/zh-HK">>, <<"hkchinesefemale">>}
                     ,{<<"female/zh-TW">>, <<"twchinesefemale">>}
                     ,{<<"female/ja-JP">>, <<"jpjapanesefemale">>}
                     ,{<<"male/ja-JP">>, <<"jpjapanesemale">>}
                     ,{<<"female/ko-KR">>, <<"krkoreanfemale">>}
                     ,{<<"male/ko-KR">>, <<"krkoreanmale">>}
                     ,{<<"female/da-DK">>, <<"eurdanishfemale">>}
                     ,{<<"female/de-DE">>, <<"eurgermanfemale">>}
                     ,{<<"male/de-DE">>, <<"eurgermanmale">>}
                     ,{<<"female/ca-ES">>, <<"eurcatalanfemale">>}
                     ,{<<"female/es-ES">>, <<"eurspanishfemale">>}
                     ,{<<"male/es-ES">>, <<"eurspanishmale">>}
                     ,{<<"female/fi-FI">>, <<"eurfinnishfemale">>}
                     ,{<<"female/fr-CA">>, <<"cafrenchfemale">>}
                     ,{<<"male/fr-CA">>, <<"cafrenchmale">>}
                     ,{<<"female/fr-FR">>, <<"eurfrenchfemale">>}
                     ,{<<"male/fr-FR">>, <<"eurfrenchmale">>}
                     ,{<<"female/it-IT">>, <<"euritalianfemale">>}
                     ,{<<"male/it-IT">>, <<"euritalianmale">>}
                     ,{<<"female/nb-NO">>, <<"eurnorwegianfemale">>}
                     ,{<<"female/nl-NL">>, <<"eurdutchfemale">>}
                     ,{<<"female/pl-PL">>, <<"eurpolishfemale">>}
                     ,{<<"female/pt-BR">>, <<"brportuguesefemale">>}
                     ,{<<"female/pt-PT">>, <<"eurportuguesefemale">>}
                     ,{<<"male/pt-PT">>, <<"eurportuguesemale">>}
                     ,{<<"female/ru-RU">>, <<"rurussianfemale">>}
                     ,{<<"male/ru-RU">>, <<"rurussianmale">>}
                     ,{<<"female/sv-SE">>, <<"swswedishfemale">>}
                     ,{<<"female/hu-HU">>, <<"huhungarianfemale">>}
                     ,{<<"female/cs-CZ">>, <<"eurczechfemale">>}
                     ,{<<"female/tr-TR">>, <<"eurturkishfemale">>}
                     ,{<<"male/tr-TR">>, <<"eurturkishmale">>}
                    ],
    case props:get_value(Voice, VoiceMappings) of
        'undefined' ->
            {'error', 'invalid_voice'};
        ISpeechVoice ->
            BaseUrl = whapps_config:get_string(?MOD_CONFIG_CAT, <<"tts_url">>, <<"http://api.ispeech.org/api/json">>),
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
            Response = ibrowse:send_req(BaseUrl, Headers, 'post', Body, HTTPOptions),
            create_response(Response)
    end;
create(_, _, _, _, _) ->
    {'error', 'unknown_provider'}.

%%------------------------------------------------------------------------------
%% Transcribe the audio binary
%%------------------------------------------------------------------------------
-spec asr_freeform(ne_binary()) -> asr_resp().
-spec asr_freeform(ne_binary(), ne_binary()) -> asr_resp().
-spec asr_freeform(ne_binary(), ne_binary(), ne_binary()) -> asr_resp().
-spec asr_freeform(ne_binary(), ne_binary(), ne_binary(), wh_proplist()) -> asr_resp().
-spec asr_freeform(ne_binary(), ne_binary(), ne_binary(), ne_binary(), wh_proplist()) -> provider_return().
asr_freeform(Bin) ->
    asr_freeform(Bin, <<"application/x-wav">>).
asr_freeform(Bin, ContentType) ->
    asr_freeform(Bin, ContentType, <<"en-US">>).
asr_freeform(Bin, ContentType, Locale) ->
    asr_freeform(Bin, ContentType, Locale, []).
asr_freeform(Bin, ContentType, Locale, Options) ->
    Provider = whapps_config:get_binary(?MOD_CONFIG_CAT, <<"asr_provider">>, <<"ispeech">>),
    case asr_freeform(Provider, Bin, ContentType, Locale, Options) of
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

asr_freeform(<<"ispeech">>, Bin, ContentType, Locale, Options) ->
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
asr_freeform(_, _, _, _, _) ->
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
    asr_commands(Bin, Commands, <<"application/x-wav">>).
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


create_response({'error', _R}=E) ->
    lager:warning("creating speech file failed with error ~p", [_R]),
    E;
create_response({'ibrowse_req_id', ReqID}) ->
    lager:debug("speech file streaming as ~p", [ReqID]),
    {'ok', ReqID};
create_response({'ok', "200", Headers, Content}) ->
    ContentType = props:get_value("Content-Type", Headers),
    ContentLength = props:get_value("Content-Length", Headers),
    lager:debug("created speech file ~s of length ~s", [ContentType, ContentLength]),
    {'ok', wh_util:to_binary(ContentType), Content};
create_response({'ok', Code, _, Content}) ->
    lager:warning("creating speech file failed with code ~s: ~s", [Code, Content]),
    {'error', 'tts_provider_failure'}.
