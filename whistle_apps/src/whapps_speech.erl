%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(whapps_speech).

-include("whistle_apps.hrl").

-export([create/1, create/2, create/3, create/4
         ,asr/1, asr/2, asr/3, asr/4, asr/5
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
                    {'ok', wh_json:json_object()} | %% {'ok', JObj}
                    {'error', provider_errors() | 'asr_provider_failure'}.

-spec create/1 :: (ne_binary()) -> create_resp().
-spec create/2 :: (ne_binary(), ne_binary()) -> create_resp().
-spec create/3 :: (ne_binary(), ne_binary(), ne_binary()) -> create_resp().
-spec create/4 :: (ne_binary(), ne_binary(), ne_binary(), proplist()) -> create_resp().
-spec create/5 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), proplist()) -> provider_return().

create(Text) ->
    create(Text, <<"female/en-US">>).

create(Text, Voice) ->
    create(Text, Voice, <<"wav">>).

create(Text, Voice, Format) ->
    create(Text, Voice, Format, []).

create(Text, Voice, Format, Options) ->
    Provider = whapps_config:get_binary(?MOD_CONFIG_CAT, <<"tts_provider">>, <<"ispeech">>),
    case create(Provider, Text, Voice, Format, Options) of
        {error, _R}=E ->
            lager:debug("creating speech file failed with error ~p", [_R]),
            E;
        {ibrowse_req_id, ReqID} ->
            lager:debug("streaming response ~p to provided option: ~p", [ReqID, props:get_value(stream_to, Options)]),
            {ok, ReqID};
        {ok, "200", Headers, Content} ->
            ContentType = props:get_value("Content-Type", Headers),
            ContentLength = props:get_value("Content-Length", Headers),
            lager:debug("created speech file ~s of length ~s", [ContentType, ContentLength]),
            {ok, wh_util:to_binary(ContentType), Content};
        {ok, Code, _, Content} ->
            lager:debug("creating speech file failed with code ~s: ~s", [Code, Content]),
            {error, tts_provider_failure}
    end.

create(<<"ispeech">>, Text, Voice, Format, Options) ->
    VoiceMappings = [{<<"female/en-US">>, <<"usenglishfemale">>}
                     ,{<<"male/en-US">>, <<"usenglishmale">>}
                     ,{<<"female/en-GB">>, <<"ukenglishfemale">>}
                     ,{<<"male/en-GB">>, <<"ukenglishmale">>}
                    ],
    case props:get_value(Voice, VoiceMappings) of
        undefined ->
            {error, invalid_voice};
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
            HTTPOptions = [{response_format, binary} | Options],

            Body = wh_json:encode(wh_json:from_list(Props)),
            ibrowse:send_req(BaseUrl, Headers, post, Body, HTTPOptions)
    end;
create(_, _, _, _, _) ->
    {error, unknown_provider}.

-spec asr/1 :: (ne_binary()) -> asr_resp().
-spec asr/2 :: (ne_binary(), ne_binary()) -> asr_resp().
-spec asr/3 :: (ne_binary(), ne_binary(), ne_binary()) -> asr_resp().
-spec asr/4 :: (ne_binary(), ne_binary(), ne_binary(), wh_proplist()) -> asr_resp().
-spec asr/5 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), wh_proplist()) -> provider_return().
asr(Bin) ->
    asr(Bin, <<"application/x-wav">>).
asr(Bin, ContentType) ->
    asr(Bin, ContentType, <<"en-US">>).
asr(Bin, ContentType, Locale) ->
    asr(Bin, ContentType, Locale, []).
asr(Bin, ContentType, Locale, Options) ->
    Provider = whapps_config:get_binary(?MOD_CONFIG_CAT, <<"asr_provider">>, <<"ispeech">>),
    case asr(Provider, Bin, ContentType, Locale, Options) of
        {error, _R}=E ->
            lager:debug("asr failed with error ~p", [_R]),
            E;
        {ibrowse_req_id, ReqID} ->
            lager:debug("streaming response ~p to provided option: ~p", [ReqID, props:get_value(stream_to, Options)]),
            {ok, ReqID};
        {ok, "200", _Headers, Content} ->
            lager:debug("asr of media succeeded: ~s", [Content]),
            {ok, wh_json:decode(Content)};
        {ok, Code, Hdrs, Content} ->
            lager:debug("asr of media failed with code ~s", [Code]),
            [lager:debug("resp header: ~p", [Hdr]) || Hdr <- Hdrs],
            lager:debug("resp: ~s", [Content]),
            {error, asr_provider_failure}
    end.

asr(<<"ispeech">>, Bin, ContentType, Locale, Options) ->
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
    HTTPOptions = [{response_format, binary} | Options],

    Body = wh_json:encode(wh_json:from_list(Props)),
    lager:debug("req body: ~s", [Body]),

    ibrowse:send_req(BaseUrl, Headers, post, Body, HTTPOptions);
asr(_, _, _, _, _) ->
    {error, unknown_provider}.
