%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(whapps_speech).

-include("whistle_apps.hrl").

-export([create/1, create/2, create/3]).

-define(MOD_CONFIG_CAT, <<"speech">>).

-spec create/1 :: (ne_binary()) -> {'ok', ne_binary(), ne_binary()} | {'error', atom()}.
-spec create/2 :: (ne_binary(), ne_binary()) -> {'ok', ne_binary(), ne_binary()} | {'error', atom()}.
-spec create/3 :: (ne_binary(), ne_binary(), ne_binary()) -> {'ok', ne_binary(), ne_binary()} | {'error', atom()}.
-spec create/4 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> {'ok', ne_binary(), ne_binary()} | {'error', atom()}.

create(Text) ->
    create(Text, <<"female/en-US">>).

create(Text, Voice) ->
    create(Text, Voice, <<"wav">>).

create(Text, Voice, Format) ->
    Provider = whapps_config:get_binary(?MOD_CONFIG_CAT, <<"tts_provider">>, <<"ispeech">>),
    case create(Provider, Text, Voice, Format) of
        {error, _R}=E ->
            lager:debug("creating speech file failed with error ~s", [_R]),
            E;
        {ok, "200", Headers, Content} ->
            ContentType = props:get_value("Content-Type", Headers),
            ContentLength = props:get_value("Content-Length", Headers),
            lager:debug("created speech file ~s of length ~s", [ContentType, ContentLength]),
            {ok, wh_util:to_binary(ContentType), Content};
        {ok, Code, _, Content} ->
            lager:debug("creating speech file failed with code ~s: ~s", [Code, Content]),
            {error, tts_provider_failure}
    end.

create(<<"ispeech">>, Text, Voice, Format) ->
    VoiceMappings = [{<<"female/en-US">>, <<"usenglishfemale">>}
                     ,{<<"male/en-US">>, <<"usenglishmale">>}
                     ,{<<"female/en-GB">>, <<"ukenglishfemale">>}
                     ,{<<"male/en-GB">>, <<"ukenglishmale">>}
                    ],
    case props:get_value(Voice, VoiceMappings) of
        undefined -> {error, invalid_voice};
        ISpeechVoice ->
            Url = whapps_config:get_string(?MOD_CONFIG_CAT, <<"tts_url">>, <<"http://api.ispeech.org/api/json">>),
            Props = [{<<"text">>, Text}
                     ,{<<"voice">>, ISpeechVoice}
                     ,{<<"format">>, Format}
                     ,{<<"action">>, <<"convert">>}
                     ,{<<"apikey">>, whapps_config:get_binary(?MOD_CONFIG_CAT, <<"tts_api_key">>, <<"">>)}
                     ,{<<"speed">>, whapps_config:get_integer(?MOD_CONFIG_CAT, <<"tts_speed">>, 0)}
                     ,{<<"startpadding">>, whapps_config:get_integer(?MOD_CONFIG_CAT, <<"tts_start_padding">>, 1)}
                     ,{<<"endpadding">>, whapps_config:get_integer(?MOD_CONFIG_CAT, <<"tts_end_padding">>, 0)} 
                    ],
            Headers = [{"Host", <<"api.ispeech.org">>}
                       ,{"Content-Type", "application/json; charset=UTF-8"}
                      ],
            HTTPOptions = [{response_format, binary}],
            Body = wh_json:encode(wh_json:from_list(Props)),
            ibrowse:send_req(Url, Headers, post, Body, HTTPOptions)            
    end;
create(_, _, _, _) ->
    {error, unknown_provider}.

