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

-export([create/1, create/2, create/3, create/4]).

-define(MOD_CONFIG_CAT, <<"speech">>).

-type provider_errors() :: 'invalid_voice' | 'unknown_provider'.
-type provider_return() :: {'error', provider_errors()} |
                           ibrowse_ret() |
                           {'ibrowse_req_id', ibrowse_req_id()}.
-type create_resp() :: {'ok', ibrowse_req_id()} |
                       {'ok', ne_binary(), ne_binary()} | %% {'ok', ContentType, BinaryData}
                       {'error', provider_errors() | 'tts_provider_failure'}.

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
            lager:debug("creating speech file failed with error ~s", [_R]),
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
            HTTPOptions = [{response_format, binary} | Options],
            Body = wh_json:encode(wh_json:from_list(Props)),
            ibrowse:send_req(Url, Headers, post, Body, HTTPOptions)            
    end;
create(_, _, _, _, _) ->
    {error, unknown_provider}.
