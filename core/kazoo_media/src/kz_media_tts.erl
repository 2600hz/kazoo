%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_media_tts).

-export([get_uri/2]).

-include("kazoo_media.hrl").

-spec get_uri(ne_binary(), kz_json:object()) -> ne_binary().
get_uri(<<"tts://", Text/binary>>, JObj) ->
    Voice = kz_json:get_value(<<"Voice">>, JObj, <<"femaile">>),
    Language = kz_json:get_value(<<"Language">>, JObj, kz_media_util:default_prompt_language()),
    TTSId = kz_util:binary_md5(<<Text/binary, "/", Voice/binary, "/", Language/binary>>),

    lager:debug("lookup tts media url for ~s", [TTSId]),
    {'ok', _TTSServer} = kz_media_cache_sup:find_tts_server(Text, JObj, TTSId),

    lager:debug("tts server for ~s at ~p", [TTSId, _TTSServer]),

    Format = kz_json:get_value(<<"Format">>, JObj, <<"wav">>),
    Host = kz_network_utils:get_hostname(),
    Port = kapps_config:get_binary(?CONFIG_CAT, <<"proxy_port">>, 24517),
    StreamType = kz_media_util:convert_stream_type(kz_json:get_value(<<"Stream-Type">>, JObj)),

    <<(kz_media_util:base_url(Host, Port))/binary, StreamType/binary
      ,"/tts/", TTSId/binary, ".", Format/binary
    >>.
