%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_media_tts).

-export([get_uri/2]).

-include("kazoo_media.hrl").

get_uri(<<"tts://", Text/binary>>, JObj) ->
    {'ok', _TTSServer} = kz_media_cache_sup:find_tts_server(Text, JObj),
    lager:debug("tts server for ~s at ~p", [Text, _TTSServer]),

    Format = kz_json:get_value(<<"Format">>, JObj, <<"wav">>),
    Host = kz_network_utils:get_hostname(),
    Port = kapps_config:get_binary(?CONFIG_CAT, <<"proxy_port">>, 24517),
    StreamType = kz_media_util:convert_stream_type(kz_json:get_value(<<"Stream-Type">>, JObj)),

    <<(kz_media_util:base_url(Host, Port))/binary, StreamType/binary
      ,"/tts/", (kz_term:binary_md5(Text))/binary, ".", Format/binary
    >>.
