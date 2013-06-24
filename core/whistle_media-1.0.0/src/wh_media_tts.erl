%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_media_tts).

-export([get_uri/2]).

-include("whistle_media.hrl").

get_uri(<<"tts://", Text/binary>>, JObj) ->
    {'ok', _TTSServer} = wh_media_cache_sup:find_tts_server(Text, JObj),
    lager:debug("tts server for ~s at ~p", [Text, _TTSServer]),

    Format = wh_json:get_value(<<"Format">>, JObj, <<"wav">>),
    Host = wh_network_utils:get_hostname(),
    Port = whapps_config:get_binary(?CONFIG_CAT, <<"proxy_port">>, 24517),
    StreamType = wh_media_util:convert_stream_type(wh_json:get_value(<<"Stream-Type">>, JObj)),

    {'ok', <<(wh_media_util:base_url(Host, Port))/binary, StreamType/binary
             ,"/tts/", (wh_util:binary_md5(Text))/binary, ".", Format/binary>>
    }.
