%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_media_tts).

-export([get_uri/2]).

-include("kazoo_media.hrl").

-spec get_uri(kz_term:ne_binary(), kz_json:object()) -> kz_term:ne_binary().
get_uri(Id, JObj) ->
    {'ok', _TTSServer} = kz_media_cache_sup:find_tts_server(Id, JObj),

    lager:debug("tts server for ~s at ~p", [Id, _TTSServer]),

    Format = kz_json:get_ne_binary_value(<<"Format">>, JObj, <<"wav">>),
    Host = kz_media_util:proxy_host(),
    Port = kapps_config:get_integer(?CONFIG_CAT, <<"proxy_port">>, 24517),
    StreamType = kz_media_util:convert_stream_type(kz_json:get_ne_binary_value(<<"Stream-Type">>, JObj)),

    UrlParts = [kz_media_util:base_url(Host, Port)
               ,StreamType
               ,<<"tts">>
               ,<<Id/binary, ".", Format/binary>>
               ],
    kz_binary:join(UrlParts, <<"/">>).
