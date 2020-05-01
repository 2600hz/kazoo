%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_custom_sip_headers).

-export([inbound/1, inbound/2, set_inbound/2
        ,outbound/1, outbound/2, set_outbound/2
        ,inbound_header/2, inbound_header/3
        ,outbound_header/2, outbound_header/3
        ]).

-include("kz_documents.hrl").

-define(IN, <<"in">>).
-define(OUT, <<"out">>).

-spec inbound(kz_json:object()) -> kz_term:api_object().
inbound(CSH) ->
    inbound(CSH, 'undefined').

-spec inbound(kz_json:object(), Default) -> kz_json:object() | Default.
inbound(CSH, Default) ->
    LegacyCSH = kz_json:filter(fun filter_csh/1, CSH),
    InCSH = kz_json:get_value(?IN, CSH, kz_json:new()),

    CustomHeaders = kz_json:merge_jobjs(InCSH, LegacyCSH),

    case kz_json:is_empty(CustomHeaders) of
        'false' -> CustomHeaders;
        'true' -> Default
    end.

-spec inbound_header(kz_json:object(), kz_json:key()) -> kz_json:json_term() | 'undefined'.
inbound_header(DeviceJObj, Name) ->
    inbound_header(DeviceJObj, Name, 'undefined').

-spec inbound_header(kz_json:object(), kz_json:key(), Default) -> kz_json:json_term() | Default.
inbound_header(DeviceJObj, Name, Default) ->
    kz_json:get_ne_value(Name, inbound(DeviceJObj), Default).

-spec outbound_header(kz_json:object(), kz_json:key()) -> kz_json:json_term() | 'undefined'.
outbound_header(DeviceJObj, Name) ->
    outbound_header(DeviceJObj, Name, 'undefined').

-spec outbound_header(kz_json:object(), kz_json:key(), Default) -> kz_json:json_term() | Default.
outbound_header(DeviceJObj, Name, Default) ->
    kz_json:get_ne_value(Name, outbound(DeviceJObj), Default).

-spec filter_csh({kz_term:ne_binary(), any()}) -> boolean().
filter_csh({<<"in">>, _}) -> 'false';
filter_csh({<<"out">>, _}) -> 'false';
filter_csh(_) -> 'true'.

-spec outbound(kz_json:object()) -> kz_term:api_object().
outbound(CSH) ->
    outbound(CSH, 'undefined').

-spec outbound(kz_json:object(), Default) -> kz_json:object() | Default.
outbound(CSH, Default) ->
    LegacyCSH = kz_json:filter(fun filter_csh/1, CSH),
    OutCSH = kz_json:get_value(?OUT, CSH, kz_json:new()),

    CustomHeaders = kz_json:merge_jobjs(OutCSH, LegacyCSH),

    case kz_json:is_empty(CustomHeaders) of
        'false' -> CustomHeaders;
        'true' -> Default
    end.

-spec set_inbound(kz_json:object(), kz_json:object()) -> kz_json:object().
set_inbound(CSH, Headers) ->
    kz_json:set_value(?IN, Headers, CSH).

-spec set_outbound(kz_json:object(), kz_json:object()) -> kz_json:object().
set_outbound(CSH, Headers) ->
    kz_json:set_value(?OUT, Headers, CSH).
