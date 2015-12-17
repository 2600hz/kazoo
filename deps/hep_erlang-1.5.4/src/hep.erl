%% Copyright (c) 2013, Matthias Endler <matthias.endler@pantech.at>
%%
%% Permission to use, copy, modify, and distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(hep).

-include("hep.hrl").

-export([encode/1]).
-export([decode/1]).

-export([ new/0
        , version/1
        , protocol_family/1, protocol_family/2
        , protocol/1, protocol/2
        , src_ip/1, src_ip/2
        , dst_ip/1, dst_ip/2
        , src_port/1, src_port/2
        , dst_port/1, dst_port/2
        , timestamp/1, timestamp/2
        , node_id/1, node_id/2
        , payload_type/1, payload_type/2
        , payload/1, payload/2
        , vendor/1, vendor/2
        ]).

-export_type([ t/0
             , version/0
             , vendor/0
             , uint8/0
             , uint16/0
             , uint32/0 ]).

-opaque t () :: #hep{}.

-type version () :: 'hep_v1' | 'hep_v2' | 'hep_v3'.
-type vendor () :: 'unknown'
                 | 'freeswitch'
                 | 'kamailio'
                 | 'opensips'
                 | 'asterisk'
                 | 'homer'
                 | 'sipxecs'.
-type protocol_family () :: 'ipv4' | 'ipv6'.
-type payload_type () :: 'reserved'
                       | 'sip'
                       | 'xmpp'
                       | 'sdp'
                       | 'rtp'
                       | 'rtcp'
                       | 'mgcp'
                       | 'megaco'
                       | 'm2ua'
                       | 'm3ua'
                       | 'iax'
                       | 'h322'
                       | 'h321'.

-type uint8 () :: 0..255.
-type uint16 () :: 0..65535.
-type uint32 () :: 0..4294967295.

%% API

-spec encode (t()) -> {ok, binary()} | {error, _}.
encode (#hep{version = Version} = Hep)
  when Version == 'hep_v1'; Version == 'hep_v2'; Version == 'hep_v3' ->
    Version:encode(Hep);
encode (Hep) ->
    {error, {invalid_hep, Hep}}.


-spec decode (binary()) -> {ok, t()} | {error, _}.
decode (Packet = <<?HEP_V1_ID, _Rest/binary>>) ->
    hep_v1:decode(Packet);
decode (Packet = <<?HEP_V2_ID, _Rest/binary>>) ->
    hep_v2:decode(Packet);
decode (Packet = <<?HEP_V3_ID, _Rest/binary>>) ->
    hep_v3:decode(Packet);
decode (Packet = <<_/binary>>) ->
    {error, invalid_packet, Packet}.


-spec version (t()) -> version().
version (#hep{version = Version}) ->
    Version;
version (Hep) ->
    {error, {invalid_packet, Hep}}.


%% Note: you can't have a dot inside a macro definition!
-define(getter(Field), Field(#hep{Field = Val}) -> Val).
-define(setter(Field), Field(Val, Hep) -> Hep#hep{Field = Val}).

new () -> #hep{}.
?getter(protocol_family).
?setter(protocol_family).
?getter(protocol).
?setter(protocol).
?getter(src_ip).
?setter(src_ip).
?getter(dst_ip).
?setter(dst_ip).
?getter(src_port).
?setter(src_port).
?getter(dst_port).
?setter(dst_port).
?getter(timestamp).
?setter(timestamp).
?getter(node_id).
?setter(node_id).
?getter(payload_type).
?setter(payload_type).
?getter(payload).
?setter(payload).
?getter(vendor).
?setter(vendor).

%% Internals

%% End of Module.
