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

-module(hep_v2).

-include("hep.hrl").

-export([encode/1]).
-export([decode/1]).

-define(node_id(Val), (Val):16).
-define(length(Val), (Val):8).

-define(length_ipv4, 16).
-define(length_ipv6, 40).

%% API

-spec encode (hep:t()) -> {ok, binary()} | {error, _}.

encode (#hep{ version = ?MODULE
            , protocol_family = ProtocolFamily = 'ipv4'
            , protocol = Protocol
            , src_ip = {S1, S2, S3, S4}
            , src_port = SrcPort
            , dst_ip = {D1, D2, D3, D4}
            , dst_port = DstPort
            , timestamp = Timestamp
            , node_id = NodeId
            , payload_type = 'sip'
            , payload = Payload}) ->
    Secs = hep_util:timestamp_secs(Timestamp),
    Micros = hep_util:timestamp_microsecs(Timestamp),
    Length = ?length_ipv4,
    Bin = <<?HEP_V2_ID, ?length(Length), ?protocol_family(hep_util:protocol_family(ProtocolFamily)),
            ?protocol(Protocol), ?port(SrcPort), ?port(DstPort),
            ?ipv4(S1, S2, S3, S4),
            ?ipv4(D1, D2, D3, D4),
            ?timestamp(Secs), ?timestamp(Micros), ?node_id(NodeId), 0:16, Payload/binary>>,
    {ok, Bin};

encode (#hep{ version = ?MODULE
            , protocol_family = ProtocolFamily = 'ipv6'
            , protocol = Protocol
            , src_ip = {S1, S2, S3, S4, S5, S6, S7, S8}
            , src_port = SrcPort
            , dst_ip = {D1, D2, D3, D4, D5, D6, D7, D8}
            , dst_port = DstPort
            , timestamp = Timestamp
            , node_id = NodeId
            , payload_type = 'sip'
            , payload = Payload}) ->
    Secs = hep_util:timestamp_secs(Timestamp),
    Micros = hep_util:timestamp_microsecs(Timestamp),
    Length = ?length_ipv6,
    Bin = <<?HEP_V2_ID, ?length(Length), ?protocol_family(hep_util:protocol_family(ProtocolFamily)),
            ?protocol(Protocol), ?port(SrcPort), ?port(DstPort),
            ?ipv6(S1, S2, S3, S4, S5, S6, S7, S8),
            ?ipv6(D1, D2, D3, D4, D5, D6, D7, D8),
            ?timestamp(Secs), ?timestamp(Micros), ?node_id(NodeId), 0:16, Payload/binary>>,
    {ok, Bin};

encode (#hep{protocol_family = ProtocolFamily})
  when ProtocolFamily =/= 'ipv4'; ProtocolFamily =/= 'ipv6' ->
    {error, {invalid_protocol_family, ProtocolFamily}};

encode (#hep{payload_type = PayloadType})
  when PayloadType =/= 'sip' ->
    {error, {invalid_payload_type, PayloadType}};

encode (#hep{version = Version})
  when Version =/= ?MODULE ->
    {error, {invalid_version, Version}};

encode (Hep) ->
    {error, {invalid_hep, Hep}}.



-spec decode (binary()) -> {ok, hep:t()} | {error, _}.

decode (<<?HEP_V2_ID, ?length(Length), ?protocol_family(ProtocolFamily),
          ?protocol(Protocol), ?port(SrcPort), ?port(DstPort),
          ?ipv4(S1, S2, S3, S4),
          ?ipv4(D1, D2, D3, D4),
          ?timestamp(Secs), ?timestamp(USecs), ?node_id(NodeId), _:16, Payload/binary>>)
  when Length == ?length_ipv4, ProtocolFamily == ?FAMILY_IPV4 ->
    HEP = #hep{ version = ?MODULE
              , protocol_family = hep_util:protocol_family(ProtocolFamily)
              , protocol = Protocol
              , src_ip = {S1, S2, S3, S4}
              , src_port = SrcPort
              , dst_ip = {D1, D2, D3, D4}
              , dst_port = DstPort
              , timestamp = to_timestamp(Secs, USecs)
              , node_id = NodeId
              , payload_type = 'sip'
              , payload = Payload
              },
     {ok, HEP};

decode (<<?HEP_V2_ID, ?length(Length), ?protocol_family(ProtocolFamily),
          ?protocol(Protocol), ?port(SrcPort), ?port(DstPort),
          ?ipv6(S1, S2, S3, S4, S5, S6, S7, S8),
          ?ipv6(D1, D2, D3, D4, D5, D6, D7, D8),
          ?timestamp(Secs), ?timestamp(USecs), ?node_id(NodeId), _:16, Payload/binary>>)
  when Length == ?length_ipv6, ProtocolFamily == ?FAMILY_IPV6 ->
    HEP = #hep{ version = ?MODULE
              , protocol_family = hep_util:protocol_family(ProtocolFamily)
              , protocol = Protocol
              , src_ip = {S1, S2, S3, S4, S5, S6, S7, S8}
              , src_port = SrcPort
              , dst_ip = {D1, D2, D3, D4, D5, D6, D7, D8}
              , dst_port = DstPort
              , timestamp = to_timestamp(Secs, USecs)
              , node_id = NodeId
              , payload_type = 'sip'
              , payload = Payload
              },
    {ok, HEP};

decode (<<Other/binary>>) ->
    {error, {invalid_packet, Other}}.

%% Internals

to_timestamp (Secs, USecs) ->
    Mega = Secs div 1000000,
    S    = Secs rem 1000000,
    {Mega, S, USecs}.

%% End of Module.
