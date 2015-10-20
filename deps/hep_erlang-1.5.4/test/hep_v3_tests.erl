%% -*- coding: utf-8 -*-
-module(hep_v3_tests).

%% hep_v3_tests: tests for module hep_v3.

-include_lib("eunit/include/eunit.hrl").
-include("hep.hrl").

%% API tests.

decode_test () ->
    Hep = hep(),
    ?assertEqual({ok, Hep}, hep_v3:decode(bin())).

encode_test () ->
    Hep = hep(),
    {ok, Data} = hep_v3:encode(Hep),
    ?assertEqual({ok, Hep}, hep_v3:decode(Data)).

ipv4_encode_decode_test () ->
    Hep = hep(),
    {ok, Bin} = hep_v3:encode(Hep),
    ?assertEqual({ok, Hep}, hep_v3:decode(Bin)).

ipv6_encode_decode_test () ->
    Hep = (hep())#hep{ protocol_family = 'ipv6'
                     , src_ip = {1, 2, 3, 4, 5, 6, 7, 8}
                     , dst_ip = {8, 7, 6, 5, 4, 3, 2, 1}
                     },
    {ok, Bin} = hep_v3:encode(Hep),
    ?assertEqual({ok, Hep}, hep_v3:decode(Bin)).

%% Internals

hep () ->
    #hep{ version = 'hep_v3'
        , vendor = 'unknown'
        , protocol_family = 'ipv4'
        , protocol = 17
        , src_ip = {212, 202, 0, 1}
        , dst_ip = {82, 116, 0, 211}
        , src_port = 12010
        , dst_port = 5060
        , timestamp = {1313, 440459, 120000}
        , payload_type = 'sip'
        , node_id = 228
        , payload = <<"INVITE sip:bob">>
        }.

bin () ->
    <<16#48, 16#45, 16#50, 16#33
      %% HEP3 ID
      ,16#00, 16#71
      %% total length = 113 octets
      ,16#00, 16#00, 16#00, 16#01, 16#00, 16#07, 16#02
      %% protocol family = 2 (IPv4)
      ,16#00, 16#00, 16#00, 16#02, 16#00, 16#07, 16#11
      %% protocol ID = 17 (UDP)
      ,16#00, 16#00, 16#00, 16#03, 16#00, 16#0a, 16#d4, 16#ca, 16#00, 16#01
      %% IPv4 source address = 212.202.0.1
      ,16#00, 16#00, 16#00, 16#04, 16#00, 16#0a, 16#52, 16#74, 16#00, 16#d3
      %% IPv4 destination address = 82.116.0.211
      ,16#00, 16#00, 16#00, 16#07, 16#00, 16#08, 16#2e, 16#ea
      %% source port = 12010
      ,16#00, 16#00, 16#00, 16#08, 16#00, 16#08, 16#13, 16#c4
      %% destination port = 5060
      ,16#00, 16#00, 16#00, 16#09, 16#00, 16#0a, 16#4e, 16#49, 16#82, 16#cb
      %% seconds timestamp 1313440459 = Mon Aug 15 22:34:19 2011
      ,16#00, 16#00, 16#00, 16#0a, 16#00, 16#0a, 16#00, 16#01, 16#d4, 16#c0
      %% micro-seconds timestamp offset 120000 = 0.12 seconds
      ,16#00, 16#00, 16#00, 16#0b, 16#00, 16#07, 16#01
      %% 01 – SIP
      ,16#00, 16#00, 16#00, 16#0c, 16#00, 16#0a, 16#00, 16#00, 16#00, 16#E4
      %% capture ID (228)
      ,16#00, 16#00, 16#00, 16#0f, 16#00, 16#14, 16#49, 16#4e, 16#56, 16#49, 16#54, 16#45, 16#20, 16#73, 16#69, 16#70, 16#3a, 16#62, 16#6f, 16#62
      %% SIP payload “INVITE sip:bob” (shortened)
    >>.

%% End of Module.
