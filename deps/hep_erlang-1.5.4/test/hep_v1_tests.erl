%% -*- coding: utf-8 -*-
-module(hep_v1_tests).

%% hep_v1_tests: tests for module hep_v1.

-include_lib("eunit/include/eunit.hrl").
-include("hep.hrl").

-define(REAL_HEP_PAYLOAD,
        <<
          "SIP/2.0 200 OK\r\n"
          "Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bK8c8b.9f00a115000000000000000000000000.0\r\n"
          "From: <sip:sipcheck@10.26.0.182>;tag=6a9eb17cd14528bda74bd05e73f170de-47fb\r\n"
          "To: <sip:10.26.0.182:11000>;tag=SXrc7NaU2p38H\r\n"
          "Call-ID: 5dca43e524c69899-13867@10.26.0.182\r\n"
          "CSeq: 10 OPTIONS\r\n"
          "Contact: <sip:10.26.0.182:11000>\r\n"
          "User-Agent: 2600hz\r\n"
          "Accept: application/sdp\r\n"
          "Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, MESSAGE, INFO, UPDATE, REGISTER, REFER, NOTIFY, PUBLISH, SUBSCRIBE\r\n"
          "Supported: path, replaces\r\n"
          "Allow-Events: talk, hold, conference, presence, as-feature-event, dialog, line-seize, call-info, sla, include-session-description, presence.winfo, message-summary, refer\r\n"
          "Content-Length: 0\r\n"
          "\r\n"
        >>).

%% API tests.

ipv4_encode_decode_test () ->
    Hep = (hep())#hep{ protocol_family = 'ipv4'
                     , src_ip = {127, 0, 0, 1}
                     , dst_ip = {0, 0, 0, 0}
                     },
    {ok, Bin} = hep_v1:encode(Hep),
    ?assertMatch({ok, #hep{ version = 'hep_v1'
                          , protocol_family = 'ipv4'
                          , protocol = 17
                          , src_ip = {127, 0, 0, 1}
                          , src_port = 1234
                          , dst_ip = {0, 0, 0, 0}
                          , dst_port = 5060
                          , timestamp = _
                          , payload_type = 'sip'
                          , payload = <<"INVITE sip:bob">>
                          }
                 }, hep_v1:decode(Bin)).

ipv6_encode_decode_test () ->
    Hep = (hep())#hep{ protocol_family = 'ipv6'
                     , src_ip = {1, 2, 3, 4, 5, 6, 7, 8}
                     , dst_ip = {8, 7, 6, 5, 4, 3, 2, 1}
                     },
    {ok, Bin} = hep_v1:encode(Hep),
    ?assertMatch({ok, #hep{ version = 'hep_v1'
                          , protocol_family = 'ipv6'
                          , protocol = 17
                          , src_ip = {1, 2, 3, 4, 5, 6, 7, 8}
                          , src_port = 1234
                          , dst_ip = {8, 7, 6, 5, 4, 3, 2, 1}
                          , dst_port = 5060
                          , timestamp = _
                          , payload_type = 'sip'
                          , payload = <<"INVITE sip:bob">>
                          }
                 }, hep_v1:decode(Bin)).


decode_test () ->
    ?assertMatch({ok, #hep{ version = 'hep_v1'
                          , protocol_family = 'ipv4'
                          , protocol = 17
                          , src_ip = {10, 26, 0, 182}
                          , src_port = 11000
                          , dst_ip = {10, 26, 0, 182}
                          , dst_port = 5060
                          , timestamp = _
                          , payload_type = 'sip'
                          , payload = ?REAL_HEP_PAYLOAD
                          }
                 }, hep_v1:decode(real_bin())).

encode_test () ->
    Hep = real_hep(),
    {ok, Data} = hep_v1:encode(Hep),
    ?assertMatch({ok, #hep{ version = 'hep_v1'
                          , protocol_family = 'ipv4'
                          , protocol = 17
                          , src_ip = {10, 26, 0, 182}
                          , src_port = 11000
                          , dst_ip = {10, 26, 0, 182}
                          , dst_port = 5060
                          , timestamp = _
                          , payload_type = 'sip'
                          , payload = ?REAL_HEP_PAYLOAD
                          }
                 }, hep_v1:decode(Data)).

%% Internals

hep () ->
    #hep{ version = 'hep_v1'
        , protocol = 17
        , src_port = 1234
        , dst_port = 5060
        , payload_type = 'sip'
        , payload = <<"INVITE sip:bob">>
        }.


real_hep () ->
    #hep{ version = 'hep_v1'
        , protocol_family = 'ipv4'
        , protocol = 17
        , src_ip = {10, 26, 0, 182}
        , src_port = 11000
        , dst_ip = {10, 26, 0, 182}
        , dst_port = 5060
        , timestamp = {1434,423172,402946}
        , node_id = undefined
        , payload_type = 'sip'
        , payload = ?REAL_HEP_PAYLOAD
        , vendor = undefined
        }.

real_bin () ->
    <<
      1,16,2,17,42,248,19,196,10,26,0,182,10,26,0,182,83,73,80,47,50,46,48,32,50,
      48,48,32,79,75,13,10,86,105,97,58,32,83,73,80,47,50,46,48,47,85,68,80,32,49,
      48,46,50,54,46,48,46,49,56,50,59,98,114,97,110,99,104,61,122,57,104,71,52,98,
      75,56,99,56,98,46,57,102,48,48,97,49,49,53,48,48,48,48,48,48,48,48,48,48,48,
      48,48,48,48,48,48,48,48,48,48,48,48,48,46,48,13,10,70,114,111,109,58,32,60,
      115,105,112,58,115,105,112,99,104,101,99,107,64,49,48,46,50,54,46,48,46,49,
      56,50,62,59,116,97,103,61,54,97,57,101,98,49,55,99,100,49,52,53,50,56,98,100,
      97,55,52,98,100,48,53,101,55,51,102,49,55,48,100,101,45,52,55,102,98,13,10,
      84,111,58,32,60,115,105,112,58,49,48,46,50,54,46,48,46,49,56,50,58,49,49,48,
      48,48,62,59,116,97,103,61,83,88,114,99,55,78,97,85,50,112,51,56,72,13,10,67,
      97,108,108,45,73,68,58,32,53,100,99,97,52,51,101,53,50,52,99,54,57,56,57,57,
      45,49,51,56,54,55,64,49,48,46,50,54,46,48,46,49,56,50,13,10,67,83,101,113,58,
      32,49,48,32,79,80,84,73,79,78,83,13,10,67,111,110,116,97,99,116,58,32,60,115,
      105,112,58,49,48,46,50,54,46,48,46,49,56,50,58,49,49,48,48,48,62,13,10,85,
      115,101,114,45,65,103,101,110,116,58,32,50,54,48,48,104,122,13,10,65,99,99,
      101,112,116,58,32,97,112,112,108,105,99,97,116,105,111,110,47,115,100,112,13,
      10,65,108,108,111,119,58,32,73,78,86,73,84,69,44,32,65,67,75,44,32,66,89,69,
      44,32,67,65,78,67,69,76,44,32,79,80,84,73,79,78,83,44,32,77,69,83,83,65,71,
      69,44,32,73,78,70,79,44,32,85,80,68,65,84,69,44,32,82,69,71,73,83,84,69,82,
      44,32,82,69,70,69,82,44,32,78,79,84,73,70,89,44,32,80,85,66,76,73,83,72,44,
      32,83,85,66,83,67,82,73,66,69,13,10,83,117,112,112,111,114,116,101,100,58,32,
      112,97,116,104,44,32,114,101,112,108,97,99,101,115,13,10,65,108,108,111,119,
      45,69,118,101,110,116,115,58,32,116,97,108,107,44,32,104,111,108,100,44,32,
      99,111,110,102,101,114,101,110,99,101,44,32,112,114,101,115,101,110,99,101,
      44,32,97,115,45,102,101,97,116,117,114,101,45,101,118,101,110,116,44,32,100,
      105,97,108,111,103,44,32,108,105,110,101,45,115,101,105,122,101,44,32,99,97,
      108,108,45,105,110,102,111,44,32,115,108,97,44,32,105,110,99,108,117,100,101,
      45,115,101,115,115,105,111,110,45,100,101,115,99,114,105,112,116,105,111,110,
      44,32,112,114,101,115,101,110,99,101,46,119,105,110,102,111,44,32,109,101,
      115,115,97,103,101,45,115,117,109,109,97,114,121,44,32,114,101,102,101,114,
      13,10,67,111,110,116,101,110,116,45,76,101,110,103,116,104,58,32,48,13,10,13,
      10
    >>.

%% End of Module.
