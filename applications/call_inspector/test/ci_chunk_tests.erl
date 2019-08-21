%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ci_chunk_tests).

%% ci_chunk_tests: tests for module ci_chunk.

-include_lib("eunit/include/eunit.hrl").

-export([chunks_1/1
        ,chunks_2/1
        ,chunks_3/1
        ,chunks_4/1
        ,chunks_5/1
        ,chunks_6/1
        ,chunks_7/1
        ]).

%% API tests.

json_test_() ->
    lists:flatmap(
      fun (Data1) ->
              Txt = lists:flatten(io_lib:format("For ~p", [Data1])),
              [{Txt ++ " chunk " ++ integer_to_list(I)
               ,?_assert(kz_json:are_equal(Data1(I), ci_chunk:to_json(ci_chunk:from_json(Data1(I)))))
                %% ,?_assert(
                %%     begin
                %%         io:format(user, "\n>L> ~s\n\n", [kz_json:encode(Data1(I))]),
                %%         io:format(user, "\n>R> ~s\n\n", [kz_json:encode(ci_chunk:to_json(ci_chunk:from_json(Data1(I))))]),
                %%         kz_json:are_equal(Data1(I), ci_chunk:to_json(ci_chunk:from_json(Data1(I))))
                %%     end
                %%    )
               }
               || I <- lists:seq(1, Data1('count'))
              ]
      end,
      [fun ?MODULE:chunks_1/1
      ,fun ?MODULE:chunks_2/1
      ,fun ?MODULE:chunks_3/1
      ,fun ?MODULE:chunks_4/1
      ,fun ?MODULE:chunks_5/1
      ,fun ?MODULE:chunks_6/1
      ,fun ?MODULE:chunks_7/1
      ]).

reorder_dialog_1_test_() ->
    Data1 = fun ?MODULE:chunks_1/1,
    Chunks = lists:map(fun ci_chunk:from_json/1, chunks_1()),
    reorder_dialog(<<"10.26.0.182:9061">>, Data1, Chunks)
        ++ reorder_dialog(<<"10.26.0.182:9061">>, Data1, kz_term:shuffle_list(Chunks))
        ++ reorder_dialog(<<"10.26.0.182:9061">>, Data1, kz_term:shuffle_list(Chunks)).

reorder_dialog_2_test_() ->
    Data1 = fun ?MODULE:chunks_2/1,
    Chunks = lists:map(fun ci_chunk:from_json/1, chunks_2()),
    reorder_dialog(<<"10.26.0.182:9060">>, Data1, Chunks)
        ++ reorder_dialog(<<"10.26.0.182:9060">>, Data1, kz_term:shuffle_list(Chunks)).

reorder_dialog_3_test_() ->
    Data1 = fun ?MODULE:chunks_3/1,
    Chunks = lists:map(fun ci_chunk:from_json/1, chunks_3()),
    reorder_dialog(<<"10.26.0.182:9061">>, Data1, Chunks)
        ++ reorder_dialog(<<"10.26.0.182:9061">>, Data1, kz_term:shuffle_list(Chunks))
        ++ reorder_dialog(<<"10.26.0.182:9061">>, Data1, kz_term:shuffle_list(Chunks)).

reorder_dialog_4_test_() ->
    Data1 = fun ?MODULE:chunks_4/1,
    Chunks = lists:map(fun ci_chunk:from_json/1, chunks_4()),
    reorder_dialog(<<"192.168.56.42:9061">>, Data1, Chunks).

reorder_dialog_5_test_() ->
    Data1 = fun ?MODULE:chunks_5/1,
    [C1, C2] = lists:map(fun ci_chunk:from_json/1, chunks_5()),
    reorder_dialog(<<"104.237.144.93:9061">>, Data1, [C1, C2])
        ++ reorder_dialog(<<"104.237.144.93:9061">>, Data1, [C2, C1]).

reorder_dialog_6_test_() ->
    Data1 = fun ?MODULE:chunks_6/1,
    [C1, C2] = lists:map(fun ci_chunk:from_json/1, chunks_6()),
    reorder_dialog(<<"104.237.144.93:9061">>, Data1, [C1, C2])
        ++ reorder_dialog(<<"104.237.144.93:9061">>, Data1, [C2, C1]).

reorder_dialog_7_test_() ->
    Data1 = fun ?MODULE:chunks_7/1,
    Chunks = lists:map(fun ci_chunk:from_json/1, chunks_7()),
    [RefParser] = parsers(Chunks),
    reorder_dialog(RefParser, Data1, Chunks)
        ++ reorder_dialog(RefParser, Data1, kz_term:shuffle_list(Chunks))
        ++ reorder_dialog(RefParser, Data1, kz_term:shuffle_list(Chunks)).

%% Internals

reorder_dialog(RefParser, Data1, Chunks) ->
    Txt0 = lists:flatten(io_lib:format("For ~p", [Data1])),
    Reordered = ci_chunk:do_reorder_dialog(RefParser, Chunks),

    %% JObjs = kz_json:encode(lists:map(fun ci_chunk:to_json/1,Reordered)),
    %% file:write_file("/tmp/ci__"++binary_to_list(ci_chunk:call_id(hd(Chunks)))++"__n.json",
    %%                 io_lib:format("~s\n",[JObjs])
    %%                ),

    Labels = [kz_json:get_value(<<"label">>, Data1(I)) || I <- lists:seq(1, Data1('count'))],
    LabelsReordered = [ci_chunk:label(Chunk) || Chunk <- Reordered],
    [{Txt0, ?_assertEqual(RefParser, ci_chunk:pick_ref_parser(Chunks))}
    ,{Txt0, ?_assertEqual(Data1('count'), length(Reordered))}
    ,{Txt0, ?_assertEqual(Data1('entities'), ci_chunk:get_dialog_entities(Reordered))}
    ,{Txt0, ?_assertEqual(Labels, LabelsReordered)}
    ] ++
        lists:append(
          [[{Txt, ?_assertEqual(ci_chunk:src_ip(C), ci_chunk:src_ip(R))}
           ,{Txt, ?_assertEqual(ci_chunk:dst_ip(C), ci_chunk:dst_ip(R))}
           ,{Txt, ?_assertEqual(ci_chunk:src_port(C), ci_chunk:src_port(R))}
           ,{Txt, ?_assertEqual(ci_chunk:dst_port(C), ci_chunk:dst_port(R))}
           ,{Txt, ?_assertEqual(ci_chunk:call_id(C), ci_chunk:call_id(R))}
           ,{Txt, ?_assertEqual(ci_chunk:timestamp(C), ci_chunk:timestamp(R))}
           ,{Txt, ?_assertEqual(ci_chunk:ref_timestamp(C), ci_chunk:ref_timestamp(R))}
           ,{Txt, ?_assertEqual(ci_chunk:label(C), ci_chunk:label(R))}
           ,{Txt, ?_assertEqual(ci_chunk:data(C), ci_chunk:data(R))}
           ,{Txt, ?_assertEqual(ci_chunk:parser(C), ci_chunk:parser(R))}
           ,{Txt, ?_assertEqual(ci_chunk:c_seq(C), ci_chunk:c_seq(R))}
           ]
           || I <- lists:seq(1, Data1('count')),
              begin
                  C = ci_chunk:from_json(Data1(I)),
                  R = lists:nth(I, Reordered),
                  Txt = Txt0 ++ " chunk " ++ integer_to_list(I),
                  true
              end
          ]).

parsers(Chunks) ->
    sets:to_list(
      sets:from_list(
        [ci_chunk:parser(Chunk) || Chunk <- Chunks]
       )).


chunks_3('count') -> 8;
chunks_3('entities') ->
    [<<"10.26.0.167:5060">>,<<"10.26.0.182:5060">>,<<"10.26.0.182:11000">>];

chunks_3(1) ->
    {[{<<"call-id">>,<<"36-52896@10.26.0.167">>},
      {<<"timestamp">>,66158851427.3383},
      {<<"ref_timestamp">>,<<"63601207277.59852">>},
      {<<"label">>,<<"INVITE sip:service@10.26.0.182:5060 SIP/2.0">>},
      {<<"raw">>,
       [<<"INVITE sip:service@10.26.0.182:5060 SIP/2.0">>,
        <<"Via: SIP/2.0/UDP 10.26.0.167:5060;branch=z9hG4bK-52896-36-0">>,
        <<"From: sipp <sip:sipp@10.26.0.167:5060>;tag=52896SIPpTag0036">>,
        <<"To: service <sip:service@10.26.0.182:5060>">>,
        <<"Call-ID: 36-52896@10.26.0.167">>,
        <<"CSeq: 1 INVITE">>,
        <<"Contact: sip:sipp@10.26.0.167:5060">>,
        <<"Max-Forwards: 70">>,
        <<"Subject: Performance Test">>,
        <<"Content-Type: application/sdp">>,
        <<"Content-Length:   133">>,
        <<>>,
        <<"v=0">>,
        <<"o=user1 53655765 2353687637 IN IP4 10.26.0.167">>,
        <<"s=-">>,
        <<"c=IN IP4 10.26.0.167">>,
        <<"t=0 0">>,
        <<"m=audio 6000 RTP/AVP 0">>,
        <<"a=rtpmap:0 PCMU/8000">>]},
      {<<"src">>, <<"10.26.0.167:5060">>},
      {<<"dst">>, <<"10.26.0.182:5060">>},
      {<<"parser">>, <<"10.26.0.182:9061">>},
      {<<"c_seq">>, <<"1 INVITE">>}]};

chunks_3(2) ->
    {[{<<"call-id">>,<<"36-52896@10.26.0.167">>},
      {<<"timestamp">>,66158854195.710014},
      {<<"ref_timestamp">>,<<"63601207277.598785">>},
      {<<"label">>,<<"SIP/2.0 100 Attempting to connect your call">>},
      {<<"raw">>,
       [<<"SIP/2.0 100 Attempting to connect your call">>,
        <<"Via: SIP/2.0/UDP 10.26.0.167:5060;branch=z9hG4bK-52896-36-0;rport=5060">>,
        <<"From: sipp <sip:sipp@10.26.0.167:5060>;tag=52896SIPpTag0036">>,
        <<"To: service <sip:service@10.26.0.182:5060>">>,
        <<"Call-ID: 36-52896@10.26.0.167">>,
        <<"CSeq: 1 INVITE">>,
        <<"Content-Length: 0">>]},
      {<<"src">>, <<"10.26.0.182:5060">>},
      {<<"dst">>, <<"10.26.0.167:5060">>},
      {<<"parser">>, <<"10.26.0.182:9061">>},
      {<<"c_seq">>, <<"1 INVITE">>}]};

chunks_3(3) ->
    {[{<<"call-id">>,<<"36-52896@10.26.0.167">>},
      {<<"timestamp">>,66158854313.216064},
      {<<"ref_timestamp">>,<<"63601207277.59906">>},
      {<<"label">>,<<"INVITE sip:service@10.26.0.182:5060 SIP/2.0">>},
      {<<"raw">>,
       [<<"INVITE sip:service@10.26.0.182:5060 SIP/2.0">>,
        <<"Record-Route: <sip:10.26.0.182;lr=on;ftag=52896SIPpTag0036>">>,
        <<"Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bK07b1.755e8b361c59510c3ade2d60ed2c0525.0">>,
        <<"Via: SIP/2.0/UDP 10.26.0.167:5060;rport=5060;branch=z9hG4bK-52896-36-0">>,
        <<"From: sipp <sip:sipp@10.26.0.167:5060>;tag=52896SIPpTag0036">>,
        <<"To: service <sip:service@10.26.0.182:5060>">>,
        <<"Call-ID: 36-52896@10.26.0.167">>,
        <<"CSeq: 1 INVITE">>,
        <<"Contact: sip:sipp@10.26.0.167:5060">>,
        <<"Max-Forwards: 50">>,
        <<"Subject: Performance Test">>,
        <<"Content-Type: application/sdp">>,
        <<"Content-Length:   133">>,
        <<"X-AUTH-IP: 10.26.0.167">>,
        <<"X-AUTH-PORT: 5060">>,
        <<>>,
        <<"v=0">>,
        <<"o=user1 53655765 2353687637 IN IP4 10.26.0.167">>,
        <<"s=-">>,
        <<"c=IN IP4 10.26.0.167">>,
        <<"t=0 0">>,
        <<"m=audio 6000 RTP/AVP 0">>,
        <<"a=rtpmap:0 PCMU/8000">>]},
      {<<"src">>, <<"10.26.0.182:5060">>},
      {<<"dst">>, <<"10.26.0.182:11000">>},
      {<<"parser">>, <<"10.26.0.182:9061">>},
      {<<"c_seq">>, <<"1 INVITE">>}]};

chunks_3(4) ->
    {[{<<"call-id">>,<<"36-52896@10.26.0.167">>},
      {<<"timestamp">>,66158853056.56326},
      {<<"ref_timestamp">>,<<"63601207277.604904">>},
      {<<"label">>,<<"SIP/2.0 100 Trying">>},
      {<<"raw">>,
       [<<"SIP/2.0 100 Trying">>,
        <<"Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bK07b1.755e8b361c59510c3ade2d60ed2c0525.0">>,
        <<"Via: SIP/2.0/UDP 10.26.0.167:5060;rport=5060;branch=z9hG4bK-52896-36-0">>,
        <<"Record-Route: <sip:10.26.0.182;lr=on;ftag=52896SIPpTag0036>">>,
        <<"From: sipp <sip:sipp@10.26.0.167:5060>;tag=52896SIPpTag0036">>,
        <<"To: service <sip:service@10.26.0.182:5060>">>,
        <<"Call-ID: 36-52896@10.26.0.167">>,
        <<"CSeq: 1 INVITE">>,
        <<"User-Agent: 2600hz">>,
        <<"Content-Length: 0">>]},
      {<<"src">>, <<"10.26.0.182:11000">>},
      {<<"dst">>, <<"10.26.0.182:5060">>},
      {<<"parser">>, <<"10.26.0.182:9061">>},
      {<<"c_seq">>, <<"1 INVITE">>}]};

chunks_3(5) ->
    {[{<<"call-id">>,<<"36-52896@10.26.0.167">>},
      {<<"timestamp">>,66158855423.26483},
      {<<"ref_timestamp">>,<<"63601207277.61019">>},
      {<<"label">>,<<"SIP/2.0 407 Proxy Authentication Required">>},
      {<<"raw">>,
       [<<"SIP/2.0 407 Proxy Authentication Required">>,
        <<"Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bK07b1.755e8b361c59510c3ade2d60ed2c0525.0">>,
        <<"Via: SIP/2.0/UDP 10.26.0.167:5060;rport=5060;branch=z9hG4bK-52896-36-0">>,
        <<"From: sipp <sip:sipp@10.26.0.167:5060>;tag=52896SIPpTag0036">>,
        <<"To: service <sip:service@10.26.0.182:5060>;tag=8DZD96F5HDKmj">>,
        <<"Call-ID: 36-52896@10.26.0.167">>,
        <<"CSeq: 1 INVITE">>,
        <<"User-Agent: 2600hz">>,
        <<"Accept: application/sdp">>,
        <<"Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, MESSAGE, INFO, UPDATE, REGISTER, REFER, NOTIFY, PUBLISH, SUBSCRIBE">>,
        <<"Supported: path, replaces">>,
        <<"Allow-Events: talk, hold, conference, presence, as-feature-event, dialog, line-seize, call-info, sla, include-session-description, presence.winfo, message-summary, refer">>,
        <<"Proxy-Authenticate: Digest realm=\"10.26.0.167\", nonce=\"bf48ea84-0fdd-11e5-9700-7b9c4900a590\", algorithm=MD5, qop=\"auth\"">>,
        <<"Content-Length: 0">>]},
      {<<"src">>, <<"10.26.0.182:11000">>},
      {<<"dst">>, <<"10.26.0.182:5060">>},
      {<<"parser">>, <<"10.26.0.182:9061">>},
      {<<"c_seq">>, <<"1 INVITE">>}]};

chunks_3(6) ->
    {[{<<"call-id">>,<<"36-52896@10.26.0.167">>},
      {<<"timestamp">>,66158855423.26483},
      {<<"ref_timestamp">>,<<"63601207277.610344">>},
      {<<"label">>,<<"ACK sip:service@10.26.0.182:5060 SIP/2.0">>},
      {<<"raw">>,
       [<<"ACK sip:service@10.26.0.182:5060 SIP/2.0">>,
        <<"Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bK07b1.755e8b361c59510c3ade2d60ed2c0525.0">>,
        <<"From: sipp <sip:sipp@10.26.0.167:5060>;tag=52896SIPpTag0036">>,
        <<"To: service <sip:service@10.26.0.182:5060>;tag=8DZD96F5HDKmj">>,
        <<"Call-ID: 36-52896@10.26.0.167">>,
        <<"CSeq: 1 ACK">>,
        <<"Max-Forwards: 50">>,
        <<"Content-Length: 0">>]},
      {<<"src">>, <<"10.26.0.182:5060">>},
      {<<"dst">>, <<"10.26.0.182:11000">>},
      {<<"parser">>, <<"10.26.0.182:9061">>},
      {<<"c_seq">>, <<"1 ACK">>}]};

chunks_3(7) ->
    {[{<<"call-id">>,<<"36-52896@10.26.0.167">>},
      {<<"timestamp">>,66158854248.92525},
      {<<"ref_timestamp">>,<<"63601207277.6105">>},
      {<<"label">>,<<"SIP/2.0 407 Proxy Authentication Required">>},
      {<<"raw">>,
       [<<"SIP/2.0 407 Proxy Authentication Required">>,
        <<"Via: SIP/2.0/UDP 10.26.0.167:5060;rport=5060;branch=z9hG4bK-52896-36-0">>,
        <<"From: sipp <sip:sipp@10.26.0.167:5060>;tag=52896SIPpTag0036">>,
        <<"To: service <sip:service@10.26.0.182:5060>;tag=8DZD96F5HDKmj">>,
        <<"Call-ID: 36-52896@10.26.0.167">>,
        <<"CSeq: 1 INVITE">>,
        <<"User-Agent: 2600hz">>,
        <<"Accept: application/sdp">>,
        <<"Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, MESSAGE, INFO, UPDATE, REGISTER, REFER, NOTIFY, PUBLISH, SUBSCRIBE">>,
        <<"Supported: path, replaces">>,
        <<"Allow-Events: talk, hold, conference, presence, as-feature-event, dialog, line-seize, call-info, sla, include-session-description, presence.winfo, message-summary, refer">>,
        <<"Proxy-Authenticate: Digest realm=\"10.26.0.167\", nonce=\"bf48ea84-0fdd-11e5-9700-7b9c4900a590\", algorithm=MD5, qop=\"auth\"">>,
        <<"Content-Length: 0">>]},
      {<<"src">>, <<"10.26.0.182:5060">>},
      {<<"dst">>, <<"10.26.0.167:5060">>},
      {<<"parser">>, <<"10.26.0.182:9061">>},
      {<<"c_seq">>, <<"1 INVITE">>}]};

chunks_3(8) ->
    {[{<<"call-id">>,<<"36-52896@10.26.0.167">>},
      {<<"timestamp">>,66158852958.06265},
      {<<"ref_timestamp">>,<<"63601207277.61347">>},
      {<<"label">>,<<"ACK sip:service@10.26.0.182:5060 SIP/2.0">>},
      {<<"raw">>,
       [<<"ACK sip:service@10.26.0.182:5060 SIP/2.0">>,
        <<"Via: SIP/2.0/UDP 10.26.0.167:5060;rport=5060;branch=z9hG4bK-52896-36-0">>,
        <<"From: sipp <sip:sipp@10.26.0.167:5060>;tag=52896SIPpTag0036">>,
        <<"To: service <sip:service@10.26.0.182:5060>;tag=8DZD96F5HDKmj">>,
        <<"Call-ID: 36-52896@10.26.0.167">>,
        <<"CSeq: 1 ACK">>,
        <<"Contact: <sip:sipp@10.26.0.167:5060;transport=UDP>">>,
        <<"Max-Forwards: 70">>,
        <<"Subject: Performance Test">>,
        <<"Content-Length: 0">>]},
      {<<"src">>, <<"10.26.0.167:5060">>},
      {<<"dst">>, <<"10.26.0.182:5060">>},
      {<<"parser">>, <<"10.26.0.182:9061">>},
      {<<"c_seq">>, <<"1 ACK">>}]}.

chunks_3() ->
    [chunks_3(1),
     chunks_3(3),
     {[{<<"call-id">>,<<"36-52896@10.26.0.167">>},
       {<<"timestamp">>,63601207277.59943},
       {<<"ref_timestamp">>,<<"63601207277.59947">>},
       {<<"label">>,<<"INVITE sip:service@10.26.0.182:5060 SIP/2.0">>},
       {<<"raw">>,
        [<<"INVITE sip:service@10.26.0.182:5060 SIP/2.0">>,
         <<"Record-Route: <sip:10.26.0.182;lr=on;ftag=52896SIPpTag0036>">>,
         <<"Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bK07b1.755e8b361c59510c3ade2d60ed2c0525.0">>,
         <<"Via: SIP/2.0/UDP 10.26.0.167:5060;rport=5060;branch=z9hG4bK-52896-36-0">>,
         <<"From: sipp <sip:sipp@10.26.0.167:5060>;tag=52896SIPpTag0036">>,
         <<"To: service <sip:service@10.26.0.182:5060>">>,
         <<"Call-ID: 36-52896@10.26.0.167">>,
         <<"CSeq: 1 INVITE">>,
         <<"Contact: sip:sipp@10.26.0.167:5060">>,
         <<"Max-Forwards: 50">>,
         <<"Subject: Performance Test">>,
         <<"Content-Type: application/sdp">>,
         <<"Content-Length:   133">>,
         <<"X-AUTH-IP: 10.26.0.167">>,
         <<"X-AUTH-PORT: 5060">>,
         <<>>,
         <<"v=0">>,
         <<"o=user1 53655765 2353687637 IN IP4 10.26.0.167">>,
         <<"s=-">>,
         <<"c=IN IP4 10.26.0.167">>,
         <<"t=0 0">>,
         <<"m=audio 6000 RTP/AVP 0">>,
         <<"a=rtpmap:0 PCMU/8000">>]},
       {<<"src">>, <<"10.26.0.182:5060">>},
       {<<"dst">>, <<"10.26.0.182:11000">>},
       {<<"parser">>, <<"10.26.0.182:9060">>},
       {<<"c_seq">>, <<"1 INVITE">>}]},
     chunks_3(2),
     {[{<<"call-id">>,<<"36-52896@10.26.0.167">>},
       {<<"timestamp">>,63601207277.60654},
       {<<"ref_timestamp">>,<<"63601207277.60659">>},
       {<<"label">>,<<"SIP/2.0 100 Trying">>},
       {<<"raw">>,
        [<<"SIP/2.0 100 Trying">>,
         <<"Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bK07b1.755e8b361c59510c3ade2d60ed2c0525.0">>,
         <<"Via: SIP/2.0/UDP 10.26.0.167:5060;rport=5060;branch=z9hG4bK-52896-36-0">>,
         <<"Record-Route: <sip:10.26.0.182;lr=on;ftag=52896SIPpTag0036>">>,
         <<"From: sipp <sip:sipp@10.26.0.167:5060>;tag=52896SIPpTag0036">>,
         <<"To: service <sip:service@10.26.0.182:5060>">>,
         <<"Call-ID: 36-52896@10.26.0.167">>,
         <<"CSeq: 1 INVITE">>,
         <<"User-Agent: 2600hz">>,
         <<"Content-Length: 0">>]},
       {<<"src">>, <<"10.26.0.182:11000">>},
       {<<"dst">>, <<"10.26.0.182:5060">>},
       {<<"parser">>, <<"10.26.0.182:9060">>},
       {<<"c_seq">>, <<"1 INVITE">>}]},
     chunks_3(4),
     chunks_3(5),
     chunks_3(6),
     {[{<<"call-id">>,<<"36-52896@10.26.0.167">>},
       {<<"timestamp">>,63601207277.61442},
       {<<"ref_timestamp">>,<<"63601207277.61446">>},
       {<<"label">>,<<"ACK sip:service@10.26.0.182:5060 SIP/2.0">>},
       {<<"raw">>,
        [<<"ACK sip:service@10.26.0.182:5060 SIP/2.0">>,
         <<"Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bK07b1.755e8b361c59510c3ade2d60ed2c0525.0">>,
         <<"From: sipp <sip:sipp@10.26.0.167:5060>;tag=52896SIPpTag0036">>,
         <<"To: service <sip:service@10.26.0.182:5060>;tag=8DZD96F5HDKmj">>,
         <<"Call-ID: 36-52896@10.26.0.167">>,
         <<"CSeq: 1 ACK">>,
         <<"Max-Forwards: 50">>,
         <<"Content-Length: 0">>]},
       {<<"src">>, <<"10.26.0.182:5060">>},
       {<<"dst">>, <<"10.26.0.182:11000">>},
       {<<"parser">>, <<"10.26.0.182:9060">>},
       {<<"c_seq">>, <<"1 ACK">>}]},
     {[{<<"call-id">>,<<"36-52896@10.26.0.167">>},
       {<<"timestamp">>,63601207277.60928},
       {<<"ref_timestamp">>,<<"63601207277.60992">>},
       {<<"label">>,<<"SIP/2.0 407 Proxy Authentication Required">>},
       {<<"raw">>,
        [<<"SIP/2.0 407 Proxy Authentication Required">>,
         <<"Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bK07b1.755e8b361c59510c3ade2d60ed2c0525.0">>,
         <<"Via: SIP/2.0/UDP 10.26.0.167:5060;rport=5060;branch=z9hG4bK-52896-36-0">>,
         <<"From: sipp <sip:sipp@10.26.0.167:5060>;tag=52896SIPpTag0036">>,
         <<"To: service <sip:service@10.26.0.182:5060>;tag=8DZD96F5HDKmj">>,
         <<"Call-ID: 36-52896@10.26.0.167">>,
         <<"CSeq: 1 INVITE">>,
         <<"User-Agent: 2600hz">>,
         <<"Accept: application/sdp">>,
         <<"Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, MESSAGE, INFO, UPDATE, REGISTER, REFER, NOTIFY, PUBLISH, SUBSCRIBE">>,
         <<"Supported: path, replaces">>,
         <<"Allow-Events: talk, hold, conference, presence, as-feature-event, dialog, line-seize, call-info, sla, include-session-description, presence.winfo, message-summary, refer">>,
         <<"Proxy-Authenticate: Digest realm=\"10.26.0.167\", nonce=\"bf48ea84-0fdd-11e5-9700-7b9c4900a590\", algorithm=MD5, qop=\"auth\"">>,
         <<"Content-Length: 0">>]},
       {<<"src">>, <<"10.26.0.182:11000">>},
       {<<"dst">>, <<"10.26.0.182:5060">>},
       {<<"parser">>, <<"10.26.0.182:9060">>},
       {<<"c_seq">>, <<"1 INVITE">>}]},
     chunks_3(7),
     chunks_3(8)].


chunks_2() -> [chunks_2(2), chunks_2(1)].
chunks_2('count') -> 2;
chunks_2('entities') -> [<<"10.26.0.182:5060">>, <<"10.26.0.182:11000">>];
chunks_2(1) ->
    {[{<<"call-id">>,<<"5dca43e524c680cf-13867@10.26.0.182">>},
      {<<"timestamp">>,63601204677.8817},
      {<<"ref_timestamp">>,<<"63601204677.88179">>},
      {<<"label">>,<<"OPTIONS sip:10.26.0.182:11000 SIP/2.0">>},
      {<<"raw">>,
       [<<"OPTIONS sip:10.26.0.182:11000 SIP/2.0">>,
        <<"Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bK6551.e2149fb2000000000000000000000000.0">>,
        <<"To: <sip:10.26.0.182:11000>">>,
        <<"From: <sip:sipcheck@10.26.0.182>;tag=6a9eb17cd14528bda74bd05e73f170de-4acf">>,
        <<"CSeq: 10 OPTIONS">>,
        <<"Call-ID: 5dca43e524c680cf-13867@10.26.0.182">>,
        <<"Max-Forwards: 70">>,
        <<"Content-Length: 0">>]},
      {<<"src">>, <<"10.26.0.182:5060">>},
      {<<"dst">>, <<"10.26.0.182:11000">>},
      {<<"parser">>, <<"10.26.0.182:9060">>},
      {<<"c_seq">>, <<"10 OPTIONS">>}]};
chunks_2(2) ->
    {[{<<"call-id">>,<<"5dca43e524c680cf-13867@10.26.0.182">>},
      {<<"timestamp">>,63601204677.89242},
      {<<"ref_timestamp">>,<<"63601204677.892494">>},
      {<<"label">>,<<"SIP/2.0 200 OK">>},
      {<<"raw">>,
       [<<"SIP/2.0 200 OK">>,
        <<"Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bK6551.e2149fb2000000000000000000000000.0">>,
        <<"From: <sip:sipcheck@10.26.0.182>;tag=6a9eb17cd14528bda74bd05e73f170de-4acf">>,
        <<"To: <sip:10.26.0.182:11000>;tag=15vvXS6m6868r">>,
        <<"Call-ID: 5dca43e524c680cf-13867@10.26.0.182">>,
        <<"CSeq: 10 OPTIONS">>,
        <<"Contact: <sip:10.26.0.182:11000>">>,
        <<"User-Agent: 2600hz">>,
        <<"Accept: application/sdp">>,
        <<"Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, MESSAGE, INFO, UPDATE, REGISTER, REFER, NOTIFY, PUBLISH, SUBSCRIBE">>,
        <<"Supported: path, replaces">>,
        <<"Allow-Events: talk, hold, conference, presence, as-feature-event, dialog, line-seize, call-info, sla, include-session-description, presence.winfo, message-summary, refer">>,
        <<"Content-Length: 0">>]},
      {<<"src">>, <<"10.26.0.182:11000">>},
      {<<"dst">>, <<"10.26.0.182:5060">>},
      {<<"parser">>, <<"10.26.0.182:9060">>},
      {<<"c_seq">>, <<"10 OPTIONS">>}]}.


chunks_1('count') -> 20;
chunks_1('entities') ->
    [<<"10.26.0.101:7653">>, <<"10.26.0.182:5060">>, <<"10.26.0.182:11000">>];

chunks_1(1) ->
    {[{<<"call-id">>,<<"63-6680@10.26.0.101">>},
      {<<"timestamp">>,63165039051.653694},
      {<<"ref_timestamp">>,<<"63601112379.24342">>},
      {<<"label">>,<<"INVITE sip:347@10.26.0.182:5060 SIP/2.0">>},
      {<<"raw">>,
       [<<"INVITE sip:347@10.26.0.182:5060 SIP/2.0">>,
        <<"Via: SIP/2.0/UDP 10.26.0.101:7653;branch=z9hG4bK63">>,
        <<"From: \"user_x2d24bd3dq\" <sip:user_x2d24bd3dq@wefwefwefwef.2600hz.com>;tag=63">>,
        <<"To: <sip:347@wefwefwefwef.2600hz.com>">>,
        <<"Call-ID: 63-6680@10.26.0.101">>,
        <<"CSeq: 1 INVITE">>,
        <<"Contact: <sip:user_x2d24bd3dq@10.26.0.101:7653>">>,
        <<"Content-Type: application/sdp">>,
        <<"Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, NOTIFY, REGISTER">>,
        <<"Max-Forwards: 70">>,
        <<"User-Agent: SIPp">>,
        <<"Allow-Events: talk,hold,conference,refer,check-sync">>,
        <<"Content-Length:   189">>,
        <<>>,
        <<"v=0">>,
        <<"o=user1 53655765 2353687637 IN IP4 10.26.0.101">>,
        <<"s=-">>,
        <<"c=IN IP4 10.26.0.101">>,
        <<"t=0 0">>,
        <<"m=audio 6000 RTP/AVP 0 101">>,
        <<"a=rtpmap:0 PCMU/8000">>,
        <<"a=rtpmap:101 telephone-event/8000">>,
        <<"a=fmtp:101 0-15">>]},
      {<<"src">>, <<"10.26.0.101:7653">>},
      {<<"dst">>, <<"10.26.0.182:5060">>},
      {<<"parser">>, <<"10.26.0.182:9061">>},
      {<<"c_seq">>, <<"1 INVITE">>}]};

chunks_1(2) ->
    {[{<<"call-id">>,<<"63-6680@10.26.0.101">>},
      {<<"timestamp">>,63165037407.683136},
      {<<"ref_timestamp">>,<<"63601112379.24456">>},
      {<<"label">>,<<"SIP/2.0 100 Attempting to connect your call">>},
      {<<"raw">>,
       [<<"SIP/2.0 100 Attempting to connect your call">>,
        <<"Via: SIP/2.0/UDP 10.26.0.101:7653;branch=z9hG4bK63;rport=7653">>,
        <<"From: \"user_x2d24bd3dq\" <sip:user_x2d24bd3dq@wefwefwefwef.2600hz.com>;tag=63">>,
        <<"To: <sip:347@wefwefwefwef.2600hz.com>">>,
        <<"Call-ID: 63-6680@10.26.0.101">>,
        <<"CSeq: 1 INVITE">>,
        <<"Content-Length: 0">>]},
      {<<"src">>, <<"10.26.0.182:5060">>},
      {<<"dst">>, <<"10.26.0.101:7653">>},
      {<<"parser">>, <<"10.26.0.182:9061">>},
      {<<"c_seq">>, <<"1 INVITE">>}]};

chunks_1(3) ->
    {[{<<"call-id">>,<<"63-6680@10.26.0.101">>},
      {<<"timestamp">>,63165035797.13593},
      {<<"ref_timestamp">>,<<"63601112379.24483">>},
      {<<"label">>,<<"INVITE sip:347@10.26.0.182:5060 SIP/2.0">>},
      {<<"raw">>,
       [<<"INVITE sip:347@10.26.0.182:5060 SIP/2.0">>,
        <<"Record-Route: <sip:10.26.0.182;lr=on;ftag=63>">>,
        <<"Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bKc105.39d0b071313f7fa81a42c21d3f74cb5f.0">>,
        <<"Via: SIP/2.0/UDP 10.26.0.101:7653;rport=7653;branch=z9hG4bK63">>,
        <<"From: \"user_x2d24bd3dq\" <sip:user_x2d24bd3dq@wefwefwefwef.2600hz.com>;tag=63">>,
        <<"To: <sip:347@wefwefwefwef.2600hz.com>">>,
        <<"Call-ID: 63-6680@10.26.0.101">>,
        <<"CSeq: 1 INVITE">>,
        <<"Contact: <sip:user_x2d24bd3dq@10.26.0.101:7653>">>,
        <<"Content-Type: application/sdp">>,
        <<"Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, NOTIFY, REGISTER">>,
        <<"Max-Forwards: 50">>,
        <<"User-Agent: SIPp">>,
        <<"Allow-Events: talk,hold,conference,refer,check-sync">>,
        <<"Content-Length:   189">>,
        <<"X-AUTH-IP: 10.26.0.101">>,
        <<"X-AUTH-PORT: 7653">>,
        <<>>,
        <<"v=0">>,
        <<"o=user1 53655765 2353687637 IN IP4 10.26.0.101">>,
        <<"s=-">>,
        <<"c=IN IP4 10.26.0.101">>,
        <<"t=0 0">>,
        <<"m=audio 6000 RTP/AVP 0 101">>,
        <<"a=rtpmap:0 PCMU/8000">>,
        <<"a=rtpmap:101 telephone-event/8000">>,
        <<"a=fmtp:101 0-15">>]},
      {<<"src">>, <<"10.26.0.182:5060">>},
      {<<"dst">>, <<"10.26.0.182:11000">>},
      {<<"parser">>, <<"10.26.0.182:9061">>},
      {<<"c_seq">>, <<"1 INVITE">>}]};

chunks_1(4) ->
    {[{<<"call-id">>,<<"63-6680@10.26.0.101">>},
      {<<"timestamp">>,63165037727.76096},
      {<<"ref_timestamp">>,<<"63601112379.24857">>},
      {<<"label">>,<<"SIP/2.0 100 Trying">>},
      {<<"raw">>,
       [<<"SIP/2.0 100 Trying">>,
        <<"Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bKc105.39d0b071313f7fa81a42c21d3f74cb5f.0">>,
        <<"Via: SIP/2.0/UDP 10.26.0.101:7653;rport=7653;branch=z9hG4bK63">>,
        <<"Record-Route: <sip:10.26.0.182;lr=on;ftag=63>">>,
        <<"From: \"user_x2d24bd3dq\" <sip:user_x2d24bd3dq@wefwefwefwef.2600hz.com>;tag=63">>,
        <<"To: <sip:347@wefwefwefwef.2600hz.com>">>,
        <<"Call-ID: 63-6680@10.26.0.101">>,
        <<"CSeq: 1 INVITE">>,
        <<"User-Agent: 2600hz">>,
        <<"Content-Length: 0">>]},
      {<<"src">>, <<"10.26.0.182:11000">>},
      {<<"dst">>, <<"10.26.0.182:5060">>},
      {<<"parser">>, <<"10.26.0.182:9061">>},
      {<<"c_seq">>, <<"1 INVITE">>}]};

chunks_1(5) ->
    {[{<<"call-id">>,<<"63-6680@10.26.0.101">>},
      {<<"timestamp">>,63165036554.60102},
      {<<"ref_timestamp">>,<<"63601112379.25335">>},
      {<<"label">>,<<"SIP/2.0 407 Proxy Authentication Required">>},
      {<<"raw">>,
       [<<"SIP/2.0 407 Proxy Authentication Required">>,
        <<"Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bKc105.39d0b071313f7fa81a42c21d3f74cb5f.0">>,
        <<"Via: SIP/2.0/UDP 10.26.0.101:7653;rport=7653;branch=z9hG4bK63">>,
        <<"From: \"user_x2d24bd3dq\" <sip:user_x2d24bd3dq@wefwefwefwef.2600hz.com>;tag=63">>,
        <<"To: <sip:347@wefwefwefwef.2600hz.com>;tag=NpFcj556Bv9aK">>,
        <<"Call-ID: 63-6680@10.26.0.101">>,
        <<"CSeq: 1 INVITE">>,
        <<"User-Agent: 2600hz">>,
        <<"Accept: application/sdp">>,
        <<"Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, MESSAGE, INFO, UPDATE, REGISTER, REFER, NOTIFY, PUBLISH, SUBSCRIBE">>,
        <<"Supported: path, replaces">>,
        <<"Allow-Events: talk, hold, conference, presence, as-feature-event, dialog, line-seize, call-info, sla, include-session-description, presence.winfo, message-summary, refer">>,
        <<"Proxy-Authenticate: Digest realm=\"wefwefwefwef.2600hz.com\", nonce=\"cb74f704-0f00-11e5-9502-7b9c4900a590\", algorithm=MD5, qop=\"auth\"">>,
        <<"Content-Length: 0">>]},
      {<<"src">>, <<"10.26.0.182:11000">>},
      {<<"dst">>, <<"10.26.0.182:5060">>},
      {<<"parser">>, <<"10.26.0.182:9061">>},
      {<<"c_seq">>, <<"1 INVITE">>}]};

chunks_1(6) ->
    {[{<<"call-id">>,<<"63-6680@10.26.0.101">>},
      {<<"timestamp">>,63165037309.575745},
      {<<"ref_timestamp">>,<<"63601112379.253555">>},
      {<<"label">>,<<"ACK sip:347@10.26.0.182:5060 SIP/2.0">>},
      {<<"raw">>,
       [<<"ACK sip:347@10.26.0.182:5060 SIP/2.0">>,
        <<"Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bKc105.39d0b071313f7fa81a42c21d3f74cb5f.0">>,
        <<"From: \"user_x2d24bd3dq\" <sip:user_x2d24bd3dq@wefwefwefwef.2600hz.com>;tag=63">>,
        <<"To: <sip:347@wefwefwefwef.2600hz.com>;tag=NpFcj556Bv9aK">>,
        <<"Call-ID: 63-6680@10.26.0.101">>,
        <<"CSeq: 1 ACK">>,
        <<"Max-Forwards: 50">>,
        <<"Content-Length: 0">>]},
      {<<"src">>, <<"10.26.0.182:5060">>},
      {<<"dst">>, <<"10.26.0.182:11000">>},
      {<<"parser">>, <<"10.26.0.182:9061">>},
      {<<"c_seq">>, <<"1 ACK">>}]};

chunks_1(7) ->
    {[{<<"call-id">>,<<"63-6680@10.26.0.101">>},
      {<<"timestamp">>,63165035480.924736},
      {<<"ref_timestamp">>,<<"63601112379.253624">>},
      {<<"label">>,<<"SIP/2.0 407 Proxy Authentication Required">>},
      {<<"raw">>,
       [<<"SIP/2.0 407 Proxy Authentication Required">>,
        <<"Via: SIP/2.0/UDP 10.26.0.101:7653;rport=7653;branch=z9hG4bK63">>,
        <<"From: \"user_x2d24bd3dq\" <sip:user_x2d24bd3dq@wefwefwefwef.2600hz.com>;tag=63">>,
        <<"To: <sip:347@wefwefwefwef.2600hz.com>;tag=NpFcj556Bv9aK">>,
        <<"Call-ID: 63-6680@10.26.0.101">>,
        <<"CSeq: 1 INVITE">>,
        <<"User-Agent: 2600hz">>,
        <<"Accept: application/sdp">>,
        <<"Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, MESSAGE, INFO, UPDATE, REGISTER, REFER, NOTIFY, PUBLISH, SUBSCRIBE">>,
        <<"Supported: path, replaces">>,
        <<"Allow-Events: talk, hold, conference, presence, as-feature-event, dialog, line-seize, call-info, sla, include-session-description, presence.winfo, message-summary, refer">>,
        <<"Proxy-Authenticate: Digest realm=\"wefwefwefwef.2600hz.com\", nonce=\"cb74f704-0f00-11e5-9502-7b9c4900a590\", algorithm=MD5, qop=\"auth\"">>,
        <<"Content-Length: 0">>]},
      {<<"src">>, <<"10.26.0.182:5060">>},
      {<<"dst">>, <<"10.26.0.101:7653">>},
      {<<"parser">>, <<"10.26.0.182:9061">>},
      {<<"c_seq">>, <<"1 INVITE">>}]};

chunks_1(8) ->
    {[{<<"call-id">>,<<"63-6680@10.26.0.101">>},
      {<<"timestamp">>,63165038400.42246},
      {<<"ref_timestamp">>,<<"63601112379.255104">>},
      {<<"label">>,<<"ACK sip:347@10.26.0.182:5060 SIP/2.0">>},
      {<<"raw">>,
       [<<"ACK sip:347@10.26.0.182:5060 SIP/2.0">>,
        <<"Via: SIP/2.0/UDP 10.26.0.101:7653;rport=7653;branch=z9hG4bK63">>,
        <<"From: \"user_x2d24bd3dq\" <sip:user_x2d24bd3dq@wefwefwefwef.2600hz.com>;tag=63">>,
        <<"To: <sip:347@10.26.0.182:5060>;tag=NpFcj556Bv9aK">>,
        <<"Call-ID: 63-6680@10.26.0.101">>,
        <<"CSeq: 1 ACK">>,
        <<"Content-Length: 0">>]},
      {<<"src">>, <<"10.26.0.101:7653">>},
      {<<"dst">>, <<"10.26.0.182:5060">>},
      {<<"parser">>, <<"10.26.0.182:9061">>},
      {<<"c_seq">>, <<"1 ACK">>}]};

chunks_1(9) ->
    {[{<<"call-id">>,<<"63-6680@10.26.0.101">>},
      {<<"timestamp">>,63165037477.74112},
      {<<"ref_timestamp">>,<<"63601112379.25619">>},
      {<<"label">>,<<"INVITE sip:347@10.26.0.182:5060 SIP/2.0">>},
      {<<"raw">>,
       [<<"INVITE sip:347@10.26.0.182:5060 SIP/2.0">>,
        <<"Via: SIP/2.0/UDP 10.26.0.101:7653;rport=7653;branch=z9hG4bK63">>,
        <<"From: \"user_x2d24bd3dq\" <sip:user_x2d24bd3dq@wefwefwefwef.2600hz.com>;tag=63">>,
        <<"To: <sip:347@10.26.0.182:5060>">>,<<"Call-ID: 63-6680@10.26.0.101">>,
        <<"CSeq: 2 INVITE">>,
        <<"Contact: <sip:user_x2d24bd3dq@10.26.0.101:7653>">>,
        <<"Proxy-Authorization: Digest username=\"user_x2d24bd3dq\",realm=\"wefwefwefwef.2600hz.com\",cnonce=\"737b8ddc\",nc=00000001,qop=auth,uri=\"sip:10.26.0.182:5060\",nonce=\"cb74f704-0f00-11e5-9502-7b9c4900a590\",response=\"6a5b1f5610af652446e874ca6596fded\",algorithm=MD5">>,
        <<"Content-Type: application/sdp">>,
        <<"Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, NOTIFY, REGISTER">>,
        <<"Max-Forwards: 70">>,
        <<"User-Agent: SIPp">>,
        <<"Allow-Events: talk,hold,conference,refer,check-sync">>,
        <<"Content-Length:   189">>,
        <<>>,
        <<"v=0">>,
        <<"o=user1 53655765 2353687637 IN IP4 10.26.0.101">>,
        <<"s=-">>,
        <<"c=IN IP4 10.26.0.101">>,
        <<"t=0 0">>,
        <<"m=audio 6000 RTP/AVP 0 101">>,
        <<"a=rtpmap:0 PCMU/8000">>,
        <<"a=rtpmap:101 telephone-event/8000">>,
        <<"a=fmtp:101 0-15">>]},
      {<<"src">>, <<"10.26.0.101:7653">>},
      {<<"dst">>, <<"10.26.0.182:5060">>},
      {<<"parser">>, <<"10.26.0.182:9061">>},
      {<<"c_seq">>, <<"2 INVITE">>}]};

chunks_1(10) ->
    {[{<<"call-id">>,<<"63-6680@10.26.0.101">>},
      {<<"timestamp">>,63165037998.1625},
      {<<"ref_timestamp">>,<<"63601112379.25673">>},
      {<<"label">>,<<"SIP/2.0 100 Attempting to connect your call">>},
      {<<"raw">>,
       [<<"SIP/2.0 100 Attempting to connect your call">>,
        <<"Via: SIP/2.0/UDP 10.26.0.101:7653;rport=7653;branch=z9hG4bK63;received=10.26.0.101">>,
        <<"From: \"user_x2d24bd3dq\" <sip:user_x2d24bd3dq@wefwefwefwef.2600hz.com>;tag=63">>,
        <<"To: <sip:347@10.26.0.182:5060>">>,
        <<"Call-ID: 63-6680@10.26.0.101">>,
        <<"CSeq: 2 INVITE">>,
        <<"Content-Length: 0">>]},
      {<<"src">>, <<"10.26.0.182:5060">>},
      {<<"dst">>, <<"10.26.0.101:7653">>},
      {<<"parser">>, <<"10.26.0.182:9061">>},
      {<<"c_seq">>, <<"2 INVITE">>}]};

chunks_1(11) ->
    {[{<<"call-id">>,<<"63-6680@10.26.0.101">>},
      {<<"timestamp">>,63165039474.5575},
      {<<"ref_timestamp">>,<<"63601112379.257034">>},
      {<<"label">>,<<"INVITE sip:347@10.26.0.182:5060 SIP/2.0">>},
      {<<"raw">>,
       [<<"INVITE sip:347@10.26.0.182:5060 SIP/2.0">>,
        <<"Record-Route: <sip:10.26.0.182;lr=on;ftag=63>">>,
        <<"Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bK9105.39d0b071313f7fa81a42c21d3f74cb5f.0">>,
        <<"Via: SIP/2.0/UDP 10.26.0.101:7653;received=10.26.0.101;rport=7653;branch=z9hG4bK63">>,
        <<"From: \"user_x2d24bd3dq\" <sip:user_x2d24bd3dq@wefwefwefwef.2600hz.com>;tag=63">>,
        <<"To: <sip:347@10.26.0.182:5060>">>,<<"Call-ID: 63-6680@10.26.0.101">>,
        <<"CSeq: 2 INVITE">>,
        <<"Contact: <sip:user_x2d24bd3dq@10.26.0.101:7653>">>,
        <<"Proxy-Authorization: Digest username=\"user_x2d24bd3dq\",realm=\"wefwefwefwef.2600hz.com\",cnonce=\"737b8ddc\",nc=00000001,qop=auth,uri=\"sip:10.26.0.182:5060\",nonce=\"cb74f704-0f00-11e5-9502-7b9c4900a590\",response=\"6a5b1f5610af652446e874ca6596fded\",algorithm=MD5">>,
        <<"Content-Type: application/sdp">>,
        <<"Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, NOTIFY, REGISTER">>,
        <<"Max-Forwards: 50">>,
        <<"User-Agent: SIPp">>,
        <<"Allow-Events: talk,hold,conference,refer,check-sync">>,
        <<"Content-Length:   189">>,
        <<"X-AUTH-IP: 10.26.0.101">>,
        <<"X-AUTH-PORT: 7653">>,
        <<>>,
        <<"v=0">>,
        <<"o=user1 53655765 2353687637 IN IP4 10.26.0.101">>,
        <<"s=-">>,
        <<"c=IN IP4 10.26.0.101">>,
        <<"t=0 0">>,
        <<"m=audio 6000 RTP/AVP 0 101">>,
        <<"a=rtpmap:0 PCMU/8000">>,
        <<"a=rtpmap:101 telephone-event/8000">>,
        <<"a=fmtp:101 0-15">>]},
      {<<"src">>, <<"10.26.0.182:5060">>},
      {<<"dst">>, <<"10.26.0.182:11000">>},
      {<<"parser">>, <<"10.26.0.182:9061">>},
      {<<"c_seq">>, <<"2 INVITE">>}]};

chunks_1(12) ->
    {[{<<"call-id">>,<<"63-6680@10.26.0.101">>},
      {<<"timestamp">>,63165039393.55527},
      {<<"ref_timestamp">>,<<"63601112379.267525">>},
      {<<"label">>,<<"SIP/2.0 100 Trying">>},
      {<<"raw">>,
       [<<"SIP/2.0 100 Trying">>,
        <<"Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bK9105.39d0b071313f7fa81a42c21d3f74cb5f.0">>,
        <<"Via: SIP/2.0/UDP 10.26.0.101:7653;received=10.26.0.101;rport=7653;branch=z9hG4bK63">>,
        <<"Record-Route: <sip:10.26.0.182;lr=on;ftag=63>">>,
        <<"From: \"user_x2d24bd3dq\" <sip:user_x2d24bd3dq@wefwefwefwef.2600hz.com>;tag=63">>,
        <<"To: <sip:347@10.26.0.182:5060>">>,
        <<"Call-ID: 63-6680@10.26.0.101">>,
        <<"CSeq: 2 INVITE">>,
        <<"User-Agent: 2600hz">>,
        <<"Content-Length: 0">>]},
      {<<"src">>, <<"10.26.0.182:11000">>},
      {<<"dst">>, <<"10.26.0.182:5060">>},
      {<<"parser">>, <<"10.26.0.182:9061">>},
      {<<"c_seq">>, <<"2 INVITE">>}]};

chunks_1(13) ->
    {[{<<"call-id">>,<<"63-6680@10.26.0.101">>},
      {<<"timestamp">>,63181814297.0512},
      {<<"ref_timestamp">>,<<"63601112380.084305">>},
      {<<"label">>,<<"SIP/2.0 200 OK">>},
      {<<"raw">>,
       [<<"SIP/2.0 200 OK">>,
        <<"Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bK9105.39d0b071313f7fa81a42c21d3f74cb5f.0">>,
        <<"Via: SIP/2.0/UDP 10.26.0.101:7653;received=10.26.0.101;rport=7653;branch=z9hG4bK63">>,
        <<"Record-Route: <sip:10.26.0.182;lr=on;ftag=63>">>,
        <<"From: \"user_x2d24bd3dq\" <sip:user_x2d24bd3dq@wefwefwefwef.2600hz.com>;tag=63">>,
        <<"To: <sip:347@10.26.0.182:5060>;tag=pZ84K0pa94ZXe">>,
        <<"Call-ID: 63-6680@10.26.0.101">>,
        <<"CSeq: 2 INVITE">>,
        <<"Contact: <sip:347@10.26.0.182:11000;transport=udp>">>,
        <<"User-Agent: 2600hz">>,<<"Accept: application/sdp">>,
        <<"Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, MESSAGE, INFO, UPDATE, REGISTER, REFER, NOTIFY, PUBLISH, SUBSCRIBE">>,
        <<"Supported: path, replaces">>,
        <<"Allow-Events: talk, hold, conference, presence, as-feature-event, dialog, line-seize, call-info, sla, include-session-description, presence.winfo, message-summary, refer">>,
        <<"Content-Type: application/sdp">>,
        <<"Content-Disposition: session">>,
        <<"Content-Length: 218">>,
        <<"Remote-Party-ID: \"347\" <sip:347@10.26.0.182>;party=calling;privacy=off;screen=no">>,
        <<>>,
        <<"v=0">>,
        <<"o=FreeSWITCH 1433315385 1433315386 IN IP4 10.26.0.182">>,
        <<"s=FreeSWITCH">>,
        <<"c=IN IP4 10.26.0.182">>,
        <<"t=0 0">>,
        <<"m=audio 28844 RTP/AVP 0 101">>,
        <<"a=rtpmap:0 PCMU/8000">>,
        <<"a=rtpmap:101 telephone-event/8000">>,
        <<"a=fmtp:101 0-16">>,
        <<"a=ptime:20">>]},
      {<<"src">>, <<"10.26.0.182:11000">>},
      {<<"dst">>, <<"10.26.0.182:5060">>},
      {<<"parser">>, <<"10.26.0.182:9061">>},
      {<<"c_seq">>, <<"2 INVITE">>}]};

chunks_1(14) ->
    {[{<<"call-id">>,<<"63-6680@10.26.0.101">>},
      {<<"timestamp">>,63181813491.81037},
      {<<"ref_timestamp">>,<<"63601112380.085335">>},
      {<<"label">>,<<"SIP/2.0 200 OK">>},
      {<<"raw">>,
       [<<"SIP/2.0 200 OK">>,
        <<"Via: SIP/2.0/UDP 10.26.0.101:7653;received=10.26.0.101;rport=7653;branch=z9hG4bK63">>,
        <<"Record-Route: <sip:10.26.0.182;lr=on;ftag=63>">>,
        <<"From: \"user_x2d24bd3dq\" <sip:user_x2d24bd3dq@wefwefwefwef.2600hz.com>;tag=63">>,
        <<"To: <sip:347@10.26.0.182:5060>;tag=pZ84K0pa94ZXe">>,
        <<"Call-ID: 63-6680@10.26.0.101">>,
        <<"CSeq: 2 INVITE">>,
        <<"Contact: <sip:347@10.26.0.182:11000;transport=udp>">>,
        <<"User-Agent: 2600hz">>,
        <<"Accept: application/sdp">>,
        <<"Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, MESSAGE, INFO, UPDATE, REGISTER, REFER, NOTIFY, PUBLISH, SUBSCRIBE">>,
        <<"Supported: path, replaces">>,
        <<"Allow-Events: talk, hold, conference, presence, as-feature-event, dialog, line-seize, call-info, sla, include-session-description, presence.winfo, message-summary, refer">>,
        <<"Content-Type: application/sdp">>,
        <<"Content-Disposition: session">>,
        <<"Content-Length: 218">>,
        <<"Remote-Party-ID: \"347\" <sip:347@10.26.0.182>;party=calling;privacy=off;screen=no">>,
        <<>>,
        <<"v=0">>,
        <<"o=FreeSWITCH 1433315385 1433315386 IN IP4 10.26.0.182">>,
        <<"s=FreeSWITCH">>,
        <<"c=IN IP4 10.26.0.182">>,
        <<"t=0 0">>,
        <<"m=audio 28844 RTP/AVP 0 101">>,
        <<"a=rtpmap:0 PCMU/8000">>,
        <<"a=rtpmap:101 telephone-event/8000">>,
        <<"a=fmtp:101 0-16">>,
        <<"a=ptime:20">>]},
      {<<"src">>, <<"10.26.0.182:5060">>},
      {<<"dst">>, <<"10.26.0.101:7653">>},
      {<<"parser">>, <<"10.26.0.182:9061">>},
      {<<"c_seq">>, <<"2 INVITE">>}]};

chunks_1(15) ->
    {[{<<"call-id">>,<<"63-6680@10.26.0.101">>},
      {<<"timestamp">>,63181816629.34637},
      {<<"ref_timestamp">>,<<"63601112380.08586">>},
      {<<"label">>,<<"ACK sip:347@10.26.0.182:11000;transport=udp SIP/2.0">>},
      {<<"raw">>,
       [<<"ACK sip:347@10.26.0.182:11000;transport=udp SIP/2.0">>,
        <<"Route: <sip:10.26.0.182;lr=on;ftag=63>">>,
        <<"Via: SIP/2.0/UDP 10.26.0.101:7653;received=10.26.0.101;rport=7653;branch=z9hG4bK63">>,
        <<"From: \"user_x2d24bd3dq\" <sip:user_x2d24bd3dq@wefwefwefwef.2600hz.com>;tag=63">>,
        <<"To: <sip:347@10.26.0.182:5060>;tag=pZ84K0pa94ZXe">>,
        <<"Call-ID: 63-6680@10.26.0.101">>,
        <<"Contact: <sip:347@10.26.0.182:11000;transport=udp>">>,
        <<"CSeq: 2 ACK">>,
        <<"Max-Forwards: 70">>,
        <<"Subject: Performance Test">>,
        <<"Content-Length: 0">>]},
      {<<"src">>, <<"10.26.0.101:7653">>},
      {<<"dst">>, <<"10.26.0.182:5060">>},
      {<<"parser">>, <<"10.26.0.182:9061">>},
      {<<"c_seq">>, <<"2 ACK">>}]};

chunks_1(16) ->
    {[{<<"call-id">>,<<"63-6680@10.26.0.101">>},
      {<<"timestamp">>,63601112380.09604},
      {<<"ref_timestamp">>,<<"63601112380.096794">>},
      {<<"label">>,<<"ACK sip:347@10.26.0.182:11000;transport=udp SIP/2.0">>},
      {<<"raw">>,
       [<<"ACK sip:347@10.26.0.182:11000;transport=udp SIP/2.0">>,
        <<"Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bK9105.39d0b071313f7fa81a42c21d3f74cb5f.0">>,
        <<"Via: SIP/2.0/UDP 10.26.0.101:7653;received=10.26.0.101;rport=7653;branch=z9hG4bK63">>,
        <<"From: \"user_x2d24bd3dq\" <sip:user_x2d24bd3dq@wefwefwefwef.2600hz.com>;tag=63">>,
        <<"To: <sip:347@10.26.0.182:5060>;tag=pZ84K0pa94ZXe">>,
        <<"Call-ID: 63-6680@10.26.0.101">>,
        <<"Contact: <sip:347@10.26.0.101:7653;transport=udp>">>,
        <<"CSeq: 2 ACK">>,
        <<"Max-Forwards: 50">>,
        <<"Subject: Performance Test">>,
        <<"Content-Length: 0">>,
        <<"X-AUTH-IP: 10.26.0.101">>,
        <<"X-AUTH-PORT: 7653">>]},
      {<<"src">>, <<"10.26.0.182:5060">>},
      {<<"dst">>, <<"10.26.0.182:11000">>},
      {<<"parser">>, <<"10.26.0.182:9060">>},
      {<<"c_seq">>, <<"2 ACK">>}]};

chunks_1(17) ->
    {[{<<"call-id">>,<<"63-6680@10.26.0.101">>},
      {<<"timestamp">>,63232146097.02918},
      {<<"ref_timestamp">>,<<"63601112383.085655">>},
      {<<"label">>,<<"BYE sip:347@10.26.0.182:11000;transport=udp SIP/2.0">>},
      {<<"raw">>,
       [<<"BYE sip:347@10.26.0.182:11000;transport=udp SIP/2.0">>,
        <<"Route: <sip:10.26.0.182;lr=on;ftag=63>">>,
        <<"Via: SIP/2.0/UDP 10.26.0.101:7653;received=10.26.0.101;rport=7653;branch=z9hG4bK63">>,
        <<"From: \"user_x2d24bd3dq\" <sip:user_x2d24bd3dq@wefwefwefwef.2600hz.com>;tag=63">>,
        <<"To: <sip:347@10.26.0.182:5060>;tag=pZ84K0pa94ZXe">>,
        <<"Call-ID: 63-6680@10.26.0.101">>,
        <<"Contact: <sip:347@10.26.0.182:11000;transport=udp>">>,
        <<"CSeq: 3 BYE">>,
        <<"Max-Forwards: 70">>,
        <<"Subject: Performance Test">>,
        <<"Content-Length: 0">>]},
      {<<"src">>, <<"10.26.0.101:7653">>},
      {<<"dst">>, <<"10.26.0.182:5060">>},
      {<<"parser">>, <<"10.26.0.182:9061">>},
      {<<"c_seq">>, <<"3 BYE">>}]};

chunks_1(18) ->
    {[{<<"call-id">>,<<"63-6680@10.26.0.101">>},
      {<<"timestamp">>,63232147992.92013},
      {<<"ref_timestamp">>,<<"63601112383.08595">>},
      {<<"label">>,<<"BYE sip:347@10.26.0.182:11000;transport=udp SIP/2.0">>},
      {<<"raw">>,
       [<<"BYE sip:347@10.26.0.182:11000;transport=udp SIP/2.0">>,
        <<"Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bKa105.39d0b071313f7fa81a42c21d3f74cb5f.0">>,
        <<"Via: SIP/2.0/UDP 10.26.0.101:7653;received=10.26.0.101;rport=7653;branch=z9hG4bK63">>,
        <<"From: \"user_x2d24bd3dq\" <sip:user_x2d24bd3dq@wefwefwefwef.2600hz.com>;tag=63">>,
        <<"To: <sip:347@10.26.0.182:5060>;tag=pZ84K0pa94ZXe">>,
        <<"Call-ID: 63-6680@10.26.0.101">>,
        <<"Contact: <sip:347@10.26.0.101:7653;transport=udp>">>,
        <<"CSeq: 3 BYE">>,
        <<"Max-Forwards: 50">>,
        <<"Subject: Performance Test">>,
        <<"Content-Length: 0">>,
        <<"X-AUTH-IP: 10.26.0.101">>,
        <<"X-AUTH-PORT: 7653">>]},
      {<<"src">>, <<"10.26.0.182:5060">>},
      {<<"dst">>, <<"10.26.0.182:11000">>},
      {<<"parser">>, <<"10.26.0.182:9061">>},
      {<<"c_seq">>, <<"3 BYE">>}]};

chunks_1(19) ->
    {[{<<"call-id">>,<<"63-6680@10.26.0.101">>},
      {<<"timestamp">>,63232144743.05543},
      {<<"ref_timestamp">>,<<"63601112383.10484">>},
      {<<"label">>,<<"SIP/2.0 200 OK">>},
      {<<"raw">>,
       [<<"SIP/2.0 200 OK">>,
        <<"Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bKa105.39d0b071313f7fa81a42c21d3f74cb5f.0">>,
        <<"Via: SIP/2.0/UDP 10.26.0.101:7653;received=10.26.0.101;rport=7653;branch=z9hG4bK63">>,
        <<"From: \"user_x2d24bd3dq\" <sip:user_x2d24bd3dq@wefwefwefwef.2600hz.com>;tag=63">>,
        <<"To: <sip:347@10.26.0.182:5060>;tag=pZ84K0pa94ZXe">>,
        <<"Call-ID: 63-6680@10.26.0.101">>,
        <<"CSeq: 3 BYE">>,
        <<"User-Agent: 2600hz">>,
        <<"Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, MESSAGE, INFO, UPDATE, REGISTER, REFER, NOTIFY, PUBLISH, SUBSCRIBE">>,
        <<"Supported: path, replaces">>,
        <<"Content-Length: 0">>]},
      {<<"src">>, <<"10.26.0.182:11000">>},
      {<<"dst">>, <<"10.26.0.182:5060">>},
      {<<"parser">>, <<"10.26.0.182:9061">>},
      {<<"c_seq">>, <<"3 BYE">>}]};

chunks_1(20) ->
    {[{<<"call-id">>,<<"63-6680@10.26.0.101">>},
      {<<"timestamp">>,63232147762.95431},
      {<<"ref_timestamp">>,<<"63601112383.1051">>},
      {<<"label">>,<<"SIP/2.0 200 OK">>},
      {<<"raw">>,
       [<<"SIP/2.0 200 OK">>,
        <<"Via: SIP/2.0/UDP 10.26.0.101:7653;received=10.26.0.101;rport=7653;branch=z9hG4bK63">>,
        <<"From: \"user_x2d24bd3dq\" <sip:user_x2d24bd3dq@wefwefwefwef.2600hz.com>;tag=63">>,
        <<"To: <sip:347@10.26.0.182:5060>;tag=pZ84K0pa94ZXe">>,
        <<"Call-ID: 63-6680@10.26.0.101">>,
        <<"CSeq: 3 BYE">>,
        <<"User-Agent: 2600hz">>,
        <<"Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, MESSAGE, INFO, UPDATE, REGISTER, REFER, NOTIFY, PUBLISH, SUBSCRIBE">>,
        <<"Supported: path, replaces">>,
        <<"Content-Length: 0">>]},
      {<<"src">>, <<"10.26.0.182:5060">>},
      {<<"dst">>, <<"10.26.0.101:7653">>},
      {<<"parser">>, <<"10.26.0.182:9061">>},
      {<<"c_seq">>, <<"3 BYE">>}]}.

chunks_1() ->
    [chunks_1(1),
     chunks_1(3),
     {[{<<"call-id">>,<<"63-6680@10.26.0.101">>},
       {<<"timestamp">>,63601112379.2463},
       {<<"ref_timestamp">>,<<"63601112379.24664">>},
       {<<"label">>,<<"INVITE sip:347@10.26.0.182:5060 SIP/2.0">>},
       {<<"raw">>,
        [<<"INVITE sip:347@10.26.0.182:5060 SIP/2.0">>,
         <<"Record-Route: <sip:10.26.0.182;lr=on;ftag=63>">>,
         <<"Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bKc105.39d0b071313f7fa81a42c21d3f74cb5f.0">>,
         <<"Via: SIP/2.0/UDP 10.26.0.101:7653;rport=7653;branch=z9hG4bK63">>,
         <<"From: \"user_x2d24bd3dq\" <sip:user_x2d24bd3dq@wefwefwefwef.2600hz.com>;tag=63">>,
         <<"To: <sip:347@wefwefwefwef.2600hz.com>">>,
         <<"Call-ID: 63-6680@10.26.0.101">>,
         <<"CSeq: 1 INVITE">>,
         <<"Contact: <sip:user_x2d24bd3dq@10.26.0.101:7653>">>,
         <<"Content-Type: application/sdp">>,
         <<"Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, NOTIFY, REGISTER">>,
         <<"Max-Forwards: 50">>,
         <<"User-Agent: SIPp">>,
         <<"Allow-Events: talk,hold,conference,refer,check-sync">>,
         <<"Content-Length:   189">>,
         <<"X-AUTH-IP: 10.26.0.101">>,
         <<"X-AUTH-PORT: 7653">>,
         <<>>,
         <<"v=0">>,
         <<"o=user1 53655765 2353687637 IN IP4 10.26.0.101">>,
         <<"s=-">>,
         <<"c=IN IP4 10.26.0.101">>,
         <<"t=0 0">>,
         <<"m=audio 6000 RTP/AVP 0 101">>,
         <<"a=rtpmap:0 PCMU/8000">>,
         <<"a=rtpmap:101 telephone-event/8000">>,
         <<"a=fmtp:101 0-15">>]},
       {<<"src">>, <<"10.26.0.182:5060">>},
       {<<"dst">>, <<"10.26.0.182:11000">>},
       {<<"parser">>, <<"10.26.0.182:9060">>},
       {<<"c_seq">>, <<"1 INVITE">>}]},
     chunks_1(2),
     {[{<<"call-id">>,<<"63-6680@10.26.0.101">>},
       {<<"timestamp">>,63601112379.25041},
       {<<"ref_timestamp">>,<<"63601112379.250465">>},
       {<<"label">>,<<"SIP/2.0 100 Trying">>},
       {<<"raw">>,
        [<<"SIP/2.0 100 Trying">>,
         <<"Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bKc105.39d0b071313f7fa81a42c21d3f74cb5f.0">>,
         <<"Via: SIP/2.0/UDP 10.26.0.101:7653;rport=7653;branch=z9hG4bK63">>,
         <<"Record-Route: <sip:10.26.0.182;lr=on;ftag=63>">>,
         <<"From: \"user_x2d24bd3dq\" <sip:user_x2d24bd3dq@wefwefwefwef.2600hz.com>;tag=63">>,
         <<"To: <sip:347@wefwefwefwef.2600hz.com>">>,
         <<"Call-ID: 63-6680@10.26.0.101">>,
         <<"CSeq: 1 INVITE">>,
         <<"User-Agent: 2600hz">>,
         <<"Content-Length: 0">>]},
       {<<"src">>, <<"10.26.0.182:11000">>},
       {<<"dst">>, <<"10.26.0.182:5060">>},
       {<<"parser">>, <<"10.26.0.182:9060">>},
       {<<"c_seq">>, <<"1 INVITE">>}]},
     chunks_1(4),
     {[{<<"call-id">>,<<"63-6680@10.26.0.101">>},
       {<<"timestamp">>,63601112379.259796},
       {<<"ref_timestamp">>,<<"63601112379.25993">>},
       {<<"label">>,<<"SIP/2.0 407 Proxy Authentication Required">>},
       {<<"raw">>,
        [<<"SIP/2.0 407 Proxy Authentication Required">>,
         <<"Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bKc105.39d0b071313f7fa81a42c21d3f74cb5f.0">>,
         <<"Via: SIP/2.0/UDP 10.26.0.101:7653;rport=7653;branch=z9hG4bK63">>,
         <<"From: \"user_x2d24bd3dq\" <sip:user_x2d24bd3dq@wefwefwefwef.2600hz.com>;tag=63">>,
         <<"To: <sip:347@wefwefwefwef.2600hz.com>;tag=NpFcj556Bv9aK">>,
         <<"Call-ID: 63-6680@10.26.0.101">>,
         <<"CSeq: 1 INVITE">>,
         <<"User-Agent: 2600hz">>,
         <<"Accept: application/sdp">>,
         <<"Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, MESSAGE, INFO, UPDATE, REGISTER, REFER, NOTIFY, PUBLISH, SUBSCRIBE">>,
         <<"Supported: path, replaces">>,
         <<"Allow-Events: talk, hold, conference, presence, as-feature-event, dialog, line-seize, call-info, sla, include-session-description, presence.winfo, message-summary, refer">>,
         <<"Proxy-Authenticate: Digest realm=\"wefwefwefwef.2600hz.com\", nonce=\"cb74f704-0f00-11e5-9502-7b9c4900a590\", algorithm=MD5, qop=\"auth\"">>,
         <<"Content-Length: 0">>]},
       {<<"src">>, <<"10.26.0.182:11000">>},
       {<<"dst">>, <<"10.26.0.182:5060">>},
       {<<"parser">>, <<"10.26.0.182:9060">>},
       {<<"c_seq">>, <<"1 INVITE">>}]},
     {[{<<"call-id">>,<<"63-6680@10.26.0.101">>},
       {<<"timestamp">>,63601112379.26164},
       {<<"ref_timestamp">>,<<"63601112379.261696">>},
       {<<"label">>,<<"ACK sip:347@10.26.0.182:5060 SIP/2.0">>},
       {<<"raw">>,
        [<<"ACK sip:347@10.26.0.182:5060 SIP/2.0">>,
         <<"Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bKc105.39d0b071313f7fa81a42c21d3f74cb5f.0">>,
         <<"From: \"user_x2d24bd3dq\" <sip:user_x2d24bd3dq@wefwefwefwef.2600hz.com>;tag=63">>,
         <<"To: <sip:347@wefwefwefwef.2600hz.com>;tag=NpFcj556Bv9aK">>,
         <<"Call-ID: 63-6680@10.26.0.101">>,
         <<"CSeq: 1 ACK">>,
         <<"Max-Forwards: 50">>,
         <<"Content-Length: 0">>]},
       {<<"src">>, <<"10.26.0.182:5060">>},
       {<<"dst">>, <<"10.26.0.182:11000">>},
       {<<"parser">>, <<"10.26.0.182:9060">>},
       {<<"c_seq">>, <<"1 ACK">>}]},
     chunks_1(6),
     chunks_1(7),
     chunks_1(9),
     chunks_1(11),
     {[{<<"call-id">>,<<"63-6680@10.26.0.101">>},
       {<<"timestamp">>,63601112379.26331},
       {<<"ref_timestamp">>,<<"63601112379.26338">>},
       {<<"label">>,<<"INVITE sip:347@10.26.0.182:5060 SIP/2.0">>},
       {<<"raw">>,
        [<<"INVITE sip:347@10.26.0.182:5060 SIP/2.0">>,
         <<"Record-Route: <sip:10.26.0.182;lr=on;ftag=63>">>,
         <<"Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bK9105.39d0b071313f7fa81a42c21d3f74cb5f.0">>,
         <<"Via: SIP/2.0/UDP 10.26.0.101:7653;received=10.26.0.101;rport=7653;branch=z9hG4bK63">>,
         <<"From: \"user_x2d24bd3dq\" <sip:user_x2d24bd3dq@wefwefwefwef.2600hz.com>;tag=63">>,
         <<"To: <sip:347@10.26.0.182:5060>">>,
         <<"Call-ID: 63-6680@10.26.0.101">>,
         <<"CSeq: 2 INVITE">>,
         <<"Contact: <sip:user_x2d24bd3dq@10.26.0.101:7653>">>,
         <<"Proxy-Authorization: Digest username=\"user_x2d24bd3dq\",realm=\"wefwefwefwef.2600hz.com\",cnonce=\"737b8ddc\",nc=00000001,qop=auth,uri=\"sip:10.26.0.182:5060\",nonce=\"cb74f704-0f00-11e5-9502-7b9c4900a590\",response=\"6a5b1f5610af652446e874ca6596fded\",algorithm=MD5">>,
         <<"Content-Type: application/sdp">>,
         <<"Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, NOTIFY, REGISTER">>,
         <<"Max-Forwards: 50">>,
         <<"User-Agent: SIPp">>,
         <<"Allow-Events: talk,hold,conference,refer,check-sync">>,
         <<"Content-Length:   189">>,
         <<"X-AUTH-IP: 10.26.0.101">>,
         <<"X-AUTH-PORT: 7653">>,
         <<>>,
         <<"v=0">>,
         <<"o=user1 53655765 2353687637 IN IP4 10.26.0.101">>,
         <<"s=-">>,
         <<"c=IN IP4 10.26.0.101">>,
         <<"t=0 0">>,
         <<"m=audio 6000 RTP/AVP 0 101">>,
         <<"a=rtpmap:0 PCMU/8000">>,
         <<"a=rtpmap:101 telephone-event/8000">>,
         <<"a=fmtp:101 0-15">>]},
       {<<"src">>, <<"10.26.0.182:5060">>},
       {<<"dst">>, <<"10.26.0.182:11000">>},
       {<<"parser">>, <<"10.26.0.182:9060">>},
       {<<"c_seq">>, <<"2 INVITE">>}]},
     chunks_1(5),
     chunks_1(8),
     chunks_1(10),
     {[{<<"call-id">>,<<"63-6680@10.26.0.101">>},
       {<<"timestamp">>,63601112379.26842},
       {<<"ref_timestamp">>,<<"63601112379.268456">>},
       {<<"label">>,<<"SIP/2.0 100 Trying">>},
       {<<"raw">>,
        [<<"SIP/2.0 100 Trying">>,
         <<"Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bK9105.39d0b071313f7fa81a42c21d3f74cb5f.0">>,
         <<"Via: SIP/2.0/UDP 10.26.0.101:7653;received=10.26.0.101;rport=7653;branch=z9hG4bK63">>,
         <<"Record-Route: <sip:10.26.0.182;lr=on;ftag=63>">>,
         <<"From: \"user_x2d24bd3dq\" <sip:user_x2d24bd3dq@wefwefwefwef.2600hz.com>;tag=63">>,
         <<"To: <sip:347@10.26.0.182:5060>">>,
         <<"Call-ID: 63-6680@10.26.0.101">>,
         <<"CSeq: 2 INVITE">>,
         <<"User-Agent: 2600hz">>,
         <<"Content-Length: 0">>]},
       {<<"src">>, <<"10.26.0.182:11000">>},
       {<<"dst">>, <<"10.26.0.182:5060">>},
       {<<"parser">>, <<"10.26.0.182:9060">>},
       {<<"c_seq">>, <<"2 INVITE">>}]},
     chunks_1(12),
     {[{<<"call-id">>,<<"63-6680@10.26.0.101">>},
       {<<"timestamp">>,63601112380.084114},
       {<<"ref_timestamp">>,<<"63601112380.08522">>},
       {<<"label">>,<<"SIP/2.0 200 OK">>},
       {<<"raw">>,
        [<<"SIP/2.0 200 OK">>,
         <<"Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bK9105.39d0b071313f7fa81a42c21d3f74cb5f.0">>,
         <<"Via: SIP/2.0/UDP 10.26.0.101:7653;received=10.26.0.101;rport=7653;branch=z9hG4bK63">>,
         <<"Record-Route: <sip:10.26.0.182;lr=on;ftag=63>">>,
         <<"From: \"user_x2d24bd3dq\" <sip:user_x2d24bd3dq@wefwefwefwef.2600hz.com>;tag=63">>,
         <<"To: <sip:347@10.26.0.182:5060>;tag=pZ84K0pa94ZXe">>,
         <<"Call-ID: 63-6680@10.26.0.101">>,
         <<"CSeq: 2 INVITE">>,
         <<"Contact: <sip:347@10.26.0.182:11000;transport=udp>">>,
         <<"User-Agent: 2600hz">>,
         <<"Accept: application/sdp">>,
         <<"Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, MESSAGE, INFO, UPDATE, REGISTER, REFER, NOTIFY, PUBLISH, SUBSCRIBE">>,
         <<"Supported: path, replaces">>,
         <<"Allow-Events: talk, hold, conference, presence, as-feature-event, dialog, line-seize, call-info, sla, include-session-description, presence.winfo, message-summary, refer">>,
         <<"Content-Type: application/sdp">>,
         <<"Content-Disposition: session">>,
         <<"Content-Length: 218">>,
         <<"Remote-Party-ID: \"347\" <sip:347@10.26.0.182>;party=calling;privacy=off;screen=no">>,
         <<>>,
         <<"v=0">>,
         <<"o=FreeSWITCH 1433315385 1433315386 IN IP4 10.26.0.182">>,
         <<"s=FreeSWITCH">>,
         <<"c=IN IP4 10.26.0.182">>,
         <<"t=0 0">>,
         <<"m=audio 28844 RTP/AVP 0 101">>,
         <<"a=rtpmap:0 PCMU/8000">>,
         <<"a=rtpmap:101 telephone-event/8000">>,
         <<"a=fmtp:101 0-16">>,
         <<"a=ptime:20">>]},
       {<<"src">>, <<"10.26.0.182:11000">>},
       {<<"dst">>, <<"10.26.0.182:5060">>},
       {<<"parser">>, <<"10.26.0.182:9060">>},
       {<<"c_seq">>, <<"2 INVITE">>}]},
     chunks_1(13),
     chunks_1(14),
     chunks_1(15),
     chunks_1(16),
     chunks_1(17),
     {[{<<"call-id">>,<<"63-6680@10.26.0.101">>},
       {<<"timestamp">>,63601112383.08688},
       {<<"ref_timestamp">>,<<"63601112383.08697">>},
       {<<"label">>,<<"BYE sip:347@10.26.0.182:11000;transport=udp SIP/2.0">>},
       {<<"raw">>,
        [<<"BYE sip:347@10.26.0.182:11000;transport=udp SIP/2.0">>,
         <<"Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bKa105.39d0b071313f7fa81a42c21d3f74cb5f.0">>,
         <<"Via: SIP/2.0/UDP 10.26.0.101:7653;received=10.26.0.101;rport=7653;branch=z9hG4bK63">>,
         <<"From: \"user_x2d24bd3dq\" <sip:user_x2d24bd3dq@wefwefwefwef.2600hz.com>;tag=63">>,
         <<"To: <sip:347@10.26.0.182:5060>;tag=pZ84K0pa94ZXe">>,
         <<"Call-ID: 63-6680@10.26.0.101">>,
         <<"Contact: <sip:347@10.26.0.101:7653;transport=udp>">>,
         <<"CSeq: 3 BYE">>,
         <<"Max-Forwards: 50">>,
         <<"Subject: Performance Test">>,
         <<"Content-Length: 0">>,
         <<"X-AUTH-IP: 10.26.0.101">>,
         <<"X-AUTH-PORT: 7653">>]},
       {<<"src">>, <<"10.26.0.182:5060">>},
       {<<"dst">>, <<"10.26.0.182:11000">>},
       {<<"parser">>, <<"10.26.0.182:9060">>},
       {<<"c_seq">>, <<"3 BYE">>}]},
     chunks_1(18),
     {[{<<"call-id">>,<<"63-6680@10.26.0.101">>},
       {<<"timestamp">>,63601112383.10524},
       {<<"ref_timestamp">>,<<"63601112383.10528">>},
       {<<"label">>,<<"SIP/2.0 200 OK">>},
       {<<"raw">>,
        [<<"SIP/2.0 200 OK">>,
         <<"Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bKa105.39d0b071313f7fa81a42c21d3f74cb5f.0">>,
         <<"Via: SIP/2.0/UDP 10.26.0.101:7653;received=10.26.0.101;rport=7653;branch=z9hG4bK63">>,
         <<"From: \"user_x2d24bd3dq\" <sip:user_x2d24bd3dq@wefwefwefwef.2600hz.com>;tag=63">>,
         <<"To: <sip:347@10.26.0.182:5060>;tag=pZ84K0pa94ZXe">>,
         <<"Call-ID: 63-6680@10.26.0.101">>,
         <<"CSeq: 3 BYE">>,
         <<"User-Agent: 2600hz">>,
         <<"Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, MESSAGE, INFO, UPDATE, REGISTER, REFER, NOTIFY, PUBLISH, SUBSCRIBE">>,
         <<"Supported: path, replaces">>,
         <<"Content-Length: 0">>]},
       {<<"src">>, <<"10.26.0.182:11000">>},
       {<<"dst">>, <<"10.26.0.182:5060">>},
       {<<"parser">>, <<"10.26.0.182:9060">>},
       {<<"c_seq">>, <<"3 BYE">>}]},
     chunks_1(20),
     chunks_1(19)].


chunks_4('count') -> 19;
chunks_4('entities') ->
    [<<"192.168.11.43:33278">>, <<"192.168.11.50:8080">>, <<"192.168.11.50:5060">>, <<"192.168.11.50:11000">>];

chunks_4(1) ->
    {[{<<"call-id">>,<<"sf34f93o1rpcbbujodu9">>},
      {<<"timestamp">>,65816927050},
      {<<"ref_timestamp">>,<<"63611209433.0">>},
      {<<"label">>,<<"INVITE sip:*97@wefwefwefwef.2600hz.com SIP/2.0">>},
      {<<"raw">>,
       [<<"INVITE sip:*97@wefwefwefwef.2600hz.com SIP/2.0">>,
        <<"Via: SIP/2.0/WS h6iuotu857tm.invalid;branch=z9hG4bK8189233">>,
        <<"Max-Forwards: 70">>,
        <<"To: <sip:*97@wefwefwefwef.2600hz.com>">>,
        <<"From: <sip:user_wpxnx7am9w@wefwefwefwef.2600hz.com>;tag=j6mvjno40c">>,
        <<"Call-ID: sf34f93o1rpcbbujodu9">>,
        <<"CSeq: 278 INVITE">>,
        <<"Contact: <sip:user_wpxnx7am9w@wefwefwefwef.2600hz.com;gr=urn:uuid:627b0e14-daac-4793-ba5e-5af6685e7ffd>">>,
        <<"Allow: ACK,CANCEL,BYE,OPTIONS,INFO,NOTIFY,INVITE,MESSAGE">>,
        <<"Content-Type: application/sdp">>,
        <<"Supported: gruu,100rel,outbound">>,
        <<"User-Agent: SIP.js/0.6.4">>,
        <<"Content-Length: 1899">>,
        <<>>,
        <<"v=0">>,
        <<"o=- 1168335802169788694 2 IN IP4 127.0.0.1">>,
        <<"s=-">>,
        <<"t=0 0">>,
        <<"a=group:BUNDLE audio">>,
        <<"a=msid-semantic: WMS qlA2uoiIZCM4ZEDDVXCKBStBUIWfvR8zM6EL">>,
        <<"m=audio 51838 UDP/TLS/RTP/SAVPF 111 103 104 9 0 8 106 105 13 126">>,
        <<"c=IN IP4 192.168.56.1">>,
        <<"a=rtcp:56393 IN IP4 192.168.56.1">>,
        <<"a=candidate:2999745851 1 udp 2122260223 192.168.56.1 51838 typ host generation 0">>,
        <<"a=candidate:931536159 1 udp 2122194687 192.168.11.43 47626 typ host generation 0">>,
        <<"a=candidate:2999745851 2 udp 2122260222 192.168.56.1 56393 typ host generation 0">>,
        <<"a=candidate:931536159 2 udp 2122194686 192.168.11.43 60311 typ host generation 0">>,
        <<"a=candidate:4233069003 1 tcp 1518280447 192.168.56.1 0 typ host tcptype active generation 0">>,
        <<"a=candidate:2030428655 1 tcp 1518214911 192.168.11.43 0 typ host tcptype active generation 0">>,
        <<"a=candidate:4233069003 2 tcp 1518280446 192.168.56.1 0 typ host tcptype active generation 0">>,
        <<"a=candidate:2030428655 2 tcp 1518214910 192.168.11.43 0 typ host tcptype active generation 0">>,
        <<"a=ice-ufrag:iEfCDnWY2LHjTjrc">>,
        <<"a=ice-pwd:qlDyYz5HIe+tYBMno9lxTNNE">>,
        <<"a=fingerprint:sha-256 CE:AA:86:95:50:E8:37:53:F6:A7:B9:8D:3E:FD:1D:4F:B3:30:05:F5:02:9F:40:25:EC:50:F6:21:25:4B:48:41">>,
        <<"a=setup:actpass">>,
        <<"a=mid:audio">>,
        <<"a=extmap:1 urn:ietf:params:rtp-hdrext:ssrc-audio-level">>,
        <<"a=extmap:3 http://www.webrtc.org/experiments/rtp-hdrext/abs-send-time">>,
        <<"a=sendrecv">>,
        <<"a=rtcp-mux">>,
        <<"a=rtpmap:111 opus/48000/2">>,
        <<"a=fmtp:111 minptime=10; useinbandfec=1">>,
        <<"a=rtpmap:103 ISAC/16000">>,
        <<"a=rtpmap:104 ISAC/32000">>,
        <<"a=rtpmap:9 G722/8000">>,
        <<"a=rtpmap:0 PCMU/8000">>,
        <<"a=rtpmap:8 PCMA/8000">>,
        <<"a=rtpmap:106 CN/32000">>,
        <<"a=rtpmap:105 CN/16000">>,
        <<"a=rtpmap:13 CN/8000">>,
        <<"a=rtpmap:126 telephone-event/8000">>,
        <<"a=maxptime:60">>,
        <<"a=ssrc:3145715846 cname:BouOoxrW+CAt7smM">>,
        <<"a=ssrc:3145715846 msid:qlA2uoiIZCM4ZEDDVXCKBStBUIWfvR8zM6EL af933e3c-c4e8-4c05-b453-5ac0708c06b3">>,
        <<"a=ssrc:3145715846 mslabel:qlA2uoiIZCM4ZEDDVXCKBStBUIWfvR8zM6EL">>,
        <<"a=ssrc:3145715846 label:af933e3c-c4e8-4c05-b453-5ac0708c06b3">>]},
      {<<"src">>,<<"192.168.11.43:33278">>},
      {<<"dst">>,<<"192.168.11.50:8080">>},
      {<<"parser">>, <<"192.168.56.42:9061">>},
      {<<"c_seq">>, <<"278 INVITE">>}]};

chunks_4(2) ->
    {[{<<"call-id">>,<<"sf34f93o1rpcbbujodu9">>},
      {<<"timestamp">>,65816924064},
      {<<"ref_timestamp">>,<<"63611209433.0">>},
      {<<"label">>,<<"SIP/2.0 100 Attempting to connect your call">>},
      {<<"raw">>,
       [<<"SIP/2.0 100 Attempting to connect your call">>,
        <<"Via: SIP/2.0/WS h6iuotu857tm.invalid;branch=z9hG4bK8189233;rport=33278;received=192.168.11.43">>,
        <<"To: <sip:*97@wefwefwefwef.2600hz.com>">>,
        <<"From: <sip:user_wpxnx7am9w@wefwefwefwef.2600hz.com>;tag=j6mvjno40c">>,
        <<"Call-ID: sf34f93o1rpcbbujodu9">>,
        <<"CSeq: 278 INVITE">>,
        <<"Content-Length: 0">>]},
      {<<"src">>,<<"192.168.11.50:8080">>},
      {<<"dst">>,<<"192.168.11.43:33278">>},
      {<<"parser">>, <<"192.168.56.42:9061">>},
      {<<"c_seq">>, <<"278 INVITE">>}]};

chunks_4(3) ->
    {[{<<"call-id">>,<<"sf34f93o1rpcbbujodu9">>},
      {<<"timestamp">>,65816923544},
      {<<"ref_timestamp">>,<<"63611209433.0">>},
      {<<"label">>,<<"INVITE sip:*97@wefwefwefwef.2600hz.com SIP/2.0">>},
      {<<"raw">>,
       [<<"INVITE sip:*97@wefwefwefwef.2600hz.com SIP/2.0">>,
        <<"Record-Route: <sip:192.168.11.50;r2=on;lr=on;ftag=j6mvjno40c>">>,
        <<"Record-Route: <sip:192.168.11.50:8080;transport=ws;r2=on;lr=on;ftag=j6mvjno40c>">>,
        <<"Via: SIP/2.0/UDP 192.168.11.50;branch=z9hG4bKc808.712f074b520713979dbd23cc4f839e61.0">>,
        <<"Via: SIP/2.0/WS h6iuotu857tm.invalid;rport=33278;received=192.168.11.43;branch=z9hG4bK8189233">>,
        <<"Max-Forwards: 50">>,
        <<"To: <sip:*97@wefwefwefwef.2600hz.com>">>,
        <<"From: <sip:user_wpxnx7am9w@wefwefwefwef.2600hz.com>;tag=j6mvjno40c">>,
        <<"Call-ID: sf34f93o1rpcbbujodu9">>,
        <<"CSeq: 278 INVITE">>,
        <<"Contact: <sip:user_wpxnx7am9w@wefwefwefwef.2600hz.com;alias=192.168.11.43~33278~5;gr=urn:uuid:627b0e14-daac-4793-ba5e-5af6685e7ffd>">>,
        <<"Allow: ACK,CANCEL,BYE,OPTIONS,INFO,NOTIFY,INVITE,MESSAGE">>,
        <<"Content-Type: application/sdp">>,
        <<"Supported: gruu,100rel,outbound">>,
        <<"User-Agent: SIP.js/0.6.4">>,
        <<"Content-Length: 1899">>,
        <<"X-AUTH-IP: 192.168.11.43">>,
        <<"X-AUTH-PORT: 33278">>,
        <<>>,
        <<"v=0">>,
        <<"o=- 1168335802169788694 2 IN IP4 127.0.0.1">>,
        <<"s=-">>,
        <<"t=0 0">>,
        <<"a=group:BUNDLE audio">>,
        <<"a=msid-semantic: WMS qlA2uoiIZCM4ZEDDVXCKBStBUIWfvR8zM6EL">>,
        <<"m=audio 51838 UDP/TLS/RTP/SAVPF 111 103 104 9 0 8 106 105 13 126">>,
        <<"c=IN IP4 192.168.56.1">>,
        <<"a=rtcp:56393 IN IP4 192.168.56.1">>,
        <<"a=candidate:2999745851 1 udp 2122260223 192.168.56.1 51838 typ host generation 0">>,
        <<"a=candidate:931536159 1 udp 2122194687 192.168.11.43 47626 typ host generation 0">>,
        <<"a=candidate:2999745851 2 udp 2122260222 192.168.56.1 56393 typ host generation 0">>,
        <<"a=candidate:931536159 2 udp 2122194686 192.168.11.43 60311 typ host generation 0">>,
        <<"a=candidate:4233069003 1 tcp 1518280447 192.168.56.1 0 typ host tcptype active generation 0">>,
        <<"a=candidate:2030428655 1 tcp 1518214911 192.168.11.43 0 typ host tcptype active generation 0">>,
        <<"a=candidate:4233069003 2 tcp 1518280446 192.168.56.1 0 typ host tcptype active generation 0">>,
        <<"a=candidate:2030428655 2 tcp 1518214910 192.168.11.43 0 typ host tcptype active generation 0">>,
        <<"a=ice-ufrag:iEfCDnWY2LHjTjrc">>,
        <<"a=ice-pwd:qlDyYz5HIe+tYBMno9lxTNNE">>,
        <<"a=fingerprint:sha-256 CE:AA:86:95:50:E8:37:53:F6:A7:B9:8D:3E:FD:1D:4F:B3:30:05:F5:02:9F:40:25:EC:50:F6:21:25:4B:48:41">>,
        <<"a=setup:actpass">>,
        <<"a=mid:audio">>,
        <<"a=extmap:1 urn:ietf:params:rtp-hdrext:ssrc-audio-level">>,
        <<"a=extmap:3 http://www.webrtc.org/experiments/rtp-hdrext/abs-send-time">>,
        <<"a=sendrecv">>,
        <<"a=rtcp-mux">>,
        <<"a=rtpmap:111 opus/48000/2">>,
        <<"a=fmtp:111 minptime=10; useinbandfec=1">>,
        <<"a=rtpmap:103 ISAC/16000">>,
        <<"a=rtpmap:104 ISAC/32000">>,
        <<"a=rtpmap:9 G722/8000">>,
        <<"a=rtpmap:0 PCMU/8000">>,
        <<"a=rtpmap:8 PCMA/8000">>,
        <<"a=rtpmap:106 CN/32000">>,
        <<"a=rtpmap:105 CN/16000">>,
        <<"a=rtpmap:13 CN/8000">>,
        <<"a=rtpmap:126 telephone-event/8000">>,
        <<"a=maxptime:60">>,
        <<"a=ssrc:3145715846 cname:BouOoxrW+CAt7smM">>,
        <<"a=ssrc:3145715846 msid:qlA2uoiIZCM4ZEDDVXCKBStBUIWfvR8zM6EL af933e3c-c4e8-4c05-b453-5ac0708c06b3">>,
        <<"a=ssrc:3145715846 mslabel:qlA2uoiIZCM4ZEDDVXCKBStBUIWfvR8zM6EL">>,
        <<"a=ssrc:3145715846 label:af933e3c-c4e8-4c05-b453-5ac0708c06b3">>]},
      {<<"src">>,<<"192.168.11.50:5060">>},
      {<<"dst">>,<<"192.168.11.50:11000">>},
      {<<"parser">>, <<"192.168.56.42:9061">>},
      {<<"c_seq">>, <<"278 INVITE">>}]};

chunks_4(4) ->
    {[{<<"call-id">>,<<"sf34f93o1rpcbbujodu9">>},
      {<<"timestamp">>,65816925306},
      {<<"ref_timestamp">>,<<"63611209433.0">>},
      {<<"label">>,<<"SIP/2.0 100 Trying">>},
      {<<"raw">>,
       [<<"SIP/2.0 100 Trying">>,
        <<"Via: SIP/2.0/UDP 192.168.11.50;branch=z9hG4bKc808.712f074b520713979dbd23cc4f839e61.0">>,
        <<"Via: SIP/2.0/WS h6iuotu857tm.invalid;rport=33278;received=192.168.11.43;branch=z9hG4bK8189233">>,
        <<"Record-Route: <sip:192.168.11.50;r2=on;lr=on;ftag=j6mvjno40c>">>,
        <<"Record-Route: <sip:192.168.11.50:8080;transport=ws;r2=on;lr=on;ftag=j6mvjno40c>">>,
        <<"From: <sip:user_wpxnx7am9w@wefwefwefwef.2600hz.com>;tag=j6mvjno40c">>,
        <<"To: <sip:*97@wefwefwefwef.2600hz.com>">>,
        <<"Call-ID: sf34f93o1rpcbbujodu9">>,
        <<"CSeq: 278 INVITE">>,
        <<"User-Agent: 2600hz">>,
        <<"Content-Length: 0">>]},
      {<<"src">>,<<"192.168.11.50:11000">>},
      {<<"dst">>,<<"192.168.11.50:5060">>},
      {<<"parser">>, <<"192.168.56.42:9061">>},
      {<<"c_seq">>, <<"278 INVITE">>}]};

chunks_4(5) ->
    {[{<<"call-id">>,<<"sf34f93o1rpcbbujodu9">>},
      {<<"timestamp">>,65816923897},
      {<<"ref_timestamp">>,<<"63611209433.0">>},
      {<<"label">>,<<"SIP/2.0 407 Proxy Authentication Required">>},
      {<<"raw">>,
       [<<"SIP/2.0 407 Proxy Authentication Required">>,
        <<"Via: SIP/2.0/UDP 192.168.11.50;branch=z9hG4bKc808.712f074b520713979dbd23cc4f839e61.0">>,
        <<"Via: SIP/2.0/WS h6iuotu857tm.invalid;rport=33278;received=192.168.11.43;branch=z9hG4bK8189233">>,
        <<"From: <sip:user_wpxnx7am9w@wefwefwefwef.2600hz.com>;tag=j6mvjno40c">>,
        <<"To: <sip:*97@wefwefwefwef.2600hz.com>;tag=rN3NU5UcZF3Qm">>,
        <<"Call-ID: sf34f93o1rpcbbujodu9">>,
        <<"CSeq: 278 INVITE">>,
        <<"User-Agent: 2600hz">>,
        <<"Accept: application/sdp">>,
        <<"Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, MESSAGE, INFO, UPDATE, REGISTER, REFER, PRACK, NOTIFY, PUBLISH, SUBSCRIBE">>,
        <<"Supported: precondition, 100rel, path, replaces">>,
        <<"Allow-Events: talk, hold, conference, presence, as-feature-event, dialog, line-seize, call-info, sla, include-session-description, presence.winfo, message-summary, refer">>,
        <<"Proxy-Authenticate: Digest realm=\"wefwefwefwef.2600hz.com\", nonce=\"d483d6c2-6ad5-11e5-a5d9-4751ff59c40e\", algorithm=MD5, qop=\"auth\"">>,
        <<"Content-Length: 0">>]},
      {<<"src">>,<<"192.168.11.50:11000">>},
      {<<"dst">>,<<"192.168.11.50:5060">>},
      {<<"parser">>, <<"192.168.56.42:9061">>},
      {<<"c_seq">>, <<"278 INVITE">>}]};

chunks_4(6) ->
    {[{<<"call-id">>,<<"sf34f93o1rpcbbujodu9">>},
      {<<"timestamp">>,65816927203},
      {<<"ref_timestamp">>,<<"63611209433.0">>},
      {<<"label">>,<<"ACK sip:*97@wefwefwefwef.2600hz.com SIP/2.0">>},
      {<<"raw">>,
       [<<"ACK sip:*97@wefwefwefwef.2600hz.com SIP/2.0">>,
        <<"Via: SIP/2.0/UDP 192.168.11.50;branch=z9hG4bKc808.712f074b520713979dbd23cc4f839e61.0">>,
        <<"Max-Forwards: 50">>,
        <<"To: <sip:*97@wefwefwefwef.2600hz.com>;tag=rN3NU5UcZF3Qm">>,
        <<"From: <sip:user_wpxnx7am9w@wefwefwefwef.2600hz.com>;tag=j6mvjno40c">>,
        <<"Call-ID: sf34f93o1rpcbbujodu9">>,
        <<"CSeq: 278 ACK">>,
        <<"Content-Length: 0">>]},
      {<<"src">>,<<"192.168.11.50:5060">>},
      {<<"dst">>,<<"192.168.11.50:11000">>},
      {<<"parser">>, <<"192.168.56.42:9061">>},
      {<<"c_seq">>, <<"278 ACK">>}]};

chunks_4(7) ->
    {[{<<"call-id">>,<<"sf34f93o1rpcbbujodu9">>},
      {<<"timestamp">>,65816925810},
      {<<"ref_timestamp">>,<<"63611209433.0">>},
      {<<"label">>,<<"SIP/2.0 407 Proxy Authentication Required">>},
      {<<"raw">>,
       [<<"SIP/2.0 407 Proxy Authentication Required">>,
        <<"Via: SIP/2.0/WS h6iuotu857tm.invalid;rport=33278;received=192.168.11.43;branch=z9hG4bK8189233">>,
        <<"From: <sip:user_wpxnx7am9w@wefwefwefwef.2600hz.com>;tag=j6mvjno40c">>,
        <<"To: <sip:*97@wefwefwefwef.2600hz.com>;tag=rN3NU5UcZF3Qm">>,
        <<"Call-ID: sf34f93o1rpcbbujodu9">>,
        <<"CSeq: 278 INVITE">>,
        <<"User-Agent: 2600hz">>,
        <<"Accept: application/sdp">>,
        <<"Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, MESSAGE, INFO, UPDATE, REGISTER, REFER, PRACK, NOTIFY, PUBLISH, SUBSCRIBE">>,
        <<"Supported: precondition, 100rel, path, replaces">>,
        <<"Allow-Events: talk, hold, conference, presence, as-feature-event, dialog, line-seize, call-info, sla, include-session-description, presence.winfo, message-summary, refer">>,
        <<"Proxy-Authenticate: Digest realm=\"wefwefwefwef.2600hz.com\", nonce=\"d483d6c2-6ad5-11e5-a5d9-4751ff59c40e\", algorithm=MD5, qop=\"auth\"">>,
        <<"Content-Length: 0">>]},
      {<<"src">>,<<"192.168.11.50:8080">>},
      {<<"dst">>,<<"192.168.11.43:33278">>},
      {<<"parser">>, <<"192.168.56.42:9061">>},
      {<<"c_seq">>, <<"278 INVITE">>}]};

chunks_4(8) ->
    {[{<<"call-id">>,<<"sf34f93o1rpcbbujodu9">>},
      {<<"timestamp">>,65816926139},
      {<<"ref_timestamp">>,<<"63611209433.0">>},
      {<<"label">>,<<"ACK sip:*97@wefwefwefwef.2600hz.com SIP/2.0">>},
      {<<"raw">>,
       [<<"ACK sip:*97@wefwefwefwef.2600hz.com SIP/2.0">>,
        <<"Via: SIP/2.0/WS h6iuotu857tm.invalid;branch=z9hG4bK8189233">>,
        <<"To: <sip:*97@wefwefwefwef.2600hz.com>;tag=rN3NU5UcZF3Qm">>,
        <<"From: <sip:user_wpxnx7am9w@wefwefwefwef.2600hz.com>;tag=j6mvjno40c">>,
        <<"Call-ID: sf34f93o1rpcbbujodu9">>,
        <<"CSeq: 278 ACK">>]},
      {<<"src">>,<<"192.168.11.43:33278">>},
      {<<"dst">>,<<"192.168.11.50:8080">>},
      {<<"parser">>, <<"192.168.56.42:9061">>},
      {<<"c_seq">>, <<"278 ACK">>}]};

chunks_4(9) ->
    {[{<<"call-id">>,<<"sf34f93o1rpcbbujodu9">>},
      {<<"timestamp">>,65816925100},
      {<<"ref_timestamp">>,<<"63611209433.0">>},
      {<<"label">>,<<"INVITE sip:*97@wefwefwefwef.2600hz.com SIP/2.0">>},
      {<<"raw">>,
       [<<"INVITE sip:*97@wefwefwefwef.2600hz.com SIP/2.0">>,
        <<"Via: SIP/2.0/WS h6iuotu857tm.invalid;branch=z9hG4bK3609238">>,
        <<"Max-Forwards: 70">>,
        <<"To: <sip:*97@wefwefwefwef.2600hz.com>">>,
        <<"From: <sip:user_wpxnx7am9w@wefwefwefwef.2600hz.com>;tag=j6mvjno40c">>,
        <<"Call-ID: sf34f93o1rpcbbujodu9">>,
        <<"CSeq: 279 INVITE">>,
        <<"Proxy-Authorization: Digest algorithm=MD5, username=\"user_wpxnx7am9w\", realm=\"wefwefwefwef.2600hz.com\", nonce=\"d483d6c2-6ad5-11e5-a5d9-4751ff59c40e\", uri=\"sip:*97@wefwefwefwef.2600hz.com\", response=\"7865876f7242c0e997dc1f4895e95c0c\", qop=auth, cnonce=\"1odio1no4lqc\", nc=00000001">>,
        <<"Contact: <sip:user_wpxnx7am9w@wefwefwefwef.2600hz.com;gr=urn:uuid:627b0e14-daac-4793-ba5e-5af6685e7ffd>">>,
        <<"Allow: ACK,CANCEL,BYE,OPTIONS,INFO,NOTIFY,INVITE,MESSAGE">>,
        <<"Content-Type: application/sdp">>,
        <<"Supported: gruu,100rel,outbound">>,
        <<"User-Agent: SIP.js/0.6.4">>,
        <<"Content-Length: 1899">>,
        <<>>,
        <<"v=0">>,
        <<"o=- 1168335802169788694 2 IN IP4 127.0.0.1">>,
        <<"s=-">>,
        <<"t=0 0">>,
        <<"a=group:BUNDLE audio">>,
        <<"a=msid-semantic: WMS qlA2uoiIZCM4ZEDDVXCKBStBUIWfvR8zM6EL">>,
        <<"m=audio 51838 UDP/TLS/RTP/SAVPF 111 103 104 9 0 8 106 105 13 126">>,
        <<"c=IN IP4 192.168.56.1">>,
        <<"a=rtcp:56393 IN IP4 192.168.56.1">>,
        <<"a=candidate:2999745851 1 udp 2122260223 192.168.56.1 51838 typ host generation 0">>,
        <<"a=candidate:931536159 1 udp 2122194687 192.168.11.43 47626 typ host generation 0">>,
        <<"a=candidate:2999745851 2 udp 2122260222 192.168.56.1 56393 typ host generation 0">>,
        <<"a=candidate:931536159 2 udp 2122194686 192.168.11.43 60311 typ host generation 0">>,
        <<"a=candidate:4233069003 1 tcp 1518280447 192.168.56.1 0 typ host tcptype active generation 0">>,
        <<"a=candidate:2030428655 1 tcp 1518214911 192.168.11.43 0 typ host tcptype active generation 0">>,
        <<"a=candidate:4233069003 2 tcp 1518280446 192.168.56.1 0 typ host tcptype active generation 0">>,
        <<"a=candidate:2030428655 2 tcp 1518214910 192.168.11.43 0 typ host tcptype active generation 0">>,
        <<"a=ice-ufrag:iEfCDnWY2LHjTjrc">>,
        <<"a=ice-pwd:qlDyYz5HIe+tYBMno9lxTNNE">>,
        <<"a=fingerprint:sha-256 CE:AA:86:95:50:E8:37:53:F6:A7:B9:8D:3E:FD:1D:4F:B3:30:05:F5:02:9F:40:25:EC:50:F6:21:25:4B:48:41">>,
        <<"a=setup:actpass">>,
        <<"a=mid:audio">>,
        <<"a=extmap:1 urn:ietf:params:rtp-hdrext:ssrc-audio-level">>,
        <<"a=extmap:3 http://www.webrtc.org/experiments/rtp-hdrext/abs-send-time">>,
        <<"a=sendrecv">>,
        <<"a=rtcp-mux">>,
        <<"a=rtpmap:111 opus/48000/2">>,
        <<"a=fmtp:111 minptime=10; useinbandfec=1">>,
        <<"a=rtpmap:103 ISAC/16000">>,
        <<"a=rtpmap:104 ISAC/32000">>,
        <<"a=rtpmap:9 G722/8000">>,
        <<"a=rtpmap:0 PCMU/8000">>,
        <<"a=rtpmap:8 PCMA/8000">>,
        <<"a=rtpmap:106 CN/32000">>,
        <<"a=rtpmap:105 CN/16000">>,
        <<"a=rtpmap:13 CN/8000">>,
        <<"a=rtpmap:126 telephone-event/8000">>,
        <<"a=maxptime:60">>,
        <<"a=ssrc:3145715846 cname:BouOoxrW+CAt7smM">>,
        <<"a=ssrc:3145715846 msid:qlA2uoiIZCM4ZEDDVXCKBStBUIWfvR8zM6EL af933e3c-c4e8-4c05-b453-5ac0708c06b3">>,
        <<"a=ssrc:3145715846 mslabel:qlA2uoiIZCM4ZEDDVXCKBStBUIWfvR8zM6EL">>,
        <<"a=ssrc:3145715846 label:af933e3c-c4e8-4c05-b453-5ac0708c06b3">>]},
      {<<"src">>,<<"192.168.11.43:33278">>},
      {<<"dst">>,<<"192.168.11.50:8080">>},
      {<<"parser">>, <<"192.168.56.42:9061">>},
      {<<"c_seq">>, <<"279 INVITE">>}]};

chunks_4(10) ->
    {[{<<"call-id">>,<<"sf34f93o1rpcbbujodu9">>},
      {<<"timestamp">>,65816927315},
      {<<"ref_timestamp">>,<<"63611209433.0">>},
      {<<"label">>,<<"SIP/2.0 100 Attempting to connect your call">>},
      {<<"raw">>,
       [<<"SIP/2.0 100 Attempting to connect your call">>,
        <<"Via: SIP/2.0/WS h6iuotu857tm.invalid;branch=z9hG4bK3609238;rport=33278;received=192.168.11.43">>,
        <<"To: <sip:*97@wefwefwefwef.2600hz.com>">>,
        <<"From: <sip:user_wpxnx7am9w@wefwefwefwef.2600hz.com>;tag=j6mvjno40c">>,
        <<"Call-ID: sf34f93o1rpcbbujodu9">>,
        <<"CSeq: 279 INVITE">>,
        <<"Content-Length: 0">>]},
      {<<"src">>,<<"192.168.11.50:8080">>},
      {<<"dst">>,<<"192.168.11.43:33278">>},
      {<<"parser">>, <<"192.168.56.42:9061">>},
      {<<"c_seq">>, <<"279 INVITE">>}]};

chunks_4(11) ->
    {[{<<"call-id">>,<<"sf34f93o1rpcbbujodu9">>},
      {<<"timestamp">>,65816925520},
      {<<"ref_timestamp">>,<<"63611209433.0">>},
      {<<"label">>,<<"INVITE sip:*97@wefwefwefwef.2600hz.com SIP/2.0">>},
      {<<"raw">>,
       [<<"INVITE sip:*97@wefwefwefwef.2600hz.com SIP/2.0">>,
        <<"Record-Route: <sip:192.168.11.50;r2=on;lr=on;ftag=j6mvjno40c>">>,
        <<"Record-Route: <sip:192.168.11.50:8080;transport=ws;r2=on;lr=on;ftag=j6mvjno40c>">>,
        <<"Via: SIP/2.0/UDP 192.168.11.50;branch=z9hG4bKb808.1c07aa9aaf5ea1d017be944c95dfc21d.0">>,
        <<"Via: SIP/2.0/WS h6iuotu857tm.invalid;rport=33278;received=192.168.11.43;branch=z9hG4bK3609238">>,
        <<"Max-Forwards: 50">>,
        <<"To: <sip:*97@wefwefwefwef.2600hz.com>">>,
        <<"From: <sip:user_wpxnx7am9w@wefwefwefwef.2600hz.com>;tag=j6mvjno40c">>,
        <<"Call-ID: sf34f93o1rpcbbujodu9">>,
        <<"CSeq: 279 INVITE">>,
        <<"Proxy-Authorization: Digest algorithm=MD5, username=\"user_wpxnx7am9w\", realm=\"wefwefwefwef.2600hz.com\", nonce=\"d483d6c2-6ad5-11e5-a5d9-4751ff59c40e\", uri=\"sip:*97@wefwefwefwef.2600hz.com\", response=\"7865876f7242c0e997dc1f4895e95c0c\", qop=auth, cnonce=\"1odio1no4lqc\", nc=00000001">>,
        <<"Contact: <sip:user_wpxnx7am9w@wefwefwefwef.2600hz.com;alias=192.168.11.43~33278~5;gr=urn:uuid:627b0e14-daac-4793-ba5e-5af6685e7ffd>">>,
        <<"Allow: ACK,CANCEL,BYE,OPTIONS,INFO,NOTIFY,INVITE,MESSAGE">>,
        <<"Content-Type: application/sdp">>,
        <<"Supported: gruu,100rel,outbound">>,
        <<"User-Agent: SIP.js/0.6.4">>,
        <<"Content-Length: 1899">>,
        <<"X-AUTH-IP: 192.168.11.43">>,
        <<"X-AUTH-PORT: 33278">>,
        <<>>,
        <<"v=0">>,
        <<"o=- 1168335802169788694 2 IN IP4 127.0.0.1">>,
        <<"s=-">>,
        <<"t=0 0">>,
        <<"a=group:BUNDLE audio">>,
        <<"a=msid-semantic: WMS qlA2uoiIZCM4ZEDDVXCKBStBUIWfvR8zM6EL">>,
        <<"m=audio 51838 UDP/TLS/RTP/SAVPF 111 103 104 9 0 8 106 105 13 126">>,
        <<"c=IN IP4 192.168.56.1">>,
        <<"a=rtcp:56393 IN IP4 192.168.56.1">>,
        <<"a=candidate:2999745851 1 udp 2122260223 192.168.56.1 51838 typ host generation 0">>,
        <<"a=candidate:931536159 1 udp 2122194687 192.168.11.43 47626 typ host generation 0">>,
        <<"a=candidate:2999745851 2 udp 2122260222 192.168.56.1 56393 typ host generation 0">>,
        <<"a=candidate:931536159 2 udp 2122194686 192.168.11.43 60311 typ host generation 0">>,
        <<"a=candidate:4233069003 1 tcp 1518280447 192.168.56.1 0 typ host tcptype active generation 0">>,
        <<"a=candidate:2030428655 1 tcp 1518214911 192.168.11.43 0 typ host tcptype active generation 0">>,
        <<"a=candidate:4233069003 2 tcp 1518280446 192.168.56.1 0 typ host tcptype active generation 0">>,
        <<"a=candidate:2030428655 2 tcp 1518214910 192.168.11.43 0 typ host tcptype active generation 0">>,
        <<"a=ice-ufrag:iEfCDnWY2LHjTjrc">>,
        <<"a=ice-pwd:qlDyYz5HIe+tYBMno9lxTNNE">>,
        <<"a=fingerprint:sha-256 CE:AA:86:95:50:E8:37:53:F6:A7:B9:8D:3E:FD:1D:4F:B3:30:05:F5:02:9F:40:25:EC:50:F6:21:25:4B:48:41">>,
        <<"a=setup:actpass">>,
        <<"a=mid:audio">>,
        <<"a=extmap:1 urn:ietf:params:rtp-hdrext:ssrc-audio-level">>,
        <<"a=extmap:3 http://www.webrtc.org/experiments/rtp-hdrext/abs-send-time">>,
        <<"a=sendrecv">>,
        <<"a=rtcp-mux">>,
        <<"a=rtpmap:111 opus/48000/2">>,
        <<"a=fmtp:111 minptime=10; useinbandfec=1">>,
        <<"a=rtpmap:103 ISAC/16000">>,
        <<"a=rtpmap:104 ISAC/32000">>,
        <<"a=rtpmap:9 G722/8000">>,
        <<"a=rtpmap:0 PCMU/8000">>,
        <<"a=rtpmap:8 PCMA/8000">>,
        <<"a=rtpmap:106 CN/32000">>,
        <<"a=rtpmap:105 CN/16000">>,
        <<"a=rtpmap:13 CN/8000">>,
        <<"a=rtpmap:126 telephone-event/8000">>,
        <<"a=maxptime:60">>,
        <<"a=ssrc:3145715846 cname:BouOoxrW+CAt7smM">>,
        <<"a=ssrc:3145715846 msid:qlA2uoiIZCM4ZEDDVXCKBStBUIWfvR8zM6EL af933e3c-c4e8-4c05-b453-5ac0708c06b3">>,
        <<"a=ssrc:3145715846 mslabel:qlA2uoiIZCM4ZEDDVXCKBStBUIWfvR8zM6EL">>,
        <<"a=ssrc:3145715846 label:af933e3c-c4e8-4c05-b453-5ac0708c06b3">>]},
      {<<"src">>,<<"192.168.11.50:5060">>},
      {<<"dst">>,<<"192.168.11.50:11000">>},
      {<<"parser">>, <<"192.168.56.42:9061">>},
      {<<"c_seq">>, <<"279 INVITE">>}]};

chunks_4(12) ->
    {[{<<"call-id">>,<<"sf34f93o1rpcbbujodu9">>},
      {<<"timestamp">>,65816924983},
      {<<"ref_timestamp">>,<<"63611209433.0">>},
      {<<"label">>,<<"SIP/2.0 100 Trying">>},
      {<<"raw">>,
       [<<"SIP/2.0 100 Trying">>,
        <<"Via: SIP/2.0/UDP 192.168.11.50;branch=z9hG4bKb808.1c07aa9aaf5ea1d017be944c95dfc21d.0">>,
        <<"Via: SIP/2.0/WS h6iuotu857tm.invalid;rport=33278;received=192.168.11.43;branch=z9hG4bK3609238">>,
        <<"Record-Route: <sip:192.168.11.50;r2=on;lr=on;ftag=j6mvjno40c>">>,
        <<"Record-Route: <sip:192.168.11.50:8080;transport=ws;r2=on;lr=on;ftag=j6mvjno40c>">>,
        <<"From: <sip:user_wpxnx7am9w@wefwefwefwef.2600hz.com>;tag=j6mvjno40c">>,
        <<"To: <sip:*97@wefwefwefwef.2600hz.com>">>,
        <<"Call-ID: sf34f93o1rpcbbujodu9">>,
        <<"CSeq: 279 INVITE">>,
        <<"User-Agent: 2600hz">>,
        <<"Content-Length: 0">>]},
      {<<"src">>,<<"192.168.11.50:11000">>},
      {<<"dst">>,<<"192.168.11.50:5060">>},
      {<<"parser">>, <<"192.168.56.42:9061">>},
      {<<"c_seq">>, <<"279 INVITE">>}]};

chunks_4(13) ->
    {[{<<"call-id">>,<<"sf34f93o1rpcbbujodu9">>},
      {<<"timestamp">>,65816924475},
      {<<"ref_timestamp">>,<<"63611209433.0">>},
      {<<"label">>,<<"SIP/2.0 200 OK">>},
      {<<"raw">>,
       [<<"SIP/2.0 200 OK">>,
        <<"Via: SIP/2.0/UDP 192.168.11.50;branch=z9hG4bKb808.1c07aa9aaf5ea1d017be944c95dfc21d.0">>,
        <<"Via: SIP/2.0/WS h6iuotu857tm.invalid;rport=33278;received=192.168.11.43;branch=z9hG4bK3609238">>,
        <<"Record-Route: <sip:192.168.11.50;r2=on;lr=on;ftag=j6mvjno40c>">>,
        <<"Record-Route: <sip:192.168.11.50:8080;transport=ws;r2=on;lr=on;ftag=j6mvjno40c>">>,
        <<"From: <sip:user_wpxnx7am9w@wefwefwefwef.2600hz.com>;tag=j6mvjno40c">>,
        <<"To: <sip:*97@wefwefwefwef.2600hz.com>;tag=SyveX0cgvrSag">>,
        <<"Call-ID: sf34f93o1rpcbbujodu9">>,
        <<"CSeq: 279 INVITE">>,
        <<"Contact: <sip:*97@192.168.11.50:11000;transport=udp>">>,
        <<"User-Agent: 2600hz">>,
        <<"Accept: application/sdp">>,
        <<"Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, MESSAGE, INFO, UPDATE, REGISTER, REFER, PRACK, NOTIFY, PUBLISH, SUBSCRIBE">>,
        <<"Supported: precondition, 100rel, path, replaces">>,
        <<"Allow-Events: talk, hold, conference, presence, as-feature-event, dialog, line-seize, call-info, sla, include-session-description, presence.winfo, message-summary, refer">>,
        <<"Content-Type: application/sdp">>,
        <<"Content-Disposition: session">>,
        <<"Content-Length: 872">>,
        <<"Remote-Party-ID: \"*97\" <sip:*97@wefwefwefwef.2600hz.com>;party=calling;privacy=off;screen=no">>,
        <<>>,
        <<"v=0">>,
        <<"o=FreeSWITCH 1443971853 1443971854 IN IP4 192.168.11.50">>,
        <<"s=FreeSWITCH">>,
        <<"c=IN IP4 192.168.11.50">>,
        <<"t=0 0">>,
        <<"a=msid-semantic: WMS KqmPBry0Vhq6gpRRKh2RFGdVAP5jZ7WA">>,
        <<"m=audio 18380 UDP/TLS/RTP/SAVPF 111 126 106">>,
        <<"a=rtpmap:111 opus/48000/2">>,
        <<"a=fmtp:111 useinbandfec=1; minptime=10">>,
        <<"a=rtpmap:126 telephone-event/8000">>,
        <<"a=rtpmap:106 CN/8000">>,
        <<"a=ptime:20">>,
        <<"a=fingerprint:sha-256 C4:15:DB:DF:30:05:13:FF:50:C9:C2:C6:EF:E3:D1:19:58:39:7F:A0:4A:B8:83:5B:49:B9:6C:12:16:E2:D3:54">>,
        <<"a=rtcp-mux">>,
        <<"a=rtcp:18380 IN IP4 192.168.11.50">>,
        <<"a=ssrc:639008697 cname:n8fOpEEbLV7RaPzL">>,
        <<"a=ssrc:639008697 msid:KqmPBry0Vhq6gpRRKh2RFGdVAP5jZ7WA a0">>,
        <<"a=ssrc:639008697 mslabel:KqmPBry0Vhq6gpRRKh2RFGdVAP5jZ7WA">>,
        <<"a=ssrc:639008697 label:KqmPBry0Vhq6gpRRKh2RFGdVAP5jZ7WAa0">>,
        <<"a=ice-ufrag:XstsZwepc9z27lsl">>,
        <<"a=ice-pwd:jxZz2uBDgylgeKS13bkSyo8A">>,
        <<"a=candidate:1855369570 1 udp 659136 192.168.11.50 18380 typ host generation 0">>]},
      {<<"src">>,<<"192.168.11.50:11000">>},
      {<<"dst">>,<<"192.168.11.50:5060">>},
      {<<"parser">>, <<"192.168.56.42:9061">>},
      {<<"c_seq">>, <<"279 INVITE">>}]};

chunks_4(14) ->
    {[{<<"call-id">>,<<"sf34f93o1rpcbbujodu9">>},
      {<<"timestamp">>,65816925935},
      {<<"ref_timestamp">>,<<"63611209433.0">>},
      {<<"label">>,<<"SIP/2.0 200 OK">>},
      {<<"raw">>,
       [<<"SIP/2.0 200 OK">>,
        <<"Via: SIP/2.0/WS h6iuotu857tm.invalid;rport=33278;received=192.168.11.43;branch=z9hG4bK3609238">>,
        <<"Record-Route: <sip:192.168.11.50;r2=on;lr=on;ftag=j6mvjno40c>">>,
        <<"Record-Route: <sip:192.168.11.50:8080;transport=ws;r2=on;lr=on;ftag=j6mvjno40c>">>,
        <<"From: <sip:user_wpxnx7am9w@wefwefwefwef.2600hz.com>;tag=j6mvjno40c">>,
        <<"To: <sip:*97@wefwefwefwef.2600hz.com>;tag=SyveX0cgvrSag">>,
        <<"Call-ID: sf34f93o1rpcbbujodu9">>,
        <<"CSeq: 279 INVITE">>,
        <<"Contact: <sip:*97@192.168.11.50:11000;transport=udp>">>,
        <<"User-Agent: 2600hz">>,
        <<"Accept: application/sdp">>,
        <<"Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, MESSAGE, INFO, UPDATE, REGISTER, REFER, PRACK, NOTIFY, PUBLISH, SUBSCRIBE">>,
        <<"Supported: precondition, 100rel, path, replaces">>,
        <<"Allow-Events: talk, hold, conference, presence, as-feature-event, dialog, line-seize, call-info, sla, include-session-description, presence.winfo, message-summary, refer">>,
        <<"Content-Type: application/sdp">>,
        <<"Content-Disposition: session">>,
        <<"Content-Length: 872">>,
        <<"Remote-Party-ID: \"*97\" <sip:*97@wefwefwefwef.2600hz.com>;party=calling;privacy=off;screen=no">>,
        <<>>,
        <<"v=0">>,
        <<"o=FreeSWITCH 1443971853 1443971854 IN IP4 192.168.11.50">>,
        <<"s=FreeSWITCH">>,
        <<"c=IN IP4 192.168.11.50">>,
        <<"t=0 0">>,
        <<"a=msid-semantic: WMS KqmPBry0Vhq6gpRRKh2RFGdVAP5jZ7WA">>,
        <<"m=audio 18380 UDP/TLS/RTP/SAVPF 111 126 106">>,
        <<"a=rtpmap:111 opus/48000/2">>,
        <<"a=fmtp:111 useinbandfec=1; minptime=10">>,
        <<"a=rtpmap:126 telephone-event/8000">>,
        <<"a=rtpmap:106 CN/8000">>,
        <<"a=ptime:20">>,
        <<"a=fingerprint:sha-256 C4:15:DB:DF:30:05:13:FF:50:C9:C2:C6:EF:E3:D1:19:58:39:7F:A0:4A:B8:83:5B:49:B9:6C:12:16:E2:D3:54">>,
        <<"a=rtcp-mux">>,
        <<"a=rtcp:18380 IN IP4 192.168.11.50">>,
        <<"a=ssrc:639008697 cname:n8fOpEEbLV7RaPzL">>,
        <<"a=ssrc:639008697 msid:KqmPBry0Vhq6gpRRKh2RFGdVAP5jZ7WA a0">>,
        <<"a=ssrc:639008697 mslabel:KqmPBry0Vhq6gpRRKh2RFGdVAP5jZ7WA">>,
        <<"a=ssrc:639008697 label:KqmPBry0Vhq6gpRRKh2RFGdVAP5jZ7WAa0">>,
        <<"a=ice-ufrag:XstsZwepc9z27lsl">>,
        <<"a=ice-pwd:jxZz2uBDgylgeKS13bkSyo8A">>,
        <<"a=candidate:1855369570 1 udp 659136 192.168.11.50 18380 typ host generation 0">>]},
      {<<"src">>,<<"192.168.11.50:8080">>},
      {<<"dst">>,<<"192.168.11.43:33278">>},
      {<<"parser">>, <<"192.168.56.42:9061">>},
      {<<"c_seq">>, <<"279 INVITE">>}]};

chunks_4(15) ->
    {[{<<"call-id">>,<<"sf34f93o1rpcbbujodu9">>},
      {<<"timestamp">>,65816927302},
      {<<"ref_timestamp">>,<<"63611209433.0">>},
      {<<"label">>,<<"ACK sip:*97@192.168.11.50:11000;transport=udp SIP/2.0">>},
      {<<"raw">>,
       [<<"ACK sip:*97@192.168.11.50:11000;transport=udp SIP/2.0">>,
        <<"Route: <sip:192.168.11.50:8080;transport=ws;r2=on;lr=on;ftag=j6mvjno40c>">>,
        <<"Route: <sip:192.168.11.50;r2=on;lr=on;ftag=j6mvjno40c>">>,
        <<"Via: SIP/2.0/WS h6iuotu857tm.invalid;branch=z9hG4bK4277958">>,
        <<"Max-Forwards: 70">>,
        <<"To: <sip:*97@wefwefwefwef.2600hz.com>;tag=SyveX0cgvrSag">>,
        <<"From: <sip:user_wpxnx7am9w@wefwefwefwef.2600hz.com>;tag=j6mvjno40c">>,
        <<"Call-ID: sf34f93o1rpcbbujodu9">>,
        <<"CSeq: 279 ACK">>,
        <<"Supported: 100rel,outbound">>,<<"User-Agent: SIP.js/0.6.4">>,
        <<"Content-Length: 0">>]},
      {<<"src">>,<<"192.168.11.43:33278">>},
      {<<"dst">>,<<"192.168.11.50:8080">>},
      {<<"parser">>, <<"192.168.56.42:9061">>},
      {<<"c_seq">>, <<"279 ACK">>}]};

chunks_4(16) ->
    {[{<<"call-id">>,<<"sf34f93o1rpcbbujodu9">>},
      {<<"timestamp">>,65850478173},
      {<<"ref_timestamp">>,<<"63611209435.0">>},
      {<<"label">>,<<"BYE sip:*97@192.168.11.50:11000;transport=udp SIP/2.0">>},
      {<<"raw">>,
       [<<"BYE sip:*97@192.168.11.50:11000;transport=udp SIP/2.0">>,
        <<"Route: <sip:192.168.11.50:8080;transport=ws;r2=on;lr=on;ftag=j6mvjno40c>">>,
        <<"Route: <sip:192.168.11.50;r2=on;lr=on;ftag=j6mvjno40c>">>,
        <<"Via: SIP/2.0/WS h6iuotu857tm.invalid;branch=z9hG4bK2505364">>,
        <<"Max-Forwards: 70">>,
        <<"To: <sip:*97@wefwefwefwef.2600hz.com>;tag=SyveX0cgvrSag">>,
        <<"From: <sip:user_wpxnx7am9w@wefwefwefwef.2600hz.com>;tag=j6mvjno40c">>,
        <<"Call-ID: sf34f93o1rpcbbujodu9">>,
        <<"CSeq: 280 BYE">>,
        <<"Supported: 100rel,outbound">>,
        <<"User-Agent: SIP.js/0.6.4">>,
        <<"Content-Length: 0">>]},
      {<<"src">>,<<"192.168.11.43:33278">>},
      {<<"dst">>,<<"192.168.11.50:8080">>},
      {<<"parser">>, <<"192.168.56.42:9061">>},
      {<<"c_seq">>, <<"280 BYE">>}]};

chunks_4(17) ->
    {[{<<"call-id">>,<<"sf34f93o1rpcbbujodu9">>},
      {<<"timestamp">>,65850481562},
      {<<"ref_timestamp">>,<<"63611209435.0">>},
      {<<"label">>,<<"BYE sip:*97@192.168.11.50:11000;transport=udp SIP/2.0">>},
      {<<"raw">>,
       [<<"BYE sip:*97@192.168.11.50:11000;transport=udp SIP/2.0">>,
        <<"Via: SIP/2.0/UDP 192.168.11.50;branch=z9hG4bK4ed8.9b9866d701ca0e0ac1f33e28ac81577d.0">>,
        <<"Via: SIP/2.0/WS h6iuotu857tm.invalid;rport=33278;received=192.168.11.43;branch=z9hG4bK2505364">>,
        <<"Max-Forwards: 50">>,
        <<"To: <sip:*97@wefwefwefwef.2600hz.com>;tag=SyveX0cgvrSag">>,
        <<"From: <sip:user_wpxnx7am9w@wefwefwefwef.2600hz.com>;tag=j6mvjno40c">>,
        <<"Call-ID: sf34f93o1rpcbbujodu9">>,
        <<"CSeq: 280 BYE">>,
        <<"Supported: 100rel,outbound">>,
        <<"User-Agent: SIP.js/0.6.4">>,
        <<"Content-Length: 0">>,
        <<"X-AUTH-IP: 192.168.11.43">>,
        <<"X-AUTH-PORT: 33278">>]},
      {<<"src">>,<<"192.168.11.50:5060">>},
      {<<"dst">>,<<"192.168.11.50:11000">>},
      {<<"parser">>, <<"192.168.56.42:9061">>},
      {<<"c_seq">>, <<"280 BYE">>}]};

chunks_4(18) ->
    {[{<<"call-id">>,<<"sf34f93o1rpcbbujodu9">>},
      {<<"timestamp">>,65850480323},
      {<<"ref_timestamp">>,<<"63611209435.0">>},
      {<<"label">>,<<"SIP/2.0 200 OK">>},
      {<<"raw">>,
       [<<"SIP/2.0 200 OK">>,
        <<"Via: SIP/2.0/UDP 192.168.11.50;branch=z9hG4bK4ed8.9b9866d701ca0e0ac1f33e28ac81577d.0">>,
        <<"Via: SIP/2.0/WS h6iuotu857tm.invalid;rport=33278;received=192.168.11.43;branch=z9hG4bK2505364">>,
        <<"From: <sip:user_wpxnx7am9w@wefwefwefwef.2600hz.com>;tag=j6mvjno40c">>,
        <<"To: <sip:*97@wefwefwefwef.2600hz.com>;tag=SyveX0cgvrSag">>,
        <<"Call-ID: sf34f93o1rpcbbujodu9">>,
        <<"CSeq: 280 BYE">>,
        <<"User-Agent: 2600hz">>,
        <<"Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, MESSAGE, INFO, UPDATE, REGISTER, REFER, PRACK, NOTIFY, PUBLISH, SUBSCRIBE">>,
        <<"Supported: precondition, 100rel, path, replaces">>,
        <<"Content-Length: 0">>]},
      {<<"src">>,<<"192.168.11.50:11000">>},
      {<<"dst">>,<<"192.168.11.50:5060">>},
      {<<"parser">>, <<"192.168.56.42:9061">>},
      {<<"c_seq">>, <<"280 BYE">>}]};

chunks_4(19) ->
    {[{<<"call-id">>,<<"sf34f93o1rpcbbujodu9">>},
      {<<"timestamp">>,65850481497},
      {<<"ref_timestamp">>,<<"63611209435.0">>},
      {<<"label">>,<<"SIP/2.0 200 OK">>},
      {<<"raw">>,
       [<<"SIP/2.0 200 OK">>,
        <<"Via: SIP/2.0/WS h6iuotu857tm.invalid;rport=33278;received=192.168.11.43;branch=z9hG4bK2505364">>,
        <<"From: <sip:user_wpxnx7am9w@wefwefwefwef.2600hz.com>;tag=j6mvjno40c">>,
        <<"To: <sip:*97@wefwefwefwef.2600hz.com>;tag=SyveX0cgvrSag">>,
        <<"Call-ID: sf34f93o1rpcbbujodu9">>,
        <<"CSeq: 280 BYE">>,
        <<"User-Agent: 2600hz">>,
        <<"Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, MESSAGE, INFO, UPDATE, REGISTER, REFER, PRACK, NOTIFY, PUBLISH, SUBSCRIBE">>,
        <<"Supported: precondition, 100rel, path, replaces">>,
        <<"Content-Length: 0">>]},
      {<<"src">>,<<"192.168.11.50:8080">>},
      {<<"dst">>,<<"192.168.11.43:33278">>},
      {<<"parser">>, <<"192.168.56.42:9061">>},
      {<<"c_seq">>, <<"280 BYE">>}]}.

chunks_4() ->
    [chunks_4(1),
     chunks_4(2),
     chunks_4(3),
     chunks_4(4),
     chunks_4(5),
     chunks_4(6),
     chunks_4(7),
     chunks_4(8),
     chunks_4(9),
     chunks_4(10),
     chunks_4(11),
     chunks_4(12),
     chunks_4(13),
     chunks_4(14),
     chunks_4(15),
     chunks_4(16),
     chunks_4(17),
     chunks_4(18),
     chunks_4(19)].


chunks_5() -> [chunks_5(1), chunks_5(2)].
chunks_5(count) -> length(chunks_5());
chunks_5(entities) -> [<<"104.237.144.93:5060">>, <<"104.237.144.93:11000">>];
chunks_5(1) ->
    kz_json:from_map(
      #{<<"c_seq">> => <<"10 OPTIONS">>
       ,<<"call-id">> => <<"2296e6ac231dc1f0-17861@104.237.144.93">>
       ,<<"dst">> => <<"104.237.144.93:11000">>
       ,<<"label">> => <<"OPTIONS sip:104.237.144.93:11000 SIP/2.0">>
       ,<<"parser">> => <<"104.237.144.93:9061">>
       ,<<"raw">> => [<<"OPTIONS sip:104.237.144.93:11000 SIP/2.0">>
                     ,<<"Via: SIP/2.0/UDP 104.237.144.93;branch=z9hG4bKb03a.a7adf2a6000000000000000000000000.0">>
                     ,<<"To: <sip:104.237.144.93:11000>">>
                     ,<<"From: <sip:sipcheck@apps001.ewr.sb.2600hz.com>;tag=58381c399f3f4cfffb20e60f3e6f0265-1edf">>
                     ,<<"CSeq: 10 OPTIONS">>
                     ,<<"Call-ID: 2296e6ac231dc1f0-17861@104.237.144.93">>
                     ,<<"Max-Forwards: 70">>
                     ,<<"Content-Length: 0">>
                     ]
       ,<<"ref_timestamp">> => <<"63657429461.0">>
       ,<<"src">> => <<"104.237.144.93:5060">>
       ,<<"timestamp">> => 63657429461
       });

chunks_5(2) ->
    kz_json:from_map(
      #{<<"c_seq">> => <<"10 OPTIONS">>
       ,<<"call-id">> => <<"2296e6ac231dc1f0-17861@104.237.144.93">>
       ,<<"dst">> => <<"104.237.144.93:5060">>
       ,<<"label">> => <<"SIP/2.0 200 OK">>
       ,<<"parser">> => <<"104.237.144.93:9061">>
       ,<<"raw">> => [<<"SIP/2.0 200 OK">>
                     ,<<"Via: SIP/2.0/UDP 104.237.144.93;branch=z9hG4bKb03a.a7adf2a6000000000000000000000000.0">>
                     ,<<"From: <sip:sipcheck@apps001.ewr.sb.2600hz.com>;tag=58381c399f3f4cfffb20e60f3e6f0265-1edf">>
                     ,<<"To: <sip:104.237.144.93:11000>;tag=Dv4gyUcma547B">>
                     ,<<"Call-ID: 2296e6ac231dc1f0-17861@104.237.144.93">>
                     ,<<"CSeq: 10 OPTIONS">>
                     ,<<"Contact: <sip:104.237.144.93:11000>">>
                     ,<<"User-Agent: 2600hz">>
                     ,<<"Accept: application/sdp">>
                     ,<<"Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, MESSAGE, INFO, UPDATE, REGISTER, REFER, NOTIFY, PUBLISH, SUBSCRIBE">>
                     ,<<"Supported: path, replaces">>
                     ,<<"Allow-Events: talk, hold, conference, presence, as-feature-event, dialog, line-seize, call-info, sla, include-session-description, presence.winfo, message-summary, refer">>
                     ,<<"Content-Length: 0">>
                     ]
       ,<<"ref_timestamp">> => <<"63657429461.1">>
       ,<<"src">> => <<"104.237.144.93:11000">>
       ,<<"timestamp">> => 63657429461
       }).


chunks_6() -> [chunks_6(1), chunks_6(2)].
chunks_6(count) -> length(chunks_6());
chunks_6(entities) -> [<<"104.237.144.93:7000">>, <<"104.237.144.93:11000">>];
chunks_6(1) ->
    kz_json:from_map(
      #{<<"c_seq">> => <<"2 BYE">>
       ,<<"call-id">> => <<"83078MTBlNWM5YWVmZDk0NWRkYWQ5NGQyOTM2YWIzMTQzMmQ">>
       ,<<"dst">> => <<"104.237.144.93:11000">>
       ,<<"label">> => <<"BYE sip:7948@104.237.144.93:11000;transport=udp SIP/2.0">>
       ,<<"parser">> => <<"104.237.144.93:9061">>
       ,<<"raw">> => [<<"BYE sip:7948@104.237.144.93:11000;transport=udp SIP/2.0">>
                     ,<<"Via: SIP/2.0/UDP 104.237.144.93:7000;branch=z9hG4bK6283.c52d1b9022e00c81053288ca5b92f09d.0">>
                     ,<<"Via: SIP/2.0/UDP 192.168.1.2:54690;received=67.180.78.125;branch=z9hG4bK-524287-1---cada5e002096c973;rport=54690">>
                     ,<<"Max-Forwards: 50">>
                     ,<<"Contact: <sip:darren_soft@67.180.78.125:54690;rinstance=fb81a6d2b51c24ba>">>
                     ,<<"To: <sip:7948@4a6863.sip.sandbox.2600hz.com>;tag=QQpga8pgH7gyF">>
                     ,<<"From: <sip:darren_soft@4a6863.sip.sandbox.2600hz.com>;tag=8f71823a">>
                     ,<<"Call-ID: 83078MTBlNWM5YWVmZDk0NWRkYWQ5NGQyOTM2YWIzMTQzMmQ">>
                     ,<<"CSeq: 2 BYE">>
                     ,<<"User-Agent: Bria 4 release 4.7.0 stamp 83078">>
                     ,<<"Content-Length: 0">>
                     ,<<"X-AUTH-IP: 67.180.78.125">>
                     ,<<"X-AUTH-PORT: 54690">>
                     ]
       ,<<"ref_timestamp">> => <<"63657597518.515564">>
       ,<<"src">> => <<"104.237.144.93:7000">>
       ,<<"timestamp">> => 63657597518
       });

chunks_6(2) ->
    kz_json:from_map(
      #{<<"c_seq">> => <<"2 BYE">>
       ,<<"call-id">> => <<"83078MTBlNWM5YWVmZDk0NWRkYWQ5NGQyOTM2YWIzMTQzMmQ">>
       ,<<"dst">> => <<"104.237.144.93:7000">>
       ,<<"label">> => <<"SIP/2.0 200 OK">>
       ,<<"parser">> => <<"104.237.144.93:9061">>
       ,<<"raw">> => [<<"SIP/2.0 200 OK">>
                     ,<<"Via: SIP/2.0/UDP 104.237.144.93:7000;branch=z9hG4bK6283.c52d1b9022e00c81053288ca5b92f09d.0">>
                     ,<<"Via: SIP/2.0/UDP 192.168.1.2:54690;received=67.180.78.125;branch=z9hG4bK-524287-1---cada5e002096c973;rport=54690">>
                     ,<<"From: <sip:darren_soft@4a6863.sip.sandbox.2600hz.com>;tag=8f71823a">>
                     ,<<"To: <sip:7948@4a6863.sip.sandbox.2600hz.com>;tag=QQpga8pgH7gyF">>
                     ,<<"Call-ID: 83078MTBlNWM5YWVmZDk0NWRkYWQ5NGQyOTM2YWIzMTQzMmQ">>
                     ,<<"CSeq: 2 BYE">>
                     ,<<"User-Agent: 2600hz">>
                     ,<<"Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, MESSAGE, INFO, UPDATE, REGISTER, REFER, NOTIFY, PUBLISH, SUBSCRIBE">>
                     ,<<"Supported: path, replaces">>
                     ,<<"Content-Length: 0">>
                     ]
       ,<<"ref_timestamp">> => <<"63657597518.531">>
       ,<<"src">> => <<"104.237.144.93:11000">>
       ,<<"timestamp">> => 63657597518
       }).


chunks_7() -> [chunks_7(1), chunks_7(2), chunks_7(3), chunks_7(4)].
chunks_7(count) -> length(chunks_7());
chunks_7(entities) -> [<<"104.237.144.93:5060">>, <<"104.237.144.93:11000">>];
chunks_7(1) ->
    kz_json:from_map(
      #{<<"c_seq">> => <<"1 INVITE">>
       ,<<"call-id">> => <<"YVVIXTNGHMHIKYDTLBAHBXCW">>
       ,<<"dst">> => <<"104.237.144.93:11000">>
       ,<<"label">> => <<"INVITE sip:9990048188880071@104.237.144.93 SIP/2.0">>
       ,<<"parser">> => <<"104.237.144.93:9061">>
       ,<<"raw">> => [<<"INVITE sip:9990048188880071@104.237.144.93 SIP/2.0">>
                     ,<<"Record-Route: <sip:104.237.144.93;lr=on;ftag=DVEFCVLI>">>
                     ,<<"Via: SIP/2.0/UDP 104.237.144.93;branch=z9hG4bK4f7f.fa988d55e5c136baf2b89b62f1f7a2d0.0">>
                     ,<<"Via: SIP/2.0/UDP 80.82.77.180:40482;received=80.82.77.180;branch=z9hG4bK-524287-1---321bda12cf15b137;rport=40482">>
                     ,<<"Max-Forwards: 50">>
                     ,<<"Contact: <sip:trunk@80.82.77.180:40482>;+sip.instance=\"<urn:uuid:4473a234-5a1c-4708-9729-8952445196c0>\"">>
                     ,<<"To: <sip:9990048188880071@104.237.144.93>">>
                     ,<<"From: <sip:trunk@104.237.144.93>;tag=DVEFCVLI">>
                     ,<<"Call-ID: YVVIXTNGHMHIKYDTLBAHBXCW">>
                     ,<<"CSeq: 1 INVITE">>
                     ,<<"Allow: INVITE, ACK, CANCEL, OPTIONS, BYE, REFER, NOTIFY, MESSAGE, REGISTER, SUBSCRIBE, INFO">>
                     ,<<"Content-Type: application/sdp">>
                     ,<<"Supported: replaces">>
                     ,<<"User-Agent: Cisco-SIPGateway/IOS-12.x">>
                     ,<<"Allow-Events: hold, talk, conference">>
                     ,<<"Content-Length: 0">>
                     ,<<"X-AUTH-IP: 80.82.77.180">>
                     ,<<"X-AUTH-PORT: 40482">>
                     ]
       ,<<"ref_timestamp">> => <<"63657540282.88624">>
       ,<<"src">> => <<"104.237.144.93:5060">>
       ,<<"timestamp">> => 63657540282
       });

chunks_7(2) ->
    kz_json:from_map(
      #{<<"c_seq">> => <<"1 INVITE">>
       ,<<"call-id">> => <<"YVVIXTNGHMHIKYDTLBAHBXCW">>
       ,<<"dst">> => <<"104.237.144.93:5060">>
       ,<<"label">> => <<"SIP/2.0 100 Trying">>
       ,<<"parser">> => <<"104.237.144.93:9061">>
       ,<<"raw">> => [<<"SIP/2.0 100 Trying">>
                     ,<<"Via: SIP/2.0/UDP 104.237.144.93;branch=z9hG4bK4f7f.fa988d55e5c136baf2b89b62f1f7a2d0.0">>
                     ,<<"Via: SIP/2.0/UDP 80.82.77.180:40482;received=80.82.77.180;branch=z9hG4bK-524287-1---321bda12cf15b137;rport=40482">>
                     ,<<"Record-Route: <sip:104.237.144.93;lr=on;ftag=DVEFCVLI>">>
                     ,<<"From: <sip:trunk@104.237.144.93>;tag=DVEFCVLI">>
                     ,<<"To: <sip:9990048188880071@104.237.144.93>">>
                     ,<<"Call-ID: YVVIXTNGHMHIKYDTLBAHBXCW">>
                     ,<<"CSeq: 1 INVITE">>
                     ,<<"User-Agent: 2600hz">>
                     ,<<"Content-Length: 0">>
                     ]
       ,<<"ref_timestamp">> => <<"63657540282.88642">>
       ,<<"src">> => <<"104.237.144.93:11000">>
       ,<<"timestamp">> => 63657540282
       });

chunks_7(3) ->
    kz_json:from_map(
      #{<<"c_seq">> => <<"1 INVITE">>
       ,<<"call-id">> => <<"YVVIXTNGHMHIKYDTLBAHBXCW">>
       ,<<"dst">> => <<"104.237.144.93:5060">>
       ,<<"label">> => <<"SIP/2.0 407 Proxy Authentication Required">>
       ,<<"parser">> => <<"104.237.144.93:9061">>
       ,<<"raw">> => [<<"SIP/2.0 407 Proxy Authentication Required">>
                     ,<<"Via: SIP/2.0/UDP 104.237.144.93;branch=z9hG4bK4f7f.fa988d55e5c136baf2b89b62f1f7a2d0.0">>
                     ,<<"Via: SIP/2.0/UDP 80.82.77.180:40482;received=80.82.77.180;branch=z9hG4bK-524287-1---321bda12cf15b137;rport=40482">>
                     ,<<"From: <sip:trunk@104.237.144.93>;tag=DVEFCVLI">>
                     ,<<"To: <sip:9990048188880071@104.237.144.93>;tag=KU273F3tNU1rp">>
                     ,<<"Call-ID: YVVIXTNGHMHIKYDTLBAHBXCW">>
                     ,<<"CSeq: 1 INVITE">>
                     ,<<"User-Agent: 2600hz">>
                     ,<<"Accept: application/sdp">>
                     ,<<"Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, MESSAGE, INFO, UPDATE, REGISTER, REFER, NOTIFY, PUBLISH, SUBSCRIBE">>
                     ,<<"Supported: path, replaces">>
                     ,<<"Allow-Events: talk, hold, conference, presence, as-feature-event, dialog, line-seize, call-info, sla, include-session-description, presence.winfo, message-summary, refer">>
                     ,<<"Proxy-Authenticate: Digest realm=\"104.237.144.93\">> nonce=\"3f0c7d4e-1036-11e7-8998-8543a0a6a878\">> algorithm=MD5, qop=\"auth\"">>
                     ,<<"Content-Length: 0">>
                     ]
       ,<<"ref_timestamp">> => <<"63657540283.01826">>
       ,<<"src">> => <<"104.237.144.93:11000">>
       ,<<"timestamp">> => 63657540283
       });

chunks_7(4) ->
    kz_json:from_map(
      #{<<"c_seq">> => <<"1 ACK">>
       ,<<"call-id">> => <<"YVVIXTNGHMHIKYDTLBAHBXCW">>
       ,<<"dst">> => <<"104.237.144.93:11000">>
       ,<<"label">> => <<"ACK sip:9990048188880071@104.237.144.93 SIP/2.0">>
       ,<<"parser">> => <<"104.237.144.93:9061">>
       ,<<"raw">> => [<<"ACK sip:9990048188880071@104.237.144.93 SIP/2.0">>
                     ,<<"Via: SIP/2.0/UDP 104.237.144.93;branch=z9hG4bK4f7f.fa988d55e5c136baf2b89b62f1f7a2d0.0">>
                     ,<<"Max-Forwards: 50">>
                     ,<<"To: <sip:9990048188880071@104.237.144.93>;tag=KU273F3tNU1rp">>
                     ,<<"From: <sip:trunk@104.237.144.93>;tag=DVEFCVLI">>
                     ,<<"Call-ID: YVVIXTNGHMHIKYDTLBAHBXCW">>
                     ,<<"CSeq: 1 ACK">>
                     ,<<"Content-Length: 0">>
                     ]
       ,<<"ref_timestamp">> => <<"63657540283.018394">>
       ,<<"src">> => <<"104.237.144.93:5060">>
       ,<<"timestamp">> => 63657540283
       }).

%% End of Module.
