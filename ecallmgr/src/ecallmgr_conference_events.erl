%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Publish conference events to AMQP
%%% @end
%%% Created : 21 Oct 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_conference_events).

-export([send_evt/1]).

%% for when the mapping isn't 1-1 FS to AMQP
%% {FS_Evt_Header, AMQP_Evt_Header}
-define(FS_CONF_EVT_MAP, [
			  {<<"Conference-Unique-ID">>, <<"Conference-UUID">>}
			  ,{<<"Floor">>, <<"Has-Floor">>}
			  ,{<<"Video">>, <<"Has-Video">>}
			  ,{<<"Hear">>, <<"Can-Hear">>}
			  ,{<<"Speak">>, <<"Can-Speak">>}
			  ,{<<"Talking">>, <<"Is-Talking">>}
			 ]).

-define(CONF_EVENT_HEADERS, [<<"Conference-Name">>, <<"Conference-Size">>, <<"Conference-UUID">>
				 ,<<"Has-Floor">>, <<"Has-Video">>, <<"Can-Hear">>, <<"Can-Speak">>
				 ,<<"Is-Talking">>, <<"Member-ID">>, <<"Member-Type">>, <<"Energy-Level">>
				 ,<<"Action">>, <<"Msg-ID">>, <<"Timestamp">>
			    ]).

send_evt(FSProp) ->
    <<"CUSTOM">> = props:get_value(<<"Event-Name">>, FSProp),
    <<"conference::maintenance">> = props:get_value(<<"Event-Subclass">>, FSProp),

    [{<<"Msg-ID">>, props:get_value(<<"Event-Date-Timestamp">>, FSProp)}
     ,{<<"Timestamp">>, props:get_value(<<"Event-Date-Timestamp">>, FSProp)} %% Timestamp in microsec?
    ].
       

%% Event-Subclass: conference%3A%3Amaintenance
%% Event-Name: CUSTOM
%% Core-UUID: 936c2612-2e2b-44a6-8c35-74af5a46a46a
%% FreeSWITCH-Hostname: pbx.2600hz.com
%% FreeSWITCH-IPv4: 174.143.141.188
%% FreeSWITCH-IPv6: %3A%3A1
%% Event-Date-Local: 2011-10-21%2023%3A53%3A49
%% Event-Date-GMT: Fri,%2021%20Oct%202011%2023%3A53%3A49%20GMT
%% Event-Date-Timestamp: 1319241229897 329
%% Event-Calling-File: mod_conference.c
%% Event-Calling-Function: conference_del_member
%% Event-Calling-Line-Number: 904
%% Channel-State: CS_HANGUP
%% Channel-Call-State: HANGUP
%% Channel-State-Number: 10
%% Channel-Name: sofia/sipinterface_3/3148096307%40184.106.180.207
%% Unique-ID: 1129d710-c493-45c2-87d4-d45f42e590c7
%% Call-Direction: inbound
%% Presence-Call-Direction: inbound
%% Channel-Presence-ID: 3148096307%40184.106.180.207
%% Answer-State: hangup
%% Channel-Read-Codec-Name: L16
%% Channel-Read-Codec-Rate: 8000
%% Channel-Read-Codec-Bit-Rate: 128000
%% Channel-Write-Codec-Name: PCMU
%% Channel-Write-Codec-Rate: 8000
%% Channel-Write-Codec-Bit-Rate: 64000
%% Caller-Direction: inbound
%% Caller-Username: 3148096307
%% Caller-Dialplan: XML
%% Caller-Caller-ID-Name: 3148096307
%% Caller-Caller-ID-Number: 3148096307
%% Caller-Network-Addr: 184.106.157.174
%% Caller-ANI: 3148096307
%% Caller-Destination-Number: conf2
%% Caller-Unique-ID: 1129d710-c493-45c2-87d4-d45f42e590c7
%% Caller-Source: mod_sofia
%% Caller-Context: context_1
%% Caller-RDNIS: 4158867998
%% Caller-Channel-Name: sofia/sipinterface_3/3148096307%40184.106.180.207
%% Caller-Profile-Index: 3
%% Caller-Profile-Created-Time: 1319241211401787
%% Caller-Channel-Created-Time: 1319241200377392
%% Caller-Channel-Answered-Time: 1319241200406549
%% Caller-Channel-Progress-Time: 0
%% Caller-Channel-Progress-Media-Time: 1319241200406549
%% Caller-Channel-Hangup-Time: 0
%% Caller-Channel-Transfer-Time: 0
%% Caller-Screen-Bit: true
%% Caller-Privacy-Hide-Name: false
%% Caller-Privacy-Hide-Number: false
%% Video: false
%% Hear: true
%% Speak: true
%% Talking: false
%% Mute-Detect: false
%% Member-ID: 75
%% Member-Type: member
%% Energy-Level: 20
%% Conference-Name: conference_2
%% Conference-Size: 0
%% Conference-Profile-Name: conference_2
%% Conference-Unique-ID: acbe4254-a404-416d-8f6d-9f8624937471
%% Action: del-member

    
