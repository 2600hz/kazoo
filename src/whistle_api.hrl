-define(DEFAULT_HEADERS, [<<"Server-ID">>, <<"Event-Category">>, <<"Event-Name">>
			      , <<"App-Name">>, <<"App-Version">>]).
-define(OPTIONAL_DEFAULT_HEADERS, [<<"Raw-Headers">>, <<"Destination-Server">>
				  , <<"Geo-Location">>, <<"Access-Group">>
				  , <<"Tenant-ID">>]).

-define(AUTH_REQ_HEADERS, [<<"Msg-ID">>, <<"To">>, <<"From">>, <<"Orig-IP">>
			       , <<"Auth-User">>, <<"Auth-Domain">>]).

-define(AUTH_RESP_HEADERS, [<<"Msg-ID">>, <<"Auth-Method">>, <<"Auth-Pass">>]).

-define(ROUTE_REQ_HEADERS, [<<"Msg-ID">>, <<"To">>, <<"From">>, <<"Call-ID">>, <<"Event-Queue">>]).
-define(OPTIONAL_ROUTE_REQ_HEADERS, [<<"Min-Setup-Cost">>, <<"Max-Setup-Cost">>, <<"Geo-Location">>
					 ,<<"Orig-IP">>, <<"Max-Call-Length">>, <<"Media">> %%process | proxy | bypass
					 , <<"Transcode">>, <<"Codecs">>, <<"Tenant-ID">>
					 ,<<"Resource-Type">> %% MMS | SMS | audio | video | chat
					 ,<<"Min-Increment-Cost">>, <<"Max-Incremental-Cost">>]).

-define(ROUTE_RESP_HEADERS, [<<"Msg-ID">>, <<"Routes">>, <<"Method">>]).
-define(OPTIONAL_ROUTE_RESP_HEADERS, [<<"Route-Error-Code">>, <<"Route-Error-Message">>]).

-define(ROUTE_RESP_ROUTE_HEADERS, [<<"Route">>, <<"Weight-Cost">>, <<"Weight-Location">>]).
-define(OPTIONAL_ROUTE_RESP_ROUTE_HEADERS, [<<"Proxy-Via">>, <<"Media">>, <<"Auth-User">>
						,<<"Auth-Password">>, <<"Codec">>]).

-define(ERROR_RESP_HEADERS, [<<"Msg-ID">>, <<"Error-Message">>]).

-define(STORE_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Media-Name">>, <<"Media-Transfer-Method">>
				,<<"Media-Transfer-Destination">>]).
-define(OPTIONAL_STORE_REQ_HEADERS, [<<"Media-Additional-Headers">>]).

-define(STORE_AMQP_RESP_HEADERS, [<<"Call-ID">>, <<"Application-Name">>, <<"Media-Transfer-Method">>
				      ,<<"Media-Name">>, <<"Media-Sequence-ID">>, <<"Media-Content">> ]).
-define(OPTIONAL_STORE_AMQP_RESP_HEADERS, []).

-define(STORE_HTTP_RESP_HEADERS, [<<"Call-ID">>, <<"Application-Name">>, <<"Media-Transfer-Method">>,
				  <<"Media-Name">>, <<"Media-Transfer-Results">>]).
-define(OPTIONAL_STORE_HTTP_RESP_HEADERS, []).

%% [{FreeSWITCH-App-Name, Whistle-App-Name}]
%% Dialplan-related applications
%% convert from FS-named applications to Whistle-named Dialplan applications
-define(SUPPORTED_APPLICATIONS, [{<<"playback">>, <<"play">>}
				 ,{<<"hangup">>, <<"hangup">>}
				 ,{<<"record">>, <<"record">>}
				 ,{<<"playback">>, <<"tone">>}
				 ,{<<"park">>, <<"park">>}
				 ,{<<"set">>, <<"set">>}
				 ,{<<"say">>, <<"say">>}
				 ,{<<"sleep">>, <<"sleep">>}
				 ,{<<"bridge">>, <<"bridge">>}
				 ,{<<"answer">>, <<"answer">>}
				 ,{<<"play_and_get_digits">>, <<"play_and_collect_digits">>}
				]).

-define(CALL_EVENT_HEADERS, [<<"Event-Timestamp">>, <<"Call-ID">>, <<"Channel-Call-State">>]).
-define(OPTIONAL_CALL_EVENT_HEADERS, [<<"Application-Name">>, <<"Application-Message">>]).

-define(TONES_REQ_HEADERS, [<<"Call-ID">>, <<"Application-Name">>, <<"Tones">>]).
-define(OPTIONAL_TONES_REQ_HEADERS, []).

-define(TONES_REQ_TONE_HEADERS, [<<"Frequencies">>, <<"Duration-ON">>, <<"Duration-OFF">>]).
-define(OPTIONAL_TONES_REQ_TONE_HEADERS, [<<"Volume">>, <<"Repeat">>]).

-define(QUEUE_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Commands">>]).
-define(OPTIONAL_QUEUE_REQ_HEADERS, []).

-define(PLAY_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Filename">>]).
-define(OPTIONAL_PLAY_REQ_HEADERS, [<<"Playback-Terminators">>]).

-define(FS_EVENTS, [<<"CHANNEL_EXECUTE">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"CHANNEL_HANGUP">>]).

-type proplist() :: list(tuple(binary(), binary())). % just want to deal with binary K/V pairs
