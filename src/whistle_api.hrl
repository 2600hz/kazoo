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

-define(ROUTE_RESP_ROUTE_HEADERS, [<<"Route">>, <<"Weight-Cost">>, <<"Weight-Location">>]).

-define(OPTIONAL_ROUTE_RESP_ROUTE_HEADERS, [<<"Proxy-Via">>, <<"Media">>, <<"Auth-User">>
						,<<"Auth-Password">>, <<"Codec">>]).

-define(ERROR_RESP_HEADERS, [<<"Msg-ID">>, <<"Error-Message">>]).

%% [{FreeSWITCH-App-Name, Whistle-App-Name}]
-define(SUPPORTED_APPLICATIONS, [{<<"playback">>, <<"play">>}
				 ,{<<"hangup">>, <<"hangup">>}
				]).

-type proplist() :: list(tuple(binary(), binary())). % just want to deal with binary K/V pairs
