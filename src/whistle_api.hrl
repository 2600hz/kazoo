%% We pass Application custom channel variables with our own prefix
%% When an event occurs, we include all prefixed vars in the API message
-define(CHANNEL_VAR_PREFIX, "ecallmgr_").

%%% *_HEADERS defines a list of Keys that must exist in every message of type *
%%% (substitute AUTH_REQ, AUTH_RESP, etc, for *).
%%%
%%% OPTIONAL_*_HEADERS defines a list of Keys that will be included in the final
%%% message if included in the passed in Proplist.
%%%
%%% *_VALUES defines a proplist of {Key, Value} pairs where Key is either in 
%%% *_HEADERS or OPTIONAL_*_HEADERS, and Value is either a singular value or a list
%%% of values that the resulting message can have, given Key.
%%% If Value is not a list, a direct match is required to validate;
%%% if Value is a list of singular values, the set value must be a member of the Value list
%%%
%%% *_TYPES defines a proplist of {Key, Type} pairs where Key is either in
%%% *_HEADERS or OPTIONAL_*_HEADERS, and Type defines a function that validates a passed in value
%%% is an appropriate type for the given Key, returning a boolean. If Key is not in the passed-in
%%% message, true is returned without running the Type fun.
%%% @spec Type :: function(Value :: any()) -> boolean()
%%%
%%% All messages MUST include the DEFAULT_HEADERS list.

%% Default Headers - http://corp.switchfreedom.com/mediawiki/index.php/General_Concepts
-define(DEFAULT_HEADERS, [<<"Server-ID">>, <<"Event-Category">>, <<"Event-Name">>
			      , <<"App-Name">>, <<"App-Version">>]).
-define(OPTIONAL_DEFAULT_HEADERS, [<<"Raw-Headers">>, <<"Destination-Server">>
				  , <<"Geo-Location">>, <<"Access-Group">>
				  , <<"Tenant-ID">>]).
-define(DEFAULT_VALUES, []).
-define(DEFAULT_TYPES, [{<<"Server-ID">>, fun is_binary/1}
			,{<<"Event-Category">>, fun is_binary/1}
			,{<<"Event-Name">>, fun is_binary/1}
			,{<<"App-Name">>, fun is_binary/1}
			,{<<"App-Version">>, fun is_binary/1}
			,{<<"Raw-Headers">>, fun is_binary/1}
			,{<<"Destination-Server">>, fun is_binary/1}
			,{<<"Geo-Location">>, fun is_binary/1}
			,{<<"Access-Group">>, fun is_binary/1}
			,{<<"Tenant-ID">>, fun is_binary/1}
			]).

%% Authentication Requests - http://corp.switchfreedom.com/mediawiki/index.php/Call_Authentication#Authentication_APIs
-define(AUTH_REQ_HEADERS, [<<"Msg-ID">>, <<"To">>, <<"From">>, <<"Orig-IP">>
			       , <<"Auth-User">>, <<"Auth-Domain">>]).
-define(OPTIONAL_AUTH_REQ_HEADERS, []).
-define(AUTH_REQ_VALUES, [{<<"Event-Category">>, <<"directory">>}
			  ,{<<"Event-Name">>, <<"auth_req">>}
			 ]).
-define(AUTH_REQ_TYPES, [{<<"Msg-ID">>, fun is_binary/1}
			 ,{<<"To">>, fun is_binary/1}
			 ,{<<"From">>, fun is_binary/1}
			 ,{<<"Orig-IP">>, fun is_binary/1}
			 ,{<<"Auth-User">>, fun is_binary/1}
			 ,{<<"Auth-Domain">>, fun is_binary/1}
			]).

%% Authentication Responses - http://corp.switchfreedom.com/mediawiki/index.php/Call_Authentication#Authentication_APIs
-define(AUTH_RESP_HEADERS, [<<"Msg-ID">>, <<"Auth-Method">>, <<"Auth-Password">>]).
-define(OPTIONAL_AUTH_RESP_HEADERS, [<<"Tenant-ID">>, <<"Access-Group">>, <<"Custom-Channel-Vars">>]).
-define(AUTH_RESP_VALUES, [{<<"Event-Category">>, <<"directory">>}
			   ,{<<"Event-Name">>, <<"auth_resp">>}
			   ,{<<"Auth-Method">>, [<<"password">>, <<"ip">>, <<"a1-hash">>, <<"error">>]}
			 ]).
-define(AUTH_RESP_TYPES, [{<<"Msg-ID">>, fun is_binary/1}
			  ,{<<"Auth-Password">>, fun is_binary/1}
			  ,{<<"Custom-Channel-Vars">>, fun({struct, L}) when is_list(L) ->
							       lists:all(fun({K, V}) when is_binary(K) andalso is_binary(V) -> true;
									    (_) -> false
									 end, L);
							  (_) -> false
						       end}
			  ,{<<"Access-Group">>, fun is_binary/1}
			  ,{<<"Tenant-ID">>, fun is_binary/1}
			 ]).


-define(ROUTE_REQ_HEADERS, [<<"Msg-ID">>, <<"To">>, <<"From">>, <<"Call-ID">>, <<"Event-Queue">>]).
-define(OPTIONAL_ROUTE_REQ_HEADERS, [<<"Min-Setup-Cost">>, <<"Max-Setup-Cost">>, <<"Geo-Location">>
					 ,<<"Orig-IP">>, <<"Max-Call-Length">>, <<"Media">> %%process | proxy | bypass
					 , <<"Transcode">>, <<"Codecs">>, <<"Custom-Channel-Vars">>
					 ,<<"Resource-Type">> %% MMS | SMS | audio | video | chat
					 ,<<"Min-Increment-Cost">>, <<"Max-Incremental-Cost">>
					 ,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
				    ]).

-define(ROUTE_RESP_HEADERS, [<<"Msg-ID">>, <<"Routes">>, <<"Method">>]).
-define(OPTIONAL_ROUTE_RESP_HEADERS, [<<"Route-Error-Code">>, <<"Route-Error-Message">>]).

-define(ROUTE_RESP_ROUTE_HEADERS, [<<"Route">>, <<"Weight-Cost">>, <<"Weight-Location">>]).
-define(OPTIONAL_ROUTE_RESP_ROUTE_HEADERS, [<<"Proxy-Via">>, <<"Media">>, <<"Auth-User">>
						,<<"Auth-Password">>, <<"Codecs">>
						,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>, <<"Caller-ID-Type">>
					   ]).

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
-define(OPTIONAL_CALL_EVENT_HEADERS, [<<"Application-Name">>, <<"Application-Message">>, <<"Custom-Channel-Vars">>]).

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
