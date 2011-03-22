-ifndef(WHISTLE_TYPES_INCLUDED).
-include("whistle_types.hrl").
-endif.

%% We pass Application custom channel variables with our own prefix
%% When an event occurs, we include all prefixed vars in the API message
-define(CHANNEL_VAR_PREFIX, "ecallmgr_").

%%% *_HEADERS defines a list of Keys that must exist in every message of type *
%%% (substitute AUTH_REQ, AUTH_RESP, etc, for *) to be considered valid.
%%%
%%% OPTIONAL_*_HEADERS defines a list of Keys that will be included in the final
%%% message if included in the passed in Proplist.
%%%
%%% *_VALUES defines a proplist of {Key, Value} pairs where Key is either in 
%%% *_HEADERS or OPTIONAL_*_HEADERS, and Value is either a singular value or a list
%%% of values that the resulting message can have, given Key.
%%% If Value is not a list, a direct match is required to validate;
%%% if Value is a list of singular values, the set value must be a member of the Value list
%%% eg: -define(FOO_HEADERS, [<<"bar">>]).
%%%     -define(OPTIONAL_FOO_HEADERS, [<<"baz">>]).
%%%     -define(FOO_VALUES, [{<<"bar">>, <<"yes">>}, {<<"baz">>, [<<"abe">>, <<"bea">>, <<"eab">>]}]).
%%%     when foo_v(Prop) is called, Prop MUST contain key <<"bar">> with value <<"yes">>, and MAY
%%%     contain key <<"baz">>; if <<"baz">> exists, it can only have values <<"abe">>, <<"bea">>, or <<"eab">>.
%%%     foo_v([]) -> fails because key <<"bar">> is missing
%%%     foo_v([{<<"bar">>, <<"no">>}]) -> fails because <<"bar">> can only have value <<"yes">>
%%%     foo_v([{<<"bar">>, <<"yes">>}]) -> passes!
%%%     foo_v([{<<"baz">>, <<"abe">>}]) -> fails, no key <<"bar">>
%%%     foo_v([{<<"bar">>, <<"no">>, }, {<<"baz">>, <<"abe">>}]) -> fails, <<"bar">> can only be <<"yes">>
%%%     foo_v([{<<"bar">>, <<"yes">>, }, {<<"baz">>, <<"zzz">>}]) -> fails, <<"zzz">> is not in ?FOO_VALUES
%%%     foo_v([{<<"bar">>, <<"yes">>, }, {<<"baz">>, <<"eab">>}]) -> passes!
%%%
%%% *_TYPES defines a proplist of {Key, Type} pairs where Key is either in
%%% *_HEADERS or OPTIONAL_*_HEADERS, and Type defines a function that validates a passed in value
%%% is an appropriate type for the given Key, returning a boolean. If Key is not in the passed-in
%%% message, true is returned without running the Type fun.
%%% @spec Type :: function(Value :: any()) -> boolean()
%%%
%%% eg: -define(FOO_TYPES, [{<<"baz">>, fun(V) -> lists:member(V, proplists:get_value(<<"baz">>, ?FOO_VALUES)) end}]).
%%%   would define a function to validate the value of key <<"baz">> in the same way ?FOO_VALUES does.
%%%
%%% All four macros must be defined; OPTIONAL, VALUES, and TYPES can be empty lists.

%% Default Headers - http://corp.switchfreedom.com/mediawiki/index.php/General_Concepts
%% All messages MUST include the DEFAULT_HEADERS list.
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

%% Registration Success
-define(REG_SUCCESS_HEADERS, [<<"Event-Timestamp">>, <<"From-User">>, <<"From-Host">>, <<"Contact">>, <<"RPid">>
				 ,<<"Expires">>, <<"To-User">>, <<"To-Host">>, <<"Network-IP">>, <<"Network-Port">>
				 , <<"Username">>, <<"Realm">>
			    ]).
-define(OPTIONAL_REG_SUCCESS_HEADERS, [<<"Status">>, <<"User-Agent">>]).
-define(REG_SUCCESS_VALUES, [{<<"Event-Category">>, <<"directory">>}
			    ,{<<"Event-Name">>, <<"reg_success">>}
			   ]).
-define(REG_SUCCESS_TYPES, []).

%% Query Registrations
-define(REG_QUERY_HEADERS, [<<"Username">>, <<"Realm">>, <<"Fields">>]).
-define(OPTIONAL_REG_QUERY_HEADERS, []).
-define(REG_QUERY_VALUES, [{<<"Event-Category">>, <<"directory">>}
			   ,{<<"Event-Name">>, <<"reg_query">>}
			  ]).
-define(REG_QUERY_TYPES, [{<<"Fields">>, fun(Fs) when is_list(Fs) ->
						 Allowed = ?OPTIONAL_REG_SUCCESS_HEADERS ++ ?REG_SUCCESS_HEADERS,
						 lists:foldl(fun(F, true) -> lists:member(F, Allowed);
								(_, false) -> false
							     end, true, Fs);
					    (_) -> false
					 end}
			 ]).

%% Registration Query Response
-define(REG_QUERY_RESP_HEADERS, [<<"Fields">>]).
-define(OPTIONAL_REG_QUERY_RESP_HEADERS, []).
-define(REG_QUERY_RESP_VALUES, [{<<"Event-Category">>, <<"directory">>}
				,{<<"Event-Name">>, <<"reg_query_resp">>}
			       ]).
-define(REG_QUERY_RESP_TYPES, []).

%% Route Requests - http://corp.switchfreedom.com/mediawiki/index.php/Resource_Control_%28Call_Setup_/_Teardown%29
-define(ROUTE_REQ_HEADERS, [<<"Msg-ID">>, <<"To">>, <<"From">>, <<"Call-ID">>
				,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
			   ]).
-define(OPTIONAL_ROUTE_REQ_HEADERS, [<<"Geo-Location">>, <<"Orig-IP">>, <<"Max-Call-Length">>, <<"Media">>
					 ,<<"Transcode">>, <<"Codecs">>, <<"Custom-Channel-Vars">>
					 ,<<"Resource-Type">>, <<"Cost-Parameters">>
				    ]).
-define(ROUTE_REQ_VALUES, [{<<"Event-Category">>, <<"dialplan">>}
			   ,{<<"Event-Name">>, <<"route_req">>}
			   ,{<<"Resource-Type">>, [<<"MMS">>, <<"SMS">>, <<"audio">>, <<"video">>, <<"chat">>]}
			   ,{<<"Media">>, [<<"process">>, <<"proxy">>, <<"bypass">>]}
			  ]).
-define(ROUTE_REQ_TYPES, [{<<"Msg-ID">>, fun is_binary/1}
			  ,{<<"To">>, fun is_binary/1}
			  ,{<<"From">>, fun is_binary/1}
			  ,{<<"Call-ID">>, fun is_binary/1}
			  ,{<<"Event-Queue">>, fun is_binary/1}
			  ,{<<"Caller-ID-Name">>, fun is_binary/1}
			  ,{<<"Caller-ID-Number">>, fun is_binary/1}
			  ,{<<"Cost-Parameters">>, fun({struct, L}) when is_list(L) ->
							   lists:all(fun({K, _V}) ->
									     lists:member(K, ?ROUTE_REQ_COST_PARAMS)
								     end, L);
						      (_) -> false
						   end}
			  ,{<<"Custom-Channel-Vars">>, fun({struct, L}) when is_list(L) ->
							       true;
							  (_) -> false
						       end}
			 ]).
-define(ROUTE_REQ_COST_PARAMS, [<<"Min-Increment-Cost">>, <<"Max-Incremental-Cost">>
				    ,<<"Min-Setup-Cost">>, <<"Max-Setup-Cost">>
			       ]).

%% Route Responses - Sub-section Route - http://corp.switchfreedom.com/mediawiki/index.php/Resource_Control_%28Call_Setup_/_Teardown%29#.3CRoute.3E
-define(ROUTE_RESP_ROUTE_HEADERS, [<<"Invite-Format">>, <<"Weight-Cost">>, <<"Weight-Location">>]).
-define(OPTIONAL_ROUTE_RESP_ROUTE_HEADERS, [ <<"Route">>, <<"To-User">>, <<"To-Realm">>, <<"To-DID">>
						 ,<<"Proxy-Via">>, <<"Media">>, <<"Auth-User">>
						 ,<<"Auth-Password">>, <<"Codecs">>, <<"Progress-Timeout">>
						 ,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>, <<"Caller-ID-Type">>
						 ,<<"Rate">>, <<"Rate-Increment">>, <<"Rate-Minimum">>, <<"Surcharge">>
					   ]).
-define(ROUTE_RESP_ROUTE_VALUES, [{<<"Media">>, [<<"process">>, <<"bypass">>, <<"auto">>]}
				  ,{<<"Caller-ID-Type">>, [<<"from">>, <<"rpid">>, <<"pid">>]}
				  ,{<<"Invite-Format">>, [<<"username">>, <<"e164">>, <<"npan">>, <<"1npan">>, <<"route">>]}
				 ]).
-define(ROUTE_RESP_ROUTE_TYPES, [ {<<"Codecs">>, fun is_list/1}
				  ,{<<"Route">>, fun is_binary/1}
				  ,{<<"To-User">>, fun is_binary/1}
				  ,{<<"To-Realm">>, fun is_binary/1}
				]).

%% Route Responses - http://corp.switchfreedom.com/mediawiki/index.php/Resource_Control_%28Call_Setup_/_Teardown%29
-define(ROUTE_RESP_HEADERS, [<<"Msg-ID">>, <<"Routes">>, <<"Method">>]).
-define(OPTIONAL_ROUTE_RESP_HEADERS, [<<"Route-Error-Code">>, <<"Route-Error-Message">>]).
-define(ROUTE_RESP_VALUES, [{<<"Event-Category">>, <<"dialplan">>}
			    ,{<<"Event-Name">>, <<"route_resp">>}
			    ,{<<"Method">>, [<<"bridge">>, <<"park">>, <<"error">>]}
			   ]).
-define(ROUTE_RESP_TYPES, [{<<"Route-Error-Code">>, fun is_binary/1}
			   ,{<<"Route-Error-Message">>, fun is_binary/1}
			   ,{<<"Routes">>, fun(L) when is_list(L) -> true;
					      (_) -> false
					   end}
			  ]).

%% Route Winner - http://corp.switchfreedom.com/mediawiki/index.php/Resource_Control_%28Call_Setup_/_Teardown%29#.22Winning.22_Application_Response_from_Call_Manager
-define(ROUTE_WIN_HEADERS, [<<"Call-ID">>, <<"Control-Queue">>]).
-define(OPTIONAL_ROUTE_WIN_HEADERS, []).
-define(ROUTE_WIN_VALUES, [{<<"Event-Name">>, <<"route_win">>}]).
-define(ROUTE_WIN_TYPES, [{<<"Call-ID">>, fun is_binary/1}
			  ,{<<"Control-Queue">>, fun is_binary/1}
			 ]).

%% Resource Request - http://corp.switchfreedom.com/mediawiki/index.php/Resource_Control_%28Call_Setup_/_Teardown%29#Originate_Call_Request
-define(RESOURCE_REQ_HEADERS, [<<"Msg-ID">>, <<"Resource-Type">>, <<"Invite-Format">>]).
-define(OPTIONAL_RESOURCE_REQ_HEADERS, [<<"Resource-Minimum">>, <<"Resource-Maximum">>, <<"Geo-Location">>, <<"Custom-Channel-Vars">>
					    ,<<"Route">>, <<"To-User">>, <<"To-Realm">>, <<"To-DID">>
				       ]).
-define(RESOURCE_REQ_VALUES, [
			      {<<"Event-Category">>, <<"originate">>}
			      ,{<<"Event-Name">>, <<"resource_req">>}
			      ,{<<"Resource-Type">>, [<<"audio">>, <<"video">>]}
			      ,{<<"Invite-Format">>, [<<"username">>, <<"e164">>, <<"npan">>, <<"1npan">>, <<"route">>]}
			     ]).
-define(RESOURCE_REQ_TYPES, [{<<"Invite-Format">>, fun is_binary/1}
			     ,{<<"Route">>, fun is_binary/1}
			     ,{<<"To-User">>, fun is_binary/1}
			     ,{<<"To-Realm">>, fun is_binary/1}
			    ]).

%% Resource Response - http://corp.switchfreedom.com/mediawiki/index.php/Resource_Control_%28Call_Setup_/_Teardown%29#Originate_Call_Response
-define(RESOURCE_RESP_HEADERS, [<<"Msg-ID">>, <<"Call-ID">>, <<"Control-Queue">>]).
-define(OPTIONAL_RESOURCE_RESP_HEADERS, []).
-define(RESOURCE_RESP_VALUES, [
			       {<<"Event-Category">>, <<"originate">>}
			       ,{<<"Event-Name">>, <<"resource_resp">>}
			      ]).
-define(RESOURCE_RESP_TYPES, []).

%% Resource Error - http://corp.switchfreedom.com/mediawiki/index.php/Resource_Control_%28Call_Setup_/_Teardown%29#Originate_Call_Error_Response
-define(RESOURCE_ERROR_HEADERS, [<<"Msg-ID">>]).
-define(OPTIONAL_RESOURCE_ERROR_HEADERS, [<<"Failed-Attempts">>, <<"Failed-Route">>, <<"Failure-Message">>]).
-define(RESOURCE_ERROR_VALUES, [{<<"Event-Name">>, [<<"originate_error">>, <<"resource_error">>]}]).
-define(RESOURCE_ERROR_TYPES, []).

%% Call Events - http://corp.switchfreedom.com/mediawiki/index.php/Dialplan_Actions#Receiving_Call_Events
-define(CALL_EVENT_HEADERS, [<<"Timestamp">>, <<"Call-ID">>, <<"Channel-Call-State">>]).
-define(OPTIONAL_CALL_EVENT_HEADERS, [<<"Application-Name">>, <<"Application-Response">>, <<"Custom-Channel-Vars">>
					  ,<<"Msg-ID">>
					  ,<<"Other-Leg-Direction">>, <<"Other-Leg-Caller-ID-Name">>, <<"Other-Leg-Caller-ID-Number">> %% BRIDGE
					  ,<<"Other-Leg-Destination-Number">>,<<"Other-Leg-Unique-ID">> %% BRIDGE
					  ,<<"Detected-Tone">>, <<"DTMF-Duration">>, <<"DTMF-Digit">> %% DTMF and Tones
                                          ,<<"Terminator">>, <<"Hangup-Cause">> %% Hangup
				     ]).
-define(CALL_EVENT_VALUES, [{<<"Event-Category">>, <<"call_event">>}]).
-define(CALL_EVENT_TYPES, [{<<"Custom-Channel-Vars">>, fun({struct, L}) when is_list(L) ->
							       true;
							  (_) -> false
						       end}
			  ]).

%% Call Status Request
-define(CALL_STATUS_REQ_HEADERS, [<<"Call-ID">>]).
-define(OPTIONAL_CALL_STATUS_REQ_HEADERS, []).
-define(CALL_STATUS_REQ_VALUES, [{<<"Event-Category">>, <<"call_event">>}
			     ,{<<"Event-Name">>, <<"status_req">>}
			    ]).
-define(CALL_STATUS_REQ_TYPES, []).

%% Call Status Response
-define(CALL_STATUS_RESP_HEADERS, [<<"Call-ID">>, <<"Status">>]).
-define(OPTIONAL_CALL_STATUS_RESP_HEADERS, [<<"Custom-Channel-Vars">>, <<"Error-Msg">>]).
-define(CALL_STATUS_RESP_VALUES, [{<<"Event-Category">>, <<"call_event">>}
				  ,{<<"Event-Name">>, <<"status_resp">>}
				  ,{<<"Status">>, [<<"active">>, <<"tmpdown">>]}
				 ]).
-define(CALL_STATUS_RESP_TYPES, []).

%% Call CDR - http://corp.switchfreedom.com/mediawiki/index.php/CallmgrCDRSpec
-define(CALL_CDR_HEADERS, [<<"Hangup-Cause">>, <<"Handling-Server-Name">>, <<"Call-ID">>, <<"Timestamp">>
			       ,<<"Call-Direction">>, <<"To-Uri">>, <<"From-Uri">>
			       ,<<"Duration-Seconds">>, <<"Billing-Seconds">>, <<"Ringing-Seconds">>
			       ,<<"Digits-Dialed">>
			  ]).
-define(OPTIONAL_CALL_CDR_HEADERS, [<<"Custom-Channel-Vars">>, <<"Remote-SDP">>, <<"Local-SDP">>, <<"Caller-ID-Name">>
					,<<"Caller-ID-Number">>, <<"Callee-ID-Name">>, <<"Callee-ID-Number">>
					,<<"User-Agent">>, <<"Caller-ID-Type">>, <<"Other-Leg-Call-ID">>
				   ]).
-define(CALL_CDR_VALUES, [{<<"Event-Category">>, <<"call_detail">>}
			  ,{<<"Event-Name">>, <<"cdr">>}
			  ,{<<"Call-Direction">>, [<<"inbound">>, <<"outbound">>]}
			  ,{<<"Caller-ID-Type">>, [<<"pid">>, <<"rpid">>, <<"from">>]}
			 ]).
-define(CALL_CDR_TYPES, []).

%% Error Responses - http://corp.switchfreedom.com/mediawiki/index.php/General_Concepts#StandardAPIFormat
-define(ERROR_RESP_HEADERS, [<<"Msg-ID">>, <<"Error-Message">>]).
-define(OPTIONAL_ERROR_RESP_HEADERS, []).
-define(ERROR_RESP_VALUES, [{<<"Event-Category">>, <<"error">>}]).
-define(ERROR_RESP_TYPES, []).

%%% Dialplan Commands and related definitions

%% Store Request - http://corp.switchfreedom.com/mediawiki/index.php/Dialplan_Actions#Store
-define(STORE_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Media-Name">>, <<"Media-Transfer-Method">>
				,<<"Media-Transfer-Destination">>]).
-define(OPTIONAL_STORE_REQ_HEADERS, [<<"Media-Additional-Headers">>, <<"Insert-At">>]).
-define(STORE_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
			   ,{<<"Event-Name">>, <<"command">>}
			   ,{<<"Application-Name">>, <<"store">>}
			   ,{<<"Media-Transfer-Method">>, [<<"stream">>, <<"put">>, <<"post">>]}
			   ,{<<"Insert-At">>, [<<"head">>, <<"tail">>, <<"flush">>, <<"now">>]}
			  ]).
-define(STORE_REQ_TYPES, [{<<"Additional-Headers">>, fun is_list/1}]).

%% Store (via AMQP) Response - http://corp.switchfreedom.com/mediawiki/index.php/Dialplan_Actions#Media_File_Store_Response_-_AMQP
-define(STORE_AMQP_RESP_HEADERS, [<<"Call-ID">>, <<"Application-Name">>, <<"Media-Transfer-Method">>
				      ,<<"Media-Name">>, <<"Media-Sequence-ID">>, <<"Media-Content">>
				 ]).
-define(OPTIONAL_STORE_AMQP_RESP_HEADERS, []).
-define(STORE_AMQP_RESP_VALUES, [{<<"Application-Name">>, <<"store">>}
				 ,{<<"Media-Transfer-Method">>, <<"stream">>}
				]).
-define(STORE_AMQP_RESP_TYPES, [{<<"Media-Content">>, fun(V) -> is_binary(V) orelse V =:= eof end}
				,{<<"Media-Name">>, fun is_binary/1}
			       ]).

%% Store (via HTTP) Response - http://corp.switchfreedom.com/mediawiki/index.php/Dialplan_Actions#Media_File_Store_Response_-_HTTP
-define(STORE_HTTP_RESP_HEADERS, [<<"Call-ID">>, <<"Application-Name">>, <<"Media-Transfer-Method">>,
				  <<"Media-Name">>, <<"Media-Transfer-Results">>]).
-define(OPTIONAL_STORE_HTTP_RESP_HEADERS, []).
-define(STORE_HTTP_RESP_VALUES, [{<<"Application-Name">>, <<"store">>}
				 ,{<<"Media-Transfer-Method">>, [<<"put">>, <<"post">>]}
				]).
-define(STORE_HTTP_RESP_TYPES, [{<<"Media-Transfer-Results">>, fun({struct, L}) when is_list(L) ->
                                                                       true;
                                                                  (_) -> false
                                                               end}]).

%% Tones Request - http://corp.switchfreedom.com/mediawiki/index.php/Dialplan_Actions#Generate_Tone
-define(TONES_REQ_HEADERS, [<<"Call-ID">>, <<"Application-Name">>, <<"Tones">>]).
-define(OPTIONAL_TONES_REQ_HEADERS, [<<"Insert-At">>]).
-define(TONES_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
			   ,{<<"Event-Name">>, <<"command">>}
			   ,{<<"Application-Name">>, <<"tones">>}
			   ,{<<"Insert-At">>, [<<"head">>, <<"tail">>, <<"flush">>, <<"now">>]}
			  ]).
-define(TONES_REQ_TYPES, [{<<"Tones">>, fun is_list/1}]).

-define(TONES_REQ_TONE_HEADERS, [<<"Frequencies">>, <<"Duration-ON">>, <<"Duration-OFF">>]).
-define(OPTIONAL_TONES_REQ_TONE_HEADERS, [<<"Volume">>, <<"Repeat">>]).
-define(TONES_REQ_TONE_VALUES, [{<<"Event-Category">>, <<"call">>}
				,{<<"Event-Name">>, <<"command">>}
			       ]).
-define(TONES_REQ_TONE_TYPES, []).

%% Tone Detect - http://corp.switchfreedom.com/mediawiki/index.php/Dialplan_Actions#Tone_Detection
-define(TONE_DETECT_REQ_HEADERS, [<<"Call-ID">>, <<"Application-Name">>, <<"Tone-Detect-Name">>, <<"Frequencies">>]).
-define(OPTIONAL_TONE_DETECT_REQ_HEADERS, [<<"Sniff-Direction">>, <<"Timeout">>, <<"On-Success">>, <<"Hits-Needed">>, <<"Insert-At">>]).
-define(TONE_DETECT_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
				 ,{<<"Event-Name">>, <<"command">>}
				 ,{<<"Application-Name">>, <<"tone_detect">>}
				 ,{<<"Sniff-Direction">>, [<<"read">>, <<"write">>]}
				 ,{<<"Insert-At">>, [<<"head">>, <<"tail">>, <<"flush">>, <<"now">>]}
				]).
-define(TONE_DETECT_REQ_TYPES, [{<<"On-Success">>, fun is_list/1}
				,{<<"Timeout">>, fun(<<"+", T/binary>>) ->
							 try whistle_util:to_integer(T), true
							 catch _:_ -> false
							 end;
						    (T) ->
							 try whistle_util:to_integer(T), true
							 catch _:_ -> false
							 end
						 end}
			       ]).

%% Queue Request - http://corp.switchfreedom.com/mediawiki/index.php/Dialplan_Actions#Queue
-define(QUEUE_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Commands">>]).
-define(OPTIONAL_QUEUE_REQ_HEADERS, [<<"Insert-At">>]).
-define(QUEUE_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
			   ,{<<"Event-Name">>, <<"command">>}
			   ,{<<"Application-Name">>, <<"queue">>}
			   ,{<<"Insert-At">>, [<<"head">>, <<"tail">>, <<"flush">>, <<"now">>]}
			  ]).
-define(QUEUE_REQ_TYPES, [{<<"Commands">>, fun is_list/1}]).

%% Bridge Request - http://corp.switchfreedom.com/mediawiki/index.php/Dialplan_Actions#Bridge
-define(BRIDGE_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Endpoints">>]).
-define(OPTIONAL_BRIDGE_REQ_HEADERS, [<<"Timeout">>, <<"Continue-On-Fail">>
                                      ,<<"Outgoing-Caller-ID-Name">>, <<"Outgoing-Caller-ID-Number">>
                                      ,<<"Ringback">>, <<"Dial-Endpoint-Method">>, <<"Insert-At">>
				     ]).
-define(BRIDGE_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
			    ,{<<"Event-Name">>, <<"command">>}
			    ,{<<"Application-Name">>, <<"bridge">>}
			    ,{<<"Dial-Endpoint-Method">>, [<<"single">>, <<"simultaneous">>]}
			    ,{<<"Continue-On-Fail">>, [<<"true">>, <<"false">>]}
			    ,{<<"Insert-At">>, [<<"head">>, <<"tail">>, <<"flush">>, <<"now">>]}
			   ]).
-define(BRIDGE_REQ_TYPES, [{<<"Endpoints">>, fun is_list/1}]).

%% Endpoints - http://corp.switchfreedom.com/mediawiki/index.php/Dialplan_Actions#.3CEndpoint.3E
-define(BRIDGE_REQ_ENDPOINT_HEADERS, [<<"Invite-Format">>]).
-define(OPTIONAL_BRIDGE_REQ_ENDPOINT_HEADERS, [ <<"Route">>, <<"To-User">>, <<"To-Realm">>, <<"To-DID">>
						    ,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
                                                    ,<<"Ignore-Early-Media">>, <<"Bypass-Media">>
                                                    ,<<"Endpoint-Timeout">>, <<"Endpoint-Progress-Timeout">>
                                                    ,<<"Endpoint-Delay">>, <<"Codecs">>
					      ]).
-define(BRIDGE_REQ_ENDPOINT_VALUES, [{<<"Invite-Format">>, [<<"username">>, <<"npan">>, <<"1npan">>, <<"e164">>, <<"route">>]}
                                     ,{<<"Ignore-Early-Media">>, [<<"true">>, <<"false">>]}
                                     ,{<<"Bypass-Media">>, [<<"true">>, <<"false">>]}
                                    ]).
-define(BRIDGE_REQ_ENDPOINT_TYPES, []).

%% Answer - http://corp.switchfreedom.com/mediawiki/index.php/Dialplan_Actions#Answer
-define(ANSWER_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>]).
-define(OPTIONAL_ANSWER_REQ_HEADERS, [<<"Insert-At">>]).
-define(ANSWER_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
			    ,{<<"Event-Name">>, <<"command">>}
			    ,{<<"Application-Name">>, <<"answer">>}
			    ,{<<"Insert-At">>, [<<"head">>, <<"tail">>, <<"flush">>, <<"now">>]}
			   ]).
-define(ANSWER_REQ_TYPES, []).

%% Hangup - http://corp.switchfreedom.com/mediawiki/index.php/Dialplan_Actions#Hangup
-define(HANGUP_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>]).
-define(OPTIONAL_HANGUP_REQ_HEADERS, [<<"Insert-At">>]).
-define(HANGUP_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
			    ,{<<"Event-Name">>, <<"command">>}
			    ,{<<"Application-Name">>, <<"hangup">>}
			    ,{<<"Insert-At">>, [<<"head">>, <<"tail">>, <<"flush">>, <<"now">>]}
			   ]).
-define(HANGUP_REQ_TYPES, []).

%% Park - http://corp.switchfreedom.com/mediawiki/index.php/Dialplan_Actions#Hold.2FPark
-define(PARK_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>]).
-define(OPTIONAL_PARK_REQ_HEADERS, [<<"Insert-At">>]).
-define(PARK_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
			  ,{<<"Event-Name">>, <<"command">>}
			  ,{<<"Application-Name">>, <<"park">>}
			  ,{<<"Insert-At">>, [<<"head">>, <<"tail">>, <<"flush">>, <<"now">>]}
			 ]).
-define(PARK_REQ_TYPES, []).

%% Set - http://corp.switchfreedom.com/mediawiki/index.php/Dialplan_Actions#Hold.2FSet
-define(SET_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Custom-Channel-Vars">>]).
-define(OPTIONAL_SET_REQ_HEADERS, [<<"Insert-At">>]).
-define(SET_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
			 ,{<<"Event-Name">>, <<"command">>}
			 ,{<<"Application-Name">>, <<"set">>}
			 ,{<<"Insert-At">>, [<<"head">>, <<"tail">>, <<"flush">>, <<"now">>]}
			 ]).
-define(SET_REQ_TYPES, [
			{<<"Custom-Channel-Vars">>, fun({struct, L}) when is_list(L) ->
							    lists:all(fun({K, V}) when is_binary(K) andalso
										       (is_binary(V) orelse is_number(V)) -> true;
									 (_) -> false
								      end, L);
						       (_) -> false
						    end}
		       ]).

%% Call Pickup - http://corp.switchfreedom.com/mediawiki/index.php/Dialplan_Actions#Call_Pickup
-define(CALL_PICKUP_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>]).
-define(OPTIONAL_CALL_PICKUP_REQ_HEADERS, [<<"Insert-At">>]).
-define(CALL_PICKUP_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
				 ,{<<"Event-Name">>, <<"command">>}
				 ,{<<"Application-Name">>, <<"call_pickup">>}
				 ,{<<"Insert-At">>, [<<"head">>, <<"tail">>, <<"flush">>, <<"now">>]}
				]).
-define(CALL_PICKUP_REQ_TYPES, []).


%% Play Request - http://corp.switchfreedom.com/mediawiki/index.php/Dialplan_Actions#Play
-define(PLAY_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Media-Name">>]).
-define(OPTIONAL_PLAY_REQ_HEADERS, [<<"Terminators">>, <<"Insert-At">>]).
-define(PLAY_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
			  ,{<<"Event-Name">>, <<"command">>}
			  ,{<<"Application-Name">>, <<"play">>}
			  ,{<<"Insert-At">>, [<<"head">>, <<"tail">>, <<"flush">>, <<"now">>]}
			 ]).
-define(PLAY_REQ_TYPES, [{<<"Terminators">>, fun is_list/1}]).

%% Media Request - when streaming is needed
-define(MEDIA_REQ_HEADERS, [<<"Media-Name">>]).
-define(OPTIONAL_MEDIA_REQ_HEADERS, [<<"Stream-Type">>]).
-define(MEDIA_REQ_VALUES, [{<<"Event-Category">>, <<"media">>}
			   ,{<<"Event-Name">>, <<"media_req">>}
			   ,{<<"Stream-Type">>, [<<"new">>, <<"extant">>]}
			  ]).
-define(MEDIA_REQ_TYPES, []).

%% Media Response
-define(MEDIA_RESP_HEADERS, [<<"Media-Name">>, <<"Stream-URL">>]).
-define(OPTIONAL_MEDIA_RESP_HEADERS, []).
-define(MEDIA_RESP_VALUES, [{<<"Event-Category">>, <<"media">>}
			   ,{<<"Event-Name">>, <<"media_resp">>}
			  ]).
-define(MEDIA_RESP_TYPES, [{<<"Stream-URL">>, fun(<<"shout://", _/binary>>) -> true; 
                                                 (<<"http://", _/binary>>) -> true; 
                                                 (_) -> false end}]).

%% Media Error
-define(MEDIA_ERROR_HEADERS, [<<"Media-Name">>, <<"Error-Code">>]).
-define(OPTIONAL_MEDIA_ERROR_HEADERS, [<<"Error-Msg">>]).
-define(MEDIA_ERROR_VALUES, [{<<"Event-Category">>, <<"media">>}
			     ,{<<"Event-Name">>, <<"media_error">>}
			     ,{<<"Error-Code">>, [<<"not_found">>, <<"no_data">>, <<"other">>]}
			     ]).
-define(MEDIA_ERROR_TYPES, []).

%% Record Request - http://corp.switchfreedom.com/mediawiki/index.php/Dialplan_Actions#Record
-define(RECORD_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Media-Name">>]).
-define(OPTIONAL_RECORD_REQ_HEADERS, [<<"Terminators">>, <<"Time-Limit">>, <<"Silence-Threshold">>
					  ,<<"Silence-Hits">>, <<"Insert-At">>
				     ]).
-define(RECORD_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
			    ,{<<"Event-Name">>, <<"command">>}
			    ,{<<"Application-Name">>, <<"record">>}
			    ,{<<"Insert-At">>, [<<"head">>, <<"tail">>, <<"flush">>, <<"now">>]}
			   ]).
-define(RECORD_REQ_TYPES, [{<<"Terminators">>, fun is_list/1}]).

%% Play and Record Digits - http://corp.switchfreedom.com/mediawiki/index.php/Dialplan_Actions#Play_and_Collect_Digits
-define(PLAY_COLLECT_DIGITS_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Minimum-Digits">>, <<"Maximum-Digits">>
				 ,<<"Timeout">>, <<"Terminators">>, <<"Media-Name">>, <<"Media-Tries">>
				 ,<<"Failed-Media-Name">>, <<"Digits-Regex">>
			    ]).
-define(OPTIONAL_PLAY_COLLECT_DIGITS_REQ_HEADERS, [<<"Insert-At">>]).
-define(PLAY_COLLECT_DIGITS_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
					 ,{<<"Event-Name">>, <<"command">>}
					 ,{<<"Application-Name">>, <<"play_and_collect_digits">>}
					 ,{<<"Insert-At">>, [<<"head">>, <<"tail">>, <<"flush">>, <<"now">>]}
					]).
-define(PLAY_COLLECT_DIGITS_REQ_TYPES, []).

%% Say - http://corp.switchfreedom.com/mediawiki/index.php/Dialplan_Actions#Say
-define(SAY_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Language">>, <<"Type">>, <<"Method">>, <<"Say-Text">>]).
-define(OPTIONAL_SAY_REQ_HEADERS, [<<"Insert-At">>]).
-define(SAY_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
			 ,{<<"Event-Name">>, <<"command">>}
			 ,{<<"Application-Name">>, <<"say">>}
			 ,{<<"Type">>, [<<"number">>, <<"items">>, <<"persons">>, <<"messages">>, <<"currency">>
					    ,<<"time_measurement">>, <<"current_date">>, <<"current_time">>
					    ,<<"current_date_time">>, <<"telephone_number">>, <<"telephone_extension">>
					    ,<<"url">>, <<"ip_address">>, <<"e-mail_address">>, <<"postal_address">>
					    ,<<"account_number">>, <<"name_spelled">>, <<"name_phonetic">>, <<"short_date_time">>]}
			 ,{<<"Method">>, [<<"none">>, <<"pronounced">>, <<"iterated">>, <<"counted">>]}
			 ,{<<"Insert-At">>, [<<"head">>, <<"tail">>, <<"flush">>, <<"now">>]}
			]).
-define(SAY_REQ_TYPES, []).

%% Sleep - http://corp.switchfreedom.com/mediawiki/index.php/Dialplan_Actions#Sleep
-define(SLEEP_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Time">>]).
-define(OPTIONAL_SLEEP_REQ_HEADERS, [<<"Insert-At">>]).
-define(SLEEP_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
			   ,{<<"Event-Name">>, <<"command">>}
			   ,{<<"Application-Name">>, <<"sleep">>}
			   ,{<<"Insert-At">>, [<<"head">>, <<"tail">>, <<"flush">>, <<"now">>]}
			  ]).
-define(SLEEP_REQ_TYPES, []).


%% Conference - http://wiki.2600hz.org/display/whistle/Conferences
-define(CONFERENCE_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Conference-ID">>]).
-define(OPTIONAL_CONFERENCE_REQ_HEADERS, [<<"Insert-At">>, <<"Mute">>, <<"Deaf">>, <<"Moderator">>]).
-define(CONFERENCE_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                                ,{<<"Event-Name">>, <<"command">>}
                                ,{<<"Application-Name">>, <<"conference">>}
                                ,{<<"Insert-At">>, [<<"head">>, <<"tail">>, <<"flush">>, <<"now">>]}
                                ,{<<"Mute">>, [<<"true">>, <<"false">>]}
                                ,{<<"Deaf">>, [<<"true">>, <<"false">>]}
                                ,{<<"Moderator">>, [<<"true">>, <<"false">>]}
                               ]).
-define(CONFERENCE_REQ_TYPES, [{<<"Call-ID">>, fun is_binary/1}
                               ,{<<"Conference-ID">>, fun is_binary/1}
                              ]).

%% Conference::Members - http://wiki.2600hz.org/display/whistle/Conferences
-define(CONF_MEMBERS_REQ_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>]).
-define(OPTIONAL_CONF_MEMBERS_REQ_HEADERS, [<<"Insert-At">>]).
-define(CONF_MEMBERS_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                               ,{<<"Event-Name">>, <<"command">>}
                               ,{<<"Application-Name">>, <<"members">>}
                              ]).
-define(CONF_MEMBERS_REQ_TYPES, [{<<"Conference-ID">>, fun is_binary/1}]).

-define(CONF_MEMBERS_RESP_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>]).
-define(OPTIONAL_CONF_MEMBERS_RESP_HEADERS, [<<"Insert-At">>, <<"Members">>, <<"Error">>]).
-define(CONF_MEMBERS_RESP_VALUES, [{<<"Event-Category">>, <<"conference">>}
                               ,{<<"Event-Name">>, <<"response">>}
                               ,{<<"Application-Name">>, <<"members">>}
                              ]).
-define(CONF_MEMBERS_RESP_TYPES, [{<<"Conference-ID">>, fun is_binary/1}]).


%% Conference::Play - http://wiki.2600hz.org/display/whistle/Conferences
-define(CONF_PLAY_REQ_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Media-Name">>]).
-define(OPTIONAL_CONF_PLAY_REQ_HEADERS, [<<"Insert-At">>, <<"Member-ID">>]).
-define(CONF_PLAY_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                               ,{<<"Event-Name">>, <<"command">>}
                               ,{<<"Application-Name">>, <<"play">>}
                              ]).
-define(CONF_PLAY_REQ_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                              ,{<<"Media-Name">>, fun is_binary/1}
                              ,{<<"Member-ID">>, fun is_binary/1}
                             ]).

%% Conference:: - http://wiki.2600hz.org/display/whistle/Conferences
-define(CONF_DEAF_REQ_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Member-ID">>]).
-define(OPTIONAL_CONF_DEAF_REQ_HEADERS, [<<"Insert-At">>]).
-define(CONF_DEAF_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                               ,{<<"Event-Name">>, <<"command">>}
                               ,{<<"Application-Name">>, <<"deaf">>}
                              ]).
-define(CONF_DEAF_REQ_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                              ,{<<"Member-ID">>, fun is_binary/1}
                             ]).

%% Conference - http://wiki.2600hz.org/display/whistle/Conferences
-define(CONF_UNDEAF_REQ_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Member-ID">>]).
-define(OPTIONAL_CONF_UNDEAF_REQ_HEADERS, [<<"Insert-At">>]).
-define(CONF_UNDEAF_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                 ,{<<"Event-Name">>, <<"command">>}
                                 ,{<<"Application-Name">>, <<"undeaf">>}
                                ]).
-define(CONF_UNDEAF_REQ_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                                ,{<<"Member-ID">>, fun is_binary/1}
                               ]).

%% Conference - http://wiki.2600hz.org/display/whistle/Conferences
-define(CONF_MUTE_REQ_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Member-ID">>]).
-define(OPTIONAL_CONF_MUTE_REQ_HEADERS, [<<"Insert-At">>]).
-define(CONF_MUTE_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                               ,{<<"Event-Name">>, <<"command">>}
                               ,{<<"Application-Name">>, <<"mute">>}
                              ]).
-define(CONF_MUTE_REQ_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                              ,{<<"Member-ID">>, fun is_binary/1}
                             ]).

%% Conference - http://wiki.2600hz.org/display/whistle/Conferences
-define(CONF_UNMUTE_REQ_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Member-ID">>]).
-define(OPTIONAL_CONF_UNMUTE_REQ_HEADERS, [<<"Insert-At">>]).
-define(CONF_UNMUTE_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                 ,{<<"Event-Name">>, <<"command">>}
                                 ,{<<"Application-Name">>, <<"unmute">>}
                                ]).
-define(CONF_UNMUTE_REQ_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                                ,{<<"Member-ID">>, fun is_binary/1}
                               ]).

%% Conference - http://wiki.2600hz.org/display/whistle/Conferences
-define(CONF_KICK_REQ_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Member-ID">>]).
-define(OPTIONAL_CONF_KICK_REQ_HEADERS, [<<"Insert-At">>]).
-define(CONF_KICK_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                               ,{<<"Event-Name">>, <<"command">>}
                               ,{<<"Application-Name">>, <<"kick">>}
                              ]).
-define(CONF_KICK_REQ_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                              ,{<<"Member-ID">>, fun is_binary/1}
                             ]).

%% Conference - http://wiki.2600hz.org/display/whistle/Conferences
-define(CONF_MOVE_REQ_HEADERS, [<<"Application-Name">>, <<"Conference-From">>, <<"Conference-To">>, <<"Member-ID">>]).
-define(OPTIONAL_CONF_MOVE_REQ_HEADERS, [<<"Insert-At">>]).
-define(CONF_MOVE_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                               ,{<<"Event-Name">>, <<"command">>}
                               ,{<<"Application-Name">>, <<"move">>}
                              ]).
-define(CONF_MOVE_REQ_TYPES, [{<<"Conference-From">>, fun is_binary/1}
                               ,{<<"Conference-To">>, fun is_binary/1}
                               ,{<<"Member-ID">>, fun is_binary/1}
                              ]).

%% Conference - http://wiki.2600hz.org/display/whistle/Conferences
-define(CONF_RELATE_REQ_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Member-ID">>, <<"Correlate-ID">>]).
-define(OPTIONAL_CONF_RELATE_REQ_HEADERS, [<<"Insert-At">>, <<"Relationship">>]).
-define(CONF_RELATE_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                 ,{<<"Event-Name">>, <<"command">>}
                                 ,{<<"Application-Name">>, <<"kick">>}
                                 ,{<<"Relationship">>, [<<"deaf">>, <<"mute">>, <<"reset">>]}
                                ]).
-define(CONF_RELATE_REQ_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                                ,{<<"Member-ID">>, fun is_binary/1}
                                ,{<<"Correlate-ID">>, fun is_binary/1}
                               ]).

%% NoOp Request
-define(NOOP_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>]).
-define(OPTIONAL_NOOP_REQ_HEADERS, [<<"Msg-ID">>, <<"Insert-At">>]).
-define(NOOP_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                          ,{<<"Event-Name">>, <<"command">>}
                          ,{<<"Application-Name">>, <<"noop">>}
                          ,{<<"Insert-At">>, [<<"head">>, <<"tail">>, <<"flush">>, <<"now">>]}
                         ]).
-define(NOOP_REQ_TYPES, [{<<"Msg-ID">>, fun is_binary/1}]).

%% [{FreeSWITCH-Flage-Name, Whistle-Flag-Name}]
%% Conference-related entry flags
%% convert from FS conference flags to Whistle conference flags
-define(CONFERENCE_FLAGS, [{<<"mute">>, <<"Mute">>}
                           ,{<<"deaf">>, <<"Deaf">>}
                           ,{<<"moderator">>, <<"Moderator">>}
                           ]).
                                
%% [{FreeSWITCH-App-Name, Whistle-App-Name}]
%% Dialplan-related applications
%% convert from FS-named applications to Whistle-named Dialplan applications
-define(SUPPORTED_APPLICATIONS, [{<<"playback">>, <<"play">>}
				 ,{<<"hangup">>, <<"hangup">>}
				 ,{<<"record">>, <<"record">>}
				 ,{<<"playback">>, <<"tones">>}
				 ,{<<"park">>, <<"park">>}
				 ,{<<"set">>, <<"set">>}
				 ,{<<"say">>, <<"say">>}
				 ,{<<"sleep">>, <<"sleep">>}
				 ,{<<"bridge">>, <<"bridge">>}
				 ,{<<"signal_bridge">>, <<"bridge">>}
				 ,{<<"answer">>, <<"answer">>}
				 ,{<<"tone_detect">>, <<"tone_detect">>}
				 ,{<<"play_and_get_digits">>, <<"play_and_collect_digits">>}
				 ,{<<"respond">>, <<"respond">>}
				 ,{<<"conference">>, <<"conference">>}
				 ,{<<"noop">>, <<"noop">>}
				]).

-define(FS_EVENTS, [<<"CHANNEL_EXECUTE">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"CHANNEL_HANGUP">>
			,<<"CHANNEL_HANGUP_COMPLETE">>, <<"CHANNEL_BRIDGE">>, <<"CHANNEL_UNBRIDGE">>
			,<<"DETECTED_TONE">>, <<"DTMF">>, <<"CALL_UPDATE">>, <<"RECORD_STOP">>
			,<<"CHANNEL_EXECUTE_ERROR">> %% custom error
		   ]).

%% List of tuples: {dialplan application-name, validation_fun}
-define(DIALPLAN_APPLICATIONS, [
				{<<"queue">>, fun whistle_api:queue_req_v/1}
				,{<<"bridge">>, fun whistle_api:bridge_req_v/1}
				,{<<"answer">>, fun whistle_api:answer_req_v/1}
				,{<<"play">>, fun whistle_api:play_req_v/1}
				,{<<"record">>, fun whistle_api:record_req_v/1}
				,{<<"store">>, fun whistle_api:store_req_v/1}
				,{<<"play_and_collect_digits">>, fun whistle_api:play_collect_digits_req_v/1}
				,{<<"tones">>, fun whistle_api:tones_req_v/1}
				,{<<"tone_detect">>, fun whistle_api:tone_detect_req_v/1}
				,{<<"park">>, fun whistle_api:park_req_v/1}
				,{<<"call_pickup">>, fun whistle_api:call_pickup_req_v/1}
				,{<<"hangup">>, fun whistle_api:hangup_req_v/1}
				,{<<"say">>, fun whistle_api:say_req_v/1}
				,{<<"sleep">>, fun whistle_api:sleep_req_v/1}
				,{<<"set">>, fun whistle_api:set_req_v/1}
				,{<<"conference">>, fun whistle_api:conference_req_v/1}
				,{<<"noop">>, fun whistle_api:noop_req_v/1}
			       ]).
