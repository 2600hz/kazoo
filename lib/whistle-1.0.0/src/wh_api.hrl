-ifndef(WHISTLE_TYPES_INCLUDED).
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_types.hrl").
-endif.

%% We pass Application custom channel variables with our own prefix
%% When an event occurs, we include all prefixed vars in the API message
-define(CHANNEL_VAR_PREFIX, "ecallmgr_").

%% For dialplan messages, an optional insert-at tuple is common across all requests
-define(INSERT_AT_TUPLE, {<<"Insert-At">>, [<<"head">>, <<"tail">>, <<"flush">>, <<"now">>]}).

%% For dialplan messages, what does the Invite-Format param accept as values?
-define(INVITE_FORMAT_TUPLE, {<<"Invite-Format">>, [<<"username">>, <<"e164">>, <<"npan">>, <<"1npan">>, <<"route">>]}).

%%% *_HEADERS defines a list of Keys that must exist in every message of type *
%%% (substitute AUTHN_REQ, AUTHN_RESP, etc, for *) to be considered valid.
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

%% Default Headers
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

%% <<<<<<< HEAD
%% =======
%% %% Authentication Requests
%% -define(AUTHN_REQ_HEADERS, [<<"Msg-ID">>, <<"To">>, <<"From">>, <<"Orig-IP">>
%% 			       , <<"Auth-User">>, <<"Auth-Realm">>]).
%% -define(OPTIONAL_AUTHN_REQ_HEADERS, [<<"Method">>]).
%% -define(AUTHN_REQ_VALUES, [{<<"Event-Category">>, <<"directory">>}
%% 			  ,{<<"Event-Name">>, <<"authn_req">>}
%% 			 ]).
%% -define(AUTHN_REQ_TYPES, [{<<"Msg-ID">>, fun is_binary/1}
%% 			 ,{<<"To">>, fun is_binary/1}
%% 			 ,{<<"From">>, fun is_binary/1}
%% 			 ,{<<"Orig-IP">>, fun is_binary/1}
%% 			 ,{<<"Auth-User">>, fun is_binary/1}
%% 			 ,{<<"Auth-Realm">>, fun is_binary/1}
%% 			]).

%% %% Configuration Document Update
%% -define(CONF_DOC_UPDATE_HEADERS, [<<"ID">>, <<"Rev">>, <<"Doc">>]).
%% -define(OPTIONAL_CONF_DOC_UPDATE_HEADERS, [<<"Account-DB">>, <<"Account-ID">>
%%                                                ,<<"Date-Modified">>, <<"Date-Created">>
%%                                                ,<<"Type">>, <<"Version">>]).
%% -define(CONF_DOC_UPDATE_VALUES, [{<<"Event-Category">>, <<"configuration">>}
%%                                  ,{<<"Event-Name">>, [<<"doc_edited">>, <<"doc_created">>, <<"doc_deleted">>]}]).
%% -define(CONF_DOC_UPDATE_TYPES, [{<<"ID">>, fun is_binary/1}
%%                                 ,{<<"Rev">>, fun is_binary/1}]).

%% %% Authentication Responses
%% -define(AUTHN_RESP_HEADERS, [<<"Msg-ID">>, <<"Auth-Method">>, <<"Auth-Password">>]).
%% -define(OPTIONAL_AUTHN_RESP_HEADERS, [<<"Tenant-ID">>, <<"Access-Group">>, <<"Custom-Channel-Vars">>]).
%% -define(AUTHN_RESP_VALUES, [{<<"Event-Category">>, <<"directory">>}
%% 			   ,{<<"Event-Name">>, <<"authn_resp">>}
%% 			   ,{<<"Auth-Method">>, [<<"password">>, <<"ip">>, <<"a1-hash">>, <<"error">>]}
%% 			 ]).
%% -define(AUTHN_RESP_TYPES, [{<<"Msg-ID">>, fun is_binary/1}
%% 			  ,{<<"Auth-Password">>, fun is_binary/1}
%% 			  ,{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}
%% 			  ,{<<"Access-Group">>, fun is_binary/1}
%% 			  ,{<<"Tenant-ID">>, fun is_binary/1}
%% 			 ]).

%% %% Registration Success
%% -define(REG_SUCCESS_HEADERS, [<<"Event-Timestamp">>, <<"From-User">>, <<"From-Host">>, <<"Contact">>, <<"RPid">>
%% 				 ,<<"Expires">>, <<"To-User">>, <<"To-Host">>, <<"Network-IP">>, <<"Network-Port">>
%% 				 , <<"Username">>, <<"Realm">>
%% 			    ]).
%% -define(OPTIONAL_REG_SUCCESS_HEADERS, [<<"Status">>, <<"User-Agent">>, <<"Call-ID">>, <<"Profile-Name">>, <<"Presence-Hosts">>
%% 					   ,<<"FreeSWITCH-Hostname">>
%% 				      ]).
%% -define(REG_SUCCESS_VALUES, [{<<"Event-Category">>, <<"directory">>}
%% 			    ,{<<"Event-Name">>, <<"reg_success">>}
%% 			   ]).
%% -define(REG_SUCCESS_TYPES, []).

%% %% Query Registrations
%% -define(REG_QUERY_HEADERS, [<<"Username">>, <<"Realm">>, <<"Fields">>]).
%% -define(OPTIONAL_REG_QUERY_HEADERS, [<<"Call-ID">>]).
%% -define(REG_QUERY_VALUES, [{<<"Event-Category">>, <<"directory">>}
%% 			   ,{<<"Event-Name">>, <<"reg_query">>}
%% 			  ]).
%% -define(REG_QUERY_TYPES, [{<<"Call-ID">>, fun is_binary/1}
%%                           ,{<<"Fields">>, fun(Fs) when is_list(Fs) ->
%%                                                   Allowed = ?OPTIONAL_REG_SUCCESS_HEADERS ++ ?REG_SUCCESS_HEADERS,
%%                                                   lists:foldl(fun(F, true) -> lists:member(F, Allowed);
%%                                                                  (_, false) -> false
%%                                                               end, true, Fs);
%%                                              (_) -> false
%%                                           end}
%% 			 ]).

%% %% Registration Query Response
%% -define(REG_QUERY_RESP_HEADERS, [<<"Fields">>]).
%% -define(OPTIONAL_REG_QUERY_RESP_HEADERS, []).
%% -define(REG_QUERY_RESP_VALUES, [{<<"Event-Category">>, <<"directory">>}
%% 				,{<<"Event-Name">>, <<"reg_query_resp">>}
%% 			       ]).
%% -define(REG_QUERY_RESP_TYPES, []).

%% %% Authorization Requests
%% -define(AUTHZ_REQ_HEADERS, [<<"Msg-ID">>, <<"To">>, <<"From">>, <<"Call-ID">>
%% 				,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
%% 			   ]).
%% -define(OPTIONAL_AUTHZ_REQ_HEADERS, [<<"Custom-Channel-Vars">>, <<"Request">>]).
%% -define(AUTHZ_REQ_VALUES, [{<<"Event-Category">>, <<"dialplan">>}
%% 			   ,{<<"Event-Name">>, <<"authz_req">>}
%% 			  ]).
%% -define(AUTHZ_REQ_TYPES, [{<<"Msg-ID">>, fun is_binary/1}
%% 			  ,{<<"To">>, fun is_binary/1}
%% 			  ,{<<"From">>, fun is_binary/1}
%% 			  ,{<<"Call-ID">>, fun is_binary/1}
%% 			  ,{<<"Caller-ID-Name">>, fun is_binary/1}
%% 			  ,{<<"Caller-ID-Number">>, fun is_binary/1}
%% 			  ,{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}
%% 			 ]).

%% %% Authorization Responses
%% -define(AUTHZ_RESP_HEADERS, [<<"Msg-ID">>, <<"Call-ID">>, <<"Is-Authorized">>]).
%% -define(OPTIONAL_AUTHZ_RESP_HEADERS, [<<"Custom-Channel-Vars">>]).
%% -define(AUTHZ_RESP_VALUES, [{<<"Event-Category">>, <<"dialplan">>}
%% 			    ,{<<"Event-Name">>, <<"authz_resp">>}
%% 			    ,{<<"Is-Authorized">>, [<<"true">>, <<"false">>]}
%% 			   ]).
%% -define(AUTHZ_RESP_TYPES, [{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}]).

%% %% Route Requests
%% -define(ROUTE_REQ_HEADERS, [<<"Msg-ID">>, <<"To">>, <<"From">>, <<"Request">>, <<"Call-ID">>
%% 				,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
%% 			   ]).
%% -define(OPTIONAL_ROUTE_REQ_HEADERS, [<<"Geo-Location">>, <<"Orig-IP">>, <<"Max-Call-Length">>, <<"Media">>
%% 					 ,<<"Transcode">>, <<"Codecs">>, <<"Custom-Channel-Vars">>
%% 					 ,<<"Resource-Type">>, <<"Cost-Parameters">>
%% 				    ]).
%% -define(ROUTE_REQ_VALUES, [{<<"Event-Category">>, <<"dialplan">>}
%% 			   ,{<<"Event-Name">>, <<"route_req">>}
%% 			   ,{<<"Resource-Type">>, [<<"MMS">>, <<"SMS">>, <<"audio">>, <<"video">>, <<"chat">>]}
%% 			   ,{<<"Media">>, [<<"process">>, <<"proxy">>, <<"bypass">>]}
%% 			  ]).
%% -define(ROUTE_REQ_TYPES, [{<<"Msg-ID">>, fun is_binary/1}
%% 			  ,{<<"To">>, fun is_binary/1}
%% 			  ,{<<"From">>, fun is_binary/1}
%% 			  ,{<<"Request">>, fun is_binary/1}
%% 			  ,{<<"Call-ID">>, fun is_binary/1}
%% 			  ,{<<"Event-Queue">>, fun is_binary/1}
%% 			  ,{<<"Caller-ID-Name">>, fun is_binary/1}
%% 			  ,{<<"Caller-ID-Number">>, fun is_binary/1}
%% 			  ,{<<"Cost-Parameters">>, fun({struct, L}) when is_list(L) ->
%% 							   lists:all(fun({K, _V}) ->
%% 									     lists:member(K, ?ROUTE_REQ_COST_PARAMS)
%% 								     end, L);
%% 						      (_) -> false
%% 						   end}
%% 			  ,{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}
%% 			 ]).
%% -define(ROUTE_REQ_COST_PARAMS, [<<"Min-Increment-Cost">>, <<"Max-Incremental-Cost">>
%% 				    ,<<"Min-Setup-Cost">>, <<"Max-Setup-Cost">>
%% 			       ]).

%% %% Route Responses
%% -define(ROUTE_RESP_ROUTE_HEADERS, [<<"Invite-Format">>, <<"Weight-Cost">>, <<"Weight-Location">>]).
%% -define(OPTIONAL_ROUTE_RESP_ROUTE_HEADERS, [ <<"Route">>, <<"To-User">>, <<"To-Realm">>, <<"To-DID">>
%% 						 ,<<"Proxy-Via">>, <<"Media">>, <<"Auth-User">>
%% 						 ,<<"Auth-Password">>, <<"Codecs">>, <<"Progress-Timeout">>
%% 						 ,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>, <<"Caller-ID-Type">>
%% 						 ,<<"Rate">>, <<"Rate-Increment">>, <<"Rate-Minimum">>, <<"Surcharge">>
%% 						 ,<<"SIP-Headers">>, <<"Custom-Channel-Vars">>
%% 					   ]).
%% -define(ROUTE_RESP_ROUTE_VALUES, [{<<"Media">>, [<<"process">>, <<"bypass">>, <<"auto">>]}
%% 				  ,{<<"Caller-ID-Type">>, [<<"from">>, <<"rpid">>, <<"pid">>]}
%% 				  ,?INVITE_FORMAT_TUPLE
%% 				 ]).
%% -define(ROUTE_RESP_ROUTE_TYPES, [ {<<"Codecs">>, fun is_list/1}
%% 				  ,{<<"Route">>, fun is_binary/1}
%% 				  ,{<<"To-User">>, fun is_binary/1}
%% 				  ,{<<"To-Realm">>, fun is_binary/1}
%% 				  ,{<<"SIP-Headers">>, ?IS_JSON_OBJECT}
%%                                   ,{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}
%% 				]).

%% %% Route Responses
%% -define(ROUTE_RESP_HEADERS, [<<"Msg-ID">>, <<"Routes">>, <<"Method">>]).
%% -define(OPTIONAL_ROUTE_RESP_HEADERS, [<<"Custom-Channel-Vars">>,
%%                                       <<"Route-Error-Code">>, <<"Route-Error-Message">>]).
%% -define(ROUTE_RESP_VALUES, [{<<"Event-Category">>, <<"dialplan">>}
%% 			    ,{<<"Event-Name">>, <<"route_resp">>}
%% 			    ,{<<"Method">>, [<<"bridge">>, <<"park">>, <<"error">>]}
%% 			   ]).
%% -define(ROUTE_RESP_TYPES, [{<<"Route-Error-Code">>, fun is_binary/1}
%% 			   ,{<<"Route-Error-Message">>, fun is_binary/1}
%% 			   ,{<<"Routes">>, fun(L) when is_list(L) -> true;
%% 					      (_) -> false
%% 					   end}
%%                            ,{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}
%% 			  ]).

%% %% Route Winner
%% -define(ROUTE_WIN_HEADERS, [<<"Call-ID">>, <<"Control-Queue">>]).
%% -define(OPTIONAL_ROUTE_WIN_HEADERS, [<<"Custom-Channel-Vars">>]).
%% -define(ROUTE_WIN_VALUES, [{<<"Event-Name">>, <<"route_win">>}]).
%% -define(ROUTE_WIN_TYPES, [{<<"Call-ID">>, fun is_binary/1}
%% 			  ,{<<"Control-Queue">>, fun is_binary/1}
%% 			  ,{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}
%% 			 ]).

%% %% Offnet Resource Request
%% -define(OFFNET_RESOURCE_REQ_HEADERS, [<<"Call-ID">>, <<"Resource-Type">>, <<"To-DID">>
%%                                       ,<<"Account-ID">>, <<"Control-Queue">>, <<"Application-Name">>
%%                                      ]).
%% -define(OPTIONAL_OFFNET_RESOURCE_REQ_HEADERS, [<<"Timeout">>, <<"Ignore-Early-Media">>, <<"Flags">>, <<"Media">>
%%                                                ,<<"Outgoing-Caller-ID-Name">>, <<"Outgoing-Caller-ID-Number">>
%%                                                ,<<"Emergency-Caller-ID-Name">>, <<"Emergency-Caller-ID-Number">>
%%                                                ,<<"Ringback">>, <<"SIP-Headers">>, <<"Custom-Channel-Vars">>
%%                                               ]).
%% -define(OFFNET_RESOURCE_REQ_VALUES, [{<<"Event-Category">>, <<"resource">>}
%%                                      ,{<<"Event-Name">>, <<"offnet_req">>}
%%                                      ,{<<"Resource-Type">>, [<<"audio">>, <<"video">>]}
%%                                      ,{<<"Application-Name">>, [<<"bridge">>]}
%%                                      ,{<<"Media">>, [<<"process">>, <<"bypass">>, <<"auto">>]}
%%                                     ]).
%% -define(OFFNET_RESOURCE_REQ_TYPES, [{<<"Call-ID">>, fun is_binary/1}
%%                                     ,{<<"Account-ID">>, fun is_binary/1}
%%                                     ,{<<"Control-Queue">>, fun is_binary/1}
%%                                     ,{<<"To-DID">>, fun is_binary/1}
%%                                     ,{<<"SIP-Headers">>, ?IS_JSON_OBJECT}
%%                                     ,{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}
%%                                     ,{<<"Flags">>, fun is_list/1}
%%                                    ]).

%% %% Resource Request
%% -define(RESOURCE_REQ_HEADERS, [<<"Msg-ID">>, <<"Resource-Type">>, <<"Invite-Format">>]).
%% -define(OPTIONAL_RESOURCE_REQ_HEADERS, [<<"Resource-Minimum">>, <<"Resource-Maximum">>, <<"Geo-Location">>
%%                                         ,<<"Route">>, <<"To-User">>, <<"To-Realm">>, <<"To-DID">>
%% 					,<<"Application-Name">>, <<"Application-Data">>, <<"SIP-Headers">>
%%                                         ,<<"Custom-Channel-Vars">>
%% 				       ]).
%% -define(RESOURCE_REQ_VALUES, [{<<"Event-Category">>, <<"resource">>}
%% 			      ,{<<"Event-Name">>, <<"originate_req">>}
%% 			      ,{<<"Resource-Type">>, [<<"audio">>, <<"video">>]}
%% 			      ,{<<"Application-Name">>, [<<"park">>, <<"bridge">>, <<"transfer">>]}
%% 			      ,?INVITE_FORMAT_TUPLE
%% 			     ]).
%% -define(RESOURCE_REQ_TYPES, [{<<"Invite-Format">>, fun is_binary/1}
%% 			     ,{<<"Route">>, fun is_binary/1}
%% 			     ,{<<"To-User">>, fun is_binary/1}
%% 			     ,{<<"To-Realm">>, fun is_binary/1}
%% 			     ,{<<"SIP-Headers">>, ?IS_JSON_OBJECT}
%%                              ,{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}
%% 			    ]).

%% %% Resource Response
%% -define(RESOURCE_RESP_HEADERS, [<<"Msg-ID">>, <<"Call-ID">>, <<"Control-Queue">>]).
%% -define(OPTIONAL_RESOURCE_RESP_HEADERS, [<<"To">>, <<"Timestamp">>, <<"Channel-Call-State">>
%%                                              ,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
%%                                              ,<<"Custom-Channel-Vars">>
%%                                         ]).
%% -define(RESOURCE_RESP_VALUES, [{<<"Event-Category">>, <<"resource">>}
%% 			       ,{<<"Event-Name">>, [<<"offnet_resp">>, <<"originate_resp">>]}
%% 			      ]).
%% -define(RESOURCE_RESP_TYPES, [{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}]).

%% %% Resource Error
%% -define(RESOURCE_ERROR_HEADERS, [<<"Msg-ID">>]).
%% -define(OPTIONAL_RESOURCE_ERROR_HEADERS, [<<"Failed-Attempts">>, <<"Failed-Route">>, <<"Failure-Message">>
%%                                               ,<<"Failure-Code">>, <<"Hangup-Cause">>, <<"Hangup-Code">>]).
%% -define(RESOURCE_ERROR_VALUES, [{<<"Event-Category">>, <<"resource">>}
%%                                 ,{<<"Event-Name">>, [<<"originate_error">>, <<"resource_error">>]}
%%                                ]).
%% -define(RESOURCE_ERROR_TYPES, []).

%% %% Call Events
%% -define(CALL_EVENT_HEADERS, [<<"Timestamp">>, <<"Call-ID">>, <<"Channel-Call-State">>]).
%% -define(OPTIONAL_CALL_EVENT_HEADERS, [<<"Application-Name">>, <<"Application-Response">>, <<"Custom-Channel-Vars">>
%% 					  ,<<"Msg-ID">>, <<"Channel-State">>, <<"Call-Direction">>
%% 					  ,<<"Other-Leg-Direction">>, <<"Other-Leg-Caller-ID-Name">>, <<"Other-Leg-Caller-ID-Number">> %% BRIDGE
%% 					  ,<<"Other-Leg-Destination-Number">>,<<"Other-Leg-Unique-ID">> %% BRIDGE
%% 					  ,<<"Detected-Tone">>, <<"DTMF-Duration">>, <<"DTMF-Digit">> %% DTMF and Tones
%%                                           ,<<"Terminator">>, <<"Hangup-Cause">>, <<"Hangup-Code">> %% Hangup
%% 				     ]).
%% -define(CALL_EVENT_VALUES, [{<<"Event-Category">>, <<"call_event">>}]).
%% -define(CALL_EVENT_TYPES, [{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}]).

%% %% Call Status Request
%% -define(CALL_STATUS_REQ_HEADERS, [<<"Call-ID">>]).
%% -define(OPTIONAL_CALL_STATUS_REQ_HEADERS, []).
%% -define(CALL_STATUS_REQ_VALUES, [{<<"Event-Category">>, <<"call_event">>}
%% 			     ,{<<"Event-Name">>, <<"status_req">>}
%% 			    ]).
%% -define(CALL_STATUS_REQ_TYPES, []).

%% %% Call Status Response
%% -define(CALL_STATUS_RESP_HEADERS, [<<"Call-ID">>, <<"Status">>]).
%% -define(OPTIONAL_CALL_STATUS_RESP_HEADERS, [<<"Custom-Channel-Vars">>, <<"Error-Msg">>, <<"Node">>]).
%% -define(CALL_STATUS_RESP_VALUES, [{<<"Event-Category">>, <<"call_event">>}
%% 				  ,{<<"Event-Name">>, <<"status_resp">>}
%% 				  ,{<<"Status">>, [<<"active">>, <<"tmpdown">>]}
%% 				 ]).
%% -define(CALL_STATUS_RESP_TYPES, [{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}]).

%% %% Call CDR
%% -define(CALL_CDR_HEADERS, [ <<"Call-ID">>]).
%% -define(OPTIONAL_CALL_CDR_HEADERS, [<<"Hangup-Cause">>, <<"Handling-Server-Name">>, <<"Custom-Channel-Vars">>
%%                                         ,<<"Remote-SDP">>, <<"Local-SDP">>, <<"Caller-ID-Name">>
%% 					,<<"Caller-ID-Number">>, <<"Callee-ID-Name">>, <<"Callee-ID-Number">>
%% 					,<<"User-Agent">>, <<"Caller-ID-Type">>, <<"Other-Leg-Call-ID">>
%%                                         ,<<"Timestamp">>
%%                                         ,<<"Call-Direction">>, <<"To-Uri">>, <<"From-Uri">>
%%                                         ,<<"Duration-Seconds">>, <<"Billing-Seconds">>, <<"Ringing-Seconds">>
%%                                         ,<<"Digits-Dialed">>
%% 				   ]).
%% -define(CALL_CDR_VALUES, [{<<"Event-Category">>, <<"call_detail">>}
%% 			  ,{<<"Event-Name">>, <<"cdr">>}
%% 			  ,{<<"Call-Direction">>, [<<"inbound">>, <<"outbound">>]}
%% 			  ,{<<"Caller-ID-Type">>, [<<"pid">>, <<"rpid">>, <<"from">>]}
%% 			 ]).
%% -define(CALL_CDR_TYPES, [{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}]).

%% >>>>>>> master
%% Error Responses
-define(ERROR_RESP_HEADERS, [<<"Msg-ID">>, <<"Error-Message">>]).
-define(OPTIONAL_ERROR_RESP_HEADERS, []).
-define(ERROR_RESP_VALUES, [{<<"Event-Category">>, <<"error">>}]).
-define(ERROR_RESP_TYPES, []).

%%% Dialplan Commands and related definitions

%% Conference
-define(CONFERENCE_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Conference-ID">>]).
-define(OPTIONAL_CONFERENCE_REQ_HEADERS, [<<"Insert-At">>, <<"Mute">>, <<"Deaf">>, <<"Moderator">>]).
-define(CONFERENCE_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                                ,{<<"Event-Name">>, <<"command">>}
                                ,{<<"Application-Name">>, <<"conference">>}
                                ,?INSERT_AT_TUPLE
                                ,{<<"Mute">>, [<<"true">>, <<"false">>]}
                                ,{<<"Deaf">>, [<<"true">>, <<"false">>]}
                                ,{<<"Moderator">>, [<<"true">>, <<"false">>]}
                               ]).
-define(CONFERENCE_REQ_TYPES, [{<<"Call-ID">>, fun is_binary/1}
                               ,{<<"Conference-ID">>, fun is_binary/1}
                              ]).

%% Conference Discovery
-define(CONF_DISCOVERY_REQ_HEADERS, [<<"Account-ID">>, <<"Call-ID">>, <<"Control-Queue">>]).
-define(OPTIONAL_CONF_DISCOVERY_REQ_HEADERS, [<<"Conference-ID">>, <<"Moderator">>]).
-define(CONF_DISCOVERY_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                    ,{<<"Event-Name">>, <<"discovery">>}
                                    ,{<<"Moderator">>, [<<"true">>, <<"false">>]}
                                   ]).
-define(CONF_DISCOVERY_REQ_TYPES, [{<<"Call-ID">>, fun is_binary/1}
                                   ,{<<"Control-Queue">>, fun is_binary/1}
                                   ,{<<"Conference-ID">>, fun is_binary/1}
                                  ]).

%% Conference Participants
-define(CONF_PARTICIPANTS_REQ_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>]).
-define(OPTIONAL_CONF_PARTICIPANTS_REQ_HEADERS, [<<"Insert-At">>]).
-define(CONF_PARTICIPANTS_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                       ,{<<"Event-Name">>, <<"command">>}
                                       ,{<<"Application-Name">>, <<"participants">>}
                                      ]).
-define(CONF_PARTICIPANTS_REQ_TYPES, [{<<"Conference-ID">>, fun is_binary/1}]).

-define(CONF_PARTICIPANTS_RESP_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>]).
-define(OPTIONAL_CONF_PARTICIPANTS_RESP_HEADERS, [<<"Insert-At">>, <<"Participants">>, <<"Error">>]).
-define(CONF_PARTICIPANTS_RESP_VALUES, [{<<"Event-Category">>, <<"conference">>}
                               ,{<<"Event-Name">>, <<"participants">>}
                               ,{<<"Application-Name">>, <<"participants">>}
                              ]).
-define(CONF_PARTICIPANTS_RESP_TYPES, [{<<"Conference-ID">>, fun is_binary/1}]).

%% Conference Play
-define(CONF_PLAY_REQ_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Media-Name">>]).
-define(OPTIONAL_CONF_PLAY_REQ_HEADERS, [<<"Insert-At">>, <<"Participant-ID">>]).
-define(CONF_PLAY_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                               ,{<<"Event-Name">>, <<"command">>}
                               ,{<<"Application-Name">>, <<"play">>}
                              ]).
-define(CONF_PLAY_REQ_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                              ,{<<"Media-Name">>, fun is_binary/1}
                              ,{<<"Participant-ID">>, fun is_binary/1}
                             ]).

%% Conference Deaf Participant
-define(CONF_DEAF_REQ_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Participant-ID">>]).
-define(OPTIONAL_CONF_DEAF_REQ_HEADERS, [<<"Insert-At">>]).
-define(CONF_DEAF_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                               ,{<<"Event-Name">>, <<"command">>}
                               ,{<<"Application-Name">>, <<"deaf">>}
                              ]).
-define(CONF_DEAF_REQ_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                              ,{<<"Participant-ID">>, fun is_binary/1}
                             ]).

%% Conference Undeaf Participant
-define(CONF_UNDEAF_REQ_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Participant-ID">>]).
-define(OPTIONAL_CONF_UNDEAF_REQ_HEADERS, [<<"Insert-At">>]).
-define(CONF_UNDEAF_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                 ,{<<"Event-Name">>, <<"command">>}
                                 ,{<<"Application-Name">>, <<"undeaf">>}
                                ]).
-define(CONF_UNDEAF_REQ_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                                ,{<<"Participant-ID">>, fun is_binary/1}
                               ]).

%% Conference Mute Participant
-define(CONF_MUTE_REQ_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Participant-ID">>]).
-define(OPTIONAL_CONF_MUTE_REQ_HEADERS, [<<"Insert-At">>]).
-define(CONF_MUTE_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                               ,{<<"Event-Name">>, <<"command">>}
                               ,{<<"Application-Name">>, <<"mute">>}
                              ]).
-define(CONF_MUTE_REQ_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                              ,{<<"Participant-ID">>, fun is_binary/1}
                             ]).

%% Conference Unmute Participant
-define(CONF_UNMUTE_REQ_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Participant-ID">>]).
-define(OPTIONAL_CONF_UNMUTE_REQ_HEADERS, [<<"Insert-At">>]).
-define(CONF_UNMUTE_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                 ,{<<"Event-Name">>, <<"command">>}
                                 ,{<<"Application-Name">>, <<"unmute">>}
                                ]).
-define(CONF_UNMUTE_REQ_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                                ,{<<"Participant-ID">>, fun is_binary/1}
                               ]).

%% Conference Kick Participant
-define(CONF_KICK_REQ_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Participant-ID">>]).
-define(OPTIONAL_CONF_KICK_REQ_HEADERS, [<<"Insert-At">>]).
-define(CONF_KICK_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                               ,{<<"Event-Name">>, <<"command">>}
                               ,{<<"Application-Name">>, <<"kick">>}
                              ]).
-define(CONF_KICK_REQ_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                              ,{<<"Participant-ID">>, fun is_binary/1}
                             ]).

%% Conference Move Participant
-define(CONF_MOVE_REQ_HEADERS, [<<"Application-Name">>, <<"Conference-From">>, <<"Conference-To">>, <<"Participant-ID">>]).
-define(OPTIONAL_CONF_MOVE_REQ_HEADERS, [<<"Insert-At">>]).
-define(CONF_MOVE_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                               ,{<<"Event-Name">>, <<"command">>}
                               ,{<<"Application-Name">>, <<"move">>}
                              ]).
-define(CONF_MOVE_REQ_TYPES, [{<<"Conference-From">>, fun is_binary/1}
                               ,{<<"Conference-To">>, fun is_binary/1}
                               ,{<<"Participant-ID">>, fun is_binary/1}
                              ]).

%% Conference Relate Participant
-define(CONF_RELATE_REQ_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Participant-ID">>, <<"Correlate-ID">>]).
-define(OPTIONAL_CONF_RELATE_REQ_HEADERS, [<<"Insert-At">>, <<"Relationship">>]).
-define(CONF_RELATE_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                 ,{<<"Event-Name">>, <<"command">>}
                                 ,{<<"Application-Name">>, <<"kick">>}
                                 ,{<<"Relationship">>, [<<"deaf">>, <<"mute">>, <<"reset">>]}
                                ]).
-define(CONF_RELATE_REQ_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                                ,{<<"Participant-ID">>, fun is_binary/1}
                                ,{<<"Correlate-ID">>, fun is_binary/1}
                               ]).

%% The AMQP passthrough of FS commands - whitelist commands allowed (exluding any prefixed by uuid_ which are auto-allowed)
-define(FS_COMMAND_WHITELIST, [<<"set">>, <<"hangup">>, <<"bridge">>]).

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
				 ,{<<"export">>, <<"set">>}
				 ,{<<"say">>, <<"say">>}
				 ,{<<"sleep">>, <<"sleep">>}
				 ,{<<"respond">>, <<"respond">>}
				 ,{<<"bridge">>, <<"bridge">>}
				 ,{<<"signal_bridge">>, <<"bridge">>}
				 ,{<<"answer">>, <<"answer">>}
				 ,{<<"pre_answer">>, <<"progress">>}
				 ,{<<"tone_detect">>, <<"tone_detect">>}
				 ,{<<"play_and_get_digits">>, <<"play_and_collect_digits">>}
				 ,{<<"respond">>, <<"respond">>}
				 ,{<<"redirect">>, <<"redirect">>}
				 ,{<<"conference">>, <<"conference">>}
				 ,{<<"noop">>, <<"noop">>}
				]).

-define(FS_EVENTS, [<<"CHANNEL_EXECUTE">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"CHANNEL_HANGUP">>
			,<<"CHANNEL_HANGUP_COMPLETE">>, <<"CHANNEL_BRIDGE">>, <<"CHANNEL_UNBRIDGE">>
			,<<"DETECTED_TONE">>, <<"DTMF">>, <<"CALL_UPDATE">>, <<"RECORD_STOP">>, <<"CHANNEL_CREATE">>
			,<<"CUSTOM">>, <<"CHANNEL_DESTROY">>, <<"CHANNEL_EXECUTE_ERROR">>, <<"CHANNEL_PROGRESS_MEDIA">> %% custom error
		   ]).

-define(FS_DEFAULT_HDRS, [<<"Event-Name">>, <<"Core-UUID">>, <<"FreeSWITCH-Hostname">>, <<"FreeSWITCH-Switchname">>
                              ,<<"FreeSWITCH-IPv4">>, <<"FreeSWITCH-IPv6">>, <<"Event-Date-Local">>
                              ,<<"Event-Date-GMT">>, <<"Event-Date-Timestamp">>, <<"Event-Calling-File">>
                              ,<<"Event-Calling-Function">>, <<"Event-Calling-Line-Number">>]).

%% List of tuples: {dialplan application-name, validation_fun}
-define(DIALPLAN_APPLICATIONS, [
				{<<"queue">>, fun wapi_dialplan:queue_v/1}
				,{<<"answer">>, fun wapi_dialplan:answer_v/1}
				,{<<"play">>, fun wapi_dialplan:play_v/1}
				,{<<"record">>, fun wapi_dialplan:record_v/1}
				,{<<"store">>, fun wapi_dialplan:store_v/1}
				,{<<"play_and_collect_digits">>, fun wapi_dialplan:play_and_collect_digits_v/1}
				,{<<"tones">>, fun wapi_dialplan:tones_v/1}
				,{<<"tone_detect">>, fun wapi_dialplan:tone_detect_v/1}
				,{<<"park">>, fun wapi_dialplan:park_v/1}
				,{<<"call_pickup">>, fun wapi_dialplan:call_pickup_v/1}
				,{<<"hangup">>, fun wapi_dialplan:hangup_v/1}
				,{<<"say">>, fun wapi_dialplan:say_v/1}
				,{<<"sleep">>, fun wapi_dialplan:sleep_v/1}
				,{<<"respond">>, fun wapi_dialplan:respond_v/1}
				,{<<"redirect">>, fun wapi_dialplan:redirect_v/1}
				,{<<"progress">>, fun wapi_dialplan:progress_v/1}
				,{<<"set">>, fun wapi_dialplan:set_v/1}
				,{<<"conference">>, fun wapi_dialplan:conference_v/1}
				,{<<"noop">>, fun wapi_dialplan:noop_v/1}
			       ]).

-define(FS_CHANNEL_STATES, [{<<"CS_NEW">>, <<"new">>}
                            ,{<<"CS_INIT">>, <<"initialize">>}
                            ,{<<"CS_ROUTING">>, <<"routing">>}
                            ,{<<"CS_SOFT_EXECUTE">>, <<"soft_execute">>}
                            ,{<<"CS_EXECUTE">>, <<"execute">>}
                            ,{<<"CS_EXCHANGE_MEDIA">>, <<"exchange_media">>}
                            ,{<<"CS_PARK">>, <<"park">>}
                            ,{<<"CS_CONSUME_MEDIA">>, <<"consume_media">>}
                            ,{<<"CS_HIBERNATE">>, <<"hibernate">>}
                            ,{<<"CS_RESET">>, <<"reset">>}
                            ,{<<"CS_HANGUP">>, <<"hangup">>}
                            ,{<<"CS_REPORTING">>, <<"reporting">>}
                            ,{<<"CS_DESTROY">>, <<"destroy">>}]).
