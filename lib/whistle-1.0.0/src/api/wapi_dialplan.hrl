-include("../wh_api.hrl").

-define(BRIDGE_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Endpoints">>]).
-define(OPTIONAL_BRIDGE_REQ_HEADERS, [<<"Timeout">>, <<"Continue-On-Fail">>, <<"Ignore-Early-Media">>
					  ,<<"Outgoing-Caller-ID-Name">>, <<"Outgoing-Caller-ID-Number">>
					  ,<<"Outgoing-Callee-ID-Name">>, <<"Outgoing-Callee-ID-Number">>
					  ,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
					  ,<<"Callee-ID-Name">>, <<"Callee-ID-Number">>
					  ,<<"Ringback">>, <<"Dial-Endpoint-Method">>, <<"Insert-At">>
					  ,<<"Media">>, <<"SIP-Headers">>, <<"Custom-Channel-Vars">>
				     ]).
-define(BRIDGE_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
			    ,{<<"Event-Name">>, <<"command">>}
			    ,{<<"Application-Name">>, <<"bridge">>}
			    ,{<<"Dial-Endpoint-Method">>, [<<"single">>, <<"simultaneous">>]}
                            ,{<<"Media">>, [<<"process">>, <<"bypass">>, <<"auto">>]}
			    ,{<<"Continue-On-Fail">>, [<<"true">>, <<"false">>]}
			    ,?INSERT_AT_TUPLE
			   ]).
-define(BRIDGE_REQ_TYPES, [{<<"Endpoints">>, fun is_list/1}
			   ,{<<"SIP-Headers">>, ?IS_JSON_OBJECT}
                           ,{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}
			  ]).

%% Bridge Endpoints
-define(BRIDGE_REQ_ENDPOINT_HEADERS, [<<"Invite-Format">>]).
-define(OPTIONAL_BRIDGE_REQ_ENDPOINT_HEADERS, [ <<"Route">>, <<"To-User">>, <<"To-Realm">>, <<"To-DID">>
						    ,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
						    ,<<"Callee-ID-Name">>, <<"Callee-ID-Number">>
                                                    ,<<"Ignore-Early-Media">>, <<"Bypass-Media">>
                                                    ,<<"Endpoint-Timeout">>, <<"Endpoint-Progress-Timeout">>
                                                    ,<<"Endpoint-Delay">>, <<"Codecs">>, <<"SIP-Headers">>
                                                    ,<<"Custom-Channel-Vars">>, <<"Auth-User">>, <<"Auth-Password">>
					      ]).
-define(BRIDGE_REQ_ENDPOINT_VALUES, [?INVITE_FORMAT_TUPLE
                                     ,{<<"Ignore-Early-Media">>, [<<"true">>, <<"false">>]}
                                     ,{<<"Bypass-Media">>, [<<"true">>, <<"false">>]}
                                    ]).
-define(BRIDGE_REQ_ENDPOINT_TYPES, [{<<"SIP-Headers">>, ?IS_JSON_OBJECT}
                                    ,{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}
                                   ]).
