%%%-------------------------------------------------------------------
%%% @yright (C) 2011-2015 2600Hz INC
%%% @doc
%%% Dialplan API definitions
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%
%%% Fix KAZOO-3406: Sponsored by Velvetech LLC, implemented by SIPLABS LLC
%%%-------------------------------------------------------------------
-ifndef(WAPI_DIALPLAN_HRL).
-include_lib("whistle/include/wh_types.hrl").

%% For dialplan messages, what does the Invite-Format param accept as values?
-define(INVITE_FORMAT_TUPLE, {<<"Invite-Format">>
                              ,[<<"username">>, <<"e164">>
                                ,<<"npan">>, <<"1npan">>
                                ,<<"route">>, <<"loopback">>
                               ]
                             }).

%% For dialplan messages, an optional insert-at tuple is common across all requests
-define(INSERT_AT_TUPLE, {<<"Insert-At">>, [<<"head">>, <<"tail">>, <<"flush">>, <<"now">>]}).
-define(IS_TERMINATOR, fun wapi_dialplan:terminators_v/1).

-define(UNBRIDGE_REQ_HEADERS, [<<"Call-ID">>, <<"Application-Name">>]).
-define(OPTIONAL_UNBRIDGE_REQ_HEADERS, [<<"Insert-At">>
                                        ,<<"Leg">>
                                       ]).
-define(UNBRIDGE_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                              ,{<<"Event-Name">>, <<"command">>}
                              ,{<<"Application-Name">>, <<"unbridge">>}
                              ,{<<"Leg">>, [<<"A">>, <<"B">>, <<"Both">>]}
                              ,?INSERT_AT_TUPLE
                             ]).
-define(UNBRIDGE_REQ_TYPES, []).

-define(DIAL_METHOD_SINGLE, <<"single">>).
-define(DIAL_METHOD_SIMUL, <<"simultaneous">>).

-define(BRIDGE_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Endpoints">>]).
-define(OPTIONAL_BRIDGE_REQ_HEADERS
        ,[<<"Timeout">>, <<"Continue-On-Fail">>, <<"Ignore-Early-Media">>
          ,<<"Outbound-Caller-ID-Name">>, <<"Outbound-Caller-ID-Number">>
          ,<<"Outbound-Callee-ID-Name">>, <<"Outbound-Callee-ID-Number">>
          ,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
          ,<<"Callee-ID-Name">>, <<"Callee-ID-Number">>
          ,<<"Dial-Endpoint-Method">>, <<"Insert-At">>
          ,<<"Media">>, <<"Hold-Media">>, <<"Ringback">>
          ,<<"Custom-Channel-Vars">>, <<"Secure-RTP">>, <<"Force-Fax">>
          ,<<"SIP-Transport">>, <<"Custom-SIP-Headers">>
          ,<<"Ignore-Completed-Elsewhere">>
          ,<<"Enable-T38-Fax">>, <<"Enable-T38-Fax-Request">>
          ,<<"Enable-T38-Passthrough">>, <<"Enable-T38-Gateway">>
          ,<<"B-Leg-Events">>
          ,<<"Ignore-Forward">>
         ]).
-define(BRIDGE_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                            ,{<<"Event-Name">>, <<"command">>}
                            ,{<<"Application-Name">>, <<"bridge">>}
                            ,{<<"Dial-Endpoint-Method">>, [?DIAL_METHOD_SINGLE, ?DIAL_METHOD_SIMUL]}
                            ,{<<"Media">>, [<<"process">>, <<"bypass">>, <<"auto">>]}
                            ,{<<"SIP-Transport">>, [<<"udp">>, <<"tcp">>, <<"tls">>]}
                            ,{<<"Enable-T38-Gateway">>, [<<"self">>, <<"peer">>]}
                            ,?INSERT_AT_TUPLE
                           ]).
-define(BRIDGE_REQ_TYPES, [{<<"Endpoints">>, fun is_list/1}
                           ,{<<"Custom-SIP-Headers">>, fun wh_json:is_json_object/1}
                           ,{<<"Custom-Channel-Vars">>, fun wh_json:is_json_object/1}
                           ,{<<"Continue-On-Fail">>, fun wh_util:is_boolean/1}
                           ,{<<"Secure-RTP">>, fun wh_util:is_boolean/1}
                           ,{<<"B-Leg-Events">>, fun b_leg_events_v/1}
                          ]).

%% Bridge Endpoints
-define(BRIDGE_REQ_ENDPOINT_HEADERS, [<<"Invite-Format">>]).
-define(OPTIONAL_BRIDGE_REQ_ENDPOINT_HEADERS
        ,[<<"Route">>, <<"To-User">>
          ,<<"To-Realm">>, <<"To-DID">>, <<"To-URI">>
          ,<<"To-IP">>, <<"To-Username">>
          ,<<"Outbound-Caller-ID-Name">>, <<"Outbound-Caller-ID-Number">>
          ,<<"Outbound-Callee-ID-Name">>, <<"Outbound-Callee-ID-Number">>
          ,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
          ,<<"Callee-ID-Name">>, <<"Callee-ID-Number">>
          ,<<"Ignore-Early-Media">>, <<"Bypass-Media">>, <<"Hold-Media">>
          ,<<"Endpoint-Timeout">>, <<"Endpoint-Progress-Timeout">>
          ,<<"Endpoint-Delay">>, <<"Codecs">>, <<"Custom-SIP-Headers">>, <<"Presence-ID">>
          ,<<"Custom-Channel-Vars">>
          ,<<"Auth-User">>, <<"Auth-Password">>, <<"Auth-Realm">>
          ,<<"Endpoint-Type">>, <<"Endpoint-Options">>, <<"Force-Fax">>
          ,<<"Proxy-Zone">>, <<"Proxy-IP">>
          ,<<"Forward-IP">>, <<"SIP-Transport">>
          ,<<"Ignore-Completed-Elsewhere">>
          ,<<"SIP-Interface">>
          ,<<"Enable-T38-Fax">>, <<"Enable-T38-Fax-Request">>
          ,<<"Enable-T38-Passthrough">>, <<"Enable-T38-Gateway">>
          ,<<"Failover">>
         ]).
-define(BRIDGE_REQ_ENDPOINT_VALUES, [?INVITE_FORMAT_TUPLE
                                     ,{<<"Endpoint-Type">>, [<<"sip">>, <<"freetdm">>, <<"skype">>]}
                                     ,{<<"Enable-T38-Gateway">>, [<<"self">>, <<"peer">>]}
                                     ,{<<"SIP-Transport">>, [<<"udp">>, <<"tcp">>, <<"tls">>, <<"sctp">>]}
                                    ]).
-define(BRIDGE_REQ_ENDPOINT_TYPES, [{<<"Custom-SIP-Headers">>, fun wh_json:is_json_object/1}
                                    ,{<<"Custom-Channel-Vars">>, fun wh_json:is_json_object/1}
                                    ,{<<"Endpoint-Options">>, fun wh_json:is_json_object/1}
                                    ,{<<"Ignore-Early-Media">>, fun wh_util:is_boolean/1}
                                    ,{<<"Bypass-Media">>, fun wh_util:is_boolean/1}
                                   ]).

%% Page Request
-define(PAGE_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Endpoints">>]).
-define(OPTIONAL_PAGE_REQ_HEADERS, [<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
                                    ,<<"Callee-ID-Name">>, <<"Callee-ID-Number">>
                                    ,<<"Timeout">>, <<"Insert-At">>
                                    ,<<"Custom-Channel-Vars">>, <<"Custom-SIP-Headers">>
                                   ]).
-define(PAGE_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                          ,{<<"Event-Name">>, <<"command">>}
                          ,{<<"Application-Name">>, <<"page">>}
                          ,?INSERT_AT_TUPLE
                         ]).
-define(PAGE_REQ_TYPES, [{<<"Endpoints">>, fun is_list/1}
                         ,{<<"Custom-SIP-Headers">>, fun wh_json:is_json_object/1}
                         ,{<<"Custom-Channel-Vars">>, fun wh_json:is_json_object/1}
                        ]).

%% Store Request
-define(STORE_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>
                            ,<<"Media-Name">>, <<"Media-Transfer-Method">>
                            ,<<"Media-Transfer-Destination">>
                           ]).
-define(OPTIONAL_STORE_REQ_HEADERS, [<<"Additional-Headers">>
                                     ,<<"Suppress-Error-Report">>
                                     ,<<"Insert-At">>
                                    ]).
-define(STORE_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                           ,{<<"Event-Name">>, <<"command">>}
                           ,{<<"Application-Name">>, <<"store">>}
                           ,{<<"Media-Transfer-Method">>, [<<"stream">>, <<"put">>, <<"post">>]}
                           ,?INSERT_AT_TUPLE
                          ]).
-define(STORE_REQ_TYPES, [{<<"Additional-Headers">>, fun is_list/1}
                          ,{<<"Suppress-Error-Report">>, fun wh_util:is_boolean/1}
                         ]).

%% Store Fax
-define(STORE_FAX_HEADERS, [<<"Application-Name">>, <<"Call-ID">>
                            ,<<"Media-Transfer-Method">>
                            ,<<"Media-Transfer-Destination">>
                           ]).
-define(OPTIONAL_STORE_FAX_HEADERS, [<<"Additional-Headers">>, <<"Insert-At">>, <<"Fax-Local-Filename">>]).
-define(STORE_FAX_VALUES, [{<<"Event-Category">>, <<"call">>}
                           ,{<<"Event-Name">>, <<"command">>}
                           ,{<<"Application-Name">>, <<"store_fax">>}
                           ,{<<"Media-Transfer-Method">>, <<"put">>}
                           ,?INSERT_AT_TUPLE
                          ]).
-define(STORE_FAX_TYPES, [{<<"Additional-Headers">>, fun is_list/1}]).

%% Store (via AMQP) Response
-define(STORE_AMQP_RESP_HEADERS, [<<"Call-ID">>, <<"Application-Name">>
                                  ,<<"Media-Transfer-Method">>
                                  ,<<"Media-Name">>, <<"Media-Content">>
                                 ]).
-define(OPTIONAL_STORE_AMQP_RESP_HEADERS, [<<"Media-Sequence-ID">>]).
-define(STORE_AMQP_RESP_VALUES, [{<<"Application-Name">>, <<"store">>}
                                 ,{<<"Media-Transfer-Method">>, <<"stream">>}
                                ]).
-define(STORE_AMQP_RESP_TYPES, [{<<"Media-Content">>, fun store_media_content_v/1}
                                ,{<<"Media-Name">>, fun is_binary/1}
                               ]).

%% Store (via HTTP) Response
-define(STORE_HTTP_RESP_HEADERS, [<<"Call-ID">>, <<"Application-Name">>
                                  ,<<"Media-Transfer-Method">>
                                  ,<<"Media-Name">>, <<"Media-Transfer-Results">>
                                 ]).
-define(OPTIONAL_STORE_HTTP_RESP_HEADERS, []).
-define(STORE_HTTP_RESP_VALUES, [{<<"Application-Name">>, <<"store">>}
                                 ,{<<"Media-Transfer-Method">>, [<<"put">>, <<"post">>]}
                                 ,{<<"Event-Name">>, <<"response">>}
                                 ,{<<"Event-Category">>, <<"call">>}
                                ]).
-define(STORE_HTTP_RESP_TYPES, [{<<"Media-Transfer-Results">>, fun wh_json:is_json_object/1}]).

%% Send DTMF Request
-define(SEND_DTMF_HEADERS, [<<"Call-ID">>, <<"Application-Name">>, <<"DTMFs">>]).
-define(OPTIONAL_SEND_DTMF_HEADERS, [<<"Insert-At">>, <<"Duration">>]).
-define(SEND_DTMF_VALUES, [{<<"Event-Category">>, <<"call">>}
                           ,{<<"Event-Name">>, <<"command">>}
                           ,{<<"Application-Name">>, <<"send_dtmf">>}
                           ,?INSERT_AT_TUPLE
                          ]).
-define(SEND_DTMF_TYPES, [{<<"DTMFs">>, fun is_binary/1}]).

-define(METAFLOW_HEADERS, [<<"Call">>]).
-define(OPTIONAL_METAFLOW_HEADERS, [<<"Numbers">>, <<"Patterns">>
                                    ,<<"Binding-Digit">>, <<"Digit-Timeout">>
                                    ,<<"Endpoint-ID">>, <<"Listen-On">>
                                   ]).
-define(METAFLOW_VALUES, [{<<"Event-Category">>, <<"call">>}
                          ,{<<"Event-Name">>, <<"command">>}
                          ,{<<"Application-Name">>, <<"metaflow">>}
                          ,{<<"Binding-Digit">>, ?ANY_DIGIT}
                          ,{<<"Listen-On">>, [<<"both">>, <<"self">>, <<"peer">>]}
                         ]).
-define(METAFLOW_TYPES, [{<<"Numbers">>, fun wh_json:is_json_object/1}
                         ,{<<"Patterns">>, fun wh_json:is_json_object/1}
                         ,{<<"Digit-Timeout">>, fun metaflow_digit_timeout_v/1}
                        ]).
-define(METAFLOW_ROUTING_KEY(CallId), <<"call.metaflow.", (amqp_util:encode(CallId))/binary>>).

%% Tones Request
-define(TONES_REQ_HEADERS, [<<"Call-ID">>, <<"Application-Name">>, <<"Tones">>]).
-define(OPTIONAL_TONES_REQ_HEADERS, [<<"Insert-At">>, <<"Terminators">>
                                     ,<<"Conference-ID">>, <<"Group-ID">>
                                    ]).
-define(TONES_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                           ,{<<"Event-Name">>, <<"command">>}
                           ,{<<"Application-Name">>, <<"tones">>}
                           ,?INSERT_AT_TUPLE
                          ]).
-define(TONES_REQ_TYPES, [{<<"Tones">>, fun is_list/1}
                          ,{<<"Terminators">>, ?IS_TERMINATOR}
                         ]).

-define(TONES_REQ_TONE_HEADERS, [<<"Frequencies">>, <<"Duration-ON">>, <<"Duration-OFF">>]).
-define(OPTIONAL_TONES_REQ_TONE_HEADERS, [<<"Volume">>, <<"Repeat">>]).
-define(TONES_REQ_TONE_VALUES, []).
-define(TONES_REQ_TONE_TYPES, []).

%% Tone Detect
-define(TONE_DETECT_REQ_HEADERS, [<<"Call-ID">>, <<"Application-Name">>
                                  ,<<"Tone-Detect-Name">>, <<"Frequencies">>
                                 ]).
-define(OPTIONAL_TONE_DETECT_REQ_HEADERS, [<<"Sniff-Direction">>, <<"Timeout">>
                                           ,<<"On-Success">>, <<"Hits-Needed">>, <<"Insert-At">>
                                          ]).
-define(TONE_DETECT_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                                 ,{<<"Event-Name">>, <<"command">>}
                                 ,{<<"Application-Name">>, <<"tone_detect">>}
                                 ,{<<"Sniff-Direction">>, [<<"read">>, <<"write">>]}
                                 ,?INSERT_AT_TUPLE
                                ]).
-define(TONE_DETECT_REQ_TYPES, [{<<"On-Success">>, fun is_list/1}
                                ,{<<"Timeout">>, fun tone_timeout_v/1}
                               ]).

%% Queue Request
-define(QUEUE_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Commands">>]).
-define(OPTIONAL_QUEUE_REQ_HEADERS, [<<"Insert-At">>]).
-define(QUEUE_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                           ,{<<"Event-Name">>, <<"command">>}
                           ,{<<"Application-Name">>, <<"queue">>}
                           ,?INSERT_AT_TUPLE
                          ]).
-define(QUEUE_REQ_TYPES, [{<<"Commands">>, fun is_list/1}]).

%% Answer
-define(ANSWER_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>]).
-define(OPTIONAL_ANSWER_REQ_HEADERS, [<<"Insert-At">>]).
-define(ANSWER_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                            ,{<<"Event-Name">>, <<"command">>}
                            ,{<<"Application-Name">>, <<"answer">>}
                            ,?INSERT_AT_TUPLE
                           ]).
-define(ANSWER_REQ_TYPES, []).

%% Echo
-define(ECHO_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>]).
-define(OPTIONAL_ECHO_REQ_HEADERS, [<<"Insert-At">>]).
-define(ECHO_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                            ,{<<"Event-Name">>, <<"command">>}
                            ,{<<"Application-Name">>, <<"echo">>}
                            ,?INSERT_AT_TUPLE
                           ]).
-define(ECHO_REQ_TYPES, []).

%% Privacy
-define(PRIVACY_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>]).
-define(OPTIONAL_PRIVACY_REQ_HEADERS, [<<"Insert-At">>, <<"Privacy-Mode">>]).
-define(PRIVACY_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                             ,{<<"Event-Name">>, <<"command">>}
                             ,{<<"Application-Name">>, <<"privacy">>}
                             ,{<<"Privacy-Mode">>, [<<"full">>, <<"name">>, <<"number">>]}
                             ,?INSERT_AT_TUPLE
                            ]).
-define(PRIVACY_REQ_TYPES, []).

%% Progress
-define(PROGRESS_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>]).
-define(OPTIONAL_PROGRESS_REQ_HEADERS, [<<"Insert-At">>]).
-define(PROGRESS_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                              ,{<<"Event-Name">>, <<"command">>}
                              ,{<<"Application-Name">>, <<"progress">>}
                              ,?INSERT_AT_TUPLE
                             ]).
-define(PROGRESS_REQ_TYPES, []).

%% Ring
-define(RING_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>]).
-define(OPTIONAL_RING_REQ_HEADERS, [<<"Insert-At">>, <<"Ringback">>]).
-define(RING_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                          ,{<<"Event-Name">>, <<"command">>}
                          ,{<<"Application-Name">>, <<"ring">>}
                          ,?INSERT_AT_TUPLE
                         ]).
-define(RING_REQ_TYPES, []).

%% Recv Fax
-define(RECV_FAX_HEADERS, [<<"Application-Name">>, <<"Call-ID">>]).
-define(OPTIONAL_RECV_FAX_HEADERS, [<<"Enable-T38-Fax">>
                                    ,<<"Enable-T38-Fax-Request">>
                                    ,<<"Enable-T38-Passthrough">>
                                    ,<<"Enable-T38-Gateway">>
                                    ,<<"Fax-Local-Filename">>
                                   ]).
-define(RECV_FAX_VALUES, [{<<"Event-Category">>, <<"call">>}
                          ,{<<"Event-Name">>, <<"command">>}
                          ,{<<"Application-Name">>, <<"receive_fax">>}
                          ,?INSERT_AT_TUPLE
                         ]).
-define(RECV_FAX_TYPES, []).

%% Hangup
%% Include the Other-Leg-Call-ID to only hangup the other leg
-define(HANGUP_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>]).
-define(OPTIONAL_HANGUP_REQ_HEADERS, [<<"Insert-At">>, <<"Other-Leg-Only">>]).
-define(HANGUP_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                            ,{<<"Event-Name">>, <<"command">>}
                            ,{<<"Application-Name">>, <<"hangup">>}
                            ,?INSERT_AT_TUPLE
                           ]).
-define(HANGUP_REQ_TYPES, [{<<"Other-Leg-Only">>, fun wh_util:is_boolean/1}
                          ]).

%% Hold
-define(HOLD_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>]).
-define(OPTIONAL_HOLD_REQ_HEADERS, [<<"Insert-At">>, <<"Hold-Media">>]).
-define(HOLD_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                          ,{<<"Event-Name">>, <<"command">>}
                          ,{<<"Application-Name">>, <<"hold">>}
                          ,?INSERT_AT_TUPLE
                         ]).
-define(HOLD_REQ_TYPES, []).

%% Park
-define(PARK_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>]).
-define(OPTIONAL_PARK_REQ_HEADERS, [<<"Insert-At">>, <<"Timeout">>, <<"Hangup-Cause">>]).
-define(PARK_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                          ,{<<"Event-Name">>, <<"command">>}
                          ,{<<"Application-Name">>, <<"park">>}
                          ,?INSERT_AT_TUPLE
                         ]).
-define(PARK_REQ_TYPES, []).

%% Set
-define(SET_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>
                          ,<<"Custom-Channel-Vars">>, <<"Custom-Call-Vars">>
                         ]).
-define(OPTIONAL_SET_REQ_HEADERS, [<<"Insert-At">>]).
-define(SET_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                         ,{<<"Event-Name">>, <<"command">>}
                         ,{<<"Application-Name">>, <<"set">>}
                         ,?INSERT_AT_TUPLE
                        ]).
-define(SET_REQ_TYPES, [{<<"Custom-Channel-Vars">>,fun wh_json:is_json_object/1}
                        ,{<<"Custom-Call-Vars">>, fun wh_json:is_json_object/1}
                       ]).

%% Set Terminators
-define(SET_TERM_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Terminators">>]).
-define(OPTIONAL_SET_TERM_HEADERS, [<<"Insert-At">>]).
-define(SET_TERM_VALUES, [{<<"Event-Category">>, <<"call">>}
                          ,{<<"Event-Name">>, <<"command">>}
                          ,{<<"Application-Name">>, <<"set_terminators">>}
                          ,?INSERT_AT_TUPLE
                         ]).
-define(SET_TERM_TYPES, [{<<"Terminators">>, ?IS_TERMINATOR}]).

%% Fetch
-define(FETCH_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>]).
-define(OPTIONAL_FETCH_REQ_HEADERS, [<<"Insert-At">>, <<"From-Other-Leg">>]).
-define(FETCH_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                           ,{<<"Event-Name">>, <<"command">>}
                           ,{<<"Application-Name">>, <<"fetch">>}
                           ,?INSERT_AT_TUPLE
                          ]).
-define(FETCH_REQ_TYPES, [{<<"From-Other-Leg">>, fun wh_util:is_boolean/1}]).

%% Call Pickup
-define(CALL_PICKUP_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Target-Call-ID">>]).
-define(OPTIONAL_CALL_PICKUP_REQ_HEADERS, [<<"Insert-At">>, <<"Unbridged-Only">>, <<"Unanswered-Only">>
                                           ,<<"Other-Leg">>
                                           ,<<"Continue-On-Fail">>, <<"Continue-On-Cancel">>
                                           ,<<"Park-After-Pickup">> %% Will park either leg after cancel
                                           ,<<"Hangup-After-Pickup">>
                                           ,<<"Move-Channel-If-Necessary">>
                                          ]).
-define(CALL_PICKUP_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                                 ,{<<"Event-Name">>, <<"command">>}
                                 ,{<<"Application-Name">>, <<"call_pickup">>}
                                 ,?INSERT_AT_TUPLE
                                ]).
-define(CALL_PICKUP_REQ_TYPES, [{<<"Park-After-Pickup">>, fun wh_util:is_boolean/1}
                                ,{<<"Hangup-After-Pickup">>, fun wh_util:is_boolean/1}
                                ,{<<"Move-Channel-If-Necessary">>, fun wh_util:is_boolean/1}
                               ]).

%% Call Pickup
-define(CONNECT_LEG_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Target-Call-ID">>]).
-define(OPTIONAL_CONNECT_LEG_REQ_HEADERS, [<<"Insert-At">>, <<"Unbridged-Only">>, <<"Unanswered-Only">>
                                           ,<<"Other-Leg">>
                                           ,<<"Continue-On-Fail">>, <<"Continue-On-Cancel">>
                                           ,<<"Park-After-Pickup">> %% Will park either leg after cancel
                                           ,<<"Hangup-After-Pickup">>
                                           ,<<"Move-Channel-If-Necessary">>
                                           ,<<"Publish-Usurp">>, <<"B-Leg-Events">>
                                          ]).
-define(CONNECT_LEG_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                                 ,{<<"Event-Name">>, <<"command">>}
                                 ,{<<"Application-Name">>, <<"connect_leg">>}
                                 ,?INSERT_AT_TUPLE
                                ]).
-define(CONNECT_LEG_REQ_TYPES, [{<<"Park-After-Pickup">>, fun wh_util:is_boolean/1}
                                ,{<<"Hangup-After-Pickup">>, fun wh_util:is_boolean/1}
                                ,{<<"Move-Channel-If-Necessary">>, fun wh_util:is_boolean/1}
                                ,{<<"Publish-Usurp">>, fun wh_util:is_boolean/1}
                                ,{<<"B-Leg-Events">>, fun b_leg_events_v/1}
                               ]).

%% Eavesdrop
-define(EAVESDROP_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Target-Call-ID">>]).
-define(OPTIONAL_EAVESDROP_REQ_HEADERS, [<<"Insert-At">>, <<"Enable-DTMF">>
                                           ,<<"Other-Leg">>
                                           ,<<"Continue-On-Fail">>, <<"Continue-On-Cancel">>
                                           ,<<"Move-Channel-If-Necessary">>
                                          ]).
-define(EAVESDROP_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                                 ,{<<"Event-Name">>, <<"command">>}
                                 ,{<<"Application-Name">>, <<"eavesdrop">>}
                                 ,?INSERT_AT_TUPLE
                                ]).
-define(EAVESDROP_REQ_TYPES, [{<<"Move-Channel-If-Necessary">>, fun wh_util:is_boolean/1}]).

%% Play Request
-define(PLAY_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Media-Name">>]).
-define(OPTIONAL_PLAY_REQ_HEADERS, [<<"Terminators">>, <<"Insert-At">>, <<"Leg">>
                                    ,<<"Voice">>, <<"Language">>, <<"Format">>
                                    ,<<"Group-ID">> % group media together (one DTMF cancels all in group)
                                   ]).
-define(PLAY_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                          ,{<<"Event-Name">>, <<"command">>}
                          ,{<<"Application-Name">>, <<"play">>}
                          ,{<<"Leg">>, [<<"A">>, <<"B">>, <<"Both">>]}
                          ,?INSERT_AT_TUPLE
                         ]).
-define(PLAY_REQ_TYPES, [{<<"Terminators">>, ?IS_TERMINATOR}]).

%% PlayStop Request
-define(PLAY_STOP_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>]).
-define(OPTIONAL_PLAY_STOP_REQ_HEADERS, [<<"Insert-At">>]).
-define(PLAY_STOP_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                               ,{<<"Event-Name">>, <<"command">>}
                               ,{<<"Application-Name">>, <<"playstop">>}
                               ,{<<"Insert-At">>, <<"now">>}
                              ]).
-define(PLAY_STOP_REQ_TYPES, []).

%% Record Request
-define(RECORD_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Media-Name">>]).
-define(OPTIONAL_RECORD_REQ_HEADERS, [<<"Terminators">>, <<"Time-Limit">>, <<"Silence-Threshold">>
                                      ,<<"Silence-Hits">>, <<"Insert-At">>
                                     ]).
-define(RECORD_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                            ,{<<"Event-Name">>, <<"command">>}
                            ,{<<"Application-Name">>, <<"record">>}
                            ,?INSERT_AT_TUPLE
                           ]).
-define(RECORD_REQ_TYPES, [{<<"Terminators">>, ?IS_TERMINATOR}]).

%% Record Call Leg into MediaName
%% Stream-To = local results in the recording being stored on the media server
%% Stream-To = remote will stream the recording to the handling ecallmgr server
-define(RECORD_CALL_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Media-Name">>
                                  ,<<"Record-Action">>
                                 ]).
-define(OPTIONAL_RECORD_CALL_REQ_HEADERS, [<<"Time-Limit">>, <<"Insert-At">>, <<"Follow-Transfer">>
                                           ,<<"Media-Transfer-Method">> ,<<"Media-Transfer-Destination">>
                                           ,<<"Additional-Headers">>, <<"Record-Sample-Rate">>
                                          ]).
-define(RECORD_CALL_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                                 ,{<<"Event-Name">>, <<"command">>}
                                 ,{<<"Application-Name">>, <<"record_call">>}
                                 ,{<<"Record-Action">>, [<<"start">>, <<"stop">>]}
                                 ,?INSERT_AT_TUPLE
                                ]).
-define(RECORD_CALL_REQ_TYPES, [{<<"Record-Sample-Rate">>, fun is_integer/1}]).

%% Play and Record Digits
-define(PLAY_COLLECT_DIGITS_REQ_HEADERS
        ,[<<"Application-Name">>, <<"Call-ID">>, <<"Minimum-Digits">>
          ,<<"Maximum-Digits">>, <<"Media-Name">>, <<"Media-Tries">>
          ,<<"Digits-Regex">>, <<"Timeout">>, <<"Terminators">>
         ]).
-define(OPTIONAL_PLAY_COLLECT_DIGITS_REQ_HEADERS, [<<"Insert-At">>, <<"Failed-Media-Name">>]).
-define(PLAY_COLLECT_DIGITS_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                                         ,{<<"Event-Name">>, <<"command">>}
                                         ,{<<"Application-Name">>, <<"play_and_collect_digits">>}
                                         ,?INSERT_AT_TUPLE
                                        ]).
-define(PLAY_COLLECT_DIGITS_REQ_TYPES, [{<<"Terminators">>, ?IS_TERMINATOR}]).

%% Say
-define(SAY_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Language">>
                          ,<<"Type">>, <<"Method">>, <<"Say-Text">>
                         ]).
-define(OPTIONAL_SAY_REQ_HEADERS, [<<"Insert-At">>]).
-define(SAY_REQ_VALUES
        ,[{<<"Event-Category">>, <<"call">>}
          ,{<<"Event-Name">>, <<"command">>}
          ,{<<"Application-Name">>, <<"say">>}
          ,{<<"Type">>, [<<"number">>, <<"items">>, <<"persons">>, <<"messages">>
                         ,<<"currency">>, <<"time_measurement">>
                         ,<<"current_date">>, <<"current_time">>
                         ,<<"current_date_time">>, <<"telephone_number">>
                         ,<<"telephone_extension">>, <<"url">>
                         ,<<"ip_address">>, <<"e-mail_address">>
                         ,<<"postal_address">>, <<"account_number">>
                         ,<<"name_spelled">>, <<"name_phonetic">>
                         ,<<"short_date_time">>
                        ]}
          ,{<<"Method">>, [<<"none">>, <<"pronounced">>, <<"iterated">>, <<"counted">>]}
          ,{<<"Gender">>, [<<"masculine">>, <<"feminine">>, <<"neuter">>]}
          ,?INSERT_AT_TUPLE
         ]).
-define(SAY_REQ_TYPES, []).

%% TTS
-define(TTS_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Text">>]).
-define(OPTIONAL_TTS_REQ_HEADERS, [<<"Terminators">>, <<"Insert-At">>
                                   ,<<"Voice">>, <<"Language">>, <<"Engine">>
                                   ,<<"Group-ID">> % group media together (one DTMF cancels all in group)
                                   ,<<"Conference-ID">>
                                  ]).
-define(TTS_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                         ,{<<"Event-Name">>, <<"command">>}
                         ,{<<"Application-Name">>, <<"tts">>}
                         ,{<<"Voice">>, [<<"male">>, <<"female">>]}
                         ,?INSERT_AT_TUPLE
                        ]).
-define(TTS_REQ_TYPES, [{<<"Terminators">>, ?IS_TERMINATOR}]).

%% Respond
-define(RESPOND_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Response-Code">>]).
-define(OPTIONAL_RESPOND_REQ_HEADERS, [<<"Insert-At">>, <<"Response-Message">>]).
-define(RESPOND_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                             ,{<<"Event-Name">>, <<"command">>}
                             ,{<<"Application-Name">>, <<"respond">>}
                             ,?INSERT_AT_TUPLE
                            ]).
-define(RESPOND_REQ_TYPES, [{<<"Response-Code">>, fun is_binary/1}
                            ,{<<"Response-Message">>, fun is_binary/1}
                           ]).

%% Redirect
-define(REDIRECT_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Redirect-Contact">>]).
-define(OPTIONAL_REDIRECT_REQ_HEADERS, [<<"Insert-At">>, <<"Redirect-Server">>, <<"Redirect-Node">>]).
-define(REDIRECT_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                              ,{<<"Event-Name">>, <<"command">>}
                              ,{<<"Application-Name">>, <<"redirect">>}
                              ,?INSERT_AT_TUPLE
                             ]).
-define(REDIRECT_REQ_TYPES, [{<<"Redirect-Contact">>, fun is_binary/1}
                             ,{<<"Redirect-Server">>, fun is_binary/1}
                             ,{<<"Redirect-Node">>, fun is_binary/1}
                            ]).

%% Execute_Extension
-define(EXECUTE_EXTENSION_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Extension">>]).
-define(OPTIONAL_EXECUTE_EXTENSION_REQ_HEADERS, [<<"Insert-At">>, <<"Reset">>, <<"Custom-Channel-Vars">>]).
-define(EXECUTE_EXTENSION_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                                       ,{<<"Event-Name">>, <<"command">>}
                                       ,{<<"Application-Name">>, <<"execute_extension">>}
                                       ,?INSERT_AT_TUPLE
                                      ]).
-define(EXECUTE_EXTENSION_REQ_TYPES, [{<<"Extension">>, fun is_binary/1}
                                     ]).
%% Sleep
-define(SLEEP_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Time">>]).
-define(OPTIONAL_SLEEP_REQ_HEADERS, [<<"Insert-At">>]).
-define(SLEEP_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                           ,{<<"Event-Name">>, <<"command">>}
                           ,{<<"Application-Name">>, <<"sleep">>}
                           ,?INSERT_AT_TUPLE
                          ]).
-define(SLEEP_REQ_TYPES, []).

%% NoOp Request
%% Filter-Applications: will remove applications in the ecallmgr command queue matching those in this list
%% So, if you want to remove Play commands, set Filter-Applications = [<<"play">>]. This will filter
%% the command queue until a non-Play command is encountered.
%% Alternatively, you can specify the elements in the Filter-Applications list as:
%% [ {"Application-Name":"play", "Fields":{"Terminators":["#"]}}, ...]
%% This says, filter Play commands terminate-able with the "#" key
%%
%% IMPORTANT: to use the filter-applications list, Insert-At must be "now"

-define(NOOP_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>]).
-define(OPTIONAL_NOOP_REQ_HEADERS, [<<"Insert-At">>, <<"Filter-Applications">>
                                    ,<<"B-Leg-Events">>
                                   ]).
-define(NOOP_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                          ,{<<"Event-Name">>, <<"command">>}
                          ,{<<"Application-Name">>, <<"noop">>}
                          ,?INSERT_AT_TUPLE
                         ]).
-define(NOOP_REQ_TYPES, [{<<"B-Leg-Events">>, fun b_leg_events_v/1}]).

%% Conference
-define(CONFERENCE_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>
                                 ,<<"Conference-ID">>
                                ]).
-define(OPTIONAL_CONFERENCE_REQ_HEADERS, [<<"Insert-At">>
                                          ,<<"Mute">>, <<"Deaf">>, <<"Moderator">>
                                          ,<<"Reinvite">>, <<"Profile">>

                                          %% sets joining member to nospeak relations
                                          ,<<"Member-Nospeak">>
                                          %% update relations
                                          ,<<"Nospeak-Check">>
                                         ]).
-define(CONFERENCE_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                                ,{<<"Event-Name">>, <<"command">>}
                                ,{<<"Application-Name">>, <<"conference">>}
                                ,?INSERT_AT_TUPLE
                               ]).
-define(CONFERENCE_REQ_TYPES, [{<<"Call-ID">>, fun is_binary/1}
                               ,{<<"Conference-ID">>, fun is_binary/1}
                               ,{<<"Mute">>, fun wh_util:is_boolean/1}
                               ,{<<"Deaf">>, fun wh_util:is_boolean/1}
                               ,{<<"Moderator">>, fun wh_util:is_boolean/1}
                               ,{<<"Reinvite">>, fun wh_util:is_boolean/1}
                              ]).

%% Originate Ready
-define(ORIGINATE_READY_HEADERS, [<<"Call-ID">>, <<"Control-Queue">>]).
-define(OPTIONAL_ORIGINATE_READY_HEADERS, []).
-define(ORIGINATE_READY_VALUES, [{<<"Event-Category">>, <<"dialplan">>}
                                 ,{<<"Event-Name">>, <<"originate_ready">>}
                                ]).
-define(ORIGINATE_READY_TYPES, []).

%% Originate Execute
-define(ORIGINATE_EXECUTE_HEADERS, [<<"Call-ID">>]).
-define(OPTIONAL_ORIGINATE_EXECUTE_HEADERS, []).
-define(ORIGINATE_EXECUTE_VALUES, [{<<"Event-Category">>, <<"dialplan">>}
                                   ,{<<"Event-Name">>, <<"originate_execute">>}
                                  ]).
-define(ORIGINATE_EXECUTE_TYPES, []).

%% Fax Detection
-define(FAX_DETECTION_REQ_HEADERS, [<<"Call-ID">>, <<"Application-Name">>, <<"Action">>]).
-define(OPTIONAL_FAX_DETECTION_REQ_HEADERS, [<<"Direction">>, <<"Action">>, <<"Duration">>, <<"Insert-At">>]).
-define(FAX_DETECTION_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                                 ,{<<"Event-Name">>, <<"command">>}
                                 ,{<<"Application-Name">>, <<"fax_detection">>}
                                 ,{<<"Direction">>, [<<"inbound">>, <<"outbound">>]}
                                 ,{<<"Action">>, [<<"start">>, <<"stop">>]}
                                 ,?INSERT_AT_TUPLE
                                ]).
-define(FAX_DETECTION_REQ_TYPES, []).

-define(WAPI_DIALPLAN_HRL, 'true').
-endif.
