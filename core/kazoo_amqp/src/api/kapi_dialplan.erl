%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Dialplan API commands.
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_dialplan).

-compile({'no_auto_import', [error/1]}).

-export([api_definitions/0, api_definition/1]).

-export([v/1]).

-export([optional_bridge_req_headers/0]).
-export([optional_bridge_req_endpoint_headers/0]).

-export([bridge/1
        ,bridge_v/1
        ]).
-export([bridge_endpoint/1
        ,bridge_endpoint_v/1
        ]).
-export([unbridge/1
        ,unbridge_v/1
        ]).
-export([page/1
        ,page_v/1
        ]).
-export([store/1
        ,store_v/1
        ]).
-export([store_amqp_resp/1
        ,store_amqp_resp_v/1
        ]).
-export([store_http_resp/1
        ,store_http_resp_v/1
        ]).
-export([noop/1
        ,noop_v/1
        ]).
-export([fetch/1
        ,fetch_v/1
        ]).
-export([respond/1
        ,respond_v/1
        ]).
-export([redirect/1
        ,redirect_v/1
        ]).
-export([progress/1
        ,progress_v/1
        ]).
-export([ring/1
        ,ring_v/1
        ]).
-export([receive_fax/1
        ,receive_fax_v/1
        ]).
-export([store_fax/1
        ,store_fax_v/1
        ]).
-export([execute_extension/1
        ,execute_extension_v/1
        ]).
-export([break/1
        ,break_v/1
        ]).
-export([play/1
        ,play_v/1
        ]).
-export([playstop/1
        ,playstop_v/1
        ]).
-export([playseek/1
        ,playseek_v/1
        ]).
-export([tts/1
        ,tts_v/1
        ]).
-export([record/1
        ,record_v/1
        ]).
-export([record_call/1
        ,record_call_v/1
        ]).
-export([answer/1
        ,answer_v/1
        ]).
-export([echo/1
        ,echo_v/1
        ]).
-export([privacy/1
        ,privacy_v/1
        ]).
-export([hold/1
        ,hold_v/1
        ]).
-export([soft_hold/1
        ,soft_hold_v/1
        ]).
-export([park/1
        ,park_v/1
        ]).
-export([play_and_collect_digits/1
        ,play_and_collect_digits_v/1
        ]).
-export([call_pickup/1
        ,call_pickup_v/1
        ]).
-export([connect_leg/1
        ,connect_leg_v/1
        ]).
-export([eavesdrop/1
        ,eavesdrop_v/1
        ]).
-export([hangup/1
        ,hangup_v/1
        ]).
-export([say/1
        ,say_v/1
        ]).
-export([sleep/1
        ,sleep_v/1
        ]).
-export([tone_detect/1
        ,tone_detect_v/1
        ]).
-export([set/1
        ,set_v/1
        ]).
-export([set_terminators/1
        ,set_terminators_v/1
        ]).
-export([send_dtmf/1
        ,send_dtmf_v/1
        ]).
-export([recv_dtmf/1
        ,recv_dtmf_v/1
        ]).
-export([tones/1
        ,tones_v/1
        ]).
-export([tones_req_tone/1
        ,tones_req_tone_v/1
        ]).
-export([conference/1
        ,conference_v/1
        ]).
-export([originate_ready/1
        ,originate_ready_v/1
        ,publish_originate_ready/2
        ,publish_originate_ready/3
        ]).
-export([originate_execute/1
        ,originate_execute_v/1
        ,publish_originate_execute/2
        ,publish_originate_execute/3
        ]).
-export([fax_detection/1
        ,fax_detection_v/1
        ]).
-export([store_vm/1
        ,store_vm_v/1
        ]).
-export([audio_level/1
        ,audio_level_v/1
        ]).
-export([transfer/1
        ,transfer_v/1
        ]).
-export([media_macro/1
        ,media_macro_v/1
        ]).
-export([play_macro/1
        ,play_macro_v/1
        ]).
-export([sound_touch/1
        ,sound_touch_v/1
        ]).
-export([hold_control/1
        ,hold_control_v/1
        ]).
-export([event_actions/1
        ,event_actions_v/1
        ]).
-export([queue/1
        ,queue_v/1
        ]).
-export([error/1
        ,error_v/1
        ,publish_error/2
        ,publish_error/3
        ]).

-export([build_command/1
        ,build_command/2
        ]).

%% API Helpers
-export([dial_method_single/0
        ,dial_method_simultaneous/0
        ,terminators/1, terminators_v/1
        ,offsite_store_url/2
        ,application_name/1
        ]).

-export([bind_q/2
        ,unbind_q/2
        ]).

-export([declare_exchanges/0]).

-export([publish_action/2, publish_action/3
        ,publish_command/2, publish_command/3
        ]).

-export([tones_req_tone_headers/1
        ,b_leg_events_v/1
        ,continue_on_fail_v/1
        ]).

-include("kz_amqp_util.hrl").
-include("kapi_dialplan.hrl").

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [bridge_definition()
    ,bridge_endpoint_definition()
    ,unbridge_definition()
    ,page_definition()
    ,store_definition()
    ,store_amqp_resp_definition()
    ,store_http_resp_definition()
    ,noop_definition()
    ,fetch_definition()
    ,respond_definition()
    ,redirect_definition()
    ,progress_definition()
    ,ring_definition()
    ,receive_fax_definition()
    ,store_fax_definition()
    ,execute_extension_definition()
    ,break_definition()
    ,play_definition()
    ,playstop_definition()
    ,playseek_definition()
    ,tts_definition()
    ,record_definition()
    ,record_call_definition()
    ,answer_definition()
    ,echo_definition()
    ,privacy_definition()
    ,hold_definition()
    ,soft_hold_definition()
    ,park_definition()
    ,play_and_collect_digits_definition()
    ,call_pickup_definition()
    ,connect_leg_definition()
    ,eavesdrop_definition()
    ,hangup_definition()
    ,say_definition()
    ,sleep_definition()
    ,tone_detect_definition()
    ,set_definition()
    ,set_terminators_definition()
    ,send_dtmf_definition()
    ,recv_dtmf_definition()
    ,tones_definition()
    ,tones_req_tone_definition()
    ,conference_definition()
    ,originate_ready_definition()
    ,originate_execute_definition()
    ,fax_detection_definition()
    ,store_vm_definition()
    ,audio_level_definition()
    ,transfer_definition()
    ,media_macro_definition()
    ,play_macro_definition()
    ,sound_touch_definition()
    ,hold_control_definition()
    ,event_actions_definition()
    ,queue_definition()
    ,error_definition()
    ].

%%------------------------------------------------------------------------------
%% @doc Get API definition of the given `Name'.
%% @see api_definitions/0
%% @end
%%------------------------------------------------------------------------------
-spec api_definition(kz_term:text()) -> kapi_definition:api().
api_definition(Name) when not is_binary(Name) ->
    api_definition(kz_term:to_binary(Name));
api_definition(<<"bridge">>) ->
    bridge_definition();
api_definition(<<"bridge_endpoint">>) ->
    bridge_endpoint_definition();
api_definition(<<"unbridge">>) ->
    unbridge_definition();
api_definition(<<"page">>) ->
    page_definition();
api_definition(<<"store">>) ->
    store_definition();
api_definition(<<"store_amqp_resp">>) ->
    store_amqp_resp_definition();
api_definition(<<"store_http_resp">>) ->
    store_http_resp_definition();
api_definition(<<"noop">>) ->
    noop_definition();
api_definition(<<"fetch">>) ->
    fetch_definition();
api_definition(<<"respond">>) ->
    respond_definition();
api_definition(<<"redirect">>) ->
    redirect_definition();
api_definition(<<"progress">>) ->
    progress_definition();
api_definition(<<"ring">>) ->
    ring_definition();
api_definition(<<"receive_fax">>) ->
    receive_fax_definition();
api_definition(<<"store_fax">>) ->
    store_fax_definition();
api_definition(<<"execute_extension">>) ->
    execute_extension_definition();
api_definition(<<"break">>) ->
    break_definition();
api_definition(<<"play">>) ->
    play_definition();
api_definition(<<"playstop">>) ->
    playstop_definition();
api_definition(<<"playseek">>) ->
    playseek_definition();
api_definition(<<"tts">>) ->
    tts_definition();
api_definition(<<"record">>) ->
    record_definition();
api_definition(<<"record_call">>) ->
    record_call_definition();
api_definition(<<"answer">>) ->
    answer_definition();
api_definition(<<"echo">>) ->
    echo_definition();
api_definition(<<"privacy">>) ->
    privacy_definition();
api_definition(<<"hold">>) ->
    hold_definition();
api_definition(<<"soft_hold">>) ->
    soft_hold_definition();
api_definition(<<"park">>) ->
    park_definition();
api_definition(<<"play_and_collect_digits">>) ->
    play_and_collect_digits_definition();
api_definition(<<"call_pickup">>) ->
    call_pickup_definition();
api_definition(<<"connect_leg">>) ->
    connect_leg_definition();
api_definition(<<"eavesdrop">>) ->
    eavesdrop_definition();
api_definition(<<"hangup">>) ->
    hangup_definition();
api_definition(<<"say">>) ->
    say_definition();
api_definition(<<"sleep">>) ->
    sleep_definition();
api_definition(<<"tone_detect">>) ->
    tone_detect_definition();
api_definition(<<"set">>) ->
    set_definition();
api_definition(<<"set_terminators">>) ->
    set_terminators_definition();
api_definition(<<"send_dtmf">>) ->
    send_dtmf_definition();
api_definition(<<"recv_dtmf">>) ->
    recv_dtmf_definition();
api_definition(<<"tones">>) ->
    tones_definition();
api_definition(<<"tones_req_tone">>) ->
    tones_req_tone_definition();
api_definition(<<"conference">>) ->
    conference_definition();
api_definition(<<"originate_ready">>) ->
    originate_ready_definition();
api_definition(<<"originate_execute">>) ->
    originate_execute_definition();
api_definition(<<"fax_detection">>) ->
    fax_detection_definition();
api_definition(<<"store_vm">>) ->
    store_vm_definition();
api_definition(<<"audio_level">>) ->
    audio_level_definition();
api_definition(<<"transfer">>) ->
    transfer_definition();
api_definition(<<"media_macro">>) ->
    media_macro_definition();
api_definition(<<"play_macro">>) ->
    play_macro_definition();
api_definition(<<"sound_touch">>) ->
    sound_touch_definition();
api_definition(<<"hold_control">>) ->
    hold_control_definition();
api_definition(<<"event_actions">>) ->
    event_actions_definition();
api_definition(<<"queue">>) ->
    queue_definition();
api_definition(<<"error">>) ->
    error_definition().

-spec bridge_definition() -> kapi_definition:api().
bridge_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Bridge a Call">>}
              ,{fun kapi_definition:set_description/2, <<"Bridge a Call">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun bridge/1}
              ,{fun kapi_definition:set_validate_fun/2, fun bridge_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Endpoints">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Asserted-Identity-Name">>
                                                            ,<<"Asserted-Identity-Number">>
                                                            ,<<"Asserted-Identity-Realm">>
                                                            ,<<"B-Leg-Events">>
                                                            ,<<"Bridge-Actions">>
                                                            ,<<"Call-Context">>
                                                            ,<<"Callee-ID-Name">>
                                                            ,<<"Callee-ID-Number">>
                                                            ,<<"Caller-ID-Name">>
                                                            ,<<"Caller-ID-Number">>
                                                            ,<<"Confirm-Cancel-Timeout">>
                                                            ,<<"Confirm-File">>
                                                            ,<<"Confirm-Key">>
                                                            ,<<"Continue-After">>
                                                            ,<<"Continue-On-Fail">>
                                                            ,<<"Custom-Application-Vars">>
                                                            ,<<"Custom-Channel-Vars">>
                                                            ,<<"Custom-SIP-Headers">>
                                                            ,<<"Dial-Endpoint-Method">>
                                                            ,<<"Enable-T38-Fax">>
                                                            ,<<"Enable-T38-Fax-Request">>
                                                            ,<<"Enable-T38-Gateway">>
                                                            ,<<"Enable-T38-Passthrough">>
                                                            ,<<"Export-Bridge-Variables">>
                                                            ,<<"Export-Variables">>
                                                            ,<<"Fail-On-Single-Reject">>
                                                            ,<<"Force-Fax">>
                                                            ,<<"Hold-Media">>
                                                            ,<<"Ignore-Completed-Elsewhere">>
                                                            ,<<"Ignore-Early-Media">>
                                                            ,<<"Ignore-Forward">>
                                                            ,<<"Insert-At">>
                                                            ,<<"Loopback-Bowout">>
                                                            ,<<"Media">>
                                                            ,<<"Outbound-Callee-ID-Name">>
                                                            ,<<"Outbound-Callee-ID-Number">>
                                                            ,<<"Outbound-Caller-ID-Name">>
                                                            ,<<"Outbound-Caller-ID-Number">>
                                                            ,<<"Privacy-Hide-Name">>
                                                            ,<<"Privacy-Hide-Number">>
                                                            ,<<"Privacy-Method">>
                                                            ,<<"Ringback">>
                                                            ,<<"Secure-RTP">>
                                                            ,<<"Simplify-Loopback">>
                                                            ,<<"SIP-Invite-Parameters">>
                                                            ,<<"SIP-Transport">>
                                                            ,<<"Timeout">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"bridge">>}
                ,{<<"Dial-Endpoint-Method">>, [?DIAL_METHOD_SINGLE, ?DIAL_METHOD_SIMUL]}
                ,{<<"Media">>, [<<"process">>, <<"bypass">>, <<"auto">>]}
                ,{<<"SIP-Transport">>, [<<"udp">>, <<"tcp">>, <<"tls">>]}
                ,{<<"Enable-T38-Gateway">>, [<<"self">>, <<"peer">>]}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"B-Leg-Events">>, fun b_leg_events_v/1}
                ,{<<"Continue-On-Fail">>, fun continue_on_fail_v/1}
                ,{<<"Continue-After">>, fun kz_term:is_boolean/1}
                ,{<<"Custom-Application-Vars">>, fun kz_json:is_json_object/1}
                ,{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                ,{<<"Custom-SIP-Headers">>, fun kz_json:is_json_object/1}
                ,{<<"Endpoints">>, fun kz_json:are_json_objects/1}
                ,{<<"SIP-Invite-Parameters">>, fun erlang:is_list/1}
                ,{<<"Secure-RTP">>, fun kz_term:is_boolean/1}
                ,{<<"Bridge-Actions">>, fun kz_json:is_json_object/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec bridge_endpoint_definition() -> kapi_definition:api().
bridge_endpoint_definition() ->
    Setters = [{fun kapi_definition:set_friendly_name/2, <<"Bridge Endpoints">>}
              ,{fun kapi_definition:set_description/2, <<"Bridge Endpoints">>}
              ,{fun kapi_definition:set_build_fun/2, fun bridge_endpoint/1}
              ,{fun kapi_definition:set_validate_fun/2, fun bridge_endpoint_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Invite-Format">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Account-ID">>
                                                            ,<<"Auth-Password">>
                                                            ,<<"Auth-Realm">>
                                                            ,<<"Auth-User">>
                                                            ,<<"Bypass-Media">>
                                                            ,<<"Call-Context">>
                                                            ,<<"Callee-ID-Name">>
                                                            ,<<"Callee-ID-Number">>
                                                            ,<<"Caller-ID-Name">>
                                                            ,<<"Caller-ID-Number">>
                                                            ,<<"Codecs">>
                                                            ,<<"Custom-Application-Vars">>
                                                            ,<<"Custom-Channel-Vars">>
                                                            ,<<"Custom-SIP-Headers">>
                                                            ,<<"Enable-T38-Fax">>
                                                            ,<<"Enable-T38-Fax-Request">>
                                                            ,<<"Enable-T38-Gateway">>
                                                            ,<<"Enable-T38-Passthrough">>
                                                            ,<<"Endpoint-Actions">>
                                                            ,<<"Endpoint-Delay">>
                                                            ,<<"Endpoint-ID">>
                                                            ,<<"Endpoint-Options">>
                                                            ,<<"Endpoint-Progress-Timeout">>
                                                            ,<<"Endpoint-Timeout">>
                                                            ,<<"Endpoint-Type">>
                                                            ,<<"Endpoint-URI">>
                                                            ,<<"Failover">>
                                                            ,<<"Force-Fax">>
                                                            ,<<"Forward-IP">>
                                                            ,<<"Hold-Media">>
                                                            ,<<"Ignore-Completed-Elsewhere">>
                                                            ,<<"Ignore-Early-Media">>
                                                            ,<<"Loopback-Bowout">>
                                                            ,<<"Outbound-Callee-ID-Name">>
                                                            ,<<"Outbound-Callee-ID-Number">>
                                                            ,<<"Outbound-Caller-ID-Name">>
                                                            ,<<"Outbound-Caller-ID-Number">>
                                                            ,<<"Outbound-Call-ID">>
                                                            ,<<"Presence-ID">>
                                                            ,<<"Privacy-Hide-Name">>
                                                            ,<<"Privacy-Hide-Number">>
                                                            ,<<"Privacy-Method">>
                                                            ,<<"Proxy-IP">>
                                                            ,<<"Proxy-Zone">>
                                                            ,<<"Route">>
                                                            ,<<"Simplify-Loopback">>
                                                            ,<<"SIP-Interface">>
                                                            ,<<"SIP-Invite-Parameters">>
                                                            ,<<"SIP-Transport">>
                                                            ,<<"To-DID">>
                                                            ,<<"To-IP">>
                                                            ,<<"To-Realm">>
                                                            ,<<"To-URI">>
                                                            ,<<"To-User">>
                                                            ,<<"To-Username">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[?INVITE_FORMAT_TUPLE
                ,{<<"Endpoint-Type">>, [<<"sip">>, <<"freetdm">>, <<"skype">>]}
                ,{<<"Enable-T38-Gateway">>, [<<"self">>, <<"peer">>]}
                ,{<<"SIP-Transport">>, [<<"udp">>, <<"tcp">>, <<"tls">>, <<"sctp">>]}
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Custom-SIP-Headers">>, fun kz_json:is_json_object/1}
                ,{<<"Custom-Application-Vars">>, fun kz_json:is_json_object/1}
                ,{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                ,{<<"Endpoint-Options">>, fun kz_json:is_json_object/1}
                ,{<<"Ignore-Early-Media">>, fun kz_term:is_boolean/1}
                ,{<<"Bypass-Media">>, fun kz_term:is_boolean/1}
                ,{<<"SIP-Invite-Parameters">>, fun erlang:is_list/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec unbridge_definition() -> kapi_definition:api().
unbridge_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Unbridge a Call">>}
              ,{fun kapi_definition:set_description/2, <<"Unbridge a Call">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun unbridge/1}
              ,{fun kapi_definition:set_validate_fun/2, fun unbridge_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Insert-At">>
                                                            ,<<"Leg">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"unbridge">>}
                ,{<<"Leg">>, [<<"A">>, <<"B">>, <<"Both">>]}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec page_definition() -> kapi_definition:api().
page_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Page Request">>}
              ,{fun kapi_definition:set_description/2, <<"Page a Call">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun page/1}
              ,{fun kapi_definition:set_validate_fun/2, fun page_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Endpoints">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Callee-ID-Name">>
                                                            ,<<"Callee-ID-Number">>
                                                            ,<<"Caller-ID-Name">>
                                                            ,<<"Caller-ID-Number">>
                                                            ,<<"Custom-Channel-Vars">>
                                                            ,<<"Custom-SIP-Headers">>
                                                            ,<<"Insert-At">>
                                                            ,<<"Page-Options">>
                                                            ,<<"Timeout">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"page">>}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Endpoints">>, fun erlang:is_list/1}
                ,{<<"Custom-SIP-Headers">>, fun kz_json:is_json_object/1}
                ,{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec store_definition() -> kapi_definition:api().
store_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Store Request">>}
              ,{fun kapi_definition:set_description/2, <<"Store Request">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun store/1}
              ,{fun kapi_definition:set_validate_fun/2, fun store_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Media-Name">>
                                                            ,<<"Media-Transfer-Destination">>
                                                            ,<<"Media-Transfer-Method">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Additional-Headers">>
                                                            ,<<"Insert-At">>
                                                            ,<<"Suppress-Error-Report">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"store">>}
                ,{<<"Media-Transfer-Method">>, [<<"stream">>, <<"put">>, <<"post">>]}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Additional-Headers">>, fun erlang:is_list/1}
                ,{<<"Suppress-Error-Report">>, fun kz_term:is_boolean/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec store_amqp_resp_definition() -> kapi_definition:api().
store_amqp_resp_definition() ->
    Setters = [{fun kapi_definition:set_friendly_name/2, <<"Store AMQP Response">>}
              ,{fun kapi_definition:set_description/2, <<"Store (via AMQP) Response">>}
              ,{fun kapi_definition:set_build_fun/2, fun store_amqp_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun store_amqp_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Media-Content">>
                                                            ,<<"Media-Name">>
                                                            ,<<"Media-Transfer-Method">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Media-Sequence-ID">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"store">>}
                ,{<<"Media-Transfer-Method">>, <<"stream">>}
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Media-Content">>, fun store_media_content_v/1}
                ,{<<"Media-Name">>, fun is_binary/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec store_http_resp_definition() -> kapi_definition:api().
store_http_resp_definition() ->
    EventName = <<"response">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Store HTTP Response">>}
              ,{fun kapi_definition:set_description/2, <<"Store (via HTTP) Response">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun store_http_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun store_http_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Media-Name">>
                                                            ,<<"Media-Transfer-Method">>
                                                            ,<<"Media-Transfer-Results">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"store">>}
                ,{<<"Media-Transfer-Method">>, [<<"put">>, <<"post">>]}
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Media-Transfer-Results">>, fun kz_json:is_json_object/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

%% NoOp Request
%% Filter-Applications: will remove applications in the ecallmgr command queue matching those in this list
%% So, if you want to remove Play commands, set Filter-Applications = [<<"play">>]. This will filter
%% the command queue until a non-Play command is encountered.
%% Alternatively, you can specify the elements in the Filter-Applications list as:
%% [ {"Application-Name":"play", "Fields":{"Terminators":["#"]}}, ...]
%% This says, filter Play commands terminate-able with the "#" key
%%
%% IMPORTANT: to use the filter-applications list, Insert-At must be "now"
-spec noop_definition() -> kapi_definition:api().
noop_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"NoOp Request">>}
              ,{fun kapi_definition:set_description/2, <<"Format a Dialplan: noop API call">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun noop/1}
              ,{fun kapi_definition:set_validate_fun/2, fun noop_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"B-Leg-Events">>
                                                            ,<<"Filter-Applications">>
                                                            ,<<"Insert-At">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"noop">>}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"B-Leg-Events">>, fun b_leg_events_v/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec fetch_definition() -> kapi_definition:api().
fetch_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Fetch">>}
              ,{fun kapi_definition:set_description/2, <<"Fetch Custom Channel variables">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun fetch/1}
              ,{fun kapi_definition:set_validate_fun/2, fun fetch_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"From-Other-Leg">>
                                                            ,<<"Insert-At">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"fetch">>}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"From-Other-Leg">>, fun kz_term:is_boolean/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec respond_definition() -> kapi_definition:api().
respond_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Respond">>}
              ,{fun kapi_definition:set_description/2, <<"Respond a session">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun respond/1}
              ,{fun kapi_definition:set_validate_fun/2, fun respond_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Response-Code">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Insert-At">>
                                                            ,<<"Response-Message">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"respond">>}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Response-Code">>, fun is_binary/1}
                ,{<<"Response-Message">>, fun is_binary/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec redirect_definition() -> kapi_definition:api().
redirect_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Redirect">>}
              ,{fun kapi_definition:set_description/2, <<"Redirect a session">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun redirect/1}
              ,{fun kapi_definition:set_validate_fun/2, fun redirect_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Redirect-Contact">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Insert-At">>
                                                            ,<<"Redirect-Node">>
                                                            ,<<"Redirect-Server">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"redirect">>}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Redirect-Contact">>, fun is_binary/1}
                ,{<<"Redirect-Node">>, fun is_binary/1}
                ,{<<"Redirect-Server">>, fun is_binary/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec progress_definition() -> kapi_definition:api().
progress_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Progress">>}
              ,{fun kapi_definition:set_description/2, <<"Progress a session">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun progress/1}
              ,{fun kapi_definition:set_validate_fun/2, fun progress_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Insert-At">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"progress">>}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec ring_definition() -> kapi_definition:api().
ring_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Ring">>}
              ,{fun kapi_definition:set_description/2, <<"Ring a session">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun ring/1}
              ,{fun kapi_definition:set_validate_fun/2, fun ring_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Insert-At">>
                                                            ,<<"Ringback">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"ring">>}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec receive_fax_definition() -> kapi_definition:api().
receive_fax_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Receive Fax">>}
              ,{fun kapi_definition:set_description/2, <<"Receive a fax, storing it to local disk">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun receive_fax/1}
              ,{fun kapi_definition:set_validate_fun/2, fun receive_fax_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Enable-T38-Fax">>
                                                            ,<<"Enable-T38-Fax-Request">>
                                                            ,<<"Enable-T38-Gateway">>
                                                            ,<<"Enable-T38-Passthrough">>
                                                            ,<<"Fax-Local-Filename">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"receive_fax">>}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec store_fax_definition() -> kapi_definition:api().
store_fax_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Store Fax">>}
              ,{fun kapi_definition:set_description/2, <<"Store a fax, storing it to the DB">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun store_fax/1}
              ,{fun kapi_definition:set_validate_fun/2, fun store_fax_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Media-Transfer-Destination">>
                                                            ,<<"Media-Transfer-Method">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Additional-Headers">>
                                                            ,<<"Fax-Local-Filename">>
                                                            ,<<"Insert-At">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"store_fax">>}
                ,{<<"Media-Transfer-Method">>, <<"put">>}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Additional-Headers">>, fun erlang:is_list/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec execute_extension_definition() -> kapi_definition:api().
execute_extension_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Execute_Extension">>}
              ,{fun kapi_definition:set_description/2, <<"Execute_Extension a session">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun execute_extension/1}
              ,{fun kapi_definition:set_validate_fun/2, fun execute_extension_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Extension">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Custom-Channel-Vars">>
                                                            ,<<"Insert-At">>
                                                            ,<<"Reset">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"execute_extension">>}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Extension">>, fun is_binary/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec break_definition() -> kapi_definition:api().
break_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Break">>}
              ,{fun kapi_definition:set_description/2, <<"Moves to the next step in callflow">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun break/1}
              ,{fun kapi_definition:set_validate_fun/2, fun break_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Insert-At">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"break">>}
                ,{<<"Insert-At">>, <<"now">>}
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec play_definition() -> kapi_definition:api().
play_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Play">>}
              ,{fun kapi_definition:set_description/2, <<"Play media">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun play/1}
              ,{fun kapi_definition:set_validate_fun/2, fun play_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Media-Name">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Endless-Playback">>
                                                            ,<<"Format">>
                                                            ,<<"Group-ID">> % group media together (one DTMF cancels all in group)
                                                            ,<<"Insert-At">>
                                                            ,<<"Language">>
                                                            ,<<"Leg">>
                                                            ,<<"Loop-Count">>
                                                            ,<<"Terminators">>
                                                            ,<<"Voice">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"play">>}
                ,{<<"Leg">>, [<<"A">>, <<"B">>, <<"Both">>]}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Endless-Playback">>, fun kz_term:is_boolean/1}
                ,{<<"Loop-Count">>, fun kz_term:is_pos_integer/1}
                ,{<<"Terminators">>, ?IS_TERMINATOR}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec playstop_definition() -> kapi_definition:api().
playstop_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"PlayStop">>}
              ,{fun kapi_definition:set_description/2, <<"Stop media from playing">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun playstop/1}
              ,{fun kapi_definition:set_validate_fun/2, fun playstop_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Insert-At">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"playstop">>}
                ,{<<"Insert-At">>, <<"now">>}
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec playseek_definition() -> kapi_definition:api().
playseek_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"PlaySeek">>}
              ,{fun kapi_definition:set_description/2, <<"Change position in playing media">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun playseek/1}
              ,{fun kapi_definition:set_validate_fun/2, fun playseek_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Direction">>
                                                            ,<<"Duration">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Insert-At">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"playseek">>}
                ,{<<"Insert-At">>, <<"now">>}
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Duration">>, fun is_integer/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec tts_definition() -> kapi_definition:api().
tts_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"TTS">>}
              ,{fun kapi_definition:set_description/2, <<"TTS - Text-to-speech">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun tts/1}
              ,{fun kapi_definition:set_validate_fun/2, fun tts_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Text">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Conference-ID">>
                                                            ,<<"Endless-Playback">>
                                                            ,<<"Engine">>
                                                            ,<<"Group-ID">> % group media together (one DTMF cancels all in group)
                                                            ,<<"Insert-At">>
                                                            ,<<"Language">>
                                                            ,<<"Leg">>
                                                            ,<<"Loop-Count">>
                                                            ,<<"Terminators">>
                                                            ,<<"Voice">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"tts">>}
                ,{<<"Voice">>, [<<"male">>, <<"female">>]}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Endless-Playback">>, fun kz_term:is_boolean/1}
                ,{<<"Loop-Count">>, fun kz_term:is_pos_integer/1}
                ,{<<"Terminators">>, ?IS_TERMINATOR}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec record_definition() -> kapi_definition:api().
record_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Record">>}
              ,{fun kapi_definition:set_description/2, <<"Record media">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun record/1}
              ,{fun kapi_definition:set_validate_fun/2, fun record_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Media-Name">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Insert-At">>
                                                            ,<<"Silence-Hits">>
                                                            ,<<"Silence-Threshold">>
                                                            ,<<"Terminators">>
                                                            ,<<"Time-Limit">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"record">>}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Terminators">>, ?IS_TERMINATOR}
                ]
               }
              ],
    kapi_definition:setters(Setters).

%% Stream-To = local -> results in the recording being stored on the media server.
%% Stream-To = remote -> will stream the recording to the handling ecallmgr server.
-spec record_call_definition() -> kapi_definition:api().
record_call_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Record Call">>}
              ,{fun kapi_definition:set_description/2, <<"Record call media">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun record_call/1}
              ,{fun kapi_definition:set_validate_fun/2, fun record_call_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Record-Action">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Additional-Headers">>
                                                            ,<<"Channels-As-Stereo">>
                                                            ,<<"Follow-Transfer">>
                                                            ,<<"Insert-At">>
                                                            ,<<"Media-Name">>
                                                            ,<<"Media-Recorder">>
                                                            ,<<"Media-Recording-Endpoint-ID">>
                                                            ,<<"Media-Recording-ID">>
                                                            ,<<"Media-Recording-Origin">>
                                                            ,<<"Media-Transfer-Destination">>
                                                            ,<<"Media-Transfer-Method">>
                                                            ,<<"Recording-Variables">>
                                                            ,<<"Record-Min-Sec">>
                                                            ,<<"Record-Sample-Rate">>
                                                            ,<<"Time-Limit">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"record_call">>}
                ,{<<"Record-Action">>, [<<"start">>, <<"stop">>, <<"mask">>, <<"unmask">>]}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Record-Sample-Rate">>, fun is_integer/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec answer_definition() -> kapi_definition:api().
answer_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Answer">>}
              ,{fun kapi_definition:set_description/2, <<"Answer a session">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun answer/1}
              ,{fun kapi_definition:set_validate_fun/2, fun answer_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Insert-At">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"answer">>}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec echo_definition() -> kapi_definition:api().
echo_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Echo">>}
              ,{fun kapi_definition:set_description/2, <<"Echo a session">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun echo/1}
              ,{fun kapi_definition:set_validate_fun/2, fun echo_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Insert-At">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"echo">>}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec privacy_definition() -> kapi_definition:api().
privacy_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Privacy">>}
              ,{fun kapi_definition:set_description/2, <<"Privacy">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun privacy/1}
              ,{fun kapi_definition:set_validate_fun/2, fun privacy_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Insert-At">>
                                                            ,<<"Privacy-Mode">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"privacy">>}
                ,{<<"Privacy-Mode">>, [<<"full">>, <<"name">>, <<"number">>, <<"none">>]}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec hold_definition() -> kapi_definition:api().
hold_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Hold">>}
              ,{fun kapi_definition:set_description/2, <<"Hold a call">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun hold/1}
              ,{fun kapi_definition:set_validate_fun/2, fun hold_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Hold-Media">>
                                                            ,<<"Insert-At">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"hold">>}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec soft_hold_definition() -> kapi_definition:api().
soft_hold_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Soft Hold">>}
              ,{fun kapi_definition:set_description/2, <<"Soft Hold">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun soft_hold/1}
              ,{fun kapi_definition:set_validate_fun/2, fun soft_hold_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Unhold-Key">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"A-MOH">>
                                                            ,<<"B-MOH">>
                                                            ,<<"Insert-At">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"soft_hold">>}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec park_definition() -> kapi_definition:api().
park_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Park">>}
              ,{fun kapi_definition:set_description/2, <<"Park a call">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun park/1}
              ,{fun kapi_definition:set_validate_fun/2, fun park_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Hangup-Cause">>
                                                            ,<<"Insert-At">>
                                                            ,<<"Timeout">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"park">>}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec play_and_collect_digits_definition() -> kapi_definition:api().
play_and_collect_digits_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Play and Record Digits">>}
              ,{fun kapi_definition:set_description/2, <<"Play media and record digits">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun play_and_collect_digits/1}
              ,{fun kapi_definition:set_validate_fun/2, fun play_and_collect_digits_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Digits-Regex">>
                                                            ,<<"Maximum-Digits">>
                                                            ,<<"Media-Name">>
                                                            ,<<"Media-Tries">>
                                                            ,<<"Minimum-Digits">>
                                                            ,<<"Terminators">>
                                                            ,<<"Timeout">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Failed-Media-Name">>
                                                            ,<<"Insert-At">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"play_and_collect_digits">>}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Terminators">>, ?IS_TERMINATOR}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec call_pickup_definition() -> kapi_definition:api().
call_pickup_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Call Pickup">>}
              ,{fun kapi_definition:set_description/2, <<"Pickup a call">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun call_pickup/1}
              ,{fun kapi_definition:set_validate_fun/2, fun call_pickup_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Target-Call-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Continue-On-Cancel">>
                                                            ,<<"Continue-On-Fail">>
                                                            ,<<"Hangup-After-Pickup">>
                                                            ,<<"Insert-At">>
                                                            ,<<"Move-Channel-If-Necessary">>
                                                            ,<<"Other-Leg">>
                                                            ,<<"Park-After-Pickup">> %% Will park either leg after cancel
                                                            ,<<"Unanswered-Only">>
                                                            ,<<"Unbridged-Only">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"call_pickup">>}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Hangup-After-Pickup">>, fun kz_term:is_boolean/1}
                ,{<<"Move-Channel-If-Necessary">>, fun kz_term:is_boolean/1}
                ,{<<"Park-After-Pickup">>, fun kz_term:is_boolean/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec connect_leg_definition() -> kapi_definition:api().
connect_leg_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Connect Leg">>}
              ,{fun kapi_definition:set_description/2, <<"Connect a leg to the current leg">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun connect_leg/1}
              ,{fun kapi_definition:set_validate_fun/2, fun connect_leg_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Target-Call-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"B-Leg-Events">>
                                                            ,<<"Continue-On-Cancel">>
                                                            ,<<"Continue-On-Fail">>
                                                            ,<<"Hangup-After-Pickup">>
                                                            ,<<"Insert-At">>
                                                            ,<<"Move-Channel-If-Necessary">>
                                                            ,<<"Other-Leg">>
                                                            ,<<"Park-After-Pickup">> %% Will park either leg after cancel
                                                            ,<<"Publish-Usurp">>
                                                            ,<<"Unanswered-Only">>
                                                            ,<<"Unbridged-Only">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"connect_leg">>}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"B-Leg-Events">>, fun b_leg_events_v/1}
                ,{<<"Hangup-After-Pickup">>, fun kz_term:is_boolean/1}
                ,{<<"Move-Channel-If-Necessary">>, fun kz_term:is_boolean/1}
                ,{<<"Park-After-Pickup">>, fun kz_term:is_boolean/1}
                ,{<<"Publish-Usurp">>, fun kz_term:is_boolean/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec eavesdrop_definition() -> kapi_definition:api().
eavesdrop_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Eavesdrop">>}
              ,{fun kapi_definition:set_description/2, <<"Eavesdrop">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun eavesdrop/1}
              ,{fun kapi_definition:set_validate_fun/2, fun eavesdrop_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Target-Call-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Continue-On-Cancel">>
                                                            ,<<"Continue-On-Fail">>
                                                            ,<<"Enable-DTMF">>
                                                            ,<<"Insert-At">>
                                                            ,<<"Move-Channel-If-Necessary">>
                                                            ,<<"Other-Leg">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"eavesdrop">>}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Move-Channel-If-Necessary">>, fun kz_term:is_boolean/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

%% Include the Other-Leg-Call-ID to only hangup the other leg
-spec hangup_definition() -> kapi_definition:api().
hangup_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Hangup">>}
              ,{fun kapi_definition:set_description/2, <<"Hangup a call">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun hangup/1}
              ,{fun kapi_definition:set_validate_fun/2, fun hangup_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Hangup-Cause">>
                                                            ,<<"Insert-At">>
                                                            ,<<"Other-Leg-Only">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"hangup">>}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Hangup-Cause">>, fun is_binary/1}
                ,{<<"Other-Leg-Only">>, fun kz_term:is_boolean/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec say_definition() -> kapi_definition:api().
say_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Say">>}
              ,{fun kapi_definition:set_description/2, <<"Say - convert text to speech">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun say/1}
              ,{fun kapi_definition:set_validate_fun/2, fun say_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Language">>
                                                            ,<<"Method">>
                                                            ,<<"Say-Text">>
                                                            ,<<"Type">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Group-ID">>
                                                            ,<<"Insert-At">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"say">>}
                ,{<<"Type">>, [<<"account_number">>
                              ,<<"currency">>
                              ,<<"current_date">>
                              ,<<"current_date_time">>
                              ,<<"current_time">>
                              ,<<"e-mail_address">>
                              ,<<"ip_address">>
                              ,<<"items">>
                              ,<<"messages">>
                              ,<<"name_phonetic">>
                              ,<<"name_spelled">>
                              ,<<"number">>
                              ,<<"persons">>
                              ,<<"postal_address">>
                              ,<<"short_date_time">>
                              ,<<"telephone_extension">>
                              ,<<"telephone_number">>
                              ,<<"time_measurement">>
                              ,<<"url">>
                              ]}
                ,{<<"Method">>, [<<"none">>, <<"pronounced">>, <<"iterated">>, <<"counted">>]}
                ,{<<"Gender">>, [<<"masculine">>, <<"feminine">>, <<"neuter">>]}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec sleep_definition() -> kapi_definition:api().
sleep_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Sleep">>}
              ,{fun kapi_definition:set_description/2, <<"Sleep - Pauses execution">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun sleep/1}
              ,{fun kapi_definition:set_validate_fun/2, fun sleep_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Time">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Insert-At">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"sleep">>}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec tone_detect_definition() -> kapi_definition:api().
tone_detect_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Tone Detect">>}
              ,{fun kapi_definition:set_description/2, <<"Detect tones on the line">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun tone_detect/1}
              ,{fun kapi_definition:set_validate_fun/2, fun tone_detect_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Frequencies">>
                                                            ,<<"Tone-Detect-Name">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Hits-Needed">>
                                                            ,<<"Insert-At">>
                                                            ,<<"On-Success">>
                                                            ,<<"Sniff-Direction">>
                                                            ,<<"Timeout">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"tone_detect">>}
                ,{<<"Sniff-Direction">>, [<<"read">>, <<"write">>]}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"On-Success">>, fun erlang:is_list/1}
                ,{<<"Timeout">>, fun tone_timeout_v/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec set_definition() -> kapi_definition:api().
set_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Set">>}
              ,{fun kapi_definition:set_description/2, <<"Set Custom Channel variables">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun set/1}
              ,{fun kapi_definition:set_validate_fun/2, fun set_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Custom-Call-Vars">>
                                                            ,<<"Custom-Channel-Vars">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Custom-Application-Vars">>
                                                            ,<<"Export-All">>
                                                            ,<<"Insert-At">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"set">>}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Custom-Application-Vars">>, fun kz_json:is_json_object/1}
                ,{<<"Custom-Channel-Vars">>,fun kz_json:is_json_object/1}
                ,{<<"Custom-Call-Vars">>, fun kz_json:is_json_object/1}
                ,{<<"Export-All">>, fun is_boolean/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec set_terminators_definition() -> kapi_definition:api().
set_terminators_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Set Terminators">>}
              ,{fun kapi_definition:set_description/2, <<"Set Terminators for playback/record">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun set_terminators/1}
              ,{fun kapi_definition:set_validate_fun/2, fun set_terminators_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Terminators">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Insert-At">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"set_terminators">>}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Terminators">>, ?IS_TERMINATOR}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec send_dtmf_definition() -> kapi_definition:api().
send_dtmf_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Send DTMF">>}
              ,{fun kapi_definition:set_description/2, <<"Create a DTMF (or DTMFs) on the channel">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun send_dtmf/1}
              ,{fun kapi_definition:set_validate_fun/2, fun send_dtmf_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"DTMFs">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Duration">>
                                                            ,<<"Insert-At">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"send_dtmf">>}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"DTMFs">>, fun is_binary/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec recv_dtmf_definition() -> kapi_definition:api().
recv_dtmf_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Receive DTMF">>}
              ,{fun kapi_definition:set_description/2, <<"Receive DTMF">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun recv_dtmf/1}
              ,{fun kapi_definition:set_validate_fun/2, fun recv_dtmf_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"DTMFs">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Insert-At">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"recv_dtmf">>}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"DTMFs">>, fun is_binary/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec tones_definition() -> kapi_definition:api().
tones_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Tones">>}
              ,{fun kapi_definition:set_description/2, <<"Create a tone on the channel">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun tones/1}
              ,{fun kapi_definition:set_validate_fun/2, fun tones_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Tones">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Conference-ID">>
                                                            ,<<"Group-ID">>
                                                            ,<<"Insert-At">>
                                                            ,<<"Terminators">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"tones">>}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Tones">>, fun erlang:is_list/1}
                ,{<<"Terminators">>, ?IS_TERMINATOR}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec tones_req_tone_definition() -> kapi_definition:api().
tones_req_tone_definition() ->
    Setters = [{fun kapi_definition:set_friendly_name/2, <<"Tone within Tones request">>}
              ,{fun kapi_definition:set_description/2, <<"A Tone within a Tones request">>}
              ,{fun kapi_definition:set_build_fun/2, fun tones_req_tone/1}
              ,{fun kapi_definition:set_validate_fun/2, fun tones_req_tone_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Duration-OFF">>
                                                            ,<<"Duration-ON">>
                                                            ,<<"Frequencies">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Repeat">>
                                                            ,<<"Volume">>
                                                            ]}
              ,{fun kapi_definition:set_values/2, []}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec conference_definition() -> kapi_definition:api().
conference_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Conference">>}
              ,{fun kapi_definition:set_description/2
               ,<<"Conference - Sends caller to a conference">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun conference/1}
              ,{fun kapi_definition:set_validate_fun/2, fun conference_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Conference-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Deaf">>
                                                            ,<<"Insert-At">>
                                                            ,<<"Member-Nospeak">> %% sets joining member to nospeak relations
                                                            ,<<"Moderator">>
                                                            ,<<"Mute">>
                                                            ,<<"Nospeak-Check">> %% update relations
                                                            ,<<"Profile">>
                                                            ,<<"Reinvite">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"conference">>}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Call-ID">>, fun is_binary/1}
                ,{<<"Conference-ID">>, fun is_binary/1}
                ,{<<"Mute">>, fun kz_term:is_boolean/1}
                ,{<<"Deaf">>, fun kz_term:is_boolean/1}
                ,{<<"Moderator">>, fun kz_term:is_boolean/1}
                ,{<<"Reinvite">>, fun kz_term:is_boolean/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec originate_ready_definition() -> kapi_definition:api().
originate_ready_definition() ->
    EventName = <<"originate_ready">>,
    Category = <<"dialplan">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Originate Ready">>}
              ,{fun kapi_definition:set_description/2
               ,<<"Send Requestor a message that the originate is ready to execute">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun originate_ready/1}
              ,{fun kapi_definition:set_validate_fun/2, fun originate_ready_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_originate_ready/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Call-ID">>
                                                            ,<<"Control-Queue">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec originate_execute_definition() -> kapi_definition:api().
originate_execute_definition() ->
    EventName = <<"originate_execute">>,
    Category = <<"dialplan">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Originate Execute">>}
              ,{fun kapi_definition:set_description/2
               ,<<"Wait for the Requestor to respond to execute the origination">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun originate_execute/1}
              ,{fun kapi_definition:set_validate_fun/2, fun originate_execute_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_originate_execute/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Call-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2
               ,[]
               }
              ],
    kapi_definition:setters(Setters).

-spec fax_detection_definition() -> kapi_definition:api().
fax_detection_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Fax Detection">>}
              ,{fun kapi_definition:set_description/2, <<"Detect fax on the line">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun fax_detection/1}
              ,{fun kapi_definition:set_validate_fun/2, fun fax_detection_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Action">>
                                                            ,<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Action">>
                                                            ,<<"Direction">>
                                                            ,<<"Duration">>
                                                            ,<<"Insert-At">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"fax_detection">>}
                ,{<<"Direction">>, [<<"inbound">>, <<"outbound">>]}
                ,{<<"Action">>, [<<"start">>, <<"stop">>]}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec store_vm_definition() -> kapi_definition:api().
store_vm_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Store VM">>}
              ,{fun kapi_definition:set_description/2, <<"Store VoiceMail">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun store_vm/1}
              ,{fun kapi_definition:set_validate_fun/2, fun store_vm_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Media-Name">>
                                                            ,<<"Media-Transfer-Destination">>
                                                            ,<<"Media-Transfer-Method">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Additional-Headers">>
                                                            ,<<"Insert-At">>
                                                            ,<<"Suppress-Error-Report">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"store_vm">>}
                ,{<<"Media-Transfer-Method">>, [<<"stream">>, <<"put">>, <<"post">>]}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Additional-Headers">>, fun erlang:is_list/1}
                ,{<<"Suppress-Error-Report">>, fun kz_term:is_boolean/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec audio_level_definition() -> kapi_definition:api().
audio_level_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Audio level">>}
              ,{fun kapi_definition:set_description/2, <<"Audio level/mute">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun audio_level/1}
              ,{fun kapi_definition:set_validate_fun/2, fun audio_level_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Action">>
                                                            ,<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Insert-At">>
                                                            ,<<"Level">>
                                                            ,<<"Mode">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"audio_level">>}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec transfer_definition() -> kapi_definition:api().
transfer_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Transfer">>}
              ,{fun kapi_definition:set_description/2, <<"Transfer">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun transfer/1}
              ,{fun kapi_definition:set_validate_fun/2, fun transfer_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Transfer-To">>
                                                            ,<<"Transfer-Type">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Attended-Transfer-Keys">>
                                                            ,<<"Caller-ID-Name">>
                                                            ,<<"Caller-ID-Number">>
                                                            ,<<"Custom-Channel-Vars">>
                                                            ,<<"Insert-At">>
                                                            ,<<"Transfer-Context">>
                                                            ,<<"Transfer-Leg">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"transfer">>}
                ,{<<"Transfer-Type">>, [<<"blind">>, <<"attended">>]}
                ,{<<"Transfer-Leg">>, [<<"bleg">>, <<"both">>]}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Call-ID">>, fun is_binary/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec media_macro_definition() -> kapi_definition:api().
media_macro_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Media macro">>}
              ,{fun kapi_definition:set_description/2, <<"Media macro">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun media_macro/1}
              ,{fun kapi_definition:set_validate_fun/2, fun media_macro_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Media-Macros">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Insert-At">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"media_macro">>}
                ,{<<"Insert-At">>, <<"now">>}
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Call-ID">>, fun is_binary/1}
                ,{<<"Media-Macros">>, fun kz_json:is_json_object/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec play_macro_definition() -> kapi_definition:api().
play_macro_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Play macro">>}
              ,{fun kapi_definition:set_description/2, <<"Play macro">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun play_macro/1}
              ,{fun kapi_definition:set_validate_fun/2, fun play_macro_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Media-Macro">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Insert-At">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"play_macro">>}
                ,{<<"Insert-At">>, <<"now">>}
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Call-ID">>, fun is_binary/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec sound_touch_definition() -> kapi_definition:api().
sound_touch_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Sound touch">>}
              ,{fun kapi_definition:set_description/2, <<"Sound touch">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun sound_touch/1}
              ,{fun kapi_definition:set_validate_fun/2, fun sound_touch_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Action">>
                                                            ,<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Insert-At">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Adjust-In-Octaves">>
                                                            ,<<"Adjust-In-Semitones">>
                                                            ,<<"Hook-DTMF">>
                                                            ,<<"Pitch">>
                                                            ,<<"Rate">>
                                                            ,<<"Sending-Leg">>
                                                            ,<<"Tempo">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"sound_touch">>}
                ,{<<"Action">>, [<<"start">>, <<"stop">>]}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Hook-DTMF">>, fun is_boolean/1}
                ,{<<"Sending-Leg">>, fun is_boolean/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec hold_control_definition() -> kapi_definition:api().
hold_control_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Hold Control">>}
              ,{fun kapi_definition:set_description/2, <<"Hold Control">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun hold_control/1}
              ,{fun kapi_definition:set_validate_fun/2, fun hold_control_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Action">>
                                                            ,<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Insert-At">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"hold_control">>}
                ,{<<"Action">>, [<<"hold">>, <<"unhold">>, <<"toggle">>]}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec event_actions_definition() -> kapi_definition:api().
event_actions_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Event Actions">>}
              ,{fun kapi_definition:set_description/2, <<"Event Actions">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun event_actions/1}
              ,{fun kapi_definition:set_validate_fun/2, fun event_actions_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Event-Actions">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Insert-At">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"event_actions">>}
                ,{<<"Insert-At">>, <<"now">>}
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Event-Actions">>, fun kz_json:is_json_object/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec queue_definition() -> kapi_definition:api().
queue_definition() ->
    EventName = <<"command">>,
    Category = <<"call">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Queue">>}
              ,{fun kapi_definition:set_description/2
               ,<<"Send a list of dialplan applications in bulk">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun queue/1}
              ,{fun kapi_definition:set_validate_fun/2, fun queue_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_command/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Commands">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Insert-At">>]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, <<"queue">>}
                ,?INSERT_AT_TUPLE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Commands">>, fun erlang:is_list/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec error_definition() -> kapi_definition:api().
error_definition() ->
    EventName = <<"dialplan">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Error">>}
              ,{fun kapi_definition:set_description/2, <<"Error - Sends error to Queue">>}
              ,{fun kapi_definition:set_build_fun/2, fun error/1}
              ,{fun kapi_definition:set_validate_fun/2, fun error_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_error/2}
              ,{fun kapi_definition:set_binding/2, fun kapi_call:event_routing_key/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Channel-Call-State">>
                                                            ,<<"Channel-State">>
                                                            ,<<"Dialplan-Error">>
                                                                 | ?ERROR_RESP_HEADERS
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Custom-Channel-Vars">>
                                                                 | ?OPTIONAL_ERROR_RESP_HEADERS
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Event-Name">>, EventName}
                 | ?ERROR_RESP_VALUES
                ]
               }
              ,{fun kapi_definition:set_types/2, ?ERROR_RESP_TYPES}
              ],
    kapi_definition:setters(Setters).

-spec optional_bridge_req_headers() -> kz_term:ne_binaries().
optional_bridge_req_headers() ->
    kapi_definition:optional_headers(bridge_definition()).

-spec optional_bridge_req_endpoint_headers() -> kz_term:ne_binaries().
optional_bridge_req_endpoint_headers() ->
    kapi_definition:optional_headers(bridge_endpoint_definition()).

-spec b_leg_events_v(kz_term:ne_binaries()) -> boolean().
b_leg_events_v(Events) ->
    lists:all(fun(ApiEvent) ->
                      lists:member(ApiEvent, ?CALL_EVENTS)
              end, Events).

-spec continue_on_fail_v(kz_term:ne_binaries() | boolean()) -> boolean().
continue_on_fail_v(Val)
  when is_list(Val) ->
    lists:all(fun(V) -> kz_term:is_ne_binary(V) end, Val);
continue_on_fail_v(Val)
  when is_boolean(Val) -> 'true';
continue_on_fail_v(_Val) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc Takes a generic API JObj, determines what type it is, and calls
%% the appropriate validator.
%% @end
%%------------------------------------------------------------------------------
-spec v(kz_term:api_terms()) -> boolean().
v(Prop) when is_list(Prop) ->
    v(Prop, application_name(Prop));
v(JObj) ->
    v(kz_json:to_proplist(JObj)).

-spec v(kz_term:api_terms(), binary()) -> boolean().
v(Prop, DPApp) ->
    try
        VFun = kz_term:to_atom(<<DPApp/binary, "_v">>),
        case kz_module:is_exported(?MODULE, VFun, 1) of
            'false' -> throw({'invalid_dialplan_object', Prop});
            'true' -> ?MODULE:VFun(Prop)
        end
    catch
        _:R ->
            throw({R, Prop})
    end.

%%------------------------------------------------------------------------------
%% @doc Bridge a call.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec bridge(kz_term:api_terms()) -> kz_api:api_formatter_return().
bridge(Prop) when is_list(Prop) ->
    EPs = [begin
               {'ok', EPProps} = bridge_endpoint_headers(EP),
               kz_json:from_list(EPProps)
           end
           || EP <- props:get_value(<<"Endpoints">>, Prop, []),
              bridge_endpoint_v(EP)
          ],
    kapi_definition:build_message(props:set_value(<<"Endpoints">>, EPs, Prop), bridge_definition());
bridge(JObj) ->
    bridge(kz_json:to_proplist(JObj)).

-spec bridge_v(kz_term:api_terms()) -> boolean().
bridge_v(Req) ->
    kapi_definition:validate(Req, bridge_definition()).

%%------------------------------------------------------------------------------
%% @doc Unbridge a call.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec unbridge(kz_term:api_terms()) -> kz_api:api_formatter_return().
unbridge(Req) ->
    kapi_definition:build_message(Req, unbridge_definition()).

-spec unbridge_v(kz_term:api_terms()) -> boolean().
unbridge_v(Req) ->
    kapi_definition:validate(Req, unbridge_definition()).

%%------------------------------------------------------------------------------
%% @doc Endpoints for bridging a call.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec bridge_endpoint(kz_term:api_terms()) -> kz_api:api_formatter_return().
bridge_endpoint(Req) ->
    kapi_definition:build_message(Req, bridge_endpoint_definition()).

-spec bridge_endpoint_headers(kz_term:api_terms()) ->
          {'ok', kz_term:proplist()} |
          {'error', string()}.
bridge_endpoint_headers(Prop) when is_list(Prop) ->
    Definition = bridge_endpoint_definition(),
    ReqHeaders = kapi_definition:required_headers(Definition),
    OptHeaders = kapi_definition:optional_headers(Definition),
    kz_api:build_message_specific_headers(Prop
                                         ,ReqHeaders
                                         ,OptHeaders
                                         );
bridge_endpoint_headers(JObj) ->
    bridge_endpoint_headers(kz_json:to_proplist(JObj)).

-spec bridge_endpoint_v(kz_term:api_terms()) -> boolean().
bridge_endpoint_v(Req) ->
    kapi_definition:validate_message(Req, bridge_endpoint_definition()).

%%------------------------------------------------------------------------------
%% @doc Page a call.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec page(kz_term:api_terms()) -> kz_api:api_formatter_return().
page(Prop) when is_list(Prop) ->
    EPs = [begin
               {'ok', EPProps} = bridge_endpoint_headers(EP),
               kz_json:from_list(EPProps)
           end
           || EP <- props:get_value(<<"Endpoints">>, Prop, []),
              bridge_endpoint_v(EP)
          ],
    kapi_definition:build_message(props:set_value(<<"Endpoints">>, EPs, Prop), page_definition()).

-spec page_v(kz_term:api_terms()) -> boolean().
page_v(Req) ->
    kapi_definition:validate(Req, page_definition()).

%%------------------------------------------------------------------------------
%% @doc Store Request.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec store(kz_term:api_terms()) -> kz_api:api_formatter_return().
store(Req) ->
    kapi_definition:build_message(Req, store_definition()).

-spec store_v(kz_term:api_terms()) -> boolean().
store_v(Req) ->
    kapi_definition:validate(Req, store_definition()).

%%------------------------------------------------------------------------------
%% @doc Store (via AMQP) Response.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec store_amqp_resp(kz_term:api_terms()) -> kz_api:api_formatter_return().
store_amqp_resp(Req) ->
    kapi_definition:build_message(Req, store_amqp_resp_definition()).

-spec store_amqp_resp_v(kz_term:api_terms()) -> boolean().
store_amqp_resp_v(Req) ->
    kapi_definition:validate(Req, store_amqp_resp_definition()).

%%------------------------------------------------------------------------------
%% @doc Store (via HTTP) Response.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec store_http_resp(kz_term:api_terms()) -> kz_api:api_formatter_return().
store_http_resp(Req) ->
    kapi_definition:build_message(Req, store_http_resp_definition()).

-spec store_http_resp_v(kz_term:api_terms()) -> boolean().
store_http_resp_v(Req) ->
    kapi_definition:validate(Req, store_http_resp_definition()).

-spec store_media_content_v(binary() | 'eof') -> boolean().
store_media_content_v(V) ->
    is_binary(V)
        orelse V =:= 'eof'.

%%------------------------------------------------------------------------------
%% @doc Create a DTMF (or DTMFs) on the channel.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec send_dtmf(kz_term:api_terms()) -> kz_api:api_formatter_return().
send_dtmf(Req) ->
    kapi_definition:build_message(Req, send_dtmf_definition()).

-spec send_dtmf_v(kz_term:api_terms()) -> boolean().
send_dtmf_v(Req) ->
    kapi_definition:validate(Req, send_dtmf_definition()).

%%------------------------------------------------------------------------------
%% @doc Receive DTMF.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec recv_dtmf(kz_term:api_terms()) -> kz_api:api_formatter_return().
recv_dtmf(Req) ->
    kapi_definition:build_message(Req, recv_dtmf_definition()).

-spec recv_dtmf_v(kz_term:api_terms()) -> boolean().
recv_dtmf_v(Req) ->
    kapi_definition:validate(Req, recv_dtmf_definition()).

%%------------------------------------------------------------------------------
%% @doc Create a tone on the channel.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec tones(kz_term:api_terms()) -> kz_api:api_formatter_return().
tones(Prop) when is_list(Prop) ->
    Tones = [begin
                 {'ok', TonesProp} = tones_req_tone_headers(Tone),
                 kz_json:from_list(TonesProp)
             end
             || Tone <- props:get_value(<<"Tones">>, Prop, []),
                tones_req_tone_v(Tone)],
    Prop1 = [ {<<"Tones">>, Tones} | props:delete(<<"Tones">>, Prop)],
    kapi_definition:build_message(Prop1, tones_definition());
tones(JObj) ->
    tones(kz_json:to_proplist(JObj)).

-spec tones_v(kz_term:api_terms()) -> boolean().
tones_v(Req) ->
    kapi_definition:validate(Req, tones_definition()).

%%------------------------------------------------------------------------------
%% @doc A Tone within a Tones request.
%% Takes {@link kz_term:api_term()} and returns a proplist
%% @end
%%------------------------------------------------------------------------------
-spec tones_req_tone(kz_term:api_terms()) -> kz_api:api_formatter_return().
tones_req_tone(Req) ->
    kapi_definition:build_message(Req, tones_req_tone_definition()).

-spec tones_req_tone_v(kz_term:api_terms()) -> boolean().
tones_req_tone_v(Req) ->
    kapi_definition:validate_message(Req, tones_req_tone_definition()).

-spec tones_req_tone_headers(kz_term:api_terms()) ->
          {'ok', kz_term:proplist()} |
          {'error', string()}.
tones_req_tone_headers(Prop) when is_list(Prop) ->
    Definition = tones_req_tone_definition(),
    ReqHeaders = kapi_definition:required_headers(Definition),
    OptHeaders = kapi_definition:optional_headers(Definition),
    kz_api:build_message_specific_headers(Prop
                                         ,ReqHeaders
                                         ,OptHeaders
                                         );
tones_req_tone_headers(JObj) ->
    tones_req_tone_headers(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Detect tones on the line.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec tone_detect(kz_term:api_terms()) -> kz_api:api_formatter_return().
tone_detect(Req) ->
    kapi_definition:build_message(Req, tone_detect_definition()).

-spec tone_detect_v(kz_term:api_terms()) -> boolean().
tone_detect_v(Req) ->
    kapi_definition:validate(Req, tone_detect_definition()).

-spec tone_timeout_v(any()) -> boolean().
tone_timeout_v(Timeout) ->
    %% <<"+123">> converts to 123, so yay!
    try kz_term:to_integer(Timeout) of
        T when T < 0 -> 'false';
        _ -> 'true'
    catch
        _:_ -> 'false'
    end.

%%------------------------------------------------------------------------------
%% @doc Send a list of dialplan applications in bulk.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec queue(kz_term:api_terms()) -> kz_api:api_formatter_return().
queue(Req) ->
    kapi_definition:build_message(Req, queue_definition()).

-spec queue_v(kz_term:api_terms()) -> boolean().
queue_v(Req) ->
    kapi_definition:validate(Req, queue_definition()).

%%------------------------------------------------------------------------------
%% @doc Play media.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec play(kz_term:api_terms()) -> kz_api:api_formatter_return().
play(Req) ->
    kapi_definition:build_message(Req, play_definition()).

-spec play_v(kz_term:api_terms()) -> boolean().
play_v(Req) ->
    kapi_definition:validate(Req, play_definition()).

%%------------------------------------------------------------------------------
%% @doc Moves to the next step in callflow.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec break(kz_term:api_terms()) -> kz_api:api_formatter_return().
break(Req) ->
    kapi_definition:build_message(Req, break_definition()).

-spec break_v(kz_term:api_terms()) -> boolean().
break_v(Req) ->
    kapi_definition:validate(Req, break_definition()).

%%------------------------------------------------------------------------------
%% @doc Stop media from playing.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec playstop(kz_term:api_terms()) -> kz_api:api_formatter_return().
playstop(Req) ->
    kapi_definition:build_message(Req, playstop_definition()).

-spec playstop_v(kz_term:api_terms()) -> boolean().
playstop_v(Req) ->
    kapi_definition:validate(Req, playstop_definition()).

%%------------------------------------------------------------------------------
%% @doc Change position in playing media.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec playseek(kz_term:api_terms()) -> kz_api:api_formatter_return().
playseek(Req) ->
    kapi_definition:build_message(Req, playseek_definition()).

-spec playseek_v(kz_term:api_terms()) -> boolean().
playseek_v(Req) ->
    kapi_definition:validate(Req, playseek_definition()).

%%------------------------------------------------------------------------------
%% @doc TTS - Text-to-speech.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec tts(kz_term:api_terms()) -> kz_api:api_formatter_return().
tts(Req) ->
    kapi_definition:build_message(Req, tts_definition()).

-spec tts_v(kz_term:api_terms()) -> boolean().
tts_v(Req) ->
    kapi_definition:validate(Req, tts_definition()).

%%------------------------------------------------------------------------------
%% @doc Record media.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec record(kz_term:api_terms()) -> kz_api:api_formatter_return().
record(Req) ->
    kapi_definition:build_message(Req, record_definition()).

-spec record_v(kz_term:api_terms()) -> boolean().
record_v(Req) ->
    kapi_definition:validate(Req, record_definition()).

%%------------------------------------------------------------------------------
%% @doc Record call media.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec record_call(kz_term:api_terms()) -> kz_api:api_formatter_return().
record_call(Req) ->
    kapi_definition:build_message(Req, record_call_definition()).

-spec record_call_v(kz_term:api_terms()) -> boolean().
record_call_v(Req) ->
    kapi_definition:validate(Req, record_call_definition()).

%%------------------------------------------------------------------------------
%% @doc Answer a session.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec answer(kz_term:api_terms()) -> kz_api:api_formatter_return().
answer(Req) ->
    kapi_definition:build_message(Req, answer_definition()).

-spec answer_v(kz_term:api_terms()) -> boolean().
answer_v(Req) ->
    kapi_definition:validate(Req, answer_definition()).

%%------------------------------------------------------------------------------
%% @doc Echo a session.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec echo(kz_term:api_terms()) -> kz_api:api_formatter_return().
echo(Req) ->
    kapi_definition:build_message(Req, echo_definition()).

-spec echo_v(kz_term:api_terms()) -> boolean().
echo_v(Req) ->
    kapi_definition:validate(Req, echo_definition()).

%%------------------------------------------------------------------------------
%% @doc Privacy.
%% @end
%%------------------------------------------------------------------------------
-spec privacy(kz_term:api_terms()) -> kz_api:api_formatter_return().
privacy(Req) ->
    kapi_definition:build_message(Req, privacy_definition()).

-spec privacy_v(kz_term:api_terms()) -> boolean().
privacy_v(Req) ->
    kapi_definition:validate(Req, privacy_definition()).

%%------------------------------------------------------------------------------
%% @doc Progress a session.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec progress(kz_term:api_terms()) -> kz_api:api_formatter_return().
progress(Req) ->
    kapi_definition:build_message(Req, progress_definition()).

-spec progress_v(kz_term:api_terms()) -> boolean().
progress_v(Req) ->
    kapi_definition:validate(Req, progress_definition()).

%%------------------------------------------------------------------------------
%% @doc Ring a session.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec ring(kz_term:api_terms()) -> kz_api:api_formatter_return().
ring(Req) ->
    kapi_definition:build_message(Req, ring_definition()).

-spec ring_v(kz_term:api_terms()) -> boolean().
ring_v(Req) ->
    kapi_definition:validate(Req, ring_definition()).

%%------------------------------------------------------------------------------
%% @doc Receive a fax, storing it to local disk.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec receive_fax(kz_term:api_terms()) -> kz_api:api_formatter_return().
receive_fax(Req) ->
    kapi_definition:build_message(Req, receive_fax_definition()).

-spec receive_fax_v(kz_term:api_terms()) -> boolean().
receive_fax_v(Req) ->
    kapi_definition:validate(Req, receive_fax_definition()).

%%------------------------------------------------------------------------------
%% @doc Store a fax, storing it to the DB.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec store_fax(kz_term:api_terms()) -> kz_api:api_formatter_return().
store_fax(Req) ->
    kapi_definition:build_message(Req, store_fax_definition()).

-spec store_fax_v(kz_term:api_terms()) -> boolean().
store_fax_v(Req) ->
    kapi_definition:validate(Req, store_fax_definition()).

%%------------------------------------------------------------------------------
%% @doc Hangup a call.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec hangup(kz_term:api_terms()) -> kz_api:api_formatter_return().
hangup(Req) ->
    kapi_definition:build_message(Req, hangup_definition()).

-spec hangup_v(kz_term:api_terms()) -> boolean().
hangup_v(Req) ->
    kapi_definition:validate(Req, hangup_definition()).

%%------------------------------------------------------------------------------
%% @doc Soft Hold.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec soft_hold(kz_term:api_terms()) -> kz_api:api_formatter_return().
soft_hold(Req) ->
    kapi_definition:build_message(Req, soft_hold_definition()).

-spec soft_hold_v(kz_term:api_terms()) -> boolean().
soft_hold_v(Req) ->
    kapi_definition:validate(Req, soft_hold_definition()).

%%------------------------------------------------------------------------------
%% @doc Hold a call.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec hold(kz_term:api_terms()) -> kz_api:api_formatter_return().
hold(Req) ->
    kapi_definition:build_message(Req, hold_definition()).

-spec hold_v(kz_term:api_terms()) -> boolean().
hold_v(Req) ->
    kapi_definition:validate(Req, hold_definition()).

%%------------------------------------------------------------------------------
%% @doc Hold control.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec hold_control(kz_term:api_terms()) -> kz_api:api_formatter_return().
hold_control(Req) ->
    kapi_definition:build_message(Req, hold_control_definition()).

-spec hold_control_v(kz_term:api_terms()) -> boolean().
hold_control_v(Req) ->
    kapi_definition:validate(Req, hold_control_definition()).

%%------------------------------------------------------------------------------
%% @doc Park a call.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec park(kz_term:api_terms()) -> kz_api:api_formatter_return().
park(Req) ->
    kapi_definition:build_message(Req, park_definition()).

-spec park_v(kz_term:api_terms()) -> boolean().
park_v(Req) ->
    kapi_definition:validate(Req, park_definition()).

%%------------------------------------------------------------------------------
%% @doc Audio level/mute.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec audio_level(kz_term:api_terms()) -> kz_api:api_formatter_return().
audio_level(Req) ->
    kapi_definition:build_message(Req, audio_level_definition()).

-spec audio_level_v(kz_term:api_terms()) -> boolean().
audio_level_v(Req) ->
    kapi_definition:validate(Req, audio_level_definition()).

%%------------------------------------------------------------------------------
%% @doc Set Custom Channel variables.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec set(kz_term:api_terms()) -> kz_api:api_formatter_return().
set(Req) ->
    kapi_definition:build_message(Req, set_definition()).

-spec set_v(kz_term:api_terms()) -> boolean().
set_v(Req) ->
    kapi_definition:validate(Req, set_definition()).

%%------------------------------------------------------------------------------
%% @doc Set Terminators for playback/record.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec set_terminators(kz_term:api_terms()) -> kz_api:api_formatter_return().
set_terminators(Req) ->
    kapi_definition:build_message(Req, set_terminators_definition()).

-spec set_terminators_v(kz_term:api_terms()) -> boolean().
set_terminators_v(Req) ->
    kapi_definition:validate(Req, set_terminators_definition()).

%%------------------------------------------------------------------------------
%% @doc Fetch Custom Channel variables.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec fetch(kz_term:api_terms()) -> kz_api:api_formatter_return().
fetch(Req) ->
    kapi_definition:build_message(Req, fetch_definition()).

-spec fetch_v(kz_term:api_terms()) -> boolean().
fetch_v(Req) ->
    kapi_definition:validate(Req, fetch_definition()).

%%------------------------------------------------------------------------------
%% @doc Play media and record digits.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec play_and_collect_digits(kz_term:api_terms()) -> kz_api:api_formatter_return().
play_and_collect_digits(Req) ->
    kapi_definition:build_message(Req, play_and_collect_digits_definition()).

-spec play_and_collect_digits_v(kz_term:api_terms()) -> boolean().
play_and_collect_digits_v(Req) ->
    kapi_definition:validate(Req, play_and_collect_digits_definition()).

%%------------------------------------------------------------------------------
%% @doc Pickup a call.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec call_pickup(kz_term:api_terms()) -> kz_api:api_formatter_return().
call_pickup(Req) ->
    kapi_definition:build_message(Req, call_pickup_definition()).

-spec call_pickup_v(kz_term:api_terms()) -> boolean().
call_pickup_v(Req) ->
    kapi_definition:validate(Req, call_pickup_definition()).

%%------------------------------------------------------------------------------
%% @doc Connect a leg to the current leg.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec connect_leg(kz_term:api_terms()) -> kz_api:api_formatter_return().
connect_leg(Req) ->
    kapi_definition:build_message(Req, connect_leg_definition()).

-spec connect_leg_v(kz_term:api_terms()) -> boolean().
connect_leg_v(Req) ->
    kapi_definition:validate(Req, connect_leg_definition()).

%%------------------------------------------------------------------------------
%% @doc Eavesdrop.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec eavesdrop(kz_term:api_terms()) -> kz_api:api_formatter_return().
eavesdrop(Req) ->
    kapi_definition:build_message(Req, eavesdrop_definition()).

-spec eavesdrop_v(kz_term:api_terms()) -> boolean().
eavesdrop_v(Req) ->
    kapi_definition:validate(Req, eavesdrop_definition()).

%%------------------------------------------------------------------------------
%% @doc Say - convert text to speech.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec say(kz_term:api_terms()) -> kz_api:api_formatter_return().
say(Req) ->
    kapi_definition:build_message(Req, say_definition()).

-spec say_v(kz_term:api_terms()) -> boolean().
say_v(Req) ->
    kapi_definition:validate(Req, say_definition()).

%%------------------------------------------------------------------------------
%% @doc Respond a session.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec respond(kz_term:api_terms()) -> kz_api:api_formatter_return().
respond(Req) ->
    kapi_definition:build_message(Req, respond_definition()).

-spec respond_v(kz_term:api_terms()) -> boolean().
respond_v(Req) ->
    kapi_definition:validate(Req, respond_definition()).

%%------------------------------------------------------------------------------
%% @doc Redirect a session.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec redirect(kz_term:api_terms()) -> kz_api:api_formatter_return().
redirect(Req) ->
    kapi_definition:build_message(Req, redirect_definition()).

-spec redirect_v(kz_term:api_terms()) -> boolean().
redirect_v(Req) ->
    kapi_definition:validate(Req, redirect_definition()).

%%------------------------------------------------------------------------------
%% @doc Execute_Extension a session.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec execute_extension(kz_term:api_terms()) -> kz_api:api_formatter_return().
execute_extension(Req) ->
    kapi_definition:build_message(Req, execute_extension_definition()).

-spec execute_extension_v(kz_term:api_terms()) -> boolean().
execute_extension_v(Req) ->
    kapi_definition:validate(Req, execute_extension_definition()).

%%------------------------------------------------------------------------------
%% @doc Sleep - Pauses execution.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec sleep(kz_term:api_terms()) -> kz_api:api_formatter_return().
sleep(Req) ->
    kapi_definition:build_message(Req, sleep_definition()).

-spec sleep_v(kz_term:api_terms()) -> boolean().
sleep_v(Req) ->
    kapi_definition:validate(Req, sleep_definition()).

%%------------------------------------------------------------------------------
%% @doc Format a Dialplan: noop API call.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec noop(kz_term:api_terms()) -> kz_api:api_formatter_return().
noop(Req) ->
    kapi_definition:build_message(Req, noop_definition()).

-spec noop_v(kz_term:api_terms()) -> boolean().
noop_v(Req) ->
    kapi_definition:validate(Req, noop_definition()).

%%------------------------------------------------------------------------------
%% @doc Conference - Sends caller to a conference.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec conference(kz_term:api_terms()) -> kz_api:api_formatter_return().
conference(Req) ->
    kapi_definition:build_message(Req, conference_definition()).

-spec conference_v(kz_term:api_terms()) -> boolean().
conference_v(Req) ->
    kapi_definition:validate(Req, conference_definition()).

%%------------------------------------------------------------------------------
%% @doc Originate Ready.
%% Send Requestor a message that the originate is ready to execute.
%% @end
%%------------------------------------------------------------------------------
-spec originate_ready(kz_term:api_terms()) -> kz_api:api_formatter_return().
originate_ready(Req) ->
    kapi_definition:build_message(Req, originate_ready_definition()).

-spec originate_ready_v(kz_term:api_terms()) -> boolean().
originate_ready_v(Req) ->
    kapi_definition:validate(Req, originate_ready_definition()).

-spec publish_originate_ready(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_originate_ready(ServerId, JObj) ->
    publish_originate_ready(ServerId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_originate_ready(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_originate_ready(ServerId, API, ContentType) ->
    Definition = originate_ready_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(ServerId, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Originate Execute.
%% Wait for the Requestor to respond to execute the origination.
%% @end
%%------------------------------------------------------------------------------
-spec originate_execute(kz_term:api_terms()) -> kz_api:api_formatter_return().
originate_execute(Req) ->
    kapi_definition:build_message(Req, originate_execute_definition()).

-spec originate_execute_v(kz_term:api_terms()) -> boolean().
originate_execute_v(Req) ->
    kapi_definition:validate(Req, originate_execute_definition()).

-spec publish_originate_execute(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_originate_execute(ServerId, JObj) ->
    publish_originate_execute(ServerId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_originate_execute(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_originate_execute(ServerId, API, ContentType) ->
    Definition = originate_execute_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(ServerId, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Error - Sends error to Queue.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec error(kz_term:api_terms()) -> kz_api:api_formatter_return().
error(Req) ->
    kapi_definition:build_message(Req, error_definition()).

-spec error_v(kz_term:api_terms()) -> boolean().
error_v(Req) ->
    kapi_definition:validate(Req, error_definition()).

-spec publish_error(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_error(CallID, JObj) ->
    publish_error(CallID, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_error(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_error(CallID, API, ContentType) ->
    Definition = error_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:callevt_publish((kapi_definition:binding(Definition))(<<"dialplan">>, CallID)
                                ,Payload
                                ,ContentType
                                ).

%%------------------------------------------------------------------------------
%% @doc Takes a generic API JObj, determines what type it is, and calls
%% the appropriate validator.
%% @end
%%------------------------------------------------------------------------------

-spec publish_command(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_command(CtrlQ, Prop) when is_list(Prop) ->
    publish_command(CtrlQ, Prop, application_name(Prop));
publish_command(CtrlQ, JObj) ->
    publish_command(CtrlQ, kz_json:to_proplist(JObj)).

-spec publish_command(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_command(CtrlQ, Prop, DPApp) ->
    {'ok', Payload} = build_command(Prop, DPApp),
    kz_amqp_util:callctl_publish(CtrlQ, Payload, ?DEFAULT_CONTENT_TYPE).

-spec build_command(kz_term:api_terms()) -> {'ok', kz_term:api_terms()}.
build_command(Prop) when is_list(Prop) ->
    build_command(Prop, application_name(Prop));
build_command(JObj) ->
    build_command(kz_json:to_proplist(JObj)).

-spec build_command(kz_term:api_terms(), kz_term:ne_binary()) -> {'ok', kz_term:api_terms()}.
build_command(Prop, DPApp) when is_list(Prop) ->
    try kz_term:to_atom(<<DPApp/binary>>) of
        BuildMsgFun ->
            case kz_module:is_exported(?MODULE, BuildMsgFun, 1) of
                'false' ->
                    {'error', 'invalid_dialplan_object'};
                'true' ->
                    ?MODULE:BuildMsgFun(kz_api:set_missing_values(Prop, ?DEFAULT_VALUES))
            end
    catch
        ?STACKTRACE(_, R, ST)
        kz_log:log_stacktrace(ST),
        throw({R, Prop})
        end;
build_command(JObj, DPApp) ->
    build_command(kz_json:to_proplist(JObj), DPApp).

%%------------------------------------------------------------------------------
%% @doc Sending DP actions to CallControl Queue.
%% @end
%%------------------------------------------------------------------------------

-spec publish_action(kz_term:ne_binary(), iodata()) -> 'ok'.
publish_action(Queue, JSON) ->
    publish_action(Queue, JSON, ?DEFAULT_CONTENT_TYPE).

-spec publish_action(kz_term:ne_binary(), iodata(), kz_term:ne_binary()) -> 'ok'.
publish_action(Queue, Payload, ContentType) ->
    kz_amqp_util:callctl_publish(Queue, Payload, ContentType).

-spec dial_method_single() -> kz_term:ne_binary().
dial_method_single() -> ?DIAL_METHOD_SINGLE.

-spec dial_method_simultaneous() -> kz_term:ne_binary().
dial_method_simultaneous() -> ?DIAL_METHOD_SIMUL.

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, _Props) ->
    kz_amqp_util:bind_q_to_callctl(Queue).

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, _Props) ->
    kz_amqp_util:unbind_q_from_callctl(Queue).

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:callctl_exchange().

-spec terminators(kz_term:api_binary()) -> kz_term:ne_binaries().
terminators(Bin) when is_binary(Bin) ->
    [<<B>> || <<B>> <= Bin, lists:member(<<B>>, ?ANY_DIGIT)];
terminators('undefined') -> ?ANY_DIGIT.

-spec terminators_v(kz_term:api_binaries() | binary()) -> boolean().
terminators_v(Ts) when is_list(Ts) ->
    lists:all(fun terminator_v/1, Ts);
terminators_v(<<>>) -> 'true';
terminators_v(<<"none">>) -> 'true';
terminators_v(_) -> 'false'.

-spec terminator_v(kz_term:ne_binary()) -> boolean().
terminator_v(T) -> lists:member(T, ?ANY_DIGIT).

-spec offsite_store_url(kz_term:api_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
offsite_store_url('undefined', _) -> throw({'error', <<"URL not defined">>});
offsite_store_url(Url, MediaName) ->
    iolist_to_binary([kz_binary:strip_right(Url, $/), "/", MediaName]).

-spec application_name(kz_term:api_terms()) -> kz_term:api_ne_binary().
application_name(Prop) when is_list(Prop) ->
    props:get_value(<<"Application-Name">>, Prop);
application_name(JObj) ->
    kz_json:get_ne_binary_value(<<"Application-Name">>, JObj).

%%------------------------------------------------------------------------------
%% @doc Detect fax on the line.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec fax_detection(kz_term:api_terms()) -> kz_api:api_formatter_return().
fax_detection(Req) ->
    kapi_definition:build_message(Req, fax_detection_definition()).

-spec fax_detection_v(kz_term:api_terms()) -> boolean().
fax_detection_v(Req) ->
    kapi_definition:validate(Req, fax_detection_definition()).

%%------------------------------------------------------------------------------
%% @doc Store VoiceMail.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec store_vm(kz_term:api_terms()) -> kz_api:api_formatter_return().
store_vm(Req) ->
    kapi_definition:build_message(Req, store_vm_definition()).

-spec store_vm_v(kz_term:api_terms()) -> boolean().
store_vm_v(Req) ->
    kapi_definition:validate(Req, store_vm_definition()).

%%------------------------------------------------------------------------------
%% @doc Transfer.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec transfer(kz_term:api_terms()) -> kz_api:api_formatter_return().
transfer(Req) ->
    kapi_definition:build_message(Req, transfer_definition()).

-spec transfer_v(kz_term:api_terms()) -> boolean().
transfer_v(Req) ->
    kapi_definition:validate(Req, transfer_definition()).

%%------------------------------------------------------------------------------
%% @doc Media macro.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec media_macro(kz_term:api_terms()) -> kz_api:api_formatter_return().
media_macro(Req) ->
    kapi_definition:build_message(Req, media_macro_definition()).

-spec media_macro_v(kz_term:api_terms()) -> boolean().
media_macro_v(Req) ->
    kapi_definition:validate(Req, media_macro_definition()).

%%------------------------------------------------------------------------------
%% @doc Play macro.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec play_macro(kz_term:api_terms()) -> kz_api:api_formatter_return().
play_macro(Req) ->
    kapi_definition:build_message(Req, play_macro_definition()).

-spec play_macro_v(kz_term:api_terms()) -> boolean().
play_macro_v(Req) ->
    kapi_definition:validate(Req, play_macro_definition()).

%%------------------------------------------------------------------------------
%% @doc Sound touch.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec sound_touch(kz_term:api_terms()) -> kz_api:api_formatter_return().
sound_touch(Req) ->
    kapi_definition:build_message(Req, sound_touch_definition()).

-spec sound_touch_v(kz_term:api_terms()) -> boolean().
sound_touch_v(Req) ->
    kapi_definition:validate(Req, sound_touch_definition()).

%%------------------------------------------------------------------------------
%% @doc Event action.
%% Takes {@link kz_term:api_term()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec event_actions(kz_term:api_terms()) -> kz_api:api_formatter_return().
event_actions(Req) ->
    kapi_definition:build_message(Req, event_actions_definition()).

-spec event_actions_v(kz_term:api_terms()) -> boolean().
event_actions_v(Req) ->
    kapi_definition:validate(Req, event_actions_definition()).
