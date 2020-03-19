%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Call-related messages, like switch events, status requests, etc AMQP API.
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_call).

-export([api_definitions/0, api_definition/1]).

-export([optional_call_event_headers/0]).

-export([event/1
        ,event_v/1
        ,publish_event/1
        ,publish_event/2
        ]).
-export([channel_status_req/1
        ,channel_status_req_v/1
        ,publish_channel_status_req/1
        ,publish_channel_status_req/2
        ,publish_channel_status_req/3
        ]).
-export([channel_status_resp/1
        ,channel_status_resp_v/1
        ,publish_channel_status_resp/2
        ,publish_channel_status_resp/3
        ]).
-export([query_auth_id_req/1
        ,query_auth_id_req_v/1
        ,publish_query_auth_id_req/1
        ,publish_query_auth_id_req/2
        ,publish_query_auth_id_req/3
        ]).
-export([query_auth_id_resp/1
        ,query_auth_id_resp_v/1
        ,publish_query_auth_id_resp/2
        ,publish_query_auth_id_resp/3
        ]).
-export([query_user_channels_req/1
        ,query_user_channels_req_v/1
        ,publish_query_user_channels_req/1
        ,publish_query_user_channels_req/4
        ]).
-export([query_user_channels_resp/1
        ,query_user_channels_resp_v/1
        ,publish_query_user_channels_resp/2
        ,publish_query_user_channels_resp/3
        ]).
-export([query_account_channels_req/1
        ,query_account_channels_req_v/1
        ,publish_query_account_channels_req/1
        ,publish_query_account_channels_req/3
        ]).
-export([query_account_channels_resp/1
        ,query_account_channels_resp_v/1
        ,publish_query_account_channels_resp/2
        ,publish_query_account_channels_resp/3
        ]).
-export([query_channels_req/1
        ,query_channels_req_v/1
        ,publish_query_channels_req/1
        ,publish_query_channels_req/2
        ]).
-export([query_channels_resp/1
        ,query_channels_resp_v/1
        ,publish_query_channels_resp/2
        ,publish_query_channels_resp/3
        ]).
-export([usurp_control/1
        ,usurp_control_v/1
        ,publish_usurp_control/2
        ,publish_usurp_control/3
        ]).
-export([usurp_publisher/1
        ,usurp_publisher_v/1
        ,publish_usurp_publisher/2
        ,publish_usurp_publisher/3
        ]).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-export([get_status/1]).
-export([event_routing_key/2]).

-export_type([event/0]).

-include_lib("kazoo_amqp/src/kz_amqp_util.hrl").
-include_lib("kazoo_amqp/include/kz_api_literals.hrl"). % ?KEY_EVENT_NAME

-type event() :: kz_json:object().

-ifdef(TEST).
-export([call_routing_key/2
        ,update_name_and_values/2
        ]).
-endif.

-spec optional_call_event_headers() -> kz_term:ne_binaries().
optional_call_event_headers() ->
    kapi_definition:optional_headers(event_definition()).

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [event_definition()
    ,channel_status_req_definition()
    ,channel_status_resp_definition()
    ,query_auth_id_req_definition()
    ,query_auth_id_resp_definition()
    ,query_user_channels_req_definition()
    ,query_user_channels_resp_definition()
    ,query_account_channels_req_definition()
    ,query_account_channels_resp_definition()
    ,query_channels_req_definition()
    ,query_channels_resp_definition()
    ,usurp_control_definition()
    ,usurp_publisher_definition()
    ].

%%------------------------------------------------------------------------------
%% @doc Get API definition of the given `Name'.
%% @see api_definitions/0
%% @end
%%------------------------------------------------------------------------------
-spec api_definition(kz_term:text()) -> kapi_definition:api().
api_definition(Name) when not is_binary(Name) ->
    api_definition(kz_term:to_binary(Name));
api_definition(<<"event">>) ->
    event_definition();
api_definition(<<"channel_status_req">>) ->
    channel_status_req_definition();
api_definition(<<"channel_status_resp">>) ->
    channel_status_resp_definition();
api_definition(<<"query_auth_id_req">>) ->
    query_auth_id_req_definition();
api_definition(<<"query_auth_id_resp">>) ->
    query_auth_id_resp_definition();
api_definition(<<"query_user_channels_req">>) ->
    query_user_channels_req_definition();
api_definition(<<"query_user_channels_resp">>) ->
    query_user_channels_resp_definition();
api_definition(<<"query_account_channels_req">>) ->
    query_account_channels_req_definition();
api_definition(<<"query_account_channels_resp">>) ->
    query_account_channels_resp_definition();
api_definition(<<"query_channels_req">>) ->
    query_channels_req_definition();
api_definition(<<"query_channels_resp">>) ->
    query_channels_resp_definition();
api_definition(<<"usurp_control">>) ->
    usurp_control_definition();
api_definition(<<"usurp_publisher">>) ->
    usurp_publisher_definition().

-spec event_definition() -> kapi_definition:api().
event_definition() ->
    Category = <<"call_event">>,
    EventName = fun kz_api:event_name/1,

    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Call Event">>}
              ,{fun kapi_definition:set_description/2
               ,<<"Format a call event from the switch for the listener">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun event/1}
              ,{fun kapi_definition:set_validate_fun/2, fun event_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_event/1}
              ,{fun kapi_definition:set_binding/2, fun call_routing_key/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Call-ID">>]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Application-Data">>
                                                            ,<<"Application-Event">>
                                                            ,<<"Application-Name">>
                                                            ,<<"Application-Response">>
                                                            ,<<"Billing-Seconds">>
                                                            ,<<"Bridge-B-Unique-ID">>
                                                            ,<<"Bridge-Hangup-Cause">>
                                                            ,<<"Call-Debug">>
                                                            ,<<"Call-Direction">>
                                                            ,<<"Callee-ID-Name">>
                                                            ,<<"Callee-ID-Number">>
                                                            ,<<"Caller-Destination-Number">>
                                                            ,<<"Caller-ID-Name">>
                                                            ,<<"Caller-ID-Number">>
                                                            ,<<"Channel-Answer-State">>
                                                            ,<<"Channel-Call-State">>
                                                            ,<<"Channel-Created-Time">>
                                                            ,<<"Channel-Debug">>
                                                            ,<<"Channel-Is-Loopback">>
                                                            ,<<"Channel-Loopback-Bowout">>
                                                            ,<<"Channel-Loopback-Bowout-Execute">>
                                                            ,<<"Channel-Loopback-Leg">>
                                                            ,<<"Channel-Loopback-Other-Leg-ID">>
                                                            ,<<"Channel-Moving">>
                                                            ,<<"Channel-Name">>
                                                            ,<<"Channel-State">>
                                                            ,<<"Conference-Config">>
                                                            ,<<"Conference-Name">>
                                                            ,<<"Connecting-Leg-A-UUID">>
                                                            ,<<"Connecting-Leg-B-UUID">>
                                                            ,<<"Control-Queue">>
                                                            ,<<"Custom-Application-Vars">>
                                                            ,<<"Custom-Channel-Vars">>
                                                            ,<<"Custom-SIP-Headers">>
                                                            ,<<"DTMF-Digit">> %% DTMF and Tones
                                                            ,<<"DTMF-Duration">>
                                                            ,<<"Detected-Tone">>
                                                            ,<<"Digits-Dialed">>
                                                            ,<<"Disposition">>
                                                            ,<<"Duration-Seconds">>
                                                            ,<<"Endpoint-Disposition">>
                                                            ,<<"Fax-Info">>
                                                            ,<<"From">>
                                                            ,<<"From-Tag">>
                                                            ,<<"From-Uri">>
                                                            ,<<"Hangup-Cause">>
                                                            ,<<"Hangup-Code">> %% Hangup
                                                            ,<<"Interaction-ID">>
                                                            ,<<"Intercepted-By">>
                                                            ,<<"Length">>
                                                            ,<<"Local-SDP">>
                                                            ,<<"Media-Recordings">>
                                                            ,<<"Media-Server">>
                                                            ,<<"Origination-Call-ID">>
                                                            ,<<"Other-Leg-Call-ID">> %% BRIDGE
                                                            ,<<"Other-Leg-Caller-ID-Name">>
                                                            ,<<"Other-Leg-Caller-ID-Number">>
                                                            ,<<"Other-Leg-Destination-Number">>
                                                            ,<<"Other-Leg-Direction">>
                                                            ,<<"Parking-Slot">>
                                                            ,<<"Presence-ID">>
                                                            ,<<"Raw-Application-Data">>
                                                            ,<<"Raw-Application-Name">>
                                                            ,<<"Recording">>
                                                            ,<<"Remote-SDP">>
                                                            ,<<"Replaced-By">>
                                                            ,<<"Request">>
                                                            ,<<"Resigning-Peer-UUID">>
                                                            ,<<"Resigning-UUID">>
                                                            ,<<"Ringing-Seconds">>
                                                            ,<<"Root-Call-Interaction-ID">>
                                                            ,<<"Silence-Terminated">> %% Record-related
                                                            ,<<"Switch-Hostname">>
                                                            ,<<"Switch-Nodename">>
                                                            ,<<"Switch-URI">>
                                                            ,<<"Switch-URL">>
                                                            ,<<"Target-Call-ID">> %% TRANSFEREE
                                                            ,<<"Terminator">>
                                                            ,<<"Timestamp">>
                                                            ,<<"To">>
                                                            ,<<"To-Tag">>
                                                            ,<<"To-Uri">>
                                                            ,<<"Transfer-Disposition">>
                                                            ,<<"Transfer-History">>
                                                            ,<<"Transfer-Source">>
                                                            ,<<"Transfer-To">>
                                                            ,<<"Transfer-Type">>
                                                            ,<<"User-Agent">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Custom-Application-Vars">>, fun kz_json:is_json_object/1}
                ,{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                ,{<<"Custom-SIP-Headers">>, fun kz_json:is_json_object/1}
                ,{<<"Fax-Info">>, fun kz_json:is_json_object/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec channel_status_req_definition() -> kapi_definition:api().
channel_status_req_definition() ->
    EventName = <<"channel_status_req">>,
    Category = <<"call_event">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Channel Status Request">>}
              ,{fun kapi_definition:set_description/2, <<"Inquire into the status of a channel">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun channel_status_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun channel_status_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_channel_status_req/1}
              ,{fun kapi_definition:set_binding/2, fun call_routing_key/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Call-ID">>]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Active-Only">>]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec channel_status_resp_definition() -> kapi_definition:api().
channel_status_resp_definition() ->
    EventName = <<"channel_status_resp">>,
    Category = <<"call_event">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Channel Status Response">>}
              ,{fun kapi_definition:set_description/2
               ,<<"Respond with status of a channel, either active or nonexistent">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun channel_status_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun channel_status_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_channel_status_resp/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Call-ID">>
                                                            ,<<"Status">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Custom-Application-Vars">>
                                                            ,<<"Custom-Channel-Vars">>
                                                            ,<<"Error-Msg">>
                                                            ,<<"From-Tag">>
                                                            ,<<"Other-Leg-Call-ID">>
                                                            ,<<"Realm">>
                                                            ,<<"Switch-Hostname">>
                                                            ,<<"Switch-Nodename">>
                                                            ,<<"Switch-URL">>
                                                            ,<<"To-Tag">>
                                                            ,<<"Username">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Status">>, [<<"active">>, <<"tmpdown">>, <<"terminated">>]}
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Custom-Application-Vars">>, fun kz_json:is_json_object/1}
                ,{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec query_auth_id_req_definition() -> kapi_definition:api().
query_auth_id_req_definition() ->
    EventName = <<"query_auth_id_req">>,
    Category = <<"call_event">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Query Auth ID Req">>}
              ,{fun kapi_definition:set_description/2, <<"Inquire into the status of a call">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun query_auth_id_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun query_auth_id_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_query_auth_id_req/1}
              ,{fun kapi_definition:set_binding/2, fun call_routing_key/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Auth-ID">>]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec query_auth_id_resp_definition() -> kapi_definition:api().
query_auth_id_resp_definition() ->
    EventName = <<"query_auth_id_resp">>,
    Category = <<"call_event">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Query Auth ID Resp">>}
              ,{fun kapi_definition:set_description/2, <<"Inquire into the status of a call">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun query_auth_id_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun query_auth_id_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_query_auth_id_resp/2}
              ,{fun kapi_definition:set_required_headers/2, []}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Channels">>]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec query_user_channels_req_definition() -> kapi_definition:api().
query_user_channels_req_definition() ->
    EventName = <<"query_user_channels_req">>,
    Category = <<"call_event">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Query User Channels Req">>}
              ,{fun kapi_definition:set_description/2, <<>>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun query_user_channels_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun query_user_channels_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_query_user_channels_req/1}
              ,{fun kapi_definition:set_binding/2, fun call_routing_key/2}
              ,{fun kapi_definition:set_required_headers/2, []}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Active-Only">>
                                                            ,<<"Authorizing-IDs">>
                                                            ,<<"Realm">>
                                                            ,<<"Username">>
                                                            ,<<"Usernames">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Active-Only">>, fun kz_term:is_boolean/1}
                ,{<<"Authorizing-IDs">>, fun erlang:is_list/1}
                ,{<<"Username">>, fun erlang:is_binary/1}
                ,{<<"Usernames">>, fun erlang:is_list/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec query_user_channels_resp_definition() -> kapi_definition:api().
query_user_channels_resp_definition() ->
    EventName = <<"query_user_channels_resp">>,
    Category = <<"call_event">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Query User Channels Resp">>}
              ,{fun kapi_definition:set_description/2, <<"Inquire into the status of a call">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun query_user_channels_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun query_user_channels_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_query_user_channels_resp/2}
              ,{fun kapi_definition:set_required_headers/2, []}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Channels">>]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec query_account_channels_req_definition() -> kapi_definition:api().
query_account_channels_req_definition() ->
    EventName = <<"query_account_channels_req">>,
    Category = <<"call_event">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Query Account Channels Req">>}
              ,{fun kapi_definition:set_description/2, <<"Inquire into the status of a call">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun query_account_channels_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun query_account_channels_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_query_account_channels_req/1}
              ,{fun kapi_definition:set_binding/2, fun call_routing_key/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Active-Only">>
                                                            ,<<"Username">>
                                                            ,<<"Usernames">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Active-Only">>, fun kz_term:is_boolean/1}
                ,{<<"Username">>, fun erlang:is_binary/1}
                ,{<<"Usernames">>, fun erlang:is_list/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec query_account_channels_resp_definition() -> kapi_definition:api().
query_account_channels_resp_definition() ->
    EventName = <<"query_account_channels_resp">>,
    Category = <<"call_event">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Query Account Channels Resp">>}
              ,{fun kapi_definition:set_description/2, <<"Inquire into the status of a call">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun query_account_channels_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun query_account_channels_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_query_account_channels_resp/2}
              ,{fun kapi_definition:set_required_headers/2, []}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Channels">>]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec query_channels_req_definition() -> kapi_definition:api().
query_channels_req_definition() ->
    EventName = <<"query_channels_req">>,
    Category = <<"call_event">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Query Channels Req">>}
              ,{fun kapi_definition:set_description/2, <<"Inquire into the status of a call">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun query_channels_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun query_channels_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_query_channels_req/1}
              ,{fun kapi_definition:set_binding/2, fun call_routing_key/2}
              ,{fun kapi_definition:set_required_headers/2, []}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Active-Only">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Fields">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Active-Only">>, fun kz_term:is_boolean/1}]
               }
              ],
    kapi_definition:setters(Setters).

-spec query_channels_resp_definition() -> kapi_definition:api().
query_channels_resp_definition() ->
    EventName = <<"query_channels_resp">>,
    Category = <<"call_event">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Query Channels Resp">>}
              ,{fun kapi_definition:set_description/2, <<"Inquire into the status of a call">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun query_channels_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun query_channels_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_query_channels_resp/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Channels">>]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Channels">>, fun kz_json:is_json_object/1}]
               }
              ],
    kapi_definition:setters(Setters).

-spec usurp_control_definition() -> kapi_definition:api().
usurp_control_definition() ->
    EventName = <<"usurp_control">>,
    Category = <<"call_event">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Call Usurp Control">>}
              ,{fun kapi_definition:set_description/2
               ,<<"Format a call id update from the switch for the listener">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun usurp_control/1}
              ,{fun kapi_definition:set_validate_fun/2, fun usurp_control_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_usurp_control/2}
              ,{fun kapi_definition:set_binding/2, fun call_routing_key/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Call-ID">>
                                                            ,<<"Fetch-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Media-Node">>
                                                            ,<<"Reason">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec usurp_publisher_definition() -> kapi_definition:api().
usurp_publisher_definition() ->
    EventName = <<"usurp_publisher">>,
    Category = <<"call_event">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Usurp Call Event Publisher">>}
              ,{fun kapi_definition:set_description/2
               ,<<"Format a call id update from the switch for the listener">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun usurp_publisher/1}
              ,{fun kapi_definition:set_validate_fun/2, fun usurp_publisher_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_usurp_publisher/2}
              ,{fun kapi_definition:set_binding/2, fun call_routing_key/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Call-ID">>
                                                            ,<<"Reference">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Media-Node">>
                                                            ,<<"Reason">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Format a call event from the switch for the listener.
%% Takes {@link lz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec event(kz_term:api_terms()) -> kz_api:api_formatter_return().
event(Req) ->
    kapi_definition:build_message(Req, update_name_and_values(event_definition(), Req)).

-spec event_v(kz_term:api_terms()) -> boolean().
event_v(Req) ->
    kapi_definition:validate(Req, update_name_and_values(event_definition(), Req)).

-spec publish_event(kz_term:api_terms()) -> 'ok'.
publish_event(Event) ->
    publish_event(Event, ?DEFAULT_CONTENT_TYPE).

-spec publish_event(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_event(Event, ContentType) when is_list(Event) ->
    CallId = find_event_call_id(Event),
    Definition = update_name_and_values(event_definition(), Event),
    EventName = kapi_definition:name(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(Event
                                                ,kapi_definition:values(Definition)
                                                ,[{'formatter', kapi_definition:build_fun(Definition)}
                                                 ,{'remove_recursive', 'false'}
                                                 ]
                                                ),
    kz_amqp_util:callevt_publish((kapi_definition:binding(Definition))(EventName, CallId)
                                ,Payload
                                ,ContentType
                                ,[{'headers', [{<<"call-id">>, binary, CallId}]}]
                                );
publish_event(Event, ContentType) ->
    publish_event(kz_json:to_proplist(Event), ContentType).

%%------------------------------------------------------------------------------
%% @doc Inquire into the status of a channel.
%% Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec channel_status_req(kz_term:api_terms()) -> kz_api:api_formatter_return().
channel_status_req(Req) ->
    kapi_definition:build_message(Req, channel_status_req_definition()).

-spec channel_status_req_v(kz_term:api_terms()) -> boolean().
channel_status_req_v(Req) ->
    kapi_definition:validate(Req, channel_status_req_definition()).

-spec publish_channel_status_req(kz_term:api_terms()) -> 'ok'.
publish_channel_status_req(API) ->
    publish_channel_status_req(API, kz_api:call_id(API)).

-spec publish_channel_status_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_channel_status_req(API, CallId) ->
    publish_channel_status_req(API, CallId, ?DEFAULT_CONTENT_TYPE).

-spec publish_channel_status_req(kz_term:api_terms(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
publish_channel_status_req(Req, CallId, ContentType) ->
    Definition = channel_status_req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:callevt_publish((kapi_definition:binding(Definition))('status_req', CallId)
                                ,Payload
                                ,ContentType
                                ).

%%------------------------------------------------------------------------------
%% @doc Respond with status of a channel, either active or nonexistent.
%% Takes {@link lz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec channel_status_resp(kz_term:api_terms()) -> kz_api:api_formatter_return().
channel_status_resp(Req) ->
    kapi_definition:build_message(Req, channel_status_resp_definition()).

-spec channel_status_resp_v(kz_term:api_terms()) -> boolean().
channel_status_resp_v(Req) ->
    kapi_definition:validate(Req, channel_status_resp_definition()).

-spec publish_channel_status_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_channel_status_resp(RespQ, JObj) ->
    publish_channel_status_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_channel_status_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_channel_status_resp(RespQ, Resp, ContentType) ->
    Definition = channel_status_resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Resp
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Inquire into the status of a call.
%% Takes {@link lz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec query_auth_id_req(kz_term:api_terms()) -> kz_api:api_formatter_return().
query_auth_id_req(Req) ->
    kapi_definition:build_message(Req, query_auth_id_req_definition()).

-spec query_auth_id_req_v(kz_term:api_terms()) -> boolean().
query_auth_id_req_v(Req) ->
    kapi_definition:validate(Req, query_auth_id_req_definition()).

-spec publish_query_auth_id_req(kz_term:api_terms()) -> 'ok'.
publish_query_auth_id_req(API) ->
    publish_query_auth_id_req(API, auth_id(API)).

-spec publish_query_auth_id_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_query_auth_id_req(API, AuthId) ->
    publish_query_auth_id_req(API, AuthId, ?DEFAULT_CONTENT_TYPE).

-spec publish_query_auth_id_req(kz_term:api_terms(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
publish_query_auth_id_req(Req, AuthId, ContentType) ->
    Definition = query_auth_id_req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:callevt_publish((kapi_definition:binding(Definition))('status_req', AuthId)
                                ,Payload
                                ,ContentType
                                ).

%%------------------------------------------------------------------------------
%% @doc Inquire into the status of a call.
%% Takes {@link lz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec query_auth_id_resp(kz_term:api_terms()) -> kz_api:api_formatter_return().
query_auth_id_resp(Req) ->
    kapi_definition:build_message(Req, query_auth_id_resp_definition()).

-spec query_auth_id_resp_v(kz_term:api_terms()) -> boolean().
query_auth_id_resp_v(Req) ->
    kapi_definition:validate(Req, query_auth_id_resp_definition()).

-spec publish_query_auth_id_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_query_auth_id_resp(RespQ, JObj) ->
    publish_query_auth_id_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_query_auth_id_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_query_auth_id_resp(RespQ, Resp, ContentType) ->
    Definition = query_auth_id_resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Resp
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Inquire into the status of a call.
%% Takes {@link lz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec query_user_channels_req(kz_term:api_terms()) -> kz_api:api_formatter_return().
query_user_channels_req(Req) ->
    kapi_definition:build_message(Req, query_user_channels_req_definition()).

-spec query_user_channels_req_v(kz_term:api_terms()) -> boolean().
query_user_channels_req_v(Req) ->
    kapi_definition:validate(Req, query_user_channels_req_definition()).

-spec publish_query_user_channels_req(kz_term:api_terms()) -> 'ok'.
publish_query_user_channels_req(API) ->
    publish_query_user_channels_req(API, username(API), realm(API), ?DEFAULT_CONTENT_TYPE).

-spec publish_query_user_channels_req(kz_term:api_terms(), kz_term:api_binary(), kz_term:api_binary(), kz_term:ne_binary()) -> 'ok'.
publish_query_user_channels_req(Req, 'undefined', 'undefined', ContentType) ->
    Definition = query_user_channels_req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:callevt_publish((kapi_definition:binding(Definition))('status_req', <<>>)
                                ,Payload
                                ,ContentType
                                );
publish_query_user_channels_req(Req, 'undefined', Realm, ContentType) ->
    Username = first_username(Req),
    publish_query_user_channels_req(Req, Username, Realm, ContentType);
publish_query_user_channels_req(Req, Username, Realm, ContentType) ->
    Definition = query_user_channels_req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    User = <<Username/binary, ":", Realm/binary>>,
    kz_amqp_util:callevt_publish((kapi_definition:binding(Definition))('status_req', User)
                                ,Payload
                                ,ContentType
                                ).

%%------------------------------------------------------------------------------
%% @doc Inquire into the status of a call.
%% Takes {@link lz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec query_user_channels_resp(kz_term:api_terms()) -> kz_api:api_formatter_return().
query_user_channels_resp(Req) ->
    kapi_definition:build_message(Req, query_user_channels_resp_definition()).

-spec query_user_channels_resp_v(kz_term:api_terms()) -> boolean().
query_user_channels_resp_v(Req) ->
    kapi_definition:validate(Req, query_user_channels_resp_definition()).

-spec publish_query_user_channels_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_query_user_channels_resp(RespQ, JObj) ->
    publish_query_user_channels_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_query_user_channels_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_query_user_channels_resp(RespQ, Resp, ContentType) ->
    Definition = query_user_channels_resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Resp
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Inquire into the status of a call.
%% Takes {@link lz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec query_account_channels_req(kz_term:api_terms()) -> kz_api:api_formatter_return().
query_account_channels_req(Req) ->
    kapi_definition:build_message(Req, query_account_channels_req_definition()).

-spec query_account_channels_req_v(kz_term:api_terms()) -> boolean().
query_account_channels_req_v(Req) ->
    kapi_definition:validate(Req, query_account_channels_req_definition()).

-spec publish_query_account_channels_req(kz_term:api_terms()) -> 'ok'.
publish_query_account_channels_req(API) ->
    publish_query_account_channels_req(API, kz_api:account_id(API), ?DEFAULT_CONTENT_TYPE).

-spec publish_query_account_channels_req(kz_term:api_terms(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
publish_query_account_channels_req(Req, AccountId, ContentType) ->
    Definition = query_account_channels_req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:callevt_publish((kapi_definition:binding(Definition))('status_req', AccountId)
                                ,Payload
                                ,ContentType
                                ).

%%------------------------------------------------------------------------------
%% @doc Inquire into the status of a call.
%% Takes {@link lz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec query_account_channels_resp(kz_term:api_terms()) -> kz_api:api_formatter_return().
query_account_channels_resp(Req) ->
    kapi_definition:build_message(Req, query_account_channels_resp_definition()).

-spec query_account_channels_resp_v(kz_term:api_terms()) -> boolean().
query_account_channels_resp_v(Req) ->
    kapi_definition:validate(Req, query_account_channels_resp_definition()).

-spec publish_query_account_channels_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_query_account_channels_resp(RespQ, JObj) ->
    publish_query_account_channels_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_query_account_channels_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_query_account_channels_resp(RespQ, Resp, ContentType) ->
    Definition = query_account_channels_resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Resp
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Inquire into the status of a call.
%% Takes {@link lz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec query_channels_req(kz_term:api_terms()) -> kz_api:api_formatter_return().
query_channels_req(Req) ->
    kapi_definition:build_message(Req, query_channels_req_definition()).

-spec query_channels_req_v(kz_term:api_terms()) -> boolean().
query_channels_req_v(Req) ->
    kapi_definition:validate(Req, query_channels_req_definition()).

-spec publish_query_channels_req(kz_term:api_terms()) -> 'ok'.
publish_query_channels_req(APIProps) ->
    publish_query_channels_req(APIProps, ?DEFAULT_CONTENT_TYPE).

-spec publish_query_channels_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_query_channels_req(APIProps, ContentType) when is_list(APIProps) ->
    Definition = query_channels_req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(APIProps
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:callevt_publish((kapi_definition:binding(Definition))('status_req', <<"channels">>)
                                ,Payload
                                ,ContentType
                                );
publish_query_channels_req(JObj, ContentType) ->
    publish_query_channels_req(kz_json:to_proplist(JObj), ContentType).

%%------------------------------------------------------------------------------
%% @doc Inquire into the status of a call.
%% Takes {@link lz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec query_channels_resp(kz_term:api_terms()) -> kz_api:api_formatter_return().
query_channels_resp(Req) ->
    kapi_definition:build_message(Req, query_channels_resp_definition()).

-spec query_channels_resp_v(kz_term:api_terms()) -> boolean().
query_channels_resp_v(Req) ->
    kapi_definition:validate(Req, query_channels_resp_definition()).

-spec publish_query_channels_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_query_channels_resp(RespQ, APIProps) ->
    publish_query_channels_resp(RespQ, APIProps, ?DEFAULT_CONTENT_TYPE).

-spec publish_query_channels_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_query_channels_resp(RespQ, APIProps, ContentType) when is_list(APIProps) ->
    Definition = query_channels_resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(APIProps
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType);
publish_query_channels_resp(RespQ, JObj, ContentType) ->
    publish_query_channels_resp(RespQ, kz_json:to_proplist(JObj), ContentType).

%%------------------------------------------------------------------------------
%% @doc Format a call id update from the switch for the listener.
%% Takes {@link lz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec usurp_control(kz_term:api_terms()) -> kz_api:api_formatter_return().
usurp_control(Req) ->
    kapi_definition:build_message(Req, usurp_control_definition()).

-spec usurp_control_v(kz_term:api_terms()) -> boolean().
usurp_control_v(Req) ->
    kapi_definition:validate(Req, usurp_control_definition()).

-spec publish_usurp_control(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_usurp_control(CallId, JObj) ->
    publish_usurp_control(CallId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_usurp_control(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_usurp_control(CallId, JObj, ContentType) ->
    Definition = usurp_control_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(JObj
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:callevt_publish((kapi_definition:binding(Definition))('usurp_control', CallId)
                                ,Payload
                                ,ContentType
                                ).

%%------------------------------------------------------------------------------
%% @doc Format a call id update from the switch for the listener.
%% Takes {@link lz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec usurp_publisher(kz_term:api_terms()) -> kz_api:api_formatter_return().
usurp_publisher(Req) ->
    kapi_definition:build_message(Req, usurp_publisher_definition()).

-spec usurp_publisher_v(kz_term:api_terms()) -> boolean().
usurp_publisher_v(Req) ->
    kapi_definition:validate(Req, usurp_publisher_definition()).

-spec publish_usurp_publisher(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_usurp_publisher(CallId, JObj) ->
    publish_usurp_publisher(CallId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_usurp_publisher(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_usurp_publisher(CallId, JObj, ContentType) ->
    Definition = usurp_publisher_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(JObj
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:callevt_publish((kapi_definition:binding(Definition))('usurp_publisher', CallId)
                                ,Payload
                                ,ContentType
                                ).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    CallId = props:get_value('callid', Props, <<"*">>),
    Events = props:get_value('restrict_to', Props, [<<"*">>]),
    bind_q(Queue, Events, CallId).

bind_q(Q, [Event|T], CallId) ->
    _ = kz_amqp_util:bind_q_to_callevt(Q, call_routing_key(Event, CallId)),
    bind_q(Q, T, CallId);
bind_q(_Q, [], _CallId) -> 'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    CallId = props:get_value('callid', Props, <<"*">>),
    Events = props:get_value('restrict_to', Props, [<<"*">>]),
    unbind_q(Queue, Events, CallId).

unbind_q(Q, [Event|T], CallId) ->
    _ = kz_amqp_util:unbind_q_from_callevt(Q, call_routing_key(Event, CallId)),
    unbind_q(Q, T, CallId);
unbind_q(_Q, [], _CallId) -> 'ok'.

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:callevt_exchange(),
    kz_amqp_util:callmgr_exchange().

-spec find_event_call_id(kz_term:proplist()) -> kz_term:api_ne_binary().
find_event_call_id(Event) ->
    Keys = case props:is_true(<<"Channel-Is-Loopback">>, Event, 'false') of
               'true' -> [<<"Call-ID">>, <<"Unique-ID">>];
               'false' -> [<<"Origination-Call-ID">>, <<"Call-ID">>, <<"Unique-ID">>]
           end,
    props:get_first_defined(Keys, Event).

-spec get_status(kz_term:api_terms()) -> kz_term:ne_binary().
get_status(API) when is_list(API) -> props:get_value(<<"Status">>, API);
get_status(API) -> kz_json:get_value(<<"Status">>, API).

-spec first_username(kz_term:api_terms()) -> kz_term:ne_binary().
first_username(Props) when is_list(Props) ->
    [U|_] = props:get_value(<<"Usernames">>, Props),
    U;
first_username(JObj) ->
    [U|_] = kz_json:get_value(<<"Usernames">>, JObj),
    U.

-spec event_routing_key(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
event_routing_key(EventName, CallId) ->
    call_routing_key(EventName, CallId).

-spec auth_id(kz_term:api_terms()) -> kz_term:ne_binary().
auth_id(Props) when is_list(Props) ->
    props:get_ne_binary_value(<<"Auth-ID">>, Props);
auth_id(JObj) ->
    kz_json:get_ne_binary_value(<<"Auth-ID">>, JObj).

-spec username(kz_term:api_terms()) -> kz_term:ne_binary().
username(Props) when is_list(Props) ->
    props:get_ne_binary_value(<<"Username">>, Props);
username(JObj) ->
    kz_json:get_ne_binary_value(<<"Username">>, JObj).

-spec realm(kz_term:api_terms()) -> kz_term:ne_binary().
realm(Props) when is_list(Props) ->
    props:get_ne_binary_value(<<"Realm">>, Props);
realm(JObj) ->
    kz_json:get_ne_binary_value(<<"Realm">>, JObj).

-spec call_routing_key(kz_term:ne_binary() | atom(), kz_term:ne_binary()) -> kz_term:ne_binary().
call_routing_key(Event, CallId) ->
    list_to_binary(["call."
                   ,kz_term:to_binary(Event)
                   ,"."
                   ,kz_amqp_util:encode(CallId)
                   ]).

%% Helpers
-spec update_name_and_values(kapi_definition:api(), kz_term:api_terms()) -> kapi_definition:api().
update_name_and_values(Definition, Req) when is_list(Req)
                                             andalso is_tuple(hd(Req)) ->
    EventName = props:get_value(<<"Event-Name">>, Req),
    kapi_definition:set_values(kapi_definition:set_name(Definition, EventName)
                              ,kapi_definition:event_type_headers(kapi_definition:category(Definition)
                                                                 ,EventName
                                                                 )
                              );
update_name_and_values(Definition, Req) ->
    update_name_and_values(Definition, Req, kz_json:is_json_object(Req)).

-spec update_name_and_values(kapi_definition:api(), kz_term:api_terms(), boolean()) -> kapi_definition:api().
update_name_and_values(Definition, Req, 'true') ->
    update_name_and_values(Definition, kz_json:to_proplist(Req)).
