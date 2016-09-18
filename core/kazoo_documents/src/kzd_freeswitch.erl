%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% FreeSWITCH proplists
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzd_freeswitch).

-export([caller_id_name/1, caller_id_name/2
        ,caller_id_number/1, caller_id_number/2
        ,callee_id_name/1, callee_id_name/2
        ,callee_id_number/1, callee_id_number/2
        ,dialed_number/1
        ,call_id/1
        ,other_leg_call_id/1
        ,call_direction/1
        ,resource_type/1, resource_type/2
        ,channel_authorized/1
        ,hunt_destination_number/1
        ,is_channel_recovering/1, is_channel_recovering/2
        ,is_consuming_global_resource/1, is_consuming_global_resource/2
        ,resource_id/1

        ,hangup_code/1, hangup_cause/1
        ,disposition/1

        ,transfer_history/1
        ,transfer_source/1

        ,authorizing_id/1
        ,authorizing_type/1

        ,account_id/1, account_billing/1
        ,reseller_id/1, reseller_billing/1

        ,from_network_ip/1, from_network_port/1
        ,user_agent/1

        ,to_did/1

        ,application_name/1, raw_application_name/1
        ,event_name/1

        ,ccv/2, ccv/3

        ,is_loopback/1, loopback_other_leg/1, loopback_leg_name/1

        ,media_recorder/1
        ]).

-include("kz_documents.hrl").

-type data() :: kz_proplist().
-export_type([data/0]).

-define(CHANNEL_VAR_PREFIX, "ecallmgr_").
-define(CCV(Key), <<"variable_", ?CHANNEL_VAR_PREFIX, Key/binary>>).

-spec caller_id_name(data()) -> api_binary().
-spec caller_id_name(data(), Default) -> ne_binary() | Default.
caller_id_name(Props) ->
    caller_id_name(Props, 'undefined').
caller_id_name(Props, Default) ->
    props:get_first_defined([<<"variable_effective_caller_id_name">>
                            ,<<"Caller-Caller-ID-Name">>
                            ]
                           ,Props
                           ,Default
                           ).

-spec caller_id_number(data()) -> api_binary().
-spec caller_id_number(data(), Default) -> ne_binary() | Default.
caller_id_number(Props) ->
    caller_id_number(Props, 'undefined').
caller_id_number(Props, Default) ->
    props:get_first_defined([<<"variable_effective_caller_id_number">>
                            ,<<"Caller-Caller-ID-Number">>
                            ]
                           ,Props
                           ,Default
                           ).

-spec callee_id_name(data()) -> api_binary().
-spec callee_id_name(data(), Default) -> ne_binary() | Default.
callee_id_name(Props) ->
    callee_id_name(Props, 'undefined').
callee_id_name(Props, Default) ->
    props:get_first_defined([<<"variable_effective_callee_id_name">>
                            ,<<"Caller-Callee-ID-Name">>
                            ]
                           ,Props
                           ,Default
                           ).

-spec callee_id_number(data()) -> api_binary().
-spec callee_id_number(data(), Default) -> ne_binary() | Default.
callee_id_number(Props) ->
    callee_id_number(Props, 'undefined').
callee_id_number(Props, Default) ->
    props:get_first_defined([<<"variable_effective_callee_id_number">>
                            ,<<"Caller-Callee-ID-Number">>
                            ]
                           ,Props
                           ,Default
                           ).

-spec dialed_number(data()) -> api_binary().
dialed_number(Props) ->
    props:get_first_defined([<<"variable_destination_number">>
                            ,<<"Caller-Destination-Number">>
                            ]
                           ,Props
                           ).

-spec call_id(data()) -> api_binary().
call_id(Props) ->
    props:get_first_defined([<<"Caller-Unique-ID">>
                            ,<<"Unique-ID">>
                            ,<<"Call-ID">>
                            ,<<"variable_uuid">>
                            ,<<"Channel-Call-UUID">>
                            ,<<"variable_sip_call_id">>
                            ,?RESIGNING_UUID
                            ], Props).

-spec other_leg_call_id(data()) -> api_binary().
other_leg_call_id(Props) ->
    props:get_value(<<"Other-Leg-Unique-ID">>, Props).

-spec call_direction(data()) -> api_binary().
call_direction(Props) ->
    props:get_first_defined([<<"Application-Logical-Direction">>
                            ,?CCV(<<"Application-Logical-Direction">>)
                            ,<<"Caller-Logical-Direction">>
                            ,<<"Call-Direction">>
                            ]
                           ,Props
                           ).

-spec resource_type(data()) -> api_binary().
-spec resource_type(data(), Default) -> ne_binary() | Default.
resource_type(Props) ->
    resource_type(Props, 'undefined').

resource_type(Props, Default) ->
    props:get_value(<<"Resource-Type">>, Props, Default).

-spec channel_authorized(data()) -> api_binary().
channel_authorized(Props) ->
    ccv(Props, <<"Channel-Authorized">>).

-spec hunt_destination_number(data()) -> api_binary().
hunt_destination_number(Props) ->
    props:get_value(<<"Hunt-Destination-Number">>, Props).

-spec is_channel_recovering(data()) -> boolean().
-spec is_channel_recovering(data(), boolean()) -> boolean().
is_channel_recovering(Props) ->
    is_channel_recovering(Props, 'false').

is_channel_recovering(Props, Default) ->
    props:is_true(<<"variable_recovered">>, Props, Default).

-spec is_consuming_global_resource(data()) -> api_boolean().
-spec is_consuming_global_resource(data(), api_boolean()) -> api_boolean().
is_consuming_global_resource(Props) ->
    is_consuming_global_resource(Props, 'undefined').

is_consuming_global_resource(Props, Default) ->
    kz_util:is_true(ccv(Props, <<"Global-Resource">>, Default)).

-spec resource_id(data()) -> api_binary().
resource_id(Props) ->
    ccv(Props, <<"Resource-ID">>).

-spec authorizing_id(data()) -> api_binary().
authorizing_id(Props) ->
    ccv(Props, <<"Authorizing-ID">>).

-spec authorizing_type(data()) -> api_binary().
authorizing_type(Props) ->
    ccv(Props, <<"Authorizing-Type">>).

-spec account_id(data()) -> api_binary().
account_id(Props) ->
    ccv(Props, <<"Account-ID">>).

-spec account_billing(data()) -> api_binary().
account_billing(Props) ->
    ccv(Props, <<"Account-Billing">>).

-spec reseller_id(data()) -> api_binary().
reseller_id(Props) ->
    ccv(Props, <<"Reseller-ID">>).

-spec reseller_billing(data()) -> api_binary().
reseller_billing(Props) ->
    ccv(Props, <<"Reseller-Billing">>).

-spec to_did(data()) -> api_binary().
to_did(Props) ->
    props:get_first_defined([?CCV(<<"E164-Destination">>)
                            ,?CCV(<<"Original-Number">>)
                            ,<<"Caller-Destination-Number">>
                            ]
                           ,Props
                           ).

-spec ccv(data(), ne_binary()) -> api_binary().
ccv(Props, Key) ->
    ccv(Props, Key, 'undefined').

-spec ccv(data(), ne_binary(), Default) -> ne_binary() | Default.
ccv(Props, Key, Default) ->
    props:get_value(?CCV(Key), Props, Default).

-spec hangup_code(data()) -> api_binary().
hangup_code(Props) ->
    props:get_first_defined([<<"variable_proto_specific_hangup_cause">>
                            ,<<"variable_last_bridge_proto_specific_hangup_cause">>
                            ], Props).

-spec disposition(data()) -> api_binary().
disposition(Props) ->
    props:get_first_defined([<<"variable_originate_disposition">>
                            ,<<"variable_endpoint_disposition">>
                            ], Props).

-spec hangup_cause(data()) -> api_binary().
hangup_cause(Props) ->
    case props:get_value(<<"variable_current_application">>, Props) of
        <<"bridge">> ->
            props:get_first_defined([<<"variable_bridge_hangup_cause">>
                                    ,<<"variable_hangup_cause">>
                                    ,<<"Hangup-Cause">>
                                    ], Props);
        _Else ->
            props:get_first_defined([<<"variable_hangup_cause">>
                                    ,<<"variable_bridge_hangup_cause">>
                                    ,<<"Hangup-Cause">>
                                    ], Props)
    end.

-spec transfer_history(data()) -> api_binary() | kz_proplist().
transfer_history(Props) ->
    props:get_value(<<"variable_transfer_history">>, Props).

-spec transfer_source(data()) -> api_binary() | kz_proplist().
transfer_source(Props) ->
    props:get_value(<<"variable_transfer_source">>, Props).

-spec raw_application_name(data()) -> api_binary().
raw_application_name(Props) ->
    props:get_first_defined([<<"Application">>
                            ,<<"kazoo_application_name">>
                            ,<<"Event-Subclass">>
                            ], Props).

-spec application_name(data()) -> api_binary().
application_name(Props) ->
    props:get_first_defined([<<"kazoo_application_name">>
                            ,<<"Application">>
                            ,<<"Event-Subclass">>
                            ], Props).

-spec event_name(data()) -> api_binary().
event_name(Props) ->
    props:get_first_defined([<<"kazoo_event_name">>
                            ,<<"Event-Name">>
                            ], Props).

-spec from_network_ip(data()) -> api_binary().
from_network_ip(Props) ->
    props:get_first_defined([<<"variable_sip_h_X-AUTH-IP">>
                            ,<<"variable_sip_received_ip">>
                            ]
                           ,Props
                           ).

-spec from_network_port(data()) -> api_binary().
from_network_port(Props) ->
    props:get_first_defined([<<"variable_sip_h_X-AUTH-PORT">>
                            ,<<"variable_sip_received_port">>
                            ]
                           ,Props
                           ).

-spec user_agent(data()) -> api_binary().
user_agent(Props) ->
    props:get_first_defined([<<"variable_sip_user_agent">>
                            ,<<"sip_user_agent">>
                            ]
                           ,Props
                           ).

-spec loopback_leg_name(data()) -> api_binary().
loopback_leg_name(Props) ->
    props:get_value(<<"variable_loopback_leg">>, Props).

-spec is_loopback(data()) -> boolean().
is_loopback(Props) ->
    props:get_value(<<"variable_loopback_leg">>, Props) =/= 'undefined'.

-spec loopback_other_leg(data()) -> api_binary().
loopback_other_leg(Props) ->
    props:get_value(<<"variable_other_loopback_leg_uuid">>, Props).

-spec media_recorder(data()) -> api_binary().
media_recorder(Props) -> ccv(Props, <<"Media-Recorder">>).
