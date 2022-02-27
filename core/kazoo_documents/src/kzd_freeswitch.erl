%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc FreeSWITCH proplists
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_freeswitch).

-export([account_id/1, account_billing/1, account_trunk_usage/1
        ,application_name/1, raw_application_name/1
        ,authorizing_id/1
        ,authorizing_type/1
        ,call_direction/1, original_call_direction/1
        ,call_id/1
        ,callee_id_name/1, callee_id_name/2
        ,callee_id_number/1, callee_id_number/2
        ,caller_id_name/1, caller_id_name/2
        ,caller_id_number/1, caller_id_number/2
        ,ccvs/1, ccv/2, ccv/3
        ,channel_authorized/1
        ,conference_name/1, conference_profile_name/1, conference_uuid/1
        ,context/1, context/2
        ,dialed_number/1
        ,disposition/1
        ,event_name/1
        ,from_network_ip/1, from_network_port/1
        ,from_user/1, from_realm/1
        ,from_tag/1, to_tag/1
        ,hangup_code/1, hangup_cause/1
        ,hostname/1, hostname/2
        ,hunt_destination_number/1
        ,hunt_context/1, hunt_context/2
        ,is_channel_recovering/1, is_channel_recovering/2
        ,is_consuming_global_resource/1, is_consuming_global_resource/2
        ,is_loopback/1, loopback_other_leg/1, loopback_leg_name/1
        ,join_time/1, join_time/2
        ,media_recorder/1
        ,origination_call_id/1
        ,other_leg_call_id/1
        ,outbound_flags/1
        ,presence_id/1, presence_direction/1
        ,request_realm/1
        ,reseller_id/1, reseller_billing/1, reseller_trunk_usage/1
        ,resource_id/1
        ,resource_type/1, resource_type/2
        ,to_did/1, to_realm/1
        ,transfer_history/1
        ,transfer_source/1
        ,user_agent/1

        ,core_uuid/1
        ,fetch_uuid/1
        ,fetch_winning_pid/1
        ,switch_url/1, switch_uri/1
        ,switch_nodename/1
        ]).

-include("kz_documents.hrl").

-type data() :: kz_term:proplist().
-export_type([data/0]).

-define(CHANNEL_VAR_PREFIX, "ecallmgr_").
-define(CCV(Key), <<"variable_", ?CHANNEL_VAR_PREFIX, Key/binary>>).
-define(CCV_HEADER(Key), <<"variable_sip_h_X-", ?CHANNEL_VAR_PREFIX, Key/binary>>).

-spec caller_id_name(data()) -> kz_term:api_binary().
caller_id_name(Props) ->
    caller_id_name(Props, 'undefined').

-spec caller_id_name(data(), Default) -> kz_term:ne_binary() | Default.
caller_id_name(Props, Default) ->
    props:get_first_defined([<<"variable_origination_caller_id_name">>
                            ,<<"variable_effective_caller_id_name">>
                            ,<<"Caller-Caller-ID-Name">>
                            ]
                           ,Props
                           ,Default
                           ).

-spec caller_id_number(data()) -> kz_term:api_binary().
caller_id_number(Props) ->
    caller_id_number(Props, 'undefined').

-spec caller_id_number(data(), Default) -> kz_term:ne_binary() | Default.
caller_id_number(Props, Default) ->
    props:get_first_defined([<<"variable_origination_caller_id_number">>
                            ,<<"variable_effective_caller_id_number">>
                            ,<<"Caller-Caller-ID-Number">>
                            ]
                           ,Props
                           ,Default
                           ).

-spec callee_id_name(data()) -> kz_term:api_binary().
callee_id_name(Props) ->
    callee_id_name(Props, 'undefined').

-spec callee_id_name(data(), Default) -> kz_term:ne_binary() | Default.
callee_id_name(Props, Default) ->
    props:get_first_defined([<<"variable_origination_callee_id_name">>
                            ,<<"variable_effective_callee_id_name">>
                            ,<<"Caller-Callee-ID-Name">>
                            ]
                           ,Props
                           ,Default
                           ).

-spec callee_id_number(data()) -> kz_term:api_binary().
callee_id_number(Props) ->
    callee_id_number(Props, 'undefined').

-spec callee_id_number(data(), Default) -> kz_term:ne_binary() | Default.
callee_id_number(Props, Default) ->
    props:get_first_defined([<<"variable_origination_callee_id_number">>
                            ,<<"variable_effective_callee_id_number">>
                            ,<<"Caller-Callee-ID-Number">>
                            ]
                           ,Props
                           ,Default
                           ).

-spec dialed_number(data()) -> kz_term:api_binary().
dialed_number(Props) ->
    props:get_first_defined([<<"variable_destination_number">>
                            ,<<"Caller-Destination-Number">>
                            ]
                           ,Props
                           ).

-spec call_id(data()) -> kz_term:api_binary().
call_id(Props) ->
    props:get_first_defined([<<"Caller-Unique-ID">>
                            ,<<"Unique-ID">>
                            ,<<"Call-ID">>
                            ,<<"variable_uuid">>
                            ,<<"Channel-Call-UUID">>
                            ,<<"variable_sip_call_id">>
                            ,?RESIGNING_UUID
                            ], Props).

-spec other_leg_call_id(data()) -> kz_term:api_binary().
other_leg_call_id(Props) ->
    props:get_value(<<"Other-Leg-Unique-ID">>, Props).

-spec original_call_direction(data()) -> kz_term:api_binary().
original_call_direction(Props) ->
    props:get_value(<<"Call-Direction">>, Props).

-spec call_direction(data()) -> kz_term:api_binary().
call_direction(Props) ->
    props:get_first_defined([<<"Application-Logical-Direction">>
                            ,?CCV(<<"Application-Logical-Direction">>)
                            ,<<"Call-Direction">>
                            ]
                           ,Props
                           ).

-spec resource_type(data()) -> kz_term:api_binary().
resource_type(Props) ->
    resource_type(Props, 'undefined').

-spec resource_type(data(), Default) -> kz_term:ne_binary() | Default.
resource_type(Props, Default) ->
    props:get_value(<<"Resource-Type">>, Props, Default).

-spec channel_authorized(data()) -> kz_term:api_binary().
channel_authorized(Props) ->
    ccv(Props, <<"Channel-Authorized">>).

-spec outbound_flags(data()) -> kz_term:api_binary() | kz_term:ne_binaries().
outbound_flags(Props) ->
    ccv(Props, <<"Outbound-Flags">>).

-spec hunt_destination_number(data()) -> kz_term:api_ne_binary().
hunt_destination_number(Props) ->
    case props:get_value(<<"Hunt-Destination-Number">>, Props) of
        'undefined' -> sip_req_user(Props);
        HuntDestinationNumber -> HuntDestinationNumber
    end.

-spec sip_req_user(data()) -> kz_term:api_ne_binary().
sip_req_user(Props) ->
    case props:get_value(<<"variable_sip_req_uri">>, Props) of
        'undefined' -> 'undefined';
        ReqURI ->
            case binary:split(ReqURI, <<"@">>) of
                [Number, _Realm] -> Number;
                _Split -> 'undefined'
            end
    end.

-spec is_channel_recovering(data()) -> boolean().
is_channel_recovering(Props) ->
    is_channel_recovering(Props, 'false').

-spec is_channel_recovering(data(), boolean()) -> boolean().
is_channel_recovering(Props, Default) ->
    props:is_true(<<"variable_recovered">>, Props, Default).

-spec is_consuming_global_resource(data()) -> kz_term:api_boolean().
is_consuming_global_resource(Props) ->
    is_consuming_global_resource(Props, 'undefined').

-spec is_consuming_global_resource(data(), kz_term:api_boolean()) -> kz_term:api_boolean().
is_consuming_global_resource(Props, Default) ->
    kz_term:is_true(ccv(Props, <<"Global-Resource">>, Default)).

-spec resource_id(data()) -> kz_term:api_binary().
resource_id(Props) ->
    ccv(Props, <<"Resource-ID">>).

-spec authorizing_id(data()) -> kz_term:api_binary().
authorizing_id(Props) ->
    ccv(Props, <<"Authorizing-ID">>).

-spec authorizing_type(data()) -> kz_term:api_binary().
authorizing_type(Props) ->
    ccv(Props, <<"Authorizing-Type">>).

-spec account_id(data()) -> kz_term:api_binary().
account_id(Props) ->
    ccv(Props, <<"Account-ID">>).

-spec account_billing(data()) -> kz_term:api_binary().
account_billing(Props) ->
    ccv(Props, <<"Account-Billing">>).

-spec account_trunk_usage(data()) -> kz_term:api_binary().
account_trunk_usage(Props) ->
    ccv(Props, <<"Account-Trunk-Usage">>).

-spec reseller_id(data()) -> kz_term:api_binary().
reseller_id(Props) ->
    ccv(Props, <<"Reseller-ID">>).

-spec reseller_billing(data()) -> kz_term:api_binary().
reseller_billing(Props) ->
    ccv(Props, <<"Reseller-Billing">>).

-spec reseller_trunk_usage(data()) -> kz_term:api_binary().
reseller_trunk_usage(Props) ->
    ccv(Props, <<"Reseller-Trunk-Usage">>).

-spec to_did(data()) -> kz_term:api_binary().
to_did(Props) ->
    props:get_first_defined([?CCV(<<"E164-Destination">>)
                            ,?CCV(<<"Original-Number">>)
                            ,<<"Caller-Destination-Number">>
                            ]
                           ,Props
                           ).

-spec to_realm(data()) -> kz_term:api_ne_binary().
to_realm(Props) ->
    case ccv(Props, <<"Realm">>) of
        'undefined' -> props:get_value(<<"variable_sip_to_host">>, Props);
        Realm -> Realm
    end.

-spec request_realm(data()) -> kz_term:api_ne_binary().
request_realm(Props) ->
    case ccv(Props, <<"Realm">>) of
        'undefined' ->
            props:get_first_defined([<<"variable_sip_auth_realm">>
                                    ,<<"variable_sip_to_host">>
                                    ,<<"variable_sip_req_host">>
                                    ]
                                   ,Props
                                   );
        Realm -> Realm
    end.

-spec ccv(data(), kz_term:ne_binary()) -> kz_term:api_binary() | kz_term:ne_binaries().
ccv(Props, Key) ->
    ccv(Props, Key, 'undefined').

-spec ccv(data(), kz_term:ne_binary(), Default) -> kz_term:ne_binary() | kz_term:ne_binaries() | Default.
ccv(Props, Key, Default) ->
    props:get_value(Key, ccvs(Props), Default).

-spec hangup_code(data()) -> kz_term:api_binary().
hangup_code(Props) ->
    props:get_first_defined([<<"variable_proto_specific_hangup_cause">>
                            ,<<"variable_last_bridge_proto_specific_hangup_cause">>
                            ], Props).

-spec disposition(data()) -> kz_term:api_binary().
disposition(Props) ->
    props:get_first_defined([<<"variable_originate_disposition">>
                            ,<<"variable_endpoint_disposition">>
                            ], Props).

-spec hangup_cause(data()) -> kz_term:api_binary().
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

-spec transfer_history(data()) -> kz_term:api_binary() | kz_term:proplist().
transfer_history(Props) ->
    props:get_value(<<"variable_transfer_history">>, Props).

-spec transfer_source(data()) -> kz_term:api_binary() | kz_term:proplist().
transfer_source(Props) ->
    props:get_value(<<"variable_transfer_source">>, Props).

-spec raw_application_name(data()) -> kz_term:api_binary().
raw_application_name(Props) ->
    props:get_first_defined([<<"Application">>
                            ,<<"kazoo_application_name">>
                            ,<<"Event-Subclass">>
                            ], Props).

-spec application_name(data()) -> kz_term:api_binary().
application_name(Props) ->
    props:get_first_defined([<<"kazoo_application_name">>
                            ,<<"Application">>
                            ,<<"Event-Subclass">>
                            ], Props).

-spec event_name(data()) -> kz_term:api_binary().
event_name(Props) ->
    props:get_first_defined([<<"kazoo_event_name">>
                            ,<<"Event-Name">>
                            ], Props).

-spec from_network_ip(data()) -> kz_term:api_binary().
from_network_ip(Props) ->
    props:get_first_defined([<<"variable_sip_h_X-AUTH-IP">>
                            ,<<"variable_sip_received_ip">>
                            ]
                           ,Props
                           ).

-spec from_network_port(data()) -> kz_term:api_binary().
from_network_port(Props) ->
    props:get_first_defined([<<"variable_sip_h_X-AUTH-PORT">>
                            ,<<"variable_sip_received_port">>
                            ]
                           ,Props
                           ).

-spec user_agent(data()) -> kz_term:api_binary().
user_agent(Props) ->
    props:get_first_defined([<<"variable_sip_user_agent">>
                            ,<<"sip_user_agent">>
                            ]
                           ,Props
                           ).

-spec loopback_leg_name(data()) -> kz_term:api_binary().
loopback_leg_name(Props) ->
    props:get_value(<<"variable_loopback_leg">>, Props).

-spec is_loopback(data()) -> boolean().
is_loopback(Props) ->
    props:get_value(<<"variable_loopback_leg">>, Props) =/= 'undefined'.

-spec loopback_other_leg(data()) -> kz_term:api_binary().
loopback_other_leg(Props) ->
    props:get_value(<<"variable_other_loopback_leg_uuid">>, Props).

-spec media_recorder(data()) -> kz_term:api_binary().
media_recorder(Props) -> ccv(Props, <<"Media-Recorder">>).

-spec presence_id(data()) -> kz_term:api_binary().
presence_id(Props) ->
    props:get_first_defined([<<"Channel-Presence-ID">>
                            ,<<"variable_presence_id">>
                            ], Props).

-spec presence_direction(data()) -> kz_term:api_binary().
presence_direction(Props) ->
    props:get_value(<<"Presence-Call-Direction">>, Props).

-spec channel_var_map({kz_term:ne_binary(), kz_term:ne_binary()}) -> {kz_term:ne_binary(), kz_term:ne_binary() | kz_term:ne_binaries()}.
channel_var_map({Key, <<"ARRAY::", Serialized/binary>>}) ->
    {Key, binary:split(Serialized, <<"|:">>, ['global'])};
channel_var_map({Key, Other}) -> {Key, Other}.

%% Extract custom channel variables to include in the event

-spec ccvs(kz_term:proplist()) -> kz_term:proplist().
ccvs(Props) ->
    lists:map(fun channel_var_map/1, custom_channel_vars(Props, [])).

-spec custom_channel_vars(kz_term:proplist(), kz_term:proplist()) -> kz_term:proplist().
custom_channel_vars(Props, Initial) ->
    CCVs = lists:foldl(fun custom_channel_vars_fold/2, Initial, Props),
    maybe_update_referred_ccv(Props, channel_vars_sort(CCVs)).

-spec channel_vars_sort(kz_term:proplist()) -> kz_term:proplist().
channel_vars_sort(ChannelVars) ->
    lists:usort(fun channel_var_sort/2, ChannelVars).

-spec channel_var_sort(tuple(), tuple()) -> boolean().
channel_var_sort({A, _}, {B, _}) -> A =< B.

-spec custom_channel_vars_fold({kz_term:ne_binary(), kz_term:ne_binary()}, kz_term:proplist()) -> kz_term:proplist().
custom_channel_vars_fold({?CCV(Key), V}, Acc) ->
    [{Key, V} | Acc];
custom_channel_vars_fold({<<?CHANNEL_VAR_PREFIX, Key/binary>>, V}, Acc) ->
    [{Key, V} | Acc];
custom_channel_vars_fold({?CCV_HEADER(Key), V}, Acc) ->
    case props:is_defined(Key, Acc) of
        'true' -> Acc;
        'false' -> [{Key, V} | Acc]
    end;
custom_channel_vars_fold({<<"X-", ?CHANNEL_VAR_PREFIX, Key/binary>>, V}, Acc) ->
    case props:is_defined(Key, Acc) of
        'true' -> Acc;
        'false' -> [{Key, V} | Acc]
    end;
custom_channel_vars_fold(_, Acc) -> Acc.

-spec maybe_update_referred_ccv(kz_term:proplist(), kz_term:proplist()) -> kz_term:proplist().
maybe_update_referred_ccv(Props, CCVs) ->
    ReferredBy = props:get_value(<<"variable_sip_h_Referred-By">>, Props),
    ReferTo = props:get_value(<<"variable_sip_refer_to">>, Props),
    update_referred_by_ccv(ReferredBy
                          ,update_referred_to_ccv(ReferTo, CCVs)
                          ).

-spec update_referred_by_ccv(kz_term:api_binary(), kz_term:proplist()) -> kz_term:proplist().
update_referred_by_ccv('undefined', CCVs) -> props:delete(<<"Referred-By">>, CCVs);
update_referred_by_ccv(ReferredBy, CCVs) ->
    props:set_value(<<"Referred-By">>
                   ,kz_http_util:urldecode(ReferredBy)
                   ,CCVs
                   ).

-spec update_referred_to_ccv(kz_term:api_binary(), kz_term:proplist()) -> kz_term:proplist().
update_referred_to_ccv('undefined', CCVs) -> props:delete(<<"Referred-To">>, CCVs);
update_referred_to_ccv(ReferredTo, CCVs) ->
    props:set_value(<<"Referred-To">>
                   ,kz_http_util:urldecode(ReferredTo)
                   ,CCVs
                   ).

-spec from_user(data()) -> kz_term:ne_binary().
from_user(Props) ->
    props:get_value(<<"sip_from_user">>, Props, <<"nouser">>).

-spec from_realm(data()) -> kz_term:api_ne_binary().
from_realm(Props) ->
    case ccv(Props, <<"Realm">>) of
        'undefined' ->
            props:get_first_defined([<<"variable_sip_auth_realm">>
                                    ,<<"variable_sip_from_host">>
                                    ,<<"sip_from_host">>
                                    ]
                                   ,Props
                                   );
        Realm -> Realm
    end.

-spec from_tag(data()) -> kz_term:api_binary().
from_tag(Props) ->
    props:get_value(<<"variable_sip_from_tag">>, Props).

-spec to_tag(data()) -> kz_term:api_binary().
to_tag(Props) ->
    props:get_value(<<"variable_sip_to_tag">>, Props).

-spec origination_call_id(data()) -> kz_term:api_binary().
origination_call_id(Props) ->
    props:get_value(<<"variable_sip_origination_call_uuid">>, Props).

-spec conference_name(data()) -> kz_term:api_ne_binary().
conference_name(Props) ->
    props:get_ne_binary_value(<<"Conference-Name">>, Props).

-spec conference_profile_name(data()) -> kz_term:api_ne_binary().
conference_profile_name(Props) ->
    props:get_ne_binary_value(<<"Conference-Profile-Name">>, Props).

-spec conference_uuid(data()) -> kz_term:api_ne_binary().
conference_uuid(Props) ->
    props:get_ne_binary_value(<<"Conference-Unique-ID">>, Props).

-spec context(data()) -> kz_term:api_ne_binary().
context(Props) ->
    context(Props, 'undefined').

-spec context(data(), Default) -> kz_term:ne_binary() | Default.
context(Props, Default) ->
    props:get_ne_binary_value(<<"Caller-Context">>, Props, Default).

-spec hunt_context(data()) -> kz_term:api_ne_binary().
hunt_context(Props) ->
    hunt_context(Props, 'undefined').

-spec hunt_context(data(), Default) -> kz_term:ne_binary() | Default.
hunt_context(Props, Default) ->
    DefContext = props:get_value(<<"context">>, Props, Default),
    props:get_ne_binary_value(<<"Hunt-Context">>, Props, DefContext).

-spec join_time(data()) -> kz_time:gregorian_seconds().
join_time(Props) ->
    join_time(Props, kz_time:now_s()).

-spec join_time(data(), Default) -> kz_time:gregorian_seconds() | Default.
join_time(Props, Default) ->
    props:get_integer_value(<<"Join-Time">>, Props, Default).

-spec core_uuid(data()) -> kz_term:api_binary().
core_uuid(Props) ->
    props:get_value(<<"Core-UUID">>, Props).

-spec fetch_uuid(data()) -> kz_term:api_binary().
fetch_uuid(Props) ->
    props:get_first_defined([<<"Fetch-UUID">>
                            ,<<"variable_Fetch-UUID">>
                            ]
                           ,Props
                           ).

-spec fetch_winning_pid(data()) -> kz_term:api_binary().
fetch_winning_pid(Props) ->
    props:get_first_defined([<<"Fetch-Winning-PID">>
                            ,<<"variable_Fetch-Winning-PID">>
                            ]
                           ,Props
                           ).

-spec switch_url(data()) -> kz_term:api_binary().
switch_url(Props) ->
    case props:get_first_defined([<<"Switch-URL">>, <<"variable_Switch-URL">>], Props) of
        'undefined' -> props:get_first_defined([<<"variable_sofia_profile_url">>
                                               ,<<"sofia_profile_url">>
                                               ],Props);
        SwitchURL -> SwitchURL
    end.

-spec switch_uri(data()) -> kz_term:api_binary().
switch_uri(Props) ->
    case props:get_first_defined([<<"Switch-URI">>, <<"variable_Switch-URI">>], Props) of
        'undefined' -> case switch_url(Props) of
                           'undefined' -> 'undefined';
                           SwitchURL -> case binary:split(SwitchURL, <<"@">>) of
                                            [_, SwitchURIHost] -> <<"sip:", SwitchURIHost/binary>>;
                                            _Else -> 'undefined'
                                        end
                       end;
        SwitchURI -> SwitchURI
    end.

-spec switch_nodename(data()) -> atom().
switch_nodename(Props) ->
    props:get_atom_value(<<"Switch-Nodename">>, Props).

-spec hostname(data()) -> kz_term:api_ne_binary().
hostname(Props) ->
    hostname(Props, 'undefined').

-spec hostname(data(), Default) -> kz_term:ne_binary() | Default.
hostname(Props, Default) ->
    props:get_ne_binary_value(<<"FreeSWITCH-Hostname">>, Props, Default).
