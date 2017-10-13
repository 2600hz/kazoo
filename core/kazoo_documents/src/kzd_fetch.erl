%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% FreeSWITCH proplists
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzd_fetch).

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
        ,dialed_number/1
        ,disposition/1
        ,event_name/1
        ,from_network_ip/1, from_network_port/1
        ,from_tag/1, to_tag/1
        ,hangup_code/1, hangup_cause/1
        ,hostname/1, hostname/2
        ,hunt_destination_number/1
        ,is_channel_recovering/1, is_channel_recovering/2
        ,is_consuming_global_resource/1, is_consuming_global_resource/2
        ,is_loopback/1, loopback_other_leg/1, loopback_leg_name/1
        ,join_time/1, join_time/2
        ,media_recorder/1
        ,origination_call_id/1
        ,other_leg_call_id/1
        ,outbound_flags/1
        ,presence_id/1, presence_direction/1
        ,reseller_id/1, reseller_billing/1, reseller_trunk_usage/1
        ,resource_id/1
        ,resource_type/1, resource_type/2
        ,to_did/1
        ,transfer_history/1
        ,transfer_source/1
        ,user_agent/1

        ,fetch_user/1
        ,fetch_auth_endpoint/1
        ,fetch_action/1, fetch_action/2
        ,fetch_node/1
        ,core_uuid/1
        ,fetch_uuid/1
        ,fetch_key_name/1
        ,fetch_key_value/1
        ,fetch_tag/1
        ,fetch_section/1
        ,fetch_winning_pid/1
        ,switch_url/1, switch_uri/1

        ,controller_queue/1, controller_pid/1
        ,hunt_context/1
        ,node/1
        ]).

-include("kz_documents.hrl").

-type data() :: kz_json:object().
-export_type([data/0]).

-define(CHANNEL_VAR_PREFIX, "ecallmgr_").
-define(CCV(Key), <<"variable_", ?CHANNEL_VAR_PREFIX, Key/binary>>).
-define(CCV_HEADER(Key), <<"variable_sip_h_X-", ?CHANNEL_VAR_PREFIX, Key/binary>>).

-spec caller_id_name(data()) -> api_binary().
-spec caller_id_name(data(), Default) -> ne_binary() | Default.
caller_id_name(JObj) ->
    caller_id_name(JObj, 'undefined').
caller_id_name(JObj, Default) ->
    kz_json:get_first_defined([<<"variable_origination_caller_id_name">>
                              ,<<"variable_effective_caller_id_name">>
                              ,<<"Caller-Caller-ID-Name">>
                              ]
                             ,JObj
                             ,Default
                             ).

-spec caller_id_number(data()) -> api_binary().
-spec caller_id_number(data(), Default) -> ne_binary() | Default.
caller_id_number(JObj) ->
    caller_id_number(JObj, 'undefined').
caller_id_number(JObj, Default) ->
    kz_json:get_first_defined([<<"variable_origination_caller_id_number">>
                              ,<<"variable_effective_caller_id_number">>
                              ,<<"Caller-Caller-ID-Number">>
                              ]
                             ,JObj
                             ,Default
                             ).

-spec callee_id_name(data()) -> api_binary().
-spec callee_id_name(data(), Default) -> ne_binary() | Default.
callee_id_name(JObj) ->
    callee_id_name(JObj, 'undefined').
callee_id_name(JObj, Default) ->
    kz_json:get_first_defined([<<"variable_origination_callee_id_name">>
                              ,<<"variable_effective_callee_id_name">>
                              ,<<"Caller-Callee-ID-Name">>
                              ,<<"Other-Leg-Caller-ID-Name">>
                              ]
                             ,JObj
                             ,Default
                             ).

-spec callee_id_number(data()) -> api_binary().
-spec callee_id_number(data(), Default) -> ne_binary() | Default.
callee_id_number(JObj) ->
    callee_id_number(JObj, 'undefined').
callee_id_number(JObj, Default) ->
    kz_json:get_first_defined([<<"variable_origination_callee_id_number">>
                              ,<<"variable_effective_callee_id_number">>
                              ,<<"Caller-Callee-ID-Number">>
                              ,<<"Other-Leg-Caller-ID-Number">>
                              ]
                             ,JObj
                             ,Default
                             ).

-spec dialed_number(data()) -> api_binary().
dialed_number(JObj) ->
    kz_json:get_first_defined([<<"variable_destination_number">>
                              ,<<"Caller-Destination-Number">>
                              ]
                             ,JObj
                             ).

-spec call_id(data()) -> api_binary().
call_id(JObj) ->
    kz_json:get_first_defined([<<"Caller-Unique-ID">>
                              ,<<"Unique-ID">>
                              ,<<"Call-ID">>
                              ,<<"variable_uuid">>
                              ,<<"Channel-Call-UUID">>
                              ,<<"variable_sip_call_id">>
                              ,?RESIGNING_UUID
                              ], JObj).

-spec other_leg_call_id(data()) -> api_binary().
other_leg_call_id(JObj) ->
    kz_json:get_value(<<"Other-Leg-Unique-ID">>, JObj).

-spec original_call_direction(data()) -> api_binary().
original_call_direction(JObj) ->
    kz_json:get_value(<<"Call-Direction">>, JObj).

-spec call_direction(data()) -> api_binary().
call_direction(JObj) ->
    kz_json:get_first_defined([<<"Application-Logical-Direction">>
                              ,?CCV(<<"Application-Logical-Direction">>)
                              ,<<"Caller-Logical-Direction">>
                              ,<<"Call-Direction">>
                              ]
                             ,JObj
                             ).

-spec resource_type(data()) -> api_binary().
-spec resource_type(data(), Default) -> ne_binary() | Default.
resource_type(JObj) ->
    resource_type(JObj, 'undefined').

resource_type(JObj, Default) ->
    kz_json:get_value(<<"Resource-Type">>, JObj, Default).

-spec channel_authorized(data()) -> api_binary().
channel_authorized(JObj) ->
    ccv(JObj, <<"Channel-Authorized">>).

-spec outbound_flags(data()) -> api_binary() | ne_binaries().
outbound_flags(JObj) ->
    ccv(JObj, <<"Outbound-Flags">>).

-spec hunt_destination_number(data()) -> api_binary().
hunt_destination_number(JObj) ->
    kz_json:get_value(<<"Hunt-Destination-Number">>, JObj).

-spec is_channel_recovering(data()) -> boolean().
-spec is_channel_recovering(data(), boolean()) -> boolean().
is_channel_recovering(JObj) ->
    is_channel_recovering(JObj, 'false').

is_channel_recovering(JObj, Default) ->
    kz_json:is_true(<<"variable_recovered">>, JObj, Default).

-spec is_consuming_global_resource(data()) -> api_boolean().
-spec is_consuming_global_resource(data(), api_boolean()) -> api_boolean().
is_consuming_global_resource(JObj) ->
    is_consuming_global_resource(JObj, 'undefined').

is_consuming_global_resource(JObj, Default) ->
    kz_term:is_true(ccv(JObj, <<"Global-Resource">>, Default)).

-spec resource_id(data()) -> api_binary().
resource_id(JObj) ->
    ccv(JObj, <<"Resource-ID">>).

-spec authorizing_id(data()) -> api_binary().
authorizing_id(JObj) ->
    ccv(JObj, <<"Authorizing-ID">>).

-spec authorizing_type(data()) -> api_binary().
authorizing_type(JObj) ->
    ccv(JObj, <<"Authorizing-Type">>).

-spec account_id(data()) -> api_binary().
account_id(JObj) ->
    ccv(JObj, <<"Account-ID">>).

-spec account_billing(data()) -> api_binary().
account_billing(JObj) ->
    ccv(JObj, <<"Account-Billing">>).

-spec account_trunk_usage(data()) -> api_binary().
account_trunk_usage(JObj) ->
    ccv(JObj, <<"Account-Trunk-Usage">>).

-spec reseller_id(data()) -> api_binary().
reseller_id(JObj) ->
    ccv(JObj, <<"Reseller-ID">>).

-spec reseller_billing(data()) -> api_binary().
reseller_billing(JObj) ->
    ccv(JObj, <<"Reseller-Billing">>).

-spec reseller_trunk_usage(data()) -> api_binary().
reseller_trunk_usage(JObj) ->
    ccv(JObj, <<"Reseller-Trunk-Usage">>).

-spec to_did(data()) -> api_binary().
to_did(JObj) ->
    kz_json:get_first_defined([?CCV(<<"E164-Destination">>)
                              ,?CCV(<<"Original-Number">>)
                              ,<<"Caller-Destination-Number">>
                              ]
                             ,JObj
                             ).

-spec ccv(data(), ne_binary()) -> api_binary() | ne_binaries().
ccv(JObj, Key) ->
    ccv(JObj, Key, 'undefined').

-spec ccv(data(), ne_binary(), Default) -> ne_binary() | ne_binaries() | Default.
ccv(JObj, Key, Default) ->
    kz_json:get_value(Key, ccvs(JObj), Default).

-spec hangup_code(data()) -> api_binary().
hangup_code(JObj) ->
    kz_json:get_first_defined([<<"variable_proto_specific_hangup_cause">>
                              ,<<"variable_last_bridge_proto_specific_hangup_cause">>
                              ], JObj).

-spec disposition(data()) -> api_binary().
disposition(JObj) ->
    kz_json:get_first_defined([<<"variable_originate_disposition">>
                              ,<<"variable_endpoint_disposition">>
                              ], JObj).

-spec hangup_cause(data()) -> api_binary().
hangup_cause(JObj) ->
    case kz_json:get_value(<<"variable_current_application">>, JObj) of
        <<"bridge">> ->
            kz_json:get_first_defined([<<"variable_bridge_hangup_cause">>
                                      ,<<"variable_hangup_cause">>
                                      ,<<"Hangup-Cause">>
                                      ], JObj);
        _Else ->
            kz_json:get_first_defined([<<"variable_hangup_cause">>
                                      ,<<"variable_bridge_hangup_cause">>
                                      ,<<"Hangup-Cause">>
                                      ], JObj)
    end.

-spec transfer_history(data()) -> api_binary() | kz_proplist().
transfer_history(JObj) ->
    kz_json:get_value(<<"variable_transfer_history">>, JObj).

-spec transfer_source(data()) -> api_binary() | kz_proplist().
transfer_source(JObj) ->
    kz_json:get_value(<<"variable_transfer_source">>, JObj).

-spec raw_application_name(data()) -> api_binary().
raw_application_name(JObj) ->
    kz_json:get_first_defined([<<"Application">>
                              ,<<"kazoo_application_name">>
                              ,<<"Event-Subclass">>
                              ], JObj).

-spec application_name(data()) -> api_binary().
application_name(JObj) ->
    kz_json:get_first_defined([<<"kazoo_application_name">>
                              ,<<"Application">>
                              ,<<"Event-Subclass">>
                              ], JObj).

-spec event_name(data()) -> api_binary().
event_name(JObj) ->
    kz_json:get_first_defined([<<"kazoo_event_name">>
                              ,<<"Event-Subclass">>
                              ,<<"Event-Name">>
                              ], JObj).

-spec from_network_ip(data()) -> api_binary().
from_network_ip(JObj) ->
    kz_json:get_first_defined([<<"variable_sip_h_X-AUTH-IP">>
                              ,<<"variable_sip_received_ip">>
                              ]
                             ,JObj
                             ).

-spec from_network_port(data()) -> api_binary().
from_network_port(JObj) ->
    kz_json:get_first_defined([<<"variable_sip_h_X-AUTH-PORT">>
                              ,<<"variable_sip_received_port">>
                              ]
                             ,JObj
                             ).

-spec user_agent(data()) -> api_binary().
user_agent(JObj) ->
    kz_json:get_first_defined([<<"variable_sip_user_agent">>
                              ,<<"sip_user_agent">>
                              ]
                             ,JObj
                             ).

-spec loopback_leg_name(data()) -> api_binary().
loopback_leg_name(JObj) ->
    kz_json:get_value(<<"variable_loopback_leg">>, JObj).

-spec is_loopback(data()) -> boolean().
is_loopback(JObj) ->
    kz_json:get_value(<<"variable_loopback_leg">>, JObj) =/= 'undefined'.

-spec loopback_other_leg(data()) -> api_binary().
loopback_other_leg(JObj) ->
    kz_json:get_value(<<"variable_other_loopback_leg_uuid">>, JObj).

-spec media_recorder(data()) -> api_binary().
media_recorder(JObj) -> ccv(JObj, <<"Media-Recorder">>).

-spec presence_id(data()) -> api_binary().
presence_id(JObj) ->
    kz_json:get_first_defined([<<"Channel-Presence-ID">>
                              ,<<"variable_presence_id">>
                              ], JObj).

-spec presence_direction(data()) -> api_binary().
presence_direction(JObj) ->
    kz_json:get_value(<<"Presence-Call-Direction">>, JObj).

-spec channel_var_map({ne_binary(), ne_binary()}) -> {ne_binary(), ne_binary() | ne_binaries()}.
channel_var_map({Key, <<"ARRAY::", Serialized/binary>>}) ->
    {Key, binary:split(Serialized, <<"|:">>, ['global'])};
channel_var_map({Key, Other}) -> {Key, Other}.

%% Extract custom channel variables to include in the event
-spec ccvs(kz_proplist()) -> kz_proplist().
-spec custom_channel_vars(kz_proplist(), kz_proplist()) -> kz_proplist().
-spec custom_channel_vars_fold(ne_binary(), ne_binary(), kz_proplist()) -> kz_proplist().
ccvs(JObj) ->
    case kz_json:get_json_value(<<"Custom-Channel-Vars">>, JObj) of
        'undefined' -> kz_json:from_list(lists:map(fun channel_var_map/1, custom_channel_vars(JObj, [])));
        CCVs -> CCVs
    end.

custom_channel_vars(JObj, Initial) ->
    CCVs = kz_json:foldl(fun custom_channel_vars_fold/3, Initial, JObj),
    maybe_update_referred_ccv(JObj, channel_vars_sort(CCVs)).

-spec channel_vars_sort(kz_proplist()) -> kz_proplist().
channel_vars_sort(ChannelVars) ->
    lists:usort(fun channel_var_sort/2, ChannelVars).

-spec channel_var_sort(tuple(), tuple()) -> boolean().
channel_var_sort({A, _}, {B, _}) -> A =< B.

custom_channel_vars_fold(?CCV(Key), V, Acc) ->
    [{Key, V} | Acc];
custom_channel_vars_fold(<<?CHANNEL_VAR_PREFIX, Key/binary>>, V, Acc) ->
    [{Key, V} | Acc];
custom_channel_vars_fold(?CCV_HEADER(Key), V, Acc) ->
    case kz_json:is_defined(Key, Acc) of
        'true' -> Acc;
        'false' -> [{Key, V} | Acc]
    end;
custom_channel_vars_fold(<<"X-", ?CHANNEL_VAR_PREFIX, Key/binary>>, V, Acc) ->
    case kz_json:is_defined(Key, Acc) of
        'true' -> Acc;
        'false' -> [{Key, V} | Acc]
    end;
custom_channel_vars_fold(_, _, Acc) -> Acc.

-spec maybe_update_referred_ccv(kz_proplist(), kz_proplist()) -> kz_proplist().
maybe_update_referred_ccv(JObj, CCVs) ->
    ReferredBy = kz_json:get_value(<<"variable_sip_h_Referred-By">>, JObj),
    ReferTo = kz_json:get_value(<<"variable_sip_refer_to">>, JObj),
    update_referred_by_ccv(ReferredBy
                          ,update_referred_to_ccv(ReferTo, CCVs)
                          ).

-spec update_referred_by_ccv(api_binary(), kz_proplist()) -> kz_proplist().
update_referred_by_ccv('undefined', CCVs) -> kz_json:delete_key(<<"Referred-By">>, CCVs);
update_referred_by_ccv(ReferredBy, CCVs) ->
    kz_json:set_value(<<"Referred-By">>
                     ,kz_http_util:urldecode(ReferredBy)
                     ,CCVs
                     ).

-spec update_referred_to_ccv(api_binary(), kz_proplist()) -> kz_proplist().
update_referred_to_ccv('undefined', CCVs) -> kz_json:delete_key(<<"Referred-To">>, CCVs);
update_referred_to_ccv(ReferredTo, CCVs) ->
    kz_json:set_value(<<"Referred-To">>
                     ,kz_http_util:urldecode(ReferredTo)
                     ,CCVs
                     ).

-spec from_tag(data()) -> api_binary().
from_tag(JObj) ->
    kz_json:get_value(<<"variable_sip_from_tag">>, JObj).

-spec to_tag(data()) -> api_binary().
to_tag(JObj) ->
    kz_json:get_value(<<"variable_sip_to_tag">>, JObj).

-spec origination_call_id(data()) -> api_binary().
origination_call_id(JObj) ->
    kz_json:get_value(<<"variable_sip_origination_call_id">>, JObj).

-spec conference_name(data()) -> api_ne_binary().
conference_name(JObj) ->
    kz_json:get_ne_binary_value(<<"Conference-Name">>, JObj).

-spec conference_profile_name(data()) -> api_ne_binary().
conference_profile_name(JObj) ->
    kz_json:get_ne_binary_value(<<"Conference-Profile-Name">>, JObj).

-spec conference_uuid(data()) -> api_ne_binary().
conference_uuid(JObj) ->
    kz_json:get_ne_binary_value(<<"Conference-Unique-ID">>, JObj).

-spec join_time(data()) -> gregorian_seconds().
-spec join_time(data(), Default) -> gregorian_seconds() | Default.
join_time(JObj) ->
    join_time(JObj, kz_time:current_tstamp()).
join_time(JObj, Default) ->
    kz_json:get_integer_value(<<"Join-Time">>, JObj, Default).

-spec core_uuid(data()) -> api_binary().
core_uuid(JObj) ->
    kz_json:get_value(<<"Core-UUID">>, JObj).

-spec fetch_uuid(data()) -> api_binary().
fetch_uuid(JObj) ->
    kz_json:get_first_defined([<<"Fetch-UUID">>
                              ,<<"variable_Fetch-UUID">>
                              ]
                             ,JObj
                             ).

-spec fetch_action(data()) -> api_binary().
fetch_action(JObj) ->
    fetch_action(JObj, 'undefined').

-spec fetch_action(data(), Default) -> api_binary() | Default.
fetch_action(JObj, Default) ->
    kz_json:get_first_defined([<<"Action">>
                              ,<<"action">>
                              ], JObj, Default).

-spec fetch_key_name(data()) -> api_binary().
fetch_key_name(JObj) ->
    kz_json:get_value(<<"Fetch-Key-Name">>, JObj).

-spec fetch_key_value(data()) -> api_binary().
fetch_key_value(JObj) ->
    kz_json:get_value(<<"Fetch-Key-Value">>, JObj).

-spec fetch_node(data()) -> api_binary().
fetch_node(JObj) ->
    kz_json:get_value(<<"Node">>, JObj).

-spec fetch_section(data()) -> api_binary().
fetch_section(JObj) ->
    kz_json:get_value(<<"Fetch-Section">>, JObj).

-spec fetch_tag(data()) -> api_binary().
fetch_tag(JObj) ->
    kz_json:get_value(<<"Fetch-Tag">>, JObj).

-spec fetch_user(data()) -> api_binary().
fetch_user(JObj) ->
    kz_json:get_value(<<"user">>, JObj).

-spec fetch_auth_endpoint(data()) -> api_binary().
fetch_auth_endpoint(JObj) ->
    list_to_binary([fetch_user(JObj), "@", fetch_key_value(JObj)]).

-spec fetch_winning_pid(data()) -> api_binary().
fetch_winning_pid(JObj) ->
    kz_json:get_first_defined([<<"Fetch-Winning-PID">>
                              ,<<"variable_Fetch-Winning-PID">>
                              ]
                             ,JObj
                             ).

-spec hunt_context(data()) -> api_binary().
hunt_context(JObj) ->
    kz_json:get_first_defined([<<"Hunt-Context">>, <<"Caller-Context">>], JObj).

-spec controller_queue(data()) -> api_binary().
controller_queue(JObj) ->
    kz_json:get_ne_binary_value(<<"Controller-Queue">>, JObj).

-spec controller_pid(data()) -> api_binary().
controller_pid(JObj) ->
    kz_json:get_ne_binary_value(<<"Controller-PID">>, JObj).

-spec switch_url(data()) -> api_binary().
switch_url(JObj) ->
    case kz_json:get_first_defined([<<"Switch-URL">>, <<"variable_Switch-URL">>], JObj) of
        undefined -> kz_json:get_first_defined([<<"variable_sofia_profile_url">>
                                               ,<<"sofia_profile_url">>
                                               ],JObj);
        SwitchURL -> SwitchURL
    end.

-spec switch_uri(data()) -> api_binary().
switch_uri(JObj) ->
    case kz_json:get_first_defined([<<"Switch-URI">>, <<"variable_Switch-URI">>], JObj) of
        undefined -> case switch_url(JObj) of
                         undefined -> undefined;
                         SwitchURL -> case binary:split(SwitchURL, <<"@">>) of
                                          [_, SwitchURIHost] -> <<"sip:", SwitchURIHost/binary>>;
                                          _Else -> undefined
                                      end
                     end;
        SwitchURI -> SwitchURI
    end.

-spec hostname(data()) -> api_ne_binary().
-spec hostname(data(), Default) -> ne_binary() | Default.
hostname(JObj) ->
    hostname(JObj, 'undefined').
hostname(JObj, Default) ->
    kz_json:get_ne_binary_value(<<"FreeSWITCH-Hostname">>, JObj, Default).

-spec node(kz_json:object()) -> api_binary().
node(JObj) ->
    kz_json:get_atom_value(<<"Node">>, JObj).
