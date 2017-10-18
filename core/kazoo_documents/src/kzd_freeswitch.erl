%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% FreeSWITCH proplists
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
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
        ,set_ccv/3, set_ccvs/2
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
        ,is_call_setup/1

        ,core_uuid/1, core_uuid_atom/1
        ,fetch_uuid/1
        ,fetch_section/1
        ,fetch_winning_pid/1
        ,switch_url/1, switch_uri/1
        ]).

-include("kz_documents.hrl").

-type data() :: kz_proplist() | kz_json:object().
-export_type([data/0]).

-define(CHANNEL_VAR_PREFIX, "ecallmgr_").
-define(CCV(Key), <<"variable_", ?CHANNEL_VAR_PREFIX, Key/binary>>).
-define(CCV_HEADER(Key), <<"variable_sip_h_X-", ?CHANNEL_VAR_PREFIX, Key/binary>>).
-define(CCVs, <<"Custom-Channel-Vars">>).

-spec caller_id_name(data()) -> api_binary().
-spec caller_id_name(data(), Default) -> ne_binary() | Default.
caller_id_name(Props) ->
    caller_id_name(Props, 'undefined').
caller_id_name(Props, Default)
  when is_list(Props) ->
    props:get_first_defined([<<"variable_origination_caller_id_name">>
                            ,<<"variable_effective_caller_id_name">>
                            ,<<"Caller-Caller-ID-Name">>
                            ]
                           ,Props
                           ,Default
                           );
caller_id_name(JObj, Default) ->
    kz_json:get_ne_binary_value(<<"Caller-ID-Name">>, JObj, Default).


-spec caller_id_number(data()) -> api_binary().
-spec caller_id_number(data(), Default) -> ne_binary() | Default.
caller_id_number(Props) ->
    caller_id_number(Props, 'undefined').
caller_id_number(Props, Default)
  when is_list(Props) ->
    props:get_first_defined([<<"variable_origination_caller_id_number">>
                            ,<<"variable_effective_caller_id_number">>
                            ,<<"Caller-Caller-ID-Number">>
                            ]
                           ,Props
                           ,Default
                           );
caller_id_number(JObj, Default) ->
    kz_json:get_ne_binary_value(<<"Caller-ID-Number">>, JObj, Default).

-spec callee_id_name(data()) -> api_binary().
-spec callee_id_name(data(), Default) -> ne_binary() | Default.
callee_id_name(Props) ->
    callee_id_name(Props, 'undefined').
callee_id_name(Props, Default)
  when is_list(Props) ->
    props:get_first_defined([<<"variable_origination_callee_id_name">>
                            ,<<"variable_effective_callee_id_name">>
                            ,<<"Caller-Callee-ID-Name">>
                            ,<<"Other-Leg-Caller-ID-Name">>
                            ]
                           ,Props
                           ,Default
                           );
callee_id_name(JObj, Default) ->
    kz_json:get_ne_binary_value(<<"Callee-ID-Name">>, JObj, Default).

-spec callee_id_number(data()) -> api_binary().
-spec callee_id_number(data(), Default) -> ne_binary() | Default.
callee_id_number(Props) ->
    callee_id_number(Props, 'undefined').
callee_id_number(Props, Default)
  when is_list(Props) ->
    props:get_first_defined([<<"variable_origination_callee_id_number">>
                            ,<<"variable_effective_callee_id_number">>
                            ,<<"Caller-Callee-ID-Number">>
                            ,<<"Other-Leg-Caller-ID-Number">>
                            ]
                           ,Props
                           ,Default
                           );
callee_id_number(JObj, Default) ->
    kz_json:get_ne_binary_value(<<"Callee-ID-Number">>, JObj, Default).

-spec dialed_number(data()) -> api_binary().
dialed_number(Props)
  when is_list(Props) ->
    props:get_first_defined([<<"variable_destination_number">>
                            ,<<"Caller-Destination-Number">>
                            ]
                           ,Props
                           );
dialed_number(JObj) ->
    kz_json:get_ne_binary_value(<<"Dialed-Number">>, JObj).

-spec call_id(data()) -> api_binary().
call_id(Props)
  when is_list(Props) ->
    props:get_first_defined([<<"Caller-Unique-ID">>
                            ,<<"Unique-ID">>
                            ,<<"Call-ID">>
                            ,<<"variable_uuid">>
                            ,<<"Channel-Call-UUID">>
                            ,<<"variable_sip_call_id">>
                            ,?RESIGNING_UUID
                            ], Props);
call_id(JObj) ->
    kz_json:get_ne_binary_value(<<"Call-ID">>, JObj).

-spec other_leg_call_id(data()) -> api_binary().
other_leg_call_id(Props)
  when is_list(Props) ->
    props:get_value(<<"Other-Leg-Unique-ID">>, Props);
other_leg_call_id(JObj) ->
    kz_json:get_ne_binary_value(<<"Other-Leg-Unique-ID">>, JObj).

-spec original_call_direction(data()) -> api_binary().
original_call_direction(Props) ->
    props:get_value(<<"Call-Direction">>, Props).

-spec call_direction(data()) -> api_binary().
call_direction(Props)
  when is_list(Props) ->
    props:get_first_defined([<<"Application-Logical-Direction">>
                            ,?CCV(<<"Application-Logical-Direction">>)
                            ,<<"Call-Direction">>
                            ]
                           ,Props
                           );
call_direction(JObj) ->
    kz_json:get_ne_binary_value(<<"Call-Direction">>, JObj).

-spec resource_type(data()) -> api_binary().
-spec resource_type(data(), Default) -> ne_binary() | Default.
resource_type(Props) ->
    resource_type(Props, 'undefined').

resource_type(Props, Default)
  when is_list(Props) ->
    props:get_value(<<"Resource-Type">>, Props, Default);
resource_type(JObj, Default) ->
    kz_json:get_ne_binary_value(<<"Resource-Type">>, JObj, Default).

-spec channel_authorized(data()) -> api_binary().
channel_authorized(Props) ->
    ccv(Props, <<"Channel-Authorized">>).

-spec outbound_flags(data()) -> api_binary() | ne_binaries().
outbound_flags(Props) ->
    ccv(Props, <<"Outbound-Flags">>).

-spec hunt_destination_number(data()) -> api_binary().
hunt_destination_number(Props)
  when is_list(Props) ->
    props:get_value(<<"Hunt-Destination-Number">>, Props);
hunt_destination_number(JObj) ->
    kz_json:get_ne_binary_value(<<"Destination-Number">>, JObj).

-spec is_channel_recovering(data()) -> boolean().
-spec is_channel_recovering(data(), boolean()) -> boolean().
is_channel_recovering(Props) ->
    is_channel_recovering(Props, 'false').

is_channel_recovering(Props, Default)
  when is_list(Props) ->
    props:is_true(<<"variable_recovered">>, Props, Default);
is_channel_recovering(JObj, Default) ->
    kz_json:is_true(<<"Channel-Recovered">>, JObj, Default).

-spec is_consuming_global_resource(data()) -> api_boolean().
-spec is_consuming_global_resource(data(), api_boolean()) -> api_boolean().
is_consuming_global_resource(Props) ->
    is_consuming_global_resource(Props, 'undefined').

is_consuming_global_resource(Props, Default) ->
    kz_term:is_true(ccv(Props, <<"Global-Resource">>, Default)).

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

-spec account_trunk_usage(data()) -> api_binary().
account_trunk_usage(Props) ->
    ccv(Props, <<"Account-Trunk-Usage">>).

-spec reseller_id(data()) -> api_binary().
reseller_id(Props) ->
    ccv(Props, <<"Reseller-ID">>).

-spec reseller_billing(data()) -> api_binary().
reseller_billing(Props) ->
    ccv(Props, <<"Reseller-Billing">>).

-spec reseller_trunk_usage(data()) -> api_binary().
reseller_trunk_usage(Props) ->
    ccv(Props, <<"Reseller-Trunk-Usage">>).

-spec to_did(data()) -> api_binary().
to_did(Props)
  when is_list(Props) ->
    props:get_first_defined([?CCV(<<"E164-Destination">>)
                            ,?CCV(<<"Original-Number">>)
                            ,<<"Caller-Destination-Number">>
                            ]
                           ,Props
                           );
to_did(JObj) ->
    kz_json:get_ne_binary_value(<<"To-DID">>, JObj).

-spec hangup_code(data()) -> api_binary().
hangup_code(Props)
  when is_list(Props) ->
    props:get_first_defined([<<"variable_proto_specific_hangup_cause">>
                            ,<<"variable_last_bridge_proto_specific_hangup_cause">>
                            ], Props);
hangup_code(JObj) ->
    kz_json:get_ne_binary_value(<<"Hangup-Code">>, JObj).

-spec disposition(data()) -> api_binary().
disposition(Props)
  when is_list(Props) ->
    props:get_first_defined([<<"variable_originate_disposition">>
                            ,<<"variable_endpoint_disposition">>
                            ], Props);
disposition(JObj) ->
    kz_json:get_ne_binary_value(<<"Disposition">>, JObj).

-spec hangup_cause(data()) -> api_binary().
hangup_cause(Props)
  when is_list(Props) ->
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
    end;
hangup_cause(JObj) ->
    kz_json:get_ne_binary_value(<<"Hangup-Cause">>, JObj).

-spec transfer_history(data()) -> api_binary() | kz_proplist().
transfer_history(Props)
  when is_list(Props) ->
    props:get_value(<<"variable_transfer_history">>, Props).

-spec transfer_source(data()) -> api_binary() | kz_proplist().
transfer_source(Props)
  when is_list(Props) ->
    props:get_value(<<"variable_transfer_source">>, Props).

-spec raw_application_name(data()) -> api_binary().
raw_application_name(Props)
  when is_list(Props) ->
    props:get_first_defined([<<"Application">>
                            ,<<"kazoo_application_name">>
                            ,<<"Event-Subclass">>
                            ], Props);
raw_application_name(JObj) ->
    kz_json:get_ne_binary_value(<<"Raw-Application-Name">>, JObj).

-spec application_name(data()) -> api_binary().
application_name(Props)
  when is_list(Props) ->
    props:get_first_defined([<<"kazoo_application_name">>
                            ,<<"Application">>
                            ,<<"Event-Subclass">>
                            ], Props);
application_name(JObj) ->
    kz_json:get_ne_binary_value(<<"Application-Name">>, JObj).

-spec event_name(data()) -> api_binary().
event_name(Props)
  when is_list(Props) ->
    props:get_first_defined([<<"kazoo_event_name">>
                            ,<<"Event-Subclass">>
                            ,<<"Event-Name">>
                            ], Props);
event_name(JObj) ->
    kz_json:get_ne_binary_value(<<"Event-Name">>, JObj).

-spec from_network_ip(data()) -> api_binary().
from_network_ip(Props)
  when is_list(Props) ->
    props:get_first_defined([<<"variable_sip_h_X-AUTH-IP">>
                            ,<<"variable_sip_received_ip">>
                            ]
                           ,Props
                           );
from_network_ip(JObj) ->
    kz_json:get_ne_binary_value(<<"Network-IP">>, JObj).

-spec from_network_port(data()) -> api_binary().
from_network_port(Props)
  when is_list(Props) ->
    props:get_first_defined([<<"variable_sip_h_X-AUTH-PORT">>
                            ,<<"variable_sip_received_port">>
                            ]
                           ,Props
                           );
from_network_port(JObj) ->
    kz_json:get_ne_binary_value(<<"Network-Port">>, JObj).

-spec user_agent(data()) -> api_binary().
user_agent(Props)
  when is_list(Props) ->
    props:get_first_defined([<<"variable_sip_user_agent">>
                            ,<<"sip_user_agent">>
                            ]
                           ,Props
                           );
user_agent(JObj) ->
    kz_json:get_ne_binary_value(<<"User-Agent">>, JObj).

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

-spec presence_id(data()) -> api_binary().
presence_id(Props) ->
    props:get_first_defined([<<"Channel-Presence-ID">>
                            ,<<"variable_presence_id">>
                            ], Props).

-spec presence_direction(data()) -> api_binary().
presence_direction(Props) ->
    props:get_value(<<"Presence-Call-Direction">>, Props).

-spec ccv(data(), ne_binary()) -> api_binary() | ne_binaries().
ccv(Props, Key) ->
    ccv(Props, Key, 'undefined').

-spec ccv(data(), ne_binary(), Default) -> ne_binary() | ne_binaries() | Default.
ccv(Props, Key, Default)
  when is_list(Props) ->
    props:get_value(Key, ccvs(Props), Default);
ccv(JObj, Key, Default) ->
    kz_json:get_ne_binary_value(Key, ccvs(JObj), Default).

-spec set_ccv(data(), ne_binary(), term()) -> data().
set_ccv(Props, Key, Value)
  when is_list(Props) ->
    props:set_value(?CCV(Key), Props, Value);
set_ccv(JObj, Key, Value) ->
    kz_json:set_value([?CCVs, Key], JObj, Value).

-spec set_ccvs(data(), [tuple()]) -> data().
set_ccvs(Props, Values)
  when is_list(Props) ->
    props:set_values([{?CCV(Key), Value} || {Key, Value} <- Values], Props);
set_ccvs(JObj, Values) ->
    kz_json:set_values([{[?CCVs, Key], Value} || {Key, Value} <- Values], JObj).

-spec channel_var_map({ne_binary(), ne_binary()}) -> {ne_binary(), ne_binary() | ne_binaries()}.
channel_var_map({Key, <<"ARRAY::", Serialized/binary>>}) ->
    {Key, binary:split(Serialized, <<"|:">>, ['global'])};
channel_var_map({Key, Other}) -> {Key, Other}.

%% Extract custom channel variables to include in the event
-spec ccvs(kz_proplist()) -> kz_proplist().
-spec custom_channel_vars(kz_proplist(), kz_proplist()) -> kz_proplist().
-spec custom_channel_vars_fold({ne_binary(), ne_binary()}, kz_proplist()) -> kz_proplist().
ccvs(Props)
  when is_list(Props) ->
    lists:map(fun channel_var_map/1, custom_channel_vars(Props, []));
ccvs(JObj) ->
    kz_json:get_json_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new()).

custom_channel_vars(Props, Initial) ->
    CCVs = lists:foldl(fun custom_channel_vars_fold/2, Initial, Props),
    maybe_update_referred_ccv(Props, channel_vars_sort(CCVs)).

-spec channel_vars_sort(kz_proplist()) -> kz_proplist().
channel_vars_sort(ChannelVars) ->
    lists:usort(fun channel_var_sort/2, ChannelVars).

-spec channel_var_sort(tuple(), tuple()) -> boolean().
channel_var_sort({A, _}, {B, _}) -> A =< B.

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

-spec maybe_update_referred_ccv(kz_proplist(), kz_proplist()) -> kz_proplist().
maybe_update_referred_ccv(Props, CCVs) ->
    ReferredBy = props:get_value(<<"variable_sip_h_Referred-By">>, Props),
    ReferTo = props:get_value(<<"variable_sip_refer_to">>, Props),
    update_referred_by_ccv(ReferredBy
                          ,update_referred_to_ccv(ReferTo, CCVs)
                          ).

-spec update_referred_by_ccv(api_binary(), kz_proplist()) -> kz_proplist().
update_referred_by_ccv('undefined', CCVs) -> props:delete(<<"Referred-By">>, CCVs);
update_referred_by_ccv(ReferredBy, CCVs) ->
    props:set_value(<<"Referred-By">>
                   ,kz_http_util:urldecode(ReferredBy)
                   ,CCVs
                   ).

-spec update_referred_to_ccv(api_binary(), kz_proplist()) -> kz_proplist().
update_referred_to_ccv('undefined', CCVs) -> props:delete(<<"Referred-To">>, CCVs);
update_referred_to_ccv(ReferredTo, CCVs) ->
    props:set_value(<<"Referred-To">>
                   ,kz_http_util:urldecode(ReferredTo)
                   ,CCVs
                   ).

-spec from_tag(data()) -> api_binary().
from_tag(Props)
  when is_list(Props) ->
    props:get_value(<<"variable_sip_from_tag">>, Props);
from_tag(JObj) ->
    kz_json:get_ne_binary_value(<<"From-Tag">>, JObj).

-spec to_tag(data()) -> api_binary().
to_tag(Props)
  when is_list(Props) ->
    props:get_value(<<"variable_sip_to_tag">>, Props);
to_tag(JObj) ->
    kz_json:get_ne_binary_value(<<"To-Tag">>, JObj).

-spec origination_call_id(data()) -> api_binary().
origination_call_id(Props)
  when is_list(Props) ->
    props:get_value(<<"variable_sip_origination_call_id">>, Props);
origination_call_id(JObj) ->
    kz_json:get_ne_binary_value(<<"Origination-Call-ID">>, JObj).

-spec conference_name(data()) -> api_ne_binary().
conference_name(Props)
  when is_list(Props) ->
    props:get_ne_binary_value(<<"Conference-Name">>, Props);
conference_name(JObj) ->
    kz_json:get_ne_binary_value(<<"Conference-Name">>, JObj).

-spec conference_profile_name(data()) -> api_ne_binary().
conference_profile_name(Props)
  when is_list(Props) ->
    props:get_ne_binary_value(<<"Conference-Profile-Name">>, Props);
conference_profile_name(JObj) ->
    kz_json:get_ne_binary_value(<<"Conference-Profile-Name">>, JObj).

-spec conference_uuid(data()) -> api_ne_binary().
conference_uuid(Props)
  when is_list(Props) ->
    props:get_ne_binary_value(<<"Conference-Unique-ID">>, Props);
conference_uuid(JObj) ->
    kz_json:get_ne_binary_value(<<"Conference-Unique-ID">>, JObj).

-spec join_time(data()) -> gregorian_seconds().
-spec join_time(data(), Default) -> gregorian_seconds() | Default.
join_time(Props) ->
<<<<<<< Upstream, based on 2600hz/master
    join_time(Props, kz_time:now_s()).
=======
    join_time(Props, kz_time:current_tstamp()).

>>>>>>> b8f37dd update node in kz_api
join_time(Props, Default) ->
    props:get_integer_value(<<"Join-Time">>, Props, Default).

-spec core_uuid(data()) -> api_binary().
core_uuid(Props)
  when is_list(Props) ->
    props:get_value(<<"Core-UUID">>, Props);
core_uuid(JObj) ->
    kz_json:get_ne_binary_value(<<"Core-UUID">>, JObj).

-spec core_uuid_atom(data()) -> atom().
core_uuid_atom(JObj) ->
    kz_json:get_atom_value(<<"Core-UUID">>, JObj).

-spec fetch_uuid(data()) -> api_binary().
fetch_uuid(Props)
  when is_list(Props) ->
    props:get_first_defined([<<"Fetch-UUID">>
                            ,<<"variable_Fetch-UUID">>
                            ]
                           ,Props
                           );
fetch_uuid(JObj) ->
    kz_json:get_ne_binary_value(<<"Fetch-UUID">>, JObj).

-spec fetch_section(data()) -> api_binary().
fetch_section(Props)
  when is_list(Props) ->
    props:get_first_defined([<<"Fetch-Section">>
                            ,<<"variable_Fetch-Section">>
                            ]
                           ,Props
                           );
fetch_section(JObj) ->
    kz_json:get_ne_binary_value(<<"Fetch-Section">>, JObj).

-spec fetch_winning_pid(data()) -> api_binary().
fetch_winning_pid(Props)
  when is_list(Props) ->
    props:get_first_defined([<<"Fetch-Winning-PID">>
                            ,<<"variable_Fetch-Winning-PID">>
                            ]
                           ,Props
                           );
fetch_winning_pid(JObj) ->
    kz_json:get_ne_binary_value(<<"Fetch-Winning-PID">>, JObj).

-spec switch_url(data()) -> api_binary().
switch_url(Props)
  when is_list(Props) ->
    case props:get_first_defined([<<"Switch-URL">>, <<"variable_Switch-URL">>], Props) of
        undefined -> props:get_first_defined([<<"variable_sofia_profile_url">>
                                             ,<<"sofia_profile_url">>
                                             ],Props);
        SwitchURL -> SwitchURL
    end;
switch_url(JObj) ->
    kz_json:get_ne_binary_value(<<"Switch-URL">>, JObj).

-spec switch_uri(data()) -> api_binary().
switch_uri(Props)
  when is_list(Props) ->
    case props:get_first_defined([<<"Switch-URI">>, <<"variable_Switch-URI">>], Props) of
        undefined -> case switch_url(Props) of
                         undefined -> undefined;
                         SwitchURL -> case binary:split(SwitchURL, <<"@">>) of
                                          [_, SwitchURIHost] -> <<"sip:", SwitchURIHost/binary>>;
                                          _Else -> undefined
                                      end
                     end;
        SwitchURI -> SwitchURI
    end;
switch_uri(JObj) ->
    kz_json:get_ne_binary_value(<<"Switch-URI">>, JObj).

-spec hostname(data()) -> api_ne_binary().
-spec hostname(data(), Default) -> ne_binary() | Default.
hostname(Props) ->
    hostname(Props, 'undefined').

hostname(Props, Default) ->
    props:get_ne_binary_value(<<"FreeSWITCH-Hostname">>, Props, Default).

-spec is_call_setup(data()) -> boolean().
is_call_setup(Props)
  when is_list(Props) ->
    props:is_true(<<"Call-Setup">>, Props, 'false');
is_call_setup(JObj) ->
    kz_json:is_true(<<"Call-Setup">>, JObj, 'false').
