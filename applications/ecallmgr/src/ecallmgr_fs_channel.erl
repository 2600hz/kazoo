%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc Track the FreeSWITCH channel information, and provide accessors
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_channel).

-export([node/1, set_node/2
        ,former_node/1
        ,is_bridged/1
        ,exists/1
        ,import_moh/1
        ,set_account_id/2
        ,set_authorized/2
        ,fetch/1, fetch/2
        ,fetch_channel/1
        ,fetch_other_leg/1, fetch_other_leg/2
        ,renew/2
        ,channel_data/2
        ,get_other_leg/2
        ]).
-export([to_json/1
        ,to_props/1
        ,channel_ccvs/1
        ,channel_cavs/1
        ]).
-export([to_api_json/1
        ,to_api_props/1
        ]).

-compile([{'no_auto_import', [node/1]}]).

-include("ecallmgr.hrl").
-include_lib("kazoo_sip/include/kzsip_uri.hrl").


%%%=============================================================================
%%% API
%%%=============================================================================
-type fetch_resp() :: kz_json:object() |
                      kz_term:proplist() |
                      channel().
-type channel_format() :: 'json' | 'proplist' | 'record'.

-spec fetch(kz_term:ne_binary()) ->
                   {'ok', fetch_resp()} |
                   {'error', 'not_found'}.
fetch(UUID) ->
    fetch(UUID, 'json').

-spec fetch(kz_term:ne_binary(), channel_format()) ->
                   {'ok', fetch_resp()} |
                   {'error', 'not_found'}.
fetch(UUID, Format) ->
    case ets:lookup(?CHANNELS_TBL, UUID) of
        [Channel] -> {'ok', format(Format, Channel)};
        _Else -> {'error', 'not_found'}
    end.

-spec fetch_other_leg(kz_term:ne_binary()) ->
                             {'ok', fetch_resp()} |
                             {'error', 'not_found'}.
fetch_other_leg(UUID) ->
    fetch_other_leg(UUID, 'json').

-spec fetch_other_leg(kz_term:ne_binary(), channel_format()) ->
                             {'ok', fetch_resp()} |
                             {'error', 'not_found'}.
fetch_other_leg(UUID, Format) ->
    case ets:lookup(?CHANNELS_TBL, UUID) of
        [#channel{other_leg=OtherLeg}] -> fetch(OtherLeg, Format);
        _Else -> {'error', 'not_found'}
    end.

-spec format(channel_format(), channel()) -> fetch_resp().
format('json', Channel) -> to_json(Channel);
format('proplist', Channel) -> to_props(Channel);
format('record', Channel) -> Channel.

-spec node(kz_term:ne_binary()) ->
                  {'ok', atom()} |
                  {'error', 'not_found'}.
node(UUID) ->
    MatchSpec = [{#channel{uuid = '$1', node = '$2', _ = '_'}
                 ,[{'=:=', '$1', {'const', UUID}}]
                 ,['$2']}
                ],
    case ets:select(?CHANNELS_TBL, MatchSpec) of
        [Node] -> {'ok', Node};
        _ -> {'error', 'not_found'}
    end.

-spec set_node(atom(), kz_term:ne_binary()) -> 'ok'.
set_node(Node, UUID) ->
    Updates =
        case node(UUID) of
            {'error', 'not_found'} -> [{#channel.node, Node}];
            {'ok', Node} -> [];
            {'ok', OldNode} ->
                [{#channel.node, Node}
                ,{#channel.former_node, OldNode}
                ]
        end,
    ecallmgr_fs_channels:updates(UUID, Updates).

-spec former_node(kz_term:ne_binary()) ->
                         {'ok', atom()} |
                         {'error', any()}.
former_node(UUID) ->
    MatchSpec = [{#channel{uuid = '$1', former_node = '$2', _ = '_'}
                 ,[{'=:=', '$1', {'const', UUID}}]
                 ,['$2']}
                ],
    case ets:select(?CHANNELS_TBL, MatchSpec) of
        ['undefined'] -> {'ok', 'undefined'};
        [Node] -> {'ok', Node};
        _ -> {'error', 'not_found'}
    end.

-spec is_bridged(kz_term:ne_binary()) -> boolean().
is_bridged(UUID) ->
    MatchSpec = [{#channel{uuid = '$1', other_leg = '$2', _ = '_'}
                 ,[{'=:=', '$1', {'const', UUID}}]
                 ,['$2']}
                ],
    case ets:select(?CHANNELS_TBL, MatchSpec) of
        ['undefined'] -> lager:debug("channel is not bridged"), 'false';
        [Bin] when is_binary(Bin) -> lager:debug("is bridged to: ~s", [Bin]), 'true';
        _E -> lager:debug("not bridged: ~p", [_E]), 'false'
    end.

-spec exists(kz_term:ne_binary()) -> boolean().
exists(UUID) -> ets:member(?CHANNELS_TBL, UUID).

-spec import_moh(kz_term:ne_binary()) -> boolean().
import_moh(UUID) ->
    try ets:lookup_element(?CHANNELS_TBL, UUID, #channel.import_moh)
    catch
        'error':'badarg' -> 'false'
    end.

-spec set_account_id(kz_term:ne_binary(), string() | kz_term:ne_binary()) -> 'ok'.
set_account_id(UUID, Value) when is_binary(Value) ->
    ecallmgr_fs_channels:update(UUID, #channel.account_id, Value);
set_account_id(UUID, Value) ->
    set_account_id(UUID, kz_term:to_binary(Value)).

-spec set_authorized(kz_term:ne_binary(), boolean() | kz_term:ne_binary()) -> 'ok'.
set_authorized(UUID, Value) ->
    ecallmgr_fs_channels:update(UUID, #channel.is_authorized, kz_term:is_true(Value)).

-spec renew(atom(), kz_term:ne_binary()) ->
                   {'ok', channel()} |
                   {'error', 'timeout' | 'badarg'}.
renew(Node, UUID) ->
    case channel_data(Node, UUID) of
        {'ok', Props} ->
            {'ok', props_to_record(Props, Node)};
        {'error', _}=E -> E
    end.

-spec channel_data(atom(), kz_term:ne_binary()) -> {'ok', kz_term:proplist()} |
                                                   freeswitch:fs_api_error().
channel_data(Node, UUID) ->
    case freeswitch:api(Node, 'uuid_dump', UUID) of
        {'error', _}=E -> E;
        {'ok', Dump} ->
            {'ok', ecallmgr_util:eventstr_to_proplist(Dump)}
    end.

-spec to_json(channel()) -> kz_json:object().
to_json(Channel) ->
    kz_json:from_list_recursive(to_props(Channel)).

-spec to_props(channel()) -> kz_term:proplist().
to_props(Channel) ->
    props:filter_undefined(
      [{<<"account_billing">>, Channel#channel.account_billing}
      ,{<<"account_id">>, Channel#channel.account_id}
      ,{<<"answered">>, Channel#channel.answered}
      ,{<<"authorizing_id">>, Channel#channel.authorizing_id}
      ,{<<"authorizing_type">>, Channel#channel.authorizing_type}
      ,{<<"bridge_id">>, Channel#channel.bridge_id}
      ,{<<"callflow_id">>, Channel#channel.callflow_id}
      ,{<<"channel_authorized">>, Channel#channel.is_authorized}
      ,{<<"context">>, Channel#channel.context}
      ,{<<"custom_application_vars">>, Channel#channel.cavs}
      ,{<<"destination">>, Channel#channel.destination}
      ,{<<"dialplan">>, Channel#channel.dialplan}
      ,{<<"direction">>, Channel#channel.direction}
      ,{<<"elapsed_s">>, kz_time:elapsed_s(Channel#channel.timestamp)}
      ,{<<"fetch_id">>, Channel#channel.fetch_id}
      ,{<<"from_tag">>, Channel#channel.from_tag}
      ,{<<"handling_locally">>, Channel#channel.handling_locally}
      ,{<<"interaction_id">>, Channel#channel.interaction_id}
      ,{<<"is_loopback">>, Channel#channel.is_loopback}
      ,{<<"is_onhold">>, Channel#channel.is_onhold}
      ,{<<"loopback_leg_name">>, Channel#channel.loopback_leg_name}
      ,{<<"loopback_other_leg">>, Channel#channel.loopback_other_leg}
      ,{<<"node">>, Channel#channel.node}
      ,{<<"other_leg">>, Channel#channel.other_leg}
      ,{<<"owner_id">>, Channel#channel.owner_id}
      ,{<<"precedence">>, Channel#channel.precedence}
      ,{<<"presence_id">>, Channel#channel.presence_id}
      ,{<<"profile">>, Channel#channel.profile}
      ,{<<"realm">>, Channel#channel.realm}
      ,{<<"reseller_billing">>, Channel#channel.reseller_billing}
      ,{<<"reseller_id">>, Channel#channel.reseller_id}
      ,{<<"resource_id">>, Channel#channel.resource_id}
      ,{<<"switch_nodename">>, Channel#channel.node}
      ,{<<"switch_url">>, ecallmgr_fs_nodes:sip_url(Channel#channel.node)}
      ,{<<"timestamp">>, Channel#channel.timestamp}
      ,{<<"to_tag">>, Channel#channel.to_tag}
      ,{<<"username">>, Channel#channel.username}
      ,{<<"uuid">>, Channel#channel.uuid}
      ]).

-spec to_api_json(kz_term:ne_binary() | channel()) -> kz_json:object().
to_api_json(Channel) ->
    kz_json:from_list(to_api_props(Channel)).

-spec to_api_props(kz_term:ne_binary() | channel()) -> kz_term:proplist().
to_api_props(#channel{}=Channel) ->
    props:filter_undefined(
      [{<<"Account-Billing">>, Channel#channel.account_billing}
      ,{<<"Account-ID">>, Channel#channel.account_id}
      ,{<<"Answered">>, Channel#channel.answered}
      ,{<<"Authorizing-ID">>, Channel#channel.authorizing_id}
      ,{<<"Authorizing-Type">>, Channel#channel.authorizing_type}
      ,{<<"Bridge-ID">>, Channel#channel.bridge_id}
      ,{<<"Call-Direction">>, Channel#channel.direction}
      ,{<<"Call-ID">>, Channel#channel.uuid}
      ,{<<"CallFlow-ID">>, Channel#channel.callflow_id}
      ,{<<"Channel-Authorized">>, Channel#channel.is_authorized}
      ,{<<"Context">>, Channel#channel.context}
      ,{<<"Custom-Application-Vars">>, kz_json:from_list(channel_cavs(Channel))}
      ,{<<"Custom-Channel-Vars">>, kz_json:from_list(channel_ccvs(Channel))}
      ,{<<"Destination">>, Channel#channel.destination}
      ,{<<"Dialplan">>, Channel#channel.dialplan}
      ,{<<"Elapsed-Seconds">>, kz_time:elapsed_s(Channel#channel.timestamp)}
      ,{<<"Fetch-ID">>, Channel#channel.fetch_id}
      ,{<<"From-Tag">>, Channel#channel.from_tag}
      ,{<<"Is-Loopback">>, Channel#channel.is_loopback}
      ,{<<"Is-On-Hold">>, Channel#channel.is_onhold}
      ,{<<"Loopback-Leg-Name">>, Channel#channel.loopback_leg_name}
      ,{<<"Loopback-Other-Leg">>, Channel#channel.loopback_other_leg}
      ,{<<"Media-Node">>, kz_term:to_binary(Channel#channel.node)}
      ,{<<"Other-Leg-Call-ID">>, Channel#channel.other_leg}
      ,{<<"Owner-ID">>, Channel#channel.owner_id}
      ,{<<"Precedence">>, Channel#channel.precedence}
      ,{<<"Presence-ID">>, Channel#channel.presence_id}
      ,{<<"Profile">>, Channel#channel.profile}
      ,{<<"Realm">>, Channel#channel.realm}
      ,{<<"Reseller-Billing">>, Channel#channel.reseller_billing}
      ,{<<"Reseller-ID">>, Channel#channel.reseller_id}
      ,{<<"Resource-ID">>, Channel#channel.resource_id}
      ,{<<"Switch-URL">>, ecallmgr_fs_nodes:sip_url(Channel#channel.node)}
      ,{<<"Timestamp">>, Channel#channel.timestamp}
      ,{<<"To-Tag">>, Channel#channel.to_tag}
      ,{<<"Username">>, Channel#channel.username}
      ,{<<?CALL_INTERACTION_ID>>, Channel#channel.interaction_id}
      ]);
to_api_props(?NE_BINARY=CallId) ->
    {'ok', #channel{}=Channel} = fetch(CallId, 'record'),
    to_api_props(Channel).

-spec channel_ccvs(channel() | kz_json:object() | kz_term:proplist()) -> kz_term:proplist().
channel_ccvs(#channel{}=Channel) ->
    props:filter_undefined(
      [{<<"Account-ID">>, Channel#channel.account_id}
      ,{<<"Account-Billing">>, Channel#channel.account_billing}
      ,{<<"Authorizing-ID">>, Channel#channel.authorizing_id}
      ,{<<"Authorizing-Type">>, Channel#channel.authorizing_type}
      ,{<<"Channel-Authorized">>, Channel#channel.is_authorized}
      ,{<<"Owner-ID">>, Channel#channel.owner_id}
      ,{<<"Resource-ID">>, Channel#channel.resource_id}
      ,{<<"Presence-ID">>, Channel#channel.presence_id}
      ,{<<"Fetch-ID">>, Channel#channel.fetch_id}
      ,{<<"Bridge-ID">>, Channel#channel.bridge_id}
      ,{<<"Precedence">>, Channel#channel.precedence}
      ,{<<"Reseller-ID">>, Channel#channel.reseller_id}
      ,{<<"Reseller-Billing">>, Channel#channel.reseller_billing}
      ,{<<"Realm">>, Channel#channel.realm}
      ,{<<"Username">>, Channel#channel.username}
      ,{<<?CALL_INTERACTION_ID>>, Channel#channel.interaction_id}
      ,{<<"CallFlow-ID">>, Channel#channel.callflow_id}
      ]);
channel_ccvs([_|_]=Props) ->
    props:filter_undefined(
      [{<<"Account-ID">>, props:get_value(<<"account_id">>, Props)}
      ,{<<"Account-Billing">>, props:get_value(<<"account_billing">>, Props)}
      ,{<<"Authorizing-ID">>, props:get_value(<<"authorizing_id">>, Props)}
      ,{<<"Authorizing-Type">>, props:get_value(<<"authorizing_type">>, Props)}
      ,{<<"Channel-Authorized">>, props:get_value(<<"channel_authorized">>, Props)}
      ,{<<"Owner-ID">>, props:get_value(<<"owner_id">>, Props)}
      ,{<<"Resource-ID">>, props:get_value(<<"resource_id">>, Props)}
      ,{<<"Presence-ID">>, props:get_value(<<"presence_id">>, Props)}
      ,{<<"Fetch-ID">>, props:get_value(<<"fetch_id">>, Props)}
      ,{<<"Bridge-ID">>, props:get_value(<<"bridge_id">>, Props)}
      ,{<<"Precedence">>, props:get_value(<<"precedence">>, Props)}
      ,{<<"Reseller-ID">>, props:get_value(<<"reseller_id">>, Props)}
      ,{<<"Reseller-Billing">>, props:get_value(<<"reseller_billing">>, Props)}
      ,{<<"Realm">>, props:get_value(<<"realm">>, Props)}
      ,{<<"Username">>, props:get_value(<<"username">>, Props)}
      ,{<<?CALL_INTERACTION_ID>>, props:get_value(<<"interaction_id">>, Props)}
      ,{<<"callflow_id">>, props:get_value(<<"callflow_id">>, Props)}
      ]);
channel_ccvs(JObj) ->
    channel_ccvs(kz_json:to_proplist(JObj)).

-spec channel_cavs(channel() | kz_term:proplist() | kz_json:object()) -> kz_term:proplist().
channel_cavs(#channel{cavs='undefined'}) -> [];
channel_cavs(#channel{cavs=CAVs}) -> CAVs;
channel_cavs([_|_]=Props) -> props:get_value(<<"custom_application_vars">>, Props, []);
channel_cavs(JObj) -> kz_json:get_list_value(<<"custom_application_vars">>, JObj, []).

-spec fetch_channel(kz_term:ne_binary()) -> kz_term:proplist() | 'undefined'.
fetch_channel(UUID) ->
    case fetch(UUID, 'proplist') of
        {'error', 'not_found'} -> fetch_remote(UUID);
        {'ok', Channel} -> Channel
    end.

-spec fetch_remote(kz_term:ne_binary()) -> kz_term:proplist() | 'undefined'.
fetch_remote(UUID) ->
    case get_active_channel_status(UUID) of
        {'error', _} -> 'undefined';
        {'ok', JObj} ->
            Props = kz_json:recursive_to_proplist(kz_json:normalize(JObj)),
            CCVs = props:get_value(<<"custom_channel_vars">>, Props, []),
            Props ++ CCVs
    end.

-spec props_to_record(kz_term:proplist(), atom()) -> channel().
props_to_record(Props, Node) ->
    UUID = props:get_value(<<"Unique-ID">>, Props),
    CCVs = ecallmgr_util:custom_channel_vars(Props),
    CAVs = ecallmgr_util:custom_application_vars(Props),
    OtherLeg = get_other_leg(props:get_value(<<"Unique-ID">>, Props), Props),

    #channel{uuid=UUID
            ,destination=props:get_value(<<"Caller-Destination-Number">>, Props)
            ,direction=kzd_freeswitch:call_direction(Props)
            ,account_id=props:get_value(<<"Account-ID">>, CCVs)
            ,account_billing=props:get_value(<<"Account-Billing">>, CCVs)
            ,authorizing_id=props:get_value(<<"Authorizing-ID">>, CCVs)
            ,authorizing_type=props:get_value(<<"Authorizing-Type">>, CCVs)
            ,is_authorized=props:is_true(<<"Channel-Authorized">>, CCVs)
            ,owner_id=props:get_value(<<"Owner-ID">>, CCVs)
            ,resource_id=props:get_value(<<"Resource-ID">>, CCVs)
            ,presence_id=props:get_value(<<"Channel-Presence-ID">>
                                        ,CCVs
                                        ,props:get_value(<<"variable_presence_id">>, Props)
                                        )
            ,fetch_id=props:get_value(<<"Fetch-ID">>, CCVs)
            ,bridge_id=props:get_value(<<"Bridge-ID">>, CCVs, UUID)
            ,reseller_id=props:get_value(<<"Reseller-ID">>, CCVs)
            ,reseller_billing=props:get_value(<<"Reseller-Billing">>, CCVs)
            ,precedence=kz_term:to_integer(props:get_value(<<"Precedence">>, CCVs, 5))
            ,realm=get_realm(Props, CCVs)
            ,username=props:get_value(<<"Username">>, CCVs, get_username(Props))
            ,import_moh=props:get_value(<<"variable_hold_music">>, Props) =:= 'undefined'
            ,answered=props:get_value(<<"Answer-State">>, Props) =:= <<"answered">>
            ,node=Node
            ,timestamp=kz_time:now_s()
            ,profile=props:get_value(<<"variable_sofia_profile_name">>, Props, ?DEFAULT_FS_PROFILE)
            ,context=kzd_freeswitch:context(Props, ?DEFAULT_FREESWITCH_CONTEXT)
            ,dialplan=props:get_value(<<"Caller-Dialplan">>, Props, ?DEFAULT_FS_DIALPLAN)
            ,other_leg=OtherLeg
            ,handling_locally=handling_locally(Props, OtherLeg)
            ,to_tag=props:get_value(<<"variable_sip_to_tag">>, Props)
            ,from_tag=props:get_value(<<"variable_sip_from_tag">>, Props)
            ,interaction_id=props:get_value(<<?CALL_INTERACTION_ID>>, CCVs)
            ,is_loopback=kzd_freeswitch:is_loopback(Props)
            ,loopback_leg_name=kzd_freeswitch:loopback_leg_name(Props)
            ,loopback_other_leg=kzd_freeswitch:loopback_other_leg(Props)
            ,callflow_id=props:get_value(<<"CallFlow-ID">>, CCVs)
            ,cavs=CAVs
            }.

-spec other_leg_handling_locally(kz_term:ne_binary()) -> boolean().
other_leg_handling_locally(OtherLeg) ->
    case fetch(OtherLeg, 'record') of
        {'ok', #channel{handling_locally=HandleLocally}} -> HandleLocally;
        _ -> 'false'
    end.

-spec handling_locally(kz_term:proplist(), kz_term:api_binary()) -> boolean().
handling_locally(Props, 'undefined') ->
    ChannelEcallmgr = props:get_value(?GET_CCV(<<"Ecallmgr-Node">>), Props),
    lager:debug("channel has ecallmgr ~s (we are ~s)", [ChannelEcallmgr, node()]),
    ChannelEcallmgr =:= kz_term:to_binary(node());
handling_locally(Props, OtherLeg) ->
    Node = kz_term:to_binary(node()),
    case props:get_value(?GET_CCV(<<"Ecallmgr-Node">>), Props) of
        Node -> 'true';
        _OtherNode ->
            lager:debug("ccv has ecallmgr ~s (we are ~s), checking other leg ~s"
                       ,[_OtherNode, node(), OtherLeg]
                       ),
            other_leg_handling_locally(OtherLeg)
    end.

-spec get_username(kz_term:proplist()) -> kz_term:api_binary().
get_username(Props) ->
    case props:get_first_defined([?GET_CCV(<<"Username">>)
                                 ,<<"variable_user_name">>
                                 ]
                                ,Props
                                )
    of
        'undefined' ->
            lager:debug("no username in CCVs or variable_user_name"),
            'undefined';
        Username -> kz_term:to_lower_binary(Username)
    end.

-spec get_realm(kzd_freeswitch:data(), kz_term:proplist()) ->
                       kz_term:api_ne_binary().
get_realm(Props, CCVs) ->
    case props:get_value(<<"Realm">>, CCVs) of
        'undefined' ->
            lager:info("no 'Realm' in CCVs, checking FS props"),
            get_realm_from_props(Props);
        Realm -> Realm
    end.

-spec get_realm_from_props(kzd_freeswitch:data()) ->
                                  kz_term:api_ne_binary().
get_realm_from_props(Props) ->
    case props:get_value(<<"variable_domain_name">>, Props) of
        'undefined' ->
            lager:info("no realm found in 'variable_domain_name' in FS props"),
            'undefined';
        Realm -> kz_term:to_lower_binary(Realm)
    end.

%% -spec update_callee(binary(), channel_updates()) -> channel_updates().
%% update_callee(UUID, Props) ->
%%     case fetch(UUID, 'record') of
%%         {'ok', #channel{callee_number = Num2
%%                        ,callee_name = Name2
%%                        }} ->
%%             Num1 = kzd_freeswitch:callee_id_number(Props),
%%             Name1 = kzd_freeswitch:callee_id_name(Props),
%%             [{#channel.callee_number, maybe_update_callee_field(Num1, Num2)}
%%             ,{#channel.callee_name, maybe_update_callee_field(Name1, Name2)}
%%             ];
%%         _ -> []
%%     end.
%% 
%% -spec maybe_update_callee_field(api_binary(), api_binary()) -> api_binary().
%% maybe_update_callee_field(Value, 'undefined') -> Value;
%% maybe_update_callee_field(_Value, Existing) -> Existing.

-spec get_other_leg(kz_term:api_binary(), kz_term:proplist()) -> kz_term:api_binary().
get_other_leg('undefined', _Props) -> 'undefined';
get_other_leg(UUID, Props) ->
    get_other_leg_name(UUID, Props, props:get_value(<<"Other-Leg-Channel-Name">>, Props)).

-spec get_other_leg_name(kz_term:ne_binary(), kz_term:proplist(), kz_term:ne_binary()) -> kz_term:api_binary().
get_other_leg_name(UUID, Props, _ChannelName) ->
    get_other_leg(UUID
                 ,Props
                 ,props:get_first_defined([<<"Other-Leg-Unique-ID">>
                                          ,<<"Other-Leg-Call-ID">>
                                          ,<<"variable_origination_uuid">>
                                          ]
                                         ,Props
                                         )
                 ).

-spec get_other_leg(kz_term:ne_binary(), kz_term:proplist(), kz_term:api_binary()) -> kz_term:api_binary().
get_other_leg(UUID, Props, 'undefined') ->
    maybe_other_bridge_leg(UUID
                          ,Props
                          ,props:get_value(<<"Bridge-A-Unique-ID">>, Props)
                          ,props:get_value(<<"Bridge-B-Unique-ID">>, Props)
                          );
get_other_leg(_UUID, _Props, OtherLeg) -> OtherLeg.

-spec maybe_other_bridge_leg(kz_term:ne_binary(), kz_term:proplist(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                                    kz_term:api_binary().
maybe_other_bridge_leg(UUID, _Props, UUID, OtherLeg) -> OtherLeg;
maybe_other_bridge_leg(UUID, _Props, OtherLeg, UUID) -> OtherLeg;
maybe_other_bridge_leg(UUID, Props, _, _) ->
    case props:get_value(?GET_CCV(<<"Bridge-ID">>), Props) of
        UUID -> 'undefined';
        BridgeId -> BridgeId
    end.
