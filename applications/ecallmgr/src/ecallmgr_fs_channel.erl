%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2017, 2600Hz
%%% @doc
%%% Track the FreeSWITCH channel information, and provide accessors
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
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
        ]).
-export([to_api_json/1
        ,to_api_props/1
        ]).

-compile([{'no_auto_import', [node/1]}]).

-include("ecallmgr.hrl").
-include_lib("kazoo_sip/include/kzsip_uri.hrl").


%%%===================================================================
%%% API
%%%===================================================================

-type fetch_resp() :: kz_json:object() |
                      kz_proplist() |
                      channel().
-type channel_format() :: 'json' | 'proplist' | 'record'.

-spec fetch(ne_binary()) ->
                   {'ok', fetch_resp()} |
                   {'error', 'not_found'}.
-spec fetch(ne_binary(), channel_format()) ->
                   {'ok', fetch_resp()} |
                   {'error', 'not_found'}.
fetch(UUID) ->
    fetch(UUID, 'json').
fetch(UUID, Format) ->
    case ets:lookup(?CHANNELS_TBL, UUID) of
        [Channel] -> {'ok', format(Format, Channel)};
        _Else -> {'error', 'not_found'}
    end.

-spec fetch_other_leg(ne_binary()) ->
                             {'ok', fetch_resp()} |
                             {'error', 'not_found'}.
-spec fetch_other_leg(ne_binary(), channel_format()) ->
                             {'ok', fetch_resp()} |
                             {'error', 'not_found'}.
fetch_other_leg(UUID) ->
    fetch_other_leg(UUID, 'json').
fetch_other_leg(UUID, Format) ->
    case ets:lookup(?CHANNELS_TBL, UUID) of
        [#channel{other_leg=OtherLeg}] -> fetch(OtherLeg, Format);
        _Else -> {'error', 'not_found'}
    end.

-spec format(channel_format(), channel()) -> fetch_resp().
format('json', Channel) -> to_json(Channel);
format('proplist', Channel) -> to_props(Channel);
format('record', Channel) -> Channel.

-spec node(ne_binary()) ->
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

-spec set_node(atom(), ne_binary()) -> 'ok'.
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

-spec former_node(ne_binary()) ->
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

-spec is_bridged(ne_binary()) -> boolean().
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

-spec exists(ne_binary()) -> boolean().
exists(UUID) -> ets:member(?CHANNELS_TBL, UUID).

-spec import_moh(ne_binary()) -> boolean().
import_moh(UUID) ->
    try ets:lookup_element(?CHANNELS_TBL, UUID, #channel.import_moh)
    catch
        'error':'badarg' -> 'false'
    end.

-spec set_account_id(ne_binary(), string() | ne_binary()) -> 'ok'.
set_account_id(UUID, Value) when is_binary(Value) ->
    ecallmgr_fs_channels:update(UUID, #channel.account_id, Value);
set_account_id(UUID, Value) ->
    set_account_id(UUID, kz_term:to_binary(Value)).

-spec set_authorized(ne_binary(), boolean() | ne_binary()) -> 'ok'.
set_authorized(UUID, Value) ->
    ecallmgr_fs_channels:update(UUID, #channel.is_authorized, kz_term:is_true(Value)).

-spec renew(atom(), ne_binary()) ->
                   {'ok', channel()} |
                   {'error', 'timeout' | 'badarg'}.
renew(Node, UUID) ->
    case channel_data(Node, UUID) of
        {'ok', Props} ->
            {'ok', props_to_record(Props, Node)};
        {'error', _}=E -> E
    end.

-spec channel_data(atom(), ne_binary()) -> {'ok', kz_proplist()} |
                                           freeswitch:fs_api_error().
channel_data(Node, UUID) ->
    case freeswitch:api(Node, 'uuid_dump', UUID) of
        {'error', _}=E -> E;
        {'ok', Dump} ->
            {'ok', ecallmgr_util:eventstr_to_proplist(Dump)}
    end.

-spec to_json(channel()) -> kz_json:object().
to_json(Channel) ->
    kz_json:from_list(to_props(Channel)).

-spec to_props(channel()) -> kz_proplist().
to_props(Channel) ->
    props:filter_undefined(
      [{<<"uuid">>, Channel#channel.uuid}
      ,{<<"destination">>, Channel#channel.destination}
      ,{<<"direction">>, Channel#channel.direction}
      ,{<<"account_id">>, Channel#channel.account_id}
      ,{<<"account_billing">>, Channel#channel.account_billing}
      ,{<<"authorizing_id">>, Channel#channel.authorizing_id}
      ,{<<"authorizing_type">>, Channel#channel.authorizing_type}
      ,{<<"channel_authorized">>, Channel#channel.is_authorized}
      ,{<<"owner_id">>, Channel#channel.owner_id}
      ,{<<"resource_id">>, Channel#channel.resource_id}
      ,{<<"presence_id">>, Channel#channel.presence_id}
      ,{<<"fetch_id">>, Channel#channel.fetch_id}
      ,{<<"bridge_id">>, Channel#channel.bridge_id}
      ,{<<"precedence">>, Channel#channel.precedence}
      ,{<<"reseller_id">>, Channel#channel.reseller_id}
      ,{<<"reseller_billing">>, Channel#channel.reseller_billing}
      ,{<<"realm">>, Channel#channel.realm}
      ,{<<"username">>, Channel#channel.username}
      ,{<<"answered">>, Channel#channel.answered}
      ,{<<"node">>, Channel#channel.node}
      ,{<<"timestamp">>, Channel#channel.timestamp}
      ,{<<"profile">>, Channel#channel.profile}
      ,{<<"context">>, Channel#channel.context}
      ,{<<"dialplan">>, Channel#channel.dialplan}
      ,{<<"other_leg">>, Channel#channel.other_leg}
      ,{<<"handling_locally">>, Channel#channel.handling_locally}
      ,{<<"switch_url">>, ecallmgr_fs_nodes:sip_url(Channel#channel.node)}
      ,{<<"switch_nodename">>, Channel#channel.node}
      ,{<<"to_tag">>, Channel#channel.to_tag}
      ,{<<"from_tag">>, Channel#channel.from_tag}
      ,{<<"elapsed_s">>, kz_time:elapsed_s(Channel#channel.timestamp)}
      ,{<<"interaction_id">>, Channel#channel.interaction_id}
      ,{<<"is_loopback">>, Channel#channel.is_loopback}
      ,{<<"loopback_leg_name">>, Channel#channel.loopback_leg_name}
      ,{<<"loopback_other_leg">>, Channel#channel.loopback_other_leg}
      ,{<<"callflow_id">>, Channel#channel.callflow_id}
      ,{<<"is_onhold">>, Channel#channel.is_onhold}
      ]).

-spec to_api_json(channel()) -> kz_json:object().
to_api_json(Channel) ->
    kz_json:from_list(to_api_props(Channel)).

-spec to_api_props(channel()) -> kz_proplist().
to_api_props(Channel) ->
    props:filter_undefined(
      [{<<"Call-ID">>, Channel#channel.uuid}
      ,{<<"Destination">>, Channel#channel.destination}
      ,{<<"Call-Direction">>, Channel#channel.direction}
      ,{<<"Account-ID">>, Channel#channel.account_id}
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
      ,{<<"Answered">>, Channel#channel.answered}
      ,{<<"Media-Node">>, kz_term:to_binary(Channel#channel.node)}
      ,{<<"Timestamp">>, Channel#channel.timestamp}
      ,{<<"Profile">>, Channel#channel.profile}
      ,{<<"Context">>, Channel#channel.context}
      ,{<<"Dialplan">>, Channel#channel.dialplan}
      ,{<<"Other-Leg-Call-ID">>, Channel#channel.other_leg}
      ,{<<"To-Tag">>, Channel#channel.to_tag}
      ,{<<"From-Tag">>, Channel#channel.from_tag}
      ,{<<"Switch-URL">>, ecallmgr_fs_nodes:sip_url(Channel#channel.node)}
      ,{<<"Elapsed-Seconds">>, kz_time:elapsed_s(Channel#channel.timestamp)}
      ,{<<?CALL_INTERACTION_ID>>, Channel#channel.interaction_id}
      ,{<<"Is-Loopback">>, Channel#channel.is_loopback}
      ,{<<"Loopback-Leg-Name">>, Channel#channel.loopback_leg_name}
      ,{<<"Loopback-Other-Leg">>, Channel#channel.loopback_other_leg}
      ,{<<"CallFlow-ID">>, Channel#channel.callflow_id}
      ,{<<"Is-On-Hold">>, Channel#channel.is_onhold}
      ]).

-spec channel_ccvs(channel() | kz_json:object() | kz_proplist()) -> kz_proplist().
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


-spec fetch_channel(ne_binary()) -> kz_proplist() | 'undefined'.
fetch_channel(UUID) ->
    case fetch(UUID, 'proplist') of
        {'error', 'not_found'} -> fetch_remote(UUID);
        {'ok', Channel} -> Channel
    end.

-spec fetch_remote(ne_binary()) -> api_object().
fetch_remote(UUID) ->
    Command = [{<<"Call-ID">>, UUID}
              ,{<<"Active-Only">>, <<"true">>}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    case kz_amqp_worker:call(Command
                            ,fun(C) -> kapi_call:publish_channel_status_req(UUID, C) end
                            ,fun kapi_call:channel_status_resp_v/1
                            )
    of
        {'error', _} -> 'undefined';
        {'ok', JObj} ->
            Props = kz_json:recursive_to_proplist(kz_json:normalize(JObj)),
            CCVs = props:get_value(<<"custom_channel_vars">>, Props, []),
            Props ++ CCVs
    end.

-spec props_to_record(kz_proplist(), atom()) -> channel().
props_to_record(Props, Node) ->
    UUID = props:get_value(<<"Unique-ID">>, Props),
    CCVs = ecallmgr_util:custom_channel_vars(Props),
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
            ,realm=props:get_value(<<"Realm">>, CCVs, get_realm(Props))
            ,username=props:get_value(<<"Username">>, CCVs, get_username(Props))
            ,import_moh=props:get_value(<<"variable_hold_music">>, Props) =:= 'undefined'
            ,answered=props:get_value(<<"Answer-State">>, Props) =:= <<"answered">>
            ,node=Node
            ,timestamp=kz_time:now_s()
            ,profile=props:get_value(<<"variable_sofia_profile_name">>, Props, ?DEFAULT_FS_PROFILE)
            ,context=props:get_value(<<"Caller-Context">>, Props, ?DEFAULT_FREESWITCH_CONTEXT)
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
            }.

-spec other_leg_handling_locally(ne_binary()) -> boolean().
other_leg_handling_locally(OtherLeg) ->
    case fetch(OtherLeg, 'record') of
        {'ok', #channel{handling_locally=HandleLocally}} -> HandleLocally;
        _ -> 'false'
    end.

-spec handling_locally(kz_proplist(), api_binary()) -> boolean().
handling_locally(Props, 'undefined') ->
    props:get_value(?GET_CCV(<<"Ecallmgr-Node">>), Props)
        =:= kz_term:to_binary(node());
handling_locally(Props, OtherLeg) ->
    Node = kz_term:to_binary(node()),
    case props:get_value(?GET_CCV(<<"Ecallmgr-Node">>), Props) of
        Node -> 'true';
        _ -> other_leg_handling_locally(OtherLeg)
    end.

-spec get_username(kz_proplist()) -> api_binary().
get_username(Props) ->
    case props:get_first_defined([?GET_CCV(<<"Username">>)
                                 ,<<"variable_user_name">>
                                 ]
                                ,Props
                                )
    of
        'undefined' -> 'undefined';
        Username -> kz_term:to_lower_binary(Username)
    end.

-spec get_realm(kz_proplist()) -> api_binary().
get_realm(Props) ->
    case props:get_first_defined([?GET_CCV(<<"Realm">>)
                                 ,<<"variable_domain_name">>
                                 ]
                                ,Props
                                )
    of
        'undefined' -> 'undefined';
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

-spec get_other_leg(api_binary(), kz_proplist()) -> api_binary().
get_other_leg('undefined', _Props) -> 'undefined';
get_other_leg(UUID, Props) ->
    get_other_leg_name(UUID, Props, props:get_value(<<"Other-Leg-Channel-Name">>, Props)).

-spec get_other_leg_name(ne_binary(), kz_proplist(), ne_binary()) -> api_binary().
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

-spec get_other_leg(ne_binary(), kz_proplist(), api_binary()) -> api_binary().
get_other_leg(UUID, Props, 'undefined') ->
    maybe_other_bridge_leg(UUID
                          ,Props
                          ,props:get_value(<<"Bridge-A-Unique-ID">>, Props)
                          ,props:get_value(<<"Bridge-B-Unique-ID">>, Props)
                          );
get_other_leg(_UUID, _Props, OtherLeg) -> OtherLeg.

-spec maybe_other_bridge_leg(ne_binary(), kz_proplist(), ne_binary(), ne_binary()) ->
                                    api_binary().
maybe_other_bridge_leg(UUID, _Props, UUID, OtherLeg) -> OtherLeg;
maybe_other_bridge_leg(UUID, _Props, OtherLeg, UUID) -> OtherLeg;
maybe_other_bridge_leg(UUID, Props, _, _) ->
    case props:get_value(?GET_CCV(<<"Bridge-ID">>), Props) of
        UUID -> 'undefined';
        BridgeId -> BridgeId
    end.
