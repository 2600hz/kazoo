%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Helpers for bridging in FreeSWITCH
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_transfer).

-export([attended/3
        ,blind/3
        ]).

-include("ecallmgr.hrl").

-spec attended(atom(), kz_term:ne_binary(), kz_json:object()) -> {kz_term:ne_binary(), kz_term:ne_binary()}.
attended(Node, UUID, JObj) ->
    TransferTo = kz_json:get_ne_binary_value(<<"Transfer-To">>, JObj),
    CCVs = kz_json:get_json_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new()),

    Realm = transfer_realm(UUID, CCVs),

    ReqURI = <<TransferTo/binary, "@", Realm/binary>>,

    Vars = [{<<"Ignore-Early-Media">>, <<"ring_ready">>}
           ,{<<"Simplify-Loopback">>, <<"false">>}
           ,{<<"Loopback-Bowout">>, <<"true">>}
           ,{<<"Loopback-Request-URI">>, ReqURI}
           ,{<<"SIP-Invite-Domain">>, Realm}
           ,{<<"Outbound-Caller-ID-Number">>, kz_json:get_ne_binary_value(<<"Caller-ID-Number">>, JObj)}
           ,{<<"Outbound-Caller-ID-Name">>, kz_json:get_ne_binary_value(<<"Caller-ID-Name">>, JObj)}
           ,{<<"Outbound-Callee-ID-Number">>, TransferTo}
           ,{<<"Outbound-Callee-ID-Name">>, TransferTo}
           ],

    Props = add_transfer_ccvs_to_vars(CCVs, Vars),
    Exports = ecallmgr_util:process_fs_kv(Node, UUID, Props, 'set'),
    Arg = lists:join(",", lists:flatten(Exports)),

    TransferContext = transfer_context(JObj),

    lager:info("transferring to ~s @ ~s on context ~s", [TransferTo, Realm, TransferContext]),

    {<<"att_xfer">>, list_to_binary(["{", Arg, "}loopback/", TransferTo, <<"/">>, TransferContext])}.

-spec blind(atom(), kz_term:ne_binary(), kz_json:object()) ->
                   [{kz_term:ne_binary(), kz_term:ne_binary()}].
blind(Node, UUID, JObj) ->
    TransferTo = kz_json:get_ne_binary_value(<<"Transfer-To">>, JObj),

    Realm = transfer_realm(UUID),
    TransferLeg = transfer_leg(JObj),
    TargetUUID = transfer_set_callid(UUID, TransferLeg),

    lager:info("transferring to ~s @ ~s", [TransferTo, Realm]),

    KVs = [{<<"SIP-Refer-To">>, <<"<sip:", TransferTo/binary, "@", Realm/binary, ">">>}
          ,{<<"SIP-Referred-By">>, transfer_referred(UUID, TransferLeg)}
          ],
    Args = kz_binary:join(ecallmgr_util:process_fs_kv(Node, TargetUUID, KVs, 'set'), <<";">>),
    [{<<"kz_uuid_setvar_multi">>, list_to_binary([TargetUUID, " ", Args])}
    ,{<<"blind_xfer">>, list_to_binary([TransferLeg, " ", TransferTo, <<" XML ">>, transfer_context(JObj)])}
    ].

-spec add_transfer_ccvs_to_vars(kz_json:object(), kz_term:proplist()) -> kz_term:proplist().
add_transfer_ccvs_to_vars(CCVs, Vars) ->
    kz_json:foldl(fun add_transfer_ccv_to_vars/3, Vars, CCVs).

-spec add_transfer_ccv_to_vars(kz_json:key(), kz_json:json_term(), kz_term:proplist()) ->
                                      kz_term:proplist().
add_transfer_ccv_to_vars(<<"Account-ID">>=K, V, Vars) -> [{K, V} | Vars];
add_transfer_ccv_to_vars(<<"Authorizing-ID">>=K, V, Vars) -> [{K, V} | Vars];
add_transfer_ccv_to_vars(<<"Authorizing-Type">>=K, V, Vars) -> [{K, V} | Vars];
add_transfer_ccv_to_vars(<<"Channel-Authorized">>=K, V, Vars) -> [{K, V} | Vars];
add_transfer_ccv_to_vars(_Key, _Value, Vars) -> Vars.

-spec transfer_realm(kz_term:ne_binary()) -> kz_term:ne_binary().
transfer_realm(UUID) ->
    case ecallmgr_fs_channel:fetch(UUID, 'record') of
        {'ok', #channel{realm='undefined'}} ->
            lager:debug("channel.realm is undefined for ~s", [UUID]),
            ?DEFAULT_REALM;
        {'ok', #channel{realm=Realm}} -> Realm;
        {'error', 'not_found'} -> ?DEFAULT_REALM
    end.

-spec transfer_realm(kz_term:ne_binary(), kz_json:object()) -> kz_term:ne_binary().
transfer_realm(UUID, CCVs) ->
    case kz_json:get_first_defined([<<"Account-Realm">>, <<"Realm">>], CCVs) of
        'undefined' -> transfer_realm(UUID);
        Realm -> Realm
    end.

-spec transfer_set_callid(kz_term:ne_binary(), binary()) -> kz_term:ne_binary().
transfer_set_callid(UUID, <<"-bleg">>) ->
    case ecallmgr_fs_channel:fetch(UUID, 'record') of
        {'ok', #channel{other_leg='undefined'}} -> UUID;
        {'ok', #channel{other_leg=OtherUUID}} -> OtherUUID;
        _ -> UUID
    end;
transfer_set_callid(UUID, _) -> UUID.

-spec transfer_referred(kz_term:ne_binary(), binary()) -> kz_term:api_binary().
transfer_referred(UUID, <<"-bleg">>) ->
    case ecallmgr_fs_channel:fetch(UUID, 'record') of
        {'ok', #channel{username='undefined'}} -> 'undefined';
        {'ok', #channel{username=Username
                       ,realm=Realm
                       }} -> <<"<sip:", Username/binary, "@", Realm/binary, ">">>;
        _Else -> 'undefined'
    end;
transfer_referred(UUID, _) ->
    case ecallmgr_fs_channel:fetch_other_leg(UUID, 'record') of
        {'ok', #channel{username='undefined'}} -> 'undefined';
        {'ok', #channel{username=Username
                       ,realm=Realm
                       }} -> <<"<sip:", Username/binary, "@", Realm/binary, ">">>;
        _Else -> 'undefined'
    end.

-spec transfer_leg(kz_json:object()) -> binary().
transfer_leg(JObj) ->
    case kz_json:get_ne_binary_value(<<"Transfer-Leg">>, JObj) of
        'undefined' -> <<>>;
        TransferLeg -> <<"-", TransferLeg/binary>>
    end.

-spec transfer_context(kz_json:object()) -> binary().
transfer_context(JObj) ->
    case kz_json:get_ne_binary_value(<<"Transfer-Context">>, JObj) of
        'undefined' -> ?DEFAULT_FREESWITCH_CONTEXT;
        Context -> Context
    end.
