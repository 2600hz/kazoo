%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Helpers for bridging in FreeSWITCH
%%% @author James Aimonetti
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_transfer).

-export([attended/3
        ,blind/3
        ]).

-include("ecallmgr.hrl").

-type attended_resp() :: {kz_term:ne_binary(), kz_term:ne_binary(), atom(), kz_term:proplist()}.
-type attended_resp(Node) :: {kz_term:ne_binary(), kz_term:ne_binary(), Node, kz_term:proplist()}.

-type blind_resp() :: [{kz_term:ne_binary(), kz_term:ne_binary()}].

-export_type([attended_resp/0
             ,blind_resp/0
             ]).

-spec attended(Node, kz_term:ne_binary(), kz_json:object()) ->
                      attended_resp(Node)
                          when Node :: atom().
attended(Node, UUID, JObj) ->
    TransferTo = kz_json:get_ne_binary_value(<<"Transfer-To">>, JObj),
    Realm = transfer_realm(UUID, JObj),
    ReqURI = <<TransferTo/binary, "@", Realm/binary>>,

    Vars = [{<<"SIP-Invite-Domain">>, Realm}
           ,{<<"Simplify-Loopback">>, <<"false">>}
           ,{<<"Loopback-Bowout">>, <<"true">>}
           ,{<<"Loopback-Request-URI">>, ReqURI}
           ,{<<"Ignore-Early-Media">>, <<"ring_ready">>}
           ,{<<"Outbound-Caller-ID-Number">>, kz_json:get_ne_binary_value(<<"Caller-ID-Number">>, JObj)}
           ,{<<"Outbound-Caller-ID-Name">>, kz_json:get_ne_binary_value(<<"Caller-ID-Name">>, JObj)}
           ,{<<"Outbound-Callee-ID-Number">>, TransferTo}
           ,{<<"Outbound-Callee-ID-Name">>, TransferTo}
           ,{<<"Timeout">>, kz_json:get_integer_value(<<"Timeout">>, JObj)}
           ],
    Props = transfer_vars(JObj, Vars),
    Arg = kz_binary:join(ecallmgr_util:process_fs_kv(Node, UUID, Props, 'set'), <<",">>),

    TransferContext = transfer_context(JObj),
    To = <<TransferTo/binary, "@", Realm/binary>>,
    KeyVars = transfer_keys(Node, UUID, JObj),
    lager:info("transferring to (~s) ~s @ ~s on context ~s", [To, TransferTo, Realm, TransferContext]),

    {<<"kz_att_xfer">>
    ,list_to_binary([KeyVars, "{", Arg, "}loopback/", TransferTo, <<"/">>, TransferContext])
    ,Node
    ,[{"hold-bleg", "true"}]
    }.

-spec blind(atom(), kz_term:ne_binary(), kz_json:object()) -> blind_resp().
blind(Node, UUID, JObj) ->
    TransferTo = kz_json:get_ne_binary_value(<<"Transfer-To">>, JObj),

    Realm = transfer_realm(UUID, JObj),
    TransferLeg = transfer_leg(JObj),
    TargetUUID = transfer_set_callid(UUID, TransferLeg),

    lager:info("transferring ~s to ~s @ ~s", [TargetUUID, TransferTo, Realm]),

    KVs = [{<<"SIP-Refer-To">>, <<"<sip:", TransferTo/binary, "@", Realm/binary, ">">>}
          ,{<<"SIP-Referred-By">>, transfer_referred(UUID, TransferLeg)}
          ,{<<"Signal-Bridge-To">>, UUID}
          ],
    AppArgs = ecallmgr_util:multi_set_args(Node, TargetUUID, props:filter_undefined(KVs)),
    [{<<"kz_uuid_multiset_encoded">>, list_to_binary([TargetUUID, " ", AppArgs])}
    ,{<<"blind_xfer">>, list_to_binary([TransferLeg, " ", TransferTo, <<" XML ">>, transfer_context(JObj)])}
    ].

-spec transfer_keys(atom(), kz_term:ne_binary(), kz_json:object()) -> binary().
transfer_keys(Node, UUID, JObj) ->
    TransferKeys = kz_json:get_json_value(<<"Attended-Transfer-Keys">>, JObj, kz_json:new()),
    TransferVars = kz_json:foldl(fun add_transfer_key/3, [], TransferKeys),
    case ecallmgr_util:process_fs_kv(Node, UUID, TransferVars, 'set') of
        [] -> <<>>;
        Exports -> list_to_binary(["%^[", kz_binary:join(Exports, <<"^">>), "]"])
    end.

add_transfer_key(K, V, Vars) ->
    [{<<"Attended-Transfer-", K/binary, "-Key">>, V} | Vars].

-spec transfer_vars(kz_json:object(), kz_term:proplist()) -> kz_term:proplist().
transfer_vars(JObj, Vars) ->
    kz_json:foldl(fun add_transfer_ccv_to_vars/3, props:filter_undefined(Vars), transfer_ccvs(JObj)).

-spec transfer_ccvs(kz_json:object()) -> kz_json:object().
transfer_ccvs(JObj) ->
    kz_json:get_json_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new()).

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
transfer_realm(UUID, JObj) ->
    case kz_json:get_first_defined([<<"Account-Realm">>, <<"Realm">>], transfer_ccvs(JObj)) of
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
        {'ok', #channel{username='undefined'}=C} ->
            lager:debug_unsafe("USER UNDEFINED : ~p", [C]),
            'undefined';
        {'ok', #channel{username=Username
                       ,realm=Realm
                       }} -> <<"<sip:", Username/binary, "@", Realm/binary, ">">>;
        _Else -> 'undefined'
    end;
transfer_referred(UUID, _) ->
    case ecallmgr_fs_channel:fetch_other_leg(UUID, 'record') of
        {'ok', #channel{username='undefined'}=C} ->
            lager:debug_unsafe("USER UNDEFINED : ~p", [C]),
            'undefined';
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
