%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
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
-module(ecallmgr_fs_bridge).

-export([call_command/3
        ,unbridge/2
        ,try_create_bridge_string/2
        ]).

-include("ecallmgr.hrl").

-define(BYPASS_MEDIA_AFTER_BRIDGE, kapps_config:get_boolean(?APP_NAME, <<"use_bypass_media_after_bridge">>, 'false')).
-define(CHANNEL_ACTIONS_KEY, [<<"Custom-Channel-Vars">>, <<"Channel-Actions">>]).

%% Keys in the Endpoint JSON that should not be de-duplicated
-define(UNLIFTABLE_KEYS, [<<"Endpoint-Type">>
                         ,<<"Failover">>
                         ,<<"Forward-IP">>
                         ,<<"Invite-Format">>
                         ,<<"Proxy-IP">>
                         ,<<"Proxy-Zone">>
                         ,<<"Route">>
                         ,<<"SIP-Interface">>
                         ,<<"SIP-Transport">>
                         ,<<"To-DID">>
                         ,<<"To-IP">>
                         ,<<"To-Realm">>
                         ,<<"To-User">>
                         ,<<"To-Username">>

                              %% Per FS-3792, group confirm must be
                              %% set per-channel, not globally
                              %% otherwise double prompting
                         ,<<"Confirm-Cancel-Timeout">>
                         ,<<"Confirm-File">>
                         ,<<"Confirm-Key">>
                         ,<<"Confirm-Read-Timeout">>
                         ,[<<"Custom-Channel-Vars">>, <<"Confirm-Cancel-Timeout">>]
                         ,[<<"Custom-Channel-Vars">>, <<"Confirm-File">>]
                         ,[<<"Custom-Channel-Vars">>, <<"Confirm-Key">>]
                         ,[<<"Custom-Channel-Vars">>, <<"Confirm-Read-Timeout">>]
                         ,<<"Account-ID">>
                         ]).

-spec call_command(atom(), kz_term:ne_binary(), kz_json:object()) -> {'error', binary()} | {binary(), kz_term:proplist()}.
call_command(Node, UUID, JObj) ->
    Endpoints = kz_json:get_list_value(<<"Endpoints">>, JObj, []),
    case kapi_dialplan:bridge_v(JObj) of
        'false' -> {'error', <<"bridge failed to execute as JObj did not validate">>};
        'true' when Endpoints =:= [] -> {'error', <<"bridge request had no endpoints">>};
        'true' ->
            %% if we are intending to ring multiple device simultaneously then
            %% execute ring_ready so we don't leave the caller hanging with dead air.
            %% this does not test how many are ACTUALLY dialed (registered)
            %% since that is one of the things we want to be ringing during

            lager:debug("executing bridge on channel ~s", [UUID]),

            Channel = case ecallmgr_fs_channel:fetch(UUID, 'record') of
                          {'ok', Chan} -> Chan;
                          _ ->
                              lager:warning("channel ~s not found in channels ets table. bypass_media may be affected", [UUID]),
                              #channel{}
                      end,

            BridgeJObj = add_endpoints_channel_actions(Node, UUID, JObj),
            AppUUID = kz_binary:rand_hex(16),

            Routines = [fun handle_ringback/5
                       ,fun maybe_early_media/5
                       ,fun handle_hold_media/5
                       ,fun handle_secure_rtp/5
                       ,fun maybe_handle_bypass_media/5
                       ,fun handle_ccvs/5
                       ,fun handle_cavs/5
                       ,fun pre_exec/5
                       ,fun handle_loopback/5
                       ,fun create_command/5
                       ,{fun post_exec/2, AppUUID}
                       ],
            lager:debug("creating bridge dialplan"),
            XferExt = lists:foldr(fun({F, Arg}, DP) when is_function(F, 2) ->
                                          F(DP, Arg);
                                     (F, DP)  when is_function(F, 5) ->
                                          F(DP, Node, UUID, Channel, BridgeJObj)
                                  end
                                 ,[]
                                 ,Routines
                                 ),
            {<<"xferext">>, XferExt, Node, [{<<"Application-UUID">>, AppUUID}]}
    end.

-spec unbridge(kz_term:ne_binary(), kz_json:object()) ->
          kz_term:proplist() |
          {'error', kz_term:ne_binary()}.
unbridge(UUID, JObj) ->
    case kapi_dialplan:unbridge_v(JObj) of
        'false' -> {'error', <<"unbridge failed to execute as API did not validate">>};
        'true' ->
            Leg =
                case kz_json:get_value(<<"Leg">>, JObj) of
                    <<"B">> -> <<"-bleg">>;
                    <<"Both">> -> <<"-both">>;
                    _ -> <<>>
                end,
            {<<"transfer">>, iolist_to_binary([UUID, " ", Leg, " park: inline"])}
    end.

%%------------------------------------------------------------------------------
%% @doc Bridge command helpers
%% @end
%%------------------------------------------------------------------------------
-spec handle_ringback(kz_term:proplist(), atom(), kz_term:ne_binary(), channel(), kz_json:object()) -> kz_term:proplist().
handle_ringback(DP, Node, UUID, _Channel, JObj) ->
    case kz_json:get_first_defined([<<"Ringback">>
                                   ,[<<"Custom-Channel-Vars">>, <<"Ringback">>]
                                   ]
                                  ,JObj
                                  )
    of
        'undefined' ->
            {'ok', Default} = ecallmgr_util:get_setting(<<"default_ringback">>),
            Props = [{<<"ringback">>, Default}],
            Exports = ecallmgr_util:process_fs_kv(Node, UUID, Props, 'export'),
            Args = ecallmgr_util:fs_args_to_binary(Exports),
            [{"application", <<"kz_export ", Args/binary>>}
             |DP
            ];
        Media ->
            Stream = ecallmgr_util:media_path(Media, 'extant', UUID, JObj),
            lager:debug("bridge has custom ringback: ~s", [Stream]),
            Props = [{<<"ringback">>, Stream}],
            Exports = ecallmgr_util:process_fs_kv(Node, UUID, Props, 'export'),
            Args = ecallmgr_util:fs_args_to_binary(Exports),
            [{"application", <<"kz_export ", Args/binary>>}
             |DP
            ]
    end.

-spec maybe_early_media(kz_term:proplist(), atom(), kz_term:ne_binary(), channel(), kz_json:object()) -> kz_term:proplist().
maybe_early_media(DP, _Node, _UUID, _Channel, JObj) ->
    Endpoints = kz_json:get_list_value(<<"Endpoints">>, JObj, []),
    case ecallmgr_util:get_dial_separator(JObj, Endpoints) of
        ?SEPARATOR_SINGLE -> DP;
        _Separator ->
            [{"application", <<"ring_ready">>} | DP]
    end.

-spec handle_hold_media(kz_term:proplist(), atom(), kz_term:ne_binary(), channel(), kz_json:object()) -> kz_term:proplist().
handle_hold_media(DP, _Node, UUID, _Channel, JObj) ->
    case kz_json:get_value(<<"Hold-Media">>, JObj) of
        'undefined' ->
            case kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Hold-Media">>], JObj) of
                'undefined' -> DP;
                Media ->
                    Stream = ecallmgr_util:media_path(Media, 'extant', UUID, JObj),
                    lager:debug("bridge has custom music-on-hold in channel vars: ~s", [Stream]),
                    [{"application", <<"set hold_music=", Stream/binary>>}
                    ,{"application", <<"set transfer_ringback=", Stream/binary>>}
                     |DP
                    ]
            end;
        Media ->
            Stream = ecallmgr_util:media_path(Media, 'extant', UUID, JObj),
            lager:debug("bridge has custom music-on-hold: ~s", [Stream]),
            [{"application", <<"set hold_music=", Stream/binary>>}
            ,{"application", <<"set transfer_ringback=", Stream/binary>>}
             |DP
            ]
    end.

-spec handle_secure_rtp(kz_term:proplist(), atom(), kz_term:ne_binary(), channel(), kz_json:object()) -> kz_term:proplist().
handle_secure_rtp(DP, _Node, _UUID, _Channel, JObj) ->
    case kz_json:is_true(<<"Secure-RTP">>, JObj, 'false') of
        'true' -> [{"application", "set zrtp_secure_media=true"}|DP];
        'false' -> DP
    end.

-spec maybe_handle_bypass_media(kz_term:proplist(), atom(), kz_term:ne_binary(), channel(), kz_json:object()) -> kz_term:proplist().
maybe_handle_bypass_media(DP, Node, UUID, Channel, JObj) ->
    case ?BYPASS_MEDIA_AFTER_BRIDGE of
        'true' -> DP;
        'false' -> handle_bypass_media(DP, Node, UUID, Channel, JObj)
    end.

-spec handle_bypass_media(kz_term:proplist(), atom(), kz_term:ne_binary(), channel(), kz_json:object()) -> kz_term:proplist().
handle_bypass_media(DP, _Node, _UUID, #channel{profile=ChannelProfile}, JObj) ->
    BridgeProfile = kz_term:to_binary(kz_json:get_value(<<"SIP-Interface">>, JObj, ?DEFAULT_FS_PROFILE)),
    case kz_json:get_value(<<"Media">>, JObj) of
        <<"process">> ->
            lager:debug("bridge will process media through host switch"),
            [{"application", "set bypass_media=false"}|DP];
        <<"bypass">> when BridgeProfile =:= ChannelProfile ->
            lager:debug("bridge will process media through host switch"),
            [{"application", "set bypass_media=true"}|DP];
        _ ->
            Endpoints = kz_json:get_ne_value(<<"Endpoints">>, JObj, []),
            maybe_bypass_endpoint_media(Endpoints, BridgeProfile, ChannelProfile, DP)
    end.

-spec maybe_bypass_endpoint_media(kz_json:objects(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> kz_term:proplist().
maybe_bypass_endpoint_media(Endpoints, BridgeProfile, ChannelProfile, DP) ->
    ShouldBypass = lists:all(fun(Endpoint) ->
                                     bypass_endpoint_media_enabled(Endpoint
                                                                  ,BridgeProfile
                                                                  ,ChannelProfile
                                                                  )
                             end
                            ,Endpoints
                            ),
    case ShouldBypass of
        'true' -> [{"application", "set bypass_media=true"}|DP];
        'false' -> DP
    end.

-spec bypass_endpoint_media_enabled(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
bypass_endpoint_media_enabled(Endpoint, BridgeProfile, ChannelProfile) ->
    EndpointProfile = kz_json:get_ne_binary_value(<<"SIP-Interface">>, Endpoint, BridgeProfile),
    kz_json:is_true(<<"Bypass-Media">>, Endpoint)
        andalso EndpointProfile =:= ChannelProfile.

-spec handle_ccvs(kz_term:proplist(), atom(), kz_term:ne_binary(), channel(), kz_json:object()) -> kz_term:proplist().
handle_ccvs(DP, Node, UUID, _Channel, JObj) ->
    case kz_json:get_json_value(<<"Custom-Channel-Vars">>, JObj) of
        'undefined' -> DP;
        CCVs ->
            Args = ecallmgr_util:process_fs_kv(Node, UUID, kz_json:to_proplist(CCVs), 'set'),
            AppArgs = ecallmgr_util:fs_args_to_binary(Args),
            [{"application", <<"kz_multiset ", AppArgs/binary>>}] ++ DP
    end.

-spec handle_cavs(kz_term:proplist(), atom(), kz_term:ne_binary(), channel(), kz_json:object()) -> kz_term:proplist().
handle_cavs(DP, Node, UUID, _Channel, JObj) ->
    case kz_json:get_json_value(<<"Custom-Application-Vars">>, JObj) of
        'undefined' -> DP;
        CAVs ->
            SetCAVs = [{?CAV(K), V} || {K, V} <- kz_json:to_proplist(CAVs)],
            Args = ecallmgr_util:process_fs_kv(Node, UUID, SetCAVs, 'set'),
            AppArgs = ecallmgr_util:fs_args_to_binary(Args),
            [{"application", <<"kz_multiset ", AppArgs/binary>>}] ++ DP
    end.

-spec handle_loopback_key(boolean(), kz_term:ne_binary(), kz_json:object()) -> kz_term:proplist().
handle_loopback_key('false', _Key, _JObj) -> [];
handle_loopback_key('true', Key, JObj) ->
    V = kz_term:to_binary(kz_json:is_false(Key, JObj, 'false')),
    K = ecallmgr_util:get_fs_key(Key),
    [{"application", <<"export ", K/binary, "=", V/binary>>}].

-spec handle_loopback_key(kz_term:ne_binary(), kz_json:object()) -> kz_term:proplist().
handle_loopback_key(Key, JObj) ->
    Exists = kz_json:get_value(Key, JObj) =/= 'undefined',
    handle_loopback_key(Exists, Key, JObj).

-spec handle_loopback_keys(kz_term:ne_binaries(), kz_json:object(), kz_term:proplist()) -> kz_term:proplist().
handle_loopback_keys([], _JObj, Acc) -> Acc;
handle_loopback_keys([Key | Keys], JObj, Acc) ->
    handle_loopback_keys(Keys, JObj, Acc ++ handle_loopback_key(Key, JObj)).

-spec handle_loopback(kz_term:proplist(), atom(), kz_term:ne_binary(), channel(), kz_json:object()) -> kz_term:proplist().
handle_loopback(DP, _Node, _UUID, _Channel, JObj) ->
    Keys = [<<"Simplify-Loopback">>, <<"Loopback-Bowout">>],
    handle_loopback_keys(Keys, JObj, DP).

-spec continue_on_fail(kz_json:object()) -> kz_term:ne_binary().
continue_on_fail(JObj) ->
    case kz_json:get_value(<<"Continue-On-Fail">>, JObj) of
        'undefined' -> <<"true">>;
        Val when is_binary(Val) -> Val;
        Val when is_boolean(Val) -> kz_term:to_binary(Val);
        Val when is_list(Val) -> kz_binary:join(Val, <<",">>);
        _ -> <<"true">>
    end.

-spec hangup_after_bridge(kz_json:object()) -> kz_term:ne_binary().
hangup_after_bridge(JObj) ->
    case kz_json:get_boolean_value(<<"Continue-After">>, JObj) of
        'true' -> <<"false">>;
        'false' -> <<"true">>;
        'undefined' -> <<"false">>
    end.

-spec pre_exec(kz_term:proplist(), atom(), kz_term:ne_binary(), channel(), kz_json:object()) -> kz_term:proplist().
pre_exec(DP, _Node, _UUID, _Channel, JObj) ->
    [{"application", "export sip_redirect_context=context_2"}
    ,{"application", list_to_binary(["set continue_on_fail=", continue_on_fail(JObj)])}
    ,{"application", list_to_binary(["set hangup_after_bridge=", hangup_after_bridge(JObj)])}
     |DP
    ].

-spec post_exec(kz_term:proplist(), kz_term:ne_binary()) -> kz_term:proplist().
post_exec(DP, AppUUID) ->
    Props = [{<<"Application-UUID">>, AppUUID}],
    Event = ecallmgr_util:create_masquerade_event(<<"bridge">>, <<"CHANNEL_EXECUTE_COMPLETE">>, Props),
    [{"application", Event}
    ,{"application", "park"}
     |DP
    ].

-spec create_command(kz_term:proplist(), atom(), kz_term:ne_binary(), channel(), kz_json:object()) -> kz_term:proplist().
create_command(DP, Node, UUID, #channel{profile=ChannelProfile}, JObj) ->
    BypassAfterBridge = ?BYPASS_MEDIA_AFTER_BRIDGE,
    BridgeProfile = kz_term:to_binary(kz_json:get_value(<<"SIP-Interface">>, JObj, ?DEFAULT_FS_PROFILE)),
    EPs = kz_json:get_list_value(<<"Endpoints">>, JObj, []),
    Endpoints = maybe_bypass_after_bridge(BypassAfterBridge, BridgeProfile, ChannelProfile, EPs),

    {CommonProperties, UniqueEndpoints} = kz_json:lift_common_properties(Endpoints, ?UNLIFTABLE_KEYS),

    lager:debug("lifting from leg to channel: ~s", [kz_json:encode(CommonProperties)]),
    UpdatedJObj = kz_json:set_value(<<"Endpoints">>, UniqueEndpoints, kz_json:merge(JObj, CommonProperties)),

    LiftedCmd = list_to_binary(["bridge "
                               ,build_channels_vars(Node, UUID, UniqueEndpoints, UpdatedJObj)
                               ,try_create_bridge_string(UniqueEndpoints, UpdatedJObj)
                               ]),

    [{"application", LiftedCmd}|DP].

-spec maybe_bypass_after_bridge(boolean(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:objects()) -> kz_json:objects().
maybe_bypass_after_bridge('false', _, _, Endpoints) ->
    [kz_json:delete_key(<<"Bypass-Media">>, Endpoint) || Endpoint <- Endpoints];
maybe_bypass_after_bridge('true', BridgeProfile, ChannelProfile, Endpoints) ->
    [maybe_remove_endpoint_bypass(Endpoint, BridgeProfile, ChannelProfile)
     || Endpoint <- Endpoints
    ].

-spec maybe_remove_endpoint_bypass(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
maybe_remove_endpoint_bypass(Endpoint, BridgeProfile, ChannelProfile) ->
    case kz_json:get_ne_binary_value(<<"SIP-Interface">>, Endpoint, BridgeProfile) of
        ChannelProfile -> Endpoint;
        _ -> kz_json:delete_key(<<"Bypass-Media">>, Endpoint)
    end.

-spec try_create_bridge_string(kz_json:objects(), kz_json:object()) -> kz_term:ne_binary().
try_create_bridge_string(Endpoints, JObj) ->
    DialSeparator = ecallmgr_util:get_dial_separator(JObj, Endpoints),
    case ecallmgr_util:build_bridge_string(Endpoints, DialSeparator) of
        <<>> ->
            lager:warning("bridge string resulted in no endpoints"),
            throw(<<"registrar returned no endpoints">>);
        BridgeString -> BridgeString
    end.

-spec build_channels_vars(atom(), kz_term:ne_binary(), kz_json:objects(), kz_json:object()) -> iolist().
build_channels_vars(Node, UUID, Endpoints, JObj) ->
    Routines = [fun maybe_force_fax/4
               ,fun maybe_ignore_early_media/4
               ,fun add_bridge_actions/4
               ],
    Props = lists:foldl(fun(F, Acc) -> Acc ++ F(Node, UUID, Endpoints, JObj) end, [], Routines),
    ecallmgr_fs_xml:get_channel_vars(kz_json:set_values(Props, JObj)).

-spec maybe_force_fax(atom(), kz_term:ne_binary(), kz_json:objects(), kz_json:object()) -> kz_term:proplist().
maybe_force_fax(_Node, _UUID, Endpoints, JObj) ->
    case kz_json:find(<<"Force-Fax">>, Endpoints, kz_json:get_value(<<"Force-Fax">>, JObj)) of
        'undefined' -> [];
        Direction -> [{[<<"Custom-Channel-Vars">>, <<"Force-Fax">>], Direction}]
    end.

-spec maybe_ignore_early_media(atom(), kz_term:ne_binary(), kz_json:objects(), kz_json:object()) -> kz_term:proplist().
maybe_ignore_early_media(_Node, _UUID, Endpoints, JObj) ->
    case ecallmgr_util:get_dial_separator(JObj, Endpoints) of
        ?SEPARATOR_SINGLE -> [];
        _Separator -> [{<<"Ignore-Early-Media">>, 'true'}]
    end.

-spec add_endpoints_channel_actions(atom(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
add_endpoints_channel_actions(Node, UUID, JObj) ->
    Endpoints = kz_json:get_list_value(<<"Endpoints">>, JObj, []),
    kz_json:set_value(<<"Endpoints">>, build_endpoints_actions(Node, UUID, Endpoints), JObj).

-spec build_endpoints_actions(atom(), kz_term:ne_binary(), kz_json:objects()) -> kz_json:objects().
build_endpoints_actions(Node, UUID, Endpoints) ->
    Fun = fun(Endpoint) -> build_endpoint_actions(Node, UUID, Endpoint) end,
    lists:map(Fun, Endpoints).

-spec build_endpoint_actions(atom(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
build_endpoint_actions(Node, UUID, Endpoint) ->
    JObj = kz_json:get_json_value(<<"Endpoint-Actions">>, Endpoint, kz_json:new()),
    Fun = fun(K, V, Acc)-> build_endpoint_actions(Node, UUID, K, V, Acc) end,
    case kz_json:foldl(Fun, [], JObj) of
        [] -> Endpoint;
        Actions ->
            Var = kz_binary:join(Actions,<<?BRIDGE_CHANNEL_VAR_SEPARATOR>>),
            kz_json:set_value(?CHANNEL_ACTIONS_KEY, Var, Endpoint)
    end.

-type ep_actions() :: kz_term:ne_binaries().

-spec build_endpoint_actions(atom(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), ep_actions()) ->
          ep_actions().
build_endpoint_actions(Node, UUID, K, V, Acc) ->
    Fun = fun(K1, V1, Acc1)-> build_endpoint_action(Node, UUID, K1, V1, Acc1) end,
    DP = kz_json:foldr(Fun, [], V),
    Acc ++ build_endpoint_action_dp(K, DP).

-spec build_endpoint_action(atom(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), fs_apps()) ->
          fs_apps().
build_endpoint_action(Node, UUID, _K, V, Acc) ->
    lager:debug("building dialplan action for ~s", [_K]),
    DP = ecallmgr_call_command:fetch_dialplan(Node, UUID, V, self()),
    Acc ++ DP.

-spec build_endpoint_action_dp(kz_term:ne_binary(), fs_apps()) -> ep_actions().
build_endpoint_action_dp(K, DP) ->
    build_endpoint_action_dp(endpoint_action_cmd(K), DP, 1, []).

-spec build_endpoint_action_dp(kz_term:ne_binary(), fs_apps(), pos_integer(), ep_actions()) -> ep_actions().
build_endpoint_action_dp(_K, [], _N, Acc) ->
    lists:reverse(Acc);
build_endpoint_action_dp(K, [{App, Args} | DP], N, Acc) ->
    DPApp = ecallmgr_util:dialplan_application(App),
    DPArgs = kz_term:to_list(Args),
    Seq = kz_term:to_list(N),
    Var = list_to_binary([K, "_", Seq, "=", DPApp, " ", DPArgs, ""]),
    build_endpoint_action_dp(K, DP, N + 1, [Var | Acc]).

-spec endpoint_action_cmd(kz_term:ne_binary()) -> kz_term:ne_binary().
endpoint_action_cmd(Event) ->
    case lists:keyfind(Event, 1, ?DP_EVENT_VARS) of
        'false' -> normalize_event_action_key(Event);
        {_, Prefix} -> Prefix
    end.

-spec add_bridge_actions(atom(), kz_term:ne_binary(), kz_json:objects(), kz_json:object()) -> kz_term:proplist().
add_bridge_actions(Node, UUID, _Endpoints, JObj) ->
    BridgeActions = kz_json:get_json_value(<<"Bridge-Actions">>, JObj, kz_json:new()),
    Fun = fun(K, V, Acc)-> build_bridge_actions(Node, UUID, K, V, Acc) end,
    case kz_json:foldl(Fun, [], BridgeActions) of
        [] -> [];
        Actions -> [{?CHANNEL_ACTIONS_KEY, kz_binary:join(Actions,<<",">>)}]
    end.

-spec build_bridge_actions(atom(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), ep_actions()) ->
          ep_actions().
build_bridge_actions(Node, UUID, K, V, Acc) ->
    Fun = fun(K1, V1, Acc1)-> build_bridge_action(Node, UUID, K1, V1, Acc1) end,
    DP = kz_json:foldr(Fun, [], V),
    Acc ++ build_bridge_action_dp(K, DP).

-spec build_bridge_action(atom(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), fs_apps()) ->
          fs_apps().
build_bridge_action(Node, UUID, _K, V, Acc) ->
    lager:debug("building dialplan action for ~s", [_K]),
    DP = ecallmgr_call_command:fetch_dialplan(Node, UUID, V, self()),
    Acc ++ DP.

-spec build_bridge_action_dp(kz_term:ne_binary(), fs_apps()) -> ep_actions().
build_bridge_action_dp(K, DP) ->
    build_bridge_action_dp(bridge_action_cmd(K), DP, 1, []).

-spec build_bridge_action_dp(kz_term:ne_binary(), fs_apps(), pos_integer(), ep_actions()) -> ep_actions().
build_bridge_action_dp(_K, [], _N, Acc) ->
    lists:reverse(Acc);
build_bridge_action_dp(K, [{App, Args} | DP], N, Acc) ->
    DPApp = ecallmgr_util:dialplan_application(App),
    DPArgs = kz_term:to_list(Args),
    Seq = kz_term:to_list(N),
    Var = list_to_binary([K, "_", Seq, "=", DPApp, " ", DPArgs, ""]),
    build_bridge_action_dp(K, DP, N + 1, [Var | Acc]).

-spec bridge_action_cmd(kz_term:ne_binary()) -> kz_term:ne_binary().
bridge_action_cmd(Event) ->
    case lists:keyfind(Event, 1, ?DP_EVENT_VARS) of
        'false' -> normalize_event_action_key(Event);
        {_, Prefix} -> Prefix
    end.

-spec normalize_event_action_key(kz_term:ne_binary()) -> kz_term:ne_binary().
normalize_event_action_key(Key) when is_binary(Key) ->
    << <<(normalize_event_action_char(B))>> || <<B>> <= Key>>.

-spec normalize_event_action_char(char()) -> char().
normalize_event_action_char($-) -> $_;
normalize_event_action_char(C) when is_integer(C), $A =< C, C =< $Z -> C + 32;
normalize_event_action_char(C) when is_integer(C), 16#C0 =< C, C =< 16#D6 -> C + 32; % from string:to_lower
normalize_event_action_char(C) when is_integer(C), 16#D8 =< C, C =< 16#DE -> C + 32; % so we only loop once
normalize_event_action_char(C) -> C.
