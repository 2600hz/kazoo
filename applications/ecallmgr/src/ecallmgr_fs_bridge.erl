%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% Helpers for bridging in FreeSWITCH
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_bridge).

-export([call_command/3
        ,unbridge/2
        ]).

-include("ecallmgr.hrl").

-define(BYPASS_MEDIA_AFTER_BRIDGE, ecallmgr_config:get_boolean(<<"use_bypass_media_after_bridge">>, 'false')).
-define(CHANNEL_ACTIONS_KEY, [<<"Custom-Channel-Vars">>, <<"Channel-Actions">>]).

-spec call_command(atom(), ne_binary(), kz_json:object()) -> {'error', binary()} | {binary(), kz_proplist()}.
call_command(Node, UUID, JObj) ->
    Endpoints = kz_json:get_list_value(<<"Endpoints">>, JObj, []),
    case kapi_dialplan:bridge_v(JObj) of
        'false' -> {'error', <<"bridge failed to execute as JObj did not validate">>};
        'true' when Endpoints =:= [] -> {'error', <<"bridge request had no endpoints">>};
        'true' ->
            %% if we are intending to ring multiple device simultaneously then
            %% execute ring_ready so we dont leave the caller hanging with dead air.
            %% this does not test how many are ACTUALLY dialed (registered)
            %% since that is one of the things we want to be ringing during

            lager:debug("executing bridge on channel ~p", [UUID]),

            Channel = case ecallmgr_fs_channel:fetch(UUID, 'record') of
                          {'ok', Chan} -> Chan;
                          _ ->
                              lager:warning("channel ~s not found in channels ets table. bypass_media may be affected", [UUID]),
                              #channel{}
                      end,

            BridgeJObj = add_endpoints_channel_actions(Node, UUID, JObj),

            Routines = [fun handle_ringback/5
                       ,fun maybe_early_media/5
                       ,fun handle_hold_media/5
                       ,fun handle_secure_rtp/5
                       ,fun maybe_handle_bypass_media/5
                       ,fun handle_ccvs/5
                       ,fun pre_exec/5
                       ,fun handle_loopback/5
                       ,fun create_command/5
                       ],
            lager:debug("creating bridge dialplan"),
            XferExt = lists:foldr(fun(F, DP) ->
                                          F(DP, Node, UUID, Channel, BridgeJObj)
                                  end
                                 ,[], Routines),
            {<<"xferext">>, XferExt}
    end.

-spec unbridge(ne_binary(), kz_json:object()) ->
                      kz_proplist() |
                      {'error', ne_binary()}.
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Bridge command helpers
%% @end
%%--------------------------------------------------------------------
-spec handle_ringback(kz_proplist(), atom(), ne_binary(), channel(), kz_json:object()) -> kz_proplist().
handle_ringback(DP, _Node, UUID, _Channel, JObj) ->
    case kz_json:get_first_defined([<<"Ringback">>
                                   ,[<<"Custom-Channel-Vars">>, <<"Ringback">>]
                                   ]
                                  ,JObj
                                  )
    of
        'undefined' -> DP;
        Media ->
            Stream = ecallmgr_util:media_path(Media, 'extant', UUID, JObj),
            lager:debug("bridge has custom ringback: ~s", [Stream]),
            [{"application", <<"kz_multiset ringback=", Stream/binary>>}
             |DP
            ]
    end.

-spec maybe_early_media(kz_proplist(), atom(), ne_binary(), channel(), kz_json:object()) -> kz_proplist().
maybe_early_media(DP, _Node, _UUID, _Channel, JObj) ->
    Endpoints = kz_json:get_list_value(<<"Endpoints">>, JObj, []),
    case ecallmgr_util:get_dial_separator(JObj, Endpoints) of
        ?SEPARATOR_SIMULTANEOUS ->
            [{"application", <<"ring_ready">>}
             |DP
            ];
        _ -> DP
    end.

-spec handle_hold_media(kz_proplist(), atom(), ne_binary(), channel(), kz_json:object()) -> kz_proplist().
handle_hold_media(DP, _Node, UUID, _Channel, JObj) ->
    case kz_json:get_value(<<"Hold-Media">>, JObj) of
        'undefined' ->
            case kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Hold-Media">>], JObj) of
                'undefined' -> DP;
                Media ->
                    Stream = ecallmgr_util:media_path(Media, 'extant', UUID, JObj),
                    lager:debug("bridge has custom music-on-hold in channel vars: ~s", [Stream]),
                    [{"application", <<"kz_multiset  hold_music=", Stream/binary," transfer_ringback=", Stream/binary>>}
                     |DP
                    ]
            end;
        Media ->
            Stream = ecallmgr_util:media_path(Media, 'extant', UUID, JObj),
            lager:debug("bridge has custom music-on-hold: ~s", [Stream]),
            [{"application", <<"kz_multiset  hold_music=", Stream/binary," transfer_ringback=", Stream/binary>>}
             |DP
            ]
    end.

-spec handle_secure_rtp(kz_proplist(), atom(), ne_binary(), channel(), kz_json:object()) -> kz_proplist().
handle_secure_rtp(DP, _Node, _UUID, _Channel, JObj) ->
    case kz_json:is_true(<<"Secure-RTP">>, JObj, 'false') of
        'true' -> [{"application", "set zrtp_secure_media=true"}|DP];
        'false' -> DP
    end.

-spec maybe_handle_bypass_media(kz_proplist(), atom(), ne_binary(), channel(), kz_json:object()) -> kz_proplist().
maybe_handle_bypass_media(DP, Node, UUID, Channel, JObj) ->
    case ?BYPASS_MEDIA_AFTER_BRIDGE of
        'true' -> DP;
        'false' -> handle_bypass_media(DP, Node, UUID, Channel, JObj)
    end.

-spec handle_bypass_media(kz_proplist(), atom(), ne_binary(), channel(), kz_json:object()) -> kz_proplist().
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

-spec maybe_bypass_endpoint_media(kz_json:objects(), ne_binary(), ne_binary(), kz_proplist()) -> kz_proplist().
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

-spec bypass_endpoint_media_enabled(kz_json:object(), ne_binary(), ne_binary()) -> boolean().
bypass_endpoint_media_enabled(Endpoint, BridgeProfile, ChannelProfile) ->
    EndpointProfile = kz_json:get_ne_binary_value(<<"SIP-Interface">>, Endpoint, BridgeProfile),
    kz_json:is_true(<<"Bypass-Media">>, Endpoint)
        andalso EndpointProfile =:= ChannelProfile.

-spec handle_ccvs(kz_proplist(), atom(), ne_binary(), channel(), kz_json:object()) -> kz_proplist().
handle_ccvs(DP, Node, UUID, _Channel, JObj) ->
    CCVs = kz_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    case kz_json:is_json_object(CCVs) of
        'false' -> DP;
        'true' ->
            Args = ecallmgr_util:process_fs_kv(Node, UUID, kz_json:to_proplist(CCVs), 'set'),
            AppArgs = ecallmgr_util:fs_args_to_binary(Args),
            [{"application", <<"kz_multiset ", AppArgs/binary>>}] ++ DP
    end.

-spec handle_loopback_key(boolean(), ne_binary(), kz_json:object()) -> kz_proplist().
handle_loopback_key('false', _Key, _JObj) -> [];
handle_loopback_key('true', Key, JObj) ->
    V = kz_term:to_binary(kz_json:is_false(Key, JObj, 'false')),
    K = ecallmgr_util:get_fs_key(Key),
    [{"application", <<"export ", K/binary, "=", V/binary>>}].

-spec handle_loopback_key(ne_binary(), kz_json:object()) -> kz_proplist().
handle_loopback_key(Key, JObj) ->
    Exists = kz_json:get_value(Key, JObj) =/= 'undefined',
    handle_loopback_key(Exists, Key, JObj).

-spec handle_loopback_keys(ne_binaries(), kz_json:object(), kz_proplist()) -> kz_proplist().
handle_loopback_keys([], _JObj, Acc) -> Acc;
handle_loopback_keys([Key | Keys], JObj, Acc) ->
    handle_loopback_keys(Keys, JObj, Acc ++ handle_loopback_key(Key, JObj)).

-spec handle_loopback(kz_proplist(), atom(), ne_binary(), channel(), kz_json:object()) -> kz_proplist().
handle_loopback(DP, _Node, _UUID, _Channel, JObj) ->
    Keys = [<<"Simplify-Loopback">>, <<"Loopback-Bowout">>],
    handle_loopback_keys(Keys, JObj, DP).

-spec pre_exec(kz_proplist(), atom(), ne_binary(), channel(), kz_json:object()) -> kz_proplist().
pre_exec(DP, _Node, _UUID, _Channel, _JObj) ->
    Exports = [{<<"sip_redirect_context">>, <<"context_2">>}
              ,{<<?CHANNEL_VAR_PREFIX, "Inception">>, <<"${", ?CHANNEL_VAR_PREFIX, "Inception}">>}
              ,{<<?CHANNEL_VAR_PREFIX, ?CALL_INTERACTION_ID>>, <<"${", ?CHANNEL_VAR_PREFIX, ?CALL_INTERACTION_ID, "}">>}
              ,{<<"Call-Control-Queue">>, <<"${Call-Control-Queue}">>}
              ,{<<"Call-Control-PID">>, <<"${Call-Control-PID}">>}
              ,{<<"Switch-URI">>, <<"${Switch-URI}">>}
              ],
    CmdExport = kz_binary:join([<<K/binary, "=", V/binary>> || {K, V} <- Exports], <<" ">>),
    [{"application", "kz_multiset continue_on_fail=true hangup_after_bridge=true"}
    ,{"application", <<"kz_export ", CmdExport/binary>>}       
     |DP
    ].

-spec create_command(kz_proplist(), atom(), ne_binary(), channel(), kz_json:object()) -> kz_proplist().
create_command(DP, _Node, _UUID, #channel{profile=ChannelProfile}, JObj) ->
    BypassAfterBridge = ?BYPASS_MEDIA_AFTER_BRIDGE,
    BridgeProfile = kz_term:to_binary(kz_json:get_value(<<"SIP-Interface">>, JObj, ?DEFAULT_FS_PROFILE)),
    EPs = kz_json:get_list_value(<<"Endpoints">>, JObj, []),
    Endpoints = maybe_bypass_after_bridge(BypassAfterBridge, BridgeProfile, ChannelProfile, EPs),
    BridgeCmd = list_to_binary(["bridge "
                               ,build_channels_vars(Endpoints, JObj)
                               ,try_create_bridge_string(Endpoints, JObj)
                               ]),
    [{"application", BridgeCmd}|DP].

-spec maybe_bypass_after_bridge(boolean(), ne_binary(), ne_binary(), kz_json:objects()) -> kz_json:objects().
maybe_bypass_after_bridge('false', _, _, Endpoints) ->
    [kz_json:delete_key(<<"Bypass-Media">>, Endpoint) || Endpoint <- Endpoints];
maybe_bypass_after_bridge('true', BridgeProfile, ChannelProfile, Endpoints) ->
    [begin
         case kz_json:get_value(<<"SIP-Interface">>, Endpoint, BridgeProfile) of
             ChannelProfile -> Endpoint;
             _ -> kz_json:delete_key(<<"Bypass-Media">>, Endpoint)
         end
     end || Endpoint <- Endpoints].

-spec try_create_bridge_string(kz_json:objects(), kz_json:object()) -> ne_binary().
try_create_bridge_string(Endpoints, JObj) ->
    DialSeparator = ecallmgr_util:get_dial_separator(JObj, Endpoints),
    case ecallmgr_util:build_bridge_string(Endpoints, DialSeparator) of
        <<>> ->
            lager:warning("bridge string resulted in no enpoints"),
            throw(<<"registrar returned no endpoints">>);
        BridgeString -> BridgeString
    end.

-spec build_channels_vars(kz_json:objects(), kz_json:object()) -> iolist().
build_channels_vars(Endpoints, JObj) ->
    Props = case kz_json:find(<<"Force-Fax">>, Endpoints, kz_json:get_value(<<"Force-Fax">>, JObj)) of
                'undefined' -> [];
                Direction ->
                    [{[<<"Custom-Channel-Vars">>, <<"Force-Fax">>], Direction}]
            end,
    ecallmgr_fs_xml:get_channel_vars(kz_json:set_values(Props, JObj)).

-spec add_endpoints_channel_actions(atom(), ne_binary(), kz_json:object()) -> kz_json:object().
add_endpoints_channel_actions(Node, UUID, JObj) ->
    Endpoints = kz_json:get_list_value(<<"Endpoints">>, JObj, []),
    kz_json:set_value(<<"Endpoints">>, build_endpoints_actions(Node, UUID, Endpoints), JObj).

-spec build_endpoints_actions(atom(), ne_binary(), kz_json:objects()) -> kz_json:objects().
build_endpoints_actions(Node, UUID, Endpoints) ->
    Fun = fun(Endpoint) -> build_endpoint_actions(Node, UUID, Endpoint) end,
    lists:map(Fun, Endpoints).

-spec build_endpoint_actions(atom(), ne_binary(), kz_json:object()) -> kz_json:object().
build_endpoint_actions(Node, UUID, Endpoint) ->
    JObj = kz_json:get_json_value(<<"Endpoint-Actions">>, Endpoint, kz_json:new()),
    Fun = fun(K, V, Acc)-> build_endpoint_actions(Node, UUID, K, V, Acc) end,
    case kz_json:foldl(Fun, [], JObj) of
        [] -> Endpoint;
        Actions ->
            Var = kz_binary:join(Actions,<<?BRIDGE_CHANNEL_VAR_SEPARATOR>>),
            kz_json:set_value(?CHANNEL_ACTIONS_KEY, Var, Endpoint)
    end.

-type ep_actions() :: ne_binaries().

-spec build_endpoint_actions(atom(), ne_binary(), ne_binary(), kz_json:object(), ep_actions()) ->
                                    ep_actions().
build_endpoint_actions(Node, UUID, K, V, Acc) ->
    Fun = fun(K1, V1, Acc1)-> build_endpoint_action(Node, UUID, K1, V1, Acc1) end,
    DP = kz_json:foldr(Fun, [], V),
    Acc ++ build_endpoint_action_dp(K, DP).

-spec build_endpoint_action(atom(), ne_binary(), ne_binary(), kz_json:object(), fs_apps()) ->
                                   fs_apps().
build_endpoint_action(Node, UUID, _K, V, Acc) ->
    lager:debug("building dialplan action for ~s", [_K]),
    DP = ecallmgr_call_command:fetch_dialplan(Node, UUID, V, self()),
    Acc ++ DP.

-spec build_endpoint_action_dp(ne_binary(), fs_apps()) -> ep_actions().
build_endpoint_action_dp(K, DP) ->
    build_endpoint_action_dp(endpoint_action_cmd(K), DP, 1, []).

-spec build_endpoint_action_dp(ne_binary(), fs_apps(), pos_integer(), ep_actions()) -> ep_actions().
build_endpoint_action_dp(_K, [], _N, Acc) ->
    lists:reverse(Acc);
build_endpoint_action_dp(K, [{App, Args} | DP], N, Acc) ->
    DPApp = ecallmgr_util:dialplan_application(App),
    DPArgs = kz_term:to_list(Args),
    Seq = kz_term:to_list(N),
    Var = list_to_binary([K, "_", Seq, "=", DPApp, " ", DPArgs, ""]),
    build_endpoint_action_dp(K, DP, N + 1, [Var | Acc]).

-spec endpoint_action_cmd(ne_binary()) -> ne_binary().
endpoint_action_cmd(Event) ->
    case lists:keyfind(Event, 1, ?DP_EVENT_VARS) of
        'false' -> normalize_event_action_key(Event);
        {_, Prefix} -> Prefix
    end.

-spec normalize_event_action_key(ne_binary()) -> ne_binary().
normalize_event_action_key(Key) when is_binary(Key) ->
    << <<(normalize_event_action_char(B))>> || <<B>> <= Key>>.

-spec normalize_event_action_char(char()) -> char().
normalize_event_action_char($-) -> $_;
normalize_event_action_char(C) when is_integer(C), $A =< C, C =< $Z -> C + 32;
normalize_event_action_char(C) when is_integer(C), 16#C0 =< C, C =< 16#D6 -> C + 32; % from string:to_lower
normalize_event_action_char(C) when is_integer(C), 16#D8 =< C, C =< 16#DE -> C + 32; % so we only loop once
normalize_event_action_char(C) -> C.
