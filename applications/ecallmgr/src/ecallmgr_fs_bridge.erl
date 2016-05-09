%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Helpers for bridging in FreeSWITCH
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_bridge).

-include_lib("kazoo/src/api/kapi_dialplan.hrl").
-include("ecallmgr.hrl").

-export([call_command/3
         ,unbridge/2
         ,maybe_b_leg_events/3
        ]).

-define(BYPASS_MEDIA_AFTER_BRIDGE, ecallmgr_config:get_boolean(<<"use_bypass_media_after_bridge">>, 'false')).

call_command(Node, UUID, JObj) ->
    Endpoints = kz_json:get_ne_value(<<"Endpoints">>, JObj, []),
    case kapi_dialplan:bridge_v(JObj) of
        'false' -> {'error', <<"bridge failed to execute as JObj did not validate">>};
        'true' when Endpoints =:= [] -> {'error', <<"bridge request had no endpoints">>};
        'true' ->
            %% if we are intending to ring multiple device simultaneously then
            %% execute ring_ready so we dont leave the caller hanging with dead air.
            %% this does not test how many are ACTUALLY dialed (registered)
            %% since that is one of the things we want to be ringing during
            _ = handle_ringback(Node, UUID, JObj),
            _ = maybe_early_media(Node, UUID, JObj),
            _ = maybe_b_leg_events(Node, UUID, JObj),

            {'ok', Channel} = ecallmgr_fs_channel:fetch(UUID, 'record'),

            Routines = [fun handle_hold_media/5
                        ,fun handle_secure_rtp/5
                        ,fun maybe_handle_bypass_media/5
                        ,fun handle_ccvs/5
                        ,fun pre_exec/5
                        ,fun handle_loopback/5
                        ,fun create_command/5
                        ,fun post_exec/5
                       ],
            lager:debug("creating bridge dialplan"),
            XferExt = lists:foldr(fun(F, DP) ->
                                          F(DP, Node, UUID, Channel, JObj)
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
-spec handle_ringback(atom(), ne_binary(), kz_json:object()) -> 'ok'.
handle_ringback(Node, UUID, JObj) ->
    case kz_json:get_first_defined([<<"Ringback">>
                                    ,[<<"Custom-Channel-Vars">>, <<"Ringback">>]
                                   ]
                                   ,JObj
                                  )
    of
        'undefined' -> 'ok';
        Media ->
            Stream = ecallmgr_util:media_path(Media, 'extant', UUID, JObj),
            lager:debug("bridge has custom ringback: ~s", [Stream]),
            ecallmgr_fs_command:set(Node, UUID, [{<<"ringback">>, Stream}])
    end.

-spec maybe_early_media(atom(), ne_binary(), kz_json:object()) -> 'ok'.
maybe_early_media(Node, UUID, JObj) ->
    Endpoints = kz_json:get_ne_value(<<"Endpoints">>, JObj, []),
    case ecallmgr_util:get_dial_separator(JObj, Endpoints) of
        ?SEPARATOR_SINGLE -> 'ok';
        ?SEPARATOR_SIMULTANEOUS ->
            lager:debug("bridge is simultaneous to multiple endpoints, starting local ringing"),
            %% we don't really care if this succeeds, the call will fail later on
            ecallmgr_util:send_cmd(Node, UUID, <<"ring_ready">>, ""),
            'ok'
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
maybe_bypass_endpoint_media([Endpoint], BridgeProfile, ChannelProfile, DP) ->
    EndpointProfile = kz_json:get_value(<<"SIP-Interface">>, Endpoint, BridgeProfile),
    case kz_json:is_true(<<"Bypass-Media">>, Endpoint)
        andalso EndpointProfile =:= ChannelProfile of
        'true' -> [{"application", "set bypass_media=true"}|DP];
        'false' -> DP
    end;
maybe_bypass_endpoint_media(_, _, _, DP) ->
    DP.

-spec handle_ccvs(kz_proplist(), atom(), ne_binary(), channel(), kz_json:object()) -> kz_proplist().
handle_ccvs(DP, _Node, _UUID, _Channel, JObj) ->
    CCVs = kz_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    case kz_json:is_json_object(CCVs) of
        'true' ->
            [{"application", <<"set ", Var/binary, "=", (kz_term:to_binary(V))/binary>>}
             || {K, V} <- kz_json:to_proplist(CCVs),
                (Var = props:get_value(K, ?SPECIAL_CHANNEL_VARS)) =/= 'undefined'
            ] ++ DP;
        _ ->
            DP
    end.

-spec handle_loopback_key(ne_binary(), kz_json:object()) -> kz_proplist().
handle_loopback_key(Key, JObj) ->
    V = kz_term:to_binary(kz_json:is_false(Key, JObj, 'false')),
    K = ecallmgr_util:get_fs_key(Key),
    [{"application", <<"export ", K/binary, "=", V/binary>>}].

-spec handle_loopback_keys(ne_binary(), kz_json:object(), kz_proplist()) -> kz_proplist().
handle_loopback_keys([], _JObj, Acc) -> Acc;
handle_loopback_keys([Key | Keys], JObj, Acc) ->
    handle_loopback_keys(Keys, JObj, handle_loopback_key(Key, JObj) ++ Acc).

-spec handle_loopback(kz_proplist(), atom(), ne_binary(), channel(), kz_json:object()) -> kz_proplist().
handle_loopback(DP, _Node, _UUID, _Channel, JObj) ->
    Keys = [<<"Simplify-Loopback">>, <<"Loopback-Bowout">>],
    handle_loopback_keys(Keys, JObj, DP).

-spec pre_exec(kz_proplist(), atom(), ne_binary(), channel(), kz_json:object()) -> kz_proplist().
pre_exec(DP, _Node, _UUID, _Channel, _JObj) ->
    [{"application", "set continue_on_fail=true"}
     ,{"application", "export sip_redirect_context=context_2"}
     ,{"application", "set hangup_after_bridge=true"}
     ,{"application", lists:concat(["export "
                                    ,?CHANNEL_VAR_PREFIX, "Inception"
                                    ,"="
                                    ,"${", ?CHANNEL_VAR_PREFIX, "Inception}"
                                   ])}
     ,{"application", lists:concat(["export "
                                    ,?CHANNEL_VAR_PREFIX, ?CALL_INTERACTION_ID
                                    ,"="
                                    ,"${", ?CHANNEL_VAR_PREFIX, ?CALL_INTERACTION_ID, "}"
                                   ])}
     |DP
    ].

-spec create_command(kz_proplist(), atom(), ne_binary(), channel(), kz_json:object()) -> kz_proplist().
create_command(DP, _Node, _UUID, #channel{profile=ChannelProfile}, JObj) ->
    BypassAfterBridge = ?BYPASS_MEDIA_AFTER_BRIDGE,
    BridgeProfile = kz_term:to_binary(kz_json:get_value(<<"SIP-Interface">>, JObj, ?DEFAULT_FS_PROFILE)),
    EPs = kz_json:get_ne_value(<<"Endpoints">>, JObj, []),
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

-spec post_exec(kz_proplist(), atom(), ne_binary(), channel(), kz_json:object()) -> kz_proplist().
post_exec(DP, _Node, _UUID, _Channel, _JObj) ->
    Event = ecallmgr_util:create_masquerade_event(<<"bridge">>, <<"CHANNEL_EXECUTE_COMPLETE">>),
    [{"application", Event}
     ,{"application", "park "}
     |DP
    ].

-spec maybe_b_leg_events(atom(), ne_binary(), kz_json:object()) -> 'ok'.
maybe_b_leg_events(Node, UUID, JObj) ->
    Events = kz_json:get_value(<<"B-Leg-Events">>, JObj, []),
    ecallmgr_call_events:listen_for_other_leg(Node, UUID, Events).
