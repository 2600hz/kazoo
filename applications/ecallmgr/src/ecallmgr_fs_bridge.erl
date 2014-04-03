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

-include("ecallmgr.hrl").

-export([call_command/3
         ,maybe_b_leg_events/3
        ]).

call_command(Node, UUID, JObj) ->
    Endpoints = wh_json:get_ne_value(<<"Endpoints">>, JObj, []),
    case wapi_dialplan:bridge_v(JObj) of
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

            lager:debug("creating bridge dialplan"),
            Routines = [fun handle_hold_media/4
                        ,fun handle_secure_rtp/4
                        ,fun handle_bypass_media/4
                        ,fun handle_ccvs/4
                        ,fun pre_exec/4
                        ,fun create_command/4
                        ,fun post_exec/4
                       ],
            lager:debug("creating bridge dialplan"),
            XferExt = lists:foldr(fun(F, DP) ->
                                          F(DP, Node, UUID, JObj) end
                                  ,[], Routines),
            {<<"xferext">>, XferExt}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Bridge command helpers
%% @end
%%--------------------------------------------------------------------
handle_ringback(Node, UUID, JObj) ->
    case wh_json:get_value(<<"Ringback">>, JObj) of
        'undefined' ->
            case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Ringback">>], JObj) of
                'undefined' -> 'ok';
                Media ->
                    Stream = ecallmgr_util:media_path(Media, 'extant', UUID, JObj),
                    lager:debug("bridge has custom ringback in channel vars: ~s", [Stream]),
                    ecallmgr_util:set(Node, UUID, [{<<"ringback">>, Stream}])
            end;
        Media ->
            Stream = ecallmgr_util:media_path(Media, extant, UUID, JObj),
            lager:debug("bridge has custom ringback: ~s", [Stream]),
            ecallmgr_util:set(Node, UUID, [{<<"ringback">>, Stream}])
    end.

maybe_early_media(Node, UUID, JObj) ->
    case wh_json:get_value(<<"Dial-Endpoint-Method">>, JObj, <<"single">>) of
        <<"simultaneous">> ->
            case length(wh_json:get_ne_value(<<"Endpoints">>, JObj, [])) > 1 of
                'false' -> 'ok';
                'true' ->
                    lager:debug("bridge is simultaneous to multiple endpoints, starting local ringing"),
                    %% we don't really care if this succeeds, the call will fail later on
                    ecallmgr_util:send_cmd(Node, UUID, <<"ring_ready">>, "")
            end;
        _Else -> 'ok'
    end.

handle_hold_media(DP, _Node, UUID, JObj) ->
    case wh_json:get_value(<<"Hold-Media">>, JObj) of
        'undefined' ->
            case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Hold-Media">>], JObj) of
                'undefined' -> DP;
                Media ->
                    Stream = ecallmgr_util:media_path(Media, extant, UUID, JObj),
                    lager:debug("bridge has custom music-on-hold in channel vars: ~s", [Stream]),
                    [{"application", <<"set hold_music=", Stream/binary>>}
                     ,{"application", <<"set transfer_ringback=", Stream/binary>>}
                     |DP
                    ]
            end;
        Media ->
            Stream = ecallmgr_util:media_path(Media, extant, UUID, JObj),
            lager:debug("bridge has custom music-on-hold: ~s", [Stream]),
            [{"application", <<"set hold_music=", Stream/binary>>}
             ,{"application", <<"set transfer_ringback=", Stream/binary>>}
             |DP
            ]
    end.

handle_secure_rtp(DP, _Node, _UUID, JObj) ->
    case wh_json:is_true(<<"Secure-RTP">>, JObj, 'false') of
        'true' -> [{"application", "set zrtp_secure_media=true"}|DP];
        'false' -> DP
    end.

handle_bypass_media(DP, _Node, _UUID, JObj) ->
    case wh_json:get_value(<<"Media">>, JObj) of
        <<"process">> ->
            lager:debug("bridge will process media through host switch"),
            [{"application", "set bypass_media=false"}|DP];
        <<"bypass">> ->
            lager:debug("bridge will connect the media peer-to-peer"),
            [{"application", "set bypass_media=true"}|DP];
        _ ->
            Endpoints = wh_json:get_ne_value(<<"Endpoints">>, JObj, []),
            maybe_bypass_endpoint_media(Endpoints, DP)
    end.

-spec maybe_bypass_endpoint_media(wh_json:objects(), wh_proplist()) -> wh_proplist().
maybe_bypass_endpoint_media([Endpoint], DP) ->
    case wh_json:is_true(<<"Bypass-Media">>, Endpoint) of
        'true' ->
            [{"application", "set bypass_media=true"}|DP];
        'false' ->
            DP
    end;
maybe_bypass_endpoint_media(_, DP) ->
    DP.

handle_ccvs(DP, _Node, _UUID, JObj) ->
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    case wh_json:is_json_object(CCVs) of
        'true' ->
            [{"application", <<"set ", Var/binary, "=", (wh_util:to_binary(V))/binary>>}
             || {K, V} <- wh_json:to_proplist(CCVs),
                (Var = props:get_value(K, ?SPECIAL_CHANNEL_VARS)) =/= 'undefined'
            ] ++ DP;
        _ ->
            DP
    end.

pre_exec(DP, _, _, _) ->
    [{"application", "set continue_on_fail=true"}
     ,{"application", "export sip_redirect_context=context_2"}
     ,{"application", lists:concat(["export "
                                    ,?CHANNEL_VAR_PREFIX, "Inception"
                                    ,"="
                                    ,"${", ?CHANNEL_VAR_PREFIX, "Inception}"
                                   ])}
     |DP
    ].

create_command(DP, _Node, _UUID, JObj) ->
    Endpoints = wh_json:get_ne_value(<<"Endpoints">>, JObj, []),
    BridgeCmd = list_to_binary(["bridge "
                                ,build_channels_vars(Endpoints, JObj)
                                ,try_create_bridge_string(Endpoints, JObj)
                               ]),
    [{"application", BridgeCmd}|DP].

try_create_bridge_string(Endpoints, JObj) ->
    DialSeparator = determine_dial_separator(Endpoints, JObj),
    case ecallmgr_util:build_bridge_string(Endpoints, DialSeparator) of
        <<>> ->
            lager:warning("bridge string resulted in no enpoints"),
            throw(<<"registrar returned no endpoints">>);
        BridgeString -> BridgeString
    end.

build_channels_vars(Endpoints, JObj) ->
    Props = case wh_json:find(<<"Force-Fax">>, Endpoints, wh_json:get_value(<<"Force-Fax">>, JObj)) of
                'undefined' -> [];
                Direction ->
                    [{[<<"Custom-Channel-Vars">>, <<"Force-Fax">>], Direction}]
            end,
    ecallmgr_fs_xml:get_channel_vars(wh_json:set_values(Props, JObj)).

determine_dial_separator(Endpoints, JObj) ->
    case wh_json:get_value(<<"Dial-Endpoint-Method">>, JObj, <<"single">>) of
        <<"simultaneous">> when length(Endpoints) > 1 -> <<",">>;
        _Else -> <<"|">>
    end.

post_exec(DP, _, _, _) ->
    Event = ecallmgr_util:create_masquerade_event(<<"bridge">>, <<"CHANNEL_EXECUTE_COMPLETE">>),
    [{"application", Event}
     ,{"application", "park "}
     |DP
    ].

-spec maybe_b_leg_events(atom(), ne_binary(), wh_json:object()) -> 'ok'.
maybe_b_leg_events(Node, UUID, JObj) ->
    Events = wh_json:get_value(<<"B-Leg-Events">>, JObj, []),
    ecallmgr_call_events:listen_for_other_leg(Node, UUID, Events).
