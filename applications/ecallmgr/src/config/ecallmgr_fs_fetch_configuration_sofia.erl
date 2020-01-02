%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc Send config commands to FS
%%%
%%% @author Edouard Swiac
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_fetch_configuration_sofia).

%% API
-export([init/0]).

-export([sofia/1]).

-include("ecallmgr.hrl").


%%%=============================================================================
%%% API
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Initializes the bindings
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = kazoo_bindings:bind(<<"fetch.configuration.*.*.sofia.conf">>, ?MODULE, 'sofia'),
    'ok'.

-spec sofia(map()) -> fs_sendmsg_ret().
sofia(#{node := Node, fetch_id := Id}=Ctx) ->
    kz_log:put_callid(Id),
    case kapps_config:is_true(?APP_NAME, <<"sofia_conf">>, 'false') of
        'false' ->
            lager:info("sofia conf disabled"),
            {'ok', Resp} = ecallmgr_fs_xml:not_found(),
            freeswitch:fetch_reply(Ctx#{reply => iolist_to_binary(Resp)});
        'true' ->
            Profiles = kapps_config:get_json(?APP_NAME, <<"fs_profiles">>, kz_json:new()),
            DefaultProfiles = default_sip_profiles(Node),
            try ecallmgr_fs_xml:sip_profiles_xml(kz_json:merge(DefaultProfiles, Profiles)) of
                {'ok', ConfigXml} ->
                    lager:debug("sending sofia XML to ~s: ~s", [Node, ConfigXml]),
                    freeswitch:fetch_reply(Ctx#{reply => erlang:iolist_to_binary(ConfigXml)})
            catch
                _E:_R ->
                    lager:info("sofia profile resp failed to convert to XML (~s): ~p", [_E, _R]),
                    {'ok', Resp} = ecallmgr_fs_xml:not_found(),
                    freeswitch:fetch_reply(Ctx#{reply => iolist_to_binary(Resp)})
            end
    end.


-spec default_sip_profiles(atom()) -> kz_json:object().
default_sip_profiles(Node) ->
    Gateways = case kapps_config:is_true(?APP_NAME, <<"process_gateways">>, 'false') of
                   'false' -> kz_json:new();
                   'true' ->
                       SysconfResp = kapps_config:get_json(?APP_NAME, <<"gateways">>, kz_json:new()),
                       _ = maybe_kill_node_gateways(SysconfResp, Node),
                       SysconfResp
               end,
    JObj = kz_json:from_list([{kz_term:to_binary(?DEFAULT_FS_PROFILE)
                              ,kz_json:from_list(default_sip_profile())}
                             ]),
    kz_json:set_value([kz_term:to_binary(?DEFAULT_FS_PROFILE), <<"Gateways">>]
                     ,Gateways
                     ,JObj
                     ).

-spec default_sip_profile() -> kz_term:proplist().
default_sip_profile() ->
    [{<<"Settings">>, kz_json:from_list(default_sip_settings())}
    ,{<<"Gateways">>, kz_json:from_list(default_sip_gateways())}
    ].

-spec default_sip_settings() -> kz_term:proplist().
default_sip_settings() ->
    [{<<"message-threads">>, <<"10">>}
    ,{<<"auth-calls">>, <<"true">>}
    ,{<<"apply-nat-acl">>, <<"rfc1918.auto">>}
    ,{<<"apply-inbound-acl">>, <<"trusted">>}
    ,{<<"apply-proxy-acl">>, <<"authoritative">>}
    ,{<<"local-network-acl">>, <<"localnet.auto">>}
    ,{<<"challenge-realm">>, <<"auto_from">>}
    ,{<<"multiple-registrations">>, <<"false">>}
    ,{<<"accept-blind-reg">>, <<"false">>}
    ,{<<"accept-blind-auth">>, <<"false">>}
    ,{<<"nonce-ttl">>, <<"86400">>}
    ,{<<"disable-register">>, <<"false">>}
    ,{<<"inbound-reg-force-matching-username">>, <<"true">>}
    ,{<<"auth-all-packets">>, <<"false">>}
    ,{<<"context">>, <<"context_2">>}
    ,{<<"dialplan">>, <<"XML">>}
    ,{<<"manual-redirect">>, <<"false">>}
    ,{<<"disable-transfer">>, <<"false">>}
    ,{<<"sip-ip">>, <<"$${local_ip_v4}">>}
    ,{<<"ext-sip-ip">>, <<"auto">>}
    ,{<<"sip-port">>, <<"5060">>}
    ,{<<"user-agent-string">>, <<"2600hz">>}
    ,{<<"enable-100rel">>, <<"false">>}
    ,{<<"max-proceeding">>, <<"1000">>}
    ,{<<"inbound-use-callid-as-uuid">>, <<"true">>}
    ,{<<"outbound-use-uuid-as-callid">>, <<"true">>}
    ,{<<"rtp-ip">>, <<"$${local_ip_v4}">>}
    ,{<<"ext-rtp-ip">>, <<"auto">>}
    ,{<<"rtp-timer-name">>, <<"soft">>}
    ,{<<"rtp-autoflush-during-bridge">>, <<"true">>}
    ,{<<"rtp-rewrite-timestamps">>, <<"false">>}
    ,{<<"hold-music">>, <<"local_stream://default">>}
    ,{<<"record-path">>, <<"$${recordings_dir}">>}
    ,{<<"record-template">>, <<"${caller_id_number}.${target_domain}.${strftime(%Y-%m-%d-%H-%M-%S)}.wav">>}
    ,{<<"dtmf-duration">>, <<"960">>}
    ,{<<"rfc2833-pt">>, <<"101">>}
    ,{<<"dtmf-type">>, <<"rfc2833">>}
    ,{<<"pass-rfc2833">>, <<"false">>}
    ,{<<"inbound-codec-prefs">>, <<"$${codecs}">>}
    ,{<<"outbound-codec-prefs">>, <<"$${codecs}">>}
    ,{<<"inbound-codec-negotiation">>, <<"generous">>}
    ,{<<"inbound-late-negotiation">>, <<"false">>}
    ,{<<"disable-transcoding">>, <<"false">>}
    ,{<<"t38-passthru">>, <<"true">>}
    ,{<<"all-reg-options-ping">>, <<"true">>}
    ,{<<"enable-timer">>, <<"false">>}
    ,{<<"rtp-timeout-sec">>, <<"3600">>}
    ,{<<"rtp-hold-timeout-sec">>, <<"3600">>}
    ,{<<"minimum-session-expires">>, <<"90">>}
    ,{<<"manage-presence">>, <<"true">>}
    ,{<<"send-message-query-on-register">>, <<"false">>}
    ,{<<"watchdog-enabled">>, <<"false">>}
    ,{<<"debug">>, <<"info">>}
    ,{<<"sip-trace">>, <<"true">>}
    ,{<<"log-auth-failures">>, <<"true">>}
    ,{<<"log-level">>, <<"info">>}
    ,{<<"tracelevel">>, <<"debug">>}
    ,{<<"debug-presence">>, <<"0">>}
    ,{<<"debug-sla">>, <<"0">>}
    ,{<<"auto-restart">>, <<"false">>}
    ,{<<"rtp-enable-zrtp">>, <<"true">>}
    ,{<<"liberal-dtmf">>, <<"true">>}
    ].

default_sip_gateways() -> [].

maybe_kill_node_gateways(JObj, Node) ->
    try get_node_gateways(Node) of
        Gateways ->
            NewNames = kz_json:get_keys(JObj),
            _ = maybe_kill_changed_gateways(NewNames, Gateways, JObj, Node),
            RunningNames = kz_json:get_keys(Gateways),
            _ = maybe_kill_removed_gateways(RunningNames, JObj, Node)
    catch
        _:_:_ -> 'ok'
    end.

maybe_kill_removed_gateways([], _, _) -> 'ok';
maybe_kill_removed_gateways([GatewayName|Names], JObj, Node) ->
    _ = case kz_json:get_value(GatewayName, JObj) of
            'undefined' -> kill_gateway(GatewayName, Node);
            _Else -> 'ok'
        end,
    maybe_kill_removed_gateways(Names, JObj, Node).

maybe_kill_changed_gateways([], _, _, _) -> 'ok';
maybe_kill_changed_gateways([GatewayName|Names], Gateways, JObj, Node) ->
    Running =  kz_json:get_value(GatewayName, Gateways),
    New = kz_json:get_value(GatewayName, JObj),
    _ = maybe_kill_changed_gateway(GatewayName, Running, New, Node),
    maybe_kill_changed_gateways(Names, Gateways, JObj, Node).

maybe_kill_changed_gateway(_, 'undefined', _, _) -> 'ok';
maybe_kill_changed_gateway(GatewayName, Running, New, Node) ->
    case compare_node_gateways(Running, New) of
        'false' -> kill_gateway(GatewayName, Node);
        'true' -> 'ok'
    end.

compare_node_gateways(Running, New) ->
    NewVersion = kz_json:get_value([<<"Variables">>, <<"Gateway-Version">>], New),
    case kz_json:get_value([<<"Inbound-Variables">>, <<"Gateway-Version">>], Running) of
        'undefined' -> 'true';
        NewVersion -> 'true';
        _Else -> 'false'
    end.

kill_gateway(GatewayName, Node) ->
    Args = ["profile "
           ,?DEFAULT_FS_PROFILE
           ," killgw "
           ,kz_term:to_list(GatewayName)
           ],
    freeswitch:api(Node, 'sofia', lists:flatten(Args)).

get_node_gateways(Node) ->
    {'ok', Response} = freeswitch:api(Node, 'sofia', "xmlstatus gateway"),
    {Xml, _} = xmerl_scan:string(kz_term:to_list(Response)),
    ecallmgr_fs_xml:sofia_gateways_xml_to_json(Xml).
