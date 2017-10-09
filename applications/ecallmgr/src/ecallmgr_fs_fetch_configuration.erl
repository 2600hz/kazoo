%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz INC
%%% @doc
%%% Send config commands to FS
%%% @end
%%% @contributors
%%%   Edouard Swiac
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_fetch_configuration).

%% API
-export([init/0]).

-export([acl/1]).
-export([conference/1]).
-export([kazoo/1]).
-export([sofia/1]).
-export([not_found/1]).

-include("ecallmgr.hrl").


%%--------------------------------------------------------------------
%% @doc
%% Initializes the bindings
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    kazoo_bindings:bind(<<"fetch.configuration.configuration.name.acl.conf">>, ?MODULE, 'acl'),
    kazoo_bindings:bind(<<"fetch.configuration.configuration.name.conference.conf">>, ?MODULE, 'conference'),
    kazoo_bindings:bind(<<"fetch.configuration.configuration.name.kazoo.conf">>, ?MODULE, 'kazoo'),
    kazoo_bindings:bind(<<"fetch.configuration.configuration.name.sofia.conf">>, ?MODULE, 'sofia'),
%    kazoo_bindings:bind(<<"fetch.configuration.configuration.name.*.conf">>, ?MODULE, 'not_found'),
    'ok'.


%%% ACL

-spec acl(map()) -> fs_sendmsg_ret().
acl(#{node := Node, fetch_id := FetchId, jobj := _JObj}) ->
    kz_util:put_callid(FetchId),

    SysconfResp = ecallmgr_config:fetch(<<"acls">>, kz_json:new(), ecallmgr_fs_node:fetch_timeout(Node)),

    try generate_acl_xml(SysconfResp) of
        'undefined' ->
            lager:warning("failed to query for ACLs; is sysconf running?"),
            {'ok', Resp} = ecallmgr_fs_xml:not_found(),
            freeswitch:fetch_reply(Node, FetchId, 'configuration', Resp);
        ConfigXml ->
            lager:debug("sending acl XML to ~s: ~s", [Node, ConfigXml]),
            freeswitch:fetch_reply(Node, FetchId, 'configuration', ConfigXml)
    catch
        _E:_R ->
            lager:info("acl resp failed to convert to XML (~s): ~p", [_E, _R]),
            {'ok', Resp} = ecallmgr_fs_xml:not_found(),
            freeswitch:fetch_reply(Node, FetchId, 'configuration', iolist_to_binary(Resp))
    end.

-spec generate_acl_xml(api_object()) -> api_binary().
generate_acl_xml('undefined') ->
    'undefined';
generate_acl_xml(SysconfResp) ->
    'false' = kz_json:is_empty(SysconfResp),
    {'ok', ConfigXml} = ecallmgr_fs_xml:acl_xml(SysconfResp),
    erlang:iolist_to_binary(ConfigXml).

%% KAZOO
-spec kazoo(tuple()) -> fs_sendmsg_ret().
kazoo(#{node := Node, fetch_id := FetchId, jobj := JObj}) ->
    kz_util:put_callid(FetchId),
    lager:debug("received configuration request for kazoo configuration ~p , ~p", [Node, FetchId]),
    fetch_mod_kazoo_config(Node, FetchId, kz_api:event_name(JObj), JObj).


-spec fetch_mod_kazoo_config(atom(), ne_binary(), ne_binary(), kz_json:object()) -> fs_sendmsg_ret().
fetch_mod_kazoo_config(Node, Id, <<"COMMAND">>, _JObj) ->
    lager:debug_unsafe("kazoo conf request : ~s", [kz_json:encode(_JObj, ['pretty'])]),
    config_req_not_handled(Node, Id, <<"kazoo.conf">>);
fetch_mod_kazoo_config(Node, Id, <<"REQUEST_PARAMS">>, JObj) ->
    Action = kz_json:get_ne_binary_value(<<"Action">>, JObj),
    fetch_mod_kazoo_config_action(Node, Id, Action, JObj);
fetch_mod_kazoo_config(Node, Id, Event, _JObj) ->
    lager:debug("unhandled mod kazoo config event : ~p : ~p", [Node, Event]),
    config_req_not_handled(Node, Id, <<"kazoo.conf">>).

-spec config_req_not_handled(atom(), ne_binary(), ne_binary()) -> fs_sendmsg_ret().
config_req_not_handled(Node, Id, Conf) ->
    {'ok', NotHandled} = ecallmgr_fs_xml:not_found(),
    lager:debug("ignoring conf ~s: ~s", [Conf, Id]),
    freeswitch:fetch_reply(Node, Id, 'configuration', iolist_to_binary(NotHandled)).

-spec fetch_mod_kazoo_config_action(atom(), ne_binary(), api_ne_binary(), kz_proplist()) -> fs_sendmsg_ret().
fetch_mod_kazoo_config_action(Node, Id, <<"request-filter">>, _Data) ->
    {'ok', Xml} = ecallmgr_fs_xml:event_filters_resp_xml(?FS_EVENT_FILTERS),
    lager:debug("replying with xml response for request-filter params request"),
    freeswitch:fetch_reply(Node, Id, 'configuration', iolist_to_binary(Xml));
fetch_mod_kazoo_config_action(Node, Id, 'undefined', _Data) ->
    config_req_not_handled(Node, Id, <<"kazoo.conf">>);
fetch_mod_kazoo_config_action(Node, Id, Action, _Data) ->
    lager:debug("unhandled mod kazoo config action : ~p : ~p", [Node, Action]),
    config_req_not_handled(Node, Id, <<"kazoo.conf">>).

%% CONFERENCE
-spec conference(map()) -> fs_sendmsg_ret().
conference(#{node := Node, fetch_id := FetchId, jobj := JObj}) ->
    kz_util:put_callid(FetchId),
    fetch_conference_config(Node, FetchId, kz_api:event_name(JObj), JObj).

-spec fix_conference_profile(kz_json:object()) -> kz_json:object().
fix_conference_profile(Resp) ->
    Ps = kz_json:get_value(<<"Profiles">>, Resp),
    JObj = kz_json:map(fun fix_conference_profile/2, Ps),
    kz_json:set_value(<<"Profiles">>, JObj, Resp).

-spec fix_conference_profile(kz_json:path(), kz_json:object()) -> {kz_json:path(), kz_json:object()}.
fix_conference_profile(Name, Profile) ->
    [AccountId | _] = binary:split(Name, <<"_">>),
    Routines = [fun maybe_fix_profile_tts/1
               ,fun conference_sounds/1
               ,fun set_verbose_events/1
               ,{fun kz_json:set_value/3, <<"caller-controls">>, <<AccountId/binary, "_caller-controls">>}
               ,{fun kz_json:set_value/3, <<"moderator-controls">>, <<AccountId/binary, "_moderator-controls">>}
               ],
    {Name, kz_json:exec(Routines, Profile)}.

-spec set_verbose_events(kz_json:object()) -> kz_json:object().
set_verbose_events(Profile) ->
    kz_json:set_value(<<"verbose-events">>, <<"true">>, Profile).

-spec maybe_fix_profile_tts(kz_json:object()) -> kz_json:object().
maybe_fix_profile_tts(Profile) ->
    case kz_json:get_value(<<"tts-engine">>, Profile) of
        'undefined' -> Profile;
        <<"flite">> -> fix_flite_tts(Profile);
        _ -> Profile
    end.

-spec fix_flite_tts(kz_json:object()) -> kz_json:object().
fix_flite_tts(Profile) ->
    Voice = kz_json:get_value(<<"tts-voice">>, Profile),
    kz_json:set_value(<<"tts-voice">>, ecallmgr_fs_flite:voice(Voice), Profile).

-spec conference_sounds(kz_json:object()) -> kz_json:object().
conference_sounds(Profile) ->
    kz_json:foldl(fun conference_sound/3, Profile, Profile).

conference_sound(Key, Value, Profile) ->
    maybe_convert_sound(kz_binary:reverse(Key), Key, Value, Profile).

maybe_convert_sound(<<"dnuos-", _/binary>>, Key, Value, Profile) ->
    MediaName = ecallmgr_util:media_path(Value),
    kz_json:set_value(Key, MediaName, Profile);
maybe_convert_sound(_, _, _, Profile) -> Profile.


-spec fetch_conference_config(atom(), ne_binary(), ne_binary(), kz_json:object()) -> fs_sendmsg_ret().
fetch_conference_config(Node, Id, <<"COMMAND">>, JObj) ->
    Profile = kz_json:get_value(<<"profile_name">>, JObj),
    Conference = kz_json:get_value(<<"conference_name">>, JObj),
    maybe_fetch_conference_profile(Node, Id, Profile, Conference);
fetch_conference_config(Node, Id, <<"REQUEST_PARAMS">>, JObj) ->
    Action = kz_json:get_value(<<"Action">>, JObj),
    ConfName = kz_json:get_value(<<"Conf-Name">>, JObj),
    lager:debug("request conference:~p params:~p", [ConfName, Action]),
    fetch_conference_params(Node, Id, Action, ConfName, JObj).

fetch_conference_params(Node, Id, <<"request-controls">>, ConfName, JObj) ->
    Controls = kz_json:get_value(<<"Controls">>, JObj),
    lager:debug("request controls:~p for conference:~p", [Controls, ConfName]),
    Cmd = [{<<"Request">>, <<"Controls">>}
          ,{<<"Profile">>, ConfName}
          ,{<<"Controls">>, Controls} | kz_api:default_headers(?APP_NAME, ?APP_VERSION)],
    Resp = kz_amqp_worker:call(Cmd
                              ,fun kapi_conference:publish_config_req/1
                              ,fun kapi_conference:config_resp_v/1
                              ,ecallmgr_fs_node:fetch_timeout(Node)
                              ),
    {'ok', Xml} = handle_conference_params_response(Resp),
    send_conference_profile_xml(Node, Id, Xml);
fetch_conference_params(Node, Id, Action, ConfName, _JObj) ->
    lager:debug("undefined request_params action:~p conference:~p", [Action, ConfName]),
    {'ok', XmlResp} = ecallmgr_fs_xml:not_found(),
    send_conference_profile_xml(Node, Id, XmlResp).

handle_conference_params_response({'ok', Resp}) ->
    lager:debug("replying with xml response for conference params request"),
    ecallmgr_fs_xml:conference_resp_xml(Resp);
handle_conference_params_response({'error', 'timeout'}) ->
    lager:debug("timed out waiting for conference params"),
    ecallmgr_fs_xml:not_found();
handle_conference_params_response(_Error) ->
    lager:debug("failed to lookup conference params, error:~p", [_Error]),
    ecallmgr_fs_xml:not_found().

-spec maybe_fetch_conference_profile(atom(), ne_binary(), api_binary(), api_binary()) -> fs_sendmsg_ret().
maybe_fetch_conference_profile(Node, Id, 'undefined', _) ->
    lager:debug("failed to lookup undefined conference profile"),
    {'ok', XmlResp} = ecallmgr_fs_xml:not_found(),
    send_conference_profile_xml(Node, Id, XmlResp);

maybe_fetch_conference_profile(Node, Id, <<"page">> = Profile, Conference) ->
    [_ , AccountId | _] = binary:split(Conference, <<"_">>, ['global']),
    fetch_conference_profile(Node, Id, Profile, AccountId);
maybe_fetch_conference_profile(Node, Id, Profile, _Conference) ->
    [AccountId | _] = binary:split(Profile, <<"_">>),
    fetch_conference_profile(Node, Id, Profile, AccountId).
    
-spec fetch_conference_profile(atom(), ne_binary(), api_binary(), api_binary()) -> fs_sendmsg_ret().
fetch_conference_profile(Node, Id, Profile, AccountId) ->
    Cmd = [{<<"Request">>, <<"Conference">>}
          ,{<<"Profile">>, Profile}
          ,{<<"Account-ID">>, AccountId}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("fetching profile '~s'", [Profile]),
    XmlResp = case kz_amqp_worker:call(Cmd
                                      ,fun kapi_conference:publish_config_req/1
                                      ,fun kapi_conference:config_resp_v/1
                                      ,ecallmgr_fs_node:fetch_timeout(Node)
                                      )
              of
                  {'ok', Resp} ->
                      FixedTTS = fix_conference_profile(Resp),
                      {'ok', Xml} = ecallmgr_fs_xml:conference_resp_xml(FixedTTS, [{<<"Account-ID">>, AccountId}]),
                      lager:debug("replying with conference profile ~s", [Profile]),
                      Xml;
                  {'error', 'timeout'} ->
                      lager:debug("timed out waiting for conference profile for ~s", [Profile]),
                      {'ok', Resp} = ecallmgr_fs_xml:not_found(),
                      Resp;
                  _Other ->
                      lager:debug("failed to lookup conference profile for ~s: ~p", [Profile, _Other]),
                      {'ok', Resp} = ecallmgr_fs_xml:not_found(),
                      Resp
              end,
    send_conference_profile_xml(Node, Id, iolist_to_binary(XmlResp)).

-spec send_conference_profile_xml(atom(), ne_binary(), iolist()) -> fs_sendmsg_ret().
send_conference_profile_xml(Node, Id, XmlResp) ->
    lager:debug("sending conference profile XML to ~s: ~s", [Node, XmlResp]),
    freeswitch:fetch_reply(Node, Id, 'configuration', iolist_to_binary(XmlResp)).

%% SOFIA
-spec sofia(tuple()) -> fs_sendmsg_ret().
sofia(#{node := Node, fetch_id := FetchId, jobj := _JObj}) ->
    kz_util:put_callid(FetchId),
    case ecallmgr_config:is_true(<<"sofia_conf">>) of
        'false' ->
            lager:info("sofia conf disabled"),
            {'ok', Resp} = ecallmgr_fs_xml:not_found(),
            freeswitch:fetch_reply(Node, FetchId, 'configuration', iolist_to_binary(Resp));
        'true' ->
            Profiles = ecallmgr_config:fetch(<<"fs_profiles">>, kz_json:new()),
            DefaultProfiles = default_sip_profiles(Node),
            try ecallmgr_fs_xml:sip_profiles_xml(kz_json:merge(DefaultProfiles, Profiles)) of
                {'ok', ConfigXml} ->
                    lager:debug("sending sofia XML to ~s: ~s", [Node, ConfigXml]),
                    freeswitch:fetch_reply(Node, FetchId, 'configuration', erlang:iolist_to_binary(ConfigXml))
            catch
                _E:_R ->
                    lager:info("sofia profile resp failed to convert to XML (~s): ~p", [_E, _R]),
                    {'ok', Resp} = ecallmgr_fs_xml:not_found(),
                    freeswitch:fetch_reply(Node, FetchId, 'configuration', iolist_to_binary(Resp))
            end
    end.


-spec default_sip_profiles(atom()) -> kz_json:object().
default_sip_profiles(Node) ->
    Gateways = case ecallmgr_config:is_true(<<"process_gateways">>) of
                   'false' -> kz_json:new();
                   'true' ->
                       SysconfResp = ecallmgr_config:fetch(<<"gateways">>, kz_json:new()),
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

-spec default_sip_profile() -> kz_proplist().
default_sip_profile() ->
    [{<<"Settings">>, kz_json:from_list(default_sip_settings())}
    ,{<<"Gateways">>, kz_json:from_list(default_sip_gateways())}
    ].

-spec default_sip_settings() -> kz_proplist().
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
        _:_ -> 'ok'
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

%% NOT_FOUND
-spec not_found(tuple()) -> fs_sendmsg_ret().
not_found(#{node := Node, fetch_id := FetchId, jobj := _JObj}) ->
    {'ok', XmlResp} = ecallmgr_fs_xml:not_found(),
    freeswitch:fetch_reply(Node, FetchId, 'configuration', iolist_to_binary(XmlResp)).
