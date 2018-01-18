%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz INC
%%% @doc
%%% Send config commands to FS
%%% @end
%%% @contributors
%%%   Edouard Swiac
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_config).
-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2]).
-export([handle_config_req/4]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-define(SERVER, ?MODULE).

-include("ecallmgr.hrl").

-record(state, {node :: atom()
               ,options = [] :: kz_term:proplist()
               }).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------

-spec start_link(atom()) -> kz_types:startlink_ret().
start_link(Node) -> start_link(Node, []).

-spec start_link(atom(), kz_term:proplist()) -> kz_types:startlink_ret().
start_link(Node, Options) ->
    gen_server:start_link(?SERVER, [Node, Options], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init([atom() | kz_term:proplist()]) -> {'ok', state()}.
init([Node, Options]) ->
    process_flag('trap_exit', 'true'),
    kz_util:put_callid(Node),
    lager:info("starting new fs config listener for ~s", [Node]),
    gen_server:cast(self(), 'bind_to_configuration'),
    {'ok', #state{node=Node, options=Options}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast('bind_to_configuration', #state{node=Node}=State) ->
    case freeswitch:bind(Node, 'configuration') of
        'ok' -> {'noreply', State};
        {'error', Reason} ->
            lager:critical("unable to establish config bindings: ~p", [Reason]),
            {'stop', Reason, State}
    end;
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'fetch', 'configuration', <<"configuration">>, <<"name">>, Conf, ID, []}, #state{node=Node}=State) ->
    lager:debug("fetch configuration request from ~s: ~s", [Node, ID]),
    _ = kz_util:spawn(fun handle_config_req/4, [Node, ID, Conf, 'undefined']),
    {'noreply', State};
handle_info({'fetch', 'configuration', <<"configuration">>, <<"name">>, Conf, ID, ['undefined' | Data]}, #state{node=Node}=State) ->
    lager:debug("fetch configuration request from ~s: ~s", [Node, ID]),
    _ = kz_util:spawn(fun handle_config_req/4, [Node, ID, Conf, Data]),
    {'noreply', State};
handle_info({'fetch', 'configuration', <<"configuration">>, <<"name">>, Conf, ID, [UUID | Data]}, #state{node=Node}=State)
  when is_binary(UUID) ->
    lager:debug("fetch configuration request from ~s: ~s", [Node, ID]),
    _ = kz_util:spawn(fun handle_config_req/4, [Node, ID, Conf, Data]),
    {'noreply', State};
handle_info({'fetch', 'configuration', <<"configuration">>, <<"name">>, Conf, ID, Data}, #state{node=Node}=State)
  when is_list(Data) ->
    lager:debug("fetch configuration request from ~s: ~s", [Node, ID]),
    _ = kz_util:spawn(fun handle_config_req/4, [Node, ID, Conf, Data]),
    {'noreply', State};
handle_info({_Fetch, _Section, _Something, _Key, _Value, ID, _Data}, #state{node=Node}=State) ->
    lager:debug("unhandled fetch from section ~s for ~s:~s", [_Section, _Something, _Key]),
    {'ok', Resp} = ecallmgr_fs_xml:not_found(),
    _ = freeswitch:fetch_reply(Node, ID, 'configuration', iolist_to_binary(Resp)),
    {'noreply', State};
handle_info({'EXIT', _, 'noconnection'}, State) ->
    {stop, {'shutdown', 'noconnection'}, State};
handle_info({'EXIT', _, Reason}, State) ->
    {stop, Reason, State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{node=Node}) ->
    lager:info("config listener for ~s terminating: ~p", [Node, _Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec handle_config_req(atom(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist() | 'undefined') -> fs_sendmsg_ret().
handle_config_req(Node, Id, <<"acl.conf">>, _Props) ->
    kz_util:put_callid(Id),

    SysconfResp = ecallmgr_config:fetch(<<"acls">>, kz_json:new(), ecallmgr_fs_node:fetch_timeout(Node)),

    try generate_acl_xml(SysconfResp) of
        'undefined' ->
            lager:warning("failed to query for ACLs; is sysconf running?"),
            {'ok', Resp} = ecallmgr_fs_xml:not_found(),
            freeswitch:fetch_reply(Node, Id, 'configuration', Resp);
        ConfigXml ->
            lager:debug("sending acl XML to ~s: ~s", [Node, ConfigXml]),
            freeswitch:fetch_reply(Node, Id, 'configuration', ConfigXml)
    catch
        _E:_R ->
            lager:info("acl resp failed to convert to XML (~s): ~p", [_E, _R]),
            {'ok', Resp} = ecallmgr_fs_xml:not_found(),
            freeswitch:fetch_reply(Node, Id, 'configuration', iolist_to_binary(Resp))
    end;

handle_config_req(Node, Id, <<"sofia.conf">>, _Props) ->
    kz_util:put_callid(Id),
    case ecallmgr_config:is_true(<<"sofia_conf">>) of
        'false' ->
            lager:info("sofia conf disabled"),
            {'ok', Resp} = ecallmgr_fs_xml:not_found(),
            freeswitch:fetch_reply(Node, Id, 'configuration', iolist_to_binary(Resp));
        'true' ->
            Profiles = ecallmgr_config:fetch(<<"fs_profiles">>, kz_json:new()),
            DefaultProfiles = default_sip_profiles(Node),
            try ecallmgr_fs_xml:sip_profiles_xml(kz_json:merge(DefaultProfiles, Profiles)) of
                {'ok', ConfigXml} ->
                    lager:debug("sending sofia XML to ~s: ~s", [Node, ConfigXml]),
                    freeswitch:fetch_reply(Node, Id, 'configuration', erlang:iolist_to_binary(ConfigXml))
            catch
                _E:_R ->
                    lager:info("sofia profile resp failed to convert to XML (~s): ~p", [_E, _R]),
                    {'ok', Resp} = ecallmgr_fs_xml:not_found(),
                    freeswitch:fetch_reply(Node, Id, 'configuration', iolist_to_binary(Resp))
            end
    end;

handle_config_req(Node, Id, <<"conference.conf">>, Data) ->
    kz_util:put_callid(Id),
    fetch_conference_config(Node, Id, kzd_freeswitch:event_name(Data), Data);
handle_config_req(Node, Id, <<"kazoo.conf">>, Data) ->
    kz_util:put_callid(Id),
    lager:debug("received configuration request for kazoo configuration ~p , ~p", [Node, Id]),
    fetch_mod_kazoo_config(Node, Id, kzd_freeswitch:event_name(Data), Data);
handle_config_req(Node, Id, Conf, Data) ->
    kz_util:put_callid(Id),
    case kazoo_bindings:map(<<"freeswitch.config.", Conf/binary>>, [Node, Id, Conf, Data]) of
        [] -> config_req_not_handled(Node, Id, Conf);
        _  -> 'ok'
    end.

-spec config_req_not_handled(atom(), kz_term:ne_binary(), kz_term:ne_binary()) -> fs_sendmsg_ret().
config_req_not_handled(Node, Id, Conf) ->
    {'ok', NotHandled} = ecallmgr_fs_xml:not_found(),
    lager:debug("ignoring conf ~s: ~s", [Conf, Id]),
    freeswitch:fetch_reply(Node, Id, 'configuration', iolist_to_binary(NotHandled)).

-spec generate_acl_xml(kz_term:api_object()) -> kz_term:api_binary().
generate_acl_xml('undefined') ->
    'undefined';
generate_acl_xml(SysconfResp) ->
    'false' = kz_json:is_empty(SysconfResp),
    {'ok', ConfigXml} = ecallmgr_fs_xml:acl_xml(SysconfResp),
    erlang:iolist_to_binary(ConfigXml).

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


-spec fetch_conference_config(atom(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> fs_sendmsg_ret().
fetch_conference_config(Node, Id, <<"COMMAND">>, Data) ->
    maybe_fetch_conference_profile(Node, Id, props:get_value(<<"profile_name">>, Data));
fetch_conference_config(Node, Id, <<"REQUEST_PARAMS">>, Data) ->
    Action = props:get_value(<<"Action">>, Data),
    ConfName = props:get_value(<<"Conf-Name">>, Data),
    lager:debug("request conference:~p params:~p", [ConfName, Action]),
    fetch_conference_params(Node, Id, Action, ConfName, Data).

fetch_conference_params(Node, Id, <<"request-controls">>, ConfName, Data) ->
    Controls = props:get_value(<<"Controls">>, Data),
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
fetch_conference_params(Node, Id, Action, ConfName, _Data) ->
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

-spec maybe_fetch_conference_profile(atom(), kz_term:ne_binary(), kz_term:api_binary()) -> fs_sendmsg_ret().
maybe_fetch_conference_profile(Node, Id, 'undefined') ->
    lager:debug("failed to lookup undefined conference profile"),
    {'ok', XmlResp} = ecallmgr_fs_xml:not_found(),
    send_conference_profile_xml(Node, Id, XmlResp);

maybe_fetch_conference_profile(Node, Id, Profile) ->
    Cmd = [{<<"Request">>, <<"Conference">>}
          ,{<<"Profile">>, Profile}
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
                      {'ok', Xml} = ecallmgr_fs_xml:conference_resp_xml(FixedTTS),
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
    send_conference_profile_xml(Node, Id, XmlResp).

-spec send_conference_profile_xml(atom(), kz_term:ne_binary(), iolist()) -> fs_sendmsg_ret().
send_conference_profile_xml(Node, Id, XmlResp) ->
    lager:debug("sending conference profile XML to ~s: ~s", [Node, XmlResp]),
    freeswitch:fetch_reply(Node, Id, 'configuration', iolist_to_binary(XmlResp)).


-spec fetch_mod_kazoo_config(atom(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> fs_sendmsg_ret().
fetch_mod_kazoo_config(Node, Id, <<"COMMAND">>, _Data) ->
    config_req_not_handled(Node, Id, <<"kazoo.conf">>);
fetch_mod_kazoo_config(Node, Id, <<"REQUEST_PARAMS">>, Data) ->
    Action = props:get_ne_binary_value(<<"Action">>, Data),
    fetch_mod_kazoo_config_action(Node, Id, Action, Data);
fetch_mod_kazoo_config(Node, Id, Event, _Data) ->
    lager:debug("unhandled mod kazoo config event : ~p : ~p", [Node, Event]),
    config_req_not_handled(Node, Id, <<"kazoo.conf">>).

-spec fetch_mod_kazoo_config_action(atom(), kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:proplist()) -> fs_sendmsg_ret().
fetch_mod_kazoo_config_action(Node, Id, <<"request-filter">>, _Data) ->
    {'ok', Xml} = ecallmgr_fs_xml:event_filters_resp_xml(?FS_EVENT_FILTERS),
    lager:debug("replying with xml response for request-filter params request"),
    freeswitch:fetch_reply(Node, Id, 'configuration', iolist_to_binary(Xml));
fetch_mod_kazoo_config_action(Node, Id, 'undefined', _Data) ->
    config_req_not_handled(Node, Id, <<"kazoo.conf">>);
fetch_mod_kazoo_config_action(Node, Id, Action, _Data) ->
    lager:debug("unhandled mod kazoo config action : ~p : ~p", [Node, Action]),
    config_req_not_handled(Node, Id, <<"kazoo.conf">>).
