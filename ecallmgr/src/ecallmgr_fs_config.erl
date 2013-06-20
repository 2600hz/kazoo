%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
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
-export([handle_config_req/3, handle_config_req/4]).
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
                ,options = [] :: wh_proplist()
               }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Node) -> start_link(Node, []).
start_link(Node, Options) ->
    gen_server:start_link(?MODULE, [Node, Options], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Node, Options]) ->
    put('callid', Node),
    lager:info("starting new fs config listener for ~s", [Node]),
    case freeswitch:bind(Node, 'configuration') of
        'ok' -> {'ok', #state{node=Node, options=Options}};
        {'error', Reason} ->
            lager:critical("unable to establish config bindings: ~p", [Reason]),
            {'stop', Reason};
        'timeout' ->
            lager:critical("unable to establish config bindings: timeout waiting for ~s", [Node]),
            {'stop', 'timeout'}
    end.

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
handle_info({'fetch', 'configuration', <<"configuration">>, <<"name">>, Conf, ID, []}, #state{node=Node}=State) ->
    lager:debug("fetch configuration request from ~s: ~s", [Node, ID]),
    spawn(?MODULE, 'handle_config_req', [Node, ID, Conf]),
    {'noreply', State};
handle_info({'fetch', 'configuration', <<"configuration">>, <<"name">>, Conf, ID, ['undefined' | Data]}, #state{node=Node}=State) ->
    lager:debug("fetch configuration request from ~s: ~s", [Node, ID]),
    spawn(?MODULE, 'handle_config_req', [Node, ID, Conf, Data]),
    {'noreply', State};
handle_info({_Fetch, _Section, _Something, _Key, _Value, ID, _Data}, #state{node=Node}=State) ->
    lager:debug("unhandled fetch from section ~s for ~s:~s", [_Section, _Something, _Key]),
    {'ok', Resp} = ecallmgr_fs_xml:not_found(),
    _ = freeswitch:fetch_reply(Node, ID, 'configuration', iolist_to_binary(Resp)),
    {'noreply', State};
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
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec handle_config_req(atom(), ne_binary(), ne_binary()) -> fs_sendmsg_ret().
handle_config_req(Node, ID, <<"acl.conf">>) ->
    put('callid', ID),
    SysconfResp = ecallmgr_config:fetch(<<"acls">>, wh_json:new()),
    try generate_acl_xml(SysconfResp) of
        ConfigXml ->
            lager:debug("sending XML to ~s: ~s", [Node, ConfigXml]),
            freeswitch:fetch_reply(Node, ID, 'configuration', ConfigXml)
    catch
        _E:_R ->
            lager:info("acl resp failed to convert to XML (~s): ~p", [_E, _R]),
            {'ok', Resp} = ecallmgr_fs_xml:not_found(),
            freeswitch:fetch_reply(Node, ID, 'configuration', iolist_to_binary(Resp))
    end;
handle_config_req(Node, Id, <<"sofia.conf">>) ->
    put('callid', Id),
    case wh_util:is_true(ecallmgr_config:get(<<"sofia_conf">>)) of
        'true' -> do_sofia_conf(Node, Id);
        'false' ->
            lager:info("sofia conf disabled"),
            {'ok', Resp} = ecallmgr_fs_xml:not_found(),
            freeswitch:fetch_reply(Node, Id, 'configuration', iolist_to_binary(Resp))
    end;
handle_config_req(Node, ID, _Conf) ->
    lager:debug("ignoring conf ~s: ~s", [_Conf, ID]),
    {'ok', Resp} = ecallmgr_fs_xml:not_found(),
    freeswitch:fetch_reply(Node, ID, 'configuration', iolist_to_binary(Resp)).

handle_config_req(Node, ID, <<"conference.conf">>, Data) ->
    put('callid', ID),
    Profile = props:get_value(<<"profile_name">>, Data, <<"default">>),
    Cmd =
        [{<<"Profile">>, Profile}
         | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
        ],
    XmlResp = case wh_amqp_worker:call(?ECALLMGR_AMQP_POOL, Cmd
                                       ,fun wapi_conference:publish_config_req/1
                                       ,fun wapi_conference:config_resp_v/1
                                      )
              of
                  {'ok', Resp} ->
                      FixedTTS = maybe_fix_conference_tts(Resp),
                      {'ok', Xml} = ecallmgr_fs_xml:conference_resp_xml(FixedTTS),
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
    lager:debug("replying to ~s with profile ~s: ~s", [ID, Profile, XmlResp]),
    freeswitch:fetch_reply(Node, ID, 'configuration', iolist_to_binary(XmlResp));

handle_config_req(Node, ID, _Conf, _) ->
    lager:debug("ignoring conf ~s: ~s", [_Conf, ID]),
    {'ok', Resp} = ecallmgr_fs_xml:not_found(),
    freeswitch:fetch_reply(Node, ID, 'configuration', iolist_to_binary(Resp)).

do_sofia_conf(Node, Id) ->
    Gateways = case wh_util:is_true(ecallmgr_config:get(<<"process_gateways">>, 'false')) of
                   'false' -> wh_json:new();
                   'true' ->
                       SysconfResp = ecallmgr_config:fetch(<<"gateways">>, wh_json:new()),
                       _ = maybe_kill_node_gateways(SysconfResp, Node),
                       SysconfResp
               end,
    DefaultProfiles = wh_json:set_value([wh_util:to_binary(?DEFAULT_FS_PROFILE), <<"Gateways">>]
                                        ,Gateways
                                        ,wh_json:from_list(default_sip_profiles())),
    try ecallmgr_fs_xml:sip_profiles_xml(DefaultProfiles) of
        {'ok', ConfigXml} ->
            lager:debug("sending sofia XML to ~s: ~s", [Node, ConfigXml]),
            freeswitch:fetch_reply(Node, Id, 'configuration', erlang:iolist_to_binary(ConfigXml))
    catch
        _E:_R ->
            lager:info("sofia resp failed to convert to XML (~s): ~p", [_E, _R]),
            {'ok', Resp} = ecallmgr_fs_xml:not_found(),
            freeswitch:fetch_reply(Node, Id, 'configuration', iolist_to_binary(Resp))
    end.

generate_acl_xml(SysconfResp) ->
    'false' = wh_json:is_empty(SysconfResp),
    {'ok', ConfigXml} = ecallmgr_fs_xml:acl_xml(SysconfResp),
    erlang:iolist_to_binary(ConfigXml).

default_sip_profiles() ->
    [{wh_util:to_binary(?DEFAULT_FS_PROFILE), wh_json:from_list(default_sip_profile())}].

default_sip_profile() ->
    [{<<"Settings">>, wh_json:from_list(default_sip_settings())}
     ,{<<"Gateways">>, wh_json:from_list(default_sip_gateways())}
    ].

-spec default_sip_settings() -> wh_proplist().
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
     ,{<<"minimum-session-expires">>, <<"120">>}
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
    ].

default_sip_gateways() -> [].

maybe_kill_node_gateways(JObj, Node) ->
    try get_node_gateways(Node) of
        Gateways ->
            NewNames = wh_json:get_keys(JObj),
            _ = maybe_kill_changed_gateways(NewNames, Gateways, JObj, Node),
            RunningNames = wh_json:get_keys(Gateways),
            _ = maybe_kill_removed_gateways(RunningNames, JObj, Node)
    catch
        _:_ -> 'ok'
    end.

maybe_kill_removed_gateways([], _, _) -> 'ok';
maybe_kill_removed_gateways([GatewayName|Names], JObj, Node) ->
    _ = case wh_json:get_value(GatewayName, JObj) of
            'undefined' -> kill_gateway(GatewayName, Node);
            _Else -> 'ok'
        end,
    maybe_kill_removed_gateways(Names, JObj, Node).

maybe_kill_changed_gateways([], _, _, _) -> 'ok';
maybe_kill_changed_gateways([GatewayName|Names], Gateways, JObj, Node) ->
    Running =  wh_json:get_value(GatewayName, Gateways),
    New = wh_json:get_value(GatewayName, JObj),
    _ = maybe_kill_changed_gateway(GatewayName, Running, New, Node),
    maybe_kill_changed_gateways(Names, Gateways, JObj, Node).

maybe_kill_changed_gateway(_, 'undefined', _, _) -> 'ok';
maybe_kill_changed_gateway(GatewayName, Running, New, Node) ->
    case compare_node_gateways(Running, New) of
        'false' -> kill_gateway(GatewayName, Node);
        'true' -> 'ok'
    end.

compare_node_gateways(Running, New) ->
    NewVersion = wh_json:get_value([<<"Variables">>, <<"Gateway-Version">>], New),
    case wh_json:get_value([<<"Inbound-Variables">>, <<"Gateway-Version">>], Running) of
        'undefined' -> 'true';
        NewVersion -> 'true';
        _Else -> 'false'
    end.

kill_gateway(GatewayName, Node) ->
    Args = ["profile "
            ,?DEFAULT_FS_PROFILE
            ," killgw "
            ,wh_util:to_list(GatewayName)
           ],
    freeswitch:api(Node, 'sofia', lists:flatten(Args)).

get_node_gateways(Node) ->
    {'ok', Response} = freeswitch:api(Node, 'sofia', "xmlstatus gateway"),
    {Xml, _} = xmerl_scan:string(wh_util:to_list(Response)),
    ecallmgr_fs_xml:sofia_gateways_xml_to_json(Xml).

maybe_fix_conference_tts(Resp) ->
    Ps = wh_json:get_value(<<"Profiles">>, Resp),
    wh_json:set_value(<<"Profiles">>, wh_json:map(fun maybe_fix_profile_tts/2, Ps), Resp).

maybe_fix_profile_tts(Name, Profile) ->
    {Name, case wh_json:get_value(<<"tts-engine">>, Profile) of
               'undefined' -> Profile;
               <<"flite">> -> fix_flite_tts(Profile);
               _ -> Profile
           end}.
fix_flite_tts(Profile) ->
    Voice = wh_json:get_value(<<"tts-voice">>, Profile),
    wh_json:set_value(<<"tts-voice">>, ecallmgr_fs_flite:voice(Voice), Profile).
            
