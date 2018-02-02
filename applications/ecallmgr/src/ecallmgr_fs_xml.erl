%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz INC
%%% @doc
%%% Generate the XML for various FS responses
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_xml).

-export([build_leg_vars/1, get_leg_vars/1, get_channel_vars/1, get_channel_vars/2
        ,route_resp_xml/3 ,authn_resp_xml/1, reverse_authn_resp_xml/1
        ,acl_xml/1, not_found/0, empty_response/0
        ,sip_profiles_xml/1, sofia_gateways_xml_to_json/1
        ,sip_channel_xml/1
        ,escape/2
        ,conference_resp_xml/1
        ,event_filters_resp_xml/1
        ]).

-export([config_el/2, config_el/3]).
-export([section_el/2, section_el/3]).
-export([params_el/1, param_el/2, maybe_param_el/2]).
-export([xml_attrib/2]).

-export([action_el/1, action_el/2, action_el/3]).
-export([condition_el/1, condition_el/3]).
-export([extension_el/1, extension_el/3]).
-export([context_el/2]).
-export([variables_el/1, variable_el/2]).
-export([hunt_context/1, context/1, context/2]).

-include("ecallmgr.hrl").

-define(DEFAULT_USER_CACHE_TIME_IN_MS, ?MILLISECONDS_IN_HOUR). %% 1 hour

-spec acl_xml(kz_json:object()) -> {'ok', iolist()}.
acl_xml(AclsJObj) ->
    AclsFold = lists:foldl(fun arrange_acl_node/2, orddict:new(), kz_json:to_proplist(AclsJObj)),

    NetworkListEl = network_list_el([V || {_, V} <- orddict:to_list(AclsFold)]),

    ConfigEl = config_el(<<"acl.conf">>, <<"kazoo generated ACL lists">>, NetworkListEl),

    SectionEl = section_el(<<"configuration">>, ConfigEl),

    {'ok', xmerl:export([SectionEl], 'fs_xml')}.

-spec sip_profiles_xml(kz_json:object()) -> {'ok', iolist()}.
sip_profiles_xml(JObj) ->
    ProfilesEl = sofia_profiles_el(JObj),

    ConfigEl = config_el(<<"sofia.conf">>, ProfilesEl),

    SectionEl = section_el(<<"configuration">>, ConfigEl),

    {'ok', xmerl:export([SectionEl], 'fs_xml')}.

-spec sip_channel_xml(kz_term:proplist()) -> {'ok', iolist()}.
sip_channel_xml(Props) ->
    ParamsEl = params_el([param_el(K, V) || {K, V} <- Props]),
    ChannelEl = channel_el(props:get_value(<<"uuid">>, Props), ParamsEl),
    SectionEl = section_el(<<"channels">>, ChannelEl),
    {'ok', xmerl:export([SectionEl], 'fs_xml')}.

-spec authn_resp_xml(kz_term:api_terms()) -> {'ok', iolist()}.
authn_resp_xml([_|_]=RespProp) ->
    authn_resp_xml(props:get_value(<<"Auth-Method">>, RespProp)
                  ,kz_json:from_list(RespProp)
                  );
authn_resp_xml(JObj) ->
    DomainName = kz_json:get_value(<<"Domain-Name">>, JObj),
    UserId = kz_json:get_value(<<"User-ID">>, JObj),

    case authn_resp_xml(kz_json:get_value(<<"Auth-Method">>, JObj), JObj) of
        {'ok', []}=OK -> OK;
        {'ok', Elements} ->
            Number = kz_json:get_value([<<"Custom-SIP-Headers">>,<<"P-Kazoo-Primary-Number">>],JObj),
            Expires = ecallmgr_util:maybe_add_expires_deviation_ms(
                        kz_json:get_value(<<"Expires">>,JObj)
                       ),
            Username = kz_json:get_value(<<"Auth-Username">>, JObj, UserId),
            UserEl = user_el(user_el_props(Number, Username, Expires), Elements),
            DomainEl = domain_el(kz_json:get_value(<<"Auth-Realm">>, JObj, DomainName), UserEl),
            SectionEl = section_el(<<"directory">>, DomainEl),
            {'ok', xmerl:export([SectionEl], 'fs_xml')}
    end.

-spec authn_resp_xml(kz_term:ne_binary(), kz_json:object()) -> {'ok', kz_types:xml_els()}.
authn_resp_xml(<<"gsm">>, JObj) ->
    PassEl1 = param_el(<<"password">>, kz_json:get_value(<<"Auth-Password">>, JObj)),
    PassEl2 = param_el(<<"nonce">>, kz_json:get_value(<<"Auth-Nonce">>, JObj)),
    ParamsEl = params_el([PassEl1, PassEl2]),

    VariableEls = [variable_el(K, V) || {K, V} <- get_channel_params(JObj) ],
    VariablesEl = variables_el(VariableEls),

    HeaderEls = [header_el(K, V) || {K, V} <- get_custom_sip_headers(JObj) ],
    HeadersEl = registration_headers_el(HeaderEls),
    {'ok', [VariablesEl, ParamsEl, HeadersEl]};
authn_resp_xml(<<"password">>, JObj) ->
    PassEl = param_el(<<"password">>, kz_json:get_value(<<"Auth-Password">>, JObj)),
    ParamsEl = params_el([PassEl]),

    VariableEls = [variable_el(K, V) || {K, V} <- get_channel_params(JObj)],
    VariablesEl = variables_el(VariableEls),

    {'ok', [VariablesEl, ParamsEl]};
authn_resp_xml(<<"a1-hash">>, JObj) ->
    PassEl = param_el(<<"a1-hash">>, kz_json:get_value(<<"Auth-Password">>, JObj)),
    ParamsEl = params_el([PassEl]),

    VariableEls = [variable_el(K, V) || {K, V} <- get_channel_params(JObj)],
    VariablesEl = variables_el(VariableEls),

    {'ok', [VariablesEl, ParamsEl]};
authn_resp_xml(<<"ip">>, _JObj) ->
    empty_response();
authn_resp_xml(_Method, _JObj) ->
    lager:debug("unknown method ~s", [_Method]),
    empty_response().

-spec reverse_authn_resp_xml(kz_term:api_terms()) -> {'ok', iolist()}.
reverse_authn_resp_xml([_|_]=RespProp) ->
    reverse_authn_resp_xml(props:get_value(<<"Auth-Method">>, RespProp)
                          ,kz_json:from_list(RespProp)
                          );
reverse_authn_resp_xml(JObj) ->
    case reverse_authn_resp_xml(kz_json:get_value(<<"Auth-Method">>, JObj), JObj) of
        {'ok', []}=OK -> OK;
        {'ok', Elements} ->
            UserEl = user_el(kz_json:get_value(<<"User-ID">>, JObj), Elements),
            DomainEl = domain_el(kz_json:get_value(<<"Domain-Name">>, JObj), UserEl),
            SectionEl = section_el(<<"directory">>, DomainEl),
            {'ok', xmerl:export([SectionEl], 'fs_xml')}
    end.

-spec reverse_authn_resp_xml(kz_term:ne_binary(), kz_json:object()) ->
                                    {'ok', kz_types:xml_els()}.
reverse_authn_resp_xml(<<"password">>, JObj) ->
    UserId = kz_json:get_value(<<"User-ID">>, JObj),

    PassEl = param_el(<<"reverse-auth-pass">>, kz_json:get_value(<<"Auth-Password">>, JObj)),
    UserEl = param_el(<<"reverse-auth-user">>, kz_json:get_value(<<"Auth-Username">>, JObj, UserId)),

    ParamsEl = params_el([PassEl, UserEl]),

    VariableEls = [variable_el(K, V) || {K, V} <- get_channel_params(JObj)],
    VariablesEl = variables_el(VariableEls),

    {'ok', [VariablesEl, ParamsEl]};
reverse_authn_resp_xml(_Method, _JObj) ->
    lager:debug("unknown method ~s", [_Method]),
    empty_response().

-spec empty_response() -> {'ok', []}.
empty_response() ->
    {'ok', ""}. %"<document type=\"freeswitch/xml\"></document>").

-spec conference_resp_xml(kz_term:api_terms()) -> {'ok', iolist()}.
conference_resp_xml([_|_]=Resp) ->
    Ps = props:get_value(<<"Profiles">>, Resp, kz_json:new()),
    CCs = props:get_value(<<"Caller-Controls">>, Resp, kz_json:new()),
    As = props:get_value(<<"Advertise">>, Resp, kz_json:new()),
    CPs = props:get_value(<<"Chat-Permissions">>, Resp, kz_json:new()),

    ProfilesEl = conference_profiles_xml(Ps),
    AdvertiseEl = advertise_xml(As),
    CallerControlsEl = caller_controls_xml(CCs),
    ChatPermsEl = chat_permissions_xml(CPs),

    ConfigurationEl = config_el(<<"conference.conf">>, <<"Built by Kazoo">>
                               ,[AdvertiseEl, ProfilesEl, CallerControlsEl, ChatPermsEl]
                               ),
    SectionEl = section_el(<<"configuration">>, ConfigurationEl),
    {'ok', xmerl:export([SectionEl], 'fs_xml')};

conference_resp_xml(Resp) -> conference_resp_xml(kz_json:to_proplist(Resp)).

conference_profiles_xml(Profiles) when is_list(Profiles) ->
    ProfileEls = [conference_profile_xml(Name, Params) || {Name, Params} <- Profiles],
    profiles_el(ProfileEls);
conference_profiles_xml(Profiles) -> conference_profiles_xml(kz_json:to_proplist(Profiles)).

advertise_xml(As) when is_list(As) ->
    RoomEls = [room_el(Name, Status) || {Name, Status} <- As],
    advertise_el(RoomEls);
advertise_xml(As) -> advertise_xml(kz_json:to_proplist(As)).

caller_controls_xml(CCs) when is_list(CCs) ->
    GroupsEls = [group_xml(Name, Params) || {Name, Params} <- CCs],
    caller_controls_el(GroupsEls);
caller_controls_xml(CCs) -> caller_controls_xml(kz_json:to_proplist(CCs)).

group_xml(Name, Controls) when is_list(Controls) ->
    ControlEls = [control_el(kz_json:get_value(<<"action">>, Control)
                            ,kz_json:get_value(<<"digits">>, Control)
                            ,kz_json:get_value(<<"data">>, Control)
                            )
                  || Control <- Controls
                 ],
    group_el(Name, ControlEls);
group_xml(Name, Controls) ->
    group_xml(Name, kz_json:to_proplist(Controls)).

chat_permissions_xml(CPs) when is_list(CPs) ->
    ProfileEls = [profile_xml(Name, Users) || {Name, Users} <- CPs],
    chat_permissions_el(ProfileEls);
chat_permissions_xml(CPs) -> chat_permissions_xml(kz_json:to_proplist(CPs)).

profile_xml(Name, Users) ->
    UserEls = [chat_user_el(User, Commands) || {User, Commands} <- kz_json:to_proplist(Users)],
    profile_el(Name, UserEls).

conference_profile_xml(Name, Params) ->
    ParamEls = [param_el(K, V) || {K, V} <- kz_json:to_proplist(Params)],
    profile_el(Name, ParamEls).

-spec route_resp_xml(atom(), kz_term:api_terms(), kz_term:proplist()) -> {'ok', iolist()}.
route_resp_xml(Section, [_|_]=RespProp, Props) -> route_resp_xml(Section, kz_json:from_list(RespProp), Props);
route_resp_xml(Section, RespJObj, Props) ->
    route_resp_xml(kz_json:get_value(<<"Method">>, RespJObj)
                  ,kz_json:get_value(<<"Routes">>, RespJObj, [])
                  ,kz_json:set_value(<<"Fetch-Section">>, kz_term:to_binary(Section), RespJObj)
                  ,Props
                  ).

%% Prop = Route Response
-type route_resp_fold_acc() :: {pos_integer(), kz_types:xml_els()}.

-spec route_resp_fold(kz_json:object(), route_resp_fold_acc()) ->
                             route_resp_fold_acc().
route_resp_fold(RouteJObj, {Idx, Acc}) ->
    case ecallmgr_util:build_channel(RouteJObj) of
        {'error', _} -> {Idx+1, Acc};
        {'ok', Channel} ->
            route_resp_fold(RouteJObj, {Idx, Acc}, Channel)
    end.

-spec route_resp_fold(kz_json:object(), route_resp_fold_acc(), kz_term:ne_binary()) ->
                             route_resp_fold_acc().
route_resp_fold(RouteJObj, {Idx, Acc}, Channel) ->
    RouteJObj1 =
        case kz_json:get_value(<<"Progress-Timeout">>, RouteJObj) of
            'undefined' ->
                kz_json:set_value(<<"Progress-Timeout">>, <<"6">>, RouteJObj);
            I when is_integer(I) ->
                kz_json:set_value(<<"Progress-Timeout">>, integer_to_list(I), RouteJObj);
            _ -> RouteJObj
        end,

    ChannelVars = get_channel_vars(kz_json:to_proplist(RouteJObj1)),

    BPEl = action_el(<<"set">>, [<<"bypass_media=">>, should_bypass_media(RouteJObj)]),
    HangupEl = action_el(<<"set">>, <<"hangup_after_bridge=true">>),
    FailureEl = action_el(<<"set">>, <<"failure_causes=NORMAL_CLEARING,ORIGINATOR_CANCEL,CRASH">>),
    BridgeEl = action_el(<<"bridge">>, [ChannelVars, Channel]),

    ConditionEl = condition_el([BPEl, HangupEl, FailureEl, BridgeEl]),
    ExtEl = extension_el([<<"match_">>, Idx+$0], <<"true">>, [ConditionEl]),

    {Idx+1, [ExtEl | Acc]}.

-spec should_bypass_media(kz_json:object()) -> string().
should_bypass_media(RouteJObj) ->
    case kz_json:get_value(<<"Media">>, RouteJObj) of
        <<"bypass">> -> "true";
        _ -> "false" %% default to not bypassing media
    end.

-spec route_resp_xml(kz_term:ne_binary(), kz_json:objects(), kz_json:object(), kz_term:proplist()) -> {'ok', iolist()}.
route_resp_xml(<<"bridge">>, Routes, JObj, Props) ->
    lager:debug("creating a bridge XML response"),
    LogEl = route_resp_log_winning_node(),
    RingbackEl = route_resp_ringback(JObj),
    TransferEl = route_resp_transfer_ringback(JObj),
    %% format the Route based on protocol
    {_Idx, Extensions} = lists:foldr(fun route_resp_fold/2, {1, []}, Routes),
    FailRespondEl = action_el(<<"respond">>, <<"${bridge_hangup_cause}">>),
    FailConditionEl = condition_el(FailRespondEl),
    FailExtEl = extension_el(<<"failed_bridge">>, <<"false">>, [FailConditionEl]),
    Context = hunt_context(Props),
    ContextEl = context_el(Context, [LogEl, RingbackEl, TransferEl] ++ unset_custom_sip_headers(Props) ++ Extensions ++ [FailExtEl]),
    SectionEl = section_el(<<"dialplan">>, <<"Route Bridge Response">>, ContextEl),
    {'ok', xmerl:export([SectionEl], 'fs_xml')};

route_resp_xml(<<"park">>, _Routes, JObj, Props) ->
    Exten = [route_resp_log_winning_node()
            ,route_resp_set_winning_node()
            ,route_resp_bridge_id()
            ,route_resp_ringback(JObj)
            ,route_resp_transfer_ringback(JObj)
            ,route_resp_pre_park_action(JObj)
            ,maybe_start_dtmf_action(Props)
             | route_resp_ccvs(JObj)
             ++ route_resp_cavs(JObj)
             ++ unset_custom_sip_headers(Props)
             ++ [action_el(<<"park">>)]
            ],
    ParkExtEl = extension_el(<<"park">>, 'undefined', [condition_el(Exten)]),
    Context = kz_json:get_value(<<"Context">>, JObj, ?DEFAULT_FREESWITCH_CONTEXT),
    ContextEl = context_el(Context, [ParkExtEl]),
    SectionEl = section_el(<<"dialplan">>, <<"Route Park Response">>, ContextEl),
    {'ok', xmerl:export([SectionEl], 'fs_xml')};

route_resp_xml(<<"error">>, _Routes, JObj, Props) ->
    Section = kz_json:get_value(<<"Fetch-Section">>, JObj, <<"dialplan">>),
    route_resp_xml(<<Section/binary, "_error">>,_Routes, JObj, Props);

route_resp_xml(<<"dialplan_error">>, _Routes, JObj, Props) ->
    ErrCode = kz_json:get_value(<<"Route-Error-Code">>, JObj),
    ErrMsg = [" ", kz_json:get_value(<<"Route-Error-Message">>, JObj, <<>>)],
    Exten = [route_resp_log_winning_node()
            ,route_resp_set_winning_node()
            ,route_resp_bridge_id()
            ,route_resp_ringback(JObj)
            ,route_resp_transfer_ringback(JObj)
            ,action_el(<<"respond">>, [ErrCode, ErrMsg])
            ],
    ErrExtEl = extension_el([condition_el(Exten)]),
    Context = kz_json:get_value(<<"Context">>, JObj, hunt_context(Props)),
    ContextEl = context_el(Context, [ErrExtEl]),
    SectionEl = section_el(<<"dialplan">>, <<"Route Error Response">>, ContextEl),
    {'ok', xmerl:export([SectionEl], 'fs_xml')};

route_resp_xml(<<"chatplan_error">>, _Routes, JObj, _Props) ->
    ErrCode = kz_json:get_value(<<"Route-Error-Code">>, JObj),
    ErrMsg = [" ", kz_json:get_value(<<"Route-Error-Message">>, JObj, <<>>)],
    Exten = [action_el(<<"reply">>, [ErrCode, ErrMsg])],
    ErrExtEl = extension_el([condition_el(Exten)]),
    ContextEl = context_el(?DEFAULT_FREESWITCH_CONTEXT, [ErrExtEl]),
    SectionEl = section_el(<<"chatplan">>, <<"Route Error Response">>, ContextEl),
    {'ok', xmerl:export([SectionEl], 'fs_xml')};

route_resp_xml(<<"sms">>, _Routes, _JObj, Props) ->
    lager:debug("creating a chatplan XML response"),
    StopActionEl = action_el(<<"stop">>, <<"stored">>),
    StopExtEl = extension_el(<<"chat plan">>, <<"false">>, [condition_el([StopActionEl])]),
    Context = hunt_context(Props),
    ContextEl = context_el(Context, [StopExtEl]),
    SectionEl = section_el(<<"chatplan">>, <<"Chat Response">>, ContextEl),
    {'ok', xmerl:export([SectionEl], 'fs_xml')};

route_resp_xml(<<"sms_error">>, _Routes, JObj, _Props) ->
    ErrCode = kz_json:get_value(<<"Route-Error-Code">>, JObj),
    ErrMsg = [" ", kz_json:get_value(<<"Route-Error-Message">>, JObj, <<>>)],
    Exten = [route_resp_log_winning_node()
            ,route_resp_set_winning_node()
            ,action_el(<<"respond">>, [ErrCode, ErrMsg])
            ],
    ErrExtEl = extension_el([condition_el(Exten)]),
    ContextEl = context_el(?DEFAULT_FREESWITCH_CONTEXT, [ErrExtEl]),
    SectionEl = section_el(<<"chatplan">>, <<"Route Error Response">>, ContextEl),
    {'ok', xmerl:export([SectionEl], 'fs_xml')};

route_resp_xml(Method, Routes, JObj, Props) ->
    lager:debug("trying fun for ~p", [Method]),
    Fun = props:get_value(<<"Route-Resp-Xml-Fun">>, Props),
    maybe_route_resp_xml_fun(Fun, Method, Routes, JObj, Props).

maybe_route_resp_xml_fun(Fun, Method, Routes, JObj, Props)
  when is_function(Fun, 4) ->
    Fun(Method, Routes, JObj, Props);
maybe_route_resp_xml_fun(_Fun, Method, Routes, JObj, Props) ->
    lager:error("route resp xml method ~p not handled, reverting to error", [Method]),
    route_resp_xml(<<"error">>, Routes, JObj, Props).

route_resp_bridge_id() ->
    Action = action_el(<<"export">>, [?SET_CCV(<<"Bridge-ID">>, <<"${UUID}">>)], 'true'),
    condition_el(Action, ?GET_CCV(<<"Bridge-ID">>), <<"^$">>).

-spec unset_custom_sip_headers(kz_term:proplist()) -> kz_types:xml_els().
unset_custom_sip_headers(Props) ->
    case get_custom_sip_headers(Props) of
        [] -> [];
        [{K,_}] -> [action_el(<<"unset">>, K)];
        [{K1, _} | KVs] ->
            Keys = ["^^;", K1] ++ [<<";", K/binary>> || {K, _} <- KVs],
            [action_el(<<"multiunset">>, list_to_binary(Keys))]
    end.

-spec not_found() -> {'ok', iolist()}.
not_found() ->
    ResultEl = result_el(<<"not found">>),
    SectionEl = section_el(<<"result">>, <<"Route Not Found">>, ResultEl),
    {'ok', xmerl:export([SectionEl], 'fs_xml')}.

-spec route_resp_log_winning_node() -> kz_types:xml_el().
route_resp_log_winning_node() ->
    action_el(<<"log">>, [<<"NOTICE log|${uuid}|", (kz_term:to_binary(node()))/binary, " won call control">>]).

route_resp_set_winning_node() ->
    action_el(<<"export">>, [?SET_CCV(<<"Ecallmgr-Node">>, (kz_term:to_binary(node())))]).

-spec route_resp_ringback(kz_json:object()) -> kz_types:xml_el().
route_resp_ringback(JObj) ->
    case kz_json:get_value(<<"Ringback-Media">>, JObj) of
        'undefined' ->
            {'ok', RBSetting} = ecallmgr_util:get_setting(<<"default_ringback">>),
            action_el(<<"set">>, <<"ringback=", (kz_term:to_binary(RBSetting))/binary>>);
        Media ->
            MsgId = kz_json:get_value(<<"Msg-ID">>, JObj),
            Stream = ecallmgr_util:media_path(Media, 'extant', MsgId, JObj),
            action_el(<<"set">>, <<"ringback=", (kz_term:to_binary(Stream))/binary>>)
    end.

-spec route_resp_ccvs(kz_json:object()) -> kz_types:xml_els().
route_resp_ccvs(JObj) ->
    case kz_json:get_json_value(<<"Custom-Channel-Vars">>, JObj) of
        'undefined' -> [];
        CCVs -> [action_el(<<"kz_multiset">>, route_ccvs_list(kz_json:to_proplist(CCVs)))]
    end.

-spec route_resp_cavs(kz_json:object()) -> kz_types:xml_els().
route_resp_cavs(JObj) ->
    case kz_json:get_json_value(<<"Custom-Application-Vars">>, JObj) of
        'undefined' -> [];
        CAVs -> [action_el(<<"kz_multiset">>, route_cavs_list(kz_json:to_proplist(CAVs)))]
    end.

-spec route_ccvs_list(kz_term:proplist()) -> kz_term:ne_binary().
route_ccvs_list(CCVs) ->
    L = [kz_term:to_list(ecallmgr_util:get_fs_kv(K, V))
         || {K, V} <- CCVs
        ],
    <<"^^|", (kz_term:to_binary(string:join(L, "|")))/binary>>.

-spec route_cavs_list(kz_term:proplist()) -> kz_term:ne_binary().
route_cavs_list(CAVs) ->
    L = [kz_term:to_list(ecallmgr_util:get_fs_kv(?CAV(K), V))
         || {K, V} <- CAVs
        ],
    <<"^^|", (kz_term:to_binary(string:join(L, "|")))/binary>>.

-spec route_resp_transfer_ringback(kz_json:object()) -> kz_types:xml_el().
route_resp_transfer_ringback(JObj) ->
    case kz_json:get_value(<<"Transfer-Media">>, JObj) of
        'undefined' ->
            {'ok', RBSetting} = ecallmgr_util:get_setting(<<"default_ringback">>),
            action_el(<<"set">>, <<"transfer_ringback=", (kz_term:to_binary(RBSetting))/binary>>);
        Media ->
            MsgId = kz_json:get_value(<<"Msg-ID">>, JObj),
            Stream = ecallmgr_util:media_path(Media, 'extant', MsgId, JObj),
            action_el(<<"set">>, <<"transfer_ringback=", (kz_term:to_binary(Stream))/binary>>)
    end.

-spec route_resp_pre_park_action(kz_json:object()) -> 'undefined' | kz_types:xml_el().
route_resp_pre_park_action(JObj) ->
    case kz_json:get_value(<<"Pre-Park">>, JObj) of
        <<"ring_ready">> -> action_el(<<"ring_ready">>);
        <<"answer">> -> action_el(<<"answer">>);
        _Else -> 'undefined'
    end.

-spec maybe_start_dtmf_action(kz_term:proplist()) -> 'undefined' | kz_types:xml_el().
maybe_start_dtmf_action(Props) ->
    case ecallmgr_config:is_true(<<"should_detect_inband_dtmf">>) of
        'false' -> 'undefined';
        'true' -> check_dtmf_type(Props)
    end.

-spec check_dtmf_type(kz_term:proplist()) -> 'undefined' | kz_types:xml_el().
check_dtmf_type(Props) ->
    case props:get_value(<<"variable_switch_r_sdp">>, Props, <<"101 telephone-event">>) of
        <<"101 telephone-event">> -> 'undefined';
        _ -> action_el(<<"start_dtmf">>)
    end.

-spec build_leg_vars(kz_json:object() | kz_term:proplist()) -> kz_term:ne_binaries().
build_leg_vars([]) -> [];
build_leg_vars([_|_]=Prop) -> lists:foldr(fun get_channel_vars/2, [], Prop);
build_leg_vars(JObj) -> build_leg_vars(kz_json:to_proplist(JObj)).

-spec get_leg_vars(kz_json:object() | kz_term:proplist()) -> iolist().
get_leg_vars([]) -> [];
get_leg_vars([Binary|_]=Binaries)
  when is_binary(Binary) ->
    ["[^^", ?BRIDGE_CHANNEL_VAR_SEPARATOR
    ,string:join([kz_term:to_list(V) || V <- Binaries]
                ,?BRIDGE_CHANNEL_VAR_SEPARATOR
                )
    ,"]"
    ];
get_leg_vars([_|_]=Prop) ->
    ["[^^", ?BRIDGE_CHANNEL_VAR_SEPARATOR
    ,string:join([kz_term:to_list(V)
                  || V <- lists:foldr(fun get_channel_vars/2, [], Prop)]
                ,?BRIDGE_CHANNEL_VAR_SEPARATOR
                )
    ,"]"
    ];
get_leg_vars(JObj) -> get_leg_vars(kz_json:to_proplist(JObj)).

-spec get_channel_vars(kz_json:object() | kz_term:proplist()) -> iolist().
get_channel_vars([]) -> [];
get_channel_vars([_|_]=Prop) ->
    P = Prop ++ [{<<"Overwrite-Channel-Vars">>, <<"true">>}],
    ["{", string:join([kz_term:to_list(V) || V <- lists:foldr(fun get_channel_vars/2, [], P)], ","), "}"];
get_channel_vars(JObj) -> get_channel_vars(kz_json:to_proplist(JObj)).

-spec get_channel_vars({binary(), binary() | kz_json:object()}, kz_term:ne_binaries()) -> iolist().
get_channel_vars({<<"Custom-Channel-Vars">>, JObj}, Vars) ->
    kz_json:foldl(fun get_channel_vars_fold/3, Vars, JObj);

get_channel_vars({<<"Custom-Application-Vars">>, JObj}, Vars) ->
    kz_json:foldl(fun get_application_vars_fold/3, Vars, JObj);

get_channel_vars({<<"Custom-SIP-Headers">>, SIPJObj}, Vars) ->
    kz_json:foldl(fun sip_headers_fold/3, Vars, SIPJObj);

get_channel_vars({<<"To-User">>, Username}, Vars) ->
    [list_to_binary([?CHANNEL_VAR_PREFIX, "Username"
                    ,"='", kz_term:to_list(Username), "'"
                    ])
     | Vars
    ];
get_channel_vars({<<"To-Realm">>, Realm}, Vars) ->
    [list_to_binary([?CHANNEL_VAR_PREFIX, "Realm"
                    ,"='", kz_term:to_list(Realm), "'"
                    ])
     | Vars
    ];
get_channel_vars({<<"To-URI">>, ToURI}, Vars) ->
    [<<"sip_invite_to_uri=<", ToURI/binary, ">">>
         | Vars
    ];

get_channel_vars({<<"Caller-ID-Type">>, <<"from">>}, Vars) ->
    [ <<"sip_cid_type=none">> | Vars];
get_channel_vars({<<"Caller-ID-Type">>, <<"rpid">>}, Vars) ->
    [ <<"sip_cid_type=rpid">> | Vars];
get_channel_vars({<<"Caller-ID-Type">>, <<"pid">>}, Vars) ->
    [ <<"sip_cid_type=pid">> | Vars];

get_channel_vars({<<"origination_uuid">> = K, UUID}, Vars) ->
    [ <<K/binary, "=", UUID/binary>> | Vars];

get_channel_vars({<<"Hold-Media">>, Media}, Vars) ->
    [list_to_binary(["hold_music="
                    ,kz_term:to_list(ecallmgr_util:media_path(Media, 'extant', get('callid'), kz_json:new()))
                    ])
     | Vars];

get_channel_vars({<<"Codecs">>, []}, Vars) ->
    Vars;
get_channel_vars({<<"Codecs">>, Cs}, Vars) ->
    Codecs = [kz_term:to_list(codec_mappings(C))
              || C <- Cs,
                 not kz_term:is_empty(C)
             ],
    CodecStr = string:join(Codecs, ":"),
    [list_to_binary(["absolute_codec_string='^^:", CodecStr, "'"])
     |Vars
    ];

%% SPECIAL CASE: Timeout must be larger than zero
get_channel_vars({<<"Timeout">>, V}, Vars) ->
    case kz_term:to_integer(V) of
        TO when TO > 0 ->
            [<<"call_timeout=", (kz_term:to_binary(TO))/binary>>
            ,<<"originate_timeout=", (kz_term:to_binary(TO))/binary>>
                 | Vars
            ];
        _Else -> Vars
    end;

get_channel_vars({<<"Forward-IP">>, <<"sip:", _/binary>>=V}, Vars) ->
    [ list_to_binary(["sip_route_uri='", V, "'"]) | Vars];

get_channel_vars({<<"Forward-IP">>, V}, Vars) ->
    get_channel_vars({<<"Forward-IP">>, <<"sip:", V/binary>>}, Vars);

get_channel_vars({<<"Enable-T38-Gateway">>, Direction}, Vars) ->
    [<<"execute_on_answer='t38_gateway ", Direction/binary, "'">> | Vars];

get_channel_vars({<<"Confirm-File">>, V}, Vars) ->
    [list_to_binary(["group_confirm_file='"
                    ,kz_term:to_list(ecallmgr_util:media_path(V, 'extant', get('callid'), kz_json:new()))
                    ,"'"
                    ]) | Vars];

get_channel_vars({<<"SIP-Invite-Parameters">>, V}, Vars) ->
    [list_to_binary(["sip_invite_params='", kz_util:iolist_join(<<";">>, V), "'"]) | Vars];

get_channel_vars({AMQPHeader, V}, Vars) when not is_list(V) ->
    case lists:keyfind(AMQPHeader, 1, ?SPECIAL_CHANNEL_VARS) of
        'false' -> Vars;
        {_, Prefix} ->
            Val = ecallmgr_util:maybe_sanitize_fs_value(AMQPHeader, V),
            [encode_fs_val(Prefix, Val) | Vars]
    end;
get_channel_vars(_, Vars) -> Vars.

-spec sip_headers_fold(kz_json:path(), kz_json:json_term(), iolist()) -> iolist().
sip_headers_fold(<<"Diversions">>, Vs, Vars0) ->
    diversion_headers_fold(Vs, Vars0);
sip_headers_fold(K, V, Vars0) ->
    [list_to_binary(["sip_h_", K, "=", maybe_expand_macro(kz_term:to_binary(V))]) | Vars0].

-define(DEFAULT_EXPANDABLE_MACROS
       ,kz_json:from_list([{<<"{caller_id_name}">>, <<"${caller_id_name}">>}
                          ,{<<"{caller_id_number}">>, <<"${caller_id_number}">>}
                          ,{<<"{account_id}">>, <<"${" ?CHANNEL_VAR_PREFIX "Account-ID}">>}
                          ,{<<"{reseller_id}">>, <<"${" ?CHANNEL_VAR_PREFIX "Reseller-ID}">>}
                          ,{<<"{billing_id}">>, <<"${" ?CHANNEL_VAR_PREFIX "Billing-ID}">>}
                          ])).
-define(EXPANDABLE_MACROS, ecallmgr_config:get_json(<<"expandable_macros">>, ?DEFAULT_EXPANDABLE_MACROS)).

-spec maybe_expand_macro(kz_term:ne_binary()) -> kz_term:ne_binary().
maybe_expand_macro(HeaderValue) ->
    kz_json:get_ne_binary_value(HeaderValue, ?EXPANDABLE_MACROS, HeaderValue).

-spec diversion_headers_fold(kz_term:ne_binaries(), iolist()) -> iolist().
diversion_headers_fold(Vs, Vars0) ->
    lists:foldl(fun diversion_header_fold/2, Vars0, Vs).

-spec diversion_header_fold(kz_term:ne_binary(), iolist()) -> iolist().
diversion_header_fold(<<_/binary>> = V, Vars0) ->
    lager:debug("setting diversion ~s on the channel", [V]),
    [list_to_binary(["sip_h_Diversion=", V]) | Vars0].

-spec get_channel_vars_fold(kz_json:path(), kz_json:json_term(), iolist()) -> iolist().
get_channel_vars_fold(<<"Force-Fax">>, Direction, Acc) ->
    [<<"execute_on_answer='t38_gateway ", Direction/binary, "'">>|Acc];
get_channel_vars_fold(<<"Channel-Actions">>, Actions, Acc) ->
    [Actions |Acc];
get_channel_vars_fold(K, V, Acc) ->
    case lists:keyfind(K, 1, ?SPECIAL_CHANNEL_VARS) of
        'false' ->
            [list_to_binary([?CHANNEL_VAR_PREFIX, kz_term:to_list(K)
                            ,"='", kz_term:to_list(V), "'"])
             | Acc];
        {_, <<"group_confirm_file">>} ->
            [list_to_binary(["group_confirm_file='"
                            ,kz_term:to_list(ecallmgr_util:media_path(V, 'extant', get('callid'), kz_json:new()))
                            ,"'"
                            ])
             | Acc];
        {_, Prefix} ->
            Val = ecallmgr_util:maybe_sanitize_fs_value(K, V),
            [encode_fs_val(Prefix, Val) | Acc]
    end.

-spec get_application_vars_fold(kz_json:key(), kz_json:json_term(), iolist()) -> iolist().
get_application_vars_fold(K, V, Acc) ->
    [list_to_binary([?APPLICATION_VAR_PREFIX, kz_term:to_list(K), "='", kz_term:to_list(V), "'"])
     | Acc
    ].

-spec codec_mappings(kz_term:ne_binary()) -> kz_term:ne_binary().
codec_mappings(<<"G722_32">>) ->
    <<"G7221@32000h">>;
codec_mappings(<<"G722_16">>) ->
    <<"G722:G7221@16000h">>;
codec_mappings(<<"CELT_32">>) ->
    <<"CELT@32000h">>;
codec_mappings(<<"CELT_48">>) ->
    <<"CELT@48000h">>;
codec_mappings(Codec) ->
    Codec.

encode_fs_val(Prefix, V) ->
    list_to_binary([Prefix, "='", escape(V, $\'), "'"]).

-spec escape(kz_term:text(), char()) -> kz_term:ne_binary().
escape(V, C) ->
    iolist_to_binary([encode(A, C) || <<A>> <= kz_term:to_binary(V)]).
encode(C, C) -> [$\\, C];
encode(C, _) -> C.

-spec get_channel_params(kz_json:object() | kz_term:proplist()) -> kz_term:proplist().
get_channel_params(Props) when is_list(Props) ->
    [get_channel_params_fold(K, V) || {K, V} <- Props];
get_channel_params(JObj) ->
    get_channel_params(
      kz_json:to_proplist(
        kz_json:get_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new())
       )).

-spec get_channel_params_fold(kz_term:ne_binary(), kz_term:ne_binary()) ->
                                     {kz_term:ne_binary(), kz_term:ne_binary()}.
get_channel_params_fold(Key, Val) ->
    case lists:keyfind(Key, 1, ?SPECIAL_CHANNEL_VARS) of
        'false' ->
            {list_to_binary([?CHANNEL_VAR_PREFIX, Key]), Val};
        {_Key, Prefix} ->
            {Prefix, ecallmgr_util:maybe_sanitize_fs_value(Key, Val)}
    end.

-spec get_custom_sip_headers(kz_json:object() | kz_term:proplist()) -> kz_json:json_proplist().
get_custom_sip_headers([_|_]=Props) ->
    [normalize_custom_sip_header_name(P) || P <- props:filter(fun is_custom_sip_header/1, Props)];
get_custom_sip_headers(JObj) ->
    kz_json:to_proplist(kz_json:get_value(<<"Custom-SIP-Headers">>, JObj, kz_json:new())).

-spec normalize_custom_sip_header_name(any()) -> any().
normalize_custom_sip_header_name({<<"variable_", K/binary>>, V}) -> {K, V};
normalize_custom_sip_header_name(A) -> A.

-spec is_custom_sip_header(any()) -> boolean().
is_custom_sip_header({<<"variable_sip_h_X-", _/binary>>, _}) -> 'true';
is_custom_sip_header(_) -> 'false'.

-spec arrange_acl_node({kz_term:ne_binary(), kz_json:object()}, orddict:orddict()) -> orddict:orddict().
arrange_acl_node({_, JObj}, Dict) ->
    AclList = kz_json:get_value(<<"network-list-name">>, JObj),

    NodeEl = acl_node_el(kz_json:get_value(<<"type">>, JObj), kz_json:get_value(<<"cidr">>, JObj)),

    case orddict:find(AclList, Dict) of
        {'ok', ListEl} ->
            lager:debug("found existing list ~s", [AclList]),
            orddict:store(AclList, prepend_child(ListEl, NodeEl), Dict);
        'error' ->
            lager:debug("creating new list xml for ~s", [AclList]),
            orddict:store(AclList, prepend_child(acl_list_el(AclList), NodeEl), Dict)
    end.

-spec hunt_context(kz_term:proplist()) -> kz_term:api_binary().
hunt_context(Props) ->
    props:get_value(<<"Hunt-Context">>, Props, ?DEFAULT_FREESWITCH_CONTEXT).

-spec context(kz_json:object()) -> kz_term:api_binary().
context(JObj) ->
    kz_json:get_value(<<"Context">>, JObj, ?DEFAULT_FREESWITCH_CONTEXT).

-spec context(kz_json:object(), kz_term:proplist()) -> kz_term:api_binary().
context(JObj, Props) ->
    kz_json:get_value(<<"Context">>, JObj, hunt_context(Props)).

%%%-------------------------------------------------------------------
%% XML record creators and helpers
%%%-------------------------------------------------------------------
-spec acl_node_el(kz_types:xml_attrib_value(), kz_types:xml_attrib_value()) -> kz_types:xml_el().
acl_node_el(Type, CIDR) ->
    #xmlElement{name='node'
               ,attributes=[xml_attrib('type', Type)
                           ,xml_attrib('cidr', CIDR)
                           ]
               }.

-spec acl_list_el(kz_types:xml_attrib_value()) -> kz_types:xml_el().
acl_list_el(Name) ->
    acl_list_el(Name, <<"deny">>).

-spec acl_list_el(kz_types:xml_attrib_value(), kz_types:xml_attrib_value()) -> kz_types:xml_el().
acl_list_el(Name, Default) ->
    acl_list_el(Name, Default, []).

-spec acl_list_el(kz_types:xml_attrib_value(), kz_types:xml_attrib_value(), kz_types:xml_els()) -> kz_types:xml_el().
acl_list_el(Name, Default, Children) ->
    #xmlElement{name='list'
               ,attributes=[xml_attrib('name', Name)
                           ,xml_attrib('default', Default)
                           ]
               ,content=Children
               }.

-spec network_list_el(kz_types:xml_els()) -> kz_types:xml_el().
network_list_el(ListsEls) ->
    #xmlElement{name='network-lists', content=ListsEls}.

-spec config_el(kz_term:ne_binary(), kz_types:xml_el() | kz_types:xml_els()) -> kz_types:xml_el() | kz_types:xml_els().
config_el(Name, Content) ->
    config_el(Name, <<"configuration ", (kz_term:to_binary(Name))/binary, " built by kazoo">>, Content).

-spec config_el(kz_term:ne_binary(), kz_term:ne_binary(), kz_types:xml_el() | kz_types:xml_els()) -> kz_types:xml_el() | kz_types:xml_els().
config_el(Name, Desc, #xmlElement{}=Content) ->
    config_el(Name, Desc, [Content]);
config_el(Name, Desc, Content) ->
    #xmlElement{name='configuration'
               ,attributes=[xml_attrib('name', Name)
                           ,xml_attrib('description', Desc)
                           ]
               ,content=Content
               }.

-spec channel_el(kz_term:api_binary(), kz_types:xml_el() | kz_types:xml_els()) -> kz_types:xml_el() | kz_types:xml_els().
channel_el('undefined', Content) -> Content;
channel_el(UUID, Content) ->
    channel_el(UUID, <<"channel ", (kz_term:to_binary(UUID))/binary, " tracked by kazoo">>, Content).

-spec channel_el(kz_term:ne_binary(), kz_term:ne_binary(), kz_types:xml_el() | kz_types:xml_els()) -> kz_types:xml_el().
channel_el(UUID, Desc, #xmlElement{}=Content) ->
    channel_el(UUID, Desc, [Content]);
channel_el(UUID, Desc, Content) ->
    #xmlElement{name='channel'
               ,attributes=[xml_attrib('uuid', UUID)
                           ,xml_attrib('description', Desc)
                           ]
               ,content=Content
               }.

-spec section_el(kz_types:xml_attrib_value(), kz_types:xml_el() | kz_types:xml_els()) -> kz_types:xml_el().
section_el(Name, #xmlElement{}=Content) ->
    section_el(Name, [Content]);
section_el(Name, Content) ->
    #xmlElement{name='section'
               ,attributes=[xml_attrib('name', Name)]
               ,content=Content
               }.

-spec section_el(kz_types:xml_attrib_value(), kz_types:xml_attrib_value(), kz_types:xml_el() | kz_types:xml_els()) -> kz_types:xml_el().
section_el(Name, Desc, #xmlElement{}=Content) ->
    section_el(Name, Desc, [Content]);
section_el(Name, Desc, Content) ->
    #xmlElement{name='section'
               ,attributes=[xml_attrib('name', Name)
                           ,xml_attrib('description', Desc)
                           ]
               ,content=Content
               }.

-spec domain_el(kz_types:xml_attrib_value(), kz_types:xml_el() | kz_types:xml_els()) -> kz_types:xml_el().
domain_el(Name, Child) when not is_list(Child) ->
    domain_el(Name, [Child]);
domain_el(Name, Children) ->
    #xmlElement{name='domain'
               ,attributes=[xml_attrib('name', Name)]
               ,content=Children
               }.

-spec user_el(kz_types:xml_attrib_value() | kz_term:proplist(), kz_types:xml_els()) -> kz_types:xml_el().
user_el(Id, Children) when not is_list(Id) ->
    user_el(user_el_default_props(Id), Children);
user_el(Props, Children) ->
    #xmlElement{name='user'
               ,attributes=[xml_attrib(K, V)
                            || {K, V} <- props:unique(
                                           props:filter_undefined(Props)
                                          )
                           ]
               ,content=Children
               }.

-spec user_el_props(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_integer()) -> kz_term:proplist().
user_el_props(Number, Username, 'undefined') ->
    [{'number-alias', Number}
    ,{'cacheable', ecallmgr_config:get_integer(<<"user_cache_time_in_ms">>
                                              ,?DEFAULT_USER_CACHE_TIME_IN_MS
                                              )
     }
     | user_el_default_props(Username)
    ];
user_el_props(Number, Username, Expires) when Expires < 1 ->
    [{'number-alias', Number}
     | user_el_default_props(Username)
    ];
user_el_props(Number, Username, Expires) ->
    [{'number-alias', Number}
    ,{'cacheable', Expires}
     | user_el_default_props(Username)
    ].

-spec user_el_default_props(kz_types:xml_attrib_value()) -> kz_term:proplist().
user_el_default_props(Id) ->
    [{'id', Id}].

-spec chat_user_el(kz_types:xml_attrib_value(), kz_types:xml_attrib_value()) -> kz_types:xml_el().
chat_user_el(Name, Commands) ->
    #xmlElement{name='user'
               ,attributes=[xml_attrib('name', Name)
                           ,xml_attrib('commands', Commands)
                           ]
               }.

-spec params_el(kz_types:xml_els()) -> kz_types:xml_el().
params_el(Children) ->
    #xmlElement{name='params'
               ,content=Children
               }.

-spec param_el(kz_types:xml_attrib_value(), kz_types:xml_attrib_value()) -> kz_types:xml_el().
param_el(<<"moh-sound">> = Name, MediaName) ->
    Value = ecallmgr_util:media_path(MediaName, kz_util:get_callid(), kz_json:new()),
    #xmlElement{name='param'
               ,attributes=[xml_attrib('name', Name)
                           ,xml_attrib('value', Value)
                           ]
               };
param_el(<<"max-members-sound">> = Name, MediaName) ->
    Value = ecallmgr_util:media_path(MediaName, kz_util:get_callid(), kz_json:new()),
    #xmlElement{name='param'
               ,attributes=[xml_attrib('name', Name)
                           ,xml_attrib('value', Value)
                           ]
               };
param_el(Name, Value) ->
    #xmlElement{name='param'
               ,attributes=[xml_attrib('name', Name)
                           ,xml_attrib('value', Value)
                           ]
               }.

-spec maybe_param_el(kz_types:xml_attrib_value(), kz_types:xml_attrib_value()) -> kz_types:xml_el() | 'undefined'.
maybe_param_el(Name, Value) ->
    case kz_term:is_empty(Value) of
        'true' -> 'undefined';
        'false' -> param_el(Name, Value)
    end.

profile_el(Name, Children) ->
    #xmlElement{name='profile'
               ,content=Children
               ,attributes=[xml_attrib('name', Name)]
               }.

profiles_el(Children) ->
    #xmlElement{name='profiles'
               ,content=Children
               }.

group_el(Name, Children) ->
    #xmlElement{name='group'
               ,content=Children
               ,attributes=[xml_attrib('name', Name)]
               }.

control_el(Action, Digits) ->
    #xmlElement{name='control'
               ,attributes=[xml_attrib('action', Action)
                           ,xml_attrib('digits', Digits)
                           ]
               }.

control_el(Action, Digits, 'undefined') -> control_el(Action, Digits);
control_el(Action, Digits, Data) ->
    #xmlElement{name='control'
               ,attributes=[xml_attrib('action', Action)
                           ,xml_attrib('digits', Digits)
                           ,xml_attrib('data', Data)
                           ]
               }.

advertise_el(Rooms) ->
    #xmlElement{name='advertise'
               ,content=Rooms
               }.

caller_controls_el(Groups) ->
    #xmlElement{name='caller-controls'
               ,content=Groups
               }.

chat_permissions_el(Profiles) ->
    #xmlElement{name='chat-permissions'
               ,content=Profiles
               }.

-spec variables_el(kz_types:xml_els()) -> kz_types:xml_el().
variables_el(Children) ->
    #xmlElement{name='variables'
               ,content=Children
               }.

-spec variable_el(kz_types:xml_attrib_value(), kz_types:xml_attrib_value()) -> kz_types:xml_el().
variable_el(Name, Value) ->
    #xmlElement{name='variable'
               ,attributes=[xml_attrib('name', Name)
                           ,xml_attrib('value', Value)
                           ]
               }.

-spec registration_headers_el(kz_types:xml_els()) -> kz_types:xml_el().
registration_headers_el(Children) ->
    #xmlElement{name='registration-headers'
               ,content=Children
               }.

-spec header_el(kz_types:xml_attrib_value(), kz_types:xml_attrib_value()) -> kz_types:xml_el().
header_el(Name, Value) ->
    #xmlElement{name='header'
               ,attributes=[xml_attrib('name', Name)
                           ,xml_attrib('value', Value)
                           ]
               }.

-spec context_el(kz_types:xml_attrib_value(), kz_types:xml_els()) -> kz_types:xml_el().
context_el(Name, Children) ->
    #xmlElement{name='context'
               ,attributes=[xml_attrib('name', Name)]
               ,content=Children
               }.

-spec extension_el(kz_types:xml_els()) -> kz_types:xml_el().
extension_el(Children) ->
    #xmlElement{name='extension'
               ,content=Children
               }.

-spec extension_el(kz_types:xml_attrib_value(), kz_types:xml_attrib_value() | 'undefined', kz_types:xml_els()) -> kz_types:xml_el().
extension_el(Name, 'undefined', Children) ->
    #xmlElement{name='extension'
               ,attributes=[xml_attrib('name', Name)]
               ,content=[Child || Child <- Children, Child =/= 'undefined']
               };
extension_el(Name, Continue, Children) ->
    #xmlElement{name='extension'
               ,attributes=[xml_attrib('name', Name)
                           ,xml_attrib('continue', kz_term:is_true(Continue))
                           ]
               ,content=[Child || Child <- Children, Child =/= 'undefined']
               }.

-spec condition_el(kz_types:xml_el() | kz_types:xml_els() | 'undefined') -> kz_types:xml_el().
condition_el(Child) when not is_list(Child) ->
    condition_el([Child]);
condition_el(Children) ->
    #xmlElement{name='condition'
               ,content=[Child || Child <- Children, Child =/= 'undefined']
               }.

-spec condition_el(kz_types:xml_el() | kz_types:xml_els() | 'undefined', kz_types:xml_attrib_value(), kz_types:xml_attrib_value()) -> kz_types:xml_el().
condition_el(Child, Field, Expression) when not is_list(Child) ->
    condition_el([Child], Field, Expression);
condition_el(Children, Field, Expression) ->
    #xmlElement{name='condition'
               ,content=[Child || Child <- Children, Child =/= 'undefined']
               ,attributes=[xml_attrib('field', Field)
                           ,xml_attrib('expression', Expression)
                           ]
               }.

-spec action_el(kz_types:xml_attrib_value()) -> kz_types:xml_el().
action_el(App) ->
    #xmlElement{name='action'
               ,attributes=[xml_attrib('application', App)]
               }.

-spec action_el(kz_types:xml_attrib_value(), kz_types:xml_attrib_value()) -> kz_types:xml_el().
action_el(App, Data) ->
    #xmlElement{name='action'
               ,attributes=[xml_attrib('application', App)
                           ,xml_attrib('data', Data)
                           ]
               }.

-spec action_el(kz_types:xml_attrib_value(), kz_types:xml_attrib_value(), boolean()) -> kz_types:xml_el().
action_el(App, Data, Inline) ->
    #xmlElement{name='action'
               ,attributes=[xml_attrib('application', App)
                           ,xml_attrib('data', Data)
                           ,xml_attrib('inline', kz_term:to_binary(Inline))
                           ]
               }.

-spec result_el(kz_types:xml_attrib_value()) -> kz_types:xml_el().
result_el(Status) ->
    #xmlElement{name='result'
               ,attributes=[xml_attrib('status', Status)]
               }.

room_el(Name, Status) ->
    #xmlElement{name='room'
               ,attributes=[xml_attrib('name', Name)
                           ,xml_attrib('status', Status)
                           ]
               }.

-spec prepend_child(kz_types:xml_el(), kz_types:xml_el()) -> kz_types:xml_el().
prepend_child(#xmlElement{content=Contents}=El, Child) ->
    El#xmlElement{content=[Child|Contents]}.

-spec xml_attrib(kz_types:xml_attrib_name(), kz_types:xml_attrib_value()) -> kz_types:xml_attrib().
xml_attrib(Name, Value) when is_atom(Name) ->
    #xmlAttribute{name=Name, value=kz_term:to_list(Value)}.

sofia_profiles_el(JObj) ->
    Content = lists:foldl(fun(Key, Xml) ->
                                  Profile = kz_json:get_value(Key, JObj),
                                  [#xmlElement{name='profile'
                                              ,attributes=[xml_attrib('name', Key)]
                                              ,content=sofia_profile_el(Profile)
                                              }
                                   | Xml
                                  ]
                          end, [], kz_json:get_keys(JObj)),
    #xmlElement{name='profiles', content=Content}.

sofia_profile_el(JObj) ->
    Settings = kz_json:get_value(<<"Settings">>, JObj, kz_json:new()),
    Gateways = kz_json:get_value(<<"Gateways">>, JObj, kz_json:new()),
    [#xmlElement{name='settings'
                ,content=sofia_settings_el(Settings)
                }
    ,#xmlElement{name='gateways'
                ,content=sofia_gateways_el(Gateways)
                }
    ].

sofia_settings_el(JObj) ->
    lists:foldl(fun(Key, Xml) ->
                        Value = kz_json:get_value(Key, JObj),
                        Name = kz_term:to_lower_binary(Key),
                        [#xmlElement{name='param'
                                    ,attributes=[xml_attrib('name', Name)
                                                ,xml_attrib('value', Value)
                                                ]
                                    }
                         | Xml
                        ]
                end, [], kz_json:get_keys(JObj)).

sofia_gateways_el(JObj) ->
    lists:foldl(fun(Key, Xml) ->
                        Gateway = kz_json:get_value(Key, JObj),
                        [#xmlElement{name='gateway'
                                    ,attributes=[xml_attrib('name', Key)]
                                    ,content=sofia_gateway_el(Gateway)
                                    }
                         | Xml
                        ]
                end, [], kz_json:get_keys(JObj)).

sofia_gateway_el(JObj) ->
    lists:foldl(fun(<<"Variables">>, Xml) ->
                        Variables = kz_json:get_value(<<"Variables">>, JObj),
                        [#xmlElement{name='variables'
                                    ,content=sofia_gateway_vars_el(Variables)
                                    }
                         | Xml
                        ];
                   (Key, Xml) ->
                        Value = kz_json:get_value(Key, JObj),
                        Name = kz_term:to_lower_binary(Key),
                        [#xmlElement{name='param'
                                    ,attributes=[xml_attrib('name', Name)
                                                ,xml_attrib('value', Value)
                                                ]
                                    }
                         | Xml
                        ]
                end, [], kz_json:get_keys(JObj)).

sofia_gateway_vars_el(JObj) ->
    lists:foldl(fun(Key, Xml) ->
                        Value = kz_json:get_value(Key, JObj),
                        [#xmlElement{name='variable'
                                    ,attributes=[xml_attrib('name', Key)
                                                ,xml_attrib('value', Value)
                                                ,xml_attrib('direction', "inbound")
                                                ]
                                    }
                         | Xml
                        ]
                end, [], kz_json:get_keys(JObj)).

-spec sofia_gateways_xml_to_json(kz_types:xml_el() | kz_types:xml_els()) -> kz_json:object().
sofia_gateways_xml_to_json(Xml) ->
    lists:foldl(fun sofia_gateway_xml_to_json/2
               ,kz_json:new()
               ,get_sofia_gateways_el(Xml)
               ).

get_sofia_gateways_el(Xml) ->
    case xmerl_xpath:string("/gateways/gateway", Xml) of
        #xmlElement{}=Gateways -> [Gateways];
        Else -> Else
    end.

sofia_gateway_xml_to_json(Xml, JObj) ->
    Id = kz_xml:get_value("/gateway/name/text()", Xml),
    InboundVars = xmerl_xpath:string("/gateway/inbound-variables/*", Xml),
    OutboundVars = xmerl_xpath:string("/gateway/outbound-variables/*", Xml),
    Props = [{<<"Username">>, kz_xml:get_value("/gateway/username/text()", Xml)}
            ,{<<"Password">>, kz_xml:get_value("/gateway/password/text()", Xml)}
            ,{<<"Realm">>, kz_xml:get_value("/gateway/realm/text()", Xml)}
            ,{<<"Proxy">>, kz_xml:get_value("/gateway/proxy/text()", Xml)}
            ,{<<"From-Domain">>, kz_xml:get_value("/gateway/from/text()", Xml)}
            ,{<<"Expire-Seconds">>, kz_xml:get_value("/gateway/expires/text()", Xml)}
            ,{<<"Inbound-Variables">>, sofia_gateway_vars_xml_to_json(InboundVars, kz_json:new())}
            ,{<<"Outbound-Variables">>, sofia_gateway_vars_xml_to_json(OutboundVars, kz_json:new())}
            ],
    kz_json:set_value(Id, kz_json:from_list(Props), JObj).

-spec sofia_gateway_vars_xml_to_json(kz_types:xml_el() | kz_types:xml_els(), kz_json:object()) -> kz_json:object().
sofia_gateway_vars_xml_to_json(#xmlElement{}=Xml, JObj) ->
    sofia_gateway_vars_xml_to_json([Xml], JObj);
sofia_gateway_vars_xml_to_json([], JObj) ->
    JObj;
sofia_gateway_vars_xml_to_json([Var|Vars], JObj) ->
    Key = kz_xml:get_value("/variable/@name", Var),
    Value = kz_xml:get_value("/variable/@value", Var),
    sofia_gateway_vars_xml_to_json(Vars, kz_json:set_value(Key, Value, JObj)).

-spec event_filters_resp_xml(kz_term:ne_binaries()) -> {'ok', iolist()}.
event_filters_resp_xml(Headers) ->
    EventFiltersEl = event_filters_xml(Headers),
    ConfigurationEl = config_el(<<"kazoo.conf">>, <<"Built by Kazoo">>, [EventFiltersEl]),
    SectionEl = section_el(<<"configuration">>, ConfigurationEl),
    {'ok', xmerl:export([SectionEl], 'fs_xml')}.

event_filters_xml(Headers) ->
    EventFiltersEls = [event_filter_el(Header) || Header <- Headers],
    event_filters_el(EventFiltersEls).

event_filter_el(Header) ->
    #xmlElement{name='header'
               ,attributes=[xml_attrib('name', Header)]
               }.

event_filters_el(Filters) ->
    #xmlElement{name='event-filter'
               ,content=Filters
               ,attributes=[xml_attrib('type', <<"whitelist">>)]
               }.
