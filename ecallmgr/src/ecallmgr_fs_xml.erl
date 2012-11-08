%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Generate the XML for various FS responses
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_xml).

-export([get_leg_vars/1, get_channel_vars/1, get_channel_vars/2
         ,route_resp_xml/1 ,authn_resp_xml/1, acl_xml/1
         ,route_not_found/0, empty_response/0
         ,sip_profiles_xml/1, sofia_gateways_xml_to_json/1
        ]).

-include("ecallmgr.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-spec acl_xml/1 :: (wh_json:json_object()) -> {'ok', iolist()}.
acl_xml(AclsJObj) ->
    AclsFold = lists:foldl(fun arrange_acl_node/2, orddict:new(), wh_json:to_proplist(AclsJObj)),

    NetworkListEl = network_list_el([V || {_, V} <- orddict:to_list(AclsFold)]),

    ConfigEl = config_el(<<"acl.conf">>, <<"kazoo generated ACL lists">>, NetworkListEl),

    SectionEl = section_el(<<"configuration">>, ConfigEl),

    {ok, xmerl:export([SectionEl], fs_xml)}.

sip_profiles_xml(JObj) ->
    ProfilesEl = sofia_profiles_el(JObj),

    ConfigEl = config_el(<<"sofia.conf">>, ProfilesEl),

    SectionEl = section_el(<<"configuration">>, ConfigEl),

    {ok, xmerl:export([SectionEl], fs_xml)}.    

-spec authn_resp_xml/1 :: (api_terms()) -> {'ok', iolist()}.
-spec authn_resp_xml/2 :: (ne_binary(), wh_json:json_object()) -> {'ok', iolist()}.
authn_resp_xml([_|_]=RespProp) ->
    authn_resp_xml(props:get_value(<<"Auth-Method">>, RespProp), wh_json:from_list(RespProp));
authn_resp_xml(RespJObj) ->
    authn_resp_xml(wh_json:get_value(<<"Auth-Method">>, RespJObj), RespJObj).

authn_resp_xml(<<"password">>, JObj) ->
    PassEl = param_el(<<"password">>, wh_json:get_value(<<"Auth-Password">>, JObj)),
    ParamsEl = params_el([PassEl]),

    VariableEls = [variable_el(K, V) || {K, V} <- get_channel_params(JObj)],
    VariablesEl = variables_el(VariableEls),

    UserEl = user_el(wh_json:get_value(<<"Auth-User">>, JObj), [VariablesEl, ParamsEl]),

    DomainEl = domain_el(wh_json:get_value(<<"Auth-Realm">>, JObj), UserEl),

    SectionEl = section_el(<<"directory">>, DomainEl),

    {ok, xmerl:export([SectionEl], fs_xml)};

authn_resp_xml(<<"a1-hash">>, JObj) ->
    PassEl = param_el(<<"a1-hash">>, wh_json:get_value(<<"Auth-Password">>, JObj)),
    ParamsEl = params_el([PassEl]),

    VariableEls = [variable_el(K, V) || {K, V} <- get_channel_params(JObj)],
    VariablesEl = variables_el(VariableEls),

    UserEl = user_el(wh_json:get_value(<<"Auth-User">>, JObj), [VariablesEl, ParamsEl]),

    DomainEl = domain_el(wh_json:get_value(<<"Auth-Realm">>, JObj), UserEl),

    SectionEl = section_el(<<"directory">>, DomainEl),

    {ok, xmerl:export([SectionEl], fs_xml)};

authn_resp_xml(<<"ip">>, _JObj) ->
    empty_response();
authn_resp_xml(_Method, _JObj) ->
    lager:debug("unknown method ~s", [_Method]),
    empty_response().

-spec empty_response/0 :: () -> {'ok', []}.
empty_response() ->
    {ok, ""}. %"<document type=\"freeswitch/xml\"></document>").

-spec route_resp_xml/1 :: (api_terms()) -> {'ok', iolist()}.
route_resp_xml([_|_]=RespProp) ->
    route_resp_xml(wh_json:from_list(RespProp));
route_resp_xml(RespJObj) ->
    route_resp_xml(wh_json:get_value(<<"Method">>, RespJObj)
                   ,wh_json:get_value(<<"Routes">>, RespJObj, [])
                   ,RespJObj
                  ).

%% Prop = Route Response
-spec route_resp_xml/3 :: (ne_binary(), wh_json:json_objects(), wh_json:json_object()) -> {'ok', iolist()}.
route_resp_xml(<<"bridge">>, Routes, _JObj) ->
    lager:debug("creating a bridge XML response"),
    LogEl = route_resp_log_winning_node(),
    RingbackEl = route_resp_ringback(),
    %% format the Route based on protocol
    {_Idx, Extensions} = lists:foldr(
                                   fun(RouteJObj, {Idx, Acc}) ->
                                           case ecallmgr_util:build_channel(RouteJObj) of
                                               {error, _} ->
                                                   {Idx+1, Acc};
                                               {ok, Route} ->
                                                   BypassMedia = case wh_json:get_value(<<"Media">>, RouteJObj) of
                                                                     <<"bypass">> -> "true";
                                                                     _ -> "false" %% default to not bypassing media
                                                                 end,

                                                   RouteJObj1 = case wh_json:get_value(<<"Progress-Timeout">>, RouteJObj) of
                                                                    undefined ->
                                                                        wh_json:set_value(<<"Progress-Timeout">>, <<"6">>, RouteJObj);
                                                                    I when is_integer(I) ->
                                                                        wh_json:set_value(<<"Progress-Timeout">>, integer_to_list(I), RouteJObj);
                                                                    _ -> RouteJObj
                                                                end,

                                                   ChannelVars = get_channel_vars(wh_json:to_proplist(RouteJObj1)),

                                                   BPEl = action_el(<<"set">>, [<<"bypass_media=">>, BypassMedia]),
                                                   HangupEl = action_el(<<"set">>, <<"hangup_after_bridge=true">>),
                                                   FailureEl = action_el(<<"set">>, <<"failure_causes=NORMAL_CLEARING,ORIGINATOR_CANCEL,CRASH">>),
                                                   BridgeEl = action_el(<<"set">>, [ChannelVars, Route]),

                                                   ConditionEl = condition_el([BPEl, HangupEl, FailureEl, BridgeEl]),
                                                   ExtEl = extension_el([<<"match_">>, Idx+$0], <<"true">>, [ConditionEl]),

                                                   {Idx+1, [ExtEl | Acc]}
                                           end
                                   end, {1, []}, Routes),

    FailRespondEl = action_el(<<"respond">>, <<"${bridge_hangup_cause}">>),
    FailConditionEl = condition_el(FailRespondEl),
    FailExtEl = extension_el(<<"failed_bridge">>, <<"false">>, [FailConditionEl]),

    ContextEl = context_el(?WHISTLE_CONTEXT, [LogEl, RingbackEl] ++ Extensions ++ [FailExtEl]),
    SectionEl = section_el(<<"dialplan">>, <<"Route Bridge Response">>, ContextEl),
    {ok, xmerl:export([SectionEl], fs_xml)};

route_resp_xml(<<"park">>, _Routes, JObj) ->
    LogEl = route_resp_log_winning_node(),
    RingbackEl = route_resp_ringback(),
    ParkEl = action_el(<<"park">>),

    ParkConditionEl = case route_resp_pre_park_action(JObj) of
                          undefined ->
                              condition_el([LogEl, RingbackEl, ParkEl]);
                          PreParkEl ->
                              condition_el([LogEl, RingbackEl, PreParkEl, ParkEl])
                      end,

    ParkExtEl = extension_el(<<"park">>, undefined, [ParkConditionEl]),

    ContextEl = context_el(?WHISTLE_CONTEXT, [ParkExtEl]),
    SectionEl = section_el(<<"dialplan">>, <<"Route Park Response">>, ContextEl),
    {ok, xmerl:export([SectionEl], fs_xml)};

route_resp_xml(<<"error">>, _Routes, JObj) ->
    LogEl = route_resp_log_winning_node(),

    RingbackEl = route_resp_ringback(),

    ErrCode = wh_json:get_value(<<"Route-Error-Code">>, JObj),
    ErrMsg = [" ", wh_json:get_value(<<"Route-Error-Message">>, JObj, <<"">>)],

    ErrEl = action_el(<<"respond">>, [ErrCode, ErrMsg]),
    ErrCondEl = condition_el([LogEl, RingbackEl, ErrEl]),
    ErrExtEl = extension_el([ErrCondEl]),

    ContextEl = context_el(?WHISTLE_CONTEXT, [ErrExtEl]),
    SectionEl = section_el(<<"dialplan">>, <<"Route Error Response">>, ContextEl),
    {ok, xmerl:export([SectionEl], fs_xml)}.

-spec route_not_found/0 :: () -> {'ok', iolist()}.
route_not_found() ->
    ResultEl = result_el(<<"not found">>),
    SectionEl = section_el(<<"result">>, <<"Route Not Found">>, ResultEl),
    {ok, xmerl:export([SectionEl], fs_xml)}.

-spec route_resp_log_winning_node/0 :: () -> xml_el().
route_resp_log_winning_node() ->
    action_el(<<"log">>, [<<"NOTICE log|${uuid}|", (wh_util:to_binary(node()))/binary, " won call control">>]).

-spec route_resp_ringback/0 :: () -> xml_el().
route_resp_ringback() ->
    case ecallmgr_util:get_setting(<<"default_ringback">>, <<"%(2000,4000,440,480)">>) of
        {ok, RBSetting} ->
            action_el(<<"set">>, <<"ringback=", (wh_util:to_binary(RBSetting))/binary>>);
        _Else ->
            action_el(<<"log">>, [<<"NOTICE log|${uuid}|unable to set default ringback">>])
    end.

-spec route_resp_pre_park_action/1 :: (wh_json:json_object()) -> 'undefined' | xml_el().
route_resp_pre_park_action(JObj) ->
    case wh_json:get_value(<<"Pre-Park">>, JObj) of
        <<"ring_ready">> -> action_el(<<"ring_ready">>);
        <<"answer">> -> action_el(<<"answer">>);
        _Else -> undefined
    end.

-spec get_leg_vars/1 :: (wh_json:json_object() | proplist()) -> [nonempty_string(),...].
get_leg_vars([_|_]=Prop) ->
    ["[", string:join([wh_util:to_list(V) || V <- lists:foldr(fun get_channel_vars/2, [], Prop)], ","), "]"];
get_leg_vars(JObj) -> get_leg_vars(wh_json:to_proplist(JObj)).

-spec get_channel_vars/1 :: (wh_json:json_object() | proplist()) -> iolist().
get_channel_vars([_|_]=Prop) ->
    P = Prop ++ [{<<"Overwrite-Channel-Vars">>, <<"true">>}],
    ["{", string:join([wh_util:to_list(V) || V <- lists:foldr(fun get_channel_vars/2, [], P)], ","), "}"];
get_channel_vars(JObj) -> get_channel_vars(wh_json:to_proplist(JObj)).

-spec get_channel_vars/2 :: ({binary(), binary() | wh_json:json_object()}, [binary(),...] | []) -> iolist().
get_channel_vars({<<"Custom-Channel-Vars">>, JObj}, Vars) ->
    lists:foldl(fun({<<"Force-Fax">>, Direction}, Acc) ->
                        [<<"execute_on_answer='t38_gateway ", Direction/binary, "'">>|Acc];
                   ({K, V}, Acc) ->
                        case lists:keyfind(K, 1, ?SPECIAL_CHANNEL_VARS) of
                            false -> [list_to_binary([?CHANNEL_VAR_PREFIX, wh_util:to_list(K)
                                                      ,"='", wh_util:to_list(V), "'"]) | Acc];
                            {_, <<"group_confirm_file">>} -> [list_to_binary(["group_confirm_file='"
                                                                             ,wh_util:to_list(ecallmgr_util:media_path(V, extant, get(callid), wh_json:new()))
                                                                             ,"'"
                                                                            ]) | Acc];
                            {_, Prefix} -> [list_to_binary([Prefix, "='", wh_util:to_list(V), "'"]) | Acc]
                        end
                end, Vars, wh_json:to_proplist(JObj));

get_channel_vars({<<"SIP-Headers">>, SIPJObj}, Vars) ->
    SIPHeaders = wh_json:to_proplist(SIPJObj),
    lists:foldl(fun({K,V}, Vars0) ->
                        [ list_to_binary(["sip_h_", K, "=", V]) | Vars0]
                end, Vars, SIPHeaders);

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
                     ,wh_util:to_list(ecallmgr_util:media_path(Media, extant, get(callid), wh_json:new()))
                    ])
     | Vars];

get_channel_vars({<<"Codecs">>, []}, Vars) ->
    Vars;
get_channel_vars({<<"Codecs">>, Cs}, Vars) ->
    Codecs = [ wh_util:to_list(C) || C <- Cs, not wh_util:is_empty(C) ],
    CodecStr = string:join(Codecs, ","),
    [ list_to_binary(["codec_string='", CodecStr, "'"]) | Vars];

%% SPECIAL CASE: Timeout must be larger than zero
get_channel_vars({<<"Timeout">>, V}, Vars) ->
    case wh_util:to_integer(V) of
        TO when TO > 0 ->
            [ <<"call_timeout=", (wh_util:to_binary(TO))/binary>> | Vars];
        _Else ->
            Vars
    end;

get_channel_vars({<<"Forward-IP">>, <<"sip:", _/binary>>=V}, Vars) ->
    [ list_to_binary(["sip_route_uri='", V, "'"]) | Vars];

get_channel_vars({<<"Forward-IP">>, V}, Vars) ->
    get_channel_vars({<<"Forward-IP">>, <<"sip:", V/binary>>}, Vars);

get_channel_vars({AMQPHeader, V}, Vars) when not is_list(V) ->
    case lists:keyfind(AMQPHeader, 1, ?SPECIAL_CHANNEL_VARS) of
        false -> Vars;
        {_, Prefix} -> [list_to_binary([Prefix, "='", wh_util:to_list(V), "'"]) | Vars]
    end;

get_channel_vars(_, Vars) ->
    Vars.

-spec get_channel_params/1 :: (wh_json:json_object()) -> wh_json:json_proplist().
get_channel_params(JObj) ->
    CV0 = case wh_json:get_value(<<"Tenant-ID">>, JObj) of
              undefined -> [];
              TID -> [{list_to_binary([?CHANNEL_VAR_PREFIX, "Tenant-ID"]), TID}]
          end,

    CV1 = case wh_json:get_value(<<"Access-Group">>, JObj) of
              undefined -> CV0;
              AG -> [{list_to_binary([?CHANNEL_VAR_PREFIX, "Access-Group"]), AG} | CV0]
          end,

    Custom = wh_json:to_proplist(wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new())),
    lists:foldl(fun({K,V}, CV) ->
                        [{list_to_binary([?CHANNEL_VAR_PREFIX, K]), V} | CV]
                end, CV1, Custom).

-spec arrange_acl_node/2 :: ({ne_binary(), wh_json:json_object()}, orddict:orddict()) -> orddict:orddict().
arrange_acl_node({_, JObj}, Dict) ->
    AclList = wh_json:get_value(<<"network-list-name">>, JObj),

    NodeEl = acl_node_el(wh_json:get_value(<<"type">>, JObj), wh_json:get_value(<<"cidr">>, JObj)),

    case orddict:find(AclList, Dict) of
        {ok, ListEl} ->
            lager:debug("found existing list ~s", [AclList]),
            orddict:store(AclList, prepend_child(ListEl, NodeEl), Dict);
        error ->
            lager:debug("creating new list xml for ~s", [AclList]),
            orddict:store(AclList, prepend_child(acl_list_el(AclList), NodeEl), Dict)
    end.

%%%-------------------------------------------------------------------
%% XML record creators and helpers
%%%-------------------------------------------------------------------
-type xml_attrib_name() :: atom().
-type xml_attrib_value() :: ne_binary() | nonempty_string() | iolist() | atom().
-type xml_attrib() :: #xmlAttribute{}.

-type xml_el() :: #xmlElement{}.
-type xml_els() :: [xml_el(),...] | [].

-spec acl_node_el/2 :: (xml_attrib_value(), xml_attrib_value()) -> xml_el().
acl_node_el(Type, CIDR) ->
    #xmlElement{name='node'
                ,attributes=[xml_attrib(type, Type)
                             ,xml_attrib(cidr, CIDR)
                            ]
               }.

-spec acl_list_el/1 :: (xml_attrib_value()) -> xml_el().
-spec acl_list_el/2 :: (xml_attrib_value(), xml_attrib_value()) -> xml_el().
-spec acl_list_el/3 :: (xml_attrib_value(), xml_attrib_value(), xml_els()) -> xml_el().
acl_list_el(Name) ->
    acl_list_el(Name, <<"deny">>).
acl_list_el(Name, Default) ->
    acl_list_el(Name, Default, []).
acl_list_el(Name, Default, Children) ->
    #xmlElement{name='list'
                ,attributes=[xml_attrib(name, Name)
                             ,xml_attrib(default, Default)
                            ]
                ,content=Children
               }.

-spec network_list_el/1 :: (xml_els()) -> xml_el().
network_list_el(ListsEls) ->
    #xmlElement{name='network-lists', content=ListsEls}.

config_el(Name, Content) ->
    config_el(Name, <<"configuration ", (wh_util:to_binary(Name))/binary, " built by kazoo">>, Content).

config_el(Name, Desc, #xmlElement{}=Content) ->
    config_el(Name, Desc, [Content]);
config_el(Name, Desc, Content) ->
    #xmlElement{name='configuration'
                ,attributes=[xml_attrib(name, Name)
                             ,xml_attrib(description, Desc)
                            ]
                ,content=Content
               }.

-spec section_el/2 :: (xml_attrib_value(), xml_el()) -> xml_el().
-spec section_el/3 :: (xml_attrib_value(), xml_attrib_value(), xml_el()) -> xml_el().
section_el(Name, #xmlElement{}=Content) ->
    section_el(Name, [Content]);
section_el(Name, Content) ->
    #xmlElement{name='section'
                ,attributes=[xml_attrib(name, Name)]
                ,content=Content
               }.

section_el(Name, Desc, #xmlElement{}=Content) ->
    section_el(Name, Desc, [Content]);
section_el(Name, Desc, Content) ->
    #xmlElement{name='section'
                ,attributes=[xml_attrib(name, Name)
                             ,xml_attrib(description, Desc)
                            ]
                ,content=Content
               }.

-spec domain_el/2 :: (xml_attrib_value(), xml_el() | xml_els()) -> xml_el().
domain_el(Name, Child) when not is_list(Child) ->
    domain_el(Name, [Child]);
domain_el(Name, Children) ->
    #xmlElement{name='domain'
                ,attributes=[xml_attrib(name, Name)]
                ,content=Children
               }.

-spec user_el/2 :: (xml_attrib_value(), xml_els()) -> xml_el().
user_el(Id, Children) ->
    #xmlElement{name='user'
                ,attributes=[xml_attrib(id, Id)]
                ,content=Children
               }.

-spec params_el/1 :: (xml_els()) -> xml_el().
params_el(Children) ->
    #xmlElement{name='params'
                ,content=Children
               }.

-spec param_el/2 :: (xml_attrib_value(), xml_attrib_value()) -> xml_el().
param_el(Name, Value) ->
    #xmlElement{name='param'
                ,attributes=[xml_attrib(name, Name)
                             ,xml_attrib(value, Value)
                            ]
               }.

-spec variables_el/1 :: (xml_els()) -> xml_el().
variables_el(Children) ->
    #xmlElement{name='variables'
                ,content=Children
               }.

-spec variable_el/2 :: (xml_attrib_value(), xml_attrib_value()) -> xml_el().
variable_el(Name, Value) ->
    #xmlElement{name='variable'
                ,attributes=[xml_attrib(name, Name)
                             ,xml_attrib(value, Value)
                            ]
               }.

-spec context_el/2 :: (xml_attrib_value(), xml_els()) -> xml_el().
context_el(Name, Children) ->
    #xmlElement{name='context'
                ,attributes=[xml_attrib(name, Name)]
                ,content=Children
               }.

-spec extension_el/1 :: (xml_els()) -> xml_el().
-spec extension_el/3 :: (xml_attrib_value(), xml_attrib_value() | 'undefined', xml_els()) -> xml_el().
extension_el(Children) ->
    #xmlElement{name='extension'
                ,content=Children
               }.

extension_el(Name, undefined, Children) ->
    #xmlElement{name='extension'
                ,attributes=[xml_attrib(name, Name)]
                ,content=Children
               };
extension_el(Name, Continue, Children) ->
    #xmlElement{name='extension'
                ,attributes=[xml_attrib(name, Name)
                             ,xml_attrib(continue, wh_util:is_true(Continue))
                            ]
                ,content=Children
               }.

-spec condition_el/1 :: (xml_el() | xml_els()) -> xml_el().
condition_el(Child) when not is_list(Child) ->
    condition_el([Child]);
condition_el(Children) ->
    #xmlElement{name='condition'
                ,content=Children
               }.

-spec action_el/1 :: (xml_attrib_value()) -> xml_el().
-spec action_el/2 :: (xml_attrib_value(), xml_attrib_value()) -> xml_el().
action_el(App) ->
    #xmlElement{name='action'
                ,attributes=[xml_attrib(application, App)]
               }.
action_el(App, Data) ->
    #xmlElement{name='action'
                ,attributes=[xml_attrib(application, App)
                             ,xml_attrib(data, Data)
                            ]
               }.

-spec result_el/1 :: (xml_attrib_value()) -> xml_el().
result_el(Status) ->
    #xmlElement{name='result'
                ,attributes=[xml_attrib(status, Status)]
               }.

-spec prepend_child/2 :: (xml_el(), xml_el()) -> xml_el().
prepend_child(#xmlElement{content=Contents}=El, Child) ->
    El#xmlElement{content=[Child|Contents]}.

-spec xml_attrib/2 :: (xml_attrib_name(), xml_attrib_value()) -> xml_attrib().
xml_attrib(Name, Value) when is_atom(Name) ->
    #xmlAttribute{name=Name, value=wh_util:to_list(Value)}.

sofia_profiles_el(JObj) ->
    Content = lists:foldl(fun(Key, Xml) ->
                                  Profile = wh_json:get_value(Key, JObj),
                                  [#xmlElement{name='profile'
                                               ,attributes=[xml_attrib(name, Key)]
                                               ,content=sofia_profile_el(Profile)
                                              }
                                   | Xml
                                  ]
                          end, [], wh_json:get_keys(JObj)),
    #xmlElement{name='profiles', content=Content}.
    
sofia_profile_el(JObj) ->
    Settings = wh_json:get_value(<<"Settings">>, JObj, wh_json:new()),
    Gateways = wh_json:get_value(<<"Gateways">>, JObj, wh_json:new()),
    [#xmlElement{name='settings'
                 ,content=sofia_settings_el(Settings)
                }
     ,#xmlElement{name='gateways'
                  ,content=sofia_gateways_el(Gateways)
                 }            
    ].

sofia_settings_el(JObj) ->
    lists:foldl(fun(Key, Xml) ->
                        Value = wh_json:get_value(Key, JObj),
                        Name = wh_util:to_lower_binary(Key),
                        [#xmlElement{name='param'
                                     ,attributes=[xml_attrib('name', Name)
                                                  ,xml_attrib('value', Value)
                                                 ]
                                    }
                         | Xml
                        ]
                end, [], wh_json:get_keys(JObj)).

sofia_gateways_el(JObj) -> 
    lists:foldl(fun(Key, Xml) ->
                        Gateway = wh_json:get_value(Key, JObj),
                        [#xmlElement{name='gateway'
                                     ,attributes=[xml_attrib(name, Key)]
                                     ,content=sofia_gateway_el(Gateway)
                                    }
                                    | Xml
                        ]
                end, [], wh_json:get_keys(JObj)).

sofia_gateway_el(JObj) ->
    lists:foldl(fun(<<"Variables">>, Xml) -> 
                        Variables = wh_json:get_value(<<"Variables">>, JObj),
                        [#xmlElement{name='variables'
                                     ,content=sofia_gateway_vars_el(Variables)
                                    }
                         | Xml
                        ];
                   (Key, Xml) ->
                        Value = wh_json:get_value(Key, JObj),
                        Name = wh_util:to_lower_binary(Key),
                        [#xmlElement{name='param'
                                     ,attributes=[xml_attrib('name', Name)
                                                  ,xml_attrib('value', Value)
                                                 ]
                                    }
                         | Xml
                        ]
                end, [], wh_json:get_keys(JObj)).

sofia_gateway_vars_el(JObj) ->
    lists:foldl(fun(Key, Xml) ->
                        Value = wh_json:get_value(Key, JObj),
                        [#xmlElement{name='variable'
                                     ,attributes=[xml_attrib('name', Key)
                                                  ,xml_attrib('value', Value)
                                                  ,xml_attrib('direction', "inbound")
                                                 ]
                                    }
                         | Xml
                        ]
                end, [], wh_json:get_keys(JObj)).    


sofia_gateways_xml_to_json(Xml) ->
    lists:foldl(fun sofia_gateway_xml_to_json/2
                ,wh_json:new()
                ,get_sofia_gateways_el(Xml)).

get_sofia_gateways_el(Xml) ->
    case xmerl_xpath:string("/gateways/gateway", Xml) of
        #xmlElement{}=Gateways -> [Gateways];
        Else -> Else
    end.

sofia_gateway_xml_to_json(Xml, JObj) ->
    Id = wh_util:get_xml_value("/gateway/name/text()", Xml),
    InboundVars = xmerl_xpath:string("/gateway/inbound-variables/*", Xml),
    OutboundVars = xmerl_xpath:string("/gateway/outbound-variables/*", Xml),
    Props = [{<<"Username">>, wh_util:get_xml_value("/gateway/username/text()", Xml)}
             ,{<<"Password">>, wh_util:get_xml_value("/gateway/password/text()", Xml)}
             ,{<<"Realm">>, wh_util:get_xml_value("/gateway/realm/text()", Xml)}
             ,{<<"Proxy">>, wh_util:get_xml_value("/gateway/proxy/text()", Xml)}
             ,{<<"From-Domain">>, wh_util:get_xml_value("/gateway/from/text()", Xml)}
             ,{<<"Expire-Seconds">>, wh_util:get_xml_value("/gateway/expires/text()", Xml)}
             ,{<<"Inbound-Variables">>, sofia_gateway_vars_xml_to_json(InboundVars, wh_json:new())}
             ,{<<"Outbound-Variables">>, sofia_gateway_vars_xml_to_json(OutboundVars, wh_json:new())}
            ],
    wh_json:set_value(Id, wh_json:from_list(Props), JObj).

sofia_gateway_vars_xml_to_json(#xmlElement{}=Xml, JObj) ->
    sofia_gateway_vars_xml_to_json([Xml], JObj);
sofia_gateway_vars_xml_to_json([], JObj) ->
    JObj;
sofia_gateway_vars_xml_to_json([Var|Vars], JObj) ->
    Key = wh_util:get_xml_value("/variable/@name", Var),
    Value = wh_util:get_xml_value("/variable/@value", Var),
    sofia_gateway_vars_xml_to_json(Vars, wh_json:set_value(Key, Value, JObj)).

