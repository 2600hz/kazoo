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

-export([build_sip_route/2, build_freetdm_route/1
         ,get_leg_vars/1, get_channel_vars/1, get_channel_vars/2
         ,route_resp_xml/1 ,authn_resp_xml/1, acl_xml/1
         ,route_not_found/0, empty_response/0
        ]).

-include("ecallmgr.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-spec acl_xml/1 :: (wh_json:json_object()) -> {'ok', iolist()}.
acl_xml(AclsJObj) ->
    AclsFold = lists:foldl(fun arrange_acl_node/2, orddict:new(), wh_json:to_proplist(AclsJObj)),

    NetworkListEl = network_list_el([V || {_, V} <- orddict:to_list(AclsFold)]),

    ConfigEl = config_el(<<"acl.conf">>, <<"ecallmgr generated ACL lists">>, NetworkListEl),

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
    %% format the Route based on protocol
    {_Idx, Extensions} = lists:foldr(
                                   fun(RouteJObj, {Idx, Acc}) ->
                                           case build_sip_route(RouteJObj, wh_json:get_value(<<"Invite-Format">>, RouteJObj)) of
                                               {error, timeout} ->
                                                   {Idx+1, Acc};
                                               Route ->
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

    ContextEl = context_el(?WHISTLE_CONTEXT, Extensions ++ [FailExtEl]),
    SectionEl = section_el(<<"dialplan">>, <<"Route Bridge Response">>, ContextEl),
    {ok, xmerl:export([SectionEl], fs_xml)};

route_resp_xml(<<"park">>, _Routes, _JObj) ->
    ParkRespondEl = action_el(<<"park">>),
    ParkConditionEl = condition_el(ParkRespondEl),
    ParkExtEl = extension_el(<<"park">>, undefined, [ParkConditionEl]),

    ContextEl = context_el(?WHISTLE_CONTEXT, [ParkExtEl]),
    SectionEl = section_el(<<"dialplan">>, <<"Route Park Response">>, ContextEl),
    {ok, xmerl:export([SectionEl], fs_xml)};

route_resp_xml(<<"error">>, _Routes, JObj) ->
    ErrCode = wh_json:get_value(<<"Route-Error-Code">>, JObj),
    ErrMsg = [" ", wh_json:get_value(<<"Route-Error-Message">>, JObj, <<"">>)],

    ErrEl = action_el(<<"respond">>, [ErrCode, ErrMsg]),
    ErrCondEl = condition_el(ErrEl),
    ErrExtEl = extension_el([ErrCondEl]),

    ContextEl = context_el(?WHISTLE_CONTEXT, [ErrExtEl]),
    SectionEl = section_el(<<"dialplan">>, <<"Route Error Response">>, ContextEl),
    {ok, xmerl:export([SectionEl], fs_xml)}.

-spec route_not_found/0 :: () -> {'ok', iolist()}.
route_not_found() ->
    ResultEl = result_el(<<"not found">>),
    SectionEl = section_el(<<"result">>, <<"Route Not Found">>, ResultEl),
    {ok, xmerl:export([SectionEl], fs_xml)}.

-spec build_sip_route/2 :: (api_terms(), ne_binary() | 'undefined') -> ne_binary() |
                                                                       {'error', 'timeout'}.
build_sip_route(Route, undefined) ->
    build_sip_route(Route, <<"username">>);
build_sip_route([_|_]=RouteProp, DIDFormat) ->
    build_sip_route(wh_json:from_list(RouteProp), DIDFormat);
build_sip_route(RouteJObj, <<"route">>) ->
    case wh_json:get_value(<<"Route">>, RouteJObj) of
        <<"sip:", _/binary>> = R1 -> <<?SIP_INTERFACE, (R1)/binary>>;
        R2 -> R2
    end;
build_sip_route(RouteJObj, <<"username">>) ->
    User = wh_json:get_value(<<"To-User">>, RouteJObj),
    Realm = wh_json:get_value(<<"To-Realm">>, RouteJObj),
    case ecallmgr_registrar:lookup_contact(Realm, User) of
        {ok, Contact} ->
            RURI = binary:replace(
                     re:replace(Contact, "^[^\@]+", User, [{return, binary}])
                     ,<<">">>, <<>>
                    ),
            list_to_binary([?SIP_INTERFACE, RURI, sip_transport(RouteJObj)]);
        {error, timeout}=E ->
            lager:debug("failed to lookup user ~s@~s in the registrar", [User, Realm]),
            E
    end;
build_sip_route(RouteJObj, DIDFormat) ->
    User = wh_json:get_value(<<"To-User">>, RouteJObj),
    Realm = wh_json:get_value(<<"To-Realm">>, RouteJObj),
    DID = format_did(wh_json:get_value(<<"To-DID">>, RouteJObj), DIDFormat),
    case ecallmgr_registrar:lookup_contact(Realm, User) of
        {ok, Contact} ->
            RURI = binary:replace(
                     re:replace(Contact, "^[^\@]+", DID, [{return, binary}])
                     ,<<">">>, <<>>
                    ),
            list_to_binary([?SIP_INTERFACE, RURI, sip_transport(RouteJObj)]);
        {error, timeout}=E ->
            lager:debug("failed to lookup user ~s@~s in the registrar", [User, Realm]),
            E
    end.

build_freetdm_route(EndpointJObj) ->
    Opts = wh_json:get_value(<<"Endpoint-Options">>, EndpointJObj, wh_json:new()),
    Span = wh_json:get_binary_value(<<"Span">>, Opts, <<"1">>),
    StartFrom = case wh_json:get_binary_value(<<"Channel-Selection">>, Opts) of
                    <<"descending">> -> <<"A">>;
                    _ -> <<"a">>
                end,
    DID = format_did(wh_json:get_binary_value(<<"To-DID">>, EndpointJObj), wh_json:get_value(<<"Invite-Format">>, EndpointJObj, <<"e164">>)),

    lager:debug("freetdm/span: ~s/start: ~s/DID: ~s", [Span, StartFrom, DID]),

    [<<"freetdm/">>, Span, <<"/">>, StartFrom, <<"/">>, DID].

-spec format_did/2 :: (ne_binary(), ne_binary()) -> ne_binary().
format_did(DID, <<"e164">>) ->
    wnm_util:to_e164(DID);
format_did(DID, <<"npan">>) ->
    wnm_util:to_npan(DID);
format_did(DID, <<"1npan">>) ->
    wnm_util:to_1npan(DID);
format_did(DID, _) ->
    DID.

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
    lists:foldl(fun({K, V}, Acc) ->
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

get_channel_vars({AMQPHeader, V}, Vars) when not is_list(V) ->
    case lists:keyfind(AMQPHeader, 1, ?SPECIAL_CHANNEL_VARS) of
        false -> Vars;
        {_, Prefix} -> [list_to_binary([Prefix, "='", wh_util:to_list(V), "'"]) | Vars]
    end;

get_channel_vars(_, Vars) ->
    Vars.


-spec sip_transport/1 :: (wh_json:json_object() | ne_binary() | 'undefined') -> binary().
sip_transport(<<"tcp">>) ->
    <<";transport=tcp">>;
sip_transport(<<"udp">>) ->
    <<";transport=udp">>;
sip_transport(<<"tls">>) ->
    <<";transport=tls">>;
sip_transport(<<"sctp">>) ->
    <<";transport=sctp">>;
sip_transport(undefined) ->
    <<>>;
sip_transport(JObj) when not is_binary(JObj) ->
    sip_transport(wh_util:to_lower_binary(wh_json:get_value(<<"Transport">>, JObj)));
sip_transport(_) ->
    <<>>.

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

config_el(Name, Desc, NetworkListEl) ->
    #xmlElement{name='configuration'
                ,attributes=[xml_attrib(name, Name)
                             ,xml_attrib(description, Desc)
                            ]
                ,content=[NetworkListEl]
               }.

-spec section_el/2 :: (xml_attrib_value(), xml_el()) -> xml_el().
-spec section_el/3 :: (xml_attrib_value(), xml_attrib_value(), xml_el()) -> xml_el().
section_el(Name, Content) ->
    #xmlElement{name='section'
                ,attributes=[xml_attrib(name, Name)]
                ,content=[Content]
               }.
section_el(Name, Desc, Content) ->
    #xmlElement{name='section'
                ,attributes=[xml_attrib(name, Name)
                             ,xml_attrib(description, Desc)
                            ]
                ,content=[Content]
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
