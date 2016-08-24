%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% Receive route(dialplan) requests from FS, request routes and respond
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_router_util).

-export([register_binding/3, register_bindings/3]).
-export([search_for_route/5, search_for_route/6]).
-export([reply_affirmative/6]).

-include_lib("kazoo_sip/include/kzsip_uri.hrl").
-include("ecallmgr.hrl").

-define(CALLER_PRIVACY(Props)
       ,props:is_true(<<"Caller-Screen-Bit">>, Props, 'false')
       ).

-define(CALLER_PRIVACY_NUMBER(Props)
       ,?CALLER_PRIVACY(Props)
        andalso props:is_true(<<"Caller-Privacy-Hide-Number">>, Props, 'false')
       ).

-define(CALLER_PRIVACY_NAME(Props)
       ,?CALLER_PRIVACY(Props)
        andalso props:is_true(<<"Caller-Privacy-Hide-Name">>, Props, 'false')
       ).

%%%===================================================================
%%% API
%%%===================================================================
-type search_ret() :: 'ok' | {'ok', kz_json:object()}.

-spec search_for_route(atom(), atom(), ne_binary(), ne_binary(), kz_proplist()) -> search_ret().
search_for_route(Section, Node, FetchId, CallId, Props) ->
    search_for_route(Section, Node, FetchId, CallId, Props, 'true').

-spec search_for_route(atom(), atom(), ne_binary(), ne_binary(), kz_proplist(), boolean()) -> search_ret().
search_for_route(Section, Node, FetchId, CallId, Props, 'false' = Authz) ->
    do_search_for_route(Section, Node, FetchId, CallId, Props, Authz);
search_for_route(Section, Node, FetchId, CallId, Props, 'true' = Authz) ->
    SetupCall = props:set_value(<<"Call-Setup">>, <<"true">>, Props),
    _ = kz_util:spawn(fun ecallmgr_fs_authz:authorize/3, [SetupCall, CallId, Node]),
    do_search_for_route(Section, Node, FetchId, CallId, Props, Authz).

-spec do_search_for_route(atom(), atom(), ne_binary(), ne_binary(), kz_proplist(), boolean()) -> search_ret().
do_search_for_route(Section, Node, FetchId, CallId, Props, Authz) ->
    Request = route_req(CallId, FetchId, Props, Node),
    ReqResp = kz_amqp_worker:call(Request
                                 ,fun kapi_route:publish_req/1
                                 ,fun kapi_route:is_actionable_resp/1
                                 ,ecallmgr_fs_node:fetch_timeout(Node)
                                 ),
    case ReqResp of
        {'error', _R} ->
            lager:info("did not receive route response for request ~s: ~p", [FetchId, _R]);
        {'ok', JObj} ->
            'true' = kapi_route:resp_v(JObj),
            maybe_wait_for_authz(Section, Node, FetchId, CallId, JObj, Props, Authz)
    end.

-spec maybe_wait_for_authz(atom(), atom(), ne_binary(), ne_binary(), kz_json:object(), kz_proplist(), boolean()) -> search_ret().
maybe_wait_for_authz(Section, Node, FetchId, CallId, JObj, Props, 'false') ->
    reply_affirmative(Section, Node, FetchId, CallId, JObj, Props);
maybe_wait_for_authz(Section, Node, FetchId, CallId, JObj, Props, 'true') ->
    case ecallmgr_config:is_true(<<"authz_enabled">>, 'false')
        andalso kz_json:get_value(<<"Method">>, JObj) =/= <<"error">>
    of
        'true' -> wait_for_authz(Section, Node, FetchId, CallId, JObj, Props);
        'false' -> reply_affirmative(Section, Node, FetchId, CallId, JObj, Props)
    end.

-spec wait_for_authz(atom(), atom(), ne_binary(), ne_binary(), kz_json:object(), kz_proplist()) -> search_ret().
wait_for_authz(Section, Node, FetchId, CallId, JObj, Props) ->
    case kz_cache:wait_for_key_local(?ECALLMGR_UTIL_CACHE, ?AUTHZ_RESPONSE_KEY(CallId)) of
        {'ok', {'true', AuthzCCVs}} ->
            _ = kz_cache:erase_local(?ECALLMGR_UTIL_CACHE, ?AUTHZ_RESPONSE_KEY(CallId)),
            CCVs = kz_json:get_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new()),
            J = kz_json:set_value(<<"Custom-Channel-Vars">>
                                 ,kz_json:merge_jobjs(CCVs, AuthzCCVs)
                                 ,JObj
                                 ),
            reply_affirmative(Section, Node, FetchId, CallId, J, Props);
        _Else -> reply_forbidden(Section, Node, FetchId)
    end.

%% Reply with a 402 for unauthzed calls
-spec reply_forbidden(atom(), atom(), ne_binary()) -> 'ok'.
reply_forbidden(Section, Node, FetchId) ->
    lager:info("received forbidden route response for ~s, sending 403 Incoming call barred", [FetchId]),
    Props = [{<<"Method">>, <<"error">>}
            ,{<<"Route-Error-Code">>, <<"403">>}
            ,{<<"Route-Error-Message">>, <<"Incoming call barred">>}
            ],
    {'ok', XML} = ecallmgr_fs_xml:route_resp_xml(Section, Props, []),
    lager:debug("sending XML to ~s: ~s", [Node, XML]),
    case freeswitch:fetch_reply(Node, FetchId, Section, iolist_to_binary(XML), 3 * ?MILLISECONDS_IN_SECOND) of
        'ok' -> lager:info("node ~s accepted ~s route response for request ~s", [Node, Section, FetchId]);
        {'error', Reason} -> lager:debug("node ~s rejected our ~s route unauthz: ~p", [Node, Section, Reason])
    end.

-spec reply_affirmative(atom(), atom(), ne_binary(), ne_binary(), kz_json:object(), kz_proplist()) -> search_ret().
reply_affirmative(Section, Node, FetchId, _CallId, JObj, Props) ->
    lager:info("received affirmative route response for request ~s", [FetchId]),
    {'ok', XML} = route_resp_xml(Section, JObj, Props),
    lager:debug("sending XML to ~s: ~s", [Node, XML]),
    case freeswitch:fetch_reply(Node, FetchId, Section, iolist_to_binary(XML), 3 * ?MILLISECONDS_IN_SECOND) of
        {'error', _Reason} -> lager:debug("node ~s rejected our ~s route response: ~p", [Node, Section, _Reason]);
        'ok' ->
            lager:info("node ~s accepted ~s route response for request ~s", [Node, Section, FetchId]),
            {'ok', JObj}
    end.

route_resp_xml(Section, JObj, Props) ->
    route_resp_xml(props:get_value(<<"Route-Resp-Fun">>, Props), Section, JObj, Props).

route_resp_xml(Fun, Section, JObj, Props)
  when is_function(Fun, 3) ->
    Fun(Section, JObj, Props);
route_resp_xml(_, Section, JObj, Props) ->
    ecallmgr_fs_xml:route_resp_xml(Section, JObj, Props).

-spec route_req(ne_binary(), ne_binary(), kz_proplist(), atom()) -> kz_proplist().
route_req(CallId, FetchId, Props, Node) ->
    SwitchURL = ecallmgr_fs_node:sip_url(Node),
    [_, SwitchURIHost] = binary:split(SwitchURL, <<"@">>),
    SwitchURI = <<"sip:", SwitchURIHost/binary>>,
    props:filter_empty(
      [{<<"Msg-ID">>, FetchId}
      ,{<<"Call-ID">>, CallId}
      ,{<<"Call-Direction">>, kzd_freeswitch:call_direction(Props)}
      ,{<<"Message-ID">>, props:get_value(<<"Message-ID">>, Props)}
      ,{<<"Caller-ID-Name">>, caller_id_name(Props)}
      ,{<<"Caller-ID-Number">>, caller_id_number(Props)}
      ,{<<"From-Network-Addr">>, kzd_freeswitch:from_network_ip(Props)}
      ,{<<"From-Network-Port">>, kzd_freeswitch:from_network_port(Props)}
      ,{<<"User-Agent">>, kzd_freeswitch:user_agent(Props)}
      ,{<<"To">>, ecallmgr_util:get_sip_to(Props)}
      ,{<<"From">>, ecallmgr_util:get_sip_from(Props)}
      ,{<<"Request">>, ecallmgr_util:get_sip_request(Props)}
      ,{<<"Body">>, get_body(Props) }
      ,{<<"SIP-Request-Host">>, props:get_value(<<"variable_sip_req_host">>, Props)}
      ,{<<"Switch-Nodename">>, kz_util:to_binary(Node)}
      ,{<<"Switch-Hostname">>, props:get_value(<<"FreeSWITCH-Hostname">>, Props)}
      ,{<<"Switch-URL">>, SwitchURL}
      ,{<<"Switch-URI">>, SwitchURI}
      ,{<<"Custom-Channel-Vars">>, kz_json:from_list(route_req_ccvs(FetchId, Props))}
      ,{<<"Custom-SIP-Headers">>, kz_json:from_list(ecallmgr_util:custom_sip_headers(Props))}
      ,{<<"Resource-Type">>, kzd_freeswitch:resource_type(Props, <<"audio">>)}
      ,{<<"To-Tag">>, props:get_value(<<"variable_sip_to_tag">>, Props)}
      ,{<<"From-Tag">>, props:get_value(<<"variable_sip_from_tag">>, Props)}
      ,{<<"Custom-Routing-Headers">>, props:get_value(<<"Custom-Routing-Headers">>, Props)}
       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).

-spec route_req_ccvs(ne_binary(), kz_proplist()) -> kz_proplist().
route_req_ccvs(FetchId, Props) ->
    {RedirectedBy, RedirectedReason} = get_redirected(Props),
    CCVs = ecallmgr_util:custom_channel_vars(Props),
    props:filter_undefined(
      [{<<?CALL_INTERACTION_ID>>, props:get_value(<<?CALL_INTERACTION_ID>>, CCVs, ?CALL_INTERACTION_DEFAULT)}
      ,{<<"Fetch-ID">>, FetchId}
      ,{<<"Redirected-By">>, RedirectedBy}
      ,{<<"Redirected-Reason">>, RedirectedReason}
      ,{<<"Caller-Privacy-Number">>, ?CALLER_PRIVACY_NUMBER(Props)}
      ,{<<"Caller-Privacy-Name">>, ?CALLER_PRIVACY_NAME(Props)}
       | props:delete(<<?CALL_INTERACTION_ID>>, CCVs)
      ]
     ).

%% TODO
%% check content-type and decode properly
%% some sip clients send text/html with entities encoded
%% some other use application/vnd.3gpp.sms
-spec get_body(kz_proplist()) -> api_binary().
get_body(Props) ->
    props:get_value(<<"body">>, Props).

-spec get_redirected(kz_proplist()) ->
                            {api_binary(), api_binary()}.
get_redirected(Props) ->
    case props:get_value(<<"variable_last_bridge_hangup_cause">>, Props) of
        <<"REDIRECTION_TO_NEW_DESTINATION">> ->
            case props:get_value(<<"variable_sip_redirected_by">>, Props) of
                'undefined' -> {'undefined' , 'undefined'};
                Contact ->
                    [#uri{ext_opts=Opts}=Uri] = kzsip_uri:uris(Contact),
                    {kzsip_uri:ruri(Uri#uri{ext_opts=[]}) , props:get_value(<<"reason">>,Opts)}
            end;
        _ -> {'undefined' , 'undefined'}
    end.

-spec caller_id_name(kz_proplist()) -> ne_binary().
caller_id_name(Props) ->
    caller_id_name(?CALLER_PRIVACY_NAME(Props), Props).

-spec caller_id_name(boolean(), kz_proplist()) -> ne_binary().
caller_id_name('true', _Props) ->
    kz_util:anonymous_caller_id_name();
caller_id_name('false', Props) ->
    kzd_freeswitch:caller_id_name(Props, kz_util:anonymous_caller_id_name()).

-spec caller_id_number(kz_proplist()) -> ne_binary().
caller_id_number(Props) ->
    caller_id_number(?CALLER_PRIVACY_NUMBER(Props), Props).

-spec caller_id_number(boolean(), kz_proplist()) -> ne_binary().
caller_id_number('true', _Props) ->
    kz_util:anonymous_caller_id_number();
caller_id_number('false', Props) ->
    kzd_freeswitch:caller_id_number(Props, kz_util:anonymous_caller_id_number()).

-spec register_bindings(atom(), atom(), ne_binaries()) -> boolean().
register_bindings(Node, Section, Bindings) ->
    lists:all(fun(Binding) -> register_binding(Node, Section, Binding) end, Bindings).

-spec register_binding(atom(), atom(), ne_binary()) -> boolean().
register_binding(Node, Section, Binding) ->
    gproc:reg({'p', 'l',  ?FS_ROUTE_MSG(Node, Section, Binding)}) =:= 'true'.
