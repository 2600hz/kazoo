%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Receive route(dialplan) requests from FS, request routes and respond
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_router_util).

-export([register_binding/3, register_bindings/3]).
-export([search_for_route/5, search_for_route/6]).
-export([reply_affirmative/6]).
-export([route_req/4, route_req/5]).

-include_lib("kazoo_sip/include/kzsip_uri.hrl").
-include("ecallmgr.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

-type search_ret() :: 'ok' | {'ok', kz_json:object()}.

-spec search_for_route(atom(), atom(), kz_term:ne_binary(), kz_term:ne_binary(), kzd_freeswitch:data()) ->
                              search_ret().
search_for_route(Section, Node, FetchId, CallId, Props) ->
    Authz = kapps_config:is_true(?APP_NAME, <<"authz_enabled">>, 'false'),
    search_for_route(Section, Node, FetchId, CallId, Props, Authz).

-spec search_for_route(atom(), atom(), kz_term:ne_binary(), kz_term:ne_binary(), kzd_freeswitch:data(), boolean()) ->
                              search_ret().
search_for_route(Section, Node, FetchId, CallId, Props, 'false') ->
    do_search_for_route(Section, Node, FetchId, CallId, Props, 'undefined');
search_for_route(Section, Node, FetchId, CallId, Props, 'true') ->
    AuthzWorker = spawn_authorize_call_fun(Node, CallId, Props),
    lager:debug("authz worker in ~p", [AuthzWorker]),
    do_search_for_route(Section, Node, FetchId, CallId, Props, AuthzWorker).

-spec do_search_for_route(atom(), atom(), kz_term:ne_binary(), kz_term:ne_binary(), kzd_freeswitch:data(), kz_term:api_pid_ref()) ->
                                 search_ret().
do_search_for_route(Section, Node, FetchId, CallId, Props, AuthzWorker) ->
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
            maybe_wait_for_authz(Section, Node, FetchId, CallId, JObj, Props, AuthzWorker)
    end.

-spec spawn_authorize_call_fun(atom(), kz_term:ne_binary(), kzd_freeswitch:data()) -> kz_term:pid_ref().
spawn_authorize_call_fun(Node, CallId, Props) ->
    Ref = make_ref(),
    Pid = kz_util:spawn(fun authorize_call_fun/5, [self(), Ref, Node, CallId, Props]),
    {Pid, Ref}.

-spec authorize_call_fun(pid(), reference(), atom(), kz_term:ne_binary(), kzd_freeswitch:data()) ->
                                {'authorize_reply', reference(), ecallmgr_fs_authz:authz_reply()}.
authorize_call_fun(Parent, Ref, Node, CallId, Props) ->
    kz_util:put_callid(CallId),
    Parent ! {'authorize_reply', Ref, ecallmgr_fs_authz:authorize(Props, CallId, Node)}.

-spec maybe_wait_for_authz(atom(), atom(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), kz_term:proplist(), 'undefined' | kz_term:pid_ref()) -> search_ret().
maybe_wait_for_authz(Section, Node, FetchId, CallId, JObj, Props, 'undefined') ->
    CCVs = kz_json:get_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new()),
    J = kz_json:set_value(<<"Custom-Channel-Vars">>
                         ,kz_json:set_value(<<"Channel-Authorized">>, <<"true">>, CCVs)
                         ,JObj
                         ),
    reply_affirmative(Section, Node, FetchId, CallId, J, Props);
maybe_wait_for_authz(Section, Node, FetchId, CallId, JObj, Props, AuthzWorker) ->
    case kz_json:get_value(<<"Method">>, JObj) =/= <<"error">> of
        'true' -> wait_for_authz(Section, Node, FetchId, CallId, JObj, Props, AuthzWorker);
        'false' -> reply_affirmative(Section, Node, FetchId, CallId, JObj, Props)
    end.

-spec wait_for_authz(atom(), atom(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), kz_term:proplist(), kz_term:pid_ref()) -> search_ret().
wait_for_authz(Section, Node, FetchId, CallId, JObj, Props, {Pid, Ref}) ->
    lager:info("waiting for authz reply from worker ~p", [Pid]),
    receive
        {'authorize_reply', Ref, 'false'} -> reply_forbidden(Section, Node, FetchId);
        {'authorize_reply', Ref, 'true'} -> reply_affirmative(Section, Node, FetchId, CallId, JObj, Props);
        {'authorize_reply', Ref, {'true', AuthzCCVs}} ->
            CCVs = kz_json:get_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new()),
            J = kz_json:set_value(<<"Custom-Channel-Vars">>
                                 ,kz_json:merge_jobjs(CCVs, AuthzCCVs)
                                 ,JObj
                                 ),
            reply_affirmative(Section, Node, FetchId, CallId, J, Props)
    after 5000 ->
            lager:warning("timeout waiting for authz reply from worker ~p", [Pid])
    end.

%% Reply with a 402 for unauthzed calls
-spec reply_forbidden(atom(), atom(), kz_term:ne_binary()) -> 'ok'.
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

-spec reply_affirmative(atom(), atom(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), kz_term:proplist()) -> search_ret().
reply_affirmative(Section, Node, FetchId, _CallId, JObj0, Props) ->
    lager:info("received affirmative route response for request ~s", [FetchId]),
    CCVs = kz_json:set_values([{<<"Application-Name">>, kz_json:get_value(<<"App-Name">>, JObj0)}
                              ,{<<"Application-Node">>, kz_json:get_value(<<"Node">>, JObj0)}
                              ], kz_json:get_value(<<"Custom-Channel-Vars">>, JObj0, kz_json:new())),
    JObj = kz_json:set_value(<<"Custom-Channel-Vars">>, CCVs, JObj0),
    
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

-spec route_req(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist(), atom()) -> kz_term:proplist().
route_req(CallId, FetchId, Props, Node) ->
    route_req(<<>>, CallId, FetchId, Props, Node).

-spec route_req(binary(), ne_binary(), ne_binary(), kz_proplist(), atom()) -> kz_proplist().
route_req(ServerId, CallId, FetchId, Props, Node) ->
    AccountId = kzd_freeswitch:account_id(Props),
    Context = kzd_freeswitch:hunt_context(Props, ?DEFAULT_FREESWITCH_CONTEXT),

    lager:debug("route req for ~s (~s context ~s)"
               ,[CallId, kzd_freeswitch:origination_call_id(Props), Context]
               ),
    props:filter_empty(
      [{<<"Body">>, get_body(Props)}
      ,{<<"Call-Direction">>, kzd_freeswitch:call_direction(Props)}
      ,{<<"Call-ID">>, CallId}
      ,{<<"Caller-ID-Name">>
       ,kzd_freeswitch:caller_id_name(Props, kapps_call:unknown_caller_id_name(AccountId))
       }
      ,{<<"Caller-ID-Number">>
       ,kzd_freeswitch:caller_id_number(Props, kz_privacy:anonymous_caller_id_number(AccountId))
       }
      ,{<<"Context">>, Context}
      ,{<<"Custom-Application-Vars">>, kz_json:from_list(ecallmgr_util:custom_application_vars(Props))}
      ,{<<"Custom-Channel-Vars">>, kz_json:from_list(route_req_ccvs(FetchId, Props))}
      ,{<<"Custom-Routing-Headers">>, props:get_value(<<"Custom-Routing-Headers">>, Props)}
      ,{<<"Custom-SIP-Headers">>, kz_json:from_list(ecallmgr_util:custom_sip_headers(Props))}
      ,{<<"DTMF-Type">>, props:get_value(<<"variable_switch_r_sdp">>, Props, <<"101 telephone-event">>)}
      ,{<<"From">>, ecallmgr_util:get_sip_from(Props)}
      ,{<<"From-Network-Addr">>, kzd_freeswitch:from_network_ip(Props)}
      ,{<<"From-Network-Port">>, kzd_freeswitch:from_network_port(Props)}
      ,{<<"From-Tag">>, props:get_value(<<"variable_sip_from_tag">>, Props)}
      ,{<<"Message-ID">>, props:get_value(<<"Message-ID">>, Props)}
      ,{<<"Msg-ID">>, FetchId}
      ,{<<"Origination-Call-ID">>, kzd_freeswitch:origination_call_id(Props)}
      ,{<<"Remote-SDP">>, props:get_value(<<"variable_switch_r_sdp">>, Props)}
      ,{<<"Request">>, ecallmgr_util:get_sip_request(Props)}
      ,{<<"Resource-Type">>, kzd_freeswitch:resource_type(Props, <<"audio">>)}
      ,{<<"SIP-Request-Host">>, props:get_value(<<"variable_sip_req_host">>, Props)}
      ,{<<"Switch-Hostname">>, kzd_freeswitch:hostname(Props)}
      ,{<<"Switch-Nodename">>, kz_term:to_binary(Node)}
      ,{<<"Switch-URI">>, props:get_value(<<"Switch-URI">>, Props)}
      ,{<<"Switch-URL">>, props:get_value(<<"Switch-URL">>, Props)}
      ,{<<"To">>, ecallmgr_util:get_sip_to(Props)}
      ,{<<"To-Tag">>, props:get_value(<<"variable_sip_to_tag">>, Props)}
      ,{<<"User-Agent">>, kzd_freeswitch:user_agent(Props)}
       | kz_api:default_headers(ServerId, ?APP_NAME, ?APP_VERSION)
      ]).

-spec route_req_ccvs(kz_term:ne_binary(), kz_term:proplist()) -> kz_term:proplist().
route_req_ccvs(FetchId, Props) ->
    {RedirectedBy, RedirectedReason} = get_redirected(Props),
    CCVs = ecallmgr_util:custom_channel_vars(Props),
    props:filter_undefined(
      [{<<?CALL_INTERACTION_ID>>, props:get_value(<<?CALL_INTERACTION_ID>>, CCVs, ?CALL_INTERACTION_DEFAULT)}
      ,{<<"Fetch-ID">>, FetchId}
      ,{<<"Redirected-By">>, RedirectedBy}
      ,{<<"Redirected-Reason">>, RedirectedReason}
       | props:delete_keys([<<?CALL_INTERACTION_ID>>
                           ,<<"Fetch-ID">>
                           ], CCVs)
       ++ kz_privacy:flags(Props)
      ]
     ).

%% TODO
%% check content-type and decode properly
%% some sip clients send text/html with entities encoded
%% some other use application/vnd.3gpp.sms
-spec get_body(kz_term:proplist()) -> kz_term:api_binary().
get_body(Props) ->
    props:get_value(<<"body">>, Props).

-spec get_redirected(kz_term:proplist()) ->
                            {kz_term:api_binary(), kz_term:api_binary()}.
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

-spec register_bindings(atom(), atom(), kz_term:ne_binaries()) -> boolean().
register_bindings(Node, Section, Bindings) ->
    lists:all(fun(Binding) -> register_binding(Node, Section, Binding) end, Bindings).

-spec register_binding(atom(), atom(), kz_term:ne_binary()) -> boolean().
register_binding(Node, Section, Binding) ->
    gproc:reg({'p', 'l',  ?FS_ROUTE_MSG(Node, Section, Binding)}) =:= 'true'.
