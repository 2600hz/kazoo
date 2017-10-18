%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Receive route(dialplan) requests from FS, request routes and respond
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_router_util).

-export([route_req/4, route_req/5]).

-include_lib("kazoo_sip/include/kzsip_uri.hrl").
-include("ecallmgr.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================
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
