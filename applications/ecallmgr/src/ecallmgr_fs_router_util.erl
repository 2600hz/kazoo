%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
%%% @doc
%%% Receive route(dialplan) requests from FS, request routes and respond
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_router_util).

-export([route_req/4, route_req/5]).

-include_lib("kazoo_sip/include/kzsip_uri.hrl").
-include("ecallmgr.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec route_req(ne_binary(), ne_binary(), kz_proplist(), atom()) -> kz_proplist().
route_req(CallId, FetchId, Props, Node) ->
    route_req(<<>>, CallId, FetchId, Props, Node).

-spec route_req(binary(), ne_binary(), ne_binary(), kz_proplist(), atom()) -> kz_proplist().
route_req(ServerId, CallId, FetchId, Props, Node) ->
    AccountId = kzd_freeswitch:account_id(Props),
    props:filter_empty(
      [{<<"Msg-ID">>, FetchId}
      ,{<<"Call-ID">>, CallId}
      ,{<<"Call-Direction">>, kzd_freeswitch:call_direction(Props)}
      ,{<<"Message-ID">>, props:get_value(<<"Message-ID">>, Props)}
      ,{<<"Caller-ID-Name">>
       ,kzd_freeswitch:caller_id_name(Props, kz_privacy:anonymous_caller_id_name(AccountId))
       }
      ,{<<"Caller-ID-Number">>
       ,kzd_freeswitch:caller_id_number(Props, kz_privacy:anonymous_caller_id_number(AccountId))
       }
      ,{<<"From-Network-Addr">>, kzd_freeswitch:from_network_ip(Props)}
      ,{<<"From-Network-Port">>, kzd_freeswitch:from_network_port(Props)}
      ,{<<"User-Agent">>, kzd_freeswitch:user_agent(Props)}
      ,{<<"To">>, ecallmgr_util:get_sip_to(Props)}
      ,{<<"From">>, ecallmgr_util:get_sip_from(Props)}
      ,{<<"Request">>, ecallmgr_util:get_sip_request(Props)}
      ,{<<"Body">>, get_body(Props) }
      ,{<<"SIP-Request-Host">>, props:get_value(<<"variable_sip_req_host">>, Props)}
      ,{<<"Switch-Nodename">>, kz_term:to_binary(Node)}
      ,{<<"Switch-Hostname">>, kzd_freeswitch:hostname(Props)}
      ,{<<"Switch-URL">>, kzd_freeswitch:switch_url(Props)}
      ,{<<"Switch-URI">>, kzd_freeswitch:switch_uri(Props)}
      ,{<<"Custom-Channel-Vars">>, kz_json:from_list(route_req_ccvs(FetchId, Props))}
      ,{<<"Custom-SIP-Headers">>, kz_json:from_list(ecallmgr_util:custom_sip_headers(Props))}
      ,{<<"Resource-Type">>, kzd_freeswitch:resource_type(Props, <<"audio">>)}
      ,{<<"To-Tag">>, props:get_value(<<"variable_sip_to_tag">>, Props)}
      ,{<<"From-Tag">>, props:get_value(<<"variable_sip_from_tag">>, Props)}
      ,{<<"Custom-Routing-Headers">>, props:get_value(<<"Custom-Routing-Headers">>, Props)}
      ,{<<"Remote-SDP">>, props:get_value(<<"variable_switch_r_sdp">>, Props)}
      ,{<<"DTMF-Type">>, props:get_value(<<"variable_switch_r_sdp">>, Props, <<"101 telephone-event">>)}
       | kz_api:default_headers(ServerId, ?APP_NAME, ?APP_VERSION)
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
       | props:delete_keys([<<?CALL_INTERACTION_ID>>
                           ,<<"Fetch-ID">>
                           ], CCVs) ++ kz_privacy:flags(Props)
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
