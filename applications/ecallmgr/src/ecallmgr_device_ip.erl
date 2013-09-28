-module(ecallmgr_device_ip).

-behaviour(gen_server).

-export([maybe_spoof_device/1]).

-define(SERVER, ?MODULE).

-include("ecallmgr.hrl").

maybe_spoof_device(FSData) ->
    case props:get_value(<<"variable_sip_acl_authed_by">>,FSData) of
	<<"trusted">> ->
	    IP = props:get_value(<<"Caller-Network-Addr">>,FSData),
	    case wh_cache:peek_local(?ECALLMGR_UTIL_CACHE,IP) of
		{ok, fail} ->
		    FSData;
		{ok, Vals} ->
		    update_device_params(FSData,Vals);
		_ ->
		    CallId = props:get_value(<<"variable_sip_call_id">>,
					     FSData),
		    case maybe_get_device_params(IP,CallId) of
			{ok, Vals2} ->
			    wh_cache:store_local(?ECALLMGR_UTIL_CACHE,IP,
						 Vals2),
			    update_device_params(FSData,Vals2);
			_ ->
			    wh_cache:store_local(?ECALLMGR_UTIL_CACHE,IP,fail),
			    FSData
		    end
	    end;
	_ ->
	    FSData
    end.
	
update_device_params(FSData,Vals) ->
    Domain =  props:get_value(<<"variable_sip_from_host">>,FSData),
    case props:get_value(<<"domain">>,Vals) of
	Domain ->
	    User = props:get_value(<<"variable_sip_from_user">>,FSData),
	    Owner_id = props:get_value(<<"owner_id">>,Vals),
	    Device_id = props:get_value(<<"id">>,Vals),
	    Account_id = props:get_value(<<"account_id">>,Vals),
	    FSData2 = remove_trusted(FSData),
	    [{<<"variable_sip_number_alias">>,User}
	     ,{<<"variable_sip_auth_username">>,User}
	     ,{<<"variable_sip_auth_realm">>,Domain}
	     ,{<<"variable_number_alias">>,User}
	     ,{<<"variable_user_name">>,User}
	     ,{<<"variable_domain_name">>,Domain}
	     ,{<<"variable_ecallmgr_Inception">>,<<"on-net">>}
	     ,{<<"variable_ecallmgr_Owner-ID">>,Owner_id}
	     ,{<<"variable_ecallmgr_Authorizing-Type">>,<<"device">>}
	     ,{<<"variable_ecallmgr_Authorizing-ID">>,Device_id}
	     ,{<<"variable_ecallmgr_Account-ID">>,Account_id}
	     ,{<<"variable_ecallmgr_Realm">>,Domain}
	     ,{<<"variable_ecallmgr_Username">>,User}
	     | FSData2 ];
	_ ->
	    FSData
    end.

remove_trusted(FSData) ->
    Rem = [<<"variable_sip_acl_authed_by">>
	       ,<<"variable_sip_from_epid">>
	       ,<<"variable_sip_req_params">>
	       ,<<"variable_sip_to_params">>
	       ,<<"variable_sip_contact_params">>
	       ,<<"variable_sip_acl_authed_by">>
	       ,<<"variable_sip_from_epid">>
	  ],
    [{K,V} || {K,V} <- FSData, lists:member(K,Rem) == false].

maybe_get_device_params(IP,CallId) ->
    Req = [{<<"Call-ID">>,CallId}
	   ,{<<"IP">>,IP}
	   ,{<<"Msg-ID">>,CallId}
	   |  wh_api:default_headers(?APP_NAME, ?APP_VERSION)
	  ],
    ReqResp = wh_amqp_worker:call(?ECALLMGR_AMQP_POOL
                                  ,props:filter_undefined(Req)
                                  ,fun(Payload) ->
					   P = wh_json:from_list(Payload),
					   P2 = wh_json:encode(P),
					   amqp_util:targeted_publish(
					     <<"deviceip">>,P2)
				   end
				  ,fun wh_amqp_worker:any_resp/1
                                 ),
    lager:debug("device response ~p",[ReqResp]),
    case ReqResp of
	{ok,_} ->
	    ReqResp;
	_ ->
	    fail
    end.
%%    {ok,[{<<"domain">>,<<"fabrikam.com">>},
%%	 {<<"id">>,<<"0581dbb802375ea2004db9d3ce6d2f7d">>},
%%	 {<<"owner_id">>,<<"1dd0185db27d13eee90f7ccbe31fb1fc">>},
%%	 {<<"account_id">>,<<"5b5f9d9deaaec4e4605eeb620c30cd53">>}]};

