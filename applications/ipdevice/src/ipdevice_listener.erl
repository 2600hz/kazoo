-module(ipdevice_listener).
-behaviour(gen_listener).

-include_lib("whistle/include/wh_types.hrl").
%% API
-export([start_link/0,start/0,init/1,handle_call/3,handle_cast/2,handle_info/2
         ,terminate/2,code_change/3,handle_req/2]).

-record(state,{}).

-define(SERVER,?MODULE).
-define(RESPONDERS, [{{?MODULE,handle_req},[{<<"*">>, <<"*">>}]}]).

-define(BINDINGS, [{'self',[]}]).
-define(QUEUE_NAME, <<"ipdevice">>).
%-define(QUEUE_OPTIONS, [{exclusive, false}]).
%-define(CONSUME_OPTIONS, [{exclusive, false}]).

-spec start() -> 'ok' | {'error', _}.
start() ->
    application:start(ipdevice).

-spec start_link() -> startlink_ret().
start_link() ->
    lager:debug("starting new ipdevice proc"),
    [wh_util:ensure_started(A) || A <- [ 'sasl'
					 ,'whistle_amqp'
					 ,'whistle_couch'
					 ,'ibrowse'
					 ,'lager'
				       ] ],
    gen_listener:start_link(?MODULE
                            ,[{bindings, ?BINDINGS}
                              ,{responders, ?RESPONDERS}
                              ,{queue_name, ?QUEUE_NAME}
                              %,{queue_options, ?QUEUE_OPTIONS}
                              %,{consume_options, ?CONSUME_OPTIONS}
                             ]
                            ,[]).

init([]) ->
    {ok,#state{}}.

handle_call(_Request,_From,State) ->
    {reply,{error,not_implemented},State}.

handle_cast(_Request,State) ->
    {noreply,State}.

handle_info(_Request,State) ->
    {noreply,State}.

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

-spec handle_req(wh_json:object(), wh_proplist()) -> any().
handle_req(JObj,_Props) ->
    lager:debug("request ~p~n",[JObj]),
    MsgId = wh_json:get_value(<<"Msg-ID">>,JObj),
    ServerId = wh_json:get_value(<<"Server-ID">>,JObj),
    case wh_json:get_value(<<"Caller-Network-Addr">>,JObj) of
	'undefined' ->
	    reply_not_ip_device(ServerId,MsgId,<<"no_ip_address">>);
	IP ->
	    case couch_mgr:get_results(<<"sip_auth">>
					   ,<<"credentials/lookup_by_ip">>
					   ,[{key,IP}]) of
		{ok, [SipAuth]} ->
		    check_domain(SipAuth
				 ,ServerId
				 ,MsgId
				 ,JObj
				);
		Err ->
		    lager:debug("no results for sip_auth resources: ~p", [Err]),
		    reply_not_ip_device(MsgId,ServerId,<<"is_carrier">>)
	    end
    end.

-spec check_domain(ne_binary(),ne_binary(),ne_binary(),wh_json:object()) -> 
			  any().
check_domain(SipAuth,ServerId,MsgId,JObj) ->
    AccountId = wh_json:get_value([<<"value">>, <<"account_id">>], SipAuth),
    AcctDB = wh_util:format_account_id(AccountId, encoded),
    Domain = wh_json:get_value(<<"variable_sip_from_host">>,JObj),
    case couch_mgr:get_results(AcctDB,<<"account/listing_by_realm">>,[]) of
	{ok, [AcctJObj]} ->
	    case wh_json:get_value(<<"key">>, AcctJObj) of
		Domain ->
		    DeviceId = wh_json:get_value(<<"id">>, SipAuth),
		    OwnerId = wh_json:get_value([<<"value">>, <<"owner_id">>]
						,SipAuth),
		    NewJObj = update_device_params(JObj
						   ,Domain
						   ,DeviceId
						   ,OwnerId
						   ,AccountId
						  ),
%		    SendJObj = wh_json:from_list([{'account_id',AccountId}
%						  ,{'id',AuthorizingId}
%						  ,{'owner_id',OwnerId}
%						  ,{'domain',Domain}
%						  ,{<<"Msg-ID">>,MsgId}
%						 ]),
		    lager:debug("sending ~p",[NewJObj]),
		    amqp_util:targeted_publish(ServerId,wh_json:encode(NewJObj));
		_ ->
		    reply_not_ip_device(MsgId,ServerId,<<"wrong_domain">>)
	    end;
	_ ->
	    reply_not_ip_device(MsgId,ServerId,<<"db_failure">>)
    end.

-spec update_device_params(wh_json:object(),ne_binary(),ne_binary(),
			   ne_binary(),ne_binary()) ->  wh_json:object().
update_device_params(JObj,Domain,DeviceId,OwnerId, AccountId) ->
    User = wh_json:get_value(<<"variable_sip_from_user">>,JObj),
    JObj2 = remove_trusted_acl_params(JObj),
    wh_json:set_values([{<<"variable_sip_number_alias">>,User}
			,{<<"variable_sip_auth_username">>,User}
			,{<<"variable_sip_auth_realm">>,Domain}
			,{<<"variable_number_alias">>,User}
			,{<<"variable_user_name">>,User}
			,{<<"variable_domain_name">>,Domain}
			,{<<"variable_ecallmgr_Inception">>,<<"on-net">>}
			,{<<"variable_ecallmgr_Owner-ID">>,OwnerId}
			,{<<"variable_ecallmgr_Authorizing-Type">>,<<"device">>}
			,{<<"variable_ecallmgr_Authorizing-ID">>,DeviceId}
			,{<<"variable_ecallmgr_Account-ID">>,AccountId}
			,{<<"variable_ecallmgr_Realm">>,Domain}
			,{<<"variable_ecallmgr_Username">>,User}
		       ],JObj2).

-spec remove_trusted_acl_params(wh_json:object()) -> wh_json:object().
remove_trusted_acl_params(JObj) ->
    wh_json:delete_keys( [ <<"variable_sip_acl_authed_by">>
			       , <<"variable_sip_from_epid">>
			       ,<<"variable_sip_req_params">>
			       ,<<"variable_sip_to_params">>
			       ,<<"variable_sip_contact_params">>
			       ,<<"variable_sip_acl_authed_by">>
			       ,<<"variable_sip_from_epid">>
			 ],JObj).

-spec reply_not_ip_device(ne_binary(),ne_binary(),ne_binary()) -> any().
reply_not_ip_device(MsgId,ServerId,Msg) ->
    lager:debug("sending not ip device: ~s",[Msg]),
    SendJObj = wh_json:from_list([{<<"Msg-ID">>,MsgId}
				  ,{<<"result">>,Msg}
				 ]),
    amqp_util:targeted_publish(ServerId,wh_json:encode(SendJObj)).
    
