%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(sysconf_acls).

-export([build/1]).

-include("sysconf.hrl").

-type ip_list() :: [ne_binary(),...] | [].
-type acls() :: wh_json:object().

build(Node) ->
    Routines = [fun offnet_resources/1
                ,fun local_resources/1
                ,fun sip_auth_ips/1
               ],
    lists:foldl(fun(F, J) -> F(J) end, system_config_acls(Node), Routines).

-spec system_config_acls(ne_binary()) -> acls().
system_config_acls(Node) ->
    whapps_config:get(<<"ecallmgr">>, <<"acls">>, wh_json:new(), Node).

-spec sip_auth_ips(acls()) -> acls().
sip_auth_ips(ACLs) ->
    ViewOptions = [],
    case couch_mgr:get_results(?WH_SIP_DB, <<"credentials/lookup_by_ip">>, ViewOptions) of
        {error, _R} ->
            lager:debug("Unable to get view results for sip_auth resources: ~p", [_R]),
            ACLs;
        {ok, JObjs} ->
            lists:foldr(fun handle_sip_auth_result/2, ACLs, JObjs)
    end.

-spec handle_sip_auth_result(wh_json:object(), acls()) -> acls().
handle_sip_auth_result(JObj, ACLs) ->
    IPs = wh_network_utils:resolve(wh_json:get_value(<<"key">>, JObj)),
    AccountId = wh_json:get_value([<<"value">>, <<"account_id">>], JObj),
    AuthorizingId = wh_json:get_value(<<"id">>, JObj),
    AuthorizingType = wh_json:get_value([<<"value">>, <<"authorizing_type">>], JObj),    
    ACL2  = add_acl_objects(IPs, AccountId, AuthorizingId, AuthorizingType,
			    <<"trusted">>, ACLs),
    add_acl_objects(IPs, AccountId, AuthorizingId, AuthorizingType,
		    <<"authoratative">>, ACL2).
-spec local_resources(acls()) -> acls().
local_resources(ACLs) ->
    ViewOptions = [{include_docs, true}],
    case couch_mgr:get_results(?WH_SIP_DB, <<"resources/listing_active_by_weight">>, ViewOptions) of
        {error, _R} ->
            lager:debug("Unable to get view results for local resources: ~p", [_R]),
            ACLs;
        {ok, JObjs} ->
            lists:foldr(fun handle_resource_result/2, ACLs, JObjs)
    end.

-spec offnet_resources(acls()) -> acls().
offnet_resources(ACLs) ->
    ViewOptions = [{include_docs, true}],
    case couch_mgr:get_results(?WH_OFFNET_DB, <<"resources/listing_active_by_weight">>, ViewOptions) of
        {error, _R} ->
            lager:debug("Unable to get view results for offnet resources: ~p", [_R]),
            ACLs;
        {ok, JObjs} ->
            lists:foldr(fun handle_resource_result/2, ACLs, JObjs)
    end.

-spec handle_resource_result(wh_json:object(), acls()) -> acls().
handle_resource_result(JObj, ACLs) ->
    IPs = resource_ips(wh_json:get_value(<<"doc">>, JObj)),
    AuthorizingId = wh_json:get_value(<<"id">>, JObj),
    add_acl_objects(IPs, undefined, AuthorizingId, <<"resource">>, 
		    <<"trusted">>, ACLs).

-spec resource_ips(acls()) -> acls().
resource_ips(JObj) ->
    Routines = [fun resource_inbound_ips/2
                ,fun resource_server_ips/2
               ],
    IPs = lists:foldl(fun(F, I) -> F(JObj, I) end, [], Routines),
    lists:flatten(IPs).

-spec resource_inbound_ips(wh_json:object(), ip_list()) -> ip_list().
resource_inbound_ips(JObj, IPs) ->
    lists:foldl(fun(Address, I) ->
                        [wh_network_utils:resolve(Address)|I]
                end, IPs, wh_json:get_value(<<"inbound_ips">>, JObj, [])).    

-spec resource_server_ips(wh_json:object(), ip_list()) -> ip_list().
resource_server_ips(JObj, IPs) ->
    lists:foldl(fun(Gateway, I) ->
                        case (not wh_json:is_false(<<"enabled">>, Gateway)) of
                            false -> I;
                            true ->  
                                Server = wh_json:get_value(<<"server">>, Gateway),
                                [wh_network_utils:resolve(Server)|I]
                        end
                end, IPs, wh_json:get_value(<<"gateways">>, JObj, [])).

-spec add_acl_objects(ip_list(), 'undefined'|ne_binary(), ne_binary(), ne_binary(), ne_binary() , acls()) -> acls().

add_acl_objects([], _, _, _, _, ACLs) ->
    ACLs;
add_acl_objects([IP|IPs], AccountId, AuthorizingId, AuthorizingType, ListName, ACLs) when ListName == <<"trusted">> orelse ListName == <<"authoratative">> ->
    Props = [{<<"type">>, <<"allow">>}
             ,{<<"network-list-name">>, ListName}
             ,{<<"cidr">>, <<IP/binary, "/32">>}
             ,{<<"account_id">>, AccountId}
             ,{<<"authorizing_id">>, AuthorizingId}
             ,{<<"authorizing_type">>, AuthorizingType}
            ],
    A = wh_json:set_value(IP, wh_json:from_list(props:filter_undefined(Props)), ACLs),
    add_acl_objects(IPs, AccountId, AuthorizingId, AuthorizingType, ListName, A).
