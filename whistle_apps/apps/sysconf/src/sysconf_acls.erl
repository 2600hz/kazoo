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
-type acls() :: wh_json:json_object().

build(Node) ->
    Routines = [fun offnet_resources/1
                ,fun local_resources/1
                ,fun sip_auth_ips/1
               ],
    lists:foldl(fun(F, J) -> F(J) end, system_config_acls(Node), Routines).

-spec system_config_acls/1 :: (ne_binary()) -> acls().
system_config_acls(Node) ->
    whapps_config:get(<<"ecallmgr">>, <<"acls">>, wh_json:new(), Node).

-spec sip_auth_ips/1 :: (acls()) -> acls().
sip_auth_ips(ACLs) ->
    ViewOptions = [],
    case couch_mgr:get_results(?WH_SIP_DB, <<"credentials/lookup_by_ip">>, ViewOptions) of
        {error, _R} ->
            lager:debug("Unable to get view results for sip_auth resources: ~p", [_R]),
            ACLs;
        {ok, JObjs} ->
            lists:foldr(fun handle_sip_auth_result/2, ACLs, JObjs)
    end.

-spec handle_sip_auth_result/2 :: (wh_json:json_object(), acls()) -> acls().
handle_sip_auth_result(JObj, ACLs) ->
    IPs = resolve_ip(wh_json:get_value(<<"key">>, JObj), []),
    AccountId = wh_json:get_value([<<"value">>, <<"account_id">>], JObj),
    AuthorizingId = wh_json:get_value(<<"id">>, JObj),
    AuthorizingType = wh_json:get_value([<<"value">>, <<"authorizing_type">>], JObj),    
    add_trusted_objects(IPs, AccountId, AuthorizingId, AuthorizingType, ACLs).

-spec local_resources/1 :: (acls()) -> acls().
local_resources(ACLs) ->
    ViewOptions = [{include_docs, true}],
    case couch_mgr:get_results(?WH_SIP_DB, <<"resources/listing_active_by_weight">>, ViewOptions) of
        {error, _R} ->
            lager:debug("Unable to get view results for local resources: ~p", [_R]),
            ACLs;
        {ok, JObjs} ->
            lists:foldr(fun handle_resource_result/2, ACLs, JObjs)
    end.

-spec offnet_resources/1 :: (acls()) -> acls().
offnet_resources(ACLs) ->
    ViewOptions = [{include_docs, true}],
    case couch_mgr:get_results(?WH_OFFNET_DB, <<"resources/listing_active_by_weight">>, ViewOptions) of
        {error, _R} ->
            lager:debug("Unable to get view results for offnet resources: ~p", [_R]),
            ACLs;
        {ok, JObjs} ->
            lists:foldr(fun handle_resource_result/2, ACLs, JObjs)
    end.

-spec handle_resource_result/2 :: (wh_json:json_object(), acls()) -> acls().
handle_resource_result(JObj, ACLs) ->
    IPs = resource_ips(wh_json:get_value(<<"doc">>, JObj)),
    AuthorizingId = wh_json:get_value(<<"id">>, JObj),
    add_trusted_objects(IPs, undefined, AuthorizingId, <<"resource">>, ACLs).

-spec resource_ips/1 :: (acls()) -> acls().
resource_ips(JObj) ->
    Routines = [fun resource_inbound_ips/2
                ,fun resource_server_ips/2
               ],
    lists:foldl(fun(F, I) -> F(JObj, I) end, [], Routines).

-spec resource_inbound_ips/2 :: (wh_json:json_object(), ip_list()) -> ip_list().
resource_inbound_ips(JObj, IPs) ->
    lists:foldl(fun resolve_ip/2, IPs, wh_json:get_value(<<"inbound_ips">>, JObj, [])).    

-spec resource_server_ips/2 :: (wh_json:json_object(), ip_list()) -> ip_list().
resource_server_ips(JObj, IPs) ->
    lists:foldl(fun(Gateway, I) ->
                        case (not wh_json:is_false(<<"enabled">>, Gateway)) of
                            false -> I;
                            true ->  
                                Server = wh_json:get_value(<<"server">>, Gateway),
                                resolve_ip(Server, I)
                        end
                end, IPs, wh_json:get_value(<<"gateways">>, JObj, [])).

-spec resolve_ip/2 :: (ne_binary(), ip_list()) -> ip_list().
resolve_ip(Domain, IPs) ->
    case binary:split(Domain, <<":">>) of
        [D|_] -> maybe_is_ip(D, IPs);
        _ -> maybe_is_ip(Domain, IPs)
    end.

maybe_is_ip(Domain, IPs) ->
    case wh_util:is_ipv4(Domain) of
        true -> [Domain|IPs];
        false -> maybe_resolve_srv_records(Domain, IPs)
    end.

-spec maybe_resolve_srv_records/2 :: (ne_binary(), ip_list()) -> ip_list().
maybe_resolve_srv_records(Domain, IPs) ->
    Lookup = <<"_sip._udp.", Domain/binary>>,
    case inet_res:lookup(wh_util:to_list(Lookup), in, srv) of
        [] -> maybe_resolve_a_records([Domain], IPs);
        SRVs -> maybe_resolve_a_records([D || {_, _, _, D} <- SRVs], IPs)
    end.

-spec maybe_resolve_a_records/2 :: (ne_binary(), ip_list()) -> ip_list().
maybe_resolve_a_records(Domains, IPs) ->
    lists:foldr(fun(Domain, I) ->
                        case wh_util:is_ipv4(Domain) of
                            true -> [Domain|I];
                            false ->
                                D = wh_util:to_list(Domain),
                                resolve_a_record(D, I)
                        end
                end, IPs, Domains).

-spec resolve_a_record/2 :: (ne_binary(), ip_list()) -> ip_list().
resolve_a_record(Domain, IPs) ->
    lists:foldr(fun(IPTuple, I) ->
                        [iptuple_to_binary(IPTuple)|I]
                end, IPs, inet_res:lookup(Domain, in, a)).

iptuple_to_binary({A,B,C,D}) ->
    <<(wh_util:to_binary(A))/binary, "."
      ,(wh_util:to_binary(B))/binary, "."
      ,(wh_util:to_binary(C))/binary, "."
      ,(wh_util:to_binary(D))/binary>>.

-spec add_trusted_objects/5 :: (ip_list(), 'undefined'|ne_binary(), ne_binary(), ne_binary(), acls()) -> acls().
add_trusted_objects([], _, _, _, ACLs) ->
    ACLs;
add_trusted_objects([IP|IPs], AccountId, AuthorizingId, AuthorizingType, ACLs) ->
    Props = [{<<"type">>, <<"allow">>}
             ,{<<"network-list-name">>, <<"trusted">>}
             ,{<<"cidr">>, <<IP/binary, "/32">>}
             ,{<<"account_id">>, AccountId}
             ,{<<"authorizing_id">>, AuthorizingId}
             ,{<<"authorizing_type">>, AuthorizingType}
            ],
    A = wh_json:set_value(IP, wh_json:from_list(props:filter_undefined(Props)), ACLs),
    add_trusted_objects(IPs, AccountId, AuthorizingId, AuthorizingType, A).
