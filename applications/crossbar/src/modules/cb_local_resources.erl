%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% Handle client requests for local resource documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_local_resources).

-export([init/0]).

-export([validate_request/2
         ,maybe_remove_aggregate/2
         ,maybe_aggregate_resources/1
         ,maybe_aggregate_resource/1
         ,maybe_remove_aggregates/1
        ]).

-include("../crossbar.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".local_resources">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = couch_mgr:revise_doc_from_file(?WH_SIP_DB, 'crossbar', "views/resources.json"),
    crossbar_maintenance:start_module('cb_resources').

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_request(api_binary(), cb_context:context()) -> cb_context:context().
validate_request(ResourceId, Context) ->
    check_for_registering_gateways(ResourceId, Context).

-spec check_for_registering_gateways(api_binary(), cb_context:context()) -> cb_context:context().
check_for_registering_gateways(ResourceId, Context) ->
    case lists:any(fun is_registering_gateway/1
                   ,cb_context:req_value(Context, <<"gateways">>, [])
                  )
    of
        'true' ->
            check_if_peer(ResourceId, cb_context:store(Context, 'aggregate_resource', 'true'));
        'false' ->
            check_if_peer(ResourceId, Context)
    end.

-spec is_registering_gateway(wh_json:object()) -> boolean().
is_registering_gateway(Gateway) ->
    wh_json:is_true(<<"register">>, Gateway)
        andalso wh_json:is_true(<<"enabled">>, Gateway).

-spec check_if_peer(api_binary(), cb_context:context()) -> cb_context:context().
check_if_peer(ResourceId, Context) ->
    case {wh_util:is_true(cb_context:req_value(Context, <<"peer">>))
          ,whapps_config:get_is_true(?MOD_CONFIG_CAT, <<"allow_peers">>, 'false')
         }
    of
        {'true', 'true'} ->
            check_if_gateways_have_ip(ResourceId, Context);
        {'true', 'false'} ->
            C = cb_context:add_validation_error(
                    [<<"peer">>]
                    ,<<"forbidden">>
                    ,wh_json:from_list([
                        {<<"message">>, <<"Peers are currently disabled, please contact the system admin">>}
                     ])
                    ,Context
                ),
            check_resource_schema(ResourceId, C);
        {_, _} ->
            check_resource_schema(ResourceId, Context)
    end.

-spec check_if_gateways_have_ip(api_binary(), cb_context:context()) -> cb_context:context().
check_if_gateways_have_ip(ResourceId, Context) ->
    Gateways = cb_context:req_value(Context, <<"gateways">>, []),
    IPs = extract_gateway_ips(Gateways, 0, []),
    SIPAuth = get_all_sip_auth_ips(),
    ACLs = get_all_acl_ips(),
    validate_gateway_ips(IPs, SIPAuth, ACLs, ResourceId, Context, cb_context:resp_status(Context)).

-spec validate_gateway_ips(gateway_ips(), sip_auth_ips(), acl_ips(), api_binary(), cb_context:context(), crossbar_status()) -> cb_context:context().
validate_gateway_ips([], _, _, ResourceId, Context, 'error') ->
    check_resource_schema(ResourceId, Context);
validate_gateway_ips([], _, _, ResourceId, Context, 'success') ->
    check_resource_schema(ResourceId, cb_context:store(Context, 'aggregate_resource', 'true'));
validate_gateway_ips([{Idx, 'undefined', 'undefined'}|IPs], SIPAuth, ACLs, ResourceId, Context, 'success') ->
    C = cb_context:add_validation_error(
            [<<"gateways">>, Idx, <<"server">>]
            ,<<"required">>
            ,wh_json:from_list([
                {<<"message">>, <<"Gateway server must be an IP when peering with the resource">>}
                ,{<<"cause">>, Idx}
             ])
            ,Context
        ),
    validate_gateway_ips(IPs, SIPAuth, ACLs, ResourceId, C, cb_context:resp_status(C));
validate_gateway_ips([{Idx, 'undefined', ServerIP}|IPs], SIPAuth, ACLs, ResourceId, Context, 'success') ->
    case wh_network_utils:is_ipv4(ServerIP) of
        'true' ->
            case validate_ip(ServerIP, SIPAuth, ACLs, ResourceId) of
                'true' ->
                    validate_gateway_ips(IPs, SIPAuth, ACLs, ResourceId, Context, cb_context:resp_status(Context));
                'false' ->
                    C = cb_context:add_validation_error(
                            [<<"gateways">>, Idx, <<"server">>]
                            ,<<"unique">>
                            ,wh_json:from_list([
                                {<<"message">>, <<"Gateway server ip is already in use">>}
                                ,{<<"cause">>, Idx}
                             ])
                            ,Context
                        ),
                    validate_gateway_ips(IPs, SIPAuth, ACLs, ResourceId, C, cb_context:resp_status(C))
            end;
        'false' ->
            validate_gateway_ips([{Idx, 'undefined', 'undefined'}|IPs], SIPAuth, ACLs, ResourceId, Context, cb_context:resp_status(Context))
    end;
validate_gateway_ips([{Idx, InboundIP, ServerIP}|IPs], SIPAuth, ACLs, ResourceId, Context, 'success') ->
    case wh_network_utils:is_ipv4(InboundIP) of
        'true' ->
            case validate_ip(InboundIP, SIPAuth, ACLs, ResourceId) of
                'true' ->
                    validate_gateway_ips(IPs, SIPAuth, ACLs, ResourceId, Context, cb_context:resp_status(Context));
                'false' ->
                    C = cb_context:add_validation_error([
                            <<"gateways">>, Idx, <<"inbound_ip">>]
                            ,<<"unique">>
                            ,wh_json:from_list([
                                {<<"message">>, <<"Gateway inbound ip is already in use">>}
                                ,{<<"cause">>, Idx}
                             ])
                            ,Context
                        ),
                    validate_gateway_ips(IPs, SIPAuth, ACLs, ResourceId, C, cb_context:resp_status(C))
            end;
        'false' ->
            validate_gateway_ips([{Idx, 'undefined', ServerIP}|IPs], SIPAuth, ACLs, ResourceId, Context, cb_context:resp_status(Context))
    end.

-spec check_resource_schema(api_binary(), cb_context:context()) -> cb_context:context().
check_resource_schema(ResourceId, Context) ->
    %% This has been validated in cb_resources already!
    on_successful_validation(ResourceId, Context).

-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context, wh_doc:set_type(cb_context:doc(Context), <<"resource">>));
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_aggregate_resource(cb_context:context()) -> boolean().
-spec maybe_aggregate_resource(cb_context:context(), crossbar_status()) -> boolean().
maybe_aggregate_resource(Context) ->
    maybe_aggregate_resource(Context, cb_context:resp_status(Context)).

maybe_aggregate_resource(Context, 'success') ->
    case wh_util:is_true(cb_context:fetch(Context, 'aggregate_resource')) of
        'false' ->
            ResourceId = wh_doc:id(cb_context:doc(Context)),
            maybe_remove_aggregate(ResourceId, Context);
        'true' ->
            lager:debug("adding resource to the sip auth aggregate"),
            couch_mgr:ensure_saved(?WH_SIP_DB, wh_doc:delete_revision(cb_context:doc(Context))),
            _ = wapi_switch:publish_reload_gateways(),
            _ = wapi_switch:publish_reload_acls(),
            'true'
    end;
maybe_aggregate_resource(_Context, _Status) -> 'false'.

-spec maybe_remove_aggregate(ne_binary(), cb_context:context()) -> boolean().
-spec maybe_remove_aggregate(ne_binary(), cb_context:context(), crossbar_status()) -> boolean().

maybe_remove_aggregate(ResourceId, Context) ->
    maybe_remove_aggregate(ResourceId, Context, cb_context:resp_status(Context)).

maybe_remove_aggregate(ResourceId, _Context, 'success') ->
    case couch_mgr:open_doc(?WH_SIP_DB, ResourceId) of
        {'ok', JObj} ->
            couch_mgr:del_doc(?WH_SIP_DB, JObj),
            _ = wapi_switch:publish_reload_gateways(),
            _ = wapi_switch:publish_reload_acls(),
            'true';
        {'error', 'not_found'} -> 'false'
    end;
maybe_remove_aggregate(_ResourceId, _Context, _Status) -> 'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-type sip_auth_ip() :: {ne_binary(), ne_binary()}.
-type sip_auth_ips() :: [sip_auth_ip(),...] | [].

-spec get_all_sip_auth_ips() -> sip_auth_ips().
get_all_sip_auth_ips() ->
    ViewOptions = [],
    case couch_mgr:get_results(?WH_SIP_DB, <<"credentials/lookup_by_ip">>, ViewOptions) of
        {'ok', JObjs} ->
            lists:foldr(fun(JObj, IPs) ->
                                IP = wh_json:get_value(<<"key">>, JObj),
                                ID = wh_doc:id(JObj),
                                [{IP, ID}|IPs]
                        end, [], JObjs);
        {'error', _} -> []
    end.

-type acl_ips() :: ne_binaries().
-spec get_all_acl_ips() -> acl_ips().
get_all_acl_ips() ->
    Req = [{<<"Category">>, <<"ecallmgr">>}
           ,{<<"Key">>, <<"acls">>}
           ,{<<"Node">>, <<"all">>}
           ,{<<"Default">>, wh_json:new()}
           ,{<<"Msg-ID">>, wh_util:rand_hex_binary(16)}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    Resp = whapps_util:amqp_pool_request(
             props:filter_undefined(Req)
             ,fun wapi_sysconf:publish_get_req/1
             ,fun wapi_sysconf:get_resp_v/1
            ),
    case Resp of
        {'error', _} -> [];
        {'ok', JObj} ->
            extract_all_ips(wh_json:get_value(<<"Value">>, JObj, wh_json:new()))
    end.

-spec extract_all_ips(wh_json:object()) -> acl_ips().
extract_all_ips(JObj) ->
    lists:foldr(fun(K, IPs) ->
                        case wh_json:get_value([K, <<"cidr">>], JObj) of
                            'undefined' -> IPs;
                            CIDR ->
                                AuthorizingId = wh_json:get_value([K, <<"authorizing_id">>], JObj),
                                [{CIDR, AuthorizingId} | IPs]
                        end
                end, [], wh_json:get_keys(JObj)).

-type gateway_ip() :: {non_neg_integer(), api_binary(), api_binary()}.
-type gateway_ips() :: [gateway_ip(),...] | [].
-spec extract_gateway_ips(wh_json:objects(), non_neg_integer(), gateway_ips()) -> gateway_ips().
extract_gateway_ips([], _, IPs) -> IPs;
extract_gateway_ips([Gateway|Gateways], Idx, IPs) ->
    IP = {Idx
          ,wh_json:get_ne_value(<<"inbound_ip">>, Gateway)
          ,wh_json:get_ne_value(<<"server">>, Gateway)
         },
    extract_gateway_ips(Gateways, Idx + 1, [IP|IPs]).

-spec validate_ip(api_binary(), sip_auth_ips(), acl_ips(), api_binary()) -> boolean().
validate_ip(IP, SIPAuth, ACLs, ResourceId) ->
    lists:all(fun({CIDR, AuthId}) ->
                      AuthId =:= ResourceId
                          orelse not (wh_network_utils:verify_cidr(IP, CIDR))
              end, ACLs)
        andalso lists:all(fun({AuthIp, Id}) ->
                                  IP =/= AuthIp
                                      orelse ResourceId =:= Id
                          end, SIPAuth).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_aggregate_resources(wh_json:objects()) -> 'ok'.
maybe_aggregate_resources([]) -> 'ok';
maybe_aggregate_resources([Resource|Resources]) ->
    case lists:any(fun(Gateway) ->
                           wh_json:is_true(<<"register">>, Gateway)
                               andalso
                                 (not wh_json:is_false(<<"enabled">>, Gateway))
                   end, wh_json:get_value(<<"gateways">>, Resource, []))
    of
        'true' ->
            lager:debug("adding resource to the sip auth aggregate"),
            couch_mgr:ensure_saved(?WH_SIP_DB, wh_json:delete_key(<<"_rev">>, Resource)),
            _ = wapi_switch:publish_reload_gateways(),
            _ = wapi_switch:publish_reload_acls(),
            maybe_aggregate_resources(Resources);
        'false' ->
            _ = maybe_remove_aggregates([Resource]),
            maybe_aggregate_resources(Resources)
    end.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_remove_aggregates(wh_json:objects()) -> 'ok'.
maybe_remove_aggregates([]) -> 'ok';
maybe_remove_aggregates([Resource|Resources]) ->
    case couch_mgr:open_doc(?WH_SIP_DB, wh_doc:id(Resource)) of
        {'ok', JObj} ->
            couch_mgr:del_doc(?WH_SIP_DB, JObj),
            _ = wapi_switch:publish_reload_gateways(),
            _ = wapi_switch:publish_reload_acls(),
            maybe_remove_aggregates(Resources);
        {'error', 'not_found'} ->
            maybe_remove_aggregates(Resources)
    end.
