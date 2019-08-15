%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Handle client requests for local resource documents
%%% @author Karl Anderson
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_local_resources).

-export([init/0]).

-export([validate_request/2
        ,maybe_remove_aggregate/2
        ,maybe_aggregate_resources/1
        ,maybe_aggregate_resource/1
        ,maybe_remove_aggregates/1
        ]).

-include("crossbar.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".local_resources">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> ok.
init() ->
    _ = crossbar_maintenance:start_module('cb_resources'),
    ok.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_request(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_request(ResourceId, Context) ->
    check_for_registering_gateways(ResourceId, Context).

-spec check_for_registering_gateways(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
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

-spec is_registering_gateway(kz_json:object()) -> boolean().
is_registering_gateway(Gateway) ->
    kz_json:is_true(<<"register">>, Gateway)
        andalso kz_json:is_true(<<"enabled">>, Gateway).

-spec check_if_peer(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_if_peer(ResourceId, Context) ->
    case {kz_term:is_true(cb_context:req_value(Context, <<"peer">>))
         ,kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"allow_peers">>, 'false')
         }
    of
        {'true', 'true'} ->
            check_if_gateways_have_ip(ResourceId, Context);
        {'true', 'false'} ->
            C = cb_context:add_validation_error([<<"peer">>]
                                               ,<<"forbidden">>
                                               ,kz_json:from_list([{<<"message">>, <<"Peers are currently disabled, please contact the system admin">>}])
                                               ,Context
                                               ),
            check_resource_schema(ResourceId, C);
        {_, _} ->
            check_resource_schema(ResourceId, Context)
    end.

-spec check_if_gateways_have_ip(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_if_gateways_have_ip(ResourceId, Context) ->
    Gateways = cb_context:req_value(Context, <<"gateways">>, []),
    IPs = extract_gateway_ips(Gateways, 0, []),
    SIPAuth = get_all_sip_auth_ips(),
    ACLs = get_all_acl_ips(),
    validate_gateway_ips(IPs, SIPAuth, ACLs, ResourceId, Context, cb_context:resp_status(Context)).

-spec validate_gateway_ips(gateway_ips(), sip_auth_ips(), acl_ips(), kz_term:api_binary(), cb_context:context(), crossbar_status()) -> cb_context:context().
validate_gateway_ips([], _, _, ResourceId, Context, 'error') ->
    check_resource_schema(ResourceId, Context);
validate_gateway_ips([], _, _, ResourceId, Context, 'success') ->
    check_resource_schema(ResourceId, cb_context:store(Context, 'aggregate_resource', 'true'));
validate_gateway_ips([{Idx, 'undefined', 'undefined'}|IPs], SIPAuth, ACLs, ResourceId, Context, 'success') ->
    C = cb_context:add_validation_error([<<"gateways">>, Idx, <<"server">>]
                                       ,<<"required">>
                                       ,kz_json:from_list([{<<"message">>, <<"Gateway server must be an IP when peering with the resource">>}
                                                          ,{<<"cause">>, Idx}
                                                          ])
                                       ,Context
                                       ),
    validate_gateway_ips(IPs, SIPAuth, ACLs, ResourceId, C, cb_context:resp_status(C));
validate_gateway_ips([{Idx, 'undefined', ServerIP}|IPs], SIPAuth, ACLs, ResourceId, Context, 'success') ->
    case kz_network_utils:is_ipv4(ServerIP) of
        'true' ->
            case validate_ip(ServerIP, SIPAuth, ACLs, ResourceId) of
                'true' ->
                    validate_gateway_ips(IPs, SIPAuth, ACLs, ResourceId, Context, cb_context:resp_status(Context));
                'false' ->
                    C = cb_context:add_validation_error([<<"gateways">>, Idx, <<"server">>]
                                                       ,<<"unique">>
                                                       ,kz_json:from_list([{<<"message">>, <<"Gateway server ip is already in use">>}
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
    case kz_network_utils:is_ipv4(InboundIP) of
        'true' ->
            case validate_ip(InboundIP, SIPAuth, ACLs, ResourceId) of
                'true' ->
                    validate_gateway_ips(IPs, SIPAuth, ACLs, ResourceId, Context, cb_context:resp_status(Context));
                'false' ->
                    C = cb_context:add_validation_error([<<"gateways">>, Idx, <<"inbound_ip">>]
                                                       ,<<"unique">>
                                                       ,kz_json:from_list([{<<"message">>, <<"Gateway inbound ip is already in use">>}
                                                                          ,{<<"cause">>, Idx}
                                                                          ])
                                                       ,Context
                                                       ),
                    validate_gateway_ips(IPs, SIPAuth, ACLs, ResourceId, C, cb_context:resp_status(C))
            end;
        'false' ->
            validate_gateway_ips([{Idx, 'undefined', ServerIP}|IPs], SIPAuth, ACLs, ResourceId, Context, cb_context:resp_status(Context))
    end.

-spec check_resource_schema(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_resource_schema(ResourceId, Context) ->
    %% This has been validated in cb_resources already!
    on_successful_validation(ResourceId, Context).

-spec on_successful_validation(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context, kz_doc:set_type(cb_context:doc(Context), <<"resource">>));
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context, ?TYPE_CHECK_OPTION(<<"resource">>)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec maybe_aggregate_resource(cb_context:context()) -> boolean().
maybe_aggregate_resource(Context) ->
    maybe_aggregate_resource(Context, cb_context:resp_status(Context)).

-spec maybe_aggregate_resource(cb_context:context(), crossbar_status()) -> boolean().
maybe_aggregate_resource(Context, 'success') ->
    ResourceId = kz_doc:id(cb_context:doc(Context)),
    case kz_term:is_true(cb_context:fetch(Context, 'aggregate_resource')) of
        'false' ->
            maybe_remove_aggregate(ResourceId, Context);
        'true' ->
            aggregate_resource(kz_doc:set_id(cb_context:doc(Context), ResourceId)),
            _ = kapi_switch:publish_reload_gateways(),
            _ = kapi_switch:publish_reload_acls(),
            'true'
    end;
maybe_aggregate_resource(_Context, _Status) -> 'false'.

-spec aggregate_resource(kz_json:object()) -> 'ok'.
aggregate_resource(Resource) ->
    lager:debug("adding resource to the sip auth aggregate"),
    Doc = kz_doc:delete_revision(Resource),
    Update = kz_json:to_proplist(kz_json:flatten(Doc)),
    UpdateOptions = [{'update', Update}
                    ,{'create', []}
                    ,{'ensure_saved', 'true'}
                    ],
    {'ok', _} = kz_datamgr:update_doc(?KZ_SIP_DB, kz_doc:id(Resource), UpdateOptions),
    'ok'.

-spec maybe_remove_aggregate(kz_term:ne_binary(), cb_context:context()) -> boolean().
maybe_remove_aggregate(ResourceId, Context) ->
    maybe_remove_aggregate(ResourceId, Context, cb_context:resp_status(Context)).

-spec maybe_remove_aggregate(kz_term:ne_binary(), cb_context:context(), crossbar_status()) -> boolean().
maybe_remove_aggregate(ResourceId, _Context, 'success') ->
    case kz_datamgr:del_doc(?KZ_SIP_DB, ResourceId) of
        {'ok', _JObj} ->
            _ = kapi_switch:publish_reload_gateways(),
            _ = kapi_switch:publish_reload_acls(),
            'true';
        {'error', 'not_found'} -> 'false'
    end;
maybe_remove_aggregate(_ResourceId, _Context, _Status) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-type sip_auth_ip() :: {kz_term:ne_binary(), kz_term:ne_binary()}.
-type sip_auth_ips() :: [sip_auth_ip()].

-spec get_all_sip_auth_ips() -> sip_auth_ips().
get_all_sip_auth_ips() ->
    ViewOptions = [],
    case kz_datamgr:get_results(?KZ_SIP_DB, <<"credentials/lookup_by_ip">>, ViewOptions) of
        {'ok', JObjs} -> lists:foldr(fun get_sip_auth_ip/2, [], JObjs);
        {'error', _} -> []
    end.

-spec get_sip_auth_ip(kz_json:object(), sip_auth_ips()) -> sip_auth_ips().
get_sip_auth_ip(JObj, IPs) ->
    [{kz_json:get_value(<<"key">>, JObj), kz_doc:id(JObj)} | IPs].

-type acl_ips() :: kz_term:ne_binaries().
-spec get_all_acl_ips() -> acl_ips().
get_all_acl_ips() ->
    Req = [{<<"Category">>, <<"ecallmgr">>}
          ,{<<"Key">>, <<"acls">>}
          ,{<<"Node">>, <<"all">>}
          ,{<<"Default">>, kz_json:new()}
          ,{<<"Msg-ID">>, kz_binary:rand_hex(16)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    Resp = kz_amqp_worker:call(props:filter_undefined(Req)
                              ,fun kapi_sysconf:publish_get_req/1
                              ,fun kapi_sysconf:get_resp_v/1
                              ),
    case Resp of
        {'error', _} -> [];
        {'ok', JObj} ->
            extract_all_ips(kz_json:get_value(<<"Value">>, JObj, kz_json:new()))
    end.

-spec extract_all_ips(kz_json:object()) -> acl_ips().
extract_all_ips(JObj) ->
    kz_json:foldl(fun extract_ips_fold/3, [], JObj).

-spec extract_ips_fold(kz_json:path(), kz_json:object(), acl_ips()) -> acl_ips().
extract_ips_fold(_K, JObj, IPs) ->
    case kz_json:get_value(<<"cidr">>, JObj) of
        'undefined' -> IPs;
        CIDR ->
            AuthorizingId = kz_json:get_value(<<"authorizing_id">>, JObj),
            [{CIDR, AuthorizingId} | IPs]
    end.

-type gateway_ip() :: {non_neg_integer(), kz_term:api_binary(), kz_term:api_binary()}.
-type gateway_ips() :: [gateway_ip()].
-spec extract_gateway_ips(kz_json:objects(), non_neg_integer(), gateway_ips()) -> gateway_ips().
extract_gateway_ips([], _, IPs) -> IPs;
extract_gateway_ips([Gateway|Gateways], Idx, IPs) ->
    IP = {Idx
         ,kz_json:get_ne_value(<<"inbound_ip">>, Gateway)
         ,kz_json:get_ne_value(<<"server">>, Gateway)
         },
    extract_gateway_ips(Gateways, Idx + 1, [IP|IPs]).

-spec validate_ip(kz_term:api_binary(), sip_auth_ips(), acl_ips(), kz_term:api_binary()) -> boolean().
validate_ip(IP, SIPAuth, ACLs, ResourceId) ->
    lists:all(fun({CIDR, AuthId}) ->
                      AuthId =:= ResourceId
                          orelse not (kz_network_utils:verify_cidr(IP, CIDR))
              end, ACLs)
        andalso lists:all(fun({AuthIp, Id}) ->
                                  IP =/= AuthIp
                                      orelse ResourceId =:= Id
                          end, SIPAuth).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_aggregate_resources(kz_json:objects()) -> 'ok'.
maybe_aggregate_resources([]) -> 'ok';
maybe_aggregate_resources([Resource|Resources]) ->
    case lists:any(fun(Gateway) ->
                           kz_json:is_true(<<"register">>, Gateway)
                               andalso (not kz_json:is_false(<<"enabled">>, Gateway))
                   end
                  ,kz_json:get_list_value(<<"gateways">>, Resource, [])
                  )
    of
        'true' ->
            aggregate_resource(Resource),
            _ = kapi_switch:publish_reload_gateways(),
            _ = kapi_switch:publish_reload_acls(),
            maybe_aggregate_resources(Resources);
        'false' ->
            _ = maybe_remove_aggregates([Resource]),
            maybe_aggregate_resources(Resources)
    end.
%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_remove_aggregates(kz_json:objects()) -> 'ok'.
maybe_remove_aggregates([]) -> 'ok';
maybe_remove_aggregates([Resource|Resources]) ->
    case kz_datamgr:del_doc(?KZ_SIP_DB, kz_doc:id(Resource)) of
        {'ok', _JObj} ->
            _ = kapi_switch:publish_reload_gateways(),
            _ = kapi_switch:publish_reload_acls(),
            maybe_remove_aggregates(Resources);
        {'error', 'not_found'} ->
            maybe_remove_aggregates(Resources)
    end.
