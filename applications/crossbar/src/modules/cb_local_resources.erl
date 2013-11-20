%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
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

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,put/1
         ,post/2
         ,delete/2
        ]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"local_resources/crossbar_listing">>).
-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".local_resources">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.local_resources">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"*.resource_exists.local_resources">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"*.validate.local_resources">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"*.execute.put.local_resources">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"*.execute.post.local_resources">>, ?MODULE, post),
    crossbar_bindings:bind(<<"*.execute.delete.local_resources">>, ?MODULE, delete).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> true.
resource_exists(_) -> true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    summary(Context);
validate(#cb_context{req_verb = ?HTTP_PUT}=Context) ->
    validate_request(undefined, Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, Id) ->
    read(Id, Context);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, Id) ->
    validate_request(Id, Context);
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, Id) ->
    read(Id, Context).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _) ->
    Context1 = crossbar_doc:save(Context),
    _ = maybe_aggregate_resource(Context1),
    Context1.

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Context1 = crossbar_doc:save(Context),
    _ = maybe_aggregate_resource(Context1),
    Context1.

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, ResourceId) ->
    Context1 = crossbar_doc:delete(Context),
    _ = maybe_remove_aggregate(ResourceId, Context1),
    Context1.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    crossbar_doc:load(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
validate_request(ResourceId, Context) ->
    check_for_registering_gateways(ResourceId, Context).

check_for_registering_gateways(ResourceId, #cb_context{req_data=JObj}=Context) ->    
    case lists:any(fun(Gateway) ->
                           wh_json:is_true(<<"register">>, Gateway)
                               andalso
                                 (not wh_json:is_false(<<"enabled">>, Gateway))
                   end, wh_json:get_value(<<"gateways">>, JObj, []))
    of
        true ->
            check_if_peer(ResourceId, cb_context:store(aggregate_resource, true, Context));
        false ->
            check_if_peer(ResourceId, Context)
    end.

check_if_peer(ResourceId, #cb_context{req_data=JObj}=Context) ->
    case {wh_json:is_true(<<"peer">>, JObj),
          whapps_config:get_is_true(?MOD_CONFIG_CAT, <<"allow_peers">>, false)}
    of
        {true, true} ->
            check_if_gateways_have_ip(ResourceId, Context);
        {true, false} ->
            C = cb_context:add_validation_error([<<"peer">>]
                                                ,<<"forbidden">>
                                                ,<<"Peers are currently disabled, please contact the system admin">>
                                                 ,Context),
            check_resource_schema(ResourceId, C);
        {_, _} ->
            check_resource_schema(ResourceId, Context)
    end.

check_if_gateways_have_ip(ResourceId, #cb_context{req_data=JObj}=Context) ->
    Gateways = wh_json:get_value(<<"gateways">>, JObj, []),
    IPs = extract_gateway_ips(Gateways, 0, []),
    SIPAuth = get_all_sip_auth_ips(),
    ACLs = get_all_acl_ips(),
    validate_gateway_ips(IPs, SIPAuth, ACLs, ResourceId, Context).

validate_gateway_ips([], _, _, ResourceId, #cb_context{resp_status='error'}=Context) ->
    check_resource_schema(ResourceId, Context);
validate_gateway_ips([], _, _, ResourceId, Context) ->
    check_resource_schema(ResourceId, cb_context:store('aggregate_resource', 'true', Context));
validate_gateway_ips([{Idx, 'undefined', 'undefined'}|IPs], SIPAuth, ACLs, ResourceId, Context) ->
    C = cb_context:add_validation_error([<<"gateways">>, Idx, <<"server">>]
                                        ,<<"required">>
                                        ,<<"Gateway server must be an IP when peering with the resource">>
                                        ,Context),
    validate_gateway_ips(IPs, SIPAuth, ACLs, ResourceId, C);
validate_gateway_ips([{Idx, 'undefined', ServerIP}|IPs], SIPAuth, ACLs, ResourceId, Context) ->
    case wh_network_utils:is_ipv4(ServerIP) of
        'true' ->
            case validate_ip(ServerIP, SIPAuth, ACLs, ResourceId) of
                'true' ->
                    validate_gateway_ips(IPs, SIPAuth, ACLs, ResourceId, Context);
                'false' ->
                    C = cb_context:add_validation_error([<<"gateways">>, Idx, <<"server">>]
                                                        ,<<"unique">>
                                                        ,<<"Gateway server ip is already in use">>
                                                        ,Context),
                    validate_gateway_ips(IPs, SIPAuth, ACLs, ResourceId, C)
            end;
        'false' ->
            validate_gateway_ips([{Idx, 'undefined', 'undefined'}|IPs], SIPAuth, ACLs, ResourceId, Context)
    end;
validate_gateway_ips([{Idx, InboundIP, ServerIP}|IPs], SIPAuth, ACLs, ResourceId, Context) ->
    case wh_network_utils:is_ipv4(InboundIP) of
        'true' ->
            case validate_ip(InboundIP, SIPAuth, ACLs, ResourceId) of
                'true' ->
                    validate_gateway_ips(IPs, SIPAuth, ACLs, ResourceId, Context);
                'false' ->
                    C = cb_context:add_validation_error([<<"gateways">>, Idx, <<"inbound_ip">>]
                                                        ,<<"unique">>
                                                        ,<<"Gateway inbound ip is already in use">>
                                                        ,Context),
                    validate_gateway_ips(IPs, SIPAuth, ACLs, ResourceId, C)
            end;
        'false' ->
            validate_gateway_ips([{Idx, 'undefined', ServerIP}|IPs], SIPAuth, ACLs, ResourceId, Context)
    end.

check_resource_schema(ResourceId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(ResourceId, C) end,
    cb_context:validate_request_data(<<"local_resources">>, Context, OnSuccess).

-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', #cb_context{doc=JObj}=Context) ->
    Context#cb_context{doc=wh_json:set_value(<<"pvt_type">>, <<"resource">>, JObj)};
on_successful_validation(Id, #cb_context{}=Context) ->
    crossbar_doc:load_merge(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_aggregate_resource(cb_context:context()) -> boolean().
maybe_aggregate_resource(#cb_context{resp_status='success', doc=JObj}=Context) ->
    case wh_util:is_true(cb_context:fetch('aggregate_resource', Context)) of
        'false' ->
            ResourceId = wh_json:get_value(<<"_id">>, JObj),
            maybe_remove_aggregate(ResourceId, Context);
        'true' ->
            lager:debug("adding resource to the sip auth aggregate"),
            couch_mgr:ensure_saved(?WH_SIP_DB, wh_json:delete_key(<<"_rev">>, JObj)),
            _ = wapi_switch:publish_reload_gateways(),
            _ = wapi_switch:publish_reload_acls(),
            'true'
    end;
maybe_aggregate_resource(_) -> 'false'.

-spec maybe_remove_aggregate(ne_binary(), cb_context:context()) -> boolean().
maybe_remove_aggregate(ResourceId, #cb_context{resp_status='success'}) ->
    case couch_mgr:open_doc(?WH_SIP_DB, ResourceId) of
        {'ok', JObj} ->
            couch_mgr:del_doc(?WH_SIP_DB, JObj),
            _ = wapi_switch:publish_reload_gateways(),
            _ = wapi_switch:publish_reload_acls(),
            'true';
        {'error', 'not_found'} -> 'false'
    end;
maybe_remove_aggregate(_, _) -> 'false'.

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
                                ID = wh_json:get_value(<<"id">>, JObj),
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
                                [{CIDR, AuthorizingId}
                                 |IPs
                                ]
                        end
                end, [], wh_json:get_keys(JObj)).

-type gateway_ip() :: {non_neg_integer(), api_binary(), api_binary()}.
-type gateway_ips() :: [gateway_ip(),...] | [].
-spec extract_gateway_ips(wh_json:objects(), non_neg_integer(), gateway_ips()) -> gateway_ips().
extract_gateway_ips([], _, IPs) -> IPs;
extract_gateway_ips([Gateway|Gateways], Idx, IPs) ->
    IP = {Idx
          ,wh_json:get_ne_value(<<"inbound_ip">>, Gateway)
          ,wh_json:get_ne_value(<<"server">>, Gateway)},
    extract_gateway_ips(Gateways, Idx + 1, [IP|IPs]).

-spec validate_ip(api_binary(), sip_auth_ips(), acl_ips(), ne_binary()) -> boolean().
validate_ip(IP, SIPAuth, ACLs, ResourceId) ->
    lists:all(fun({CIDR, AuthId}) ->
                      AuthId =:= ResourceId
                          orelse not (wh_network_utils:verify_cidr(IP, CIDR))
              end, ACLs)
        andalso lists:all(fun({AuthIp, Id}) ->
                                  IP =/= AuthIp
                                      orelse ResourceId =:= Id
                          end, SIPAuth).
