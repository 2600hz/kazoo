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

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,put/1, put/2
         ,post/2
         ,delete/2
        ]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"local_resources/crossbar_listing">>).
-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".local_resources">>).
-define(COLLECTION, <<"collection">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.local_resources">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.local_resources">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.local_resources">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.local_resources">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.local_resources">>, ?MODULE, 'post'),
    crossbar_bindings:bind(<<"*.execute.delete.local_resources">>, ?MODULE, 'delete').

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
allowed_methods(?COLLECTION) ->
    [?HTTP_PUT, ?HTTP_POST, ?HTTP_DELETE];
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
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.

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
validate(Context) ->
    validate_resources(Context, cb_context:req_verb(Context)).

validate_resources(Context, ?HTTP_GET) ->
    summary(Context);
validate_resources(Context, ?HTTP_PUT) ->
    validate_request('undefined', Context).

validate(Context, ?COLLECTION) ->
    cb_context:set_resp_status(Context, 'success');
validate(Context, Id) ->
    validate_resource(Context, Id, cb_context:req_verb(Context)).

validate_resource(Context, Id, ?HTTP_GET) ->
    read(Id, Context);
validate_resource(Context, Id, ?HTTP_POST) ->
    validate_request(Id, Context);
validate_resource(Context, Id, ?HTTP_DELETE) ->
    read(Id, Context).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, ?COLLECTION) ->
    collection_process(Context, cb_context:req_verb(Context));
post(Context, _) ->
    Context1 = crossbar_doc:save(Context),
    _ = maybe_aggregate_resource(Context1),
    Context1.

-spec put(cb_context:context()) -> cb_context:context().
-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context) ->
    Context1 = crossbar_doc:save(Context),
    _ = maybe_aggregate_resource(Context1),
    Context1.

put(Context, ?COLLECTION) ->
    collection_process(Context, cb_context:req_verb(Context)).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, ?COLLECTION) ->
    collection_process(Context, cb_context:req_verb(Context));
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
-spec validate_request(api_binary(), cb_context:context()) -> cb_context:context().
validate_request(ResourceId, Context) ->
    check_for_registering_gateways(ResourceId, Context).

-spec check_for_registering_gateways(api_binary(), cb_context:context()) -> cb_context:context().
check_for_registering_gateways(ResourceId, Context) ->
    case lists:any(fun is_registering_gateway/1
                   ,cb_context:req_value(<<"gateways">>, Context, [])
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
    case {wh_json:is_true(cb_context:req_value(<<"peer">>, Context))
          ,whapps_config:get_is_true(?MOD_CONFIG_CAT, <<"allow_peers">>, 'false')
         }
    of
        {'true', 'true'} ->
            check_if_gateways_have_ip(ResourceId, Context);
        {'true', 'false'} ->
            C = cb_context:add_validation_error([<<"peer">>]
                                                ,<<"forbidden">>
                                                ,<<"Peers are currently disabled, please contact the system admin">>
                                                ,Context
                                               ),
            check_resource_schema(ResourceId, C);
        {_, _} ->
            check_resource_schema(ResourceId, Context)
    end.

-spec check_if_gateways_have_ip(api_binary(), cb_context:context()) -> cb_context:context().
check_if_gateways_have_ip(ResourceId, Context) ->
    Gateways = cb_context:req_value(<<"gateways">>, Context, []),
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
    C = cb_context:add_validation_error([<<"gateways">>, Idx, <<"server">>]
                                        ,<<"required">>
                                        ,<<"Gateway server must be an IP when peering with the resource">>
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
                    C = cb_context:add_validation_error([<<"gateways">>, Idx, <<"server">>]
                                                        ,<<"unique">>
                                                        ,<<"Gateway server ip is already in use">>
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
                    C = cb_context:add_validation_error([<<"gateways">>, Idx, <<"inbound_ip">>]
                                                        ,<<"unique">>
                                                        ,<<"Gateway inbound ip is already in use">>
                                                        ,Context),
                    validate_gateway_ips(IPs, SIPAuth, ACLs, ResourceId, C, cb_context:resp_status(C))
            end;
        'false' ->
            validate_gateway_ips([{Idx, 'undefined', ServerIP}|IPs], SIPAuth, ACLs, ResourceId, Context, cb_context:resp_status(Context))
    end.

-spec check_resource_schema(api_binary(), cb_context:context()) -> cb_context:context().
check_resource_schema(ResourceId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(ResourceId, C) end,
    cb_context:validate_request_data(<<"resources">>, Context, OnSuccess).

-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context
                       ,wh_json:set_value(<<"pvt_type">>, <<"resource">>, cb_context:doc(Context))
                      );
on_successful_validation(Id, Context) ->
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
-spec maybe_aggregate_resource(cb_context:context(), crossbar_status()) -> boolean().
maybe_aggregate_resource(Context) ->
    maybe_aggregate_resource(Context, cb_context:resp_status(Context)).

maybe_aggregate_resource(Context, 'success') ->
    case wh_util:is_true(cb_context:fetch(Context, 'aggregate_resource')) of
        'false' ->
            ResourceId = wh_json:get_value(<<"_id">>, cb_context:doc(Context)),
            maybe_remove_aggregate(ResourceId, Context);
        'true' ->
            lager:debug("adding resource to the sip auth aggregate"),
            couch_mgr:ensure_saved(?WH_SIP_DB, wh_json:delete_key(<<"_rev">>, cb_context:doc(Context))),
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
-spec collection_process(cb_context:context(), ne_binary()) -> cb_context:context().
collection_process(Context, ?HTTP_POST) ->
    ReqData = cb_context:req_data(Context),
    Db = cb_context:account_db(Context),
    Updates = [{wh_json:get_value(<<"id">>, JObj), clean_resource(JObj)} || JObj <- ReqData],
    Ids = props:get_keys(Updates),
    ViewOptions = [{'keys', Ids}
                   ,'include_docs'
                  ],
    case couch_mgr:all_docs(Db, ViewOptions) of
        {'error', _R} ->
            lager:error("could not open ~p in ~p", [Ids, Db]),
            crossbar_util:response('error', <<"failed to open resources">>, Context);
        {'ok', JObjs} ->
            Resources = [update_resource(JObj, Updates) || JObj <- JObjs],
            case couch_mgr:save_docs(Db, Resources) of
                {'error', _R} ->
                    lager:error("failed to update ~p in ~p", [Ids, Db]),
                    crossbar_util:response('error', <<"failed to update resources">>, Context);
                {'ok', _} ->
                    _ = maybe_aggregate_resources(Resources),
                    cb_context:set_resp_data(Context, [clean_resource(Resource) || Resource <- Resources])
            end
    end;
collection_process(Context, ?HTTP_PUT) ->
    ReqData = cb_context:req_data(Context),
    Db = cb_context:account_db(Context),
    Options = [{'type', <<"resource">>}],
    Resources = [wh_doc:update_pvt_parameters(JObj, 'undefined', Options) || JObj <- ReqData],
    case couch_mgr:save_docs(Db, Resources) of
        {'error', _R} ->
            lager:error("failed to create resources"),
            crossbar_util:response('error', <<"failed to create resources">>, Context);
        {'ok', JObjs} ->
            Ids = [wh_json:get_value(<<"id">>, JObj) || JObj <- JObjs],
            ViewOptions = [{'keys', Ids}
                           ,'include_docs'
                          ],
            _ = maybe_aggregate_resources(Resources),
            case couch_mgr:all_docs(Db, ViewOptions) of
                {'error', _R} ->
                    lager:error("could not open ~p in ~p", [Ids, Db]),
                    cb_context:set_resp_data(Context, Ids);
                {'ok', NewResources} ->
                    cb_context:set_resp_data(Context, [clean_resource(Resource) || Resource <- NewResources])
            end
    end;
collection_process(Context, ?HTTP_DELETE) ->
    ReqData = cb_context:req_data(Context),
    Db = cb_context:account_db(Context),
    case couch_mgr:del_docs(Db, ReqData) of
        {'error', _R} ->
            lager:error("failed to delete resources"),
            crossbar_util:response('error', <<"failed to delete resources">>, Context);
        {'ok', JObjs} ->
            _ = maybe_remove_aggregates(ReqData),
            cb_context:set_resp_data(Context, [wh_json:delete_key(<<"rev">>, JObj) || JObj <- JObjs])
    end.

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
    ResourceId = wh_json:get_first_defined([<<"_id">>, <<"id">>], Resource),
    case couch_mgr:open_doc(?WH_SIP_DB, ResourceId) of
        {'ok', JObj} ->
            couch_mgr:del_doc(?WH_SIP_DB, JObj),
            _ = wapi_switch:publish_reload_gateways(),
            _ = wapi_switch:publish_reload_acls(),
            maybe_remove_aggregates(Resources);
        {'error', 'not_found'} ->
            maybe_remove_aggregates(Resources)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec clean_resource(wh_json:object()) -> wh_json:object().
clean_resource(JObj) ->
    case wh_json:get_value(<<"doc">>, JObj) of
        'undefined' ->
            case wh_json:get_value(<<"_id">>, JObj) of
                'undefined' ->
                     JObj1 = wh_doc:public_fields(JObj),
                    wh_json:delete_key(<<"id">>, JObj1);
                Id ->
                    JObj1 = wh_json:set_value(<<"id">>, Id, JObj),
                    wh_doc:public_fields(JObj1)
            end;
        Doc -> clean_resource(Doc)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update_resource(wh_json:object(), wh_proplist()) -> wh_json:object().
update_resource(JObj, Updates) ->
    Doc = wh_json:get_value(<<"doc">>, JObj),
    Id = wh_json:get_value(<<"_id">>, Doc),
    wh_json:merge_recursive([Doc, props:get_value(Id, Updates)]).
