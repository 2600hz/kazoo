%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Devices module
%%%
%%% Handle client requests for device documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_devices).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,billing/1
         ,validate/1, validate/2
         ,put/1
         ,post/2
         ,delete/2
         ,reconcile_services/1
         ,is_ip_acl_unique/1
        ]).

-include_lib("crossbar/include/crossbar.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".devices">>).

-define(CB_LIST, <<"devices/crossbar_listing">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.devices">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.devices">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.billing">>, ?MODULE, billing),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.devices">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.devices">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.devices">>, ?MODULE, post),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.delete.devices">>, ?MODULE, delete),
    crossbar_bindings:bind(<<"v1_resource.finish_request.*.devices">>, ?MODULE, reconcile_services).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/0 :: () -> http_methods().
-spec allowed_methods/1 :: (path_token()) -> http_methods().
allowed_methods() ->
    ['GET', 'PUT'].
allowed_methods(<<"status">>) ->
    ['GET'];
allowed_methods(_) ->
    ['GET', 'POST', 'DELETE'].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/0 :: () -> 'true'.
-spec resource_exists/1 :: (path_token()) -> 'true'.
resource_exists() -> true.
resource_exists(_) -> true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Ensure we will be able to bill for devices
%% @end
%%--------------------------------------------------------------------
billing(#cb_context{req_nouns=[{<<"devices">>, _}|_], req_verb = <<"get">>}=Context) ->
    Context;
billing(#cb_context{req_nouns=[{<<"devices">>, _}|_], account_id=AccountId}=Context) ->
    try wh_services:allow_updates(AccountId) of
        true -> Context
    catch
        throw:{Error, Reason} ->
            crossbar_util:response(error, wh_util:to_binary(Error), 500, Reason, Context)
    end;
billing(Context) -> Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Bill for devices
%% @end
%%--------------------------------------------------------------------
-spec reconcile_services/1 :: (#cb_context{}) -> #cb_context{}.
reconcile_services(#cb_context{req_verb = <<"get">>}=Context) ->
    Context;
reconcile_services(#cb_context{account_id=AccountId}=Context) ->
    _ = wh_services:reconcile(AccountId, <<"devices">>),
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
-spec validate/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = <<"get">>}=Context) ->
    load_device_summary(Context);
validate(#cb_context{req_verb = <<"put">>}=Context) ->
    validate_request(undefined, Context).

validate(#cb_context{req_verb = <<"get">>}=Context, <<"status">>) ->
    load_device_status(Context);
validate(#cb_context{req_verb = <<"get">>}=Context, DeviceId) ->
    load_device(DeviceId, Context);
validate(#cb_context{req_verb = <<"post">>}=Context, DeviceId) ->
    validate_request(DeviceId, Context);
validate(#cb_context{req_verb = <<"delete">>}=Context, DeviceId) ->
    load_device(DeviceId, Context).

-spec post/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
post(#cb_context{}=Context, _DeviceId) ->
    Context1 = crossbar_doc:save(Context),
    _ = maybe_aggregate_device(Context1),
    _ = maybe_provision(Context1),
    Context1.

-spec put/1 :: (#cb_context{}) -> #cb_context{}.
put(#cb_context{}=Context) ->
    Context1 = crossbar_doc:save(Context),
    _ = maybe_aggregate_device(Context1),
    _ = maybe_provision(Context1),
    Context1.

-spec delete/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
delete(#cb_context{}=Context, _DeviceId) ->
    Context1 = crossbar_doc:delete(Context),
    _ = maybe_remove_aggreate(Context),
    Context1.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec load_device_summary/1 :: (#cb_context{}) -> #cb_context{}.
load_device_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec validate_request/2 :: ('undefined'|ne_binary(), #cb_context{}) -> #cb_context{}.
validate_request(DeviceId, Context) ->
    prepare_outbound_flags(DeviceId, Context).

prepare_outbound_flags(DeviceId, #cb_context{req_data=JObj}=Context) -> 
    J = case wh_json:get_value(<<"outbound_flags">>, JObj) of
            [] -> JObj;
            Flags when is_list(Flags) ->
                OutboundFlags = [wh_util:strip_binary(Flag) 
                                 || Flag <- Flags
                                ],
                wh_json:set_value(<<"outbound_flags">>, OutboundFlags, JObj);
            _Else ->
                wh_json:set_value(<<"outbound_flags">>, [], JObj)
        end,
    prepare_device_realm(DeviceId, Context#cb_context{req_data=J}).

prepare_device_realm(DeviceId, #cb_context{req_data=JObj}=Context) ->
    AccountRealm = crossbar_util:get_account_realm(Context),
    Realm = wh_json:get_ne_value([<<"sip">>, <<"realm">>], JObj, AccountRealm),   
    case AccountRealm =:= Realm of
        true -> 
            J = wh_json:delete_key([<<"sip">>, <<"realm">>], JObj),
            validate_device_creds(Realm, DeviceId, Context#cb_context{req_data=J});
        false -> 
            validate_device_creds(Realm, DeviceId, cb_context:store(aggregate_device, true, Context))
    end.

validate_device_creds(Realm, DeviceId, #cb_context{req_data=JObj}=Context) ->
    case wh_json:get_value([<<"sip">>, <<"method">>], JObj, <<"password">>) of
        <<"password">> -> validate_device_password(Realm, DeviceId, Context);
        <<"ip">> -> validate_device_ip(DeviceId, Context);
        _Else ->
            C = cb_context:add_validation_error([<<"sip">>, <<"method">>]
                                                   ,<<"enum">>
                                                   ,<<"SIP authentication method is invalid">>
                                                   ,Context),
            check_device_schema(DeviceId, C)
    end.

validate_device_password(Realm, DeviceId, #cb_context{db_name=Db, req_data=JObj}=Context) ->
    Username = wh_json:get_ne_value([<<"sip">>, <<"username">>], JObj),
    case is_sip_creds_unique(Db, Realm, Username, DeviceId) of
        true -> check_device_schema(DeviceId, Context);
        false ->
            C = cb_context:add_validation_error([<<"sip">>, <<"username">>]
                                                   ,<<"unique">>
                                                   ,<<"SIP credentials already in use">>
                                                   ,Context),
            check_device_schema(DeviceId, C)
    end.

validate_device_ip(DeviceId, #cb_context{req_data=JObj}=Context) ->
    IP = wh_json:get_value([<<"sip">>, <<"ip">>], JObj),
    case wh_util:is_ipv4(IP) of
        true -> validate_device_ip(IP, DeviceId, Context);
        false ->
            C = cb_context:add_validation_error([<<"sip">>, <<"ip">>]
                                                   ,<<"format">>
                                                   ,<<"SIP IP must be a public IP4 address">>
                                                   ,Context),
            check_device_schema(DeviceId, C)
    end.

validate_device_ip(IP, DeviceId, Context) ->
    case is_ip_unique(IP, DeviceId) of
        true -> 
            check_device_schema(DeviceId, cb_context:store(aggregate_device, true, Context));
        false ->
            C = cb_context:add_validation_error([<<"sip">>, <<"ip">>]
                                                   ,<<"unique">>
                                                   ,<<"SIP IP already in use">>
                                                   ,Context),
            check_device_schema(DeviceId, C)            
    end.

check_device_schema(DeviceId, Context) ->
    C = cb_context:validate_request_data(<<"devices">>, Context),
    finalize_request_validation(DeviceId, C).

finalize_request_validation(undefined, #cb_context{doc=Doc}=Context) ->
    Props = [{<<"pvt_type">>, <<"device">>}],
    Context#cb_context{doc=wh_json:set_values(Props, Doc)};
finalize_request_validation(DeviceId, #cb_context{doc=JObj}=Context) -> 
    crossbar_doc:load_merge(DeviceId, JObj, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a device document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_device/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
load_device(DeviceId, Context) ->
    crossbar_doc:load(DeviceId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieve the status of the devices linked to the account
%% Reads registered devices in registrations, then map to devices of the account
%% @end
%%--------------------------------------------------------------------
-spec load_device_status/1 :: (#cb_context{}) -> #cb_context{}.
load_device_status(Context) ->
    AccountRealm = crossbar_util:get_account_realm(Context),
    crossbar_util:response(lookup_regs(AccountRealm), Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results/2 :: (wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns the complete list of registrations in a the first registrar
%% to respond for a given account realm.  This is not 100% accurate
%% as an endpoint might be stored in another registrar, but it is
%% accurate enough for the status icons.
%% @end
%%--------------------------------------------------------------------
-spec lookup_regs/1 :: (ne_binary()) -> wh_json:json_objects().
lookup_regs(AccountRealm) ->
    Q = amqp_util:new_queue(),
    ok = amqp_util:bind_q_to_targeted(Q),
    ok = amqp_util:basic_consume(Q),
    Req = [{<<"Realm">>, AccountRealm}
           ,{<<"Fields">>, [<<"Authorizing-ID">>]}
           | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
          ],
    wapi_registration:publish_query_req(Req),
    receive
        {_, #amqp_msg{payload = Payload}} ->
            JObj = wh_json:decode(Payload),
            true = wapi_registration:query_resp_v(JObj),
            case wh_json:get_value(<<"Fields">>, JObj) of
                undefined -> [];
                Regs ->
                    [wh_json:from_list([{<<"device_id">>, AuthorizingId}
                                        ,{<<"registered">>, true}
                                       ])
                     || Reg <- Regs
                            ,(AuthorizingId = wh_json:get_value(<<"Authorizing-ID">>, Reg)) =/= undefined
                    ]
            end
    after
        1000 -> []
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check if the device sip creds are unique
%% @end
%%--------------------------------------------------------------------
-spec is_sip_creds_unique/4 :: (undefined | ne_binary(), ne_binary(), ne_binary(), undefined | ne_binary())
                               -> boolean().

%% no account id and no doc id (ie initial create with no account)
is_sip_creds_unique(undefined, _, _, undefined) ->
    true;
is_sip_creds_unique(AccountDb, Realm, Username, DeviceId) ->
    is_creds_locally_unique(AccountDb, Username, DeviceId) 
        andalso is_creds_global_unique(Realm, Username, DeviceId).

is_creds_locally_unique(AccountDb, Username, DeviceId) ->
    ViewOptions = [{<<"key">>, Username}],
    case couch_mgr:get_results(AccountDb, <<"devices/sip_credentials">>, ViewOptions) of
        {ok, []} -> true;
        {ok, [JObj]} -> wh_json:get_value(<<"id">>, JObj) =:= DeviceId;
        {error, not_found} -> true;
        _ -> false
    end.

is_creds_global_unique(Realm, Username, DeviceId) ->
    ViewOptions = [{<<"key">>, [Realm, Username]}],
    case couch_mgr:get_results(?WH_SIP_DB, <<"credentials/lookup">>, ViewOptions) of
        {ok, []} -> true;
        {ok, [JObj]} -> wh_json:get_value(<<"id">>, JObj) =:= DeviceId;
        {error, not_found} -> true;
        _ -> false
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check if the device sip ip is unique
%% @end
%%--------------------------------------------------------------------
is_ip_unique(IP, DeviceId) ->
    is_ip_acl_unique(IP) 
        andalso is_ip_sip_auth_unique(IP, DeviceId).

is_ip_acl_unique(IP) ->
    lists:all(fun(CIDR) -> not (wh_util:verify_cidr(IP, CIDR)) end, get_all_acl_ips()).

is_ip_sip_auth_unique(IP, DeviceId) ->
    ViewOptions = [{<<"key">>, IP}],
    case couch_mgr:get_results(?WH_SIP_DB, <<"credentials/lookup_by_ip">>, ViewOptions) of
        {ok, []} -> true;
        {ok, [JObj]} -> wh_json:get_value(<<"id">>, JObj) =:= DeviceId;
        {error, not_found} -> true;
        _ -> false
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% post data to a provisiong server
%% @end
%%--------------------------------------------------------------------
-spec do_simple_provision/1 :: (#cb_context{}) -> 'ok'.
do_simple_provision(#cb_context{doc=JObj}=Context) ->
    case whapps_config:get_string(?MOD_CONFIG_CAT, <<"provisioning_url">>) of
        undefined -> ok;
        Url ->
            AccountRealm = crossbar_util:get_account_realm(Context),
            Headers = [{K, V}
                       || {K, V} <- [{"Host", whapps_config:get_string(?MOD_CONFIG_CAT, <<"provisioning_host">>)}
                                     ,{"Referer", whapps_config:get_string(?MOD_CONFIG_CAT, <<"provisioning_referer">>)}
                                     ,{"User-Agent", wh_util:to_list(erlang:node())}
                                     ,{"Content-Type", "application/x-www-form-urlencoded"}]
                              ,V =/= undefined],
            HTTPOptions = [],
            Body = [{"device[mac]", re:replace(wh_json:get_string_value(<<"mac_address">>, JObj, ""), "[^0-9a-fA-F]", "", [{return, list}, global])}
                    ,{"device[label]", wh_json:get_string_value(<<"name">>, JObj)}
                    ,{"sip[realm]", wh_json:get_string_value([<<"sip">>, <<"realm">>], JObj, AccountRealm)}
                    ,{"sip[username]", wh_json:get_string_value([<<"sip">>, <<"username">>], JObj)}
                    ,{"sip[password]", wh_json:get_string_value([<<"sip">>, <<"password">>], JObj)}
                    ,{"submit", "true"}],
            Encoded = mochiweb_util:urlencode(Body),
            lager:debug("posting to ~s with ~s", [Url, Encoded]),
            ibrowse:send_req(Url, Headers, post, Encoded, HTTPOptions)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_provision/1 :: (#cb_context{}) -> boolean().
maybe_provision(#cb_context{resp_status=success}=Context) ->
    spawn(fun() -> do_simple_provision(Context) end),
    true;
maybe_provision(_) -> false.
 
-spec maybe_aggregate_device/1 :: (#cb_context{}) -> boolean().
maybe_aggregate_device(#cb_context{resp_status=success, doc=JObj}=Context) ->
    case wh_util:is_true(cb_context:fetch(aggregate_device, Context)) of
        false -> maybe_remove_aggreate(Context);
        true -> 
            lager:debug("adding device to the sip auth aggregate"),
            couch_mgr:ensure_saved(?WH_SIP_DB, wh_json:delete_key(<<"_rev">>, JObj)),
            true
    end;
maybe_aggregate_device(_) -> false.

-spec maybe_remove_aggreate/1 :: (#cb_context{}) -> boolean().
maybe_remove_aggreate(#cb_context{resp_status=success, doc=JObj}) ->
    DeviceId = wh_json:get_value(<<"_id">>, JObj),
    case couch_mgr:lookup_doc_rev(?WH_SIP_DB, DeviceId) of
        {ok, Rev} ->
            couch_mgr:del_doc(?WH_SIP_DB, wh_json:set_value(<<"_rev">>, Rev, JObj)),
            true;
        {error, not_found} -> false
    end;    
maybe_remove_aggreate(_) -> false.






get_all_acl_ips() ->
    case couch_mgr:open_doc(?WH_CONFIG_DB, <<"ecallmgr">>) of
        {ok, JObj} -> extract_acl_ips(JObj);
        {error, _} -> []
    end.

extract_acl_ips(JObj) ->
    ACLs = extract_all_acls(JObj),
    extract_all_ips(ACLs).

extract_all_acls(JObj) ->
    lists:foldr(fun(K, ACLs) ->
                        ACL = wh_json:get_value([K, <<"acls">>], JObj, wh_json:new()),
                        wh_json:merge_jobjs(ACL, ACLs)
                end, wh_json:new(), wh_json:get_keys(JObj)).

extract_all_ips(JObj) ->
    lists:foldr(fun(K, IPs) ->
                        case wh_json:get_value([K, <<"cidr">>], JObj) of
                            undefined -> IPs;
                            CIDR -> [CIDR|IPs]
                        end
                end, [], wh_json:get_keys(JObj)).
