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
         ,allowed_methods/0, allowed_methods/1, allowed_methods/3
         ,resource_exists/0, resource_exists/1, resource_exists/3
         ,billing/1
         ,authenticate/1
         ,authorize/1
         ,validate/1, validate/2, validate/4
         ,put/1
         ,post/2
         ,delete/2
         ,reconcile_services/1
         ,is_ip_acl_unique/1
         ,get_all_acl_ips/0
        ]).

-include_lib("crossbar/include/crossbar.hrl").

-define(QUICKCALL_URL, [{<<"devices">>, [_, <<"quickcall">>, _]}
                        ,{?WH_ACCOUNTS_DB, [_]}
                       ]).

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".devices">>).

-define(CB_LIST, <<"devices/crossbar_listing">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.devices">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.devices">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.authenticate">>, ?MODULE, authenticate),
    _ = crossbar_bindings:bind(<<"v1_resource.authorize">>, ?MODULE, authorize),
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
-spec allowed_methods/3 :: (path_token(), path_token(), path_token()) -> http_methods().

allowed_methods() ->
    ['GET', 'PUT'].

allowed_methods(<<"status">>) ->
    ['GET'];
allowed_methods(_) ->
    ['GET', 'POST', 'DELETE'].

allowed_methods(_, <<"quickcall">>, _) ->
    ['GET'].

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
-spec resource_exists/3 :: (path_token(), path_token(), path_token()) -> 'true'.

resource_exists() -> true.
resource_exists(_) -> true.
resource_exists(_, <<"quickcall">>, _) -> true.

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
%%
%% @end
%%--------------------------------------------------------------------
-spec authenticate/1 :: (#cb_context{}) -> 'true'.
authenticate(#cb_context{req_nouns=?DEVICES_QCALL_NOUNS, req_verb = <<"get">>}) ->
    lager:debug("authenticating request"),
    true.

-spec authorize/1 :: (#cb_context{}) -> 'true'.
authorize(#cb_context{req_nouns=?DEVICES_QCALL_NOUNS, req_verb = <<"get">>}) ->
    lager:debug("authorizing request"),
    true.

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

validate(#cb_context{req_verb = <<"get">>}=Context, DeviceId, <<"quickcall">>, _) ->
    Context1 = maybe_validate_quickcall(load_device(DeviceId, Context)),
    case cb_context:has_errors(Context1) of
        true -> Context1;
        false -> 
            cb_modules_util:maybe_originate_quickcall(Context1)
    end.

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
delete(#cb_context{}=Context, DeviceId) ->
    Context1 = crossbar_doc:delete(Context),
    _ = maybe_remove_aggreate(DeviceId, Context),
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

-spec prepare_outbound_flags/2 :: ('undefined'|ne_binary(), #cb_context{}) -> #cb_context{}.
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

-spec prepare_device_realm/2 :: ('undefined'|ne_binary(), #cb_context{}) -> #cb_context{}.
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

-spec validate_device_creds/3 :: (ne_binary(), 'undefined'|ne_binary(), #cb_context{}) -> #cb_context{}.
validate_device_creds(Realm, DeviceId, #cb_context{req_data=JObj}=Context) ->
    case wh_json:get_value([<<"sip">>, <<"method">>], JObj, <<"password">>) of
        <<"password">> -> validate_device_password(Realm, DeviceId, Context);
        <<"ip">> -> 
            IP = wh_json:get_value([<<"sip">>, <<"ip">>], JObj),
            validate_device_ip(IP, DeviceId, Context);
        _Else ->
            C = cb_context:add_validation_error([<<"sip">>, <<"method">>]
                                                   ,<<"enum">>
                                                   ,<<"SIP authentication method is invalid">>
                                                   ,Context),
            check_device_schema(DeviceId, C)
    end.

-spec validate_device_password/3 :: (ne_binary(), 'undefined'|ne_binary(), #cb_context{}) -> #cb_context{}.
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

-spec validate_device_ip/3 :: (ne_binary(), 'undefined'|ne_binary(), #cb_context{}) -> #cb_context{}.
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
    OnSuccess = fun(C) -> on_successful_validation(DeviceId, C) end,
    cb_context:validate_request_data(<<"devices">>, Context, OnSuccess).

on_successful_validation(undefined, #cb_context{doc=Doc}=Context) ->
    Props = [{<<"pvt_type">>, <<"device">>}],
    Context#cb_context{doc=wh_json:set_values(Props, Doc)};
on_successful_validation(DeviceId, #cb_context{}=Context) -> 
    crossbar_doc:load_merge(DeviceId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
maybe_validate_quickcall(#cb_context{resp_status=success, doc=JObj, auth_token=AuthToken}=Context) ->
    case (not wh_util:is_empty(AuthToken))
          orelse wh_json:is_true(<<"allow_anoymous_quickcalls">>, JObj) 
    of
        false -> cb_context:add_system_error(invalid_crentials, Context);
        true -> Context
    end.

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
    Req = [{<<"Realm">>, AccountRealm}
           ,{<<"Fields">>, [<<"Authorizing-ID">>]}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    Resp =  whapps_util:amqp_pool_request(Req
                                          ,fun wapi_registration:publish_query_req/1
                                          ,fun wapi_registration:query_resp_v/1
                                          ,1000),
    case Resp of
        {error, _} -> [];
        {ok, JObj} -> extract_device_registrations(JObj)
    end.

-spec extract_device_registrations/1 :: (wh_json:json_object()) -> wh_json:json_objects().
extract_device_registrations(JObj) ->
    [wh_json:from_list([{<<"device_id">>, AuthorizingId}
                        ,{<<"registered">>, true}
                       ])
     || Reg <- wh_json:get_value(<<"Fields">>, JObj, [])
            ,(AuthorizingId = wh_json:get_value(<<"Authorizing-ID">>, Reg)) =/= undefined
    ].

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
    lists:all(fun(CIDR) -> not (wh_network_utils:verify_cidr(IP, CIDR)) end, get_all_acl_ips()).

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
        false -> 
            DeviceId = wh_json:get_value(<<"_id">>, JObj),
            maybe_remove_aggreate(DeviceId, Context);
        true -> 
            lager:debug("adding device to the sip auth aggregate"),
            couch_mgr:ensure_saved(?WH_SIP_DB, wh_json:delete_key(<<"_rev">>, JObj)),
            wapi_switch:publish_reload_acls(),
            true
    end;
maybe_aggregate_device(_) -> false.

-spec maybe_remove_aggreate/2 :: (ne_binary(), #cb_context{}) -> boolean().
maybe_remove_aggreate(DeviceId, #cb_context{resp_status=success, doc=JObj}) ->
    case couch_mgr:open_doc(?WH_SIP_DB, DeviceId) of
        {ok, JObj} ->
            couch_mgr:del_doc(?WH_SIP_DB, JObj),
            wapi_switch:publish_reload_acls(),
            true;
        {error, not_found} -> false
    end;    
maybe_remove_aggreate(_, _) -> false.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_all_acl_ips/0 :: () -> [] | [ne_binary(),...].
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
        {error, _} -> [];
        {ok, JObj} -> 
            extract_all_ips(wh_json:get_value(<<"Value">>, JObj, wh_json:new()))
    end.

-spec extract_all_ips/1 :: (wh_json:json_object()) -> [] | [ne_binary(),...].
extract_all_ips(JObj) ->
    lists:foldr(fun(K, IPs) ->
                        case wh_json:get_value([K, <<"cidr">>], JObj) of
                            undefined -> IPs;
                            CIDR -> [CIDR|IPs]
                        end
                end, [], wh_json:get_keys(JObj)).
