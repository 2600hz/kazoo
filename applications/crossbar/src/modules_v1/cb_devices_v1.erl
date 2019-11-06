%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Devices module
%%% Handle client requests for device documents
%%%
%%%
%%% @author Karl Anderson
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_devices_v1).

-export([init/0
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,authenticate/1
        ,authorize/1
        ,validate/1, validate/2, validate/3
        ,put/1, put/2
        ,post/2, post/3
        ,delete/2
        ,lookup_regs/1
        ]).

-include("crossbar.hrl").

-define(STATUS_PATH_TOKEN, <<"status">>).
-define(CHECK_SYNC_PATH_TOKEN, <<"sync">>).

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".devices">>).

-define(CB_LIST, <<"devices/crossbar_listing">>).
-define(OWNER_LIST, <<"devices/listing_by_owner">>).
-define(CB_LIST_MAC, <<"devices/listing_by_macaddress">>).

-define(KEY_MAC_ADDRESS, <<"mac_address">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.devices">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.devices">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"v1_resource.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"v1_resource.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.devices">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.devices">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.devices">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.delete.devices">>, ?MODULE, 'delete'),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc This function determines the verbs that are appropriate for the
%% given Nouns. For example `/accounts/' can only accept `GET' and `PUT'.
%%
%% Failure here returns `405 Method Not Allowed'.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?STATUS_PATH_TOKEN) ->
    [?HTTP_GET];
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PUT, ?HTTP_DELETE].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(_DeviceId, ?CHECK_SYNC_PATH_TOKEN) ->
    [?HTTP_POST].

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns are valid.
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(_DeviceId, ?CHECK_SYNC_PATH_TOKEN) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> 'true'.
authenticate(Context) ->
    authenticate(cb_context:req_nouns(Context), cb_context:req_verb(Context)).
authenticate(?DEVICES_QCALL_NOUNS(_DeviceId, _Number), ?HTTP_GET) ->
    lager:debug("authenticating request"),
    'true';
authenticate(_Nouns, _Verb) -> 'false'.

-spec authorize(cb_context:context()) -> 'true'.
authorize(Context) ->
    authorize(cb_context:req_nouns(Context), cb_context:req_verb(Context)).
authorize(?DEVICES_QCALL_NOUNS(_DeviceId, _Number), ?HTTP_GET) ->
    lager:debug("authorizing request"),
    'true';
authorize(_Nouns, _Verb) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400.
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_devices(Context, cb_context:req_verb(Context)).

validate_devices(Context, ?HTTP_GET) ->
    load_device_summary(Context);
validate_devices(Context, ?HTTP_PUT) ->
    validate_request('undefined', Context).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?STATUS_PATH_TOKEN) ->
    validate_device(Context, ?STATUS_PATH_TOKEN, cb_context:req_verb(Context));
validate(Context, DeviceId) ->
    validate_device(Context, DeviceId, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, DeviceId, ?CHECK_SYNC_PATH_TOKEN) ->
    load_device(DeviceId, Context).

validate_device(Context, ?STATUS_PATH_TOKEN, ?HTTP_GET) ->
    load_device_status(Context);
validate_device(Context, DeviceId, ?HTTP_GET) ->
    load_device(DeviceId, Context);
validate_device(Context, DeviceId, ?HTTP_POST) ->
    validate_request(DeviceId, Context);
validate_device(Context, DeviceId, ?HTTP_PUT) ->
    validate_action(Context, DeviceId, cb_context:req_value(Context, <<"action">>));
validate_device(Context, DeviceId, ?HTTP_DELETE) ->
    load_device(DeviceId, Context).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, DeviceId) ->
    case changed_mac_address(Context) of
        'true' ->
            Context1 = cb_modules_util:take_sync_field(Context),
            Context2 = crossbar_doc:save(Context1),
            case cb_context:resp_status(Context2) of
                'success' ->
                    _ = kz_process:spawn(fun crossbar_util:flush_registration/1, [Context2]),
                    _ = crossbar_util:maybe_refresh_fs_xml('device', Context2),
                    _ = maybe_aggregate_device(DeviceId, Context2),
                    _ = kz_process:spawn(fun update_device_provisioning/1, [Context2]),
                    Context2;
                _ ->
                    Context2
            end;
        'false' ->
            error_used_mac_address(Context)
    end.

-spec update_device_provisioning(cb_context:context()) -> 'ok'.
update_device_provisioning(Context) ->
    update_device_provisioning(Context, cb_context:resp_status(Context)).

-spec update_device_provisioning(cb_context:context(), crossbar_status()) -> 'ok'.
update_device_provisioning(Context, 'success') ->
    _ = provisioner_util:provision_device(cb_context:doc(Context)
                                         ,cb_context:fetch(Context, 'db_doc')
                                         ,#{'req_verb' => cb_context:req_verb(Context)
                                           ,'auth_token' => cb_context:auth_token(Context)
                                           ,'new_mac_address' => cb_context:req_value(Context, <<"mac_address">>)
                                           }
                                         ),
    sync_sip_data(Context);
update_device_provisioning(_Context, _Status) -> 'ok'.

-spec sync_sip_data(cb_context:context()) -> 'ok'.
sync_sip_data(Context) ->
    NewDoc = cb_context:doc(Context),
    OldDoc = cb_context:fetch(Context, 'db_doc'),
    AccountId = cb_context:account_id(Context),

    case cb_context:fetch(Context, 'sync') of
        'false' -> 'ok';
        'true' -> provisioner_util:sync_device(AccountId, OldDoc, NewDoc);
        'force' -> provisioner_util:force_sync_device(AccountId, NewDoc)
    end.

-spec post(cb_context:context(), path_token(), path_token()) ->
                  cb_context:context().
post(Context, DeviceId, ?CHECK_SYNC_PATH_TOKEN) ->
    lager:debug("publishing check_sync for ~s", [DeviceId]),
    Context1 = cb_context:store(Context, 'sync', 'force'),
    sync_sip_data(Context1),
    crossbar_util:response_202(<<"sync request sent">>, Context).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Context1 = crossbar_doc:save(Context),
    _ = maybe_aggregate_device('undefined', Context1),
    _ = kz_process:spawn(fun update_device_provisioning/1, [Context1]),
    Context1.

-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, DeviceId) ->
    put_action(Context, DeviceId, cb_context:req_value(Context, <<"action">>)).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, DeviceId) ->
    Context1 = crossbar_doc:delete(Context),
    case cb_context:resp_status(Context) of
        'success' ->
            _ = crossbar_util:flush_registration(Context1),
            _ = crossbar_util:refresh_fs_xml(Context1),
            _ = kz_process:spawn(fun maybe_delete_provision/1, [Context1]),
            _ = maybe_remove_aggregate(DeviceId, Context1),
            Context1;
        _ ->
            Context1
    end.

-spec maybe_delete_provision(cb_context:context()) -> 'ok'.
maybe_delete_provision(Context) ->
    maybe_delete_provision(Context, cb_context:resp_status(Context)).

-spec maybe_delete_provision(cb_context:context(), crossbar_status()) -> 'ok'.
maybe_delete_provision(Context, 'success') ->
    DeviceDoc = cb_context:doc(Context),
    AuthToken = cb_context:auth_token(Context),
    _ = provisioner_util:delete_provision(DeviceDoc, AuthToken),
    'ok';
maybe_delete_provision(_Context, _Status) -> 'ok'.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Attempt to load list of accounts, each summarized. Or a specific
%% account summary.
%% @end
%%------------------------------------------------------------------------------

-spec load_device_summary(cb_context:context()) ->
                                 cb_context:context().
load_device_summary(Context) ->
    load_device_summary(Context, cb_context:req_nouns(Context)).

-spec load_device_summary(cb_context:context(), req_nouns()) ->
                                 cb_context:context().
load_device_summary(Context, [{<<"devices">>, []}
                             ,{<<"users">>, [UserId]}
                              |_]
                   ) ->
    load_users_device_summary(Context, UserId);
load_device_summary(Context, _ReqNouns) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

-spec load_users_device_summary(cb_context:context(), kz_term:ne_binary()) ->
                                       cb_context:context().
load_users_device_summary(Context, UserId) ->
    crossbar_doc:load_view(?OWNER_LIST
                          ,[{'key', UserId}]
                          ,Context
                          ,fun normalize_view_results/2
                          ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_request(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_request('undefined', Context) ->
    check_mac_address('undefined', Context);
validate_request(DeviceId, Context) ->
    prepare_outbound_flags(DeviceId, Context).

%%------------------------------------------------------------------------------
%% @doc Validate payloads for actions on a device.
%% @end
%%------------------------------------------------------------------------------
-spec validate_action(cb_context:context(), kz_term:ne_binary(), kz_term:api_binary()) ->
                             cb_context:context().
validate_action(Context, DeviceId, <<"notify">>) ->
    Context1 = cb_context:validate_request_data(<<"devices_notify">>, Context),
    case cb_context:resp_status(Context1) of
        'success' -> load_device(DeviceId, Context);
        _ -> Context1
    end;
validate_action(Context, _, 'undefined') ->
    crossbar_util:response_400(<<"action required">>, kz_json:new(), Context);
validate_action(Context, _, _) ->
    crossbar_util:response_400(<<"invalid action">>, kz_json:new(), Context).

-spec changed_mac_address(cb_context:context()) -> boolean().
changed_mac_address(Context) ->
    NewAddress = cb_context:req_value(Context, ?KEY_MAC_ADDRESS),
    OldAddress = kz_json:get_ne_value(?KEY_MAC_ADDRESS, cb_context:fetch(Context, 'db_doc')),
    NewAddress =:= OldAddress
        orelse unique_mac_address(NewAddress, Context).

-spec check_mac_address(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_mac_address(DeviceId, Context) ->
    MacAddress = cb_context:req_value(Context, ?KEY_MAC_ADDRESS),
    case unique_mac_address(MacAddress, Context) of
        'true' ->
            prepare_outbound_flags(DeviceId, Context);
        'false' ->
            error_used_mac_address(Context)
    end.

-spec unique_mac_address(kz_term:api_binary(), cb_context:context()) -> boolean().
unique_mac_address('undefined', _Context) -> 'true';
unique_mac_address(MacAddress, Context) ->
    DbName = cb_context:account_db(Context),
    not lists:member(MacAddress, get_mac_addresses(DbName))
        andalso not provisioner_util:is_mac_address_in_use(MacAddress, cb_context:auth_token(Context)).

-spec error_used_mac_address(cb_context:context()) -> cb_context:context().
error_used_mac_address(Context) ->
    MacAddress = cb_context:req_value(Context, ?KEY_MAC_ADDRESS),
    cb_context:add_validation_error(?KEY_MAC_ADDRESS
                                   ,<<"unique">>
                                   ,kz_json:from_list(
                                      [{<<"message">>, <<"Mac address already in use">>}
                                      ,{<<"cause">>, MacAddress}
                                      ])
                                   ,Context
                                   ).

-spec get_mac_addresses(kz_term:ne_binary()) -> kz_term:ne_binaries().
get_mac_addresses(DbName) ->
    case kz_datamgr:get_all_results(DbName, ?CB_LIST_MAC) of
        {'ok', AdJObj} -> kz_datamgr:get_result_keys(AdJObj);
        _ -> []
    end.

-spec prepare_outbound_flags(kz_term:api_binary(), cb_context:context()) ->
                                    cb_context:context().
prepare_outbound_flags(DeviceId, Context) ->
    JObj =
        case cb_context:req_value(Context, <<"outbound_flags">>) of
            'undefined' -> cb_context:req_data(Context);
            [] -> cb_context:req_data(Context);
            Flags when is_list(Flags) ->
                OutboundFlags = [kz_binary:strip(Flag) || Flag <- Flags],
                kz_json:set_value(<<"outbound_flags">>, OutboundFlags, cb_context:req_data(Context));
            _Else ->
                kz_json:set_value(<<"outbound_flags">>, [], cb_context:req_data(Context))
        end,
    prepare_device_realm(DeviceId, cb_context:set_req_data(Context, JObj)).

-spec prepare_device_realm(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
prepare_device_realm(DeviceId, Context) ->
    AccountRealm = kzd_accounts:fetch_realm(cb_context:account_id(Context)),
    Realm = cb_context:req_value(Context, [<<"sip">>, <<"realm">>], AccountRealm),
    case AccountRealm =:= Realm of
        'true' ->
            JObj = kz_json:delete_key([<<"sip">>, <<"realm">>], cb_context:req_data(Context)),
            validate_device_creds(Realm, DeviceId, cb_context:set_req_data(Context, JObj));
        'false' ->
            validate_device_creds(Realm, DeviceId, cb_context:store(Context, 'aggregate_device', 'true'))
    end.

-spec validate_device_creds(kz_term:ne_binary(), kz_term:api_binary(), cb_context:context()) ->
                                   cb_context:context().
validate_device_creds(Realm, DeviceId, Context) ->
    case cb_context:req_value(Context, [<<"sip">>, <<"method">>], <<"password">>) of
        <<"password">> -> validate_device_password(Realm, DeviceId, Context);
        <<"ip">> ->
            IP = cb_context:req_value(Context, [<<"sip">>, <<"ip">>]),
            validate_device_ip(IP, DeviceId, Context);
        Else ->
            C = cb_context:add_validation_error([<<"sip">>, <<"method">>]
                                               ,<<"enum">>
                                               ,kz_json:from_list([{<<"message">>, <<"SIP authentication method is invalid">>}
                                                                  ,{<<"target">>, [<<"password">>, <<"ip">>]}
                                                                  ,{<<"cause">>, Else}
                                                                  ])
                                               ,Context
                                               ),
            check_emergency_caller_id(DeviceId, C)
    end.

-spec validate_device_password(kz_term:ne_binary(), kz_term:api_binary(), cb_context:context()) ->
                                      cb_context:context().
validate_device_password(Realm, DeviceId, Context) ->
    Username = cb_context:req_value(Context, [<<"sip">>, <<"username">>]),
    case is_sip_creds_unique(cb_context:account_db(Context), Realm, Username, DeviceId) of
        'true' -> check_emergency_caller_id(DeviceId, Context);
        'false' ->
            C = cb_context:add_validation_error([<<"sip">>, <<"username">>]
                                               ,<<"unique">>
                                               ,kz_json:from_list([{<<"message">>, <<"SIP credentials already in use">>}
                                                                  ,{<<"cause">>, Username}
                                                                  ])
                                               ,Context
                                               ),
            check_emergency_caller_id(DeviceId, C)
    end.

-spec validate_device_ip(kz_term:ne_binary(), kz_term:api_binary(), cb_context:context()) ->
                                cb_context:context().
validate_device_ip(IP, DeviceId, Context) ->
    case kz_network_utils:is_ipv4(IP) of
        'true' -> validate_device_ip_unique(IP, DeviceId, Context);
        'false' ->
            C = cb_context:add_validation_error([<<"sip">>, <<"ip">>]
                                               ,<<"type">>
                                               ,kz_json:from_list([{<<"message">>, <<"Must be a valid IPv4 RFC 791">>}
                                                                  ,{<<"cause">>, IP}
                                                                  ])
                                               ,Context
                                               ),
            check_emergency_caller_id(DeviceId, C)
    end.

-spec validate_device_ip_unique(kz_term:ne_binary(), kz_term:api_binary(), cb_context:context()) ->
                                       cb_context:context().
validate_device_ip_unique(IP, DeviceId, Context) ->
    case cb_devices_utils:is_ip_unique(IP, DeviceId) of
        'true' ->
            check_emergency_caller_id(DeviceId, cb_context:store(Context, 'aggregate_device', 'true'));
        'false' ->
            C = cb_context:add_validation_error([<<"sip">>, <<"ip">>]
                                               ,<<"unique">>
                                               ,kz_json:from_list([{<<"message">>, <<"SIP IP already in use">>}
                                                                  ,{<<"cause">>, IP}
                                                                  ])
                                               ,Context
                                               ),
            check_emergency_caller_id(DeviceId, C)
    end.

-spec check_emergency_caller_id(kz_term:api_binary(), cb_context:context()) ->
                                       cb_context:context().
check_emergency_caller_id(DeviceId, Context) ->
    Context1 = crossbar_util:format_emergency_caller_id_number(Context),
    check_device_schema(DeviceId, Context1).

-spec check_device_schema(kz_term:api_binary(), cb_context:context()) ->
                                 cb_context:context().
check_device_schema(DeviceId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(DeviceId, C) end,
    cb_context:validate_request_data(<<"devices">>, Context, OnSuccess).

-spec on_successful_validation(kz_term:api_binary(), cb_context:context()) ->
                                      cb_context:context().
on_successful_validation('undefined', Context) ->
    Props = [{<<"pvt_type">>, <<"device">>}],
    cb_context:set_doc(Context, kz_json:set_values(Props, cb_context:doc(Context)));
on_successful_validation(DeviceId, Context) ->
    crossbar_doc:load_merge(DeviceId, Context, ?TYPE_CHECK_OPTION(kzd_devices:type())).

%%------------------------------------------------------------------------------
%% @doc Load a device document from the database.
%% @end
%%------------------------------------------------------------------------------
-spec load_device(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
load_device(DeviceId, Context) ->
    crossbar_doc:load(DeviceId, Context, ?TYPE_CHECK_OPTION(kzd_devices:type())).

%%------------------------------------------------------------------------------
%% @doc Retrieve the status of the devices linked to the account
%% Reads registered devices in registrations, then map to devices of the account
%% @end
%%------------------------------------------------------------------------------
-spec load_device_status(cb_context:context()) -> cb_context:context().
load_device_status(Context) ->
    AccountRealm = kzd_accounts:fetch_realm(cb_context:account_id(Context)),
    RegStatuses = lookup_regs(AccountRealm),
    lager:debug("reg statuses: ~p", [RegStatuses]),
    crossbar_util:response(RegStatuses, Context).

%%------------------------------------------------------------------------------
%% @doc Normalizes the results of a view.
%% @end
%%------------------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) ->
                                    kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].

%%------------------------------------------------------------------------------
%% @doc Returns the complete list of registrations in a the first registrar
%% to respond for a given account realm.  This is not 100% accurate
%% as an endpoint might be stored in another registrar, but it is
%% accurate enough for the status icons.
%% @end
%%------------------------------------------------------------------------------
-spec lookup_regs(kz_term:ne_binary()) -> kz_json:objects().
lookup_regs(AccountRealm) ->
    Req = [{<<"Realm">>, AccountRealm}
          ,{<<"Fields">>, [<<"Authorizing-ID">>]}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kz_amqp_worker:call_collect(Req
                                    ,fun kapi_registration:publish_query_req/1
                                    ,'ecallmgr'
                                    )
    of
        {'error', _E} ->
            lager:debug("error getting reg: ~p", [_E]),
            [];
        {_, JObjs} ->
            [kz_json:from_list([{<<"device_id">>, AuthorizingId}
                               ,{<<"registered">>, 'true'}
                               ])
             || AuthorizingId <- extract_device_registrations(JObjs)
            ]
    end.

-spec extract_device_registrations(kz_json:objects()) -> kz_term:ne_binaries().
extract_device_registrations(JObjs) ->
    sets:to_list(extract_device_registrations(JObjs, sets:new())).

-spec extract_device_registrations(kz_json:objects(), sets:set()) -> sets:set().
extract_device_registrations([], Set) -> Set;
extract_device_registrations([JObj|JObjs], Set) ->
    S = lists:foldl(fun extract_device_registration/2
                   ,Set
                   ,kz_json:get_value(<<"Fields">>, JObj, [])
                   ),
    extract_device_registrations(JObjs, S).

-spec extract_device_registration(kz_json:object(), sets:set()) -> sets:set().
extract_device_registration(JObj, Set) ->
    case kz_json:get_ne_binary_value(<<"Authorizing-ID">>, JObj) of
        'undefined' -> Set;
        AuthId -> sets:add_element(AuthId, Set)
    end.

%%------------------------------------------------------------------------------
%% @doc Check if the device sip creds are unique
%% @end
%%------------------------------------------------------------------------------
-spec is_sip_creds_unique(kz_term:api_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_binary()) ->
                                 boolean().

%% no account id and no doc id (ie initial create with no account)
is_sip_creds_unique('undefined', _, _, 'undefined') -> 'true';
is_sip_creds_unique(AccountDb, Realm, Username, DeviceId) ->
    is_creds_locally_unique(AccountDb, Username, DeviceId)
        andalso is_creds_global_unique(Realm, Username, DeviceId).

-spec is_creds_locally_unique(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_creds_locally_unique(AccountDb, Username, DeviceId) ->
    ViewOptions = [{'key', kz_term:to_lower_binary(Username)}],
    case kz_datamgr:get_results(AccountDb, <<"devices/sip_credentials">>, ViewOptions) of
        {'ok', []} -> 'true';
        {'ok', [JObj]} -> kz_doc:id(JObj) =:= DeviceId;
        {'error', 'not_found'} -> 'true';
        _ -> 'false'
    end.

-spec is_creds_global_unique(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_creds_global_unique(Realm, Username, DeviceId) ->
    ViewOptions = [{'key', [kz_term:to_lower_binary(Realm)
                           ,kz_term:to_lower_binary(Username)
                           ]
                   }],
    case kz_datamgr:get_results(?KZ_SIP_DB, <<"credentials/lookup">>, ViewOptions) of
        {'ok', []} -> 'true';
        {'ok', [JObj]} -> kz_doc:id(JObj) =:= DeviceId;
        {'error', 'not_found'} -> 'true';
        _ -> 'false'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec maybe_aggregate_device(kz_term:api_binary(), cb_context:context()) -> boolean().
maybe_aggregate_device(DeviceId, Context) ->
    maybe_aggregate_device(DeviceId, Context, cb_context:resp_status(Context)).

-spec maybe_aggregate_device(kz_term:api_binary(), cb_context:context(), crossbar_status()) -> boolean().
maybe_aggregate_device(DeviceId, Context, 'success') ->
    case kz_term:is_true(cb_context:fetch(Context, 'aggregate_device'))
        andalso ?DEVICES_ALLOW_AGGREGATES
    of
        'false' ->
            maybe_remove_aggregate(DeviceId, Context);
        'true' ->
            aggregate_device(cb_context:doc(Context)),
            _ = kz_amqp_worker:cast([], fun(_) -> kapi_switch:publish_reload_acls() end),
            'true'
    end;
maybe_aggregate_device(_, _, _) -> 'false'.

-spec aggregate_device(kz_json:object()) -> 'ok'.
aggregate_device(Device) ->
    lager:debug("adding device to the sip auth aggregate"),
    Doc = kz_doc:delete_revision(Device),
    Update = kz_json:to_proplist(kz_json:flatten(Doc)),
    UpdateOptions = [{'update', Update}
                    ,{'create', []}
                    ,{'ensure_saved', 'true'}
                    ],
    {'ok', _} = kz_datamgr:update_doc(?KZ_SIP_DB, kz_doc:id(Device), UpdateOptions),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec maybe_remove_aggregate(kz_term:api_binary(), cb_context:context()) -> boolean().
maybe_remove_aggregate(DeviceId, Context) ->
    maybe_remove_aggregate(DeviceId, Context, cb_context:resp_status(Context)).

-spec maybe_remove_aggregate(kz_term:api_binary(), cb_context:context(), crossbar_status()) -> boolean().
maybe_remove_aggregate('undefined', _Context, _RespStatus) -> 'false';
maybe_remove_aggregate(DeviceId, _Context, 'success') ->
    case kz_datamgr:del_doc(?KZ_SIP_DB, DeviceId) of
        {'ok', _JObj} ->
            _ = kz_amqp_worker:cast([], fun(_) -> kapi_switch:publish_reload_acls() end),
            'true';
        {'error', 'not_found'} -> 'false'
    end;
maybe_remove_aggregate(_, _, _) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc Perform actions on a device
%% @end
%%------------------------------------------------------------------------------
-spec put_action(cb_context:context(), kz_term:ne_binary(), kz_term:api_binary()) ->
                        cb_context:context().
put_action(Context, DeviceId, <<"notify">>) ->
    lager:debug("publishing NOTIFY for ~s", [DeviceId]),
    Username = kzd_devices:sip_username(cb_context:doc(Context)),
    Realm = kzd_accounts:fetch_realm(cb_context:account_id(Context)),
    Req = props:filter_undefined(
            [{<<"Body">>, cb_context:req_value(Context, [<<"data">>, <<"body">>, <<"data">>])}
            ,{<<"Content-Type">>, cb_context:req_value(Context, [<<"data">>, <<"body">>, <<"content_type">>])}
            ,{<<"Event">>, cb_context:req_value(Context, [<<"data">>, <<"event">>])}
            ,{<<"Msg-ID">>, cb_context:req_id(Context)}
            ,{<<"Realm">>, Realm}
            ,{<<"Username">>, Username}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    kapi_switch:publish_notify(Req),
    crossbar_util:response_202(<<"NOTIFY sent">>, Context).
