%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc Devices module
%%% Handle client requests for device documents
%%%
%%%
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_devices_v2).

-export([init/0
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,validate_resource/1, validate_resource/2
        ,authenticate/1
        ,authorize/1
        ,validate/1, validate/2, validate/3
        ,put/1, put/2
        ,post/2, post/3
        ,patch/2
        ,delete/2
        ,lookup_regs/1
        ]).

-include("crossbar.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").

-define(STATUS_PATH_TOKEN, <<"status">>).
-define(CHECK_SYNC_PATH_TOKEN, <<"sync">>).

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".devices">>).

-define(CB_LIST, <<"devices/crossbar_listing">>).
-define(OWNER_LIST, <<"devices/listing_by_owner">>).
-define(CB_LIST_MAC, <<"devices/listing_by_macaddress">>).

-define(KEY_MAC_ADDRESS, <<"mac_address">>).
-define(KEY_MOBILE_MDN, [<<"mobile">>, <<"mdn">>]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    Bindings = [{<<"v2_resource.allowed_methods.devices">>, 'allowed_methods'}
               ,{<<"v2_resource.resource_exists.devices">>, 'resource_exists'}
               ,{<<"v2_resource.authenticate">>, 'authenticate'}
               ,{<<"v2_resource.authorize">>, 'authorize'}
               ,{<<"v2_resource.validate_resource.devices">>, 'validate_resource'}
               ,{<<"v2_resource.validate.devices">>, 'validate'}
               ,{<<"v2_resource.execute.put.devices">>, 'put'}
               ,{<<"v2_resource.execute.post.devices">>, 'post'}
               ,{<<"v2_resource.execute.patch.devices">>, 'patch'}
               ,{<<"v2_resource.execute.delete.devices">>, 'delete'}
               ],
    cb_modules_util:bind(?MODULE, Bindings),
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
allowed_methods(_DeviceId) ->
    [?HTTP_GET, ?HTTP_PATCH, ?HTTP_POST, ?HTTP_PUT, ?HTTP_DELETE].

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
resource_exists(_DeviceId) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(_DeviceId, ?CHECK_SYNC_PATH_TOKEN) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    authenticate(cb_context:req_verb(Context), cb_context:req_nouns(Context)).

authenticate(?HTTP_GET, ?DEVICES_QCALL_NOUNS(_DeviceId, _Number)) ->
    lager:debug("authenticating request"),
    'true';
authenticate(_Verb, _Nouns) ->
    'false'.

-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize(cb_context:req_verb(Context), cb_context:req_nouns(Context)).

authorize(?HTTP_GET, ?DEVICES_QCALL_NOUNS(_DeviceId, _Number)) ->
    lager:debug("authorizing request"),
    'true';
authorize(_Verb, _Nouns) ->
    'false'.

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns and Resource Ids are valid.
%% If valid, updates Context with deviceId
%%
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------
-spec validate_resource(cb_context:context()) -> cb_context:context().
validate_resource(Context) -> Context.

-spec validate_resource(cb_context:context(), path_token()) -> cb_context:context().
validate_resource(Context, ?STATUS_PATH_TOKEN) -> Context;
validate_resource(Context, DeviceId) -> validate_device_id(Context, DeviceId).

-spec validate_device_id(cb_context:context(), kz_term:api_binary()) -> cb_context:context().
validate_device_id(Context, DeviceId) ->
    case kz_datamgr:open_cache_doc(cb_context:account_db(Context), DeviceId) of
        {'ok', _} -> cb_context:set_device_id(Context, DeviceId);
        {'error', 'not_found'} ->
            error_no_entity(Context, DeviceId);
        {'error', _R} -> crossbar_util:response_db_fatal(Context)
    end.

-spec error_no_entity(cb_context:context(), path_token()) ->
                             cb_context:context().
error_no_entity(Context, DeviceId) ->
    cb_context:add_system_error('bad_identifier'
                               ,kz_json:from_list([{<<"cause">>, DeviceId}])
                               ,Context
                               ).

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
validate(Context, PathToken) ->
    validate_device(Context, PathToken, cb_context:req_verb(Context)).

validate_device(Context, ?STATUS_PATH_TOKEN, ?HTTP_GET) ->
    load_device_status(Context);
validate_device(Context, DeviceId, ?HTTP_GET) ->
    load_device(DeviceId, Context);
validate_device(Context, DeviceId, ?HTTP_POST) ->
    validate_request(DeviceId, Context);
validate_device(Context, DeviceId, ?HTTP_PATCH) ->
    validate_patch(Context, DeviceId);
validate_device(Context, DeviceId, ?HTTP_PUT) ->
    validate_action(Context, DeviceId, cb_context:req_value(Context, <<"action">>));
validate_device(Context, DeviceId, ?HTTP_DELETE) ->
    load_device(DeviceId, Context).

validate_patch(Context, DeviceId) ->
    crossbar_doc:patch_and_validate(DeviceId, Context, fun validate_request/2).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, DeviceId, ?CHECK_SYNC_PATH_TOKEN) ->
    load_device(DeviceId, Context).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, DeviceId) ->
    _ = kz_util:spawn(fun crossbar_util:flush_registration/1, [Context]),
    case changed_mac_address(Context) of
        'false' -> error_used_mac_address(Context);
        'true' ->
            _ = crossbar_util:maybe_refresh_fs_xml('device', Context),
            Context1 = cb_modules_util:take_sync_field(Context),
            Context2 = prune_null_provisioner_fields(Context1),
            Context3 = crossbar_doc:save(Context2),
            _ = maybe_aggregate_device(DeviceId, Context3),
            _ = kz_util:spawn(fun update_device_provisioning/1, [Context3]),
            maybe_add_mobile_mdn(Context3)
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

-spec prune_null_provisioner_fields(cb_context:context()) -> cb_context:context().
prune_null_provisioner_fields(Context) ->
    Data = cb_context:doc(Context),
    Keys = [[<<"provision">>, <<"combo_keys">>]
           ,[<<"provision">>, <<"feature_keys">>]
           ],
    NewData = prune_null_provisioner_fields(Keys, Data),
    cb_context:set_doc(Context, NewData).

-spec prune_null_provisioner_fields(kz_json:paths(), kz_json:object()) -> kz_json:object().
prune_null_provisioner_fields([], JObj) -> JObj;
prune_null_provisioner_fields([Key|Keys], JObj) ->
    case kz_json:get_value(Key, JObj) of
        'undefined' -> prune_null_provisioner_fields(Keys, JObj);
        Value ->
            NewValue = kz_json:filter(fun filter_null_fields/1, Value),
            NewJObj = kz_json:set_value(Key, NewValue, JObj),
            prune_null_provisioner_fields(Keys, NewJObj)
    end.

-spec filter_null_fields(kz_json:json_terms()) -> boolean().
filter_null_fields({_, 'null'}) -> false;
filter_null_fields(_) -> 'true'.

-spec post(cb_context:context(), path_token(), path_token()) ->
                  cb_context:context().
post(Context, DeviceId, ?CHECK_SYNC_PATH_TOKEN) ->
    lager:debug("publishing check_sync for ~s", [DeviceId]),
    Context1 = cb_context:store(Context, 'sync', 'force'),
    sync_sip_data(Context1),
    crossbar_util:response_202(<<"sync request sent">>, Context).

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, Id) ->
    post(Context, Id).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Context1 = crossbar_doc:save(Context),

    case cb_context:resp_status(Context1) of
        'success' -> handle_new_device(Context1);
        _Status -> Context1
    end.

-spec handle_new_device(cb_context:context()) -> cb_context:context().
handle_new_device(Context) ->
    _ = maybe_aggregate_device('undefined', Context),
    _ = kz_util:spawn(fun update_device_provisioning/1, [Context]),
    maybe_add_mobile_mdn(Context).

-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, DeviceId) ->
    put_action(Context, DeviceId, cb_context:req_value(Context, <<"action">>)).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, DeviceId) ->
    _ = crossbar_util:refresh_fs_xml(Context),
    Context1 = crossbar_doc:delete(Context),
    case get_device_type(Context) of
        <<"mobile">> -> remove_mobile_mdn(Context);
        _Else ->
            _ = crossbar_util:flush_registration(Context),
            _ = kz_util:spawn(fun maybe_delete_provision/1, [Context]),
            _ = maybe_remove_aggregate(DeviceId, Context),
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

-spec load_users_device_summary(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
load_users_device_summary(Context, UserId) ->
    View = ?OWNER_LIST,
    ViewOptions = [{'key', UserId}],
    crossbar_doc:load_view(View, ViewOptions, Context, fun normalize_view_results/2).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_request(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_request('undefined', Context) ->
    maybe_check_mdn('undefined', Context);
validate_request(DeviceId, Context) ->
    Context1 = crossbar_doc:load(DeviceId, Context, ?TYPE_CHECK_OPTION(kzd_devices:type())),
    case cb_context:resp_status(Context1) of
        'success' -> maybe_check_mdn(DeviceId, Context1);
        _Else -> Context1
    end.

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

-spec maybe_check_mdn(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
maybe_check_mdn(DeviceId, Context) ->
    case get_device_type(Context) of
        <<"mobile">> -> check_mdn_undefined(DeviceId, Context);
        _Else when DeviceId =:= 'undefined' ->
            check_mac_address(DeviceId, Context);
        _Else ->
            prepare_outbound_flags(DeviceId, Context)
    end.

-spec check_mdn_undefined(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_mdn_undefined(DeviceId, Context) ->
    case get_mdn(Context) =:= 'undefined' of
        'true' -> error_mdn_undefined(Context);
        'false' -> check_mdn_changed(DeviceId, Context)
    end.

-spec error_mdn_undefined(cb_context:context()) -> cb_context:context().
error_mdn_undefined(Context) ->
    Msg = kz_json:from_list(
            [{<<"message">>, <<"Field is required but missing">>}]
           ),
    cb_context:add_validation_error(?KEY_MOBILE_MDN, <<"required">>, Msg, Context).

-spec check_mdn_changed(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_mdn_changed('undefined', Context) ->
    check_mdn_taken('undefined', Context);
check_mdn_changed(DeviceId, Context) ->
    IsSuperAdmin = cb_context:is_superduper_admin(Context),
    case has_mdn_changed(Context) of
        'true' when IsSuperAdmin ->
            Context1 = cb_context:store(Context, 'remove_mobile_mdn', 'true'),
            check_mdn_taken(DeviceId, Context1);
        'true' -> error_mdn_changed(Context);
        'false' -> prepare_outbound_flags(DeviceId, Context)
    end.

-spec error_mdn_changed(cb_context:context()) -> cb_context:context().
error_mdn_changed(Context) ->
    _OldMDN = kz_json:get_ne_value(?KEY_MOBILE_MDN, cb_context:fetch(Context, 'db_doc')),
    NewMDN = cb_context:req_value(Context, ?KEY_MOBILE_MDN),
    lager:debug("mobile device number attempted to be changed from ~p to ~p"
               ,[_OldMDN, NewMDN]),
    Msg = kz_json:from_list(
            [{<<"message">>, <<"Mobile Device Number cannot be changed">>}
            ,{<<"cause">>, NewMDN}
            ]),
    cb_context:add_validation_error(?KEY_MOBILE_MDN, <<"invalid">>, Msg, Context).

-spec check_mdn_taken(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_mdn_taken(DeviceId, Context) ->
    MDN = get_mdn(Context),
    case knm_number:get(MDN, knm_number_options:mdn_options()) of
        {error, not_found} ->
            lager:debug("endpoint mdn ~s is not taken", [MDN]),
            check_mdn_registered(DeviceId, Context);
        {ok, _Number} ->
            lager:debug("mdn ~s taken", [MDN]),
            error_mdn_taken(MDN, Context);
        {error, _R} ->
            lager:debug("number ~s taken: ~p", [MDN, _R]),
            error_mdn_taken(MDN, Context)
    end.

-spec error_mdn_taken(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
error_mdn_taken(MDN, Context) ->
    Msg = kz_json:from_list(
            [{<<"message">>, <<"Mobile Device Number already exists in the system">>}
            ,{<<"cause">>, MDN}
            ]),
    cb_context:add_validation_error(?KEY_MOBILE_MDN, <<"unique">>, Msg, Context).

-spec check_mdn_registered(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_mdn_registered(DeviceId, Context) ->
    %%TODO: issue API request to TOP (if configured with URL) and validate
    %%   that the number is present in that system, if not stop the request
    %%   and don't set handle_mobile_mdn
    Context1 = cb_context:store(Context, 'add_mobile_mdn', 'true'),
    case DeviceId == 'undefined' of
        'true' -> check_mac_address(DeviceId, Context1);
        'false' -> prepare_outbound_flags(DeviceId, Context1)
    end.

-spec get_mac_address(cb_context:context()) -> kz_term:api_binary().
get_mac_address(Context) ->
    provisioner_util:cleanse_mac_address(
      cb_context:req_value(Context, ?KEY_MAC_ADDRESS)
     ).

-spec changed_mac_address(cb_context:context()) -> boolean().
changed_mac_address(Context) ->
    NewAddress = get_mac_address(Context),
    OldAddress = kz_json:get_ne_value(?KEY_MAC_ADDRESS, cb_context:fetch(Context, 'db_doc')),
    NewAddress =:= provisioner_util:cleanse_mac_address(OldAddress)
        orelse unique_mac_address(NewAddress, Context).

-spec check_mac_address(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_mac_address(DeviceId, Context) ->
    MacAddress = get_mac_address(Context),
    case unique_mac_address(MacAddress, Context) of
        'false' -> error_used_mac_address(Context);
        'true' ->
            prepare_outbound_flags(DeviceId, Context)
    end.

-spec unique_mac_address(kz_term:api_binary(), cb_context:context()) -> boolean().
unique_mac_address('undefined', _Context) -> 'true';
unique_mac_address(MacAddress, Context) ->
    DbName = cb_context:account_db(Context),
    not lists:member(MacAddress, get_mac_addresses(DbName))
        andalso not provisioner_util:is_mac_address_in_use(MacAddress, cb_context:auth_token(Context)).

-spec error_used_mac_address(cb_context:context()) -> cb_context:context().
error_used_mac_address(Context) ->
    MacAddress = get_mac_address(Context),
    Msg = kz_json:from_list(
            [{<<"message">>, <<"Mac address already in use">>}
            ,{<<"cause">>, MacAddress}
            ]),
    cb_context:add_validation_error(?KEY_MAC_ADDRESS, <<"unique">>, Msg, Context).

-spec get_mac_addresses(kz_term:ne_binary()) -> kz_term:ne_binaries().
get_mac_addresses(DbName) ->
    MACs = case kz_datamgr:get_all_results(DbName, ?CB_LIST_MAC) of
               {'ok', AdJObj} -> kz_datamgr:get_result_keys(AdJObj);
               _ -> []
           end,
    [provisioner_util:cleanse_mac_address(MAC) || MAC <- MACs].

-spec prepare_outbound_flags(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
prepare_outbound_flags(DeviceId, Context) ->
    JObj = case cb_context:req_value(Context, <<"outbound_flags">>) of
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

-spec validate_device_creds(kz_term:api_binary(), kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_device_creds(Realm, DeviceId, Context) ->
    case cb_context:req_value(Context, [<<"sip">>, <<"method">>], <<"password">>) of
        <<"password">> ->
            validate_device_password(Realm, DeviceId, Context);
        <<"ip">> ->
            IP = cb_context:req_value(Context, [<<"sip">>, <<"ip">>]),
            validate_device_ip(IP, DeviceId, Context);
        Else ->
            Msg =
                kz_json:from_list([{<<"message">>, <<"SIP authentication method is invalid">>}
                                  ,{<<"target">>, [<<"password">>, <<"ip">>]}
                                  ,{<<"cause">>, Else}
                                  ]),
            C = cb_context:add_validation_error([<<"sip">>, <<"method">>]
                                               ,<<"enum">>
                                               ,Msg
                                               ,Context
                                               ),
            check_emergency_caller_id(DeviceId, C)
    end.

-spec validate_device_password(kz_term:ne_binary(), kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_device_password(Realm, DeviceId, Context) ->
    Username = cb_context:req_value(Context, [<<"sip">>, <<"username">>]),
    case is_sip_creds_unique(cb_context:account_db(Context), Realm, Username, DeviceId) of
        'true' -> check_emergency_caller_id(DeviceId, Context);
        'false' ->
            Msg =
                kz_json:from_list([{<<"message">>, <<"SIP credentials already in use">>}
                                  ,{<<"cause">>, Username}
                                  ]),
            C = cb_context:add_validation_error([<<"sip">>, <<"username">>]
                                               ,<<"unique">>
                                               ,Msg
                                               ,Context
                                               ),
            check_emergency_caller_id(DeviceId, C)
    end.

-spec validate_device_ip(kz_term:ne_binary(), kz_term:api_binary(), cb_context:context()) ->
                                cb_context:context().
validate_device_ip(IP, DeviceId, Context) ->
    case kz_network_utils:is_ipv4(IP) of
        'true' ->
            validate_device_ip_unique(IP, DeviceId, Context);
        'false' ->
            Msg =
                kz_json:from_list([{<<"message">>, <<"Must be a valid IPv4 RFC 791">>}
                                  ,{<<"cause">>, IP}
                                  ]),
            C = cb_context:add_validation_error([<<"sip">>, <<"ip">>]
                                               ,<<"type">>
                                               ,Msg
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
            Msg = kz_json:from_list(
                    [{<<"message">>, <<"SIP IP already in use">>}
                    ,{<<"cause">>, IP}
                    ]),
            C = cb_context:add_validation_error([<<"sip">>, <<"ip">>]
                                               ,<<"unique">>
                                               ,Msg
                                               ,Context
                                               ),
            check_emergency_caller_id(DeviceId, C)
    end.

-spec check_emergency_caller_id(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_emergency_caller_id(DeviceId, Context) ->
    Context1 = crossbar_util:format_emergency_caller_id_number(Context),
    check_device_type_change(DeviceId, Context1).

-spec check_device_type_change(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_device_type_change('undefined', Context) ->
    check_device_schema('undefined', Context);
check_device_type_change(DeviceId, Context) ->
    NewDeviceType = kzd_devices:device_type(cb_context:req_data(Context)),
    IsSuperAdmin = cb_context:is_superduper_admin(Context),
    OldDeviceType = kzd_devices:device_type(cb_context:fetch(Context, 'db_doc')),
    case {NewDeviceType, OldDeviceType} of
        {Same, Same} -> check_device_schema(DeviceId, Context);
        _Else when IsSuperAdmin -> check_device_schema(DeviceId, Context);
        {'undefined', _} -> error_device_type_change(<<>>, Context);
        _Else -> error_device_type_change(NewDeviceType, Context)
    end.

-spec error_device_type_change(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
error_device_type_change(DeviceType, Context) ->
    Msg =
        kz_json:from_list(
          [{<<"message">>, <<"Not authorized to change type of device">>}
          ,{<<"cause">>, DeviceType}
          ]),
    cb_context:add_validation_error(<<"device_type">>, <<"invalid">>, Msg, Context).

-spec check_device_schema(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_device_schema(DeviceId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(DeviceId, C) end,
    cb_context:validate_request_data(<<"devices">>, Context, OnSuccess).

-spec on_successful_validation(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
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
%% @doc Retrieve the status of the devices linked to the account/
%% Reads registered devices in registrations, then map to devices of the account/
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
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj) | Acc].

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
                                    ,{'ecallmgr', 'true'}
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
    Fields = kz_json:get_value(<<"Fields">>, JObj, []),
    S = lists:foldl(fun(J, S) ->
                            case kz_json:get_ne_value(<<"Authorizing-ID">>, J) of
                                'undefined' -> S;
                                AuthId -> sets:add_element(AuthId, S)
                            end
                    end, Set, Fields),
    extract_device_registrations(JObjs, S).

%%------------------------------------------------------------------------------
%% @doc Check if the device sip credentials are unique.
%% @end
%%------------------------------------------------------------------------------
-spec is_sip_creds_unique(kz_term:api_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_binary()) ->
                                 boolean().
%% no account id and no doc id (ie initial create with no account)
is_sip_creds_unique('undefined', _, _, 'undefined') -> 'true';
is_sip_creds_unique(AccountDb, Realm, Username, DeviceId) ->
    is_creds_locally_unique(AccountDb, Username, DeviceId)
        andalso is_creds_global_unique(Realm, Username, DeviceId).

is_creds_locally_unique(AccountDb, Username, DeviceId) ->
    ViewOptions = [{'key', kz_term:to_lower_binary(Username)}],
    case kz_datamgr:get_results(AccountDb, <<"devices/sip_credentials">>, ViewOptions) of
        {'ok', []} -> 'true';
        {'ok', [JObj]} -> kz_doc:id(JObj) =:= DeviceId;
        {'error', 'not_found'} -> 'true';
        _ -> 'false'
    end.

is_creds_global_unique(Realm, Username, DeviceId) ->
    ViewOptions = [{'key', [kz_term:to_lower_binary(Realm)
                           ,kz_term:to_lower_binary(Username)
                           ]
                   }
                  ],
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

-spec maybe_aggregate_device(kz_term:api_ne_binary(), cb_context:context()) -> boolean().
maybe_aggregate_device(DeviceId, Context) ->
    maybe_aggregate_device(DeviceId, Context, cb_context:resp_status(Context)).

-spec maybe_aggregate_device(kz_term:api_ne_binary(), cb_context:context(), crossbar_status()) -> boolean().
maybe_aggregate_device(DeviceId, Context, 'success') ->
    case kz_term:is_true(cb_context:fetch(Context, 'aggregate_device'))
        andalso ?DEVICES_ALLOW_AGGREGATES
    of
        'false' -> maybe_remove_aggregate(DeviceId, Context);
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
        {'error', 'not_found'} -> 'false';
        {'ok', _JObj} ->
            _ = kz_amqp_worker:cast([], fun(_) -> kapi_switch:publish_reload_acls() end),
            'true'
    end;
maybe_remove_aggregate(_, _, _) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc Perform actions on a device.
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

%%------------------------------------------------------------------------------
%% @doc Looks for device_type from DB but if it is undefined there,
%% gets the one from request data.
%% @end
%%------------------------------------------------------------------------------
-spec get_device_type(cb_context:context()) -> kz_term:api_binary().
get_device_type(Context) ->
    case kzd_devices:device_type(cb_context:fetch(Context, 'db_doc')) of
        'undefined' ->
            kzd_devices:device_type(cb_context:req_data(Context));
        DeviceType -> DeviceType
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_mobile_mdn(cb_context:context()) -> cb_context:context().
maybe_add_mobile_mdn(Context) ->
    case kz_term:is_true(cb_context:fetch(Context, 'add_mobile_mdn')) of
        'true' -> add_mobile_mdn(Context);
        'false' -> Context
    end.

-spec add_mobile_mdn(cb_context:context()) -> cb_context:context().
add_mobile_mdn(Context) ->
    Normalized = get_mdn(Context),
    AuthAccountId = cb_context:auth_account_id(Context),
    MobileField =
        kz_json:from_list(
          [{<<"provider">>, <<"tower-of-power">>}
          ,{<<"authorizing">>, kz_json:from_list([{<<"account-id">>, AuthAccountId}])}
          ,{<<"device-id">>, kz_doc:id(cb_context:doc(Context))}
          ]),
    PublicFields = kz_json:from_list([{<<"mobile">>, MobileField}]),
    Options = [{'assign_to', cb_context:account_id(Context)}
              ,{'public_fields', PublicFields}
              ,{'module_name', ?CARRIER_MDN}
               |knm_number_options:mdn_options()
              ],
    case knm_number:create(Normalized, Options) of
        {'error', _}=Error ->
            _ = crossbar_doc:delete(Context),
            cb_phone_numbers_v2:set_response(Error, Context);
        {'ok', _} ->
            lager:debug("created new mdn ~s with public fields set to ~s"
                       ,[Normalized, kz_json:encode(PublicFields)]
                       ),
            maybe_remove_mobile_mdn(Context)
    end.

-spec maybe_remove_mobile_mdn(cb_context:context()) -> cb_context:context().
maybe_remove_mobile_mdn(Context) ->
    case kz_term:is_true(cb_context:fetch(Context, 'remove_mobile_mdn')) of
        'true' -> remove_mobile_mdn(Context);
        'false' -> Context
    end.

-spec remove_mobile_mdn(cb_context:context()) -> cb_context:context().
remove_mobile_mdn(Context) ->
    case kz_json:get_ne_value(?KEY_MOBILE_MDN, cb_context:fetch(Context, 'db_doc')) of
        'undefined' -> Context;
        MDN -> remove_if_mobile(MDN, Context)
    end.

-spec remove_if_mobile(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
remove_if_mobile(MDN, Context) ->
    Normalized = knm_converters:normalize(MDN),
    case knm_number:get(Normalized, knm_number_options:mdn_options()) of
        {'ok', Number} ->
            PN = knm_number:phone_number(Number),
            IsMdnCarrier = ?CARRIER_MDN =:= knm_phone_number:module_name(PN),
            case kz_json:get_ne_value(<<"mobile">>, knm_number:to_public_json(Number)) of
                'undefined' when not IsMdnCarrier ->
                    lager:error("not removing number ~s: somehow not an mdn", [Normalized]),
                    Context;
                Mobile ->
                    lager:debug("hard removing old mdn ~s with mobile properties ~s"
                               ,[Normalized, kz_json:encode(Mobile)]),
                    _ = knm_number:delete(Normalized, knm_number_options:mdn_options()),
                    Context
            end;
        {'error', _R} ->
            lager:debug("unable to fetch mdn ~s for removal: ~p", [Normalized, _R]),
            Context
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_mdn(cb_context:context()) -> kz_term:api_binary().
get_mdn(Context) ->
    ReqMDN = cb_context:req_value(Context, ?KEY_MOBILE_MDN),
    case kz_term:is_empty(ReqMDN)
        andalso kz_json:get_ne_value(?KEY_MOBILE_MDN, cb_context:fetch(Context, 'db_doc'))
    of
        false -> knm_converters:normalize(ReqMDN);
        undefined -> undefined;
        MDN -> knm_converters:normalize(MDN)
    end.

-spec has_mdn_changed(cb_context:context()) -> boolean().
has_mdn_changed(Context) ->
    %% This shouldn't be empty or undefined because caller already checked that with get_mdn/1
    NewMDN = cb_context:req_value(Context, ?KEY_MOBILE_MDN),
    case kz_json:get_ne_value(?KEY_MOBILE_MDN, cb_context:fetch(Context, 'db_doc')) of
        'undefined' -> 'true';
        OldMDN -> knm_converters:normalize(NewMDN) =/=
                      knm_converters:normalize(OldMDN)
    end.
