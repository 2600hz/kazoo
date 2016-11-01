%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
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
-module(cb_devices_v2).

-export([init/0
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3
        ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3
        ,validate_resource/1, validate_resource/2
        ,billing/1
        ,authenticate/1
        ,authorize/1
        ,validate/1, validate/2, validate/3, validate/4
        ,put/1
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

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok.
init() ->
    Bindings = [{<<"v2_resource.allowed_methods.devices">>, 'allowed_methods'}
               ,{<<"v2_resource.resource_exists.devices">>, 'resource_exists'}
               ,{<<"v2_resource.authenticate">>, 'authenticate'}
               ,{<<"v2_resource.authorize">>, 'authorize'}
               ,{<<"v2_resource.billing">>, 'billing'}
               ,{<<"v2_resource.validate_resource.devices">>, 'validate_resource'}
               ,{<<"v2_resource.validate.devices">>, 'validate'}
               ,{<<"v2_resource.execute.put.devices">>, 'put'}
               ,{<<"v2_resource.execute.post.devices">>, 'post'}
               ,{<<"v2_resource.execute.patch.devices">>, 'patch'}
               ,{<<"v2_resource.execute.delete.devices">>, 'delete'}
               ],
    cb_modules_util:bind(?MODULE, Bindings),
    _ = crossbar_bindings:bind(<<"v2_resource.finish_request.*.devices">>
                              ,'crossbar_services'
                              ,'reconcile'
                              ),
    ok.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() ->
                             http_methods().
-spec allowed_methods(path_token()) ->
                             http_methods().
-spec allowed_methods(path_token(), path_token()) ->
                             http_methods().
-spec allowed_methods(path_token(), path_token(), path_token()) ->
                             http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

allowed_methods(?STATUS_PATH_TOKEN) ->
    [?HTTP_GET];
allowed_methods(_DeviceId) ->
    [?HTTP_GET, ?HTTP_PATCH, ?HTTP_POST, ?HTTP_DELETE].

allowed_methods(_DeviceId, ?CHECK_SYNC_PATH_TOKEN) ->
    [?HTTP_POST].

allowed_methods(_DeviceId, ?QUICKCALL_PATH_TOKEN, _PhoneNumber) ->
    [?HTTP_GET].

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
-spec resource_exists(path_token(), path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token(), path_token()) -> 'true'.

resource_exists() -> 'true'.
resource_exists(_DeviceId) -> 'true'.
resource_exists(_DeviceId, ?CHECK_SYNC_PATH_TOKEN) -> 'true'.
resource_exists(_DeviceId, ?QUICKCALL_PATH_TOKEN, _Number) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Ensure we will be able to bill for devices
%% @end
%%--------------------------------------------------------------------
-spec billing(cb_context:context()) -> cb_context:context().
billing(Context) ->
    billing(Context, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

billing(Context, ?HTTP_GET, [{<<"devices">>, _}|_]) ->
    Context;
billing(Context, _ReqVerb, [{<<"devices">>, _}|_Nouns]) ->
    try kz_services:allow_updates(cb_context:account_id(Context)) of
        'true' ->
            lager:debug("allowing service updates"),
            Context
    catch
        'throw':{Error, Reason} ->
            lager:debug("account ~s is not allowed to make billing updates: ~s: ~p"
                       ,[props:get_value(<<"accounts">>, _Nouns), Error, Reason]),
            crossbar_util:response('error', kz_util:to_binary(Error), 500, Reason, Context)
    end;
billing(Context, _ReqVerb, _Nouns) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns and Resource Ids are valid.
%% If valid, updates Context with deviceId
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec validate_resource(cb_context:context()) -> cb_context:context().
validate_resource(Context) -> Context.

-spec validate_resource(cb_context:context(), path_token()) -> cb_context:context().
validate_resource(Context, ?STATUS_PATH_TOKEN) -> Context;
validate_resource(Context, DeviceId) -> validate_device_id(Context, DeviceId).

-spec validate_device_id(cb_context:context(), api_binary()) -> cb_context:context().
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
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_devices(Context, cb_context:req_verb(Context)).

validate_devices(Context, ?HTTP_GET) ->
    load_device_summary(Context);
validate_devices(Context, ?HTTP_PUT) ->
    validate_request('undefined', Context).

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
validate_device(Context, DeviceId, ?HTTP_DELETE) ->
    load_device(DeviceId, Context).

validate_patch(Context, DeviceId) ->
    crossbar_doc:patch_and_validate(DeviceId, Context, fun validate_request/2).

validate(Context, DeviceId, ?CHECK_SYNC_PATH_TOKEN) ->
    load_device(DeviceId, Context).

validate(Context, DeviceId, ?QUICKCALL_PATH_TOKEN, _ToDial) ->
    Context1 = crossbar_util:maybe_validate_quickcall(load_device(DeviceId, Context)),
    case cb_context:has_errors(Context1) of
        'true' -> Context1;
        'false' ->
            cb_modules_util:maybe_originate_quickcall(Context1)
    end.

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, DeviceId) ->
    _ = kz_util:spawn(fun crossbar_util:flush_registration/1, [Context]),
    case changed_mac_address(Context) of
        'false' -> error_used_mac_address(Context);
        'true' ->
            _ = crossbar_util:maybe_refresh_fs_xml('device', Context),
            Context1 = cb_modules_util:take_sync_field(Context),
            Context2 = crossbar_doc:save(Context1),
            _ = maybe_aggregate_device(DeviceId, Context2),
            _ = kz_util:spawn(
                  fun() ->
                          _ = provisioner_util:maybe_provision(Context2),
                          _ = provisioner_util:maybe_sync_sip_data(Context1, 'device')
                  end),
            maybe_add_mobile_mdn(Context2)
    end.

-spec post(cb_context:context(), path_token(), path_token()) ->
                  cb_context:context().
post(Context, DeviceId, ?CHECK_SYNC_PATH_TOKEN) ->
    lager:debug("publishing check_sync for ~s", [DeviceId]),
    Context1 = cb_context:store(Context, 'sync', 'force'),
    _ = provisioner_util:maybe_sync_sip_data(Context1, 'device'),
    crossbar_util:response_202(<<"sync request sent">>, Context).

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, _Id) ->
    crossbar_doc:save(Context).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Callback =
        fun() ->
                Context1 = crossbar_doc:save(Context),
                _ = maybe_aggregate_device('undefined', Context1),
                _ = kz_util:spawn(fun provisioner_util:maybe_provision/1, [Context1]),
                maybe_add_mobile_mdn(Context1)
        end,
    crossbar_services:maybe_dry_run(Context, Callback).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, DeviceId) ->
    _ = crossbar_util:refresh_fs_xml(Context),
    Context1 = crossbar_doc:delete(Context),
    case get_device_type(Context) of
        <<"mobile">> -> remove_mobile_mdn(Context);
        _Else ->
            _ = crossbar_util:flush_registration(Context),
            _ = kz_util:spawn(fun provisioner_util:maybe_delete_provision/1, [Context]),
            _ = maybe_remove_aggregate(DeviceId, Context),
            Context1
    end.

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
-spec load_device_summary(cb_context:context()) ->
                                 cb_context:context().
-spec load_device_summary(cb_context:context(), req_nouns()) ->
                                 cb_context:context().
load_device_summary(Context) ->
    load_device_summary(Context, cb_context:req_nouns(Context)).

load_device_summary(Context, [{<<"devices">>, []}
                             ,{<<"users">>, [UserId]}
                              |_]
                   ) ->
    load_users_device_summary(Context, UserId);
load_device_summary(Context, _ReqNouns) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

-spec load_users_device_summary(cb_context:context(), ne_binary()) -> cb_context:context().
load_users_device_summary(Context, UserId) ->
    View = ?OWNER_LIST,
    ViewOptions = [{'key', UserId}],
    crossbar_doc:load_view(View, ViewOptions, Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_request(api_binary(), cb_context:context()) -> cb_context:context().
validate_request('undefined', Context) ->
    maybe_check_mdn('undefined', Context);
validate_request(DeviceId, Context) ->
    Context1 = crossbar_doc:load(DeviceId, Context, ?TYPE_CHECK_OPTION(kz_device:type())),
    case cb_context:resp_status(Context1) of
        'success' -> maybe_check_mdn(DeviceId, Context1);
        _Else -> Context1
    end.

-spec maybe_check_mdn(api_binary(), cb_context:context()) -> cb_context:context().
maybe_check_mdn(DeviceId, Context) ->
    case get_device_type(Context) of
        <<"mobile">> -> check_mdn_undefined(DeviceId, Context);
        _Else when DeviceId =:= 'undefined' ->
            check_mac_address(DeviceId, Context);
        _Else ->
            prepare_outbound_flags(DeviceId, Context)
    end.

-spec check_mdn_undefined(api_binary(), cb_context:context()) -> cb_context:context().
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

-spec check_mdn_changed(api_binary(), cb_context:context()) -> cb_context:context().
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

-spec check_mdn_taken(api_binary(), cb_context:context()) -> cb_context:context().
check_mdn_taken(DeviceId, Context) ->
    MDN = get_mdn(Context),
    case knm_number:get(MDN) of
        {'ok', _Number} -> error_mdn_taken(MDN, Context);
        _Otherwise ->
            lager:debug("endpoint mdn ~s is not taken: ~p", [MDN, _Otherwise]),
            check_mdn_registered(DeviceId, Context)
    end.

-spec error_mdn_taken(ne_binary(), cb_context:context()) -> cb_context:context().
error_mdn_taken(MDN, Context) ->
    Msg = kz_json:from_list(
            [{<<"message">>, <<"Mobile Device Number already exists in the system">>}
            ,{<<"cause">>, MDN}
            ]),
    cb_context:add_validation_error(?KEY_MOBILE_MDN, <<"unique">>, Msg, Context).

-spec check_mdn_registered(api_binary(), cb_context:context()) -> cb_context:context().
check_mdn_registered(DeviceId, Context) ->
    %%TODO: issue API request to TOP (if configured with URL) and validate
    %%   that the number is present in that system, if not stop the request
    %%   and don't set handle_modible_mdn
    Context1 = cb_context:store(Context, 'add_mobile_mdn', 'true'),
    case DeviceId == 'undefined' of
        'true' -> check_mac_address(DeviceId, Context1);
        'false' -> prepare_outbound_flags(DeviceId, Context1)
    end.

-spec get_mac_address(cb_context:context()) -> api_binary().
get_mac_address(Context) ->
    kz_util:to_lower_binary(cb_context:req_value(Context, ?KEY_MAC_ADDRESS)).

-spec changed_mac_address(cb_context:context()) -> boolean().
changed_mac_address(Context) ->
    NewAddress = get_mac_address(Context),
    OldAddress = kz_json:get_ne_value(?KEY_MAC_ADDRESS, cb_context:fetch(Context, 'db_doc')),
    NewAddress =:= kz_util:to_lower_binary(OldAddress)
        orelse unique_mac_address(NewAddress, Context).

-spec check_mac_address(api_binary(), cb_context:context()) -> cb_context:context().
check_mac_address(DeviceId, Context) ->
    MacAddress = get_mac_address(Context),
    case unique_mac_address(MacAddress, Context) of
        'false' -> error_used_mac_address(Context);
        'true' ->
            prepare_outbound_flags(DeviceId, Context)
    end.

-spec unique_mac_address(api_binary(), cb_context:context()) -> boolean().
unique_mac_address('undefined', _Context) -> 'true';
unique_mac_address(MacAddress, Context) ->
    DbName = cb_context:account_db(Context),
    not lists:member(MacAddress, get_mac_addresses(DbName))
        andalso not provisioner_util:is_mac_address_in_use(Context, MacAddress).

-spec error_used_mac_address(cb_context:context()) -> cb_context:context().
error_used_mac_address(Context) ->
    MacAddress = get_mac_address(Context),
    Msg = kz_json:from_list(
            [{<<"message">>, <<"Mac address already in use">>}
            ,{<<"cause">>, MacAddress}
            ]),
    cb_context:add_validation_error(?KEY_MAC_ADDRESS, <<"unique">>, Msg, Context).

-spec get_mac_addresses(ne_binary()) -> ne_binaries().
get_mac_addresses(DbName) ->
    MACs = case kz_datamgr:get_all_results(DbName, ?CB_LIST_MAC) of
               {'ok', AdJObj} -> kz_datamgr:get_result_keys(AdJObj);
               _ -> []
           end,
    lists:map(fun kz_util:to_lower_binary/1, MACs).

-spec prepare_outbound_flags(api_binary(), cb_context:context()) -> cb_context:context().
prepare_outbound_flags(DeviceId, Context) ->
    JObj = case cb_context:req_value(Context, <<"outbound_flags">>) of
               'undefined' -> cb_context:req_data(Context);
               [] -> cb_context:req_data(Context);
               Flags when is_list(Flags) ->
                   OutboundFlags = [kz_util:strip_binary(Flag) || Flag <- Flags],
                   kz_json:set_value(<<"outbound_flags">>, OutboundFlags, cb_context:req_data(Context));
               _Else ->
                   kz_json:set_value(<<"outbound_flags">>, [], cb_context:req_data(Context))
           end,
    prepare_device_realm(DeviceId, cb_context:set_req_data(Context, JObj)).

-spec prepare_device_realm(api_binary(), cb_context:context()) -> cb_context:context().
prepare_device_realm(DeviceId, Context) ->
    AccountRealm = kz_util:get_account_realm(cb_context:account_id(Context)),
    Realm = cb_context:req_value(Context, [<<"sip">>, <<"realm">>], AccountRealm),
    case AccountRealm =:= Realm of
        'true' ->
            JObj = kz_json:delete_key([<<"sip">>, <<"realm">>], cb_context:req_data(Context)),
            validate_device_creds(Realm, DeviceId, cb_context:set_req_data(Context, JObj));
        'false' ->
            validate_device_creds(Realm, DeviceId, cb_context:store(Context, 'aggregate_device', 'true'))
    end.

-spec validate_device_creds(api_binary(), api_binary(), cb_context:context()) -> cb_context:context().
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

-spec validate_device_password(ne_binary(), api_binary(), cb_context:context()) -> cb_context:context().
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

-spec validate_device_ip(ne_binary(), api_binary(), cb_context:context()) ->
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

-spec validate_device_ip_unique(ne_binary(), api_binary(), cb_context:context()) ->
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

-spec check_emergency_caller_id(api_binary(), cb_context:context()) -> cb_context:context().
check_emergency_caller_id(DeviceId, Context) ->
    Context1 = crossbar_util:format_emergency_caller_id_number(Context),
    check_device_type_change(DeviceId, Context1).

-spec check_device_type_change(api_binary(), cb_context:context()) -> cb_context:context().
check_device_type_change('undefined', Context) ->
    check_device_schema('undefined', Context);
check_device_type_change(DeviceId, Context) ->
    NewDeviceType = kz_device:device_type(cb_context:req_data(Context)),
    IsSuperAdmin = cb_context:is_superduper_admin(Context),
    OldDeviceType = kz_device:device_type(cb_context:fetch(Context, 'db_doc')),
    case {NewDeviceType, OldDeviceType} of
        {Same, Same} -> check_device_schema(DeviceId, Context);
        _Else when IsSuperAdmin -> check_device_schema(DeviceId, Context);
        {'undefined', _} -> error_device_type_change(<<>>, Context);
        _Else -> error_device_type_change(NewDeviceType, Context)
    end.

-spec error_device_type_change(api_binary(), cb_context:context()) -> cb_context:context().
error_device_type_change(DeviceType, Context) ->
    Msg =
        kz_json:from_list(
          [{<<"message">>, <<"Not authorized to change type of device">>}
          ,{<<"cause">>, DeviceType}
          ]),
    cb_context:add_validation_error(<<"device_type">>, <<"invalid">>, Msg, Context).

-spec check_device_schema(api_binary(), cb_context:context()) -> cb_context:context().
check_device_schema(DeviceId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(DeviceId, C) end,
    cb_context:validate_request_data(<<"devices">>, Context, OnSuccess).

-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    Props = [{<<"pvt_type">>, <<"device">>}],
    cb_context:set_doc(Context, kz_json:set_values(Props, cb_context:doc(Context)));
on_successful_validation(DeviceId, Context) ->
    crossbar_doc:load_merge(DeviceId, Context, ?TYPE_CHECK_OPTION(kz_device:type())).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a device document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_device(ne_binary(), cb_context:context()) -> cb_context:context().
load_device(DeviceId, Context) ->
    crossbar_doc:load(DeviceId, Context, ?TYPE_CHECK_OPTION(kz_device:type())).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieve the status of the devices linked to the account
%% Reads registered devices in registrations, then map to devices of the account
%% @end
%%--------------------------------------------------------------------
-spec load_device_status(cb_context:context()) -> cb_context:context().
load_device_status(Context) ->
    AccountRealm = kz_util:get_account_realm(cb_context:account_id(Context)),
    RegStatuses = lookup_regs(AccountRealm),
    lager:debug("reg statuses: ~p", [RegStatuses]),
    crossbar_util:response(RegStatuses, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj) | Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns the complete list of registrations in a the first registrar
%% to respond for a given account realm.  This is not 100% accurate
%% as an endpoint might be stored in another registrar, but it is
%% accurate enough for the status icons.
%% @end
%%--------------------------------------------------------------------
-spec lookup_regs(ne_binary()) -> kz_json:objects().
lookup_regs(AccountRealm) ->
    Req = [{<<"Realm">>, AccountRealm}
          ,{<<"Fields">>, [<<"Authorizing-ID">>]}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kapps_util:amqp_pool_collect(Req
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

-spec extract_device_registrations(kz_json:objects()) -> ne_binaries().
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check if the device sip creds are unique
%% @end
%%--------------------------------------------------------------------
-spec is_sip_creds_unique(api_binary(), ne_binary(), ne_binary(), api_binary()) ->
                                 boolean().
%% no account id and no doc id (ie initial create with no account)
is_sip_creds_unique('undefined', _, _, 'undefined') -> 'true';
is_sip_creds_unique(AccountDb, Realm, Username, DeviceId) ->
    is_creds_locally_unique(AccountDb, Username, DeviceId)
        andalso is_creds_global_unique(Realm, Username, DeviceId).

is_creds_locally_unique(AccountDb, Username, DeviceId) ->
    ViewOptions = [{'key', kz_util:to_lower_binary(Username)}],
    case kz_datamgr:get_results(AccountDb, <<"devices/sip_credentials">>, ViewOptions) of
        {'ok', []} -> 'true';
        {'ok', [JObj]} -> kz_doc:id(JObj) =:= DeviceId;
        {'error', 'not_found'} -> 'true';
        _ -> 'false'
    end.

is_creds_global_unique(Realm, Username, DeviceId) ->
    ViewOptions = [{'key', [kz_util:to_lower_binary(Realm)
                           ,kz_util:to_lower_binary(Username)
                           ]
                   }],
    case kz_datamgr:get_results(?KZ_SIP_DB, <<"credentials/lookup">>, ViewOptions) of
        {'ok', []} -> 'true';
        {'ok', [JObj]} -> kz_doc:id(JObj) =:= DeviceId;
        {'error', 'not_found'} -> 'true';
        _ -> 'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_aggregate_device(api_binary(), cb_context:context()) -> boolean().
-spec maybe_aggregate_device(api_binary(), cb_context:context(), crossbar_status()) -> boolean().
maybe_aggregate_device(DeviceId, Context) ->
    maybe_aggregate_device(DeviceId, Context, cb_context:resp_status(Context)).
maybe_aggregate_device(DeviceId, Context, 'success') ->
    case kz_util:is_true(cb_context:fetch(Context, 'aggregate_device'))
        andalso ?DEVICES_ALLOW_AGGREGATES
    of
        'false' -> maybe_remove_aggregate(DeviceId, Context);
        'true' ->
            lager:debug("adding device to the sip auth aggregate"),
            {'ok', _} = kz_datamgr:ensure_saved(?KZ_SIP_DB, kz_doc:delete_revision(cb_context:doc(Context))),
            kapps_util:amqp_pool_send([], fun(_) -> kapi_switch:publish_reload_acls() end),
            'true'
    end;
maybe_aggregate_device(_, _, _) -> 'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_remove_aggregate(api_binary(), cb_context:context()) -> boolean().
-spec maybe_remove_aggregate(api_binary(), cb_context:context(), crossbar_status()) -> boolean().
maybe_remove_aggregate(DeviceId, Context) ->
    maybe_remove_aggregate(DeviceId, Context, cb_context:resp_status(Context)).

maybe_remove_aggregate('undefined', _Context, _RespStatus) -> 'false';
maybe_remove_aggregate(DeviceId, _Context, 'success') ->
    case kz_datamgr:open_doc(?KZ_SIP_DB, DeviceId) of
        {'error', 'not_found'} -> 'false';
        {'ok', JObj} ->
            _ = kz_datamgr:del_doc(?KZ_SIP_DB, JObj),
            kapps_util:amqp_pool_send([], fun(_) -> kapi_switch:publish_reload_acls() end),
            'true'
    end;
maybe_remove_aggregate(_, _, _) -> 'false'.


%%--------------------------------------------------------------------
%% @private
%% @doc Looks for device_type from DB but if it is undefined there,
%%   gets the one from request data.
%% @end
%%--------------------------------------------------------------------
-spec get_device_type(cb_context:context()) -> api_binary().
get_device_type(Context) ->
    case kz_device:device_type(cb_context:fetch(Context, 'db_doc')) of
        'undefined' ->
            kz_device:device_type(cb_context:req_data(Context));
        DeviceType -> DeviceType
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_add_mobile_mdn(cb_context:context()) -> cb_context:context().
maybe_add_mobile_mdn(Context) ->
    case kz_util:is_true(cb_context:fetch(Context, 'add_mobile_mdn')) of
        'true' -> add_mobile_mdn(Context);
        'false' -> Context
    end.

-spec add_mobile_mdn(cb_context:context()) -> cb_context:context().
add_mobile_mdn(Context) ->
    Normalized = knm_converters:normalize(get_mdn(Context)),
    Options = [{'assign_to', cb_context:account_id(Context)}
              ,{'dry_run', not cb_context:accepting_charges(Context)}
              ,{'module_name', ?CARRIER_MDN}
              ,{'state', ?NUMBER_STATE_IN_SERVICE}
              ],
    case knm_number:reconcile(Normalized, Options) of
        {'error', _R} ->
            lager:debug("unable to add mdn ~s to database: ~p", [Normalized, _R]),
            _ = crossbar_doc:delete(Context),
            cb_context:add_system_error('datastore_fault', Context);
        _Else ->
            lager:debug("created new mdn ~s", [Normalized]),
            set_mobile_public_fields(Normalized, Context)
    end.

-spec set_mobile_public_fields(ne_binary(), cb_context:context()) -> cb_context:context().
set_mobile_public_fields(Normalized, Context) ->
    AuthAccountId = cb_context:auth_account_id(Context),
    MobileField =
        kz_json:from_list(
          [{<<"provider">>, <<"tower-of-power">>}
          ,{<<"authorizing">>, kz_json:from_list(
                                 [{<<"account-id">>, AuthAccountId}
                                 ])}
          ,{<<"device-id">>, kz_doc:id(cb_context:doc(Context))}
          ]),
    PublicFields = kz_json:from_list([{<<"mobile">>, MobileField}]),
    Options = [{'auth_by', AuthAccountId}
              ,{'dry_run', not cb_context:accepting_charges(Context)}
              ,{'public_fields', PublicFields}
              ],
    case knm_number:move(Normalized, cb_context:account_id(Context), Options) of
        {'error', _R} ->
            lager:debug("unable to update public fields on mdn ~s: ~p"
                       ,[Normalized, _R]),
            _ = crossbar_doc:delete(Context),
            cb_context:add_system_error('datastore_fault', Context);
        _Else ->
            lager:debug("set public fields for new mdn ~s to ~s"
                       ,[Normalized, kz_json:encode(PublicFields)]),
            maybe_remove_mobile_mdn(Context)
    end.

-spec maybe_remove_mobile_mdn(cb_context:context()) -> cb_context:context().
maybe_remove_mobile_mdn(Context) ->
    case kz_util:is_true(cb_context:fetch(Context, 'remove_mobile_mdn')) of
        'true' -> remove_mobile_mdn(Context);
        'false' -> Context
    end.

-spec remove_mobile_mdn(cb_context:context()) -> cb_context:context().
remove_mobile_mdn(Context) ->
    case kz_json:get_ne_value(?KEY_MOBILE_MDN, cb_context:fetch(Context, 'db_doc')) of
        'undefined' -> Context;
        MDN -> remove_if_mobile(MDN, Context)
    end.

-spec remove_if_mobile(ne_binary(), cb_context:context()) -> cb_context:context().
remove_if_mobile(MDN, Context) ->
    Normalized = knm_converters:normalize(MDN),
    case knm_number:get(Normalized) of
        {'ok', Number} ->
            case kz_json:get_ne_value(<<"mobile">>, knm_number:to_public_json(Number)) of
                'undefined' -> Context;
                Mobile ->
                    lager:debug("removing mdn ~s with mobile properties ~s"
                               ,[Normalized, kz_json:encode(Mobile)]),
                    Options = [{'auth_by', ?KNM_DEFAULT_AUTH_BY}
                              ,{'dry_run', not cb_context:accepting_charges(Context)}
                              ],
                    _ = knm_number:release(Normalized, Options),
                    Context
            end;
        {'error', _R} ->
            lager:debug("unable to fetch mdn ~s for removal: ~p", [Normalized, _R]),
            Context
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_mdn(cb_context:context()) -> api_binary().
get_mdn(Context) ->
    case cb_context:req_value(Context, ?KEY_MOBILE_MDN) of
        'undefined' ->
            kz_json:get_ne_value(?KEY_MOBILE_MDN, cb_context:fetch(Context, 'db_doc'));
        MDN -> MDN
    end.

-spec has_mdn_changed(cb_context:context()) -> boolean().
has_mdn_changed(Context) ->
    NewMDN = cb_context:req_value(Context, ?KEY_MOBILE_MDN),
    case kz_json:get_ne_value(?KEY_MOBILE_MDN, cb_context:fetch(Context, 'db_doc')) of
        'undefined' -> 'true';
        OldMDN -> knm_converters:normalize(NewMDN) =/=
                      knm_converters:normalize(OldMDN)
    end.
