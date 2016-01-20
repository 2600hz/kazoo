%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz
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
         ,delete/2
         ,lookup_regs/1
        ]).

-include("../crossbar.hrl").

-define(STATUS_PATH_TOKEN, <<"status">>).
-define(CHECK_SYNC_PATH_TOKEN, <<"sync">>).

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".devices">>).

-define(CB_LIST, <<"devices/crossbar_listing">>).
-define(OWNER_LIST, <<"devices/listing_by_owner">>).
-define(CB_LIST_MAC, <<"devices/listing_by_macaddress">>).

-define(KEY_MAC_ADDRESS, <<"mac_address">>).

%%%===================================================================
%%% API
%%%===================================================================
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
                ,{<<"v2_resource.execute.delete.devices">>, 'delete'}
               ],
    cb_modules_util:bind(?MODULE, Bindings),

    crossbar_bindings:bind(
      <<"v2_resource.finish_request.*.devices">>
      ,'crossbar_services'
      ,'reconcile'
     ).

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
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

allowed_methods(_DeviceId, ?CHECK_SYNC_PATH_TOKEN) ->
    [?HTTP_POST].

allowed_methods(_, ?QUICKCALL_PATH_TOKEN, _) ->
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

resource_exists(_) -> 'true'.

resource_exists(_DeviceId, ?CHECK_SYNC_PATH_TOKEN) -> 'true'.

resource_exists(_, ?QUICKCALL_PATH_TOKEN, _) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Ensure we will be able to bill for devices
%% @end
%%--------------------------------------------------------------------
billing(Context) ->
    billing(Context, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

billing(Context, ?HTTP_GET, [{<<"devices">>, _}|_]) ->
    Context;
billing(Context, _ReqVerb, [{<<"devices">>, _}|_Nouns]) ->
    try wh_services:allow_updates(cb_context:account_id(Context)) of
        'true' ->
            lager:debug("allowing service updates"),
            Context
    catch
        'throw':{Error, Reason} ->
            lager:debug("account ~s is not allowed to make billing updates: ~s: ~p"
                        ,[props:get_value(<<"accounts">>, _Nouns)
                          ,Error
                          ,Reason
                         ]
                       ),
            crossbar_util:response('error', wh_util:to_binary(Error), 500, Reason, Context)
    end;
billing(Context, _ReqVerb, _Nouns) -> Context.

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
    case couch_mgr:open_cache_doc(cb_context:account_db(Context), DeviceId) of
        {'ok', _} -> cb_context:set_device_id(Context, DeviceId);
        {'error', 'not_found'} ->
            cb_context:add_system_error(
              'bad_identifier'
              ,wh_json:from_list([{<<"cause">>, DeviceId}])
              ,Context
             );
        {'error', _R} -> crossbar_util:response_db_fatal(Context)
    end.

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
validate_device(Context, DeviceId, ?HTTP_DELETE) ->
    load_device(DeviceId, Context).

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
    _ = wh_util:spawn(fun crossbar_util:flush_registration/1, [Context]),
    case changed_mac_address(Context) of
        'true' ->
            _ = crossbar_util:maybe_refresh_fs_xml('device', Context),
            Context1 = cb_modules_util:take_sync_field(Context),
            Context2 = crossbar_doc:save(Context1),
            _ = maybe_aggregate_device(DeviceId, Context2),
            _ = wh_util:spawn(
                  fun() ->
                          _ = provisioner_util:maybe_provision(Context2),
                          _ = provisioner_util:maybe_sync_sip_data(Context1, 'device')
                  end),
            Context1;
        'false' ->
            error_used_mac_address(Context)
    end.

-spec post(cb_context:context(), path_token(), path_token()) ->
                  cb_context:context().
post(Context, DeviceId, ?CHECK_SYNC_PATH_TOKEN) ->
    lager:debug("publishing check_sync for ~s", [DeviceId]),
    Context1 = cb_context:store(Context, 'sync', 'force'),
    _ = provisioner_util:maybe_sync_sip_data(Context1, 'device'),
    crossbar_util:response_202(<<"sync request sent">>, Context).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Callback =
        fun() ->
            Context1 = crossbar_doc:save(Context),
            _ = maybe_aggregate_device('undefined', Context1),
            _ = wh_util:spawn(fun provisioner_util:maybe_provision/1, [Context1]),
            Context1
        end,
    crossbar_services:maybe_dry_run(Context, Callback).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, DeviceId) ->
    _ = crossbar_util:refresh_fs_xml(Context),
    Context1 = crossbar_doc:delete(Context),
    _ = crossbar_util:flush_registration(Context),
    _ = wh_util:spawn(fun provisioner_util:maybe_delete_provision/1, [Context]),
    _ = maybe_remove_aggregate(DeviceId, Context),
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

-spec load_users_device_summary(cb_context:context(), ne_binary()) ->
                                       cb_context:context().
load_users_device_summary(Context, UserId) ->
    crossbar_doc:load_view(?OWNER_LIST
                           ,[{'key', UserId}]
                           ,Context
                           ,fun normalize_view_results/2
                          ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_request(api_binary(), cb_context:context()) -> cb_context:context().
validate_request('undefined', Context) ->
    check_mac_address('undefined', Context);
validate_request(DeviceId, Context) ->
    prepare_outbound_flags(DeviceId, Context).

-spec changed_mac_address(cb_context:context()) -> boolean().
changed_mac_address(Context) ->
    NewAddress = cb_context:req_value(Context, ?KEY_MAC_ADDRESS),
    OldAddress = wh_json:get_ne_value(?KEY_MAC_ADDRESS, cb_context:fetch(Context, 'db_doc')),
    case NewAddress =:= OldAddress of
        'true' -> 'true';
        'false' ->
            unique_mac_address(NewAddress, Context)
    end.

-spec check_mac_address(api_binary(), cb_context:context()) -> cb_context:context().
check_mac_address(DeviceId, Context) ->
    MacAddress = cb_context:req_value(Context, ?KEY_MAC_ADDRESS),
    case unique_mac_address(MacAddress, Context) of
        'true' ->
            prepare_outbound_flags(DeviceId, Context);
        'false' ->
            error_used_mac_address(Context)
    end.

-spec unique_mac_address(api_binary(), cb_context:context()) -> boolean().
unique_mac_address('undefined', _Context) -> 'true';
unique_mac_address(MacAddress, Context) ->
    DbName = cb_context:account_db(Context),
    not lists:member(MacAddress, get_mac_addresses(DbName))
        andalso not provisioner_util:is_mac_address_in_use(Context, MacAddress).

-spec error_used_mac_address(cb_context:context()) -> cb_context:context().
error_used_mac_address(Context) ->
    MacAddress = cb_context:req_value(Context, ?KEY_MAC_ADDRESS),
    cb_context:add_validation_error(
      ?KEY_MAC_ADDRESS
      ,<<"unique">>
      ,wh_json:from_list(
         [{<<"message">>, <<"Mac address already in use">>}
          ,{<<"cause">>, MacAddress}
         ])
      ,Context
     ).

-spec get_mac_addresses(ne_binary()) -> ne_binaries().
get_mac_addresses(DbName) ->
    case couch_mgr:get_all_results(DbName, ?CB_LIST_MAC) of
        {'ok', AdJObj} -> couch_mgr:get_result_keys(AdJObj);
        _ -> []
    end.

-spec prepare_outbound_flags(api_binary(), cb_context:context()) -> cb_context:context().
prepare_outbound_flags(DeviceId, Context) ->
    JObj = case cb_context:req_value(Context, <<"outbound_flags">>) of
               'undefined' -> cb_context:req_data(Context);
               [] -> cb_context:req_data(Context);
               Flags when is_list(Flags) ->
                   OutboundFlags = [wh_util:strip_binary(Flag)
                                    || Flag <- Flags
                                   ],
                   wh_json:set_value(<<"outbound_flags">>, OutboundFlags, cb_context:req_data(Context));
               _Else ->
                   wh_json:set_value(<<"outbound_flags">>, [], cb_context:req_data(Context))
           end,
    prepare_device_realm(DeviceId, cb_context:set_req_data(Context, JObj)).

-spec prepare_device_realm(api_binary(), cb_context:context()) -> cb_context:context().
prepare_device_realm(DeviceId, Context) ->
    AccountRealm = wh_util:get_account_realm(cb_context:account_id(Context)),
    Realm = cb_context:req_value(Context, [<<"sip">>, <<"realm">>], AccountRealm),
    case AccountRealm =:= Realm of
        'true' ->
            JObj = wh_json:delete_key([<<"sip">>, <<"realm">>], cb_context:req_data(Context)),
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
            C = cb_context:add_validation_error(
                  [<<"sip">>, <<"method">>]
                  ,<<"enum">>
                  ,wh_json:from_list([{<<"message">>, <<"SIP authentication method is invalid">>}
                                      ,{<<"target">>, [<<"password">>, <<"ip">>]}
                                      ,{<<"cause">>, Else}
                                     ])
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
            C = cb_context:add_validation_error(
                  [<<"sip">>, <<"username">>]
                  ,<<"unique">>
                  ,wh_json:from_list([{<<"message">>, <<"SIP credentials already in use">>}
                                      ,{<<"cause">>, Username}
                                     ])
                  ,Context
                 ),
            check_emergency_caller_id(DeviceId, C)
    end.

-spec validate_device_ip(ne_binary(), api_binary(), cb_context:context()) ->
                                cb_context:context().
validate_device_ip(IP, DeviceId, Context) ->
    case wh_network_utils:is_ipv4(IP) of
        'true' ->
            validate_device_ip_unique(IP, DeviceId, Context);
        'false' ->
            C = cb_context:add_validation_error(
                  [<<"sip">>, <<"ip">>]
                  ,<<"type">>
                  ,wh_json:from_list([{<<"message">>, <<"Must be a valid IPv4 RFC 791">>}
                                      ,{<<"cause">>, IP}
                                     ])
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
            C = cb_context:add_validation_error(
                  [<<"sip">>, <<"ip">>]
                  ,<<"unique">>
                  ,wh_json:from_list(
                     [{<<"message">>, <<"SIP IP already in use">>}
                      ,{<<"cause">>, IP}
                     ])
                  ,Context
                 ),
            check_emergency_caller_id(DeviceId, C)
    end.

-spec check_emergency_caller_id(api_binary(), cb_context:context()) -> cb_context:context().
check_emergency_caller_id(DeviceId, Context) ->
    Context1 = crossbar_util:format_emergency_caller_id_number(Context),
    check_device_schema(DeviceId, Context1).

-spec check_device_schema(api_binary(), cb_context:context()) -> cb_context:context().
check_device_schema(DeviceId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(DeviceId, C) end,
    cb_context:validate_request_data(<<"devices">>
                                     ,Context
                                     ,OnSuccess
                                    ).

-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    Props = [{<<"pvt_type">>, <<"device">>}],
    cb_context:set_doc(Context, wh_json:set_values(Props, cb_context:doc(Context)));
on_successful_validation(DeviceId, Context) ->
    crossbar_doc:load_merge(DeviceId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a device document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_device(ne_binary(), cb_context:context()) -> cb_context:context().
load_device(DeviceId, Context) ->
    crossbar_doc:load(DeviceId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieve the status of the devices linked to the account
%% Reads registered devices in registrations, then map to devices of the account
%% @end
%%--------------------------------------------------------------------
-spec load_device_status(cb_context:context()) -> cb_context:context().
load_device_status(Context) ->
    AccountRealm = wh_util:get_account_realm(cb_context:account_id(Context)),
    RegStatuses = lookup_regs(AccountRealm),
    lager:debug("reg statuses: ~p", [RegStatuses]),
    crossbar_util:response(RegStatuses, Context).

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
%% Returns the complete list of registrations in a the first registrar
%% to respond for a given account realm.  This is not 100% accurate
%% as an endpoint might be stored in another registrar, but it is
%% accurate enough for the status icons.
%% @end
%%--------------------------------------------------------------------
-spec lookup_regs(ne_binary()) -> wh_json:objects().
lookup_regs(AccountRealm) ->
    Req = [{<<"Realm">>, AccountRealm}
           ,{<<"Fields">>, [<<"Authorizing-ID">>]}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case whapps_util:amqp_pool_collect(Req
                                       ,fun wapi_registration:publish_query_req/1
                                       ,{'ecallmgr', 'true'}
                                      )
    of
        {'error', _E} ->
            lager:debug("error getting reg: ~p", [_E]),
            [];
        {_, JObjs} ->
            [wh_json:from_list([{<<"device_id">>, AuthorizingId}
                                ,{<<"registered">>, 'true'}
                               ])
             || AuthorizingId <- extract_device_registrations(JObjs)
            ]
    end.

-spec extract_device_registrations(wh_json:objects()) -> ne_binaries().
extract_device_registrations(JObjs) ->
    sets:to_list(extract_device_registrations(JObjs, sets:new())).

-spec extract_device_registrations(wh_json:objects(), sets:set()) -> sets:set().
extract_device_registrations([], Set) -> Set;
extract_device_registrations([JObj|JObjs], Set) ->
    Fields = wh_json:get_value(<<"Fields">>, JObj, []),
    S = lists:foldl(fun(J, S) ->
                            case wh_json:get_ne_value(<<"Authorizing-ID">>, J) of
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
    ViewOptions = [{<<"key">>, wh_util:to_lower_binary(Username)}],
    case couch_mgr:get_results(AccountDb, <<"devices/sip_credentials">>, ViewOptions) of
        {'ok', []} -> 'true';
        {'ok', [JObj]} -> wh_doc:id(JObj) =:= DeviceId;
        {'error', 'not_found'} -> 'true';
        _ -> 'false'
    end.

is_creds_global_unique(Realm, Username, DeviceId) ->
    ViewOptions = [{<<"key">>, [wh_util:to_lower_binary(Realm)
                                ,wh_util:to_lower_binary(Username)
                               ]
                   }],
    case couch_mgr:get_results(?WH_SIP_DB, <<"credentials/lookup">>, ViewOptions) of
        {'ok', []} -> 'true';
        {'ok', [JObj]} -> wh_doc:id(JObj) =:= DeviceId;
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
    case wh_util:is_true(cb_context:fetch(Context, 'aggregate_device'))
        andalso whapps_config:get_is_true(?MOD_CONFIG_CAT, <<"allow_aggregates">>, 'true')
    of
        'false' ->
            maybe_remove_aggregate(DeviceId, Context);
        'true' ->
            lager:debug("adding device to the sip auth aggregate"),
            {'ok', _} = couch_mgr:ensure_saved(?WH_SIP_DB, wh_doc:delete_revision(cb_context:doc(Context))),
            whapps_util:amqp_pool_send([], fun(_) -> wapi_switch:publish_reload_acls() end),
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
    case couch_mgr:open_doc(?WH_SIP_DB, DeviceId) of
        {'ok', JObj} ->
            _ = couch_mgr:del_doc(?WH_SIP_DB, JObj),
            whapps_util:amqp_pool_send([], fun(_) -> wapi_switch:publish_reload_acls() end),
            'true';
        {'error', 'not_found'} -> 'false'
    end;
maybe_remove_aggregate(_, _, _) -> 'false'.
