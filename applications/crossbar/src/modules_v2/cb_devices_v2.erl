%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz
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
         ,allowed_methods/0, allowed_methods/1, allowed_methods/3
         ,resource_exists/0, resource_exists/1, resource_exists/3
         ,billing/1
         ,authenticate/1
         ,authorize/1
         ,validate/1, validate/2, validate/4
         ,put/1
         ,post/2
         ,delete/2
         ,lookup_regs/1
        ]).

-include("../crossbar.hrl").

-define(QUICKCALL_PATH_TOKEN, <<"quickcall">>).
-define(QUICKCALL_URL, [{<<"devices">>, [_, ?QUICKCALL_PATH_TOKEN, _]}
                        ,{?WH_ACCOUNTS_DB, [_]}
                       ]).

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".devices">>).

-define(CB_LIST, <<"devices/crossbar_listing">>).
-define(CB_LIST_MAC, <<"devices/listing_by_macaddress">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v2_resource.allowed_methods.devices">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"v2_resource.resource_exists.devices">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"v2_resource.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"v2_resource.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"v2_resource.billing">>, ?MODULE, 'billing'),
    _ = crossbar_bindings:bind(<<"v2_resource.validate.devices">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"v2_resource.execute.put.devices">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"v2_resource.execute.post.devices">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"v2_resource.execute.delete.devices">>, ?MODULE, 'delete'),
    crossbar_bindings:bind(<<"v1_resource.finish_request.*.devices">>, 'cb_modules_util', 'reconcile_services').

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
-spec allowed_methods(path_token(), path_token(), path_token()) -> http_methods().

allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

allowed_methods(<<"status">>) ->
    [?HTTP_GET];
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

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
-spec resource_exists(path_token(), path_token(), path_token()) -> 'true'.

resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
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
billing(Context, _ReqVerb, [{<<"devices">>, _}|_]) ->
    try wh_services:allow_updates(cb_context:account_id(Context)) of
        'true' -> Context
    catch
        'throw':{Error, Reason} ->
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
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_devices(Context, cb_context:req_verb(Context)).

validate_devices(Context, ?HTTP_GET) ->
    load_device_summary(Context);
validate_devices(Context, ?HTTP_PUT) ->
    validate_request('undefined', Context).

validate(Context, PathToken) ->
    validate_device(Context, PathToken, cb_context:req_verb(Context)).

validate_device(Context, <<"status">>, ?HTTP_GET) ->
    load_device_status(Context);
validate_device(Context, DeviceId, ?HTTP_GET) ->
    load_device(DeviceId, Context);
validate_device(Context, DeviceId, ?HTTP_POST) ->
    validate_request(DeviceId, Context);
validate_device(Context, DeviceId, ?HTTP_DELETE) ->
    load_device(DeviceId, Context).

validate(Context, DeviceId, ?QUICKCALL_PATH_TOKEN, _) ->
    Context1 = maybe_validate_quickcall(load_device(DeviceId, Context)),
    case cb_context:has_errors(Context1) of
        'true' -> Context1;
        'false' ->
            cb_modules_util:maybe_originate_quickcall(Context1)
    end.

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, DeviceId) ->
    case changed_mac_address(Context) of
        'true' ->
            Context1 = crossbar_doc:save(Context),
            _ = maybe_aggregate_device(DeviceId, Context1),
            _ = registration_update(Context),
            _ = provisioner_util:maybe_provision(Context1),
            Context1;
        'false' ->
            error_used_mac_address(Context)
    end.

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    DryRun = (not wh_json:is_true(<<"accept_charges">>, cb_context:req_json(Context), 'false')),
    put_resp(DryRun, Context).

put_resp('true', Context) ->
    RespJObj = dry_run(Context),
    case wh_json:is_empty(RespJObj) of
        'false' -> crossbar_util:response_402(RespJObj, Context);
        'true' ->
            NewReqJObj = wh_json:set_value(<<"accept_charges">>, <<"true">>, cb_context:req_json(Context)),
            ?MODULE:put(cb_context:set_req_json(Context, NewReqJObj))
    end;
put_resp('false', Context) ->
    Context1 = crossbar_doc:save(Context),
    _ = maybe_aggregate_device('undefined', Context1),
    _ = provisioner_util:maybe_provision(Context1),
    Context1.

-spec dry_run(cb_context:context()) -> wh_json:object().
dry_run(Context) ->
    JObj = cb_context:doc(Context),
    AccountId = cb_context:account_id(Context),

    DeviceType = wh_json:get_value(<<"device_type">>, JObj),

    Services = wh_services:fetch(AccountId),
    UpdateServices = wh_service_devices:reconcile(Services, DeviceType),

    Charges = wh_services:activation_charges(<<"devices">>, DeviceType, Services),

    case Charges > 0 of
        'false' -> wh_services:calculate_charges(UpdateServices, []);
        'true' ->
            Transaction = wh_transaction:debit(AccountId, wht_util:dollars_to_units(Charges)),
            Desc = <<"activation charges for "
                     ,DeviceType/binary
                     ," "
                     ,(wh_json:get_value(<<"name">>, JObj))/binary
                   >>,
            Transaction2 = wh_transaction:set_description(Desc, Transaction),
            wh_services:calculate_charges(UpdateServices, [Transaction2])
    end.

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, DeviceId) ->
    Context1 = crossbar_doc:delete(Context),
    _ = registration_update(Context),
    _ = provisioner_util:maybe_delete_provision(Context),
    _ = maybe_remove_aggregate(DeviceId, Context),
    Context1.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec registration_update(cb_context:context()) -> 'ok'.
registration_update(Context) ->
    cb_devices_v1:registration_update(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec load_device_summary(cb_context:context()) -> cb_context:context().
load_device_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

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
    NewAddress = cb_context:req_value(Context, <<"mac_address">>),
    OldAddress = wh_json:get_ne_value(<<"mac_address">>, cb_context:fetch(Context, 'db_doc')),
    case NewAddress =:= OldAddress of
        'true' -> 'true';
        'false' ->
            unique_mac_address(NewAddress, cb_context:account_db(Context))
    end.

-spec check_mac_address(api_binary(), cb_context:context()) -> cb_context:context().
check_mac_address(DeviceId, Context) ->
    case unique_mac_address(cb_context:req_value(Context, <<"mac_address">>)
                            ,cb_context:account_db(Context)
                           )
    of
        'true' ->
            prepare_outbound_flags(DeviceId, Context);
        'false' ->
           error_used_mac_address(Context)
    end.

-spec unique_mac_address(api_binary(), ne_binary()) -> boolean().
unique_mac_address('undefined', _) -> 'true';
unique_mac_address(MacAddress, DbName) ->
    not(lists:member(MacAddress, get_mac_addresses(DbName))).

-spec error_used_mac_address(cb_context:context()) -> cb_context:context().
error_used_mac_address(Context) ->
    cb_context:add_validation_error(<<"mac_address">>
                                    ,<<"unique">>
                                    ,<<"mac address already in use">>
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
    AccountRealm = crossbar_util:get_account_realm(Context),
    Realm = cb_context:req_value(Context, [<<"sip">>, <<"realm">>], AccountRealm),
    case AccountRealm =:= Realm of
        'true' ->
            JObj = wh_json:delete_key([<<"sip">>, <<"realm">>], cb_context:req_data(Context)),
            validate_device_creds(Realm, DeviceId, cb_context:set_req_data(Context, JObj));
        'false' ->
            validate_device_creds(Realm, DeviceId, cb_context:store(Context, 'aggregate_device', 'true'))
    end.

-spec validate_device_creds(ne_binary(), api_binary(), cb_context:context()) -> cb_context:context().
validate_device_creds(Realm, DeviceId, Context) ->
    case cb_context:req_value(Context, [<<"sip">>, <<"method">>], <<"password">>) of
        <<"password">> -> validate_device_password(Realm, DeviceId, Context);
        <<"ip">> ->
            IP = cb_context:req_value(Context, [<<"sip">>, <<"ip">>]),
            validate_device_ip(IP, DeviceId, Context);
        _Else ->
            C = cb_context:add_validation_error([<<"sip">>, <<"method">>]
                                               ,<<"enum">>
                                               ,<<"SIP authentication method is invalid">>
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
            C = cb_context:add_validation_error([<<"sip">>, <<"username">>]
                                                ,<<"unique">>
                                                ,<<"SIP credentials already in use">>
                                                ,Context
                                               ),
            check_emergency_caller_id(DeviceId, C)
    end.

-spec validate_device_ip(ne_binary(), api_binary(), cb_context:context()) -> cb_context:context().
validate_device_ip(IP, DeviceId, Context) ->
    case wh_network_utils:is_ipv4(IP) of
        'true' -> validate_device_ip_unique(IP, DeviceId, Context);
        'false' ->
            C = cb_context:add_validation_error([<<"sip">>, <<"ip">>]
                                                ,<<"type">>
                                                ,<<"Must be a valid IPv4 RFC 791">>
                                                ,Context
                                               ),
            check_emergency_caller_id(DeviceId, C)
    end.

validate_device_ip_unique(IP, DeviceId, Context) ->
    case cb_devices_utils:is_ip_unique(IP, DeviceId) of
        'true' ->
            check_emergency_caller_id(DeviceId, cb_context:store(Context, 'aggregate_device', 'true'));
        'false' ->
            C = cb_context:add_validation_error([<<"sip">>, <<"ip">>]
                                                ,<<"unique">>
                                                ,<<"SIP IP already in use">>
                                                ,Context),
            check_emergency_caller_id(DeviceId, C)
    end.

-spec check_emergency_caller_id(api_binary(), cb_context:context()) -> cb_context:context().
check_emergency_caller_id(DeviceId, Context) ->
    Context1 = crossbar_util:format_emergency_caller_id_number(Context),
    check_device_schema(DeviceId, Context1).

check_device_schema(DeviceId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(DeviceId, C) end,
    cb_context:validate_request_data(<<"devices">>, Context, OnSuccess).

on_successful_validation('undefined', Context) ->
    Props = [{<<"pvt_type">>, <<"device">>}],
    cb_context:set_doc(Context, wh_json:set_values(Props, cb_context:doc(Context)));
on_successful_validation(DeviceId, Context) ->
    crossbar_doc:load_merge(DeviceId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
maybe_validate_quickcall(Context) ->
    maybe_validate_quickcall(Context, cb_context:resp_status(Context)).
maybe_validate_quickcall(Context, 'success') ->
    case kz_buckets:consume_tokens(cb_modules_util:bucket_name(Context)
                                   ,cb_modules_util:token_cost(Context, 1, [?QUICKCALL_PATH_TOKEN])
                                  )
        of
        'true' -> maybe_allow_quickcalls(Context);
        'false' -> cb_context:add_system_error('too_many_requests', Context)
    end.

-spec maybe_allow_quickcalls(cb_context:context()) -> cb_context:context().
maybe_allow_quickcalls(Context) ->
    case (not wh_util:is_empty(cb_context:auth_token(Context)))
        orelse wh_util:is_true(cb_context:req_value(Context, <<"allow_anonymous_quickcalls">>))
    of
        'false' -> cb_context:add_system_error('invalid_credentials', Context);
        'true' -> Context
    end.

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
    AccountRealm = crossbar_util:get_account_realm(Context),
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

-spec extract_device_registrations(wh_json:objects(), set()) -> set().
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
        {'ok', [JObj]} -> wh_json:get_value(<<"id">>, JObj) =:= DeviceId;
        {'error', 'not_found'} -> 'true';
        _ -> 'false'
    end.

is_creds_global_unique(Realm, Username, DeviceId) ->
    ViewOptions = [{<<"key">>, [wh_util:to_lower_binary(Realm)
                                , wh_util:to_lower_binary(Username)
                               ]
                   }],
    case couch_mgr:get_results(?WH_SIP_DB, <<"credentials/lookup">>, ViewOptions) of
        {'ok', []} -> 'true';
        {'ok', [JObj]} -> wh_json:get_value(<<"id">>, JObj) =:= DeviceId;
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
            {'ok', _} = couch_mgr:ensure_saved(?WH_SIP_DB, wh_json:delete_key(<<"_rev">>, cb_context:doc(Context))),
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
