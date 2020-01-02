%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
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
-module(cb_devices).

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
-include_lib("kazoo_numbers/include/knm_phone_number.hrl").

-define(STATUS_PATH_TOKEN, <<"status">>).
-define(CHECK_SYNC_PATH_TOKEN, <<"sync">>).

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".devices">>).

-define(CB_LIST, <<"devices/crossbar_listing">>).
-define(OWNER_LIST, <<"devices/listing_by_owner">>).
-define(CB_LIST_MAC, <<"devices/listing_by_macaddress">>).

-define(KEY_MAC_ADDRESS, <<"mac_address">>).
-define(KEY_MDN, [<<"mobile">>, <<"mdn">>]).
-define(KEY_SIP_REALM, [<<"sip">>, <<"realm">>]).
-define(KEY_SIP_METHOD, [<<"sip">>, <<"method">>]).
-define(KEY_SIP_IP, [<<"sip">>, <<"ip">>]).
-define(KEY_SIP_USERNAME, [<<"sip">>, <<"username">>]).
-define(KEY_OUTBOUND_FLAGS, <<"outbound_flags">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    Bindings = [{<<"*.allowed_methods.devices">>, 'allowed_methods'}
               ,{<<"*.resource_exists.devices">>, 'resource_exists'}
               ,{<<"*.authenticate.devices">>, 'authenticate'}
               ,{<<"*.authorize.devices">>, 'authorize'}
               ,{<<"*.validate_resource.devices">>, 'validate_resource'}
               ,{<<"*.validate.devices">>, 'validate'}
               ,{<<"*.execute.put.devices">>, 'put'}
               ,{<<"*.execute.post.devices">>, 'post'}
               ,{<<"*.execute.patch.devices">>, 'patch'}
               ,{<<"*.execute.delete.devices">>, 'delete'}
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
    case kz_datamgr:open_cache_doc(cb_context:account_id(Context), DeviceId) of
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
    validate_device('undefined', Context).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, PathToken) ->
    validate_device(Context, PathToken, cb_context:req_verb(Context)).

validate_device(Context, ?STATUS_PATH_TOKEN, ?HTTP_GET) ->
    load_device_status(Context);
validate_device(Context, DeviceId, ?HTTP_GET) ->
    load_device(DeviceId, Context);
validate_device(Context, DeviceId, ?HTTP_POST) ->
    validate_device(DeviceId, load_device(DeviceId, Context));
validate_device(Context, DeviceId, ?HTTP_PATCH) ->
    validate_patch(Context, DeviceId);
validate_device(Context, DeviceId, ?HTTP_PUT) ->
    validate_action(Context, DeviceId, cb_context:req_value(Context, <<"action">>));
validate_device(Context, DeviceId, ?HTTP_DELETE) ->
    load_device(DeviceId, Context).

validate_patch(Context, DeviceId) ->
    crossbar_doc:patch_and_validate(DeviceId, Context, fun validate_device/2).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, DeviceId, ?CHECK_SYNC_PATH_TOKEN) ->
    load_device(DeviceId, Context).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, DeviceId) ->
    Context1 = crossbar_doc:save(
                 cb_modules_util:take_sync_field(Context)
                ),
    handle_device_update(DeviceId, Context1).

-spec post(cb_context:context(), path_token(), path_token()) ->
          cb_context:context().
post(Context, DeviceId, ?CHECK_SYNC_PATH_TOKEN) ->
    lager:debug("publishing check_sync for ~s", [DeviceId]),
    _ = sync_sip_data(Context, 'true'),
    crossbar_util:response_202(<<"sync request sent">>, Context).

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, Id) ->
    post(Context, Id).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Context1 = crossbar_doc:save(Context),
    handle_device_update('undefined', Context1).

-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, DeviceId) ->
    put_action(Context, DeviceId, cb_context:req_value(Context, <<"action">>)).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, DeviceId) ->
    Context1 = crossbar_doc:delete(Context),
    handle_device_removal(DeviceId, Context1).

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
-spec validate_device(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_device(DeviceId, Context) ->
    Routines = [fun prepare_mac_address/2
               ,fun prepare_outbound_flags/2
               ,fun prepare_device_realm/2
               ,fun prepare_provisioner_fields/2
               ,fun prepare_emergency_caller_id/2
               ,fun validate_mdn/2
               ,fun validate_mac_address/2
               ,fun validate_device_creds/2
               ,fun check_device_type_change/2
               ,fun check_device_schema/2
               ],
    lists:foldl(fun(F, C) -> F(DeviceId, C) end
               ,Context
               ,Routines
               ).

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
-spec get_mac_address(cb_context:context()) -> kz_term:api_binary().
get_mac_address(Context) ->
    provisioner_util:cleanse_mac_address(
      cb_context:req_value(Context, ?KEY_MAC_ADDRESS)
     ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_mdn(kzd_devices:doc()) -> kz_term:api_binary().
get_mdn(JObj) ->
    case kz_json:get_ne_value(?KEY_MDN, JObj) of
        'undefined' -> 'undefined';
        MDN -> knm_converters:normalize(MDN)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec prepare_mac_address(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
prepare_mac_address(_DeviceId, Context) ->
    case get_mac_address(Context) of
        'undefined' -> Context;
        MacAddress ->
            JObj = kzd_devices:set_mac_address(cb_context:req_data(Context)
                                              ,MacAddress
                                              ),
            cb_context:set_req_data(Context, JObj)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec prepare_outbound_flags(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
prepare_outbound_flags(_DeviceId, Context) ->
    JObj = case cb_context:req_value(Context, ?KEY_OUTBOUND_FLAGS) of
               'undefined' -> cb_context:req_data(Context);
               [] -> cb_context:req_data(Context);
               Flags when is_list(Flags) ->
                   OutboundFlags = [kz_binary:strip(Flag) || Flag <- Flags],
                   kz_json:set_value(?KEY_OUTBOUND_FLAGS, OutboundFlags, cb_context:req_data(Context));
               _Else ->
                   kz_json:set_value(?KEY_OUTBOUND_FLAGS, [], cb_context:req_data(Context))
           end,
    cb_context:set_req_data(Context, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec prepare_device_realm(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
prepare_device_realm(_DeviceId, Context) ->
    AccountRealm = kzd_accounts:fetch_realm(cb_context:account_id(Context)),
    Realm = cb_context:req_value(Context, ?KEY_SIP_REALM, AccountRealm),
    case AccountRealm =:= Realm of
        'true' ->
            JObj = kz_json:delete_key(?KEY_SIP_REALM, cb_context:req_data(Context)),
            cb_context:set_req_data(Context, JObj);
        'false' ->
            cb_context:store(Context, 'aggregate_device', 'true')
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec prepare_provisioner_fields(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
prepare_provisioner_fields(_DeviceId, Context) ->
    Keys = [<<"combo_keys">>
           ,<<"feature_keys">>
           ],
    ReqData = cb_context:req_data(Context),
    JObj = case cb_context:req_value(Context, <<"provision">>) of
               'undefined' -> ReqData;
               Provision ->
                   NewData = prune_null_provisioner_fields(Keys, Provision),
                   kz_json:set_value(<<"provision">>, NewData, ReqData)
           end,
    Setters = [{fun cb_context:set_req_data/2, JObj}
              ,{fun cb_context:store/3
               ,'unfiltered_req_data'
               ,ReqData
               }
              ],
    cb_context:setters(Context, Setters).

-spec prune_null_provisioner_fields(kz_json:keys(), kz_json:object()) -> kz_json:object().
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
filter_null_fields({_, 'null'}) -> 'false';
filter_null_fields(_) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec prepare_emergency_caller_id(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
prepare_emergency_caller_id(_DeviceId, Context) ->
    crossbar_util:format_emergency_caller_id_number(Context).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_mdn(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_mdn(DeviceId, Context) ->
    case get_device_type(Context) of
        <<"mobile">> -> check_mdn_undefined(DeviceId, Context);
        _Else -> Context
    end.

-spec check_mdn_undefined(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_mdn_undefined(DeviceId, Context) ->
    MDN = cb_context:req_value(Context, ?KEY_MDN),
    case kz_term:is_empty(MDN) of
        'true' -> error_mdn_undefined(Context);
        'false' ->
            NormalizedMDN = knm_converters:normalize(MDN),
            check_mdn_changed(NormalizedMDN, DeviceId, Context)
    end.

-spec error_mdn_undefined(cb_context:context()) -> cb_context:context().
error_mdn_undefined(Context) ->
    Msg = kz_json:from_list(
            [{<<"message">>, <<"Field is required but missing">>}]
           ),
    cb_context:add_validation_error(?KEY_MDN, <<"required">>, Msg, Context).

-spec check_mdn_changed(kz_term:ne_binary(), kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_mdn_changed(MDN, 'undefined', Context) ->
    check_mdn_taken(MDN, 'undefined', Context);
check_mdn_changed(MDN, DeviceId, Context) ->
    IsSuperAdmin = cb_context:is_superduper_admin(Context),
    case get_mdn(cb_context:fetch(Context, 'db_doc')) =/= MDN of
        'true' when IsSuperAdmin ->
            check_mdn_taken(MDN, DeviceId, Context);
        'true' -> error_mdn_changed(Context);
        'false' -> Context
    end.

-spec error_mdn_changed(cb_context:context()) -> cb_context:context().
error_mdn_changed(Context) ->
    _OldMDN = kz_json:get_ne_value(?KEY_MDN, cb_context:fetch(Context, 'db_doc')),
    NewMDN = cb_context:req_value(Context, ?KEY_MDN),
    lager:debug("mobile device number attempted to be changed from ~p to ~p"
               ,[_OldMDN, NewMDN]
               ),
    Msg = kz_json:from_list(
            [{<<"message">>, <<"Mobile Device Number cannot be changed">>}
            ,{<<"cause">>, NewMDN}
            ]),
    cb_context:add_validation_error(?KEY_MDN, <<"invalid">>, Msg, Context).

-spec check_mdn_taken(kz_term:ne_binary(), kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_mdn_taken(MDN, DeviceId, Context) ->
    case knm_number:get(MDN, knm_number_options:mdn_options()) of
        {'error', 'not_found'} ->
            lager:debug("endpoint mdn ~s is not taken", [MDN]),
            check_mdn_registered(DeviceId, Context);
        {'ok', _Number} ->
            lager:debug("mdn ~s taken", [MDN]),
            error_mdn_taken(MDN, Context);
        {'error', _R} ->
            lager:debug("number ~s taken: ~p", [MDN, _R]),
            error_mdn_taken(MDN, Context)
    end.

-spec error_mdn_taken(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
error_mdn_taken(MDN, Context) ->
    Msg = kz_json:from_list(
            [{<<"message">>, <<"Mobile Device Number already exists in the system">>}
            ,{<<"cause">>, MDN}
            ]),
    cb_context:add_validation_error(?KEY_MDN, <<"unique">>, Msg, Context).

-spec check_mdn_registered(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_mdn_registered(_DeviceId, Context) ->
    %%TODO: issue API request to TOP (if configured with URL) and validate
    %%   that the number is present in that system, if not stop the request
    Context.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_mac_address(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_mac_address('undefined', Context) ->
    check_mac_address('undefined', Context);
validate_mac_address(DeviceId, Context) ->
    MacAddress = get_mac_address(Context),
    CurrentAddress = get_current_mac_address(DeviceId, Context),
    case MacAddress =:= CurrentAddress of
        'false' -> check_mac_address(DeviceId, Context);
        'true' -> Context
    end.

-spec check_mac_address(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_mac_address(_DeviceId, Context) ->
    MacAddress = get_mac_address(Context),
    case unique_mac_address(MacAddress, Context) of
        'false' -> error_used_mac_address(Context);
        'true' -> Context
    end.

-spec error_used_mac_address(cb_context:context()) -> cb_context:context().
error_used_mac_address(Context) ->
    MacAddress = get_mac_address(Context),
    Msg = kz_json:from_list(
            [{<<"message">>, <<"MAC address already in use">>}
            ,{<<"cause">>, MacAddress}
            ]),
    cb_context:add_validation_error(?KEY_MAC_ADDRESS, <<"unique">>, Msg, Context).

-spec unique_mac_address(kz_term:api_binary(), cb_context:context()) -> boolean().
unique_mac_address('undefined', _Context) -> 'true';
unique_mac_address(MacAddress, Context) ->
    DbName = cb_context:db_name(Context),
    not lists:member(MacAddress, get_mac_addresses(DbName))
        andalso not provisioner_util:is_mac_address_in_use(MacAddress, cb_context:auth_token(Context)).

-spec get_mac_addresses(kz_term:ne_binary()) -> kz_term:ne_binaries().
get_mac_addresses(DbName) ->
    MACs = case kz_datamgr:get_all_results(DbName, ?CB_LIST_MAC) of
               {'ok', AdJObj} -> kz_datamgr:get_result_keys(AdJObj);
               _ -> []
           end,
    [provisioner_util:cleanse_mac_address(MAC) || MAC <- MACs].

-spec get_current_mac_address(kz_term:api_binary(), cb_context:context()) -> kz_term:api_binary().
get_current_mac_address('undefined', _Context) ->
    'undefined';
get_current_mac_address(_DeviceId, Context) ->
    JObj = cb_context:fetch(Context, 'db_doc', kz_json:new()),
    kzd_devices:mac_address(JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_device_creds(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_device_creds(DeviceId, Context) ->
    AccountRealm = kzd_accounts:fetch_realm(cb_context:account_id(Context)),
    Realm = cb_context:req_value(Context, ?KEY_SIP_REALM, AccountRealm),
    case cb_context:req_value(Context, ?KEY_SIP_METHOD, <<"password">>) of
        <<"password">> ->
            check_device_password(Realm, DeviceId, Context);
        <<"ip">> ->
            IP = cb_context:req_value(Context, ?KEY_SIP_IP),
            check_device_ip(IP, DeviceId, Context);
        Else ->
            Msg =
                kz_json:from_list([{<<"message">>, <<"SIP authentication method is invalid">>}
                                  ,{<<"target">>, [<<"password">>, <<"ip">>]}
                                  ,{<<"cause">>, Else}
                                  ]),
            cb_context:add_validation_error(?KEY_SIP_METHOD
                                           ,<<"enum">>
                                           ,Msg
                                           ,Context
                                           )
    end.

-spec check_device_password(kz_term:ne_binary(), kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_device_password(Realm, DeviceId, Context) ->
    Username = cb_context:req_value(Context, ?KEY_SIP_USERNAME),
    case check_sip_creds_unique(cb_context:account_id(Context), Realm, Username, DeviceId) of
        'true' -> Context;
        'false' ->
            Msg =
                kz_json:from_list([{<<"message">>, <<"SIP credentials already in use">>}
                                  ,{<<"cause">>, Username}
                                  ]),
            cb_context:add_validation_error(?KEY_SIP_USERNAME
                                           ,<<"unique">>
                                           ,Msg
                                           ,Context
                                           )
    end.

-spec check_sip_creds_unique(kz_term:api_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_binary()) ->
          boolean().
%% no account id and no doc id (ie initial create with no account)
check_sip_creds_unique('undefined', _, _, 'undefined') -> 'true';
check_sip_creds_unique(AccountId, Realm, Username, DeviceId) ->
    is_creds_locally_unique(AccountId, Username, DeviceId)
        andalso is_creds_global_unique(Realm, Username, DeviceId).

-spec is_creds_locally_unique(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_creds_locally_unique(AccountId, Username, DeviceId) ->
    ViewOptions = [{'key', kz_term:to_lower_binary(Username)}],
    case kz_datamgr:get_results(AccountId, <<"devices/sip_credentials">>, ViewOptions) of
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
                   }
                  ],
    case kz_datamgr:get_results(?KZ_SIP_DB, <<"credentials/lookup">>, ViewOptions) of
        {'ok', []} -> 'true';
        {'ok', [JObj]} -> kz_doc:id(JObj) =:= DeviceId;
        {'error', 'not_found'} -> 'true';
        _ -> 'false'
    end.

-spec check_device_ip(kz_term:ne_binary(), kz_term:api_binary(), cb_context:context()) ->
          cb_context:context().
check_device_ip(IP, DeviceId, Context) ->
    case kz_network_utils:is_ipv4(IP) of
        'true' ->
            check_device_ip_unique(IP, DeviceId, Context);
        'false' ->
            Msg =
                kz_json:from_list([{<<"message">>, <<"Must be a valid IPv4 RFC 791">>}
                                  ,{<<"cause">>, IP}
                                  ]),
            cb_context:add_validation_error(?KEY_SIP_IP
                                           ,<<"type">>
                                           ,Msg
                                           ,Context
                                           )
    end.

-spec check_device_ip_unique(kz_term:ne_binary(), kz_term:api_binary(), cb_context:context()) ->
          cb_context:context().
check_device_ip_unique(IP, DeviceId, Context) ->
    case cb_devices_utils:is_ip_unique(IP, DeviceId) of
        'true' ->
            cb_context:store(Context, 'aggregate_device', 'true');
        'false' ->
            Msg = kz_json:from_list(
                    [{<<"message">>, <<"SIP IP already in use">>}
                    ,{<<"cause">>, IP}
                    ]),
            cb_context:add_validation_error(?KEY_SIP_IP
                                           ,<<"unique">>
                                           ,Msg
                                           ,Context
                                           )
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec check_device_type_change(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_device_type_change('undefined', Context) ->
    check_device_schema('undefined', Context);
check_device_type_change(_DeviceId, Context) ->
    NewDeviceType = kzd_devices:device_type(cb_context:req_data(Context)),
    OldDeviceType = kzd_devices:device_type(cb_context:fetch(Context, 'db_doc')),
    IsSuperAdmin = cb_context:is_superduper_admin(Context),
    case NewDeviceType =:= OldDeviceType of
        'true' -> Context;
        'false' when IsSuperAdmin -> Context;
        'false' -> error_device_type_change(NewDeviceType, Context)
    end.

-spec error_device_type_change(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
error_device_type_change('undefined', Context) ->
    error_device_type_change(<<>>, Context);
error_device_type_change(DeviceType, Context) ->
    Msg =
        kz_json:from_list(
          [{<<"message">>, <<"Not authorized to change type of device">>}
          ,{<<"cause">>, DeviceType}
          ]),
    cb_context:add_validation_error(<<"device_type">>, <<"invalid">>, Msg, Context).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
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
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_device_update(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
handle_device_update(DeviceId, Context) ->
    case cb_context:resp_status(Context) =:= 'success' of
        'false' -> Context;
        'true' ->
            _ = kz_process:spawn(fun crossbar_util:flush_registration/1, [Context]),
            _ = crossbar_util:maybe_refresh_fs_xml('device', Context),
            Routines = [fun maybe_add_mdn/2
                       ,fun maybe_remove_previous_mdn/2
                       ,fun maybe_aggregate_device/2
                       ,fun maybe_update_provision/2
                       ],
            lists:foldl(fun(F, C) ->
                                case cb_context:resp_status(C) =:= 'success' of
                                    'true' -> F(DeviceId, C);
                                    'false' -> C
                                end
                        end
                       ,Context
                       ,Routines
                       )
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_aggregate_device(kz_term:api_ne_binary(), cb_context:context()) -> cb_context:context().
maybe_aggregate_device(DeviceId, Context) ->
    case kz_term:is_true(cb_context:fetch(Context, 'aggregate_device'))
        andalso ?DEVICES_ALLOW_AGGREGATES
    of
        'false' -> maybe_remove_aggregate(DeviceId, Context);
        'true' -> aggregate_device(Context)
    end.

-spec aggregate_device(cb_context:context()) -> cb_context:context().
aggregate_device(Context) ->
    Device = cb_context:doc(Context),
    lager:debug("adding device to the sip auth aggregate"),
    Doc = kz_doc:delete_revision(Device),
    Update = kz_json:to_proplist(kz_json:flatten(Doc)),
    UpdateOptions = [{'update', Update}
                    ,{'create', []}
                    ,{'ensure_saved', 'true'}
                    ],
    case kz_datamgr:update_doc(?KZ_SIP_DB, kz_doc:id(Device), UpdateOptions) of
        {'ok', _} ->
            _ = kz_amqp_worker:cast([], fun(_) -> kapi_switch:publish_reload_acls() end),
            Context;
        {'error', _R} ->
            lager:debug("failed to aggregate device: ~p", [_R]),
            crossbar_util:response_db_fatal(Context)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_update_provision(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
maybe_update_provision(_DeviceId, Context) ->
    _ = kz_process:spawn(fun update_provision/1, [Context]),
    Context.

-spec update_provision(cb_context:context()) -> 'ok'.
update_provision(Context) ->
    JObj = cb_context:doc(Context),
    Props = kz_json:to_proplist(
              kz_doc:private_fields(JObj)
             ),
    ReqData = cb_context:fetch(Context, 'unfiltered_req_data', JObj),
    NewDevice = kz_json:set_values(Props, ReqData),
    OldDevice = cb_context:fetch(Context, 'db_doc'),
    _ = provisioner_util:provision_device(NewDevice
                                         ,OldDevice
                                         ,#{'req_verb' => cb_context:req_verb(Context)
                                           ,'auth_token' => cb_context:auth_token(Context)
                                           }
                                         ),
    sync_sip_data(Context, 'false').

%%------------------------------------------------------------------------------
%% @doc There are only 2 possible scenarios for adding/updating a MDN:
%% 1. When the request is to create a new device (add).
%% 2. When the MDN has changed and the requester is a superduper admin (update).
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_mdn(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
maybe_add_mdn(_DeviceId, Context) ->
    MDN = get_mdn(cb_context:doc(Context)),
    case get_device_type(Context) =:= <<"mobile">>
        andalso kz_term:is_not_empty(MDN)
        andalso (is_new_device(Context)
                 orelse was_mdn_changed_and_user_is_super_admin(Context, MDN)
                )
    of
        'true' -> add_mdn(MDN, Context);
        'false' -> Context
    end.

-spec is_new_device(cb_context:context()) -> boolean().
is_new_device(Context) ->
    %% If `db_doc' is empty it means the request is to create a device.
    kz_term:is_empty(cb_context:fetch(Context, 'db_doc')).

-spec was_mdn_changed_and_user_is_super_admin(cb_context:context(), kz_term:ne_binary()) -> boolean().
was_mdn_changed_and_user_is_super_admin(Context, NewMDN) ->
    %% Check if MDN has been changed.
    kz_json:get_ne_value(?KEY_MDN, cb_context:fetch(Context, 'db_doc')) =/= NewMDN
    %% only superduper admins can change the MDN once created.
        andalso cb_context:is_superduper_admin(Context).

-spec add_mdn(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
add_mdn(MDN, Context) ->
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
    case knm_number:create(MDN, Options) of
        {'error', _}=Error ->
            _ = crossbar_doc:delete(Context),
            cb_phone_numbers:set_response(Error, Context);
        {'ok', _} ->
            lager:debug("created new mdn ~s with public fields set to ~s"
                       ,[MDN, kz_json:encode(PublicFields)]
                       ),
            Context
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_remove_previous_mdn(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
maybe_remove_previous_mdn(_DeviceId, Context) ->
    CurrentMDN = get_mdn(cb_context:doc(Context)),
    PreviousMDN = get_mdn(cb_context:fetch(Context, 'db_doc')),
    case get_device_type(Context) =:= <<"mobile">>
        andalso PreviousMDN =/= CurrentMDN
        andalso kz_term:is_not_empty(PreviousMDN)
    of
        'true' -> remove_mdn(PreviousMDN, Context);
        'false' -> Context
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_device_removal(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
handle_device_removal(DeviceId, Context) ->
    case cb_context:resp_status(Context) =:= 'success' of
        'false' -> Context;
        'true' ->
            _ = kz_process:spawn(fun crossbar_util:flush_registration/1, [Context]),
            _ = crossbar_util:refresh_fs_xml(Context),
            Routines = [fun maybe_remove_aggregate/2
                       ,fun maybe_delete_provision/2
                       ,fun maybe_remove_mdn/2
                       ],
            lists:foldl(fun(F, C) ->
                                case cb_context:resp_status(C) =:= 'success' of
                                    'true' -> F(DeviceId, C);
                                    'false' -> C
                                end
                        end
                       ,Context
                       ,Routines
                       )
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_remove_aggregate(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
maybe_remove_aggregate('undefined', Context) -> Context;
maybe_remove_aggregate(DeviceId, Context) ->
    remove_aggregate(DeviceId, Context).

-spec remove_aggregate(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
remove_aggregate(DeviceId, Context) ->
    case kz_datamgr:del_doc(?KZ_SIP_DB, DeviceId) of
        {'error', 'not_found'} -> Context;
        {'ok', _JObj} ->
            _ = kz_amqp_worker:cast([], fun(_) -> kapi_switch:publish_reload_acls() end),
            Context
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_delete_provision(kz_term:api_ne_binary(), cb_context:context()) -> cb_context:context().
maybe_delete_provision(_DeviceId, Context) ->
    MacAddress = kzd_devices:mac_address(cb_context:doc(Context)),
    case kz_term:is_not_empty(MacAddress) of
        'false' -> Context;
        'true' ->
            _ = kz_process:spawn(fun delete_provision/1, [Context]),
            Context
    end.

-spec delete_provision(cb_context:context()) -> 'ok'.
delete_provision(Context) ->
    DeviceDoc = cb_context:doc(Context),
    AuthToken = cb_context:auth_token(Context),
    _ = provisioner_util:delete_provision(DeviceDoc, AuthToken),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_remove_mdn(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
maybe_remove_mdn(_DeviceId, Context) ->
    MDN = get_mdn(cb_context:doc(Context)),
    case get_device_type(Context) =:= <<"mobile">>
        andalso kz_term:is_not_empty(MDN)
    of
        'true' -> remove_mdn(MDN, Context);
        'false' -> Context
    end.

-spec remove_mdn(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
remove_mdn(MDN, Context) ->
    case knm_number:get(MDN, knm_number_options:mdn_options()) of
        {'ok', PN} ->
            IsMdnCarrier = ?CARRIER_MDN =:= knm_phone_number:module_name(PN),
            case kz_json:get_ne_value(<<"mobile">>, knm_phone_number:to_public_json(PN)) of
                'undefined' when not IsMdnCarrier ->
                    lager:error("not removing number ~s: somehow not an mdn", [MDN]),
                    Context;
                Mobile ->
                    lager:debug("hard removing old mdn ~s with mobile properties ~s"
                               ,[MDN, kz_json:encode(Mobile)]),
                    _ = knm_number:delete(MDN, knm_number_options:mdn_options()),
                    Context
            end;
        {'error', _R} ->
            lager:debug("unable to fetch mdn ~s for removal: ~p", [MDN, _R]),
            Context
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec sync_sip_data(cb_context:context(), boolean()) -> 'ok'.
sync_sip_data(Context, UseTheForce) ->
    NewDoc = cb_context:doc(Context),
    OldDoc = cb_context:fetch(Context, 'db_doc'),
    AccountId = cb_context:account_id(Context),
    case UseTheForce of
        'true' -> provisioner_util:sync_device(AccountId, OldDoc, NewDoc);
        'false' -> provisioner_util:force_sync_device(AccountId, NewDoc)
    end.
