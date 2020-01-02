%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc Common functions for the provisioner modules
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(provisioner_util).

-export([provision_device/3]).
-export([delete_provision/2]).
-export([maybe_update_account/3]).
-export([maybe_delete_account/2]).
-export([maybe_send_contact_list/4]).
-export([get_provision_defaults/1]).
-export([is_mac_address_in_use/2]).
-export([maybe_sync_sip_data/5]).
-export([cleanse_mac_address/1]).

-export([sync_user/1, force_sync_user/2
        ,sync_device/3, force_sync_device/2
        ]).

-include("kazoo_provisioner.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_web/include/kazoo_web.hrl").

-define(MOD_CONFIG_CAT, <<"crossbar.devices">>).
-define(PROVISIONER_CONFIG, <<"provisioner">>).
-define(TEMPLATE_ATTCH, <<"template">>).

-define(BASE_HEADERS
       ,props:filter_undefined(
          [{"host", kapps_config:get_string(?MOD_CONFIG_CAT, <<"provisioning_host">>)}
          ,{"referer", kapps_config:get_string(?MOD_CONFIG_CAT, <<"provisioning_referer">>)}
          ,{"user-agent", kz_term:to_list(erlang:node())}
          ])).
-define(JSON_HEADERS, [{"content-type", "application/json"} | ?BASE_HEADERS]).
-define(FORM_HEADERS, [{"content-type", "application/x-www-form-urlencoded"} | ?BASE_HEADERS]).

-define(LIST_BY_PRESENCE_ID, <<"devices/listing_by_presence_id">>).

-spec cleanse_mac_address(kz_term:api_ne_binary()) -> kz_term:api_ne_binary().
cleanse_mac_address('undefined') -> 'undefined';
cleanse_mac_address(MACAddress) ->
    MAC = kz_term:to_lower_binary(strip_non_hex(MACAddress)),
    case kz_term:is_empty(MAC) of
        'false' -> MAC;
        'true' -> 'undefined'
    end.

-spec strip_non_hex(kz_term:ne_binary()) -> binary().
strip_non_hex(MACAddress) ->
    ReOptions = ['global'
                ,{'return','binary'}
                ],
    re:replace(MACAddress, <<"[^0-9a-fA-F]">>, <<>>, ReOptions).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-type provisioner_options() :: #{'req_verb' => kz_term:ne_binary()
                                ,'auth_token' => kz_term:ne_binary()
                                }.

-spec provision_device(kzd_devices:doc(), kzd_devices:doc(), provisioner_options()) -> boolean().
provision_device(NewDeviceDoc, OldDeviceDoc, ProvisionerOptions) ->
    do_provision_device(ensure_mac_cleansed(NewDeviceDoc)
                       ,ensure_mac_cleansed(OldDeviceDoc)
                       ,ProvisionerOptions
                       ).

-spec ensure_mac_cleansed(kzd_devices:doc()) -> kzd_devices:doc().
ensure_mac_cleansed(DeviceDoc) ->
    case kzd_devices:mac_address(DeviceDoc) of
        'undefined' -> DeviceDoc;
        MacAddress ->
            kzd_devices:set_mac_address(DeviceDoc, MacAddress)
    end.

-spec do_provision_device(kzd_devices:doc(), kzd_devices:doc(), provisioner_options()) -> boolean().
do_provision_device(NewDeviceDoc, OldDeviceDoc, ProvisionerOptions) ->
    case get_provisioning_type() of
        <<"provisioner_v5">> ->
            do_provision_v5(NewDeviceDoc, OldDeviceDoc, ProvisionerOptions);
        _ -> 'false'
    end.

-spec do_provision_v5(kzd_devices:doc(), kzd_devices:doc(), provisioner_options()) -> boolean().
do_provision_v5(NewDeviceDoc, _OldDeviceDoc, #{'req_verb' := ?HTTP_PUT
                                              ,'auth_token' := AuthToken
                                              }) ->
    case kz_term:is_empty(kzd_devices:mac_address(NewDeviceDoc)) of
        'true' -> 'false';
        'false' ->
            _ = provisioner_v5:update_device(NewDeviceDoc, AuthToken),
            'true'
    end;
do_provision_v5(NewDevice, OldDevice, #{'req_verb' := ?HTTP_POST
                                       ,'auth_token' := AuthToken
                                       }) ->
    NewMacAddress = kzd_devices:mac_address(NewDevice),
    OldMacAddress = kzd_devices:mac_address(OldDevice),
    case NewMacAddress =:= OldMacAddress of
        'true' ->
            _ = provisioner_v5:update_device(NewDevice, AuthToken);
        'false' ->
            NewDevice1 = kzd_devices:set_mac_address(NewDevice, OldMacAddress),
            _ = provisioner_v5:delete_device(NewDevice1, AuthToken),
            _ = provisioner_v5:update_device(NewDevice, AuthToken)
    end,
    'true'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec delete_provision(kzd_devices:doc(), kz_term:ne_binary()) -> boolean().
delete_provision(NewDeviceDoc, AuthToken) ->
    case get_provisioning_type() of
        <<"provisioner_v5">>  ->
            _ = provisioner_v5:delete_device(NewDeviceDoc, AuthToken),
            'true';
        _ ->
            'false'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_update_account(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> boolean().
maybe_update_account(AccountId, AuthToken, Doc) ->
    case get_provisioning_type() of
        <<"provisioner_v5">> ->
            _ = provisioner_v5:update_account(AccountId, Doc, AuthToken),
            'true';
        _Type -> 'false'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_delete_account(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
maybe_delete_account(AccountId, AuthToken) ->
    case get_provisioning_type() of
        <<"provisioner_v5">> ->
            _ = provisioner_v5:delete_account(AccountId, AuthToken),
            'true';
        _Type -> 'false'
    end.

-spec maybe_send_contact_list(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), kz_json:object()) -> 'ok'.
maybe_send_contact_list(AccountId, AuthToken, NewDoc, _OldDoc) ->
    _ = case get_provisioning_type() of
            <<"provisioner_v5">> ->
                provisioner_v5:update_user(AccountId, NewDoc, AuthToken);
            _ -> 'ok'
        end,
    'ok'.

%%------------------------------------------------------------------------------
%% @doc This doesn't belong here, needs to be in an external library. Make request to
%% get provisioning defaults
%% @end
%%------------------------------------------------------------------------------
-spec get_provision_defaults(kz_json:object()) ->
          {'ok', kz_json:object()} |
          {'error', kz_term:ne_binary()}.
get_provision_defaults(JObj) ->
    Brand   = kz_http_util:urlencode(kz_json:get_string_value([<<"properties">>, <<"brand">>], JObj)),
    Model   = kz_http_util:urlencode(kz_json:get_string_value([<<"properties">>, <<"model">>], JObj)),
    Product = kz_http_util:urlencode(kz_json:get_string_value([<<"properties">>, <<"product">>], JObj)),

    Url = [kapps_config:get_string(?MOD_CONFIG_CAT, <<"provisioning_url">>)
          ,"?request=data"
          ,"&brand=", kz_term:to_list(Brand)
          ,"&model=", kz_term:to_list(Model)
          ,"&product=", kz_term:to_list(Product)
          ],
    UrlString = lists:flatten(Url),
    lager:debug("attempting to pull provisioning configs from ~s", [UrlString]),
    case kz_http:get(UrlString, ?BASE_HEADERS) of
        {'ok', 200, _, Response} ->
            lager:debug("great success, acquired provisioning template"),
            JResp = kz_json:decode(Response),
            {'ok', kz_json:set_value(<<"template">>, JResp, JObj)};
        {'ok', Status, _, _} ->
            lager:debug("could not get provisioning template defaults: ~p", [Status]),
            {'error', <<"Error retrieving content from external site">>, 500};
        {'error', _R} ->
            lager:debug("could not get provisioning template defaults: ~p", [_R]),
            {'error', <<"Error retrieving content from external site">>}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_mac_address_in_use(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_mac_address_in_use(MacAddress, AuthToken) ->
    case get_provisioning_type() of
        <<"provisioner_v5">> ->
            %% Note: following call will take a "long" time
            'false' =/= provisioner_v5:check_MAC(MacAddress, AuthToken);
        _ -> 'false'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_provisioning_type() -> kz_term:api_ne_binary().
get_provisioning_type() ->
    case kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"provisioning_type">>) of
        'undefined' ->
            lager:debug("using ~p for provisioner_type", [?PROVISIONER_CONFIG]),
            kapps_config:get_ne_binary(?PROVISIONER_CONFIG, <<"provisioning_type">>);
        Result ->
            lager:debug("using ~p for provisioner_type", [?MOD_CONFIG_CAT]),
            Result
    end.

-type should_sync() :: boolean() | 'force'.
-type sync_type() :: 'user' | 'device'.
-type sync_data() :: kzd_devices:doc() | kzd_users:doc().

-spec maybe_sync_sip_data(should_sync(), sync_type(), kz_term:ne_binary(), sync_data(), sync_data()) -> 'ok'.
maybe_sync_sip_data('false', _Type, _AccountId, _Old, _New) ->
    lager:debug("sync not configured for ~s", [_Type]);
maybe_sync_sip_data('true', 'device', AccountId, OldDoc, NewDoc) ->
    sync_device(AccountId, OldDoc, NewDoc);
maybe_sync_sip_data('force', 'device', AccountId, _OldDoc, NewDoc) ->
    force_sync_device(AccountId, NewDoc);
maybe_sync_sip_data('true', 'user', AccountId, _OldDoc, _NewDoc) ->
    sync_user(AccountId);
maybe_sync_sip_data('force', 'user', AccountId, _OldDoc, NewDoc) ->
    force_sync_user(AccountId, NewDoc).

-spec sync_device(kz_term:ne_binary(), kzd_devices:doc(), kzd_devices:doc()) -> 'ok'.
sync_device(AccountId, OldDevice, NewDevice) ->
    OldUsername = kzd_devices:sip_username(OldDevice),
    case kzd_devices:sip_username(NewDevice) =/= OldUsername
        orelse kzd_devices:sip_password(NewDevice) =/= kzd_devices:sip_password(OldDevice)
    of
        'false' ->
            lager:debug("nothing has changed on device; no check-sync needed");
        'true' ->
            Realm = kzd_accounts:fetch_realm(AccountId),
            send_check_sync(OldUsername, Realm, kz_log:get_callid())
    end.

-spec force_sync_device(kz_term:ne_binary(), kzd_devices:doc()) -> 'ok'.
force_sync_device(AccountId, NewDevice) ->
    Username = kzd_devices:sip_username(NewDevice),
    Realm = kzd_accounts:fetch_realm(AccountId),
    send_check_sync(Username, Realm, kz_log:get_callid()).

-spec sync_user(kz_term:ne_binary()) -> 'ok'.
sync_user(AccountId) ->
    Realm = kzd_accounts:fetch_realm(AccountId),
    Req = [{<<"Realm">>, Realm}
          ,{<<"Fields">>, [<<"Username">>]}
          ],
    ReqResp = kz_amqp_worker:call(Req
                                 ,fun kapi_registration:publish_query_req/1
                                 ,fun kapi_registration:query_resp_v/1
                                 ),
    case ReqResp of
        {'error', _E} -> lager:debug("no devices to send check sync to for realm ~s", [Realm]);
        {'timeout', _} -> lager:debug("timed out query for fetching devices for ~s", [Realm]);
        {'ok', JObj} ->
            lists:foreach(fun(J) ->
                                  Username = kz_json:get_ne_binary_value(<<"Username">>, J),
                                  send_check_sync(Username, Realm, 'undefined')
                          end
                         ,kz_json:get_list_value(<<"Fields">>, JObj, [])
                         )
    end.

-spec user_devices(kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kz_json:objects()} |
          kz_datamgr:data_error().
user_devices(AccountId, UserId) ->
    AccountDb = kzs_util:format_account_db(AccountId),

    Options = [{'key', UserId}, 'include_docs'],
    case kz_datamgr:get_results(AccountDb, ?LIST_BY_PRESENCE_ID, Options) of
        {'error', _}=E -> E;
        {'ok', JObjs} ->
            {'ok', [kz_json:get_json_value(<<"doc">>, JObj) || JObj <- JObjs]}
    end.

-spec force_sync_user(kz_term:ne_binary(), kzd_users:doc()) -> 'ok'.
force_sync_user(AccountId, NewUser) ->
    case user_devices(AccountId, kz_doc:id(NewUser)) of
        {'error', _E} ->
            lager:debug("unable to sync user devices: ~p", [_E]);
        {'ok', []} ->
            lager:debug("no user devices to sync");
        {'ok', DeviceDocs} ->
            Realm = kzd_accounts:fetch_realm(AccountId),
            _ = [send_check_sync(kzd_devices:presence_id(DeviceDoc), Realm, kz_log:get_callid())
                 || DeviceDoc <- DeviceDocs
                ],
            'ok'
    end.

-spec send_check_sync(kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary()) -> 'ok'.
send_check_sync('undefined', _Realm, _MsgId) ->
    lager:warning("did not send check sync: username is undefined");
send_check_sync(_Username, 'undefined', _MsgId) ->
    lager:warning("did not send check sync: realm is undefined");
send_check_sync(Username, Realm, MsgId) ->
    lager:debug("sending check sync for ~s @ ~s", [Username, Realm]),
    publish_check_sync(MsgId, [{<<"Event">>, <<"check-sync">>}
                              ,{<<"Realm">>, Realm}
                              ,{<<"Username">>, Username}
                               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                              ]).

-spec publish_check_sync(kz_term:api_binary(), kz_term:proplist()) -> 'ok'.
publish_check_sync('undefined', Req) ->
    publish_check_sync(Req);
publish_check_sync(MsgId, Req) ->
    publish_check_sync([{<<"Msg-ID">>, MsgId} | Req]).

-spec publish_check_sync(kz_term:proplist()) -> 'ok'.
publish_check_sync(Req) ->
    kz_amqp_worker:cast(Req, fun kapi_switch:publish_notify/1).
