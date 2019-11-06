%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
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
    MacAddress = kzd_devices:mac_address(NewDeviceDoc),
    case get_provisioning_type() of
        <<"super_awesome_provisioner">> ->
            do_full_provision(NewDeviceDoc, OldDeviceDoc, MacAddress);
        <<"awesome_provisioner">> when MacAddress =/= 'undefined' ->
            do_awesome_provision(NewDeviceDoc);
        <<"simple_provisioner">> when MacAddress =/= 'undefined' ->
            do_simple_provision(MacAddress, NewDeviceDoc);
        <<"provisioner_v5">> ->
            do_provision_v5(NewDeviceDoc, OldDeviceDoc, ProvisionerOptions);
        _ -> 'false'
    end.

-spec do_full_provision(kzd_devices:doc(), kzd_devices:doc(), kz_term:api_ne_binary()) -> boolean().
do_full_provision(NewDeviceDoc, OldDeviceDoc, 'undefined') ->
    case kzd_devices:mac_address(OldDeviceDoc) of
        'undefined' -> 'ok';
        OldMACAddress -> delete_full_provision(OldMACAddress, NewDeviceDoc)
    end,
    'false';
do_full_provision(NewDeviceDoc, OldDeviceDoc, MACAddress) ->
    _ = do_full_provisioner_provider(kz_doc:account_id(NewDeviceDoc)),
    _ = full_provision(NewDeviceDoc, OldDeviceDoc, MACAddress),
    'true'.

-spec do_awesome_provision(kzd_devices:doc()) -> boolean().
do_awesome_provision(NewDeviceDoc) ->
    case get_template(NewDeviceDoc) of
        {'error', _} -> 'false';
        {'ok', Template} ->
            send_provisioning_template(NewDeviceDoc, Template),
            'true'
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
    MACAddress = kzd_devices:mac_address(NewDeviceDoc),
    case MACAddress =/= 'undefined'
        andalso get_provisioning_type()
    of
        <<"super_awesome_provisioner">> ->
            _ = delete_full_provision(MACAddress, NewDeviceDoc),
            'true';
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
        <<"super_awesome_provisioner">> ->
            _ = delete_account(AccountId),
            'true';
        <<"provisioner_v5">> ->
            _ = provisioner_v5:delete_account(AccountId, AuthToken),
            'true';
        _Type -> 'false'
    end.

-spec maybe_send_contact_list(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), kz_json:object()) -> 'ok'.
maybe_send_contact_list(AccountId, AuthToken, NewDoc, OldDoc) ->
    _ = case get_provisioning_type() of
            <<"super_awesome_provisioner">> ->
                do_full_provision_contact_list(AccountId, NewDoc, OldDoc);
            <<"provisioner_v5">> ->
                provisioner_v5:update_user(AccountId, NewDoc, AuthToken);
            _ -> 'ok'
        end,
    'ok'.

-spec do_full_provisioner_provider(kz_term:ne_binary()) -> boolean().
do_full_provisioner_provider(AccountId) ->
    do_full_provision_contact_list(AccountId).

-spec do_full_provision_contact_list(kz_term:ne_binary()) -> boolean().
do_full_provision_contact_list(?NE_BINARY = AccountId) ->
    case kzd_accounts:fetch(AccountId) of
        {'ok', JObj} ->
            Routines = [fun kz_doc:public_fields/1
                       ,fun(J) ->
                                ResellerId = kz_services_reseller:get_id(AccountId),
                                kz_json:set_value(<<"provider_id">>, ResellerId, J)
                        end
                       ,fun(J) -> kz_json:delete_key(<<"available_apps">>, J) end
                       ,fun(J) ->
                                AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
                                ContactList = provisioner_contact_list:build(AccountDb),
                                kz_json:set_value(<<"directory">>, ContactList, J)
                        end
                       ],
            Provider = lists:foldl(fun(F, J) -> F(J) end, JObj, Routines),
            PartialURL = <<AccountId/binary, "/">>,
            maybe_send_to_full_provisioner(PartialURL, Provider);
        {'error', _R} ->
            lager:warning("failed to get account definition for ~s: ~p", [AccountId, _R]),
            'false'
    end.

do_full_provision_contact_list(AccountId, NewDoc, OldDoc) ->
    case should_build_contact_list(NewDoc, OldDoc) of
        'true' -> do_full_provision_contact_list(AccountId);
        'false' -> 'ok'
    end.

-spec should_build_contact_list(kz_json:object(), kz_json:objec()) -> boolean().
should_build_contact_list(JObj, OriginalJObj) ->
    case kz_json:is_json_object(OriginalJObj) of
        'false' ->
            kz_doc:type(JObj) =:= <<"callflow">>;
        'true' ->
            kz_doc:type(JObj) =:= <<"callflow">>
                orelse kz_json:get_value(<<"name">>, JObj) =/=  kz_json:get_value(<<"name">>, OriginalJObj)
                orelse kz_json:get_value(<<"first_name">>, JObj) =/=  kz_json:get_value(<<"first_name">>, OriginalJObj)
                orelse kz_json:get_value(<<"last_name">>, JObj) =/=  kz_json:get_value(<<"last_name">>, OriginalJObj)
                orelse kz_json:get_value([<<"contact_list">>, <<"exclude">>], JObj) =/=
                kz_json:get_value([<<"contact_list">>, <<"exclude">>], OriginalJObj)
    end.

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
%% @doc post data to a provisioning server
%% @end
%%------------------------------------------------------------------------------
-spec do_simple_provision(kz_term:ne_binary(), kzd_devices:doc()) -> boolean().
do_simple_provision(MACAddress, NewDeviceDoc) ->
    do_simple_provision(MACAddress, NewDeviceDoc, kapps_config:get_string(?MOD_CONFIG_CAT, <<"provisioning_url">>)).

do_simple_provision(_MACAddress, _NewDeviceDoc, 'undefined') -> 'false';
do_simple_provision(MACAddress, NewDeviceDoc, URL) ->
    AccountRealm = kzd_accounts:fetch_realm(kz_doc:account_id(NewDeviceDoc)),
    Body =
        kz_json:from_list(
          [{<<"device[mac]">>, MACAddress}
          ,{<<"device[label]">>, kzd_devices:name(NewDeviceDoc)}
          ,{<<"sip[realm]">>, kzd_devices:sip_realm(NewDeviceDoc, AccountRealm)}
          ,{<<"sip[username]">>, kzd_devices:sip_username(NewDeviceDoc)}
          ,{<<"sip[password]">>, kzd_devices:sip_password(NewDeviceDoc)}
          ,{<<"submit">>, <<"true">>}
          ]),
    Encoded = kz_http_util:json_to_querystring(Body),
    lager:debug("posting to ~s with: ~-300s", [URL, Encoded]),
    _Res = kz_http:post(URL, ?FORM_HEADERS, Encoded),
    lager:debug("response from server: ~p", [_Res]),
    'true'.

%%------------------------------------------------------------------------------
%% @doc post data to a provisioning server
%% @end
%%------------------------------------------------------------------------------
-spec delete_account(kz_term:ne_binary()) -> boolean().
delete_account(<<_/binary>> = AccountId) ->
    maybe_send_to_full_provisioner(AccountId).

-spec delete_full_provision(kz_term:ne_binary(), kzd_devices:doc()) -> boolean().
delete_full_provision(MACAddress, NewDeviceDoc) ->
    AccountId = kz_doc:account_id(NewDeviceDoc),
    PartialURL = <<AccountId/binary, "/", MACAddress/binary>>,
    maybe_send_to_full_provisioner(PartialURL).

-spec full_provision(kzd_devices:doc(), kzd_devices:doc(), kz_term:ne_binary()) -> boolean().
full_provision(NewDeviceDoc, OldDevice, MACAddress) ->
    {'ok', Data} = get_merged_device(MACAddress, NewDeviceDoc),
    case kzd_devices:mac_address(OldDevice) of
        'undefined' -> 'ok';
        MACAddress -> 'ok';
        OldMACAddress ->
            delete_full_provision(OldMACAddress, Data)
    end,

    AccountId = kz_doc:account_id(NewDeviceDoc),
    PartialURL = <<AccountId/binary, "/", MACAddress/binary>>,
    maybe_send_to_full_provisioner(PartialURL, Data).

-spec maybe_send_to_full_provisioner(kz_term:ne_binary()) -> boolean().
maybe_send_to_full_provisioner(PartialURL) ->
    case kapps_config:get_binary(?MOD_CONFIG_CAT, <<"provisioning_url">>) of
        'undefined' -> 'false';
        Url ->
            FullUrl = kz_term:to_lower_string(<<Url/binary, "/", PartialURL/binary>>),
            send_to_full_provisioner(FullUrl)
    end.

-spec maybe_send_to_full_provisioner(kz_term:ne_binary(), kz_json:object()) -> boolean().
maybe_send_to_full_provisioner(PartialURL, JObj) ->
    case kapps_config:get_binary(?MOD_CONFIG_CAT, <<"provisioning_url">>) of
        'undefined' -> 'false';
        Url ->
            FullUrl = kz_term:to_lower_string(<<Url/binary, "/", PartialURL/binary>>),
            {'ok', _, _, RawJObj} = kz_http:get(FullUrl, ?JSON_HEADERS, [{'timeout', 10 * ?MILLISECONDS_IN_SECOND}]),
            case kz_json:get_integer_value([<<"error">>, <<"code">>], kz_json:decode(RawJObj)) of
                'undefined' -> send_to_full_provisioner('post', FullUrl, JObj);
                404 -> send_to_full_provisioner('put', FullUrl, JObj);
                _ -> 'false'
            end
    end.

-spec send_to_full_provisioner(string()) -> boolean().
send_to_full_provisioner(FullUrl) ->
    lager:debug("making ~s request to ~s", ['delete', FullUrl]),
    Res = kz_http:delete(FullUrl, ?JSON_HEADERS, [], [{'timeout', 10 * ?MILLISECONDS_IN_SECOND}]),
    lager:debug("response from server: ~p", [Res]),
    'true'.

-spec send_to_full_provisioner('put' | 'post', string(), kz_json:object()) -> boolean().
send_to_full_provisioner('put', FullUrl, JObj) ->
    Body = kz_term:to_list(kz_json:encode(JObj)),
    lager:debug("making put request to ~s with: ~-300p", [FullUrl, Body]),
    Res = kz_http:put(FullUrl, ?JSON_HEADERS, Body, [{'timeout', 10 * ?MILLISECONDS_IN_SECOND}]),
    lager:debug("response from server: ~p", [Res]),
    'true';
send_to_full_provisioner('post', FullUrl, JObj) ->
    J = kz_json:from_list(
          [{<<"provider_id">>, kz_json:get_value(<<"provider_id">>, JObj)}
          ,{<<"name">>, kz_json:get_value(<<"name">>, JObj)}
          ,{<<"settings">>, JObj}
          ]),
    Body = kz_term:to_list(kz_json:encode(J)),
    lager:debug("making post request to ~s with: ~-300p", [FullUrl, Body]),
    Res = kz_http:post(FullUrl, ?JSON_HEADERS, Body, [{'timeout', 10 * ?MILLISECONDS_IN_SECOND}]),
    lager:debug("response from server: ~p", [Res]),
    'true'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_merged_device(kz_term:ne_binary(), kzd_devices:doc()) ->
                               {'ok', kz_json:object()}.
get_merged_device(MACAddress, NewDeviceDoc) ->
    {'ok', _Data} = merge_device(MACAddress, NewDeviceDoc).

-spec merge_device(kz_term:ne_binary(), kzd_devices:doc()) ->
                          {'ok', kz_json:object()}.
merge_device(MACAddress, NewDeviceDoc) ->
    AccountId = kz_doc:account_id(NewDeviceDoc),

    Routines = [fun(J) -> kzd_devices:set_mac_address(J, MACAddress) end
               ,fun(J) ->
                        OwnerId = kzd_devices:owner_id(NewDeviceDoc),
                        Owner = get_owner(OwnerId, AccountId),
                        kz_json:merge(J, Owner)
                end
               ,fun(J) -> kz_json:delete_key(<<"apps">>, J) end
               ,fun(J) -> kz_json:set_value(<<"account_id">>, AccountId, J) end
               ],
    MergedDevice = lists:foldl(fun(F, J) -> F(J) end, NewDeviceDoc, Routines),
    {'ok', kz_doc:public_fields(MergedDevice)}.

-spec get_owner(kz_term:api_binary(), kz_term:ne_binary()) -> kz_json:object().
get_owner('undefined', _) -> kz_json:new();
get_owner(OwnerId, AccountId) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    case kz_datamgr:open_cache_doc(AccountDb, OwnerId) of
        {'ok', Owner} -> Owner;
        {'error', _R} ->
            lager:debug("unable to open user definition ~s/~s: ~p", [AccountDb, OwnerId, _R]),
            kz_json:new()
    end.

%%------------------------------------------------------------------------------
%% @doc Do awesome provisioning
%% @end
%%------------------------------------------------------------------------------
-spec send_provisioning_template(kzd_devices:doc(), kz_json:object()) -> 'ok'.
send_provisioning_template(NewDeviceDoc, _DeviceTemplate) ->
    %% TODO: theoretically this is the start of multiple line support....
    Line = <<"lineloop|line_1">>,
    MACAddress = kz_term:to_list(kzd_devices:mac_address(NewDeviceDoc, "")),
    MAC = re:replace(MACAddress
                    ,"[^0-9a-fA-F]", "", [{'return', 'list'}, 'global']
                    ),
    LineGenerators = [fun set_device_line_defaults/1
                     ,fun set_account_line_defaults/1
                     ],
    TmplGenerators = [fun set_account_id/1
                     ,fun set_account_overrides/1
                     ,fun set_user_overrides/1
                     ,fun set_device_overrides/1
                     ,fun set_global_overrides/1
                     ],
    LineUpdaters = lists:foldr(fun(F, U) -> F(NewDeviceDoc) ++ U end, [], LineGenerators),
    TmplUpdaters = lists:foldr(fun(F, U) -> F(NewDeviceDoc) ++ U end, [], TmplGenerators),
    DefaultTemplate = lists:foldr(fun(F, J) -> F(J) end, NewDeviceDoc, TmplUpdaters),
    LineLoop = kz_json:get_value([<<"data">>, <<"globals">>, <<"globals">>, Line], DefaultTemplate),
    LineTemplate = lists:foldr(fun(F, J) -> F(J) end, LineLoop, LineUpdaters),

    Template = kz_json:set_value([<<"data">>, <<"globals">>, <<"globals">>, Line], LineTemplate, DefaultTemplate),
    send_provisioning_request(Template, MAC).

%%------------------------------------------------------------------------------
%% @doc If the device specifies a local template id then return that
%% template
%% @end
%%------------------------------------------------------------------------------
-spec get_template(kzd_devices:doc()) ->
                          {'ok', kz_json:object()} |
                          {'error', any()}.
get_template(NewDeviceDoc) ->
    get_template(NewDeviceDoc, kzd_devices:provision_id(NewDeviceDoc)).

get_template(NewDeviceDoc, 'undefined') ->
    lager:debug("unknown template id for device ~s", [kz_doc:id(NewDeviceDoc)]),
    {'error', 'not_found'};
get_template(NewDeviceDoc, TemplateId) ->
    case kz_datamgr:fetch_attachment(kz_doc:account_db(NewDeviceDoc), TemplateId, ?TEMPLATE_ATTCH) of
        {'error', _R}=E ->
            lager:debug("could not fetch template doc ~s: ~p", [TemplateId, _R]),
            E;
        {'ok', Attachment} ->
            {'ok', kz_json:decode(Attachment)}
    end.

%%------------------------------------------------------------------------------
%% @doc add the account_id to the root of the provisioning json
%% @end
%%------------------------------------------------------------------------------
-spec set_account_id(kzd_devices:doc()) ->
                            [fun((kz_json:object()) -> kz_json:object()),...].
set_account_id(_NewDeviceDoc) ->
    [fun(J) ->
             kz_json:set_value(<<"account_id">>, kz_doc:account_id(J), J)
     end
    ].

%%------------------------------------------------------------------------------
%% @doc get the settings from the account doc that should be used in the
%% base properties for the line
%% @end
%%------------------------------------------------------------------------------
-spec set_account_line_defaults(kzd_devices:doc()) ->
                                       [fun((kz_json:object()) -> kz_json:object()),...].
set_account_line_defaults(NewDeviceDoc) ->
    Account = case kzd_accounts:fetch(kz_doc:id(NewDeviceDoc)) of
                  {'ok', AccountJObj} -> AccountJObj;
                  {'error', _} -> kz_json:new()
              end,

    [fun(J) ->
             set_if_defined(J, Account, fun kzd_accounts:realm/1, [<<"server_host">>, <<"value">>])
     end
    ,fun(J) ->
             set_if_defined(J, Account, fun kzd_accounts:name/1, [<<"displayname">>, <<"value">>])
     end
    ].

-spec set_if_defined(kz_json:object(), kz_json:object(), fun((kz_json:object()) -> kz_json:api_json_term()), kz_json:path()) -> kz_json:object().
set_if_defined(Acc, JObj, Getter, SetPath) ->
    case Getter(JObj) of
        'undefined' -> Acc;
        Value -> kz_json:set_value(SetPath, Value, Acc)
    end.

%%------------------------------------------------------------------------------
%% @doc get the settings from the device doc that should be used in the
%% base properties for the line
%% @end
%%------------------------------------------------------------------------------
-spec set_device_line_defaults(kzd_devices:doc()) ->
                                      [fun((kz_json:object()) -> kz_json:object()),...].
set_device_line_defaults(Device) ->
    [fun(J) ->
             set_if_defined(J, Device, fun kzd_devices:sip_username/1, [<<"authname">>, <<"value">>])
     end
    ,fun(J) ->
             set_if_defined(J, Device, fun kzd_devices:sip_username/1, [<<"username">>, <<"value">>])
     end
    ,fun(J) ->
             set_if_defined(J, Device, fun kzd_devices:sip_password/1, [<<"secret">>, <<"value">>])
     end
    ,fun(J) ->
             set_if_defined(J, Device, fun kzd_devices:sip_realm/1, [<<"server_host">>, <<"value">>])
     end
    ,fun(J) ->
             set_if_defined(J, Device, fun kzd_devices:name/1, [<<"displayname">>, <<"value">>])
     end
    ].

%%------------------------------------------------------------------------------
%% @doc merge in any overrides from the global provisioning db
%% @end
%%------------------------------------------------------------------------------
-spec set_global_overrides(kzd_devices:doc()) ->
                                  [fun((kz_json:object()) -> kz_json:object()),...].
set_global_overrides(_) ->
    GlobalDefaults = case kz_datamgr:open_cache_doc(?KZ_PROVISIONER_DB, <<"base_properties">>) of
                         {'ok', JObj} -> JObj;
                         {'error', _} -> kz_json:new()
                     end,

    case kz_json:get_json_value(<<"defaults">>, GlobalDefaults) of
        'undefined' -> [fun kz_term:identity/1];
        Overrides -> [fun(J) -> kz_json:merge(J, Overrides) end]
    end.

%%------------------------------------------------------------------------------
%% @doc merge in any overrides from the account doc
%% @end
%%------------------------------------------------------------------------------
-spec set_account_overrides(kzd_devices:doc()) ->
                                   [fun((kz_json:object()) -> kz_json:object()),...].
set_account_overrides(DeviceDoc) ->
    AccountJObj = case kzd_accounts:fetch(kz_doc:account_id(DeviceDoc)) of
                      {'ok', JObj} -> JObj;
                      {'error', _} -> kz_json:new()
                  end,

    case kz_json:get_json_value([<<"provision">>, <<"overrides">>], AccountJObj) of
        'undefined' -> [fun kz_term:identity/1];
        Overrides -> [fun(J) -> kz_json:merge(J, Overrides) end]
    end.

%%------------------------------------------------------------------------------
%% @doc merge in any overrides from the user doc
%% @end
%%------------------------------------------------------------------------------
-spec set_user_overrides(kzd_devices:doc()) ->
                                [fun((kz_json:object()) -> kz_json:object()),...].
set_user_overrides(DeviceDoc) ->
    set_user_overrides(DeviceDoc, kzd_devices:owner_id(DeviceDoc)).

set_user_overrides(_DeviceDoc, 'undefined') ->
    [fun kz_term:identity/1];
set_user_overrides(DeviceDoc, OwnerId) ->
    set_user_overrides(DeviceDoc, OwnerId, kzd_users:fetch(kz_doc:account_id(DeviceDoc), OwnerId)).

set_user_overrides(_DeviceDoc, _OwnerId, {'error', _}) ->
    [fun kz_term:identity/1];
set_user_overrides(_DeviceDoc, _OwnerId, {'ok', Owner}) ->
    case kz_json:get_json_value([<<"provision">>, <<"overrides">>], Owner) of
        'undefined' -> [fun kz_term:identity/1];
        Overrides -> [fun(J) -> kz_json:merge(J, Overrides) end]
    end.

%%------------------------------------------------------------------------------
%% @doc merge in any overrides from the device doc
%% @end
%%------------------------------------------------------------------------------
-spec set_device_overrides(kzd_devices:doc()) ->
                                  [fun((kz_json:object()) -> kz_json:object()),...].
set_device_overrides(Device) ->
    case kz_json:get_json_value([<<"provision">>, <<"overrides">>], Device) of
        'undefined' -> [fun kz_term:identity/1];
        Overrides -> [fun(J) -> kz_json:merge(J, Overrides) end]
    end.

%%------------------------------------------------------------------------------
%% @doc Send awesome provisioning request
%% @end
%%------------------------------------------------------------------------------
-spec send_provisioning_request(kz_json:object(), kz_term:ne_binary()) -> 'ok'.
send_provisioning_request(Template, MACAddress) ->
    ProvisionRequest = kz_json:encode(Template),
    UrlTmpl = kapps_config:get_string(?MOD_CONFIG_CAT, <<"provisioning_url">>),
    UrlString = re:replace(UrlTmpl, "{{mac_address}}", MACAddress, ['global', {'return', 'list'}]),
    lager:debug("provisioning via ~s", [UrlString]),
    case kz_http:post(UrlString, ?JSON_HEADERS, ProvisionRequest) of
        {'ok', 200, _, Response} ->
            lager:debug("SUCCESS! BOOM! ~s", [Response]);
        {'ok', Code, _, Response} ->
            lager:debug("ERROR! OH NO! ~p. ~s", [Code, Response]);
        {'error', R} ->
            lager:debug("ERROR! OH NO! ~p", [R])
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
    AccountDb = kz_util:format_account_db(AccountId),

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
