%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Common functions for the provisioner modules
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(provisioner_v5).

-export([update_device/2]).
-export([delete_device/2]).
-export([delete_account/2]).
-export([update_account/3]).
-export([update_user/3]).
-export([check_MAC/2]).

-ifdef(TEST).
-export([is_device_enabled/3
        ,device_display_name/3
        ]).
-endif.

-include("provisioner_v5.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_device(kzd_devices:doc(), kz_term:ne_binary()) -> 'ok'.
update_device(JObj, AuthToken) ->
    AccountId = kz_doc:account_id(JObj),
    Request = device_settings(set_owner(JObj)),
    _ = update_account(AccountId, AuthToken),
    send_req('devices_post'
            ,maybe_add_device_defaults(Request)
            ,AuthToken
            ,AccountId
            ,kzd_devices:mac_address(JObj)
            ).

-spec maybe_add_device_defaults(kz_json:object()) -> kz_json:object().
maybe_add_device_defaults(JObj) ->
    LinesPath = [<<"settings">>, <<"lines">>],
    F = fun (K, V) -> {K, maybe_set_line_defaults(V)} end,
    NewLines = kz_json:map(F, kz_json:get_value(LinesPath, JObj)),
    kz_json:set_value(LinesPath, NewLines, JObj).

-spec maybe_set_line_defaults(kz_json:object()) -> kz_json:object().
maybe_set_line_defaults(LineJObj) ->
    Defaults = [{[<<"advanced">>, <<"expire">>], 360}
               ,{[<<"advanced">>, <<"srtp">>], 'false'}
               ,{[<<"basic">>, <<"enabled">>], 'true'}
               ],
    kz_json:insert_values(Defaults, LineJObj).

-spec device_settings(kzd_devices:doc()) -> kz_json:object().
device_settings(Device) ->
    kz_json:from_list(
      [{<<"brand">>, get_brand(Device)}
      ,{<<"family">>, get_family(Device)}
      ,{<<"model">>, get_model(Device)}
      ,{<<"name">>, kzd_devices:name(Device)}
      ,{<<"settings">>, settings(Device)}
      ]).

-spec get_brand(kzd_devices:doc()) -> binary().
get_brand(Device) ->
    kzd_devices:provision_endpoint_brand(Device, <<>>).

-spec get_family(kzd_devices:doc()) -> binary().
get_family(Device) ->
    case kzd_devices:provision_endpoint_family(Device, <<>>) of
        %% Temporary hack to fix family names till a script can clean the database
        <<"f", Family/binary>> -> Family;
        Family -> Family
    end.

-spec get_model(kzd_devices:doc()) -> kz_term:ne_binary().
get_model(Device) ->
    Family = kzd_devices:provision_endpoint_model(Device, <<>>),
    case kz_term:to_lower_binary(Family) of
        <<"t19">> -> <<"t19p">>;
        <<"t21">> -> <<"t21p">>;
        <<"t22">> -> <<"t22p">>;
        <<"t23">> -> <<"t23p">>;
        <<"t26">> -> <<"t26p">>;
        <<"t27">> -> <<"t27p">>;
        <<"t28">> -> <<"t28p">>;
        <<"t29">> -> <<"t29g">>;
        <<"t32">> -> <<"t32g">>;
        <<"t38">> -> <<"t38g">>;
        <<"t41">> -> <<"t41p">>;
        <<"t42">> -> <<"t42g">>;
        <<"t46">> -> <<"t46g">>;
        <<"t48">> -> <<"t48g">>;
        Else -> Else
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec delete_device(kzd_devices:doc(), kz_term:ne_binary()) -> 'ok'.
delete_device(JObj, AuthToken) ->
    send_req('devices_delete'
            ,'undefined'
            ,AuthToken
            ,kz_doc:account_id(JObj)
            ,kzd_devices:mac_address(JObj)
            ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec delete_account(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
delete_account(AccountId, AuthToken) ->
    send_req('accounts_delete'
            ,'undefined'
            ,AuthToken
            ,AccountId
            ,'undefined'
            ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_account(kz_term:ne_binary(), kzd_accounts:doc(), kz_term:ne_binary()) -> 'ok'.
update_account(AccountId, _JObj, AuthToken) ->
    send_req('accounts_update'
            ,kz_json:from_list([{<<"settings">>, kz_json:new()}])
            ,AuthToken
            ,AccountId
            ,'undefined'
            ).

-spec update_account(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
update_account(Account, AuthToken) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    case kzd_accounts:fetch(AccountId) of
        {'ok', JObj} -> update_account(AccountId, JObj, AuthToken);
        {'error', _R} ->
            lager:debug("unable to fetch account ~s: ~p", [AccountId, _R])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_user(kz_term:ne_binary(), kzd_users:doc(), kz_term:ne_binary()) -> 'ok'.
update_user(AccountId, JObj, AuthToken) ->
    case kz_doc:type(JObj) of
        <<"user">> -> save_user(AccountId, JObj, AuthToken);
        _ -> 'ok' %% Gets rid of VMbox
    end.

-spec save_user(kz_term:ne_binary(), kzd_users:doc(), kz_term:ne_binary()) -> 'ok'.
save_user(AccountId, JObj, AuthToken) ->
    _ = update_account(AccountId, AuthToken),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    Devices = kz_attributes:owned_by_docs(kz_doc:id(JObj), AccountDb),
    lists:foreach(fun(Device) ->
                          Settings = settings(Device),
                          maybe_save_device(Device, Settings, AccountId, AuthToken)
                  end
                 ,Devices
                 ).

-spec maybe_save_device(kzd_devices:doc(), kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                               'ok' | {'EXIT', _}.
maybe_save_device(Device, Settings, AccountId, AuthToken) ->
    Request = kz_json:from_list(
                [{<<"brand">>, get_brand(Device)}
                ,{<<"family">>, get_family(Device)}
                ,{<<"model">>, get_model(Device)}
                ,{<<"name">>, kzd_devices:name(Device)}
                ,{<<"settings">>, Settings}
                ]),
    catch save_device(AccountId, Device, Request, AuthToken).

-spec save_device(kz_term:ne_binary(), kzd_devices:doc(), kz_json:object(), kz_term:ne_binary()) -> 'ok'.
save_device(AccountId, Device, Request, AuthToken) ->
    case kzd_devices:mac_address(Device) of
        'undefined' -> 'ok';
        MacAddress ->
            send_req('devices_post', Request, AuthToken, AccountId, MacAddress)
    end.

%%------------------------------------------------------------------------------
%% @doc Use before a POST or PUT to a device.
%% Return the account id a MAC address belongs to, `false' otherwise.
%% @end
%%------------------------------------------------------------------------------
-spec check_MAC(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary() | 'false'.
check_MAC(MacAddress, AuthToken) ->
    Headers = req_headers(AuthToken),
    UrlString = req_uri('devices', MacAddress),
    lager:debug("pre-provisioning via ~s", [UrlString]),
    case kz_http:get(UrlString, Headers) of
        {'ok', 200, _RespHeaders, JSONStr} ->
            lager:debug("provisioner says ~s", [JSONStr]),
            JObj = kz_json:decode(JSONStr),
            kz_json:get_ne_binary_value([<<"data">>, <<"account_id">>], JObj);
        _AnythingElse ->
            lager:debug("device ~s not found: ~p", [MacAddress, _AnythingElse]),
            'false'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_owner(kzd_devices:doc()) -> kzd_devices:doc().
set_owner(Device) ->
    OwnerId = kzd_devices:owner_id(Device),
    case kzd_users:fetch(kz_doc:account_id(Device), OwnerId) of
        {'error', _R} -> Device;
        {'ok', Doc} ->
            kz_json:merge(Doc, Device, #{'keep_null' => 'true'})
    end.

%%------------------------------------------------------------------------------
%% @doc Calculate the settings for the device based on the merge down
%%
%% When configuring a device, settings from the device, user, and account must be
%% merged down into a unified view of what is sent to the provisioner.
%%
%% For instance, when a user is disabled, the device(s) will be disabled, but if
%% the device is re-saved (and still owned by the disabled user) and enabled, it
%% needs to continue to be disabled on provisioner.
%%
%% This function should create the merged version of the device's settings in a
%% way similar to kz_endpoint when building bridge strings.
%% @end
%%------------------------------------------------------------------------------
-spec settings(kzd_devices:doc()) -> kz_json:object().
settings(Device) ->
    Props = props:filter_empty([{<<"lines">>, settings_lines(Device)}
                               ,{<<"codecs">>, settings_codecs(Device)}
                               ,{<<"datetime">>, settings_datetime(Device)}
                               ,{<<"feature_keys">>, settings_feature_keys(Device)}
                               ,{<<"line_keys">>, settings_line_keys(Device)}
                               ,{<<"combo_keys">>, settings_combo_keys(Device)}
                               ]),
    kz_json:from_list(Props).

-spec settings_line_keys(kzd_devices:doc()) -> kz_term:api_object().
settings_line_keys(Device) ->
    settings_line_keys(get_brand(Device), get_family(Device)).

-spec settings_line_keys(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_object().
settings_line_keys(<<"yealink">>, _) ->
    LineKeys = [{<<"account">>, <<"1">>}
               ,{<<"type">>, <<"15">>}
               ],
    Key = kz_json:from_list([{<<"key">>, kz_json:from_list(LineKeys)}]),
    kz_json:from_list([{<<"0">>, Key}]);
settings_line_keys(_, _) -> 'undefined'.

-spec settings_lines(kzd_devices:doc()) -> kz_json:object().
settings_lines(Device) ->
    SettingsLines = props:filter_empty(
                      [{<<"basic">>, settings_basic(Device)}
                      ,{<<"sip">>, settings_sip(Device)}
                      ,{<<"advanced">>, settings_advanced(Device)}
                      ]),
    case SettingsLines of
        [] -> kz_json:new();
        Props -> kz_json:from_list([{<<"0">>, kz_json:from_list(Props)}])
    end.

-spec settings_basic(kzd_devices:doc()) -> kz_json:object().
settings_basic(DeviceDoc) ->
    {'ok', AccountDoc} = fetch_account_from_device(DeviceDoc),
    {'ok', UserDoc} = fetch_user_from_device(DeviceDoc),

    settings_basic(DeviceDoc, UserDoc, AccountDoc).

-spec settings_basic(kzd_devices:doc(), kzd_users:doc(), kzd_accounts:doc()) ->
                            kz_json:object().
settings_basic(DeviceDoc, UserDoc, AccountDoc) ->
    IsEnabled = is_device_enabled(DeviceDoc, UserDoc, AccountDoc),
    DisplayName = device_display_name(DeviceDoc, UserDoc, AccountDoc),

    Props = [{<<"display_name">>, DisplayName}
            ,{<<"enable">>, IsEnabled}
            ],
    kz_json:from_list(Props).

-spec device_display_name(kzd_devices:doc(), kzd_users:doc(), kzd_accounts:doc()) ->
                                 kz_term:api_ne_binary().
device_display_name(DeviceDoc, UserDoc, AccountDoc) ->
    case [DN || DN <- [kzd_users:name(UserDoc)
                      ,kzd_devices:name(DeviceDoc)
                      ,kzd_accounts:name(AccountDoc)
                      ],
                not is_empty_display_name(DN)
         ]
    of
        [] -> 'undefined';
        [DisplayName |_] -> DisplayName
    end.

-spec is_empty_display_name(kz_term:api_binary()) -> boolean().
is_empty_display_name(<<>>) -> 'true';
is_empty_display_name(<<" ">>) -> 'true'; %% kzd_users:name/1 possible result
is_empty_display_name('undefined') -> 'true';
is_empty_display_name(_DN) -> 'false'.

-spec is_device_enabled(kzd_devices:doc(), kzd_users:doc(), kzd_accounts:doc()) ->
                               boolean().
is_device_enabled(DeviceDoc, UserDoc, AccountDoc) ->
    lists:all(fun kz_term:is_true/1
             ,[kzd_devices:enabled(DeviceDoc)
              ,kzd_users:enabled(UserDoc)
              ,kzd_accounts:is_enabled(AccountDoc)
              ]).

-spec fetch_account_from_device(kzd_devices:doc()) ->
                                       {'ok', kzd_accounts:doc()} |
                                       kz_datamgr:data_error().
fetch_account_from_device(DeviceDoc) ->
    kzd_accounts:fetch(kz_doc:account_id(DeviceDoc)).

-spec fetch_user_from_device(kzd_devices:doc()) ->
                                    {'ok', kzd_users:doc()} |
                                    kz_datamgr:data_error().
fetch_user_from_device(DeviceDoc) ->
    fetch_user_from_device(DeviceDoc, kzd_devices:owner_id(DeviceDoc)).

-spec fetch_user_from_device(kzd_devices:doc(), kz_term:api_ne_binary()) ->
                                    {'ok', kzd_users:doc()} |
                                    kz_datamgr:data_error().
fetch_user_from_device(_DeviceDoc, 'undefined') ->
    {'ok', kzd_users:new()};
fetch_user_from_device(DeviceDoc, OwnerId) ->
    kzd_users:fetch(kz_doc:account_id(DeviceDoc), OwnerId).

-spec settings_sip(kzd_devices:doc()) -> kz_json:object().
settings_sip(DeviceDoc) ->
    Realm = find_realm(DeviceDoc),
    Props = [{<<"username">>, kzd_devices:sip_username(DeviceDoc)}
            ,{<<"password">>, kzd_devices:sip_password(DeviceDoc)}
            ,{<<"realm">>, Realm}
            ],
    kz_json:from_list(Props).

-spec find_realm(kzd_devices:doc()) -> kz_term:api_ne_binary().
find_realm(Device) ->
    find_realm(Device, kzd_devices:sip_realm(Device)).

-spec find_realm(kzd_devices:doc(), kz_term:api_ne_binary()) -> kz_term:api_ne_binary().
find_realm(Device, 'undefined') ->
    kz_json:get_ne_binary_value(<<"realm">>, Device);
find_realm(_Device, Realm) -> Realm.

-spec settings_advanced(kzd_devices:doc()) -> kz_json:object().
settings_advanced(Device) ->
    Media = kzd_devices:media(Device, kz_json:new()),
    EncryptionMethods = kz_json:get_list_value([<<"encryption">>, <<"methods">>], Media, []),

    kz_json:from_list(
      [{<<"expire">>, kzd_devices:sip_expire_seconds(Device)}
       | [{M, 'true'} || M <- EncryptionMethods]
      ]).

-spec settings_datetime(kzd_devices:doc()) -> kz_json:object().
settings_datetime(Device) ->
    kz_json:from_list([{<<"time">>, settings_time(Device)}]).

-spec settings_feature_keys(kzd_devices:doc()) -> kz_json:object().
settings_feature_keys(Device) ->
    settings_keys(?FEATURE_KEYS, <<"feature_keys">>, Device).

-spec settings_combo_keys(kzd_devices:doc()) -> kz_json:object().
settings_combo_keys(Device) ->
    settings_keys(?COMBO_KEYS, <<"combo_keys">>, Device).

-spec settings_keys(kz_json:object(), kz_term:ne_binary(), kzd_devices:doc()) -> kz_json:object().
settings_keys(Assoc, KeyKind, Device) ->
    FeatureKeys = kz_json:get_value([<<"provision">>, KeyKind], Device, kz_json:new()),
    Brand = get_brand(Device),
    Family = get_family(Device),
    AccountId = kz_doc:account_id(Device),

    Fun = fun(Key, 'null', Acc) ->
                  %% workaround since `kz_json:set_value/3' is removing key if the value is `null'.
                  kz_json:from_list([{Key, 'null'} | kz_json:to_proplist(Acc)]);
             (Key, Value, Acc) ->
                  Type = kz_json:get_binary_value(<<"type">>, Value),
                  V = kz_json:get_value(<<"value">>, Value),
                  FeatureKey = get_feature_key(Type, V, Brand, Family, AccountId, Assoc),
                  maybe_add_feature_key(Key, FeatureKey, Acc)
          end,

    Keys = kz_json:foldl(Fun, kz_json:new(), FeatureKeys),

    case get_line_key(Brand, Family) of
        'undefined' -> Keys;
        LineKey -> kz_json:set_value(<<"account">>, LineKey, Keys)
    end.

-spec get_label(kzd_users:doc()) -> kz_term:api_binary().
get_label(User) ->
    case {kzd_users:first_name(User)
         ,kzd_users:last_name(User)
         }
    of
        {'undefined', 'undefined'} ->
            kz_json:get_ne_binary_value(<<"name">>, User);
        {First, 'undefined'} ->
            First;
        {'undefined', Last} ->
            Last;
        {First, Last} ->
            <<First/binary, " ", Last/binary>>
    end.

-spec get_feature_key(kz_term:ne_binary(), kz_term:api_ne_binary() | 0..10 | kz_json:object(), binary(), binary(), kz_term:ne_binary(), kz_json:object()) -> kz_term:api_object().
get_feature_key(<<"line">>=Type, _Value, Brand, Family, _AccountId, Assoc) ->
    kz_json:from_list(
      [{<<"label">>, <<>>}
      ,{<<"value">>, <<>>}
      ,{<<"type">>, get_feature_key_type(Assoc, Type, Brand, Family)}
      ,{<<"account">>, get_line_key(Brand, Family)}
      ]);
get_feature_key(_Type, 'undefined', _Brand, _Family, _AccountId, _Assoc) ->
    'undefined';
get_feature_key(<<"presence">>=Type, Value, Brand, Family, AccountId, Assoc) ->
    case get_user(AccountId, Value, <<>>) of
        'undefined' -> 'undefined';
        {Presence, Label} ->
            kz_json:from_list(
              [{<<"label">>, Label}
              ,{<<"value">>, Presence}
              ,{<<"type">>, get_feature_key_type(Assoc, Type, Brand, Family)}
              ,{<<"account">>, get_line_key(Brand, Family)}
              ])
    end;
get_feature_key(<<"speed_dial">>=Type, Value, Brand, Family, _AccountId, Assoc) ->
    case get_label_value(Value, <<>>) of
        'undefined' -> 'undefined';
        {RealValue, Label} ->
            kz_json:from_list(
              [{<<"label">>, Label}
              ,{<<"value">>, RealValue}
              ,{<<"type">>, get_feature_key_type(Assoc, Type, Brand, Family)}
              ,{<<"account">>, get_line_key(Brand, Family)}
              ])
    end;
get_feature_key(<<"personal_parking">>=Type, Value, Brand, Family, AccountId, Assoc) ->
    case get_user(AccountId, Value, <<"Park ">>) of
        'undefined' -> 'undefined';
        {Presence, Label} ->
            kz_json:from_list(
              [{<<"label">>, Label}
              ,{<<"value">>, <<"*3", Presence/binary>>}
              ,{<<"type">>, get_feature_key_type(Assoc, Type, Brand, Family)}
              ,{<<"account">>, get_line_key(Brand, Family)}
              ])
    end;
get_feature_key(<<"parking">>=Type, Value, Brand, Family, _AccountId, Assoc) ->
    case get_label_value(Value, <<"Park ">>) of
        'undefined' -> 'undefined';
        {RealValue, Label} ->
            kz_json:from_list(
              [{<<"label">>, Label}
              ,{<<"value">>, <<"*3", RealValue/binary>>}
              ,{<<"type">>, get_feature_key_type(Assoc, Type, Brand, Family)}
              ,{<<"account">>, get_line_key(Brand, Family)}
              ])
    end.

-spec get_line_key(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_binary().
get_line_key(<<"grandstream">>, _) -> <<"1">>;
get_line_key(<<"obihai">>, _) -> <<"1">>;
get_line_key(<<"vtech">>, _) -> <<"1">>;
get_line_key(<<"yealink">>, _) -> <<"1">>;
get_line_key(_, _) -> 'undefined'.

-spec get_feature_key_type(kz_json:object(), kz_term:ne_binary(), binary(), binary()) -> kz_term:api_object().
get_feature_key_type(Assoc, Type, Brand, Family) ->
    kz_json:get_first_defined([[Brand, Family, Type]
                              ,[Brand, <<"_">>, Type]
                              ]
                             ,Assoc
                             ).

-spec get_user(kz_term:ne_binary(), kz_term:ne_binary(), binary()) ->
                      {kz_term:ne_binary(), kz_term:api_ne_binary()} |
                      'undefined'.
get_user(AccountId, ?NE_BINARY = UserId, PrefixLabel) ->
    {'ok', UserJObj} = kzd_users:fetch(AccountId, UserId),

    case kzd_users:presence_id(UserJObj) of
        'undefined' -> 'undefined';
        Presence ->
            Label = case get_label(UserJObj) of
                        'undefined' -> 'undefined';
                        L -> <<PrefixLabel/binary, L/binary>>
                    end,
            {Presence, Label}
    end;
get_user(AccountId, JObj, PrefixLabel) ->
    case kz_json:is_json_object(JObj) of
        'true' ->
            get_user(AccountId
                    ,kz_json:get_ne_binary_value(<<"label">>, JObj)
                    ,kz_json:get_ne_binary_value(<<"value">>, JObj)
                    ,PrefixLabel
                    );
        'false' ->
            'undefined'
    end.

-spec get_user(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:api_ne_binary(), binary()) ->
                      {kz_term:ne_binary(), kz_term:api_ne_binary()} |
                      'undefined'.
get_user(_AccountId, _CustomLabel, 'undefined', _PrefixLabel) ->
    'undefined';
get_user(AccountId, CustomLabel, UserId, PrefixLabel) ->
    case get_user(AccountId, UserId, <<>>) of
        'undefined' ->
            'undefined';
        {Presence, Label} when CustomLabel =:= 'undefined'
                               andalso Label =/= 'undefined' ->
            {Presence, <<PrefixLabel/binary, Label/binary>>};
        {Presence, Label} when CustomLabel =:= 'undefined' ->
            {Presence, Label};
        {Presence, _} ->
            {Presence, CustomLabel}
    end.

-spec get_label_value(kz_term:ne_binary() | kz_json:object() | pos_integer(), binary()) ->
                             {kz_term:ne_binary(), kz_term:ne_binary()} |
                             'undefined'.
get_label_value(Value, PrefixLabel) when is_integer(Value) ->
    get_label_value(kz_term:to_binary(Value), PrefixLabel);
get_label_value(?NE_BINARY = Value, PrefixLabel) ->
    {Value, <<PrefixLabel/binary, Value/binary>>};
get_label_value(JObj, PrefixLabel) ->
    case kz_json:is_json_object(JObj) of
        'true' ->
            case {kz_json:get_ne_binary_value(<<"value">>, JObj)
                 ,kz_json:get_ne_binary_value(<<"label">>, JObj)
                 }
            of
                {'undefined', _} -> 'undefined';
                {Value, 'undefined'} -> get_label_value(Value, PrefixLabel);
                {Value, Label} -> {Value, Label}
            end;
        'false' ->
            'undefined'
    end.

-spec maybe_add_feature_key(kz_term:ne_binary(), kz_term:api_object(), kz_json:object()) -> kz_json:object().
maybe_add_feature_key(_Key, 'undefined', JObj) -> JObj;
maybe_add_feature_key(Key, FeatureKey, JObj) ->
    kz_json:set_value(Key
                     ,kz_json:from_list([{<<"key">>, FeatureKey}])
                     ,JObj
                     ).

-spec settings_time(kzd_devices:doc()) -> kz_json:object().
settings_time(Device) ->
    kz_json:from_list([{<<"timezone">>, kzd_devices:timezone(Device)}]).

-spec settings_codecs(kz_json:object()) -> kz_json:object().
settings_codecs(JObj) ->
    Codecs = props:filter_empty(
               [{<<"audio">>, format_codecs(JObj, <<"audio">>)}
               ,{<<"video">>, format_codecs(JObj, <<"video">>)}
               ]),
    case Codecs of
        [] -> kz_json:new();
        Props ->
            kz_json:from_list([{<<"0">>, kz_json:from_list(Props)}])
    end.

-spec format_codecs(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
format_codecs(JObj, Type) ->
    Codecs = kz_json:get_value([<<"media">>, Type, <<"codecs">>], JObj, []),
    Keys = [<<"primary_codec">>
           ,<<"secondary_codec">>
           ,<<"tertiary_codec">>
           ,<<"quaternary_codec">>
           ],
    format_codecs(Codecs, Keys, kz_json:new()).

-spec format_codecs(kz_term:ne_binaries(), kz_term:ne_binaries(), kz_json:object()) -> kz_json:object().
format_codecs(_Codecs, [], JObj) -> JObj;
format_codecs([], [Key|Keys], JObj) ->
    %% kz_json:set_value does not let you set null values so this does that...
    format_codecs([], Keys, kz_json:from_list([{Key, 'null'} | kz_json:to_proplist(JObj)]));
format_codecs([Codec|Codecs], [Key|Keys], JObj) ->
    format_codecs(Codecs, Keys, kz_json:set_value(Key, Codec, JObj)).

%%------------------------------------------------------------------------------
%% @doc Send provisioning request
%% @end
%%------------------------------------------------------------------------------
-spec send_req(atom(), kz_term:api_object(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_binary()) -> 'ok'.
send_req('devices_post', JObj, AuthToken, AccountId, MACAddress) ->
    Data = kz_json:encode(device_payload(JObj)),
    Headers = req_headers(AuthToken),
    UrlString = req_uri('devices', AccountId, MACAddress),
    lager:debug("provisioning via ~s: ~s", [UrlString, Data]),
    Resp = kz_http:post(UrlString, Headers, Data),
    handle_resp(Resp, AccountId, AuthToken);
send_req('devices_delete', _, AuthToken, AccountId, MACAddress) ->
    Headers = req_headers(AuthToken),
    UrlString = req_uri('devices', AccountId, MACAddress),
    lager:debug("deprovisioning via ~s", [UrlString]),
    Resp = kz_http:delete(UrlString, Headers),
    handle_resp(Resp, AccountId, AuthToken);
send_req('accounts_delete', _, AuthToken, AccountId, _) ->
    Headers = req_headers(AuthToken),
    UrlString = req_uri('accounts', AccountId),
    lager:debug("accounts delete via ~s", [UrlString]),
    Resp = kz_http:delete(UrlString, Headers),
    handle_resp(Resp, AccountId, AuthToken);
send_req('accounts_update', JObj, AuthToken, AccountId, _) ->
    Data = kz_json:encode(account_payload(JObj, AccountId)),
    Headers = req_headers(AuthToken),
    UrlString = req_uri('accounts', AccountId),
    lager:debug("account update via ~s: ~s", [UrlString, Data]),
    Resp = kz_http:post(UrlString, Headers, Data),
    handle_resp(Resp, AccountId, AuthToken).

-spec req_uri('accounts' | 'devices', kz_term:ne_binary()) -> iolist().
req_uri('accounts', AccountId) ->
    provisioning_uri([<<"accounts">>, AccountId]);
req_uri('devices', MacAddress) ->
    provisioning_uri([<<"devices">>, <<"search">>, MacAddress]).

-spec req_uri('devices', kz_term:ne_binary(), kz_term:ne_binary()) -> iolist().
req_uri('devices', AccountId, MACAddress) ->
    EncodedAddress = binary:replace(MACAddress, <<":">>, <<>>, ['global']),
    provisioning_uri([<<"devices">>, AccountId, EncodedAddress]).

-spec provisioning_uri(iolist()) -> iolist().
provisioning_uri(ExplodedPath) ->
    Url = kapps_config:get_binary(?MOD_CONFIG_CAT, <<"provisioning_url">>),
    Uri = kz_util:uri(Url, ExplodedPath),
    binary:bin_to_list(Uri).

-spec account_payload(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
account_payload(JObj, AccountId) ->
    ResellerId = kz_services_reseller:get_id(AccountId),
    kz_json:from_list(
      [{<<"create_if_missing">>, 'true'}
      ,{<<"reseller_id">>, ResellerId}
      ,{<<"merge">>, 'true'}
      ,{<<"data">>, JObj}
      ]).

-spec device_payload(kz_json:object()) -> kz_json:object().
device_payload(JObj) ->
    kz_json:from_list(
      [{<<"create_if_missing">>, 'true'}
      ,{<<"generate">>, 'true'}
      ,{<<"merge">>, 'true'}
      ,{<<"data">>, JObj}
      ]).

-spec handle_resp(kz_http:ret(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
handle_resp({'ok', 200, _, Resp}, _, _) ->
    lager:debug("provisioning success ~s", [Resp]);
handle_resp({'ok', Code, _, Resp}, AccountId, AuthToken) ->
    lager:warning("provisioning error ~p. ~s", [Code, Resp]),
    create_alert(kz_json:decode(Resp), AccountId, AuthToken);
handle_resp(_Error, _, _) ->
    lager:error("provisioning fatal error ~p", [_Error]).

create_alert(JObj, AccountId, AuthToken) ->
    Props = [{<<"metadata">>, JObj}
            ,{<<"category">>, <<"provisioner">>}
            ],

    OwnerId =
        case kz_datamgr:open_cache_doc(?KZ_TOKEN_DB, AuthToken) of
            {'error', _R} -> 'undefined';
            {'ok', AuthJObj} -> kz_json:get_value(<<"owner_id">>, AuthJObj)
        end,

    From = [kz_json:from_list([{<<"type">>, <<"account">>}
                              ,{<<"value">>, AccountId}
                              ])
           ,kz_json:from_list([{<<"type">>, <<"user">>}
                              ,{<<"value">>, OwnerId}
                              ])
           ],

    To = [kz_json:from_list([{<<"type">>, AccountId}
                            ,{<<"value">>, <<"admins">>}
                            ])
         ,kz_json:from_list([{<<"type">>, kz_services_reseller:get_id(AccountId)}
                            ,{<<"value">>, <<"admins">>}
                            ])
         ],

    Title = <<"Provisioning Error">>,
    Msg = <<"Error trying to provision device">>,
    {'ok', AlertJObj} = kapps_alert:create(Title, Msg, From, To, Props),
    {'ok', _} = kapps_alert:save(AlertJObj),
    lager:debug("created alert ~s: ~s", [Title, Msg]).

-spec req_headers(kz_term:ne_binary()) -> kz_term:proplist().
req_headers(Token) ->
    props:filter_undefined(
      [{"Content-Type", "application/json"}
      ,{"X-Auth-Token", kz_term:to_list(Token)}
      ,{"X-Kazoo-Cluster-ID", kzd_cluster:id()}
      ,{"User-Agent", kz_term:to_list(erlang:node())}
      ]).

