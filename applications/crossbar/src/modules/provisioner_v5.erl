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

-include("crossbar.hrl").
-include("provisioner_v5.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_device(kz_json:object(), kz_term:ne_binary()) -> 'ok'.
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

-spec delete_device(kz_json:object(), kz_term:ne_binary()) -> 'ok'.
delete_device(JObj, AuthToken) ->
    send_req('devices_delete'
            ,'undefined'
            ,AuthToken
            ,kz_doc:account_id(JObj)
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
    kz_json:expand(
      kz_json:from_list(
        [case KV of
             {[<<"advanced">>, <<"expire">>], undefined} -> {Path, 360};
             {[<<"advanced">>, <<"srtp">>], undefined} -> {Path, false};
             {[<<"basic">>, <<"enabled">>], undefined} -> {Path, true};
             _ -> KV
         end
         || {Path,_}=KV <- kz_json:to_proplist(kz_json:flatten(LineJObj))
        ])).

-spec device_settings(kz_json:object()) -> kz_json:object().
device_settings(JObj) ->
    kz_json:from_list(
      [{<<"brand">>, get_brand(JObj)}
      ,{<<"family">>, get_family(JObj)}
      ,{<<"model">>, get_model(JObj)}
      ,{<<"name">>, kzd_devices:name(JObj)}
      ,{<<"settings">>, settings(JObj)}
      ]).

-spec get_brand(kz_json:object()) -> binary().
get_brand(JObj) ->
    kz_json:get_binary_value([<<"provision">>, <<"endpoint_brand">>], JObj, <<>>).

-spec get_family(kz_json:object()) -> binary().
get_family(JObj) ->
    case kz_json:get_binary_value([<<"provision">>, <<"endpoint_family">>], JObj, <<>>) of
        %% Temporary hack to fix family names till a script can clean the database
        <<"f", Family/binary>> -> Family;
        Family -> Family
    end.

-spec get_model(kz_json:object()) -> kz_term:ne_binary().
get_model(JObj) ->
    Family = kz_json:get_binary_value([<<"provision">>, <<"endpoint_model">>], JObj, <<>>),
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
-spec update_account(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary()) -> 'ok'.
update_account(AccountId, JObj, AuthToken) ->
    send_req('accounts_update'
            ,account_settings(JObj)
            ,AuthToken
            ,AccountId
            ,'undefined'
            ).

-spec delete_account(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
delete_account(AccountId, AuthToken) ->
    send_req('accounts_delete'
            ,'undefined'
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

-spec account_settings(kz_json:object()) -> kz_json:object().
account_settings(JObj) ->
    kz_json:from_list(
      [{<<"settings">>, settings(JObj)}
      ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_user(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary()) -> 'ok'.
update_user(AccountId, JObj, AuthToken) ->
    case kz_doc:type(JObj) of
        <<"user">> -> save_user(AccountId, JObj, AuthToken);
        _ -> 'ok' %% Gets rid of VMbox
    end.

-spec save_user(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary()) -> 'ok'.
save_user(AccountId, JObj, AuthToken) ->
    _ = update_account(AccountId, AuthToken),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    Devices = crossbar_util:get_devices_by_owner(AccountDb, kz_doc:id(JObj)),
    Settings = settings(JObj),
    lists:foreach(
      fun(Device) ->
              maybe_save_device(Device, Settings, AccountId, AuthToken)
      end, Devices).

-spec maybe_save_device(kz_json:object(), kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) ->
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

-spec save_device(kz_term:ne_binary(), kz_json:object(), kz_json:object(), kz_term:ne_binary()) -> 'ok'.
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
            kz_json:get_value([<<"data">>, <<"account_id">>], JObj);
        _AnythingElse ->
            lager:debug("device ~s not found: ~p", [MacAddress, _AnythingElse]),
            'false'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_owner(kz_json:object()) -> kz_json:object().
set_owner(JObj) ->
    OwnerId = kz_json:get_ne_value(<<"owner_id">>, JObj),
    case get_owner(OwnerId, kz_doc:account_id(JObj)) of
        {'ok', Doc} -> kz_json:merge(Doc, JObj);
        {'error', _R} -> JObj
    end.

-spec get_owner(kz_term:api_binary(), kz_term:ne_binary()) ->
                       {'ok', kz_json:object()} |
                       {'error', any()}.
get_owner('undefined', _) -> {'error', 'undefined'};
get_owner(OwnerId, AccountId) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    kz_datamgr:open_cache_doc(AccountDb, OwnerId).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec settings(kz_json:object()) -> kz_json:object().
settings(JObj) ->
    Props = props:filter_empty(
              [{<<"lines">>, settings_lines(JObj)}
              ,{<<"codecs">>, settings_codecs(JObj)}
              ,{<<"datetime">>, settings_datetime(JObj)}
              ,{<<"feature_keys">>, settings_feature_keys(JObj)}
              ,{<<"line_keys">>, settings_line_keys(JObj)}
              ,{<<"combo_keys">>, settings_combo_keys(JObj)}
              ]),
    kz_json:from_list(Props).

-spec settings_line_keys(kz_json:object()) -> kz_json:object().
settings_line_keys(JObj) ->
    settings_line_keys(get_brand(JObj), get_family(JObj)).

-spec settings_line_keys(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
settings_line_keys(<<"yealink">>, _) ->
    Props = props:filter_empty(
              [{<<"account">>, <<"1">>}
              ,{<<"type">>, <<"15">>}
              ]),
    Key = kz_json:from_list([{<<"key">>, kz_json:from_list(Props)}]),
    kz_json:from_list([{<<"0">>, Key}]);
settings_line_keys(_, _) -> 'undefined'.

-spec settings_lines(kz_json:object()) -> kz_json:object().
settings_lines(JObj) ->
    case props:filter_empty(
           [{<<"basic">>, settings_basic(JObj)}
           ,{<<"sip">>, settings_sip(JObj)}
           ,{<<"advanced">>, settings_advanced(JObj)}
           ])
    of
        [] -> kz_json:new();
        Props -> kz_json:from_list([{<<"0">>, kz_json:from_list(Props)}])
    end.

-spec settings_basic(kz_json:object()) -> kz_json:object().
settings_basic(JObj) ->
    Enabled = case kz_json:get_ne_value(<<"enabled">>, JObj) of
                  'undefined' -> 'undefined';
                  Else -> kz_term:is_true(Else)
              end,
    Props = props:filter_undefined(
              [{<<"display_name">>, kz_json:get_ne_value(<<"name">>, JObj)}
              ,{<<"enable">>, Enabled}
              ]),
    kz_json:from_list(Props).

-spec settings_sip(kz_json:object()) -> kz_json:object().
settings_sip(JObj) ->
    RealmPaths = [[<<"sip">>, <<"realm">>]
                 ,<<"realm">>
                 ],
    Realm = kz_json:get_first_defined(RealmPaths, JObj),
    Props = props:filter_undefined(
              [{<<"username">>, kzd_devices:sip_username(JObj)}
              ,{<<"password">>, kzd_devices:sip_password(JObj)}
              ,{<<"realm">>, Realm}
              ]),
    kz_json:from_list(Props).

-spec settings_advanced(kz_json:object()) -> kz_json:object().
settings_advanced(JObj) ->
    EncryptionMethods = kz_json:get_value([<<"media">>, <<"encryption">>, <<"methods">>], JObj, []),
    kz_json:from_list(
      [{<<"expire">>, kz_json:get_integer_value([<<"sip">>, <<"expire_seconds">>], JObj)}
       | [{M, 'true'} || M <- EncryptionMethods]
      ]).

-spec settings_datetime(kz_json:object()) -> kz_json:object().
settings_datetime(JObj) ->
    kz_json:from_list(
      [{<<"time">>, settings_time(JObj)}
      ]).

-spec settings_feature_keys(kz_json:object()) -> kz_json:object().
settings_feature_keys(JObj) ->
    settings_keys(?FEATURE_KEYS, <<"feature_keys">>, JObj).

-spec settings_combo_keys(kz_json:object()) -> kz_json:object().
settings_combo_keys(JObj) ->
    settings_keys(?COMBO_KEYS, <<"combo_keys">>, JObj).

-spec settings_keys(kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
settings_keys(Assoc, KeyKind, JObj) ->
    FeatureKeys = kz_json:get_value([<<"provision">>, KeyKind], JObj, kz_json:new()),
    Brand = get_brand(JObj),
    Family = get_family(JObj),
    AccountId = kz_doc:account_id(JObj),

    Fun = fun(Key, null, Acc) ->
                  %% workaround since `kz_json:set_value/3' is removing key if the value is `null'.
                  kz_json:from_list([{Key, null} | kz_json:to_proplist(Acc)]);
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

-spec get_label(kz_json:object()) -> binary().
get_label(Doc) ->
    case {kz_json:get_ne_binary_value(<<"first_name">>, Doc)
         ,kz_json:get_ne_binary_value(<<"last_name">>, Doc)
         }
    of
        {'undefined', 'undefined'} ->
            kz_json:get_value(<<"name">>, Doc);
        {First, 'undefined'} ->
            First;
        {'undefined', Last} ->
            Last;
        {First, Last} ->
            <<First/binary, " ", Last/binary>>
    end.

-spec get_feature_key(kz_term:ne_binary(), kz_term:api_ne_binary() | 0..10 | kz_json:object(), binary(), binary(), kz_term:ne_binary(), kz_json:object()) -> kz_term:api_object().
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
    end;
get_feature_key(<<"line">>=Type, _Value, Brand, Family, _AccountId, Assoc) ->
    kz_json:from_list(
      [{<<"label">>, <<>>}
      ,{<<"value">>, <<>>}
      ,{<<"type">>, get_feature_key_type(Assoc, Type, Brand, Family)}
      ,{<<"account">>, get_line_key(Brand, Family)}
      ]).

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
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    {'ok', UserJObj} = kz_datamgr:open_cache_doc(AccountDb, UserId),
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

-spec get_label_value(kz_term:ne_binary() | kz_json:object(), binary()) ->
                             {kz_term:ne_binary(), kz_term:ne_binary()} |
                             'undefined'.
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

-spec settings_time(kz_json:object()) -> kz_json:object().
settings_time(JObj) ->
    kz_json:from_list(
      [{<<"timezone">>, kz_json:get_value(<<"timezone">>, JObj)}
      ]).

-spec settings_codecs(kz_json:object()) -> kz_json:object().
settings_codecs(JObj) ->
    case props:filter_empty(
           [{<<"audio">>, settings_audio(JObj)}
           ])
    of
        [] -> kz_json:new();
        Props ->
            kz_json:from_list([{<<"0">>, kz_json:from_list(Props)}])
    end.

-spec settings_audio(kz_json:object()) -> kz_json:object().
settings_audio(JObj) ->
    Codecs = kz_json:get_value([<<"media">>, <<"audio">>, <<"codecs">>], JObj, []),
    Keys = [<<"primary_codec">>
           ,<<"secondary_codec">>
           ,<<"tertiary_codec">>
           ,<<"quaternary_codec">>
           ],
    settings_audio(Codecs, Keys, kz_json:new()).


-spec settings_audio(kz_term:ne_binaries(), kz_term:ne_binaries(), kz_json:object()) -> kz_json:object().
settings_audio([], [], JObj) -> JObj;
settings_audio([], [Key|Keys], JObj) ->
    %% kz_json:set_value does not let you set null values so this does that...
    settings_audio([], Keys, kz_json:from_list([{Key, 'null'} | kz_json:to_proplist(JObj)]));
settings_audio([Codec|Codecs], [Key|Keys], JObj) ->
    settings_audio(Codecs, Keys, kz_json:set_value(Key, Codec, JObj)).

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
    {ok, _} = kapps_alert:save(AlertJObj),
    ok.

-spec req_headers(kz_term:ne_binary()) -> kz_term:proplist().
req_headers(Token) ->
    props:filter_undefined(
      [{"Content-Type", "application/json"}
      ,{"X-Auth-Token", kz_term:to_list(Token)}
      ,{"X-Kazoo-Cluster-ID", get_cluster_id()}
      ,{"User-Agent", kz_term:to_list(erlang:node())}
      ]).

-spec get_cluster_id() -> nonempty_string().
get_cluster_id() ->
    case kapps_config:get_string(?MOD_CONFIG_CAT, <<"cluster_id">>) of
        'undefined' ->
            ClusterId = kz_binary:rand_hex(16),
            {'ok', _JObj} = kapps_config:set_default(?MOD_CONFIG_CAT, <<"cluster_id">>, ClusterId),
            kz_term:to_list(ClusterId);
        ClusterId -> ClusterId
    end.
