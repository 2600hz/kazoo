%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz INC
%%% @doc
%%%
%%% Common functions for the provisioner modules
%%%
%%% @end
%%% @contributors
%%%    Peter Defebvre
%%%-------------------------------------------------------------------
-module(provisioner_v5).

-export([update_device/2]).
-export([delete_device/2]).
-export([delete_account/2]).
-export([update_account/3]).
-export([update_user/3]).
-export([check_MAC/2]).

-include("crossbar.hrl").
-include("provisioner_v5.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_device(kz_json:object(), ne_binary()) -> 'ok'.
update_device(JObj, AuthToken) ->
    AccountId = kz_doc:account_id(JObj),
    Request = device_settings(set_owner(JObj)),
    case check_request(Request) of
        {'ok', Data} ->
            _ = update_account(AccountId, AuthToken),
            send_req('devices_post'
		    ,Data
		    ,AuthToken
		    ,AccountId
		    ,kz_device:mac_address(JObj)
                    );
        {'error', Errors} ->
            handle_validation_error(Errors, AccountId)
    end.

-spec delete_device(kz_json:object(), ne_binary()) -> 'ok'.
delete_device(JObj, AuthToken) ->
    send_req('devices_delete'
	    ,'undefined'
	    ,AuthToken
	    ,kz_doc:account_id(JObj)
	    ,kz_device:mac_address(JObj)
            ).

-spec device_settings(kz_json:object()) -> kz_json:object().
device_settings(JObj) ->
    kz_json:from_list(
      [{<<"brand">>, get_brand(JObj)}
      ,{<<"family">>, get_family(JObj)}
      ,{<<"model">>, get_model(JObj)}
      ,{<<"name">>, kz_device:name(JObj)}
      ,{<<"settings">>, settings(JObj)}
      ]
     ).

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

-spec get_model(kz_json:object()) -> ne_binary().
get_model(JObj) ->
    Family = kz_json:get_binary_value([<<"provision">>, <<"endpoint_model">>], JObj, <<>>),
    case kz_util:to_lower_binary(Family) of
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_account(ne_binary(), kz_json:object(), ne_binary()) -> 'ok'.
update_account(AccountId, JObj, AuthToken) ->
    send_req('accounts_update'
	    ,account_settings(JObj)
	    ,AuthToken
	    ,AccountId
	    ,'undefined'
            ).

-spec delete_account(ne_binary(), ne_binary()) -> 'ok'.
delete_account(AccountId, AuthToken) ->
    send_req('accounts_delete'
	    ,'undefined'
	    ,AuthToken
	    ,AccountId
	    ,'undefined'
            ).
-spec update_account(ne_binary(), ne_binary()) -> 'ok'.
update_account(Account, AuthToken) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    case kz_account:fetch(AccountId) of
        {'ok', JObj} -> update_account(AccountId, JObj, AuthToken);
        {'error', _R} ->
            lager:debug("unable to fetch account ~s: ~p", [AccountId, _R])
    end.

-spec account_settings(kz_json:object()) -> kz_json:object().
account_settings(JObj) ->
    kz_json:from_list(
      [{<<"settings">>, settings(JObj)}]
     ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_user(ne_binary(), kz_json:object(), ne_binary()) -> 'ok'.
update_user(AccountId, JObj, AuthToken) ->
    case kz_doc:type(JObj) of
        <<"user">> -> save_user(AccountId, JObj, AuthToken);
        _ -> 'ok' %% Gets rid of VMbox
    end.

-spec save_user(ne_binary(), kz_json:object(), ne_binary()) -> 'ok'.
save_user(AccountId, JObj, AuthToken) ->
    _ = update_account(AccountId, AuthToken),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    Devices = crossbar_util:get_devices_by_owner(AccountDb, kz_doc:id(JObj)),
    Settings = settings(JObj),
    lists:foreach(
      fun(Device) ->
              maybe_save_device(Device, Settings, AccountId, AuthToken)
      end, Devices).

-spec maybe_save_device(kz_json:object(), kz_json:object(), ne_binary(), ne_binary()) ->
                               'ok' | {'EXIT', _}.
maybe_save_device(Device, Settings, AccountId, AuthToken) ->
    Request = kz_json:from_list(
                [{<<"brand">>, get_brand(Device)}
		,{<<"family">>, get_family(Device)}
		,{<<"model">>, get_model(Device)}
		,{<<"name">>, kz_device:name(Device)}
		,{<<"settings">>, Settings}
                ]
               ),
    catch save_device(AccountId, Device, Request, AuthToken).

-spec save_device(ne_binary(), kz_json:object(), kz_json:object(), ne_binary()) -> 'ok'.
save_device(AccountId, Device, Request, AuthToken) ->
    case kz_device:mac_address(Device) of
        'undefined' -> 'ok';
        MacAddress ->
            send_req('devices_post'
		    ,Request
		    ,AuthToken
		    ,AccountId
		    ,MacAddress
                    )
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Use before a POST or PUT to a device.
%% Return the account id a MAC address belongs to, `false' otherwise.
%% @end
%%--------------------------------------------------------------------
-spec check_MAC(ne_binary(), ne_binary()) -> ne_binary() | 'false'.
check_MAC(MacAddress, AuthToken) ->
    Headers = req_headers(AuthToken),
    UrlString = req_uri('devices', MacAddress),
    lager:debug("pre-provisioning via ~s", [UrlString]),
    case kz_http:get(UrlString, Headers) of
        {'ok', 200, _RespHeaders, JSONStr} ->
            JObj = kz_json:decode(JSONStr),
            kz_json:get_value([<<"data">>, <<"account_id">>], JObj);
        _AnythingElse -> 'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_owner(kz_json:object()) -> kz_json:object().
set_owner(JObj) ->
    OwnerId = kz_json:get_ne_value(<<"owner_id">>, JObj),
    case get_owner(OwnerId, kz_doc:account_id(JObj)) of
        {'ok', Doc} -> kz_json:merge_recursive(Doc, JObj);
        {'error', _R} -> JObj
    end.

-spec get_owner(api_binary(), ne_binary()) ->
                       {'ok', kz_json:object()} |
                       {'error', any()}.
get_owner('undefined', _) -> {'error', 'undefined'};
get_owner(OwnerId, AccountId) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    kz_datamgr:open_cache_doc(AccountDb, OwnerId).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec settings(kz_json:object()) -> kz_json:object().
settings(JObj) ->
    Props = props:filter_empty(
              [{<<"lines">>, settings_lines(JObj)}
	      ,{<<"codecs">>, settings_codecs(JObj)}
	      ,{<<"datetime">>, settings_datetime(JObj)}
	      ,{<<"feature_keys">>, settings_feature_keys(JObj)}
	      ,{<<"line_keys">>, settings_line_keys(JObj)}
              ]
             ),
    kz_json:from_list(Props).

-spec settings_line_keys(kz_json:object()) -> kz_json:object().
settings_line_keys(JObj) ->
    Brand = get_brand(JObj),
    Family = get_family(JObj),
    settings_line_keys(Brand, Family).

-spec settings_line_keys(ne_binary(), ne_binary()) -> kz_json:object().
settings_line_keys(<<"yealink">>, _) ->
    Props = props:filter_empty(
              [{<<"account">>, <<"1">>}
              ,{<<"type">>, <<"15">>}
              ]
             ),
    Key = kz_json:from_list([{<<"key">>, kz_json:from_list(Props)}]),
    kz_json:from_list([{<<"0">>, Key}]);
settings_line_keys(_, _) -> 'undefined'.

-spec settings_lines(kz_json:object()) -> kz_json:object().
settings_lines(JObj) ->
    case props:filter_empty(
           [{<<"basic">>, settings_basic(JObj)},
            {<<"sip">>, settings_sip(JObj)},
            {<<"advanced">>, settings_advanced(JObj)}
           ])
    of
        [] -> kz_json:new();
        Props -> kz_json:from_list([{<<"0">>, kz_json:from_list(Props)}])
    end.

-spec settings_basic(kz_json:object()) -> kz_json:object().
settings_basic(JObj) ->
    Enabled = case kz_json:get_ne_value(<<"enabled">>, JObj) of
                  'undefined' -> 'undefined';
                  Else -> kz_util:is_true(Else)
              end,
    Props = props:filter_undefined(
              [{<<"display_name">>, kz_json:get_ne_value(<<"name">>, JObj)}
	      ,{<<"enable">>, Enabled}
              ]
             ),
    kz_json:from_list(Props).

-spec settings_sip(kz_json:object()) -> kz_json:object().
settings_sip(JObj) ->
    Realm = kz_json:get_first_defined(
              [[<<"sip">>, <<"realm">>],
               <<"realm">>
              ], JObj
             ),
    Props = props:filter_undefined(
              [{<<"username">>, kz_device:sip_username(JObj)}
	      ,{<<"password">>, kz_device:sip_password(JObj)}
	      ,{<<"realm">>, Realm}
              ]
             ),
    kz_json:from_list(Props).

-spec settings_advanced(kz_json:object()) -> kz_json:object().
settings_advanced(JObj) ->
    EncryptionMethods = kz_json:get_value([<<"media">>, <<"encryption">>, <<"methods">>], JObj, []),
    Props = props:filter_undefined(
              [{<<"expire">>, kz_json:get_integer_value([<<"sip">>, <<"expire_seconds">>], JObj)}
               | [{M, 'true'} || M <- EncryptionMethods]
              ]),
    kz_json:from_list(Props).

-spec settings_datetime(kz_json:object()) -> kz_json:object().
settings_datetime(JObj) ->
    Props = props:filter_empty(
              [{<<"time">>, settings_time(JObj)}]
             ),
    kz_json:from_list(Props).

-spec settings_feature_keys(kz_json:object()) -> kz_json:object().
settings_feature_keys(JObj) ->
    FeatureKeys = kz_json:get_value([<<"provision">>, <<"feature_keys">>], JObj, kz_json:new()),
    Brand = get_brand(JObj),
    Family = get_family(JObj),
    AccountId = kz_doc:account_id(JObj),
    Keys =
        kz_json:foldl(
          fun(Key, Value, Acc) ->
                  Type = kz_json:get_binary_value(<<"type">>, Value),
                  V = kz_json:get_binary_value(<<"value">>, Value),
                  FeatureKey = get_feature_key(Type, V, Brand, Family, AccountId),
                  maybe_add_feature_key(Key, FeatureKey, Acc)
          end
		     ,kz_json:new()
		     ,FeatureKeys
         ),
    case get_line_key(Brand, Family) of
        'undefined' -> Keys;
        LineKey -> kz_json:set_value(<<"account">>, LineKey, Keys)
    end.

-spec get_feature_key(ne_binary(), ne_binary(), binary(), binary(), ne_binary()) ->
                             api_object().
get_feature_key(<<"presence">> = Type, Value, Brand, Family, AccountId) ->
    {'ok', UserJObj} = get_user(AccountId, Value),
    case kz_device:presence_id(UserJObj) of
        'undefined' -> 'undefined';
        Presence ->
            kz_json:from_list(
              props:filter_undefined(
                [{<<"label">>, Presence}
		,{<<"value">>, Presence}
		,{<<"type">>, get_feature_key_type(Type, Brand, Family)}
		,{<<"account">>, get_line_key(Brand, Family)}
                ])
             )
    end;
get_feature_key(<<"speed_dial">> = Type, Value, Brand, Family, _AccountId) ->
    kz_json:from_list(
      props:filter_undefined(
        [{<<"label">>, Value}
	,{<<"value">>, Value}
	,{<<"type">>, get_feature_key_type(Type, Brand, Family)}
	,{<<"account">>, get_line_key(Brand, Family)}
        ])
     );
get_feature_key(<<"personal_parking">> = Type, Value, Brand, Family, AccountId) ->
    {'ok', UserJObj} = get_user(AccountId, Value),
    case kz_device:presence_id(UserJObj) of
        'undefined' -> 'undefined';
        Presence ->
            kz_json:from_list(
              props:filter_undefined(
                [{<<"label">>, <<>>}
		,{<<"value">>, <<"*3", Presence/binary>>}
		,{<<"type">>, get_feature_key_type(Type, Brand, Family)}
		,{<<"account">>, get_line_key(Brand, Family)}
                ]
               )
             )
    end;
get_feature_key(<<"parking">> = Type, Value, Brand, Family, _AccountId) ->
    kz_json:from_list(
      props:filter_undefined(
        [{<<"label">>, <<>>}
	,{<<"value">>, <<"*3", Value/binary>>}
	,{<<"type">>, get_feature_key_type(Type, Brand, Family)}
	,{<<"account">>, get_line_key(Brand, Family)}
        ]
       )
     ).

-spec get_line_key(ne_binary(), ne_binary()) -> api_binary().
get_line_key(<<"yealink">>, _) -> <<"0">>;
get_line_key(_, _) -> 'undefined'.

-spec get_feature_key_type(ne_binary(), binary(), binary()) -> api_object().
get_feature_key_type(Type, Brand, Family) ->
    kz_json:get_first_defined([[Brand, Family, Type]
			      ,[Brand, <<"_">>, Type]
                              ]
			     ,?FEATURE_KEYS
                             ).

-spec get_user(ne_binary(), ne_binary()) -> {'ok', kz_json:object()} |
                                            {'error', any()}.
get_user(AccountId, UserId) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    kz_datamgr:open_cache_doc(AccountDb, UserId).

-spec maybe_add_feature_key(ne_binary(), api_object(), kz_json:object()) -> kz_json:object().
maybe_add_feature_key(_Key, 'undefined', JObj) -> JObj;
maybe_add_feature_key(Key, FeatureKey, JObj) ->
    kz_json:set_value(
      Key
		     ,kz_json:from_list([{<<"key">>, FeatureKey}])
		     ,JObj
     ).

-spec settings_time(kz_json:object()) -> kz_json:object().
settings_time(JObj) ->
    Props = props:filter_undefined(
              [{<<"timezone">>, kz_json:get_value(<<"timezone">>, JObj)}]
             ),
    kz_json:from_list(Props).

-spec settings_codecs(kz_json:object()) -> kz_json:object().
settings_codecs(JObj) ->
    case props:filter_empty(
           [{<<"audio">>, settings_audio(JObj)}]
          )
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

-spec settings_audio(ne_binaries(), ne_binaries(), kz_json:object()) -> kz_json:object().
settings_audio([], _, JObj) -> JObj;
settings_audio(_, [], JObj) -> JObj;
settings_audio([Codec|Codecs], [Key|Keys], JObj) ->
    settings_audio(Codecs, Keys, kz_json:set_value(Key, Codec, JObj)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Send provisioning request
%% @end
%%--------------------------------------------------------------------
-spec send_req(atom(), api_object(), ne_binary(), ne_binary(), api_binary()) -> 'ok'.

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
    lager:debug("unprovisioning via ~s", [UrlString]),
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

-spec req_uri('accounts' | 'devices', ne_binary()) -> iolist().
req_uri('accounts', AccountId) ->
    provisioning_uri([<<"accounts">>, AccountId]);
req_uri('devices', MacAddress) ->
    provisioning_uri([<<"devices">>, MacAddress]).

-spec req_uri('devices', ne_binary(), ne_binary()) -> iolist().
req_uri('devices', AccountId, MACAddress) ->
    EncodedAddress = binary:replace(MACAddress, <<":">>, <<>>, ['global']),
    provisioning_uri([<<"devices">>, AccountId, EncodedAddress]).

-spec provisioning_uri(iolist()) -> iolist().
provisioning_uri(ExplodedPath) ->
    Url = kapps_config:get_binary(?MOD_CONFIG_CAT, <<"provisioning_url">>),
    Uri = kz_util:uri(Url, ExplodedPath),
    binary:bin_to_list(Uri).

-spec account_payload(kz_json:object(), ne_binary()) -> kz_json:object().
account_payload(JObj, AccountId) ->
    ResellerId = kz_services:find_reseller_id(AccountId),
    kz_json:from_list(
      [{<<"create_if_missing">>, 'true'}
      ,{<<"reseller_id">>, ResellerId}
      ,{<<"merge">>, 'true'}
      ,{<<"data">>, JObj}
      ]
     ).

-spec device_payload(kz_json:object()) -> kz_json:object().
device_payload(JObj) ->
    kz_json:from_list(
      [{<<"create_if_missing">>, 'true'}
      ,{<<"generate">>, 'true'}
      ,{<<"merge">>, 'true'}
      ,{<<"data">>, JObj}
      ]
     ).

-spec handle_resp(kz_http:ret(), ne_binary(), ne_binary()) -> 'ok'.
handle_resp({'ok', 200, _, Resp}, _AccountId, _AuthToken) ->
    lager:debug("provisioning success ~s", [decode(Resp)]);
handle_resp({'ok', Code, _, Resp}, AccountId, AuthToken) ->
    JObj = decode(Resp),
    _ = create_alert(JObj, AccountId, AuthToken),
    lager:warning("provisioning error ~p. ~s", [Code, JObj]);
handle_resp(_Error, _AccountId, _AuthToken) ->
    lager:error("provisioning fatal error ~p", [_Error]).

create_alert(JObj, AccountId, AuthToken) ->
    Props = [{<<"metadata">>, JObj}
            ,{<<"category">>, <<"provisioner">>}
            ],

    OwnerId =
        case kz_datamgr:open_cache_doc(?KZ_TOKEN_DB, AuthToken) of
            {'error', _R} -> 'undefined';
            {'ok', AuthJObj} ->
                kz_json:get_value(<<"owner_id">>, AuthJObj)
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
         ,kz_json:from_list([{<<"type">>, kz_services:get_reseller_id(AccountId)}
                            ,{<<"value">>, <<"admins">>}
                            ])
         ],

    {'ok', AlertJObj} =
        kapps_alert:create(<<"Provisioning Error">>
			  ,<<"Error trying to provision device">>
			  ,From
			  ,To
			  ,Props
			  ),
    kapps_alert:save(AlertJObj).

-spec decode(string()) -> kz_json:object().
decode(JSON) ->
    try kz_json:encode(JSON) of
        JObj -> kz_json:decode(JObj)
    catch
        'error':_R ->
            io:format("~p~n", [_R]),
            JSON
    end.

-spec req_headers(ne_binary()) -> kz_proplist().
req_headers(Token) ->
    props:filter_undefined(
      [{"Content-Type", "application/json"}
      ,{"X-Auth-Token", kz_util:to_list(Token)}
      ,{"X-Kazoo-Cluster-ID", get_cluster_id()}
      ,{"User-Agent", kz_util:to_list(erlang:node())}
      ]).

-spec get_cluster_id() -> string().
get_cluster_id() ->
    case kapps_config:get_string(?MOD_CONFIG_CAT, <<"cluster_id">>) of
        'undefined' ->
            ClusterId = kz_util:rand_hex_binary(16),
            {'ok', _JObj} = kapps_config:set_default(?MOD_CONFIG_CAT, <<"cluster_id">>, ClusterId),
            kz_util:to_list(ClusterId);
        ClusterId -> ClusterId
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec check_request(kz_json:object()) ->
                           {'ok', kz_json:object()} |
                           jesse_error:error().
check_request(Data) ->
    case get_schema() of
        'undefined' ->
            lager:warning("skiping validation, missing schema"),
            {'ok', Data};
        Schema ->
            case
                jesse:validate_with_schema(Schema
                                          ,Data
                                          ,[{'allowed_errors', 'infinity'}
                                           ,{'schema_loader_fun', fun kz_json_schema:load/1}
                                           ]
                                          )
            of
                {'error', _}=Error -> Error;
                {'ok', JObj} ->
                    {'ok', kz_json_schema:add_defaults(JObj, Schema)}
            end
    end.

-spec get_schema() -> api_object().
get_schema() ->
    case kz_json_schema:load(?SCHEMA) of
        {'ok', SchemaJObj} -> SchemaJObj;
        {'error', _E} ->
            lager:error("failed to find schema ~s: ~p", [?SCHEMA, _E]),
            'undefined'
    end.

-spec handle_validation_error(jesse_error:error_reasons(), api_binary()) -> 'ok'.
handle_validation_error([], AccountId) ->
    lager:error("not sending data to provisioner, data failed to validate in ~s", [AccountId]);
handle_validation_error([{'data_invalid', _, _Reason, _Key, _Value}|Errors], AccountId) ->
    lager:error("failed to validate device: ~p ~p ~p", [_Reason, _Key, _Value]),
    handle_validation_error(Errors, AccountId).
