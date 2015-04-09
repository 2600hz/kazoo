%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
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

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(MOD_CONFIG_CAT, <<"provisioner">>).
-define(SCHEMA, <<"provisioner_v5">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_device(wh_json:object(), ne_binary()) -> 'ok'.
update_device(JObj, AuthToken) ->
    AccountId = wh_json:get_value(<<"pvt_account_id">>, JObj),
    Request = device_settings(set_owner(JObj)),
    case check_request(Request) of
        {'ok', Data} ->
            _ = update_account(AccountId, AuthToken),
            send_req('devices_post'
                     ,Data
                     ,AuthToken
                     ,AccountId
                     ,wh_json:get_value(<<"mac_address">>, JObj)
                    );
        {'error', Errors} ->
            handle_validation_error(Errors, AccountId)
    end.

-spec delete_device(wh_json:object(), ne_binary()) -> 'ok'.
delete_device(JObj, AuthToken) ->
    send_req('devices_delete'
             ,'undefined'
             ,AuthToken
             ,wh_json:get_value(<<"pvt_account_id">>, JObj)
             ,wh_json:get_value(<<"mac_address">>, JObj)
            ).

-spec device_settings(wh_json:object()) -> wh_json:object().
device_settings(JObj) ->
    wh_json:from_list(
      [{<<"brand">>, get_brand(JObj)}
      ,{<<"family">>, get_family(JObj)}
      ,{<<"model">>, get_model(JObj)}
      ,{<<"name">>, wh_json:get_value(<<"name">>, JObj)}
      ,{<<"settings">>, settings(JObj)}
      ]
     ).

-spec get_brand(wh_json:object()) -> wh_json:object().
get_brand(JObj) ->
    wh_json:get_binary_value([<<"provision">>, <<"endpoint_brand">>], JObj, <<>>).

-spec get_family(wh_json:object()) -> wh_json:object().
get_family(JObj) ->
    case wh_json:get_binary_value([<<"provision">>, <<"endpoint_family">>], JObj, <<>>) of
        %% Temporary hack to fix family names till a script can clean the database
        <<"f", Family/binary>> -> Family;
        Family -> Family
    end.

-spec get_model(wh_json:object()) -> wh_json:object().
get_model(JObj) ->
    Family = wh_json:get_binary_value([<<"provision">>, <<"endpoint_model">>], JObj, <<>>),
    case wh_util:to_lower_binary(Family) of
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
-spec update_account(ne_binary(), wh_json:object(), ne_binary()) -> 'ok'.
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
    AccountId = wh_util:format_account_id(Account, 'raw'),
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {'ok', JObj} ->
            update_account(AccountId, JObj, AuthToken);
        {'error', _R} ->
            lager:debug("unable to fetch account ~s: ~p", [AccountId, _R])
    end.

-spec account_settings(wh_json:object()) -> wh_json:object().
account_settings(JObj) ->
    wh_json:from_list(
      [{<<"settings">>, settings(JObj)}]
     ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_user(ne_binary(), wh_json:object(), ne_binary()) -> 'ok'.
update_user(AccountId, JObj, AuthToken) ->
    case wh_json:get_value(<<"pvt_type">>, JObj) of
        <<"user">> ->
            save_user(AccountId, JObj, AuthToken);
        _ -> 'ok' %% Gets rid of VMbox
    end.

-spec save_user(ne_binary(), wh_json:object(), ne_binary()) -> 'ok'.
save_user(AccountId, JObj, AuthToken) ->
    _ = update_account(AccountId, AuthToken),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    OwnerId = wh_json:get_value(<<"id">>, JObj),
    Devices = get_devices_by_owner(AccountDb, OwnerId),
    Settings = settings(JObj),
    lists:foreach(
      fun (Device) ->
              Request = wh_json:from_list(
                          [{<<"brand">>, get_brand(Device)}
                          ,{<<"family">>, get_family(Device)}
                          ,{<<"model">>, get_model(Device)}
                          ,{<<"name">>, wh_json:get_value(<<"name">>, Device)}
                          ,{<<"settings">>, Settings}
                          ]
                         ),
              catch save_device(AccountId, Device, Request, AuthToken)
      end, Devices).

-spec get_devices_by_owner(ne_binary(), ne_binary()) -> ne_binaries().
get_devices_by_owner(AccountDb, OwnerId) ->
    ViewOptions = [{'key', [OwnerId, <<"device">>]},
                   'include_docs'
                  ],
    case couch_mgr:get_results(AccountDb, <<"cf_attributes/owned">>, ViewOptions) of
        {'ok', JObjs} -> [wh_json:get_value(<<"doc">>, JObj) || JObj <- JObjs];
        {'error', _R} ->
            lager:warning("unable to find documents owned by ~s: ~p", [OwnerId, _R]),
            []
    end.

-spec save_device(ne_binary(), wh_json:object(), wh_json:object(), ne_binary()) -> 'ok'.
save_device(AccountId, Device, Request, AuthToken) ->
    case wh_json:get_ne_value(<<"mac_address">>, Device) of
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
    HTTPOptions = [],
    UrlString = req_uri('devices', MacAddress),
    lager:debug("pre-provisioning via ~s", [UrlString]),
    Resp = ibrowse:send_req(UrlString, Headers, 'get', [], HTTPOptions),
    case Resp of
        {'ok', "200", _RespHeaders, JSONStr} ->
            JObj = wh_json:decode(JSONStr),
            wh_json:get_value([<<"data">>, <<"account_id">>], JObj);
        _AnythingElse -> 'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_owner(wh_json:object()) -> wh_json:object().
set_owner(JObj) ->
    AccountId = wh_json:get_value(<<"pvt_account_id">>, JObj),
    OwnerId = wh_json:get_ne_value(<<"owner_id">>, JObj),
    case get_owner(OwnerId, AccountId) of
        {'ok', Doc} -> wh_json:merge_recursive(Doc, JObj);
        {'error', _R} -> JObj
    end.

-spec get_owner(api_binary(), ne_binary()) ->
                       {'ok', wh_json:object()} |
                       {'error', any()}.
get_owner('undefined', _) -> {'error', 'undefined'};
get_owner(OwnerId, AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    couch_mgr:open_cache_doc(AccountDb, OwnerId).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec settings(wh_json:object()) -> wh_json:object().
settings(JObj) ->
    Props = props:filter_empty(
              [{<<"lines">>, settings_lines(JObj)}
               ,{<<"codecs">>, settings_codecs(JObj)}
               ,{<<"datetime">>, settings_datetime(JObj)}
              ]
             ),
    wh_json:from_list(Props).

-spec settings_lines(wh_json:object()) -> wh_json:object().
settings_lines(JObj) ->
    case props:filter_empty(
              [{<<"basic">>, settings_basic(JObj)},
               {<<"sip">>, settings_sip(JObj)},
               {<<"advanced">>, settings_advanced(JObj)}
              ]
             )
    of
        [] -> wh_json:new();
        Props ->
            wh_json:from_list([{<<"0">>, wh_json:from_list(Props)}])
    end.

-spec settings_basic(wh_json:object()) -> wh_json:object().
settings_basic(JObj) ->
    Enabled = case wh_json:get_ne_value(<<"enabled">>, JObj) of
                  'undefined' -> 'undefined';
                  Else -> wh_util:is_true(Else)
              end,
    Props = props:filter_undefined(
              [{<<"display_name">>, wh_json:get_ne_value(<<"name">>, JObj)}
              ,{<<"enable">>, Enabled}
              ]
             ),
    wh_json:from_list(Props).

-spec settings_sip(wh_json:object()) -> wh_json:object().
settings_sip(JObj) ->
    Realm = wh_json:get_first_defined(
              [
               [<<"sip">>, <<"realm">>],
               <<"realm">>
              ], JObj
             ),
    Props = props:filter_undefined(
              [{<<"username">>, wh_json:get_value([<<"sip">>, <<"username">>], JObj)},
               {<<"password">>, wh_json:get_value([<<"sip">>, <<"password">>], JObj)},
               {<<"realm">>, Realm}
              ]
             ),
    wh_json:from_list(Props).

-spec settings_advanced(wh_json:object()) -> wh_json:object().
settings_advanced(JObj) ->
    SRTP = case wh_json:get_ne_value([<<"media">>, <<"secure_rtp">>], JObj) of
               'undefined' -> 'undefined';
               _Else -> 'true'
           end,
    Props = props:filter_undefined(
              [{<<"expire">>, wh_json:get_integer_value([<<"sip">>, <<"expire_seconds">>], JObj)},
               {<<"srtp">>, SRTP}
              ]
             ),
    wh_json:from_list(Props).

-spec settings_datetime(wh_json:object()) -> wh_json:object().
settings_datetime(JObj) ->
    Props = props:filter_empty(
              [{<<"time">>, settings_time(JObj)}]
             ),
    wh_json:from_list(Props).

-spec settings_time(wh_json:object()) -> wh_json:object().
settings_time(JObj) ->
    Props = props:filter_undefined(
              [{<<"timezone">>, wh_json:get_value(<<"timezone">>, JObj)}]
             ),
    wh_json:from_list(Props).

-spec settings_codecs(wh_json:object()) -> wh_json:object().
settings_codecs(JObj) ->
    case props:filter_empty(
              [{<<"audio">>, settings_audio(JObj)}]
             )
    of
        [] -> wh_json:new();
        Props ->
            wh_json:from_list([{<<"0">>, wh_json:from_list(Props)}])
    end.

-spec settings_audio(wh_json:object()) -> wh_json:object().
settings_audio(JObj) ->
    Codecs = wh_json:get_value([<<"media">>, <<"audio">>, <<"codecs">>], JObj, []),
    Keys = [<<"primary_codec">>
            ,<<"secondary_codec">>
            ,<<"tertiary_codec">>
            ,<<"quaternary_codec">>
           ],
    settings_audio(Codecs, Keys, wh_json:new()).

-spec settings_audio(ne_binaries(), ne_binaries(), wh_json:object()) -> wh_json:object().
settings_audio([], _, JObj) -> JObj;
settings_audio([Codec|Codecs], [Key|Keys], JObj) ->
    settings_audio(Codecs, Keys, wh_json:set_value(Key, Codec, JObj)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Send provisioning request
%% @end
%%--------------------------------------------------------------------
-spec send_req(atom(), wh_json:object() | 'undefined', ne_binary(), ne_binary(), api_binary()) -> 'ok'.

send_req('devices_post', JObj, AuthToken, AccountId, MACAddress) ->
    Data = wh_json:encode(device_payload(JObj)),
    Headers = req_headers(AuthToken),
    HTTPOptions = [],
    UrlString = req_uri('devices', AccountId, MACAddress),
    lager:debug("provisioning via ~s: ~s", [UrlString, Data]),
    Resp = ibrowse:send_req(UrlString, Headers, 'post', Data, HTTPOptions),
    handle_resp(Resp);
send_req('devices_delete', _, AuthToken, AccountId, MACAddress) ->
    Headers = req_headers(AuthToken),
    HTTPOptions = [],
    UrlString = req_uri('devices', AccountId, MACAddress),
    lager:debug("unprovisioning via ~s", [UrlString]),
    Resp = ibrowse:send_req(UrlString, Headers, 'delete', [], HTTPOptions),
    handle_resp(Resp);
send_req('accounts_delete', _, AuthToken, AccountId, _) ->
    Headers = req_headers(AuthToken),
    HTTPOptions = [],
    UrlString = req_uri('accounts', AccountId),
    lager:debug("accounts delete via ~s", [UrlString]),
    Resp = ibrowse:send_req(UrlString, Headers, 'delete', [], HTTPOptions),
    handle_resp(Resp);
send_req('accounts_update', JObj, AuthToken, AccountId, _) ->
    Data = wh_json:encode(account_payload(JObj, AccountId)),
    Headers = req_headers(AuthToken),
    HTTPOptions = [],
    UrlString = req_uri('accounts', AccountId),
    lager:debug("account update via ~s: ~s", [UrlString, Data]),
    Resp = ibrowse:send_req(UrlString, Headers, 'post', Data, HTTPOptions),
    handle_resp(Resp).

-spec req_uri('accounts' | 'devices', ne_binary()) -> iolist().
req_uri('accounts', AccountId) ->
    provisioning_uri([<<"accounts">>, AccountId]);
req_uri('devices', MacAddress) ->
    provisioning_uri([<<"devices">>, MacAddress]).

-spec req_uri('devices', ne_binary(), ne_binary()) -> ne_binary().
req_uri('devices', AccountId, MACAddress) ->
    EncodedAddress = binary:replace(MACAddress, <<":">>, <<>>, ['global']),
    provisioning_uri([<<"devices">>, AccountId, EncodedAddress]).

-spec provisioning_uri(iolist()) -> iolist().
provisioning_uri(ExplodedPath) ->
    Url = whapps_config:get_binary(?MOD_CONFIG_CAT, <<"provisioning_url">>),
    Uri = wh_util:uri(Url, ExplodedPath),
    binary:bin_to_list(Uri).

-spec account_payload(wh_json:object(), ne_binary()) -> wh_json:object().
account_payload(JObj, AccountId) ->
    ResellerId = wh_services:find_reseller_id(AccountId),
    wh_json:from_list(
      [{<<"create_if_missing">>, 'true'}
      ,{<<"reseller_id">>, ResellerId}
      ,{<<"merge">>, 'true'}
      ,{<<"data">>, JObj}
      ]
     ).

-spec device_payload(wh_json:object()) -> wh_json:object().
device_payload(JObj) ->
    wh_json:from_list(
      [{<<"create_if_missing">>, 'true'}
      ,{<<"generate">>, 'true'}
      ,{<<"merge">>, 'true'}
      ,{<<"data">>, JObj}
      ]
     ).

-spec handle_resp(any()) -> 'ok'.
handle_resp({'ok', "200", _, Resp}) ->
    lager:debug("provisioning success ~s", [decode(Resp)]);
handle_resp({'ok', Code, _, Resp}) ->
    lager:warning("provisioning error ~p. ~s", [Code, decode(Resp)]);
handle_resp(_Error) ->
    lager:error("provisioning fatal error ~p", [_Error]).

-spec decode(string()) -> ne_binary().
decode(JSON) ->
    try wh_json:encode(JSON) of
        JObj -> wh_json:decode(JObj)
    catch
        'error':_R ->
            io:format("~p~n", [_R]),
            JSON
    end.

-spec req_headers(ne_binary()) -> wh_proplist().
req_headers(Token) ->
    props:filter_undefined(
        [{"Content-Type", "application/json"}
         ,{"X-Auth-Token", wh_util:to_list(Token)}
         ,{"User-Agent", wh_util:to_list(erlang:node())}
        ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec check_request(wh_json:object()) ->
                           {'ok', wh_json:object()} |
                           jesse_error:error().
check_request(Data) ->
    case get_schema() of
        'undefined' ->
            lager:warning("skiping validation, missing schema"),
            {'ok', Data};
        Schema ->
            case
                jesse:validate_with_schema(
                    Schema
                    ,Data
                    ,[{'allowed_errors', 'infinity'}
                      ,{'schema_loader_fun', fun wh_json_schema:load/1}
                     ]
                )
            of
                {'error', _}=Error -> Error;
                {'ok', JObj} ->
                    {'ok', wh_json_schema:add_defaults(JObj, Schema)}
            end
    end.

-spec get_schema() -> api_object().
get_schema() ->
    case wh_json_schema:load(?SCHEMA) of
        {'ok', SchemaJObj} -> SchemaJObj;
        {'error', _E} ->
            lager:debug("failed to find schema ~s: ~p", [?SCHEMA, _E]),
            'undefined'
    end.

-spec handle_validation_error(jesse_error:error_reasons(), api_binary()) -> 'ok'.
handle_validation_error([], AccountId) ->
    lager:error("not sending data to provisioner, data failed to validate in ~s", [AccountId]);
handle_validation_error([{'data_invalid', _, _Reason, _Key, _Value}|Errors], AccountId) ->
    lager:error("failed to validate device: ~p ~p ~p", [_Reason, _Key, _Value]),
    handle_validation_error(Errors, AccountId).
