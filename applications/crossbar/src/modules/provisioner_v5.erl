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
    Routines =
        [fun set_realm/1
         ,fun set_owner/1
         ,fun set_timezone/1
         ,fun device_settings/1
        ],
    Data = lists:foldl(fun(F, J) -> F(J) end, JObj, Routines),
    case check_data(Data) of
        {'ok', Data} ->
            handle_validation_success(
              'post'
              ,Data
              ,AuthToken
              ,wh_json:get_value(<<"mac_address">>, JObj)
              ,AccountId
             );
        {'error', Errors} ->
            handle_validation_error(Errors, AccountId)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec delete_device(wh_json:object(), ne_binary()) -> 'ok'.
delete_device(JObj, AuthToken) ->
    send_req('devices_delete'
             ,'none'
             ,AuthToken
             ,wh_json:get_value(<<"pvt_account_id">>, JObj)
             ,wh_json:get_value(<<"mac_address">>, JObj)
            ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec delete_account(ne_binary(), ne_binary()) -> 'ok'.
delete_account(AccountId, AuthToken) ->
    send_req('accounts_delete'
             ,'none'
             ,AuthToken
             ,AccountId
             ,'none'
            ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_account(ne_binary(), wh_json:object(), ne_binary()) -> 'ok'.
update_account(AccountId, JObj, AuthToken) ->
    send_req('accounts_update'
             ,account_settings(AccountId, JObj)
             ,AuthToken
             ,AccountId
             ,'none'
            ).

%% @public
-spec update_user(ne_binary(), wh_json:object(), ne_binary()) -> 'ok'.
update_user(AccountId, JObj, AuthToken) ->
    case wh_json:get_value(<<"pvt_type">>, JObj) of
        <<"user">> ->
            save_user(AccountId, JObj, AuthToken);
        _ -> ok %% Gets rid of VMbox
    end.

-spec save_user(ne_binary(), wh_json:object(), ne_binary()) -> 'ok'.
save_user(AccountId, JObj, AuthToken) ->
    update_account(AccountId, JObj, AuthToken),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    OwnerId = wh_json:get_value(<<"id">>, JObj),
    Devices = get_devices_by_owner(AccountDb, OwnerId),
    TZ      = wh_json:get_value(<<"timezone">>, JObj),
    lists:foreach(
      fun (DeviceId) ->
              case cf_endpoint:get(DeviceId, AccountDb) of
                  {'error', _E} ->
                      lager:debug("no endpoint for device ~s: ~p", [DeviceId,_E]);
                  {'ok', Endpoint} ->
                      catch save_device(TZ, Endpoint, AuthToken)
              end
      end, Devices).

-spec get_devices_by_owner(ne_binary(), ne_binary()) -> ne_binaries().
get_devices_by_owner(AccountDb, OwnerId) ->
    ViewOptions = [{'key', [OwnerId, <<"device">>]}],
    case couch_mgr:get_results(AccountDb, <<"cf_attributes/owned">>, ViewOptions) of
        {'ok', JObjs} -> [wh_json:get_value(<<"value">>, JObj) || JObj <- JObjs];
        {'error', _R} ->
            lager:warning("unable to find documents owned by ~s: ~p", [OwnerId, _R]),
            []
    end.

-spec save_device(api_binary(), wh_json:object(), ne_binary()) -> 'ok'.
save_device(TZ, Endpoint, AuthToken) ->
    MaybeSet =
        wh_json:from_list(
          props:filter_undefined([{<<"timezone">>, TZ}])),
    Obj = wh_json:merge_jobjs(MaybeSet, Endpoint),
    update_device(Obj, AuthToken).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_realm(wh_json:object()) -> wh_json:object().
set_realm(JObj) ->
    case get_account(JObj) of
        {'ok', Doc} ->
            Realm = wh_json:get_value(<<"realm">>, Doc),
            wh_json:set_value(<<"realm">>, Realm, JObj);
        {'error', _R} ->
            AccountId = wh_json:get_value(<<"pvt_account_id">>, JObj),
            lager:warning("failed to get account definition for ~s: ~p", [AccountId, _R]),
            JObj
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_timezone(wh_json:object()) -> wh_json:object().
set_timezone(JObj) ->
    case wh_json:get_value(<<"timezone">>, JObj) of
        'undefined' -> maybe_set_account_timezone(JObj);
        TZ -> wh_json:set_value(<<"timezone">>, TZ, JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_set_account_timezone(wh_json:object()) -> wh_json:object().
maybe_set_account_timezone(JObj) ->
    case get_account(JObj) of
        {'ok', Doc} ->
            case wh_json:get_value(<<"timezone">>, Doc) of
                'undefined' -> JObj;
                TZ -> wh_json:set_value(<<"timezone">>, TZ, JObj)
            end;
        {'error', _R} -> JObj
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
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
-spec get_account(wh_json:object()) ->
                         {'ok', wh_json:object()} |
                         {'error', any()}.
get_account(JObj) ->
    AccountId = wh_json:get_value(<<"pvt_account_id">>, JObj),
    AccountDb = wh_json:get_value(<<"pvt_account_db">>, JObj),
    couch_mgr:open_cache_doc(AccountDb, AccountId).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec account_settings(ne_binary(), wh_json:object()) -> wh_json:object().
account_settings(AccountId, JObj) ->
    KeyRealm = [<<"lines">>, <<"sip">>, <<"realm">>],
    KeyTZ    = [<<"datetime">>, <<"time">>, <<"timezone">>],
    Setters =
        [ fun (J) ->
                  case wh_json:get_value(<<"realm">>, JObj) of
                      'undefined' -> J;
                      Realm -> wh_json:set_value(KeyRealm, Realm, J)
                  end
          end
        , fun (J) ->
                  case wh_json:get_value(<<"timezone">>, JObj) of
                      'undefined' -> J;
                      TZ -> wh_json:set_value(KeyTZ, TZ, J)
                  end
          end
        ],
    Settings = lists:foldl(fun (F,J) -> F(J) end, wh_json:new(), Setters),
    wh_json:from_list(
      [{<<"provider_id">>, wh_services:find_reseller_id(AccountId)}
       ,{<<"settings">>, Settings}
      ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec device_settings(wh_json:object()) -> wh_json:object().
device_settings(JObj) ->
    KeyTZ = [<<"datetime">>, <<"time">>, <<"timezone">>],
    Setters = [ fun (J) -> wh_json:set_value(<<"lines">>, set_lines(JObj), J) end
              , fun (J) -> wh_json:set_value(<<"codecs">>, [set_codecs(JObj)], J) end
              , fun (J) ->
                        case wh_json:get_value(<<"timezone">>, JObj) of
                            'undefined' -> J;
                            Timezone -> wh_json:set_value(KeyTZ, Timezone, J)
                        end
                end
              ],
    Settings = lists:foldl(fun (F,J) -> F(J) end, wh_json:new(), Setters),
    wh_json:from_list(
      [ {<<"brand">>, wh_json:get_binary_value([<<"provision">>, <<"endpoint_brand">>], JObj, <<>>)}
       ,{<<"family">>, wh_json:get_binary_value([<<"provision">>, <<"endpoint_family">>], JObj, <<>>)}
       ,{<<"model">>, wh_json:get_binary_value([<<"provision">>, <<"endpoint_model">>], JObj, <<>>)}
       ,{<<"name">>, wh_json:get_value(<<"name">>, JObj)}
       ,{<<"settings">>, Settings}
      ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_lines(wh_json:object()) -> wh_json:object().
set_lines(JObj) ->
    Routines = [fun(J) -> wh_json:set_value(<<"basic">>, set_basic(JObj), J) end
                ,fun(J) -> wh_json:set_value(<<"sip">>, set_sip(JObj), J) end
                ,fun(J) -> wh_json:set_value(<<"advanced">>, set_advanced(JObj), J) end
               ],
    lists:foldl(fun(F, J) -> F(J) end, wh_json:new(), Routines).

-spec set_basic(wh_json:object()) -> wh_json:object().
set_basic(JObj) ->
    Routines = [fun(J) ->
                    Name = wh_json:get_value(<<"name">>, JObj),
                    wh_json:set_value(<<"display_name">>, Name, J)
                end
                ,fun(J) ->
                    Enabled = wh_json:get_value(<<"enabled">>, JObj, 'true'),
                    wh_json:set_value(<<"enable">>, Enabled, J)
                end
               ],
    lists:foldl(fun(F, J) -> F(J) end, wh_json:new(), Routines).

-spec set_sip(wh_json:object()) -> wh_json:object().
set_sip(JObj) ->
    Routines = [fun(J) ->
                    Name = wh_json:get_value([<<"sip">>, <<"username">>], JObj),
                    wh_json:set_value(<<"username">>, Name, J)
                end
                ,fun(J) ->
                    Pass = wh_json:get_value([<<"sip">>, <<"password">>], JObj),
                    wh_json:set_value(<<"password">>, Pass, J)
                end
                ,fun(J) ->
                    Pass = wh_json:get_value(<<"realm">>, JObj),
                    wh_json:set_value(<<"realm">>, Pass, J)
                end
               ],
    lists:foldl(fun(F, J) -> F(J) end, wh_json:new(), Routines).

-spec set_advanced(wh_json:object()) -> wh_json:object().
set_advanced(JObj) ->
    Routines = [fun(J) ->
                    Expire = wh_json:get_integer_value([<<"sip">>, <<"expire_seconds">>], JObj, 360),
                    wh_json:set_value(<<"expire">>, Expire, J)
                end
                ,fun(J) ->
                    Srtp = wh_json:get_value([<<"media">>, <<"secure_rtp">>], JObj, 'false'),
                    wh_json:set_value(<<"srtp">>, Srtp, J)
                end
               ],
    lists:foldl(fun(F, J) -> F(J) end, wh_json:new(), Routines).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_codecs(wh_json:object()) -> wh_json:object().
set_codecs(JObj) ->
    Routines = [fun(J) -> wh_json:set_value(<<"audio">>, set_audio(JObj), J) end],
    lists:foldl(fun(F, J) -> F(J) end, wh_json:new(), Routines).

-spec set_audio(wh_json:object()) -> wh_json:object().
set_audio(JObj) ->
    Codecs = wh_json:get_value([<<"media">>, <<"audio">>, <<"codecs">>], JObj),
    Keys = [<<"primary_codec">>
            ,<<"secondary_codec">>
            ,<<"tertiary_codec">>
            ,<<"quaternary_codec">>
           ],
    set_audio(Codecs, Keys, wh_json:new()).

-spec set_audio(ne_binaries(), ne_binaries(), wh_json:object()) -> wh_json:object().
set_audio([], _, JObj) -> JObj;
set_audio([Codec|Codecs], [Key|Keys], JObj) ->
    set_audio(Codecs, Keys, wh_json:set_value(Key, Codec, JObj)).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Send provisioning request
%% @end
%%--------------------------------------------------------------------
-spec send_req(atom(), wh_json:object() | 'none', ne_binary(), ne_binary(), 'none' | ne_binary()) -> 'ok'.

send_req('devices_post', JObj, AuthToken, AccountId, MACAddress) ->
    Data = wh_json:encode(device_payload(JObj)),
    Headers = req_headers(AuthToken),
    HTTPOptions = [],
    UrlString = req_uri('devices', AccountId, MACAddress),
    lager:debug("provisioning via ~s", [UrlString]),
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
    lager:debug("account update via ~s", [UrlString]),
    Resp = ibrowse:send_req(UrlString, Headers, 'post', Data, HTTPOptions),
    handle_resp(Resp).

-spec account_payload(wh_json:object(), ne_binary()) -> wh_json:object().
account_payload(JObj, AccountId) ->
    ResellerId = wh_services:find_reseller_id(AccountId),
    wh_json:from_list([ {<<"create_if_missing">>, 'true'}
                      , {<<"reseller_id">>, ResellerId}
                      , {<<"merge">>, 'true'}
                      , {<<"data">>, JObj}
                      ]).

-spec device_payload(wh_json:object()) -> wh_json:object().
device_payload(JObj0) ->
    KeyRealm = [<<"settings">>, <<"lines">>, <<"sip">>, <<"realm">>],
    JObj = wh_json:delete_key(KeyRealm, JObj0),
    wh_json:from_list([ {<<"create_if_missing">>, 'true'}
                      , {<<"generate">>, 'true'}
                      , {<<"merge">>, 'true'}
                      , {<<"data">>, JObj}
                      ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_resp(any()) -> 'ok'.
handle_resp({'ok', "200", _, Resp}) ->
    lager:debug("provisioning success ~s", [decode(Resp)]);
handle_resp({'ok', Code, _, Resp}) ->
    lager:warning("provisioning error ~p. ~s", [Code, decode(Resp)]);
handle_resp(_Error) ->
    lager:error("provisioning fatal error ~p", [_Error]).

-spec decode(string()) -> ne_binary().
decode(JSON) ->
    wh_json:encode(
      wh_json:decode(JSON)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
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
req_uri('accounts', AccountId) ->
    Url = whapps_config:get_binary(?MOD_CONFIG_CAT, <<"provisioning_url">>),
    Uri = wh_util:uri(Url, [<<"accounts">>, AccountId]),
    binary:bin_to_list(Uri).
req_uri('devices', AccountId, MACAddress) ->
    Url = whapps_config:get_binary(?MOD_CONFIG_CAT, <<"provisioning_url">>),
    EncodedAddress = binary:replace(MACAddress, <<":">>, <<>>, ['global']),
    Uri = wh_util:uri(Url, [<<"devices">>, AccountId, EncodedAddress]),
    binary:bin_to_list(Uri).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec check_data(wh_json:object()) ->
                        {'ok', wh_json:object()} |
                        jesse_error:error().
check_data(Data) ->
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_schema() -> api_object().
get_schema() ->
    case wh_json_schema:load(?SCHEMA) of
        {'ok', SchemaJObj} -> SchemaJObj;
        {'error', _E} ->
            lager:debug("failed to find schema ~s: ~p", [?SCHEMA, _E]),
            'undefined'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_validation_success('put' | 'post', wh_json:object(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
handle_validation_success('put', Data, Token, MACAddress, AccountId) ->
    lager:debug("put data validated, sending to provisioner"),
    _ = send_req('devices_put'
             ,Data
             ,Token
             ,AccountId
             ,MACAddress);
handle_validation_success('post', Data, Token, MACAddress, AccountId) ->
    lager:debug("post data validated, sending to provisioner"),
    _ = send_req('devices_post'
             ,Data
             ,Token
             ,AccountId
             ,MACAddress).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_validation_error(jesse_error:error_reasons(), api_binary()) -> 'ok'.
handle_validation_error([], AccountId) ->
    lager:error("not sending data to provisioner, data failed to validate in ~s", [AccountId]);
handle_validation_error([{'data_invalid', _, _Reason, _Key, _Value}|Errors], AccountId) ->
    lager:error("failed to validate device: ~p ~p ~p", [_Reason, _Key, _Value]),
    handle_validation_error(Errors, AccountId).
