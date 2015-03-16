%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @doc
%%%
%%% Common functions for the provisioner modules
%%%
%%% @end
%%% @contributors
%%%    Peter Defebvre
%%%-------------------------------------------------------------------
-module(provisioner_v5).

-export([put/2]).
-export([post/2]).
-export([delete/2]).
-export([delete_account/2]).
-export([update_account/3]).
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
-spec put(wh_json:object(), ne_binary()) -> 'ok'.
put(JObj, AuthToken) ->
    AccountId = wh_json:get_value(<<"pvt_account_id">>, JObj),
    case check_data(provision_data(JObj)) of
        {'ok', Data} ->
            handle_validation_success(
              'put'
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
-spec post(wh_json:object(), ne_binary()) -> 'ok'.
post(JObj, AuthToken) ->
    AccountId = wh_json:get_value(<<"pvt_account_id">>, JObj),
    case check_data(provision_data(JObj)) of
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
-spec delete(wh_json:object(), ne_binary()) -> 'ok'.
delete(JObj, AuthToken) ->
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
    case ibrowse:send_req(UrlString, Headers, 'get', [], HTTPOptions) of
        {'ok', "200", _RespHeaders, JSON} ->
            wh_json:get_value([<<"data">>, <<"account_id">>], wh_json:decode(JSON));
        _AnythingElse -> 'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec provision_data(wh_json:object()) -> wh_json:object().
provision_data(JObj) ->
    Routines =
        [fun set_owner/1
         ,fun create_provision_settings/1
        ],
    lists:foldl(fun(F, J) -> F(J) end, JObj, Routines).

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
-spec get_owner(api_binary(), ne_binary()) ->
                       {'ok', wh_json:object()} |
                       {'error', _}.
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
-spec account_settings(ne_binary(), wh_json:object()) -> wh_json:object().
account_settings(AccountId, JObj) ->
    Settings = wh_json:from_list([{<<"lines">>, [set_line_realm(JObj)]}]),
    wh_json:from_list(
      [{<<"provider_id">>, wh_services:find_reseller_id(AccountId)}
       ,{<<"name">>, wh_json:get_value(<<"name">>, JObj)}
       ,{<<"settings">>, Settings}
      ]).

-spec set_line_realm(wh_json:object()) -> wh_json:object().
set_line_realm(JObj) ->
    wh_json:set_value(
      <<"sip">>
      ,wh_json:set_value(
         <<"realm">>
         ,wh_json:get_value(<<"realm">>, JObj)
         ,wh_json:new()
        )
      ,wh_json:new()
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec create_provision_settings(wh_json:object()) -> wh_json:object().
create_provision_settings(JObj) ->
    SubSettings =
        case wh_json:get_value(<<"timezone">>, JObj) of
            'undefined' -> 'undefined';
            Timezone ->
                wh_json:set_value([<<"datetime">>
                                  ,<<"time">>
                                  ,<<"timezone">>
                                  ]
                                  ,Timezone
                                  ,wh_json:new()
                                 )
        end,
    Settings =
        wh_json:from_list(
          props:filter_undefined(
            [{<<"lines">>, [set_line(JObj)]}
            ,{<<"codecs">>, [set_codecs(JObj)]}
            ,{<<"settings">>, SubSettings}
            ]
           )
         ),
    wh_json:from_list(
      [{<<"brand">>, wh_json:get_binary_value([<<"provision">>, <<"endpoint_brand">>], JObj, <<>>)}
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
-spec set_line(wh_json:object()) -> wh_json:object().
set_line(JObj) ->
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
-spec send_req(atom(), ne_binary(), ne_binary()) -> 'ok'.
-spec send_req(atom(), wh_json:object() | 'none', ne_binary(), ne_binary(), 'none' | ne_binary()) -> 'ok'.
send_req('files_post', AuthToken, MACAddress) ->
    Addr = binary:replace(MACAddress, <<":">>, <<>>, ['global']),
    JObj =  wh_json:from_list([{<<"mac_address">>, Addr}]),
    Data = wh_json:encode(wh_json:set_value(<<"data">>, JObj, wh_json:new())),
    Headers = req_headers(AuthToken),
    HTTPOptions = [],
    UrlString = req_uri('files'),
    lager:debug("provisioning via ~s", [UrlString]),
    Resp = ibrowse:send_req(UrlString, Headers, 'post', Data, HTTPOptions),
    handle_resp(Resp).

send_req('devices_put', JObj, AuthToken, AccountId, MACAddress) ->
    Data = wh_json:encode(wh_json:from_list([{<<"data">>, JObj}])),
    Headers = req_headers(AuthToken),
    HTTPOptions = [],
    UrlString = req_uri('devices', AccountId, MACAddress),
    lager:debug("provisioning via ~s", [UrlString]),
    Resp = ibrowse:send_req(UrlString, Headers, 'put', Data, HTTPOptions),
    handle_resp(Resp);
send_req('devices_post', JObj, AuthToken, AccountId, MACAddress) ->
    Data = wh_json:encode(
             wh_json:from_list(
               [{<<"data">>, JObj}
               ,{<<"merge">>, 'true'}
               ]
              )
            ),
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
    Data = wh_json:encode(wh_json:from_list([{<<"data">>, JObj}])),
    Headers = req_headers(AuthToken),
    HTTPOptions = [],
    UrlString = req_uri('accounts', AccountId),
    lager:debug("account update via ~s", [UrlString]),
    Resp = ibrowse:send_req(UrlString, Headers, 'post', Data, HTTPOptions),
    handle_resp(Resp).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_resp(ibrowse_ret()) -> 'ok'.
handle_resp({'ok', "200", _, Resp}) ->
    lager:debug("provisioning success ~p", [Resp]);
handle_resp({'ok', Code, _, Resp}) ->
    lager:warning("provisioning error ~p. ~p", [Code, Resp]);
handle_resp(_Error) ->
    lager:error("provisioning fatal error ~p", [_Error]).

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
-spec req_uri('files') -> iolist().
-spec req_uri('accounts' | 'devices', ne_binary()) -> iolist().
-spec req_uri('devices', ne_binary(), ne_binary()) -> iolist().

req_uri('files') ->
    provisioning_uri([<<"files/generate">>]).

req_uri('accounts', AccountId) ->
    provisioning_uri([<<"accounts">>, AccountId]);
req_uri('devices', MacAddress) ->
    provisioning_uri([<<"devices">>, MacAddress]).

req_uri('devices', AccountId, MACAddress) ->
    EncodedAddress = binary:replace(MACAddress, <<":">>, <<>>, ['global']),
    provisioning_uri([<<"devices">>, AccountId, EncodedAddress]).

%% @private
-spec provisioning_uri(iolist()) -> iolist().
provisioning_uri(ExplodedPath) ->
    Url = whapps_config:get_binary(?MOD_CONFIG_CAT, <<"provisioning_url">>),
    Uri = wh_util:uri(Url, ExplodedPath),
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
             ,MACAddress),
    send_req('files_post', Token, MACAddress);
handle_validation_success('post', Data, Token, MACAddress, AccountId) ->
    lager:debug("post data validated, sending to provisioner"),
    _ = send_req('devices_post'
             ,Data
             ,Token
             ,AccountId
             ,MACAddress),
    send_req('files_post', Token, MACAddress).

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
