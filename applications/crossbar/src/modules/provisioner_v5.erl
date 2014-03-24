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

-export([put/2]).
-export([post/2]).
-export([delete/2]).
-export([delete_account/2]).
-export([update_account/3]).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(MOD_CONFIG_CAT, <<"provisioner">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec put(ne_binary(), wh_json:object()) -> 'ok'.
put(JObj, AuthToken) ->
    Routines = [fun(J) -> set_account(J) end
                ,fun(J) -> set_owner(J) end
                ,fun(J) -> device_settings(J) end
               ],
    Data = lists:foldl(fun(F, J) -> F(J) end, JObj, Routines),
    MACAddress = wh_json:get_value(<<"mac_address">>, JObj),
    _ = send_req('devices_put'
                 ,Data
                 ,AuthToken
                 ,wh_json:get_value(<<"pvt_account_id">>, JObj)
                 ,MACAddress),
    send_req('files_post', AuthToken, MACAddress).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec post(ne_binary(), wh_json:object()) -> 'ok'.
post(JObj, AuthToken) ->
    Routines = [fun(J) -> set_account(J) end
                ,fun(J) -> set_owner(J) end
                ,fun(J) -> device_settings(J) end
               ],
    Data = lists:foldl(fun(F, J) -> F(J) end, JObj, Routines),
    MACAddress = wh_json:get_value(<<"mac_address">>, JObj),
    _ = send_req('devices_post'
                 ,Data
                 ,AuthToken
                 ,wh_json:get_value(<<"pvt_account_id">>, JObj)
                 ,MACAddress),
    send_req('files_post', AuthToken, MACAddress).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec delete(ne_binary(), wh_json:object()) -> 'ok'.
delete(JObj, AuthToken) ->
    send_req('devices_delete'
             ,'none'
             ,AuthToken
             ,wh_json:get_value(<<"pvt_account_id">>, JObj)
             ,wh_json:get_value(<<"mac_address">>, JObj)).

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
             ,'none').

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
             ,'none').
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_account(wh_json:object()) -> wh_json:object().
set_account(JObj) ->
    AccountId = wh_json:get_value(<<"pvt_account_id">>, JObj),
    AccountDb = wh_json:get_value(<<"pvt_account_db">>, JObj),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {'ok', Doc} ->
            Realm = wh_json:get_value(<<"realm">>, Doc),
            wh_json:set_value(<<"realm">>, Realm, JObj);
        {'error', _R} ->
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
    Owner = get_owner(OwnerId, AccountId),
    wh_json:merge_recursive(JObj, Owner).


-spec get_owner(api_binary(), ne_binary()) -> wh_json:object().
get_owner('undefined', _) -> wh_json:new();
get_owner(OwnerId, AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, OwnerId) of
        {'ok', Owner} -> Owner;
        {'error', _R} ->
            lager:debug("unable to open user definition ~s/~s: ~p", [AccountDb, OwnerId, _R]),
            wh_json:new()
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec account_settings(ne_binary(), wh_json:object()) -> wh_json:object().
account_settings(AccountId, JObj) ->
    Settings = wh_json:set_values([
        {<<"lines">>, [set_realm(JObj)]}
    ], wh_json:new()),

    wh_json:set_values([
        {<<"provider_id">>, wh_services:find_reseller_id(AccountId)}
        ,{<<"name">>, wh_json:get_value(<<"name">>, JObj)}
        ,{<<"settings">>, Settings}
    ], wh_json:new()).

set_realm(JObj) ->
    wh_json:set_value(
        <<"sip">>
        ,wh_json:set_value(
            <<"sip_server_1">>
            ,wh_json:get_value(<<"realm">>, JObj)
            ,wh_json:new())
        ,wh_json:new()).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec device_settings(wh_json:object()) -> wh_json:object().
device_settings(JObj) ->
    Settings = wh_json:set_values([
        {<<"lines">>, [set_line(JObj)]}
        ,{<<"codecs">>, set_codecs(JObj)}
    ], wh_json:new()),

    wh_json:set_values([
        {<<"brand">>, wh_json:get_value([<<"provision">>, <<"endpoint_brand">>], JObj)}
        ,{<<"family">>, wh_json:get_value([<<"provision">>, <<"endpoint_family">>], JObj)}
        ,{<<"model">>, wh_json:get_value([<<"provision">>, <<"endpoint_model">>], JObj)}
        ,{<<"name">>, wh_json:get_value(<<"name">>, JObj)}
        ,{<<"settings">>, Settings}
    ], wh_json:new()).

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
                    Enabled = wh_json:get_value(<<"enabled">>, JObj, 1),
                    wh_json:set_value(<<"enable">>, Enabled, J)
                end
               ],
    lists:foldl(fun(F, J) -> F(J) end, wh_json:new(), Routines).

-spec set_sip(wh_json:object()) -> wh_json:object().
set_sip(JObj) ->
    Routines = [fun(J) ->
                    Name = wh_json:get_value([<<"sip">>, <<"username">>], JObj),
                    wh_json:set_values([
                        {<<"register_name">>, Name}
                        ,{<<"username">>, Name}
                    ], J)
                end
                ,fun(J) ->
                    Pass = wh_json:get_value([<<"sip">>, <<"password">>], JObj),
                    wh_json:set_value(<<"password">>, Pass, J)
                end
                ,fun(J) ->
                    Pass = wh_json:get_value([<<"sip">>, <<"password">>], JObj),
                    wh_json:set_value(<<"password">>, Pass, J)
                end
                ,fun(J) ->
                    Pass = wh_json:get_value(<<"realm">>, JObj),
                    wh_json:set_value(<<"sip_server_1">>, Pass, J)
                end
               ],
    lists:foldl(fun(F, J) -> F(J) end, wh_json:new(), Routines).

-spec set_advanced(wh_json:object()) -> wh_json:object().
set_advanced(JObj) ->
    Routines = [fun(J) ->
                    Expire = wh_json:get_value([<<"sip">>, <<"expire_seconds">>], JObj, 360),
                    wh_json:set_value(<<"expire">>, Expire, J)
                end
                ,fun(J) ->
                    Srtp = wh_json:get_value([<<"media">>, <<"secure_rtp">>], JObj, 0),
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
    Routines = [fun(J) -> wh_json:set_value(<<"audio">>, set_audio(JObj), J) end
               ],
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
    Data = wh_json:encode(wh_json:set_value(<<"data">>, JObj, wh_json:new())),
    Headers = req_headers(AuthToken),
    HTTPOptions = [],
    UrlString = req_uri('devices', AccountId, MACAddress),
    lager:debug("provisioning via ~s", [UrlString]),
    Resp = ibrowse:send_req(UrlString, Headers, 'put', Data, HTTPOptions),
    handle_resp(Resp);
send_req('devices_post', JObj, AuthToken, AccountId, MACAddress) ->
    Data = wh_json:encode(wh_json:set_value(<<"data">>, JObj, wh_json:new())),
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
    Data = wh_json:encode(wh_json:set_value(<<"data">>, JObj, wh_json:new())),
    Headers = req_headers(AuthToken),
    HTTPOptions = [],
    UrlString = req_uri('accounts', AccountId),
    lager:debug("account update via ~s", [UrlString]),
    Resp = ibrowse:send_req(UrlString, Headers, 'post', Data, HTTPOptions),
    handle_resp(Resp);
send_req(_, _, _, _, _) ->
    'ok'.

handle_resp({'ok', "200", _, Resp}) ->
    lager:debug("provisioning success ~p", [Resp]);
handle_resp({'ok', Code, _, Resp}) ->
    lager:warning("provisioning error ~p. ~p", [Code, Resp]);
handle_resp(_Error) ->
    lager:error("provisioning fatal error ~p", [_Error]).


req_headers(Token) ->
    props:filter_undefined(
        [{"Content-Type", "application/json"}
         ,{"X-Auth-Token", wh_util:to_list(Token)}
         ,{"User-Agent", wh_util:to_list(erlang:node())}
    ]).

req_uri('files') ->
    Url = whapps_config:get_binary(?MOD_CONFIG_CAT, <<"provisioning_url">>),
    Uri = wh_util:uri(Url, [<<"api/files/generate">>]),
    binary:bin_to_list(Uri).

req_uri('accounts', AccountId) ->
    Url = whapps_config:get_binary(?MOD_CONFIG_CAT, <<"provisioning_url">>),
    Uri = wh_util:uri(Url, [<<"api/accounts">>, AccountId]),
    binary:bin_to_list(Uri).

req_uri('devices', AccountId, MACAddress) ->
    Url = whapps_config:get_binary(?MOD_CONFIG_CAT, <<"provisioning_url">>),
    EncodedAddress = binary:replace(MACAddress, <<":">>, <<>>, ['global']),
    Uri = wh_util:uri(Url, [<<"api/devices">>, AccountId, EncodedAddress]),
    binary:bin_to_list(Uri).

