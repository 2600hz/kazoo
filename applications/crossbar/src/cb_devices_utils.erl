%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(cb_devices_utils).

-export([is_ip_unique/2]).

-include("./crossbar.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check if the device sip ip is unique
%% @end
%%--------------------------------------------------------------------
-spec is_ip_unique(ne_binary(), ne_binary()) -> boolean().
is_ip_unique(IP, DeviceId) ->
    is_ip_acl_unique(IP, DeviceId)
        andalso is_ip_sip_auth_unique(IP, DeviceId).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_ip_acl_unique(ne_binary(), ne_binary()) -> boolean().
is_ip_acl_unique(IP, DeviceId) ->
    lists:all(
      fun(JObj) -> is_ip_unique(JObj, IP, DeviceId) end
      ,get_all_acl_ips()
     ).

-spec is_ip_unique(wh_json:object(), ne_binary(), ne_binary()) -> boolean().
is_ip_unique(JObj, IP, DeviceId) ->
    CIDR = wh_json:get_value(<<"ip">>, JObj),
    AuthorizingId = wh_json:get_value(<<"authorizing_id">>, JObj),
    case AuthorizingId =:= DeviceId of
        'true' -> 'true';
        'false' ->
            not (wh_network_utils:verify_cidr(IP, CIDR))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_ip_sip_auth_unique(ne_binary(), ne_binary()) -> boolean().
is_ip_sip_auth_unique(IP, DeviceId) ->
    ViewOptions = [{<<"key">>, IP}],
    case couch_mgr:get_results(?WH_SIP_DB, <<"credentials/lookup_by_ip">>, ViewOptions) of
        {'ok', []} -> 'true';
        {'ok', [JObj]} -> wh_doc:id(JObj) =:= DeviceId;
        {'error', 'not_found'} -> 'true';
        _ -> 'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_all_acl_ips() -> wh_json:objects().
get_all_acl_ips() ->
    Req = [{<<"Category">>, <<"ecallmgr">>}
           ,{<<"Key">>, <<"acls">>}
           ,{<<"Node">>, <<"all">>}
           ,{<<"Default">>, wh_json:new()}
           ,{<<"Msg-ID">>, wh_util:rand_hex_binary(16)}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    Resp = whapps_util:amqp_pool_request(
             props:filter_undefined(Req)
             ,fun wapi_sysconf:publish_get_req/1
             ,fun wapi_sysconf:get_resp_v/1
            ),
    case Resp of
        {'error', _} -> [];
        {'ok', JObj} ->
            extract_all_ips(wh_json:get_value(<<"Value">>, JObj, wh_json:new()))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_all_ips(wh_json:object()) -> wh_json:objects().
extract_all_ips(JObj) ->
    wh_json:foldl(fun extract_ip/3, [], JObj).

-spec extract_ip(wh_json:key(), wh_json:object(), wh_json:objects()) ->
                        wh_json:objects().
extract_ip(_Key, Value, Acc) ->
    case wh_json:get_value(<<"cidr">>, Value) of
        'undefined' -> Acc;
        CIDR ->
            [wh_json:from_list([{<<"ip">>, CIDR}
                                ,{<<"authorizing_id">>, wh_json:get_value(<<"authorizing_id">>, Value)}
                               ])
             |Acc
            ]
    end.
