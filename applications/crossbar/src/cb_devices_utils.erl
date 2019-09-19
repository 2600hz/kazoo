%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_devices_utils).

-export([is_ip_unique/2]).

-include("crossbar.hrl").

-define(AUTHZ_ID, <<"authorizing_id">>).

%%------------------------------------------------------------------------------
%% @doc Check if the device SIP IP is unique.
%% @end
%%------------------------------------------------------------------------------
-spec is_ip_unique(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_ip_unique(IP, DeviceId) ->
    is_ip_acl_unique(IP, DeviceId)
        andalso is_ip_sip_auth_unique(IP, DeviceId).


-spec is_ip_acl_unique(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_ip_acl_unique(IP, DeviceId) ->
    lists:all(fun(JObj) -> is_ip_unique(JObj, IP, DeviceId) end
             ,get_all_acl_ips()
             ).

-spec is_ip_unique(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_ip_unique(JObj, IP, DeviceId) ->
    case kz_json:get_value(?AUTHZ_ID, JObj) of
        DeviceId -> 'true';
        _AuthorizingId ->
            not (kz_network_utils:verify_cidr(IP, kz_json:get_value(<<"ip">>, JObj)))
    end.


-spec is_ip_sip_auth_unique(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_ip_sip_auth_unique(IP, DeviceId) ->
    case kapps_util:get_ccvs_by_ip(IP) of
        {'ok', CCVs} -> props:get_value(<<"Authorizing-ID">>, CCVs) =:= DeviceId;
        {'error', 'not_found'} -> 'true';
        {'error', _Reason} -> 'false'
    end.


-spec get_all_acl_ips() -> kz_json:objects().
get_all_acl_ips() ->
    Req = [{<<"Category">>, <<"ecallmgr">>}
          ,{<<"Key">>, <<"acls">>}
          ,{<<"Node">>, <<"all">>}
          ,{<<"Default">>, kz_json:new()}
          ,{<<"Msg-ID">>, kz_binary:rand_hex(16)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    Resp = kz_amqp_worker:call(props:filter_undefined(Req)
                              ,fun kapi_sysconf:publish_get_req/1
                              ,fun kapi_sysconf:get_resp_v/1
                              ),
    case Resp of
        {'error', _} -> [];
        {'ok', JObj} ->
            extract_all_ips(kapi_sysconf:get_value(JObj, kz_json:new()))
    end.


-spec extract_all_ips(kz_json:object()) -> kz_json:objects().
extract_all_ips(JObj) ->
    kz_json:foldl(fun extract_ip/3, [], JObj).

-spec extract_ip(kz_json:path(), kz_json:object(), kz_json:objects()) ->
                        kz_json:objects().
extract_ip(_Key, Value, Acc) ->
    case kz_json:get_value(<<"cidr">>, Value) of
        'undefined' -> Acc;
        CIDR ->
            [kz_json:from_list([{<<"ip">>, CIDR}
                               ,{?AUTHZ_ID, kz_json:get_value(?AUTHZ_ID, Value)}
                               ])
             |Acc
            ]
    end.
