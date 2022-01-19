%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2022, 2600Hz
%%% @doc Maintenance functions for all
%%% @author Luis Azedo
%%% @end
%%%-----------------------------------------------------------------------------
-module(pusher_maintenance).

-include("pusher.hrl").

-export([add_firebase_app/2
        ,add_apple_app/2, add_apple_app/3
        ,push/2
        ]).

-spec add_firebase_app(binary(), binary()) -> 'ok'.
add_firebase_app(AppId, Secret) ->
    _ = kapps_config:set_node(?CONFIG_CAT, [<<"firebase">>, <<"api_key">>], Secret, AppId),
    'ok'.

-spec add_apple_app(binary(), binary()) -> 'ok' | {'error', any()}.
add_apple_app(AppId, Certfile) ->
    add_apple_app(AppId, Certfile, ?DEFAULT_APNS_HOST).

-spec add_apple_app(binary(), binary(), binary()) -> 'ok' | {'error', any()}.
add_apple_app(AppId, Certfile, Host) ->
    case file:read_file(Certfile) of
        {'ok', Binary} ->
            _ = kapps_config:set_node(?CONFIG_CAT, [<<"apple">>, <<"certificate">>], Binary, AppId),
            _ = kapps_config:set_node(?CONFIG_CAT, [<<"apple">>, <<"host">>], Host, AppId),
            'ok';
        {'error', _} = Err -> Err
    end.

-spec push(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
push(AccountId, DeviceId) ->
    case kzd_devices:fetch(AccountId, DeviceId) of
        {'ok', Device} -> push(kz_json:get_json_value(<<"push">>, Device));
        {'error', Error} -> io:format("error: ~p~n", [Error])
    end.

push('undefined') ->
    io:format("error: no push propeties for device~n");
push(Push) ->
    CallId = kz_binary:rand_hex(16),
    MsgId = kz_binary:rand_hex(16),
    RegToken = kz_binary:rand_hex(16),
    CallerIdNumber = <<"15555555555">>,
    CallerIdName = <<"this is a push test">>,
    TokenApp = kz_json:get_ne_binary_value(<<"Token-App">>, Push),
    TokenType = kz_json:get_ne_binary_value(<<"Token-Type">>, Push),
    TokenId = kz_json:get_ne_binary_value(<<"Token-ID">>, Push),
    TokenProxy = kz_json:get_ne_binary_value(<<"Token-Proxy">>, Push),
    Payload = [{<<"call-id">>, CallId}
              ,{<<"proxy">>, TokenProxy}
              ,{<<"caller-id-number">>, CallerIdNumber}
              ,{<<"caller-id-name">>, CallerIdName}
              ,{<<"registration-token">>, RegToken}
              ],
    Msg = [{<<"Msg-ID">>, MsgId}
          ,{<<"App-Name">>, <<"Kamailio">>}
          ,{<<"App-Version">>, <<"1.0">>}
          ,{<<"Event-Category">>, <<"notification">>}
          ,{<<"Event-Name">>, <<"push_req">>}
          ,{<<"Call-ID">>, CallId}
          ,{<<"Token-ID">>, TokenId}
          ,{<<"Token-Type">>, TokenType}
          ,{<<"Token-App">>, TokenApp}
          ,{<<"Alert-Key">>, <<"IC_SIL">>}
          ,{<<"Alert-Params">>, [CallerIdNumber]}
          ,{<<"Sound">>, <<"ring.caf">>}
          ,{<<"Payload">>, kz_json:from_list(Payload)}
          ],
    pusher_listener:push(kz_json:from_list(Msg)).
