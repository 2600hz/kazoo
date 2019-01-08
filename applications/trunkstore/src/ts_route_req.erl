%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Handle route requests off AMQP
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(ts_route_req).

-export([init/0
        ,handle_req/2
        ]).

-include("ts.hrl").

-spec init() -> 'ok'.
init() -> 'ok'.

-spec handle_req(kz_json:object(), kz_term:proplist()) -> any().
handle_req(ApiJObj, _Options) ->
    'true' = kapi_route:req_v(ApiJObj),
    kz_util:put_callid(ApiJObj),
    lager:info("received request ~s asking if trunkstore can route this call", [kapi_route:fetch_id(ApiJObj)]),
    CCVs = kz_json:get_value(<<"Custom-Channel-Vars">>, ApiJObj),
    ApplicationName = kz_json:get_ne_binary_value(<<"Application-Name">>, CCVs),
    case kz_json:get_first_defined([<<"Referred-By">>, <<"Redirected-By">>], CCVs) =/= 'undefined' of
        'true' when ApplicationName =:= ?APP_NAME -> handle_onnet_req(ApiJObj);
        'true' ->
            lager:info("request is the result of a transfer by another application, ~s, ignoring"
                      ,[ApplicationName]
                      );
        'false' ->
            case kz_json:get_ne_binary_value(<<"Authorizing-Type">>, CCVs) =:= <<"sys_info">>
                andalso kz_json:get_ne_binary_value(<<"Authorizing-ID">>, CCVs) =/= 'undefined'
            of
                'true' -> handle_onnet_req(ApiJObj);
                'false' -> handle_offnet_req(ApiJObj)
            end
    end.

-spec handle_onnet_req(kz_json:object()) -> any().
handle_onnet_req(ApiJObj) ->
    %% Coming from PBX (on-net); authed by Registrar or ts_auth
    CallId = kz_json:get_ne_binary_value(<<"Call-ID">>, ApiJObj),
    lager:info("call with fetch-id ~s began on the network", [kapi_route:fetch_id(ApiJObj)]),
    ts_onnet_sup:start_handler(<<"onnet-", CallId/binary>>, ApiJObj).

-spec handle_offnet_req(kz_json:object()) -> any().
handle_offnet_req(ApiJObj) ->
    %% Coming from carrier (off-net)
    CallId = kz_json:get_ne_binary_value(<<"Call-ID">>, ApiJObj),
    lager:info("call with fetch-id ~s began from outside the network", [kapi_route:fetch_id(ApiJObj)]),
    ts_offnet_sup:start_handler(<<"offnet-", CallId/binary>>, ApiJObj).
