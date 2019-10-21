%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Handle route requests off AMQP
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ts_route_req).

-export([init/0
        ,handle_req/2
        ]).

-include("ts.hrl").

-spec init() -> 'ok'.
init() -> 'ok'.

-spec handle_req(kapi_route:req(), kz_term:proplist()) -> any().
handle_req(RouteReq, _Options) ->
    'true' = kapi_route:req_v(RouteReq),
    kz_log:put_callid(kapi_route:call_id(RouteReq)),
    kz_amqp_worker:worker_pool(trunkstore_sup:pool_name()),

    lager:info("received request ~s asking if trunkstore can route this call", [kapi_route:fetch_id(RouteReq)]),
    CCVs = kz_json:get_value(<<"Custom-Channel-Vars">>, RouteReq),
    ApplicationName = kz_json:get_ne_binary_value(<<"Application-Name">>, CCVs),
    case kz_json:get_first_defined([<<"Referred-By">>, <<"Redirected-By">>], CCVs) =/= 'undefined' of
        'true' when ApplicationName =:= ?APP_NAME ->
            maybe_handle(RouteReq, fun handle_onnet_req/2, kz_amqp_worker:checkout_worker());
        'true' ->
            lager:info("request is the result of a transfer by another application, ~s, ignoring"
                      ,[ApplicationName]
                      );
        'false' ->
            case kz_json:get_ne_binary_value(<<"Authorizing-Type">>, CCVs) =:= <<"sys_info">>
                andalso kz_json:get_ne_binary_value(<<"Authorizing-ID">>, CCVs) =/= 'undefined'
            of
                'true' -> maybe_handle(RouteReq, fun handle_onnet_req/2, kz_amqp_worker:checkout_worker());
                'false' -> maybe_handle(RouteReq, fun handle_offnet_req/2, kz_amqp_worker:checkout_worker())
            end
    end.

-spec maybe_handle(kapi_route:req(), fun(), {'ok', pid()} | {'error', any()}) -> any().
maybe_handle(_RouteReq, _HandlerFun, {'error', _E}) ->
    lager:warning("ignoring req, failed to checkout AMQP worker: ~p", [_E]);
maybe_handle(RouteReq, HandlerFun, {'ok', AMQPWorker}) ->
    lager:info("checked out AMQP worker ~p", [AMQPWorker]),
    HandlerFun(RouteReq, AMQPWorker).

-spec handle_onnet_req(kapi_route:req(), pid()) -> any().
handle_onnet_req(RouteReq, AMQPWorker) ->
    %% Coming from PBX (onnet); authed by Registrar or ts_auth
    CallId = kapi_route:call_id(RouteReq),
    lager:info("call with fetch-id ~s began on the network", [kapi_route:fetch_id(RouteReq)]),
    ts_onnet_sup:start_handler(<<"onnet-", CallId/binary>>, RouteReq, AMQPWorker).

-spec handle_offnet_req(kapi_route:req(), pid()) -> any().
handle_offnet_req(RouteReq, AMQPWorker) ->
    %% Coming from carrier (offnet)
    CallId = kapi_route:call_id(RouteReq),
    lager:info("call with fetch-id ~s began from outside the network", [kapi_route:fetch_id(RouteReq)]),
    ts_offnet_sup:start_handler(<<"offnet-", CallId/binary>>, RouteReq, AMQPWorker).
