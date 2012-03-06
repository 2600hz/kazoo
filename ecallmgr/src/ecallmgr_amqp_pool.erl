%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Dispatches AMQP workers on behalf of client requests
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_amqp_pool).

-export([authn_req/1, authn_req/2
         ,authz_req/1, authz_req/2
         ,route_req/1, route_req/2
         ,reg_query/1, reg_query/2
         ,media_req/1, media_req/2
         ,get_req/1, get_req/2
         ,set_req/1, set_req/2
        ]).

-include("ecallmgr.hrl").

-define(DEFAULT_TIMEOUT, 5000).

-spec authn_req/1 :: (proplist() | wh_json:json_object()) -> {'ok', wh_json:json_object()} |
                                                             {'error', _}.
-spec authn_req/2 :: (proplist() | wh_json:json_object(), pos_integer()) -> {'ok', wh_json:json_object()} |
                                                                            {'error', _}.
authn_req(AuthnReq) ->
    authn_req(AuthnReq, ?DEFAULT_TIMEOUT).
authn_req(AuthnReq, Timeout) ->
    send_request(AuthnReq, Timeout, fun wapi_authn:publish_req/1).

-spec authz_req/1 :: (proplist() | wh_json:json_object()) -> {'ok', wh_json:json_object()} |
                                                             {'error', _}.
-spec authz_req/2 :: (proplist() | wh_json:json_object(), pos_integer()) -> {'ok', wh_json:json_object()} |
                                                                            {'error', _}.
authz_req(AuthzReq) ->
    authz_req(AuthzReq, ?DEFAULT_TIMEOUT).
authz_req(AuthzReq, Timeout) ->
    send_request(AuthzReq, Timeout, fun wapi_authz:publish_req/1).

-spec route_req/1 :: (proplist() | wh_json:json_object()) -> {'ok', wh_json:json_object()} |
                                                             {'error', _}.
-spec route_req/2 :: (proplist() | wh_json:json_object(), pos_integer()) -> {'ok', wh_json:json_object()} |
                                                                            {'error', _}.
route_req(RouteReq) ->
    route_req(RouteReq, ?DEFAULT_TIMEOUT).
route_req(RouteReq, Timeout) ->
    send_request(RouteReq, Timeout, fun wapi_route:publish_req/1).

-spec reg_query/1 :: (proplist() | wh_json:json_object()) -> {'ok', wh_json:json_object()} |
                                                             {'error', _}.
-spec reg_query/2 :: (proplist() | wh_json:json_object(), pos_integer()) -> {'ok', wh_json:json_object()} |
                                                                            {'error', _}.
reg_query(RegReq) ->
    reg_query(RegReq, ?DEFAULT_TIMEOUT).
reg_query(RegReq, Timeout) ->
    send_request(RegReq, Timeout, fun wapi_registration:publish_query_req/1).

-spec media_req/1 :: (proplist() | wh_json:json_object()) -> {'ok', wh_json:json_object()} |
                                                             {'error', _}.
-spec media_req/2 :: (proplist() | wh_json:json_object(), pos_integer()) -> {'ok', wh_json:json_object()} |
                                                                            {'error', _}.
media_req(MediaReq) ->
    media_req(MediaReq, ?DEFAULT_TIMEOUT).
media_req(MediaReq, Timeout) ->
    send_request(MediaReq, Timeout, fun wapi_media:publish_req/1).

-spec get_req/1 :: (proplist() | wh_json:json_object()) -> {'ok', wh_json:json_object()} |
                                                           {'error', _}.
-spec get_req/2 :: (proplist() | wh_json:json_object(), pos_integer()) -> {'ok', wh_json:json_object()} |
                                                                          {'error', _}.
get_req(GetReq) ->
    get_req(GetReq, ?DEFAULT_TIMEOUT).
get_req(GetReq, Timeout) ->
    send_request(GetReq, Timeout, fun wapi_sysconf:publish_get_req/1).

-spec set_req/1 :: (proplist() | wh_json:json_object()) -> {'ok', wh_json:json_object()} |
                                                           {'error', _}.
-spec set_req/2 :: (proplist() | wh_json:json_object(), pos_integer()) -> {'ok', wh_json:json_object()} |
                                                                          {'error', _}.
set_req(GetReq) ->
    set_req(GetReq, ?DEFAULT_TIMEOUT).
set_req(GetReq, Timeout) ->
    send_request(GetReq, Timeout, fun wapi_sysconf:publish_set_req/1).

-spec send_request/3 :: (api_terms(), pos_integer(), fun((api_terms()) -> _)) -> {'ok', wh_json:json_object()} |
                                                                                 {'error', _}.
send_request(Req, Timeout, PubFun) ->
    W = poolboy:checkout(?AMQP_POOL_MGR),

    Prop = case wh_json:is_json_object(Req) of
               true -> wh_json:to_proplist(Req);
               false -> Req
           end,

    Reply = gen_listener:call(W, {request, Prop, PubFun, Timeout}, Timeout),
    lager:debug("reply received: ~p", [Reply]),
    poolboy:checkin(?AMQP_POOL_MGR, W),
    Reply.
