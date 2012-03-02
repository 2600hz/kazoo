%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Dispatches AMQP workers on behalf of client requests
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_amqp_pool).

-export([authn_req/1, authn_req/2]).

-include("ecallmgr.hrl").

-define(DEFAULT_TIMEOUT, 5000).

authn_req(AuthnReq) ->
    authn_req(AuthnReq, ?DEFAULT_TIMEOUT).
authn_req(AuthnReq, Timeout) ->
    W = poolboy:checkout(?AMQP_POOL_MGR),

    Prop = case wh_json:is_json_object(AuthnReq) of
               true -> wh_json:to_proplist(AuthnReq);
               false -> AuthnReq
           end,

    Reply = gen_listener:call(W, {request, Prop, fun wapi_authn:publish_req/1, get(callid), Timeout}, Timeout),
    poolboy:checkin(W),
    Reply.
