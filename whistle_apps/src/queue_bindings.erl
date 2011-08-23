%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Helpers for binding queues to exchanges in gen_listener
%%% @end
%%% Created : 22 Aug 2011 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(queue_bindings).

-export([add_binding_to_q/3, rm_binding_from_q/2]).

-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("hotornot/include/hon_amqp.hrl").

-type bind_types() :: authentication |
		      registrations |
		      rating |
		      authorization.

-spec add_binding_to_q/3 :: (Q, Type, Props) -> ok when
      Q :: binary(),
      Type :: bind_types(),
      Props :: proplist().
add_binding_to_q(Q, authentication, _Props) ->
    amqp_util:callmgr_exchange(),
    amqp_util:bind_q_to_callmgr(Q, ?KEY_AUTHN_REQ),
    ok;
add_binding_to_q(Q, authorization, _Props) ->
    amqp_util:callmgr_exchange(),
    _ = amqp_util:bind_q_to_callmgr(Q, ?KEY_AUTHZ_REQ),
    ok;
add_binding_to_q(Q, rating, _Props) ->
    amqp_util:callmgr_exchange(),
    amqp_util:bind_q_to_callmgr(Q, ?KEY_RATING_REQ),
    ok;
add_binding_to_q(Q, registrations, _Props) ->
    amqp_util:callmgr_exchange(),
    amqp_util:bind_q_to_callmgr(Q, ?KEY_REG_SUCCESS),
    amqp_util:bind_q_to_callmgr(Q, ?KEY_REG_QUERY),
    ok.

-spec rm_binding_from_q/2 :: (Q, Type) -> ok when
      Q :: binary(),
      Type :: bind_types().
rm_binding_from_q(Q, authentication) ->
    amqp_util:unbind_q_from_callmgr(Q, ?KEY_AUTHN_REQ);
rm_binding_from_q(Q, authorization) ->
    amqp_util:unbind_q_from_callmgr(Q, ?KEY_AUTHZ_REQ);
rm_binding_from_q(Q, rating) ->
    amqp_util:unbind_q_from_callmgr(Q, ?KEY_RATING_REQ);
rm_binding_from_q(Q, registrations) ->
    amqp_util:unbind_q_from_callmgr(Q, ?KEY_REG_SUCCESS),
    amqp_util:unbind_q_from_callmgr(Q, ?KEY_REG_QUERY).
