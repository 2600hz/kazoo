%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Helpers for binding queues to exchanges in gen_listener
%%% @end
%%% Created : 22 Aug 2011 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(queue_bindings).

-export([known_bind_types/0, add_binding_to_q/3, rm_binding_from_q/2]).

-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_types.hrl").
%% -include("hotornot/include/hon_amqp.hrl").
%% -include("whistle_apps/dth/include/dth_amqp.hrl").
%% -include("whistle_apps/callflow/include/cf_amqp.hrl").

-type bind_types() :: 'authentication' |
		      'registrations' |
		      'rating' |
		      'routing' |
		      'cdrs' |
		      'dth' |
		      'call_events' |
		      'self' |
		      'notifications' |
		      'authorization'.
-export_type([bind_types/0]).

-spec known_bind_types/0 :: () -> [bind_types(),...].
known_bind_types() ->
    ['authentication', 'registrations', 'rating', 'routing'
     ,'cdrs', 'dth', 'call_events', 'self', 'notifications'
     ,'authorization'
    ].

-spec add_binding_to_q/3 :: (Q, Type, Props) -> 'ok' when
      Q :: binary(),
      Type :: bind_types(),
      Props :: proplist().
add_binding_to_q(Q, self, _Props) ->
    amqp_util:targeted_exchange(),
    amqp_util:bind_q_to_targeted(Q),
    ok;
add_binding_to_q(Q, authentication, _Props) ->
    amqp_util:callmgr_exchange(),
    amqp_util:bind_q_to_callmgr(Q, ?KEY_AUTHN_REQ),
    ok;
add_binding_to_q(Q, authorization, _Props) ->
    amqp_util:callmgr_exchange(),
    _ = amqp_util:bind_q_to_callmgr(Q, ?KEY_AUTHZ_REQ),
    ok;
add_binding_to_q(Q, routing, _Props) ->
    amqp_util:callmgr_exchange(),
    amqp_util:bind_q_to_callmgr(Q, ?KEY_ROUTE_REQ),
    ok;
add_binding_to_q(Q, cdrs, Props) ->
    CallID = get_callid(Props),
    amqp_util:bind_q_to_callevt(Q, CallID, cdr),
    ok;
add_binding_to_q(Q, call_events, Props) ->
    CallID = get_callid(Props),
    amqp_util:bind_q_to_callevt(Q, CallID, events),
    ok;
add_binding_to_q(Q, registrations, _Props) ->
    amqp_util:callmgr_exchange(),
    amqp_util:bind_q_to_callmgr(Q, ?KEY_REG_SUCCESS),
    amqp_util:bind_q_to_callmgr(Q, ?KEY_REG_QUERY),
    ok;
add_binding_to_q(Q, rating, Props) ->
    hotornot:add_binding_to_q(Q, Props);
add_binding_to_q(Q, dth, Props) ->
    dth:add_binding_to_q(Q, Props);
add_binding_to_q(Q, notifications, Props) ->
    notify:add_binding_to_q(Q, Props).

-spec rm_binding_from_q/2 :: (Q, Type) -> 'ok' when
      Q :: binary(),
      Type :: bind_types().
rm_binding_from_q(Q, self) ->
    amqp_util:unbind_q_from_targeted(Q);
rm_binding_from_q(Q, authentication) ->
    amqp_util:unbind_q_from_callmgr(Q, ?KEY_AUTHN_REQ);
rm_binding_from_q(Q, authorization) ->
    amqp_util:unbind_q_from_callmgr(Q, ?KEY_AUTHZ_REQ);
rm_binding_from_q(Q, routing) ->
    amqp_util:unbind_q_from_callmgr(Q, ?KEY_ROUTE_REQ);
rm_binding_from_q(Q, cdrs) ->
    rm_binding_from_q(Q, cdrs, []);
rm_binding_from_q(Q, call_events) ->
    rm_binding_from_q(Q, call_events, []);
rm_binding_from_q(Q, registrations) ->
    amqp_util:unbind_q_from_callmgr(Q, ?KEY_REG_SUCCESS),
    amqp_util:unbind_q_from_callmgr(Q, ?KEY_REG_QUERY);
rm_binding_from_q(Q, rating) ->
    hotornot:rm_binding_from_q(Q);
rm_binding_from_q(Q, dth) ->
    dth:rm_binding_from_q(Q);
rm_binding_from_q(Q, notifications) ->
    notify:rm_binding_from_q(Q).

-spec rm_binding_from_q/3 :: (Q, Type, Props) -> 'ok' when
      Q :: binary(),
      Type :: bind_types(),
      Props :: proplist().
rm_binding_from_q(Q, call_events, Props) ->
    CallID = get_callid(Props),
    amqp_util:unbind_q_from_callevt(Q, CallID, events);
rm_binding_from_q(Q, cdrs, Props) ->
    CallID = get_callid(Props),
    amqp_util:unbind_q_from_callevt(Q, CallID, cdr).

-spec get_callid/1 :: (Props) -> binary() when
      Props :: proplist().
get_callid(Props) ->
    case props:get_value(callid, Props) of
	undefined -> <<"*">>;
	CID -> CID
    end.
