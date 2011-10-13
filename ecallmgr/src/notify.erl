%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP, INC
%%% @doc
%%% handle notification bindings for ecallmgr
%%% @end
%%% Created :  3 Oct 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(notify).

-author('James Aimonetti <james@2600hz.org>').
-export([add_binding_to_q/2, rm_binding_from_q/1]).

-include("ecallmgr.hrl").

add_binding_to_q(Q, Props) ->
    Keys = case props:get_value(keys, Props) of
	       undefined ->
		   [?KEY_SIP_NOTIFY];
	       Ks -> Ks
	   end,
    _ = [ bind_q_to_key(Q, Key) || Key <- Keys ],
    ok.

-spec bind_q_to_key/2 :: (Q, Key) -> ok when
      Q :: binary(),
      Key :: binary().

bind_q_to_key(Q, ?KEY_SIP_NOTIFY) ->
    amqp_util:bind_q_to_callmgr(Q, ?KEY_SIP_NOTIFY);
bind_q_to_key(Q, Key) ->
    amqp_util:bind_q_to_callmgr(Q, Key).

rm_binding_from_q(Q) ->
    amqp_util:unbind_q_from_callmgr(Q, ?KEY_SIP_NOTIFY).
