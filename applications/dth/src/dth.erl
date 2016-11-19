%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(dth).

-export([add_binding_to_q/2
        ,rm_binding_from_q/1
        ]).

-include("dth.hrl").


%% TODO move these two to a better place

-spec add_binding_to_q(ne_binary(), kz_proplist()) -> 'ok'.
add_binding_to_q(Q, _Props) ->
    amqp_util:bind_q_to_callmgr(Q, ?KEY_DTH_BLACKLIST_REQ),
    'ok'.

-spec rm_binding_from_q(ne_binary()) -> 'ok'.
rm_binding_from_q(Q) ->
    amqp_util:unbind_q_from_callmgr(Q, ?KEY_DTH_BLACKLIST_REQ).
