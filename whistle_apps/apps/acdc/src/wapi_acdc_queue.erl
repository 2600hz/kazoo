%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wapi_acdc_queue).

-export([bind_q/2
         ,unbind_q/2
        ]).

-include("acdc.hrl").

-define(MEMBER_CONNECT_REQ_ROUTING_KEY, <<"member_connect.req.">>).

-spec bind_q/2 :: (ne_binary(), wh_proplist()) -> 'ok'.
bind_q(Q, Props) ->
    QID = props:get_value(queue_id, Props, <<"*">>),

    amqp_util:callmgr_exchange(),
    amqp_util:bind_q_to_callmgr(Q, member_connect_req_routing_key(QID)).

-spec unbind_q/2 :: (ne_binary(), wh_proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    QID = props:get_value(queue_id, Props, <<"*">>),

    amqp_util:unbind_q_from_callmgr(Q, member_connect_req_routing_key(QID)).

-spec member_connect_req_routing_key/1 :: (ne_binary()) -> ne_binary().
member_connect_req_routing_key(QID) ->
    <<?MEMBER_CONNECT_REQ_ROUTING_KEY/binary, QID/binary>>.
