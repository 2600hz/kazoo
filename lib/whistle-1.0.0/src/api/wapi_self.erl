%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% For gen_listeners that bind to targeted for direct messaging
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wapi_self).

-export([bind_q/2, unbind_q/2]).

bind_q(Q, _Props) ->
    amqp_util:targeted_exchange(),
    amqp_util:bind_q_to_targeted(Q).

unbind_q(Q, _Props) ->
    amqp_util:unbind_q_from_targeted(Q).
