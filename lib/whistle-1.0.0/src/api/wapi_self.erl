%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% For gen_listeners that bind to targeted for direct messaging
%%% @end
%%% Created : 16 Oct 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(wapi_self).

-export([bind_q/2, unbind_q/1]).

bind_q(Q, _Props) ->
    amqp_util:targeted_exchange(),
    amqp_util:bind_q_to_targeted(Q).

unbind_q(Q) ->
    amqp_util:unbind_q_from_targeted(Q).
