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
-export([declare_exchanges/0]).

bind_q(Q, _Props) ->
    amqp_util:bind_q_to_targeted(Q).

unbind_q(Q, _Props) ->
    amqp_util:unbind_q_from_targeted(Q).

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:targeted_exchange().
