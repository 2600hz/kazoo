%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc For `gen_listeners' that bind to targeted for direct messaging.
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_bind).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-include_lib("kz_amqp_util.hrl").

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Q, Props) ->
    Exchange = props:get_value('exchange', Props),
    Routing = props:get_value('routing', Props),
    kz_amqp_util:bind_q_to_exchange(Q, Routing, Exchange).

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    Exchange = props:get_value('exchange', Props),
    Routing = props:get_value('routing', Props),
    kz_amqp_util:unbind_q_from_exchange(Q, Routing, Exchange).

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() -> 'ok'.
