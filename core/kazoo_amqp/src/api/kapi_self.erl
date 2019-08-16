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
-module(kapi_self).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).
-export([publish_message/2, publish_message/3]).

-include_lib("kz_amqp_util.hrl").

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Q, _Props) ->
    kz_amqp_util:bind_q_to_targeted(Q).

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Q, _Props) ->
    kz_amqp_util:unbind_q_from_targeted(Q).

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:targeted_exchange().

-spec publish_message(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
publish_message(ServerId, JObj) ->
    publish_message(ServerId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_message(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_message(ServerId, API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, [], fun build/1),
    kz_amqp_util:targeted_publish(ServerId, Payload, ContentType).

-spec build(kz_term:api_terms()) -> {'ok', iolist()}.
build(Prop) when is_list(Prop) ->
    kz_api:build_message(Prop, [], []);
build(JObj) ->
    build(kz_json:to_proplist(JObj)).
