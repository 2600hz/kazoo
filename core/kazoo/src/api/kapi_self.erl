%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% For gen_listeners that bind to targeted for direct messaging
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kapi_self).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).
-export([publish_message/2, publish_message/3]).

-include_lib("kazoo/include/kz_api.hrl").
-include_lib("kazoo/include/kz_types.hrl").

-spec bind_q(ne_binary(), kz_proplist()) -> 'ok'.
bind_q(Q, _Props) ->
    amqp_util:bind_q_to_targeted(Q).

-spec unbind_q(ne_binary(), kz_proplist()) -> 'ok'.
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

-spec publish_message(ne_binary(), kz_json:object()) -> 'ok'.
-spec publish_message(ne_binary(), maybe(terms()), ne_binary()) -> 'ok'.
publish_message(ServerId, JObj) ->
    publish_message(ServerId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_message(ServerId, API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, [], fun build/1),
    amqp_util:targeted_publish(ServerId, Payload, ContentType).

-spec build(maybe(terms())) -> {'ok', iolist()}.
build(Prop) when is_list(Prop) ->
    kz_api:build_message(Prop, [], []);
build(JObj) ->
    build(kz_json:to_proplist(JObj)).
