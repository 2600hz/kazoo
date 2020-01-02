%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc Delegate JObj from one application to another application.
%%% App/Key combo used to send messages.
%%%
%%%
%%% @author SIPLABS LLC (Maksim Krzhemenevskiy)
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_delegate).

-export([delegate/1, delegate_v/1]).

-export([bind_q/2
        ,unbind_q/2
        ]).
-export([declare_exchanges/0]).
-export([publish_delegate/2, publish_delegate/3, publish_delegate/4]).

-include_lib("kz_amqp_util.hrl").

-define(APIKEY, <<"delegate">>).

-type maybe_key() :: kz_term:ne_binary() | 'undefined'.

-define(DELEGATE_ROUTING_KEY(App, Key)
       ,list_to_binary([?APIKEY, "."
                       ,kz_amqp_util:encode(App), "."
                       ,kz_amqp_util:encode(Key)
                       ])
       ).
-define(DELEGATE_ROUTING_KEY(App)
       ,list_to_binary([?APIKEY, ".", kz_amqp_util:encode(App)])
       ).

-define(DELEGATE_HEADERS, [<<"Delegate-Message">>]).
-define(OPTIONAL_DELEGATE_HEADERS, []).
-define(DELEGATE_VALUES, [{<<"Event-Category">>, <<"delegate">>}
                         ,{<<"Event-Name">>, <<"job">>}
                         ]).
-define(DELEGATE_TYPES, []).

%%------------------------------------------------------------------------------
%% @doc Resume a Callflow's flow.
%% Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec delegate(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
delegate(Prop) when is_list(Prop) ->
    case delegate_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?DELEGATE_HEADERS, ?OPTIONAL_DELEGATE_HEADERS);
        'false' -> {'error', "Proplist failed validation for authn_error"}
    end;
delegate(JObj) -> delegate(kz_json:to_proplist(JObj)).

-spec delegate_v(kz_term:api_terms()) -> boolean().
delegate_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?DELEGATE_HEADERS, ?DELEGATE_VALUES, ?DELEGATE_TYPES);
delegate_v(JObj) -> delegate_v(kz_json:to_proplist(JObj)).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Q, Props) ->
    App = props:get_binary_value('app_name', Props),
    Key = props:get_value('route_key', Props),
    bind_q(Q, App, Key).

-spec bind_q(kz_term:ne_binary(), kz_term:ne_binary(), maybe_key()) -> 'ok'.
bind_q(Q, <<_/binary>> = App, 'undefined') ->
    kz_amqp_util:bind_q_to_kapps(Q, ?DELEGATE_ROUTING_KEY(App));
bind_q(Q, <<_/binary>> = App, <<_/binary>> = Key) ->
    kz_amqp_util:bind_q_to_kapps(Q, ?DELEGATE_ROUTING_KEY(App, Key)).

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    App = props:get_binary_value('app_name', Props),
    Key = props:get_value('route_key', Props),
    unbind_q(Q, App, Key).

-spec unbind_q(kz_term:ne_binary(), kz_term:ne_binary(), maybe_key()) -> 'ok'.
unbind_q(Q, <<_/binary>> = App, 'undefined') ->
    kz_amqp_util:unbind_q_from_kapps(Q, ?DELEGATE_ROUTING_KEY(App));
unbind_q(Q, <<_/binary>> = App, <<_/binary>> = Key) ->
    kz_amqp_util:unbind_q_from_kapps(Q, ?DELEGATE_ROUTING_KEY(App, Key)).

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:kapps_exchange().

%%------------------------------------------------------------------------------
%% @doc Publish the JSON string to the proper Exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_delegate(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_delegate(TargetApp, API) ->
    publish_delegate(TargetApp, API, 'undefined').

-spec publish_delegate(kz_term:ne_binary(), kz_term:api_terms(), maybe_key()) -> 'ok'.
publish_delegate(TargetApp, API, Key) ->
    publish_delegate(TargetApp, API, Key, ?DEFAULT_CONTENT_TYPE).

-spec publish_delegate(kz_term:ne_binary(), kz_term:api_terms(), maybe_key(), binary()) -> 'ok'.
publish_delegate(<<_/binary>> = TargetApp, API, 'undefined', ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?DELEGATE_VALUES, fun delegate/1),
    kz_amqp_util:kapps_publish(?DELEGATE_ROUTING_KEY(TargetApp), Payload, ContentType);
publish_delegate(<<_/binary>> = TargetApp, API, Key, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?DELEGATE_VALUES, fun delegate/1),
    kz_amqp_util:kapps_publish(?DELEGATE_ROUTING_KEY(TargetApp, Key), Payload, ContentType).
