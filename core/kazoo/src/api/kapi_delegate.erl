%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
%%% @doc
%%%
%%%  Delegate job from one app to another app.
%%%
%%%  App/Key combo used to send messages.
%%%
%%% @end
%%% @contributors
%%%   SIPLABS LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(kapi_delegate).

-export([delegate/1, delegate_v/1]).

-export([bind_q/2
         ,unbind_q/2
        ]).
-export([declare_exchanges/0]).
-export([publish_delegate/2, publish_delegate/3, publish_delegate/4]).

-include_lib("kazoo/include/kz_api.hrl").

-define(APIKEY, <<"delegate">>).

-type maybe_key() :: api(ne_binary()).

-define(DELEGATE_ROUTING_KEY(App, Key), <<?APIKEY/binary, "."
                                          ,(amqp_util:encode(App))/binary, "."
                                          ,(amqp_util:encode(Key))/binary
                                        >>).
-define(DELEGATE_ROUTING_KEY(App), <<?APIKEY/binary, "."
                                     ,(amqp_util:encode(App))/binary
                                   >>).

-define(DELEGATE_HEADERS, [<<"Delegate-Message">>]).
-define(OPTIONAL_DELEGATE_HEADERS, []).
-define(DELEGATE_VALUES, [{<<"Event-Category">>, <<"delegate">>}
                          ,{<<"Event-Name">>, <<"job">>}
                         ]).
-define(DELEGATE_TYPES, []).

%%--------------------------------------------------------------------
%% @doc Resume a callflow's flow
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
-spec delegate(api_terms()) -> {'ok', iolist()} | {'error', string()}.
delegate(Prop) when is_list(Prop) ->
    case delegate_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?DELEGATE_HEADERS, ?OPTIONAL_DELEGATE_HEADERS);
        'false' -> {'error', "Proplist failed validation for authn_error"}
    end;
delegate(JObj) -> delegate(kz_json:to_proplist(JObj)).

-spec delegate_v(api_terms()) -> boolean().
delegate_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?DELEGATE_HEADERS, ?DELEGATE_VALUES, ?DELEGATE_TYPES);
delegate_v(JObj) -> delegate_v(kz_json:to_proplist(JObj)).

-spec bind_q(ne_binary(), kz_proplist()) -> 'ok'.
-spec bind_q(ne_binary(), ne_binary(), maybe_key()) -> 'ok'.
bind_q(Q, Props) ->
    App = props:get_binary_value('app_name', Props),
    Key = props:get_value('route_key', Props),
    bind_q(Q, App, Key).
bind_q(Q, <<_/binary>> = App, 'undefined') ->
    amqp_util:bind_q_to_kapps(Q, ?DELEGATE_ROUTING_KEY(App));
bind_q(Q, <<_/binary>> = App, <<_/binary>> = Key) ->
    amqp_util:bind_q_to_kapps(Q, ?DELEGATE_ROUTING_KEY(App, Key)).

-spec unbind_q(ne_binary(), kz_proplist()) -> 'ok'.
-spec unbind_q(ne_binary(), ne_binary(), maybe_key()) -> 'ok'.
unbind_q(Q, Props) ->
    App = props:get_binary_value('app_name', Props),
    Key = props:get_value('route_key', Props),
    unbind_q(Q, App, Key).
unbind_q(Q, <<_/binary>> = App, 'undefined') ->
    amqp_util:unbind_q_from_kapps(Q, ?DELEGATE_ROUTING_KEY(App));
unbind_q(Q, <<_/binary>> = App, <<_/binary>> = Key) ->
    amqp_util:unbind_q_from_kapps(Q, ?DELEGATE_ROUTING_KEY(App, Key)).

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:kapps_exchange().

%%--------------------------------------------------------------------
%% @doc Publish the JSON iolist() to the proper Exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_delegate(ne_binary(), api_terms()) -> 'ok'.
-spec publish_delegate(ne_binary(), api_terms(), maybe_key()) -> 'ok'.
-spec publish_delegate(ne_binary(), api_terms(), maybe_key(), binary()) -> 'ok'.
publish_delegate(TargetApp, API) ->
    publish_delegate(TargetApp, API, 'undefined').

publish_delegate(TargetApp, API, Key) ->
    publish_delegate(TargetApp, API, Key, ?DEFAULT_CONTENT_TYPE).

publish_delegate(<<_/binary>> = TargetApp, API, 'undefined', ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?DELEGATE_VALUES, fun ?MODULE:delegate/1),
    amqp_util:kapps_publish(?DELEGATE_ROUTING_KEY(TargetApp), Payload, ContentType);
publish_delegate(<<_/binary>> = TargetApp, API, Key, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?DELEGATE_VALUES, fun ?MODULE:delegate/1),
    amqp_util:kapps_publish(?DELEGATE_ROUTING_KEY(TargetApp, Key), Payload, ContentType).
