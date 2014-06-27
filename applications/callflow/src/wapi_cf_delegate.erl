%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%%  Delegate job from callflow action to another app.
%%%
%%%  App/Key combo used to send messages.
%%%
%%% @end
%%% @contributors
%%%   SIPLABS LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(wapi_cf_delegate).

-export([delegate/1, delegate_v/1]).

-export([bind_q/2
         ,unbind_q/2
        ]).
-export([declare_exchanges/0]).
-export([publish_delegate/2, publish_delegate/3, publish_delegate/4]).

-include("callflow.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DELEGATE_ROUTING_KEY(App, Key), <<"callflow.delegate", App/binary, ".", Key/binary>>).
%-define(DELEGATE_ROUTING_KEY(App), <<"callflow.delegate", App/binary, ".common">>).

-define(DELEGATE_HEADERS, [<<"Delegate-Message">>]).
-define(OPTIONAL_DELEGATE_HEADERS, [<<"Event-Category">>, <<"Event-Name">>]).
-define(DELEGATE_VALUES, [{<<"Event-Category">>, <<"callflow">>}
                        ,{<<"Event-Name">>, <<"delegate">>}
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
        'true' -> wh_api:build_message(Prop, ?DELEGATE_HEADERS, ?OPTIONAL_DELEGATE_HEADERS);
        'false' -> {'error', "Proplist failed validation for authn_error"}
    end;
delegate(JObj) -> delegate(wh_json:to_proplist(JObj)).

-spec delegate_v(api_terms()) -> boolean().
delegate_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?DELEGATE_HEADERS, ?DELEGATE_VALUES, ?DELEGATE_TYPES);
delegate_v(JObj) -> delegate_v(wh_json:to_proplist(JObj)).

-spec bind_q(ne_binary(), wh_proplist()) -> 'ok'.
bind_q(Q, Props) ->
    App = props:get_value('app_name', Props),
    ?assertNot(App =:= 'undefined'),
    Key = props:get_value('route_key', Props, <<"#">>),
    amqp_util:bind_q_to_whapps(Q, ?DELEGATE_ROUTING_KEY(App, Key)).

-spec unbind_q(ne_binary(), wh_proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    App = props:get_ne_value('app_name', Props),
    ?assertNot(App =:= 'undefined'),
    Key = props:get_value('route_key', Props, <<"#">>),
    amqp_util:unbind_q_from_whapps(Q, ?DELEGATE_ROUTING_KEY(App, Key)).

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:whapps_exchange().

%%--------------------------------------------------------------------
%% @doc Publish the JSON iolist() to the proper Exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_delegate(ne_binary(), api_terms()) -> 'ok'.
-spec publish_delegate(ne_binary(), ne_binary(), api_terms()) -> 'ok'.
-spec publish_delegate(ne_binary(), ne_binary(), api_terms(), binary()) -> 'ok'.
publish_delegate(TargetApp, JObj) ->
    publish_delegate(TargetApp, <<"common">>, JObj).
publish_delegate(TargetApp, Key, JObj) ->
    publish_delegate(TargetApp, Key, JObj, ?DEFAULT_CONTENT_TYPE).
publish_delegate(TargetApp, Key, API, ContentType) ->
    ?assertNot(TargetApp =:= 'undefined'),
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?DELEGATE_VALUES, fun ?MODULE:delegate/1),
    amqp_util:whapps_publish(?DELEGATE_ROUTING_KEY(TargetApp, Key), Payload, ContentType).
