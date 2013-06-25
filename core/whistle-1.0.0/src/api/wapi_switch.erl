%%%-------------------------------------------------------------------
%%% @author Edouard Swiac <edouard@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Switch events messages
%%% @end
%%%-------------------------------------------------------------------
-module(wapi_switch).

-export([reload_acls/1, reload_acls_v/1]).
-export([reload_gateways/1, reload_gateways_v/1]).

-export([bind_q/2, unbind_q/2]).

-export([publish_reload_acls/0]).
-export([publish_reload_gateways/0]).

-include_lib("whistle/include/wh_api.hrl").

%% request to reload acl
-define(RELOAD_ACLS_HEADERS, []).
-define(OPTIONAL_RELOAD_ACLS_HEADERS, []).
-define(RELOAD_ACLS_VALUES, [{<<"Event-Name">>, <<"reload_acls">>}
                                        ,{<<"Event-Category">>, <<"switch_event">>}
                                       ]).
-define(RELOAD_ACLS_TYPES, []).
-define(RELOAD_ACLS_KEY, <<"switch.reload_acls">>).

%% request to reload gateways
-define(RELOAD_GATEWAYS_HEADERS, []).
-define(OPTIONAL_RELOAD_GATEWAYS_HEADERS, []).
-define(RELOAD_GATEWAYS_VALUES, [{<<"Event-Name">>, <<"reload_gateways">>}
                                              ,{<<"Event-Category">>, <<"switch_event">>}
                                             ]).
-define(RELOAD_GATEWAYS_TYPES, []).
-define(RELOAD_GATEWAYS_KEY, <<"switch.reload_gateways">>).

%% Request a reload_acls
-spec reload_acls/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
reload_acls(Prop) when is_list(Prop) ->
    case reload_acls_v(Prop) of
        true -> wh_api:build_message(Prop, ?RELOAD_ACLS_HEADERS, ?OPTIONAL_RELOAD_ACLS_HEADERS);
        false -> {error, "Proplist failed validation for switch event reload_acls req"}
    end;
reload_acls(JObj) ->
    reload_acls(wh_json:to_proplist(JObj)).

-spec reload_acls_v/1 :: (api_terms()) -> boolean().
reload_acls_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?RELOAD_ACLS_HEADERS, ?RELOAD_ACLS_VALUES, ?RELOAD_ACLS_TYPES);
reload_acls_v(JObj) ->
    reload_acls_v(wh_json:to_proplist(JObj)).

%% Request a reload_gateways
-spec reload_gateways/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
reload_gateways(Prop) when is_list(Prop) ->
    case reload_gateways_v(Prop) of
        true -> wh_api:build_message(Prop, ?RELOAD_GATEWAYS_HEADERS, ?OPTIONAL_RELOAD_GATEWAYS_HEADERS);
        false -> {error, "Proplist failed validation for switch event reload_gateways req"}
    end;
reload_gateways(JObj) ->
    reload_gateways(wh_json:to_proplist(JObj)).

-spec reload_gateways_v/1 :: (api_terms()) -> boolean().
reload_gateways_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?RELOAD_GATEWAYS_HEADERS, ?RELOAD_GATEWAYS_VALUES, ?RELOAD_GATEWAYS_TYPES);
reload_gateways_v(JObj) ->
    reload_gateways_v(wh_json:to_proplist(JObj)).

-spec bind_q/2 :: (binary(), proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    amqp_util:configuration_exchange(),
    bind_to_q(Queue, props:get_value(restrict_to, Props)).

bind_to_q(Q, undefined) ->
    ok = amqp_util:bind_q_to_configuration(Q, <<"switch.*">>);
bind_to_q(Q, [reload_acls|T]) ->
    ok = amqp_util:bind_q_to_configuration(Q, ?RELOAD_ACLS_KEY),
    bind_to_q(Q, T);
bind_to_q(Q, [reload_gateways|T]) ->
    ok = amqp_util:bind_q_to_configuration(Q, ?RELOAD_GATEWAYS_KEY),
    bind_to_q(Q, T);
bind_to_q(_Q, []) ->
    ok.

-spec unbind_q/2 :: (ne_binary(), proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    unbind_q_from(Queue, props:get_value(restrict_to, Props)).

unbind_q_from(Q, undefined) ->
    ok = amqp_util:unbind_q_from_configuration(Q, <<"switch.*">>);
unbind_q_from(Q, [reload_acls|T]) ->
    ok = amqp_util:unbind_q_from_configuration(Q, ?RELOAD_ACLS_KEY),
    unbind_q_from(Q, T);
unbind_q_from(Q, [reload_gateways|T]) ->
    ok = amqp_util:unbind_q_from_configuration(Q, ?RELOAD_GATEWAYS_KEY),
    unbind_q_from(Q, T);
unbind_q_from(_Q, []) ->
    ok.

-spec publish_reload_acls/0 :: () -> 'ok'.
publish_reload_acls() ->
    Defaults = wh_api:default_headers(<<"switch_event">>, wh_util:to_binary(?MODULE)),
    {ok, Payload} = wh_api:prepare_api_payload(Defaults, ?RELOAD_ACLS_VALUES, fun ?MODULE:reload_acls/1),
    amqp_util:configuration_publish(?RELOAD_ACLS_KEY, Payload, ?DEFAULT_CONTENT_TYPE).

-spec publish_reload_gateways/0 :: () -> 'ok'.
publish_reload_gateways() ->
    Defaults = wh_api:default_headers(<<"switch_event">>, wh_util:to_binary(?MODULE)),
    {ok, Payload} = wh_api:prepare_api_payload(Defaults, ?RELOAD_GATEWAYS_VALUES, fun ?MODULE:reload_gateways/1),
    amqp_util:configuration_publish(?RELOAD_GATEWAYS_KEY, Payload, ?DEFAULT_CONTENT_TYPE).
