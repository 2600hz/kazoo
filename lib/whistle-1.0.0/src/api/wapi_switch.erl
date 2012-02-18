%%%-------------------------------------------------------------------
%%% @author Edouard Swiac <edouard@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Switch events messages
%%% @end
%%%-------------------------------------------------------------------
-module(wapi_switch).

-export([reloadacl_req/1, reloadacl_req_v/1]).
-export([bind_q/2, unbind_q/2]).
-export([publish_reloadacl_req/2, publish_reloadacl_req/3]).

-include("../wh_api.hrl").

-define(SWITCH_EVENT_VALUES, [{<<"Event-Category">>, <<"switch_event">>}]).

%% request to reload acl
-define(SWITCH_EVENT_RELOADACL_REQ_HEADERS, []).
-define(OPTIONAL_SWITCH_EVENT_RELOADACL_REQ_HEADERS, []).
-define(SWITCH_EVENT_RELOADACL_REQ_VALUES, [{<<"Event-Name">>, <<"reloadacl">>} | ?SWITCH_EVENT_VALUES]).
-define(SWITCH_EVENT_TYPES, []).
%% Request a reloadacl
-spec reloadacl_req/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
reloadacl_req(Prop) when is_list(Prop) ->
    case reloadacl_req_v(Prop) of
        true -> wh_api:build_message(Prop, ?SWITCH_EVENT_RELOADACL_REQ_HEADERS, ?OPTIONAL_SWITCH_EVENT_RELOADACL_REQ_HEADERS);
        false -> {error, "Proplist failed validation for switch event reloadacl req"}
    end;
reloadacl_req(JObj) ->
    reloadacl_req(wh_json:to_proplist(JObj)).

-spec reloadacl_req_v/1 :: (api_terms()) -> boolean().
reloadacl_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?SWITCH_EVENT_RELOADACL_REQ_HEADERS, ?SWITCH_EVENT_RELOADACL_REQ_VALUES, ?SWITCH_EVENT_TYPES);
reloadacl_req_v(JObj) ->
    reloadacl_req_v(wh_json:to_proplist(JObj)).

-spec bind_q/2 :: (binary(), proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    SwitchId = props:get_value(switchid, Props, <<"*">>),
    amqp_util:configuration_exchange(),
    bind_q(Queue, props:get_value(restrict_to, Props), SwitchId).

bind_q(Q, undefined, SwitchId) ->
    amqp_util:bind_q_to_configuration(Q, SwitchId);

bind_q(Q, [relodacl|T], SwitchId) ->
    amqp_util:bind_q_to_configuration(Q, SwitchId),
    bind_q(Q, T, SwitchId);
bind_q(Q, [_|T], SwitchId) ->
    bind_q(Q, T, SwitchId);
bind_q(_Q, [], _SwitchId) ->
    ok.

-spec unbind_q/2 :: (ne_binary(), proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    SwitchId = props:get_value(switchid, Props, <<"*">>),
    unbind_q(Queue, props:get_value(restrict_to, Props), SwitchId).

unbind_q(Q, undefined, _SwitchId) ->
    amqp_util:unbind_q_from_configuration(Q);
unbind_q(Q, [reloadacl|T], SwitchId) ->
    amqp_util:unbind_q_from_configuration(Q, SwitchId),
    unbind_q(Q, T, SwitchId);
unbind_q(Q, [_|T], SwitchId) ->
    unbind_q(Q, T, SwitchId);
unbind_q(_Q, [], _SwitchId) ->
    ok.

-spec publish_reloadacl_req/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_reloadacl_req(Q, JObj) ->
    publish_reloadacl_req(Q, JObj, ?DEFAULT_CONTENT_TYPE).
publish_reloadacl_req(Q, JObj, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(JObj, ?SWITCH_EVENT_RELOADACL_REQ_VALUES, fun ?MODULE:reloadacl_req/1),
    amqp_util:targeted_publish(Q, Payload, ContentType).
