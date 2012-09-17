%%%-------------------------------------------------------------------
%%% @author Edouard Swiac <edouard@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Switch events messages
%%% @end
%%%-------------------------------------------------------------------
-module(wapi_switch).

-export([reloadacl/1, reloadacl_v/1]).
-export([bind_q/2, unbind_q/2]).
-export([publish_reloadacl/0]).

-include_lib("wh_api.hrl").

-define(SWITCH_EVENT_VALUES, [{<<"Event-Category">>, <<"switch_event">>}]).

%% request to reload acl
-define(SWITCH_EVENT_RELOADACL_HEADERS, []).
-define(OPTIONAL_SWITCH_EVENT_RELOADACL_HEADERS, []).
-define(SWITCH_EVENT_RELOADACL_VALUES, [{<<"Event-Name">>, <<"reloadacl">>} | ?SWITCH_EVENT_VALUES]).
-define(SWITCH_EVENT_TYPES, []).
-define(SWITCH_RELOADACL_KEY, <<"switch.reloadacl">>).

%% Request a reloadacl
-spec reloadacl/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
reloadacl(Prop) when is_list(Prop) ->
    case reloadacl_v(Prop) of
        true -> wh_api:build_message(Prop, ?SWITCH_EVENT_RELOADACL_HEADERS, ?OPTIONAL_SWITCH_EVENT_RELOADACL_HEADERS);
        false -> {error, "Proplist failed validation for switch event reloadacl req"}
    end;
reloadacl(JObj) ->
    reloadacl(wh_json:to_proplist(JObj)).

-spec reloadacl_v/1 :: (api_terms()) -> boolean().
reloadacl_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?SWITCH_EVENT_RELOADACL_HEADERS, ?SWITCH_EVENT_RELOADACL_VALUES, ?SWITCH_EVENT_TYPES);
reloadacl_v(JObj) ->
    reloadacl_v(wh_json:to_proplist(JObj)).

-spec bind_q/2 :: (binary(), proplist()) -> 'ok'.
bind_q(Queue, _Props) ->
    amqp_util:configuration_exchange(),
    amqp_util:bind_q_to_configuration(Queue, ?SWITCH_RELOADACL_KEY).

-spec unbind_q/2 :: (ne_binary(), proplist()) -> 'ok'.
unbind_q(Queue, _Props) ->
    amqp_util:unbind_q_from_configuration(Queue, ?SWITCH_RELOADACL_KEY).

-spec publish_reloadacl/0 :: () -> 'ok'.
publish_reloadacl() ->
    {ok, Payload} = wh_api:prepare_api_payload(wh_api:default_headers(<<"switch_event">>, <<"reloadacl">>)
                                               ,?SWITCH_EVENT_RELOADACL_VALUES
                                               ,fun ?MODULE:reloadacl/1
                                              ),
    amqp_util:configuration_publish(?SWITCH_RELOADACL_KEY, Payload, ?DEFAULT_CONTENT_TYPE).

