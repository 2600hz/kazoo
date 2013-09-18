%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wapi_conf_participant).

-include("conference.hrl").

-export([dialplan_req/1]).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-export([publish_dialplan_req/2, publish_dialplan_req/3]).

-define(DIALPLAN_REQ_HEADERS, [<<"Dialplan-Command">>]).
-define(OPTIONAL_DIALPLAN_REQ_HEADERS, [<<"Conference-ID">>, <<"Participant-ID">>]).
-define(DIALPLAN_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                              ,{<<"Event-Name">>, <<"dialplan_req">>}
                             ]).
-define(DIALPLAN_REQ_TYPES, []).

dialplan_req(Prop) when is_list(Prop) ->
    case dialplan_req_v(Prop) of
        true -> wh_api:build_message(Prop, ?DIALPLAN_REQ_HEADERS, ?OPTIONAL_DIALPLAN_REQ_HEADERS);
        false -> {error, "Proplist failed validation for dialplan req"}
    end;
dialplan_req(JObj) ->
    dialplan_req(wh_json:to_proplist(JObj)).

dialplan_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?DIALPLAN_REQ_HEADERS, ?DIALPLAN_REQ_VALUES, ?DIALPLAN_REQ_TYPES);
dialplan_req_v(JObj) ->
    dialplan_req_v(wh_json:to_proplist(JObj)).

bind_q(_, _) -> 'ok'.

unbind_q(_, _) -> 'ok'.

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:targeted_exchange().

%%--------------------------------------------------------------------
%% @doc
%% Publish to the participant
%% @end
%%--------------------------------------------------------------------
-spec publish_dialplan_req(ne_binary(), api_terms()) -> 'ok'.
-spec publish_dialplan_req(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_dialplan_req(Queue, JObj) ->
    publish_dialplan_req(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_dialplan_req(Queue, Req, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Req, ?DIALPLAN_REQ_VALUES, fun ?MODULE:dialplan_req/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).
