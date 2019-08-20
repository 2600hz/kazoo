%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_conf_participant).

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

-spec dialplan_req(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
dialplan_req(Prop) when is_list(Prop) ->
    case dialplan_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?DIALPLAN_REQ_HEADERS, ?OPTIONAL_DIALPLAN_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for dialplan req"}
    end;
dialplan_req(JObj) ->
    dialplan_req(kz_json:to_proplist(JObj)).

dialplan_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?DIALPLAN_REQ_HEADERS, ?DIALPLAN_REQ_VALUES, ?DIALPLAN_REQ_TYPES);
dialplan_req_v(JObj) ->
    dialplan_req_v(kz_json:to_proplist(JObj)).

-spec bind_q(any(), any()) -> 'ok'.
bind_q(_, _) -> 'ok'.

-spec unbind_q(any(), any()) -> 'ok'.
unbind_q(_, _) -> 'ok'.

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:targeted_exchange().

%%------------------------------------------------------------------------------
%% @doc Publish to the participant
%% @end
%%------------------------------------------------------------------------------

-spec publish_dialplan_req(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_dialplan_req(Queue, JObj) ->
    publish_dialplan_req(Queue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_dialplan_req(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_dialplan_req(Queue, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?DIALPLAN_REQ_VALUES, fun dialplan_req/1),
    kz_amqp_util:targeted_publish(Queue, Payload, ContentType).
