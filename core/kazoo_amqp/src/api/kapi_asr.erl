%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc ASR requests, responses, and errors AMQP API.
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_asr).

-compile({no_auto_import, [error/1]}).

-export([api_definitions/0, api_definition/1]).

-export([bind_q/2, unbind_q/1]).
-export([declare_exchanges/0]).

-export([req/1, req_v/1]).
-export([resp/1, resp_v/1]).
-export([error/1, error_v/1]).
-export([publish_req/1, publish_req/2]).
-export([publish_resp/2, publish_resp/3]).
-export([publish_error/2, publish_error/3]).

-include_lib("kz_amqp_util.hrl").

-define(CATEGORY, <<"asr">>).
-define(KEY_ASR_REQ, <<"asr.req">>).

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [req_definition()
    ,resp_definition()
    ,error_definition()
    ].

%%------------------------------------------------------------------------------
%% @doc Get API definition of the given `Name'.
%% @see api_definitions/0
%% @end
%%------------------------------------------------------------------------------
-spec api_definition(kz_term:text()) -> kapi_definition:api().
api_definition(Name) when not is_binary(Name) ->
    api_definition(kz_term:to_binary(Name));
api_definition(<<"req">>) ->
    req_definition();
api_definition(<<"resp">>) ->
    resp_definition();
api_definition(<<"error">>) ->
    error_definition().

%%------------------------------------------------------------------------------
%% @doc Request ASR.
%% @end
%%------------------------------------------------------------------------------
-spec req_definition() -> kapi_definition:api().
req_definition() ->
    EventName = <<"req">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"ASR Req">>}
              ,{fun kapi_definition:set_description/2, <<"A request for ASR services">>}
              ,{fun kapi_definition:set_category/2, ?CATEGORY}
              ,{fun kapi_definition:set_build_fun/2, fun req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_req/1}
              ,{fun kapi_definition:set_binding/2, ?KEY_ASR_REQ}
              ,{fun kapi_definition:set_required_headers/2, [<<"ASR-Endpoint">>
                                                            ,<<"ASR-Account-ID">>
                                                            ,<<"ASR-Account-Password">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Control-Queue">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Language">>
                                                            ,<<"Stream-Response">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(?CATEGORY, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec resp_definition() -> kapi_definition:api().
resp_definition() ->
    EventName = <<"resp">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"ASR Response">>}
              ,{fun kapi_definition:set_description/2, <<"An ASR Response with detected text">>}
              ,{fun kapi_definition:set_category/2, ?CATEGORY}
              ,{fun kapi_definition:set_build_fun/2, fun resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_resp/2}
              ,{fun kapi_definition:set_required_headers/2, []}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Response-Text">>]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(?CATEGORY, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec error_definition() -> kapi_definition:api().
error_definition() ->
    EventName = <<"error">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"ASR Error">>}
              ,{fun kapi_definition:set_description/2, <<"An ASR Error when converting speech to text">>}
              ,{fun kapi_definition:set_category/2, ?CATEGORY}
              ,{fun kapi_definition:set_build_fun/2, fun error/1}
              ,{fun kapi_definition:set_validate_fun/2, fun error_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_error/2}
              ,{fun kapi_definition:set_required_headers/2, []}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Error-Code">>
                                                            ,<<"Error-Msg">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(?CATEGORY, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Send request ASR.
%% Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec req(kz_term:api_terms()) -> kz_api:api_formatter_return().
req(Req) ->
    kapi_definition:build_message(Req, req_definition()).

-spec req_v(kz_term:api_terms()) -> boolean().
req_v(Req) ->
    kapi_definition:validate(Req, req_definition()).

%%------------------------------------------------------------------------------
%% @doc Prepare and publish an ASR request.
%% @end
%%------------------------------------------------------------------------------
-spec publish_req(kz_term:api_terms()) -> 'ok'.
publish_req(JObj) ->
    publish_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_req(Req, ContentType) ->
    Definition = resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:callctl_publish(kapi_definition:binding(Definition), Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Response with ASR.
%% Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec resp(kz_term:api_terms()) -> kz_api:api_formatter_return().
resp(Req) ->
    kapi_definition:build_message(Req, resp_definition()).

-spec resp_v(kz_term:api_terms()) -> boolean().
resp_v(Req) ->
    kapi_definition:validate(Req, resp_definition()).

%%------------------------------------------------------------------------------
%% @doc Prepare and publish an ASR response.
%% @end
%%------------------------------------------------------------------------------
-spec publish_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_resp(Queue, JObj) ->
    publish_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_resp(Queue, Resp, ContentType) ->
    Definition = resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Resp
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(Queue, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Asr error.
%% Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec error(kz_term:api_terms()) -> kz_api:api_formatter_return().
error(Req) ->
    kapi_definition:build_message(Req, error_definition()).

-spec error_v(kz_term:api_terms()) -> boolean().
error_v(Req) ->
    kapi_definition:validate(Req, error_definition()).

%%------------------------------------------------------------------------------
%% @doc Prepare and publish an ASR error.
%% @end
%%------------------------------------------------------------------------------
-spec publish_error(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_error(Queue, JObj) ->
    publish_error(Queue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_error(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_error(Queue, Error, ContentType) ->
    Definition = error_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Error
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(Queue, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Bind to a queue to the ASR exchange and events.
%% @end
%%------------------------------------------------------------------------------
-spec bind_q(binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, _Props) ->
    kz_amqp_util:bind_q_to_callctl(Queue, ?KEY_ASR_REQ).

%%------------------------------------------------------------------------------
%% @doc Unbind from a queue to the ASR exchange and events.
%% @end
%%------------------------------------------------------------------------------
-spec unbind_q(binary()) -> 'ok'.
unbind_q(Queue) ->
    kz_amqp_util:unbind_q_from_callctl(Queue).

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:callctl_exchange().
