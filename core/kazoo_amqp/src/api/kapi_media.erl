%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Media requests, responses, and errors.
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_media).

-compile({no_auto_import, [error/1]}).

-export([api_definitions/0, api_definition/1]).

-export([req/1
        ,req_v/1
        ,publish_req/1
        ,publish_req/2
        ]).
-export([resp/1
        ,resp_v/1
        ,publish_resp/2
        ,publish_resp/3
        ]).
-export([error/1
        ,error_v/1
        ,publish_error/2
        ,publish_error/3
        ]).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-include_lib("kz_amqp_util.hrl").

-define(MEDIA_REQ_ROUTING_KEY, <<"media_req">>).

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

-spec req_definition() -> kapi_definition:api().
req_definition() ->
    EventName = <<"media_req">>,
    Category = <<"media">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Media Request">>}
              ,{fun kapi_definition:set_description/2, <<"Media Request - when streaming is needed">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_req/1}
              ,{fun kapi_definition:set_binding/2, ?MEDIA_REQ_ROUTING_KEY}
              ,{fun kapi_definition:set_required_headers/2, [<<"Media-Name">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Stream-Type">>
                                                            ,<<"Call-ID">>
                                                                 %% TTS-related flags
                                                            ,<<"Text">>
                                                            ,<<"Voice">>
                                                            ,<<"Language">>
                                                            ,<<"Format">>
                                                            ,<<"Account-ID">>
                                                            ,<<"Protocol">>
                                                            ,<<"Engine">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Event-Category">>, Category}
                ,{<<"Event-Name">>, EventName}
                ,{<<"Stream-Type">>, [<<"new">>, <<"extant">>]}
                ,{<<"Voice">>, [<<"male">>, <<"female">>]}
                ,{<<"Format">>, [<<"mp3">>, <<"wav">>]}
                ,{<<"Protocol">>, [<<"http">>, <<"https">>, <<"shout">>, <<"vlc">>]}
                ]
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec resp_definition() -> kapi_definition:api().
resp_definition() ->
    EventName = <<"media_resp">>,
    Category = <<"media">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Media Response">>}
              ,{fun kapi_definition:set_description/2, <<"Media Response">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_resp/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Media-Name">>
                                                            ,<<"Stream-URL">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Stream-URL">>, fun(<<"shout://", _/binary>>) -> 'true';
                                       (<<"http://", _/binary>>) -> 'true';
                                       (<<"vlc://", _/binary>>) -> 'true';
                                       (_) -> 'false'
                                    end}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec error_definition() -> kapi_definition:api().
error_definition() ->
    EventName = <<"media_error">>,
    Category = <<"media">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Media Error">>}
              ,{fun kapi_definition:set_description/2, <<"Media Error">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun error/1}
              ,{fun kapi_definition:set_validate_fun/2, fun error_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_error/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Media-Name">>
                                                            ,<<"Error-Code">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Error-Msg">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Event-Category">>, Category}
                ,{<<"Event-Name">>, EventName}
                ,{<<"Error-Code">>, [<<"not_found">>, <<"no_data">>, <<"other">>]}
                ]
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Request media.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec req(kz_term:api_terms()) -> kz_api:api_formatter_return().
req(Req) ->
    kapi_definition:build_message(Req, req_definition()).

-spec req_v(kz_term:api_terms()) -> boolean().
req_v(Req) ->
    kapi_definition:validate(Req, req_definition()).

-spec publish_req(kz_term:api_terms()) -> 'ok'.
publish_req(JObj) ->
    publish_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_req(Req, ContentType) ->
    Definition = req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:kapps_publish(kapi_definition:binding(Definition), Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Response with media.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec resp(kz_json:object() | kz_term:proplist()) -> kz_api:api_formatter_return().
resp(Req) ->
    kapi_definition:build_message(Req, resp_definition()).

-spec resp_v(kz_term:proplist() | kz_json:object()) -> boolean().
resp_v(Req) ->
    kapi_definition:validate(Req, resp_definition()).

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
%% @doc Media error.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec error(kz_term:proplist() | kz_json:object()) -> kz_api:api_formatter_return().
error(Req) ->
    kapi_definition:build_message(Req, error_definition()).

-spec error_v(kz_term:proplist() | kz_json:object()) -> boolean().
error_v(Req) ->
    kapi_definition:validate(Req, error_definition()).

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

%% Bind and UnBind queue
-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, _Props) ->
    kz_amqp_util:bind_q_to_kapps(Queue, ?MEDIA_REQ_ROUTING_KEY).

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, _Props) ->
    kz_amqp_util:unbind_q_from_kapps(Queue, ?MEDIA_REQ_ROUTING_KEY).

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:kapps_exchange().
