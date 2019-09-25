%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Handles authorization requests, responses, queue bindings.
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_rate).

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
-export([bind_q/2, unbind_q/2
        ,declare_exchanges/0
        ,broadcast_resp/1, broadcast_resp/2
        ]).

%% accessors
-export([account_id/1
        ,authorizing_type/1
        ,direction/1
        ,from_did/1
        ,options/1
        ,outbound_flags/1
        ,ratedeck_id/1
        ,resource_id/1
        ,send_empty/1
        ,to_did/1
        ]).

-type req()  :: kz_json:object().
-type resp() :: kz_json:object().

-export_type([req/0
             ,resp/0
             ]).

-include_lib("kz_amqp_util.hrl").

-define(EVENT_CATEGORY, <<"rate">>).
-define(KEY_RATE_BROADCAST, <<"rate.resp.broadcast">>).

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [req_definition()
    ,resp_definition()
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
    resp_definition().

-spec req_definition() -> kapi_definition:api().
req_definition() ->
    EventName = <<"req">>,
    Category = ?EVENT_CATEGORY,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Rating Request">>}
              ,{fun kapi_definition:set_description/2, <<"Rating Request">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_req/1}
              ,{fun kapi_definition:set_required_headers/2, [<<"To-DID">>]}
              ,{fun kapi_definition:set_binding/2, <<"rate.req">>}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Account-ID">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Direction">>
                                                            ,<<"From-DID">>
                                                            ,<<"Options">>
                                                            ,<<"Outbound-Flags">>
                                                            ,<<"Ratedeck-ID">>
                                                            ,<<"Resource-ID">>
                                                            ,<<"Resource-Type">>
                                                            ,<<"Send-Empty">>
                                                            ,<<"Authorizing-Type">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Direction">>, [<<"inbound">>, <<"outbound">>]}
                ,{<<"Resource-Type">>, [<<"audio">>, <<"video">>, <<"sms">>]}
                ] ++ kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Options">>, fun is_list/1}
                ,{<<"Send-Empty">>, fun kz_term:is_boolean/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec resp_definition() -> kapi_definition:api().
resp_definition() ->
    EventName = <<"resp">>,
    Category = ?EVENT_CATEGORY,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Rating Response">>}
              ,{fun kapi_definition:set_description/2, <<"Rating Response">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_resp/2}
              ,{fun kapi_definition:set_required_headers/2, []}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Base-Cost">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Discount-Percentage">>
                                                            ,<<"Prefix">>
                                                            ,<<"Pvt-Cost">>
                                                            ,<<"Rate">>
                                                            ,<<"Rate-ID">>
                                                            ,<<"Rate-Description">>
                                                            ,<<"Rate-Increment">>
                                                            ,<<"Rate-Minimum">>
                                                            ,<<"Rate-Name">>
                                                            ,<<"Rate-NoCharge-Time">>
                                                            ,<<"Rate-Version">>
                                                            ,<<"Ratedeck-ID">>
                                                            ,<<"Surcharge">>
                                                            ,<<"Update-Callee-ID">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Rate-Increment">>, fun is_integer/1}
                ,{<<"Rate-NoCharge-Time">>, fun is_integer/1}
                ,{<<"Update-Callee-ID">>, fun kz_term:is_boolean/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Authorization Request.
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
    kz_amqp_util:callmgr_publish(Payload, ContentType, kapi_definition:binding(Definition)).

%%------------------------------------------------------------------------------
%% @doc Authorization Response.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec resp(kz_term:api_terms()) -> kz_api:api_formatter_return().
resp(Req) ->
    kapi_definition:build_message(Req, resp_definition()).

-spec resp_v(kz_term:api_terms()) -> boolean().
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
%% @doc Setup and tear down bindings for rate `gen_listeners'.
%% @end
%%------------------------------------------------------------------------------
-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    bind_to_q(Queue, props:get_value('restrict_to', Props)).

bind_to_q(Q, 'undefined') ->
    'ok' = kz_amqp_util:bind_q_to_callmgr(Q, kapi_definition:binding(req_definition()));
bind_to_q(Q, ['req'|T]) ->
    'ok' = kz_amqp_util:bind_q_to_callmgr(Q, kapi_definition:binding(req_definition())),
    bind_to_q(Q, T);
bind_to_q(Q, ['broadcast'|T]) ->
    'ok' = kz_amqp_util:bind_q_to_callmgr(Q, ?KEY_RATE_BROADCAST),
    bind_to_q(Q, T);
bind_to_q(Q, [_|T]) ->
    bind_to_q(Q, T);
bind_to_q(_Q, []) ->
    'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    unbind_q_from(Q, props:get_value('restrict_to', Props)).

unbind_q_from(Q, 'undefined') ->
    'ok' = kz_amqp_util:unbind_q_from_callmgr(Q, kapi_definition:binding(req_definition()));
unbind_q_from(Q, ['req'|T]) ->
    'ok' = kz_amqp_util:unbind_q_from_callmgr(Q, kapi_definition:binding(req_definition())),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['broadcast'|T]) ->
    'ok' = kz_amqp_util:unbind_q_from_callmgr(Q, ?KEY_RATE_BROADCAST),
    unbind_q_from(Q, T);
unbind_q_from(Q, [_|T]) ->
    unbind_q_from(Q, T);
unbind_q_from(_Q, []) ->
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:callmgr_exchange().

%%------------------------------------------------------------------------------
%% @doc Publish the JSON string to the proper Exchange.
%% @end
%%------------------------------------------------------------------------------

-spec broadcast_resp(kz_term:api_terms()) -> 'ok'.
broadcast_resp(JObj) ->
    broadcast_resp(JObj, ?DEFAULT_CONTENT_TYPE).

-spec broadcast_resp(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
broadcast_resp(Resp, ContentType) ->
    Definition = resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Resp
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:callmgr_publish(Payload, ContentType, ?KEY_RATE_BROADCAST).

-spec to_did(req()) -> kz_term:ne_binary().
to_did(Req) ->
    kz_json:get_ne_binary_value(<<"To-DID">>, Req).

-spec from_did(req()) -> kz_term:api_ne_binary().
from_did(Req) ->
    kz_json:get_ne_binary_value(<<"From-DID">>, Req).

-spec account_id(req()) -> kz_term:api_ne_binary().
account_id(Req) ->
    kz_json:get_ne_binary_value(<<"Account-ID">>, Req).

-spec ratedeck_id(req()) -> kz_term:api_ne_binary().
ratedeck_id(Req) ->
    kz_json:get_ne_binary_value(<<"Ratedeck-ID">>, Req).

-spec direction(req()) -> kz_term:api_ne_binary().
direction(Req) ->
    kz_json:get_ne_binary_value(<<"Direction">>, Req).

-spec authorizing_type(req()) -> kz_term:api_ne_binary().
authorizing_type(Req) ->
    kz_json:get_ne_binary_value(<<"Authorizing-Type">>, Req).

-spec send_empty(req()) -> boolean().
send_empty(Req) ->
    kz_json:is_true(<<"Send-Empty">>, Req, 'false').

-spec options(req()) -> kz_term:ne_binaries().
options(Req) ->
    kz_json:get_list_value(<<"Options">>, Req, []).

-spec outbound_flags(req()) -> kz_term:ne_binaries().
outbound_flags(Req) ->
    kz_json:get_list_value(<<"Outbound-Flags">>, Req, []).

-spec resource_id(req()) -> kz_term:api_ne_binary().
resource_id(Req) ->
    kz_json:get_ne_binary_value(<<"Resource-ID">>, Req).
