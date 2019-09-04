%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Pivot API.
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_pivot).

-include("kz_amqp_util.hrl").

-export([api_definitions/0, api_definition/1]).

-export([req/1
        ,req_v/1
        ,publish_req/1
        ,publish_req/2
        ]).

-export([failed/1
        ,failed_v/1
        ,publish_failed/2
        ]).

-export([processing/1
        ,processing_v/1
        ,publish_processing/2
        ]).

-export([bind_q/2
        ,unbind_q/2
        ,declare_exchanges/0
        ]).

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [req_definition()
    ,failed_definition()
    ,processing_definition()
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
api_definition(<<"failed">>) ->
    failed_definition();
api_definition(<<"processing">>) ->
    processing_definition().

-spec req_definition() -> kapi_definition:api().
req_definition() ->
    EventName = <<"pivot_req">>,
    Category = <<"dialplan">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Dialplan Pivot Request">>}
              ,{fun kapi_definition:set_description/2, <<"Dialplan Pivot Request">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_req/1}
              ,{fun kapi_definition:set_binding/2, <<"pivot.req">>}
              ,{fun kapi_definition:set_required_headers/2, [<<"Call">>
                                                            ,<<"Voice-URI">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"CDR-URI">>
                                                            ,<<"Request-Format">>
                                                            ,<<"Request-Body-Format">>
                                                            ,<<"HTTP-Method">>
                                                            ,<<"Debug">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Call">>, fun kz_json:is_json_object/1}
                ,{<<"Debug">>, fun kz_term:is_boolean/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec failed_definition() -> kapi_definition:api().
failed_definition() ->
    EventName = <<"failed">>,
    Category = <<"pivot">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Pivot Failed">>}
              ,{fun kapi_definition:set_description/2, <<"Pivot Failed">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun failed/1}
              ,{fun kapi_definition:set_validate_fun/2, fun failed_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_failed/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Call-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec processing_definition() -> kapi_definition:api().
processing_definition() ->
    EventName = <<"processing">>,
    Category = <<"pivot">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Pivot Processing">>}
              ,{fun kapi_definition:set_description/2, <<"Pivot Processing">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun processing/1}
              ,{fun kapi_definition:set_validate_fun/2, fun processing_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_processing/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Call-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%% REQ
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
    kz_amqp_util:callmgr_publish(Payload, ContentType, get_pivot_req_routing(Req)).

%% FAILED
-spec failed(kz_term:api_terms()) -> kz_api:api_formatter_return().
failed(Req) ->
    kapi_definition:build_message(Req, failed_definition()).

-spec failed_v(kz_term:api_terms()) -> boolean().
failed_v(Req) ->
    kapi_definition:validate(Req, failed_definition()).

-spec publish_failed(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_failed(Target, JObj) ->
    Definition = failed_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(JObj
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(Target, Payload).

%% PROCESSING
-spec processing(kz_term:api_terms()) -> kz_api:api_formatter_return().
processing(Req) ->
    kapi_definition:build_message(Req, processing_definition()).

-spec processing_v(kz_term:api_terms()) -> boolean().
processing_v(Req) ->
    kapi_definition:validate(Req, processing_definition()).

-spec publish_processing(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_processing(Target, JObj) ->
    Definition = processing_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(JObj
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(Target, Payload).

%% Bind and UnBind
-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    kz_amqp_util:bind_q_to_callmgr(Queue, get_pivot_req_routing(Realm)).

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    kz_amqp_util:unbind_q_from_callmgr(Queue, get_pivot_req_routing(Realm)).

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:callmgr_exchange().

get_pivot_req_routing(Realm) when is_binary(Realm) ->
    list_to_binary([kapi_definition:binding(req_definition())
                   ,"."
                   ,kz_amqp_util:encode(Realm)
                   ]);
get_pivot_req_routing(Api) ->
    get_pivot_req_routing(get_from_realm(Api)).

-spec get_from_realm(kz_term:api_terms()) -> kz_term:ne_binary().
get_from_realm(Prop) when is_list(Prop) ->
    kz_json:get_value(<<"From-Realm">>, props:get_value(<<"Call">>, Prop));
get_from_realm(JObj) ->
    kz_json:get_value([<<"Call">>, <<"From-Realm">>], JObj).
