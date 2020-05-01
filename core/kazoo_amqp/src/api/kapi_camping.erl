%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Routing requests, responses, and wins!
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_camping).

-export([api_definitions/0, api_definition/1]).

-export([req/1
        ,req_v/1
        ,publish_req/1
        ,publish_req/2
        ]).

-export([bind_q/2
        ,unbind_q/2
        ,declare_exchanges/0
        ]).

-include_lib("kz_amqp_util.hrl").

-type req() :: kz_term:api_terms().

-export_type([req/0]).

-ifdef(TEST).
-export([build_binding/1
        ]).
-endif.

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [req_definition()
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
    req_definition().

-spec req_definition() -> kapi_definition:api().
req_definition() ->
    EventName = <<"request">>,
    Category = <<"camping">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Camping Request">>}
              ,{fun kapi_definition:set_description/2, <<"Camping Request">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_req/1}
              ,{fun kapi_definition:set_binding/2, fun build_binding/1}
              ,{fun kapi_definition:set_required_headers/2, [<<"Camping-Request">>
                                                            ,<<"Account-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Camping-Request">>, fun kz_json:is_json_object/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Camping Request
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

-spec publish_req(kz_term:api_terms(), binary()) -> 'ok'.
publish_req(Req, ContentType) ->
    Definition = req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:callmgr_publish(Payload
                                ,ContentType
                                ,(kapi_definition:binding(Definition))(get_account_id(Req))
                                ).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist() | kz_term:ne_binary()) -> 'ok'.
bind_q(Queue, Props) when is_list(Props) ->
    bind_q(Queue, props:get_value('account_id', Props, <<"*">>));
bind_q(Queue, ?NE_BINARY=AccountId) ->
    kz_amqp_util:bind_q_to_callmgr(Queue
                                  ,(kapi_definition:binding(req_definition()))(AccountId)
                                  ).

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist() | kz_term:ne_binary()) -> 'ok'.
unbind_q(Queue, Props) when is_list(Props) ->
    bind_q(Queue, props:get_value('account_id', Props, <<"*">>));
unbind_q(Queue, ?NE_BINARY=AccountId) ->
    kz_amqp_util:unbind_q_from_callmgr(Queue
                                      ,(kapi_definition:binding(req_definition()))(AccountId)
                                      ).

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:callmgr_exchange().

-spec get_account_id(kz_term:api_terms()) -> kz_term:ne_binary().
get_account_id(Props) when is_list(Props) ->
    props:get_value(<<"Account-ID">>, Props);
get_account_id(JObj) ->
    kz_json:get_value(<<"Account-ID">>, JObj).

-spec build_binding(kz_term:ne_binary()) -> kz_term:ne_binary().
build_binding(AccountId) ->
    <<"camping.request.", (AccountId)/binary>>.
