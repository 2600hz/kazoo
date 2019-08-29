%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_callflow).
-include_lib("kazoo_stdlib/include/kz_types.hrl").

-export([api_definitions/0, api_definition/1]).

-export([resume/1, resume_v/1]).

-export([bind_q/2
        ,unbind_q/2
        ]).
-export([declare_exchanges/0]).
-export([publish_resume/1]).

-define(RESUME_ROUTING_KEY, <<"callflow.resume">>).

-type resume() :: kz_json:object().

-export_type([resume/0]).

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [resume_definition()].

%%------------------------------------------------------------------------------
%% @doc Get API definition of the given `Name'.
%% @see api_definitions/0
%% @end
%%------------------------------------------------------------------------------
-spec api_definition(kz_term:text()) -> kapi_definition:api().
api_definition(Name) when not is_binary(Name) ->
    api_definition(kz_term:to_binary(Name));
api_definition(<<"resume">>) ->
    resume_definition().

-spec resume_definition() -> kapi_definition:api().
resume_definition() ->
    EventName = <<"resume">>,
    Category = <<"callflow">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Resume Callflow">>}
              ,{fun kapi_definition:set_description/2, <<"Resume a Callflow's flow">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun resume/1}
              ,{fun kapi_definition:set_validate_fun/2, fun resume_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_resume/1}
              ,{fun kapi_definition:set_binding/2, ?RESUME_ROUTING_KEY}
              ,{fun kapi_definition:set_required_headers/2, [<<"Call">>
                                                            ,<<"Flow">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Resume a Callflow's flow.
%% Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec resume(kz_term:api_terms()) -> kz_api:api_formatter_return().
resume(Req) ->
    kapi_definition:build_message(Req, resume_definition()).

-spec resume_v(kz_term:api_terms()) -> boolean().
resume_v(Req) ->
    kapi_definition:validate(Req, resume_definition()).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Q, _Props) ->
    kz_amqp_util:bind_q_to_kapps(Q, ?RESUME_ROUTING_KEY).

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Q, _Props) ->
    kz_amqp_util:unbind_q_from_kapps(Q, ?RESUME_ROUTING_KEY).

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:kapps_exchange().

%%------------------------------------------------------------------------------
%% @doc Publish the JSON string to the proper Exchange.
%% @end
%%------------------------------------------------------------------------------
-spec publish_resume(kz_term:api_terms()) -> 'ok'.
publish_resume(JObj) ->
    Definition = resume_definition(),
    BuildFun = kapi_definition:build_fun(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(JObj
                                                ,kapi_definition:values(Definition)
                                                ,[{'formatter', BuildFun}
                                                 ,{'remove_recursive', 'false'}
                                                 ]
                                                ),
    kz_amqp_util:kapps_publish(kapi_definition:binding(Definition), Payload).
