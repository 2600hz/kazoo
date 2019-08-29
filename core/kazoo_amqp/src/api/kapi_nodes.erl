%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_nodes).

-export([api_definitions/0, api_definition/1]).

-export([advertise/1
        ,advertise_v/1
        ,publish_advertise/1
        ,publish_advertise/2
        ]).

-export([bind_q/2, unbind_q/1]).
-export([declare_exchanges/0]).

-include_lib("kz_amqp_util.hrl").

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [advertise_definition()].

%%------------------------------------------------------------------------------
%% @doc Get API definition of the given `Name'.
%% @see api_definitions/0
%% @end
%%------------------------------------------------------------------------------
-spec api_definition(kz_term:text()) -> kapi_definition:api().
api_definition(Name) when not is_binary(Name) ->
    api_definition(kz_term:to_binary(Name));
api_definition(<<"advertise">>) ->
    advertise_definition().

-spec advertise_definition() -> kapi_definition:api().
advertise_definition() ->
    EventName = <<"advertise">>,
    Category = <<"nodes">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Nodes advertise">>}
              ,{fun kapi_definition:set_description/2, <<"Nodes advertise">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun advertise/1}
              ,{fun kapi_definition:set_validate_fun/2, fun advertise_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_advertise/1}
              ,{fun kapi_definition:set_required_headers/2, [<<"Expires">>]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Channels">>
                                                            ,<<"Conferences">>
                                                            ,<<"Globals">>
                                                            ,<<"Media-Servers">>
                                                            ,<<"Node-Info">>
                                                            ,<<"Ports">>
                                                            ,<<"Processes">>
                                                            ,<<"Registrations">>
                                                            ,<<"Used-Memory">>
                                                            ,<<"Version">>
                                                            ,<<"WhApps">>
                                                            ,<<"Zone">>
                                                            ,<<"md5">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Nodes advertise.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec advertise(kz_term:api_terms()) -> kz_api:api_formatter_return().
advertise(Req) ->
    kapi_definition:build_message(Req, advertise_definition()).

-spec advertise_v(kz_term:api_terms()) -> boolean().
advertise_v(Req) ->
    kapi_definition:validate(Req, advertise_definition()).

%%------------------------------------------------------------------------------
%% @doc Prepare and publish a nodes advertise message.
%% @end
%%------------------------------------------------------------------------------
-spec publish_advertise(kz_term:api_terms()) -> 'ok'.
publish_advertise(JObj) ->
    publish_advertise(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_advertise(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_advertise(Advertise, ContentType) ->
    Definition = advertise_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Advertise
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:nodes_publish(Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Bind to a queue to this API exchange and events.
%% @end
%%------------------------------------------------------------------------------
-spec bind_q(binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, _Props) ->
    kz_amqp_util:bind_q_to_nodes(Queue).

%%------------------------------------------------------------------------------
%% @doc Unbind to a queue to this API exchange and events.
%% @end
%%------------------------------------------------------------------------------
-spec unbind_q(binary()) -> 'ok'.
unbind_q(Queue) ->
    kz_amqp_util:unbind_q_from_nodes(Queue).

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:nodes_exchange().
