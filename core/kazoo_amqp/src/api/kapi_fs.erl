%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc FreeSWITCH `pass-through' API.
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_fs).

-export([api_definitions/0, api_definition/1]).

-export([req/1, req_v/1]).
-export([declare_exchanges/0]).
-export([publish_req/2, publish_req/3]).

-include_lib("kz_amqp_util.hrl").


%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [req_definition()].

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
    EventName = <<"command">>,
    Category = <<"fs">>,
    %% The AMQP pass-through of FS commands - whitelist commands allowed (excluding any
    %% prefixed by uuid_ which are auto-allowed)
    CommandWhiteList = [<<"set">>, <<"hangup">>, <<"bridge">>],
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"FreeSWITCH Request">>}
              ,{fun kapi_definition:set_description/2
               ,<<"FreeSWITCH Request, Pass-through of FreeSWITCH dialplan commands">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_req/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>, <<"Args">>]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Insert-At">>]}
              ,{fun kapi_definition:set_values/2, kapi_definition:event_type_headers(Category, EventName)}
              ,{fun kapi_definition:set_types/2
               ,[{<<"Application-Name">>, fun(<<"uuid_", _/binary>>) -> 'true';
                                             (App) -> lists:member(App, CommandWhiteList)
                                          end}
                ]
               }
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc FreeSWITCH Request, Pass-through of FreeSWITCH dialplan commands.
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
%% @doc Prepare and publish a FreeSwitch request.
%% @end
%%------------------------------------------------------------------------------
-spec publish_req(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_req(Queue, JObj) ->
    publish_req(Queue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_req(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_req(Queue, Req, ContentType) ->
    Definition = req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:required_headers(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:callctl_publish(Queue, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:callctl_exchange().
