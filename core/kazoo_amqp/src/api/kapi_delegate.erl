%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc Delegate JObj from one application to another application.
%%% App/Key combo used to send messages.
%%%
%%%
%%% @author SIPLABS LLC (Maksim Krzhemenevskiy)
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_delegate).

-export([api_definitions/0, api_definition/1]).

-export([delegate/1
        ,delegate_v/1
        ,publish_delegate/2
        ,publish_delegate/3
        ,publish_delegate/4
        ]).

-export([bind_q/2
        ,unbind_q/2
        ]).
-export([declare_exchanges/0]).

-include_lib("kz_amqp_util.hrl").

-type maybe_key() :: kz_term:api_ne_binary().

-ifdef(TEST).
-export([build_binding/2
        ]).
-endif.

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [delegate_definition()
    ].

%%------------------------------------------------------------------------------
%% @doc Get API definition of the given `Name'.
%% @see api_definitions/0
%% @end
%%------------------------------------------------------------------------------
-spec api_definition(kz_term:text()) -> kapi_definition:api().
api_definition(Name) when not is_binary(Name) ->
    api_definition(kz_term:to_binary(Name));
api_definition(<<"delegate">>) ->
    delegate_definition().

-spec delegate_definition() -> kapi_definition:api().
delegate_definition() ->
    EventName = <<"job">>,
    Category = <<"delegate">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Delegate Job">>}
              ,{fun kapi_definition:set_description/2
               ,<<"Delegate JObj from one application to another application">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun delegate/1}
              ,{fun kapi_definition:set_validate_fun/2, fun delegate_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_delegate/2}
              ,{fun kapi_definition:set_binding/2, fun build_binding/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Delegate-Message">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Delegate JObj from one application to another application.
%% Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec delegate(kz_term:api_terms()) -> kz_api:api_formatter_return().
delegate(Req) ->
    kapi_definition:build_message(Req, delegate_definition()).

-spec delegate_v(kz_term:api_terms()) -> boolean().
delegate_v(Req) ->
    kapi_definition:validate(Req, delegate_definition()).

-spec publish_delegate(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_delegate(TargetApp, API) ->
    publish_delegate(TargetApp, API, 'undefined').

-spec publish_delegate(kz_term:ne_binary(), kz_term:api_terms(), maybe_key()) -> 'ok'.
publish_delegate(TargetApp, API, Key) ->
    publish_delegate(TargetApp, API, Key, ?DEFAULT_CONTENT_TYPE).

-spec publish_delegate(kz_term:ne_binary(), kz_term:api_terms(), maybe_key(), binary()) -> 'ok'.
publish_delegate(<<_/binary>> = TargetApp, API, Key, ContentType) ->
    Definition = delegate_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:kapps_publish((kapi_definition:binding(Definition))(TargetApp, Key)
                              ,Payload
                              ,ContentType
                              ).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Q, Props) ->
    App = props:get_binary_value('app_name', Props),
    Key = props:get_value('route_key', Props),
    bind_q(Q, App, Key).

-spec bind_q(kz_term:ne_binary(), kz_term:ne_binary(), maybe_key()) -> 'ok'.
bind_q(Q, <<_/binary>> = App, Key) ->
    kz_amqp_util:bind_q_to_kapps(Q
                                ,(kapi_definition:binding(delegate_definition()))(App, Key)
                                ).

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    App = props:get_binary_value('app_name', Props),
    Key = props:get_value('route_key', Props),
    unbind_q(Q, App, Key).

-spec unbind_q(kz_term:ne_binary(), kz_term:ne_binary(), maybe_key()) -> 'ok'.
unbind_q(Q, <<_/binary>> = App, Key) ->
    kz_amqp_util:unbind_q_from_kapps(Q
                                    ,(kapi_definition:binding(delegate_definition()))(App, Key)
                                    ).

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:kapps_exchange().

-spec build_binding(kz_term:ne_binary(), maybe_key()) -> kz_term:ne_binary().
build_binding(App, 'undefined') ->
    <<"delegate.", (kz_amqp_util:encode(App))/binary>>;
build_binding(App, Key) ->
    <<"delegate.", (kz_amqp_util:encode(App))/binary, ".", (kz_amqp_util:encode(Key))/binary>>.
