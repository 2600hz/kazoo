%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Handles authentication requests, responses, queue bindings AMQP API.
%%% @author James Aimonetti
%%% @author Luis Azedo
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_authn).

-compile({'no_auto_import', [error/1]}).

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
-export([token_req/1
        ,token_req_v/1
        ,token_resp/1
        ,token_resp_v/1
        ,publish_token_req/1
        ,publish_token_resp/2
        ]).

-export([bind_q/2, unbind_q/2
        ,declare_exchanges/0
        ,get_auth_user/1, get_auth_realm/1
        ,req_event_type/0
        ]).

-include_lib("kz_amqp_util.hrl").

-type token_req() :: kz_term:api_terms().

-export_type([token_req/0]).

-define(KEY_AUTHN_REQ, <<"authn.req">>). %% corresponds to the authn_req/1 api call
-define(KEY_AUTHN_TOKEN_REQ, <<"authn.token_req">>). %% corresponds to the authn_token_req/1 api call

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [req_definition()
    ,resp_definition()
    ,token_req_definition()
    ,token_resp_definition()
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
api_definition(<<"token_req">>) ->
    token_req_definition();
api_definition(<<"token_resp">>) ->
    token_resp_definition();
api_definition(<<"error">>) ->
    error_definition().

-spec req_definition() -> kapi_definition:api().
req_definition() ->
    EventName = <<"authn_req">>,
    Category = <<"directory">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Authentication Request">>}
              ,{fun kapi_definition:set_description/2, <<"Authentication Request">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_req/1}
              ,{fun kapi_definition:set_required_headers/2
               ,[<<"Auth-Realm">>
                ,<<"Auth-User">>
                ,<<"From">>
                ,<<"To">>
                ]
               }
              ,{fun kapi_definition:set_optional_headers/2
               ,[<<"Auth-Nonce">>
                ,<<"Auth-Response">>
                ,<<"Call-ID">>
                ,<<"Contact">>
                ,<<"Custom-SIP-Headers">>
                ,<<"Expires">>
                ,<<"Method">>
                ,<<"Orig-IP">>
                ,<<"Orig-Port">>
                ,<<"Switch-Hostname">>
                ,<<"User-Agent">>
                ]
               }
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Auth-Nonce">>, fun is_binary/1}
                ,{<<"Auth-Realm">>, fun is_binary/1}
                ,{<<"Auth-Response">>, fun is_binary/1}
                ,{<<"Auth-User">>, fun is_binary/1}
                ,{<<"Call-ID">>, fun is_binary/1}
                ,{<<"Contact">>, fun is_binary/1}
                ,{<<"Custom-SIP-Headers">>, fun kz_json:is_json_object/1}
                ,{<<"Expires">>, fun is_integer/1}
                ,{<<"From">>, fun is_binary/1}
                ,{<<"Method">>, fun is_binary/1}
                ,{<<"Orig-IP">>, fun is_binary/1}
                ,{<<"Orig-Port">>, fun is_binary/1}
                ,{<<"Switch-Hostname">>, fun is_binary/1}
                ,{<<"To">>, fun is_binary/1}
                ,{<<"User-Agent">>, fun is_binary/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec token_req_definition() -> kapi_definition:api().
token_req_definition() ->
    EventName = <<"token_req">>,
    Category = <<"directory">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Token Authentication Request">>}
              ,{fun kapi_definition:set_description/2, <<"Token Authentication Request">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun token_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun token_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_token_req/1}
              ,{fun kapi_definition:set_required_headers/2
               ,[<<"Auth-Token">>
                ,<<"From">>
                ,<<"To">>
                ]
               }
              ,{fun kapi_definition:set_optional_headers/2
               ,[<<"Call-ID">>
                ,<<"Contact">>
                ,<<"Custom-SIP-Headers">>
                ,<<"Expires">>
                ,<<"Orig-IP">>
                ,<<"Orig-Port">>
                ,<<"Switch-Hostname">>
                ,<<"User-Agent">>
                ]
               }
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Auth-Token">>, fun is_binary/1}
                ,{<<"Call-ID">>, fun is_binary/1}
                ,{<<"Custom-SIP-Headers">>, fun kz_json:is_json_object/1}
                ,{<<"Expires">>, fun is_integer/1}
                ,{<<"From">>, fun is_binary/1}
                ,{<<"Orig-IP">>, fun is_binary/1}
                ,{<<"Orig-Port">>, fun is_binary/1}
                ,{<<"Switch-Hostname">>, fun is_binary/1}
                ,{<<"To">>, fun is_binary/1}
                ,{<<"User-Agent">>, fun is_binary/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec resp_definition() -> kapi_definition:api().
resp_definition() ->
    EventName = <<"authn_resp">>,
    Category = <<"directory">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Authentication Response">>}
              ,{fun kapi_definition:set_description/2, <<"Authentication Response">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_resp/2}
              ,{fun kapi_definition:set_required_headers/2
               ,[<<"Auth-Method">>
                ,<<"Auth-Password">>
                ]
               }
              ,{fun kapi_definition:set_optional_headers/2
               ,[<<"Access-Group">>
                ,<<"Auth-Nonce">>
                ,<<"Auth-Username">>
                ,<<"Custom-Channel-Vars">>
                ,<<"Custom-SIP-Headers">>
                ,<<"Expires">>
                ,<<"Register-Overwrite-Notify">>
                ,<<"Suppress-Unregister-Notifications">>
                ,<<"Tenant-ID">>
                ]
               }
              ,{fun kapi_definition:set_values/2
               ,[{<<"Auth-Method">>, [<<"password">>, <<"ip">>
                                     ,<<"a1-hash">>, <<"error">>
                                     ,<<"gsm">>, <<"nonce">>, <<"a3a8">>
                                     ]}
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Access-Group">>, fun is_binary/1}
                ,{<<"Auth-Password">>, fun is_binary/1}
                ,{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                ,{<<"Custom-SIP-Headers">>, fun kz_json:is_json_object/1}
                ,{<<"Expires">>, fun is_integer/1}
                ,{<<"Tenant-ID">>, fun is_binary/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec token_resp_definition() -> kapi_definition:api().
token_resp_definition() ->
    EventName = <<"token_resp">>,
    Category = <<"directory">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Token Authentication Response">>}
              ,{fun kapi_definition:set_description/2, <<"Token Authentication Response">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun token_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun token_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_token_resp/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Token-Data">>]}
              ,{fun kapi_definition:set_optional_headers/2
               ,[<<"Access-Group">>
                ,<<"Auth-Nonce">>
                ,<<"Custom-Channel-Vars">>
                ,<<"Custom-SIP-Headers">>
                ,<<"Expires">>
                ,<<"Register-Overwrite-Notify">>
                ,<<"Suppress-Unregister-Notifications">>
                ,<<"Tenant-ID">>
                ]
               }
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Access-Group">>, fun is_binary/1}
                ,{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                ,{<<"Custom-SIP-Headers">>, fun kz_json:is_json_object/1}
                ,{<<"Expires">>, fun is_integer/1}
                ,{<<"Tenant-ID">>, fun is_binary/1}
                ,{<<"Token-Data">>, fun kz_json:is_json_object/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec error_definition() -> kapi_definition:api().
error_definition() ->
    EventName = <<"authn_err">>,
    Category = <<"directory">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Authentication Failure Response">>}
              ,{fun kapi_definition:set_description/2, <<"Authentication Failure Response">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun error/1}
              ,{fun kapi_definition:set_validate_fun/2, fun error_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_error/2}
              ,{fun kapi_definition:set_required_headers/2, []}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Authentication Request.
%% Takes {@link kz_term:proplist()}, creates JSON string or error.
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
    kz_amqp_util:callmgr_publish(Payload, ContentType, get_authn_req_routing(Req)).

%%------------------------------------------------------------------------------
%% @doc Token Authentication Request.
%% Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec token_req(kz_term:api_terms()) -> kz_api:api_formatter_return().
token_req(Req) ->
    kapi_definition:build_message(Req, token_req_definition()).

-spec token_req_v(kz_term:api_terms()) -> boolean().
token_req_v(Req) ->
    kapi_definition:validate(Req, token_req_definition()).

-spec publish_token_req(kz_term:api_terms()) -> 'ok'.
publish_token_req(JObj) ->
    publish_token_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_token_req(kz_term:api_terms(), binary()) -> 'ok'.
publish_token_req(Req, ContentType) ->
    Definition = token_req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:callmgr_publish(Payload, ContentType, get_authn_token_req_routing(Req)).

%%------------------------------------------------------------------------------
%% @doc Authentication Response.
%% Takes {@link kz_term:proplist()}, creates JSON string or error.
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

-spec publish_resp(kz_term:ne_binary(), kz_term:api_terms(), binary()) -> 'ok'.
publish_resp(Queue, Resp, ContentType) ->
    Definition = resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Resp
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(Queue, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Token Authentication Response.
%% Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec token_resp(kz_term:api_terms()) -> kz_api:api_formatter_return().
token_resp(Req) ->
    kapi_definition:build_message(Req, token_resp_definition()).

-spec token_resp_v(kz_term:api_terms()) -> boolean().
token_resp_v(Req) ->
    kapi_definition:validate(Req, token_resp_definition()).

-spec publish_token_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_token_resp(Queue, JObj) ->
    publish_token_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_token_resp(kz_term:ne_binary(), kz_term:api_terms(), binary()) -> 'ok'.
publish_token_resp(Queue, Resp, ContentType) ->
    Definition = token_resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Resp
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(Queue, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Authentication Error.
%% Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec error(kz_term:api_terms()) -> kz_api:api_formatter_return().
error(Req) ->
    kapi_definition:build_message(Req, error_definition()).

-spec error_v(kz_term:api_terms()) -> boolean().
error_v(Req) ->
    kapi_definition:validate(Req, error_definition()).

-spec publish_error(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_error(Queue, JObj) ->
    publish_error(Queue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_error(kz_term:ne_binary(), kz_term:api_terms(), binary()) -> 'ok'.
publish_error(Queue, Resp, ContentType) ->
    Definition = error_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Resp
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(Queue, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Setup and tear down bindings for authn `gen_listeners'
%% @end
%%------------------------------------------------------------------------------
-type restrict() :: 'authn_req' | 'token_req'.
-type restrict_to() :: [restrict()].
-type option() :: {'realm', kz_term:ne_binary()} |
                  {'restrict_to', restrict_to()}.
-type options() :: [option()].

-spec bind_q(kz_term:ne_binary(), options()) -> 'ok'.
bind_q(Q, Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    RestrictTo = props:get_value('restrict_to', Props, ['authn_req']),
    bind_q(Q, Realm, RestrictTo).

bind_q(Q, Realm, ['authn_req'|RestrictTo]) ->
    kz_amqp_util:bind_q_to_callmgr(Q, get_authn_req_routing(Realm)),
    bind_q(Q, Realm, RestrictTo);
bind_q(Q, Realm, ['token_req'|RestrictTo]) ->
    kz_amqp_util:bind_q_to_callmgr(Q, get_authn_token_req_routing(Realm)),
    bind_q(Q, Realm, RestrictTo);
bind_q(_Q, _Realm, []) -> 'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    RestrictTo = props:get_value('restrict_to', Props, ['authn_req']),
    unbind_q(Q, Realm, RestrictTo).

unbind_q(Q, Realm, ['authn_req' | RestrictTo]) ->
    kz_amqp_util:unbind_q_from_callmgr(Q, get_authn_req_routing(Realm)),
    unbind_q(Q, Realm, RestrictTo);
unbind_q(Q, Realm, ['token_req' | RestrictTo]) ->
    kz_amqp_util:unbind_q_from_callmgr(Q, get_authn_token_req_routing(Realm)),
    unbind_q(Q, Realm, RestrictTo);
unbind_q(_Q, _Realm, []) -> 'ok'.

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:callmgr_exchange().

%%------------------------------------------------------------------------------
%% @doc Creating the routing key for either binding queues or publishing messages.
%% @end
%%------------------------------------------------------------------------------
-spec get_authn_req_routing(kz_term:ne_binary() | kz_term:api_terms()) -> kz_term:ne_binary().
get_authn_req_routing(Realm) when is_binary(Realm) ->
    list_to_binary([?KEY_AUTHN_REQ, ".", kz_amqp_util:encode(Realm)]);
get_authn_req_routing(Req) ->
    get_authn_req_routing(get_auth_realm(Req)).

%%------------------------------------------------------------------------------
%% @doc Creating the routing key for either binding queues or publishing messages.
%% @end
%%------------------------------------------------------------------------------
-spec get_authn_token_req_routing(kz_term:ne_binary() | kz_term:api_terms()) -> kz_term:ne_binary().
get_authn_token_req_routing(Realm) when is_binary(Realm) ->
    list_to_binary([?KEY_AUTHN_TOKEN_REQ, ".", kz_amqp_util:encode(Realm)]);
get_authn_token_req_routing(Req) ->
    get_authn_token_req_routing(get_auth_realm(Req)).

-define(AUTH_DEFAULT_USERATDOMAIN, <<"nouser@nodomain">>).

%%------------------------------------------------------------------------------
%% @doc Extract the auth user from the API request.
%% @end
%%------------------------------------------------------------------------------
-spec get_auth_user(kz_json:object()) -> kz_term:api_binary().
get_auth_user(ApiJObj) ->
    AuthUser = case kz_json:get_value(<<"Auth-User">>, ApiJObj, <<"unknown">>) of
                   <<"unknown">> ->
                       To = kz_json:get_value(<<"To">>, ApiJObj, ?AUTH_DEFAULT_USERATDOMAIN),
                       [ToUser, _ToDomain] = binary:split(To, <<"@">>),
                       ToUser;
                   Username -> Username
               end,
    kz_term:to_lower_binary(AuthUser).

%%------------------------------------------------------------------------------
%% @doc Extract the auth realm from the API request, using the requests to domain
%% when provided with an IP.
%% @end
%%------------------------------------------------------------------------------
-spec get_auth_realm(kz_json:object() | kz_term:proplist()) -> kz_term:ne_binary().
get_auth_realm(ApiProp) when is_list(ApiProp) ->
    AuthRealm = props:get_value(<<"Auth-Realm">>, ApiProp, <<"missing.realm">>),
    Realm = case kz_network_utils:is_ipv4(AuthRealm)
                orelse kz_network_utils:is_ipv6(AuthRealm)
            of
                'false' -> AuthRealm;
                'true' ->
                    To = props:get_value(<<"To">>, ApiProp, ?AUTH_DEFAULT_USERATDOMAIN),
                    [_ToUser, ToDomain] = binary:split(To, <<"@">>),
                    ToDomain
            end,
    kz_term:to_lower_binary(Realm);
get_auth_realm(ApiJObj) -> get_auth_realm(kz_json:to_proplist(ApiJObj)).

-spec req_event_type() -> {kz_term:ne_binary(), kz_term:ne_binary()}.
req_event_type() ->
    Definition = req_definition(),
    {kapi_definition:category(Definition), kapi_definition:name(Definition)}.
