%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2022, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_websockets).

-export([get_req/1, get_req_v/1
        ,get_resp/1, get_resp_v/1
        ,module_req/1, module_req_v/1
        ,module_resp/1, module_resp_v/1

        ,bind_q/2
        ,unbind_q/2
        ,declare_exchanges/0

        ,publish_get_req/1, publish_get_req/2
        ,publish_get_resp/2, publish_get_resp/3
        ,publish_module_req/1, publish_module_req/2
        ,publish_module_resp/2, publish_module_resp/3
        ]).

-include("kapi_websockets.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").

-type restriction() :: 'get' | 'module_req'.
-type restrictions() :: [restriction()].
-type bind_prop() :: {'restrict_to', restrictions()}.
-type bind_props() :: [bind_prop()].

-define(DEFAULT_RESTRICTIONS, ['get', 'module_req']).

-export_type([bind_props/0]).

-spec get_req(kz_term:api_terms()) ->{'ok', iolist()} | {'error', string()}.
get_req(Prop) when is_list(Prop) ->
    case get_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?WEBSOCKETS_GET_REQ_HEADERS, ?OPTIONAL_WEBSOCKETS_GET_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for websockets get_req"}
    end;
get_req(JObj) ->
    get_req(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_req_v(kz_term:api_terms()) -> boolean().
get_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?WEBSOCKETS_GET_REQ_HEADERS, ?WEBSOCKETS_GET_REQ_VALUES, ?WEBSOCKETS_TYPES);
get_req_v(JObj) ->
    get_req_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_resp(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
get_resp(Prop) when is_list(Prop) ->
    case get_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?WEBSOCKETS_GET_RESP_HEADERS, ?OPTIONAL_WEBSOCKETS_GET_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for websockets get_resp"}
    end;
get_resp(JObj) ->
    get_resp(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_resp_v(kz_term:api_terms()) -> boolean().
get_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?WEBSOCKETS_GET_RESP_HEADERS, ?WEBSOCKETS_GET_RESP_VALUES, ?WEBSOCKETS_TYPES);
get_resp_v(JObj) ->
    get_resp_v(kz_json:to_proplist(JObj)).


-spec module_req(kz_term:api_terms()) -> {'ok', iolist()} |
          {'error', string()}.
module_req(Prop) when is_list(Prop) ->
    case module_req_v(Prop) of
        'false' -> {'error', "Proplist failed validation for module_req"};
        'true' -> kz_api:build_message(Prop, ?MODULE_REQ_HEADERS, ?OPTIONAL_MODULE_REQ_HEADERS)
    end;
module_req(JObj) ->
    module_req(kz_json:to_proplist(JObj)).

-spec module_req_v(kz_term:api_terms()) -> boolean().
module_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?MODULE_REQ_HEADERS, ?MODULE_REQ_VALUES, ?MODULE_REQ_TYPES);
module_req_v(JObj) ->
    module_req_v(kz_json:to_proplist(JObj)).

-spec module_resp(kz_term:api_terms()) -> {'ok', iolist()} |
          {'error', string()}.
module_resp(Prop) when is_list(Prop) ->
    case module_resp_v(Prop) of
        'false' -> {'error', "Proplist failed validation for module_resp"};
        'true' -> kz_api:build_message(Prop, ?MODULE_RESP_HEADERS, ?OPTIONAL_MODULE_RESP_HEADERS)
    end;
module_resp(JObj) ->
    module_resp(kz_json:to_proplist(JObj)).

-spec module_resp_v(kz_term:api_terms()) -> boolean().
module_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?MODULE_RESP_HEADERS, ?MODULE_RESP_VALUES, ?MODULE_RESP_TYPES);
module_resp_v(JObj) ->
    module_resp_v(kz_json:to_proplist(JObj)).

-spec bind_q(kz_term:ne_binary(), bind_props()) -> 'ok'.
bind_q(Queue, Props) ->
    lists:foreach(fun(Restriction) -> add_restriction(Queue, Restriction) end
                 ,props:get_value('restrict_to', Props, ?DEFAULT_RESTRICTIONS)
                 ).

-spec add_restriction(kz_term:ne_binary(), restriction()) -> 'ok'.
add_restriction(Queue, 'get') ->
    kz_amqp_util:bind_q_to_sysconf(Queue, ?KEY_WEBSOCKETS_GET_REQ);
add_restriction(Queue, 'module_req') ->
    kz_amqp_util:bind_q_to_kapps(Queue, ?MODULE_REQ_ROUTING_KEY).


-spec unbind_q(kz_term:ne_binary(), bind_props()) -> 'ok'.
unbind_q(Queue, Props) ->
    lists:foreach(fun(Restriction) -> remove_restriction(Queue, Restriction) end
                 ,props:get_value('restrict_to', Props, ?DEFAULT_RESTRICTIONS)
                 ).

-spec remove_restriction(kz_term:ne_binary(), restriction()) -> 'ok'.
remove_restriction(Queue, 'get') ->
    kz_amqp_util:unbind_q_from_sysconf(Queue, ?KEY_WEBSOCKETS_GET_REQ);
remove_restriction(Queue, 'module_req') ->
    kz_amqp_util:unbind_q_from_kapps(Queue, ?MODULE_REQ_ROUTING_KEY).

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:kapps_exchange(),
    kz_amqp_util:targeted_exchange(),
    kz_amqp_util:sysconf_exchange().

-spec publish_module_req(kz_term:api_terms()) -> 'ok'.
publish_module_req(API) ->
    publish_module_req(API, ?DEFAULT_CONTENT_TYPE).

-spec publish_module_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_module_req(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?MODULE_REQ_VALUES, fun module_req/1),
    kz_amqp_util:kapps_publish(?MODULE_REQ_ROUTING_KEY, Payload, ContentType).

-spec publish_module_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_module_resp(ServerId, API) ->
    publish_module_resp(ServerId, API, ?DEFAULT_CONTENT_TYPE).

-spec publish_module_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_module_resp(ServerId, API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?MODULE_RESP_VALUES, fun module_resp/1),
    kz_amqp_util:targeted_publish(ServerId, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec publish_get_req(kz_term:api_terms()) -> 'ok'.
publish_get_req(JObj) ->
    publish_get_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_get_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_get_req(Api, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Api, ?WEBSOCKETS_GET_REQ_VALUES, fun get_req/1),
    kz_amqp_util:sysconf_publish(?KEY_WEBSOCKETS_GET_REQ, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec publish_get_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_get_resp(RespQ, JObj) ->
    publish_get_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_get_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_get_resp(RespQ, Api, ContentType) ->
    PrepareOptions = [{'formatter', fun get_resp/1}
                     ,{'remove_recursive', 'false'}
                     ],
    {'ok', Payload} = kz_api:prepare_api_payload(Api, ?WEBSOCKETS_GET_RESP_VALUES, PrepareOptions),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).
