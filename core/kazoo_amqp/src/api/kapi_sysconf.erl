%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2022, 2600Hz
%%% @doc Expose system configuration data.
%%% System configuration data is stored as key/values in a namespace
%%% (a doc) in `system_config' DB.
%%%
%%% @author Edouard Swiac
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_sysconf).

-export([get_req/1, get_req_v/1
        ,get_resp/1, get_resp_v/1
        ,set_req/1, set_req_v/1
        ,set_resp/1, set_resp_v/1
        ,flush_req/1, flush_req_v/1

        ,bind_q/2, unbind_q/2
        ,declare_exchanges/0

        ,publish_get_req/1, publish_get_req/2
        ,publish_get_resp/2, publish_get_resp/3
        ,publish_set_req/1, publish_set_req/2
        ,publish_set_resp/2, publish_set_resp/3
        ,publish_flush_req/1, publish_flush_req/2

        ,get_category/1, get_category/2
        ,get_key/1, get_key/2
        ,get_value/1, get_value/2
        ]).

-include_lib("kz_amqp_util.hrl").

-define(CAT_KEY, <<"Category">>).
-define(KEY_KEY, <<"Key">>).
-define(VALUE_KEY, <<"Value">>).

-define(SYSCONF_VALUES, [{<<"Event-Category">>, <<"sysconf">>}]).

%% sysconf whapp routing keys for responses to clients
-define(KEY_SYSCONF_GET_REQ, <<"sysconf.get">>).
-define(KEY_SYSCONF_SET_REQ, <<"sysconf.set">>).
-define(KEY_SYSCONF_FLUSH_REQ, <<"sysconf.flush">>).

%% Configuration Document Update
%% request to read
-define(SYSCONF_GET_REQ_HEADERS, [?CAT_KEY, ?KEY_KEY]).
-define(OPTIONAL_SYSCONF_GET_REQ_HEADERS, [<<"Default">>]).
-define(SYSCONF_GET_REQ_VALUES, [{<<"Event-Name">>, <<"get_req">>} | ?SYSCONF_VALUES]).

%% answer to a read request
-define(SYSCONF_GET_RESP_HEADERS, [?CAT_KEY, ?KEY_KEY, ?VALUE_KEY]).
-define(OPTIONAL_SYSCONF_GET_RESP_HEADERS, []).
-define(SYSCONF_GET_RESP_VALUES, [{<<"Event-Name">>, <<"get_resp">>} | ?SYSCONF_VALUES]).

%% request a write
-define(SYSCONF_SET_REQ_HEADERS, [?CAT_KEY, ?KEY_KEY, ?VALUE_KEY]).
-define(OPTIONAL_SYSCONF_SET_REQ_HEADERS, [<<"Node-Specific">>]).
-define(SYSCONF_SET_REQ_VALUES, [{<<"Event-Name">>, <<"set_req">>} | ?SYSCONF_VALUES]).

-define(SYSCONF_FLUSH_REQ_HEADERS, [?CAT_KEY]).
-define(OPTIONAL_SYSCONF_FLUSH_REQ_HEADERS, [?KEY_KEY]).
-define(SYSCONF_FLUSH_REQ_VALUES, [{<<"Event-Name">>, <<"flush_req">>} | ?SYSCONF_VALUES]).

%% answer to a write request
-define(SYSCONF_SET_RESP_HEADERS, [?CAT_KEY, ?KEY_KEY, ?VALUE_KEY]).
-define(OPTIONAL_SYSCONF_SET_RESP_HEADERS, [<<"Status">>]).
-define(SYSCONF_SET_RESP_VALUES, [{<<"Event-Name">>, <<"set_resp">>} | ?SYSCONF_VALUES]).

-define(SYSCONF_TYPES, [{?CAT_KEY, fun is_binary/1}
                       ,{<<"Node">>, fun is_binary/1}
                       ]).

%%------------------------------------------------------------------------------
%% @doc READ.
%% @end
%%------------------------------------------------------------------------------
-spec get_req(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
get_req(Prop) when is_list(Prop) ->
    case get_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?SYSCONF_GET_REQ_HEADERS, ?OPTIONAL_SYSCONF_GET_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for sysconf get_req"}
    end;
get_req(JObj) ->
    get_req(kz_json:to_proplist(JObj)).

-spec get_req_v(kz_term:api_terms()) -> boolean().
get_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SYSCONF_GET_REQ_HEADERS, ?SYSCONF_GET_REQ_VALUES, ?SYSCONF_TYPES);
get_req_v(JObj) ->
    get_req_v(kz_json:to_proplist(JObj)).

-spec get_resp(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
get_resp(Prop) when is_list(Prop) ->
    case get_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?SYSCONF_GET_RESP_HEADERS, ?OPTIONAL_SYSCONF_GET_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for sysconf get_resp"}
    end;
get_resp(JObj) ->
    get_resp(kz_json:to_proplist(JObj)).

-spec get_resp_v(kz_term:api_terms()) -> boolean().
get_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SYSCONF_GET_RESP_HEADERS, ?SYSCONF_GET_RESP_VALUES, ?SYSCONF_TYPES);
get_resp_v(JObj) ->
    get_resp_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc WRITE.
%% @end
%%------------------------------------------------------------------------------
-spec set_req(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
set_req(Prop) when is_list(Prop) ->
    case set_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?SYSCONF_SET_REQ_HEADERS, ?OPTIONAL_SYSCONF_SET_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for sysconf write"}
    end;
set_req(JObj) ->
    set_req(kz_json:to_proplist(JObj)).

-spec set_req_v(kz_term:api_terms()) -> boolean().
set_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SYSCONF_SET_REQ_HEADERS, ?SYSCONF_SET_REQ_VALUES, ?SYSCONF_TYPES);
set_req_v(JObj) ->
    set_req_v(kz_json:to_proplist(JObj)).

-spec set_resp(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
set_resp(Prop) when is_list(Prop) ->
    case set_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?SYSCONF_SET_RESP_HEADERS, ?OPTIONAL_SYSCONF_SET_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for sysconf write"}
    end;
set_resp(JObj) ->
    set_resp(kz_json:to_proplist(JObj)).

-spec set_resp_v(kz_term:api_terms()) -> boolean().
set_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SYSCONF_SET_RESP_HEADERS, ?SYSCONF_SET_RESP_VALUES, ?SYSCONF_TYPES);
set_resp_v(JObj) ->
    set_resp_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Flush a given key.
%% @end
%%------------------------------------------------------------------------------
-spec flush_req(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
flush_req(Prop) when is_list(Prop) ->
    case flush_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?SYSCONF_FLUSH_REQ_HEADERS, ?OPTIONAL_SYSCONF_FLUSH_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for sysconf read"}
    end;
flush_req(JObj) ->
    flush_req(kz_json:to_proplist(JObj)).

-spec flush_req_v(kz_term:api_terms()) -> boolean().
flush_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SYSCONF_FLUSH_REQ_HEADERS, ?SYSCONF_FLUSH_REQ_VALUES, ?SYSCONF_TYPES);
flush_req_v(JObj) ->
    flush_req_v(kz_json:to_proplist(JObj)).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Q, Prop) ->
    add_bindings(Q, props:get_value('restrict_to', Prop)).

add_bindings(Q, 'undefined') ->
    add_bindings(Q, ['get', 'set', 'flush']);
add_bindings(Q, ['get'|T]) ->
    _ = kz_amqp_util:bind_q_to_sysconf(Q, routing_key_get()),
    add_bindings(Q, T);
add_bindings(Q, ['set'|T]) ->
    _ = kz_amqp_util:bind_q_to_sysconf(Q, routing_key_set()),
    add_bindings(Q, T);
add_bindings(Q, ['flush'|T]) ->
    _ = kz_amqp_util:bind_q_to_sysconf(Q, routing_key_flush()),
    add_bindings(Q, T);
add_bindings(Q, [_|T]) ->
    add_bindings(Q, T);
add_bindings(_, []) ->
    'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Q, Prop) ->
    rm_bindings(Q, props:get_value('restrict_to', Prop)).

rm_bindings(Q, 'undefined') ->
    _ = kz_amqp_util:unbind_q_from_sysconf(Q, routing_key_get()),
    _ = kz_amqp_util:unbind_q_from_sysconf(Q, routing_key_set()),
    _ = kz_amqp_util:unbind_q_from_sysconf(Q, routing_key_flush());
rm_bindings(Q, ['get'|T]) ->
    _ = kz_amqp_util:unbind_q_from_sysconf(Q, routing_key_get()),
    rm_bindings(Q, T);
rm_bindings(Q, ['set'|T]) ->
    _ = kz_amqp_util:unbind_q_from_sysconf(Q, routing_key_set()),
    rm_bindings(Q, T);
rm_bindings(Q, ['flush'|T]) ->
    _ = kz_amqp_util:unbind_q_from_sysconf(Q, routing_key_flush()),
    rm_bindings(Q, T);
rm_bindings(Q, [_|T]) ->
    rm_bindings(Q, T);
rm_bindings(_, []) ->
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:sysconf_exchange().

-spec publish_get_req(kz_term:api_terms()) -> 'ok'.
publish_get_req(JObj) ->
    publish_get_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_get_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_get_req(Api, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Api, ?SYSCONF_GET_REQ_VALUES, fun get_req/1),
    kz_amqp_util:sysconf_publish(routing_key_get(), Payload, ContentType).

-spec publish_get_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_get_resp(RespQ, JObj) ->
    publish_get_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_get_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_get_resp(RespQ, Api, ContentType) ->
    PrepareOptions = [{'formatter', fun get_resp/1}
                     ,{'remove_recursive', 'false'}
                     ],
    {'ok', Payload} = kz_api:prepare_api_payload(Api, ?SYSCONF_GET_RESP_VALUES, PrepareOptions),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).

-spec publish_set_req(kz_term:api_terms()) -> 'ok'.
publish_set_req(JObj) ->
    publish_set_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_set_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_set_req(Api, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Api, ?SYSCONF_SET_REQ_VALUES, fun set_req/1),
    kz_amqp_util:sysconf_publish(routing_key_set(), Payload, ContentType).

-spec publish_set_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_set_resp(RespQ, JObj) ->
    publish_set_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_set_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_set_resp(RespQ, Api, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Api, ?SYSCONF_SET_RESP_VALUES, fun set_resp/1),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).

-spec publish_flush_req(kz_term:api_terms()) -> 'ok'.
publish_flush_req(JObj) ->
    publish_flush_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_flush_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_flush_req(Api, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Api, ?SYSCONF_FLUSH_REQ_VALUES, fun flush_req/1),
    kz_amqp_util:sysconf_publish(routing_key_flush(), Payload, ContentType).

routing_key_get() ->
    ?KEY_SYSCONF_GET_REQ.

routing_key_set() ->
    ?KEY_SYSCONF_SET_REQ.

routing_key_flush() ->
    ?KEY_SYSCONF_FLUSH_REQ.


-spec get_category(kz_json:object()) -> kz_term:api_binary().
get_category(JObj) ->
    get_category(JObj, 'undefined').

-spec get_category(kz_json:object(), Default) -> kz_term:ne_binary() | Default.
get_category(JObj, Default) ->
    kz_json:get_value(?CAT_KEY, JObj, Default).

-spec get_key(kz_json:object()) -> kz_term:api_binary().
get_key(JObj) ->
    get_key(JObj, 'undefined').

-spec get_key(kz_json:object(), Default) -> kz_term:ne_binary() | Default.
get_key(JObj, Default) ->
    kz_json:get_value(?KEY_KEY, JObj, Default).

-spec get_value(kz_json:object()) -> kz_term:api_object().
get_value(JObj) ->
    get_value(JObj, 'undefined').

-spec get_value(kz_json:object(), Default) -> kz_json:object() | Default.
get_value(JObj, Default) ->
    kz_json:get_value(?VALUE_KEY, JObj, Default).
