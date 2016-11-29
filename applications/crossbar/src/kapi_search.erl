%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% Routing requests, responses, and wins!
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(kapi_search).

-export([declare_exchanges/0
        ,bind_q/2, unbind_q/2
        ]).

-export([register/1, register_v/1
        ,req/1, req_v/1
        ,resp/1, resp_v/1

        ,publish_register/1, publish_register/2
        ,publish_req/1, publish_req/2
        ,publish_resp/2, publish_resp/3

        ,query_id/1
        ,quantity/1
        ,offset/1
        ,results/1
        ,module/1
        ,method/1
        ,options/1
        ,option/2
        ]).

-include_lib("kazoo/include/kz_types.hrl").

-define(KEY_QUERY_ID, <<"Query-ID">>).
-define(KEY_QUANTITY, <<"Quantity">>).
-define(KEY_OFFSET, <<"Offset">>).
-define(KEY_RESULTS, <<"Results">>).
-define(KEY_MODULE, <<"Module">>).
-define(KEY_METHOD, <<"Method">>).
-define(KEY_OPTIONS, <<"Options">>).

-define(SEARCH_EXCHANGE, <<"search">>).
-define(SEARCH_EXCHANGE_TYPE, <<"topic">>).


-define(SEARCH_EVENT_CATEGORY, <<"search">>).
-define(SEARCH_RK, <<"search.*">>).

%% Search Register Request
-define(SEARCH_REGISTER_RK, <<"search.register">>).
-define(SEARCH_REGISTER_EVENT_NAME, <<"register">>).
-define(SEARCH_REGISTER_HEADERS, [?KEY_QUERY_ID]).
-define(SEARCH_REGISTER_OPTIONAL_HEADERS, []).
-define(SEARCH_REGISTER_VALUES, [{<<"Event-Category">>, ?SEARCH_EVENT_CATEGORY}
                                ,{<<"Event-Name">>, ?SEARCH_REGISTER_EVENT_NAME}
                                ]).
-define(SEARCH_REGISTER_TYPES, [{?KEY_QUERY_ID, fun is_binary/1}
                               ]).


%% Search Request
-define(SEARCH_REQ_RK, <<"search.request">>).
-define(SEARCH_REQ_EVENT_NAME, <<"request">>).
-define(SEARCH_REQ_HEADERS, [?KEY_QUERY_ID, ?KEY_OPTIONS
                            ,?KEY_OFFSET, ?KEY_QUANTITY
                            ,?KEY_MODULE, ?KEY_METHOD
                            ]).
-define(SEARCH_REQ_OPTIONAL_HEADERS, []).
-define(SEARCH_REQ_VALUES, [{<<"Event-Category">>, ?SEARCH_EVENT_CATEGORY}
                           ,{<<"Event-Name">>, ?SEARCH_REQ_EVENT_NAME}
                           ]).
-define(SEARCH_REQ_TYPES, [{?KEY_QUERY_ID, fun is_binary/1}
                          ,{?KEY_OFFSET, fun is_integer/1}
                          ,{?KEY_QUANTITY, fun is_integer/1}
                          ]).


%% Search Response
-define(SEARCH_RESP_EVENT_NAME, <<"response">>).
-define(SEARCH_RESP_HEADERS, [?KEY_RESULTS]).
-define(SEARCH_RESP_OPTIONAL_HEADERS, [?KEY_QUERY_ID]).
-define(SEARCH_RESP_VALUES, [{<<"Event-Category">>, ?SEARCH_EVENT_CATEGORY}
                            ,{<<"Event-Name">>, ?SEARCH_RESP_EVENT_NAME}
                            ]).
-define(SEARCH_RESP_TYPES, [{?KEY_QUERY_ID, fun is_binary/1}
                           ]).


%%--------------------------------------------------------------------
%% @doc Search Request
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec req(api_terms()) ->
                 {'ok', iolist()} |
                 {'error', string()}.
req(Prop) when is_list(Prop) ->
    case req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?SEARCH_REQ_HEADERS, ?SEARCH_REQ_OPTIONAL_HEADERS);
        'false' -> {'error', "Proplist failed validation for search request"}
    end;
req(JObj) -> req(kz_json:to_proplist(JObj)).

-spec req_v(api_terms()) -> boolean().
req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SEARCH_REQ_HEADERS, ?SEARCH_REQ_VALUES, ?SEARCH_REQ_TYPES);
req_v(JObj) -> req_v(kz_json:to_proplist(JObj)).


%%--------------------------------------------------------------------
%% @doc Search Response
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec resp(api_terms()) ->
                  {'ok', iolist()} |
                  {'error', string()}.
resp(Prop) when is_list(Prop) ->
    case resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?SEARCH_RESP_HEADERS, ?SEARCH_RESP_OPTIONAL_HEADERS);
        'false' -> {'error', "Proplist failed validation for search response"}
    end;
resp(JObj) -> resp(kz_json:to_proplist(JObj)).

-spec resp_v(api_terms()) -> boolean().
resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SEARCH_RESP_HEADERS, ?SEARCH_RESP_VALUES, ?SEARCH_RESP_TYPES);
resp_v(JObj) -> resp_v(kz_json:to_proplist(JObj)).


%%--------------------------------------------------------------------
%% @doc Search Flush
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec register(api_terms()) ->
                      {'ok', iolist()} |
                      {'error', string()}.
register(Prop) when is_list(Prop) ->
    case register_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?SEARCH_REGISTER_HEADERS, ?SEARCH_REGISTER_OPTIONAL_HEADERS);
        'false' -> {'error', "Proplist failed validation for search response"}
    end;
register(JObj) -> register(kz_json:to_proplist(JObj)).

-spec register_v(api_terms()) -> boolean().
register_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SEARCH_REGISTER_HEADERS, ?SEARCH_REGISTER_VALUES, ?SEARCH_REGISTER_TYPES);
register_v(JObj) -> register_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Bind AMQP Queue for routing requests
%% @end
%%--------------------------------------------------------------------
-spec bind_q(ne_binary(), kz_proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    bind_q(Queue, props:get_value('restrict_to', Props), Props).

-spec bind_q(ne_binary(), list() | 'undefined', kz_proplist()) -> 'ok'.
bind_q(Queue, 'undefined', _Props) ->
    amqp_util:bind_q_to_exchange(Queue, ?SEARCH_RK, ?SEARCH_EXCHANGE);
bind_q(Queue, ['search' | T], Props) ->
    amqp_util:bind_q_to_exchange(Queue, ?SEARCH_RK, ?SEARCH_EXCHANGE),
    bind_q(Queue, T, Props);
bind_q(Queue, [_ | T], Props) ->
    bind_q(Queue, T, Props);
bind_q(_, [], _) -> 'ok'.

-spec unbind_q(ne_binary(), kz_proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    unbind_q(Queue, props:get_value('restrict_to', Props), Props).

-spec unbind_q(ne_binary(), list() | 'undefined', kz_proplist()) -> 'ok'.
unbind_q(Queue, 'undefined', _Props) ->
    amqp_util:unbind_q_from_exchange(Queue, ?SEARCH_RK, ?SEARCH_EXCHANGE);
unbind_q(Queue, ['search' | T], Props) ->
    amqp_util:unbind_q_from_exchange(Queue, ?SEARCH_RK, ?SEARCH_EXCHANGE),
    unbind_q(Queue, T, Props);
unbind_q(Queue, [_ | T], Props) ->
    unbind_q(Queue, T, Props);
unbind_q(_, [], _) -> 'ok'.

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:new_exchange(?SEARCH_EXCHANGE, ?SEARCH_EXCHANGE_TYPE).

-spec publish_register(api_terms()) -> 'ok'.
-spec publish_register(api_terms(), binary()) -> 'ok'.
publish_register(JObj) ->
    publish_register(JObj, ?DEFAULT_CONTENT_TYPE).
publish_register(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?SEARCH_REGISTER_VALUES, fun register/1),
    amqp_util:basic_publish(?SEARCH_EXCHANGE, ?SEARCH_REGISTER_RK, Payload, ContentType).

-spec publish_req(api_terms()) -> 'ok'.
-spec publish_req(api_terms(), binary()) -> 'ok'.
publish_req(JObj) ->
    publish_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_req(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?SEARCH_REQ_VALUES, fun req/1),
    amqp_util:basic_publish(?SEARCH_EXCHANGE, ?SEARCH_REQ_RK, Payload, ContentType).

-spec publish_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_resp(RespQ, JObj) ->
    publish_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_resp(RespQ, Resp, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Resp, ?SEARCH_RESP_VALUES, fun resp/1),
    amqp_util:targeted_publish(RespQ, Payload, ContentType).

-spec query_id(kz_json:object() | kz_proplist()) -> api_ne_binary().
query_id(Props) when is_list(Props) ->
    props:get_ne_binary_value(?KEY_QUERY_ID, Props);
query_id(JObj) ->
    kz_json:get_ne_binary_value(?KEY_QUERY_ID, JObj).

-spec quantity(kz_json:object()) -> integer().
quantity(JObj) ->
    kz_json:get_integer_value(?KEY_QUANTITY, JObj).

-spec offset(kz_json:object()) -> integer().
offset(JObj) ->
    kz_json:get_integer_value(?KEY_OFFSET, JObj).

-spec results(kz_json:object()) -> list().
results(JObj) ->
    kz_json:get_value(?KEY_RESULTS, JObj).

-spec module(kz_json:object()) -> atom().
module(JObj) ->
    kz_json:get_atom_value(?KEY_MODULE, JObj).

-spec method(kz_json:object()) -> atom().
method(JObj) ->
    kz_json:get_atom_value(?KEY_METHOD, JObj).

-spec options(kz_json:object()) -> api_object().
options(JObj) ->
    kz_json:get_value(?KEY_OPTIONS, JObj).

-spec option(ne_binary(), kz_json:object()) -> any().
option(Name, JObj) ->
    kz_json:get_ne_binary_value(Name, options(JObj)).
